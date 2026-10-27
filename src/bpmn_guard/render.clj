(ns bpmn-guard.render
  "Layer 4 — pure java.awt rendering.

   Two stages, both driven only by the model + DI produced from the .bpmn file:

     render-diagram   model, di, opts            -> BufferedImage (base diagram)
     render-report    report, opts               -> BufferedImage (base + overlay)

   plus annotate-image for the legacy case where a pre-rendered PNG exists.

   Coordinate mapping: pixel = (di - extent-min) * scale + padding.
   Nothing outside the JDK (java.awt, javax.imageio) is used."
  (:require [bpmn-guard.di :as di]
            [bpmn-guard.rules :as rules]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.awt BasicStroke Color Font FontMetrics Graphics2D Polygon RenderingHints]
           [java.awt.geom Ellipse2D$Double Line2D$Double Path2D$Double RoundRectangle2D$Double]
           [java.awt.image BufferedImage]
           [java.io File IOException InputStream OutputStream]
           [javax.imageio ImageIO]))

;; ---------------------------------------------------------------------------
;; Style
;; ---------------------------------------------------------------------------

(def default-opts
  {:scale          1.0
   :padding        40
   :radius-padding 6.0
   :stroke         2.5
   :badges?        true
   :font           (Font. Font/SANS_SERIF Font/PLAIN 12)
   :badge-font     (Font. Font/SANS_SERIF Font/BOLD 10)
   :marker-font    (Font. Font/SANS_SERIF Font/BOLD 20)})

(def ^:private palette
  {:background  Color/WHITE
   :shape-fill  (Color. 242 242 242)
   :scope-fill  (Color. 250 250 250)
   :shape-line  (Color. 70 70 70)
   :edge-line   (Color. 60 60 60)
   :text        (Color. 30 30 30)
   :unknown     (Color. 200 200 200)})

(def class-colors
  "Severity class -> highlight colour.  Classes come from rules/rule-catalogue:
   :oom (R1 R3 R5) red, :deadlock (R2 R4 R7) orange, :starvation (R6) amber."
  {:oom        (Color. 215 25 25)
   :deadlock   (Color. 240 125 0)
   :starvation (Color. 225 175 0)})

(defn rule-color ^Color [rule]
  (get class-colors (get-in rules/rule-catalogue [rule :class]) (Color. 215 25 25)))

(defn badge-text ^String [rule]
  (format "[%s: %s]" (name rule) (get-in rules/rule-catalogue [rule :title] "Violation")))

;; ---------------------------------------------------------------------------
;; Coordinate mapping
;; ---------------------------------------------------------------------------

(defn make-transform
  "di, opts -> {:tx fn :ty fn :ts fn :width :height} in pixels."
  [di {:keys [scale padding]}]
  (let [{:keys [min-x min-y max-x max-y] :or {min-x 0 min-y 0 max-x 400 max-y 200}} (di/extent di)
        ts (fn [v] (* scale v))
        tx (fn [x] (+ padding (ts (- x min-x))))
        ty (fn [y] (+ padding (ts (- y min-y))))]
    {:tx tx :ty ty :ts ts
     :width  (int (Math/ceil (+ (* 2 padding) (ts (- max-x min-x)))))
     :height (int (Math/ceil (+ (* 2 padding) (ts (- max-y min-y)))))}))

(defn- px-rect [{:keys [tx ty ts]} {:keys [x y width height]}]
  {:x (tx x) :y (ty y) :w (ts width) :h (ts height)})

;; ---------------------------------------------------------------------------
;; Primitive drawing
;; ---------------------------------------------------------------------------

(defn- stroke ^BasicStroke [w]
  (BasicStroke. (float w) BasicStroke/CAP_ROUND BasicStroke/JOIN_ROUND))

(defn- dashed ^BasicStroke [w]
  (BasicStroke. (float w) BasicStroke/CAP_BUTT BasicStroke/JOIN_MITER 10.0
                (float-array [6.0 4.0]) 0.0))

(defn- enable-quality! [^Graphics2D g]
  (doto g
    (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
    (.setRenderingHint RenderingHints/KEY_STROKE_CONTROL RenderingHints/VALUE_STROKE_PURE)
    (.setRenderingHint RenderingHints/KEY_TEXT_ANTIALIASING RenderingHints/VALUE_TEXT_ANTIALIAS_ON)
    (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)))

(defn- wrap-text
  "Greedy word wrap to max-width pixels; a single over-long word is kept whole."
  [^FontMetrics fm ^String text max-width]
  (loop [words (remove str/blank? (str/split text #"\s+")) line "" lines []]
    (if (empty? words)
      (if (str/blank? line) lines (conj lines line))
      (let [w    (first words)
            cand (if (str/blank? line) w (str line " " w))]
        (if (or (str/blank? line) (<= (.stringWidth fm cand) max-width))
          (recur (rest words) cand lines)
          (recur words "" (conj lines line)))))))

(defn- draw-text-block!
  "Draw wrapped lines centred horizontally on cx; the block's vertical centre
   is at cy when :middle, or its top is at cy when :top."
  [^Graphics2D g ^String text cx cy max-width anchor ^Color color]
  (when-not (str/blank? text)
    (let [fm    (.getFontMetrics g)
          lines (wrap-text fm text max-width)
          lh    (.getHeight fm)
          total (* lh (count lines))
          top   (if (= anchor :middle) (- cy (/ total 2.0)) cy)]
      (.setColor g color)
      (doseq [[i ^String ln] (map-indexed vector lines)]
        (.drawString g ln
                     (int (Math/round (- cx (/ (.stringWidth fm ln) 2.0))))
                     (int (Math/round (+ top (* i lh) (.getAscent fm)))))))))

(defn- fill-and-outline! [^Graphics2D g shape ^Color fill ^Color line w]
  (.setColor g fill) (.fill g shape)
  (.setColor g line) (.setStroke g (stroke w)) (.draw g shape))

;; ---------------------------------------------------------------------------
;; BPMN glyphs
;; ---------------------------------------------------------------------------

(defn- draw-activity! [^Graphics2D g {:keys [x y w h]} node opts]
  (let [container? (:container? node)
        rr (RoundRectangle2D$Double. x y w h (if container? 14.0 10.0) (if container? 14.0 10.0))]
    (fill-and-outline! g rr (palette (if container? :scope-fill :shape-fill)) (palette :shape-line) 1.6)
    (when (= "callActivity" (:type node))
      (.setStroke g (stroke 3.5)) (.draw g rr))
    ;; multi-instance marker: ||| parallel, ≡ sequential
    (when-let [mi (:multi-instance node)]
      (let [cx (+ x (/ w 2.0)) by (+ y h -6)]
        (.setColor g (palette :shape-line)) (.setStroke g (stroke 1.6))
        (doseq [k [-1 0 1]]
          (if (:sequential? mi)
            (.draw g (Line2D$Double. (- cx 6) (+ by (* k 4) -4) (+ cx 6) (+ by (* k 4) -4)))
            (.draw g (Line2D$Double. (+ cx (* k 5)) (- by 10) (+ cx (* k 5)) by))))))
    ;; collapsed sub-process marker
    (when (and container? (not (:expanded? (:geom node))))
      (.setFont g (:marker-font opts))
      (draw-text-block! g "+" (+ x (/ w 2.0)) (+ y h -14) 20 :middle (palette :shape-line)))))

(defn- draw-gateway! [^Graphics2D g {:keys [x y w h]} node opts]
  (let [cx (+ x (/ w 2.0)) cy (+ y (/ h 2.0))
        p  (doto (Path2D$Double.)
             (.moveTo cx y) (.lineTo (+ x w) cy) (.lineTo cx (+ y h)) (.lineTo x cy) (.closePath))]
    (fill-and-outline! g p (palette :shape-fill) (palette :shape-line) 1.6)
    (.setFont g (:marker-font opts))
    (case (:type node)
      "exclusiveGateway"  (draw-text-block! g "X" cx cy w :middle (palette :shape-line))
      "parallelGateway"   (draw-text-block! g "+" cx cy w :middle (palette :shape-line))
      "inclusiveGateway"  (do (.setStroke g (stroke 2.0))
                              (.draw g (Ellipse2D$Double. (- cx 9) (- cy 9) 18 18)))
      "eventBasedGateway" (do (.setStroke g (stroke 1.4))
                              (.draw g (Ellipse2D$Double. (- cx 9) (- cy 9) 18 18))
                              (.draw g (Ellipse2D$Double. (- cx 6) (- cy 6) 12 12)))
      "complexGateway"    (draw-text-block! g "*" cx cy w :middle (palette :shape-line))
      nil)))

(defn- draw-event! [^Graphics2D g {:keys [x y w h]} node _opts]
  (let [outer (Ellipse2D$Double. x y w h)
        t (:type node)]
    (fill-and-outline! g outer Color/WHITE (palette :shape-line) (if (= t "endEvent") 3.5 1.6))
    (when (contains? #{"intermediateCatchEvent" "intermediateThrowEvent" "boundaryEvent"} t)
      (.setStroke g (stroke 1.6))
      (.draw g (Ellipse2D$Double. (+ x 3) (+ y 3) (- w 6) (- h 6))))
    ;; event-definition marker, drawn with primitives so no glyph font is needed
    (let [defs (:event-defs node)
          cx (+ x (/ w 2.0)) cy (+ y (/ h 2.0)) r (/ (min w h) 4.0)]
      (.setColor g (palette :shape-line)) (.setStroke g (stroke 1.4))
      (cond
        (defs "timerEventDefinition")
        (do (.draw g (Ellipse2D$Double. (- cx r) (- cy r) (* 2 r) (* 2 r)))
            (.draw g (Line2D$Double. cx cy cx (- cy (* 0.8 r))))
            (.draw g (Line2D$Double. cx cy (+ cx (* 0.6 r)) cy)))

        (defs "messageEventDefinition")
        (let [mw (* 1.6 r) mh (* 1.1 r) lx (- cx (/ mw 2)) ty (- cy (/ mh 2))]
          (.draw g (java.awt.geom.Rectangle2D$Double. lx ty mw mh))
          (.draw g (Line2D$Double. lx ty cx cy))
          (.draw g (Line2D$Double. (+ lx mw) ty cx cy)))

        (defs "terminateEventDefinition")
        (.fill g (Ellipse2D$Double. (- cx r) (- cy r) (* 2 r) (* 2 r)))

        (defs "signalEventDefinition")
        (.draw g (doto (Path2D$Double.)
                   (.moveTo cx (- cy r)) (.lineTo (+ cx r) (+ cy r)) (.lineTo (- cx r) (+ cy r)) (.closePath)))

        :else
        (when-let [letter (cond (defs "errorEventDefinition") "!"
                                (defs "escalationEventDefinition") "^"
                                (defs "conditionalEventDefinition") "?"
                                (defs "compensateEventDefinition") "<<"
                                (defs "linkEventDefinition") ">"
                                :else nil)]
          (.setFont g (Font. Font/SANS_SERIF Font/BOLD 13))
          (draw-text-block! g letter cx cy w :middle (palette :shape-line)))))))

(defn- draw-unknown! [^Graphics2D g {:keys [x y w h]}]
  (.setColor g (palette :unknown)) (.setStroke g (dashed 1.2))
  (.draw g (RoundRectangle2D$Double. x y w h 4.0 4.0)))

(defn- draw-annotation!
  "BPMN text annotation: open bracket on the left edge, wrapped text inside."
  [^Graphics2D g {:keys [x y w h]} ^String text opts]
  (.setColor g (palette :shape-line)) (.setStroke g (stroke 1.2))
  (.draw g (doto (Path2D$Double.)
             (.moveTo (+ x 10) y) (.lineTo x y) (.lineTo x (+ y h)) (.lineTo (+ x 10) (+ y h))))
  (.setFont g (:font opts))
  (let [fm (.getFontMetrics g)
        lines (wrap-text fm text (- w 12))
        lh (.getHeight fm)]
    (.setColor g (palette :text))
    (doseq [[i ^String ln] (map-indexed vector lines)]
      (.drawString g ln (int (+ x 8)) (int (+ y 4 (.getAscent fm) (* i lh)))))))

(defn- draw-association! [^Graphics2D g t geom]
  (let [pts (mapv (fn [[x y]] [((:tx t) x) ((:ty t) y)]) (:waypoints geom))]
    (.setColor g (palette :shape-line)) (.setStroke g (dashed 1.2))
    (doseq [[[ax ay] [bx by]] (partition 2 1 pts)]
      (.draw g (Line2D$Double. ax ay bx by)))))

(defn- draw-arrowhead! [^Graphics2D g [x1 y1] [x2 y2] ^Color color]
  (let [dx (- x2 x1) dy (- y2 y1) len (Math/hypot dx dy)]
    (when (pos? len)
      (let [ux (/ dx len) uy (/ dy len) size 10.0 half 4.5
            bx (- x2 (* ux size)) by (- y2 (* uy size))
            poly (doto (Polygon.)
                   (.addPoint (int (Math/round x2)) (int (Math/round y2)))
                   (.addPoint (int (Math/round (+ bx (* (- uy) half)))) (int (Math/round (+ by (* ux half)))))
                   (.addPoint (int (Math/round (- bx (* (- uy) half)))) (int (Math/round (- by (* ux half))))))]
        (.setColor g color) (.fill g poly)))))

(defn- draw-polyline! [^Graphics2D g pts ^Color color w]
  (.setColor g color) (.setStroke g (stroke w))
  (doseq [[[ax ay] [bx by]] (partition 2 1 pts)]
    (.draw g (Line2D$Double. ax ay bx by))))

(defn- draw-edge! [^Graphics2D g t geom flow opts]
  (let [pts (mapv (fn [[x y]] [((:tx t) x) ((:ty t) y)]) (:waypoints geom))]
    (when (>= (count pts) 2)
      (draw-polyline! g pts (palette :edge-line) 1.6)
      (draw-arrowhead! g (nth pts (- (count pts) 2)) (peek pts) (palette :edge-line))
      ;; default-flow slash at the source end
      (when (:default? flow)
        (let [[[x1 y1] [x2 y2]] pts
              dx (- x2 x1) dy (- y2 y1) len (max 1.0 (Math/hypot dx dy))
              ux (/ dx len) uy (/ dy len) sx (+ x1 (* ux 9)) sy (+ y1 (* uy 9))]
          (.draw g (Line2D$Double. (+ sx (* uy -5) (* ux -3)) (+ sy (* ux 5) (* uy -3))
                                   (+ sx (* uy 5) (* ux 3)) (+ sy (* ux -5) (* uy 3))))))
      ;; conditional-flow diamond at the source end
      (when (and (:conditional? flow) (not (:default? flow)))
        (let [[[x1 y1] [x2 y2]] pts
              dx (- x2 x1) dy (- y2 y1) len (max 1.0 (Math/hypot dx dy))
              ux (/ dx len) uy (/ dy len) cx (+ x1 (* ux 8)) cy (+ y1 (* uy 8))
              d (doto (Path2D$Double.)
                  (.moveTo (+ cx (* ux -8)) (+ cy (* uy -8)))
                  (.lineTo (+ cx (* uy -4)) (+ cy (* ux 4)))
                  (.lineTo (+ cx (* ux 8)) (+ cy (* uy 8)))
                  (.lineTo (+ cx (* uy 4)) (+ cy (* ux -4)))
                  (.closePath))]
          (fill-and-outline! g d Color/WHITE (palette :edge-line) 1.4)))
      (when-let [nm (:name flow)]
        (.setFont g (:font opts))
        (let [[lx ly] (if-let [lb (:label geom)]
                        [((:tx t) (+ (:x lb) (/ (:width lb) 2.0))) ((:ty t) (:y lb))]
                        (let [mid (nth pts (quot (count pts) 2))
                              prev (nth pts (max 0 (dec (quot (count pts) 2))))]
                          [(/ (+ (first mid) (first prev)) 2.0)
                           (- (/ (+ (second mid) (second prev)) 2.0) 16)]))]
          (draw-text-block! g nm lx ly 140 :top (palette :text)))))))

(defn- draw-shape! [^Graphics2D g t geom node opts]
  (let [r (px-rect t geom)]
    (if (nil? node)
      (draw-unknown! g r)
      (let [node (assoc node :geom geom)]
        (cond
          (str/ends-with? (:type node) "Gateway") (draw-gateway! g r node opts)
          (str/ends-with? (:type node) "Event")   (draw-event! g r node opts)
          :else                                   (draw-activity! g r node opts))))))

(defn- draw-label! [^Graphics2D g t geom node opts]
  (when-let [nm (:name node)]
    (let [{:keys [x y w h]} (px-rect t geom)
          type (:type node)
          cx   (+ x (/ w 2.0))]
      (.setFont g (:font opts))
      (cond
        ;; expanded sub-process: name along the top edge
        (and (:container? node) (:expanded? geom))
        (draw-text-block! g nm cx (+ y 6) (- w 16) :top (palette :text))

        ;; activities: centred inside
        (not (or (str/ends-with? type "Gateway") (str/ends-with? type "Event")))
        (draw-text-block! g nm cx (+ y (/ h 2.0)) (- w 12) :middle (palette :text))

        ;; events / gateways: at the DI label position if given, else below
        :else
        (if-let [lb (:label geom)]
          (let [{lx :x ly :y lw :w} (px-rect t lb)]
            (draw-text-block! g nm (+ lx (/ lw 2.0)) ly (max 90 lw) :top (palette :text)))
          (draw-text-block! g nm cx (+ y h 4) 120 :top (palette :text)))))))

;; ---------------------------------------------------------------------------
;; Stage 1 — base diagram
;; ---------------------------------------------------------------------------

(defn render-diagram
  "model, di, opts -> BufferedImage of the diagram, drawn from DI alone.
   Throws ex-info :type ::no-di when the file carries no diagram interchange."
  ^BufferedImage [model di opts]
  (when (empty? di)
    (throw (ex-info "BPMN file has no DI section; nothing to draw" {:type ::no-di})))
  (let [opts  (merge default-opts opts)
        t     (make-transform di opts)
        img   (BufferedImage. (:width t) (:height t) BufferedImage/TYPE_INT_ARGB)
        g     (.createGraphics img)
        nodes (:nodes model)
        flows (:flows model)
        annotations  (or (:annotations model) {})
        associations (set (map :id (:associations model)))
        shapes (di/shapes di)
        edges  (di/edges di)
        ;; draw expanded containers first so their children sit on top; larger first
        order  (sort-by (fn [[id geom]]
                          [(if (:container? (nodes id)) 0 1)
                           (- (* (:width geom) (:height geom)))])
                        shapes)]
    (try
      (enable-quality! g)
      (.setColor g (palette :background))
      (.fillRect g 0 0 (:width t) (:height t))
      (doseq [[id geom] order]
        (if-let [text (annotations id)]
          (draw-annotation! g (px-rect t geom) text opts)
          (draw-shape! g t geom (nodes id) opts)))
      (doseq [[id geom] edges]
        (cond
          (associations id) (draw-association! g t geom)
          (flows id)        (draw-edge! g t geom (flows id) opts)
          :else             (draw-association! g t geom)))
      (doseq [[id geom] order
              :when (not (annotations id))]
        (draw-label! g t geom (nodes id) opts))
      (finally (.dispose g)))
    img))

;; ---------------------------------------------------------------------------
;; Stage 2 — violation overlay
;; ---------------------------------------------------------------------------

(defn circle-for-rect
  "pixel rect -> {:cx :cy :r}"
  [{:keys [x y w h]} radius-padding]
  {:cx (+ x (/ w 2.0)) :cy (+ y (/ h 2.0))
   :r  (+ (/ (Math/hypot w h) 2.0) radius-padding)})

(defn- draw-circle! [^Graphics2D g {:keys [cx cy r]} ^Color color w]
  (.setColor g color) (.setStroke g (stroke w))
  (.draw g (Ellipse2D$Double. (- cx r) (- cy r) (* 2 r) (* 2 r))))

(defn- draw-badge!
  "High-contrast pill: coloured fill, white bold text.  Returns the pill height."
  [^Graphics2D g ^String text x y ^Color color opts]
  (.setFont g (:badge-font opts))
  (let [fm (.getFontMetrics g)
        w  (+ (.stringWidth fm text) 10)
        h  (+ (.getHeight fm) 4)]
    (.setColor g color)
    (.fill g (RoundRectangle2D$Double. x y w h 8.0 8.0))
    (.setColor g Color/WHITE)
    (.drawString g text (int (Math/round (+ x 5))) (int (Math/round (+ y 2 (.getAscent fm)))))
    h))

(defn- to-px [t pts] (mapv (fn [[x y]] [((:tx t) x) ((:ty t) y)]) pts))

(defn- translucent ^Color [^Color c alpha]
  (Color. (.getRed c) (.getGreen c) (.getBlue c) (int alpha)))

(defn- union-rect [rects]
  (let [x0 (apply min (map :x rects)) y0 (apply min (map :y rects))
        x1 (apply max (map #(+ (:x %) (:w %)) rects))
        y1 (apply max (map #(+ (:y %) (:h %)) rects))]
    {:x x0 :y y0 :w (- x1 x0) :h (- y1 y0)}))

(defn- halo!
  "Tight highlight around one shape: rounded rectangle for activities and
   containers, circle for events and gateways."
  [^Graphics2D g node {:keys [x y w h] :as r} ^Color color opts]
  (.setColor g color) (.setStroke g (stroke (:stroke opts)))
  (let [pad (:radius-padding opts)
        round? (or (nil? node)
                   (not (or (str/ends-with? (:type node) "Event")
                            (str/ends-with? (:type node) "Gateway"))))]
    (if round?
      (.draw g (RoundRectangle2D$Double. (- x pad) (- y pad) (+ w (* 2 pad)) (+ h (* 2 pad)) 14.0 14.0))
      (let [c (circle-for-rect r (/ pad 2.0))]
        (.draw g (Ellipse2D$Double. (- (:cx c) (:r c)) (- (:cy c) (:r c)) (* 2 (:r c)) (* 2 (:r c))))))))

(defn- combined-badge-text
  "One badge per element: rule ids joined, title of the most severe."
  ^String [vs]
  (let [rules (distinct (map :rule vs))
        lead  (first vs)]
    (if (= 1 (count rules))
      (badge-text (:rule lead))
      (format "[%s: %s]" (str/join "+" (map name rules))
              (get-in rules/rule-catalogue [(:rule lead) :title] "Violation")))))

(defn overlay-violations!
  "Draw every violation that has geometry onto g (pixel space via transform t).

   * :presentation :group  -> one dashed outline around the union of the
     violation's :related-bounds (a loop, a chain) and one badge at its corner.
   * shapes                -> a tight halo (rounded rect / circle) per element,
     ONE badge per element listing every rule that hit it.
   * sequence flows        -> re-stroked thick in colour along the waypoints."
  [^Graphics2D g t violations opts]
  (let [opts  (merge default-opts opts)
        rank  {:medium 0 :high 1 :critical 2}
        nodes (:nodes opts)                                   ; passed by render-report
        groups   (filter #(= :group (:presentation %)) violations)
        singles  (remove #(= :group (:presentation %)) violations)]
    ;; ---- groups first (underneath) ----------------------------------------
    (doseq [v groups
            :let [rects (keep (fn [[_ b]] (when (= :shape (:kind b)) (px-rect t b)))
                              (merge (:related-bounds v)
                                     (when (:bounds v) {(:element-id v) (:bounds v)})))]
            :when (seq rects)]
      (let [{:keys [x y w h]} (union-rect rects)
            pad   (+ 14 (:radius-padding opts))
            color (rule-color (:rule v))]
        (.setColor g (translucent color 28))
        (.fill g (RoundRectangle2D$Double. (- x pad) (- y pad) (+ w (* 2 pad)) (+ h (* 2 pad)) 18.0 18.0))
        (.setColor g color) (.setStroke g (dashed (:stroke opts)))
        (.draw g (RoundRectangle2D$Double. (- x pad) (- y pad) (+ w (* 2 pad)) (+ h (* 2 pad)) 18.0 18.0))
        (when (:badges? opts)
          (draw-badge! g (format "[%s: loop ×%d]" (name (:rule v)) (count (:related v)))
                       (- x pad) (- y pad 2 (.getHeight (.getFontMetrics g (:badge-font opts)))) color opts))))
    ;; ---- single elements ---------------------------------------------------
    (doseq [[id vs] (group-by :element-id singles)
            :let [vs   (sort-by (comp - rank :severity) vs)   ; most severe first: sets halo colour and badge lead
                  geom (:bounds (first vs))]
            :when geom]
      (let [edge? (= :edge (:kind geom))
            r     (px-rect t geom)
            color (rule-color (:rule (first vs)))
            pts   (when edge? (to-px t (:waypoints geom)))]
        (if edge?
          (do (draw-polyline! g pts (translucent color 200) (+ 3 (:stroke opts)))
              (draw-arrowhead! g (nth pts (- (count pts) 2)) (peek pts) color))
          (halo! g (get nodes id) r color opts))
        (when (:badges? opts)
          (let [[bx by] (if edge?
                          (let [[mx my] (nth pts (quot (count pts) 2))] [(+ mx 6) (- my 6)])
                          ;; above-right of the shape, outside the halo
                          [(+ (:x r) (:w r) (- (:radius-padding opts)) 2)
                           (- (:y r) (:radius-padding opts) 2 (.getHeight (.getFontMetrics g (:badge-font opts))))])]
            (draw-badge! g (combined-badge-text vs) bx by color opts)))))))

(defn hit-regions
  "Pixel rectangles that correspond to what overlay-violations! drew, so a UI
   can map a click back to a finding.  Returns [{:index i :rect {:x :y :w :h}} ..]
   where :index is the position in (:violations report).  Group outlines come
   last so a click inside a loop prefers the specific element under the cursor."
  [report opts]
  (let [opts (merge default-opts opts)
        t    (make-transform (:di report) opts)
        pad  (+ 4 (:radius-padding opts))
        grow (fn [{:keys [x y w h]} p] {:x (- x p) :y (- y p) :w (+ w (* 2 p)) :h (+ h (* 2 p))})
        vs   (map-indexed vector (:violations report))
        singles (for [[i v] vs
                      :when (and (:bounds v) (not= :group (:presentation v)))]
                  {:index i :rect (grow (px-rect t (:bounds v)) pad)})
        groups  (for [[i v] vs
                      :when (= :group (:presentation v))
                      :let [rects (keep (fn [[_ b]] (when (= :shape (:kind b)) (px-rect t b)))
                                        (merge (:related-bounds v)
                                               (when (:bounds v) {(:element-id v) (:bounds v)})))]
                      :when (seq rects)]
                  {:index i :rect (grow (union-rect rects) (+ 14 (:radius-padding opts)))})]
    (vec (concat singles groups))))

(defn hit-test
  "Index of the finding under pixel (px, py), preferring the smallest region; nil if none."
  [regions px py]
  (->> regions
       (filter (fn [{:keys [rect]}]
                 (and (<= (:x rect) px (+ (:x rect) (:w rect)))
                      (<= (:y rect) py (+ (:y rect) (:h rect))))))
       (sort-by (fn [{:keys [rect]}] (* (:w rect) (:h rect))))
       first
       :index))

(defn render-report
  "report (from bpmn-guard.core/validate), opts -> annotated BufferedImage."
  ^BufferedImage [report opts]
  (let [opts (merge default-opts opts)
        img  (render-diagram (select-keys report [:nodes :flows :annotations :associations]) (:di report) opts)
        t    (make-transform (:di report) opts)
        g    (.createGraphics img)]
    (try
      (enable-quality! g)
      (overlay-violations! g t (:violations report) (assoc opts :nodes (:nodes report)))
      (finally (.dispose g)))
    img))

;; ---------------------------------------------------------------------------
;; Legacy path: overlay onto an externally rendered PNG
;; ---------------------------------------------------------------------------

(defn read-image
  "Read an image from a File, path String or InputStream."
  ^BufferedImage [source]
  (let [img (try
              (cond
                (instance? InputStream source) (ImageIO/read ^InputStream source)
                (instance? File source)        (ImageIO/read ^File source)
                (string? source)               (ImageIO/read (io/file source))
                :else (throw (ex-info "Unsupported image source" {:type ::io-error :source (str source)})))
              (catch IOException e
                (throw (ex-info (str "Cannot read image: " (.getMessage e))
                                {:type ::io-error :source (str source)} e))))]
    (when (nil? img)
      (throw (ex-info "ImageIO could not decode image" {:type ::decode-error :source (str source)})))
    img))

(defn annotate-image
  "Overlay the report's violations on an existing base image whose pixel
   mapping is (di - origin) * scale + padding (bpmn-to-image defaults:
   origin = DI extent top-left, scale 1, padding 0)."
  ^BufferedImage [^BufferedImage base report opts]
  (let [opts (merge default-opts {:padding 0} opts)
        t    (make-transform (:di report) opts)
        out  (BufferedImage. (.getWidth base) (.getHeight base) BufferedImage/TYPE_INT_ARGB)
        g    (.createGraphics out)]
    (try
      (enable-quality! g)
      (.drawImage g base 0 0 nil)
      (overlay-violations! g t (:violations report) (assoc opts :nodes (:nodes report)))
      (finally (.dispose g)))
    out))

;; ---------------------------------------------------------------------------
;; Output
;; ---------------------------------------------------------------------------

(defn write-png
  "Write a BufferedImage as PNG to a File, path String or OutputStream."
  [^BufferedImage img target]
  (try
    (let [ok (cond
               (instance? OutputStream target) (ImageIO/write img "png" ^OutputStream target)
               (instance? File target)         (ImageIO/write img "png" ^File target)
               (string? target)                (ImageIO/write img "png" (io/file target))
               :else (throw (ex-info "Unsupported image target" {:type ::io-error :target (str target)})))]
      (when-not ok (throw (ex-info "No PNG writer available in this JVM" {:type ::io-error})))
      target)
    (catch IOException e
      (throw (ex-info (str "Cannot write PNG: " (.getMessage e))
                      {:type ::io-error :target (str target)} e)))))

(defn render-to-file
  "report, target, opts -> {:output :drawn :skipped}; draws the whole diagram
   from the .bpmn DI and overlays the violations."
  ([report target] (render-to-file report target {}))
  ([report target opts]
   (write-png (render-report report opts) target)
   (let [vs (:violations report)]
     {:output  (str target)
      :drawn   (count (filter :bounds vs))
      :skipped (mapv :element-id (remove :bounds vs))})))

(defn annotate-file
  "Legacy: external base PNG + report -> annotated PNG on disk."
  ([base-source target report] (annotate-file base-source target report {}))
  ([base-source target report opts]
   (write-png (annotate-image (read-image base-source) report opts) target)
   (let [vs (:violations report)]
     {:output  (str target)
      :drawn   (count (filter :bounds vs))
      :skipped (mapv :element-id (remove :bounds vs))})))
