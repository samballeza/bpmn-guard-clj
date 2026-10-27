(ns bpmn-guard.di
  "BPMN Diagram Interchange (DI) extraction.

   Reads <bpmndi:BPMNShape> and <bpmndi:BPMNEdge> and produces one geometry
   entry per element id:

     shape: {:kind :shape :x :y :width :height :expanded? :label {..}|nil}
     edge:  {:kind :edge  :x :y :width :height :waypoints [[x y] ..]}

   Edges carry their full waypoint polyline (for the base-diagram renderer)
   and the bounding box of those waypoints (for the violation overlay).

   Missing DI is NOT an error — the model is still fully analysable — it is
   reported as a warning and every violation gets :bounds nil."
  (:require [bpmn-guard.model :as m]
            [clojure.spec.alpha :as s]))

(s/def ::x number?)
(s/def ::y number?)
(s/def ::width (s/and number? #(>= % 0)))
(s/def ::height (s/and number? #(>= % 0)))
(s/def ::kind #{:shape :edge})
(s/def ::expanded? boolean?)
(s/def ::point (s/tuple number? number?))
(s/def ::waypoints (s/coll-of ::point :kind vector? :min-count 1))
(s/def ::label (s/nilable (s/keys :req-un [::x ::y ::width ::height])))
(s/def ::bounds
  (s/keys :req-un [::x ::y ::width ::height ::kind]
          :opt-un [::expanded? ::waypoints ::label]))
(s/def ::di (s/map-of string? ::bounds))

(defn- parse-double* [^String v]
  (when v
    (try (Double/parseDouble v)
         (catch NumberFormatException _ nil))))

(defn- rect-of [bounds-el]
  (let [x (parse-double* (m/attr bounds-el "x"))
        y (parse-double* (m/attr bounds-el "y"))
        w (parse-double* (m/attr bounds-el "width"))
        h (parse-double* (m/attr bounds-el "height"))]
    (when (and x y w h) {:x x :y y :width w :height h})))

(defn- shape-geometry [shape-el]
  (when-let [r (some-> (first (m/children-named shape-el "Bounds")) rect-of)]
    (let [label (some-> (first (m/children-named shape-el "BPMNLabel"))
                        (m/children-named "Bounds") first rect-of)]
      (assoc r
             :kind :shape
             :expanded? (= "true" (m/attr shape-el "isExpanded"))
             :label label))))

(defn- edge-geometry [edge-el]
  (let [pts (vec (keep (fn [wp]
                         (let [x (parse-double* (m/attr wp "x"))
                               y (parse-double* (m/attr wp "y"))]
                           (when (and x y) [x y])))
                       (m/children-named edge-el "waypoint")))]
    (when (seq pts)
      (let [xs (map first pts) ys (map second pts)
            x0 (apply min xs) y0 (apply min ys)
            x1 (apply max xs) y1 (apply max ys)
            label (some-> (first (m/children-named edge-el "BPMNLabel"))
                          (m/children-named "Bounds") first rect-of)]
        {:kind :edge :x x0 :y y0 :width (- x1 x0) :height (- y1 y0)
         :waypoints pts :label label}))))

(defn extract
  "root -> {:di {element-id geometry} :warnings [..]}"
  [root]
  (let [diagrams (m/children-named root "BPMNDiagram")
        planes   (mapcat #(m/children-named % "BPMNPlane") diagrams)
        all-els  (mapcat #(tree-seq m/element? m/children %) planes)
        shapes   (filter #(= "BPMNShape" (m/local-name (:tag %))) all-els)
        edges    (filter #(= "BPMNEdge"  (m/local-name (:tag %))) all-els)
        entries  (concat
                  (for [sh shapes
                        :let [id (m/attr sh "bpmnElement") g (shape-geometry sh)]
                        :when (and id g)]
                    [id g])
                  (for [e edges
                        :let [id (m/attr e "bpmnElement") g (edge-geometry e)]
                        :when (and id g)]
                    [id g]))
        di       (into {} entries)
        warnings (cond-> []
                   (empty? diagrams)
                   (conj "No <bpmndi:BPMNDiagram> found — violations will carry no coordinates and no diagram can be drawn")
                   (and (seq diagrams) (empty? di))
                   (conj "BPMNDiagram present but contains no parseable shapes"))]
    {:di di :warnings warnings}))

(defn bounds-for
  "Geometry of an element id, or nil when the diagram has no shape for it."
  [di element-id]
  (get di element-id))

(defn shapes [di] (into {} (filter #(= :shape (:kind (val %))) di)))
(defn edges  [di] (into {} (filter #(= :edge  (:kind (val %))) di)))

(defn extent
  "Bounding box of everything in the diagram (shapes, waypoints and labels):
   {:min-x :min-y :max-x :max-y}.  nil when there is no DI."
  [di]
  (when (seq di)
    (let [rects (concat (vals di) (keep :label (vals di)))
          xs (mapcat (fn [r] [(:x r) (+ (:x r) (:width r))]) rects)
          ys (mapcat (fn [r] [(:y r) (+ (:y r) (:height r))]) rects)]
      {:min-x (apply min xs) :min-y (apply min ys)
       :max-x (apply max xs) :max-y (apply max ys)})))

(defn diagram-origin
  "Top-left corner of the union of all shapes.  Returns [0 0] when there is no DI."
  [di]
  (if-let [{:keys [min-x min-y]} (extent di)]
    [min-x min-y]
    [0.0 0.0]))
