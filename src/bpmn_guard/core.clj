(ns bpmn-guard.core
  "Public entry point.

     (validate \"order.bpmn\")                 ; => report map
     (validate \"order.bpmn\" {:chain-threshold 4})
     (report->json report)

   Dual-mode CLI:

     java -jar bpmn-guard.jar                           ; no args  -> Swing desktop UI
     java -jar bpmn-guard.jar order.bpmn                ; headless, text report
     java -jar bpmn-guard.jar order.bpmn out.png        ; headless, renders annotated PNG
     java -jar bpmn-guard.jar order.bpmn out.png --json ; + JSON on stdout
         [--fail-on high|critical] [--chain-threshold N] [--scale 1.5]
         [--annotate base.png out.png]   ; overlay onto an externally rendered PNG instead

   Exit codes: 0 ok · 1 high (with --fail-on high) · 2 critical · 64 usage · 65 input · 66 render."
  (:require [bpmn-guard.di :as di]
            [bpmn-guard.graph :as g]
            [bpmn-guard.model :as m]
            [bpmn-guard.render :as render]
            [bpmn-guard.rules :as rules]
            [clojure.data.json :as json]
            [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:gen-class))

(s/def ::bounds (s/nilable ::di/bounds))
(s/def ::related-bounds (s/map-of string? ::di/bounds))
(s/def ::violation
  (s/merge ::rules/violation (s/keys :req-un [::bounds ::related-bounds])))
(s/def ::violations (s/coll-of ::violation :kind vector?))
(s/def ::summary (s/map-of #{:critical :high :medium} nat-int?))
(s/def ::report
  (s/keys :req-un [::violations ::summary ::m/warnings ::m/processes ::m/nodes ::m/flows ::di/di]))

(defn- attach-bounds [di v]
  (assoc v
         :bounds         (di/bounds-for di (:element-id v))
         :related-bounds (into {} (keep (fn [id] (when-let [b (di/bounds-for di id)] [id b]))
                                        (:related v)))))

(defn validate
  "Parse, build the graph, run every rule and attach DI coordinates.
   `source` is anything clojure.java.io/reader accepts.
   Throws ex-info (:type :bpmn-guard.model/io-error | ::xml-error) on unreadable input.
   The report carries :nodes and :flows (the lowered model) so the renderer can
   draw the diagram without re-parsing the file."
  ([source] (validate source {}))
  ([source opts]
   (let [root     (m/parse-root source)
         model    (m/root->model root)
         {di :di di-warnings :warnings} (di/extract root)
         pg       (g/build model)
         vs       (mapv (partial attach-bounds di) (rules/run-all pg opts))
         no-di    (keep (fn [v] (when (nil? (:bounds v)) (:element-id v))) vs)
         warnings (vec (concat (:warnings model)
                               di-warnings
                               (when (and (seq di) (seq no-di))
                                 [(str "No DI shape for flagged element(s): " (pr-str (vec no-di)))])))
         report   {:source     (str source)
                   :processes  (:processes model)
                   :nodes      (:nodes model)
                   :flows      (:flows model)
                   :annotations  (:annotations model)
                   :associations (:associations model)
                   :violations vs
                   :summary    (merge {:critical 0 :high 0 :medium 0}
                                      (frequencies (map :severity vs)))
                   :warnings   warnings
                   :di         di}]
     (when-not (s/valid? ::report report)
       (throw (ex-info "Internal error: report failed spec"
                       {:type ::spec-error :explain (s/explain-data ::report report)})))
     report)))

;; ---------------------------------------------------------------------------
;; Output
;; ---------------------------------------------------------------------------

(defn- jsonable
  "Keywords -> strings; drop the model and DI maps (re-derivable from the file)."
  [report]
  (-> report
      (dissoc :di :nodes :flows :annotations :associations)
      (update :violations
              (fn [vs] (mapv #(-> % (update :rule name) (update :severity name)) vs)))
      (update :summary #(into {} (map (fn [[k v]] [(name k) v]) %)))))

(defn report->json
  ([report] (report->json report false))
  ([report pretty?]
   (if pretty?
     (with-out-str (json/pprint (jsonable report)))
     (json/write-str (jsonable report)))))

(defn report->text [report]
  (let [{:keys [source summary violations warnings]} report]
    (str/join
     "\n"
     (concat
      [(format "bpmn-guard: %s" source)
       (format "  critical=%d  high=%d  medium=%d" (:critical summary) (:high summary) (:medium summary))]
      (for [w warnings] (str "  warn: " w))
      [""]
      (for [v violations]
        (format "[%s %-8s] %s %s%s\n    %s\n    fix: %s"
                (name (:rule v)) (name (:severity v))
                (:element-type v) (:element-id v)
                (if-let [b (:bounds v)]
                  (format " @(%.0f,%.0f %.0fx%.0f)" (:x b) (:y b) (:width b) (:height b))
                  " @(no DI)")
                (:message v)
                (:remediation v)))))))

(defn exit-code [report fail-on]
  (let [{:keys [critical high]} (:summary report)]
    (cond
      (pos? critical) 2
      (and (= fail-on :high) (pos? high)) 1
      :else 0)))

;; ---------------------------------------------------------------------------
;; CLI
;; ---------------------------------------------------------------------------

(def ^:private usage
  (str "usage: bpmn-guard                                  launch desktop UI\n"
       "       bpmn-guard <file.bpmn> [out.png] [--json] [--fail-on high|critical]\n"
       "                  [--chain-threshold N] [--scale S] [--annotate base.png out.png]"))

(defn- parse-args [args]
  (loop [[a & more] args, acc {:fail-on :critical :positional []}]
    (cond
      (nil? a)                  acc
      (= a "--json")            (recur more (assoc acc :json? true))
      (= a "--annotate")        (recur (nnext more) (assoc acc :annotate [(first more) (second more)]))
      (= a "--fail-on")         (recur (next more) (assoc acc :fail-on (keyword (first more))))
      (= a "--chain-threshold") (recur (next more) (assoc acc :chain-threshold (Long/parseLong (first more))))
      (= a "--scale")           (recur (next more) (assoc acc :scale (Double/parseDouble (first more))))
      (str/starts-with? a "--") (throw (ex-info (str "Unknown option " a) {:type ::usage}))
      :else                     (recur more (update acc :positional conj a)))))

(defn- die! [code & msg]
  (binding [*out* *err*] (apply println msg))
  (System/exit code))

(defn run-headless
  "CI entry: validate `file`, optionally render `out-png`, print a report.
   Returns the exit code instead of exiting (so it is testable)."
  [{:keys [file out-png json? annotate fail-on chain-threshold scale]}]
  (let [report (validate file (cond-> {} chain-threshold (assoc :chain-threshold chain-threshold)))]
    (println (if json? (report->json report true) (report->text report)))
    (when out-png
      (if (empty? (:di report))
        (binding [*out* *err*] (println "warn: no DI in file, PNG not written"))
        (let [{:keys [drawn skipped]} (render/render-to-file report out-png (cond-> {} scale (assoc :scale scale)))]
          (binding [*out* *err*]
            (println (format "rendered %s: %d violation(s) drawn%s" out-png drawn
                             (if (seq skipped) (str ", without DI: " (pr-str skipped)) "")))))))
    (when-let [[base out] annotate]
      (let [{:keys [drawn]} (render/annotate-file base out report (cond-> {} scale (assoc :scale scale)))]
        (binding [*out* *err*] (println (format "annotated %s: %d violation(s) drawn" out drawn)))))
    (exit-code report fail-on)))

(defn -main [& args]
  (let [{:keys [positional] :as cfg}
        (try (parse-args args)
             (catch clojure.lang.ExceptionInfo e (die! 64 (.getMessage e) "\n" usage)))]
    (if (and (empty? positional) (nil? (:annotate cfg)))
      ;; interactive mode — resolved at runtime so the headless path never loads Swing
      (let [launch (requiring-resolve 'bpmn-guard.gui/launch!)]
        (launch)
        nil)
      (let [[file out-png & extra] positional]
        (when (seq extra) (die! 64 "too many arguments\n" usage))
        (when-not file (die! 64 usage))
        (let [code (try
                     (run-headless (assoc cfg :file file :out-png out-png))
                     (catch clojure.lang.ExceptionInfo e
                       (die! (if (= :bpmn-guard.render/io-error (:type (ex-data e))) 66 65)
                             "error:" (.getMessage e))))]
          (shutdown-agents)
          (System/exit code))))))
