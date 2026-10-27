(ns bpmn-guard.model
  "Layer 1 — XML parsing.

   Parses a Camunda BPMN 2.0 file with clojure.data.xml and lowers it into a
   flat, namespace-agnostic model of plain Clojure maps:

     {:processes  [{:id .. :name .. :executable? ..}]
      :nodes      {node-id FlowNode}
      :flows      {flow-id SequenceFlow}
      :di         {element-id Bounds}      ; see bpmn-guard.di
      :warnings   [..]}

   Everything downstream (graph, rules, rendering) consumes this model only;
   nothing else touches the raw XML tree."
  (:require [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:import [java.io IOException InputStream Reader]))

;; ---------------------------------------------------------------------------
;; Specs
;; ---------------------------------------------------------------------------

(s/def ::id string?)
(s/def ::type string?)                                     ; BPMN local-name, e.g. "serviceTask"
(s/def ::name (s/nilable string?))
(s/def ::scope ::id)                                       ; id of the enclosing process/subProcess
(s/def ::async-before? boolean?)
(s/def ::async-after? boolean?)
(s/def ::exclusive? boolean?)
(s/def ::sequential? boolean?)
(s/def ::collection (s/nilable string?))
(s/def ::multi-instance (s/nilable (s/keys :req-un [::sequential?] :opt-un [::collection])))
(s/def ::event-defs (s/coll-of string? :kind set?))
(s/def ::attached-to (s/nilable ::id))
(s/def ::cancel-activity? boolean?)
(s/def ::variable-writes (s/coll-of map? :kind vector?))
(s/def ::condition (s/nilable string?))
(s/def ::annotations (s/map-of string? string?))
(s/def ::associations (s/coll-of map? :kind vector?))
(s/def ::container? boolean?)

(s/def ::node
  (s/keys :req-un [::id ::type ::scope ::async-before? ::async-after?
                   ::event-defs ::variable-writes ::container?]
          :opt-un [::name ::multi-instance ::attached-to ::cancel-activity? ::exclusive?]))

(s/def ::source ::id)
(s/def ::target ::id)
(s/def ::conditional? boolean?)
(s/def ::default? boolean?)
(s/def ::flow
  (s/keys :req-un [::id ::source ::target ::scope ::conditional? ::default?]
          :opt-un [::name ::condition]))

(s/def ::executable? boolean?)
(s/def ::process (s/keys :req-un [::id ::executable?] :opt-un [::name]))

(s/def ::nodes (s/map-of ::id ::node))
(s/def ::flows (s/map-of ::id ::flow))
(s/def ::processes (s/coll-of ::process :kind vector?))
(s/def ::warnings (s/coll-of string? :kind vector?))
(s/def ::model (s/keys :req-un [::processes ::nodes ::flows ::warnings]
                       :opt-un [::di ::annotations ::associations]))

;; ---------------------------------------------------------------------------
;; Namespace-agnostic XML helpers
;;
;; clojure.data.xml encodes namespaced tags/attributes as keywords whose
;; namespace is the percent-encoded URI (e.g. :xmlns.http%3A%2F%2F...MODEL/process)
;; and whose *name* is the local name.  Matching on (name k) lets the engine
;; accept any prefix (bpmn:, bpmn2:, semantic:, none) and any Camunda prefix.
;; ---------------------------------------------------------------------------

(def ^:private camunda-ns "camunda.org/schema")

(defn element? [x] (and (map? x) (keyword? (:tag x))))
(defn local-name ^String [k] (name k))

(defn attr
  "Attribute value by local name, ignoring namespace prefix."
  [el ^String local]
  (some (fn [[k v]] (when (= (local-name k) local) v)) (:attrs el)))

(defn- camunda-attr
  "Attribute value by local name, restricted to the Camunda namespace
   (or an unqualified attribute of that name, for leniency with hand-edited XML)."
  [el ^String local]
  (some (fn [[k v]]
          (let [ns-str (some-> (namespace k) (str/replace "xmlns." "")
                               (java.net.URLDecoder/decode "UTF-8"))]
            (when (and (= (local-name k) local)
                       (or (nil? ns-str) (str/includes? ns-str camunda-ns)))
              v)))
        (:attrs el)))

(defn children [el] (filter element? (:content el)))

(defn children-named [el ^String local]
  (filter #(= (local-name (:tag %)) local) (children el)))

(defn- text-of [el]
  (apply str (filter string? (:content el))))

(defn- bool-attr [el local] (= "true" (some-> (attr el local) str/trim)))

(defn- element-id
  "BPMN requires ids, but a hand-written file may omit one; fall back to a
   deterministic synthetic id so the model still validates."
  [el]
  (or (attr el "id")
      (str "anon-" (local-name (:tag el)) "-" (Integer/toHexString (hash el)))))
(defn- camunda-bool [el local] (= "true" (some-> (camunda-attr el local) str/trim)))

;; ---------------------------------------------------------------------------
;; BPMN vocabulary
;; ---------------------------------------------------------------------------

(def container-types #{"process" "subProcess" "transaction" "adHocSubProcess"})

(def flow-node-types
  #{"startEvent" "endEvent" "intermediateCatchEvent" "intermediateThrowEvent" "boundaryEvent"
    "task" "userTask" "serviceTask" "scriptTask" "businessRuleTask" "sendTask" "receiveTask"
    "manualTask" "callActivity" "subProcess" "transaction" "adHocSubProcess"
    "exclusiveGateway" "inclusiveGateway" "parallelGateway" "eventBasedGateway" "complexGateway"})

(def activity-types
  #{"task" "userTask" "serviceTask" "scriptTask" "businessRuleTask" "sendTask" "receiveTask"
    "manualTask" "callActivity" "subProcess" "transaction" "adHocSubProcess"})

;; ---------------------------------------------------------------------------
;; Variable-write detection (used by Rule 1)
;;
;; A "write" is any statically visible way an element puts a value into a
;; NON-local variable scope:
;;   * camunda:resultVariable
;;   * camunda:outputParameter   (plain inputOutput and connector inputOutput)
;;   * execution.setVariable / connector.setVariable / delegateExecution.setVariable
;;     in ANY script that belongs to the element: <bpmn:script>, camunda:script in
;;     listeners, input/output parameters and connector mappings.
;; setVariableLocal, camunda:inputParameter and removeVariable are not writes.
;; Each write is annotated with :payload? when the value is, by name or by
;; expression, an API response / JSON / request body — those are the writes
;; that fill ACT_GE_BYTEARRAY.
;; ---------------------------------------------------------------------------

(def ^:private set-variable-re
  #"\b(?:execution|connector|delegateExecution|runtimeService|task)\s*\.\s*setVariable\s*\(\s*[\"']([^\"']+)[\"']")

(def ^:private set-variable-local-re
  #"\bsetVariableLocal\s*\(\s*[\"']([^\"']+)[\"']")

(def ^:private payload-re
  #"(?i)response|payload|request|json|spin|\bS\s*\(|body|result|record|list|projection")

(defn- excerpt
  "First meaningful line of an expression or script, capped at 120 chars —
   evidence should identify the source text, not reproduce it."
  [^String text]
  (when text
    (let [line (->> (str/split-lines text)
                    (map str/trim)
                    (remove #(or (str/blank? %) (str/starts-with? % "import ") (str/starts-with? % "//")))
                    first)
          line (or line "")]
      (if (> (count line) 120) (str (subs line 0 117) "...") line))))

(defn- payload-like? [& texts]
  (boolean (some #(and % (re-find payload-re %)) texts)))

(defn- all-descendants [el]
  (rest (tree-seq element? children el)))

(defn- script-variable-writes [script-text where]
  (let [globals (map second (re-seq set-variable-re script-text))
        locals  (set (map second (re-seq set-variable-local-re script-text)))]
    (->> globals
         (remove locals)
         distinct
         (map (fn [v]
                ;; the RHS of this particular setVariable call, for the payload heuristic
                (let [rhs (second (re-find (re-pattern (str "setVariable\\s*\\(\\s*[\"']" (java.util.regex.Pattern/quote v)
                                                            "[\"']\\s*,\\s*([^\\n;]{0,120})"))
                                           script-text))]
                  {:kind :script-set-variable :variable v :where where
                   :payload? (payload-like? v rhs)})))
         vec)))

(defn- script-location
  "Human-readable location of a camunda:script inside an element's extensions."
  [path]
  (let [names (map #(local-name (:tag %)) path)]
    (cond
      (some #{"executionListener"} names) (str "executionListener/" (or (some #(attr % "event") path) "?"))
      (some #{"taskListener"} names)      "taskListener"
      (some #{"connector"} names)         (if (some #{"outputParameter"} names)
                                            (str "connector outputParameter '" (some #(attr % "name") (reverse path)) "'")
                                            (str "connector inputParameter '" (some #(attr % "name") (reverse path)) "'"))
      (some #{"outputParameter"} names)   (str "outputParameter '" (some #(attr % "name") (reverse path)) "'")
      (some #{"inputParameter"} names)    (str "inputParameter '" (some #(attr % "name") (reverse path)) "'")
      :else                               "script")))

(defn- scripts-with-path
  "All <script> descendants of the element's extensionElements plus the
   element's own <bpmn:script>, each with the path of ancestor elements."
  [el]
  (letfn [(walk [node path]
            (let [path' (conj path node)]
              (concat (when (= "script" (local-name (:tag node))) [[node path']])
                      (mapcat #(walk % path') (children node)))))]
    (concat (for [sc (children-named el "script")] [sc [el sc]])
            (mapcat #(walk % [el]) (children-named el "extensionElements")))))

(defn- variable-writes [el]
  (let [result-var (camunda-attr el "resultVariable")
        ext-els    (children-named el "extensionElements")
        out-params (for [ext ext-els
                         out (filter #(= "outputParameter" (local-name (:tag %))) (all-descendants ext))
                         :let [n (attr out "name")
                               inside-connector? (boolean
                                                  (some (fn [c] (some #(identical? % out) (all-descendants c)))
                                                        (filter #(= "connector" (local-name (:tag %)))
                                                                (all-descendants ext))))
                               expr (str/trim (text-of out))
                               nested-script (first (children-named out "script"))]
                         :when n]
                     {:kind     (if inside-connector? :connector-output :output-parameter)
                      :variable n
                      :where    (if inside-connector? "connector outputParameter" "outputParameter")
                      :expression (excerpt (if nested-script (text-of nested-script) expr))
                      :payload? (payload-like? n expr (when nested-script (text-of nested-script)))})
        scripts    (for [[sc path] (scripts-with-path el)
                         w (script-variable-writes (text-of sc) (script-location path))]
                     w)]
    (vec (concat (when result-var
                   [{:kind :result-variable :variable result-var :where "camunda:resultVariable"
                     :payload? (payload-like? result-var)}])
                 out-params
                 scripts))))

;; ---------------------------------------------------------------------------
;; Text annotations & associations (rendering only)
;; ---------------------------------------------------------------------------

(defn- annotations-in [container-el]
  (into {} (for [a (filter #(= "textAnnotation" (local-name (:tag %))) (all-descendants container-el))
                 :let [t (first (children-named a "text"))]]
             [(element-id a) (str/trim (if t (text-of t) ""))])))

(defn- associations-in [container-el]
  (vec (for [a (filter #(= "association" (local-name (:tag %))) (all-descendants container-el))]
         {:id (element-id a) :source (attr a "sourceRef") :target (attr a "targetRef")})))

;; ---------------------------------------------------------------------------
;; Flow nodes & sequence flows
;; ---------------------------------------------------------------------------

(defn- multi-instance [el]
  (when-let [mi (first (children-named el "multiInstanceLoopCharacteristics"))]
    {:sequential?      (bool-attr mi "isSequential")
     :collection       (camunda-attr mi "collection")
     :element-variable (camunda-attr mi "elementVariable")
     :async-before?    (camunda-bool mi "asyncBefore")
     :async-after?     (camunda-bool mi "asyncAfter")}))

(defn- event-definitions [el]
  (->> (children el)
       (map #(local-name (:tag %)))
       (filter #(str/ends-with? % "EventDefinition"))
       set))

(defn- ->node [el scope-id]
  (let [type (local-name (:tag el))]
    (cond-> {:id              (element-id el)
             :type            type
             :name            (attr el "name")
             :scope           scope-id
             :async-before?   (camunda-bool el "asyncBefore")
             :async-after?    (camunda-bool el "asyncAfter")
             :exclusive?      (not= "false" (camunda-attr el "exclusive"))
             :event-defs      (event-definitions el)
             :multi-instance  (multi-instance el)
             :variable-writes (variable-writes el)
             :container?      (contains? container-types type)
             :event-subprocess? (bool-attr el "triggeredByEvent")
             :implementation  (camunda-attr el "type")}      ; "external" ⇒ wait state
      (= type "boundaryEvent")
      (assoc :attached-to     (attr el "attachedToRef")
             ;; BPMN default for cancelActivity is true
             :cancel-activity? (not= "false" (attr el "cancelActivity"))))))

(defn- ->flow [el scope-id]
  {:id           (element-id el)
   :name         (attr el "name")
   :source       (attr el "sourceRef")
   :target       (attr el "targetRef")
   :scope        scope-id
   :conditional? (boolean (seq (children-named el "conditionExpression")))
   :condition    (some-> (first (children-named el "conditionExpression")) text-of str/trim
                         (str/replace #"\s+" " "))
   :default?     false})                                     ; patched in below

(defn- walk-container
  "Recursively collect nodes and flows from a process / subProcess element.
   Returns {:nodes [..] :flows [..]}."
  [container-el]
  (let [scope-id (element-id container-el)
        kids     (children container-el)
        nodes    (->> kids
                      (filter #(contains? flow-node-types (local-name (:tag %))))
                      (map #(->node % scope-id)))
        flows    (->> (children-named container-el "sequenceFlow")
                      (map #(->flow % scope-id)))
        ;; default flows are declared on the gateway/activity, not the flow
        defaults (->> kids
                      (keep #(attr % "default"))
                      set)
        flows    (map #(assoc % :default? (contains? defaults (:id %))) flows)
        nested   (->> kids
                      (filter #(contains? container-types (local-name (:tag %))))
                      (map walk-container))]
    {:nodes (vec (concat nodes (mapcat :nodes nested)))
     :flows (vec (concat flows (mapcat :flows nested)))}))

;; ---------------------------------------------------------------------------
;; Public API
;; ---------------------------------------------------------------------------

(defn- index-by [k coll]
  (into {} (map (juxt k identity)) coll))

(defn- check-duplicates [coll label]
  (let [dups (->> coll (map :id) frequencies (filter #(> (val %) 1)) (map key))]
    (mapv #(format "Duplicate %s id '%s'" label %) dups)))

(defn parse-root
  "Parse XML from anything clojure.java.io/reader accepts (File, path String,
   InputStream, Reader, URL).  Throws ex-info with :type ::io-error or ::xml-error."
  [source]
  (try
    (with-open [^Reader r (io/reader source)]
      (let [root (xml/parse r :namespace-aware true :include-node? #{:element :characters})]
        ;; clojure.data.xml parses lazily; realise the whole tree while the
        ;; reader is still open so that I/O and well-formedness errors surface
        ;; here, under this try, and never later inside a rule.
        (dorun (tree-seq element? :content root))
        root))
    (catch IOException e
      (throw (ex-info (str "Cannot read BPMN source: " (.getMessage e))
                      {:type ::io-error :source (str source)} e)))
    (catch javax.xml.stream.XMLStreamException e
      (throw (ex-info (str "Malformed BPMN XML: " (.getMessage e))
                      {:type ::xml-error :source (str source)} e)))
    (catch RuntimeException e
      ;; data.xml may surface parser failures wrapped; keep the contract uniform
      (if (instance? javax.xml.stream.XMLStreamException (.getCause e))
        (throw (ex-info (str "Malformed BPMN XML: " (.getMessage (.getCause e)))
                        {:type ::xml-error :source (str source)} e))
        (throw e)))))

(defn root->model
  "Lower a parsed XML root (a <definitions> element) into the flat model."
  [root]
  (when-not (= "definitions" (local-name (:tag root)))
    (throw (ex-info "Root element is not <definitions>; is this a BPMN 2.0 file?"
                    {:type ::xml-error :root-tag (:tag root)})))
  (let [process-els (children-named root "process")
        processes   (mapv (fn [p] {:id          (element-id p)
                                   :name        (attr p "name")
                                   :executable? (bool-attr p "isExecutable")})
                          process-els)
        walked      (map walk-container process-els)
        nodes       (vec (mapcat :nodes walked))
        flows       (vec (mapcat :flows walked))
        node-index  (index-by :id nodes)
        dangling    (for [f flows
                          :let [missing (remove node-index [(:source f) (:target f)])]
                          :when (seq missing)]
                      (format "sequenceFlow '%s' references unknown node(s) %s"
                              (:id f) (pr-str (vec missing))))
        warnings    (vec (concat (when (empty? process-els) ["No <process> elements found"])
                                 (check-duplicates nodes "flow node")
                                 (check-duplicates flows "sequenceFlow")
                                 dangling))
        model       {:processes processes
                     :annotations  (into {} (map annotations-in process-els))
                     :associations (vec (mapcat associations-in process-els))
                     :nodes     node-index
                     :flows     (index-by :id (filter #(and (node-index (:source %))
                                                            (node-index (:target %)))
                                                      flows))
                     :warnings  warnings}]
    (when-not (s/valid? ::model model)
      (throw (ex-info "Internal error: lowered model failed spec"
                      {:type ::spec-error :explain (s/explain-data ::model model)})))
    model))

(defn parse
  "Parse a BPMN file/stream directly into the model."
  [source]
  (root->model (parse-root source)))

;; Convenience predicates shared by the rules layer --------------------------

(defn activity? [node] (contains? activity-types (:type node)))

(defn compute-task?
  "Synchronous, CPU/DB-bound task types that run inside the caller's transaction."
  [node]
  (and (contains? #{"serviceTask" "scriptTask" "businessRuleTask" "sendTask"} (:type node))
       ;; an External Task (camunda:type="external") is a wait state, not compute
       (not= "external" (:implementation node))))

(defn wait-state?
  "True when the engine commits and releases the thread at this node."
  [node]
  (let [t (:type node)
        defs (:event-defs node)
        catching-def? (some defs #{"timerEventDefinition" "messageEventDefinition"
                                   "signalEventDefinition" "conditionalEventDefinition"})]
    (boolean
     (or (contains? #{"userTask" "receiveTask" "intermediateCatchEvent" "eventBasedGateway"} t)
         (and (= t "boundaryEvent") catching-def?)
         (= "external" (:implementation node))
         ;; a timer start event or an async-continuation makes the engine commit
         (:async-before? node) (:async-after? node)))))

(defn async-boundary? [node]
  (or (:async-before? node) (:async-after? node)))
