(ns bpmn-guard.rules
  "Layer 3 — the deterministic rule set.

   Every rule is a pure function  ProcessGraph, opts -> [Violation].
   A Violation is a plain map (spec ::violation).  Rules never consult DI;
   coordinates are attached afterwards by bpmn-guard.core.

     :R1  sequential multi-instance body writes global variables / loops back
     :R2  scope-boundary bypass & orphaned tokens
     :R3  synchronous cycle (no wait state, no async continuation)
     :R4  XOR/OR split converging on an AND join (deterministic deadlock)
     :R5  parallel multi-instance without asyncBefore
     :R6  unbroken chain of >= N synchronous compute tasks
     :R7  event-based gateway targeting a non-catching node (engine fault)"
  (:require [bpmn-guard.graph :as g]
            [bpmn-guard.model :as m]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.string :as str])
  (:import [org.jgrapht Graph]))

;; ---------------------------------------------------------------------------
;; Violation spec
;; ---------------------------------------------------------------------------

(s/def ::rule #{:R1 :R2 :R3 :R4 :R5 :R6 :R7})
(s/def ::remediation string?)
(s/def ::presentation #{:element :group})
(s/def ::kind keyword?)                                   ; finer label within a rule, e.g. :summary :writer :loop
(s/def ::severity #{:critical :high :medium})
(s/def ::element-id string?)
(s/def ::element-type (s/nilable string?))
(s/def ::element-name (s/nilable string?))
(s/def ::scope (s/nilable string?))
(s/def ::message string?)
(s/def ::related (s/coll-of string? :kind vector?))
(s/def ::evidence map?)
(s/def ::violation
  (s/keys :req-un [::rule ::severity ::element-id ::element-type ::message ::related ::evidence
                   ::remediation]
          :opt-un [::element-name ::scope ::presentation ::kind]))

;; ---------------------------------------------------------------------------
;; Rule catalogue — shared by the renderer (badge text / colour class) and the GUI
;; ---------------------------------------------------------------------------

(def rule-catalogue
  {:R1 {:title "Seq MI Var Leak"
        :class :oom
        :remediation (str "Keep per-iteration values out of the process-instance scope: (1) in connector/"
                          "listener scripts use execution.setVariableLocal, and have connector output "
                          "scripts return only the fields you need instead of S(response); (2) add an "
                          "end executionListener on the multi-instance body that removes payload "
                          "variables (execution.removeVariable) once consumed; (3) reset retry counters "
                          "at the start of each iteration; (4) lower history level to ACTIVITY or exclude "
                          "these variables via a HistoryLevel/HistoryEventProducer filter so retries do "
                          "not add ACT_HI_DETAIL rows; (5) if the loop is only a retry, prefer the job "
                          "executor's retry cycle (camunda:failedJobRetryTimeCycle) over a modelled loop.")}
   :R2 {:title "Scope Bypass"
        :class :deadlock
        :remediation (str "Route the token through the sub-process end event (or an error/escalation "
                          "end event caught by a boundary event); never cross a scope with a plain "
                          "sequence flow; give every non-end node an outgoing flow.")}
   :R3 {:title "Sync Loop"
        :class :oom
        :remediation (str "Put camunda:asyncBefore=\"true\" on the first node of the loop (or a timer "
                          "intermediate catch event) so each iteration commits and releases the thread; "
                          "bound retries with a counter variable on the gateway condition.")}
   :R4 {:title "XOR→AND Deadlock"
        :class :deadlock
        :remediation (str "Join exclusive branches with an exclusiveGateway (or inclusiveGateway for "
                          "OR splits); only join with a parallelGateway what a parallelGateway split.")}
   :R5 {:title "Parallel MI Burst"
        :class :oom
        :remediation (str "Set camunda:asyncBefore=\"true\" (and usually asyncAfter) on the multi-instance "
                          "activity so instances are created by the job executor in separate "
                          "transactions; consider isSequential=\"true\" or batching the collection.")}
   :R6 {:title "Long Sync Chain"
        :class :starvation
        :remediation (str "Insert camunda:asyncBefore=\"true\" every 2–3 service tasks (or on the "
                          "slowest remote call) so the chain spans several short transactions.")}
   :R7 {:title "Bad EBG Target"
        :class :deadlock
        :remediation (str "An eventBasedGateway may only lead to intermediateCatchEvents (message, "
                          "timer, signal, conditional) or receiveTasks; move any task/gateway behind "
                          "the catching event.")}})

(defn remediation-for [rule] (get-in rule-catalogue [rule :remediation] ""))

(def default-opts
  {:chain-threshold 5          ; Rule 6
   :max-scc-enumeration 20     ; Rule 3: enumerate simple cycles only for SCCs up to this size
   :max-cycles-per-scc 25})

;; ---------------------------------------------------------------------------
;; Helpers
;; ---------------------------------------------------------------------------

(defn- violation
  [pg rule severity element-id message & {:keys [related evidence]
                                          :or   {related [] evidence {}}}]
  (let [n (g/node pg element-id)
        f (get-in pg [:model :flows element-id])]
    {:rule         rule
     :severity     severity
     :element-id   element-id
     :element-type (or (:type n) (when f "sequenceFlow"))
     :element-name (or (:name n) (:name f))
     :scope        (or (:scope n) (:scope f))
     :message      message
     :remediation  (remediation-for rule)
     :related      (vec (distinct related))
     :evidence     evidence}))

(defn- all-nodes [pg] (vals (get-in pg [:model :nodes])))
(defn- all-flows [pg] (vals (get-in pg [:model :flows])))

(defn- label [pg id]
  (let [n (g/node pg id)]
    (if-let [nm (:name n)] (format "'%s' (%s)" nm id) id)))

(defn- descendant-ids
  "Ids of every node transitively contained in container `cid`."
  [pg cid]
  (let [by-scope (group-by :scope (all-nodes pg))]
    (loop [todo [cid] acc #{}]
      (if (empty? todo)
        acc
        (let [kids (map :id (get by-scope (first todo)))]
          (recur (into (subvec todo 1) (filter #(:container? (g/node pg %)) kids))
                 (into acc kids)))))))

;; ---------------------------------------------------------------------------
;; Rule 1 — sequential multi-instance body: variable accumulation
;;
;; Produces, per sequential multi-instance container:
;;   * ONE summary finding on the container itself (critical when API-payload
;;     sized values are written), with the per-iteration write count, the
;;     loop multipliers and the resulting write formula;
;;   * one finding per writing activity listing exactly which variables it
;;     writes, by which mechanism (outputParameter, connector output, script in
;;     a listener / mapping), and whether the value is payload-sized;
;;   * one GROUP finding per inner cycle (drawn as a single outline around the
;;     cycle, not a badge on every node) stating the back-edge condition, whether
;;     the loop looks bounded, and which writes it re-executes.
;; ---------------------------------------------------------------------------

(defn- fmt-write [w]
  (format "%s%s (%s)"
          (:variable w)
          (if (:payload? w) "*" "")
          (:where w)))

(defn- counter-bound
  "If any back-edge condition compares a variable against a number with < or <=,
   return that condition — the loop is bounded by a counter.  nil otherwise."
  [conditions]
  (some (fn [c] (when (and c (re-find #"[<>]=?\s*\d+|\d+\s*[<>]=?" c)) c)) conditions))

(defn- cycle-order
  "Walk the cycle from an anchor following in-SCC successors so the message
   reads as a path rather than a sorted set."
  [pg scc anchor]
  (loop [v anchor, out [anchor], seen #{anchor}]
    (let [nxt (first (remove seen (filter scc (g/successors pg v))))]
      (if nxt (recur nxt (conj out nxt) (conj seen nxt)) out))))

(defn rule-1 [pg _opts]
  (vec
   (mapcat
    (fn [sp]
      (let [mi        (:multi-instance sp)
            body-ids  (descendant-ids pg (:id sp))
            body      (map #(g/node pg %) body-ids)
            writers   (filter #(seq (:variable-writes %)) body)
            all-writes (mapcat (fn [n] (map #(assoc % :node (:id n)) (:variable-writes n))) writers)
            payloads  (filter :payload? all-writes)
            sccs      (g/strongly-connected-sets (g/subgraph pg body-ids))
            ;; per cycle: back-edge conditions, wait states, writes it re-executes
            loops     (for [scc sccs
                            :let [in-scc?   (set scc)
                                  anchor    (or (first (sort-by (fn [id] (- (count (:variable-writes (g/node pg id))))) (filter #(seq (:variable-writes (g/node pg %))) scc)))
                                                (first (sort scc)))
                                  path      (cycle-order pg in-scc? anchor)
                                  cflows    (filter #(and (in-scc? (:source %)) (in-scc? (:target %)) (:conditional? %))
                                                    (all-flows pg))
                                  conds     (keep :condition cflows)
                                  bound     (counter-bound conds)
                                  waits     (filter #(m/wait-state? (g/node pg %)) scc)
                                  re-writes (filter #(in-scc? (:node %)) all-writes)]]
                        {:scc (vec (sort scc)) :path path :anchor anchor :conditions (vec conds)
                         :bound bound :waits (vec (sort waits)) :re-writes (vec re-writes)})
            n-writes  (count all-writes)
            loop-term (str/join " + " (for [l loops] (format "retries×%d" (count (:re-writes l)))))
            formula   (if (seq loops)
                        (format "|%s| × (%d%s)" (or (:collection mi) "collection") n-writes
                                (if (str/blank? loop-term) "" (str " + " loop-term)))
                        (format "|%s| × %d" (or (:collection mi) "collection") n-writes))]
        (when (or (seq writers) (seq loops))
          (concat
           ;; --- summary on the container -------------------------------------
           [(assoc (violation pg :R1 (if (seq payloads) :critical :high) (:id sp)
                       (str (format "Sequential multi-instance %s iterates over '%s'%s. "
                                    (label pg (:id sp)) (or (:collection mi) "?")
                                    (if-let [ev (:element-variable mi)] (format " (element variable '%s')" ev) ""))
                            (if (seq writers)
                              (format "Each iteration performs %d non-local variable write(s) from %d activit%s: %s. "
                                      n-writes (count writers) (if (= 1 (count writers)) "y" "ies")
                                      (str/join ", " (map fmt-write all-writes)))
                              "The body performs no statically visible non-local variable writes. ")
                            (when (seq payloads)
                              (format "%d of them (marked *) carry API responses or request bodies, which are serialised into ACT_GE_BYTEARRAY on every write. "
                                      (count payloads)))
                            (when (seq loops)
                              (str/join " "
                                        (for [l loops]
                                          (format "An inner cycle %s%s re-executes %d of these writes per pass%s. "
                                                  (pr-str (:path l))
                                                  (if (:bound l) (format " bounded by `%s`" (:bound l)) " with NO counter-based bound visible")
                                                  (count (:re-writes l))
                                                  (if (seq (:waits l)) (format " (it commits at %s, so it is not a thread lock)" (pr-str (:waits l))) "")))))
                            (format "Writes per process instance ≈ %s; every write is also a history row (ACT_HI_VARINST / ACT_HI_DETAIL at FULL history level). "
                                    formula)
                            "Variables written with execution.setVariable that do not already exist locally land on the process instance and survive all iterations.")
                       :related (into (vec (map :id writers)) (mapcat :scc loops))
                       :evidence {:collection (:collection mi)
                                  :element-variable (:element-variable mi)
                                  :writes (vec all-writes)
                                  :payload-writes (vec payloads)
                                  :loops (vec loops)
                                  :formula formula})
                   :kind :summary)]
           ;; --- one finding per writing activity -------------------------------
           (for [n writers
                 :let [ws (:variable-writes n)
                       in-loop (first (filter #(contains? (set (:scc %)) (:id n)) loops))]]
             (assoc (violation pg :R1 (if (some :payload? ws) :critical :high) (:id n)
                        (format (str "%s writes %d non-local variable(s) on every iteration of %s: %s.%s%s")
                                (label pg (:id n)) (count ws) (label pg (:id sp))
                                (str/join ", " (map fmt-write ws))
                                (if (some :payload? ws) " Starred values are API payloads." "")
                                (if in-loop
                                  (format " This activity also sits on the inner cycle %s, so these writes repeat on every retry."
                                          (pr-str (:path in-loop)))
                                  ""))
                        :related [(:id sp)]
                        :evidence {:loop (:id sp) :writes ws})
                    :kind :writer))
           ;; --- one GROUP finding per inner cycle ------------------------------
           (for [l loops]
             (assoc (violation pg :R1 :high (:anchor l)
                               (format (str "Inner cycle %s inside sequential multi-instance %s%s. "
                                            "Each pass re-executes %d variable write(s)%s; the effective iteration "
                                            "count is collection size × passes.%s")
                                       (pr-str (:path l)) (label pg (:id sp))
                                       (if (:bound l)
                                         (format " is bounded by `%s`" (:bound l))
                                         " has no counter-based bound in any back-edge condition")
                                       (count (:re-writes l))
                                       (if (seq (:re-writes l))
                                         (str ": " (str/join ", " (map fmt-write (:re-writes l))))
                                         "")
                                       (if (seq (:waits l))
                                         (format " The cycle commits at %s, so it is a variable/history multiplier, not a thread lock." (pr-str (:waits l)))
                                         " The cycle has no wait state — see R3."))
                               :related (:scc l)
                               :evidence {:loop (:id sp) :cycle (:path l) :conditions (:conditions l)
                                          :bound (:bound l) :re-writes (:re-writes l)})
                    :presentation :group
                    :kind :loop))))))
    (filter #(and (:container? %) (get-in % [:multi-instance :sequential?])) (all-nodes pg)))))

;; ---------------------------------------------------------------------------
;; Rule 2 — boundary scope bypass & orphaned tokens
;; ---------------------------------------------------------------------------

(defn rule-2 [pg _opts]
  (let [nodes (get-in pg [:model :nodes])
        ;; (a) sequence flow crossing a scope boundary
        cross (for [f (all-flows pg)
                    :let [src (nodes (:source f)) tgt (nodes (:target f))]
                    :when (not= (:scope src) (:scope tgt))]
                (violation pg :R2 :critical (:id f)
                           (format (str "sequenceFlow %s crosses from scope '%s' into scope '%s' "
                                        "(%s -> %s). A token leaving a sub-process through a plain "
                                        "sequence flow never completes the sub-process scope execution; "
                                        "the engine leaves it active in ACT_RU_EXECUTION.")
                                   (:id f) (:scope src) (:scope tgt)
                                   (label pg (:source f)) (label pg (:target f)))
                           :related [(:source f) (:target f)]
                           :evidence {:source-scope (:scope src) :target-scope (:scope tgt)}))
        ;; (b) boundary event declared in a different container than its host
        misattached (for [n (all-nodes pg)
                          :when (= "boundaryEvent" (:type n))
                          :let [host (nodes (:attached-to n))]
                          :when (and host (not= (:scope host) (:scope n)))]
                      (violation pg :R2 :critical (:id n)
                                 (format "boundaryEvent %s is attached to %s but declared in scope '%s' instead of '%s'."
                                         (:id n) (label pg (:id host)) (:scope n) (:scope host))
                                 :related [(:id host)]
                                 :evidence {:attached-to (:id host)}))
        ;; (c) boundary event path re-entering the body of the activity it interrupts
        reentry (for [n (all-nodes pg)
                      :when (and (= "boundaryEvent" (:type n)) (nodes (:attached-to n)))
                      :let [host-id (:attached-to n)
                            inside  (descendant-ids pg host-id)
                            reach   (disj (g/reachable-from pg (:id n) #{host-id}) (:id n))
                            hit     (sort (filter inside reach))]
                      :when (seq hit)]
                  (violation pg :R2 :critical (:id n)
                             (format (str "boundaryEvent %s on %s routes back into the host's body (%s) "
                                          "without passing through the host — the body scope is re-entered "
                                          "while its original scope execution has been cancelled/left behind.")
                                     (:id n) (label pg host-id) (pr-str (vec hit)))
                             :related (into [host-id] hit)
                             :evidence {:host host-id :re-entered (vec hit)}))
        ;; (d) dead-end: a non-terminal node with no outgoing flow silently drops its token
        dead-ends (for [n (all-nodes pg)
                        :when (and (zero? (g/out-degree pg (:id n)))
                                   (not= "endEvent" (:type n))
                                   (not (:event-subprocess? n))
                                   ;; compensation handlers legitimately have no outgoing flow
                                   (not (contains? (:event-defs n) "compensateEventDefinition"))
                                   ;; a boundary event with no outgoing flow is a tolerated BPMN idiom
                                   (not= "boundaryEvent" (:type n)))]
                    (violation pg :R2 :high (:id n)
                               (format (str "%s has no outgoing sequence flow and is not an end event. "
                                            "Its token ends silently; if a sibling branch is still active "
                                            "the enclosing scope '%s' can never complete.")
                                       (label pg (:id n)) (:scope n))
                               :evidence {:out-degree 0}))]
    (vec (concat cross misattached reentry dead-ends))))

;; ---------------------------------------------------------------------------
;; Rule 3 — synchronous infinite loops
;;
;; Exact and polynomial: delete every vertex that is a wait state or carries an
;; async continuation, then any strongly connected component left in the
;; residual graph is, by construction, a cycle with zero wait states and zero
;; save points.  A bounded cycle DFS is then run *per component* (bounded) only
;; to give the user concrete cycle paths.
;; ---------------------------------------------------------------------------

(defn rule-3 [pg opts]
  (let [{:keys [max-scc-enumeration max-cycles-per-scc]} (merge default-opts opts)
        residual-ids (->> (all-nodes pg)
                          (remove m/wait-state?)
                          (remove m/async-boundary?)
                          (map :id))
        residual     (g/subgraph pg residual-ids)
        sccs         (g/strongly-connected-sets residual)]
    (vec
     (for [scc sccs
           :let [ordered (vec (sort scc))
                 {:keys [cycles truncated?]}
                 (if (<= (count scc) max-scc-enumeration)
                   (g/simple-cycles (g/subgraph pg scc) max-cycles-per-scc)
                   {:cycles [] :truncated? true})]
           id ordered]
       (violation pg :R3 :critical id
                  (format (str "%s lies on a synchronous cycle %s that contains no wait state "
                               "(userTask/receiveTask/catch event/external task) and no "
                               "camunda:asyncBefore/asyncAfter. The engine will spin inside one "
                               "thread and one transaction until StackOverflowError or OOM.")
                          (label pg id) (pr-str ordered))
                  :related ordered
                  :evidence {:scc ordered
                             :sample-cycles cycles
                             :cycles-truncated? truncated?})))))

;; ---------------------------------------------------------------------------
;; Rule 4 — XOR / OR split converging on an AND join
;; ---------------------------------------------------------------------------

(def ^:private split-types #{"exclusiveGateway" "inclusiveGateway"})

(defn rule-4 [pg _opts]
  (vec
   (for [split (all-nodes pg)
         :when (and (split-types (:type split)) (> (g/out-degree pg (:id split)) 1))
         :let [split-id   (:id split)
               branches   (g/out-edges pg split-id)
               ;; nodes reachable down each branch, never passing back through the split
               reach      (into {} (for [b branches]
                                     [b (g/reachable-from pg (g/edge-target pg b) #{split-id})]))
               cone       (apply set/union (vals reach))]
         join  (all-nodes pg)
         :when (and (= "parallelGateway" (:type join))
                    (> (g/in-degree pg (:id join)) 1)
                    (contains? cone (:id join)))
         :let [join-id   (:id join)
               ins       (g/in-edges pg join-id)
               feeds     (fn [b] (set (filter #(contains? (reach b) (g/edge-source pg %)) ins)))
               per-branch (map (fn [b] [b (feeds b)]) branches)
               feeding   (filter (comp seq second) per-branch)
               full?     (fn [fs] (= (count fs) (count ins)))
               n-feeding (count feeding)
               bypassing (- (count branches) n-feeding)]
         :when (and (pos? n-feeding)
                    (not (every? #(full? (second %)) feeding)))
         :let [xor?     (= "exclusiveGateway" (:type split))
               severity (cond
                          (and xor? (>= n-feeding 2))       :critical
                          (or xor? (>= n-feeding 2))        :high
                          :else                             :medium)
               branch-desc (str/join ", "
                                     (for [[b fs] feeding]
                                       (format "%s→%d/%d inputs"
                                               (:id (g/edge-flow pg b)) (count fs) (count ins))))]]
     (violation pg :R4 severity join-id
                (format (str "parallelGateway %s joins %d incoming flows but is fed by %s %s "
                             "whose branches each activate only a subset (%s)%s. The AND join waits "
                             "for every incoming flow, so with a %s choice at least one token "
                             "waits forever: deterministic deadlock.")
                        (label pg join-id) (count ins)
                        (:type split) (label pg split-id)
                        branch-desc
                        (if (pos? bypassing)
                          (format ", and %d branch(es) bypass the join entirely" bypassing)
                          "")
                        (if xor? "single-branch" "partial"))
                :related (into [split-id] (map (comp :id #(g/edge-flow pg %) first) feeding))
                :evidence {:split split-id
                           :split-type (:type split)
                           :join-inputs (count ins)
                           :branch-coverage (into {} (for [[b fs] per-branch]
                                                       [(:id (g/edge-flow pg b))
                                                        (mapv #(:id (g/edge-flow pg %)) fs)]))}))))

;; ---------------------------------------------------------------------------
;; Rule 5 — parallel multi-instance without an async save point
;; ---------------------------------------------------------------------------

(defn rule-5 [pg _opts]
  (vec
   (for [n (all-nodes pg)
         :let [mi (:multi-instance n)]
         :when (and mi (not (:sequential? mi)) (not (:async-before? n)))]
     (violation pg :R5 :critical (:id n)
                (format (str "%s is a parallel multi-instance %s without camunda:asyncBefore=\"true\". "
                             "All instances over %s are created in the caller's thread and transaction; "
                             "a large collection exhausts heap and the JDBC transaction in one shot.")
                        (label pg (:id n)) (:type n)
                        (if-let [c (:collection mi)] (str "collection '" c "'") "the collection"))
                :evidence {:multi-instance mi}))))

;; ---------------------------------------------------------------------------
;; Rule 6 — unbroken synchronous compute chains
;;
;; Build the "chain graph": compute tasks only, keeping an edge u->v only if
;; neither u's asyncAfter nor v's asyncBefore commits in between.  If the chain
;; graph is acyclic, a single longest-path DP in topological order gives every
;; maximal chain in O(V+E).  A cyclic chain graph is unbounded by definition.
;; ---------------------------------------------------------------------------

(defn- chain-graph [pg]
  (let [compute (set (map :id (filter m/compute-task? (all-nodes pg))))
        edges   (for [id compute
                      e  (g/out-edges pg id)
                      :let [tgt (g/edge-target pg e)
                            f   (g/edge-flow pg e)]
                      :when (and (compute tgt)
                                 (nil? (:synthetic f))
                                 (not (:async-after? (g/node pg id)))
                                 (not (:async-before? (g/node pg tgt))))]
                  e)]
    [compute (g/subgraph pg compute edges)]))

(defn rule-6 [pg opts]
  (let [{:keys [chain-threshold]} (merge default-opts opts)
        [compute ^Graph cg] (chain-graph pg)
        mk (fn [chain severity extra]
             (violation pg :R6 severity (last chain)
                        (format (str "Chain of %d consecutive synchronous compute tasks %s with no "
                                     "camunda:asyncBefore save point. The whole chain executes in one "
                                     "JDBC transaction holding its connection and row locks; exceeds "
                                     "transaction/lock timeouts under load.%s")
                                (count chain) (pr-str chain) extra)
                        :related chain
                        :evidence {:chain chain :length (count chain)}))]
    (if (empty? compute)
      []
      (if-not (g/acyclic? cg)
        ;; unbounded chain — report each SCC once (Rule 3 reports the thread-lock aspect)
        (vec (for [scc (g/strongly-connected-sets cg)]
               (mk (vec (sort scc)) :critical
                   " Chain is cyclic: unbounded length.")))
        (let [order (g/topological-order cg)
              dist  (reduce (fn [acc v]
                              (let [preds (seq (.incomingEdgesOf cg v))
                                    best  (when preds
                                            (apply max-key #(get-in acc [% :len])
                                                   (map #(.getEdgeSource cg %) preds)))]
                                (assoc acc v (if best
                                               {:len (inc (get-in acc [best :len])) :prev best}
                                               {:len 1 :prev nil}))))
                            {}
                            order)
              chain-of (fn [v] (vec (reverse (take-while some? (iterate #(get-in dist [% :prev]) v)))))
              ends  (filter #(and (>= (get-in dist [% :len]) chain-threshold)
                                  (zero? (.outDegreeOf cg %)))
                            order)
              chains (distinct (map chain-of ends))]
          (vec (for [c chains] (mk c :high ""))))))))

;; ---------------------------------------------------------------------------
;; Driver
;; ---------------------------------------------------------------------------

(def ^:private valid-ebg-target-types #{"intermediateCatchEvent" "receiveTask"})

(def ^:private valid-ebg-event-defs
  #{"messageEventDefinition" "timerEventDefinition" "signalEventDefinition" "conditionalEventDefinition"})

(defn rule-7 [pg _opts]
  (vec
   (for [ebg (all-nodes pg)
         :when (= "eventBasedGateway" (:type ebg))
         e (g/out-edges pg (:id ebg))
         :let [tgt-id (g/edge-target pg e)
               tgt    (g/node pg tgt-id)
               flow   (g/edge-flow pg e)
               ok?    (and (valid-ebg-target-types (:type tgt))
                           (or (= "receiveTask" (:type tgt))
                               (some valid-ebg-event-defs (:event-defs tgt))))]
         :when (not ok?)]
     (violation pg :R7 :critical tgt-id
                (format (str "eventBasedGateway %s leads directly to %s (%s) via %s. The engine "
                             "requires every target of an event-based gateway to be a catching "
                             "intermediate event or a receiveTask; deployment is rejected or the "
                             "gateway can never fire.")
                        (label pg (:id ebg)) (label pg tgt-id) (:type tgt) (:id flow))
                :related [(:id ebg) (:id flow)]
                :evidence {:gateway (:id ebg) :flow (:id flow) :target-type (:type tgt)
                           :target-event-defs (vec (:event-defs tgt))}))))

;; ---------------------------------------------------------------------------
;; Driver
;; ---------------------------------------------------------------------------

(def all-rules [rule-1 rule-2 rule-3 rule-4 rule-5 rule-6 rule-7])

(defn run-all
  "ProcessGraph, opts -> [Violation], spec-checked, sorted by severity then id."
  ([pg] (run-all pg {}))
  ([pg opts]
   (let [sev-rank {:critical 0 :high 1 :medium 2}
         vs (->> all-rules
                 (mapcat #(% pg opts))
                 (sort-by (juxt (comp sev-rank :severity) :rule :element-id))
                 vec)]
     (doseq [v vs]
       (when-not (s/valid? ::violation v)
         (throw (ex-info "Rule produced an invalid violation"
                         {:type ::spec-error :explain (s/explain-data ::violation v)}))))
     vs)))
