(ns bpmn-guard.graph
  "Layer 2 — directed graph over JGraphT.

   Vertices are flow-node ids (String).  Edges are JGraphT DefaultEdge
   objects; since BPMN allows parallel edges (two conditional flows A->B) and
   self loops, the underlying structure is a DirectedPseudograph.

   Each edge is mapped back to the sequence flow it came from in :edge->flow.
   Boundary events get a *synthetic* edge host -> boundaryEvent so that
   boundary-driven retry loops participate in cycle detection; synthetic edges
   are marked :synthetic :boundary."
  (:require [bpmn-guard.model :as m])
  (:import [org.jgrapht Graph Graphs]
           [org.jgrapht.graph AsSubgraph DefaultEdge DirectedPseudograph]
           [org.jgrapht.alg.connectivity KosarajuStrongConnectivityInspector]
           [org.jgrapht.alg.cycle CycleDetector]
           [org.jgrapht.traverse TopologicalOrderIterator]
           [java.util Set]))

(defrecord ProcessGraph [^Graph graph edge->flow model])

(defn build
  "model -> ProcessGraph"
  ^ProcessGraph [model]
  (let [g (DirectedPseudograph. DefaultEdge)
        nodes (:nodes model)]
    (doseq [id (keys nodes)] (.addVertex g id))
    (let [flow-edges
          (into {}
                (for [f (vals (:flows model))
                      :let [^DefaultEdge e (.addEdge g (:source f) (:target f))]
                      :when e]
                  [e f]))
          boundary-edges
          (into {}
                (for [n (vals nodes)
                      :when (and (= "boundaryEvent" (:type n))
                                 (contains? nodes (:attached-to n)))
                      :let [^DefaultEdge e (.addEdge g (:attached-to n) (:id n))]
                      :when e]
                  [e {:id        (str (:attached-to n) "~>" (:id n))
                      :source    (:attached-to n)
                      :target    (:id n)
                      :scope     (:scope n)
                      :synthetic :boundary
                      :conditional? false
                      :default? false}]))]
      (->ProcessGraph g (merge flow-edges boundary-edges) model))))

;; ---------------------------------------------------------------------------
;; Primitive accessors
;; ---------------------------------------------------------------------------

(defn node [^ProcessGraph pg id] (get-in pg [:model :nodes id]))

(defn successors [^ProcessGraph pg id]
  (vec (Graphs/successorListOf ^Graph (:graph pg) id)))

(defn predecessors [^ProcessGraph pg id]
  (vec (Graphs/predecessorListOf ^Graph (:graph pg) id)))

(defn out-edges [^ProcessGraph pg id]
  (vec (.outgoingEdgesOf ^Graph (:graph pg) id)))

(defn in-edges [^ProcessGraph pg id]
  (vec (.incomingEdgesOf ^Graph (:graph pg) id)))

(defn edge-source [^ProcessGraph pg ^DefaultEdge e] (.getEdgeSource ^Graph (:graph pg) e))
(defn edge-target [^ProcessGraph pg ^DefaultEdge e] (.getEdgeTarget ^Graph (:graph pg) e))
(defn edge-flow   [^ProcessGraph pg ^DefaultEdge e] (get (:edge->flow pg) e))

(defn out-degree [^ProcessGraph pg id] (.outDegreeOf ^Graph (:graph pg) id))
(defn in-degree  [^ProcessGraph pg id] (.inDegreeOf  ^Graph (:graph pg) id))

;; ---------------------------------------------------------------------------
;; Traversal
;; ---------------------------------------------------------------------------

(defn reachable-from
  "Set of vertices reachable from `start` (inclusive), never expanding any
   vertex in `blocked`.  Plain BFS; O(V+E)."
  ([pg start] (reachable-from pg start #{}))
  ([pg start blocked]
   (loop [frontier [start] seen #{start}]
     (if (empty? frontier)
       seen
       (let [next (->> frontier
                       (remove blocked)
                       (mapcat #(successors pg %))
                       (remove seen)
                       distinct)]
         (recur (vec next) (into seen next)))))))

(defn subgraph
  "AsSubgraph view restricted to `vertex-ids` and, optionally, `edges`."
  (^Graph [^ProcessGraph pg vertex-ids]
   (AsSubgraph. ^Graph (:graph pg) ^Set (set vertex-ids)))
  (^Graph [^ProcessGraph pg vertex-ids edges]
   (AsSubgraph. ^Graph (:graph pg) ^Set (set vertex-ids) ^Set (set edges))))

(defn strongly-connected-sets
  "Kosaraju/Tarjan-style SCC decomposition of a (sub)graph.
   Returns only non-trivial components: size > 1 or a self loop."
  [^Graph g]
  (let [insp (KosarajuStrongConnectivityInspector. g)]
    (->> (.stronglyConnectedSets insp)
         (map set)
         (filter (fn [vs]
                   (or (> (count vs) 1)
                       (let [v (first vs)]
                         (.containsEdge g v v)))))
         vec)))

(defn simple-cycles
  "Simple-cycle enumeration on `g` (bounded DFS; exponential in the worst case,
   so callers pass `max-cycles`; returns {:cycles [[v ..] ..] :truncated? bool}."
  [^Graph g max-cycles]
  ;; JGraphT 1.5.2's JohnsonSimpleCycles throws IntrusiveEdgeException while
  ;; building its working subgraphs (even on a fresh DefaultDirectedGraph), so
  ;; cycles are enumerated here with a bounded DFS over a vertex-only adjacency
  ;; map.  Each simple cycle is reported once, rooted at its lowest-ordered
  ;; vertex (the same canonical form Johnson's algorithm uses).  The search
  ;; stops after `max-cycles` results or `step-budget` edge visits.
  (let [step-budget 200000
        adj   (into {}
                    (for [v (.vertexSet g)]
                      [v (vec (distinct (map #(.getEdgeTarget g %) (.outgoingEdgesOf g v))))]))
        verts (vec (sort (keys adj)))
        idx   (zipmap verts (range))
        found (volatile! [])
        steps (volatile! 0)
        stop? (volatile! false)]
    (letfn [(dfs [start v path on-path]
              (doseq [w (adj v)
                      :while (not @stop?)]
                (vswap! steps inc)
                (cond
                  (> @steps step-budget)
                  (vreset! stop? true)

                  (= w start)
                  (if (< (count @found) max-cycles)
                    (vswap! found conj path)
                    (vreset! stop? true))

                  (and (> (idx w) (idx start)) (not (on-path w)))
                  (dfs start w (conj path w) (conj on-path w)))))]
      (doseq [s verts :while (not @stop?)]
        (dfs s s [s] #{s})))
    {:cycles @found :truncated? @stop?}))

(defn acyclic? [^Graph g]
  (not (.detectCycles (CycleDetector. g))))

(defn topological-order [^Graph g]
  (vec (iterator-seq (TopologicalOrderIterator. g))))
