(ns bpmn-guard.rules-test
  (:require [bpmn-guard.core :as core]
            [bpmn-guard.render :as render]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]])
  (:import [java.awt.image BufferedImage]
           [java.io File]))

(def sample (io/resource "samples/risky-order.bpmn"))

(defmacro with-out-str* [& body]
  `(binding [*out* (java.io.StringWriter.)] ~@body))

(defn- ids-for [report rule]
  (->> (:violations report) (filter #(= rule (:rule %))) (map :element-id) set))

(deftest sample-triggers-every-rule
  (let [report (core/validate sample)]
    (testing "R1: summary on the sequential MI container plus one finding per writer"
      (is (= #{"PerItem" "SumItem"} (ids-for report :R1)))
      (let [summary (first (filter #(and (= :R1 (:rule %)) (= "PerItem" (:element-id %))) (:violations report)))]
        (is (= "items" (get-in summary [:evidence :collection])))
        (is (= ["runningTotal"] (mapv :variable (get-in summary [:evidence :writes]))))
        (is (.contains ^String (:message summary) "|items|"))))
    (testing "R2 cross-scope sequence flow"
      (is (contains? (ids-for report :R2) "f17")))
    (testing "R3 synchronous retry loop"
      (is (= #{"CallPricing" "PricingOk"} (ids-for report :R3))))
    (testing "R4 XOR split into AND join"
      (is (= #{"JoinApproval"} (ids-for report :R4)))
      (is (= :critical (->> (:violations report) (filter #(= :R4 (:rule %))) first :severity))))
    (testing "R5 parallel MI without asyncBefore"
      (is (= #{"NotifyPartners"} (ids-for report :R5))))
    (testing "R6 chain of >= 5 compute tasks"
      (let [v (first (filter #(= :R6 (:rule %)) (:violations report)))]
        (is (some? v))
        (is (>= (get-in v [:evidence :length]) 5))
        (is (= ["Enrich1" "Enrich2" "Enrich3" "Enrich4" "Enrich5" "NotifyPartners"]
               (get-in v [:evidence :chain])))))
    (testing "R7 event-based gateway into a serviceTask"
      (is (= #{"Poll"} (ids-for report :R7))))
    (testing "every violation carries remediation advice"
      (is (every? (comp seq :remediation) (:violations report))))
    (testing "DI bounds are attached, including edge waypoints"
      (let [by-id (into {} (map (juxt :element-id identity) (:violations report)))
            b     (:bounds (by-id "NotifyPartners"))]
        (is (= [1520.0 160.0 100.0 80.0 :shape] ((juxt :x :y :width :height :kind) b)))
        (is (= :edge (get-in by-id ["f17" :bounds :kind])))
        (is (= 4 (count (get-in by-id ["f17" :bounds :waypoints]))))))
    (testing "summary and exit code"
      (is (pos? (get-in report [:summary :critical])))
      (is (= 2 (core/exit-code report :critical))))))

(deftest fixes-silence-rules
  (testing "asyncBefore on the loop breaks R3; a userTask does too"
    (let [xml (slurp sample)
          fixed (.replace xml "<bpmn:serviceTask id=\"CallPricing\" name=\"Call pricing API\""
                          "<bpmn:serviceTask id=\"CallPricing\" name=\"Call pricing API\" camunda:asyncBefore=\"true\"")
          report (core/validate (java.io.StringReader. fixed))]
      (is (empty? (ids-for report :R3)))))
  (testing "asyncBefore on parallel MI silences R5"
    (let [xml (slurp sample)
          fixed (.replace xml "<bpmn:serviceTask id=\"NotifyPartners\" name=\"Notify partners\""
                          "<bpmn:serviceTask id=\"NotifyPartners\" name=\"Notify partners\" camunda:asyncBefore=\"true\"")
          report (core/validate (java.io.StringReader. fixed))]
      (is (empty? (ids-for report :R5))))))

(deftest missing-di-is-a-warning-not-an-error
  (let [xml   (slurp sample)
        no-di (subs xml 0 (.indexOf xml "<bpmndi:BPMNDiagram"))
        no-di (str no-di "</bpmn:definitions>")
        report (core/validate (java.io.StringReader. no-di))]
    (is (seq (:violations report)))
    (is (every? nil? (map :bounds (:violations report))))
    (is (some #(.contains ^String % "BPMNDiagram") (:warnings report)))))

(deftest io-errors-are-ex-info
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Cannot read"
                        (core/validate "/definitely/not/here.bpmn")))
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Malformed"
                        (core/validate (java.io.StringReader. "<definitions><process>")))))

(deftest render-draws-diagram-and-overlay
  (let [report (core/validate sample)
        out    (File/createTempFile "bpmn-guard" ".png")
        result (render/render-to-file report out {:scale 1.0 :padding 40})
        img    (render/read-image out)
        {:keys [tx ty]} (render/make-transform (:di report) (merge render/default-opts {:padding 40}))
        rgb    (fn [x y] (let [p (.getRGB img (int x) (int y))]
                           [(bit-and (bit-shift-right p 16) 0xFF)
                            (bit-and (bit-shift-right p 8) 0xFF)
                            (bit-and p 0xFF)]))]
    (is (pos? (:drawn result)))
    (is (> (.length out) 0))
    (testing "canvas is sized from the DI extent plus padding"
      (is (> (.getWidth img) 2300))
      (is (= 40.0 (tx 100.0))))     ; min DI x is 100 (Start shape)
    (testing "background is white and a task body is light grey"
      (is (= [255 255 255] (rgb 5 5)))
      (is (= [242 242 242] (rgb (tx 1540.0) (ty 170.0)))))   ; inside NotifyPartners, above its label
    (testing "NotifyPartners (R5 critical + R6 high) gets a RED halo along its bottom edge"
      ;; the halo is a rounded rectangle 6px outside the shape (:radius-padding), 2.5px stroke;
      ;; scan a few rows around the edge so antialiasing cannot make this flaky
      (let [edge-y (+ (ty 240.0) 6)
            px     (for [dy (range -3 4)] (rgb (tx 1570.0) (+ edge-y dy)))]
        (is (some (fn [[r g b]] (and (> r 180) (< g 90) (< b 90))) px)
            (str "no red pixel near the halo edge; saw " (pr-str (vec px))))))
    (testing "a task that only has R6 (amber class) would not be red; the halo of Enrich5 is not drawn red"
      ;; Enrich5 is part of the R6 chain but is not the flagged element (the chain's last node is),
      ;; so its bottom edge row must stay the plain shape outline, not a red halo
      (let [px (for [dy (range -3 4)] (rgb (tx 1440.0) (+ (ty 240.0) 6 dy)))]
        (is (not-any? (fn [[r g b]] (and (> r 180) (< g 90) (< b 90))) px))))))

(deftest headless-cli-returns-exit-code
  (let [out (File/createTempFile "cli" ".png")
        code (with-out-str* (core/run-headless {:file (str (io/file sample))
                                                :out-png (.getPath out)
                                                :fail-on :critical}))]
    (is (= 2 code))
    (is (> (.length out) 0))))
