(ns build
  "clj -T:build uber   => target/bpmn-guard.jar (java -jar target/bpmn-guard.jar file.bpmn)"
  (:require [clojure.tools.build.api :as b]))

(def class-dir "target/classes")
(def uber-file "target/bpmn-guard.jar")
(def basis (delay (b/create-basis {:project "deps.edn"})))

(defn clean [_] (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (b/copy-dir {:src-dirs ["src" "resources"] :target-dir class-dir})
  (b/compile-clj {:basis @basis :ns-compile '[bpmn-guard.core] :class-dir class-dir})
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis @basis
           :main 'bpmn-guard.core}))
