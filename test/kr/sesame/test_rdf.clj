(ns kr.sesame.test-rdf
  (:require [clojure.test :refer [run-tests]]
            [kr.core.test-kb :refer [*kb-creator-fn*]]
            kr.core.test-rdf
            [kr.sesame.test-kb :refer [sesame-memory-test-kb]]))

(def sesame-kb-creator-fn sesame-memory-test-kb)

(defn test-ns-hook []
  (binding [*kb-creator-fn* sesame-kb-creator-fn]
    (run-tests 'kr.core.test-rdf)))