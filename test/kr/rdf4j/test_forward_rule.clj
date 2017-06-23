
(ns kr.rdf4j.test-forward-rule
  (use clojure.test
       [kr.rdf4j.test-kb :exclude [test-ns-hook]])
  (require kr.core.test-kb
           kr.core.test-forward-rule))

(defn test-ns-hook []
    (binding [kr.core.test-kb/*kb-creator-fn*
            rdf4j-memory-test-kb]
      (run-tests 'kr.core.test-forward-rule)))
