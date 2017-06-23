(ns kr.rdf4j.test-sparql
  (use clojure.test
       [kr.rdf4j.test-kb :exclude [test-ns-hook]])
  (require kr.core.test-kb
           kr.core.test-sparql))

;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------

(defn test-ns-hook []
  (binding [kr.core.test-kb/*kb-creator-fn*
            rdf4j-memory-test-kb]
    (run-tests 'kr.core.test-sparql)))

;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------
