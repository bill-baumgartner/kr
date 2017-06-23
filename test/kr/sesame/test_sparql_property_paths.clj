(ns kr.sesame.test-sparql-property-paths
  (use clojure.test
       [kr.sesame.test-kb :exclude [test-ns-hook]])
  (require kr.core.test-kb
           kr.core.test-sparql-property-paths))

;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------

(defn test-ns-hook []
  (binding [kr.core.test-kb/*kb-creator-fn*
            sesame-memory-test-kb]
    (run-tests 'kr.core.test-sparql-property-paths)))

;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------