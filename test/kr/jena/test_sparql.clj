(ns kr.jena.test-sparql
  (use clojure.test
       [kr.jena.test-kb :exclude [test-ns-hook]])
  (require kr.core.test-kb
           kr.core.test-sparql))

;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------

(defn test-ns-hook []
  (binding [kr.core.test-kb/*kb-creator-fn*
            jena-memory-test-kb
            kr.jena.rdf/*force-add-named-to-default*
            true]
    (run-tests 'kr.core.test-sparql)
    ))

;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------