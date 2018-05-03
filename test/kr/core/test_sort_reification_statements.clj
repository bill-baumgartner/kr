(ns kr.core.test-sort-reification-statements
  (use clojure.test
       kr.core.test-kb

       kr.core.variable
       kr.core.kb
       kr.core.rdf
       kr.core.sparql
       kr.core.rule
       kr.core.forward-rule

       clojure.pprint
       ))



;; todo - reify ordering doesn't seem to matter here, but does in the kabob project???
(def rule-8 '{:head ((?/hacker ex/inDept    ?/dept)
                     (?/dept ex/deptID    ?/deptid)
                     (?/dept   rdf/type     ex/Department))
              :body ((?/hacker ex/hasBoss   ?/boss)
                     (?/hacker ex/atCompany ?/co))
              :reify ([?/dept {:ln (:sha-1 ?/boss ?/co)
                               :ns "ex" :prefix "DEPT_"}]
                      [?/deptid {:ln (:sha-1 ?/dept ?/co)
                                 :ns "ex" :prefix "DEPT_"}])
              })

(def rule-8-inv '{:head ((?/hacker ex/inDept    ?/dept)
                         (?/dept ex/deptID    ?/deptid)
                         (?/dept   rdf/type     ex/Department))
                  :body ((?/hacker ex/hasBoss   ?/boss)
                         (?/hacker ex/atCompany ?/co))
                  :reify ([?/deptid {:ln (:sha-1 ?/dept ?/co)
                                     :ns "ex" :prefix "DEPT_"}]
                          [?/dept {:ln (:sha-1 ?/boss ?/co)
                                   :ns "ex" :prefix "DEPT_"}])
                  })

(def test-rule-reify-proper-order
  '{:name "test-rule-2a"
    :head ((?/bio_sc rdfs/subClassOf ?/bio)
            (?/bio_sc ex/other ?/other))
    :body
          ((?/rec ex/drug ?/drug)
            (?/rec ex/bio ?/bio))
    :reify ([?/other {:ln (:sha-1 ?/bio "other" )
                      :ns "ex" :prefix "O_"}]
             [?/bio_sc {:ln (:sha-1 ?/bio ?/other)
                        :ns "ex" :prefix "B_"}])
    })

(def test-rule-reify-improper-order
  '{:name "test-rule-2b"
    :head ((?/bio_sc rdfs/subClassOf ?/bio)
            (?/bio_sc ex/other ?/other))
    :body
          ((?/rec ex/drug ?/drug)
            (?/rec ex/bio ?/bio))
    :reify ([?/bio_sc {:ln (:sha-1 ?/bio ?/other)
                       :ns "ex" :prefix "O_"}]
             [?/other {:ln (:sha-1 ?/bio "other")
                       :ns "ex" :prefix "B_"}])
    })

;;; --------------------------------------------------------
;;; tests
;;; --------------------------------------------------------


(deftest test-sort-rule-1
  (let [reify-block-1 '([?/bio_sc {:ln (:sha-1 ?/bio ?/other)
                                 :ns "ex" :prefix "O_"}]
                       [?/other {:ln (:sha-1 ?/bio "other")
                                 :ns "ex" :prefix "B_"}])
        unsorted-1 (map (fn [entry]
                        (if (sequential? entry)
                          (let [[var opts :as form] entry]
                            [var (reify-rule-form-fn test-rule-reify-improper-order form)])
                          (vector entry (default-reify-rule-form-fn))))
                      reify-block-1)

        reify-block-2 '([?/other {:ln (:sha-1 ?/bio "other" )
                                  :ns "ex" :prefix "O_"}]
                         [?/bio_sc {:ln (:sha-1 ?/bio ?/other)
                                    :ns "ex" :prefix "B_"}])

        unsorted-2 (map (fn [entry]
                          (if (sequential? entry)
                            (let [[var opts :as form] entry]
                              [var (reify-rule-form-fn test-rule-reify-improper-order form)])
                            (vector entry (default-reify-rule-form-fn))))
                        reify-block-2)
         ]

    (println "UNSORTED 1")
    (pprint unsorted-1)
  (println "SORTED 1")
  (pprint (sort-reification-based-on-dependencies unsorted-1))

    (println "UNSORTED 2")
    (pprint unsorted-2)
    (println "SORTED 2")
    (pprint (sort-reification-based-on-dependencies unsorted-2))
  ))













;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------
