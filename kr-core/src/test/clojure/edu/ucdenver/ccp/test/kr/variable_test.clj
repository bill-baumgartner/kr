(ns edu.ucdenver.ccp.test.kr.variable-test
  (:use clojure.test)
  (require [edu.ucdenver.ccp.kr.variable :refer [count-block?
                                                 math-block?
                                                 math-blocks 
                                                 symbols-no-vars]]))

(deftest count-block?-test
  (is (count-block? `[:count ?/a]))
  (is (not (count-block? `?/a)))
  (is (not (count-block? `[:float :as ?/result ?/a "+" ?/b])))
  (is (not (count-block? `[:float :as ?/result ?/count "/8"]))))
  
(deftest math-block?-test
  (is (math-block? `{:num_type :float :as ?/result :eqn [?/a "+" ?/b]}))
  (is (math-block? `{:num_type :integer :as ?/result :eqn [?/a "+" ?/b]}))
  (is (not (math-block? `{:as ?/result :eqn [?/a "+" ?/b]}))) ;; missing :num_type
  (is (not (math-block? `[:float :as ?/result ?/a "+" ?/b]))))


(def head-with-count-block
  `((?/super ex/hasDescendentCount [:count :distinct ?/mid])
    (?/a ex/blah ?/b)
    (?/c ex/blahblah ?/d)))

(def expected-symbols-from-count-block
  `#{ex/hasDescendentCount ex/blah ex/blahblah})

(deftest symbols-no-vars-test-count-block
  (is (= expected-symbols-from-count-block (set (symbols-no-vars head-with-count-block)))))

(def head-with-math-block
  `((?/jd rdf/type iaohan/JiangDistance)
  (?/jd obo/RO_0000057 ?/c1) ;; RO:has_participant
    (?/jd obo/RO_0000057 {:num_type :float
                          :as ?/something
                          :eqn ["-2*fake:fn(" ?/p1 ")"]}) 
    (?/jd iaohan/jiang_distance {:num_type :float
                                 :as ?/jiang_d
                                 :eqn ["-2*ccp_sparql_ext:ln("
                                       ?/pms
                                       ") - (ccp_sparql_ext:ln("
                                       ?/p1
                                       ") + ccp_sparql_ext:ln("
                                       ?/p2
                                       "))"]})))

(def expected-math-blocks
  `#{{:num_type :float
     :as ?/something
     :eqn ["-2*fake:fn(" ?/p1 ")"]}
    {:num_type :float
     :as ?/jiang_d
     :eqn ["-2*ccp_sparql_ext:ln("
           ?/pms
           ") - (ccp_sparql_ext:ln("
           ?/p1
           ") + ccp_sparql_ext:ln("
           ?/p2
           "))"]}})


(deftest math-blocks-extraction-test
  (is (= expected-math-blocks (math-blocks head-with-math-block))))

(def expected-symbols-from-math-block
  `#{xsd/float fake/fn rdf/type iaohan/JiangDistance obo/RO_0000057 iaohan/jiang_distance ccp_sparql_ext/ln})

(deftest symbols-no-vars-test-math-block
  (is (= expected-symbols-from-math-block (set (symbols-no-vars head-with-math-block)))))
