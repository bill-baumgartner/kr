(ns edu.ucdenver.ccp.test.kr.variable-test
  (:use clojure.test)
  (require [edu.ucdenver.ccp.kr.variable :refer [count-block?
                                                 math-block?
                                                 symbols-no-vars]]))

(deftest count-block?-test
  (is (count-block? `[:count ?/a]))
  (is (not (count-block? `?/a)))
  (is (not (count-block? `[:float :as ?/result ?/a "+" ?/b])))
  (is (not (count-block? `[:float :as ?/result ?/count "/8"]))))
  

(deftest math-block?-test
  (is (math-block? `[:float :as ?/result ?/a "+" ?/b]))
  (is (math-block? `[:integer :as ?/result ?/a "+" ?/b]))
  (is (math-block? `[:double :as ?/result ?/a "+" ?/b]))
  (is (math-block? `[:decimal :as ?/result ?/a "+" ?/b]))
  (is (not (math-block? `[:imaginary :as ?/result ?/a "+" ?/b])))
  (is (not (math-block? `[1 :as ?/result ?/a "+" ?/b]))))


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
  (?/jd obo/RO_0000057 [:float :as ?/something "-2*fake:fn(" ?/p1 ")"]) 
  (?/jd iaohan/jiang_distance [:float :as ?/jiang_d
                               "-2*ccp_sparql_ext:ln("
                               ?/pms
                               ") - (ccp_sparql_ext:ln("
                               ?/p1
                               ") + ccp_sparql_ext:ln("
                               ?/p2
                               "))"])))

(def expected-symbols-from-math-block
  `#{xsd/float fake/fn rdf/type iaohan/JiangDistance obo/RO_0000057 iaohan/jiang_distance ccp_sparql_ext/ln})

(deftest symbols-no-vars-test-math-block
  (is (= expected-symbols-from-math-block (set (symbols-no-vars head-with-math-block)))))