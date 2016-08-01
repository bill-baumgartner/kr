(ns edu.ucdenver.ccp.test.kr.variable-test
  (:use clojure.test)
  (require [edu.ucdenver.ccp.kr.variable :refer [count-block?]]))

(deftest count-block?-test
  (is (count-block? `[:count ?/a]))
  (is (not (count-block? `?/a))))
