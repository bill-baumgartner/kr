(ns kr.sesame.test-kb
  (:require [clojure.test :refer [run-tests]]
            [kr.core.kb :as c-kb]
            [kr.sesame
             [kb :refer [new-sesame-memory-kb]]
             [writer-kb :refer [new-sesame-writer-kb]]]
            [kr.core.test-kb :as t-kb])
  (:import [java.io ByteArrayOutputStream]))

;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------

(defn sesame-memory-test-kb []
  (c-kb/open (new-sesame-memory-kb)))

(defn test-ns-hook []
  (binding [t-kb/*kb-creator-fn* sesame-memory-test-kb]
    (run-tests 'kr.core.test-kb)))

(defn sesame-writer-test-kb []
  (new-sesame-writer-kb (ByteArrayOutputStream.)))

(defn test-ns-hook []
  (binding [t-kb/*kb-creator-fn* sesame-writer-test-kb]
    (run-tests 'kr.core.test-kb)))