(ns kr.rdf4j.test-kb
  (:require [clojure.test :refer [run-tests]]
            [kr.core.kb :as c-kb]
            [kr.rdf4j
             [kb :refer [new-rdf4j-memory-kb]]
             [writer-kb :refer [new-rdf4j-writer-kb]]]
            [kr.core.test-kb :as t-kb])
  (:import [java.io ByteArrayOutputStream]))

(defn rdf4j-memory-test-kb []
  (c-kb/open (new-rdf4j-memory-kb)))

(defn test-ns-hook []
  (binding [t-kb/*kb-creator-fn* rdf4j-memory-test-kb]
    (run-tests 'kr.core.test-kb)))

(defn rdf4j-writer-test-kb []
  (new-rdf4j-writer-kb (ByteArrayOutputStream.)))

(defn test-ns-hook []
  (binding [t-kb/*kb-creator-fn* rdf4j-writer-test-kb]
    (run-tests 'kr.core.test-kb)))

;; -- for REPL eval ----------------------------------------------------------

(comment {:desc "Eval (CIDER: C-c C-e) the form (denoted by `:eval`) at the
  REPL to install the creator function such that the core test-kb tests can be
   run individually at the REPL."
          :eval (alter-var-root #'kr.core.test-kb/*kb-creator-fn*
                                (fn [_] rdf4j-writer-test-kb))})