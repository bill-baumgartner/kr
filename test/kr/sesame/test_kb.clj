(ns kr.sesame.test-kb
  (use clojure.test
       kr.core.kb
       kr.sesame.kb
       kr.sesame.writer-kb)
  (require kr.core.test-kb)
  (import java.io.ByteArrayOutputStream))

;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------

(defn sesame-memory-test-kb []
  (kr.core.kb/open
   (kr.sesame.kb/new-sesame-memory-kb)))

(defn test-ns-hook []
  (binding [kr.core.test-kb/*kb-creator-fn*
            sesame-memory-test-kb]
    (run-tests 'kr.core.test-kb)))


;;run the kb tests for the writer-kb too
(defn sesame-writer-test-kb []
  (new-sesame-writer-kb (ByteArrayOutputStream.)))

(defn test-ns-hook []
  (binding [kr.core.test-kb/*kb-creator-fn*
            sesame-writer-test-kb]
    (run-tests 'kr.core.test-kb)))

;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------
