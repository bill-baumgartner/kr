(ns kr.rdf4j.writer-kb
  (use kr.core.kb
        [kr.core.rdf :exclude (resource)]
        kr.rdf4j.kb
        [kr.rdf4j.rdf :exclude (resource)]
        [clojure.java.io :exclude (resource)])
  (:import org.eclipse.rdf4j.model.impl.ValueFactoryImpl
           java.nio.charset.Charset
           [org.eclipse.rdf4j.rio RDFFormat Rio RDFWriter RDFWriterFactory]
           org.eclipse.rdf4j.rio.ntriples.NTriplesWriterFactory))

;;; --------------------------------------------------------
;;; connections
;;; --------------------------------------------------------

;;this is nonsese becasue to the circular defintions
;;  and what can and cannot be forward delcared
(declare initialize-rdf4j-writer
         open-rdf4j-writer
         close-rdf4j-writer
         rdf4j-write-statement
         rdf4j-write-statements)

;;; --------------------------------------------------------
;;; protocol implementation
;;; --------------------------------------------------------

(defrecord Rdf4jWriterKB [target connection]
  KB

  (native [kb] target)
  (initialize [kb] kb) ;(initialize-rdf4j-writer kb))
  (open [kb] (open-rdf4j-writer kb))
  (close [kb] (close-rdf4j-writer kb))

  rdfKB

  ;; (ns-maps [kb] ns-maps-var)
  ;; (ns-map-to-short [kb] (:ns-map-to-short (deref ns-maps-var)))
  ;; (ns-map-to-long [kb] (:ns-map-to-long (deref ns-maps-var)))
  (root-ns-map [kb] (ns-map-to-long kb))
  (register-ns [kb short long] nil) ; no-op
  
  (create-resource [kb name] (rdf4j-create-resource kb name))
  (create-property [kb name] (rdf4j-create-property kb name))
  (create-literal [kb val] (rdf4j-create-literal kb val))
  (create-literal [kb val type] (rdf4j-create-literal kb val type))

  ;;TODO convert to creating proper string literals
  ;; (create-string-literal [kb str] (rdf4j-create-string-iteral kb val))
  ;; (create-string-literal [kb str lang] 
  ;;                        (rdf4j-create-string literal kb val type))
  (create-string-literal [kb str] (rdf4j-create-literal kb str))
  (create-string-literal [kb str lang] 
                         (rdf4j-create-literal kb str lang))


  (create-blank-node [kb name] (rdf4j-create-blank-node kb name))
  (create-statement [kb s p o] (rdf4j-create-statement kb s p o))

  (add-statement [kb stmt] (rdf4j-write-statement kb stmt))
  (add-statement [kb stmt context] (rdf4j-write-statement kb stmt context))
  (add-statement [kb s p o] (rdf4j-write-statement kb s p o))
  (add-statement [kb s p o context] (rdf4j-write-statement kb s p o context))

  (add-statements [kb stmts] (rdf4j-write-statements kb stmts))
  (add-statements [kb stmts context] (rdf4j-write-statements kb stmts context))

  ;; (ask-statement  [kb s p o context] (rdf4j-ask-statement kb s p o context))
  ;; (query-statement [kb s p o context]
  ;;   (rdf4j-query-statement kb s p o context))

  ;; (load-rdf-file [kb file] (rdf4j-load-rdf-file kb file))
  ;; (load-rdf-file [kb file type] (rdf4j-load-rdf-file kb file type))
  ;;the following will throw exception for unknown rdf format
  ;;(load-rdf-stream [kb stream] (rdf4j-load-rdf-stream kb stream))
  ;;(load-rdf-stream [kb stream type] (rdf4j-load-rdf-stream kb stream type))
)

;;; "constructors"
;;; --------------------------------------------------------
    
(defn new-writer [out-stream]
  (let [writer (Rio/createWriter RDFFormat/NTRIPLES out-stream)]
                                 ;(output-stream target))]
    (.startRDF writer) ;side effect function doesn't return itself
    writer))

(defn open-rdf4j-writer [kb]
  (let [out (output-stream (:target kb))
        writer (new-writer out)]
    (copy-rdf4j-slots (assoc (Rdf4jWriterKB. (:target kb) writer)
                                               ;(new-writer (:target kb)))
                         :output-stream out
                         :value-factory (:value-factory kb))
                       kb)))

(defn close-rdf4j-writer [kb]
  (when (:connection kb)
    (.endRDF (:connection kb))
    (.close (:output-stream kb)))
  (copy-rdf4j-slots (assoc (Rdf4jWriterKB. (:target kb)
                                             nil)
                       :value-factory (:value-factory kb))
                     kb))


;;if the target is a zipped output stream it will happily write there
;; e.g. pass in (GZIPOutputStream. (output-stream ...))
(defn new-rdf4j-writer-kb [target]
  (initialize-ns-mappings
   (assoc (Rdf4jWriterKB. target nil) ;(initial-ns-mappings) nil)
     :value-factory (org.eclipse.rdf4j.model.impl.ValueFactoryImpl.))))
  ;;(.getValueFactory repository)))


;;these can't handle graphs ... TODO change to NQUAD writer??

(defn rdf4j-write-statement
  ([kb stmt] (.handleStatement (connection! kb)
                               ^Statment stmt))
  ([kb stmt context] (.handleStatement (connection! kb)
                                       ^Statement stmt))
  ([kb s p o] (.handleStatement (connection! kb) 
                                ^Statement (statement kb s p o)))
  ([kb s p o context] (.handleStatement (connection! kb) 
                                        ^Statement (statement kb s p o))))


(defn rdf4j-write-statements
  ([kb stmts] (dorun (map (partial rdf4j-write-statement kb) stmts)))
  ([kb stmts context]  (dorun (map (partial rdf4j-write-statement kb) stmts))))


;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------
