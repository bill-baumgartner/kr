
(ns kr.sesame.rdf
  (use kr.core.variable
       kr.core.kb
       kr.core.clj-ify
       [kr.core.rdf :exclude (resource)]
       clojure.java.io)
  (:import [java.io IOException]

    ;; Interfaces
           [org.eclipse.rdf4j.model Literal Resource Statement URI]

    ;; Classes

    ;; FIXME: Depending on impl classes is suspect, much like relying on
    ;; anything in com.sun.*.  These are internal classes from which no
    ;; consistent interface should be expected.  We should by using the
    ;; interfaces that these implement and the appropriate ValueFactory
    ;; to create instances of these where necessary.
           [org.eclipse.rdf4j.model.impl StatementImpl URIImpl]
           [org.eclipse.rdf4j.repository RepositoryConnection]
           org.eclipse.rdf4j.repository.http.HTTPRepository
           org.eclipse.rdf4j.query.resultio.TupleQueryResultFormat
           [org.eclipse.rdf4j.rio RDFFormat Rio]))

;; ---------------------------------------------------------------------------
;; helpers
;; ---------------------------------------------------------------------------

(defn sesame-iteration-seq [results]
  (lazy-seq
    (when (.hasNext results)
      (cons
        (.next results)
        (sesame-iteration-seq results)))))


;; --------------------------------------------------------
;; sesame specific connection
;; --------------------------------------------------------

(defn connection! [kb]
  ^RepositoryConnection (:connection (connection kb)))

;; --------------------------------------------------------
;; namespaces
;; --------------------------------------------------------

(defn sesame-register-ns [kb short long]
  (.setNamespace (connection! kb) short long))

;;TODO? use clj-ify under the hood?
(defn sesame-server-ns-map [kb]
  (reduce (fn [m ns]
            (assoc m (.getPrefix ns) (.getName ns)))
          {}
          (doall
            (sesame-iteration-seq
              (.getNamespaces (connection! kb))))))

;; --------------------------------------------------------
;; sesame-ify
;; --------------------------------------------------------

(defn sesame-uri [uri-string]
  (URIImpl. uri-string))
;;maybe this shoudl be
;;  (.createURI (kb :value-factory) uri-string))

(defn sesame-create-resource
  [kb r]
  ^Resource (sesame-uri r))

(defn sesame-create-property
  [kb p]
  (sesame-uri p))

(defn sesame-create-blank-node
  [kb id]
  (.createBNode (:value-factory kb) id))

(defn sesame-create-literal
  ([kb l]
   (.createLiteral (:value-factory kb) l))
  ([kb s type-or-lang]
   (.createLiteral (:value-factory kb)
                   s
                   (if (string? type-or-lang)
                     type-or-lang
                     (kr.core.rdf/resource kb type-or-lang)))))


(defn sesame-create-statement
  [kb s p o]
  (StatementImpl. s p o))

(defn sesame-context-array
  ([] (make-array Resource 0))
  ([kb c] (if c
            (let [a (make-array Resource 1)]
              (aset a 0 ^Resource (kr.core.rdf/resource kb c))
              ;;(sesame-uri (resource-ify c)))
              a)
            (sesame-context-array)))
  ([kb c & rest] (let [a (make-array Resource (+ 1 (count rest)))]
                   (map (fn [v i]
                          (aset a i (kr.core.rdf/resource kb v)))
                        (cons c rest)
                        (range))
                   a)))

;;; --------------------------------------------------------
;;; literal-types
;;; --------------------------------------------------------

(defmulti literal-clj-ify (fn [kb l]
                            (let [t (.getDatatype l)]
                              (and t (.toString t)))))

(defmethod literal-clj-ify :default [kb l]
  (.stringValue l))

;;for some reason strings come back as nil which is different than :default
(defmethod literal-clj-ify nil [kb l]
  (.stringValue l))

;; need to flesh this out and test...

;; (defmethod literal-clj-ify "http://www.w3.org/2001/XMLSchema#integer" [kb l]
;;   (.intValue l))

(defmacro lit-clj-ify [type & body]
  `(defmethod literal-clj-ify ~type ~(vec '(kb l))
     ~@body))

(lit-clj-ify "http://www.w3.org/2001/XMLSchema#boolean" (.booleanValue l))

(lit-clj-ify "http://www.w3.org/2001/XMLSchema#int" (.intValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#integer" (.integerValue l))

(lit-clj-ify "http://www.w3.org/2001/XMLSchema#long" (.longValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#float" (.floatValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#double" (.doubleValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#decimal" (.decimalValue l))

(lit-clj-ify "http://www.w3.org/2001/XMLSchema#dateTime" (.calendarValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#time" (.calendarValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#date" (.calendarValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#gYearMonth" (.calendarValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#gMonthDay" (.calendarValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#gYear" (.calendarValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#gMonth" (.calendarValue l))
(lit-clj-ify "http://www.w3.org/2001/XMLSchema#gDay" (.calendarValue l))


(defn literal-to-string-value [kb l]
  (.stringValue l))

(defn literal-to-value [kb l]
  (literal-clj-ify kb l))

(defn literal-language [l]
  (let [lang (.orElse (.getLanguage l) nil)]
    (if (= "" lang)
      nil
      lang)))

(def ^{:private true} rdf:langString
  "http://www.w3.org/1999/02/22-rdf-syntax-ns#langString")

(def ^{:private true} xsd:string
  "http://www.w3.org/2001/XMLSchema#string")

(defn literal-type-or-language [kb literal]
  (let [data-type (.getDatatype literal)]
    (cond (= (str data-type) rdf:langString) (literal-language literal)
          ;; In an apparent chanage between Sesame 2 and Sesame 4, an IRI is
          ;; now returned as the type for string literals.  Honouring this
          ;; seems like the right thing to do, but to preserve backwards
          ;; compatibility, we continue to return 'nil' as the type for
          ;; strings.
          (= (str data-type) xsd:string) nil
          true (clj-ify kb data-type))))

(defn literal-to-clj [kb l]
  (clj-ify-literal kb l
                   literal-to-value
                   literal-to-string-value
                   literal-type-or-language))

;;; --------------------------------------------------------
;;; clj-ify
;;; --------------------------------------------------------

(defmethod clj-ify org.eclipse.rdf4j.model.URI [kb r]
  (if (or (= "" (.getLocalName r))
          (= "" (.getNamespace r)))
    (convert-string-to-sym kb (.stringValue r))
    (convert-string-to-sym kb
                           (.stringValue r)
                           (.getNamespace r)
                           (.getLocalName r))))

(defmethod clj-ify org.eclipse.rdf4j.model.Resource [kb r]
  (convert-string-to-sym kb (.stringValue r)))

(defmethod clj-ify org.eclipse.rdf4j.model.BNode [kb bnode]
  (symbol *anon-ns-name* (.stringValue bnode)))

;;need to get numbers back out of literals
(defmethod clj-ify org.eclipse.rdf4j.model.Literal [kb v]
  (literal-to-clj kb v))

(defmethod clj-ify org.eclipse.rdf4j.model.Value [kb v]
  (.stringValue v))

(prefer-method clj-ify [org.eclipse.rdf4j.model.Literal] [org.eclipse.rdf4j.model.Value])


(defmethod clj-ify org.eclipse.rdf4j.model.Statement [kb s]
  (list (clj-ify kb (.getSubject s))
        (clj-ify kb (.getPredicate s))
        (clj-ify kb (.getObject s))))

(defmethod clj-ify org.eclipse.rdf4j.repository.RepositoryResult [kb results]
  (map (partial clj-ify kb)
       (sesame-iteration-seq results)))

;; --------------------------------------------------------
;; adding
;; --------------------------------------------------------

(defn sesame-add-statement
  ([kb stmt] (.add (connection! kb)
                   ^Statment stmt
                   (sesame-context-array))) ;;(make-array Resource 0)))
  ([kb stmt context] (.add (connection! kb)
                           ^Statement stmt
                           (sesame-context-array kb context)))
  ([kb s p o] (.add (connection! kb)
                    ^Statement (statement kb s p o)
                    (sesame-context-array)))
  ([kb s p o context] (.add (connection! kb)
                            ^Statement (statement kb s p o)
                            ;;^Resource s p o 
                            (sesame-context-array kb context))))

(defn sesame-add-statements
  ([kb stmts] (.add (connection! kb)
                    ^Iterable (map (fn [s]
                                     (apply statement kb s))
                                   stmts)
                    (sesame-context-array)))
  ([kb stmts context] (.add (connection! kb)
                            ^Iterable (map (fn [s]
                                             (apply statement kb s))
                                           stmts)
                            (sesame-context-array kb context))))

(defmulti convert-to-sesame-type identity)
(defmethod convert-to-sesame-type :n3 [sym] RDFFormat/N3)
(defmethod convert-to-sesame-type :ntriple [sym] RDFFormat/NTRIPLES)
(defmethod convert-to-sesame-type :rdfxml [sym] RDFFormat/RDFXML)
(defmethod convert-to-sesame-type :trig [sym] RDFFormat/TRIG)
(defmethod convert-to-sesame-type :trix [sym] RDFFormat/TRIX)
(defmethod convert-to-sesame-type :turtle [sym] RDFFormat/TURTLE)

(defn sesame-load-rdf-file
  ([kb file]
   (.add (connection! kb)
         file
         "" ;nil ;""
         (Rio/getParserFormatForFileName (.getName file))
         (sesame-context-array kb *graph*)))
  ([kb file type]
   (.add (connection! kb)
         file
         "" ;nil ;""
         (convert-to-sesame-type type)
         (sesame-context-array kb *graph*))))

(defn sesame-load-rdf-stream
  ([kb stream]
   (throw (IOException. "Unknown RDF format type for stream.")))
  ([kb stream type]
   (.add (connection! kb)
         stream
         "" ;nil ;""
         (convert-to-sesame-type type)
         (sesame-context-array kb *graph*))))


;; --------------------------------------------------------
;; querying
;; --------------------------------------------------------

(defn sesame-ask-statement
  ([kb s p o context]
   (.hasStatement ^RepositoryConnection (connection! kb)
                  ^Resource (and s (kr.core.rdf/resource kb s))
                  ^URI (and p (property kb p))
                  ^Value (and o (object kb o))
                  *use-inference*
                  (sesame-context-array kb context))))

(defn sesame-query-statement
  ([kb s p o context]
   (clj-ify kb
            (let [result (.getStatements ^RepositoryConnection (connection! kb)
                                         ^Resource (and s (kr.core.rdf/resource kb s))
                                         ^URI (and p (property kb p))
                                         ^Value (and o (object kb o))
                                         *use-inference*
                                         (sesame-context-array kb context))]
              result))))