(ns kr.rdf4j.kb
  (use kr.core.kb
       kr.core.rdf
       kr.core.sparql
       kr.rdf4j.rdf
       kr.rdf4j.sparql
       )
  (import
    org.eclipse.rdf4j.model.URI
    org.eclipse.rdf4j.model.Resource
    org.eclipse.rdf4j.model.Statement

    org.eclipse.rdf4j.model.impl.StatementImpl
    org.eclipse.rdf4j.model.impl.URIImpl

    org.eclipse.rdf4j.repository.Repository
    org.eclipse.rdf4j.repository.http.HTTPRepository
    org.eclipse.rdf4j.repository.RepositoryConnection

    org.eclipse.rdf4j.repository.sail.SailRepository;
    org.eclipse.rdf4j.sail.memory.MemoryStore;

    org.eclipse.rdf4j.query.resultio.TupleQueryResultFormat
    ))

;;; --------------------------------------------------------
;;; specials
;;; --------------------------------------------------------

(def ^:dynamic *default-server* nil)
(def ^:dynamic *repository-name* nil)
(def ^:dynamic *username* nil)
(def ^:dynamic *password* nil)

(def ^:dynamic *kb-features* (list sparql-1-0 sparql-1-1))


;;; --------------------------------------------------------
;;; triple store connection and setup
;;; --------------------------------------------------------



;;; --------------------------------------------------------
;;; connections
;;; --------------------------------------------------------

;;this is nonsese becasue to the circular defintions
;;  and what can and cannot be forward delcared
(declare rdf4j-initialize
         new-rdf4j-connection
         close-existing-rdf4j-connection)

(defn rdf4j-connection [kb]
  (new-rdf4j-connection kb))

(defn close-rdf4j-connection [kb]
  (close-existing-rdf4j-connection kb))


;;; --------------------------------------------------------
;;; namespaces
;;; --------------------------------------------------------

;;; --------------------------------------------------------
;;; protocol implementation
;;; --------------------------------------------------------

;;TODO seperate server and connection??

(defrecord Rdf4jKB [server connection kb-features]
  KB

  (native [kb] server)
  (initialize [kb] (rdf4j-initialize kb))
  (open [kb] (new-rdf4j-connection kb))
  (close [kb] (close-rdf4j-connection kb))
  (features [kb] kb-features)

  rdfKB

  (root-ns-map [kb] (rdf4j-server-ns-map kb))
  ;; (ns-maps [kb] ns-maps-var)
  ;; (ns-map-to-short [kb] (:ns-map-to-short (deref ns-maps-var)))
  ;; (ns-map-to-long [kb] (:ns-map-to-long (deref ns-maps-var)))
  (register-ns [kb short long] (rdf4j-register-ns kb short long))

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

  (add-statement [kb stmt] (rdf4j-add-statement kb stmt))
  (add-statement [kb stmt context] (rdf4j-add-statement kb stmt context))
  (add-statement [kb s p o] (rdf4j-add-statement kb s p o))
  (add-statement [kb s p o context] (rdf4j-add-statement kb s p o context))

  (add-statements [kb stmts] (rdf4j-add-statements kb stmts))
  (add-statements [kb stmts context] (rdf4j-add-statements kb stmts context))

  (ask-statement  [kb s p o context] (rdf4j-ask-statement kb s p o context))
  (query-statement [kb s p o context] (rdf4j-query-statement kb s p o context))


  (load-rdf-file [kb file] (rdf4j-load-rdf-file kb file))
  (load-rdf-file [kb file type] (rdf4j-load-rdf-file kb file type))

  ;;the following will throw exception for unknown rdf format
  (load-rdf-stream [kb stream] (rdf4j-load-rdf-stream kb stream))

  (load-rdf-stream [kb stream type] (rdf4j-load-rdf-stream kb stream type))



  sparqlKB

  (ask-pattern [kb pattern]
    (rdf4j-ask-pattern kb pattern))
  (ask-pattern [kb pattern options]
    (rdf4j-ask-pattern kb pattern options))

  (query-pattern [kb pattern]
    (rdf4j-query-pattern kb pattern))
  (query-pattern [kb pattern options]
    (rdf4j-query-pattern kb pattern options))

  (count-pattern [kb pattern]
    (rdf4j-count-pattern kb pattern))
  (count-pattern [kb pattern options]
    (rdf4j-count-pattern kb pattern options))

  (visit-pattern [kb visitor pattern]
    (rdf4j-visit-pattern kb visitor pattern))
  (visit-pattern [kb visitor pattern options]
    (rdf4j-visit-pattern kb visitor pattern options))

  (construct-pattern [kb create-pattern pattern]
    (rdf4j-construct-pattern kb create-pattern pattern))
  (construct-pattern [kb create-pattern pattern options]
    (rdf4j-construct-pattern kb create-pattern pattern options))
  (construct-visit-pattern [kb visitor create-pattern pattern]
    (rdf4j-construct-visit-pattern kb visitor create-pattern pattern))
  (construct-visit-pattern [kb visitor create-pattern pattern options]
    (rdf4j-construct-visit-pattern kb visitor create-pattern pattern options))


  (boolean-sparql [kb query-string]
    (rdf4j-boolean-sparql kb query-string))
  (ask-sparql [kb query-string]
    (rdf4j-ask-sparql kb query-string))
  (query-sparql [kb query-string]
    (rdf4j-query-sparql kb query-string))
  (count-sparql [kb query-string]
    (rdf4j-count-sparql kb query-string))
  (visit-sparql [kb visitor query-string]
    (rdf4j-visit-sparql kb visitor query-string))

  (construct-sparql [kb sparql-string]
    (rdf4j-construct-sparql kb sparql-string))
  (construct-visit-sparql [kb visitor sparql-string]
    (rdf4j-construct-visit-sparql kb visitor sparql-string))

  )

;; TODO factor this out to a "connection" package

;;; "constructors"
;;; --------------------------------------------------------

;; the way new Rdf4jKBConnection is being called it isn't preserving
;;   the additional keys that are added on to the rdf4j server
;;   specifically the :value-factory

;; (defn new-rdf4j-server []
;;   (let [repository (HTTPRepository. *default-server* *repository-name*)]
;;     (.setPreferredTupleQueryResultFormat repository
;;                                          TupleQueryResultFormat/SPARQL)
;;     (if (and *username* *password*)
;;       (.setUsernameAndPassword repository *username* *password*))
;;     (assoc (Rdf4jKB. repository (initial-ns-mappings) nil)
;;       :value-factory (.getValueFactory repository))))

(defn copy-rdf4j-slots [target-kb source-kb]
  (copy-rdf-slots (copy-kb-slots target-kb source-kb)
                  source-kb))


(defn rdf4j-kb-helper [repository]
  (initialize-ns-mappings
    (assoc (Rdf4jKB. repository nil *kb-features*)
      :value-factory (.getValueFactory repository))))


;; (defn new-rdf4j-server [& {:keys [server repo-name username password]
;;                             :or {server *default-server*
;;                                  repo-name *repository-name*
;;                                  username *username*
;;                                  password *password*}}]
;;   (println "server" server  " name" repo-name)
;;   (println "username" username  " password" password)
;;   (let [repository (HTTPRepository. server repo-name)]
;;     (.setPreferredTupleQueryResultFormat repository
;;                                          TupleQueryResultFormat/SPARQL)
;;     (if (and username password)
;;       (.setUsernameAndPassword repository username password))
;;     (assoc (Rdf4jKB. repository (initial-ns-mappings) nil)
;;       :value-factory (.getValueFactory repository))))

(defn new-rdf4j-server [& {:keys [server repo-name username password]
                            :or {server *default-server*
                                 repo-name *repository-name*
                                 username *username*
                                 password *password*}}]
  ;; (println "server" server  " name" repo-name)
  ;; (println "username" username  " password" password)
  (let [repository (org.eclipse.rdf4j.repository.http.HTTPRepository. server
                                                                repo-name)]
    (.setPreferredTupleQueryResultFormat repository
                                         TupleQueryResultFormat/SPARQL)
    (if (and username password)
      (.setUsernameAndPassword repository username password))
    (rdf4j-kb-helper repository)))

;; (defn new-rdf4j-server-helper [server & [repo-name username password]]
;;   (apply new-rdf4j-server
;;          (concat [:server server]
;;                  (if repo-name [:repo-name repo-name] nil)
;;                  (if username [:username username] nil)
;;                  (if password [:password password] nil))))


(defn new-rdf4j-connection [kb]
  (copy-rdf4j-slots (assoc (Rdf4jKB. (:server kb)
                                       (.getConnection (:server kb))
                                       (features kb))
                       :value-factory (:value-factory kb))
                     kb))

(defn close-existing-rdf4j-connection [kb]
  (when (:connection kb)
    (.close (:connection kb)))
  (copy-rdf4j-slots (assoc (Rdf4jKB. (:server kb) nil (features kb))
                       :value-factory (:value-factory kb))
                     kb))

(defmethod kb org.eclipse.rdf4j.repository.http.HTTPRepository [arg]
  (if (class? arg)
    (new-rdf4j-server)
    (rdf4j-kb-helper arg)))


(defmethod kb org.eclipse.rdf4j.repository.Repository [arg]
  (if (class? arg)
    (new-rdf4j-server)
    (rdf4j-kb-helper arg)))

(defmethod kb org.eclipse.rdf4j.sail.memory.MemoryStore [arg]
  (if (class? arg)
    (let [repo (SailRepository. (MemoryStore.))]
      ;; (.setPreferredTupleQueryResultFormat repo
      ;;                                      TupleQueryResultFormat/SPARQL)
      (.initialize repo)
      (rdf4j-kb-helper repo))
    (rdf4j-kb-helper arg)))

;; need to add more kb constructors in here for taking in various rdf4j objects
;;   repository, sail, etc. instances


(defn rdf4j-initialize [kb]
  (synch-ns-mappings kb))

;; (defn new-rdf4j-memory-kb []
;;   (let [repo (SailRepository. (MemoryStore.))]
;;     (.initialize repo)
;;     ;(.setPreferredTupleQueryResultFormat repo TupleQueryResultFormat/SPARQL)
;;     (assoc (Rdf4jKB. repo (initial-ns-mappings) nil)
;;       :value-factory (.getValueFactory repo))))

(defn new-rdf4j-memory-kb []
  (kb org.eclipse.rdf4j.sail.memory.MemoryStore))


(defmethod kb :rdf4j-mem [_]
  (new-rdf4j-memory-kb))


;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------
