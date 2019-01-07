(ns kr.core.rule
  (use kr.core.utils
       kr.core.unify
       kr.core.assertion
       kr.core.variable
       kr.core.kb
       kr.core.clj-ify
       kr.core.rdf
       kr.core.sparql
       [clojure.java.io :exclude (resource)]
       clojure.set
       clojure.pprint))
;;  (import java.io.PushbackReader))
       
;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------


;; rule standard keys:
;; :name
;; :head
;; :body
;; :direction
;; :preferred-reasoner

;;; --------------------------------------------------------
;;; constants
;;; --------------------------------------------------------

(def ^:dynamic *verify-on-read* nil)

(def ^:dynamic *default-ok-ns* #{"_" "?"})

(def ^:dynamic *print-bad-rules* true)

;;; --------------------------------------------------------
;;; helpers
;;; --------------------------------------------------------

;; (defn read-rule [in]
;;   '())

;;this doesn't have to be a file, it can be anything that can be made reader
(defn load-rules-from-file [file]
  (all-input file))
;;(read-all-input file))

(defn directory? [f]
  ;;make sure it's not a string no .isDirectory on strings
  (and (instance? java.io.File f)
       (.isDirectory f)))

(defn load-rules-from-directory [dir]
  (mapcat all-input
          (remove directory?
                  (remove (fn [file]
                            (or (not (.endsWith (str file) ".clj"))
                                (.contains (str file) "deprecated")
                                (.contains (str file) "under_construction")))
                          (directory-seq dir)))))


;;loads from all the files that match the pattern
(defn load-rules-from-classpath [pattern]
  (mapcat all-input
          (remove directory?
                  ;; rules are stored in .clj files, so the .clj suffix
                  ;; is specified as a requirement in the
                  ;; classpath-matching function
                  (classpath-matching pattern ".clj"))))

  

  ;; (with-open [r (PushbackReader. (reader file))]
  ;;   (loop [form (read r nil r)
  ;;          results nil]
  ;;     (if (= form r) ; sentinal value stream can't read itself
  ;;       results
  ;;       (recur (read r nil r) (conj results form))))))

;;; --------------------------------------------------------
;;; static rule testing
;;; --------------------------------------------------------


(defn connected-rule? [{head :head 
                        body :body
                        :as rule}]
  "This checks queries that have disjoint solutions, e.g. potential for cross-products.
  Currently, if the body is itself a SPARQL query, this method simply returns true.
  Further code would be needed to parse the variables from a SPARQL query. "
  (cond (string? body) true
  :else (let [dja-body (disjoint-assertions? body)
              dja-head (disjoint-assertions? head)
              dja-headbody (disjoint-assertions? (concat head body))]
          (if dja-body (println (str "Warning disjoint assertions in rule body: " (:name rule))))
          (if dja-head (println (str "Warning disjoint assertions in rule head: " (:name rule))))
          (if dja-headbody (println (str "Warning disjoint assertions in rule head/body: " (:name rule))))
          (and (not dja-body) (not dja-head) (not dja-headbody)))))

       ; (and (not (disjoint-assertions? body))
       ;(not (disjoint-assertions? head))
       ;;; can't just test the combination -
       ;;;   the head could make the body connected when it otherwise isn't
       ;(not (disjoint-assertions? (concat head body))))))

;;test if there's a var in the head not mentioned in the body
(defn all-head-vars-in-body? [{head :head
                               body :body
                               :as  rule}]
  (let [head-vars (variables head)
        body-vars (variables body)
        result (every? (set body-vars) head-vars)]
    (if (not result) (println (str "Warning all head variables not found in rule body: " (:name rule))))
    result))


(defn some-head-vars-in-body? [{head :head
                               body :body
                               :as rule}]
  (let [head-vars (variables head)
        body-vars (variables body)]
    (some (set head-vars) body-vars)))


(defn all-ns-known? [safe-ns-set {head :head
                                  body :body
                                  :as rule}]
  (every? (fn [sym]
            ;;either the sym is a keyword or it's ns is in the set
            (or (keyword? sym)
                ((union *default-ok-ns* (set safe-ns-set))
                 (namespace sym))))
          ;;get list of distinct symbols, remove all literals
          (remove (complement symbol?)
                  (distinct (flatten (concat head (if (string? body) '() body)))))))
  


(defn bad-rules [test rules fail-message]
  (let [bad (remove test rules)] ; remove if you pass the test
    (when *print-bad-rules*
      (doseq [rule bad]
        (println fail-message (:source (meta rule)))))
    bad)) ; return the bad rules

;;; --------------------------------------------------------
;;; kb rule testing
;;; --------------------------------------------------------

(defn body-in-kb? [kb {body :body
                       :as rule}]
  (ask kb body))





;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------


;;; --------------------------------------------------------
;;; simple forward rule
;;; --------------------------------------------------------


(defn apply-horn-rule
  "in prolog: head :- body (head is true if body is)"
  [head body]
  (map (fn [bindings]
         (subst-bindings head bindings))
       (clj-ify (query body))))



(defn apply-horn-rule-and-refiy
  "in prolog: head :- body (head is true if body is)"
  [head body]
  (map (fn [bindings]
         (reify-assertions (subst-bindings head bindings)))
       (clj-ify (query body))))


(defn apply-post-processing-rule [kb {name :name 
                                      template :query
                                      transform :post-process :as rule}]
  ;; print statement below for debugging purposes only - remove at any time
  ;;(prn (str "# query hits for rule " name ": " (.size (query kb template))))
  (dorun
   (map (fn [bindings count]
          ;; this isn't running at the right time...
          ;(if (= 0 (mod count 1000))
          ;  (println count))
          ;(dorun (map add!
                      (transform bindings));))
        (query kb template)
        (range))))
  



;; (defn apply-multi-stage-horn-rule
;;   "multiple bodies which are applied sequentially.
;;    reification is done as necessary, however not to the first part.
;;    note the parts are listed in reverse order to be consistent with
;;    prolog head :- body ordering."
;;   [& parts]
;;   (apply-multi-stage-horn-rule-reversed (reverse parts) '()))

;;   (map (fn [bindings]
;;          (reify-assertions (subst-bindings head bindings)))
;;        (clj-ify (query body))))

;; (defun apply-multi-stage-horn-rule-reversed [parts bindings]
;;   (let [[




;; for things that need to be reified allow for a name template?
;;   so that names that can be found / reused can be generated?
;; this is dangerously hacky from a KR perspective



;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------


;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------


