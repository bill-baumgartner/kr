(ns kr.core.forward-rule
  (use kr.core.utils
       kr.core.unify
       kr.core.assertion
       kr.core.variable
       kr.core.reify
       kr.core.kb
       kr.core.clj-ify
       kr.core.rdf
       kr.core.sparql
       kr.core.rule
       [clojure.java.io :exclude (resource)]
       clojure.set
       clojure.pprint)
  (require [com.stuartsierra.dependency :as dep]))
;;  (import java.io.PushbackReader))

;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------

;;; --------------------------------------------------------
;;; constants
;;; --------------------------------------------------------


;;; --------------------------------------------------------
;;; kb rule testing
;;; --------------------------------------------------------



;;; --------------------------------------------------------
;;; reification
;;; --------------------------------------------------------

;;; reify from rule expression
;;; --------------------------------------------------------

(defmacro with-reify-name-bindings [name-bindings & body]
  `(binding [*reify-ns* (or (:ns ~name-bindings) *reify-ns*)
             *reify-prefix* (or (:prefix ~name-bindings) *reify-prefix*)
             *reify-suffix* (or (:suffix ~name-bindings) *reify-suffix*)]
     ~@body))


;;form [var {ns-name :ns
;;           prefix :prefix suffix :suffix
;;           l-name :ln <<one of the following with listed params>>
;;           :unique
;;           :localname   [vars ..]
;;           :md5         [vars ..]
;;           :regex       [match replace vars..]
;;           :fn          (fn [bindings] ..)
;;           :as reify-opts}

;; this will take the above type of form and return a function
;; that will function to refify symbols for that form when passed the bindings

(defmulti reify-rule-form-fn
          (fn [rule reify-form]
            (cond
              (not (sequential? reify-form)) :default
              (not (map? (second reify-form))) :default     ;is this actually an error?
              (keyword? (:ln (second reify-form))) (:ln (second reify-form))
              :else (first (:ln (second reify-form))))))


(defn extend-reify-map [reify-opts fn & [dependency-vars]]
  (assoc reify-opts
    :reify-fn fn
    :dependencies dependency-vars))

(defmethod reify-rule-form-fn :default [rule [var reify-opts]]
  (extend-reify-map reify-opts
                    (fn [bindings]
                      (with-reify-name-bindings reify-opts
                                                (reify-unique)))))

(defmethod reify-rule-form-fn :unique [rule [var reify-opts]]
  (extend-reify-map reify-opts
                    (fn [bindings]
                      (with-reify-name-bindings reify-opts
                                                (reify-unique)))))

(defmethod reify-rule-form-fn :localname [rule
                                          [var {[fn-name & params] :ln
                                                :as                reify-opts}]]
  (extend-reify-map reify-opts
                    (fn [bindings]
                      (with-reify-name-bindings reify-opts
                                                (apply reify-localname (subst-bindings params bindings))))
                    (variables params)))

(defmethod reify-rule-form-fn :md5 [rule
                                    [var {[fn-name & params] :ln
                                          :as                reify-opts}]]
  (extend-reify-map reify-opts
                    (fn [bindings]
                      (with-reify-name-bindings reify-opts
                                                (apply reify-md5 (subst-bindings params bindings))))
                    (variables params)))

(defmethod reify-rule-form-fn :sha-1 [rule
                                      [var {[fn-name & params] :ln
                                            :as                reify-opts}]]
  (extend-reify-map reify-opts
                    (fn [bindings]
                      (with-reify-name-bindings reify-opts
                                                (apply reify-sha-1 (subst-bindings params bindings))))
                    (variables params)))



(defn restriction-signature [assertions]
  (let [parts (map vec
                   (remove (fn [[p o :as triple-part]]
                             (= 'rdf/type p))
                           (distinct
                             (map rest assertions))))]
    ;;(pprint parts)
    (cons 'owl/Restriction
          (flatten
            (sort parts)))))


(defn get-restriction-assertions [restriction assertions]
  (remove (fn [[s]]
            (not (= restriction s)))
          assertions))

(defmethod reify-rule-form-fn :restriction [rule
                                            [var {[fn-name & params] :ln
                                                  :as                reify-opts}]]
  (let [head (:head rule)
        restriction-assertions (get-restriction-assertions var head)
        restriction-sig (restriction-signature restriction-assertions)]
    ;;(println "restriction-sig:")
    ;;(println restriction-sig)
    (extend-reify-map reify-opts
                      (fn [bindings]
                        (with-reify-name-bindings reify-opts
                                                  (apply reify-sha-1 (subst-bindings restriction-sig bindings))))
                      (variables restriction-sig))))



(defmethod reify-rule-form-fn :regex [rule
                                      [var {[fn-name match replace & vars] :ln
                                            :as                            reify-opts}]]
  (extend-reify-map reify-opts
                    (fn [bindings]
                      (with-reify-name-bindings reify-opts
                                                (apply reify-regex match replace
                                                       (subst-bindings vars bindings))))
                    ;;(apply reify-regex match replace (map bindings vars))))
                    (variables vars)))


(defmethod reify-rule-form-fn :fn [rule
                                   [var {[fn-name fcn] :ln
                                         :as           reify-opts}]]
  (extend-reify-map reify-opts
                    (fn [bindings]
                      (with-reify-name-bindings reify-opts
                                                (reify-sym (fcn bindings))))))
;; ideally these should be rigged to go last
;; just move them last after?
;; pull them all off and put them on at the end


(defn default-reify-rule-form-fn []
  (extend-reify-map {}
                    (fn [bindings]
                      (reify-unique))))


(defn reification-dependencies [reify-list]
  (mapcat (fn [[v {deps :dependencies}]]
            (concat `((~v ~nil))
                    (map (partial list v) deps)))
          reify-list))

(defn sort-reification-based-on-dependencies [reify-list]
  (let [original-map (reduce (fn [m [v options]]
                               (assoc m v options))
                             {}
                             reify-list)]
    (remove (fn [[var reify-def]]
              (nil? reify-def))
            (map (fn [var]
                   (vector var (original-map var)))
                 (dep/topo-sort
                   (reduce (fn [graph [var dependency]]
                             (dep/depend graph var dependency))
                           (dep/graph)
                           (reification-dependencies reify-list)))))))


(defn get-reify-list [rule reify-block]
  (map (fn [entry]
         (if (sequential? entry)
           (let [[var opts :as form] entry]
             [var (reify-rule-form-fn rule form)])
           (vector entry (default-reify-rule-form-fn))))
       reify-block))

(defn add-reify-fns [{reify :reify :as rule}]
  (assoc rule
    :reify
    (sort-reification-based-on-dependencies
      (get-reify-list rule reify))))

;;; --------------------------------------------------------
;;; forward chaining
;;; --------------------------------------------------------

;; the variables in the reification section aren't actually independent
;;   some of the variables being reified rely on other variables that
;;   need to be reified.  so they need to be ordered or managed in some way

(defn reify-bindings [reify-with-fns bindings]
  (reduce (fn [new-bindings [var {reify-fn :reify-fn}]]
            ;;check for key in bindings already (rule out optionals)
            (if (new-bindings var)
              new-bindings
              (assoc new-bindings var (reify-fn new-bindings))))
          bindings
          reify-with-fns))

;;instantiates a rule and puts the triples in the target kb
(defn run-forward-rule [source-kb target-kb source-rule]
  (let [{head  :head
         body  :body
         reify :reify
         :as   rule} (add-reify-fns source-rule)
        bindings-fn (fn [bindings]
                      (dorun
                        (map (partial add! target-kb)
                             (doall
                               (subst-bindings head
                                               (reify-bindings reify
                                                               bindings))))))]
    (pprint rule)
    (cond (string? body) (visit-sparql source-kb bindings-fn body)
          :else (query-visit source-kb bindings-fn body {:select-vars (concat (variables head)
                                                                              (variables reify))}))))

(defn ask-forward-rule [source-kb {head :head
                                   body :body
                                   :as  rule}]
  (ask source-kb body))

(defn count-forward-rule [source-kb {head :head
                                     body :body
                                     :as  rule}]
  (query-count source-kb
               body
               {:select-vars (variables head)}))



;; (query-visit source-kb
;;              (fn [bindings]
;;                (prn bindings)
;;                (dorun (map (fn [triple]
;;                              (prn triple)
;;                              (add! target-kb triple))
;;                              ;;(partial add! target-kb)
;;                            (dorun (map (fn [triple]
;;                                          (prn triple)
;;                                          (statement kb triple))
;;                                          ;;(partial statement kb)
;;                                        (reify-assertions
;;                                         (subst-bindings head bindings)))))))
;;              body))







;;; --------------------------------------------------------
;;; helpers
;;; --------------------------------------------------------

(defn reify-variables [{reify :reify :as rule}]
  (map (fn [entry]
         (if (sequential? entry)
           (first entry)
           entry))
       reify))

;;; --------------------------------------------------------
;;; static rule testing
;;; --------------------------------------------------------

(defn all-head-vars-in-body-sans-reify-vars? [{rule-name :name
                                               head      :head
                                               body      :body
                                               :as       rule}]
  (let [head-vars (variables head)
        body-vars (variables body)
        result (every? (set body-vars)
                       (remove (set (reify-variables rule)) head-vars))]
    (if (not result) (println (str "\nWarning all head variables not found in rule body: " rule-name
                                   "\nhead-vars: " (pr-str head-vars)
                                   "\nbody-vars: " (pr-str body-vars))))
    result))


(defn all-reify-vars-in-head? [{rule-name :name
                                head      :head
                                :as       rule}]
  (let [head-vars (variables head)
        reify-vars (reify-variables rule)
        result (every? (set head-vars) reify-vars)]
    (if (not result) (println (str "===============\nWarning all reify variables not found in rule head: " rule-name
                                   "\nhead-vars: " (pr-str head-vars)
                                   "\nreify-vars: " (pr-str reify-vars))))
    result))

(defn all-head-vars-not-in-body-in-reify? [{rule-name :name
                                            head      :head
                                            body      :body
                                            :as       rule}]
  (let [head-vars (variables head)
        body-vars (variables body)
        reify-vars (reify-variables rule)
        result (every? (set reify-vars)
                       (remove (set body-vars) head-vars))]

    (if (not result) (println (str "===============\nWarning some head variables not found in rule body or reify block: " rule-name
                                   "\nhead-vars: " (pr-str head-vars)
                                   "\nreify-vars: " (pr-str reify-vars)
                                   "\nbody-vars: " (pr-str body-vars))))
    result))



(defn all-reify-dependencies-in-reify-or-body? [{rule-name :name
                                                 reify     :reify
                                                 body      :body
                                                 :as       rule}]
  (let [body-vars (variables body)
        reify-vars (reify-variables rule)
        reify-dependencies (remove nil?
                                   (set (flatten
                                          (reification-dependencies
                                            (get-reify-list rule reify)))))
        result (every? (set (concat reify-vars body-vars)) reify-dependencies)]
    (if (not result) (println (str "===============\nWarning some reify dependencies do not exist in either the body or as reify variables: " rule-name
                                   "\nreify-dependencies: " (pr-str reify-dependencies)
                                   "\nreify-vars: " (pr-str reify-vars)
                                   "\nbody-vars: " (pr-str body-vars))))
    result))

(defn forward-safe? [rule]
  (every? (fn [test]
            (test rule))
          (list connected-rule?
                all-head-vars-in-body?)))

(defn forward-safe-with-reification? [rule]
  (every? (fn [test]
            (test rule))
          (list connected-rule?
                all-reify-vars-in-head?
                all-reify-dependencies-in-reify-or-body?
                all-head-vars-not-in-body-in-reify?
                all-head-vars-in-body-sans-reify-vars?)))


(def rdf-constant-to-ns-map (hash-map "AllDifferent" "owl"
                                      "allValuesFrom" "owl"
                                      "AnnotationProperty" "owl"
                                      "backwardCompatibleWith" "owl"
                                      "cardinality" "owl"
                                      "Class" "owl"
                                      "complementOf" "owl"
                                      "DataRange" "owl"
                                      "DatatypeProperty" "owl"
                                      "DeprecatedClass" "owl"
                                      "DeprecatedProperty" "owl"
                                      "differentFrom" "owl"
                                      "disjointWith" "owl"
                                      "distinctMembers" "owl"
                                      "equivalentClass" "owl"
                                      "equivalentProperty" "owl"
                                      "FunctionalProperty" "owl"
                                      "hasValue" "owl"
                                      "imports" "owl"
                                      "incompatibleWith" "owl"
                                      "intersectionOf" "owl"
                                      "InverseFunctionalProperty" "owl"
                                      "inverseOf" "owl"
                                      "maxCardinality" "owl"
                                      "minCardinality" "owl"
                                      "Nothing" "owl"
                                      "ObjectProperty" "owl"
                                      "oneOf" "owl"
                                      "onProperty" "owl"
                                      "Ontology" "owl"
                                      "OntologyProperty" "owl"
                                      "priorVersion" "owl"
                                      "Restriction" "owl"
                                      "sameAs" "owl"
                                      "someValuesFrom" "owl"
                                      "SymmetricProperty" "owl"
                                      "Thing" "owl"
                                      "TransitiveProperty" "owl"
                                      "unionOf" "owl"
                                      "versionInfo" "owl"


                                      "Datatype" "rdfs"
                                      "Resource" "rdfs"
                                      "Container" "rdfs"
                                      "Literal" "rdfs"
                                      "domain" "rdfs"
                                      "range" "rdfs"
                                      "subPropertyOf" "rdfs"
                                      "subClassOf" "rdfs"
                                      "comment" "rdfs"
                                      "label" "rdfs"
                                      "isDefinedBy" "rdfs"
                                      "seeAlso" "rdfs"
                                      "member" "rdfs"


                                      "XMLLiteral" "rdf"
                                      "ContainerMembershipProperty" "rdfs"
                                      "Seq" "rdf"
                                      "Bag" "rdf"
                                      "Alt" "rdf"
                                      "Statement" "rdf"
                                      "Property" "rdf"
                                      "List" "rdf"
                                      "type" "rdf"
                                      "first" "rdf"
                                      "rest" "rdf"
                                      "subject" "rdf"
                                      "predicate" "rdf"
                                      "object" "rdf"
                                      "value" "rdf"
                                      "about" "rdf"
                                      "Description" "rdf"
                                      "resource" "rdf"
                                      "datatype" "rdf"
                                      "ID" "rdf"
                                      "li" "rdf"
                                      "_n" "rdf"
                                      "nodeID" "rdf"
                                      "parseType" "rdf"
                                      "RDF" "rdf"

                                      "base" "xml"
                                      "lang" "xml"
                                      ))


(defn head-property-ns-correct? [{rule-name :name
                                  head      :head
                                  :as       rule}]

  (let [flat-head (flatten head)
        is-as-expected (map (fn [ele]
                              (if (and (symbol? ele)
                                       (not (variable? ele))
                                       (contains? rdf-constant-to-ns-map (name ele)))
                                (if (not= (namespace ele) (rdf-constant-to-ns-map (name ele)))
                                  false
                                  true)))
                            (flatten head))
        result (every? true? (remove nil? is-as-expected))]

    (if (not result) (println (str "===============\nObserved unexpected namespaces "
                                   (pr-str (keys (filter (fn [[k v]] (false? v)) (zipmap flat-head is-as-expected))))
                                   " in rule: " rule-name)))
    result))



(defn variables-with-proper-slashes? [{rule-name :name
                                       head      :head
                                       reify     :reify
                                       body      :body
                                       :as       rule}]
  (let [bad-head-vars (re-find #"(\?[^/].*?)[ \)]" (.toString head))
        bad-body-vars (when (not (string? body)) (re-find #"(\?[^/].*?)[ \)]" (.toString body)))
        bad-reify-vars (when (not (nil? reify)) (re-find #"(\?[^/].*?)[ \)]" (.toString reify)))
        result (every? nil? (list bad-head-vars bad-body-vars bad-reify-vars))]

    (if (not result) (println (str "===============\nWarning, observed at least one variable missing a slash after the '?' in: " rule-name
                                   "\nbad-head-vars: " (pr-str bad-head-vars)
                                   "\nbad-reify-vars: " (pr-str bad-reify-vars)
                                   "\nbad-body-vars: " (pr-str bad-body-vars))))
    result))





;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------



;;; --------------------------------------------------------
;;; 
;;; --------------------------------------------------------


;;; --------------------------------------------------------
;;; END
;;; --------------------------------------------------------


