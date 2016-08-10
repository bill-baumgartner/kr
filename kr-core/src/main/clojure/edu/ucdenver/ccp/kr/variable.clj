(ns edu.ucdenver.ccp.kr.variable
  (use edu.ucdenver.ccp.utils))

;;(def variable-prefix \?)
(def variable-ns "?")

(def temp-variable-prefix "var")

;; (defn variable-string? [s]
;;   (= (nth s 0) variable-prefix))


(defn math-block? [x]
  "A math-block is a map with keys :num_type, :as, :eqn, and optionally :group_by"
  (and (map? x) (every? x `(:num_type :as :eqn))))

(defn count-block? [x]
  "Is x a count block, e.g. [:count ?/v]?"
   (and (vector? x) (= :count (first x))))

(defn variable? [x]
  "Is x a variable (a symbol in the variable-ns '?/')?"
  (and (symbol? x)
       (= variable-ns (namespace x))))

(defn variable [v]
  (cond
   (string? v) (variable (symbol v))
   (variable? v) v
   (symbol? v) (symbol variable-ns (name v))))
   
(defn temp-variable
  ([]       (variable (gensym temp-variable-prefix)))
  ([prefix] (variable (gensym prefix))))


(defn distinct-elements
  ([elem? expr] (distinct-elements elem? expr #'nonempty-seq #'seq))
  ([elem? expr branch? children]
     (set (filter elem? (tree-seq branch? children expr)))))

(defn variables [expr]
  (distinct-elements variable? expr))
;; ([expr] (list-variables expr #'variable? #'nonempty-seq #'seq))
;; ([expr var?] (distinct-elements expr var?))
;; ([expr var? branch? children] (distinct-elements expr var? branch? children))

(defn count-blocks [expr]
  "return any count-blocks, e.g. [:count ?/v] from the input"
   (distinct-elements count-block? expr))

(defn math-blocks [expr]
  "return any math-blocks, e.g. {:num_type :float :as ?/sum :eqn [?/a / ?/b]} from the input"
   (distinct-elements math-block? expr))

(defn symbols [expr]
  (distinct-elements symbol? expr))

(defn symbols-no-vars [expr]
  "returns symbols that are not sparql variables from the input expr. This
   function is used when constructing sparql queries to extract the 
   symbols whose namespaces must be included in the PREFIX block of
   the query."
  (distinct (concat (distinct-elements #(and (symbol? %) (not (variable? %))) expr)
          ;; If the expr consists of a rule head with a math block then
          ;; we need to add a symbol with the xsd namespace, e.g. xsd/float
          ;; to ensure that the xsd namespace appears in the query PREFIX.
          ;;
          ;; There can be symbols embedded in strings within math blocks,
          ;; e.g. calls to functions. These symbols must be extracted so
          ;; that their namespaces will also appear in the query PREFIX.
          (if (not (empty? (math-blocks expr)))
            (cons 'xsd/float  ;; we add xsd/float here to ensure the xsd ns is included
                  ;; below is a nested map structure. The outer map takes extracted
                  ;; symbols as strings and replaces ':' with '/' and then creates symbols
                  (map #(if (string? %) (symbol (clojure.string/replace % #":" "/")))
                       (keep identity ;; keep identity removes nil from the list
                             (flatten
                              ;; The inner map runs a regex over each string component
                              ;; of the math block equations and extracts all symbols
                              ;; present as strings
                              (map (fn [x] (if (string? x) (re-seq #"\b\w+:\w+\b" x)))
                                   (flatten (map :eqn (math-blocks expr))))))))))))


