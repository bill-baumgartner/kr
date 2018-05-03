(ns kr.core.variable
  (use kr.core.utils))

;;(def variable-prefix \?)
(def variable-ns "?")

(def temp-variable-prefix "var")

;; (defn variable-string? [s]
;;   (= (nth s 0) variable-prefix))


(defn variable? [x]
  ;;"Is x a variable (a symbol beginning with '?')?"
  "Is x a variable (a symbol in the variable-ns '?/')?"
  (and (symbol? x)
       (= variable-ns (namespace x))))
       ;;(variable-string? (name x))))


(defn variable [v]
  (cond
   (string? v) (variable (symbol v))
   (variable? v) v
   (symbol? v) (symbol variable-ns (name v))))
   
   ;; (symbol? v) (symbol (namespace v)
   ;;                     (str variable-prefix (name v)))))

(defn temp-variable
  ([]       (variable (gensym temp-variable-prefix)))
  ([prefix] (variable (gensym prefix))))


(defn distinct-elements
  ([elem? expr] (distinct-elements elem? expr #'nonempty-seq #'seq))
  ([elem? expr branch? children]
     (set (filter elem? (tree-seq branch? children expr)))))

(defn variables-from-sparql [query]
  "Extract the variables returned by the select statement in the input query. Note: will not work with 'select *'."
  (when (and (not (.isEmpty query)) (.contains (.toLowerCase query) "select"))
  (map symbol
       (clojure.string/split
         (clojure.string/replace
           (clojure.string/replace
             (clojure.string/trim (nth (re-find #"select (.*)\{"
                                                (.toLowerCase (clojure.string/replace (clojure.string/replace query #"\n" " ") #"\s+" " ")))
                                       1))
             #" where$" "")
           #"\?" "?/")
         #" "))))

(defn variables [expr]
  "If the input expression is a string, then we assume that it is a SPARQL query."
  (cond (string? expr) (variables-from-sparql expr)
        :else (distinct-elements variable? expr)))
;; ([expr] (list-variables expr #'variable? #'nonempty-seq #'seq))
;; ([expr var?] (distinct-elements expr var?))
;; ([expr var? branch? children] (distinct-elements expr var? branch? children))

(defn symbols [expr]
  (distinct-elements symbol? expr))

(defn symbols-no-vars [expr]
  (distinct-elements #(and (symbol? %) (not (variable? %))) expr))

