(ns edu.ucdenver.ccp.kr.variable
  (use edu.ucdenver.ccp.utils))

;;(def variable-prefix \?)
(def variable-ns "?")

(def temp-variable-prefix "var")

;; (defn variable-string? [s]
;;   (= (nth s 0) variable-prefix))


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
   
   ;; (symbol? v) (symbol (namespace v)
   ;;                     (str variable-prefix (name v)))))

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

(defn symbols [expr]
  (distinct-elements symbol? expr))

(defn symbols-no-vars [expr]
  (distinct-elements #(and (symbol? %) (not (variable? %))) expr))

