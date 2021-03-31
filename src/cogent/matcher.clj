(ns cogent.matcher
  (:require [cogent.helper :refer :all]))

(def symbol-guard
  {nil       (constantly true)
   "number"  number?
   "symbol"  symbol?
   "nonzero" (partial not= 0)})

(defn- variable-pattern? [pattern]
  (and (symbol? pattern) (.startsWith (name pattern) "?")))

;; returns list of tuples of [{variable eclass}] where substitutions is a map of var name to canonical id
(defn ematch [egraph pattern]
  (letfn [(match-expr* [pattern class]
            (mapcat (partial match-expr pattern) (get-in egraph [:eclass->enodes class])))
          (match-expr [pattern expression]
            (cond
              (variable-pattern? pattern)
              (let [guard (symbol-guard (namespace pattern))]
                (when (guard expression)
                  [{(symbol (name pattern)) expression}]))

              (and (seq? pattern) (vector? expression) (= (count pattern) (count expression)))
              (->> (map match-expr* pattern expression)
                   (apply cartesian)
                   (keep (partial apply merge-disjunct)))

              (= pattern expression)
              [{}]))]
    (for [[node class] (:enode->eclass egraph)
          var-map (match-expr pattern node)]
      [var-map class])))

;; returns seq of matches for all rules
(defn ematch-rules [rules egraph]
  (for [[lhs rhs]      rules
        [substitutions eclass] (ematch egraph lhs)]
    [eclass rhs substitutions]))

;; rules can be used to pre-calculate matcher truee.
#_(defn preprocess-rules [rules]
  ;; all rules are list. we can group them by prefix.
    (assert (map? rules))
    (group-by (fn [[pattern rhs]]
                (assert (seq? pattern))
                (first pattern))
              rules))