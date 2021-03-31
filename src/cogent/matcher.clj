(ns cogent.matcher
  (:require [cogent.helper :refer :all]))


;; returns list of tuples of [{variable eclass}] where substitutions is a map of var name to canonical id
(defn ematch [egraph pattern]
  (letfn [(match-expr*
            [pattern class]
            (mapcat (partial match-expr pattern) (get-in egraph [:eclass->enodes class])))
          (match-expr
            [pattern expression]
            (cond
              (and (symbol? pattern) (.startsWith (name pattern) "?"))
              (case (namespace pattern)
                "number"
                (when (number? expression)
                  [{(symbol (name pattern)) expression}])

                "symbol"
                (when (symbol? expression)
                  [{(symbol (name pattern)) expression}])

                nil
                [{pattern expression}])

              (and (seq? pattern) (vector? expression) (= (count pattern) (count expression)))
              (->> (map match-expr* pattern expression)
                   (apply cartesian)
                   (keep (partial apply merge-disjunct)))

              (= pattern expression)
              [{}]))]
    (for [node (keys (:enode->eclass egraph))
          var-map (match-expr pattern node)]
      [var-map (get-in egraph [:enode->eclass node])])))


;; returns seq of matches for all rules
(defn ematch-rules [rules egraph]
  (for [[lhs rhs]      rules
        [substitutions eclass] (ematch egraph lhs)]
    [eclass rhs substitutions]))

;; rules can be used to pre-calculate matcher truee.