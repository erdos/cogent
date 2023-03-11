(ns cogent.matcher
  (:require [cogent.egraph :refer [get-all-eclasses get-eclass get-enodes]]
            [cogent.helper :refer :all]))

(defn- variable-pattern? [pattern]
  (and (symbol? pattern) (.startsWith (name pattern) "?")))

(defn get-guard [pattern]
  (case (and (variable-pattern? pattern) (namespace pattern))
    "number"  number?
    "symbol"  symbol?
    "nonzero" (partial not= 0)
    nil))

(defrecord Resolved [value]) ;; vector of class ids or scalar
(defrecord Unresolved [class])

;; Tries to match pattern against class.
;; Returns seq of {sym -> resolved/unresolved} bindings.
(defn- ematch-1 [egraph pattern class]
  (cond (get-guard pattern)
        (let [guard-fn       (get-guard pattern)
              pattern-symbol (symbol (name pattern))]
          (for [value (get-enodes egraph class)
                :when (guard-fn value)]
            {pattern-symbol (->Resolved value)}))
                  ;; this is very naive. what if current node's parent is already part of a group.
                  ;; but by passing this rule, it would be part of another group also but the two would not show up.
                  ;; TODO: test this theory.
        (variable-pattern? pattern)
        [{pattern (->Unresolved class)}]

        (seq? pattern)
        (for [node (get-enodes egraph class)
              :when (vector? node)
              :when (= (count pattern) (count node))
              m (->> (map (partial ematch-1 egraph) pattern node)
                     (apply cartesian)
                     (keep (partial apply merge-disjunct)))]
          m)

        :else ;; if pattern is a scalar value then values must be in same eclass
        (when (= class (get-eclass egraph pattern))
          [{}])))

;; Matches all classes in egraph against pattern.
;; Returns eduction of [{sym -> resolved/unresolved} class] pairs.
(defn ematch [egraph pattern]
  (eduction (mapcat
             (fn [class]
                 ; (eduction (map (fn [m] [m c])) (matching pattern c))
               (for [m (ematch-1 egraph pattern class)] [m class])))
            (get-all-eclasses egraph)))

;; Matches all rules against all classes in egraph.
;; Returns seq of [class created-node] pairs.
(defn ematch-rules [rules egraph]
  (for [[lhs rhs]      rules
        [substitutions eclass] (ematch egraph lhs)]
    [eclass (rhs substitutions)
    #_ (println "Match " lhs substitutions (-> rhs meta :cogent.rules/rhs) eclass)
     ]))
