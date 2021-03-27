(ns cogent.core
  (:require [clojure.walk]
            [cogent.rules :as rules]))

;;
;; See: egg: Fast and Extensible Equality Saturation
;; https://arxiv.org/pdf/2004.03082.pdf
;;
;;
;; Todo: check Tarjan's union find algorithm
;; https://dl.acm.org/doi/10.1145/321879.321884

(def empty-graph
  ^{:next-idx 0}
  {:enode->eclass {}
   :eclass->enodes {}})

(declare egraph-add)

(defn initial-egraph [expr]
  (egraph-add empty-graph expr))

(defn- rhs-substitute [rhs subst]
  (clojure.walk/postwalk-replace subst rhs))

;; cartesian product of collections
(defn cartesian
  ([as bs]    (for [a as b bs] [a b]))
  ([as bs cs] (for [a as b bs c cs] [a b c])))

(defn merge-disjunct [m & ms]
  (reduce (partial reduce-kv
                   (fn [m k v]
                     (if (and (contains? m k) (not= v (m k)))
                       (reduced nil)
                       (assoc m k v))))
          m ms))

;; if match found: returns list of tuples of [substitutions eclass]
(defn ematch [egraph lhs]
  (letfn [(match-expr [lhs expr]
            (cond
              (and (symbol? lhs) (.startsWith (name lhs) "?"))
              [{lhs expr}]

              (and (seq? lhs) (seq? expr) (= (count lhs) (count expr)))
              (keep (partial apply merge-disjunct) (apply cartesian (map match-expr lhs expr)))

              (= lhs expr)
              [{}]))]
    (for [node (keys (:enode->eclass egraph))
          bind (match-expr lhs node)]
      [bind (get-in egraph [:enode->eclass node])])))


;; returns tuple of [graph, class2]
(defn egraph-add
  ([egraph value]
   (egraph-add (vary-meta egraph update :next-idx inc)
               (-> egraph meta :next-idx)
               value))
  ([egraph eclass value]
   (assert (int? eclass) (str "Not class  " (pr-str eclass)))
   (assert (every? sequential? (vals (:eclass->enodes egraph))))
   (if-let [old-class (get-in egraph [:enode->eclass value])]
     (if (= eclass old-class)
       egraph
       (-> egraph
           (update :eclass->enodes dissoc old-class)
           (update-in [:eclass->enodes eclass] into
                      (get-in egraph [:eclass->enodes old-class]))
           (assoc-in [:enode->eclass value] eclass)
           (update :enode->eclass
                   into
                   (map vector
                        (get-in egraph [:eclass->enodes old-class])
                        (repeat eclass)))))
     (reduce egraph-add
             (-> egraph
                 (assoc-in [:enode->eclass value] eclass)
                 (update-in [:eclass->enodes eclass] conj value))
             (when (coll? value) (seq value))))))

(defn- fixpt [mapping value]
  (assert (ifn? mapping))
  (let [mapped (mapping value)]
    (if (= mapped value)
      mapped
      (recur mapping mapped))))


(defn- equality-saturation-step [rewrites egraph]
  (reduce (fn [egraph [eclass value]] (egraph-add egraph eclass value))
          egraph
          (for [[lhs rhs]      rewrites
                [subst eclass] (ematch egraph lhs)]
            [eclass (rhs-substitute rhs subst)])))


(defn equality-saturation [expr rewrites]
  (fixpt (partial equality-saturation-step rewrites) (initial-egraph expr)))

(defn congruent? [egraph form1 form2]
  ;; get classes of both, check if these are congruent
  )

;; return bindings for solved form
(defn solve [egraph form])

(defn tautology? [egraph expression]
  (congruent? egraph expression true))

(-> '(* 1 (* 3 (* 1 0)))
    (equality-saturation rules/rules)
    (println))

