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

(def ^:dynamic *rules* cogent.rules/rules)

(def empty-graph
  ^{:next-idx 0}
  {:enode->eclass {}
   :eclass->enodes {}})

(declare egraph-add add-canonical)

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


;; return tuple if [graph class]
(defn add-canonical [egraph expression]
  ;; expression is not canonical!
  (let [[egraph canonical]
        (if (seq? expression)
          (reduce (fn [[egraph elems] elem]
                    (let [[egraph elem-class] (add-canonical egraph elem)]
                      [egraph (conj elems elem-class)]))
                  [egraph []]
                  expression)
          [egraph expression])]
    (assert (not (map? canonical)))
    (if-let [existing-class (get (:enode->eclass egraph) canonical)]
      [egraph existing-class]
      (let [new-class (-> egraph meta :next-idx (or 0))]
        [(-> egraph
             (update :eclass->enodes update new-class conj canonical)
             (update :enode->eclass assoc canonical new-class)
             (vary-meta update :next-idx (fnil inc 0)))
         new-class]))))

;; returns graph.
(defn add-canonical-id [egraph expression class]
  (let [[egraph canon-expr]
        (if (seq? expression)
          (reduce (fn [[egraph canon-expr] elem]
                    (let [[egraph class] (add-canonical egraph elem)]
                      [egraph (conj canon-expr class)])) [egraph []] expression)
          [egraph expression])]
      ;; add expression with given id.
    (if-let [existing-class (get (:enode->eclass egraph) canon-expr)]
      (if (= existing-class class)
        egraph
        (-> egraph ;; merge them
          ;; remove old class
            (update :eclass->enodes dissoc existing-class)
          ;; rename existing occurrences of old id everywhere
            (update :eclass->enodes (fn [m]
                                      (zipmap (keys m)
                                              (for [v (vals m)]
                                                (for [v v]
                                                  (if (vector? v)
                                                    (mapv (fn [x] (if (= x existing-class) class x)) v)
                                                    v))))))
            (update :enode->eclass (fn [m] (zipmap
                                            (for [v (keys m)]
                                              (if (vector? v)
                                                (mapv (fn [x] (if (= x existing-class) class x)) v)
                                                v))
                                            (vals m))))

          ;; add new  item
            (update :eclass->enodes assoc class (conj (get-in egraph [:eclass->enodes existing-class])
                                                      canon-expr))
            (update :enode->eclass assoc canon-expr class)
          ;; bump next id
            (vary-meta update :next-idx (fnil max 1) (inc class))))
      (-> egraph
          (update :eclass->enodes update class conj canon-expr)
          (update :enode->eclass assoc canon-expr class)
          (vary-meta update :next-idx (fnil max 1) (inc class))))))


(defn initial-egraph [expr] (first (add-canonical empty-graph expr)))

;; returns list of tuples of [{variable eclass}] where substitutions is a map of var name to canonical id
(defn ematch [egraph pattern]
  (letfn [(match-expr*
           [pattern class]
           (mapcat (partial match-expr pattern) (get-in egraph [:eclass->enodes class])))
          (match-expr
           [pattern expression]
           (cond
             (and (symbol? pattern) (.startsWith (name pattern) "?"))
             [{pattern expression}]

             (and (seq? pattern) (vector? expression) (= (count pattern) (count expression)))
             (->> (map match-expr* pattern expression)
                  (apply cartesian)
                  (keep (partial apply merge-disjunct)))

             (= pattern expression)
             [{}]))]
    (for [node (keys (:enode->eclass egraph))
          var-map (match-expr pattern node)]
      [var-map (get-in egraph [:enode->eclass node])])))


(defn- fixpt [mapping value]
  (assert (ifn? mapping))
  (let [mapped (mapping value)]
    (if (= mapped value)
      mapped
      (recur mapping mapped))))

(def kill (atom 0))

(defn- equality-saturation-step [rewrites egraph]
  (println :step)
  (doseq [[k v] (sort (:eclass->enodes egraph))]
    (println " -" k ":"  v)) 
  (assert (< (swap! kill inc) 100) "Too many iterations!")
  (reduce (fn [egraph [eclass value]] (add-canonical-id egraph value eclass))
          egraph
          (for [[lhs rhs]      rewrites
                [subst eclass] (ematch egraph lhs)
                ;_ (println :adding lhs :=> subst)
                ]
            [eclass (rhs-substitute rhs subst)])))


(defn equality-saturation [expr rewrites]
  (reset! kill 0)
  (fixpt (partial equality-saturation-step rewrites) (initial-egraph expr)))

(defn congruent?
  ([form1 form2]
   (congruent? (equality-saturation form1 *rules*) form1 form2))
  ([egraph form1 form2]
   (let [class1 (->> (ematch egraph form1) (map second) set)
         class2 (->> (ematch egraph form2) (map second) set)]
     ;(println :1 form1 (ematch egraph form1))
     ;(println :2 form2 (ematch egraph form2))
     (or
      (and (= 1 (count class1))
           (= class1 class2))
      (println "Not equals" form1 form2 egraph)
      false))))

(defn tautology? [expression]
  (congruent? expression true))


;; return bindings for solved form
(defn solve [egraph form])


#_
(-> '(* 1 (* 3 (* 1 0)))
      (equality-saturation rules/rules)
      (println))
