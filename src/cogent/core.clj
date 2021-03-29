(ns cogent.core
  (:require [clojure.walk]
            [cogent.rules :as rules]
            [cogent.union-find :as union-find]))

;;
;; See: egg: Fast and Extensible Equality Saturation
;; https://arxiv.org/pdf/2004.03082.pdf
;;
;;
;; Todo: check Tarjan's union find algorithm
;; https://dl.acm.org/doi/10.1145/321879.321884


(declare debug)

(def ^:dynamic *rules* cogent.rules/rules)

(def empty-graph
  (assoc union-find/empty-set
         :enode->eclass {}
         :eclass->enodes {}))

(defn- rhs-substitute [rhs subst]
  (assert (map? subst))
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
    (if-let [existing-class (get (:enode->eclass egraph) canonical)]
      [egraph existing-class] ;; (union-find/find-class egraph existing-class)
      (let [[egraph new-class] (union-find/make-set egraph)]
        [(-> egraph
             (update :eclass->enodes assoc new-class #{canonical})
             (update :enode->eclass assoc canonical new-class))
         new-class]))))

;; returns egraph
;; expression is not canonical!
(defn add-canonical-id [egraph expression class]
  (let [[egraph canon-expr]
        (if (seq? expression)
          (reduce (fn [[egraph canon-expr] elem]
                    (let [[egraph class] (add-canonical egraph elem)]
                      [egraph (conj canon-expr class)])) [egraph []] expression)
          [egraph expression])
        existing-class (get (:enode->eclass egraph) canon-expr)

        egraph (update egraph :eclass->enodes update class (fnil conj #{}) canon-expr)
        egraph (update egraph :enode->eclass assoc canon-expr class)]
    (if existing-class
      (first (union-find/merge-set egraph existing-class class))
      egraph)))


(defn initial-egraph [expr] (first (add-canonical empty-graph expr)))

;; returns list of tuples of [{variable eclass}] where substitutions is a map of var name to canonical id
(defn ematch [egraph pattern]
  (letfn [(match-expr*
            [pattern class]
            (mapcat (partial match-expr pattern) (get-in egraph [:eclass->enodes class])))
          (match-expr
            [pattern expression]
           ;(println :matching pattern expression)
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

;; rebuild indices
(defn rebuild [egraph]
  ;(println :rebuild egraph)
  #_(println "Before rebuild")
;  (debug egraph)
  (letfn [(normalize-node
            [node]
            (if (vector? node)
              (mapv (partial union-find/find-class egraph) node)
              node))
          (normalize-class [class] (union-find/find-class egraph class))]
    (-> egraph
        (update :enode->eclass
                (fn [m] (zipmap (map normalize-node (keys m))
                                (map normalize-class (vals m)))))
        (update :eclass->enodes
                (fn [m]
                  ;; if keys resolve to same value then we merge them.
                  (reduce-kv (fn [m k v]
                               (update m
                                       (normalize-class k)
                                       (fnil into #{})
                                       (map normalize-node v)))
                             {} m)))
        )))

(defn- fixpt [mapping value]
  (assert (ifn? mapping))
  (let [mapped (mapping value)]
    (if (= mapped value)
      mapped
      (recur mapping mapped))))

(def kill (atom 0))

(defn- equality-saturation-step [rewrites egraph]
  (println :step)
  (debug egraph)
  (assert (< (swap! kill inc) 20) "Too many iterations!")
  (reduce (fn [egraph [eclass value]] (add-canonical-id egraph value eclass))
          egraph
          (for [[lhs rhs]      rewrites
                [subst eclass] (ematch egraph lhs)]
            [eclass (rhs-substitute rhs subst)])))

(defn graph-equality-saturation [egraph rewrites]
  (reset! kill 0)
  (fixpt (comp rebuild (partial equality-saturation-step rewrites)) egraph))

(defn debug [egraph]
  (println "Egraph")
  ; (println @(::union-find/parents egraph))
  (doseq [[k v] (sort (:eclass->enodes egraph))]
    (println " -" k ":"  v)))

(defn congruent?
  ([form1 form2]
   (let [egraph (-> empty-graph
                    (add-canonical form1) (first)
                    ;(doto (debug))
                    (add-canonical form2) (first)
                    ;(doto (debug))
                    (graph-equality-saturation *rules*))
         class1 (->> (ematch egraph form1) (map second) set)
         class2 (->> (ematch egraph form2) (map second) set)]
     ;(println :1 form1 (ematch egraph form1))
     ;(println :2 form2 (ematch egraph form2))
     (or
      (and (= 1 (count class1))
           (= class1 class2))
      (do
        (println "Not equals!")
        (println form1 class1)
        (println form2 class2)
        (debug egraph)
        false)))))

(defn tautology? [expression]
  (congruent? expression true))

(defn- scalar? [x] (or (number? x) (boolean? x)))

(def ^:private logical-ops '#{not and or = < >})

;; return solutions for x
(defn solve [form]
  (-> empty-graph
      (add-canonical form) (first)
      (graph-equality-saturation *rules*)
      (ematch '(= x ?x))
      (->> (keep (fn [x]
                   (let [v (get (first x) '?x)]
                     (when (scalar? v)
                       (first x))))))))

#_
(-> '(* 1 (* 3 (* 1 0)))
      (equality-saturation rules/rules)
      (println))
