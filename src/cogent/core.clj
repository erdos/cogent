(ns cogent.core
  (:require [clojure.walk]
            [cogent.helper :refer :all]
            [cogent.rules :as rules]
            [cogent.union-find :as union-find]))

;;
;; See: egg: Fast and Extensible Equality Saturation
;; https://arxiv.org/pdf/2004.03082.pdf
;;
;;
;; Todo: check Tarjan's union find algorithm
;; 

(declare debug)


(def ^:dynamic *rules* cogent.rules/rules)


(def empty-graph
  (assoc union-find/empty-set
         :enode->eclass {}
         :eclass->enodes {}))


(defn- rhs-substitute [rhs subst]
  (assert (map? subst))
  (clojure.walk/postwalk-replace subst rhs))


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


(defn- fix-unions [egraph]
  (->>
   (:enode->eclass egraph)
   (reduce-kv (fn [m old-node old-class]
                (let [old-class (union-find/find-class egraph old-class)
                      new-node (if (vector? old-node)
                                 (mapv (partial union-find/find-class egraph) old-node)
                                 old-node)]
                  (update m new-node (fnil conj #{}) old-class)))
              {})
   (vals)
   (reduce (fn [egraph old-classes]
             (first
              (reduce (fn [[egraph class1] class2]
                        (union-find/merge-set egraph class1 class2))
                      [egraph (first old-classes)]
                      (next old-classes))))
           egraph)))


(defn rebuild [egraph]
  (println :before-rebuilding)
  (debug egraph)
  ; (println :rebuilding @(::union-find/parents egraph))
  (let [egraph (fixpt fix-unions egraph)
        _ (println :unions-fixed) _ (debug egraph)
        [new-graph rename-class] (union-find/compact egraph)
        rename-node (fn [old-node]
                      (if (vector? old-node)
                        (mapv rename-class old-node)
                        old-node))]
    (println :rename-class (into (sorted-map) rename-class))
    (if (= egraph new-graph)
      egraph
      (reduce-kv (fn [new-graph old-node old-class]
                   (let [new-node  (rename-node old-node)
                         new-class (rename-class old-class)]
                     (when-let [existing-classs (get (:enode->eclass new-graph) new-node)]
                       ;; each node should be uniquely mapped...
                       (assert (= existing-classs new-class)
                               (str new-node " : "  existing-classs " vs " new-class)))
                     (-> new-graph
                         (update :eclass->enodes update new-class (fnil conj #{}) new-node)
                         (update :enode->eclass assoc new-node new-class))))
                 new-graph
                 (:enode->eclass egraph)))))


(def ^:private kill (atom 0))


(defn- equality-saturation-step [rewrites egraph]
;  (println :step)
;  (debug egraph)
  (assert (< (swap! kill inc) 100) "Too many iterations!")
  (doseq  [[class nodes] (:eclass->enodes egraph)
           node nodes
           :when (vector? node)
           v node]
    ;; every referenced item is found.
    (assert (contains? (:eclass->enodes egraph) v)
            (str "Unexpected id: " v " << " (union-find/find-class egraph v))))

  (reduce (fn [egraph [eclass value]]
            (let [[egraph new-id] (add-canonical egraph value)
                  [egraph _ _] (union-find/merge-set egraph new-id eclass)]
              egraph))
          egraph
          (for [[lhs rhs]      rewrites
                [subst eclass] (ematch egraph lhs)]
            [eclass (rhs-substitute rhs subst)])))


;; checks that each class may contain exactly 1 scalar value
;; otherwise throws exception.
(defn- check-graph-contradiction [egraph]
  (println :after-rebuilding)
  (debug egraph)
  (reduce-kv (fn [m k v]
               (if (scalar? k)
                 (if (contains? m v)
                   (throw (ex-info "Contradiction has been found" {:v v}))
                   (conj m v))
                 m))
             #{}
             (:enode->eclass egraph))
  egraph)


(defn graph-equality-saturation [egraph rewrites]
  (reset! kill 0)
  (fixpt (comp check-graph-contradiction
               rebuild
               (partial equality-saturation-step rewrites))
         egraph))


(defn debug [egraph]
  (println "Egraph")

  (println (union-find/all-parents egraph))
  (println (into (sorted-map) (:eclass->enodes egraph)))
  (doseq [[k v] (sort (:eclass->enodes egraph))]
    (apply println "-" k "\t:"  (sort-by (juxt vector? boolean? number? symbol? identity) v)))
  (println "---"))

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