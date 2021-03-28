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
  (letfn [(match-expr* [pattern class]
            (mapcat (partial match-expr pattern) (get-in egraph [:eclass->enodes class])))
          (match-expr [lhs expr]
            (cond
              (and (symbol? lhs) (.startsWith (name lhs) "?"))
              [{lhs expr}]

              (and (seq? lhs) (seq? expr) (= (count lhs) (count expr)))
              (->> (map match-expr* lhs (map (:enode->eclass egraph) expr))
                   (apply cartesian)
                   (keep (partial apply merge-disjunct)))

              (= lhs expr)
              [{}]))]
    (for [node (keys (:enode->eclass egraph))
          var-map (match-expr lhs node)]
      [var-map (get-in egraph [:enode->eclass node])])))


;; returns tuple of [graph, class2]
(defn egraph-add
  ([egraph value]
   (if (contains? (:enode->eclass egraph) value)
     egraph
     (egraph-add (vary-meta egraph update :next-idx inc)
                 (-> egraph meta :next-idx)
                 value)))
  ([egraph eclass value]
   (assert (int? eclass) (str "Not class  " (pr-str eclass)))
   (assert (every? sequential? (vals (:eclass->enodes egraph))))
   (if-let [old-class (get-in egraph [:enode->eclass value])]
     (if (= eclass old-class)
       egraph
       ;; (assert false "Not happening.")
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
 (println :step )
  (doseq [[k v] (sort (:eclass->enodes egraph))]
    (println " -" k ":"  v))
  (reduce (fn [egraph [eclass value]] (egraph-add egraph eclass value))
          egraph
          (for [[lhs rhs]      rewrites
                [subst eclass] (ematch egraph lhs)
                ;_ (println :adding lhs :=> subst)
                ]
            [eclass (rhs-substitute rhs subst)])))


(defn equality-saturation [expr rewrites]
  (fixpt (partial equality-saturation-step rewrites) (initial-egraph expr)))

(defn congruent?
  ([form1 form2]
   (congruent? (equality-saturation form1 *rules*) form1 form2))
  ([egraph form1 form2]
   (let [class1 (->> (ematch egraph form1) (map second) set)
         class2 (->> (ematch egraph form2) (map second) set)]
     (println :1 form1 (ematch egraph form1))
     (println :2 form2 (ematch egraph form2))
     (or
      (and (= 1 (count class1))
           (= class1 class2))
      (println "Not equals" form1 form2 egraph)
      false))))

(defn tautology? [expression]
  (congruent? expression true))


;; return bindings for solved form
(defn solve [egraph form])


#_(-> '(* 1 (* 3 (* 1 0)))
      (equality-saturation rules/rules)
      (println))
