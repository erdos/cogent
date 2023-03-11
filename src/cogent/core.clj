(ns cogent.core
  (:require [cogent.egraph :refer :all]
            [cogent.helper :refer :all]
            [cogent.matcher :refer [ematch ematch-rules]]
            [cogent.rules :as rules]
            [cogent.union-find :as union-find]))

(def ^:dynamic *rules* cogent.rules/rules)

;; return tuple if [graph class]
(defn add-canonical [egraph expression]
  (assert (some? expression))
  (cond
    (instance? cogent.matcher.Unresolved expression)
    [egraph (.class expression)]

    (seq? expression)
    (apply into-egraph
           (reduce (fn [[egraph elems] elem]
                     (let [[egraph elem-class] (add-canonical egraph elem)]
                       [egraph (conj elems elem-class)]))
                   [egraph []]
                   expression))

    :else
    (let [expression (if (instance? cogent.matcher.Resolved expression) (.value expression) expression)]
      (into-egraph egraph expression))))

(defn- add-canonical-id [egraph new-value eclass]
  (assert egraph)
  (assert (some? new-value))
  (assert (integer? eclass))
  (let [[egraph new-id] (add-canonical egraph new-value)
        _ (assert new-id)
        [egraph _ _] (union-find/merge-set egraph new-id eclass)]
    (assert new-id)
    egraph))


(defn- fix-unions-step [egraph]
  (->>
   (:enode->eclass egraph)
   (reduce-kv (fn [m old-node old-class]
                (let [old-class (union-find/find-class egraph old-class)
                      new-node (if (vector? old-node)
                                 (mapv' (partial union-find/find-class egraph) old-node)
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


(defn- fix-unions [egraph] (fixpt fix-unions-step egraph))


(defn rebuild [egraph]
  (let [egraph (fix-unions egraph)
        [new-graph rename-class] (union-find/compact egraph)
        rename-node (fn [old-node]
                      (if (vector? old-node)
                        (mapv' rename-class old-node)
                        old-node))]
    (if (= egraph new-graph)
      egraph
      (reduce-kv (fn [new-graph old-node old-class]
                   (let [new-node  (rename-node old-node)
                         new-class (rename-class old-class)]
                     (assert (integer? new-class))
                     (when-let [existing-class (get-eclass new-graph new-node)]
                       ;; each node should be uniquely mapped...
                       (assert (= existing-class new-class)
                               (str new-node " : "  existing-class " vs " new-class)))
                     (add-enode new-graph new-class new-node)))
                 new-graph
                 (:enode->eclass egraph)))))


(def ^:private kill (atom 0))

(defn- equality-saturation-step [egraph]
  (assert (< (swap! kill inc) 100) "Too many iterations!")
  (reduce (fn [egraph [eclass value]]
            (add-canonical-id egraph value eclass))
          egraph
          (ematch-rules *rules* egraph)))


(defn- check-graph-inconsistency [egraph]
  ;; (debug egraph)
  ;; if different scalars are in the same class
  (doseq [values (vals (:eclass->enodes egraph))
          :let [values (set (for [v values] (if (number? v) (double v) v)))]]
    (when (< 1 (countif scalar? values))
      (debug egraph)
      (throw (ex-info "Inconsistency was found!" {:scalars (filter scalar? values)}))))
  egraph)


(defn graph-equality-saturation [egraph]
  (reset! kill 0)
  (fixpt (comp check-graph-inconsistency
               rebuild
               equality-saturation-step)
         egraph))


(defn congruent?
  ([form1 form2]
   (assert (some? form1)) (assert (some? form2))
   (let [egraph (-> empty-graph
                    (add-canonical form1) (first)
                    (add-canonical form2) (first)
                    (graph-equality-saturation))
         class1 (into #{} (map second) (ematch egraph form1))
         class2 (into #{} (map second) (ematch egraph form2))]
     (or (and (= 1 (count class1))
              (= class1 class2))
         (do (println "Not equals!")
             (println form1 class1)
             (println form2 class2)
             (debug egraph)
             false)))))


(defn tautology? [expression]
  (congruent? expression 'true))

(defn contradiction? [expression]
  (congruent? expression 'false))

; (def ^:private logical-ops '#{not and or = < >})

;; return solutions for x
(defn solve [form]
  (let [egraph (-> empty-graph
                   (add-canonical form) (first)
                   (graph-equality-saturation))]
    (->>
     (ematch egraph '(= x ?x))
     (mapcat (fn [[mapping _]]
               (let [v (get mapping '?x)]
                 (when (instance? cogent.matcher.Unresolved v)
                   (->> (.class ^cogent.matcher.Unresolved v)
                        (get-enodes egraph))))))
     (remove vector?)
     (set))))

