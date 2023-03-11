(ns cogent.egraph
  (:require [cogent.union-find :as union-find]))


(def empty-graph
  (assoc union-find/empty-set
         :enode->eclass {}
         :eclass->enodes {}))

(defn egraph? [x]
  (and (map? x)
       (contains? x :enode->eclass)
       (contains? x :eclass->enodes)))

(defn add-enode [egraph eclass enode]
  (-> egraph
      (update :eclass->enodes update eclass (fnil conj #{}) enode)
      (update :enode->eclass assoc enode eclass)))


(defn get-eclass [egraph enode]
  (get (:enode->eclass egraph) enode))

(defn get-enodes [egraph eclass]
  (get (:eclass->enodes egraph) eclass))

(defn debug [egraph]
  (println "Egraph")
  (println (union-find/all-parents egraph))
  (println (into (sorted-map) (:eclass->enodes egraph)))
  (doseq [[k v] (sort (:eclass->enodes egraph))]
    (apply println "-" k "\t:"  (sort-by (juxt vector? boolean? number? symbol? identity) v)))
  (println "---"))