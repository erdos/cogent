(ns cogent.union-find
  "Immutable union-set data structure
   
   https://dl.acm.org/doi/10.1145/321879.321884
   ")

(def empty-set {::parents (delay [])})

;; returns tuple of [graph new-id]
(defn make-set [data]
  (let [new-id (count @(::parents data))]
    [(assoc data ::parents (atom (conj @(::parents data) new-id)))
     new-id]))

#_(defn parent [data id]
    (get (::parents data) id))

(defn merge-set [data old-parent new-parent]
  (assert (map? data))
  (assert (integer? old-parent))
  (assert (integer? new-parent))
  (if (= old-parent new-parent)
    [data old-parent]
    (let [parent (min old-parent new-parent)
          child (max old-parent new-parent)]
      [(assoc data ::parents (atom (assoc @(::parents data) child parent)))
       parent])))

(defn find-class [data current]
  (assert (< current (count @(::parents data))))
  (loop [current current]
    (let [parent (get @(::parents data) current)]
      (if (= current parent)
        parent
        (let [grandparent (get @(::parents data) parent)]
          (swap! (::parents data) assoc current grandparent)
          (recur grandparent))))))

(defn all-parents [data]
  (assert (::parents data))
  (mapv (partial find-class data) (range (count @(::parents data)))))

;; compacts set to a form where every union is represented by one id exactly.
;; returns tuple of [new-data remplacements-map]
(defn compact [data]
  (assert (::parents data))
  (let [max-id (count @(::parents data))]
    (loop [idx 0
           idx->parent {}
           parent->newid {}]
      (if (< idx max-id)
        (let [parent (find-class data idx)
              idx->parent (assoc idx->parent idx parent)
              parent->newid (if (contains? parent->newid parent)
                              parent->newid
                              (assoc parent->newid parent (count parent->newid)))]
          (recur (inc idx) idx->parent parent->newid))
        (if (= (count parent->newid) max-id)
          [data
           nil]
          [{::parents (atom (vec (range (count parent->newid))))}
           (reduce-kv (fn [m k v] (assoc m k (parent->newid v))) {} idx->parent)])))))
