(ns cogent.union-find)

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