(ns cogent.helper)


(defn fixpt [mapping value]
  (assert (ifn? mapping))
  (let [mapped (mapping value)]
    (if (= mapped value)
      mapped
      (recur mapping mapped))))


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


(defn scalar? [x] (or (number? x) (boolean? x)))

;; faster mapv
(defn mapv' [f ^clojure.lang.IPersistentVector v]
  (case (.length v)
    2
    (let [v0 (.nth v 0)
          fv0 (f v0)
          v1 (.nth v 1)
          fv1 (f v1)]
      (if (and (identical? v0 fv0) (identical? v1 fv1))
        v
        [fv0 fv1]))
    3
    (let [v0 (.nth v 0)
          fv0 (f v0)
          v1 (.nth v 1)
          fv1 (f v1)
          v2 (.nth v 2)
          fv2 (f v2)]
      (if (and (identical? v0 fv0) (identical? v1 fv1) (identical? v2 fv2))
        v
        [fv0 fv1 fv2]))))


(defn countif [predicate xs]
  (reduce (fn [sum elem] (if (predicate elem) (inc sum) sum)) 0 xs))