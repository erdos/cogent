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
