(ns cogent.union-find-test
  (:require [clojure.test :refer :all]
            [cogent.union-find :refer :all]))


(deftest test-1
  (let [e        empty-set
        [e id0]  (make-set e)
        _        (assert (= 0 id0))
        [e id1]  (make-set e)
        _        (assert (= 1 id1))
        [e id2]  (make-set e)
        _        (assert (= 2 id2))
        [e id3]  (make-set e)
        _        (assert (= 3 id3))
        [e id01] (merge-set e id1 id0)]
    (is (= id01 (find-class e id0) (find-class e id1)))
    (is (= id01 (min id0 id1)))))

(deftest test-recursive
  (let [e (-> empty-set
              make-set first
              make-set first
              make-set first
              (merge-set 0 1) first
              (merge-set 1 0) first)]
    ;; circular reference
    (is (= 0 (find-class e 0)))
    (is (= 0 (find-class e 1)))
    ;; unchanged
    (is (= 2 (find-class e 2)))))