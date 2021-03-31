(ns cogent.helper-test
  (:require [clojure.test :refer :all]
            [cogent.helper :refer [merge-disjunct scalar?]]))


(deftest test-merge-disjunct
  (testing "Empty maps"
    (is (= {} (merge-disjunct {} {})))
    (is (= {:a 1} (merge-disjunct {} {:a 1}) (merge-disjunct {:a 1} {}))))
  (testing "All intersecing entries are the"
    (is (= {:a 1 :b 2} (merge-disjunct {:a 1} {:a 1 :b 2}))))
  (testing "Common keys have different values"
    (is (= nil (merge-disjunct {:a 1} {:a 2})))
    (is (= nil (merge-disjunct {:a 1 :c 4} {:a 2 :b 3})))))


(deftest scalar?-test
  (is (every? scalar? [-1 0 1 2.3 true false]))
  (is (not-any? scalar? '[a :kw x/y nil {} [] (a b c) (1)])))
