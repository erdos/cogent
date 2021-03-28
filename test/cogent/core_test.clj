(ns cogent.core-test
  (:require [clojure.test :refer :all]
            [cogent.core :refer :all]))


(deftest test-merge-disjunct
  (testing "Empty maps"
    (is (= {} (merge-disjunct {} {})))
    (is (= {:a 1} (merge-disjunct {} {:a 1}) (merge-disjunct {:a 1} {}))))
  (testing "All intersecing entries are the"
    (is (= {:a 1 :b 2} (merge-disjunct {:a 1} {:a 1 :b 2}))))
  (testing "Common keys have different values"
    (is (= nil (merge-disjunct {:a 1} {:a 2})))
    (is (= nil (merge-disjunct {:a 1 :c 4} {:a 2 :b 3})))))


(deftest test-initial-graph
  (is (= '{:enode->eclass {x 0}
           :eclass->enodes {0 [x]}}
         (initial-egraph 'x)))
  (is (= '{:enode->eclass {[0 1 2] 3
                           *       0
                           2       1
                           x       2}
           :eclass->enodes {0 [*]
                            1 [2]
                            2 [x]
                            3 [[0 1 2]]}}
         (initial-egraph '(* 2 x))))

  (is (= '{:enode->eclass {[0 1 1] 2, * 0, x 1}
           :eclass->enodes {0 [*], 1 [x], 2 [[0 1 1]]}}
         (initial-egraph '(* x x)))))

(deftest test-add-canonical
  (testing "Adding same value twice has no effect"
    (is (= (-> empty-graph
               (add-canonical '(+ a (+ b c))))
           (-> empty-graph
               (add-canonical '(+ a (+ b c)))
               (first)
               (add-canonical '(+ a (+ b c))))))
    (let [g (add-canonical empty-graph '(+ a (+ b c)))]
      (is (= (first g)
             (first (add-canonical (first g) '(+ b c))))))))

(deftest test-ematch
  (testing "Matching constant values without binding"
    (is (= [[{} 1]] (ematch (initial-egraph '(* 2 x)) 2)))
    (is (= [[{} 2]] (ematch (initial-egraph '(* 2 x)) 'x)))
    (is (= [[{} 3]] (ematch (initial-egraph '(* 2 x)) '(* 2 x)))))
  (testing "Bind to any expression"
    (is (= '([{?x *} 0] [{?x 2} 1] [{?x x} 2] [{?x [0 1 2]} 3])
           (ematch (initial-egraph '(* 2 x)) '?x)))))

#_(deftest test-ematch
    (let [egraph (initial-egraph '(* 2 x))]
      (testing "Matching constant values, without binding"
        (is (= [[{} 2]] (ematch egraph 2)))
        (is (= [[{} 3]] (ematch egraph 'x))))

      (is (= '[[{?a (* 2 x)} 0]
               [{?a *} 1]
               [{?a 2} 2]
               [{?a x} 3]]
             (ematch egraph '?a)))

      (is (= '[[{?a x} 0]] (ematch egraph '(* 2 ?a))))
      (is (= '[[{?a 2 ?b x} 0]] (ematch egraph '(* ?a ?b))))

      (testing "Variable is already used and bound to different value"
        (is (= '[] (ematch egraph '(* ?a ?a)))))
      (testing "Variable is already used and bound to same value"
        (is (= '[[{?a x} 0]]
               (ematch (initial-egraph '(* x x)) '(* ?a ?a)))))))

#_(deftest test-congruent
    (is (congruent? '(* 2 x) '(* x 2))))


#_(deftest test-tautology
    (is (tautology? '(or x (and true (not x))))))
