(ns cogent.core-test
  (:require [clojure.test :refer :all]
            [cogent.core :refer :all]))

(deftest test-initial-graph
  (is (= '{:enode->eclass {x 0}
           :eclass->enodes {0 [x]}}
         (initial-egraph 'x)))

  (is (= '{:enode->eclass {(* 2 x) 0
                           *       1
                           2       2
                           x       3}
           :eclass->enodes {0 [(* 2 x)]
                            1 [*]
                            2 [2]
                            3 [x]}}
         (initial-egraph '(* 2 x))))

  (is (= '{:enode->eclass {(* x x) 0, * 1, x 3}
           :eclass->enodes {0 [(* x x)], 1 [*], 3 [x]}}
         (initial-egraph '(* x x)))))


(deftest test-ematch
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

