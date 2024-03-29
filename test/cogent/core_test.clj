(ns cogent.core-test
  (:require [clojure.test :refer :all]
            [cogent.egraph :refer :all]
            [cogent.core :refer :all]
            [cogent.matcher :refer :all]))

(defn- proj [x] (select-keys x [:enode->eclass :eclass->enodes]))

(defn- initial-egraph [expr] (first (add-canonical empty-graph expr)))


(deftest test-initial-graph
  (is (= '{:enode->eclass {x 0}
           :eclass->enodes {0 #{x}}}
         (proj (initial-egraph 'x))))
  (is (= '{:enode->eclass {[0 1 2] 3
                           *       0
                           2       1
                           x       2}
           :eclass->enodes {0 #{*}
                            1 #{2}
                            2 #{x}
                            3 #{[0 1 2]}}}
         (proj (initial-egraph '(* 2 x)))))

  (is (= '{:enode->eclass {[0 1 1] 2, * 0, x 1}
           :eclass->enodes {0 #{*}, 1 #{x}, 2 #{[0 1 1]}}}
         (proj (initial-egraph '(* x x))))))

(deftest test-add-canonical
  (testing "Adding same value twice has no effect"
    (is (= (-> empty-graph
               (add-canonical '(+ a (+ b c)))
               (proj))
           (-> empty-graph
               (add-canonical '(+ a (+ b c)))
               (first)
               (add-canonical '(+ a (+ b c)))
               (proj))))
    (let [g (add-canonical empty-graph '(+ a (+ b c)))]
      (is (= (first g)
             (first (add-canonical (first g) '(+ b c))))))))

(deftest test-add-canonical-id)

(deftest test-ematch
  (testing "Matching constant values without binding"
    (is (= [[{} 1]] (ematch (initial-egraph '(* 2 x)) 2)))
    (is (= [[{} 2]] (ematch (initial-egraph '(* 2 x)) 'x)))
    (is (= [[{} 3]] (ematch (initial-egraph '(* 2 x)) '(* 2 x)))))
  (testing "Bind to any expression"
    (is (= '([{?x *} 0] [{?x 2} 1] [{?x x} 2] [{?x [0 1 2]} 3])
           (ematch (initial-egraph '(* 2 x)) '?x)))))


(deftest test-ematch
  (let [egraph (initial-egraph '(* 2 x))]
    (testing "Matching constant values, without binding"
      (is (= [[{} 1]] (ematch egraph 2)))
      (is (= [[{} 2]] (ematch egraph 'x))))

    (is (= [[{'?a (->Unresolved 0)} 0]
            [{'?a (->Unresolved 1)} 1]
            [{'?a (->Unresolved 2)} 2]
            [{'?a (->Unresolved 3)} 3]]
           (ematch egraph '?a)))

    (is (= [[{'?a (->Unresolved 2)} 3]]
           (ematch egraph '(* 2 ?a))))
    (is (= [[{'?a (->Unresolved 1) '?b (->Unresolved 2)} 3]]
           (ematch egraph '(* ?a ?b))))

    (testing "Variable is already used and bound to different value"
      (is (= '[] (ematch egraph '(* ?a ?a)))))
    (testing "Variable is already used and bound to same value"
      (is (= [[{'?a (->Unresolved 1)} 2]]
             (ematch (initial-egraph '(* x x)) '(* ?a ?a)))))))

(deftest congruence-test
  (is (congruent? '(* 1 0) 0))
  (is (not (congruent? '(* 1 0) 1)))

  (is (congruent? '(* 2 x) '(* x 2)))
  (is (congruent? '(* a (+ b c)) '(+ (* c a) (* b a))))

  (doseq [e '[(+ a (+ b c))
              (+ (+ b c) a)
              (+ c (+ b a))
              (+ c (* 1 (+ a (* 1 b))))]]
    (is (congruent? '(+ a (+ b c)) e)))

  ;; simple equation solving...
  (is (congruent? '(= 3 x) '(= (+ 3 r) (+ r x)))))

(deftest test-solve
  (is (= '#{true} (solve '(= true (and true x))))))

(deftest test-tautology
  (time (is (tautology? '(or x (and true (not x)))))))

(deftest test-contradiction
  (is (contradiction? '(= 1 2)))
  ; (is (contradiction? '(and (< x 0) (> x 0))))
  )

(deftest egraph?-test
  (is (not (egraph? 1)))
  (is (not (egraph? nil)))
  (is (not (egraph? {})))
  (is (egraph? empty-graph)))