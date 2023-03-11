(ns cogent.rules-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cogent.core :refer [congruent? tautology? solve]]))

(clojure.test/use-fixtures :each
  (fn [f]
    (println "Starting")
    (let [before (System/nanoTime)
          _      (f)
          after  (System/nanoTime)]
      (println " - Done, elapsed:" (quot (- after before) 1000000)))))

(deftest test-logics
  (println "Testing Logics")
  (is (tautology? '(or (or w p) (or q (not p)))))
  

  ;; TODO: this is slow:
  (println "Testing slow logical expression:")
  (is (tautology? '(or (or w (or x p)) (or q (not p)))))

  )

(deftest test-solve-contradict
  (testing "no solution with contradiction"
    (is (empty? (solve '(= (+ x 1) (+ (+ x 1) 1)))))))

(deftest test-diff-congruent
  (println "Testing diff congruente")
  (is (congruent? 1 '(d x (+ 3 x))))
  (is (congruent? 0 '(d x 3)))
  ;(is (congruent? 'a '(d x (* a x))))
  (is (congruent? 0 '(d x y)))

  )

#_
(deftest test-buggy
    ;; this is super slow:::
  (is (congruent? 'y '(d x (* y x))))
  )

(deftest test-ski-combinator-calculus
  (println "Testing SKI")
  (is (congruent? '(app S (app I (app I b))) '(app b b)))
  (is (congruent? '(app (app S K) (app S K)) 'K)))
