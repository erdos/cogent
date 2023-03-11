(ns cogent.rules-test
  (:require [clojure.test :refer [deftest testing is are]]
            [cogent.core :refer [congruent?]]))

(clojure.test/use-fixtures :each
  (fn [f]
    (println "Starting" f)
    (let [before (System/nanoTime)
          _      (f)
          after  (System/nanoTime)]
      (println " - Done, elapsed:" (quot (- after before) 1000000)))))

(deftest test-diff-congruent
  (is (congruent? 1 '(d x (+ 3 x))))
  (is (congruent? 0 '(d x 3)))
  ;(is (congruent? 3 '(d x (* 3 x))))
  (is (congruent? 0 '(d x y)))

  )

#_
(deftest test-buggy
    ;; this is super slow:::
  (is (congruent? 'y '(d x (* y x))))
  )

(deftest test-ski-combinator-calculus
  (is (congruent? '(app S (app I (app I b))) '(app b b)))
  (is (congruent? '(app (app S K) (app S K)) 'K)))
