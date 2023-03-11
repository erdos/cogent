(ns cogent.rules
  (:require [clojure.walk]))

;; see also:
;; https://www.cs.cornell.edu/~ross/publications/eqsat/MikeThesis.pdf

(def rules (array-map))

(declare ?a ?b ?c ?d ?x ?y ==>)

(defn- rhs-substitutor [pattern]
  (fn [substitutions]
    (assert (map? substitutions))
    (clojure.walk/postwalk-replace substitutions pattern)))

(defmacro defrule [rule-name pattern body]
  (let [body (if (and (seq? body) (= 'fn (first body)))
               body
               `(rhs-substitutor '~body))]
    `(alter-var-root #'rules assoc '~pattern ~body)))

(defmacro defrules [rule-name & bodies]
  (let [bodies (remove string? bodies)
        bodies (if (some '#{==> ===} bodies)
                 (partition 3 bodies)
                 (map (juxt first (constantly '==>) second) (partition 2 bodies)))]
    (cons 'do
          (for [[a op b] bodies
                [a b] (if (= '=== op)
                        [[a b] [b a]] [[a b]])]
            `(defrule ~(gensym (name rule-name)) ~a ~b)))))

(defrules logical-operators
  (or ?a ?a)         ?a
  (or ?a ?b)         (or ?b ?a)
  (or ?a (or ?b ?c)) (or (or ?a ?b) ?c)
  (or ?a (not ?a))   true
  (or ?a true)       true
  (or ?a false)      ?a

  (and ?a ?a)          ?a
  (and ?a ?b)          (and ?b ?a)
  (and ?a (and ?b ?c)) (and (and ?a ?b) ?c)
  (and ?a (not ?a))    false
  (and ?a true)        ?a
  (and ?a false)       false

  (not true)   false
  (not false)  true
  (not (not ?a)) ?a)

  ;; https://github.com/egraphs-good/egg/blob/main/tests/prop.rs

(defrules constant-folding
  (+ number/?a number/?b)    ==> (fn [m] (+ (m '?a) (m '?b)))
  (- number/?a number/?b)    ==> (fn [m] (- (m '?a) (m '?b)))
  (/ number/?a number/?b)    ==> (fn [m] (/ (m '?a) (m '?b)))
  (* number/?a number/?b)    ==> (fn [m] (* (m '?a) (m '?b)))
  (pow number/?a number/?b)  ==> (fn [m] (Math/pow (m '?a) (m '?b)))
  (= number/?a number/?b)    ==> (fn [m] (= (m '?a) (m '?b)))
  (< number/?a number/?b)    ==> (fn [m] (< (m '?a) (m '?b))))

(defrules logical-laws
  ;; De Morgan
  (not (and ?a ?b))    === (or (not ?a) (not ?b))
  (not (or ?a ?b))     === (and (not ?a) (not ?b))

  ;; Distributivity
  (and (or ?a ?b) ?c)  ===  (or (and ?a ?c) (and ?b ?c))
  (or (and ?a ?b) ?c)  ===  (and (or ?a ?c) (or ?b ?c))

  ;; Absorption  
  (or ?a (and ?a ?b))  ==> ?a
  (and ?a (or ?a ?b))  ==> ?a

  "")


(defrules logical-syntax-candy
  (=> ?a ?b)     ==>   (or (not ?a) ?b)
  (<=> ?a ?b)    ==>   (or (and ?a ?b) (and (not ?a) (not ?b))))

(defrules basic-algebra-multiplication
  (* ?a ?b)        ==>  (* ?b ?a)               ;; commutative
  (* 0 ?a)         ==>  0                       ;; null elem

  (* 1 ?a)         ==>  ?a                      ;; identity
  (* ?a (* ?b ?c)) ==>  (* (* ?a ?b) ?c) ;; associative
  )       

(defrules basic-algebra-addition
  (+ ?a ?b)         ==> (+ ?b ?a)               ;; commutative
  ;; TODO: this breaks me!
  (+ nonzero/?a (+ nonzero/?b nonzero/?c))  ==> (+ (+ ?a ?b) ?c)        ;; associative
  (+ 0 ?a)          ==> ?a                      ;; null elem
  )

(defrules addition-multiplication-distributivity
  (* (+ ?a ?b) ?c)        ==> (+ (* ?a ?c) (* ?b ?c)) ;; distributive
  ;(+ (* ?a ?c) (* ?b ?c)) ==> (* (+ ?a ?b) ?c) ;; factor 
  )

(defrules basic-algebra

    ;; subtraction
  ;(- ?a ?b)         (+ ?a (* -1 ?b))
  ;(+ ?a (* -1 ?b))  (- ?a ?b)
  ;(- ?a ?a)         0

    ;; division
  ;; (/ ?a ?a)        1 ONLY IF ?a!=0

  ; (/ (+ ?a ?b) ?c) (+ (/ ?a ?c) (/ ?b ?c))

    ;; constants
  ;; (+ nonzero/?a ?a)        (* 2 ?a)
  )

#_
(defrules power-fn
  ;; power
  ; (* ?x ?x) (pow ?x 2)
  (pow ?x 2) (* ?x ?x)
  (pow ?x 1) ?x
  (pow ?x 0) 1

    ;; TODO: maybe do some integration as well here?

    ;; 
  (sqrt ?x)        (pow x (/ 1 2))
  (/ ?x (sqrt ?x)) (sqrt ?x)

  (pow (* ?a ?b) ?c) (* (pow ?a ?c) (pow ?b ?c))
   ;; TODO: do we need both directions?
  )

(defrules equivalence-relation
  (= ?a ?a)                 ==> true      ;; reflexive
  (= ?a ?b)                 ==> (= ?b ?a) ;; symmetric
  (and (= ?a ?b) (= ?b ?c)) ==> (= ?a ?c) ;; transitive

  ;;  equivalence should not be used for logical functions!
  ;;  this is because logical equivalence is not transitive!
  (= true ?a)               ==> ?a
  (= true false)            ==> false
  ;(= false ?a)              === (not ?a)
  )

(defrules equation-simplify
  (= (+ ?a ?b) (+ ?a ?c)) ==> (= ?b ?c)
  (= (- ?a ?c) (- ?b ?c)) ==> (= ?a ?b)

  (= (/ ?a ?x) (/ ?b ?x)) ==> (= ?a ?b)
  (= (* ?a ?x) (* ?b ?x)) ==> (or (= 0 ?x) (= ?a ?b))

  (= 0 (* ?a ?b))         ==> (or (= 0 ?a) (= 0 ?b))
  (= 0 (/ ?a ?b))         ==> (= 0 ?a)
  (/ 0 ?x)                ==> 0

  (= 1 (* ?x ?x))         ==> (or (= 1 ?x) (= -1 ?x))
  (= (* ?x ?x) ?x)        ==> (or (= 1 ?x) (= 0 ?x)))

(defrules relations
  (< ?a ?b)       === (> ?b ?a)
  (<= ?a ?b)      === (or (< ?a ?b) (= ?a ?b))
  (>= ?a ?b)      === (<= ?b ?a)
  (< ?a ?b)       === (not (=> ?a ?b))
  (not (= ?a ?b)) === (or (< ?a ?b) (> ?a ?b)))

(defrules exponentials
  (exp 0)                ==> 1
  (log 1)                ==> 0
  (exp (+ ?a ?b))        === (* (exp ?a) (exp ?b))
  (log (* ?a ?b))        === (+ (log ?a) (log ?b))
  (log (exp ?a))         ==> ?a
  (exp (log ?a))         ==> ?a

  (exp ?x)               === (pow e ?x))

;; TODO
#_(defrules absolute-value
    (abs 0)               ==> 0
    (= 0 (abs ?x))        ==> (= ?x 0)
  ;; (sqrt (pow ?x 2))     ==> (abs ?x)
    )

(defrules symbolic-differentiation
  ;; https://github.com/egraphs-good/egg/blob/main/tests/math.rs

  (d symbol/?x symbol/?y) ==> (fn [m] (if (= (m '?x) (m '?y)) 1 0))
  (d ?x number/?y)        ==> 0

  (d ?x (+ ?a ?b)) ==> (+ (d ?x ?a) (d ?x ?b))
  (d ?x (* ?a ?b)) ==> (+ (* (d ?x ?a) ?b) (* ?a (d ?x ?b)))

  "chain rule"
  (d ?x (exp ?y))  ==> (* (exp ?y) (d ?x ?y))
  (d ?x (log ?y))  ==> (/ (d ?x ?y) ?y)

;  (d ?x (pow ?a number/?b)) ==> 

  ;(d ?x (exp ?a))  ==> (exp ?a)
  ;(d ?x (log ?a))  ==> (/ 1 ?a)
  )


#_(defrules trigonometry
    (+ (pow (sin ?x) 2) (pow (cos ?x) 2)) ==> 1
    (- 1 (pow (sin ?x) 2))                === (pow (cos ?x) 2)
    (- 1 (pow (cos ?x) 2))                === (pow (sin ?x) 2)

    (/ (sin ?x) (cos ?x))                 === (tan ?x)

    "symmetries"
    (sin (* -1 ?x))                       === (* -1 (sin ?x))
    (cos (* -1 ?x))                       === (cos ?x)
    (tan (* -1 ?x))                       === (* -1 (tan ?x))

    "additive rules"
    (sin (+ ?a ?b))                       === (+ (* (sin ?a) (cos ?b)) (* (cos ?a) (sin ?b)))
    (sin (- ?a ?b))                       === (- (* (sin ?a) (cos ?b)) (* (cos ?a) (sin ?b)))
    (cos (+ ?a ?b))                       === (- (* (cos ?a) (cos ?b)) (* (sin ?a) (sin ?b)))
    (cos (- ?a ?b))                       === (+ (* (cos ?a) (cos ?b)) (* (sin ?a) (sin ?b)))

  ;; 
    )

#_(defrules completing-the-square
    (= (+ (* ?a (pow ?x 2)) (+ (* ?b ?x) ?c)) 0)
    ==>
    (= (+ (* ?a (pow (+ x (/ ?b (* 2 ?a))) 2)) (- ?c (/ (pow ?b 2) (* 4 ?a)))) 0))


(defrules quadratic-solver
  (= ?a (pow ?b 2))     ==> (or (= ?b (pow ?a 1/2))
                                (= ?b (* -1 (pow ?a 1/2)))))


(defrules ski-combinator-calculus
  (app I ?x)  ==> ?x
  (app (app K ?x) ?y)  ==> ?x
  (app (app (app S ?x) ?y) ?z) ==> (app (app ?x ?z) (app ?y ?z))
  ;; fn composition is associative
  (app ?a (app ?b ?c)) <==> (app (app ?a ?b) ?c))

