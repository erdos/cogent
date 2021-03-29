(ns cogent.rules)

(def rules {})

(def ?a '?a) (def ?b '?b) (def ?c '?c)

(defmacro defrule [rule-name pattern body]
  `(alter-var-root #'rules assoc '~pattern '~body))

(defmacro defrules [rule-name & bodies]
  (cons 'do
        (for [[a op b] (if (some '#{=> <=>} bodies)
                         (partition 3 bodies)
                         (map (juxt first = second) (partition 2 bodies)))
              [a b] (if (= '<=> op)
                      [[a b] [b a]] [[a b]])]
          `(defrule ~(gensym (name rule-name)) ~a ~b))))

#_(defrules for-tests
    (* ?a ?b) (* ?b ?a)

    (and true ?x) ?x
    (and ?x ?y) (and ?y ?x)
  ;(or ?x ?y) (or ?y ?x)
  ;  
    (* ?a (* ?b ?c)) (* (* ?a ?b) ?c)
  ; (* ?a ?b) (* ?b ?a)
    )


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


(defrules logical-distr
  ;; distributivity
  (and (or ?a ?b) ?c)  <=>  (or (and ?a ?c) (and ?b ?c))
  (or (and ?a ?b) ?c) <=>  (and (or ?a ?c) (or ?b ?c)))


(defrules logical-demoragan
  (not (and ?a ?b)) <=> (or (not ?a) (not ?b))
  (not (or ?a ?b))  <=> (and (not ?a) (not ?b)))


(defrules basic-rewrites
  (* ?a ?b)        (* ?b ?a)
  (* 0 ?a)         0
  (* 1 ?a)         ?a
  (* ?a (* ?b ?c)) (* (* ?a ?b) ?c)

  (+ ?a ?b)        (+ ?b ?a)               ;; commutative
  (+ ?a (+ ?b ?c)) (+ (+ ?a ?b) ?c)
  (+ ?a 0)         ?a                      ;; null elem
  (* (+ ?a ?b) ?c) (+ (* ?a ?c) (* ?b ?c)) ;; distributive
  (+ (* ?a ?c) (* ?b ?c)) (* (+ ?a ?b) ?c) ;; factor

    ;; subtraction
  (- ?a ?b)        (+ ?a (* -1 ?b))
  (- ?a ?a)        0

    ;; division
  (/ ?a ?a)        1
  (/ (+ ?a ?b) ?c) (+ (/ ?a ?c) (/ ?b ?c))

    ;; constants
  (+ ?a ?a)        (* 2 ?a)

    ;; power
  (pow ?x 1) ?x
  (pow ?x 0) 1
    ;; TODO: maybe do some integration as well here?

    ;; 
  (sqrt ?x)        (pow x (/ 1 2))
  (/ ?x (sqrt ?x)) (sqrt ?x)
   ;; TODO: do we need both directions?
  )


(defrules equivalence-relation
  (= ?a ?a)        true               ;; reflexive
  (= ?a ?b)        (= ?b ?a)          ;; symmetric
  (and (= ?a ?b) (= ?b ?c)) (= ?a ?c) ;; transitive

  (= true ?a)      ?a
  (= false ?a)     (not ?a)
  (not ?a)         (= false ?a))


(defrules equation-simplify
  (= (+ ?a ?b) (+ ?a ?c)) => (= ?b ?c)
  (= (- ?a ?c) (- ?b ?c)) => (= ?a ?b)

  (= (/ ?a ?x) (/ ?b ?x)) => (= ?a ?b)
  (= (* ?a ?x) (* ?b ?x)) => (or (= 0 ?x) (= ?a ?b))

  (= 0 (* ?a ?b))         => (or (= 0 ?a) (= 0 ?b))
  (= 0 (/ ?a ?b))         => (= 0 ?a)
  (/ 0 ?x)                => 0
  )


(defrules exponentials
  (exp 0)                => 1
  (log 1)                => 0
  (exp (+ ?a ?b))       <=> (* (exp ?a) (exp ?b))
  (log (* ?a ?b))       <=> (+ (log ?a) (log ?b))
  (log (exp ?a))         => ?a
  (exp (log ?a))         => ?a

  (exp ?x)              <=> (pow e ?x))


(defrules symbolic-differentiation
      ;; https://github.com/egraphs-good/egg/blob/main/tests/math.rs
    ;; TODO: differentiation
    ;; (d ?x ?x) 1 ;; if symbol
    ;; (d ?x ?x) 0 ;; f  c onstant or distinct var.
    ;; (d ?x (+ ?a ?b)) (+ (d ?x ?a) (d ?x ?b))

  (d ?x ?x)        => 1
  (d ?x ?symbol/y) => (fn [?x ?y] (when (not= ?x ?y)));; if dinstinct!!

  (d ?x (+ ?a ?b)) => (+ (d ?x ?a) (d ?x ?b))
  (d ?x (* ?a ?b)) => (+ (* (d ?x ?a) ?b) (* ?a (d ?x ?b)))

  ;; chain rule
  (d ?x (exp ?y))  => (* (exp ?y) (d ?x ?y))

;  (d ?x (pow ?a number/?b)) => 

  ;(d ?x (exp ?a))  => (exp ?a)
  ;(d ?x (log ?a))  => (/ 1 ?a)
  )


(defrules trigonometry
  (+ (pow (sin ?x) 2) (pow (cos ?x) 2)) => 1
  (- 1 (pow (sin ?x) 2))                <=> (pow (cos ?x) 2)
  (- 1 (pow (cos ?x) 2))                <=> (pow (sin ?x) 2)

  (/ (sin ?x) (cos ?x))                 <=> (tan ?x)

  (sin (* -1 ?x))                       <=> (* -1 (sin ?x))
  (cos (* -1 ?x))                       <=> (cos ?x)
  (tan (* -1 ?x))                       <=> (* -1 (tan ?x))

  (sin (+ ?a ?b))                       <=> (+ (* (sin ?a) (cos ?b)) (* (cos ?a) (sin ?b)))
  (sin (- ?a ?b))                       <=> (- (* (sin ?a) (cos ?b)) (* (cos ?a) (sin ?b)))
  (cos (+ ?a ?b))                       <=> (- (* (cos ?a) (cos ?b)) (* (sin ?a) (sin ?b)))
  (cos (- ?a ?b))                       <=> (+ (* (cos ?a) (cos ?b)) (* (sin ?a) (sin ?b)))

  ;; 
  )


(defrules completing-the-square
  (= (+ (* ?a (pow ?x 2)) (+ (* ?b ?x) ?c)) 0)
  =>
  (= (+ (* ?a (pow (+ x (/ ?b (* 2 ?a))) 2)) (- ?c (/ (pow ?b 2) (* 4 ?a)))) 0))

