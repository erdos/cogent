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


#_(defrules logical-distr
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

    ;; constants
  (+ ?a ?a)        (* 2 ?a)
  (+ ?a ?a ?a)     (* 3 ?a)

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
  (= (+ ?a ?b) (?a ?c))   => (= ?b ?c)
  (= (- ?a ?c) (- ?b ?c)) => (= ?a ?b)

  (= (/ ?a ?x) (/ ?b ?x)) => (= ?a ?b)
  (= (* ?a ?x) (* ?b ?x)) => (or (= 0 ?x) (= ?a ?b))

  (= 0 (* ?a ?b))         => (or (= 0 ?a) (= 0 ?b))
  (= 0 (/ ?a ?b))         => (= 0 ?a))

(defrules exponentials

  (exp 0)                => 1
  (log 1)                => 0
  (exp (+ ?a ?b))       <=> (* (exp ?a) (exp ?b))
  (log (* ?a ?b))       <=> (+ (log ?a) (log ?b))
  (log (exp ?a))         => ?a
  (exp (log ?a))         => ?a)

(defrules symbolic-differentiation
      ;; https://github.com/egraphs-good/egg/blob/main/tests/math.rs
    ;; TODO: differentiation
    ;; (d ?x ?x) 1 ;; if symbol
    ;; (d ?x ?x) 0 ;; f  c onstant or distinct var.
    ;; (d ?x (+ ?a ?b)) (+ (d ?x ?a) (d ?x ?b))

  (d (+ ?a ?b)) => (+ (d ?a) (d ?b))
  (d (exp ?a))  => (exp ?a)
  (d (log ?a))  => (/ 1 ?a))
