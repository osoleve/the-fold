;;; field.sexp — Development tasks for lattice/algebra/field.ss
;;;
;;; Module: field (tier 1, depends on prelude, ring)
;;; 250 lines, ~15 exported functions
;;; Provides: field construction, arithmetic (division, inverse, power with
;;;           negative exponents), field→ring conversion, standard fields
;;;           (Q, R, Z_p), modular inverse
;;;
;;; Test file: lattice/algebra/test-ring-field.ss (field portion)
;;;
;;; Git history:
;;;   6179bdca fix(algebra): let→let* in ring.ss ideal functions + 42 new ring/field tests
;;;   6e98e779 refactor(algebra): Introduce Field abstraction for polynomial division
;;;   24724eb7 fix(galois): Support large fields without element enumeration

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement mod-inverse
  ;; ──────────────────────────────────────────────
  ;; The extended Euclidean algorithm is the computational heart of
  ;; finite field arithmetic. Without it, division in Z_p doesn't work.

  (task
    (id "algebra/field/mod-inverse-impl")
    (module field)
    (tier 1)
    (type implement)
    (description "Implement mod-inverse using the extended Euclidean algorithm: given a and m, compute a^-1 mod m (the unique integer x in [0,m) such that a*x ≡ 1 mod m). The algorithm maintains invariants old-r, r (remainders) and old-s, s (coefficients) where at each step q = old-r / r, then (old-r, r) ← (r, old-r - q*r) and (old-s, s) ← (s, old-s - q*s). When r reaches 0, if old-r = 1 then the inverse is (modulo old-s m), otherwise no inverse exists (error).")
    (context "Available: modulo, quotient, -, *. Error with (error 'mod-inverse \"no inverse exists\") if gcd(a,m) ≠ 1. The initial values are: old-r=m, r=(modulo a m), old-s=0, s=1.")
    (before "(define (mod-inverse a m)
  ;; TODO: implement extended Euclidean algorithm
  ;; Returns x such that a*x ≡ 1 (mod m)
  0)")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-equal 4 (mod-inverse 2 7))"
                 "(assert-equal 5 (mod-inverse 3 7))"
                 "(assert-equal 1 (mod-inverse 1 7))"
                 "(assert-equal 6 (mod-inverse 6 7))"
                 "(assert-equal 1 (modulo (* 5 (mod-inverse 5 13)) 13))"
                 "(assert-error (mod-inverse 2 6))"))
    (available-modules (prelude ring field))
    (hints ("Initialize: old-r=m, r=(modulo a m), old-s=0, s=1"
            "Loop while r ≠ 0: q = quotient(old-r, r)"
            "Update: r ← old-r - q*r, s ← old-s - q*s"
            "At end: if old-r = 1, result is (modulo old-s m), else error"))
    (reference-solution "(define (mod-inverse a m)
  (let loop ([old-r m] [r (modulo a m)]
             [old-s 0] [s 1])
    (if (= r 0)
        (if (= old-r 1)
            (modulo old-s m)
            (error 'mod-inverse \"no inverse exists\"))
        (let ([q (quotient old-r r)])
          (loop r (- old-r (* q r))
                s (- old-s (* q s)))))))")
    (alternatives ())
    (source-commit "6e98e779")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement field-power
  ;; ──────────────────────────────────────────────
  ;; Like group-power but for fields: handles negative exponents
  ;; by taking the multiplicative inverse first.

  (task
    (id "algebra/field/field-power-impl")
    (module field)
    (tier 1)
    (type implement)
    (description "Implement field-power: compute a^n in a field F. For n=0 return one. For negative n, compute (field-inv F a)^|n| — i.e., invert first, then raise to the positive power. For positive n, use repeated squaring: even n → square of half-power, odd n → a * power(a, n-1). Use field-mul for multiplication and field-inv for inversion.")
    (context "Available: field-one, field-mul, field-inv (multiplicative inverse: 1/a), =, <, even?, /. Works for any field including Q-field, R-field, and finite fields GF(p).")
    (before "(define (field-power f a n)
  ;; TODO: implement with repeated squaring + negative exponent support
  (field-one f))")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-equal 1 (field-power (make-field-zp 7) 3 0))"
                 "(assert-equal 6 (field-power (make-field-zp 7) 3 3))"
                 "(assert-equal 5 (field-power (make-field-zp 7) 3 -1))"
                 "(assert-equal 8 (field-power Q-field 2 3))"
                 "(assert-equal 1/8 (field-power Q-field 2 -3))"))
    (available-modules (prelude ring field))
    (hints ("Three regimes: n=0, n<0, n>0"
            "For n<0: recursively call with (field-inv F a) and (- n)"
            "For even positive n: half = power(a, n/2), return half*half"))
    (reference-solution "(define (field-power f a n)
  (cond
    [(= n 0) (field-one f)]
    [(< n 0) (field-inv f (field-power f a (- n)))]
    [(= n 1) a]
    [(even? n)
     (let ([half (field-power f a (/ n 2))])
       (field-mul f half half))]
    [else
     (field-mul f a (field-power f a (- n 1)))]))")
    (alternatives ())
    (source-commit "6e98e779")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement field->ring conversion
  ;; ──────────────────────────────────────────────
  ;; Extract the underlying ring structure from a field.
  ;; This demonstrates the subtype relationship: every field IS a ring.

  (task
    (id "algebra/field/field-to-ring-impl")
    (module field)
    (tier 1)
    (type implement)
    (description "Implement field->ring: extract the underlying ring structure from a field. A field has all the components of a ring (elements, addition, multiplication, zero, one, negation, equality) plus division. Strip away the division function and repackage the remaining components using make-ring. The resulting ring should have the same elements and arithmetic as the field.")
    (context "Available: field-elements, field-add-op, field-mul-op, field-zero, field-one, field-neg-fn, field-equal-fn (accessors for field components), make-ring (ring constructor taking: elements add-op mul-op zero one neg-fn equal-fn).")
    (before "(define (field->ring f)
  ;; TODO: extract ring structure from field
  ;; Use field accessors to get components, then make-ring
  f)")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-true (ring? (field->ring (make-field-zp 5))))"
                 "(assert-equal 5 (ring-order (field->ring (make-field-zp 5))))"
                 "(assert-equal (field-add (make-field-zp 5) 3 4) (ring-add (field->ring (make-field-zp 5)) 3 4))"
                 "(assert-true (is-integral-domain? (field->ring (make-field-zp 5))))"))
    (available-modules (prelude ring field))
    (hints ("A field has 7 components a ring needs: elements, add, mul, zero, one, neg, equal"
            "Use the field-* accessors to extract each one"
            "Pass them to make-ring in the right order"))
    (reference-solution "(define (field->ring f)
  (make-ring
   (field-elements f)
   (field-add-op f)
   (field-mul-op f)
   (field-zero f)
   (field-one f)
   (field-neg-fn f)
   (field-equal-fn f)))")
    (alternatives ())
    (source-commit "6e98e779")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Extend with multiplicative-order
  ;; ──────────────────────────────────────────────
  ;; The multiplicative order of a in GF(p) is the smallest positive
  ;; n such that a^n = 1. By Fermat's little theorem, it divides p-1.

  (task
    (id "algebra/field/multiplicative-order-extend")
    (module field)
    (tier 1)
    (type extend)
    (description "Add a function field-multiplicative-order that computes the multiplicative order of a nonzero element in a finite field. The multiplicative order of a is the smallest positive integer n such that a^n = 1 (the field's one). Iterate: start with current = a, multiply by a each step, counting until you reach one. By Fermat's little theorem, the order divides p-1 for GF(p), so you need at most p-1 steps.")
    (context "Available: field-mul, field-one, field-equal?. The input element a must not be zero. For GF(7): ord(1)=1, ord(2)=3 (since 2^3=8≡1), ord(3)=6 (primitive root).")
    (before ";; field-multiplicative-order does not yet exist in the module")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-equal 1 (field-multiplicative-order (make-field-zp 7) 1))"
                 "(assert-equal 3 (field-multiplicative-order (make-field-zp 7) 2))"
                 "(assert-equal 6 (field-multiplicative-order (make-field-zp 7) 3))"
                 "(assert-equal 2 (field-multiplicative-order (make-field-zp 7) 6))"))
    (available-modules (prelude ring field))
    (hints ("Start with current = a and n = 1"
            "Each step: current = field-mul f current a, n = n + 1"
            "When field-equal? f current (field-one f), return n"))
    (reference-solution "(define (field-multiplicative-order f a)
  (let ([one (field-one f)])
    (let loop ([n 1] [current a])
      (if (field-equal? f current one)
          n
          (loop (+ n 1) (field-mul f current a))))))")
    (alternatives ())
    (source-commit "6e98e779")
    (difficulty 3))

) ;; end tasks
