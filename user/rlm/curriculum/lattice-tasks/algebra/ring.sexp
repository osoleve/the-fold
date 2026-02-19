;;; ring.sexp — Development tasks for lattice/algebra/ring.ss
;;;
;;; Module: ring (tier 0, depends on prelude, group)
;;; 520 lines, ~25 exported functions
;;; Provides: ring construction, arithmetic, properties (commutative, zero divisors,
;;;           integral domain, units), Z_n rings, homomorphisms, ideals (principal,
;;;           sum, product, prime, maximal)
;;;
;;; Test file: lattice/algebra/test-ring-field.ss (292 lines, ring portion)
;;;
;;; Git history:
;;;   6179bdca fix(algebra): let→let* in ring.ss ideal functions + 42 new ring/field tests
;;;   d83deba2 feat(module): migrate lattice/algebra/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement is-zero-divisor?
  ;; ──────────────────────────────────────────────
  ;; Zero divisors break nice algebraic properties. In Z_6, 2*3=0 but
  ;; neither 2 nor 3 is zero — both are zero divisors. Their presence
  ;; is what distinguishes Z_6 from an integral domain like Z_5.

  (task
    (id "algebra/ring/zero-divisor-impl")
    (module ring)
    (tier 0)
    (type implement)
    (description "Implement is-zero-divisor?: given a ring R and an element a, determine if a is a zero divisor. Element a is a zero divisor if a is not zero AND there exists some nonzero b in R such that a*b = 0 or b*a = 0. Zero itself is NOT a zero divisor by convention. Iterate over all elements of the ring to find such a b.")
    (context "Available: ring-elements (list of elements), ring-mul (multiplication), ring-zero (zero element), ring-equal? (equality). The ring is finite, so you can check all elements.")
    (before "(define (is-zero-divisor? r a)
  ;; TODO: implement
  ;; Check if a ≠ 0 and ∃ b ≠ 0 such that a*b = 0 or b*a = 0
  #f)")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-true (is-zero-divisor? (make-ring-zn 6) 2))"
                 "(assert-true (is-zero-divisor? (make-ring-zn 6) 3))"
                 "(assert-false (is-zero-divisor? (make-ring-zn 6) 1))"
                 "(assert-false (is-zero-divisor? (make-ring-zn 6) 5))"
                 "(assert-false (is-zero-divisor? (make-ring-zn 5) 2))"))
    (available-modules (prelude group ring))
    (hints ("First check: if a equals zero, return #f (zero is not a zero divisor)"
            "Then iterate over all elements b: skip b if b is zero"
            "Check if a*b = 0 OR b*a = 0"))
    (reference-solution "(define (is-zero-divisor? r a)
  (let ([elems (ring-elements r)]
        [zero (ring-zero r)])
    (if (ring-equal? r a zero)
        #f
        (let loop ([bs elems])
          (if (null? bs)
              #f
              (let ([b (car bs)])
                (if (and (not (ring-equal? r b zero))
                         (or (ring-equal? r (ring-mul r a b) zero)
                             (ring-equal? r (ring-mul r b a) zero)))
                    #t
                    (loop (cdr bs)))))))))")
    (alternatives ())
    (source-commit "6179bdca")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement make-principal-ideal
  ;; ──────────────────────────────────────────────
  ;; The principal ideal <a> in ring R is the set {r*a | r ∈ R}.
  ;; Must deduplicate using the ring's equality function.

  (task
    (id "algebra/ring/principal-ideal-impl")
    (module ring)
    (tier 0)
    (type implement)
    (description "Implement make-principal-ideal: given a ring R and an element a, generate the principal ideal <a> = {r*a | r in R} for all r in R. The result must be deduplicated using the ring's equality function (not Scheme's equal?). Return the result wrapped with make-ideal. Use the helper unique-with-equal for deduplication.")
    (context "Available: ring-elements, ring-mul, ring-equal-fn (returns the equality predicate), make-ideal (wraps ring + elements into an ideal), map. The helper unique-with-equal is defined in ring.ss: (unique-with-equal lst eq-fn) removes duplicates using a custom equality.")
    (before "(define (make-principal-ideal r a)
  ;; TODO: generate <a> = {r*a | r in R} with dedup
  (make-ideal r (list a)))")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(let ([i2 (make-principal-ideal (make-ring-zn 6) 2)]) (assert-equal 3 (length (ideal-elements i2))))"
                 "(let ([i3 (make-principal-ideal (make-ring-zn 6) 3)]) (assert-equal 2 (length (ideal-elements i3))))"
                 "(let ([i0 (make-principal-ideal (make-ring-zn 6) 0)]) (assert-equal 1 (length (ideal-elements i0))))"
                 "(assert-true (is-valid-ideal? (make-principal-ideal (make-ring-zn 6) 2)))"))
    (available-modules (prelude group ring))
    (hints ("Map ring-mul over all ring elements to get {r*a | r in R}"
            "Deduplicate with unique-with-equal and ring-equal-fn"
            "Wrap with make-ideal"))
    (reference-solution "(define (make-principal-ideal r a)
  (let ([elems (unique-with-equal
                (map (lambda (r-elem) (ring-mul r r-elem a))
                     (ring-elements r))
                (ring-equal-fn r))])
    (make-ideal r elems)))")
    (alternatives ())
    (source-commit "6179bdca")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Fix ring-power
  ;; ──────────────────────────────────────────────
  ;; Repeated squaring with a subtle bug: the even case
  ;; multiplies a * half instead of half * half.

  (task
    (id "algebra/ring/fix-ring-power")
    (module ring)
    (tier 0)
    (type fix)
    (description "The ring-power function computes a^n using repeated squaring but has a bug. For even n, it should compute half = a^(n/2), then return half * half. The buggy version computes a * half instead of half * half, giving wrong results for n >= 4. Fix the even case.")
    (context "Available: ring-mul (multiplication), ring-one (multiplicative identity), = (numeric equality), even?, /. The function should handle: n=0 → one, n=1 → a, even n → square of half-power, odd n → a * power(a, n-1).")
    (before "(define (ring-power r a n)
  (cond
    [(= n 0) (ring-one r)]
    [(= n 1) a]
    [(even? n)
     (let ([half (ring-power r a (/ n 2))])
       (ring-mul r a half))]  ;; BUG: should be (ring-mul r half half)
    [else
     (ring-mul r a (ring-power r a (- n 1)))]))")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-equal 1 (ring-power (make-ring-zn 7) 3 0))"
                 "(assert-equal 3 (ring-power (make-ring-zn 7) 3 1))"
                 "(assert-equal 2 (ring-power (make-ring-zn 7) 3 2))"
                 "(assert-equal 6 (ring-power (make-ring-zn 7) 3 3))"
                 "(assert-equal 1 (ring-power (make-ring-zn 7) 3 6))"))
    (available-modules (prelude group ring))
    (hints ("The bug is in the even? branch"
            "half = a^(n/2), so half * half = a^n — but a * half = a^(n/2+1)"
            "Change (ring-mul r a half) to (ring-mul r half half)"))
    (reference-solution "(define (ring-power r a n)
  (cond
    [(= n 0) (ring-one r)]
    [(= n 1) a]
    [(even? n)
     (let ([half (ring-power r a (/ n 2))])
       (ring-mul r half half))]
    [else
     (ring-mul r a (ring-power r a (- n 1)))]))")
    (alternatives ())
    (source-commit "6179bdca")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Extend with ring-characteristic
  ;; ──────────────────────────────────────────────
  ;; The characteristic of a ring is the smallest positive n such
  ;; that n * 1 = 0 (adding 1 to itself n times). If no such n
  ;; exists, the characteristic is 0.

  (task
    (id "algebra/ring/characteristic-extend")
    (module ring)
    (tier 0)
    (type extend)
    (description "Add a function ring-characteristic that computes the characteristic of a finite ring R. The characteristic is the smallest positive integer n such that adding the multiplicative identity (one) to itself n times equals zero. Formally: char(R) = min{n > 0 | n * 1_R = 0_R}. Use ring-add to accumulate and ring-equal? to test against ring-zero. For a finite ring, the characteristic always exists and divides |R|, so you only need to check up to ring-order.")
    (context "Available: ring-add, ring-one, ring-zero, ring-equal?, ring-order. Iterate: start with current = one, add one repeatedly, check if result equals zero.")
    (before ";; ring-characteristic does not yet exist in the module")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-equal 6 (ring-characteristic (make-ring-zn 6)))"
                 "(assert-equal 5 (ring-characteristic (make-ring-zn 5)))"
                 "(assert-equal 2 (ring-characteristic (make-ring-zn 2)))"
                 "(assert-equal 7 (ring-characteristic (make-ring-zn 7)))"))
    (available-modules (prelude group ring))
    (hints ("Start with current = (ring-one r), accumulate = (ring-one r)"
            "Each step: accumulate = ring-add r accumulate (ring-one r)"
            "If accumulate equals (ring-zero r), return the step count"))
    (reference-solution "(define (ring-characteristic r)
  (let ([one (ring-one r)]
        [zero (ring-zero r)])
    (let loop ([n 1] [acc one])
      (if (ring-equal? r acc zero)
          n
          (if (> n (ring-order r))
              0
              (loop (+ n 1) (ring-add r acc one)))))))")
    (alternatives ())
    (source-commit "6179bdca")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement is-unit?
  ;; ──────────────────────────────────────────────
  ;; A unit is an element with a multiplicative inverse.
  ;; In Z_6, the units are {1, 5} since 1*1=1 and 5*5=1 (mod 6).
  ;; In Z_5 (a field), every nonzero element is a unit.

  (task
    (id "algebra/ring/is-unit-impl")
    (module ring)
    (tier 0)
    (type implement)
    (description "Implement is-unit?: given a ring R and an element a, determine if a is a unit (has a multiplicative inverse). Element a is a unit if there exists b in R such that a*b = 1 (the multiplicative identity). Iterate over all ring elements to find such a b.")
    (context "Available: ring-elements, ring-mul, ring-one, ring-equal?. The ring is finite, so you can check all elements. Note: you only need to check a*b = 1 (right inverse); for finite rings, a right inverse is automatically a left inverse.")
    (before "(define (is-unit? r a)
  ;; TODO: implement
  ;; Check if ∃ b in R such that a*b = 1
  #f)")
    (test-file "lattice/algebra/test-ring-field.ss")
    (test-forms ("(assert-true (is-unit? (make-ring-zn 6) 1))"
                 "(assert-true (is-unit? (make-ring-zn 6) 5))"
                 "(assert-false (is-unit? (make-ring-zn 6) 2))"
                 "(assert-false (is-unit? (make-ring-zn 6) 3))"
                 "(assert-equal 4 (length (ring-units (make-ring-zn 5))))"))
    (available-modules (prelude group ring))
    (hints ("Iterate over all elements b in ring-elements"
            "Check if ring-mul r a b equals ring-one r"
            "Return #t on first match, #f if no match found"))
    (reference-solution "(define (is-unit? r a)
  (let ([elems (ring-elements r)]
        [one (ring-one r)])
    (let loop ([bs elems])
      (if (null? bs)
          #f
          (if (ring-equal? r (ring-mul r a (car bs)) one)
              #t
              (loop (cdr bs)))))))")
    (alternatives ())
    (source-commit "6179bdca")
    (difficulty 2))

) ;; end tasks
