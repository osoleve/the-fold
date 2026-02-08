;;; lattice/algebra/test-ring-field.ss — Tests for Ring and Field Theory
;;; Run with: scheme --script lattice/algebra/test-ring-field.ss

(load "core/testing/test-framework.ss")
(load "lattice/algebra/field.ss")  ; loads ring.ss transitively

;;; ============================================================
;;; Ring Tests
;;; ============================================================

(test-group "ring-construction"
  (define-test "make-ring-zn creates valid ring"
    (let ([z6 (make-ring-zn 6)])
      (assert-true (ring? z6))
      (assert-equal 6 (ring-order z6))
      (assert-equal 0 (ring-zero z6))
      (assert-equal 1 (ring-one z6))))

  (define-test "ring-elements lists all elements"
    (let ([z4 (make-ring-zn 4)])
      (assert-equal '(0 1 2 3) (ring-elements z4))))

  (define-test "ring? rejects non-rings"
    (assert-false (ring? 42))
    (assert-false (ring? '(not-a-ring)))
    (assert-false (ring? '()))))

(test-group "ring-arithmetic"
  (define-test "addition mod n"
    (let ([z6 (make-ring-zn 6)])
      (assert-equal 1 (ring-add z6 4 3))
      (assert-equal 0 (ring-add z6 3 3))
      (assert-equal 5 (ring-add z6 0 5))))

  (define-test "multiplication mod n"
    (let ([z6 (make-ring-zn 6)])
      (assert-equal 0 (ring-mul z6 2 3))
      (assert-equal 4 (ring-mul z6 2 2))
      (assert-equal 1 (ring-mul z6 5 5))))

  (define-test "negation mod n"
    (let ([z6 (make-ring-zn 6)])
      (assert-equal 4 (ring-neg z6 2))
      (assert-equal 0 (ring-neg z6 0))
      (assert-equal 1 (ring-neg z6 5))))

  (define-test "subtraction mod n"
    (let ([z6 (make-ring-zn 6)])
      (assert-equal 2 (ring-sub z6 5 3))
      (assert-equal 4 (ring-sub z6 1 3))))

  (define-test "power via repeated squaring"
    (let ([z7 (make-ring-zn 7)])
      (assert-equal 1 (ring-power z7 3 0))
      (assert-equal 3 (ring-power z7 3 1))
      (assert-equal 2 (ring-power z7 3 2))
      (assert-equal 6 (ring-power z7 3 3))
      ;; Fermat's little theorem: a^(p-1) ≡ 1 (mod p) for prime p
      (assert-equal 1 (ring-power z7 3 6))))

  (define-test "ring-sum and ring-product"
    (let ([z5 (make-ring-zn 5)])
      (assert-equal 0 (ring-sum z5 '(1 2 3 4)))
      (assert-equal 4 (ring-product z5 '(1 2 3 4))))))

(test-group "ring-properties"
  (define-test "Z_n is commutative"
    (assert-true (is-commutative-ring? (make-ring-zn 6))))

  (define-test "Z_6 has zero divisors"
    (let ([z6 (make-ring-zn 6)])
      (assert-true (has-zero-divisors? z6))
      (assert-true (is-zero-divisor? z6 2))
      (assert-true (is-zero-divisor? z6 3))
      (assert-false (is-zero-divisor? z6 1))
      (assert-false (is-zero-divisor? z6 5))))

  (define-test "Z_5 has no zero divisors (prime)"
    (assert-false (has-zero-divisors? (make-ring-zn 5))))

  (define-test "Z_5 is an integral domain"
    (assert-true (is-integral-domain? (make-ring-zn 5))))

  (define-test "Z_6 is not an integral domain"
    (assert-false (is-integral-domain? (make-ring-zn 6))))

  (define-test "units in Z_6"
    (let* ([z6 (make-ring-zn 6)]
           [units (ring-units z6)])
      (assert-true (is-unit? z6 1))
      (assert-true (is-unit? z6 5))
      (assert-false (is-unit? z6 2))
      (assert-false (is-unit? z6 3))
      (assert-equal 2 (length units))))

  (define-test "every nonzero element of Z_5 is a unit"
    (let* ([z5 (make-ring-zn 5)]
           [units (ring-units z5)])
      (assert-equal 4 (length units)))))

(test-group "ring-homomorphisms"
  (define-test "natural projection Z_6 → Z_3"
    (let* ([z6 (make-ring-zn 6)]
           [z3 (make-ring-zn 3)]
           [phi (lambda (a) (modulo a 3))]
           [h (make-ring-homomorphism z6 z3 phi)])
      (assert-true (ring-hom? h))
      (assert-equal 0 (ring-hom-apply h 3))
      (assert-equal 2 (ring-hom-apply h 5))
      (assert-true (is-valid-ring-homomorphism? h))))

  (define-test "kernel of Z_6 → Z_3 is {0, 3}"
    (let* ([z6 (make-ring-zn 6)]
           [z3 (make-ring-zn 3)]
           [phi (lambda (a) (modulo a 3))]
           [h (make-ring-homomorphism z6 z3 phi)]
           [ker (ring-hom-kernel h)])
      (assert-equal 2 (length ker))
      (assert-true (if (member 0 ker) #t #f))
      (assert-true (if (member 3 ker) #t #f))))

  (define-test "image of Z_6 → Z_3 is all of Z_3"
    (let* ([z6 (make-ring-zn 6)]
           [z3 (make-ring-zn 3)]
           [phi (lambda (a) (modulo a 3))]
           [h (make-ring-homomorphism z6 z3 phi)]
           [img (ring-hom-image h)])
      (assert-equal 3 (length img)))))

(test-group "ideals"
  (define-test "principal ideal in Z_6"
    (let* ([z6 (make-ring-zn 6)]
           [i2 (make-principal-ideal z6 2)])
      (assert-true (ideal? i2))
      ;; <2> in Z_6 = {0, 2, 4}
      (assert-equal 3 (length (ideal-elements i2)))
      (assert-true (is-valid-ideal? i2))))

  (define-test "<3> in Z_6 = {0, 3}"
    (let* ([z6 (make-ring-zn 6)]
           [i3 (make-principal-ideal z6 3)])
      (assert-equal 2 (length (ideal-elements i3)))
      (assert-true (is-valid-ideal? i3))))

  (define-test "<0> is the zero ideal"
    (let* ([z6 (make-ring-zn 6)]
           [i0 (make-principal-ideal z6 0)])
      (assert-equal 1 (length (ideal-elements i0)))
      (assert-true (is-valid-ideal? i0))))

  (define-test "ideal sum"
    (let* ([z6 (make-ring-zn 6)]
           [i2 (make-principal-ideal z6 2)]
           [i3 (make-principal-ideal z6 3)]
           [sum (ideal-sum i2 i3)])
      ;; <2> + <3> should be all of Z_6 since gcd(2,3)=1
      (assert-equal 6 (length (ideal-elements sum)))))

  (define-test "prime ideal in Z_6"
    (let* ([z6 (make-ring-zn 6)]
           [i2 (make-principal-ideal z6 2)]
           [i3 (make-principal-ideal z6 3)])
      ;; <2> and <3> are prime ideals in Z_6
      (assert-true (is-prime-ideal? i2))
      (assert-true (is-prime-ideal? i3))))

  (define-test "maximal ideal in Z_6"
    (let* ([z6 (make-ring-zn 6)]
           [i2 (make-principal-ideal z6 2)]
           [i3 (make-principal-ideal z6 3)])
      ;; In Z_6, <2> and <3> are maximal
      (assert-true (is-maximal-ideal? i2))
      (assert-true (is-maximal-ideal? i3)))))

;;; ============================================================
;;; Field Tests
;;; ============================================================

(test-group "field-construction"
  (define-test "make-field-zp creates valid field"
    (let ([gf5 (make-field-zp 5)])
      (assert-true (field? gf5))
      (assert-equal 5 (length (field-elements gf5)))
      (assert-equal 0 (field-zero gf5))
      (assert-equal 1 (field-one gf5))))

  (define-test "field metadata"
    (let ([gf7 (make-field-zp 7)])
      (assert-equal 7 (field-meta-ref gf7 'order))
      (assert-equal 7 (field-meta-ref gf7 'characteristic))
      (assert-false (field-meta-ref gf7 'nonexistent))))

  (define-test "Q-field is a field"
    (assert-true (field? Q-field))
    (assert-equal 0 (field-zero Q-field))
    (assert-equal 1 (field-one Q-field)))

  (define-test "R-field is a field"
    (assert-true (field? R-field))
    (assert-equal 0.0 (field-zero R-field))
    (assert-equal 1.0 (field-one R-field)))

  (define-test "field? rejects non-fields"
    (assert-false (field? 42))
    (assert-false (field? (make-ring-zn 6)))))

(test-group "field-arithmetic"
  (define-test "addition in GF(7)"
    (let ([gf7 (make-field-zp 7)])
      (assert-equal 5 (field-add gf7 3 2))
      (assert-equal 0 (field-add gf7 4 3))
      (assert-equal 6 (field-add gf7 0 6))))

  (define-test "multiplication in GF(7)"
    (let ([gf7 (make-field-zp 7)])
      (assert-equal 6 (field-mul gf7 3 2))
      (assert-equal 5 (field-mul gf7 4 3))
      (assert-equal 1 (field-mul gf7 4 2))))

  (define-test "subtraction in GF(7)"
    (let ([gf7 (make-field-zp 7)])
      (assert-equal 1 (field-sub gf7 3 2))
      (assert-equal 5 (field-sub gf7 1 3))))

  (define-test "division in GF(7)"
    (let ([gf7 (make-field-zp 7)])
      ;; 3/2 mod 7: 2^-1 mod 7 = 4, so 3*4 mod 7 = 5
      (assert-equal 5 (field-div gf7 3 2))
      ;; 1/3 mod 7: 3^-1 mod 7 = 5, so 1*5 mod 7 = 5
      (assert-equal 5 (field-div gf7 1 3))))

  (define-test "inverse in GF(7)"
    (let ([gf7 (make-field-zp 7)])
      ;; Every non-zero element: a * a^-1 = 1
      (assert-equal 1 (field-mul gf7 2 (field-inv gf7 2)))
      (assert-equal 1 (field-mul gf7 3 (field-inv gf7 3)))
      (assert-equal 1 (field-mul gf7 6 (field-inv gf7 6)))))

  (define-test "power with negative exponents"
    (let ([gf7 (make-field-zp 7)])
      ;; 3^(-1) mod 7 = 5
      (assert-equal 5 (field-power gf7 3 -1))
      ;; 3^0 = 1
      (assert-equal 1 (field-power gf7 3 0))
      ;; 3^3 mod 7 = 27 mod 7 = 6
      (assert-equal 6 (field-power gf7 3 3))))

  (define-test "Q-field arithmetic"
    (assert-equal 7/3 (field-add Q-field 1/3 2))
    (assert-equal 2/3 (field-mul Q-field 1/3 2))
    (assert-equal 1/6 (field-div Q-field 1/3 2))
    (assert-equal 3 (field-inv Q-field 1/3))))

(test-group "field-conversion"
  (define-test "field->ring preserves structure"
    (let* ([gf5 (make-field-zp 5)]
           [r (field->ring gf5)])
      (assert-true (ring? r))
      (assert-equal 5 (ring-order r))
      ;; Ring arithmetic matches field
      (assert-equal (field-add gf5 3 4) (ring-add r 3 4))
      (assert-equal (field-mul gf5 3 4) (ring-mul r 3 4))))

  (define-test "field->ring yields integral domain"
    (let* ([gf5 (make-field-zp 5)]
           [r (field->ring gf5)])
      (assert-true (is-integral-domain? r)))))

(test-group "mod-inverse"
  (define-test "mod-inverse basic cases"
    (assert-equal 4 (mod-inverse 2 7))   ; 2*4 ≡ 1 (mod 7)
    (assert-equal 5 (mod-inverse 3 7))   ; 3*5 ≡ 1 (mod 7)
    (assert-equal 1 (mod-inverse 1 7))   ; 1*1 ≡ 1 (mod 7)
    (assert-equal 6 (mod-inverse 6 7)))  ; 6*6 ≡ 1 (mod 7)

  (define-test "mod-inverse larger prime"
    (let ([p 13])
      ;; Verify a * a^-1 ≡ 1 (mod p) for several values
      (assert-equal 1 (modulo (* 5 (mod-inverse 5 p)) p))
      (assert-equal 1 (modulo (* 7 (mod-inverse 7 p)) p))
      (assert-equal 1 (modulo (* 11 (mod-inverse 11 p)) p))))

  (define-test "mod-inverse errors for non-coprime"
    (assert-error (lambda () (mod-inverse 2 6)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
