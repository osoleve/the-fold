;;; test-galois.ss — Tests for Galois field arithmetic
(load "core/testing/test-framework.ss")
(load "lattice/algebra/galois.ss")

(test-group "galois-fields"

  ;;; ==========================================================================
  ;;; GF(p) Prime Fields
  ;;; ==========================================================================

  (test-group "gf-prime"

    (define-test "gf-prime creates field with correct order"
      (let ([F (gf-prime 7)])
        (assert-equal 7 (gf-order F))))

    (define-test "gf-prime arithmetic works"
      (let* ([F (gf-prime 5)]
             [add (field-add-op F)]
             [mul (field-mul-op F)])
        ;; 3 + 4 = 2 (mod 5)
        (assert-equal 2 (add 3 4))
        ;; 3 * 4 = 2 (mod 5)
        (assert-equal 2 (mul 3 4))))

    (define-test "gf-prime inversion works"
      (let* ([F (gf-prime 7)]
             [mul (field-mul-op F)])
        ;; 3 * 5 = 15 = 1 (mod 7)
        (assert-equal 1 (mul 3 (field-inv F 3)))))

    (define-test "gf-characteristic returns p"
      (let ([F (gf-prime 11)])
        (assert-equal 11 (gf-characteristic F)))))

  ;;; ==========================================================================
  ;;; GF(2^n) Binary Fields
  ;;; ==========================================================================

  (test-group "gf2n-binary"

    (define-test "gf2n addition is XOR"
      (let* ([F (make-gf2-8-aes)]
             [a (gf2n-from-int F #x57)]
             [b (gf2n-from-int F #x83)]
             [sum (field-add F a b)])
        ;; 0x57 XOR 0x83 = 0xD4
        (assert-equal #xD4 (gf2n-to-int sum))))

    (define-test "gf2n multiplication works (AES example)"
      (let* ([F (make-gf2-8-aes)]
             [a (gf2n-from-int F #x57)]
             [b (gf2n-from-int F #x83)]
             [prod (field-mul F a b)])
        ;; Known AES multiplication result: 0x57 * 0x83 = 0xC1 in GF(2^8)
        (assert-equal #xC1 (gf2n-to-int prod))))

    (define-test "gf2n multiplication by 1 is identity"
      (let* ([F (make-gf2-8-aes)]
             [a (gf2n-from-int F #xAB)]
             [one (field-one F)]
             [prod (field-mul F a one)])
        (assert-equal #xAB (gf2n-to-int prod))))

    (define-test "gf2n inversion works"
      (let* ([F (make-gf2-8-aes)]
             [a (gf2n-from-int F #x53)]
             [a-inv (field-inv F a)]
             [prod (field-mul F a a-inv)])
        (assert-equal 1 (gf2n-to-int prod))))

    (define-test "gf2n negation is identity (characteristic 2)"
      (let* ([F (make-gf2-8-aes)]
             [a (gf2n-from-int F #x42)]
             [neg-a (field-neg F a)])
        (assert-equal (gf2n-to-int a) (gf2n-to-int neg-a))))

    (define-test "gf2-4 has 16 elements"
      (let ([F (make-gf2-4)])
        (assert-equal 16 (gf-order F))))

    (define-test "gf2-8 field axioms hold"
      (let* ([F (make-gf2-8-aes)]
             [a (gf2n-from-int F 42)]
             [b (gf2n-from-int F 17)]
             [c (gf2n-from-int F 99)]
             [add (field-add-op F)]
             [mul (field-mul-op F)]
             [zero (field-zero F)]
             [one (field-one F)]
             [eq? (field-equal-fn F)])
        ;; Additive identity
        (assert-true (eq? a (add a zero)))
        ;; Multiplicative identity
        (assert-true (eq? a (mul a one)))
        ;; Commutativity of addition
        (assert-true (eq? (add a b) (add b a)))
        ;; Commutativity of multiplication
        (assert-true (eq? (mul a b) (mul b a)))
        ;; Distributivity
        (assert-true (eq? (mul a (add b c))
                          (add (mul a b) (mul a c)))))))

  ;;; ==========================================================================
  ;;; GF(p^n) Extension Fields
  ;;; ==========================================================================

  (test-group "gf-extension"

    (define-test "gf-p2 creates field with p^2 elements"
      (let ([F (make-gf-p2 3)])
        (assert-equal 9 (gf-order F))))

    (define-test "gf-p2 arithmetic works"
      (let* ([F (make-gf-p2 3)]
             [zero (field-zero F)]
             [one (field-one F)]
             [sum (field-add F one one)]
             [eq? (field-equal-fn F)])
        ;; 1 + 1 = 2 (as constant polynomial)
        (let ([coeffs (gf-ext-coeffs sum)])
          (assert-equal '(2) coeffs))))

    (define-test "gf-extension multiplication reduces mod irreducible"
      ;; In GF(3^2) with x^2 + 1: x * x = x^2 = -1 = 2
      (let* ([F (make-gf-p2 3)]
             [base (gf-prime 3)]
             [x-elem (make-gf-ext-element '(0 1)
                       (make-polynomial base '(1 0 1))
                       base)]
             [x-squared (field-mul F x-elem x-elem)]
             [coeffs (gf-ext-coeffs x-squared)])
        ;; x^2 mod (x^2 + 1) = -1 = 2 in GF(3)
        (assert-equal '(2) coeffs))))

  ;;; ==========================================================================
  ;;; Irreducible Polynomial Tests
  ;;; ==========================================================================

  (test-group "irreducibility"

    (define-test "x^2 + 1 is irreducible over GF(3)"
      (let* ([F (gf-prime 3)]
             ;; x^2 + 1 = [1, 0, 1]
             [p (make-polynomial F '(1 0 1))])
        (assert-true (poly-irreducible? p))))

    (define-test "x^2 - 1 is reducible over GF(3)"
      (let* ([F (gf-prime 3)]
             ;; x^2 - 1 = x^2 + 2 = [2, 0, 1] in GF(3)
             [p (make-polynomial F '(2 0 1))])
        ;; x^2 - 1 = (x-1)(x+1) = (x+2)(x+1) in GF(3)
        (assert-false (poly-irreducible? p))))

    (define-test "linear polynomials are irreducible"
      (let* ([F (gf-prime 5)]
             ;; x + 3 = [3, 1]
             [p (make-polynomial F '(3 1))])
        (assert-true (poly-irreducible? p))))

    (define-test "constants are not irreducible"
      (let* ([F (gf-prime 5)]
             [p (make-polynomial F '(3))])
        (assert-false (poly-irreducible? p))))

    ;; Rabin's test specific cases
    (define-test "x^4 + x + 1 is irreducible over GF(2)"
      ;; Classic GF(2^4) irreducible
      (let* ([F (gf-prime 2)]
             ;; x^4 + x + 1 = [1, 1, 0, 0, 1] (ascending)
             [p (make-polynomial F '(1 1 0 0 1))])
        (assert-true (poly-irreducible? p))))

    (define-test "x^4 + 1 is reducible over GF(2)"
      ;; x^4 + 1 = (x+1)^4 in characteristic 2
      (let* ([F (gf-prime 2)]
             [p (make-polynomial F '(1 0 0 0 1))])
        (assert-false (poly-irreducible? p))))

    (define-test "x^8 + x^4 + x^3 + x + 1 is irreducible over GF(2)"
      ;; AES polynomial
      (let* ([F (gf-prime 2)]
             ;; [1, 1, 0, 1, 1, 0, 0, 0, 1] ascending
             [p (make-polynomial F '(1 1 0 1 1 0 0 0 1))])
        (assert-true (poly-irreducible? p))))

    (define-test "Rabin test with larger prime field"
      ;; x^3 + x + 1 over GF(7) - should be irreducible (no roots, no quadratic factors)
      (let* ([F (gf-prime 7)]
             [p (make-polynomial F '(1 1 0 1))])  ; 1 + x + x^3
        ;; First verify no roots
        (assert-true (poly-irreducible? p))))

    (define-test "Rabin test correctly identifies reducible degree-4"
      ;; x^4 - 1 = (x-1)(x+1)(x^2+1) over GF(5)
      (let* ([F (gf-prime 5)]
             ;; x^4 - 1 = x^4 + 4 in GF(5) = [4, 0, 0, 0, 1]
             [p (make-polynomial F '(4 0 0 0 1))])
        (assert-false (poly-irreducible? p)))))

  ;;; ==========================================================================
  ;;; Primitive Elements
  ;;; ==========================================================================

  (test-group "primitive-elements"

    (define-test "2 is primitive in GF(5)"
      (let ([F (gf-prime 5)])
        ;; 2^1 = 2, 2^2 = 4, 2^3 = 3, 2^4 = 1 (generates all non-zero)
        (assert-true (gf-primitive? F 2))))

    (define-test "4 is not primitive in GF(5)"
      (let ([F (gf-prime 5)])
        ;; 4^1 = 4, 4^2 = 1 (only generates {1, 4})
        (assert-false (gf-primitive? F 4))))

    (define-test "gf-find-primitive finds a generator"
      (let* ([F (gf-prime 7)]
             [g (gf-find-primitive F)])
        (assert-true (gf-primitive? F g)))))

  ;;; ==========================================================================
  ;;; Trace and Norm
  ;;; ==========================================================================

  (test-group "trace-and-norm"

    (define-test "trace of element in GF(2^4)"
      (let* ([F (make-gf2-4)]
             [a (gf2n-from-int F 3)]  ; 0011 = x + 1
             ;; Trace(a) = a + a^2 + a^4 + a^8
             ;; In GF(2^4), a^16 = a, so trace sums 4 conjugates
             [tr (gf-trace F a 4)])
        ;; Just verify it returns an element (detailed calculation complex)
        (assert-true (gf2n-element? tr))))

    (define-test "norm of 1 is 1"
      (let* ([F (make-gf2-4)]
             [one (field-one F)]
             [n (gf-norm F one 4)])
        ;; Norm(1) = 1 * 1 * 1 * 1 = 1
        (assert-equal 1 (gf2n-to-int n)))))

  ;;; ==========================================================================
  ;;; Edge Cases
  ;;; ==========================================================================

  (test-group "edge-cases"

    (define-test "division by zero check would error"
      ;; Just verify zero is zero
      (let* ([F (make-gf2-8-aes)]
             [zero (field-zero F)])
        (assert-equal 0 (gf2n-to-int zero))))

    (define-test "gf2n power works"
      (let* ([F (make-gf2-8-aes)]
             [g (gf2n-from-int F 3)]  ; A common generator
             [g-cubed (field-power F g 3)])
        ;; Verify it's computed correctly by manual calculation
        (let* ([g2 (field-mul F g g)]
               [expected (field-mul F g2 g)])
          (assert-true (field-equal? F g-cubed expected)))))

    (define-test "gf2-8 order is 256"
      (let ([F (make-gf2-8-aes)])
        (assert-equal 256 (gf-order F)))))

  ;;; ==========================================================================
  ;;; Large Field Tests (no enumeration)
  ;;; ==========================================================================

  (test-group "large-fields"

    (define-test "make-gf2n-large creates field instantly for n=128"
      ;; This would hang/crash if we tried to enumerate 2^128 elements
      (let* ([irred-128 (+ (bitwise-arithmetic-shift-left 1 128)
                           (bitwise-arithmetic-shift-left 1 7)
                           (bitwise-arithmetic-shift-left 1 2)
                           (bitwise-arithmetic-shift-left 1 1)
                           1)]  ; x^128 + x^7 + x^2 + x + 1
             [F (make-gf2n-large 128 irred-128)])
        ;; Verify metadata works
        (assert-equal (bitwise-arithmetic-shift-left 1 128) (gf-order F))
        (assert-equal 2 (gf-characteristic F))))

    (define-test "gf2n-large arithmetic works"
      (let* ([irred-64 (+ (bitwise-arithmetic-shift-left 1 64)
                          (bitwise-arithmetic-shift-left 1 4)
                          (bitwise-arithmetic-shift-left 1 3)
                          (bitwise-arithmetic-shift-left 1 1)
                          1)]  ; x^64 + x^4 + x^3 + x + 1
             [F (make-gf2n-large 64 irred-64)]
             [a (gf2n-from-int F #xDEADBEEFCAFEBABE)]
             [b (gf2n-from-int F #x1234567890ABCDEF)]
             [one (field-one F)]
             [zero (field-zero F)])
        ;; Basic axioms
        (assert-true (field-equal? F (field-add F a zero) a))
        (assert-true (field-equal? F (field-mul F a one) a))
        ;; Addition is XOR
        (assert-equal (bitwise-xor #xDEADBEEFCAFEBABE #x1234567890ABCDEF)
                      (gf2n-to-int (field-add F a b)))))

    (define-test "make-field-zp-large creates field instantly"
      ;; Use a large prime (Mersenne prime 2^61 - 1)
      (let* ([p (- (bitwise-arithmetic-shift-left 1 61) 1)]
             [F (make-field-zp-large p)])
        (assert-equal p (gf-order F))
        (assert-equal p (gf-characteristic F))))

    (define-test "large prime field arithmetic works"
      (let* ([p 1000000007]  ; A billion-ish prime
             [F (make-field-zp-large p)])
        ;; Basic operations
        (assert-equal 0 (field-add F 500000000 500000007))
        (assert-equal 1 (field-mul F 500000004 (field-inv F 500000004)))))))

(run-all-tests)
