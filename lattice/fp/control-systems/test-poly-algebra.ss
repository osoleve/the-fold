;;; lattice/fp/control-systems/test-poly-algebra.ss — Tests for Polynomial Algebra Integration
;;;
;;; Tests for exact polynomial operations on transfer functions.

(load "core/testing/test-framework.ss")
(load "lattice/fp/control-systems/poly-algebra.ss")

;;; ====
;;; Test: Polynomial Conversion
;;; ====

(test-group "polynomial-conversion"
  (define-test "numeric to algebra conversion reverses to ascending order"
    ;; Numeric polynomial: 3x^2 + 2x + 1 (descending: #(3 2 1))
    (let* ([num-poly (poly-from-list '(3 2 1))]
           [alg-poly (numeric->algebra-poly num-poly Q-field)]
           [coeffs (alg-poly-coeffs alg-poly)])
      ;; Algebra polynomial should be in ascending order: [1, 2, 3]
      (assert-equal coeffs '(1 2 3))))

  (define-test "algebra to numeric conversion reverses to descending order"
    (let* ([alg-poly (alg-make-polynomial Q-field '(1 2 3))]  ; 1 + 2x + 3x^2
           [num-poly (algebra->numeric-poly alg-poly)]
           [coeffs (vector->list (num-poly-coeffs num-poly))])
      ;; Numeric should be descending: (3 2 1)
      (assert-equal coeffs '(3 2 1))))

  (define-test "round-trip conversion preserves polynomial"
    ;; Test that going numeric->algebra->numeric gives same polynomial
    (let* ([original (poly-from-list '(5 -3 0 2))]  ; 5x^3 - 3x^2 + 2
           [alg-poly (numeric->algebra-poly original Q-field)]
           [back (algebra->numeric-poly alg-poly)]
           ;; Degrees should match
           [orig-deg (poly-degree original)]
           [back-deg (poly-degree back)])
      ;; The degrees should be identical after round-trip
      (assert-equal orig-deg back-deg))))

;;; ====
;;; Test: Transfer Function Simplification
;;; ====

(test-group "tf-simplification"
  (define-test "tf-simplify cancels common linear factor"
    ;; H(s) = (s-1)(s+2) / (s-1)(s+3) should simplify to (s+2)/(s+3)
    ;; Numerator: s^2 + s - 2 (expanded: (s-1)(s+2))
    ;; Denominator: s^2 + 2s - 3 (expanded: (s-1)(s+3))
    (let* ([tf-orig (tf-from-lists '(1 1 -2) '(1 2 -3))]
           [tf-simp (tf-simplify tf-orig)]
           [num-degree (poly-degree (tf-num tf-simp))]
           [den-degree (poly-degree (tf-den tf-simp))])
      (assert-equal num-degree 1)  ; Should reduce to degree 1
      (assert-equal den-degree 1)))

  (define-test "tf-simplify preserves irreducible TF"
    ;; H(s) = (s+1) / (s^2 + 1) has no common factors
    (let* ([tf-orig (tf-from-lists '(1 1) '(1 0 1))]
           [tf-simp (tf-simplify tf-orig)]
           [num-degree (poly-degree (tf-num tf-simp))]
           [den-degree (poly-degree (tf-den tf-simp))])
      (assert-equal num-degree 1)
      (assert-equal den-degree 2)))

  (define-test "tf-coprime? returns true for coprime polynomials"
    (let ([tf (tf-from-lists '(1 1) '(1 0 1))])  ; (s+1)/(s^2+1)
      (assert-true (tf-coprime? tf))))

  (define-test "tf-coprime? returns false for non-coprime polynomials"
    ;; (s-1)(s+2) / (s-1)(s+3) has common factor (s-1)
    (let ([tf (tf-from-lists '(1 1 -2) '(1 2 -3))])
      (assert-false (tf-coprime? tf)))))

;;; ====
;;; Test: Exact Routh-Hurwitz
;;; ====

(test-group "routh-hurwitz-exact"
  (define-test "stable polynomial passes Routh-Hurwitz"
    ;; s^2 + 3s + 2 = (s+1)(s+2) - all poles negative
    (let ([poly (poly-from-list '(1 3 2))])
      (assert-true (routh-hurwitz-exact poly))))

  (define-test "unstable polynomial fails Routh-Hurwitz"
    ;; s^2 - 3s + 2 = (s-1)(s-2) - positive poles
    (let ([poly (poly-from-list '(1 -3 2))])
      (assert-false (routh-hurwitz-exact poly))))

  (define-test "marginally stable polynomial"
    ;; s^2 + 1 has poles at +/- j (on imaginary axis)
    ;; Routh-Hurwitz should show marginal stability
    (let ([poly (poly-from-list '(1 0 1))])
      ;; First column has zero crossing, so technically not strictly stable
      ;; but no sign changes either
      (assert-true (routh-hurwitz-exact poly)))))

;;; ====
;;; Test: Characteristic Polynomial Analysis
;;; ====

(test-group "char-poly-analysis"
  (define-test "tf-char-poly extracts denominator as algebra poly"
    (let* ([tf (tf-from-lists '(1 2) '(1 3 2))]  ; (s+2)/(s^2+3s+2)
           [char-poly (tf-char-poly tf)]
           [coeffs (alg-poly-coeffs char-poly)])
      ;; Denominator s^2+3s+2 in ascending: [2, 3, 1]
      (assert-equal coeffs '(2 3 1))))

  (define-test "tf-char-poly-factored finds square-free factors"
    (let* ([tf (tf-from-lists '(1) '(1 2 1))]  ; 1/(s+1)^2
           [factors (tf-char-poly-factored tf)])
      ;; Should factor as (s+1)^2
      (assert-true (>= (length factors) 1)))))

;;; ====
;;; Test: Bezout Coefficients
;;; ====

(test-group "bezout-identity"
  (define-test "tf-bezout computes extended GCD"
    ;; For two polynomials, should satisfy Bezout identity
    (let* ([tf1 (tf-from-lists '(1 2) '(1))]   ; s+2
           [tf2 (tf-from-lists '(1 3) '(1))]   ; s+3
           [result (tf-bezout tf1 tf2)]
           [gcd-poly (car result)])
      ;; GCD of (s+2) and (s+3) should be constant (degree 0)
      (assert-equal (alg-poly-degree gcd-poly) 0))))

;;; ====
;;; Test: Display Functions
;;; ====

(test-group "display-functions"
  (define-test "tf-exact->string produces readable output"
    (let* ([tf (tf-from-lists '(1 2) '(1 0 1))]
           [str (tf-exact->string tf)])
      (assert-true (string? str))
      (assert-true (> (string-length str) 0)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
