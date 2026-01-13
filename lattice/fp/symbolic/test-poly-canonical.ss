;;; lattice/fp/symbolic/test-poly-canonical.ss — Tests for Polynomial Canonical Form
;;;
;;; Tests for polynomial algebra integration with symbolic math.

(load "core/testing/test-framework.ss")
(load "lattice/fp/symbolic/poly-canonical.ss")

;;; ============================================================
;;; Test: Expression to Polynomial Conversion
;;; ============================================================

(test-group "expr-to-poly-conversion"
  (define-test "convert simple variable"
    (let* ([expr (var 'x)]
           [poly (expr->polynomial expr 'x Q-field-sym)])
      (assert-true (not (not poly)))  ; Check poly is truthy (not #f)
      (assert-equal (poly-degree poly) 1)))

  (define-test "convert constant"
    (let* ([expr (num 5)]
           [poly (expr->polynomial expr 'x Q-field-sym)])
      (assert-true (not (not poly)))  ; Check poly is truthy (not #f)
      (assert-equal (poly-degree poly) 0)))

  (define-test "convert x^2 + 2x + 1"
    (let* ([x (var 'x)]
           [expr (sum (sum (power x (num 2))
                           (product (num 2) x))
                      (num 1))]
           [poly (expr->polynomial expr 'x Q-field-sym)])
      (assert-true (not (not poly)))  ; Check poly is truthy (not #f)
      (assert-equal (poly-degree poly) 2)))

  (define-test "reject non-polynomial expression"
    (let* ([x (var 'x)]
           [expr (sym-sin x)]  ; sin(x) is not polynomial
           [poly (expr->polynomial expr 'x Q-field-sym)])
      (assert-false poly))))

;;; ============================================================
;;; Test: Polynomial to Expression Conversion
;;; ============================================================

(test-group "poly-to-expr-conversion"
  (define-test "convert degree 0 polynomial"
    (let* ([poly (make-polynomial Q-field-sym '(5))]
           [expr (polynomial->expr poly 'x)])
      (assert-true (num? expr))
      (assert-equal (num-val expr) 5)))

  (define-test "convert degree 1 polynomial"
    (let* ([poly (make-polynomial Q-field-sym '(3 2))]  ; 3 + 2x
           [expr (polynomial->expr poly 'x)])
      (assert-true (sum? expr))))

  (define-test "convert degree 2 polynomial with zero coefficient"
    (let* ([poly (make-polynomial Q-field-sym '(1 0 1))]  ; 1 + x^2
           [expr (polynomial->expr poly 'x)])
      ;; Should skip the zero coefficient term
      (assert-true (sum? expr)))))

;;; ============================================================
;;; Test: Expression Degree
;;; ============================================================

(test-group "expr-degree"
  (define-test "constant has degree 0"
    (assert-equal (expr-degree (num 5) 'x) 0))

  (define-test "variable x has degree 1 in x"
    (assert-equal (expr-degree (var 'x) 'x) 1))

  (define-test "variable y has degree 0 in x"
    (assert-equal (expr-degree (var 'y) 'x) 0))

  (define-test "x^3 has degree 3"
    (assert-equal (expr-degree (power (var 'x) (num 3)) 'x) 3))

  (define-test "x*y has degree 1 in x"
    (assert-equal (expr-degree (product (var 'x) (var 'y)) 'x) 1))

  (define-test "x^2 + x + 1 has degree 2"
    (let* ([x (var 'x)]
           [expr (sum (power x (num 2)) (sum x (num 1)))])
      (assert-equal (expr-degree expr 'x) 2))))

;;; ============================================================
;;; Test: Polynomial Expression Predicate
;;; ============================================================

(test-group "polynomial-expr-predicate"
  (define-test "number is polynomial"
    (assert-true (polynomial-expr? (num 5) 'x)))

  (define-test "variable is polynomial"
    (assert-true (polynomial-expr? (var 'x) 'x)))

  (define-test "sum of polynomials is polynomial"
    (let ([expr (sum (var 'x) (num 1))])
      (assert-true (polynomial-expr? expr 'x))))

  (define-test "product of polynomials is polynomial"
    (let ([expr (product (var 'x) (var 'x))])
      (assert-true (polynomial-expr? expr 'x))))

  (define-test "sin(x) is not polynomial"
    (assert-false (polynomial-expr? (sym-sin (var 'x)) 'x)))

  (define-test "x^(1/2) is not polynomial"
    (assert-false (polynomial-expr? (power (var 'x) (quotient (num 1) (num 2))) 'x))))

;;; ============================================================
;;; Test: GCD-Based Simplification
;;; ============================================================

(test-group "gcd-simplification"
  (define-test "simplify-rational reduces common factors"
    ;; (x^2 - 1) / (x - 1) = (x+1)(x-1)/(x-1) = x+1
    (let* ([x (var 'x)]
           [numer (difference (power x (num 2)) (num 1))]  ; x^2 - 1
           [denom (difference x (num 1))]                   ; x - 1
           [expr (quotient numer denom)]
           [simplified (simplify-rational expr 'x)])
      ;; Result should have lower degree
      (if (quotient? simplified)
          (assert-true (< (expr-degree (quot-numer simplified) 'x) 2))
          (assert-equal (expr-degree simplified 'x) 1))))

  (define-test "simplify-rational preserves irreducible"
    ;; (x + 1) / (x^2 + 1) has no common factors
    (let* ([x (var 'x)]
           [numer (sum x (num 1))]
           [denom (sum (power x (num 2)) (num 1))]
           [expr (quotient numer denom)]
           [simplified (simplify-rational expr 'x)])
      ;; Should remain a quotient with same degrees
      (assert-true (quotient? simplified)))))

;;; ============================================================
;;; Test: Polynomial GCD
;;; ============================================================

(test-group "poly-gcd"
  (define-test "GCD of coprime polynomials is constant"
    (let* ([x (var 'x)]
           [expr1 (sum x (num 1))]        ; x + 1
           [expr2 (sum x (num 2))]        ; x + 2
           [gcd-expr (expr-poly-gcd expr1 expr2 'x)])
      (assert-equal (expr-degree gcd-expr 'x) 0)))

  (define-test "GCD of polynomial with itself is itself"
    (let* ([x (var 'x)]
           [expr (sum (power x (num 2)) x)]  ; x^2 + x
           [gcd-expr (expr-poly-gcd expr expr 'x)])
      (assert-equal (expr-degree gcd-expr 'x) (expr-degree expr 'x)))))

;;; ============================================================
;;; Test: Polynomial Division
;;; ============================================================

(test-group "poly-division"
  (define-test "divide x^2 by x gives x with remainder 0"
    (let* ([x (var 'x)]
           [numer (power x (num 2))]
           [denom x]
           [result (poly-divide-expr numer denom 'x)]
           [quot-expr (car result)]
           [rem-expr (cdr result)])
      (assert-equal (expr-degree quot-expr 'x) 1)
      (assert-equal (expr-degree rem-expr 'x) 0)))

  (define-test "divide x^2+1 by x gives x with remainder 1"
    (let* ([x (var 'x)]
           [numer (sum (power x (num 2)) (num 1))]
           [denom x]
           [result (poly-divide-expr numer denom 'x)]
           [quot-expr (car result)]
           [rem-expr (cdr result)])
      (assert-equal (expr-degree quot-expr 'x) 1)
      ;; Remainder should be constant (1)
      (assert-true (num? rem-expr)))))

;;; ============================================================
;;; Test: To Polynomial Form
;;; ============================================================

(test-group "to-polynomial-form"
  (define-test "converts to canonical form"
    (let* ([x (var 'x)]
           ;; (x+1)^2 = x^2 + 2x + 1
           [expr (power (sum x (num 1)) (num 2))]
           [canon (to-polynomial-form expr 'x)])
      (assert-equal (expr-degree canon 'x) 2))))

;;; ============================================================
;;; Test: Square-Free Operations
;;; ============================================================

(test-group "square-free"
  (define-test "square-free of x^2 is x"
    (let* ([x (var 'x)]
           [expr (power x (num 2))]
           [sqf (expr-square-free expr 'x)])
      ;; Square-free part should have degree 1
      (assert-equal (expr-degree sqf 'x) 1)))

  (define-test "factored returns multiplicity"
    (let* ([x (var 'x)]
           [expr (power x (num 2))]  ; x^2 = x * x
           [factors (expr-factored expr 'x)])
      ;; Should have factor with multiplicity 2
      (assert-true (>= (length factors) 1)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
