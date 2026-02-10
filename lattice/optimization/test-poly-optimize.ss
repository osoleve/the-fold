;;; lattice/optimization/test-poly-optimize.ss --- Tests for Polynomial Optimization
;;;
;;; Tests covering:
;;;   - Exact polynomial gradients
;;;   - Polynomial root finding
;;;   - Constrained polynomial optimization
;;;   - SOS decomposition utilities
;;;
;;; Run with: scheme --script lattice/optimization/test-poly-optimize.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/algebra/field.ss")
(load "lattice/algebra/polynomial.ss")
(load "lattice/algebra/multivariate.ss")
(load "lattice/optimization/poly-optimize.ss")

;;; ====
;;; Test Helper Functions
;;; ====

(define (approximately-equal? a b tol)
  (< (abs (- a b)) tol))

(define *test-tol* 1e-6)
(define *root-tol* 1e-8)

;;; Helper to create convergence criteria for tests
(define (test-criteria)
  (make-convergence-criteria 1e-8 1e-10 1e-10 100))

;;; ====
;;; Univariate Polynomial Gradient Tests
;;; ====

(test-group "Univariate Polynomial Gradients"

  (define-test "poly-gradient of constant is zero"
    (let* ([p (make-polynomial R-field '(5.0))]  ; p(x) = 5
           [grad-fn (poly-gradient p)])
      (approximately-equal? (grad-fn 0.0) 0.0 *test-tol*)))

  (define-test "poly-gradient of linear is constant"
    (let* ([p (make-polynomial R-field '(2.0 3.0))]  ; p(x) = 2 + 3x
           [grad-fn (poly-gradient p)])
      ;; p'(x) = 3
      (and (approximately-equal? (grad-fn 0.0) 3.0 *test-tol*)
           (approximately-equal? (grad-fn 5.0) 3.0 *test-tol*))))

  (define-test "poly-gradient of quadratic"
    (let* ([p (make-polynomial R-field '(1.0 0.0 1.0))]  ; p(x) = 1 + x²
           [grad-fn (poly-gradient p)])
      ;; p'(x) = 2x
      (and (approximately-equal? (grad-fn 0.0) 0.0 *test-tol*)
           (approximately-equal? (grad-fn 3.0) 6.0 *test-tol*)
           (approximately-equal? (grad-fn -2.0) -4.0 *test-tol*))))

  (define-test "poly-hessian-fn of quadratic is constant"
    (let* ([p (make-polynomial R-field '(1.0 0.0 2.0))]  ; p(x) = 1 + 2x²
           [hess-fn (poly-hessian-fn p)])
      ;; p''(x) = 4
      (and (approximately-equal? (hess-fn 0.0) 4.0 *test-tol*)
           (approximately-equal? (hess-fn 10.0) 4.0 *test-tol*))))

  (define-test "poly-gradient of cubic"
    (let* ([p (make-polynomial R-field '(0.0 0.0 0.0 1.0))]  ; p(x) = x³
           [grad-fn (poly-gradient p)])
      ;; p'(x) = 3x²
      (and (approximately-equal? (grad-fn 2.0) 12.0 *test-tol*)
           (approximately-equal? (grad-fn -1.0) 3.0 *test-tol*)))))

;;; ====
;;; Univariate Polynomial Minimization Tests
;;; ====

(test-group "Univariate Polynomial Minimization"

  (define-test "minimize simple quadratic x²"
    (let* ([p (make-polynomial R-field '(0.0 0.0 1.0))]  ; x²
           [criteria (test-criteria)]
           [result (poly-minimize-univariate p 5.0 criteria)]
           [x-opt (car (opt-result-x result))])
      ;; Minimum at x = 0
      (approximately-equal? x-opt 0.0 1e-4)))

  (define-test "minimize shifted quadratic (x-3)²"
    ;; (x-3)² = x² - 6x + 9
    (let* ([p (make-polynomial R-field '(9.0 -6.0 1.0))]
           [criteria (test-criteria)]
           [result (poly-minimize-univariate p 0.0 criteria)]
           [x-opt (car (opt-result-x result))])
      ;; Minimum at x = 3
      (approximately-equal? x-opt 3.0 1e-4)))

  (define-test "minimize quartic with unique minimum"
    ;; p(x) = x⁴ - 2x² + 1 = (x² - 1)²
    ;; Has minima at x = ±1 with value 0
    (let* ([p (make-polynomial R-field '(1.0 0.0 -2.0 0.0 1.0))]
           [criteria (test-criteria)]
           [result (poly-minimize-univariate p 0.5 criteria)]
           [x-opt (car (opt-result-x result))]
           [f-opt (opt-result-f result)])
      ;; Should find one of the minima
      (and (< f-opt 0.01)
           (or (approximately-equal? (abs x-opt) 1.0 0.1)
               (approximately-equal? x-opt 0.0 0.1))))))

;;; ====
;;; Root Finding Tests
;;; ====

(test-group "Polynomial Root Finding"

  (define-test "find root of linear"
    (let* ([p (make-polynomial R-field '(3.0 -1.0))]  ; -x + 3 = 0 → x = 3
           [criteria (test-criteria)]
           [root (poly-find-root p 0.0 criteria)])
      (and root (approximately-equal? root 3.0 *root-tol*))))

  (define-test "find root of quadratic (x-2)(x-3)"
    ;; (x-2)(x-3) = x² - 5x + 6
    (let* ([p (make-polynomial R-field '(6.0 -5.0 1.0))]
           [criteria (test-criteria)]
           [root (poly-find-root p 1.5 criteria)])
      ;; Should find root at 2 (closer to starting point)
      (and root (approximately-equal? root 2.0 *root-tol*))))

  (define-test "find root of x³ - 1"
    ;; x³ - 1 = 0 → x = 1
    (let* ([p (make-polynomial R-field '(-1.0 0.0 0.0 1.0))]
           [criteria (test-criteria)]
           [root (poly-find-root p 0.5 criteria)])
      (and root (approximately-equal? root 1.0 *root-tol*))))

  (define-test "find-all-roots of quadratic"
    (let* ([p (make-polynomial R-field '(6.0 -5.0 1.0))]  ; (x-2)(x-3)
           [criteria (test-criteria)]
           [roots (poly-find-all-roots p criteria)]
           [sorted (sort-by (lambda (a b) (< a b)) roots)])
      ;; Should find roots at 2 and 3
      (and (= (length roots) 2)
           (approximately-equal? (car sorted) 2.0 0.01)
           (approximately-equal? (cadr sorted) 3.0 0.01)))))

;;; ====
;;; Multivariate Gradient Tests
;;; ====

(test-group "Multivariate Polynomial Gradients"

  (define-test "mpoly-partial of x + y wrt x"
    (let* ([F R-field]
           [vars '(x y)]
           [ordering (make-ordering 'grevlex vars)]
           ;; p = x + y (terms: 1*x + 1*y)
           [p (make-mpoly F vars ordering
                          (list (cons 1.0 (mono-var 'x))
                                (cons 1.0 (mono-var 'y))))]
           [dp-dx (mpoly-partial p 'x)]
           [val (mpoly-eval dp-dx '((x . 5.0) (y . 3.0)))])
      ;; ∂(x+y)/∂x = 1
      (approximately-equal? val 1.0 *test-tol*)))

  (define-test "mpoly-partial of x*y wrt x"
    (let* ([F R-field]
           [vars '(x y)]
           [ordering (make-ordering 'grevlex vars)]
           ;; p = x*y (term: 1*xy)
           [mono-xy (make-monomial '((x . 1) (y . 1)))]
           [p (make-mpoly F vars ordering (list (cons 1.0 mono-xy)))]
           [dp-dx (mpoly-partial p 'x)]
           [val (mpoly-eval dp-dx '((x . 2.0) (y . 3.0)))])
      ;; ∂(xy)/∂x = y → 3.0
      (approximately-equal? val 3.0 *test-tol*)))

  (define-test "mpoly-gradient of x² + y²"
    (let* ([F R-field]
           [vars '(x y)]
           [ordering (make-ordering 'grevlex vars)]
           ;; p = x² + y²
           [p (make-mpoly F vars ordering
                          (list (cons 1.0 (make-monomial '((x . 2))))
                                (cons 1.0 (make-monomial '((y . 2))))))]
           [grad (mpoly-gradient-at p '((x . 3.0) (y . 4.0)))])
      ;; ∇p = (2x, 2y) → (6, 8) at (3, 4)
      (and (approximately-equal? (car grad) 6.0 *test-tol*)
           (approximately-equal? (cadr grad) 8.0 *test-tol*)))))

;;; ====
;;; Constraint Tests
;;; ====

(test-group "Polynomial Constraints"

  (define-test "make-poly-constraint creates valid structure"
    (let* ([p (make-polynomial R-field '(1.0 -1.0))]  ; x - 1
           [c (make-poly-constraint p '<=)])
      (and (poly-constraint? c)
           (eq? (pc-op c) '<=))))

  (define-test "constraint-satisfied? for equality"
    (let* ([p (make-polynomial R-field '(0.0))]
           [c (make-poly-constraint p '=)])
      (constraint-satisfied? c 0.0 1e-6)))

  (define-test "constraint-satisfied? for inequality <="
    (let* ([p (make-polynomial R-field '(0.0))]
           [c (make-poly-constraint p '<=)])
      (and (constraint-satisfied? c -1.0 1e-6)
           (constraint-satisfied? c 0.0 1e-6)
           (not (constraint-satisfied? c 1.0 1e-6)))))

  (define-test "constraint-violation computes correctly"
    (let* ([p (make-polynomial R-field '(0.0))]
           [c (make-poly-constraint p '<=)])
      (and (approximately-equal? (constraint-violation c -1.0) 0.0 *test-tol*)
           (approximately-equal? (constraint-violation c 2.0) 2.0 *test-tol*)))))

;;; ====
;;; Sum-of-Squares Tests
;;; ====

(test-group "Sum-of-Squares"

  (define-test "sos-gram-matrix-size for degree 2"
    (= (sos-gram-matrix-size 2) 2))  ; d=2 → size = 2

  (define-test "sos-gram-matrix-size for degree 4"
    (= (sos-gram-matrix-size 4) 3))  ; d=4 → size = 3

  (define-test "poly-is-sos-necessary rejects odd degree"
    (let ([p (make-polynomial R-field '(1.0 0.0 0.0 1.0))])  ; x³ + 1
      (not (poly-is-sos-necessary p))))

  (define-test "poly-is-sos-necessary accepts even degree non-negative leading"
    (let ([p (make-polynomial R-field '(1.0 0.0 1.0))])  ; x² + 1
      (poly-is-sos-necessary p)))

  (define-test "poly-is-sos-univariate-simple recognizes x²"
    (let ([p (make-polynomial R-field '(0.0 0.0 1.0))])  ; x²
      (poly-is-sos-univariate-simple p)))

  (define-test "poly-is-sos-univariate-simple recognizes x² + 1"
    (let ([p (make-polynomial R-field '(1.0 0.0 1.0))])  ; x² + 1
      (poly-is-sos-univariate-simple p)))

  (define-test "poly-is-sos-univariate-simple rejects x² - 1"
    ;; x² - 1 is NOT SOS (negative at x=0)
    ;; But our simple check only catches quadratics perfectly
    ;; x² - 1 has a=1, b=0, c=-1, so b²=0, 4ac=-4, 0 <= -4 is false
    (let ([p (make-polynomial R-field '(-1.0 0.0 1.0))])
      (not (poly-is-sos-univariate-simple p)))))

;;; ====
;;; Constrained Optimization Tests
;;; ====

(test-group "Constrained Polynomial Optimization"

  (define-test "minimize x² subject to x >= 1"
    (let* ([obj (make-polynomial R-field '(0.0 0.0 1.0))]  ; x²
           ;; Constraint: x - 1 >= 0 (i.e., x >= 1)
           [constraint-poly (make-polynomial R-field '(-1.0 1.0))]  ; x - 1
           [constraint (make-poly-constraint constraint-poly '>=)]
           [criteria (test-criteria)]
           [result (poly-minimize-constrained obj (list constraint) 5.0 criteria)]
           [x-opt (car (opt-result-x result))])
      ;; Minimum of x² subject to x >= 1 is at x = 1
      (approximately-equal? x-opt 1.0 0.1))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
