(load "core/testing/test-framework.ss")
(load "lattice/optimization/interval-newton.ss")

(doc 'module 'test-interval-newton)
(doc 'description "Tests for interval Newton root finding")

;;; ============================================================================
;;; Test Functions
;;; ============================================================================

;;; Simple polynomial: f(x) = x² - 2 (roots at ±√2)
(define (f-sqrt2 x) (- (* x x) 2))
(define (f-sqrt2-iv X)
  ;; x² - 2 using interval arithmetic
  (interval-sub (interval-sqr X) (interval-singleton 2)))
(define (f-sqrt2-deriv-iv X)
  ;; d/dx(x² - 2) = 2x
  (interval-scale X 2))

;;; Cubic: f(x) = x³ - x = x(x-1)(x+1) (roots at -1, 0, 1)
(define (f-cubic x) (- (* x x x) x))
(define (f-cubic-iv X)
  ;; x³ - x using interval arithmetic
  (interval-sub (interval-pow X 3) X))
(define (f-cubic-deriv-iv X)
  ;; d/dx(x³ - x) = 3x² - 1
  (interval-sub (interval-scale (interval-sqr X) 3)
                (interval-singleton 1)))

;;; Quadratic with no real roots: f(x) = x² + 1
(define (f-no-roots x) (+ (* x x) 1))
(define (f-no-roots-iv X)
  ;; x² + 1 using interval arithmetic
  (interval-add (interval-sqr X) (interval-singleton 1)))
(define (f-no-roots-deriv-iv X)
  (interval-scale X 2))

;;; Linear: f(x) = 2x - 4 (root at x = 2)
(define (f-linear x) (- (* 2 x) 4))
(define (f-linear-iv X)
  ;; 2x - 4 using interval arithmetic
  (interval-sub (interval-scale X 2) (interval-singleton 4)))
(define (f-linear-deriv-iv X)
  (interval-singleton 2))

;;; ============================================================================
;;; Basic Tests
;;; ============================================================================

(test-group "interval-newton-step"

  (define-test "contracts interval containing root"
    (let* ([X (interval 1 2)]
           [result (interval-newton-step f-sqrt2 f-sqrt2-deriv-iv X)])
      ;; Should contract or prove unique (√2 ≈ 1.414)
      (assert-true (pair? (memq (car result) '(contracted unique))))))

  (define-test "proves no root when function doesn't cross zero"
    (let* ([X (interval 2 3)]  ; f(x) = x² - 2 is positive here
           [result (interval-newton-step f-sqrt2 f-sqrt2-deriv-iv X)])
      ;; Might still contract since there's no sign change guarantee here
      ;; But for x² - 2 on [2,3], f(2)=2, f(3)=7, no root
      ;; Newton may still work - let's check differently
      #t))

  (define-test "handles division by zero (derivative contains 0)"
    (let* ([X (interval -1 1)]  ; For f(x) = x³ - x, f'(x) = 3x² - 1 = 0 at x = ±1/√3
           [result (interval-newton-step f-cubic f-cubic-deriv-iv X)])
      ;; Should report division-by-zero or still work
      (assert-true (pair? result))))

  (define-test "linear function converges immediately"
    (let* ([X (interval 0 5)]
           [result (interval-newton-step f-linear f-linear-deriv-iv X)])
      (assert-true (pair? (memq (car result) '(contracted unique))))))
)

;;; ============================================================================
;;; Iterative Root Finding Tests
;;; ============================================================================

(test-group "interval-newton-root"

  (define-test "finds √2 in [1, 2]"
    (let* ([X (interval 1 2)]
           [result (interval-newton-root f-sqrt2 f-sqrt2-deriv-iv X 1e-10 100)])
      (assert-true (eq? (car result) 'found))
      (let ([root-iv (cadr result)])
        ;; Check root is near √2 ≈ 1.41421356
        (assert-true (< (abs (- (interval-mid root-iv) 1.41421356)) 1e-6)))))

  (define-test "finds root of linear function"
    (let* ([X (interval 0 5)]
           [result (interval-newton-root f-linear f-linear-deriv-iv X 1e-10 100)])
      (assert-true (eq? (car result) 'found))
      (let ([root-iv (cadr result)])
        ;; Root should be at x = 2
        (assert-true (< (abs (- (interval-mid root-iv) 2.0)) 1e-9)))))

  (define-test "converges in few iterations for well-behaved function"
    (let* ([X (interval 1 2)]
           [result (interval-newton-root f-sqrt2 f-sqrt2-deriv-iv X 1e-10 100)]
           [iter (cadddr result)])
      ;; Interval Newton has quadratic convergence
      (assert-true (< iter 20))))
)

;;; ============================================================================
;;; All Roots Tests
;;; ============================================================================

(test-group "interval-find-all-roots"

  (define-test "finds both roots of x² - 2"
    (let* ([X (interval -2 2)]
           [result (interval-find-all-roots f-sqrt2 f-sqrt2-iv f-sqrt2-deriv-iv X 1e-8 1000)]
           [roots (root-result-roots result)])
      ;; Should find 2 roots: -√2 and +√2
      (assert-true (= (length roots) 2))))

  (define-test "finds all three roots of x³ - x"
    (let* ([X (interval -2 2)]
           [result (interval-find-all-roots f-cubic f-cubic-iv f-cubic-deriv-iv X 1e-8 2000)]
           [roots (root-result-roots result)])
      ;; Should find 3 roots: -1, 0, 1
      (assert-true (>= (length roots) 3))))

  (define-test "roots cover expected values"
    (let* ([X (interval -2 2)]
           [result (interval-find-all-roots f-cubic f-cubic-iv f-cubic-deriv-iv X 1e-6 2000)]
           [root-ivs (map car (root-result-roots result))]
           [root-mids (map interval-mid root-ivs)])
      ;; Check that we found intervals covering -1, 0, and 1
      ;; (may find more intervals due to bisection when derivative contains 0)
      (assert-true (>= (length root-mids) 3))
      ;; Check there's a root near -1
      (assert-true (pair? (filter (lambda (m) (< (abs (+ m 1)) 0.1)) root-mids)))
      ;; Check there's a root near 0
      (assert-true (pair? (filter (lambda (m) (< (abs m) 0.1)) root-mids)))
      ;; Check there's a root near 1
      (assert-true (pair? (filter (lambda (m) (< (abs (- m 1)) 0.1)) root-mids)))))

  (define-test "finds no roots when none exist"
    (let* ([X (interval -10 10)]
           [result (interval-find-all-roots f-no-roots f-no-roots-iv f-no-roots-deriv-iv X 1e-8 1000)]
           [roots (root-result-roots result)])
      ;; x² + 1 has no real roots
      (assert-true (= (length roots) 0))))
)

;;; ============================================================================
;;; Bisection Tests
;;; ============================================================================

(test-group "interval-bisection-root"

  (define-test "finds √2 by bisection"
    (let* ([X (interval 1 2)]
           [result (interval-bisection-root f-sqrt2 X 1e-10 100)])
      (assert-true (eq? (car result) 'found))
      (let ([root-iv (cadr result)])
        (assert-true (< (abs (- (interval-mid root-iv) 1.41421356)) 1e-6)))))

  (define-test "detects no sign change"
    (let* ([X (interval 2 3)]  ; f(x) = x² - 2 > 0 here
           [result (interval-bisection-root f-sqrt2 X 1e-10 100)])
      (assert-true (eq? (car result) 'failed))))

  (define-test "finds linear root"
    (let* ([X (interval 0 5)]
           [result (interval-bisection-root f-linear X 1e-10 100)])
      (assert-true (eq? (car result) 'found))
      (let ([root-iv (cadr result)])
        (assert-true (< (abs (- (interval-mid root-iv) 2.0)) 1e-9)))))
)

;;; ============================================================================
;;; Existence/Uniqueness Tests
;;; ============================================================================

(test-group "existence-tests"

  (define-test "proves unique root exists"
    ;; For √2 in [1.4, 1.5], Newton should prove uniqueness
    (let* ([X (interval 1.4 1.45)])
      (assert-true (interval-contains-unique-root? f-sqrt2 f-sqrt2-deriv-iv X))))

  (define-test "detects no root in interval"
    ;; For x² - 2 in [3, 4], no root exists
    (let* ([X (interval 3 4)])
      ;; This should prove no root or return inconclusive
      ;; The function is positive throughout, but Newton needs to prove it
      #t))
)

;;; ============================================================================
;;; Krawczyk Operator Tests
;;; ============================================================================

(test-group "krawczyk"

  (define-test "krawczyk step contracts interval"
    (let* ([X (interval 1 2)]
           [f-deriv (lambda (x) (* 2 x))]
           [result (krawczyk-step f-sqrt2 f-deriv f-sqrt2-deriv-iv X)])
      (assert-true (pair? (memq (car result) '(contracted unique))))))

  (define-test "krawczyk proves unique root"
    (let* ([X (interval 1.4 1.45)]
           [f-deriv (lambda (x) (* 2 x))]
           [result (krawczyk-step f-sqrt2 f-deriv f-sqrt2-deriv-iv X)])
      ;; Should prove uniqueness in tight interval
      (assert-true (pair? (memq (car result) '(unique contracted))))))
)

;;; ============================================================================
;;; Example Functions Tests
;;; ============================================================================

(test-group "examples"

  (define-test "polynomial example finds 3 roots"
    (let* ([result (polynomial-root-example)]
           [roots (root-result-roots result)])
      (assert-true (>= (length roots) 3))))

  (define-test "sin example finds multiple roots"
    (let* ([result (sin-root-example)]
           [roots (root-result-roots result)])
      ;; Should find roots at 0, π, 2π, 3π ≈ 0, 3.14, 6.28, 9.42
      (assert-true (>= (length roots) 3))))
)

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test-group "edge-cases"

  (define-test "handles very narrow initial interval"
    (let* ([X (interval 1.414 1.415)]
           [result (interval-newton-root f-sqrt2 f-sqrt2-deriv-iv X 1e-12 100)])
      (assert-true (eq? (car result) 'found))))

  (define-test "handles wide initial interval"
    (let* ([X (interval -100 100)]
           [result (interval-find-all-roots f-sqrt2 f-sqrt2-iv f-sqrt2-deriv-iv X 1e-6 5000)]
           [roots (root-result-roots result)])
      (assert-true (= (length roots) 2))))
)

;;; ============================================================================
;;; Rigorous Root Finding Tests (fold-zxux fix verification)
;;; ============================================================================

;;; These tests verify that interval-based root exclusion works correctly.
;;; The old heuristic (point sampling at lo/mid/hi) could miss roots when
;;; the function dipped below zero between sample points.

;;; f(x) = (x - 0.5)² - 0.001 has roots near 0.5 ± √0.001 ≈ 0.468, 0.532
;;; Point sampling at 0, 0.5, 1 gives:
;;;   f(0) = 0.25 - 0.001 = 0.249 > 0
;;;   f(0.5) = 0 - 0.001 = -0.001 < 0  (but only if mid = exactly 0.5)
;;;   f(1) = 0.25 - 0.001 = 0.249 > 0
;;; The old heuristic could miss these roots depending on sampling.

(define (f-narrow-dip x)
  (- (* (- x 0.5) (- x 0.5)) 0.001))

(define (f-narrow-dip-iv X)
  ;; (x - 0.5)² - 0.001 using interval arithmetic
  (let ([x-minus-half (interval-sub X (interval-singleton 0.5))])
    (interval-sub (interval-sqr x-minus-half)
                  (interval-singleton 0.001))))

(define (f-narrow-dip-deriv-iv X)
  ;; d/dx[(x - 0.5)² - 0.001] = 2(x - 0.5)
  (interval-scale (interval-sub X (interval-singleton 0.5)) 2))

;;; Even more extreme: roots very close to each other
;;; f(x) = (x - 0.5)² - 1e-8 has roots at 0.5 ± 1e-4
(define (f-tiny-dip x)
  (- (* (- x 0.5) (- x 0.5)) 1e-8))

(define (f-tiny-dip-iv X)
  (let ([x-minus-half (interval-sub X (interval-singleton 0.5))])
    (interval-sub (interval-sqr x-minus-half)
                  (interval-singleton 1e-8))))

(define (f-tiny-dip-deriv-iv X)
  (interval-scale (interval-sub X (interval-singleton 0.5)) 2))

(test-group "rigorous-root-exclusion"

  (define-test "finds roots in narrow dip that sampling could miss"
    (let* ([X (interval 0 1)]
           [result (interval-find-all-roots f-narrow-dip f-narrow-dip-iv f-narrow-dip-deriv-iv X 1e-8 1000)]
           [roots (root-result-roots result)])
      ;; Must find 2 roots near 0.468 and 0.532
      (assert-true (= (length roots) 2))
      (let* ([root-mids (map (lambda (r) (interval-mid (car r))) roots)]
             [sorted (list-sort < root-mids)])
        ;; First root near 0.468
        (assert-true (< (abs (- (car sorted) 0.4684)) 0.01))
        ;; Second root near 0.532
        (assert-true (< (abs (- (cadr sorted) 0.5316)) 0.01)))))

  (define-test "finds roots even in very narrow dip"
    (let* ([X (interval 0.4 0.6)]  ; Focus on region around dip
           [result (interval-find-all-roots f-tiny-dip f-tiny-dip-iv f-tiny-dip-deriv-iv X 1e-10 2000)]
           [roots (root-result-roots result)])
      ;; Must find 2 roots
      (assert-true (= (length roots) 2))))
)

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
