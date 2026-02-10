;;; lattice/numeric/test-interval-integrate.ss — Tests for Verified Interval Integration
;;;
;;; Tests verify:
;;;   - Enclosure correctness (true integral lies within computed bounds)
;;;   - Convergence (tighter bounds with more subdivisions)
;;;   - Method comparison (midpoint tighter than naive)
;;;   - Known integrals (exact results for polynomials, transcendentals)

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/numeric/interval-integrate.ss")

;;; Helper: check that a known value lies within an interval
(define (interval-encloses? iv x)
  (and (>= x (interval-lo iv))
       (<= x (interval-hi iv))))

;;; ============================================================================
;;; Basic Integration Tests — Known Integrals
;;; ============================================================================

(test-group "interval-integrate-known-integrals"

  ;; ∫₀¹ 1 dx = 1 (constant function)
  (define-test "constant function integral"
    (let ([result (interval-integrate-naive
                   (lambda (iv) (interval-singleton 1))
                   0 1 10)])
      (assert-true (interval-encloses? result 1.0))
      ;; Constant function should give exact result
      (assert-true (< (interval-width result) 1e-10))))

  ;; ∫₀¹ x dx = 0.5 (linear function)
  (define-test "linear function integral (naive)"
    (let ([result (interval-integrate-naive
                   (lambda (iv) iv)  ; f(x) = x
                   0 1 100)])
      (assert-true (interval-encloses? result 0.5))))

  ;; ∫₀¹ x dx = 0.5 (midpoint should be tighter)
  (define-test "linear function integral (midpoint)"
    (let ([result (interval-integrate-midpoint
                   (lambda (iv) iv)
                   0 1 100)])
      (assert-true (interval-encloses? result 0.5))))

  ;; ∫₀¹ x² dx = 1/3
  (define-test "quadratic function integral"
    (let ([result (interval-integrate-midpoint
                   (lambda (iv) (interval-mul iv iv))
                   0 1 100)])
      (assert-true (interval-encloses? result (/ 1.0 3)))))

  ;; ∫₀π sin(x) dx = 2
  (define-test "sin integral over [0, pi]"
    (let* ([pi 3.141592653589793]
           [result (interval-integrate-midpoint
                    (lambda (iv) (interval-sin iv))
                    0 pi 100)])
      (assert-true (interval-encloses? result 2.0))))

  ;; ∫₀¹ eˣ dx = e - 1 ≈ 1.71828...
  (define-test "exp integral"
    (let ([result (interval-integrate-midpoint
                   (lambda (iv) (interval-exp iv))
                   0 1 100)]
          [exact (- (exp 1) 1)])
      (assert-true (interval-encloses? result exact))))

  ;; ∫₁² 1/x dx = ln(2) ≈ 0.6931...
  (define-test "reciprocal integral gives ln(2)"
    (let ([result (interval-integrate-midpoint
                   (lambda (iv) (interval-recip iv))
                   1 2 100)]
          [exact (log 2)])
      (assert-true (interval-encloses? result exact)))))

;;; ============================================================================
;;; Convergence Tests
;;; ============================================================================

(test-group "interval-integrate-convergence"

  ;; Width should decrease with more subdivisions
  (define-test "naive convergence: width decreases"
    (let* ([f (lambda (iv) (interval-mul iv iv))]
           [w10 (interval-width (interval-integrate-naive f 0 1 10))]
           [w100 (interval-width (interval-integrate-naive f 0 1 100))])
      (assert-true (< w100 w10))))

  (define-test "midpoint convergence: width decreases"
    (let* ([f (lambda (iv) (interval-mul iv iv))]
           [w10 (interval-width (interval-integrate-midpoint f 0 1 10))]
           [w100 (interval-width (interval-integrate-midpoint f 0 1 100))])
      (assert-true (< w100 w10))))

  ;; More subdivisions should give tighter bounds but still enclose
  (define-test "convergence preserves enclosure"
    (let* ([f (lambda (iv) (interval-sin iv))]
           [pi 3.141592653589793]
           [r10 (interval-integrate-naive f 0 pi 10)]
           [r100 (interval-integrate-naive f 0 pi 100)])
      (assert-true (interval-encloses? r10 2.0))
      (assert-true (interval-encloses? r100 2.0))
      (assert-true (< (interval-width r100) (interval-width r10))))))

;;; ============================================================================
;;; Method Comparison Tests
;;; ============================================================================

(test-group "interval-integrate-method-comparison"

  ;; Bisected should give strictly tighter bounds than naive for smooth functions
  (define-test "midpoint strictly tighter than naive for x^2"
    (let* ([f (lambda (iv) (interval-mul iv iv))]
           [naive-w (interval-width (interval-integrate-naive f 0 1 32))]
           [mid-w (interval-width (interval-integrate-midpoint f 0 1 32))])
      (assert-true (< mid-w naive-w))))

  (define-test "midpoint strictly tighter than naive for sin"
    (let* ([pi 3.141592653589793]
           [f (lambda (iv) (interval-sin iv))]
           [naive-w (interval-width (interval-integrate-naive f 0 pi 32))]
           [mid-w (interval-width (interval-integrate-midpoint f 0 pi 32))])
      (assert-true (< mid-w naive-w))))

  ;; Both methods should still enclose the true value
  (define-test "both methods enclose x^2 integral"
    (let* ([f (lambda (iv) (interval-mul iv iv))]
           [naive (interval-integrate-naive f 0 1 32)]
           [mid (interval-integrate-midpoint f 0 1 32)])
      (assert-true (interval-encloses? naive (/ 1.0 3)))
      (assert-true (interval-encloses? mid (/ 1.0 3))))))

;;; ============================================================================
;;; Adaptive Integration Tests
;;; ============================================================================

(test-group "interval-integrate-adaptive"

  (define-test "adaptive encloses constant integral"
    (let ([result (interval-integrate-adaptive
                   (lambda (iv) (interval-singleton 1))
                   0 1 1e-6 100)])
      (assert-true (interval-encloses? result 1.0))))

  (define-test "adaptive encloses x^2 integral"
    (let ([result (interval-integrate-adaptive
                   (lambda (iv) (interval-mul iv iv))
                   0 1 1e-4 200)])
      (assert-true (interval-encloses? result (/ 1.0 3)))))

  (define-test "adaptive achieves requested tolerance"
    (let* ([tol 1e-3]
           [result (interval-integrate-adaptive
                    (lambda (iv) (interval-mul iv iv))
                    0 1 tol 2000)])
      (assert-true (interval-encloses? result (/ 1.0 3)))
      (assert-true (<= (interval-width result) tol))))

  ;; High subdivision count — exercises heap-based O(N log N) path
  (define-test "adaptive handles 5000 subdivisions"
    (let* ([tol 1e-10]
           [result (interval-integrate-adaptive
                    (lambda (iv) (interval-mul iv iv))
                    0 1 tol 5000)])
      (assert-true (interval-encloses? result (/ 1.0 3)))))

  ;; Transcendental: ∫₀^π sin(x) dx = 2
  ;; interval-sin has wide enclosures near extrema, so convergence is slow.
  ;; 500 subs gets ~0.01 width. Verify enclosure and tightening.
  (define-test "adaptive encloses sin integral"
    (let* ([pi 3.141592653589793]
           [result (interval-integrate-adaptive
                    (lambda (iv) (interval-sin iv))
                    0 pi 1e-2 500)])
      (assert-true (interval-encloses? result 2.0))
      (assert-true (<= (interval-width result) 1e-1))))

  ;; Peaked function: ∫₀¹ 1/(1+25x²) dx ≈ 0.5493603...
  ;; Adaptive should focus refinement near x=0 where the peak is
  (define-test "adaptive handles peaked function (Runge)"
    (let* ([f (lambda (iv)
                (interval-recip
                 (interval-add (interval-singleton 1)
                               (interval-scale (interval-mul iv iv) 25))))]
           [exact (/ (atan 5.0) 5.0)]
           [result (interval-integrate-adaptive f 0 1 1e-4 1000)])
      (assert-true (interval-encloses? result exact)))))

;;; ============================================================================
;;; Rigorous Integration Tests
;;; ============================================================================

(test-group "interval-integrate-rigorous"

  (define-test "rigorous encloses constant integral"
    (let ([result (interval-integrate-rigorous
                   (lambda (iv) (interval-singleton 1))
                   0 1 10)])
      (assert-true (interval-encloses? result 1.0))))

  (define-test "rigorous encloses x^2 integral"
    (let ([result (interval-integrate-rigorous
                   (lambda (iv) (interval-mul iv iv))
                   0 1 100)])
      (assert-true (interval-encloses? result (/ 1.0 3)))))

  (define-test "rigorous encloses sin integral"
    (let* ([pi 3.141592653589793]
           [result (interval-integrate-rigorous
                    (lambda (iv) (interval-sin iv))
                    0 pi 100)])
      (assert-true (interval-encloses? result 2.0)))))

;;; ============================================================================
;;; Convenience API Tests
;;; ============================================================================

(test-group "interval-integrate-convenience"

  (define-test "default method is midpoint"
    (let* ([f (lambda (iv) (interval-mul iv iv))]
           [default-result (interval-integrate f 0 1)]
           [midpoint-result (interval-integrate-midpoint f 0 1 64)])
      ;; Should get same result as midpoint with n=64
      (assert-equal (interval-lo default-result) (interval-lo midpoint-result))
      (assert-equal (interval-hi default-result) (interval-hi midpoint-result))))

  (define-test "naive method via convenience"
    (let ([result (interval-integrate
                   (lambda (iv) (interval-singleton 1))
                   0 1 'naive 10)])
      (assert-true (interval-encloses? result 1.0))))

  (define-test "midpoint method via convenience"
    (let ([result (interval-integrate
                   (lambda (iv) (interval-mul iv iv))
                   0 1 'midpoint 100)])
      (assert-true (interval-encloses? result (/ 1.0 3))))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test-group "interval-integrate-edge-cases"

  ;; Zero-width domain: ∫_a^a f(x) dx = 0
  (define-test "zero-width domain gives zero"
    (let ([result (interval-integrate-naive
                   (lambda (iv) (interval-singleton 42))
                   5 5 10)])
      (assert-true (interval-encloses? result 0.0))
      (assert-true (< (interval-width result) 1e-10))))

  ;; Negative function: ∫₀¹ -x dx = -0.5
  (define-test "negative function integral"
    (let ([result (interval-integrate-midpoint
                   (lambda (iv) (interval-neg iv))
                   0 1 100)])
      (assert-true (interval-encloses? result -0.5))))

  ;; n=0 should error
  (define-test "naive errors on n=0"
    (assert-error
     (lambda () (interval-integrate-naive
                 (lambda (iv) iv) 0 1 0))))

  (define-test "midpoint errors on n=0"
    (assert-error
     (lambda () (interval-integrate-midpoint
                 (lambda (iv) iv) 0 1 0))))

  (define-test "rigorous errors on n=0"
    (assert-error
     (lambda () (interval-integrate-rigorous
                 (lambda (iv) iv) 0 1 0))))

  ;; Wide interval but few subdivisions should still enclose
  (define-test "coarse subdivision still encloses"
    (let ([result (interval-integrate-naive
                   (lambda (iv) (interval-exp iv))
                   0 1 2)]
          [exact (- (exp 1) 1)])
      (assert-true (interval-encloses? result exact)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(display "\n=== Interval Integration Tests ===\n")
(run-all-tests)
