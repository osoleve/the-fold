;;; lattice/numeric/test-interpolate.ss — Tests for Interpolation Module
;;;
;;; Comprehensive tests for interpolation, splines, Bezier curves, and fitting.

(load "lattice/numeric/interpolate.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  [PASS] ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  [FAIL] ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-approx name expected actual tol)
  (set! *test-count* (+ *test-count* 1))
  (if (< (abs (- expected actual)) tol)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  [PASS] ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "  [FAIL] ")
       (display name)
       (newline)
       (display "    Expected: ")
       (write expected)
       (newline)
       (display "    Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

;;; ============================================================
;;; Test Data
;;; ============================================================

;; Piecewise linear points
(define points '((0 . 0) (1 . 1) (2 . 4) (3 . 9)))

;; Quadratic test data y = x²
(define xs-quad '(0 1 2 3))
(define ys-quad '(0 1 4 9))

;; Linear test data y = 2x + 1
(define xs-lin '(0 1 2))
(define ys-lin '(1 3 5))

;; Spline test data
(define spline-xs '(0.0 1.0 2.0 3.0))
(define spline-ys '(0.0 1.0 4.0 9.0))

;; Bezier control points
(define bp0 '(0 . 0))
(define bp1 '(1 . 1))
(define bp2 '(2 . 0))
(define bp3 '(3 . 0))

;; Regression data (perfect line)
(define reg-xs '(0 1 2 3 4))
(define reg-ys '(1 3 5 7 9))

;; Noisy regression data
(define noisy-xs '(1 2 3 4 5))
(define noisy-ys '(2.1 4.0 5.9 8.1 10.0))

;; Polynomial fit data
(define fit-xs '(0 1 2 3 4))
(define fit-ys '(0 1 4 9 16))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(define (run-tests)
  (display "================================================================\n")
  (display "           INTERPOLATION AND CURVE FITTING TEST SUITE\n")
  (display "================================================================\n\n")

  ;;; ============================================================
  ;;; Linear Interpolation
  ;;; ============================================================

  (display "--- Linear Interpolation ---\n")

  ;; lerp
  (test-approx "lerp 0 10 0" 0 (lerp 0 10 0) 1e-10)
  (test-approx "lerp 0 10 1" 10 (lerp 0 10 1) 1e-10)
  (test-approx "lerp 0 10 0.5" 5 (lerp 0 10 0.5) 1e-10)
  (test-approx "lerp 0 10 0.3" 3 (lerp 0 10 0.3) 1e-10)
  (test-approx "lerp -5 5 0.5" 0 (lerp -5 5 0.5) 1e-10)

  ;; lerp-inverse
  (test-approx "lerp-inverse 0 10 5" 0.5 (lerp-inverse 0 10 5) 1e-10)
  (test-approx "lerp-inverse 0 10 0" 0 (lerp-inverse 0 10 0) 1e-10)
  (test-approx "lerp-inverse 0 10 10" 1 (lerp-inverse 0 10 10) 1e-10)

  ;; Piecewise linear
  (test-approx "interp-linear at 0" 0 (interp-linear points 0) 1e-10)
  (test-approx "interp-linear at 1" 1 (interp-linear points 1) 1e-10)
  (test-approx "interp-linear at 2" 4 (interp-linear points 2) 1e-10)
  (test-approx "interp-linear at 1.5" 2.5 (interp-linear points 1.5) 1e-10)
  (test-approx "interp-linear at 0.5" 0.5 (interp-linear points 0.5) 1e-10)

  ;;; ============================================================
  ;;; Lagrange Interpolation
  ;;; ============================================================

  (display "\n--- Lagrange Interpolation ---\n")

  (test-approx "lagrange at 0" 0 (interp-lagrange xs-quad ys-quad 0) 1e-10)
  (test-approx "lagrange at 1" 1 (interp-lagrange xs-quad ys-quad 1) 1e-10)
  (test-approx "lagrange at 2" 4 (interp-lagrange xs-quad ys-quad 2) 1e-10)
  (test-approx "lagrange at 3" 9 (interp-lagrange xs-quad ys-quad 3) 1e-10)
  (test-approx "lagrange at 1.5" 2.25 (interp-lagrange xs-quad ys-quad 1.5) 1e-10)

  ;; Linear function y = 2x + 1
  (test-approx "lagrange linear at 0.5" 2 (interp-lagrange xs-lin ys-lin 0.5) 1e-10)
  (test-approx "lagrange linear at 1.5" 4 (interp-lagrange xs-lin ys-lin 1.5) 1e-10)

  ;;; ============================================================
  ;;; Newton Interpolation
  ;;; ============================================================

  (display "\n--- Newton Interpolation ---\n")

  ;; Divided differences
  (let ([dd (divided-differences '(0 1 2) '(0 1 4))])
    (test-approx "divided-diff [0]" 0 (car dd) 1e-10)
    (test-approx "divided-diff [1]" 1 (cadr dd) 1e-10)
    (test-approx "divided-diff [2]" 1 (caddr dd) 1e-10))

  ;; Newton matches Lagrange
  (test-approx "newton at 0" (interp-lagrange xs-quad ys-quad 0)
               (interp-newton xs-quad ys-quad 0) 1e-10)
  (test-approx "newton at 1.5" (interp-lagrange xs-quad ys-quad 1.5)
               (interp-newton xs-quad ys-quad 1.5) 1e-10)
  (test-approx "newton at 2.5" (interp-lagrange xs-quad ys-quad 2.5)
               (interp-newton xs-quad ys-quad 2.5) 1e-10)

  ;;; ============================================================
  ;;; Hermite Interpolation
  ;;; ============================================================

  (display "\n--- Hermite Interpolation ---\n")

  ;; Endpoints
  (test-approx "hermite at t=0" 0 (interp-hermite 0 1 0 0 0) 1e-10)
  (test-approx "hermite at t=1" 1 (interp-hermite 0 1 0 0 1) 1e-10)

  ;; With tangents
  (test-approx "hermite midpoint (zero tangent)" 0.5 (interp-hermite 0 1 0 0 0.5) 1e-10)

  ;; Symmetric tangents
  (test-approx "hermite symmetric" 0.5 (interp-hermite 0 1 1 1 0.5) 1e-10)

  ;;; ============================================================
  ;;; Cubic Spline
  ;;; ============================================================

  (display "\n--- Cubic Spline ---\n")

  (let ([spline (cubic-spline-natural spline-xs spline-ys)])
    ;; Interpolation at data points
    (test-approx "spline at x=0" 0 (spline-eval spline spline-xs 0.0) 1e-10)
    (test-approx "spline at x=1" 1 (spline-eval spline spline-xs 1.0) 1e-10)
    (test-approx "spline at x=2" 4 (spline-eval spline spline-xs 2.0) 1e-10)
    (test-approx "spline at x=3" 9 (spline-eval spline spline-xs 3.0) 1e-10)

    ;; Between points (should be smooth)
    (let ([v05 (spline-eval spline spline-xs 0.5)]
          [v15 (spline-eval spline spline-xs 1.5)]
          [v25 (spline-eval spline spline-xs 2.5)])
      (test-true "spline between 0 and 1" (and (> v05 0) (< v05 1)))
      (test-true "spline between 1 and 2" (and (> v15 1) (< v15 4)))
      (test-true "spline between 2 and 3" (and (> v25 4) (< v25 9)))))

  ;;; ============================================================
  ;;; Bezier Curves
  ;;; ============================================================

  (display "\n--- Bezier Curves ---\n")

  ;; Linear Bezier
  (let ([result (bezier-linear bp0 bp1 0.5)])
    (test-approx "bezier-linear x" 0.5 (car result) 1e-10)
    (test-approx "bezier-linear y" 0.5 (cdr result) 1e-10))

  ;; Quadratic Bezier
  (let ([result (bezier-quadratic bp0 bp1 bp2 0)])
    (test-approx "bezier-quad t=0 x" 0 (car result) 1e-10)
    (test-approx "bezier-quad t=0 y" 0 (cdr result) 1e-10))
  (let ([result (bezier-quadratic bp0 bp1 bp2 1)])
    (test-approx "bezier-quad t=1 x" 2 (car result) 1e-10)
    (test-approx "bezier-quad t=1 y" 0 (cdr result) 1e-10))
  (let ([result (bezier-quadratic bp0 bp1 bp2 0.5)])
    (test-approx "bezier-quad t=0.5 x" 1 (car result) 1e-10)
    (test-approx "bezier-quad t=0.5 y" 0.5 (cdr result) 1e-10))

  ;; Cubic Bezier
  (let ([result (bezier-cubic bp0 bp1 bp2 bp3 0)])
    (test-approx "bezier-cubic t=0 x" 0 (car result) 1e-10)
    (test-approx "bezier-cubic t=0 y" 0 (cdr result) 1e-10))
  (let ([result (bezier-cubic bp0 bp1 bp2 bp3 1)])
    (test-approx "bezier-cubic t=1 x" 3 (car result) 1e-10)
    (test-approx "bezier-cubic t=1 y" 0 (cdr result) 1e-10))

  ;; General Bezier (should match cubic)
  (let* ([ctrl (list bp0 bp1 bp2 bp3)]
         [result-cubic (bezier-cubic bp0 bp1 bp2 bp3 0.5)]
         [result-general (bezier-general ctrl 0.5)])
    (test-approx "bezier-general matches cubic x" (car result-cubic) (car result-general) 1e-10)
    (test-approx "bezier-general matches cubic y" (cdr result-cubic) (cdr result-general) 1e-10))

  ;;; ============================================================
  ;;; Linear Regression
  ;;; ============================================================

  (display "\n--- Linear Regression ---\n")

  ;; Perfect line y = 2x + 1
  (let ([reg (linreg reg-xs reg-ys)])
    (test-approx "linreg slope" 2 (car reg) 1e-10)
    (test-approx "linreg intercept" 1 (cdr reg) 1e-10))
  (test-approx "linreg R² (perfect)" 1 (linreg-r2 reg-xs reg-ys) 1e-10)

  ;; Noisy data
  (let ([reg (linreg noisy-xs noisy-ys)])
    (test-approx "noisy linreg slope ~2" 2 (car reg) 0.1)
    (test-approx "noisy linreg intercept ~0" 0 (cdr reg) 0.2))
  (test-true "noisy R² > 0.99" (> (linreg-r2 noisy-xs noisy-ys) 0.99))

  ;;; ============================================================
  ;;; Polynomial Fitting
  ;;; ============================================================

  (display "\n--- Polynomial Fitting ---\n")

  ;; Fit y = x² with degree 2
  (let ([fitted (polyfit fit-xs fit-ys 2)])
    ;; Evaluate at test points
    (test-approx "polyfit x²: f(0)" 0 (poly-eval fitted 0) 0.1)
    (test-approx "polyfit x²: f(1)" 1 (poly-eval fitted 1) 0.1)
    (test-approx "polyfit x²: f(2)" 4 (poly-eval fitted 2) 0.1)
    (test-approx "polyfit x²: f(2.5)" 6.25 (poly-eval fitted 2.5) 0.1))

  ;;; ============================================================
  ;;; Chebyshev
  ;;; ============================================================

  (display "\n--- Chebyshev ---\n")

  ;; Chebyshev nodes
  (let ([nodes (chebyshev-nodes 4)])
    (test "chebyshev-nodes count" 4 (length nodes))
    (test-true "chebyshev-nodes in [-1,1]"
               (and (>= (apply min nodes) -1)
                    (<= (apply max nodes) 1))))

  ;; Chebyshev polynomials
  (test-approx "T_0(x) = 1" 1 (chebyshev-t 0 0.5) 1e-10)
  (test-approx "T_1(x) = x" 0.5 (chebyshev-t 1 0.5) 1e-10)
  (test-approx "T_2(x) = 2x²-1" (- (* 2 0.5 0.5) 1) (chebyshev-t 2 0.5) 1e-10)
  (test-approx "T_3(x) = 4x³-3x" (- (* 4 0.5 0.5 0.5) (* 3 0.5)) (chebyshev-t 3 0.5) 1e-10)

  ;; Chebyshev nodes in interval
  (let ([nodes (chebyshev-nodes-interval 3 0 1)])
    (test "chebyshev-nodes-interval count" 3 (length nodes))
    (test-true "nodes in [0,1]"
               (and (>= (apply min nodes) 0)
                    (<= (apply max nodes) 1))))

  ;;; ============================================================
  ;;; B-Splines
  ;;; ============================================================

  (display "\n--- B-Splines ---\n")

  ;; Uniform knot vector for degree 2 B-spline
  (let ([knots (vector 0 1 2 3 4 5)])
    ;; B-spline basis functions (degree 0 are step functions)
    (test-approx "bspline-basis p=0 in interval" 1.0 (bspline-basis knots 2 0 2.5) 1e-10)
    (test-approx "bspline-basis p=0 outside" 0.0 (bspline-basis knots 2 0 1.5) 1e-10)

    ;; Degree 1 B-spline basis N_{2,1}(x) on [2,4), peaks at x=3
    (test-approx "bspline-basis p=1 at peak" 1.0 (bspline-basis knots 2 1 3.0) 1e-10)
    (test-approx "bspline-basis p=1 rising" 0.5 (bspline-basis knots 2 1 2.5) 1e-10))

  ;; B-spline curve
  (let ([knots (vector 0 0 0 1 1 1)]  ; Clamped cubic-like
        [ctrl '((0 . 0) (1 . 2) (2 . 0))])
    ;; Endpoints should be at control points for clamped splines
    (let ([start (bspline-curve knots ctrl 2 0)]
          [end (bspline-curve knots ctrl 2 0.999)])
      (test-approx "bspline-curve start x" 0 (car start) 0.1)
      (test-approx "bspline-curve end x" 2 (car end) 0.1)))

  ;;; ============================================================
  ;;; Summary
  ;;; ============================================================

  (display "\n================================================================\n")
  (display "                    TEST RESULTS\n")
  (display "================================================================\n\n")
  (display "Tests passed: ") (display *pass-count*) (newline)
  (display "Tests failed: ") (display *fail-count*) (newline)
  (display "Total tests:  ") (display *test-count*) (newline)
  (newline)

  (if (= *fail-count* 0)
      (display "[SUCCESS] All interpolation tests passed.\n")
      (display "[FAILURE] Some tests failed!\n")))

;; Run the tests
(run-tests)
