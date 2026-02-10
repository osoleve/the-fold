;;; lattice/numeric/test-interpolate.ss — Tests for Interpolation Module
;;;
;;; Comprehensive tests for interpolation, splines, Bezier curves, and fitting.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/numeric/interpolate.ss")

;;; ====
;;; Test Helpers
;;; ====

(define (approx-equal? expected actual tol)
  (< (abs (- expected actual)) tol))

;;; ====
;;; Test Data
;;; ====

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

;;; ====
;;; Tests
;;; ====

(test-group "Linear Interpolation"
  ;; lerp
  (define-test "lerp 0 10 0"
    (assert-true (approx-equal? 0 (lerp 0 10 0) 1e-10)))
  (define-test "lerp 0 10 1"
    (assert-true (approx-equal? 10 (lerp 0 10 1) 1e-10)))
  (define-test "lerp 0 10 0.5"
    (assert-true (approx-equal? 5 (lerp 0 10 0.5) 1e-10)))
  (define-test "lerp 0 10 0.3"
    (assert-true (approx-equal? 3 (lerp 0 10 0.3) 1e-10)))
  (define-test "lerp -5 5 0.5"
    (assert-true (approx-equal? 0 (lerp -5 5 0.5) 1e-10)))

  ;; lerp-inverse
  (define-test "lerp-inverse 0 10 5"
    (assert-true (approx-equal? 0.5 (lerp-inverse 0 10 5) 1e-10)))
  (define-test "lerp-inverse 0 10 0"
    (assert-true (approx-equal? 0 (lerp-inverse 0 10 0) 1e-10)))
  (define-test "lerp-inverse 0 10 10"
    (assert-true (approx-equal? 1 (lerp-inverse 0 10 10) 1e-10)))

  ;; Piecewise linear
  (define-test "interp-linear at 0"
    (assert-true (approx-equal? 0 (interp-linear points 0) 1e-10)))
  (define-test "interp-linear at 1"
    (assert-true (approx-equal? 1 (interp-linear points 1) 1e-10)))
  (define-test "interp-linear at 2"
    (assert-true (approx-equal? 4 (interp-linear points 2) 1e-10)))
  (define-test "interp-linear at 1.5"
    (assert-true (approx-equal? 2.5 (interp-linear points 1.5) 1e-10)))
  (define-test "interp-linear at 0.5"
    (assert-true (approx-equal? 0.5 (interp-linear points 0.5) 1e-10))))


(test-group "Lagrange Interpolation"
  (define-test "lagrange at 0"
    (assert-true (approx-equal? 0 (interp-lagrange xs-quad ys-quad 0) 1e-10)))
  (define-test "lagrange at 1"
    (assert-true (approx-equal? 1 (interp-lagrange xs-quad ys-quad 1) 1e-10)))
  (define-test "lagrange at 2"
    (assert-true (approx-equal? 4 (interp-lagrange xs-quad ys-quad 2) 1e-10)))
  (define-test "lagrange at 3"
    (assert-true (approx-equal? 9 (interp-lagrange xs-quad ys-quad 3) 1e-10)))
  (define-test "lagrange at 1.5"
    (assert-true (approx-equal? 2.25 (interp-lagrange xs-quad ys-quad 1.5) 1e-10)))

  ;; Linear function y = 2x + 1
  (define-test "lagrange linear at 0.5"
    (assert-true (approx-equal? 2 (interp-lagrange xs-lin ys-lin 0.5) 1e-10)))
  (define-test "lagrange linear at 1.5"
    (assert-true (approx-equal? 4 (interp-lagrange xs-lin ys-lin 1.5) 1e-10))))


(test-group "Newton Interpolation"
  ;; Divided differences
  (define-test "divided-diff [0]"
    (let ([dd (divided-differences '(0 1 2) '(0 1 4))])
      (assert-true (approx-equal? 0 (car dd) 1e-10))))
  (define-test "divided-diff [1]"
    (let ([dd (divided-differences '(0 1 2) '(0 1 4))])
      (assert-true (approx-equal? 1 (cadr dd) 1e-10))))
  (define-test "divided-diff [2]"
    (let ([dd (divided-differences '(0 1 2) '(0 1 4))])
      (assert-true (approx-equal? 1 (caddr dd) 1e-10))))

  ;; Newton matches Lagrange
  (define-test "newton at 0"
    (assert-true (approx-equal? (interp-lagrange xs-quad ys-quad 0)
                                (interp-newton xs-quad ys-quad 0) 1e-10)))
  (define-test "newton at 1.5"
    (assert-true (approx-equal? (interp-lagrange xs-quad ys-quad 1.5)
                                (interp-newton xs-quad ys-quad 1.5) 1e-10)))
  (define-test "newton at 2.5"
    (assert-true (approx-equal? (interp-lagrange xs-quad ys-quad 2.5)
                                (interp-newton xs-quad ys-quad 2.5) 1e-10))))


(test-group "Hermite Interpolation"
  ;; Endpoints
  (define-test "hermite at t=0"
    (assert-true (approx-equal? 0 (interp-hermite 0 1 0 0 0) 1e-10)))
  (define-test "hermite at t=1"
    (assert-true (approx-equal? 1 (interp-hermite 0 1 0 0 1) 1e-10)))

  ;; With tangents
  (define-test "hermite midpoint (zero tangent)"
    (assert-true (approx-equal? 0.5 (interp-hermite 0 1 0 0 0.5) 1e-10)))

  ;; Symmetric tangents
  (define-test "hermite symmetric"
    (assert-true (approx-equal? 0.5 (interp-hermite 0 1 1 1 0.5) 1e-10))))


(test-group "Cubic Spline"
  ;; Interpolation at data points
  (define-test "spline at x=0"
    (let ([spline (cubic-spline-natural spline-xs spline-ys)])
      (assert-true (approx-equal? 0 (spline-eval spline spline-xs 0.0) 1e-10))))
  (define-test "spline at x=1"
    (let ([spline (cubic-spline-natural spline-xs spline-ys)])
      (assert-true (approx-equal? 1 (spline-eval spline spline-xs 1.0) 1e-10))))
  (define-test "spline at x=2"
    (let ([spline (cubic-spline-natural spline-xs spline-ys)])
      (assert-true (approx-equal? 4 (spline-eval spline spline-xs 2.0) 1e-10))))
  (define-test "spline at x=3"
    (let ([spline (cubic-spline-natural spline-xs spline-ys)])
      (assert-true (approx-equal? 9 (spline-eval spline spline-xs 3.0) 1e-10))))

  ;; Between points (should be smooth)
  (define-test "spline between 0 and 1"
    (let* ([spline (cubic-spline-natural spline-xs spline-ys)]
           [v05 (spline-eval spline spline-xs 0.5)])
      (assert-true (and (> v05 0) (< v05 1)))))
  (define-test "spline between 1 and 2"
    (let* ([spline (cubic-spline-natural spline-xs spline-ys)]
           [v15 (spline-eval spline spline-xs 1.5)])
      (assert-true (and (> v15 1) (< v15 4)))))
  (define-test "spline between 2 and 3"
    (let* ([spline (cubic-spline-natural spline-xs spline-ys)]
           [v25 (spline-eval spline spline-xs 2.5)])
      (assert-true (and (> v25 4) (< v25 9))))))


(test-group "Bezier Curves"
  ;; Linear Bezier
  (define-test "bezier-linear x"
    (let ([result (bezier-linear bp0 bp1 0.5)])
      (assert-true (approx-equal? 0.5 (car result) 1e-10))))
  (define-test "bezier-linear y"
    (let ([result (bezier-linear bp0 bp1 0.5)])
      (assert-true (approx-equal? 0.5 (cdr result) 1e-10))))

  ;; Quadratic Bezier
  (define-test "bezier-quad t=0 x"
    (let ([result (bezier-quadratic bp0 bp1 bp2 0)])
      (assert-true (approx-equal? 0 (car result) 1e-10))))
  (define-test "bezier-quad t=0 y"
    (let ([result (bezier-quadratic bp0 bp1 bp2 0)])
      (assert-true (approx-equal? 0 (cdr result) 1e-10))))
  (define-test "bezier-quad t=1 x"
    (let ([result (bezier-quadratic bp0 bp1 bp2 1)])
      (assert-true (approx-equal? 2 (car result) 1e-10))))
  (define-test "bezier-quad t=1 y"
    (let ([result (bezier-quadratic bp0 bp1 bp2 1)])
      (assert-true (approx-equal? 0 (cdr result) 1e-10))))
  (define-test "bezier-quad t=0.5 x"
    (let ([result (bezier-quadratic bp0 bp1 bp2 0.5)])
      (assert-true (approx-equal? 1 (car result) 1e-10))))
  (define-test "bezier-quad t=0.5 y"
    (let ([result (bezier-quadratic bp0 bp1 bp2 0.5)])
      (assert-true (approx-equal? 0.5 (cdr result) 1e-10))))

  ;; Cubic Bezier
  (define-test "bezier-cubic t=0 x"
    (let ([result (bezier-cubic bp0 bp1 bp2 bp3 0)])
      (assert-true (approx-equal? 0 (car result) 1e-10))))
  (define-test "bezier-cubic t=0 y"
    (let ([result (bezier-cubic bp0 bp1 bp2 bp3 0)])
      (assert-true (approx-equal? 0 (cdr result) 1e-10))))
  (define-test "bezier-cubic t=1 x"
    (let ([result (bezier-cubic bp0 bp1 bp2 bp3 1)])
      (assert-true (approx-equal? 3 (car result) 1e-10))))
  (define-test "bezier-cubic t=1 y"
    (let ([result (bezier-cubic bp0 bp1 bp2 bp3 1)])
      (assert-true (approx-equal? 0 (cdr result) 1e-10))))

  ;; General Bezier (should match cubic)
  (define-test "bezier-general matches cubic x"
    (let* ([ctrl (list bp0 bp1 bp2 bp3)]
           [result-cubic (bezier-cubic bp0 bp1 bp2 bp3 0.5)]
           [result-general (bezier-general ctrl 0.5)])
      (assert-true (approx-equal? (car result-cubic) (car result-general) 1e-10))))
  (define-test "bezier-general matches cubic y"
    (let* ([ctrl (list bp0 bp1 bp2 bp3)]
           [result-cubic (bezier-cubic bp0 bp1 bp2 bp3 0.5)]
           [result-general (bezier-general ctrl 0.5)])
      (assert-true (approx-equal? (cdr result-cubic) (cdr result-general) 1e-10)))))


(test-group "Linear Regression"
  ;; Perfect line y = 2x + 1
  (define-test "linreg slope"
    (let ([reg (linreg reg-xs reg-ys)])
      (assert-true (approx-equal? 2 (car reg) 1e-10))))
  (define-test "linreg intercept"
    (let ([reg (linreg reg-xs reg-ys)])
      (assert-true (approx-equal? 1 (cdr reg) 1e-10))))
  (define-test "linreg R² (perfect)"
    (assert-true (approx-equal? 1 (linreg-r2 reg-xs reg-ys) 1e-10)))

  ;; Noisy data
  (define-test "noisy linreg slope ~2"
    (let ([reg (linreg noisy-xs noisy-ys)])
      (assert-true (approx-equal? 2 (car reg) 0.1))))
  (define-test "noisy linreg intercept ~0"
    (let ([reg (linreg noisy-xs noisy-ys)])
      (assert-true (approx-equal? 0 (cdr reg) 0.2))))
  (define-test "noisy R² > 0.99"
    (assert-true (> (linreg-r2 noisy-xs noisy-ys) 0.99))))


(test-group "Polynomial Fitting"
  ;; Fit y = x² with degree 2
  (define-test "polyfit x²: f(0)"
    (let ([fitted (polyfit fit-xs fit-ys 2)])
      (assert-true (approx-equal? 0 (poly-eval fitted 0) 0.1))))
  (define-test "polyfit x²: f(1)"
    (let ([fitted (polyfit fit-xs fit-ys 2)])
      (assert-true (approx-equal? 1 (poly-eval fitted 1) 0.1))))
  (define-test "polyfit x²: f(2)"
    (let ([fitted (polyfit fit-xs fit-ys 2)])
      (assert-true (approx-equal? 4 (poly-eval fitted 2) 0.1))))
  (define-test "polyfit x²: f(2.5)"
    (let ([fitted (polyfit fit-xs fit-ys 2)])
      (assert-true (approx-equal? 6.25 (poly-eval fitted 2.5) 0.1)))))


(test-group "Chebyshev"
  ;; Chebyshev nodes
  (define-test "chebyshev-nodes count"
    (let ([nodes (chebyshev-nodes 4)])
      (assert-equal 4 (length nodes))))
  (define-test "chebyshev-nodes in [-1,1]"
    (let ([nodes (chebyshev-nodes 4)])
      (assert-true (and (>= (apply min nodes) -1)
                        (<= (apply max nodes) 1)))))

  ;; Chebyshev polynomials
  (define-test "T_0(x) = 1"
    (assert-true (approx-equal? 1 (chebyshev-t 0 0.5) 1e-10)))
  (define-test "T_1(x) = x"
    (assert-true (approx-equal? 0.5 (chebyshev-t 1 0.5) 1e-10)))
  (define-test "T_2(x) = 2x²-1"
    (assert-true (approx-equal? (- (* 2 0.5 0.5) 1) (chebyshev-t 2 0.5) 1e-10)))
  (define-test "T_3(x) = 4x³-3x"
    (assert-true (approx-equal? (- (* 4 0.5 0.5 0.5) (* 3 0.5)) (chebyshev-t 3 0.5) 1e-10)))

  ;; Chebyshev nodes in interval
  (define-test "chebyshev-nodes-interval count"
    (let ([nodes (chebyshev-nodes-interval 3 0 1)])
      (assert-equal 3 (length nodes))))
  (define-test "nodes in [0,1]"
    (let ([nodes (chebyshev-nodes-interval 3 0 1)])
      (assert-true (and (>= (apply min nodes) 0)
                        (<= (apply max nodes) 1))))))


(test-group "B-Splines"
  ;; Uniform knot vector for degree 2 B-spline
  (define-test "bspline-basis p=0 in interval"
    (let ([knots (vector 0 1 2 3 4 5)])
      (assert-true (approx-equal? 1.0 (bspline-basis knots 2 0 2.5) 1e-10))))
  (define-test "bspline-basis p=0 outside"
    (let ([knots (vector 0 1 2 3 4 5)])
      (assert-true (approx-equal? 0.0 (bspline-basis knots 2 0 1.5) 1e-10))))

  ;; Degree 1 B-spline basis N_{2,1}(x) on [2,4), peaks at x=3
  (define-test "bspline-basis p=1 at peak"
    (let ([knots (vector 0 1 2 3 4 5)])
      (assert-true (approx-equal? 1.0 (bspline-basis knots 2 1 3.0) 1e-10))))
  (define-test "bspline-basis p=1 rising"
    (let ([knots (vector 0 1 2 3 4 5)])
      (assert-true (approx-equal? 0.5 (bspline-basis knots 2 1 2.5) 1e-10))))

  ;; B-spline curve
  (define-test "bspline-curve start x"
    (let* ([knots (vector 0 0 0 1 1 1)]
           [ctrl '((0 . 0) (1 . 2) (2 . 0))]
           [start (bspline-curve knots ctrl 2 0)])
      (assert-true (approx-equal? 0 (car start) 0.1))))
  (define-test "bspline-curve end x"
    (let* ([knots (vector 0 0 0 1 1 1)]
           [ctrl '((0 . 0) (1 . 2) (2 . 0))]
           [end (bspline-curve knots ctrl 2 0.999)])
      (assert-true (approx-equal? 2 (car end) 0.1)))))

;; Run the tests
(run-all-tests)
