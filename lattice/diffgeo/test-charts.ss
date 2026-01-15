;;; lattice/diffgeo/test-charts.ss — Tests for Coordinate Charts
;;;
;;; Run with: scheme --script lattice/diffgeo/test-charts.ss

(load "core/testing/test-framework.ss")
(load "lattice/diffgeo/charts.ss")

(display "\n====\n")
(display "Coordinate Charts Tests\n")
(display "====\n\n")

;;; Helper: approximately equal vectors
(define (vec-approx-equal? v1 v2 eps)
  (and (= (vector-length v1) (vector-length v2))
       (let loop ([i 0])
         (if (= i (vector-length v1))
             #t
             (and (< (abs (- (vector-ref v1 i) (vector-ref v2 i))) eps)
                  (loop (+ i 1)))))))

;;; ====
;;; Chart Construction Tests
;;; ====

(test-group chart-construction

  (define-test test-make-identity-chart
    (let ([chart (make-identity-chart 'test 2)])
      (assert-true (chart? chart))
      (assert-equal 'test (chart-name chart))
      (assert-equal 2 (chart-dim chart))))

  (define-test test-make-polar-chart
    (let ([polar (make-polar-chart)])
      (assert-true (chart? polar))
      (assert-equal 'polar (chart-name polar))
      (assert-equal 2 (chart-dim polar))))

  (define-test test-make-spherical-chart
    (let ([spherical (make-spherical-chart)])
      (assert-true (chart? spherical))
      (assert-equal 'spherical (chart-name spherical))
      (assert-equal 3 (chart-dim spherical))))

  (define-test test-make-cylindrical-chart
    (let ([cylindrical (make-cylindrical-chart)])
      (assert-true (chart? cylindrical))
      (assert-equal 'cylindrical (chart-name cylindrical))
      (assert-equal 3 (chart-dim cylindrical))))
)

;;; ====
;;; Chart Operations Tests
;;; ====

(test-group chart-operations

  ;; Test polar coordinates: (1, 0) should map to (1, 0) in polar
  (define-test test-polar-apply-x-axis
    (let* ([polar (make-polar-chart)]
           [point (vector 1.0 0.0)]
           [coords (chart-apply polar point)])
      (assert-true (vec-approx-equal? coords (vector 1.0 0.0) 1e-10))))

  ;; Test polar: (0, 1) should map to (1, π/2)
  (define-test test-polar-apply-y-axis
    (let* ([polar (make-polar-chart)]
           [point (vector 0.0 1.0)]
           [coords (chart-apply polar point)]
           [expected (vector 1.0 (/ 3.14159265358979 2))])
      (assert-true (vec-approx-equal? coords expected 1e-5))))

  ;; Test polar inverse: (2, π/4) should map to (√2, √2)
  (define-test test-polar-inverse
    (let* ([polar (make-polar-chart)]
           [coords (vector 2.0 (/ 3.14159265358979 4))]
           [point (chart-apply-inverse polar coords)]
           [expected (vector (sqrt 2) (sqrt 2))])
      (assert-true (vec-approx-equal? point expected 1e-5))))

  ;; Test round-trip: polar -> cartesian -> polar
  (define-test test-polar-roundtrip
    (let* ([polar (make-polar-chart)]
           [original (vector 3.0 0.5)]
           [point (chart-apply-inverse polar original)]
           [coords (chart-apply polar point)])
      (assert-true (vec-approx-equal? coords original 1e-10))))

  ;; Test domain: origin should NOT be in polar domain
  (define-test test-polar-domain-excludes-origin
    (let ([polar (make-polar-chart)])
      (assert-false (chart-contains? polar (vector 0.0 0.0)))))

  ;; Test domain: negative x-axis should NOT be in polar domain
  (define-test test-polar-domain-excludes-neg-x
    (let ([polar (make-polar-chart)])
      (assert-false (chart-contains? polar (vector -1.0 0.0)))))

  ;; Test spherical: (1, 0, 0) → (1, π/2, 0)
  (define-test test-spherical-apply
    (let* ([spherical (make-spherical-chart)]
           [point (vector 1.0 0.0 0.0)]
           [coords (chart-apply spherical point)]
           [expected (vector 1.0 (/ 3.14159265358979 2) 0.0)])
      (assert-true (vec-approx-equal? coords expected 1e-5))))

  ;; Test cylindrical round-trip
  (define-test test-cylindrical-roundtrip
    (let* ([cyl (make-cylindrical-chart)]
           [original (vector 2.0 0.7 3.0)]  ; (ρ, φ, z)
           [point (chart-apply-inverse cyl original)]
           [coords (chart-apply cyl point)])
      (assert-true (vec-approx-equal? coords original 1e-10))))
)

;;; ====
;;; Transition Function Tests
;;; ====

(test-group transition-functions

  ;; Transition from polar to cartesian (identity on R^2)
  (define-test test-transition-polar-to-cartesian
    (let* ([polar (make-polar-chart)]
           [cartesian (make-cartesian-chart)]
           [polar-coords (vector 2.0 (/ 3.14159265358979 4))]
           [cart-coords (transition-apply polar cartesian polar-coords)]
           [expected (vector (sqrt 2) (sqrt 2))])
      (assert-true (vec-approx-equal? cart-coords expected 1e-5))))

  ;; Transition from cartesian to polar
  (define-test test-transition-cartesian-to-polar
    (let* ([polar (make-polar-chart)]
           [cartesian (make-cartesian-chart)]
           [cart-coords (vector 1.0 1.0)]
           [polar-coords (transition-apply cartesian polar cart-coords)]
           [expected (vector (sqrt 2) (/ 3.14159265358979 4))])
      (assert-true (vec-approx-equal? polar-coords expected 1e-5))))

  ;; Round-trip through transitions
  (define-test test-transition-roundtrip
    (let* ([polar (make-polar-chart)]
           [cartesian (make-cartesian-chart)]
           [original (vector 3.0 0.8)]
           [via-cart (transition-apply polar cartesian original)]
           [back (transition-apply cartesian polar via-cart)])
      (assert-true (vec-approx-equal? back original 1e-10))))
)

;;; ====
;;; Jacobian Tests
;;; ====

(test-group jacobian-computation

  ;; Jacobian of identity should be identity matrix
  (define-test test-jacobian-identity
    (let* ([f (lambda (v) v)]
           [coords (vector 1.0 2.0)]
           [J (jacobian-numerical f coords)]
           ;; Construct identity matrix directly (avoid name conflict with prelude)
           [expected (matrix-from-lists '((1 0) (0 1)))])
      (assert-true (matrix-approx-equal? J expected 1e-5))))

  ;; Jacobian of polar-to-cartesian transition
  ;; For τ(r,θ) = (r cos θ, r sin θ):
  ;; J = [ cos θ   -r sin θ ]
  ;;     [ sin θ    r cos θ ]
  (define-test test-jacobian-polar-to-cart
    (let* ([polar (make-polar-chart)]
           [cartesian (make-cartesian-chart)]
           [r 2.0]
           [theta (/ 3.14159265358979 3)]  ; 60 degrees
           [coords (vector r theta)]
           [J (transition-jacobian polar cartesian coords)]
           ;; Expected values
           [cos-t (cos theta)]
           [sin-t (sin theta)]
           [expected (matrix-from-lists
                       (list (list cos-t (- (* r sin-t)))
                             (list sin-t (* r cos-t))))])
      (assert-true (matrix-approx-equal? J expected 1e-5))))

  ;; Test determinant of 2x2
  (define-test test-determinant-2x2
    (let ([M (matrix-from-lists '((1 2) (3 4)))])
      (assert-true (< (abs (- (jacobian-determinant M) -2.0)) 1e-10))))

  ;; Test determinant of 3x3
  (define-test test-determinant-3x3
    (let ([M (matrix-from-lists '((1 2 3) (4 5 6) (7 8 10)))])
      (assert-true (< (abs (- (jacobian-determinant M) -3.0)) 1e-10))))

  ;; Jacobian determinant of polar-to-cart should be r
  (define-test test-jacobian-det-polar
    (let* ([polar (make-polar-chart)]
           [cartesian (make-cartesian-chart)]
           [r 3.0]
           [theta 0.5]
           [J (transition-jacobian polar cartesian (vector r theta))]
           [det (jacobian-determinant J)])
      (assert-true (< (abs (- det r)) 1e-5))))
)

;;; ====
;;; Atlas Tests
;;; ====

(test-group atlas-operations

  (define-test test-make-atlas
    (let* ([c1 (make-identity-chart 'chart1 2)]
           [c2 (make-polar-chart)]
           [atlas (make-atlas 'test-atlas (list c1 c2))])
      (assert-true (atlas? atlas))
      (assert-equal 'test-atlas (atlas-name atlas))
      (assert-equal 2 (atlas-dim atlas))
      (assert-equal 2 (atlas-chart-count atlas))))

  (define-test test-atlas-dimension-check
    (let* ([c2d (make-identity-chart 'c2d 2)]
           [c3d (make-identity-chart 'c3d 3)]
           [result (make-atlas 'bad (list c2d c3d))])
      ;; Should return error for inconsistent dimensions
      (assert-true (and (pair? result) (eq? (car result) 'error)))))

  (define-test test-atlas-find-chart
    (let* ([cart (make-cartesian-chart)]
           [polar (make-polar-chart)]
           [atlas (make-atlas 'r2-atlas (list cart polar))]
           [point (vector 1.0 1.0)]
           [found (atlas-find-chart atlas point)])
      ;; Should find some chart (cartesian covers all of R^2)
      (assert-true (chart? found))))

  (define-test test-atlas-find-chart-by-name
    (let* ([cart (make-cartesian-chart)]
           [polar (make-polar-chart)]
           [atlas (make-atlas 'r2-atlas (list cart polar))]
           [found (atlas-find-chart-by-name atlas 'polar)])
      (assert-true (chart? found))
      (assert-equal 'polar (chart-name found))))

  (define-test test-atlas-coords
    (let* ([cart (make-cartesian-chart)]
           [atlas (make-atlas 'simple (list cart))]
           [point (vector 3.0 4.0)]
           [result (atlas-coords atlas point)])
      (assert-equal 'cartesian (car result))
      (assert-true (vec-approx-equal? (cdr result) point 1e-10))))

  (define-test test-atlas-add-chart
    (let* ([c1 (make-cartesian-chart)]
           [atlas (make-atlas 'growing (list c1))]
           [c2 (make-polar-chart)]
           [atlas2 (atlas-add-chart atlas c2)])
      (assert-equal 2 (atlas-chart-count atlas2))))
)

;;; ====
;;; Summary
;;; ====

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
      (exit 1))
