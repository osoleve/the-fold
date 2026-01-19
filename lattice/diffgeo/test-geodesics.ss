;;; lattice/diffgeo/test-geodesics.ss — Tests for Geodesic Computation
;;;
;;; Tests cover:
;;;   - Euclidean geodesics (straight lines)
;;;   - Polar coordinates geodesics
;;;   - Exponential/logarithm map roundtrips
;;;   - Parallel transport preserves norm
;;;   - Geodesic distance

(load "core/testing/test-framework.ss")
(load "lattice/diffgeo/geodesics.ss")

;;; ============================================================================
;;; Test Configuration
;;; ============================================================================

(define *test-tolerance* 1e-4)

(define (approx-equal? a b tol)
  (< (abs (- a b)) tol))

(define (vec-approx-equal? v1 v2 tol)
  (let ([n (vector-length v1)])
    (let loop ([i 0])
      (if (>= i n)
          #t
          (and (approx-equal? (vector-ref v1 i) (vector-ref v2 i) tol)
               (loop (+ i 1)))))))

;;; ============================================================================
;;; Euclidean Metric Tests
;;; ============================================================================

(test-group "euclidean-geodesics"
  ;; Create 2D Euclidean metric
  (define euclidean-chart (make-identity-chart 'euclidean-2d 2))
  (define euclidean-metric (make-euclidean-metric euclidean-chart))

  (define-test "geodesic is straight line"
    ;; In flat space, geodesic from (0,0) with velocity (1,0) should reach (1,0) at t=1
    (let* ([p (vector 0.0 0.0)]
           [v (vector 1.0 0.0)]
           [endpoint (exp-map euclidean-metric p v 100)])
      (assert-true (vec-approx-equal? endpoint (vector 1.0 0.0) *test-tolerance*))))

  (define-test "geodesic preserves direction in flat space"
    ;; Diagonal geodesic
    (let* ([p (vector 0.0 0.0)]
           [v (vector 1.0 1.0)]
           [endpoint (exp-map euclidean-metric p v 100)])
      (assert-true (vec-approx-equal? endpoint (vector 1.0 1.0) *test-tolerance*))))

  (define-test "exp-log roundtrip"
    ;; log(exp(v)) should equal v in flat space
    (let* ([p (vector 0.0 0.0)]
           [v (vector 0.5 0.3)]
           [q (exp-map euclidean-metric p v 100)]
           [result (log-map euclidean-metric p q 100)])
      (assert-true (and (pair? result) (eq? (car result) 'ok)))
      (let ([v-recovered (cadr result)])
        (assert-true (vec-approx-equal? v-recovered v *test-tolerance*)))))

  (define-test "geodesic distance in flat space"
    ;; Distance should equal Euclidean distance
    (let* ([p (vector 0.0 0.0)]
           [q (vector 3.0 4.0)]  ; Distance should be 5
           [dist (geodesic-distance euclidean-metric p q 100)])
      (assert-true (approx-equal? dist 5.0 *test-tolerance*))))

  (define-test "parallel transport in flat space"
    ;; In flat space, parallel transport doesn't change the vector
    (let* ([p (vector 0.0 0.0)]
           [geodesic-vel (vector 1.0 0.0)]
           [V (vector 0.0 1.0)]  ; Orthogonal to geodesic
           [V-transported (parallel-transport euclidean-metric p geodesic-vel V 1.0 100)])
      (assert-true (vec-approx-equal? V-transported V *test-tolerance*)))))

;;; ============================================================================
;;; Polar Coordinates Tests
;;; ============================================================================

(test-group "polar-geodesics"
  ;; Create polar metric
  ;; ds² = dr² + r²dθ²
  (define polar-chart (make-polar-chart))
  (define polar-metric (make-polar-metric polar-chart))

  (define-test "radial geodesic stays radial"
    ;; Starting at (r=1, θ=0) with velocity (1, 0) should move radially
    (let* ([p (vector 1.0 0.0)]    ; r=1, θ=0
           [v (vector 1.0 0.0)]    ; dr/dt=1, dθ/dt=0
           [endpoint (exp-map polar-metric p v 100)])
      ;; Should reach approximately (r=2, θ=0)
      (assert-true (approx-equal? (vector-ref endpoint 0) 2.0 *test-tolerance*))
      (assert-true (approx-equal? (vector-ref endpoint 1) 0.0 *test-tolerance*))))

  (define-test "circular geodesic"
    ;; For circular motion at radius r, the geodesic equation gives
    ;; d²r/dt² = r(dθ/dt)² (centripetal acceleration)
    ;; A pure tangential initial velocity won't stay on a circle
    ;; (geodesics in polar coords curve inward/outward)
    ;; This is just checking the geodesic computes without error
    (let* ([p (vector 2.0 0.0)]    ; r=2, θ=0
           [v (vector 0.0 0.5)]    ; Pure angular velocity (in coord space)
           [endpoint (exp-map polar-metric p v 100)])
      ;; Just verify we get a result
      (assert-true (vector? endpoint)))))

;;; ============================================================================
;;; Parallel Transport Tests
;;; ============================================================================

(test-group "parallel-transport"
  (define euclidean-chart (make-identity-chart 'euclidean-2d 2))
  (define euclidean-metric (make-euclidean-metric euclidean-chart))

  (define-test "preserves metric norm"
    ;; g(V,V) should be constant along parallel transport
    (let* ([p (vector 0.0 0.0)]
           [geodesic-vel (vector 1.0 1.0)]
           [V (vector 1.0 0.0)]
           [initial-norm (metric-norm euclidean-metric p V)]
           [V-transported (parallel-transport euclidean-metric p geodesic-vel V 1.0 100)]
           [q (exp-map euclidean-metric p geodesic-vel 100)]
           [final-norm (metric-norm euclidean-metric q V-transported)])
      (assert-true (approx-equal? initial-norm final-norm *test-tolerance*))))

  (define-test "transports tangent vector to itself"
    ;; The tangent vector to a geodesic parallel transports to itself
    ;; (this is the definition of geodesic)
    (let* ([p (vector 0.0 0.0)]
           [v (vector 1.0 0.5)]
           ;; Transport v along the geodesic defined by v itself
           [v-transported (parallel-transport euclidean-metric p v v 1.0 100)]
           ;; Final velocity should equal v (up to scaling by metric)
           )
      ;; In flat space with Euclidean metric, v-transported = v
      (assert-true (vec-approx-equal? v-transported v *test-tolerance*)))))

;;; ============================================================================
;;; Geodesic Interpolation Tests
;;; ============================================================================

(test-group "geodesic-interpolation"
  (define euclidean-chart (make-identity-chart 'euclidean-2d 2))
  (define euclidean-metric (make-euclidean-metric euclidean-chart))

  (define (interp-result-vec result)
    ;; Helper: extract vec from result or return #f if error
    (if (and (pair? result) (eq? (car result) 'err))
        #f
        result))

  (define-test "t=0 gives start point"
    (let* ([p (vector 0.0 0.0)]
           [q (vector 2.0 2.0)]
           [interp (interp-result-vec (geodesic-interpolate euclidean-metric p q 0.0 100))])
      (assert-true (and interp (vec-approx-equal? interp p *test-tolerance*)))))

  (define-test "t=1 gives end point"
    (let* ([p (vector 0.0 0.0)]
           [q (vector 2.0 2.0)]
           [interp (interp-result-vec (geodesic-interpolate euclidean-metric p q 1.0 100))])
      (assert-true (and interp (vec-approx-equal? interp q *test-tolerance*)))))

  (define-test "t=0.5 gives midpoint"
    (let* ([p (vector 0.0 0.0)]
           [q (vector 2.0 2.0)]
           [interp (interp-result-vec (geodesic-interpolate euclidean-metric p q 0.5 100))])
      (assert-true (and interp (vec-approx-equal? interp (vector 1.0 1.0) *test-tolerance*))))))

;;; ============================================================================
;;; Edge Cases
;;; ============================================================================

(test-group "edge-cases"
  (define euclidean-chart (make-identity-chart 'euclidean-2d 2))
  (define euclidean-metric (make-euclidean-metric euclidean-chart))

  (define-test "zero velocity stays at point"
    (let* ([p (vector 1.0 1.0)]
           [v (vector 0.0 0.0)]
           [endpoint (exp-map euclidean-metric p v 100)])
      (assert-true (vec-approx-equal? endpoint p *test-tolerance*))))

  (define-test "distance from point to itself is zero"
    (let* ([p (vector 1.0 2.0)]
           [dist (geodesic-distance euclidean-metric p p 100)])
      (assert-true (approx-equal? dist 0.0 *test-tolerance*)))))

;;; ============================================================================
;;; 3D Tests
;;; ============================================================================

(test-group "3d-geodesics"
  (define euclidean-chart-3d (make-identity-chart 'euclidean-3d 3))
  (define euclidean-metric-3d (make-euclidean-metric euclidean-chart-3d))

  (define-test "3d straight line geodesic"
    (let* ([p (vector 0.0 0.0 0.0)]
           [v (vector 1.0 2.0 3.0)]
           [endpoint (exp-map euclidean-metric-3d p v 100)])
      (assert-true (vec-approx-equal? endpoint v *test-tolerance*))))

  (define-test "3d distance"
    (let* ([p (vector 0.0 0.0 0.0)]
           [q (vector 1.0 2.0 2.0)]  ; Distance = 3
           [dist (geodesic-distance euclidean-metric-3d p q 100)])
      (assert-true (approx-equal? dist 3.0 *test-tolerance*)))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
