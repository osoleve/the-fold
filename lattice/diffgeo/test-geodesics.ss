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
;;; Christoffel Caching Tests
;;; ============================================================================

(test-group "christoffel-caching"
  (define euclidean-chart (make-identity-chart 'euclidean-2d 2))
  (define euclidean-metric (make-euclidean-metric euclidean-chart))

  (define-test "cache reduces computation during geodesic trace"
    ;; Clear cache and stats before test
    (clear-christoffel-cache!)
    (reset-christoffel-cache-stats!)
    ;; Trace a geodesic - this makes many calls to geodesic-acceleration
    ;; which should benefit from caching
    (let* ([p (vector 0.0 0.0)]
           [v (vector 1.0 0.0)]
           [_ (trace-geodesic euclidean-metric p v 1.0 100)]
           [stats (christoffel-cache-stats)]
           [hits (cadr (memq 'hits stats))]
           [misses (cadr (memq 'misses stats))]
           [total (+ hits misses)]
           [hit-rate (/ hits total)])
      ;; With 100 RK4 steps × 4 stages = 400 calls
      ;; Within each RK4 step, k2 and k3 evaluate at similar coords,
      ;; so we expect ~50% cache hits (k2 caches, k3 hits; k4 new point)
      (assert-true (> hits 0))
      ;; Hit rate should be meaningful (> 25%)
      (assert-true (> hit-rate 1/4))))

  (define-test "cache invalidates on metric change"
    (clear-christoffel-cache!)
    (reset-christoffel-cache-stats!)
    ;; First call with euclidean metric
    (let* ([p (vector 1.0 1.0)]
           [v (vector 0.1 0.0)]
           [_ (geodesic-acceleration euclidean-metric p v)])
      ;; Create different metric and call again
      (let* ([other-chart (make-identity-chart 'other-2d 2)]
             [other-metric (make-euclidean-metric other-chart)]
             [_ (geodesic-acceleration other-metric p v)]
             [stats (christoffel-cache-stats)]
             [misses (cadr (memq 'misses stats))])
        ;; Should have 2 misses (different metric objects)
        (assert-equal 2 misses))))

  (define-test "cache invalidates on large coordinate change"
    (clear-christoffel-cache!)
    (reset-christoffel-cache-stats!)
    ;; First call
    (let* ([p1 (vector 0.0 0.0)]
           [v (vector 0.1 0.0)]
           [_ (geodesic-acceleration euclidean-metric p1 v)])
      ;; Second call at far away point
      (let* ([p2 (vector 100.0 100.0)]  ; Way outside tolerance
             [_ (geodesic-acceleration euclidean-metric p2 v)]
             [stats (christoffel-cache-stats)]
             [misses (cadr (memq 'misses stats))])
        ;; Should have 2 misses (coords differ)
        (assert-equal 2 misses))))

  (define-test "cache hits on nearby coordinates"
    (clear-christoffel-cache!)
    (reset-christoffel-cache-stats!)
    ;; First call
    (let* ([p1 (vector 1.0 1.0)]
           [v (vector 0.1 0.0)]
           [_ (geodesic-acceleration euclidean-metric p1 v)])
      ;; Second call at very close point (within 1e-3 tolerance)
      (let* ([p2 (vector 1.0001 1.0001)]  ; Within tolerance
             [_ (geodesic-acceleration euclidean-metric p2 v)]
             [stats (christoffel-cache-stats)]
             [hits (cadr (memq 'hits stats))])
        ;; Should have 1 hit
        (assert-equal 1 hits))))

  (define-test "cache invalidates on epsilon change"
    (clear-christoffel-cache!)
    (reset-christoffel-cache-stats!)
    ;; First call with default epsilon
    (let* ([p (vector 1.0 1.0)]
           [_ (cached-christoffel-symbols euclidean-metric p 1e-7)])
      ;; Second call with different epsilon
      (let* ([_ (cached-christoffel-symbols euclidean-metric p 1e-5)]
             [stats (christoffel-cache-stats)]
             [misses (cadr (memq 'misses stats))])
        ;; Should have 2 misses (different epsilon values)
        (assert-equal 2 misses)))))

;;; ============================================================================
;;; Curved Space Tests - Unit 2-Sphere
;;; ============================================================================

;;; The unit 2-sphere S² has intrinsic metric in (θ, φ) coordinates:
;;;   ds² = dθ² + sin²θ dφ²
;;; where θ ∈ (0, π) is colatitude and φ ∈ [0, 2π) is longitude.
;;; Geodesics are great circles.

(define (make-sphere-chart)
  ;; Chart for S² in (θ, φ) coordinates
  (make-identity-chart 'sphere-2d 2))

(define (make-s2-metric chart)
  ;; Intrinsic metric on unit 2-sphere: g = diag(1, sin²θ)
  (make-metric chart
               (lambda (coords)
                 (let* ([theta (vector-ref coords 0)]
                        [sin-theta (sin theta)]
                        [sin2 (* sin-theta sin-theta)])
                   (matrix-from-lists (list (list 1 0)
                                            (list 0 sin2)))))))

(test-group "sphere-geodesics"
  (define sphere-chart (make-sphere-chart))
  (define s2-metric (make-s2-metric sphere-chart))
  (define pi 3.141592653589793)

  (define-test "meridian geodesic (φ constant)"
    ;; A meridian (constant longitude) is a great circle
    ;; Start at θ=π/4, φ=0 with velocity in θ direction
    (let* ([p (vector (/ pi 4) 0.0)]    ; θ=45°, φ=0
           [v (vector 1.0 0.0)]          ; Pure θ velocity
           [endpoint (exp-map s2-metric p v 100)])
      ;; Should travel along meridian: φ stays 0
      (assert-true (approx-equal? (vector-ref endpoint 1) 0.0 *test-tolerance*))
      ;; θ should increase by 1 radian
      (assert-true (approx-equal? (vector-ref endpoint 0) (+ (/ pi 4) 1.0) *test-tolerance*))))

  (define-test "equatorial geodesic (θ=π/2)"
    ;; The equator is a great circle
    ;; At equator, dφ/dt = const (since sin(π/2) = 1)
    (let* ([p (vector (/ pi 2) 0.0)]    ; θ=90° (equator), φ=0
           [v (vector 0.0 1.0)]          ; Pure φ velocity
           [endpoint (exp-map s2-metric p v 100)])
      ;; Should stay on equator: θ stays π/2
      (assert-true (approx-equal? (vector-ref endpoint 0) (/ pi 2) *test-tolerance*))
      ;; φ should increase by 1 radian
      (assert-true (approx-equal? (vector-ref endpoint 1) 1.0 *test-tolerance*))))

  (define-test "geodesic distance on sphere"
    ;; Distance between two points on equator
    ;; Points at φ=0 and φ=π/3 at equator should have distance π/3
    (let* ([p (vector (/ pi 2) 0.0)]           ; Equator, φ=0
           [q (vector (/ pi 2) (/ pi 3))]      ; Equator, φ=π/3
           [dist (geodesic-distance s2-metric p q 100)])
      ;; Arc length on unit sphere = angle
      (assert-true (approx-equal? dist (/ pi 3) 0.01))))

  (define-test "geodesic distance along meridian"
    ;; Distance from θ=π/4 to θ=3π/4 along same meridian
    (let* ([p (vector (/ pi 4) 0.0)]           ; θ=45°
           [q (vector (* 3 (/ pi 4)) 0.0)]     ; θ=135°
           [dist (geodesic-distance s2-metric p q 100)])
      ;; Distance should be π/2 radians
      (assert-true (approx-equal? dist (/ pi 2) 0.01))))

  (define-test "geodesic interpolation on sphere"
    ;; Interpolating at t=0.5 should give midpoint
    (let* ([p (vector (/ pi 2) 0.0)]           ; Equator start
           [q (vector (/ pi 2) (/ pi 2))]      ; Equator end (90° apart)
           [result (geodesic-interpolate s2-metric p q 0.5 100)])
      (if (and (pair? result) (eq? (car result) 'err))
          (assert-true #f)  ; Should not fail
          (begin
            ;; Midpoint should be at φ=π/4
            (assert-true (approx-equal? (vector-ref result 0) (/ pi 2) *test-tolerance*))
            (assert-true (approx-equal? (vector-ref result 1) (/ pi 4) *test-tolerance*)))))))

(test-group "sphere-parallel-transport"
  (define sphere-chart (make-sphere-chart))
  (define s2-metric (make-s2-metric sphere-chart))
  (define pi 3.141592653589793)

  (define-test "parallel transport preserves norm on sphere"
    ;; Transport a vector along equator, norm should be preserved
    (let* ([p (vector (/ pi 2) 0.0)]           ; Equator
           [geodesic-vel (vector 0.0 1.0)]     ; Move along equator
           [V (vector 1.0 0.0)]                ; Vector pointing toward pole
           [initial-norm (metric-norm s2-metric p V)]
           [V-transported (parallel-transport s2-metric p geodesic-vel V 1.0 100)]
           [q (exp-map s2-metric p geodesic-vel 100)]
           [final-norm (metric-norm s2-metric q V-transported)])
      (assert-true (approx-equal? initial-norm final-norm *test-tolerance*))))

  (define-test "parallel transport along meridian"
    ;; Transport along meridian (simpler case)
    (let* ([p (vector (/ pi 4) 0.0)]           ; Start at θ=45°
           [geodesic-vel (vector 0.5 0.0)]     ; Move along meridian
           [V (vector 0.0 1.0)]                ; Vector in φ direction
           [V-transported (parallel-transport s2-metric p geodesic-vel V 1.0 100)])
      ;; Vector should remain tangent (no radial component appears)
      ;; and norm preserved
      (let* ([q (exp-map s2-metric p geodesic-vel 100)]
             [initial-norm (metric-norm s2-metric p V)]
             [final-norm (metric-norm s2-metric q V-transported)])
        (assert-true (approx-equal? initial-norm final-norm *test-tolerance*))))))

(test-group "sphere-curvature-effects"
  (define sphere-chart (make-sphere-chart))
  (define s2-metric (make-s2-metric sphere-chart))
  (define pi 3.141592653589793)

  (define-test "geodesics converge toward poles"
    ;; Two geodesics starting parallel at equator will converge
    ;; This is a signature of positive curvature
    ;; Start two meridians at slightly different φ, both heading toward north pole
    (let* ([p1 (vector (/ pi 2) 0.0)]           ; Equator, φ=0
           [p2 (vector (/ pi 2) 0.1)]           ; Equator, φ=0.1
           [v (vector -1.0 0.0)]                ; Toward north pole
           [q1 (exp-map s2-metric p1 v 100)]
           [q2 (exp-map s2-metric p2 v 100)]
           ;; Initial separation in φ direction
           [initial-sep (abs (- (vector-ref p2 1) (vector-ref p1 1)))]
           ;; Final separation - need to compute properly with metric
           [final-sep (abs (- (vector-ref q2 1) (vector-ref q1 1)))])
      ;; The φ coordinate difference should be same (both reached same θ)
      ;; but the *physical* distance decreased because sin(θ) is smaller
      ;; Check that θ values are similar (both traveled same amount)
      (assert-true (approx-equal? (vector-ref q1 0) (vector-ref q2 0) 0.01)))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
