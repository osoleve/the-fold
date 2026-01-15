;;; lattice/diffgeo/test-tangent.ss — Tests for Tangent and Cotangent Spaces
;;;
;;; Run with: scheme --script lattice/diffgeo/test-tangent.ss

(load "core/testing/test-framework.ss")
(load "lattice/diffgeo/tangent.ss")

(display "\n====\n")
(display "Tangent and Cotangent Space Tests\n")
(display "====\n\n")

;;; Helper: approximately equal vectors
(define (vec-approx-equal? v1 v2 eps)
  (and (= (vector-length v1) (vector-length v2))
       (let loop ([i 0])
         (if (= i (vector-length v1))
             #t
             (and (< (abs (- (vector-ref v1 i) (vector-ref v2 i))) eps)
                  (loop (+ i 1)))))))

;;; ============================================================================
;;; Tangent Vector Construction Tests
;;; ============================================================================

(test-group tangent-construction

  (define-test test-make-tangent-vector
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [components (vector 3.0 4.0)]
           [tv (make-tangent-vector point cart components)])
      (assert-true (tangent-vector? tv))
      (assert-true (vec-approx-equal? (tangent-vector-point tv) point 1e-10))
      (assert-equal 'cartesian (chart-name (tangent-vector-chart tv)))
      (assert-true (vec-approx-equal? (tangent-vector-components tv) components 1e-10))))

  (define-test test-tangent-vector-dim
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [tv (make-tangent-vector point cart (vector 1.0 0.0))])
      (assert-equal 2 (tangent-vector-dim tv))))

  (define-test test-tangent-zero
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [tv (tangent-zero point cart)])
      (assert-true (tangent-vector? tv))
      (assert-true (vec-approx-equal? (tangent-vector-components tv) (vector 0.0 0.0) 1e-10))))

  (define-test test-tangent-basis-vector
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [e0 (tangent-basis-vector point cart 0)]
           [e1 (tangent-basis-vector point cart 1)])
      (assert-true (vec-approx-equal? (tangent-vector-components e0) (vector 1.0 0.0) 1e-10))
      (assert-true (vec-approx-equal? (tangent-vector-components e1) (vector 0.0 1.0) 1e-10))))

  (define-test test-tangent-point-not-in-chart
    (let* ([polar (make-polar-chart)]
           [origin (vector 0.0 0.0)]  ; Origin not in polar chart
           [result (make-tangent-vector origin polar (vector 1.0 0.0))])
      (assert-true (and (pair? result) (eq? (car result) 'error)))))
)

;;; ============================================================================
;;; Tangent Vector Operations Tests
;;; ============================================================================

(test-group tangent-operations

  (define-test test-tangent-add
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [tv1 (make-tangent-vector point cart (vector 1.0 0.0))]
           [tv2 (make-tangent-vector point cart (vector 0.0 1.0))]
           [sum (tangent-add tv1 tv2)])
      (assert-true (tangent-vector? sum))
      (assert-true (vec-approx-equal? (tangent-vector-components sum) (vector 1.0 1.0) 1e-10))))

  (define-test test-tangent-sub
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [tv1 (make-tangent-vector point cart (vector 3.0 4.0))]
           [tv2 (make-tangent-vector point cart (vector 1.0 1.0))]
           [diff (tangent-sub tv1 tv2)])
      (assert-true (vec-approx-equal? (tangent-vector-components diff) (vector 2.0 3.0) 1e-10))))

  (define-test test-tangent-scale
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [tv (make-tangent-vector point cart (vector 1.0 2.0))]
           [scaled (tangent-scale 3.0 tv)])
      (assert-true (vec-approx-equal? (tangent-vector-components scaled) (vector 3.0 6.0) 1e-10))))

  (define-test test-tangent-negate
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [tv (make-tangent-vector point cart (vector 1.0 -2.0))]
           [neg (tangent-negate tv)])
      (assert-true (vec-approx-equal? (tangent-vector-components neg) (vector -1.0 2.0) 1e-10))))

  (define-test test-tangent-add-different-points-error
    (let* ([cart (make-cartesian-chart)]
           [tv1 (make-tangent-vector (vector 1.0 0.0) cart (vector 1.0 0.0))]
           [tv2 (make-tangent-vector (vector 0.0 1.0) cart (vector 0.0 1.0))]
           [result (tangent-add tv1 tv2)])
      (assert-true (and (pair? result) (eq? (car result) 'error)))))
)

;;; ============================================================================
;;; Cotangent Vector Tests
;;; ============================================================================

(test-group cotangent-construction

  (define-test test-make-cotangent-vector
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [components (vector 3.0 4.0)]
           [cv (make-cotangent-vector point cart components)])
      (assert-true (cotangent-vector? cv))
      (assert-true (vec-approx-equal? (cotangent-vector-components cv) components 1e-10))))

  (define-test test-cotangent-basis-form
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [dx0 (cotangent-basis-form point cart 0)]
           [dx1 (cotangent-basis-form point cart 1)])
      (assert-true (vec-approx-equal? (cotangent-vector-components dx0) (vector 1.0 0.0) 1e-10))
      (assert-true (vec-approx-equal? (cotangent-vector-components dx1) (vector 0.0 1.0) 1e-10))))

  (define-test test-cotangent-operations
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [cv1 (make-cotangent-vector point cart (vector 1.0 2.0))]
           [cv2 (make-cotangent-vector point cart (vector 3.0 1.0))]
           [sum (cotangent-add cv1 cv2)])
      (assert-true (vec-approx-equal? (cotangent-vector-components sum) (vector 4.0 3.0) 1e-10))))
)

;;; ============================================================================
;;; Pairing Tests
;;; ============================================================================

(test-group pairing

  ;; The pairing ⟨ω, v⟩ should equal Σᵢ ωᵢ vⁱ
  (define-test test-covector-apply-simple
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [cv (make-cotangent-vector point cart (vector 2.0 3.0))]  ; 2 dx + 3 dy
           [tv (make-tangent-vector point cart (vector 4.0 5.0))])   ; 4 ∂/∂x + 5 ∂/∂y
      ;; ⟨2dx + 3dy, 4∂/∂x + 5∂/∂y⟩ = 2*4 + 3*5 = 8 + 15 = 23
      (assert-true (< (abs (- (covector-apply cv tv) 23.0)) 1e-10))))

  ;; Basis duality: ⟨dxⁱ, ∂/∂xʲ⟩ = δⁱⱼ
  (define-test test-basis-duality
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [dx0 (cotangent-basis-form point cart 0)]
           [dx1 (cotangent-basis-form point cart 1)]
           [e0 (tangent-basis-vector point cart 0)]
           [e1 (tangent-basis-vector point cart 1)])
      ;; ⟨dx⁰, ∂/∂x⁰⟩ = 1
      (assert-true (< (abs (- (covector-apply dx0 e0) 1.0)) 1e-10))
      ;; ⟨dx⁰, ∂/∂x¹⟩ = 0
      (assert-true (< (abs (covector-apply dx0 e1)) 1e-10))
      ;; ⟨dx¹, ∂/∂x⁰⟩ = 0
      (assert-true (< (abs (covector-apply dx1 e0)) 1e-10))
      ;; ⟨dx¹, ∂/∂x¹⟩ = 1
      (assert-true (< (abs (- (covector-apply dx1 e1) 1.0)) 1e-10))))
)

;;; ============================================================================
;;; Chart Change Tests
;;; ============================================================================

(test-group chart-change

  ;; Test tangent vector transformation from cartesian to polar
  ;; At point (1, 0) in cartesian:
  ;;   ∂/∂x in cartesian = ∂/∂r in polar (since r direction points along x)
  ;;   ∂/∂y in cartesian = (1/r)∂/∂θ in polar = ∂/∂θ at r=1
  (define-test test-tangent-change-cart-to-polar
    (let* ([cart (make-cartesian-chart)]
           [polar (make-polar-chart)]
           [point (vector 1.0 0.0)]  ; r=1, θ=0
           ;; ∂/∂x at (1,0) should map to ∂/∂r
           [tv-x (make-tangent-vector point cart (vector 1.0 0.0))]
           [tv-polar (tangent-change-chart tv-x polar)])
      (assert-true (tangent-vector? tv-polar))
      ;; Should be approximately ∂/∂r direction: (1, 0) in polar basis
      (let ([comps (tangent-vector-components tv-polar)])
        (assert-true (< (abs (- (vector-ref comps 0) 1.0)) 1e-5))
        (assert-true (< (abs (vector-ref comps 1)) 1e-5)))))

  ;; Test that ∂/∂y at (1, 0) transforms to ∂/∂θ in polar
  (define-test test-tangent-change-y-to-theta
    (let* ([cart (make-cartesian-chart)]
           [polar (make-polar-chart)]
           [point (vector 1.0 0.0)]  ; r=1, θ=0
           ;; ∂/∂y at (1,0) should map to (1/r)∂/∂θ = ∂/∂θ at r=1
           [tv-y (make-tangent-vector point cart (vector 0.0 1.0))]
           [tv-polar (tangent-change-chart tv-y polar)])
      (let ([comps (tangent-vector-components tv-polar)])
        ;; Should be approximately (0, 1) in polar basis
        (assert-true (< (abs (vector-ref comps 0)) 1e-5))
        (assert-true (< (abs (- (vector-ref comps 1) 1.0)) 1e-5)))))

  ;; Round-trip: transform to polar and back should recover original
  (define-test test-tangent-change-roundtrip
    (let* ([cart (make-cartesian-chart)]
           [polar (make-polar-chart)]
           [point (vector 1.0 1.0)]  ; In first quadrant
           [original-comps (vector 2.0 3.0)]
           [tv (make-tangent-vector point cart original-comps)]
           [tv-polar (tangent-change-chart tv polar)]
           [tv-back (tangent-change-chart tv-polar cart)])
      (assert-true (vec-approx-equal? (tangent-vector-components tv-back) original-comps 1e-4))))
)

;;; ============================================================================
;;; Tangent Space Tests
;;; ============================================================================

(test-group tangent-space

  (define-test test-make-tangent-space
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [ts (make-tangent-space point cart)])
      (assert-true (tangent-space? ts))
      (assert-equal 2 (tangent-space-dim ts))))

  (define-test test-tangent-space-basis
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [ts (make-tangent-space point cart)]
           [basis (tangent-space-basis ts)])
      (assert-equal 2 (length basis))
      (assert-true (tangent-vector? (car basis)))
      (assert-true (tangent-vector? (cadr basis)))))

  (define-test test-tangent-space-vector
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [ts (make-tangent-space point cart)]
           [tv (tangent-space-vector ts (vector 3.0 4.0))])
      (assert-true (tangent-vector? tv))
      (assert-true (vec-approx-equal? (tangent-vector-components tv) (vector 3.0 4.0) 1e-10))))
)

;;; ============================================================================
;;; Cotangent Space Tests
;;; ============================================================================

(test-group cotangent-space

  (define-test test-make-cotangent-space
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [cs (make-cotangent-space point cart)])
      (assert-true (cotangent-space? cs))
      (assert-equal 2 (cotangent-space-dim cs))))

  (define-test test-cotangent-space-basis
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           [cs (make-cotangent-space point cart)]
           [basis (cotangent-space-basis cs)])
      (assert-equal 2 (length basis))
      (assert-true (cotangent-vector? (car basis)))))
)

;;; ============================================================================
;;; Tangent Bundle Tests
;;; ============================================================================

(test-group tangent-bundle

  (define-test test-make-tangent-bundle
    (let* ([cart (make-cartesian-chart)]
           [atlas (make-atlas 'r2 (list cart))]
           [tb (make-tangent-bundle atlas)])
      (assert-true (tangent-bundle? tb))
      (assert-equal 4 (tangent-bundle-dim tb))))  ; dim(TM) = 2n = 4

  (define-test test-tangent-bundle-fiber
    (let* ([cart (make-cartesian-chart)]
           [atlas (make-atlas 'r2 (list cart))]
           [tb (make-tangent-bundle atlas)]
           [point (vector 1.0 2.0)]
           [fiber (tangent-bundle-fiber tb point)])
      (assert-true (tangent-space? fiber))
      (assert-equal 2 (tangent-space-dim fiber))))

  (define-test test-tangent-bundle-section
    (let* ([cart (make-cartesian-chart)]
           [atlas (make-atlas 'r2 (list cart))]
           [tb (make-tangent-bundle atlas)]
           ;; Define a constant vector field v = 3∂/∂x + 4∂/∂y
           [field (tangent-bundle-section tb
                    (lambda (p c) (vector 3.0 4.0)))]
           [point (vector 1.0 2.0)]
           [v-at-point (field point)])
      (assert-true (tangent-vector? v-at-point))
      (assert-true (vec-approx-equal? (tangent-vector-components v-at-point)
                                       (vector 3.0 4.0) 1e-10))))
)

;;; ============================================================================
;;; Pushforward Tests
;;; ============================================================================

(test-group pushforward

  ;; Test pushforward through scaling: f(x, y) = (2x, 3y)
  ;; Jacobian is [[2, 0], [0, 3]]
  ;; So df(∂/∂x) = 2∂/∂x, df(∂/∂y) = 3∂/∂y
  (define-test test-pushforward-scaling
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 1.0)]
           [tv (make-tangent-vector point cart (vector 1.0 1.0))]
           [f (lambda (v) (vector (* 2 (vector-ref v 0))
                                  (* 3 (vector-ref v 1))))]
           [pushed (pushforward f tv cart cart)])
      (assert-true (tangent-vector? pushed))
      ;; df(∂/∂x + ∂/∂y) = 2∂/∂x + 3∂/∂y
      (assert-true (vec-approx-equal? (tangent-vector-components pushed)
                                       (vector 2.0 3.0) 1e-5))))

  ;; Test pushforward through rotation by 90 degrees
  ;; f(x, y) = (-y, x)
  ;; Jacobian is [[0, -1], [1, 0]]
  (define-test test-pushforward-rotation
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 0.0)]
           [tv (make-tangent-vector point cart (vector 1.0 0.0))]  ; ∂/∂x
           [f (lambda (v) (vector (- (vector-ref v 1))
                                  (vector-ref v 0)))]
           [pushed (pushforward f tv cart cart)])
      ;; ∂/∂x should map to ∂/∂y (rotation by 90°)
      (assert-true (vec-approx-equal? (tangent-vector-components pushed)
                                       (vector 0.0 1.0) 1e-5))))
)

;;; ============================================================================
;;; Pullback Tests
;;; ============================================================================

(test-group pullback

  ;; Test pullback of a 1-form through scaling
  ;; f(x, y) = (2x, 3y), so f*(dx) pulls back to 2dx, f*(dy) to 3dy
  (define-test test-pullback-scaling
    (let* ([cart (make-cartesian-chart)]
           [source-point (vector 1.0 1.0)]
           [target-point (vector 2.0 3.0)]  ; f(1,1)
           [omega (make-cotangent-vector target-point cart (vector 1.0 0.0))]  ; dx at target
           [f (lambda (v) (vector (* 2 (vector-ref v 0))
                                  (* 3 (vector-ref v 1))))]
           [pulled (pullback-at f omega source-point cart cart)])
      (assert-true (cotangent-vector? pulled))
      ;; f*(dx) = 2dx at source (Jacobian transpose)
      (assert-true (vec-approx-equal? (cotangent-vector-components pulled)
                                       (vector 2.0 0.0) 1e-5))))

  ;; Verify duality: ⟨f*ω, v⟩ = ⟨ω, df(v)⟩
  (define-test test-pushforward-pullback-duality
    (let* ([cart (make-cartesian-chart)]
           [source-point (vector 1.0 1.0)]
           [f (lambda (v) (vector (* 2 (vector-ref v 0))
                                  (* 3 (vector-ref v 1))))]
           [target-point (f source-point)]
           ;; v at source
           [v (make-tangent-vector source-point cart (vector 1.0 2.0))]
           ;; ω at target
           [omega (make-cotangent-vector target-point cart (vector 4.0 5.0))]
           ;; Compute both sides
           [df-v (pushforward f v cart cart)]
           [f-star-omega (pullback-at f omega source-point cart cart)]
           [lhs (covector-apply f-star-omega v)]
           [rhs (covector-apply omega df-v)])
      ;; ⟨f*ω, v⟩ should equal ⟨ω, df(v)⟩
      (assert-true (< (abs (- lhs rhs)) 1e-5))))
)

;;; ============================================================================
;;; Differential Tests
;;; ============================================================================

(test-group differential

  ;; Test differential of f(x, y) = x² + y²
  ;; df = 2x dx + 2y dy
  (define-test test-differential-sum-of-squares
    (let* ([cart (make-cartesian-chart)]
           [point (vector 3.0 4.0)]
           [f (lambda (v) (+ (* (vector-ref v 0) (vector-ref v 0))
                             (* (vector-ref v 1) (vector-ref v 1))))]
           [df (differential f point cart)])
      (assert-true (cotangent-vector? df))
      ;; At (3, 4): df = 6dx + 8dy
      (assert-true (vec-approx-equal? (cotangent-vector-components df)
                                       (vector 6.0 8.0) 1e-5))))

  ;; Test differential of f(x, y) = xy
  ;; df = y dx + x dy
  (define-test test-differential-product
    (let* ([cart (make-cartesian-chart)]
           [point (vector 2.0 5.0)]
           [f (lambda (v) (* (vector-ref v 0) (vector-ref v 1)))]
           [df (differential f point cart)])
      ;; At (2, 5): df = 5dx + 2dy
      (assert-true (vec-approx-equal? (cotangent-vector-components df)
                                       (vector 5.0 2.0) 1e-5))))

  ;; Verify that df(v) equals directional derivative
  (define-test test-differential-as-directional-derivative
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 1.0)]
           [f (lambda (v) (+ (* (vector-ref v 0) (vector-ref v 0))
                             (* (vector-ref v 1) (vector-ref v 1))))]
           [df (differential f point cart)]
           [v (make-tangent-vector point cart (vector 1.0 0.0))]  ; ∂/∂x direction
           [pairing (covector-apply df v)])
      ;; ⟨df, ∂/∂x⟩ at (1,1) should be 2*1 = 2 (partial derivative wrt x)
      (assert-true (< (abs (- pairing 2.0)) 1e-5))))
)

;;; ============================================================================
;;; 3D Tests (Spherical Coordinates)
;;; ============================================================================

(test-group three-dimensional

  (define-test test-tangent-spherical
    (let* ([spherical (make-spherical-chart)]
           [point (vector 1.0 1.0 0.0)]  ; Not on z-axis or branch cut
           [tv (make-tangent-vector point spherical (vector 1.0 0.0 0.0))])
      (assert-true (tangent-vector? tv))
      (assert-equal 3 (tangent-vector-dim tv))))

  (define-test test-tangent-space-3d
    (let* ([spherical (make-spherical-chart)]
           [point (vector 1.0 1.0 0.0)]
           [ts (make-tangent-space point spherical)]
           [basis (tangent-space-basis ts)])
      (assert-equal 3 (length basis))))
)

;;; ============================================================================
;;; Lie Bracket Tests
;;; ============================================================================

(test-group lie-bracket

  ;; [X, X] = 0 (Lie bracket with self is zero)
  (define-test test-lie-bracket-self-zero
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           ;; X = x∂/∂x + y∂/∂y (radial field)
           [X (lambda (p)
                (make-tangent-vector p cart
                  (vector (vector-ref p 0) (vector-ref p 1))))]
           [bracket (lie-bracket X X point cart)])
      ;; [X, X] should be zero
      (assert-true (vec-approx-equal? (tangent-vector-components bracket)
                                       (vector 0.0 0.0) 1e-5))))

  ;; [X, Y] = -[Y, X] (antisymmetry)
  (define-test test-lie-bracket-antisymmetry
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 2.0)]
           ;; X = ∂/∂x (constant field)
           [X (lambda (p)
                (make-tangent-vector p cart (vector 1.0 0.0)))]
           ;; Y = x∂/∂y (varies with x)
           [Y (lambda (p)
                (make-tangent-vector p cart
                  (vector 0.0 (vector-ref p 0))))]
           [XY (lie-bracket X Y point cart)]
           [YX (lie-bracket Y X point cart)])
      ;; [X, Y] + [Y, X] should be zero
      (let ([sum (tangent-add XY YX)])
        (assert-true (vec-approx-equal? (tangent-vector-components sum)
                                         (vector 0.0 0.0) 1e-5)))))

  ;; Specific computation: [∂/∂x, x∂/∂y] = ∂/∂y
  ;; Because ∂/∂x applied to Y^2 = x gives 1, so [X,Y]^2 = 1
  (define-test test-lie-bracket-specific
    (let* ([cart (make-cartesian-chart)]
           [point (vector 2.0 3.0)]
           ;; X = ∂/∂x
           [X (lambda (p)
                (make-tangent-vector p cart (vector 1.0 0.0)))]
           ;; Y = x∂/∂y  (Y^1 = 0, Y^2 = x)
           [Y (lambda (p)
                (make-tangent-vector p cart
                  (vector 0.0 (vector-ref p 0))))]
           [bracket (lie-bracket X Y point cart)])
      ;; [∂/∂x, x∂/∂y] = ∂/∂y (since ∂x/∂x = 1)
      (assert-true (vec-approx-equal? (tangent-vector-components bracket)
                                       (vector 0.0 1.0) 1e-5))))

  ;; Constant fields have zero bracket
  (define-test test-lie-bracket-constant-fields
    (let* ([cart (make-cartesian-chart)]
           [point (vector 1.0 1.0)]
           ;; Two constant fields
           [X (lambda (p) (make-tangent-vector p cart (vector 1.0 0.0)))]
           [Y (lambda (p) (make-tangent-vector p cart (vector 0.0 1.0)))]
           [bracket (lie-bracket X Y point cart)])
      ;; [constant, constant] = 0
      (assert-true (vec-approx-equal? (tangent-vector-components bracket)
                                       (vector 0.0 0.0) 1e-5))))

  ;; Test lie-bracket-field creates a proper vector field
  (define-test test-lie-bracket-field
    (let* ([cart (make-cartesian-chart)]
           [X (lambda (p) (make-tangent-vector p cart (vector 1.0 0.0)))]
           [Y (lambda (p)
                (make-tangent-vector p cart
                  (vector 0.0 (vector-ref p 0))))]
           [bracket-field (lie-bracket-field X Y cart)]
           [p1 (vector 1.0 0.0)]
           [p2 (vector 2.0 0.0)]
           [v1 (bracket-field p1)]
           [v2 (bracket-field p2)])
      ;; [∂/∂x, x∂/∂y] = ∂/∂y everywhere (independent of x)
      (assert-true (tangent-vector? v1))
      (assert-true (tangent-vector? v2))
      (assert-true (vec-approx-equal? (tangent-vector-components v1)
                                       (vector 0.0 1.0) 1e-5))
      (assert-true (vec-approx-equal? (tangent-vector-components v2)
                                       (vector 0.0 1.0) 1e-5))))
)

;;; ============================================================================
;;; Summary
;;; ============================================================================

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
      (exit 1))
