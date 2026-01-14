;;; core/diff-physics/test-smooth-collision.ss --- Tests for Differentiable Collision
;;;
;;; Comprehensive tests for smooth collision primitives including:
;;; - Signed Distance Functions (SDFs)
;;; - Circle-circle and circle-plane contacts
;;; - Soft contact forces
;;; - Friction models
;;; - Material properties
;;;
;;; Run with: scheme --script core/diff-physics/test-smooth-collision.ss

(load "core/test-framework.ss")
(load "lattice/physics/diff/smooth-collision.ss")

(display "
====
         SMOOTH COLLISION TESTS
====
")

;;; ====
;;; Test Utilities
;;; ====

(define (assert-near actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    FAIL: Expected ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (display ", got ")
          (display actual)
          (newline)))

(define pi 3.141592653589793)

;;; ====
;;; Circle SDF Tests
;;; ====

(test-group circle-sdf-tests
            
            (define-test sdf-circle-outside
              ;; Point outside circle
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 5 0) tape)]
                     [center (lift-vec2 (vec2 0 0) tape)]
                     [radius 2]
                     [sdf (traced-sdf-circle point center radius)])
                    ;; Distance is 5 - 2 = 3
                    (assert-near (traced-value sdf) 3 0.01)))
            
            (define-test sdf-circle-inside
              ;; Point inside circle - negative SDF
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 0.5 0) tape)]
                     [center (lift-vec2 (vec2 0 0) tape)]
                     [radius 2]
                     [sdf (traced-sdf-circle point center radius)])
                    ;; Distance is 0.5 - 2 = -1.5
                    (assert-near (traced-value sdf) -1.5 0.01)))
            
            (define-test sdf-circle-on-boundary
              ;; Point on circle boundary
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 2 0) tape)]
                     [center (lift-vec2 (vec2 0 0) tape)]
                     [radius 2]
                     [sdf (traced-sdf-circle point center radius)])
                    (assert-near (traced-value sdf) 0 0.01)))
            
            (define-test sdf-circle-at-center
              ;; Point at center - most negative
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 0 0) tape)]
                     [center (lift-vec2 (vec2 0 0) tape)]
                     [radius 3]
                     [sdf (traced-sdf-circle point center radius)])
                    (assert-near (traced-value sdf) -3 0.1)))
            )

;;; ====
;;; Point SDF Tests
;;; ====

(test-group point-sdf-tests
            
            (define-test sdf-point-distance
              (let* ([tape (make-reverse-tape)]
                     [query (lift-vec2 (vec2 3 4) tape)]
                     [target (lift-vec2 (vec2 0 0) tape)]
                     [sdf (traced-sdf-point query target)])
                    ;; Distance should be 5
                    (assert-near (traced-value sdf) 5 0.01)))
            
            (define-test sdf-point-same-location
              (let* ([tape (make-reverse-tape)]
                     [query (lift-vec2 (vec2 2 3) tape)]
                     [target (lift-vec2 (vec2 2 3) tape)]
                     [sdf (traced-sdf-point query target)])
                    ;; Should be very close to 0 (smoothed)
                    (assert-near (traced-value sdf) 0 0.001)))
            )

;;; ====
;;; Box SDF Tests
;;; ====

(test-group box-sdf-tests
            
            (define-test sdf-box-inside
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 0.5 0.5) tape)]
                     [center (lift-vec2 (vec2 0 0) tape)]
                     [half-extents (vec2 2 1)]
                     [sdf (traced-sdf-box point center half-extents)])
                    ;; Inside should be negative
                    (assert-true (< (traced-value sdf) 0))))
            
            (define-test sdf-box-outside-axis
              ;; Point outside along x-axis
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 5 0) tape)]
                     [center (lift-vec2 (vec2 0 0) tape)]
                     [half-extents (vec2 2 1)]
                     [sdf (traced-sdf-box point center half-extents)])
                    ;; Should be approximately 3 (5 - 2)
                    (assert-near (traced-value sdf) 3 0.5)))
            )

;;; ====
;;; Capsule SDF Tests
;;; ====

(test-group capsule-sdf-tests
            
            (define-test sdf-capsule-near-endpoint
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 -1 0) tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 4 0) tape)]
                     [radius 0.5]
                     [sdf (traced-sdf-capsule point a b radius)])
                    ;; Distance to a is 1, minus radius 0.5 = 0.5
                    (assert-near (traced-value sdf) 0.5 0.1)))
            
            (define-test sdf-capsule-near-middle
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 2 2) tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 4 0) tape)]
                     [radius 1]
                     [sdf (traced-sdf-capsule point a b radius)])
                    ;; Closest point on segment is (2, 0), distance is 2 - 1 = 1
                    (assert-near (traced-value sdf) 1 0.1)))
            
            (define-test sdf-line-segment-zero-radius
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 1 1) tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 2 0) tape)]
                     [sdf (traced-sdf-line-segment point a b)])
                    ;; Distance is 1 (perpendicular)
                    (assert-near (traced-value sdf) 1 0.1)))
            )

;;; ====
;;; Half-Plane SDF Tests
;;; ====

(test-group half-plane-sdf-tests
            
            (define-test sdf-half-plane-above
              ;; Ground plane at y=0, normal pointing up
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 5 3) tape)]
                     [plane-point (lift-vec2 (vec2 0 0) tape)]
                     [normal (lift-vec2 (vec2 0 1) tape)]
                     [sdf (traced-sdf-half-plane point plane-point normal)])
                    ;; Above plane, positive distance of 3
                    (assert-near (traced-value sdf) 3 0.01)))
            
            (define-test sdf-half-plane-below
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 0 -2) tape)]
                     [plane-point (lift-vec2 (vec2 0 0) tape)]
                     [normal (lift-vec2 (vec2 0 1) tape)]
                     [sdf (traced-sdf-half-plane point plane-point normal)])
                    ;; Below plane, negative distance
                    (assert-near (traced-value sdf) -2 0.01)))
            
            (define-test sdf-half-plane-on-plane
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec2 (vec2 100 0) tape)]
                     [plane-point (lift-vec2 (vec2 0 0) tape)]
                     [normal (lift-vec2 (vec2 0 1) tape)]
                     [sdf (traced-sdf-half-plane point plane-point normal)])
                    (assert-near (traced-value sdf) 0 0.01)))
            )

;;; ====
;;; Circle-Circle Contact Tests
;;; ====

(test-group circle-contact-tests
            
            (define-test circles-overlapping
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [pos-b (lift-vec2 (vec2 2 0) tape)]
                     [radius-a 1.5]
                     [radius-b 1])
                    (call-with-values
                     (lambda () (traced-circle-contact pos-a radius-a pos-b radius-b))
                     (lambda (contact normal penetration)
                             ;; Penetration = 1.5 + 1 - 2 = 0.5
                             (assert-near (traced-value penetration) 0.5 0.01)
                             ;; Normal points from A to B: (1, 0)
                             (assert-near (traced-value (traced-vec2-x normal)) 1 0.01)
                             (assert-near (traced-value (traced-vec2-y normal)) 0 0.01)))))
            
            (define-test circles-separated
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [pos-b (lift-vec2 (vec2 5 0) tape)]
                     [radius-a 1]
                     [radius-b 1])
                    (call-with-values
                     (lambda () (traced-circle-contact pos-a radius-a pos-b radius-b))
                     (lambda (contact normal penetration)
                             ;; Penetration = 2 - 5 = -3 (negative = separated)
                             (assert-near (traced-value penetration) -3 0.01)))))
            
            (define-test circles-just-touching
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [pos-b (lift-vec2 (vec2 3 0) tape)]
                     [radius-a 1.5]
                     [radius-b 1.5])
                    (call-with-values
                     (lambda () (traced-circle-contact pos-a radius-a pos-b radius-b))
                     (lambda (contact normal penetration)
                             (assert-near (traced-value penetration) 0 0.01)))))
            )

;;; ====
;;; Circle-Plane Contact Tests
;;; ====

(test-group circle-plane-tests
            
            (define-test circle-above-plane
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 5) tape)]
                     [radius 1]
                     [plane-point (lift-vec2 (vec2 0 0) tape)]
                     [plane-normal (lift-vec2 (vec2 0 1) tape)])
                    (call-with-values
                     (lambda () (traced-circle-plane-contact pos radius plane-point plane-normal))
                     (lambda (contact normal penetration)
                             ;; Penetration = 1 - 5 = -4 (not contacting)
                             (assert-near (traced-value penetration) -4 0.01)))))
            
            (define-test circle-penetrating-plane
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0.5) tape)]
                     [radius 1]
                     [plane-point (lift-vec2 (vec2 0 0) tape)]
                     [plane-normal (lift-vec2 (vec2 0 1) tape)])
                    (call-with-values
                     (lambda () (traced-circle-plane-contact pos radius plane-point plane-normal))
                     (lambda (contact normal penetration)
                             ;; Center at 0.5, radius 1, so bottom at -0.5
                             ;; Penetration = 1 - 0.5 = 0.5
                             (assert-near (traced-value penetration) 0.5 0.01)))))
            )

;;; ====
;;; Soft Contact Force Tests
;;; ====

(test-group soft-contact-force-tests
            
            (define-test soft-force-no-penetration
              (let* ([tape (make-reverse-tape)]
                     [penetration (make-traced-var -1 tape)]  ; not touching
                     [stiffness 1000]
                     [alpha 100]
                     [force (traced-soft-contact-force penetration stiffness alpha)])
                    ;; Should be very close to 0
                    (assert-near (traced-value force) 0 0.1)))
            
            (define-test soft-force-with-penetration
              (let* ([tape (make-reverse-tape)]
                     [penetration (make-traced-var 0.1 tape)]
                     [stiffness 1000]
                     [alpha 100]
                     [force (traced-soft-contact-force penetration stiffness alpha)])
                    ;; Should be approximately stiffness * penetration = 100
                    (assert-near (traced-value force) 100 20)))
            
            (define-test soft-force-increases-with-penetration
              (let* ([tape (make-reverse-tape)]
                     [pen1 (make-traced-var 0.1 tape)]
                     [pen2 (make-traced-var 0.2 tape)]
                     [stiffness 1000]
                     [alpha 100]
                     [force1 (traced-soft-contact-force pen1 stiffness alpha)]
                     [force2 (traced-soft-contact-force pen2 stiffness alpha)])
                    (assert-true (< (traced-value force1) (traced-value force2)))))
            
            (define-test soft-force-is-smooth
              ;; Verify smoothness by checking neighboring values
              (let* ([tape (make-reverse-tape)]
                     [forces (map (lambda (pen)
                                          (let ([p (make-traced-var pen tape)])
                                               (traced-value (traced-soft-contact-force p 1000 100))))
                                  (list -0.1 -0.05 0 0.05 0.1))])
                    ;; Should be monotonically increasing
                    (assert-true (apply < forces))))
            
            (define-test soft-force-damped-approaching
              (let* ([tape (make-reverse-tape)]
                     [penetration (make-traced-var 0.1 tape)]
                     [approach-vel (make-traced-var 1 tape)]  ; moving into contact
                     [stiffness 1000]
                     [damping 100]
                     [alpha 100]
                     [force (traced-soft-contact-force-damped penetration approach-vel stiffness damping alpha)]
                     [force-undamped (traced-soft-contact-force penetration stiffness alpha)])
                    ;; Damped force should be higher when approaching
                    (assert-true (> (traced-value force) (traced-value force-undamped)))))
            )

;;; ====
;;; Circle-Circle Force Tests
;;; ====

(test-group circle-circle-force-tests
            
            (define-test circles-no-contact-no-force
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [radius-a 1]
                     [pos-b (lift-vec2 (vec2 10 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [radius-b 1]
                     [stiffness 10000]
                     [damping 100]
                     [alpha 100]
                     [force (traced-circle-circle-force pos-a vel-a radius-a
                                                        pos-b vel-b radius-b
                                                        stiffness damping alpha)]
                     [fx (traced-value (traced-vec2-x force))])
                    ;; Should be very small
                    (assert-near fx 0 1)))
            
            (define-test circles-colliding-repel
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [radius-a 1.2]
                     [pos-b (lift-vec2 (vec2 2 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [radius-b 1.2]
                     [stiffness 10000]
                     [damping 100]
                     [alpha 100]
                     [force (traced-circle-circle-force pos-a vel-a radius-a
                                                        pos-b vel-b radius-b
                                                        stiffness damping alpha)]
                     [fx (traced-value (traced-vec2-x force))])
                    ;; Force on A should point away from B (negative x)
                    (assert-true (< fx 0))))
            )

;;; ====
;;; Ground Plane Force Tests
;;; ====

(test-group ground-plane-force-tests
            
            (define-test ground-force-above-ground
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 10) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [radius 1]
                     [ground-y 0]
                     [stiffness 10000]
                     [damping 100]
                     [alpha 100]
                     [force (traced-ground-plane-force pos vel radius ground-y stiffness damping alpha)]
                     [fy (traced-value (traced-vec2-y force))])
                    ;; No contact, minimal force
                    (assert-near fy 0 1)))
            
            (define-test ground-force-penetrating
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0.5) tape)]  ; Center at 0.5, radius 1 = bottom at -0.5
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [radius 1]
                     [ground-y 0]
                     [stiffness 10000]
                     [damping 100]
                     [alpha 100]
                     [force (traced-ground-plane-force pos vel radius ground-y stiffness damping alpha)]
                     [fy (traced-value (traced-vec2-y force))])
                    ;; Should push upward (positive y)
                    (assert-true (> fy 0))))
            )

;;; ====
;;; Friction Tests
;;; ====

(test-group friction-tests
            
            (define-test soft-friction-zero-velocity
              (let* ([tape (make-reverse-tape)]
                     [tangent-vel (lift-vec2 (vec2 0 0) tape)]
                     [normal-force (make-traced-var 100 tape)]
                     [mu 0.5]
                     [smoothness 10]
                     [friction (traced-soft-friction tangent-vel normal-force mu smoothness)]
                     [fx (traced-value (traced-vec2-x friction))]
                     [fy (traced-value (traced-vec2-y friction))])
                    ;; Near zero velocity, friction should be near zero
                    (assert-near fx 0 5)
                    (assert-near fy 0 5)))
            
            (define-test soft-friction-opposes-motion
              (let* ([tape (make-reverse-tape)]
                     [tangent-vel (lift-vec2 (vec2 5 0) tape)]  ; moving right
                     [normal-force (make-traced-var 100 tape)]
                     [mu 0.5]
                     [smoothness 10]
                     [friction (traced-soft-friction tangent-vel normal-force mu smoothness)]
                     [fx (traced-value (traced-vec2-x friction))])
                    ;; Friction should oppose motion (point left)
                    (assert-true (< fx 0))))
            
            (define-test contact-friction-decomposition
              (let* ([tape (make-reverse-tape)]
                     [rel-vel (lift-vec2 (vec2 3 1) tape)]  ; diagonal motion
                     [normal (lift-vec2 (vec2 0 1) tape)]   ; vertical normal
                     [normal-force (make-traced-var 50 tape)]
                     [mu 0.3]
                     [friction (traced-contact-friction-force rel-vel normal normal-force mu)]
                     [fx (traced-value (traced-vec2-x friction))])
                    ;; Tangent component is (3, 0), friction should oppose in x
                    (assert-true (< fx 0))))
            )

;;; ====
;;; Material Tests
;;; ====

(test-group material-tests
            
            (define-test make-soft-material-structure
              (let ([mat (make-soft-material 5000 200 50 0.4)])
                   (assert-equal (soft-material-stiffness mat) 5000)
                   (assert-equal (soft-material-damping mat) 200)
                   (assert-equal (soft-material-alpha mat) 50)
                   (assert-equal (soft-material-friction mat) 0.4)))
            
            (define-test default-material-has-values
              (let ([mat (default-soft-material)])
                   (assert-true (> (soft-material-stiffness mat) 0))
                   (assert-true (> (soft-material-damping mat) 0))
                   (assert-true (> (soft-material-alpha mat) 0))
                   (assert-true (> (soft-material-friction mat) 0))))
            )

;;; ====
;;; Wall Forces Tests
;;; ====

(test-group wall-forces-tests
            
            (define-test wall-forces-center-no-contact
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 5 5) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [radius 0.5]
                     [min-corner (vec2 0 0)]
                     [max-corner (vec2 10 10)]
                     [mat (make-soft-material 10000 100 100 0.5)]
                     [force (traced-wall-forces pos vel radius min-corner max-corner mat)]
                     [fx (traced-value (traced-vec2-x force))]
                     [fy (traced-value (traced-vec2-y force))])
                    ;; In center, no contact
                    (assert-near fx 0 1)
                    (assert-near fy 0 1)))
            
            (define-test wall-forces-near-left-wall
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0.3 5) tape)]  ; Close to left wall
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [radius 0.5]
                     [min-corner (vec2 0 0)]
                     [max-corner (vec2 10 10)]
                     [mat (make-soft-material 10000 100 100 0.5)]
                     [force (traced-wall-forces pos vel radius min-corner max-corner mat)]
                     [fx (traced-value (traced-vec2-x force))])
                    ;; Should be pushed right (positive x)
                    (assert-true (> fx 0))))
            )

;;; ====
;;; Gradient Correctness Tests
;;; ====

(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

(test-group gradient-tests
            
            (define-test sdf-circle-gradient-correct
              (let* ([test-fn (lambda (px)
                                      (let* ([tape (make-reverse-tape)]
                                             [point (traced-vec2 (make-traced-var px tape)
                                                                 (make-traced-var 0 tape))]
                                             [center (lift-vec2-const (vec2 0 0))]
                                             [sdf (traced-sdf-circle point center 1)])
                                            (traced-value sdf)))]
                     [x0 3]
                     [numerical (finite-diff test-fn x0 1e-5)]
                     [grads (gradient (lambda (px py)
                                              (traced-sdf-circle (traced-vec2 px py)
                                                                 (lift-vec2-const (vec2 0 0))
                                                                 1))
                                      (list 3 0))]
                     [analytical (car grads)])
                    ;; At (3, 0), d/dx of distance is 1
                    (assert-near analytical numerical 0.001)))
            
            (define-test soft-contact-force-gradient-correct
              (let* ([test-fn (lambda (pen)
                                      (let* ([tape (make-reverse-tape)]
                                             [p (make-traced-var pen tape)]
                                             [f (traced-soft-contact-force p 1000 100)])
                                            (traced-value f)))]
                     [x0 0.1]
                     [numerical (finite-diff test-fn x0 1e-5)]
                     [grads (gradient (lambda (pen)
                                              (traced-soft-contact-force pen 1000 100))
                                      (list 0.1))]
                     [analytical (car grads)])
                    (assert-near analytical numerical 10)))
            )

;;; ====
;;; Run All Tests
;;; ====

(newline)
(display "----")
(newline)
(display "  Test Summary:")
(newline)
(display "----")
(newline)
(display "  Total:  ")
(display *tests-run*)
(display " tests")
(newline)
(display "  Passed: ")
(display *tests-passed*)
(newline)
(display "  Failed: ")
(display *tests-failed*)
(newline)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
