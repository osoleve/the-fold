;;; core/diff-physics-3d/test-smooth-collision3d.ss --- Tests for smooth-collision3d
;;;
;;; Run with: scheme --script core/diff-physics-3d/test-smooth-collision3d.ss

(load "core/test-framework.ss")
(load "core/diff-physics-3d/smooth-collision3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         SMOOTH COLLISION 3D TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; SDF Tests
;;; ============================================================

(test-group sdf-tests
            
            (define-test sphere-sdf-outside
              ;; Point (2, 0, 0), sphere at origin with radius 1
              ;; Distance should be 2 - 1 = 1 (positive, outside)
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec3 (vec3 2 0 0) tape)]
                     [center (lift-vec3 (vec3 0 0 0) tape)]
                     [sdf (traced-sdf-sphere point center 1.0)])
                    (assert-true (< (abs (- (traced-value sdf) 1.0)) 1e-6))))
            
            (define-test sphere-sdf-inside
              ;; Point (0.5, 0, 0), sphere at origin with radius 1
              ;; Distance should be 0.5 - 1 = -0.5 (negative, inside)
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec3 (vec3 0.5 0 0) tape)]
                     [center (lift-vec3 (vec3 0 0 0) tape)]
                     [sdf (traced-sdf-sphere point center 1.0)])
                    (assert-true (< (abs (- (traced-value sdf) -0.5)) 1e-6))))
            
            (define-test sphere-sdf-on-surface
              ;; Point (1, 0, 0), sphere at origin with radius 1
              ;; Distance should be 0 (on surface)
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec3 (vec3 1 0 0) tape)]
                     [center (lift-vec3 (vec3 0 0 0) tape)]
                     [sdf (traced-sdf-sphere point center 1.0)])
                    (assert-true (< (abs (traced-value sdf)) 1e-6))))
            
            (define-test box-sdf-outside
              ;; Point (2, 0, 0), box at origin with half-extents (1, 1, 1)
              ;; Distance should be ~1 (outside, closest to face)
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec3 (vec3 2 0 0) tape)]
                     [center (lift-vec3 (vec3 0 0 0) tape)]
                     [sdf (traced-sdf-box-3d point center (vec3 1 1 1))])
                    (assert-true (> (traced-value sdf) 0.5))))
            
            (define-test box-sdf-inside
              ;; Point (0, 0, 0), box at origin with half-extents (1, 1, 1)
              ;; Distance should be negative (inside)
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec3 (vec3 0 0 0) tape)]
                     [center (lift-vec3 (vec3 0 0 0) tape)]
                     [sdf (traced-sdf-box-3d point center (vec3 1 1 1))])
                    (assert-true (< (traced-value sdf) 0))))
            
            (define-test half-space-sdf
              ;; Point (0, 2, 0), plane at y=0 with normal (0, 1, 0)
              ;; Distance should be 2
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec3 (vec3 0 2 0) tape)]
                     [plane-pt (lift-vec3 (vec3 0 0 0) tape)]
                     [normal (lift-vec3 (vec3 0 1 0) tape)]
                     [sdf (traced-sdf-half-space point plane-pt normal)])
                    (assert-true (< (abs (- (traced-value sdf) 2.0)) 1e-6))))
            
            (define-test capsule-sdf-at-endpoint
              ;; Point (0, 0, 2), capsule from (0,0,0) to (0,0,1) with radius 0.5
              ;; Distance to endpoint (0,0,1) is 1, so sdf = 1 - 0.5 = 0.5
              (let* ([tape (make-reverse-tape)]
                     [point (lift-vec3 (vec3 0 0 2) tape)]
                     [a (lift-vec3 (vec3 0 0 0) tape)]
                     [b (lift-vec3 (vec3 0 0 1) tape)]
                     [sdf (traced-sdf-capsule-3d point a b 0.5)])
                    (assert-true (< (abs (- (traced-value sdf) 0.5)) 1e-5)))))

;;; ============================================================
;;; Contact Geometry Tests
;;; ============================================================

(test-group contact-tests
            
            (define-test sphere-sphere-contact-separated
              ;; Two spheres at (0,0,0) and (3,0,0) with radius 1
              ;; Not touching, penetration should be negative
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec3 (vec3 0 0 0) tape)]
                     [pos-b (lift-vec3 (vec3 3 0 0) tape)])
                    (call-with-values
                     (lambda () (traced-sphere-contact pos-a 1.0 pos-b 1.0))
                     (lambda (contact normal penetration)
                             ;; Penetration = (1 + 1) - 3 = -1 (negative, not touching)
                             (assert-true (< (traced-value penetration) 0))))))
            
            (define-test sphere-sphere-contact-overlapping
              ;; Two spheres at (0,0,0) and (1.5,0,0) with radius 1
              ;; Touching, penetration should be positive
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec3 (vec3 0 0 0) tape)]
                     [pos-b (lift-vec3 (vec3 1.5 0 0) tape)])
                    (call-with-values
                     (lambda () (traced-sphere-contact pos-a 1.0 pos-b 1.0))
                     (lambda (contact normal penetration)
                             ;; Penetration = (1 + 1) - 1.5 = 0.5
                             (assert-true (< (abs (- (traced-value penetration) 0.5)) 1e-6))
                             ;; Normal should point from A to B: (1, 0, 0)
                             (assert-true (< (abs (- (traced-value (traced-vec3-x normal)) 1.0)) 1e-6))
                             (assert-true (< (abs (traced-value (traced-vec3-y normal))) 1e-6))))))
            
            (define-test sphere-plane-contact
              ;; Sphere at (0, 0.5, 0) with radius 1, plane at y=0 with normal (0,1,0)
              ;; Penetration = 1 - 0.5 = 0.5
              (let* ([tape (make-reverse-tape)]
                     [sphere-pos (lift-vec3 (vec3 0 0.5 0) tape)]
                     [plane-pt (lift-vec3 (vec3 0 0 0) tape)]
                     [plane-normal (lift-vec3 (vec3 0 1 0) tape)])
                    (call-with-values
                     (lambda () (traced-sphere-plane-contact sphere-pos 1.0 plane-pt plane-normal))
                     (lambda (contact normal penetration)
                             (assert-true (< (abs (- (traced-value penetration) 0.5)) 1e-6)))))))

;;; ============================================================
;;; Force Tests
;;; ============================================================

(test-group force-tests
            
            (define-test soft-contact-force-no-penetration
              ;; No penetration (negative) should give ~zero force
              (let* ([tape (make-reverse-tape)]
                     [penetration (make-traced-var -1.0 tape)]
                     [force (traced-soft-contact-force penetration 10000 100)])
                    ;; softplus(-1, 100) ≈ 0
                    (assert-true (< (traced-value force) 1.0))))
            
            (define-test soft-contact-force-with-penetration
              ;; Positive penetration should give positive force
              (let* ([tape (make-reverse-tape)]
                     [penetration (make-traced-var 0.1 tape)]
                     [force (traced-soft-contact-force penetration 10000 100)])
                    ;; Force should be roughly stiffness * penetration = 1000
                    (assert-true (> (traced-value force) 500))))
            
            (define-test sphere-sphere-force-separated
              ;; Two separated spheres should have ~zero force
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec3 (vec3 0 0 0) tape)]
                     [vel-a (lift-vec3 (vec3 0 0 0) tape)]
                     [pos-b (lift-vec3 (vec3 5 0 0) tape)]
                     [vel-b (lift-vec3 (vec3 0 0 0) tape)]
                     [force (traced-sphere-sphere-force pos-a vel-a 1.0 pos-b vel-b 1.0 10000 100 100)]
                     [force-mag (traced-vec3-magnitude force)])
                    (assert-true (< (traced-value force-mag) 1.0))))
            
            (define-test sphere-sphere-force-overlapping
              ;; Two overlapping spheres should have repulsive force
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec3 (vec3 0 0 0) tape)]
                     [vel-a (lift-vec3 (vec3 0 0 0) tape)]
                     [pos-b (lift-vec3 (vec3 1.5 0 0) tape)]
                     [vel-b (lift-vec3 (vec3 0 0 0) tape)]
                     [force (traced-sphere-sphere-force pos-a vel-a 1.0 pos-b vel-b 1.0 10000 100 100)])
                    ;; Force on A should point in -x direction (away from B)
                    (assert-true (< (traced-value (traced-vec3-x force)) 0))))
            
            (define-test ground-plane-force
              ;; Sphere at (0, 0.5, 0) with radius 1, ground at y=0
              ;; Should have upward force
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0.5 0) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [force (traced-ground-plane-force-3d pos vel 1.0 0.0 10000 100 100)])
                    ;; Force should point up (+y)
                    (assert-true (> (traced-value (traced-vec3-y force)) 0)))))

;;; ============================================================
;;; Material Tests
;;; ============================================================

(test-group material-tests
            
            (define-test default-material
              (let ([mat (default-soft-material-3d)])
                   (assert-equal *default-contact-stiffness-3d* (soft-material-3d-stiffness mat))
                   (assert-equal *default-contact-damping-3d* (soft-material-3d-damping mat))
                   (assert-equal *default-contact-alpha-3d* (soft-material-3d-alpha mat))
                   (assert-equal *default-friction-mu-3d* (soft-material-3d-friction mat))))
            
            (define-test custom-material
              (let ([mat (make-soft-material-3d 5000 50 50 0.3)])
                   (assert-equal 5000 (soft-material-3d-stiffness mat))
                   (assert-equal 50 (soft-material-3d-damping mat))
                   (assert-equal 50 (soft-material-3d-alpha mat))
                   (assert-equal 0.3 (soft-material-3d-friction mat)))))

;;; ============================================================
;;; Friction Tests
;;; ============================================================

(test-group friction-tests
            
            (define-test friction-opposes-motion
              ;; Moving tangent to contact should produce opposing friction
              (let* ([tape (make-reverse-tape)]
                     [tangent-vel (lift-vec3 (vec3 1 0 0) tape)]  ; moving in +x
                     [normal-force (make-traced-var 100 tape)]    ; 100N normal force
                     [friction (traced-soft-friction-3d tangent-vel normal-force 0.5 10)])
                    ;; Friction should point in -x direction
                    (assert-true (< (traced-value (traced-vec3-x friction)) 0))))
            
            (define-test friction-magnitude
              ;; Friction magnitude should be bounded by μ * Fn
              (let* ([tape (make-reverse-tape)]
                     [tangent-vel (lift-vec3 (vec3 10 0 0) tape)]  ; fast motion
                     [normal-force (make-traced-var 100 tape)]
                     [mu 0.5]
                     [friction (traced-soft-friction-3d tangent-vel normal-force mu 10)]
                     [friction-mag (traced-vec3-magnitude friction)])
                    ;; Friction magnitude should approach μ * Fn = 50
                    (assert-true (< (traced-value friction-mag) 60)))))

;;; ============================================================
;;; Wall Boundary Tests
;;; ============================================================

(test-group wall-tests
            
            (define-test wall-forces-inside-boundary
              ;; Sphere well inside boundary should have ~zero wall forces
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [mat (default-soft-material-3d)]
                     [force (traced-wall-forces-3d pos vel 0.1
                                                   (vec3 -10 -10 -10)
                                                   (vec3 10 10 10)
                                                   mat)]
                     [force-mag (traced-vec3-magnitude force)])
                    (assert-true (< (traced-value force-mag) 1.0))))
            
            (define-test wall-forces-near-boundary
              ;; Sphere near -X wall should have +X force
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 -9.5 0 0) tape)]  ; near x = -10 wall
                     [vel (lift-vec3 (vec3 0 0 0) tape)]
                     [mat (default-soft-material-3d)]
                     [force (traced-wall-forces-3d pos vel 1.0
                                                   (vec3 -10 -10 -10)
                                                   (vec3 10 10 10)
                                                   mat)])
                    ;; Force should push in +X direction
                    (assert-true (> (traced-value (traced-vec3-x force)) 0)))))

;;; ============================================================
;;; Gradient Verification Tests
;;; ============================================================

(test-group gradient-tests
            
            (define-test sphere-sdf-gradient
              ;; Check that sphere SDF gradient points radially outward
              (let* ([center (vec3 0 0 0)]
                     [sdf-fn (lambda (pt) (traced-sdf-sphere pt (lift-vec3-const center) 1.0))]
                     [test-point (vec3 2 0 0)]
                     [normal (traced-sdf-normal-3d sdf-fn test-point)])
                    ;; Normal at (2,0,0) should be (1,0,0)
                    (assert-true (< (abs (- (vec3-x normal) 1.0)) 1e-4))
                    (assert-true (< (abs (vec3-y normal)) 1e-4))
                    (assert-true (< (abs (vec3-z normal)) 1e-4))))
            
            (define-test half-space-sdf-gradient
              ;; Half-space gradient should equal normal
              (let* ([sdf-fn (lambda (pt)
                                     (traced-sdf-half-space pt
                                                            (lift-vec3-const (vec3 0 0 0))
                                                            (lift-vec3-const (vec3 0 1 0))))]
                     [test-point (vec3 5 3 7)]
                     [normal (traced-sdf-normal-3d sdf-fn test-point)])
                    ;; Normal should be (0, 1, 0)
                    (assert-true (< (abs (vec3-x normal)) 1e-4))
                    (assert-true (< (abs (- (vec3-y normal) 1.0)) 1e-4))
                    (assert-true (< (abs (vec3-z normal)) 1e-4)))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)
