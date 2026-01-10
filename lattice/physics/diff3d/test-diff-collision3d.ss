;;; core/diff-physics-3d/test-diff-collision3d.ss --- Tests for diff-collision3d
;;;
;;; Run with: scheme --script core/diff-physics-3d/test-diff-collision3d.ss

(load "core/test-framework.ss")
(load "lattice/physics/diff3d/diff-collision3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         DIFF COLLISION 3D TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define *test-dt* 0.01)
(define *test-mat* (default-soft-material-3d))

;;; Make a simple traced body for testing
(define (make-test-body-3d tape pos vel)
  (make-traced-body-3d
   (lift-vec3 pos tape)
   (lift-vec3 vel tape)
   (lift-quat (quat-identity) tape)
   (lift-vec3 (vec3 0 0 0) tape)
   1.0   ; mass
   (inertia-solid-sphere 1.0 1.0)))  ; inertia for unit sphere

;;; ============================================================
;;; Sphere-Sphere Collision Force Tests
;;; ============================================================

(test-group sphere-collision-force-tests
            
            (define-test spheres-separated-zero-force
              ;; Two spheres far apart should have ~zero force
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 10 0 0) (vec3 0 0 0))])
                    (call-with-values
                     (lambda () (traced-sphere-collision-force body-a 1.0 body-b 1.0 *test-mat*))
                     (lambda (force torque)
                             (let ([force-mag (traced-value (traced-vec3-magnitude force))])
                                  (assert-true (< force-mag 1.0)))))))
            
            (define-test spheres-overlapping-repulsive-force
              ;; Two overlapping spheres should have repulsive force
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 1.5 0 0) (vec3 0 0 0))])
                    (call-with-values
                     (lambda () (traced-sphere-collision-force body-a 1.0 body-b 1.0 *test-mat*))
                     (lambda (force torque)
                             ;; Force on A should point away from B (in -x direction)
                             (assert-true (< (traced-value (traced-vec3-x force)) 0))))))
            
            (define-test spheres-approaching-damped
              ;; Two spheres approaching should have additional damping force
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 1 0 0))]  ; moving toward B
                     [body-b (make-test-body-3d tape (vec3 1.5 0 0) (vec3 -1 0 0))]  ; moving toward A
                     [mat *test-mat*])
                    (call-with-values
                     (lambda () (traced-sphere-collision-force body-a 1.0 body-b 1.0 mat))
                     (lambda (force torque)
                             ;; Force should be larger due to damping (approach velocity)
                             (let ([force-mag (traced-value (traced-vec3-magnitude force))])
                                  (assert-true (> force-mag 100))))))))

;;; ============================================================
;;; Ground Collision Tests
;;; ============================================================

(test-group ground-collision-tests
            
            (define-test sphere-above-ground-zero-force
              ;; Sphere above ground should have ~zero force
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 5 0) (vec3 0 0 0))])
                    (call-with-values
                     (lambda () (traced-ground-collision-force-3d body 1.0 0.0 *test-mat*))
                     (lambda (force torque)
                             (let ([force-mag (traced-value (traced-vec3-magnitude force))])
                                  (assert-true (< force-mag 1.0)))))))
            
            (define-test sphere-penetrating-ground-upward-force
              ;; Sphere penetrating ground should have upward force
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 0.5 0) (vec3 0 0 0))])  ; radius 1, penetrating
                    (call-with-values
                     (lambda () (traced-ground-collision-force-3d body 1.0 0.0 *test-mat*))
                     (lambda (force torque)
                             ;; Force should point up (+y)
                             (assert-true (> (traced-value (traced-vec3-y force)) 0))))))
            
            (define-test sphere-sliding-friction
              ;; Sphere sliding on ground should have friction opposing motion
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 0.8 0) (vec3 5 0 0))])  ; sliding in +x
                    (call-with-values
                     (lambda () (traced-ground-collision-force-3d body 1.0 0.0 *test-mat*))
                     (lambda (force torque)
                             ;; Friction should oppose motion (negative x component)
                             (assert-true (< (traced-value (traced-vec3-x force)) 0)))))))

;;; ============================================================
;;; Collision Step Tests
;;; ============================================================

(test-group collision-step-tests
            
            (define-test ground-collision-step-bounces
              ;; Sphere falling onto ground should bounce (velocity reverses sign)
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 0.9 0) (vec3 0 -10 0))]  ; falling
                     [body2 (traced-ground-collision-step-3d body 1.0 0.0 *test-mat* 0.001)])
                    ;; After step, vertical velocity should be less negative (or positive)
                    (let ([vy1 (traced-value (traced-vec3-y (traced-body-3d-vel body)))]
                          [vy2 (traced-value (traced-vec3-y (traced-body-3d-vel body2)))])
                         (assert-true (> vy2 vy1)))))  ; velocity increased (less negative or positive)
            
            (define-test sphere-collision-step-separates
              ;; Two overlapping spheres should move apart after collision step
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 1.5 0 0) (vec3 0 0 0))])
                    (call-with-values
                     (lambda () (traced-sphere-collision-impulses body-a 1.0 body-b 1.0 *test-mat* 0.001))
                     (lambda (new-a new-b)
                             ;; A should be moving in -x direction
                             (assert-true (< (traced-value (traced-vec3-x (traced-body-3d-vel new-a))) 0))
                             ;; B should be moving in +x direction
                             (assert-true (> (traced-value (traced-vec3-x (traced-body-3d-vel new-b))) 0)))))))

;;; ============================================================
;;; Multi-Body System Tests
;;; ============================================================

(test-group multi-body-tests
            
            (define-test collision-pair-structure
              (let ([pair (make-collision-pair-3d 0 1 1.0 2.0)])
                   (assert-equal 0 (collision-pair-3d-idx-a pair))
                   (assert-equal 1 (collision-pair-3d-idx-b pair))
                   (assert-equal 1.0 (collision-pair-3d-radius-a pair))
                   (assert-equal 2.0 (collision-pair-3d-radius-b pair))))
            
            (define-test accumulate-forces-opposite
              ;; Forces on colliding pair should be equal and opposite
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 1.5 0 0) (vec3 0 0 0))]
                     [bodies (vector body-a body-b)]
                     [pairs (list (make-collision-pair-3d 0 1 1.0 1.0))])
                    (call-with-values
                     (lambda () (traced-accumulate-collision-forces-3d bodies pairs *test-mat*))
                     (lambda (forces torques)
                             (let ([fa (vector-ref forces 0)]
                                   [fb (vector-ref forces 1)])
                                  ;; Forces should be opposite
                                  (let ([sum-x (+ (traced-value (traced-vec3-x fa))
                                                  (traced-value (traced-vec3-x fb)))])
                                       (assert-true (< (abs sum-x) 1e-6)))))))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(test-group utility-tests
            
            (define-test spheres-overlapping-check
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec3 (vec3 0 0 0) tape)]
                     [pos-b (lift-vec3 (vec3 1.5 0 0) tape)])
                    (assert-true (traced-spheres-overlapping? pos-a 1.0 pos-b 1.0))))
            
            (define-test spheres-separated-check
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec3 (vec3 0 0 0) tape)]
                     [pos-b (lift-vec3 (vec3 5 0 0) tape)])
                    (assert-false (traced-spheres-overlapping? pos-a 1.0 pos-b 1.0))))
            
            (define-test sphere-ground-overlapping-check
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0.5 0) tape)])
                    (assert-true (traced-sphere-ground-overlapping? pos 1.0 0.0))))
            
            (define-test effective-restitution-underdamped
              ;; Underdamped system should have positive restitution
              (let ([e (effective-restitution-3d 10000 100 1.0 1.0)])
                   (assert-true (> e 0))
                   (assert-true (< e 1))))
            
            (define-test effective-restitution-overdamped
              ;; Overdamped system should have zero restitution
              (let ([e (effective-restitution-3d 100 1000 1.0 1.0)])
                   (assert-equal 0 e))))

;;; ============================================================
;;; Physics Step Integration Tests
;;; ============================================================

(test-group physics-step-tests
            
            (define-test apply-gravity-downward
              ;; Gravity should accelerate body downward
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 5 0) (vec3 0 0 0))]
                     [body2 (traced-apply-gravity-3d body (vec3 0 -9.8 0) 0.1)])
                    ;; Velocity should be negative after gravity
                    (assert-true (< (traced-value (traced-vec3-y (traced-body-3d-vel body2))) 0))))
            
            (define-test complete-physics-step
              ;; Test full physics step with gravity and ground
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 0.8 0) (vec3 0 -5 0))]  ; falling, near ground
                     [body2 (traced-physics-step-3d body 1.0 '() 0.0 (vec3 0 -9.8 0) #f #f *test-mat* 0.01)])
                    ;; Body should still exist and have valid state
                    (assert-true (traced-body-3d? body2))
                    ;; Ground should provide upward force, limiting downward velocity
                    (let ([vy (traced-value (traced-vec3-y (traced-body-3d-vel body2)))])
                         ;; Velocity should be less negative than pure gravity would cause
                         (assert-true (> vy -6))))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)
