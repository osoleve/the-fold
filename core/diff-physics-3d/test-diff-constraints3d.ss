;;; core/diff-physics-3d/test-diff-constraints3d.ss --- Tests for diff-constraints3d
;;;
;;; Run with: scheme --script core/diff-physics-3d/test-diff-constraints3d.ss

(load "core/test-framework.ss")
(load "core/diff-physics-3d/diff-constraints3d.ss")

(display "\n")
(display "==============================================================\n")
(display "         DIFF CONSTRAINTS 3D TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define *test-dt* 0.01)

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
;;; Spring Constraint Tests
;;; ============================================================

(test-group spring-constraint-tests
            
            (define-test spring-force-stretched
              ;; Spring stretched beyond rest length should pull bodies together
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec3 (vec3 0 0 0) tape)]
                     [b (lift-vec3 (vec3 3 0 0) tape)]
                     [force (traced-spring-force-3d a b 2.0 100.0)])
                    ;; Force on a should point toward b (+x direction)
                    (assert-true (> (traced-value (traced-vec3-x force)) 0))))
            
            (define-test spring-force-compressed
              ;; Spring compressed below rest length should push bodies apart
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec3 (vec3 0 0 0) tape)]
                     [b (lift-vec3 (vec3 1 0 0) tape)]
                     [force (traced-spring-force-3d a b 2.0 100.0)])
                    ;; Force on a should point away from b (-x direction)
                    (assert-true (< (traced-value (traced-vec3-x force)) 0))))
            
            (define-test spring-force-at-rest
              ;; Spring at rest length should have ~zero force
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec3 (vec3 0 0 0) tape)]
                     [b (lift-vec3 (vec3 2 0 0) tape)]
                     [force (traced-spring-force-3d a b 2.0 100.0)]
                     [mag (traced-vec3-magnitude force)])
                    (assert-true (< (traced-value mag) 0.01))))
            
            (define-test spring-damping-opposes-velocity
              ;; Damping should oppose relative velocity
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec3 (vec3 0 0 0) tape)]
                     [b (lift-vec3 (vec3 2 0 0) tape)]
                     [va (lift-vec3 (vec3 1 0 0) tape)]  ; a moving toward b
                     [vb (lift-vec3 (vec3 0 0 0) tape)]
                     [force (traced-spring-force-damped-3d a b va vb 2.0 0.0 100.0)])
                    ;; Damping should resist a's motion (negative x force)
                    (assert-true (< (traced-value (traced-vec3-x force)) 0)))))

;;; ============================================================
;;; Anchor Constraint Tests
;;; ============================================================

(test-group anchor-constraint-tests
            
            (define-test anchor-force-pulls-to-target
              ;; Body displaced from target should be pulled back
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 1 0 0) (vec3 0 0 0))]
                     [local-anchor (vec3 0 0 0)]
                     [world-target (vec3 0 0 0)]
                     [stiffness 10000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-anchor-force-3d body local-anchor world-target stiffness damping))
                     (lambda (force torque)
                             ;; Force should pull in -x direction (toward target)
                             (assert-true (< (traced-value (traced-vec3-x force)) 0))))))
            
            (define-test anchor-force-at-target-zero
              ;; Body at target should have ~zero force
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [local-anchor (vec3 0 0 0)]
                     [world-target (vec3 0 0 0)]
                     [stiffness 10000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-anchor-force-3d body local-anchor world-target stiffness damping))
                     (lambda (force torque)
                             ;; Use smooth magnitude to handle near-zero vectors
                             (let ([force-mag (traced-vec3-smooth-magnitude force 1e-10)])
                                  (assert-true (< (traced-value force-mag) 0.01))))))))

;;; ============================================================
;;; Ball-Socket Joint Tests
;;; ============================================================

(test-group ball-socket-tests
            
            (define-test ball-socket-pulls-anchors-together
              ;; Separated anchors should be pulled together
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 2 0 0) (vec3 0 0 0))]
                     [anchor-a (vec3 0.5 0 0)]  ; Right side of A
                     [anchor-b (vec3 -0.5 0 0)] ; Left side of B
                     ;; World positions: A anchor at (0.5,0,0), B anchor at (1.5,0,0)
                     [stiffness 10000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-ball-socket-forces-3d body-a anchor-a body-b anchor-b stiffness damping))
                     (lambda (result-a result-b)
                             (let ([force-a (car result-a)]
                                   [force-b (car result-b)])
                                  ;; Force on A should point toward B's anchor (+x)
                                  (assert-true (> (traced-value (traced-vec3-x force-a)) 0))
                                  ;; Force on B should be opposite (-x)
                                  (assert-true (< (traced-value (traced-vec3-x force-b)) 0)))))))
            
            (define-test ball-socket-forces-opposite
              ;; Forces should be equal and opposite (Newton's third law)
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 2 0 0) (vec3 0 0 0))]
                     [anchor-a (vec3 0 0 0)]
                     [anchor-b (vec3 0 0 0)]
                     [stiffness 10000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-ball-socket-forces-3d body-a anchor-a body-b anchor-b stiffness damping))
                     (lambda (result-a result-b)
                             (let* ([force-a (car result-a)]
                                    [force-b (car result-b)]
                                    [sum (traced-vec3-add force-a force-b)]
                                    [sum-mag (traced-vec3-magnitude sum)])
                                   ;; Sum should be ~zero
                                   (assert-true (< (traced-value sum-mag) 0.01))))))))

;;; ============================================================
;;; Hinge Joint Tests
;;; ============================================================

(test-group hinge-joint-tests
            
            (define-test hinge-maintains-position
              ;; Hinge should pull anchors together like ball-socket
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 2 0 0) (vec3 0 0 0))]
                     [anchor-a (vec3 0.5 0 0)]
                     [anchor-b (vec3 -0.5 0 0)]
                     [axis-a (vec3 0 0 1)]   ; Z-axis hinge
                     [axis-b (vec3 0 0 1)]
                     [stiffness 10000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-hinge-forces-3d body-a anchor-a axis-a body-b anchor-b axis-b stiffness damping))
                     (lambda (result-a result-b)
                             (let ([force-a (car result-a)])
                                  ;; Position force should pull A toward B
                                  (assert-true (> (traced-value (traced-vec3-x force-a)) 0))))))))

;;; ============================================================
;;; Fixed Joint Tests
;;; ============================================================

(test-group fixed-joint-tests
            
            (define-test fixed-joint-maintains-position
              ;; Fixed joint should maintain position
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 2 0 0) (vec3 0 0 0))]
                     [anchor-a (vec3 0.5 0 0)]
                     [anchor-b (vec3 -0.5 0 0)]
                     [ref-orient (quat-identity)]
                     [stiffness 10000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-fixed-forces-3d body-a anchor-a body-b anchor-b ref-orient stiffness damping))
                     (lambda (result-a result-b)
                             (let ([force-a (car result-a)])
                                  ;; Position force should pull A toward B
                                  (assert-true (> (traced-value (traced-vec3-x force-a)) 0)))))))
            
            (define-test fixed-joint-at-rest-minimal-force
              ;; Fixed joint at rest configuration should have minimal force
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 1 0 0) (vec3 0 0 0))]
                     [anchor-a (vec3 0.5 0 0)]  ; World: (0.5, 0, 0)
                     [anchor-b (vec3 -0.5 0 0)] ; World: (0.5, 0, 0) - same point
                     [ref-orient (quat-identity)]
                     [stiffness 10000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-fixed-forces-3d body-a anchor-a body-b anchor-b ref-orient stiffness damping))
                     (lambda (result-a result-b)
                             (let* ([force-a (car result-a)]
                                    [force-mag (traced-vec3-magnitude force-a)])
                                   ;; Position force should be ~zero when anchors coincide
                                   (assert-true (< (traced-value force-mag) 1.0))))))))

;;; ============================================================
;;; Constraint Application Tests
;;; ============================================================

(test-group constraint-application-tests
            
            (define-test apply-spring-constraint-modifies-velocities
              ;; Applying spring constraint should change body velocities
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 3 0 0) (vec3 0 0 0))]
                     [bodies (vector body-a body-b)]
                     [spring (make-spring-constraint-3d 0 (vec3 0 0 0) 1 (vec3 0 0 0) 2.0 10000 100)]
                     [new-bodies (traced-apply-spring-constraint-3d bodies spring 0.01)])
                    ;; Body A should now have positive x velocity (pulled toward B)
                    (let ([new-a (vector-ref new-bodies 0)])
                         (assert-true (> (traced-value (traced-vec3-x (traced-body-3d-vel new-a))) 0)))))
            
            (define-test apply-ball-socket-separates-bodies
              ;; Ball-socket on overlapping bodies should push apart
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 3 0 0) (vec3 0 0 0))]
                     [bodies (vector body-a body-b)]
                     ;; Anchors at (1,0,0) and (2,0,0) - want to pull together
                     [joint (make-ball-socket-joint-3d 0 (vec3 1 0 0) 1 (vec3 -1 0 0) 10000 100)]
                     [new-bodies (traced-apply-ball-socket-joint-3d bodies joint 0.01)])
                    ;; Body A should be pulled toward B (+x velocity)
                    (let ([new-a (vector-ref new-bodies 0)])
                         (assert-true (> (traced-value (traced-vec3-x (traced-body-3d-vel new-a))) 0))))))

;;; ============================================================
;;; Complete Constraint Step Tests
;;; ============================================================

(test-group constraint-step-tests
            
            (define-test constraint-step-with-gravity
              ;; Bodies should fall under gravity
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 5 0) (vec3 0 0 0))]
                     [bodies (vector body-a)]
                     [gravity (vec3 0 -9.8 0)]
                     [new-bodies (traced-constraint-step-3d bodies '() gravity 0.01 1)])
                    ;; Body should have negative y velocity
                    (let ([new-a (vector-ref new-bodies 0)])
                         (assert-true (< (traced-value (traced-vec3-y (traced-body-3d-vel new-a))) 0)))))
            
            (define-test constraint-step-multiple-iterations
              ;; Multiple iterations with low damping should give larger velocity magnitude
              ;; (High damping can counteract spring forces after first iteration)
              (let* ([tape (make-reverse-tape)]
                     [body-a (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [body-b (make-test-body-3d tape (vec3 3 0 0) (vec3 0 0 0))]
                     [bodies (vector body-a body-b)]
                     ;; Use low damping to allow velocity accumulation
                     [spring (make-spring-constraint-3d 0 (vec3 0 0 0) 1 (vec3 0 0 0) 2.0 10000 1)]
                     [new-bodies-1 (traced-constraint-step-3d bodies (list spring) (vec3 0 0 0) 0.01 1)]
                     [new-bodies-3 (traced-constraint-step-3d bodies (list spring) (vec3 0 0 0) 0.01 3)])
                    ;; 3 iterations should give larger absolute velocity than 1
                    (let* ([vel-1 (traced-body-3d-vel (vector-ref new-bodies-1 0))]
                           [vel-3 (traced-body-3d-vel (vector-ref new-bodies-3 0))]
                           [mag-1 (traced-value (traced-vec3-smooth-magnitude vel-1 1e-10))]
                           [mag-3 (traced-value (traced-vec3-smooth-magnitude vel-3 1e-10))])
                          (assert-true (> mag-3 mag-1))))))

;;; ============================================================
;;; Pendulum Helper Tests
;;; ============================================================

(test-group pendulum-helper-tests
            
            (define-test make-pendulum-creates-body-and-constraint
              (let ([tape (make-reverse-tape)])
                   (call-with-values
                    (lambda () (make-pendulum-system-3d (vec3 0 5 0) 2.0 1.0 0.5 tape))
                    (lambda (bob constraint)
                            ;; Bob should exist
                            (assert-true (traced-body-3d? bob))
                            ;; Constraint should be anchor type
                            (assert-equal 'anchor-constraint-3d (constraint-type-3d constraint))))))
            
            (define-test make-chain-creates-multiple-bodies
              (let ([tape (make-reverse-tape)])
                   (call-with-values
                    (lambda () (make-chain-system-3d (vec3 0 10 0) 5 1.0 1.0 0.3 tape))
                    (lambda (bodies constraints)
                            ;; Should have 5 bodies
                            (assert-equal 5 (vector-length bodies))
                            ;; Should have 5 constraints (1 anchor + 4 ball-socket)
                            (assert-equal 5 (length constraints)))))))

;;; ============================================================
;;; Run all tests
;;; ============================================================

(run-all-tests)

