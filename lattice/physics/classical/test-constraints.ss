;;; user/physics/test-constraints.ss — Constraint System Tests
;;;
;;; Tests for the 2D physics constraint system:
;;;   - Distance constraint
;;;   - Revolute joint
;;;   - Spring constraint
;;;   - Weld joint
;;;
;;; Run with: scheme --script user/physics/test-constraints.ss

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/physics/classical/world.ss")

;;; ====
;;; Test Helpers
;;; ====

;;; default-material for testing
(define test-material (make-material 0.5 0.3 0.3))

;;; Run simulation for n steps (using world's fixed dt)
(define (run-steps world n dt)
  (let ([fixed-dt (config-fixed-dt (world-config world))])
       (let loop ([i 0])
            (when (< i n)
                  ;; Use fixed-dt to ensure steps actually happen
                  (world-step! world fixed-dt)
                  (loop (+ i 1))))))

;;; Get entity position
(define (entity-position world id)
  (let ([entity (world-get-entity world id)])
       (if entity
           (let ([body (entity-body entity)])
                (if (rigid-body? body)
                    (rigid-body-pos body)
                    (body-pos body)))
           (vec2 0 0))))

;;; Distance between two entities
(define (entity-distance world id-a id-b)
  (vec2-magnitude (vec2-sub (entity-position world id-a)
                            (entity-position world id-b))))

;;; ====
;;; Distance Constraint Tests
;;; ====

(test-group distance-constraint-tests
            
            ;; Test: Distance maintained after stepping (no external forces)
            (define-test distance-maintained-no-forces
              (let* ([w (make-world (vec2 0 0))]  ; No gravity
                     [e1 (make-circle-entity 'a (vec2 0 0) 1 1 test-material)]
                     [e2 (make-circle-entity 'b (vec2 10 0) 1 1 test-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    ;; Add distance constraint
                    (let ([c (make-distance-joint 'dist-1 'a 'b
                                                  (vec2 0 0) (vec2 0 0) 10)])
                         (world-add-constraint! w c))
                    ;; Give them different velocities
                    (set-velocity! w 'a (vec2 5 0))
                    (set-velocity! w 'b (vec2 -5 0))
                    ;; Step
                    (run-steps w 100 0.016)
                    ;; Check distance is approximately maintained
                    (let ([dist (entity-distance w 'a 'b)])
                         (assert-true (< (abs (- dist 10)) 0.5)))))
            
            ;; Test: Distance constraint with gravity (pendulum)
            (define-test pendulum-swing
              (let* ([w (make-world (vec2 0 100))]  ; Gravity down
                     [ball (make-circle-entity 'ball (vec2 5 0) 0.5 1 test-material)]
                     [anchor (make-static-circle 'anchor (vec2 0 0) 0.1 test-material)])
                    (world-add-entity! w ball)
                    (world-add-entity! w anchor)
                    ;; Pendulum: ball connected to static anchor
                    (let ([c (make-distance-joint 'pendulum 'ball 'anchor
                                                  (vec2 0 0) (vec2 0 0) 5)])
                         (world-add-constraint! w c))
                    ;; Let it swing
                    (run-steps w 200 0.016)
                    ;; Distance should still be 5
                    (let ([dist (entity-distance w 'ball 'anchor)])
                         (assert-true (< (abs (- dist 5)) 0.3)))))
            
            ;; Test: Distance constraint with one static body
            (define-test distance-one-static
              (let* ([w (make-world (vec2 0 0))]
                     [ball (make-circle-entity 'ball (vec2 0 0) 1 1 test-material)]
                     [anchor (make-static-circle 'anchor (vec2 10 0) 0.1 test-material)])
                    (world-add-entity! w ball)
                    (world-add-entity! w anchor)
                    (let ([c (make-distance-joint 'rod 'ball 'anchor
                                                  (vec2 0 0) (vec2 0 0) 10)])
                         (world-add-constraint! w c))
                    ;; Push the ball
                    (set-velocity! w 'ball (vec2 0 10))
                    (run-steps w 50 0.016)
                    ;; Anchor should not have moved
                    (let ([anchor-pos (entity-position w 'anchor)])
                         (assert-true (< (vec2-magnitude (vec2-sub anchor-pos (vec2 10 0))) 0.01)))))
            )

;;; ====
;;; Revolute Joint Tests
;;; ====

(test-group revolute-joint-tests
            
            ;; Test: Bodies stay connected at anchor point
            (define-test revolute-stays-connected
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 1 1 test-material)]
                     [e2 (make-circle-entity 'b (vec2 2 0) 1 1 test-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    ;; Revolute at midpoint
                    (let ([c (make-revolute-joint 'rev-1 'a 'b
                                                  (vec2 1 0) (vec2 -1 0))])  ; Local anchors
                         (world-add-constraint! w c))
                    ;; Push them apart
                    (set-velocity! w 'a (vec2 -5 3))
                    (set-velocity! w 'b (vec2 5 -3))
                    (run-steps w 100 0.016)
                    ;; Check anchors are still together
                    (let* ([pos-a (entity-position w 'a)]
                           [pos-b (entity-position w 'b)]
                           [anchor-a (vec2-add pos-a (vec2 1 0))]
                           [anchor-b (vec2-add pos-b (vec2 -1 0))]
                           [error (vec2-magnitude (vec2-sub anchor-a anchor-b))])
                          (assert-true (< error 0.3)))))
            
            ;; Test: Revolute to static anchor (rotating around pivot)
            (define-test revolute-static-pivot
              (let* ([w (make-world (vec2 0 100))]  ; Gravity
                     [arm (make-circle-entity 'arm (vec2 5 0) 0.5 1 test-material)]
                     [pivot (make-static-circle 'pivot (vec2 0 0) 0.1 test-material)])
                    (world-add-entity! w arm)
                    (world-add-entity! w pivot)
                    ;; Arm connected at its end to pivot
                    (let ([c (make-revolute-joint 'hinge 'arm 'pivot
                                                  (vec2 -5 0) (vec2 0 0))])
                         (world-add-constraint! w c))
                    (run-steps w 100 0.016)
                    ;; Arm's left end should still be at origin
                    (let* ([arm-pos (entity-position w 'arm)]
                           [arm-anchor (vec2-add arm-pos (vec2 -5 0))]
                           [error (vec2-magnitude arm-anchor)])
                          (assert-true (< error 0.3)))))
            )

;;; ====
;;; Spring Constraint Tests
;;; ====

(test-group spring-constraint-tests
            
            ;; Test: Spring oscillates toward rest length
            (define-test spring-oscillation
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 1 1 test-material)]
                     [e2 (make-circle-entity 'b (vec2 15 0) 1 1 test-material)])  ; Stretched
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    ;; Spring with rest=10, stiffness=50, damping=5
                    (let ([c (make-spring-joint 'spring 'a 'b
                                                (vec2 0 0) (vec2 0 0)
                                                10 50 5)])
                         (world-add-constraint! w c))
                    ;; Let it settle
                    (run-steps w 500 0.016)
                    ;; Should be near rest length
                    (let ([dist (entity-distance w 'a 'b)])
                         (assert-true (< (abs (- dist 10)) 1.0)))))
            
            ;; Test: Spring with damping settles
            (define-test spring-damping-settles
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 1 1 test-material)]
                     [e2 (make-circle-entity 'b (vec2 20 0) 1 1 test-material)])  ; Very stretched
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    ;; High damping spring
                    (let ([c (make-spring-joint 'spring 'a 'b
                                                (vec2 0 0) (vec2 0 0)
                                                10 100 20)])  ; High damping
                         (world-add-constraint! w c))
                    ;; Initial velocity
                    (let ([vel-before (body-vel (entity-body (world-get-entity w 'a)))])
                         (run-steps w 300 0.016)
                         ;; Velocities should have settled
                         (let* ([ent-a (world-get-entity w 'a)]
                                [vel-after (body-vel (entity-body ent-a))]
                                [speed (vec2-magnitude vel-after)])
                               (assert-true (< speed 0.5))))))
            
            ;; Test: Compressed spring pushes apart
            (define-test spring-compression
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 1 1 test-material)]
                     [e2 (make-circle-entity 'b (vec2 5 0) 1 1 test-material)])  ; Compressed
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    ;; Spring wants to be 10 units, but starts at 5
                    (let ([c (make-spring-joint 'spring 'a 'b
                                                (vec2 0 0) (vec2 0 0)
                                                10 100 5)])
                         (world-add-constraint! w c))
                    (run-steps w 200 0.016)
                    ;; Should have expanded past 5
                    (let ([dist (entity-distance w 'a 'b)])
                         (assert-true (> dist 7)))))
            )

;;; ====
;;; Weld Joint Tests (requires RigidBody2D)
;;; ====

(test-group weld-joint-tests
            
            ;; Test: Welded bodies maintain relative angle
            (define-test weld-maintains-angle
              (let* ([w (make-world (vec2 0 0))]
                     ;; Create rigid bodies with rotation
                     [body-a (make-rigid-body (vec2 0 0) (vec2 0 0) 0 0 1 (circle-inertia 1 1))]
                     [body-b (make-rigid-body (vec2 3 0) (vec2 0 0) 0 0 1 (circle-inertia 1 1))]
                     [shape-a (make-circle (vec2 0 0) 1)]
                     [shape-b (make-circle (vec2 3 0) 1)]
                     [e1 (make-entity 'a body-a shape-a test-material #f)]
                     [e2 (make-entity 'b body-b shape-b test-material #f)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    ;; Weld them together with 0 reference angle
                    (let ([c (make-weld-joint 'weld 'a 'b
                                              (vec2 1.5 0) (vec2 -1.5 0)
                                              0)])  ; Same angle
                         (world-add-constraint! w c))
                    ;; Give body-a angular velocity
                    (world-update-entity! w 'a
                                          (lambda (e)
                                                  (entity-with-body e
                                                                    (rigid-body-with-angular-vel (entity-body e) 2))))
                    (run-steps w 100 0.016)
                    ;; Both should have similar angles
                    (let* ([ent-a (world-get-entity w 'a)]
                           [ent-b (world-get-entity w 'b)]
                           [angle-a (rigid-body-angle (entity-body ent-a))]
                           [angle-b (rigid-body-angle (entity-body ent-b))]
                           [angle-diff (abs (- angle-b angle-a))])
                          (assert-true (< angle-diff 0.3)))))
            )

;;; ====
;;; Integration Tests
;;; ====

(test-group integration-tests
            
            ;; Test: Multiple constraints work together
            (define-test chain-of-bodies
              (let* ([w (make-world (vec2 0 50))]  ; Light gravity
                     ;; Create a chain of 4 bodies
                     [e0 (make-static-circle 'anchor (vec2 0 0) 0.1 test-material)]
                     [e1 (make-circle-entity 'b1 (vec2 3 0) 0.5 1 test-material)]
                     [e2 (make-circle-entity 'b2 (vec2 6 0) 0.5 1 test-material)]
                     [e3 (make-circle-entity 'b3 (vec2 9 0) 0.5 1 test-material)])
                    (world-add-entity! w e0)
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (world-add-entity! w e3)
                    ;; Connect with distance constraints
                    (world-add-constraint! w (make-distance-joint 'd1 'anchor 'b1
                                                                  (vec2 0 0) (vec2 0 0) 3))
                    (world-add-constraint! w (make-distance-joint 'd2 'b1 'b2
                                                                  (vec2 0 0) (vec2 0 0) 3))
                    (world-add-constraint! w (make-distance-joint 'd3 'b2 'b3
                                                                  (vec2 0 0) (vec2 0 0) 3))
                    ;; Let it swing
                    (run-steps w 200 0.016)
                    ;; All distances should be approximately maintained
                    (let ([d1 (vec2-magnitude (vec2-sub (entity-position w 'anchor)
                                                        (entity-position w 'b1)))]
                          [d2 (entity-distance w 'b1 'b2)]
                          [d3 (entity-distance w 'b2 'b3)])
                         (assert-true (< (abs (- d1 3)) 0.5))
                         (assert-true (< (abs (- d2 3)) 0.5))
                         (assert-true (< (abs (- d3 3)) 0.5)))))
            
            ;; Test: Constraint removal
            (define-test constraint-removal
              (let* ([w (make-world (vec2 0 0))]
                     [e1 (make-circle-entity 'a (vec2 0 0) 1 1 test-material)]
                     [e2 (make-circle-entity 'b (vec2 10 0) 1 1 test-material)])
                    (world-add-entity! w e1)
                    (world-add-entity! w e2)
                    (let ([c (make-distance-joint 'temp 'a 'b
                                                  (vec2 0 0) (vec2 0 0) 10)])
                         (world-add-constraint! w c))
                    ;; Verify constraint exists
                    (assert-true (not (null? (world-constraint-list w))))
                    ;; Remove it
                    (world-remove-constraint! w 'temp)
                    ;; Verify it's gone
                    (assert-true (null? (world-constraint-list w)))))
            )

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
