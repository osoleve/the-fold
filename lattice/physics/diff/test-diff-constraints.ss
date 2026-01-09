;;; core/diff-physics/test-diff-constraints.ss --- Tests for Differentiable Constraints
;;;
;;; Comprehensive tests for the constraint solver including:
;;; - Spring constraints
;;; - Distance constraints
;;; - Anchor constraints
;;; - Revolute joints
;;; - Angular springs
;;; - Multi-body constraint systems
;;;
;;; Run with: scheme --script core/diff-physics/test-diff-constraints.ss

(load "core/test-framework.ss")
(load "lattice/physics/diff/diff-constraints.ss")
(load "lattice/physics/diff/traced-integrators.ss")

(display "
==============================================================
         DIFFERENTIABLE CONSTRAINTS TESTS
==============================================================
")

;;; ============================================================
;;; Test Utilities
;;; ============================================================

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

(define (assert-vec2-near actual expected tolerance)
  (unless (and (< (abs (- (vec2-x actual) (vec2-x expected))) tolerance)
               (< (abs (- (vec2-y actual) (vec2-y expected))) tolerance))
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    FAIL: Expected ")
          (display expected)
          (display " +/- ")
          (display tolerance)
          (display ", got ")
          (display actual)
          (newline)))

;;; ============================================================
;;; Spring Force Tests
;;; ============================================================

(test-group spring-force-tests
            
            (define-test spring-force-stretched
              ;; Spring with rest length 1, stretched to length 3
              ;; Should produce force pulling a toward b
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 3 0) tape)]
                     [rest-len 1]
                     [stiffness 100]
                     [force (traced-spring-force a b rest-len stiffness)]
                     [fx (traced-value (traced-vec2-x force))]
                     [fy (traced-value (traced-vec2-y force))])
                    ;; Stretch is 2, so force magnitude is 200 in +x direction
                    (assert-near fx 200 0.01)
                    (assert-near fy 0 0.01)))
            
            (define-test spring-force-compressed
              ;; Spring at rest length 3, compressed to 1
              ;; Force should push a away from b (negative x)
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 1 0) tape)]
                     [rest-len 3]
                     [stiffness 50]
                     [force (traced-spring-force a b rest-len stiffness)]
                     [fx (traced-value (traced-vec2-x force))])
                    ;; Stretch is -2, so force is -100 (pointing toward b)
                    (assert-near fx -100 0.01)))
            
            (define-test spring-force-at-rest
              ;; Spring at natural length should produce zero force
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 2 0) tape)]
                     [rest-len 2]
                     [stiffness 100]
                     [force (traced-spring-force a b rest-len stiffness)]
                     [fx (traced-value (traced-vec2-x force))])
                    (assert-near fx 0 0.01)))
            
            (define-test spring-force-diagonal
              ;; Spring stretched diagonally
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 3 4) tape)]  ; distance = 5
                     [rest-len 0]
                     [stiffness 10]
                     [force (traced-spring-force a b rest-len stiffness)]
                     [fx (traced-value (traced-vec2-x force))]
                     [fy (traced-value (traced-vec2-y force))])
                    ;; Force magnitude is 50, direction is (3/5, 4/5)
                    (assert-near fx 30 0.01)
                    (assert-near fy 40 0.01)))
            
            (define-test spring-force-damped-computes-value
              ;; Damped spring with bodies at rest should match undamped
              (let* ([tape (make-reverse-tape)]
                     [a (lift-vec2 (vec2 0 0) tape)]
                     [b (lift-vec2 (vec2 2 0) tape)]
                     [va (lift-vec2 (vec2 0 0) tape)]   ; a at rest
                     [vb (lift-vec2 (vec2 0 0) tape)]   ; b at rest
                     [rest-len 1]
                     [stiffness 100]
                     [damping 10]
                     [force-undamped (traced-spring-force a b rest-len stiffness)]
                     [force-damped (traced-spring-force-damped a b va vb rest-len stiffness damping)]
                     [fx-undamped (traced-value (traced-vec2-x force-undamped))]
                     [fx-damped (traced-value (traced-vec2-x force-damped))])
                    ;; At rest, damped force should equal undamped force
                    (assert-near fx-damped fx-undamped 1)))
            )

;;; ============================================================
;;; Spring Constraint Structure Tests
;;; ============================================================

(test-group spring-constraint-structure-tests
            
            (define-test make-spring-constraint-structure
              (let* ([anchor-a (lift-vec2-const (vec2 0.5 0))]
                     [anchor-b (lift-vec2-const (vec2 -0.5 0))]
                     [c (make-spring-constraint 0 anchor-a 1 anchor-b 2.0 500 50)])
                    (assert-equal (constraint-type c) 'spring-constraint)
                    (assert-equal (spring-constraint-body-a c) 0)
                    (assert-equal (spring-constraint-body-b c) 1)
                    (assert-near (spring-constraint-rest-length c) 2.0 0.001)
                    (assert-near (spring-constraint-stiffness c) 500 0.001)
                    (assert-near (spring-constraint-damping c) 50 0.001)))
            
            (define-test make-distance-constraint-uses-high-stiffness
              (let* ([anchor-a (lift-vec2-const (vec2 0 0))]
                     [anchor-b (lift-vec2-const (vec2 0 0))]
                     [c (make-distance-constraint 0 anchor-a 1 anchor-b 1.5)])
                    (assert-equal (constraint-type c) 'spring-constraint)
                    (assert-near (spring-constraint-rest-length c) 1.5 0.001)
                    (assert-true (> (spring-constraint-stiffness c) 1000))))
            )

;;; ============================================================
;;; Anchor Constraint Tests
;;; ============================================================

(test-group anchor-constraint-tests
            
            (define-test anchor-constraint-structure
              (let* ([local-anchor (lift-vec2-const (vec2 0 0))]
                     [world-target (lift-vec2-const (vec2 5 5))]
                     [c (make-anchor-constraint 0 local-anchor world-target 1000 100)])
                    (assert-equal (constraint-type c) 'anchor-constraint)
                    (assert-equal (anchor-constraint-body c) 0)
                    (assert-near (anchor-constraint-stiffness c) 1000 0.001)))
            
            (define-test anchor-force-pulls-toward-target
              ;; Body at origin should be pulled toward target
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [local-anchor (lift-vec2-const (vec2 0 0))]
                     [world-target (lift-vec2 (vec2 3 4) tape)]
                     [stiffness 100]
                     [damping 10])
                    (call-with-values
                     (lambda () (traced-anchor-force body local-anchor world-target stiffness damping))
                     (lambda (force torque)
                             (let ([fx (traced-value (traced-vec2-x force))]
                                   [fy (traced-value (traced-vec2-y force))])
                                  ;; Force should point toward target (3, 4)
                                  (assert-true (> fx 0))
                                  (assert-true (> fy 0)))))))
            )

;;; ============================================================
;;; Revolute Joint Tests
;;; ============================================================

(test-group revolute-joint-tests
            
            (define-test revolute-joint-structure
              (let* ([anchor-a (lift-vec2-const (vec2 1 0))]
                     [anchor-b (lift-vec2-const (vec2 -1 0))]
                     [j (make-revolute-joint 0 anchor-a 1 anchor-b 5000 500)])
                    (assert-equal (constraint-type j) 'revolute-joint)
                    (assert-equal (revolute-joint-body-a j) 0)
                    (assert-equal (revolute-joint-body-b j) 1)))
            
            (define-test revolute-forces-pulls-anchors-together
              ;; Two bodies with separated anchors should be pulled together
              (let* ([tape (make-reverse-tape)]
                     ;; Body A at origin
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [angle-a (make-traced-var 0 tape)]
                     [omega-a (make-traced-var 0 tape)]
                     [body-a (make-traced-body pos-a vel-a angle-a omega-a 1 0.5)]
                     ;; Body B at (4, 0)
                     [pos-b (lift-vec2 (vec2 4 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [angle-b (make-traced-var 0 tape)]
                     [omega-b (make-traced-var 0 tape)]
                     [body-b (make-traced-body pos-b vel-b angle-b omega-b 1 0.5)]
                     ;; Anchor at right edge of A, left edge of B
                     [anchor-a (lift-vec2-const (vec2 1 0))]
                     [anchor-b (lift-vec2-const (vec2 -1 0))]
                     [stiffness 1000]
                     [damping 100])
                    (call-with-values
                     (lambda () (traced-revolute-forces body-a anchor-a body-b anchor-b stiffness damping))
                     (lambda (result-a result-b)
                             (let ([force-a (car result-a)]
                                   [force-b (car result-b)])
                                  ;; A's anchor at (1,0), B's anchor at (3,0)
                                  ;; A should be pulled right, B should be pulled left
                                  (assert-true (> (traced-value (traced-vec2-x force-a)) 0))
                                  (assert-true (< (traced-value (traced-vec2-x force-b)) 0)))))))
            )

;;; ============================================================
;;; Angular Spring Tests
;;; ============================================================

(test-group angular-spring-tests
            
            (define-test angular-spring-zero-error
              ;; At target angle, torque should be zero
              (let* ([tape (make-reverse-tape)]
                     [angle-a (make-traced-var 0 tape)]
                     [angle-b (make-traced-var 0.5 tape)]
                     [omega-a (make-traced-var 0 tape)]
                     [omega-b (make-traced-var 0 tape)]
                     [target-diff 0.5]
                     [stiffness 100]
                     [damping 10]
                     [torque (traced-angular-spring-torque angle-a angle-b omega-a omega-b
                                                           target-diff stiffness damping)])
                    (assert-near (traced-value torque) 0 0.01)))
            
            (define-test angular-spring-positive-error
              ;; Angle difference exceeds target
              (let* ([tape (make-reverse-tape)]
                     [angle-a (make-traced-var 0 tape)]
                     [angle-b (make-traced-var 1.0 tape)]  ; diff = 1.0
                     [omega-a (make-traced-var 0 tape)]
                     [omega-b (make-traced-var 0 tape)]
                     [target-diff 0.5]  ; want 0.5
                     [stiffness 100]
                     [damping 0]
                     [torque (traced-angular-spring-torque angle-a angle-b omega-a omega-b
                                                           target-diff stiffness damping)])
                    ;; Error is 0.5, so torque on A is 50 (positive = CCW)
                    (assert-near (traced-value torque) 50 0.01)))
            )

;;; ============================================================
;;; Constraint System Tests
;;; ============================================================

(test-group constraint-system-tests
            
            (define-test apply-constraint-dispatches-correctly
              ;; Test that apply-constraint handles different types
              (let* ([tape (make-reverse-tape)]
                     ;; Create two bodies
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [angle-a (make-traced-var 0 tape)]
                     [omega-a (make-traced-var 0 tape)]
                     [body-a (make-traced-body pos-a vel-a angle-a omega-a 1 0.5)]
                     [pos-b (lift-vec2 (vec2 2 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [angle-b (make-traced-var 0 tape)]
                     [omega-b (make-traced-var 0 tape)]
                     [body-b (make-traced-body pos-b vel-b angle-b omega-b 1 0.5)]
                     [bodies (vector body-a body-b)]
                     ;; Spring constraint
                     [spring (make-spring-constraint 0 (lift-vec2-const (vec2 0 0))
                                                     1 (lift-vec2-const (vec2 0 0))
                                                     1 100 10)]
                     [dt 0.01]
                     [new-bodies (traced-apply-constraint bodies spring dt)])
                    (assert-equal (vector-length new-bodies) 2)
                    (assert-true (traced-body? (vector-ref new-bodies 0)))))
            
            (define-test apply-all-constraints-processes-list
              (let* ([tape (make-reverse-tape)]
                     ;; Single body
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [bodies (vector body)]
                     ;; Anchor constraint
                     [anchor (make-anchor-constraint 0 (lift-vec2-const (vec2 0 0))
                                                     (lift-vec2-const (vec2 1 0))
                                                     1000 100)]
                     [constraints (list anchor)]
                     [dt 0.01]
                     [new-bodies (traced-apply-all-constraints bodies constraints dt)])
                    (assert-equal (vector-length new-bodies) 1)
                    ;; Body should have been affected (velocity changed toward target)
                    (let ([new-vel (traced-body-vel (vector-ref new-bodies 0))])
                         (assert-true (> (traced-value (traced-vec2-x new-vel)) 0)))))
            )

;;; ============================================================
;;; Constraint Step Integration Tests
;;; ============================================================

(test-group constraint-step-tests
            
            (define-test constraint-step-applies-gravity
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 10) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [bodies (vector body)]
                     [constraints '()]
                     [gravity (lift-vec2-const (vec2 0 -9.8))]
                     [dt 0.1]
                     [iterations 1]
                     [new-bodies (traced-constraint-step bodies constraints gravity dt iterations)])
                    ;; After one step, velocity should be -0.98 in y
                    (let ([new-vel (traced-body-vel (vector-ref new-bodies 0))])
                         (assert-near (traced-value (traced-vec2-y new-vel)) -0.98 0.01))))
            
            (define-test constraint-step-skips-static-bodies
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     ;; Static body (mass = 0)
                     [body (make-traced-static-body pos angle)]
                     [bodies (vector body)]
                     [gravity (lift-vec2-const (vec2 0 -9.8))]
                     [dt 0.1]
                     [new-bodies (traced-constraint-step bodies '() gravity dt 1)])
                    ;; Static body should not move
                    (let ([new-pos (traced-body-pos (vector-ref new-bodies 0))])
                         (assert-near (traced-value (traced-vec2-y new-pos)) 0 0.001))))
            )

;;; ============================================================
;;; Pendulum System Tests
;;; ============================================================

(test-group pendulum-system-tests
            
            (define-test make-pendulum-creates-body-and-constraint
              (let* ([tape (make-reverse-tape)]
                     [pivot (vec2 0 10)]
                     [length 2]
                     [mass 1]
                     [radius 0.1])
                    (call-with-values
                     (lambda () (make-pendulum-system pivot length mass radius tape))
                     (lambda (bob constraint)
                             (assert-true (traced-body? bob))
                             (assert-equal (constraint-type constraint) 'anchor-constraint)
                             ;; Bob should be below pivot
                             (let ([bob-y (traced-value (traced-vec2-y (traced-body-pos bob)))])
                                  (assert-near bob-y (- (vec2-y pivot) length) 0.001))))))
            )

;;; ============================================================
;;; Chain System Tests
;;; ============================================================

(test-group chain-system-tests
            
            (define-test make-chain-creates-correct-structure
              (let* ([tape (make-reverse-tape)]
                     [start (vec2 0 10)]
                     [num-links 3]
                     [link-length 1]
                     [mass 0.5]
                     [radius 0.1])
                    (call-with-values
                     (lambda () (make-chain-system start num-links link-length mass radius tape))
                     (lambda (bodies constraints)
                             ;; Should have 3 bodies
                             (assert-equal (vector-length bodies) 3)
                             ;; Should have 1 anchor + 2 distance constraints
                             (assert-equal (length constraints) 3)
                             ;; First constraint should be anchor
                             (assert-equal (constraint-type (car constraints)) 'anchor-constraint)))))
            )

;;; ============================================================
;;; Gradient Correctness Tests
;;; ============================================================

;;; Finite difference helper
(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

(test-group gradient-tests
            
            (define-test spring-force-gradient-correct
              ;; Verify gradient of spring force x-component w.r.t. position
              (let* ([test-fn (lambda (ax)
                                      (let* ([tape (make-reverse-tape)]
                                             [a (traced-vec2 (make-traced-var ax tape)
                                                             (make-traced-var 0 tape))]
                                             [b (lift-vec2-const (vec2 3 0))]
                                             [force (traced-spring-force a b 1 100)])
                                            (traced-value (traced-vec2-x force))))]
                     [x0 0]
                     [epsilon 1e-5]
                     [numerical-grad (finite-diff test-fn x0 epsilon)]
                     ;; Analytical gradient
                     [grads (gradient
                             (lambda (ax ay)
                                     (let* ([a (traced-vec2 ax ay)]
                                            [b (lift-vec2-const (vec2 3 0))]
                                            [force (traced-spring-force a b 1 100)])
                                           (traced-vec2-x force)))
                             (list 0 0))]
                     [analytical-grad (car grads)])
                    (assert-near analytical-grad numerical-grad 0.001)))
            
            (define-test angular-spring-gradient-correct
              (let* ([test-fn (lambda (angle-a)
                                      (let* ([tape (make-reverse-tape)]
                                             [ta (make-traced-var angle-a tape)]
                                             [tb (make-traced-var 1 tape)]
                                             [oa (make-traced-var 0 tape)]
                                             [ob (make-traced-var 0 tape)]
                                             [torque (traced-angular-spring-torque ta tb oa ob 0.5 100 10)])
                                            (traced-value torque)))]
                     [x0 0]
                     [epsilon 1e-5]
                     [numerical-grad (finite-diff test-fn x0 epsilon)]
                     [grads (gradient
                             (lambda (angle-a angle-b omega-a omega-b)
                                     (traced-angular-spring-torque angle-a angle-b omega-a omega-b 0.5 100 10))
                             (list 0 1 0 0))]
                     [analytical-grad (car grads)])
                    (assert-near analytical-grad numerical-grad 0.001)))
            )

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(newline)
(display "--------------------------------------------------------------")
(newline)
(display "  Test Summary:")
(newline)
(display "--------------------------------------------------------------")
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
