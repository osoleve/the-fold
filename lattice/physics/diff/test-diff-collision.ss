;;; core/diff-physics/test-diff-collision.ss --- Tests for Differentiable Collision Response
;;;
;;; Comprehensive tests for impulse-based collision response including:
;;; - Circle-circle collision forces
;;; - Ground collision
;;; - Multi-body collision system
;;; - Contact detection utilities
;;; - Complete collision steps
;;;
;;; Run with: scheme --script core/diff-physics/test-diff-collision.ss

(load "core/test-framework.ss")
(load "lattice/physics/diff/diff-collision.ss")

(display "
====
         DIFFERENTIABLE COLLISION RESPONSE TESTS
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

(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

(define pi 3.141592653589793)

;;; Create default soft material for tests
(define test-material (make-soft-material 10000 100 100 0.3))

;;; ====
;;; Circle-Circle Collision Force Tests
;;; ====

(test-group circle-circle-force-tests
            
            (define-test no-contact-no-force
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [angle-a (make-traced-var 0 tape)]
                     [omega-a (make-traced-var 0 tape)]
                     [body-a (make-traced-body pos-a vel-a angle-a omega-a 1 0.5)]
                     [radius-a 1]
                     [pos-b (lift-vec2 (vec2 10 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [angle-b (make-traced-var 0 tape)]
                     [omega-b (make-traced-var 0 tape)]
                     [body-b (make-traced-body pos-b vel-b angle-b omega-b 1 0.5)]
                     [radius-b 1])
                    (call-with-values
                     (lambda () (traced-circle-collision-force body-a radius-a body-b radius-b test-material))
                     (lambda (force torque)
                             (assert-near (traced-value (traced-vec2-x force)) 0 1)
                             (assert-near (traced-value (traced-vec2-y force)) 0 1)))))
            
            (define-test overlapping-circles-repel
              (let* ([tape (make-reverse-tape)]
                     ;; Circles overlapping along x-axis
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [angle-a (make-traced-var 0 tape)]
                     [omega-a (make-traced-var 0 tape)]
                     [body-a (make-traced-body pos-a vel-a angle-a omega-a 1 0.5)]
                     [radius-a 1.5]
                     [pos-b (lift-vec2 (vec2 2 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [angle-b (make-traced-var 0 tape)]
                     [omega-b (make-traced-var 0 tape)]
                     [body-b (make-traced-body pos-b vel-b angle-b omega-b 1 0.5)]
                     [radius-b 1.5])
                    (call-with-values
                     (lambda () (traced-circle-collision-force body-a radius-a body-b radius-b test-material))
                     (lambda (force torque)
                             ;; Force on A should point away from B (negative x)
                             (assert-true (< (traced-value (traced-vec2-x force)) 0))))))
            
            (define-test force-magnitude-increases-with-penetration
              (let* ([tape (make-reverse-tape)]
                     ;; Less overlap
                     [pos-a1 (lift-vec2 (vec2 0 0) tape)]
                     [vel-a1 (lift-vec2 (vec2 0 0) tape)]
                     [body-a1 (make-traced-body pos-a1 vel-a1 (make-traced-var 0 tape)
                                                (make-traced-var 0 tape) 1 0.5)]
                     [pos-b1 (lift-vec2 (vec2 1.9 0) tape)]
                     [vel-b1 (lift-vec2 (vec2 0 0) tape)]
                     [body-b1 (make-traced-body pos-b1 vel-b1 (make-traced-var 0 tape)
                                                (make-traced-var 0 tape) 1 0.5)]
                     ;; More overlap
                     [pos-a2 (lift-vec2 (vec2 0 0) tape)]
                     [vel-a2 (lift-vec2 (vec2 0 0) tape)]
                     [body-a2 (make-traced-body pos-a2 vel-a2 (make-traced-var 0 tape)
                                                (make-traced-var 0 tape) 1 0.5)]
                     [pos-b2 (lift-vec2 (vec2 1.5 0) tape)]
                     [vel-b2 (lift-vec2 (vec2 0 0) tape)]
                     [body-b2 (make-traced-body pos-b2 vel-b2 (make-traced-var 0 tape)
                                                (make-traced-var 0 tape) 1 0.5)])
                    (call-with-values
                     (lambda () (traced-circle-collision-force body-a1 1 body-b1 1 test-material))
                     (lambda (force1 torque1)
                             (call-with-values
                              (lambda () (traced-circle-collision-force body-a2 1 body-b2 1 test-material))
                              (lambda (force2 torque2)
                                      (let ([mag1 (abs (traced-value (traced-vec2-x force1)))]
                                            [mag2 (abs (traced-value (traced-vec2-x force2)))])
                                           (assert-true (< mag1 mag2)))))))))
            
            (define-test approaching-circles-have-repelling-force
              ;; When bodies are overlapping, they should experience repelling force
              (let* ([tape (make-reverse-tape)]
                     ;; Overlapping, stationary bodies
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [body-a (make-traced-body pos-a vel-a (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [pos-b (lift-vec2 (vec2 1.5 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [body-b (make-traced-body pos-b vel-b (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)])
                    (call-with-values
                     (lambda () (traced-circle-collision-force body-a 1 body-b 1 test-material))
                     (lambda (force torque)
                             ;; Force on A should have nonzero magnitude (repelling)
                             (let ([mag (abs (traced-value (traced-vec2-x force)))])
                                  (assert-true (> mag 0)))))))
            )

;;; ====
;;; Circle-Circle Impulse Application Tests
;;; ====

(test-group circle-impulse-tests
            
            (define-test impulse-separates-bodies
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [body-a (make-traced-body pos-a vel-a (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [pos-b (lift-vec2 (vec2 1.5 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [body-b (make-traced-body pos-b vel-b (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [dt 0.01])
                    (call-with-values
                     (lambda () (traced-circle-collision-impulses body-a 1 body-b 1 test-material dt))
                     (lambda (new-a new-b)
                             ;; A should be moving left (away from B)
                             (assert-true (< (traced-value (traced-vec2-x (traced-body-vel new-a))) 0))
                             ;; B should be moving right (away from A)
                             (assert-true (> (traced-value (traced-vec2-x (traced-body-vel new-b))) 0))))))
            
            (define-test impulses-conserve-momentum-approx
              ;; Equal mass bodies: total momentum should be approximately conserved
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 1 0) tape)]
                     [body-a (make-traced-body pos-a vel-a (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [pos-b (lift-vec2 (vec2 1.5 0) tape)]
                     [vel-b (lift-vec2 (vec2 -1 0) tape)]
                     [body-b (make-traced-body pos-b vel-b (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [dt 0.001]
                     ;; Initial momentum
                     [p0x (+ (traced-value (traced-vec2-x vel-a))
                             (traced-value (traced-vec2-x vel-b)))])
                    (call-with-values
                     (lambda () (traced-circle-collision-impulses body-a 1 body-b 1 test-material dt))
                     (lambda (new-a new-b)
                             (let ([p1x (+ (traced-value (traced-vec2-x (traced-body-vel new-a)))
                                           (traced-value (traced-vec2-x (traced-body-vel new-b))))])
                                  ;; Momentum should be approximately conserved
                                  (assert-near p0x p1x 0.1))))))
            )

;;; ====
;;; Ground Collision Tests
;;; ====

(test-group ground-collision-tests
            
            (define-test above-ground-no-force
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 10) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [body (make-traced-body pos vel (make-traced-var 0 tape)
                                             (make-traced-var 0 tape) 1 0.5)]
                     [radius 1]
                     [ground-y 0])
                    (call-with-values
                     (lambda () (traced-ground-collision-force body radius ground-y test-material))
                     (lambda (force torque)
                             (assert-near (traced-value (traced-vec2-y force)) 0 1)))))
            
            (define-test penetrating-ground-pushes-up
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0.5) tape)]  ; Center at 0.5, radius 1, bottom at -0.5
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [body (make-traced-body pos vel (make-traced-var 0 tape)
                                             (make-traced-var 0 tape) 1 0.5)]
                     [radius 1]
                     [ground-y 0])
                    (call-with-values
                     (lambda () (traced-ground-collision-force body radius ground-y test-material))
                     (lambda (force torque)
                             ;; Should push up
                             (assert-true (> (traced-value (traced-vec2-y force)) 0))))))
            
            (define-test ground-collision-step-applies-force
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0.5) tape)]
                     [vel (lift-vec2 (vec2 0 -5) tape)]  ; falling
                     [body (make-traced-body pos vel (make-traced-var 0 tape)
                                             (make-traced-var 0 tape) 1 0.5)]
                     [radius 1]
                     [ground-y 0]
                     [dt 0.01]
                     [new-body (traced-ground-collision-step body radius ground-y test-material dt)]
                     [new-vy (traced-value (traced-vec2-y (traced-body-vel new-body)))])
                    ;; Velocity should be less negative (slowing down)
                    (assert-true (> new-vy -5))))
            
            (define-test ground-friction-slows-horizontal
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0.5) tape)]  ; penetrating
                     [vel (lift-vec2 (vec2 10 0) tape)]   ; moving right
                     [body (make-traced-body pos vel (make-traced-var 0 tape)
                                             (make-traced-var 0 tape) 1 0.5)]
                     [radius 1]
                     [ground-y 0])
                    (call-with-values
                     (lambda () (traced-ground-collision-force body radius ground-y test-material))
                     (lambda (force torque)
                             ;; Friction should oppose horizontal motion
                             (assert-true (< (traced-value (traced-vec2-x force)) 0))))))
            )

;;; ====
;;; Collision Pair Tests
;;; ====

(test-group collision-pair-tests
            
            (define-test make-collision-pair-structure
              (let ([pair (make-collision-pair 0 1 0.5 0.75)])
                   (assert-equal (collision-pair-idx-a pair) 0)
                   (assert-equal (collision-pair-idx-b pair) 1)
                   (assert-near (collision-pair-radius-a pair) 0.5 1e-10)
                   (assert-near (collision-pair-radius-b pair) 0.75 1e-10)))
            )

;;; ====
;;; Multi-Body Collision Tests
;;; ====

(test-group multi-body-tests
            
            (define-test accumulate-forces-empty-pairs
              (let* ([tape (make-reverse-tape)]
                     [body (make-traced-body (lift-vec2 (vec2 0 0) tape)
                                             (lift-vec2 (vec2 0 0) tape)
                                             (make-traced-var 0 tape)
                                             (make-traced-var 0 tape) 1 0.5)]
                     [bodies (vector body)]
                     [pairs '()])
                    (call-with-values
                     (lambda () (traced-accumulate-collision-forces bodies pairs test-material))
                     (lambda (forces torques)
                             (assert-equal (vector-length forces) 1)
                             (assert-equal (vector-length torques) 1)))))
            
            (define-test accumulate-forces-single-pair
              (let* ([tape (make-reverse-tape)]
                     ;; Two overlapping bodies
                     [body-a (make-traced-body (lift-vec2 (vec2 0 0) tape)
                                               (lift-vec2 (vec2 0 0) tape)
                                               (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [body-b (make-traced-body (lift-vec2 (vec2 1.5 0) tape)
                                               (lift-vec2 (vec2 0 0) tape)
                                               (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [bodies (vector body-a body-b)]
                     [pairs (list (make-collision-pair 0 1 1 1))])
                    (call-with-values
                     (lambda () (traced-accumulate-collision-forces bodies pairs test-material))
                     (lambda (forces torques)
                             ;; Forces should be opposite
                             (let ([fx-a (traced-value (traced-vec2-x (vector-ref forces 0)))]
                                   [fx-b (traced-value (traced-vec2-x (vector-ref forces 1)))])
                                  (assert-near fx-a (- fx-b) 1))))))
            )

;;; ====
;;; Contact Detection Tests
;;; ====

(test-group contact-detection-tests
            
            (define-test circles-overlapping-detects-overlap
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [pos-b (lift-vec2 (vec2 1.5 0) tape)])
                    (assert-true (traced-circles-overlapping? pos-a 1 pos-b 1))))
            
            (define-test circles-overlapping-detects-separation
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [pos-b (lift-vec2 (vec2 5 0) tape)])
                    (assert-false (traced-circles-overlapping? pos-a 1 pos-b 1))))
            
            (define-test circle-ground-overlapping-above
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 5) tape)])
                    (assert-false (traced-circle-ground-overlapping? pos 1 0))))
            
            (define-test circle-ground-overlapping-penetrating
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0.5) tape)])
                    (assert-true (traced-circle-ground-overlapping? pos 1 0))))
            )

;;; ====
;;; Restitution Calculation Tests
;;; ====

(test-group restitution-tests
            
            (define-test effective-restitution-underdamped
              ;; Low damping should give positive restitution
              (let ([e (effective-restitution 10000 10 1 1)])
                   (assert-true (> e 0))
                   (assert-true (< e 1))))
            
            (define-test effective-restitution-overdamped
              ;; Very high damping should give zero restitution
              (let ([e (effective-restitution 1000 10000 1 1)])
                   (assert-near e 0 0.01)))
            
            (define-test effective-restitution-decreases-with-damping
              (let ([e1 (effective-restitution 10000 10 1 1)]
                    [e2 (effective-restitution 10000 100 1 1)])
                   (assert-true (> e1 e2))))
            )

;;; ====
;;; Complete Collision Step Tests
;;; ====

(test-group collision-step-tests
            
            (define-test collision-step-with-no-others
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 10) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [body (make-traced-body pos vel (make-traced-var 0 tape)
                                             (make-traced-var 0 tape) 1 0.5)]
                     [radius 1]
                     [others '()]
                     [ground-y #f]  ; no ground
                     [dt 0.01]
                     [new-body (traced-collision-step body radius others ground-y test-material dt)])
                    ;; Should be unchanged (no forces)
                    (assert-near (traced-value (traced-vec2-x (traced-body-vel new-body))) 0 0.01)))
            
            (define-test collision-step-with-ground
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0.5) tape)]  ; penetrating ground
                     [vel (lift-vec2 (vec2 0 -10) tape)]
                     [body (make-traced-body pos vel (make-traced-var 0 tape)
                                             (make-traced-var 0 tape) 1 0.5)]
                     [radius 1]
                     [others '()]
                     [ground-y 0]
                     [dt 0.01]
                     [new-body (traced-collision-step body radius others ground-y test-material dt)]
                     [new-vy (traced-value (traced-vec2-y (traced-body-vel new-body)))])
                    ;; Should be slowed down by ground reaction
                    (assert-true (> new-vy -10))))
            
            (define-test collision-step-with-other-body
              (let* ([tape (make-reverse-tape)]
                     [pos-a (lift-vec2 (vec2 0 0) tape)]
                     [vel-a (lift-vec2 (vec2 0 0) tape)]
                     [body-a (make-traced-body pos-a vel-a (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [pos-b (lift-vec2 (vec2 1.5 0) tape)]
                     [vel-b (lift-vec2 (vec2 0 0) tape)]
                     [body-b (make-traced-body pos-b vel-b (make-traced-var 0 tape)
                                               (make-traced-var 0 tape) 1 0.5)]
                     [radius 1]
                     [others (list (list body-b 1))]
                     [ground-y #f]
                     [dt 0.01]
                     [new-body (traced-collision-step body-a radius others ground-y test-material dt)]
                     [new-vx (traced-value (traced-vec2-x (traced-body-vel new-body)))])
                    ;; Should be pushed left (away from body-b)
                    (assert-true (< new-vx 0))))
            )

;;; ====
;;; Gradient Tests
;;; ====

(test-group gradient-tests
            
            (define-test collision-force-gradient-flows
              ;; Verify gradient w.r.t. position affects force using finite diff
              (let* ([test-fn (lambda (px)
                                      (let* ([tape (make-reverse-tape)]
                                             [pos-a (traced-vec2 (make-traced-var px tape)
                                                                 (make-traced-var 0 tape))]
                                             [vel-a (lift-vec2 (vec2 0 0) tape)]
                                             [body-a (make-traced-body pos-a vel-a
                                                                       (make-traced-var 0 tape)
                                                                       (make-traced-var 0 tape) 1 0.5)]
                                             [pos-b (lift-vec2 (vec2 2 0) tape)]
                                             [vel-b (lift-vec2 (vec2 0 0) tape)]
                                             [body-b (make-traced-body pos-b vel-b
                                                                       (make-traced-var 0 tape)
                                                                       (make-traced-var 0 tape) 1 0.5)])
                                            (call-with-values
                                             (lambda () (traced-circle-collision-force body-a 1 body-b 1 test-material))
                                             (lambda (force torque)
                                                     (traced-value (traced-vec2-x force))))))]
                     [px0 0.5]
                     [numerical (finite-diff test-fn px0 1e-5)])
                    ;; Should have non-zero gradient
                    (assert-true (not (= numerical 0)))))
            
            (define-test ground-collision-gradient-flows
              (let* ([test-fn (lambda (py)
                                      (let* ([tape (make-reverse-tape)]
                                             [pos (traced-vec2 (make-traced-var 0 tape)
                                                               (make-traced-var py tape))]
                                             [vel (lift-vec2 (vec2 0 0) tape)]
                                             [body (make-traced-body pos vel
                                                                     (make-traced-var 0 tape)
                                                                     (make-traced-var 0 tape) 1 0.5)])
                                            (call-with-values
                                             (lambda () (traced-ground-collision-force body 1 0 test-material))
                                             (lambda (force torque)
                                                     (traced-value (traced-vec2-y force))))))]
                     [py0 0.5]
                     [numerical (finite-diff test-fn py0 1e-5)])
                    ;; As y increases (ball moves up), force should decrease
                    (assert-true (< numerical 0))))
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
