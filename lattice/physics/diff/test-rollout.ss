;;; core/diff-physics/test-rollout.ss --- Tests for Simulation Rollout
;;;
;;; Comprehensive tests for multi-step physics simulation including:
;;; - Basic rollout
;;; - Trajectory recording
;;; - Checkpointing for memory efficiency
;;; - Loss functions
;;; - Projectile motion
;;; - Substepping
;;;
;;; Run with: scheme --script core/diff-physics/test-rollout.ss

(load "core/test-framework.ss")
(load "lattice/physics/diff/rollout.ss")

(display "
==============================================================
         SIMULATION ROLLOUT TESTS
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

(define (finite-diff f x epsilon)
  (/ (- (f (+ x epsilon)) (f (- x epsilon)))
     (* 2 epsilon)))

;;; ============================================================
;;; Basic Rollout Tests
;;; ============================================================

(test-group basic-rollout-tests
            
            (define-test traced-rollout-zero-steps
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 1 2) tape)]
                     [vel (lift-vec2 (vec2 3 4) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [step-fn (lambda (b) b)]
                     [final (traced-rollout body step-fn 0)])
                    ;; Should be unchanged
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos final))) 1 1e-10)))
            
            (define-test traced-rollout-single-step
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 1 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [gravity (lift-vec2-const (vec2 0 -10))]
                     [dt 0.1]
                     [step-fn (lambda (b) (traced-gravity-step b gravity dt))]
                     [final (traced-rollout body step-fn 1)])
                    ;; After 1 step: vel_y = -1, pos moves by new vel
                    (let ([final-vy (traced-value (traced-vec2-y (traced-body-vel final)))])
                         (assert-near final-vy -1 0.01))))
            
            (define-test traced-rollout-multiple-steps
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 100) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [gravity (lift-vec2-const (vec2 0 -10))]
                     [dt 0.1]
                     [step-fn (lambda (b) (traced-gravity-step b gravity dt))]
                     [final (traced-rollout body step-fn 10)])
                    ;; After 10 steps, should have fallen significantly
                    (let ([final-y (traced-value (traced-vec2-y (traced-body-pos final)))])
                         (assert-true (< final-y 100)))))
            
            (define-test traced-rollout-with-gravity-convenience
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 50) tape)]
                     [vel (lift-vec2 (vec2 10 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [gravity (lift-vec2-const (vec2 0 -9.8))]
                     [dt 0.1]
                     [n 20]
                     [final (traced-rollout-with-gravity body gravity dt n)])
                    ;; Should have moved right and down
                    (let ([final-x (traced-value (traced-vec2-x (traced-body-pos final)))]
                          [final-y (traced-value (traced-vec2-y (traced-body-pos final)))])
                         (assert-true (> final-x 0))
                         (assert-true (< final-y 50)))))
            )

;;; ============================================================
;;; Trajectory Recording Tests
;;; ============================================================

(test-group trajectory-tests
            
            (define-test trajectory-length-correct
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 1 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [step-fn (lambda (b) (traced-gravity-step b (lift-vec2-const (vec2 0 -10)) 0.1))]
                     [trajectory (traced-rollout-trajectory body step-fn 5)])
                    ;; Should have 6 states (initial + 5 steps)
                    (assert-equal (length trajectory) 6)))
            
            (define-test trajectory-first-is-initial
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 7 11) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [step-fn (lambda (b) (traced-gravity-step b (lift-vec2-const (vec2 0 -10)) 0.1))]
                     [trajectory (traced-rollout-trajectory body step-fn 3)]
                     [first-body (car trajectory)])
                    (assert-near (traced-value (traced-vec2-x (traced-body-pos first-body))) 7 1e-10)
                    (assert-near (traced-value (traced-vec2-y (traced-body-pos first-body))) 11 1e-10)))
            
            (define-test trajectory-final-helper
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [step-fn (lambda (b) (traced-gravity-step b (lift-vec2-const (vec2 0 -10)) 0.1))]
                     [trajectory (traced-rollout-trajectory body step-fn 5)]
                     [final (traced-trajectory-final trajectory)]
                     [last-direct (car (reverse trajectory))])
                    (assert-equal final last-direct)))
            
            (define-test trajectory-monotonic-fall
              ;; Verify y-position decreases under gravity
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 100) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [step-fn (lambda (b) (traced-gravity-step b (lift-vec2-const (vec2 0 -10)) 0.1))]
                     [trajectory (traced-rollout-trajectory body step-fn 10)]
                     [y-positions (map (lambda (b)
                                               (traced-value (traced-vec2-y (traced-body-pos b))))
                                       trajectory)])
                    ;; Each y should be less than or equal to previous (actually less after first step)
                    (let check-monotonic ([ys y-positions])
                         (when (and (pair? ys) (pair? (cdr ys)))
                               (assert-true (>= (car ys) (cadr ys)))
                               (check-monotonic (cdr ys))))))
            )

;;; ============================================================
;;; Checkpointing Tests
;;; ============================================================

(test-group checkpoint-tests
            
            (define-test checkpoint-interval-small
              ;; For small T, interval should be 1 or small
              (assert-true (<= (checkpoint-interval 4) 2)))
            
            (define-test checkpoint-interval-medium
              ;; For T=100, sqrt(100) = 10
              (assert-equal (checkpoint-interval 100) 10))
            
            (define-test checkpoint-interval-large
              ;; For T=10000, sqrt = 100
              (assert-equal (checkpoint-interval 10000) 100))
            
            (define-test rollout-checkpoints-stores-initial
              (let* ([initial (make-rigid-body (vec2 0 100) (vec2 0 0) 0 0 1 0.5)]
                     [step-fn (lambda (b)
                                      (let* ([new-vel (vec2-add (rigid-body-vel b)
                                                                (vec2-scale (vec2 0 -10) 0.1))]
                                             [new-pos (vec2-add (rigid-body-pos b)
                                                                (vec2-scale new-vel 0.1))])
                                            (make-rigid-body new-pos new-vel
                                                             (rigid-body-angle b)
                                                             (rigid-body-angular-vel b)
                                                             (rigid-body-mass b)
                                                             (rigid-body-inertia b))))]
                     [checkpoints (rollout-checkpoints initial step-fn 10 5)])
                    ;; First checkpoint should be initial state
                    (let ([first (vector-ref checkpoints 0)])
                         (assert-near (vec2-x (rigid-body-pos first)) 0 1e-10)
                         (assert-near (vec2-y (rigid-body-pos first)) 100 1e-10))))
            
            (define-test rollout-checkpoints-correct-count
              (let* ([initial (make-rigid-body (vec2 0 0) (vec2 1 0) 0 0 1 0.5)]
                     [step-fn (lambda (b)
                                      (make-rigid-body
                                       (vec2-add (rigid-body-pos b)
                                                 (vec2-scale (rigid-body-vel b) 0.1))
                                       (rigid-body-vel b)
                                       (rigid-body-angle b)
                                       (rigid-body-angular-vel b)
                                       (rigid-body-mass b)
                                       (rigid-body-inertia b)))]
                     [checkpoints (rollout-checkpoints initial step-fn 20 5)])
                    ;; 20 steps, checkpoint every 5 = checkpoints at 0, 5, 10, 15, 20 = 5 checkpoints
                    (assert-equal (vector-length checkpoints) 5)))
            )

;;; ============================================================
;;; Loss Function Tests
;;; ============================================================

(test-group loss-function-tests
            
            (define-test target-position-loss-zero-at-target
              (let* ([target (vec2 5 10)]
                     [loss-fn (target-position-loss target)]
                     [body (make-rigid-body target (vec2 0 0) 0 0 1 0.5)]
                     [loss (loss-fn body)])
                    (assert-near loss 0 1e-10)))
            
            (define-test target-position-loss-positive-away
              (let* ([target (vec2 0 0)]
                     [loss-fn (target-position-loss target)]
                     [body (make-rigid-body (vec2 3 4) (vec2 0 0) 0 0 1 0.5)]
                     [loss (loss-fn body)])
                    ;; Distance^2 = 9 + 16 = 25
                    (assert-near loss 25 1e-10)))
            
            (define-test target-state-loss-zero-at-target
              (let* ([target-pos (vec2 1 2)]
                     [target-vel (vec2 3 4)]
                     [target-angle 0.5]
                     [target-omega 0.1]
                     [loss-fn (target-state-loss target-pos target-vel target-angle target-omega
                                                 1 1 1 1)]
                     [body (make-rigid-body target-pos target-vel target-angle target-omega 1 0.5)]
                     [loss (loss-fn body)])
                    (assert-near loss 0 1e-10)))
            
            (define-test target-state-loss-weighted
              (let* ([target-pos (vec2 0 0)]
                     [target-vel (vec2 0 0)]
                     [target-angle 0]
                     [target-omega 0]
                     ;; Only position weight nonzero
                     [loss-fn (target-state-loss target-pos target-vel target-angle target-omega
                                                 1 0 0 0)]
                     [body (make-rigid-body (vec2 3 4) (vec2 100 100) 5 10 1 0.5)]
                     [loss (loss-fn body)])
                    ;; Only position error: 25
                    (assert-near loss 25 1e-10)))
            
            (define-test energy-minimization-loss
              (let* ([body (make-rigid-body (vec2 0 0) (vec2 3 4) 0 2 2 1)]
                     [loss (energy-minimization-loss body)])
                    ;; KE = 0.5 * 2 * 25 + 0.5 * 1 * 4 = 25 + 2 = 27
                    (assert-near loss 27 1e-10)))
            )

;;; ============================================================
;;; Projectile Loss Tests
;;; ============================================================

(test-group projectile-loss-tests
            
            (define-test projectile-hit-loss-creates-callable
              (let* ([start (vec2 0 0)]
                     [target (vec2 10 0)]
                     [gravity (vec2 0 -10)]
                     [dt 0.1]
                     [steps 10]
                     [loss-fn (projectile-hit-loss start target gravity dt steps)])
                    (assert-true (procedure? loss-fn))))
            
            (define-test projectile-hit-loss-computes-value
              (let* ([start (vec2 0 0)]
                     [target (vec2 10 0)]
                     [gravity (vec2 0 -10)]
                     [dt 0.1]
                     [steps 10]
                     [loss-fn (projectile-hit-loss start target gravity dt steps)]
                     [initial-vel (vec2 5 5)]
                     [loss (loss-fn initial-vel)])
                    (assert-true (number? loss))
                    (assert-true (>= loss 0))))
            
            (define-test projectile-loss-varies-with-velocity
              ;; Different velocities should give different losses
              (let* ([start (vec2 0 0)]
                     [target (vec2 10 0)]
                     [gravity (vec2 0 -10)]
                     [dt 0.1]
                     [steps 10]
                     [loss-fn (projectile-hit-loss start target gravity dt steps)]
                     [loss1 (loss-fn (vec2 1 1))]
                     [loss2 (loss-fn (vec2 10 10))])
                    (assert-true (not (= loss1 loss2)))))
            )

;;; ============================================================
;;; Substepping Tests
;;; ============================================================

(test-group substepping-tests
            
            (define-test substep-runs-multiple
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [counter 0]
                     [step-fn (lambda (b)
                                      (set! counter (+ counter 1))
                                      (traced-gravity-step b (lift-vec2-const (vec2 0 -10)) 0.01))]
                     ;; 2 main steps with 5 substeps each = 10 total
                     [final (traced-rollout-substep body step-fn 2 5)])
                    (assert-equal counter 10)))
            
            (define-test substep-equivalent-to-rollout
              ;; 10 steps of dt should equal 2 steps of 5 substeps
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 100) tape)]
                     [vel (lift-vec2 (vec2 0 0) tape)]
                     [angle (make-traced-var 0 tape)]
                     [omega (make-traced-var 0 tape)]
                     [body (make-traced-body pos vel angle omega 1 0.5)]
                     [gravity (lift-vec2-const (vec2 0 -10))]
                     [dt 0.1]
                     [step-fn (lambda (b) (traced-gravity-step b gravity dt))]
                     ;; Direct rollout of 10 steps
                     [final1 (traced-rollout body step-fn 10)]
                     ;; 2 main steps with 5 substeps
                     [final2 (traced-rollout-substep body step-fn 2 5)])
                    ;; Should give same result
                    (assert-near (traced-value (traced-vec2-y (traced-body-pos final1)))
                                 (traced-value (traced-vec2-y (traced-body-pos final2)))
                                 1e-6)))
            )

;;; ============================================================
;;; Point Mass Tests
;;; ============================================================

(test-group point-mass-tests
            
            (define-test traced-point-step-basic
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 0 0) tape)]
                     [vel (lift-vec2 (vec2 1 0) tape)]
                     [accel (lift-vec2-const (vec2 0 -10))]
                     [dt 0.1])
                    (call-with-values
                     (lambda () (traced-point-step pos vel accel dt))
                     (lambda (new-pos new-vel)
                             ;; new_vel = (1, -1)
                             (assert-near (traced-value (traced-vec2-x new-vel)) 1 1e-10)
                             (assert-near (traced-value (traced-vec2-y new-vel)) -1 1e-10)
                             ;; new_pos = pos + new_vel * dt = (0.1, -0.1)
                             (assert-near (traced-value (traced-vec2-x new-pos)) 0.1 1e-10)
                             (assert-near (traced-value (traced-vec2-y new-pos)) -0.1 1e-10)))))
            
            (define-test traced-projectile-step-same-as-point
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec2 (vec2 5 10) tape)]
                     [vel (lift-vec2 (vec2 2 3) tape)]
                     [gravity (lift-vec2-const (vec2 0 -9.8))]
                     [dt 0.05])
                    (call-with-values
                     (lambda () (traced-point-step pos vel gravity dt))
                     (lambda (p1 v1)
                             (call-with-values
                              (lambda () (traced-projectile-step pos vel gravity dt))
                              (lambda (p2 v2)
                                      (assert-near (traced-value (traced-vec2-x p1))
                                                   (traced-value (traced-vec2-x p2)) 1e-10)
                                      (assert-near (traced-value (traced-vec2-y p1))
                                                   (traced-value (traced-vec2-y p2)) 1e-10)))))))
            )

;;; ============================================================
;;; Trajectory Loss Tests
;;; ============================================================

(test-group trajectory-loss-tests
            
            (define-test trajectory-loss-zero-on-path
              (let* ([targets (list (vec2 0 0) (vec2 1 0) (vec2 2 0))]
                     [loss-fn (trajectory-loss targets)]
                     [trajectory (list (make-rigid-body (vec2 0 0) (vec2 0 0) 0 0 1 0.5)
                                       (make-rigid-body (vec2 1 0) (vec2 0 0) 0 0 1 0.5)
                                       (make-rigid-body (vec2 2 0) (vec2 0 0) 0 0 1 0.5))]
                     [loss (loss-fn trajectory)])
                    (assert-near loss 0 1e-10)))
            
            (define-test trajectory-loss-accumulates-errors
              (let* ([targets (list (vec2 0 0) (vec2 0 0))]
                     [loss-fn (trajectory-loss targets)]
                     ;; Both off by (3, 4) -> error = 25 each
                     [trajectory (list (make-rigid-body (vec2 3 4) (vec2 0 0) 0 0 1 0.5)
                                       (make-rigid-body (vec2 3 4) (vec2 0 0) 0 0 1 0.5))]
                     [loss (loss-fn trajectory)])
                    (assert-near loss 50 1e-10)))
            )

;;; ============================================================
;;; Gradient Through Rollout Tests
;;; ============================================================

(test-group gradient-rollout-tests
            
            (define-test gradient-flows-through-rollout
              ;; Verify gradient w.r.t. initial velocity affects final position using finite diff
              (let* ([test-fn (lambda (vx)
                                      (let* ([tape (make-reverse-tape)]
                                             [pos (lift-vec2 (vec2 0 0) tape)]
                                             [vel (traced-vec2 (make-traced-var vx tape)
                                                               (make-traced-var 0 tape))]
                                             [angle (make-traced-var 0 tape)]
                                             [omega (make-traced-var 0 tape)]
                                             [body (make-traced-body pos vel angle omega 1 0.5)]
                                             [gravity (lift-vec2-const (vec2 0 0))]  ; no gravity
                                             [dt 0.1]
                                             [final (traced-rollout-with-gravity body gravity dt 5)])
                                            (traced-value (traced-vec2-x (traced-body-pos final)))))]
                     [vx0 1]
                     [numerical (finite-diff test-fn vx0 1e-5)])
                    ;; d(final_x)/d(vx) should be approximately n * dt = 0.5
                    ;; (with symplectic Euler, it's slightly different)
                    (assert-true (> numerical 0.4))
                    (assert-true (< numerical 0.6))))
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
