(load "core/test-framework.ss")
(load "lattice/physics/classical3d/rigid-body3d.ss")
(load "lattice/physics/diff3d/optimize3d.ss")

(doc 'module 'test-rollout3d)
(doc 'description "Tests for rollout3d and optimize3d")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/diff3d/test-rollout3d.ss")

(display "\n")
(display "====\n")
(display "         ROLLOUT 3D & OPTIMIZE 3D TESTS\n")
(display "====\n")

;;; ====
;;; Test Helpers
;;; ====

(define *test-dt* 0.01)
(define *test-gravity* (vec3 0 -9.8 0))

;;; Make a simple traced body for testing
(define (make-test-body-3d tape pos vel)
  (make-traced-body-3d
   (lift-vec3 pos tape)
   (lift-vec3 vel tape)
   (lift-quat (quat-identity) tape)
   (lift-vec3 (vec3 0 0 0) tape)
   1.0
   (inertia-solid-sphere 1.0 1.0)))

;;; ====
;;; Basic Rollout Tests
;;; ====

(test-group basic-rollout-tests

            (define-test rollout-zero-steps
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 5 0) (vec3 0 0 0))]
                     [final (traced-rollout-3d body (lambda (b) b) 0)])
                    ;; Should return same body
                    (assert-equal 0 (traced-value (traced-vec3-x (traced-body-3d-pos final))))
                    (assert-equal 5 (traced-value (traced-vec3-y (traced-body-3d-pos final))))))

            (define-test rollout-with-gravity
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 10 0) (vec3 0 0 0))]
                     [gravity (lift-vec3-const *test-gravity*)]
                     [final (traced-rollout-with-gravity-3d body gravity *test-dt* 10)])
                    ;; Should have fallen (y decreased)
                    (assert-true (< (traced-value (traced-vec3-y (traced-body-3d-pos final))) 10))))

            (define-test rollout-preserves-x-with-no-force
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 5 0 0) (vec3 1 0 0))]
                     [zero-gravity (lift-vec3-const (vec3 0 0 0))]
                     [final (traced-rollout-with-gravity-3d body zero-gravity *test-dt* 10)])
                    ;; X should have increased (moving with velocity)
                    (assert-true (> (traced-value (traced-vec3-x (traced-body-3d-pos final))) 5)))))

;;; ====
;;; Trajectory Recording Tests
;;; ====

(test-group trajectory-tests

            (define-test trajectory-length
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 0 0) (vec3 0 0 0))]
                     [traj (traced-rollout-trajectory-3d body (lambda (b) b) 5)])
                    ;; Should have 6 entries (initial + 5 steps)
                    (assert-equal 6 (length traj))))

            (define-test trajectory-final
              (let* ([tape (make-reverse-tape)]
                     [body (make-test-body-3d tape (vec3 0 10 0) (vec3 0 0 0))]
                     [gravity (lift-vec3-const *test-gravity*)]
                     [traj (traced-rollout-trajectory-3d
                            body
                            (lambda (b) (traced-gravity-step-3d b gravity *test-dt*))
                            5)]
                     [final (traced-trajectory-final-3d traj)])
                    ;; Final should have lower y than initial
                    (assert-true (< (traced-value (traced-vec3-y (traced-body-3d-pos final)))
                                    (traced-value (traced-vec3-y (traced-body-3d-pos (car traj)))))))))

;;; ====
;;; Checkpointing Tests
;;; ====

(test-group checkpoint-tests

            (define-test checkpoint-interval-calculation
              ;; sqrt(100) = 10
              (assert-equal 10 (checkpoint-interval-3d 100))
              ;; sqrt(25) = 5
              (assert-equal 5 (checkpoint-interval-3d 25)))

            (define-test rollout-checkpoints
              (let* ([body (make-rigid-body-3d (vec3 0 10 0) (vec3 0 0 0)
                                               (quat-identity) (vec3 0 0 0)
                                               1.0 (inertia-solid-sphere 1.0 1.0))]
                     [step-fn (lambda (b)
                                      (rigid-body-3d-with-pos
                                       b
                                       (vec3-add (rigid-body-3d-pos b) (vec3 0 -0.1 0))))]
                     [checkpoints (rollout-checkpoints-3d body step-fn 10 5)])
                    ;; Should have 3 checkpoints (0, 5, 10)
                    (assert-true (>= (vector-length checkpoints) 2)))))

;;; ====
;;; Loss Function Tests
;;; ====

(test-group loss-function-tests

            (define-test target-position-loss
              (let* ([target (vec3 10 0 0)]
                     [loss-fn (target-position-loss-3d target)]
                     [body-at-target (make-rigid-body-3d target (vec3 0 0 0)
                                                         (quat-identity) (vec3 0 0 0)
                                                         1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-away (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0)
                                                    (quat-identity) (vec3 0 0 0)
                                                    1.0 (inertia-solid-sphere 1.0 1.0))])
                    ;; At target should have ~zero loss
                    (assert-true (< (loss-fn body-at-target) 0.001))
                    ;; Away should have positive loss
                    (assert-true (> (loss-fn body-away) 0))))

            (define-test target-orientation-loss
              (let* ([target (quat-identity)]
                     [loss-fn (target-orientation-loss-3d target)]
                     [body-aligned (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0)
                                                       target (vec3 0 0 0)
                                                       1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-rotated (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0)
                                                       (quat-from-axis-angle (vec3 1 0 0) 1.57)
                                                       (vec3 0 0 0)
                                                       1.0 (inertia-solid-sphere 1.0 1.0))])
                    ;; Aligned should have ~zero loss
                    (assert-true (< (loss-fn body-aligned) 0.001))
                    ;; Rotated should have positive loss
                    (assert-true (> (loss-fn body-rotated) 0))))

            (define-test energy-minimization-loss
              (let* ([body-at-rest (make-rigid-body-3d (vec3 0 0 0) (vec3 0 0 0)
                                                       (quat-identity) (vec3 0 0 0)
                                                       1.0 (inertia-solid-sphere 1.0 1.0))]
                     [body-moving (make-rigid-body-3d (vec3 0 0 0) (vec3 10 0 0)
                                                      (quat-identity) (vec3 0 0 0)
                                                      1.0 (inertia-solid-sphere 1.0 1.0))])
                    ;; At rest should have zero energy
                    (assert-equal 0 (energy-minimization-loss-3d body-at-rest))
                    ;; Moving should have positive energy
                    (assert-true (> (energy-minimization-loss-3d body-moving) 0)))))

;;; ====
;;; Optimization Tests
;;; ====

(test-group optimization-tests

            (define-test gradient-descent-step
              (let* ([params '(1.0 2.0 3.0)]
                     [grads '(0.1 0.2 0.3)]
                     [lr 0.5]
                     [new-params (gradient-descent-step-3d params grads lr)])
                    ;; x - lr*g = 1 - 0.5*0.1 = 0.95
                    (assert-true (< (abs (- (car new-params) 0.95)) 0.001))))

            (define-test simple-gradient-descent
              ;; Minimize x^2 + y^2 starting from (1, 1) using traced arithmetic
              (let* ([loss-fn (lambda (params)
                                      ;; Must use traced arithmetic
                                      (let ([x (car params)]
                                            [y (cadr params)])
                                           (traced-add (traced-mul x x) (traced-mul y y))))]
                     [initial '(1.0 1.0)]
                     [result (gradient-descent-3d loss-fn initial 0.1 100)])
                    ;; Should converge close to (0, 0)
                    (assert-true (< (abs (car result)) 0.1))
                    (assert-true (< (abs (cadr result)) 0.1))))

            (define-test adam-converges
              ;; Minimize x^2 + y^2 + z^2 using traced arithmetic
              (let* ([loss-fn (lambda (params)
                                      (let ([x (list-ref params 0)]
                                            [y (list-ref params 1)]
                                            [z (list-ref params 2)])
                                           (traced-add (traced-add (traced-mul x x) (traced-mul y y))
                                                       (traced-mul z z))))]
                     [initial '(1.0 1.0 1.0)]
                     [result (adam-3d loss-fn initial 0.1 100)])
                    ;; Should converge close to (0, 0, 0)
                    (assert-true (< (abs (list-ref result 0)) 0.2))
                    (assert-true (< (abs (list-ref result 1)) 0.2))
                    (assert-true (< (abs (list-ref result 2)) 0.2)))))

;;; ====
;;; Point Mass Simulation Tests
;;; ====

(test-group point-mass-tests

            (define-test point-step
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 1 0 0) tape)]
                     [gravity (lift-vec3-const (vec3 0 0 0))]
                     [dt 0.1])
                    (call-with-values
                     (lambda () (traced-point-step-3d pos vel gravity dt))
                     (lambda (new-pos new-vel)
                             ;; Position should advance by velocity * dt
                             (assert-true (> (traced-value (traced-vec3-x new-pos)) 0))))))

            (define-test projectile-motion
              (let* ([tape (make-reverse-tape)]
                     [pos (lift-vec3 (vec3 0 0 0) tape)]
                     [vel (lift-vec3 (vec3 10 20 0) tape)]  ; Launch upward and forward
                     [gravity (lift-vec3-const (vec3 0 -10 0))]
                     [dt 0.1]
                     [final-pos (let loop ([p pos] [v vel] [i 0])
                                     (if (>= i 10)
                                         p
                                         (call-with-values
                                          (lambda () (traced-point-step-3d p v gravity dt))
                                          (lambda (np nv) (loop np nv (+ i 1))))))])
                    ;; Should have moved forward in X
                    (assert-true (> (traced-value (traced-vec3-x final-pos)) 0)))))

;;; ====
;;; Utility Function Tests
;;; ====

(test-group utility-tests

            (define-test list-update
              (let ([lst '(1 2 3 4 5)]
                    [updated (list-update-3d '(1 2 3 4 5) 2 (lambda (x) (* x 10)))])
                   (assert-equal '(1 2 30 4 5) updated)))

            (define-test iota-generation
              (assert-equal '(0 1 2 3 4) (iota-3d 5))
              (assert-equal '() (iota-3d 0))))

;;; ====
;;; Run all tests
;;; ====

(run-all-tests)

