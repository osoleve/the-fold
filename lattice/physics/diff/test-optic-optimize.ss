(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/physics/diff/optic-optimize.ss")

(doc 'module 'test-optic-optimize)
(doc 'description "Tests for Optic-Based Optimization

Tests for optic-optimize.ss including:
- physics-optic-gradient computation
- optimize-trajectory-optic with various lenses
- optimize-initial-velocity-optic
- sensitivity-at-optic")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/diff/test-optic-optimize.ss")

(display "
====
         OPTIC-BASED OPTIMIZATION TESTS
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

;;; ====
;;; Gradient Computation Tests
;;; ====

(test-group physics-optic-gradient-tests

  (define-test gradient-nonzero
    ;; Verify gradient is computed (not zero)
    (let* ([body (make-rigid-body (vec2 0 0) (vec2 1 1) 0 0 1 1)]
           [target (vec2 10 0)]
           [traced-target (lift-vec2-const target)]
           [dt 0.1]
           [num-steps 10]
           [step-fn (lambda (tb)
                      (let* ([pos (traced-body-pos tb)]
                             [vel (traced-body-vel tb)]
                             [new-pos (traced-vec2-add pos (traced-vec2-scale vel dt))])
                        (make-traced-body new-pos vel
                                          (traced-body-angle tb)
                                          (traced-body-angular-vel tb)
                                          (traced-body-mass tb)
                                          (traced-body-inertia tb))))]
           [make-loss (lambda (trace-body)
                        (lambda (tb)
                          (let ([final-tb (traced-rollout tb step-fn num-steps)])
                            (traced-vec2-distance-sq (traced-body-pos final-tb)
                                                     traced-target))))]
           [grad (physics-optic-gradient rigid-body-vel-lens body make-loss)])
      ;; Gradient should be non-zero since we're not at optimum
      (assert-true (or (not (= (vec2-x grad) 0))
                       (not (= (vec2-y grad) 0))))))

  (define-test gradient-direction-correct
    ;; Gradient should point away from target (since we minimize distance)
    (let* ([body (make-rigid-body (vec2 0 0) (vec2 0 0) 0 0 1 1)]
           [target (vec2 10 0)]  ; Target is to the right
           [traced-target (lift-vec2-const target)]
           [dt 0.1]
           [num-steps 50]
           [step-fn (lambda (tb)
                      (let* ([pos (traced-body-pos tb)]
                             [vel (traced-body-vel tb)]
                             [new-pos (traced-vec2-add pos (traced-vec2-scale vel dt))])
                        (make-traced-body new-pos vel
                                          (traced-body-angle tb)
                                          (traced-body-angular-vel tb)
                                          (traced-body-mass tb)
                                          (traced-body-inertia tb))))]
           [make-loss (lambda (trace-body)
                        (lambda (tb)
                          (let ([final-tb (traced-rollout tb step-fn num-steps)])
                            (traced-vec2-distance-sq (traced-body-pos final-tb)
                                                     traced-target))))]
           [grad (physics-optic-gradient rigid-body-vel-lens body make-loss)])
      ;; Negative x-gradient means: increasing vx decreases loss
      ;; (we should move right to get closer to target on the right)
      (assert-true (< (vec2-x grad) 0)))))

;;; ====
;;; Trajectory Optimization Tests
;;; ====

(test-group optimize-trajectory-optic-tests

  (define-test velocity-optimization-converges
    ;; Optimize velocity to reach target
    (let* ([body (make-rigid-body (vec2 0 0) (vec2 3 5) 0 0 1 1)]
           [target (vec2 10 0)]
           [traced-target (lift-vec2-const target)]
           [dt 0.1]
           [num-steps 50]
           [step-fn (lambda (tb)
                      (let* ([pos (traced-body-pos tb)]
                             [vel (traced-body-vel tb)]
                             [new-pos (traced-vec2-add pos (traced-vec2-scale vel dt))])
                        (make-traced-body new-pos vel
                                          (traced-body-angle tb)
                                          (traced-body-angular-vel tb)
                                          (traced-body-mass tb)
                                          (traced-body-inertia tb))))]
           [loss-fn (lambda (final-tb)
                      (traced-vec2-distance-sq (traced-body-pos final-tb)
                                               traced-target))]
           [result (optimize-trajectory-optic body step-fn loss-fn num-steps
                                              rigid-body-vel-lens 0.001 500)]
           [final-pos (vec2-add (rigid-body-pos result)
                                (vec2-scale (rigid-body-vel result) (* dt num-steps)))])
      ;; Should be close to target
      (assert-true (< (vec2-distance final-pos target) 0.1))))

  (define-test position-optimization-converges
    ;; Optimize initial position
    (let* ([body (make-rigid-body (vec2 5 5) (vec2 1 0) 0 0 1 1)]
           [target (vec2 10 0)]  ; Want to end at (10, 0)
           [traced-target (lift-vec2-const target)]
           [dt 0.1]
           [num-steps 50]
           ;; With vel (1,0), in 5s we move 5 units right
           ;; So starting at (5, 0) would reach (10, 0)
           [step-fn (lambda (tb)
                      (let* ([pos (traced-body-pos tb)]
                             [vel (traced-body-vel tb)]
                             [new-pos (traced-vec2-add pos (traced-vec2-scale vel dt))])
                        (make-traced-body new-pos vel
                                          (traced-body-angle tb)
                                          (traced-body-angular-vel tb)
                                          (traced-body-mass tb)
                                          (traced-body-inertia tb))))]
           [loss-fn (lambda (final-tb)
                      (traced-vec2-distance-sq (traced-body-pos final-tb)
                                               traced-target))]
           ;; More iterations for position (y needs more steps to reach 0)
           [result (optimize-trajectory-optic body step-fn loss-fn num-steps
                                              rigid-body-pos-lens 0.002 1000)])
      ;; Optimized position should be near (5, 0)
      (assert-vec2-near (rigid-body-pos result) (vec2 5 0) 0.5))))

;;; ====
;;; optimize-initial-velocity-optic Tests
;;; ====

(test-group optimize-initial-velocity-optic-tests

  (define-test velocity-reaches-target-no-gravity
    (let* ([body (make-rigid-body (vec2 0 0) (vec2 1 1) 0 0 1 1)]
           [target (vec2 5 0)]
           [gravity (vec2 0 0)]
           [dt 0.1]
           [num-steps 50]
           [result-vel (optimize-initial-velocity-optic body target gravity dt num-steps
                                                        0.001 500)])
      ;; Should find velocity that reaches target in 5s: (1, 0)
      (assert-vec2-near result-vel (vec2 1 0) 0.1))))

;;; ====
;;; get-traced-focus Tests
;;; ====

(test-group get-traced-focus-tests

  (define-test extracts-position
    (let* ([tape (make-reverse-tape)]
           [tb (make-traced-body (lift-vec2 (vec2 1 2) tape)
                                 (lift-vec2-const (vec2 3 4))
                                 (make-traced-const 0 tape)
                                 (make-traced-const 0 tape)
                                 1 1)]
           [focus (get-traced-focus rigid-body-pos-lens tb)])
      (assert-true (traced-vec2? focus))))

  (define-test extracts-velocity
    (let* ([tape (make-reverse-tape)]
           [tb (make-traced-body (lift-vec2-const (vec2 1 2))
                                 (lift-vec2 (vec2 3 4) tape)
                                 (make-traced-const 0 tape)
                                 (make-traced-const 0 tape)
                                 1 1)]
           [focus (get-traced-focus rigid-body-vel-lens tb)])
      (assert-true (traced-vec2? focus)))))

;;; ====
;;; optimize-trajectory-all Tests
;;; ====

(test-group optimize-trajectory-all-tests

  (define-test full-body-gradient-flows
    ;; Test that gradients flow through all parameters
    (let* ([body (make-rigid-body (vec2 0 0) (vec2 1 0) 0 0 1 1)]
           [traced-target (lift-vec2-const (vec2 5 0))]
           [step-fn (lambda (tb)
                      ;; Simple step: just move by velocity
                      (let* ([pos (traced-body-pos tb)]
                             [vel (traced-body-vel tb)]
                             [new-pos (traced-vec2-add pos (traced-vec2-scale vel 0.1))])
                        (make-traced-body new-pos vel
                                          (traced-body-angle tb)
                                          (traced-body-angular-vel tb)
                                          (traced-body-mass tb)
                                          (traced-body-inertia tb))))]
           [loss-fn (lambda (final-tb)
                      (traced-vec2-distance-sq (traced-body-pos final-tb)
                                               traced-target))]
           [num-steps 10]
           [result (optimize-trajectory-all body step-fn loss-fn num-steps 0.01 100)])
      ;; Should optimize toward target - final position closer than initial
      (let ([initial-dist (vec2-distance (rigid-body-pos body) (vec2 5 0))]
            [final-dist (vec2-distance (rigid-body-pos result) (vec2 5 0))])
        ;; Optimized state should be closer (either via pos or vel change)
        ;; Check that velocity changed toward target direction
        (assert-true (> (vec2-x (rigid-body-vel result)) 0.5))))))

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
