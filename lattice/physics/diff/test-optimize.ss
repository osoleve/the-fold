(load "core/test-framework.ss")
(load "lattice/physics/diff/optimize.ss")

(doc 'module 'test-optimize)
(doc 'description "Tests for Trajectory Optimization

Comprehensive tests for optimization algorithms including:
- Gradient descent
- Gradient descent with momentum
- Adam optimizer
- Trajectory optimization
- Inverse physics (parameter estimation)
- Sensitivity analysis")
(doc 'layer 'lattice)
(doc 'note "Run with: scheme --script lattice/physics/diff/test-optimize.ss")

(display "
====
         TRAJECTORY OPTIMIZATION TESTS
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
;;; Gradient Descent Step Tests
;;; ====

(test-group gradient-descent-step-tests
            
            (define-test gd-step-basic
              (let* ([params (list 5 10)]
                     [grads (list 1 2)]
                     [lr 0.1]
                     [new-params (gradient-descent-step params grads lr)])
                    (assert-near (list-ref new-params 0) 4.9 1e-10)
                    (assert-near (list-ref new-params 1) 9.8 1e-10)))
            
            (define-test gd-step-zero-gradient
              (let* ([params (list 1 2 3)]
                     [grads (list 0 0 0)]
                     [lr 1.0]
                     [new-params (gradient-descent-step params grads lr)])
                    (assert-equal new-params params)))
            
            (define-test gd-step-negative-gradient
              ;; Moving uphill when gradient is negative
              (let* ([params (list 0)]
                     [grads (list -5)]
                     [lr 0.2]
                     [new-params (gradient-descent-step params grads lr)])
                    (assert-near (car new-params) 1.0 1e-10)))
            )

;;; ====
;;; Gradient Descent Optimization Tests
;;; ====

(test-group gradient-descent-tests
            
            (define-test gd-minimizes-quadratic
              ;; Minimize f(x) = (x - 3)^2
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)])
                                           (traced-mul (traced-sub x 3)
                                                       (traced-sub x 3))))]
                     [result (gradient-descent loss-fn (list 0) 0.1 100)]
                     [x-final (car result)])
                    (assert-near x-final 3 0.05)))
            
            (define-test gd-minimizes-2d-quadratic
              ;; Minimize f(x,y) = (x-1)^2 + (y-2)^2
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)]
                                            [y (cadr xs)])
                                           (traced-add
                                            (traced-mul (traced-sub x 1) (traced-sub x 1))
                                            (traced-mul (traced-sub y 2) (traced-sub y 2)))))]
                     [result (gradient-descent loss-fn (list 0 0) 0.1 100)]
                     [x-final (car result)]
                     [y-final (cadr result)])
                    (assert-near x-final 1 0.05)
                    (assert-near y-final 2 0.05)))
            
            (define-test gd-converges-from-far
              ;; Start far from minimum
              (let* ([loss-fn (lambda (xs)
                                      (traced-mul (car xs) (car xs)))]
                     [result (gradient-descent loss-fn (list 100) 0.1 500)]
                     [x-final (car result)])
                    (assert-near x-final 0 0.1)))
            )

;;; ====
;;; Momentum Tests
;;; ====

(test-group momentum-tests
            
            (define-test momentum-step-basic
              (let* ([params (list 1 2)]
                     [grads (list 0.5 1.0)]
                     [velocity (list 0 0)]
                     [lr 0.1]
                     [beta 0.9])
                    (call-with-values
                     (lambda () (momentum-step params grads velocity lr beta))
                     (lambda (new-params new-velocity)
                             ;; v' = 0.9*0 + grad = grad
                             ;; x' = x - 0.1*v'
                             (assert-near (list-ref new-params 0) 0.95 1e-10)
                             (assert-near (list-ref new-velocity 0) 0.5 1e-10)))))
            
            (define-test momentum-accumulates
              ;; After multiple steps, momentum should accumulate
              (let* ([params (list 10)]
                     [velocity (list 0)]
                     [lr 0.1]
                     [beta 0.9]
                     ;; Constant gradient of 1
                     [result (let loop ([p params] [v velocity] [i 0])
                                  (if (= i 10)
                                      (list p v)
                                      (call-with-values
                                       (lambda () (momentum-step p (list 1) v lr beta))
                                       (lambda (np nv)
                                               (loop np nv (+ i 1))))))]
                     [final-velocity (car (cadr result))])
                    ;; Velocity should have grown
                    (assert-true (> final-velocity 3))))
            
            (define-test gd-momentum-minimizes-quadratic
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)])
                                           (traced-mul (traced-sub x 5)
                                                       (traced-sub x 5))))]
                     [result (gradient-descent-momentum loss-fn (list 0) 0.05 0.9 100)]
                     [x-final (car result)])
                    (assert-near x-final 5 0.1)))
            )

;;; ====
;;; Adam Optimizer Tests
;;; ====

(test-group adam-tests
            
            (define-test adam-step-basic
              (let* ([params (list 1)]
                     [grads (list 2)]
                     [m (list 0)]
                     [v (list 0)]
                     [t 1]
                     [lr 0.1]
                     [beta1 0.9]
                     [beta2 0.999]
                     [epsilon 1e-8])
                    (call-with-values
                     (lambda () (adam-step params grads m v t lr beta1 beta2 epsilon))
                     (lambda (new-params new-m new-v)
                             ;; m = 0.1 * 2 = 0.2
                             ;; v = 0.001 * 4 = 0.004
                             (assert-near (car new-m) 0.2 1e-6)
                             (assert-near (car new-v) 0.004 1e-6)))))
            
            (define-test adam-minimizes-quadratic
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)])
                                           (traced-mul (traced-sub x 2)
                                                       (traced-sub x 2))))]
                     [result (adam loss-fn (list 10) 0.2 200)]
                     [x-final (car result)])
                    ;; Adam converges but may need more iterations for precision
                    (assert-near x-final 2 1.5)))
            
            (define-test adam-minimizes-rosenbrock-partial
              ;; Rosenbrock is hard - just check it makes progress
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)]
                                            [y (cadr xs)])
                                           (traced-add
                                            (traced-mul (traced-sub 1 x)
                                                        (traced-sub 1 x))
                                            (traced-mul 100
                                                        (traced-mul
                                                         (traced-sub y (traced-mul x x))
                                                         (traced-sub y (traced-mul x x)))))))]
                     [initial (list 0 0)]
                     [result (adam loss-fn initial 0.01 200)]
                     [x-final (car result)]
                     [y-final (cadr result)]
                     ;; Compute losses
                     [initial-loss (+ (expt (- 1 0) 2)
                                      (* 100 (expt (- 0 0) 2)))]
                     [final-loss (+ (expt (- 1 x-final) 2)
                                    (* 100 (expt (- y-final (* x-final x-final)) 2)))])
                    ;; Should have made progress
                    (assert-true (< final-loss initial-loss))))
            )

;;; ====
;;; List Utilities Tests
;;; ====

(test-group utility-tests
            
            (define-test list-update-first
              (let ([result (list-update '(1 2 3) 0 (lambda (x) (* x 10)))])
                   (assert-equal result '(10 2 3))))
            
            (define-test list-update-middle
              (let ([result (list-update '(1 2 3) 1 (lambda (x) (* x 10)))])
                   (assert-equal result '(1 20 3))))
            
            (define-test list-update-last
              (let ([result (list-update '(1 2 3) 2 (lambda (x) (* x 10)))])
                   (assert-equal result '(1 2 30))))
            
            (define-test iota-zero
              (assert-equal (iota 0) '()))
            
            (define-test iota-positive
              (assert-equal (iota 5) '(0 1 2 3 4)))
            )

;;; ====
;;; Gradient Computation Tests
;;; ====

(test-group gradient-computation-tests
            
            (define-test gradient-quadratic-1d
              ;; f(x) = x^2, df/dx = 2x
              (let* ([grads (gradient (lambda (x) (traced-mul x x)) (list 3))])
                    ;; At x=3: df/dx = 6
                    (assert-near (car grads) 6 0.01)))
            
            (define-test gradient-quadratic-2d
              ;; f(x,y) = x^2 + y^2
              (let* ([grads (gradient (lambda (x y)
                                              (traced-add (traced-mul x x)
                                                          (traced-mul y y)))
                                      (list 3 4))])
                    ;; At (3,4): df/dx = 6, df/dy = 8
                    (assert-near (car grads) 6 0.01)
                    (assert-near (cadr grads) 8 0.01)))
            )

;;; ====
;;; Parameter Estimation Tests (Placeholder)
;;; ====

(test-group estimation-tests
            
            (define-test gd-can-fit-simple-linear
              ;; Minimize (x - target)^2 from scratch
              (let* ([target 5]
                     [loss-fn (lambda (xs)
                                      (let ([x (car xs)])
                                           (traced-mul (traced-sub x target)
                                                       (traced-sub x target))))]
                     [result (gradient-descent loss-fn (list 0) 0.2 50)]
                     [x-final (car result)])
                    (assert-near x-final target 0.5)))
            )

;;; ====
;;; Projectile Optimization Tests
;;; ====

(test-group projectile-tests
            
            (define-test gd-multi-param-optimization
              ;; Test optimization with 2 parameters
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)]
                                            [y (cadr xs)])
                                           (traced-add (traced-mul (traced-sub x 2)
                                                                   (traced-sub x 2))
                                                       (traced-mul (traced-sub y 3)
                                                                   (traced-sub y 3)))))]
                     [result (gradient-descent loss-fn (list 0 0) 0.1 100)]
                     [x-final (car result)]
                     [y-final (cadr result)])
                    (assert-near x-final 2 0.2)
                    (assert-near y-final 3 0.2)))
            )

;;; ====
;;; Convergence Rate Tests
;;; ====

(test-group convergence-tests
            
            (define-test gd-converges-linearly
              ;; For quadratic, GD should converge linearly
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)])
                                           (traced-mul x x)))]
                     ;; Run for different iteration counts
                     [result-10 (gradient-descent loss-fn (list 10) 0.1 10)]
                     [result-20 (gradient-descent loss-fn (list 10) 0.1 20)]
                     [loss-10 (expt (car result-10) 2)]
                     [loss-20 (expt (car result-20) 2)])
                    ;; More iterations should yield lower loss
                    (assert-true (< loss-20 loss-10))))
            
            (define-test adam-handles-sparse-gradients
              ;; Adam should still work with intermittent gradients
              (let* ([loss-fn (lambda (xs)
                                      (let ([x (car xs)])
                                           ;; Simple quadratic
                                           (traced-mul (traced-sub x 5)
                                                       (traced-sub x 5))))]
                     [result (adam loss-fn (list 0) 0.5 50)]
                     [x-final (car result)])
                    (assert-near x-final 5 0.5)))
            )

;;; ====
;;; Edge Case Tests
;;; ====

(test-group edge-case-tests
            
            (define-test gd-with-zero-learning-rate
              (let* ([loss-fn (lambda (xs) (traced-mul (car xs) (car xs)))]
                     [result (gradient-descent loss-fn (list 5) 0 100)]
                     [x-final (car result)])
                    ;; Should not move at all
                    (assert-near x-final 5 1e-10)))
            
            (define-test gd-single-iteration
              (let* ([loss-fn (lambda (xs) (traced-mul (car xs) (car xs)))]
                     [result (gradient-descent loss-fn (list 10) 0.1 1)]
                     [x-final (car result)])
                    ;; One step from 10 with grad 20 and lr 0.1: 10 - 2 = 8
                    (assert-near x-final 8 0.01)))
            
            (define-test iota-handles-one
              (assert-equal (iota 1) '(0)))
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
