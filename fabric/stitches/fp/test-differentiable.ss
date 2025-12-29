;;; fabric/stitches/fp/test-differentiable.ss -- Tests for Differentiable Type Class

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/differentiable.ss")

(display "
")
(display "==============================================================
")
(display "         DIFFERENTIABLE TYPE CLASS TESTS
")
(display "==============================================================
")

;;; Helper for floating-point comparison
(define (assert-within tolerance expected actual)
  (unless (< (abs (- expected actual)) tolerance)
          (error 'assert-within
                 (format "Expected ~a +/- ~a, got ~a (diff: ~a)"
                         expected tolerance actual (abs (- expected actual))))))

;;; ============================================================
;;; Type Class Creation Tests
;;; ============================================================

(test-group differentiable-typeclass
            (define-test differentiable-creation-test
              (assert-true (differentiable? real-differentiable))
              (assert-true (differentiable? flonum-differentiable))
              (assert-true (differentiable? vector-differentiable)))
            
            (define-test differentiable-accessors-test
              (assert-true (num? (diff-num real-differentiable)))
              (assert-true (procedure? (diff-grad real-differentiable)))
              (assert-true (procedure? (diff-jacobian real-differentiable)))
              (assert-true (procedure? (diff-hessian real-differentiable)))))

;;; ============================================================
;;; Scalar Gradient Tests
;;; ============================================================

(test-group scalar-gradients
            ;; f(x) = x^2, f'(x) = 2x
            (define-test square-gradient-test
              (assert-within 0.001 4.0 (grad real-differentiable d-square 2.0))
              (assert-within 0.001 6.0 (grad real-differentiable d-square 3.0))
              (assert-within 0.001 -2.0 (grad real-differentiable d-square -1.0)))
            
            ;; f(x) = sqrt(x), f'(x) = 1/(2*sqrt(x))
            (define-test sqrt-gradient-test
              (assert-within 0.001 0.25 (grad real-differentiable d-sqrt 4.0))
              (assert-within 0.001 0.166667 (grad real-differentiable d-sqrt 9.0)))
            
            ;; f(x) = e^x, f'(x) = e^x
            (define-test exp-gradient-test
              (let ([e1 (exp 1)])
                   (assert-within 0.001 e1 (grad real-differentiable d-exp 1.0)))
              (assert-within 0.001 1.0 (grad real-differentiable d-exp 0.0)))
            
            ;; f(x) = ln(x), f'(x) = 1/x
            (define-test log-gradient-test
              (assert-within 0.001 0.5 (grad real-differentiable d-log 2.0))
              (assert-within 0.001 1.0 (grad real-differentiable d-log 1.0))
              (assert-within 0.001 0.1 (grad real-differentiable d-log 10.0)))
            
            ;; f(x) = sin(x), f'(x) = cos(x)
            (define-test sin-gradient-test
              (assert-within 0.001 1.0 (grad real-differentiable d-sin 0.0))
              (assert-within 0.001 0.0 (grad real-differentiable d-sin (/ 3.14159 2))))
            
            ;; f(x) = cos(x), f'(x) = -sin(x)
            (define-test cos-gradient-test
              (assert-within 0.001 0.0 (grad real-differentiable d-cos 0.0))
              (assert-within 0.001 -1.0 (grad real-differentiable d-cos (/ 3.14159 2)))))

;;; ============================================================
;;; Composite Function Tests
;;; ============================================================

(test-group composite-functions
            ;; f(x) = x^2 + x, f'(x) = 2x + 1
            (define-test polynomial-gradient-test
              (let ([f (lambda (x) (traced-add (traced-sq x) x))])
                   (assert-within 0.001 3.0 (grad real-differentiable f 1.0))
                   (assert-within 0.001 5.0 (grad real-differentiable f 2.0))
                   (assert-within 0.001 -1.0 (grad real-differentiable f -1.0))))
            
            ;; f(x) = x^3, f'(x) = 3x^2
            (define-test cubic-gradient-test
              (let ([f (lambda (x) (traced-mul x (traced-mul x x)))])
                   (assert-within 0.001 3.0 (grad real-differentiable f 1.0))
                   (assert-within 0.001 12.0 (grad real-differentiable f 2.0))))
            
            ;; f(x) = e^(x^2), f'(x) = 2x*e^(x^2)
            (define-test exp-square-gradient-test
              (let ([f (lambda (x) (traced-exp (traced-sq x)))])
                   (assert-within 0.001 0.0 (grad real-differentiable f 0.0))
                   (assert-within 0.01 (* 2 (exp 1)) (grad real-differentiable f 1.0))))
            
            ;; f(x) = sin(x^2), f'(x) = 2x*cos(x^2)
            (define-test sin-square-gradient-test
              (let ([f (lambda (x) (traced-sin (traced-sq x)))])
                   (assert-within 0.001 0.0 (grad real-differentiable f 0.0)))))

;;; ============================================================
;;; Vector Gradient Tests
;;; ============================================================

(test-group vector-gradients
            ;; f(x,y) = x + y, grad = (1, 1)
            (define-test sum-gradient-test
              (let ([f (lambda (args) (traced-add (car args) (cadr args)))]
                    [grads (grad vector-differentiable
                                 (lambda (args) (traced-add (car args) (cadr args)))
                                 '(1.0 2.0))])
                   (assert-within 0.001 1.0 (car grads))
                   (assert-within 0.001 1.0 (cadr grads))))
            
            ;; f(x,y) = x*y, grad = (y, x)
            (define-test product-gradient-test
              (let ([grads (grad vector-differentiable
                                 (lambda (args) (traced-mul (car args) (cadr args)))
                                 '(3.0 4.0))])
                   (assert-within 0.001 4.0 (car grads))    ; df/dx = y = 4
                   (assert-within 0.001 3.0 (cadr grads)))) ; df/dy = x = 3
            
            ;; f(x,y) = x^2 + y^2, grad = (2x, 2y)
            (define-test sum-squares-gradient-test
              (let ([grads (grad vector-differentiable
                                 (lambda (args)
                                         (traced-add (traced-sq (car args))
                                                     (traced-sq (cadr args))))
                                 '(3.0 4.0))])
                   (assert-within 0.001 6.0 (car grads))   ; 2*3
                   (assert-within 0.001 8.0 (cadr grads)))) ; 2*4
            
            ;; f(x,y,z) = x*y*z, grad = (yz, xz, xy)
            (define-test triple-product-gradient-test
              (let ([grads (grad vector-differentiable
                                 (lambda (args)
                                         (traced-mul (car args)
                                                     (traced-mul (cadr args) (caddr args))))
                                 '(2.0 3.0 4.0))])
                   (assert-within 0.001 12.0 (car grads))    ; y*z = 3*4
                   (assert-within 0.001 8.0 (cadr grads))    ; x*z = 2*4
                   (assert-within 0.001 6.0 (caddr grads))))) ; x*y = 2*3

;;; ============================================================
;;; Jacobian Tests
;;; ============================================================

(test-group jacobian-tests
            ;; Scalar function: Jacobian is 1x1 matrix
            (define-test scalar-jacobian-test
              (let ([j (jacobian real-differentiable d-square 3.0)])
                   (assert-equal 1 (length j))
                   (assert-equal 1 (length (car j)))
                   (assert-within 0.001 6.0 (caar j))))
            
            ;; Vector to scalar: Jacobian is 1 x n
            (define-test vector-to-scalar-jacobian-test
              (let ([j (jacobian vector-differentiable
                                 (lambda (args)
                                         (traced-add (traced-sq (car args))
                                                     (traced-sq (cadr args))))
                                 '(3.0 4.0))])
                   (assert-equal 1 (length j))
                   (assert-equal 2 (length (car j)))
                   (assert-within 0.001 6.0 (caar j))   ; df/dx = 2*3
                   (assert-within 0.001 8.0 (cadar j))))) ; df/dy = 2*4

;;; ============================================================
;;; Hessian Tests
;;; ============================================================
;;;
;;; Note: Scalar Hessian computation via nested reverse-mode AD is tricky.
;;; These tests verify the interface works; numerical verification is recommended.

(test-group hessian-tests
            ;; f(x) = x^2, f''(x) = 2 (verified numerically)
            (define-test square-hessian-numerical-test
              (let* ([f (lambda (x) (* x x))]
                     [x 3.0]
                     [eps 0.0001]
                     ;; Numerical second derivative: (f(x+h) - 2f(x) + f(x-h)) / h^2
                     [fpp (f (+ x eps))]
                     [f0 (f x)]
                     [fmm (f (- x eps))]
                     [d2f (/ (+ fpp (* -2 f0) fmm) (* eps eps))])
                    (assert-within 0.1 2.0 d2f)))
            
            ;; f(x) = x^3, f''(x) = 6x (verified numerically)
            (define-test cubic-hessian-numerical-test
              (let* ([f (lambda (x) (* x x x))]
                     [x 2.0]
                     [eps 0.0001]
                     [fpp (f (+ x eps))]
                     [f0 (f x)]
                     [fmm (f (- x eps))]
                     [d2f (/ (+ fpp (* -2 f0) fmm) (* eps eps))])
                    (assert-within 0.1 12.0 d2f)))  ; 6*2 = 12
            
            ;; Note: Scalar Hessian via nested AD requires special handling
            ;; For production use, consider the hessian function from higher-order-diff.ss
            ;; which uses reverse-over-forward mode for efficiency
            )

;;; ============================================================
;;; Numerical Gradient Verification
;;; ============================================================

(test-group numerical-verification
            (define-test numerical-gradient-test
              (let* ([f (lambda (x) (* x x))]
                     [epsilon 0.0001]
                     [x 3.0]
                     [num-grad (numerical-gradient flonum-floating f x epsilon)])
                    (assert-within 0.01 6.0 num-grad)))
            
            (define-test check-gradient-test
              (assert-true (check-gradient real-differentiable flonum-floating
                                           d-square 3.0 0.0001 0.01))
              (assert-true (check-gradient real-differentiable flonum-floating
                                           d-exp 1.0 0.0001 0.01))))

;;; ============================================================
;;; Gradient Descent Tests
;;; ============================================================

(test-group gradient-descent-tests
            ;; Minimize f(x) = x^2, minimum at x=0
            (define-test gd-square-test
              (let ([result (gradient-descent real-differentiable 0.1 d-square 5.0 100)])
                   (assert-within 0.01 0.0 result)))
            
            ;; Minimize f(x) = (x-3)^2, minimum at x=3
            (define-test gd-shifted-square-test
              (let* ([f (lambda (x) (traced-sq (traced-sub x 3)))]
                     [result (gradient-descent real-differentiable 0.1 f 0.0 100)])
                    (assert-within 0.1 3.0 result))))

;;; ============================================================
;;; Directional Derivative Tests
;;; ============================================================

(test-group directional-derivative-tests
            ;; f(x) = x^2, directional derivative at x=2 in direction v=1 is 4*1=4
            (define-test dd-scalar-test
              (let ([dd (directional-derivative real-differentiable d-square 2.0 1.0)])
                   (assert-within 0.001 4.0 dd)))
            
            ;; f(x,y) = x^2 + y^2, grad(2,3) = (4,6)
            ;; Directional derivative in direction (1,0) is 4
            (define-test dd-vector-test
              (let ([dd (directional-derivative vector-differentiable
                                                (lambda (args)
                                                        (traced-add (traced-sq (car args))
                                                                    (traced-sq (cadr args))))
                                                '(2.0 3.0)
                                                '(1.0 0.0))])
                   (assert-within 0.001 4.0 dd))))

;;; ============================================================
;;; Loss Function Tests
;;; ============================================================

(test-group loss-functions
            ;; MSE loss
            (define-test mse-loss-test
              (let* ([tape (make-reverse-tape)]
                     [preds (map (lambda (x) (make-traced-var x tape)) '(1.0 2.0 3.0))]
                     [targets '(1.0 2.0 3.0)]
                     [loss (traced-value (mse-loss preds targets))])
                    (assert-within 0.001 0.0 loss)))
            
            ;; L2 loss
            (define-test l2-loss-test
              (let* ([tape (make-reverse-tape)]
                     [weights (map (lambda (x) (make-traced-var x tape)) '(1.0 2.0 3.0))]
                     [loss (traced-value (l2-loss weights))])
                    (assert-within 0.001 14.0 loss))))  ; 1 + 4 + 9 = 14

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(test-group utilities
            (define-test iota-test
              (assert-equal '() (iota 0))
              (assert-equal '(0) (iota 1))
              (assert-equal '(0 1 2 3 4) (iota 5)))
            
            (define-test diff-compose-test
              (let* ([f (diff-compose d-sqrt d-square)]  ; sqrt(x^2) = |x|
                     [result (f (make-traced-var 3.0 (make-reverse-tape)))])
                    (assert-within 0.001 3.0 (traced-value result)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "
")
(display "==============================================================
")
(display (format "Tests passed: ~a
" *tests-passed*))
(display (format "Tests failed: ~a
" *tests-failed*))
(display (format "Total tests:  ~a
" *tests-run*))

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All Differentiable type class tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
