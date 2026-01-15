;;; lattice/autodiff/test-symbolic-diff.ss — Tests for Symbolic-Autodiff Integration
;;;
;;; Tests for eval-expr, simplify, expr-to-traced, and high-level integration.
;;;
;;; NOTE: Run from project root: scheme --script lattice/autodiff/test-symbolic-diff.ss

(load "core/testing/test-framework.ss")
(load "lattice/autodiff/symbolic-diff.ss")

;;; Custom assertion for floating point comparisons
(define (assert-nearly-equal expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      #t
      (begin
       (display "      Expected: ") (display expected)
       (display " (±") (display tolerance) (display ")") (newline)
       (display "      Got:      ") (display actual) (newline)
       (assert-true #f))))

(display "\n")
(display "====\n")
(display "         SYMBOLIC-AUTODIFF INTEGRATION TESTS\n")
(display "====\n")

;;; ====
;;; eval-expr tests
;;; ====

(test-group eval-expr-tests

            (define-test constant
              (assert-equal 42 (eval-expr (num 42) '())))

            (define-test variable-lookup
              (assert-equal 3.0 (eval-expr (var 'x) '((x . 3.0)))))

            (define-test special-constants
              (assert-nearly-equal 3.14159 (eval-expr (var 'pi) '()) 0.001)
              (assert-nearly-equal 2.71828 (eval-expr (var 'e) '()) 0.001))

            (define-test sum
              (assert-equal 5.0 (eval-expr (sum (var 'x) (var 'y))
                                           '((x . 2.0) (y . 3.0)))))

            (define-test product
              (assert-equal 6.0 (eval-expr (product (var 'x) (var 'y))
                                           '((x . 2.0) (y . 3.0)))))

            (define-test difference
              (assert-equal 1.0 (eval-expr (difference (var 'x) (var 'y))
                                           '((x . 3.0) (y . 2.0)))))

            (define-test negation
              (assert-equal -5.0 (eval-expr (make-neg (var 'x)) '((x . 5.0)))))

            (define-test quotient
              (assert-equal 2.0 (eval-expr (quotient (var 'x) (var 'y))
                                           '((x . 6.0) (y . 3.0)))))

            (define-test power
              (assert-equal 8.0 (eval-expr (power (var 'x) (num 3))
                                           '((x . 2.0)))))

            (define-test sin-function
              (assert-nearly-equal 0.0 (eval-expr (sym-sin (num 0)) '()) 0.0001))

            (define-test cos-function
              (assert-nearly-equal 1.0 (eval-expr (sym-cos (num 0)) '()) 0.0001))

            (define-test exp-log
              (assert-nearly-equal 1.0 (eval-expr (sym-exp (num 0)) '()) 0.0001)
              (assert-nearly-equal 0.0 (eval-expr (sym-log (num 1)) '()) 0.0001))

            (define-test complex-polynomial
              ;; x^2 + 2*x + 1 at x=3 should be 16
              (let ([expr (sum (power (var 'x) (num 2))
                               (sum (product (num 2) (var 'x))
                                    (num 1)))])
                   (assert-equal 16.0 (eval-expr expr '((x . 3.0)))))))

;;; ====
;;; simplify tests
;;; ====

(test-group simplify-tests

            (define-test sum-zeros
              ;; x + 0 = x
              (let ([result (simplify (sum (var 'x) (num 0)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))

            (define-test product-zeros
              ;; x * 0 = 0
              (let ([result (simplify (product (var 'x) (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))

            (define-test product-ones
              ;; x * 1 = x
              (let ([result (simplify (product (var 'x) (num 1)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))

            (define-test power-zero
              ;; x^0 = 1
              (let ([result (simplify (power (var 'x) (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))

            (define-test power-one
              ;; x^1 = x
              (let ([result (simplify (power (var 'x) (num 1)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))

            (define-test collect-numerics
              ;; 2 + 3 = 5
              (let ([result (simplify (sum (num 2) (num 3)))])
                   (assert-true (num? result))
                   (assert-equal 5 (num-val result))))

            (define-test diff-same
              ;; x - x = 0
              (let ([result (simplify (difference (var 'x) (var 'x)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))

            (define-test neg-neg
              ;; -(-x) = x
              (let ([result (simplify (make-neg (make-neg (var 'x))))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))

            (define-test collect-like-terms
              ;; x + x = 2*x
              (let ([result (simplify (sum (var 'x) (var 'x)))])
                   (assert-true (product? result))
                   (assert-true (num? (car (product-factors result))))
                   (assert-equal 2 (num-val (car (product-factors result))))))

            (define-test simplify-sin-zero
              ;; sin(0) = 0
              (let ([result (simplify (sym-sin (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))

            (define-test simplify-cos-zero
              ;; cos(0) = 1
              (let ([result (simplify (sym-cos (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))

            (define-test simplify-sinh-zero
              ;; sinh(0) = 0
              (let ([result (simplify (sym-sinh (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))

            (define-test simplify-cosh-zero
              ;; cosh(0) = 1
              (let ([result (simplify (sym-cosh (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))

            (define-test simplify-tanh-zero
              ;; tanh(0) = 0
              (let ([result (simplify (sym-tanh (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))

            (define-test simplify-atan-zero
              ;; atan(0) = 0
              (let ([result (simplify (sym-atan (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result)))))

;;; ====
;;; expr-to-traced tests
;;; ====

(test-group expr-to-traced-tests

            (define-test constant-function
              (let* ([f (expr-to-traced (num 5) '(x))]
                     [result (f 3.0)])
                    (assert-equal 5 result)))

            (define-test identity-function
              (with-fresh-ad-scope
               (lambda ()
                       (let* ([f (expr-to-traced (var 'x) '(x))]
                              [tape (make-reverse-tape)]
                              [tx (make-traced-var 3.0 tape)]
                              [result (f tx)])
                             (assert-true (traced? result))
                             (assert-equal 3.0 (traced-value result))))))

            (define-test square-gradient
              ;; d/dx[x^2] at x=3 should be 6
              (let* ([f (expr-to-traced (power (var 'x) (num 2)) '(x))]
                     [grad (gradient f '(3.0))])
                    (assert-nearly-equal 6.0 (car grad) 0.0001)))

            (define-test product-gradient
              ;; d/dx[x*y] = y, d/dy[x*y] = x at (2,3)
              (let* ([f (expr-to-traced (product (var 'x) (var 'y)) '(x y))]
                     [grad (gradient f '(2.0 3.0))])
                    (assert-nearly-equal 3.0 (car grad) 0.0001)
                    (assert-nearly-equal 2.0 (cadr grad) 0.0001)))

            (define-test sin-gradient
              ;; d/dx[sin(x)] at x=0 = cos(0) = 1
              (let* ([f (expr-to-traced (sym-sin (var 'x)) '(x))]
                     [grad (gradient f '(0.0))])
                    (assert-nearly-equal 1.0 (car grad) 0.0001)))

            (define-test complex-expr-gradient
              ;; d/dx[x^2 + sin(x)] = 2x + cos(x)
              ;; At x=pi/4: 2*(pi/4) + cos(pi/4) ≈ 1.571 + 0.707 ≈ 2.278
              (let* ([expr (sum (power (var 'x) (num 2)) (sym-sin (var 'x)))]
                     [f (expr-to-traced expr '(x))]
                     [x (/ 3.141592653589793 4)]
                     [grad (gradient f (list x))]
                     [expected (+ (* 2 x) (cos x))])
                    (assert-nearly-equal expected (car grad) 0.0001))))

;;; ====
;;; verify-derivative tests
;;; ====

(test-group verify-derivative-tests

            (define-test verify-polynomial
              ;; d/dx[x^3] = 3x^2 at x=2: 12
              (assert-true (verify-derivative (power (var 'x) (num 3)) 'x 2.0 0.0001)))

            (define-test verify-exp
              ;; d/dx[exp(x)] = exp(x) at x=1
              (assert-true (verify-derivative (sym-exp (var 'x)) 'x 1.0 0.0001)))

            (define-test verify-sin
              ;; d/dx[sin(x)] = cos(x) at x=pi/4
              (assert-true (verify-derivative (sym-sin (var 'x)) 'x 0.785 0.0001)))

            (define-test verify-quotient
              ;; d/dx[1/x] = -1/x^2 at x=2: -0.25
              (assert-true (verify-derivative (quotient (num 1) (var 'x)) 'x 2.0 0.0001)))

            (define-test verify-chain-rule
              ;; d/dx[sin(x^2)] = cos(x^2) * 2x at x=1
              (assert-true (verify-derivative (sym-sin (power (var 'x) (num 2))) 'x 1.0 0.0001))))

;;; ====
;;; symbolic-gradient-fn tests
;;; ====

(test-group symbolic-gradient-fn-tests

            (define-test gradient-polynomial
              ;; ∇[x^2 + y^2] = [2x, 2y] at (3,4) = [6, 8]
              (let* ([expr (sum (power (var 'x) (num 2)) (power (var 'y) (num 2)))]
                     [grad-fn (symbolic-gradient-fn expr '(x y))]
                     [grad (grad-fn '(3.0 4.0))])
                    (assert-nearly-equal 6.0 (car grad) 0.0001)
                    (assert-nearly-equal 8.0 (cadr grad) 0.0001)))

            (define-test gradient-mixed
              ;; ∇[x*y + sin(x)] at (0, 5)
              ;; d/dx = y + cos(x) = 5 + 1 = 6
              ;; d/dy = x = 0
              (let* ([expr (sum (product (var 'x) (var 'y)) (sym-sin (var 'x)))]
                     [grad-fn (symbolic-gradient-fn expr '(x y))]
                     [grad (grad-fn '(0.0 5.0))])
                    (assert-nearly-equal 6.0 (car grad) 0.0001)
                    (assert-nearly-equal 0.0 (cadr grad) 0.0001))))

;;; ====
;;; symbolic-hessian-fn tests
;;; ====

(test-group symbolic-hessian-fn-tests

            (define-test hessian-quadratic
              ;; H[x^2 + y^2] = [[2, 0], [0, 2]]
              (let* ([expr (sum (power (var 'x) (num 2)) (power (var 'y) (num 2)))]
                     [hess-fn (symbolic-hessian-fn expr '(x y))]
                     [hess (hess-fn '(1.0 1.0))])
                    (assert-nearly-equal 2.0 (car (car hess)) 0.0001)
                    (assert-nearly-equal 0.0 (cadr (car hess)) 0.0001)
                    (assert-nearly-equal 0.0 (car (cadr hess)) 0.0001)
                    (assert-nearly-equal 2.0 (cadr (cadr hess)) 0.0001))))

;;; ====
;;; Full Integration tests
;;; ====

(test-group full-integration-tests

            (define-test symbolic-deriv-to-autodiff
              ;; Take symbolic derivative of x*sin(x), compile, evaluate
              ;; d/dx[x*sin(x)] = sin(x) + x*cos(x)
              ;; At x=pi/2: sin(pi/2) + (pi/2)*cos(pi/2) = 1 + 0 = 1
              (let* ([expr (product (var 'x) (sym-sin (var 'x)))]
                     [deriv-expr (simplify (deriv expr 'x))]
                     [f (expr-to-traced deriv-expr '(x))]
                     [x (/ 3.141592653589793 2)]
                     [result (let ([r (f x)])
                                  (if (traced? r) (traced-value r) r))])
                    (assert-nearly-equal 1.0 result 0.0001)))

            (define-test second-derivative-hybrid
              ;; Symbolic first derivative, autodiff second derivative
              ;; d/dx[x^3] = 3x^2
              ;; d²/dx²[x³] = d/dx[3x²] = 6x, at x=2: 12
              (let* ([expr (power (var 'x) (num 3))]
                     [d1 (simplify (deriv expr 'x))]
                     [f (expr-to-traced d1 '(x))]
                     [second-deriv (car (gradient f '(2.0)))])
                    (assert-nearly-equal 12.0 second-deriv 0.0001)))

            (define-test rosenbrock-gradient
              ;; Rosenbrock: f(x,y) = (1-x)^2 + 100*(y-x^2)^2
              ;; At minimum (1,1), gradient should be [0, 0]
              (let* ([one-minus-x (difference (num 1) (var 'x))]
                     [y-minus-x2 (difference (var 'y) (power (var 'x) (num 2)))]
                     [term1 (power one-minus-x (num 2))]
                     [term2 (product (num 100) (power y-minus-x2 (num 2)))]
                     [rosenbrock (sum term1 term2)]
                     [grad-fn (symbolic-gradient-fn rosenbrock '(x y))]
                     [grad (grad-fn '(1.0 1.0))])
                    ;; At (1,1): gradient should be [0, 0]
                    (assert-nearly-equal 0.0 (car grad) 0.001)
                    (assert-nearly-equal 0.0 (cadr grad) 0.001))))

;;; ====
;;; expr->infix tests
;;; ====

(test-group expr-infix-tests

            (define-test simple-expressions
              (assert-equal "x" (expr->infix (var 'x)))
              (assert-equal "42" (expr->infix (num 42)))
              (assert-equal "(x + y)" (expr->infix (sum (var 'x) (var 'y))))
              (assert-equal "(x*y)" (expr->infix (product (var 'x) (var 'y))))
              (assert-equal "(x^2)" (expr->infix (power (var 'x) (num 2))))
              (assert-equal "sin(x)" (expr->infix (sym-sin (var 'x))))))

(display "\nRunning tests...\n\n")
(run-all-tests)
