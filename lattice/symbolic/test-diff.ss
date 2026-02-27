;;; core/fp/symbolic/test-diff.ss — Tests for Symbolic Differentiation
;;;
;;; NOTE: Run from project root: scheme --script core/fp/symbolic/test-diff.ss

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/symbolic/diff.ss")

(display "\n")
(display "====\n")
(display "         SYMBOLIC DIFFERENTIATION TESTS\n")
(display "====\n")

;;; ====
;;; Basic Differentiation Rules
;;; ====

(test-group basic-rules
            (define-test constant-rule
              ;; d/dx[5] = 0
              (let ([result (deriv (num 5) 'x)])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test variable-rule-same
              ;; d/dx[x] = 1
              (let ([result (deriv (var 'x) 'x)])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test variable-rule-different
              ;; d/dx[y] = 0
              (let ([result (deriv (var 'y) 'x)])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result)))))

;;; ====
;;; Sum Rule
;;; ====

(test-group sum-rule
            (define-test sum-of-constants
              ;; d/dx[3 + 5] = 0
              (let ([result (deriv (sum (num 3) (num 5)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test sum-with-variable
              ;; d/dx[x + 5] = 1
              (let ([result (deriv (sum (var 'x) (num 5)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test sum-of-variables
              ;; d/dx[x + y] = 1 (simplified from 1 + 0)
              (let ([result (deriv (sum (var 'x) (var 'y)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test sum-n-ary
              ;; d/dx[x + y + z] = 1
              (let ([result (deriv (make-sum (list (var 'x) (var 'y) (var 'z))) 'x)])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result)))))

;;; ====
;;; Product Rule
;;; ====

(test-group product-rule
            (define-test product-constant
              ;; d/dx[5 * x] = 5
              (let ([result (deriv (product (num 5) (var 'x)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 5 (num-val result))))
            
            (define-test product-x-squared
              ;; d/dx[x * x] = x + x (our simplification doesn't collect like terms to 2x)
              (let ([result (deriv (product (var 'x) (var 'x)) 'x)])
                   ;; Result should be a sum (x + x) after product rule
                   (assert-true (sum? result))
                   (assert-equal 2 (length (sum-terms result)))))
            
            (define-test product-with-constant
              ;; d/dx[3 * x * y] = 3y (treating y as constant w.r.t. x)
              (let ([result (deriv (make-product (list (num 3) (var 'x) (var 'y))) 'x)])
                   ;; Should simplify to 3*y
                   (assert-true (product? result))))
            
            (define-test product-two-functions
              ;; d/dx[(x^2) * (x^3)] = 5x^4
              ;; Using product rule: 2x * x^3 + x^2 * 3x^2 = 2x^4 + 3x^4 = 5x^4
              (let* ([x2 (power (var 'x) (num 2))]
                     [x3 (power (var 'x) (num 3))]
                     [result (deriv (product x2 x3) 'x)])
                    ;; Result should be a sum
                    (assert-true (sum? result)))))

;;; ====
;;; Difference Rule
;;; ====

(test-group difference-rule
            (define-test difference-simple
              ;; d/dx[x - 5] = 1
              (let ([result (deriv (difference (var 'x) (num 5)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test difference-variables
              ;; d/dx[x - y] = 1
              (let ([result (deriv (difference (var 'x) (var 'y)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test negation
              ;; d/dx[-x] = -1
              (let ([result (deriv (make-neg (var 'x)) 'x)])
                   (assert-true (difference? result))
                   ;; Should be negation of 1
                   (assert-false (diff-right result)))))

;;; ====
;;; Quotient Rule
;;; ====

(test-group quotient-rule
            (define-test quotient-constant-numerator
              ;; d/dx[1/x] = -1/x^2
              (let ([result (deriv (division (num 1) (var 'x)) 'x)])
                   ;; Should be a quotient
                   (assert-true (quotient? result))
                   ;; Numerator should be negative
                   (let ([numer (quot-numer result)])
                        (assert-true (difference? numer)))))
            
            (define-test quotient-x-over-constant
              ;; d/dx[x/5] = 1/5 = 5/25 (from quotient rule)
              (let ([result (deriv (division (var 'x) (num 5)) 'x)])
                   (assert-true (quotient? result))
                   (let ([numer (quot-numer result)]
                         [denom (quot-denom result)])
                        ;; Numerator: 1*5 - x*0 = 5
                        (assert-true (num? numer))
                        (assert-equal 5 (num-val numer))
                        ;; Denominator: 5^2 = 25 (simplified from power)
                        (assert-true (num? denom))
                        (assert-equal 25 (num-val denom)))))
            
            (define-test quotient-polynomial
              ;; d/dx[x^2/x] should give (2x*x - x^2*1)/x^2
              (let* ([x (var 'x)]
                     [x2 (power x (num 2))]
                     [result (deriv (division x2 x) 'x)])
                    (assert-true (quotient? result)))))

;;; ====
;;; Power Rule
;;; ====

(test-group power-rule
            (define-test power-constant-exponent
              ;; d/dx[x^3] = 3x^2
              (let ([result (deriv (power (var 'x) (num 3)) 'x)])
                   ;; Should be 3 * x^2 * 1 = 3x^2
                   (assert-true (product? result))
                   (let ([factors (product-factors result)])
                        ;; First factor should be 3
                        (assert-true (num? (car factors)))
                        (assert-equal 3 (num-val (car factors))))))
            
            (define-test power-constant-base
              ;; d/dx[2^x] = 2^x * ln(2)
              (let ([result (deriv (power (num 2) (var 'x)) 'x)])
                   ;; Should be a product of 2^x and ln(2)
                   (assert-true (product? result))))
            
            (define-test power-zero-exponent
              ;; d/dx[x^0] = d/dx[1] = 0
              (let ([result (deriv (power (var 'x) (num 0)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test power-one-exponent
              ;; d/dx[x^1] = d/dx[x] = 1
              (let ([result (deriv (power (var 'x) (num 1)) 'x)])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test power-negative-exponent
              ;; d/dx[x^(-2)] = -2x^(-3)
              (let ([result (deriv (power (var 'x) (num -2)) 'x)])
                   ;; Should be -2 * x^(-3)
                   (assert-true (product? result)))))

;;; ====
;;; Chain Rule (Function Applications)
;;; ====

(test-group chain-rule
            (define-test sin-of-x
              ;; d/dx[sin(x)] = cos(x)
              (let ([result (deriv (sym-sin (var 'x)) 'x)])
                   ;; cos(x) * 1 = cos(x)
                   (assert-true (app? result))
                   (assert-equal 'cos (app-fn result))))
            
            (define-test cos-of-x
              ;; d/dx[cos(x)] = -sin(x)
              (let ([result (deriv (sym-cos (var 'x)) 'x)])
                   ;; Should be negation of sin(x)
                   (assert-true (difference? result))))
            
            (define-test exp-of-x
              ;; d/dx[exp(x)] = exp(x)
              (let ([result (deriv (sym-exp (var 'x)) 'x)])
                   ;; exp(x) * 1 = exp(x)
                   (assert-true (app? result))
                   (assert-equal 'exp (app-fn result))))
            
            (define-test log-of-x
              ;; d/dx[log(x)] = 1/x
              (let ([result (deriv (sym-log (var 'x)) 'x)])
                   ;; (1/x) * 1 = 1/x
                   (assert-true (quotient? result))))
            
            (define-test sin-of-x-squared
              ;; d/dx[sin(x^2)] = cos(x^2) * 2x
              (let* ([x2 (power (var 'x) (num 2))]
                     [result (deriv (sym-sin x2) 'x)])
                    ;; Should be product of cos(x^2) and derivative of x^2
                    (assert-true (product? result))))
            
            (define-test nested-functions
              ;; d/dx[sin(cos(x))] = cos(cos(x)) * (-sin(x))
              (let* ([cos-x (sym-cos (var 'x))]
                     [sin-cos-x (sym-sin cos-x)]
                     [result (deriv sin-cos-x 'x)])
                    ;; Should be a product
                    (assert-true (product? result)))))

;;; ====
;;; Partial Derivatives
;;; ====

(test-group partial-derivatives
            (define-test partial-x-of-xy
              ;; ∂/∂x[x*y] = y
              (let ([result (partial (product (var 'x) (var 'y)) 'x)])
                   ;; Should be y (from x*0 + 1*y)
                   (assert-true (var? result))
                   (assert-equal 'y (var-name result))))
            
            (define-test partial-y-of-xy
              ;; ∂/∂y[x*y] = x
              (let ([result (partial (product (var 'x) (var 'y)) 'y)])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test partial-mixed
              ;; ∂/∂x[x^2 + y^2] = 2x
              (let* ([x2 (power (var 'x) (num 2))]
                     [y2 (power (var 'y) (num 2))]
                     [result (partial (sum x2 y2) 'x)])
                    ;; Should be 2x (from derivative of x^2) + 0
                    (assert-true (product? result)))))

;;; ====
;;; Gradient
;;; ====

(test-group gradient
            (define-test gradient-linear
              ;; ∇[3x + 4y] = [3, 4]
              (let* ([expr (sum (product (num 3) (var 'x))
                                (product (num 4) (var 'y)))]
                     [grad (gradient expr '(x y))])
                    (assert-equal 2 (length grad))
                    ;; ∂/∂x = 3
                    (assert-true (num? (car grad)))
                    (assert-equal 3 (num-val (car grad)))
                    ;; ∂/∂y = 4
                    (assert-true (num? (cadr grad)))
                    (assert-equal 4 (num-val (cadr grad)))))
            
            (define-test gradient-quadratic
              ;; ∇[x^2 + y^2] = [2x, 2y]
              (let* ([x2 (power (var 'x) (num 2))]
                     [y2 (power (var 'y) (num 2))]
                     [expr (sum x2 y2)]
                     [grad (gradient expr '(x y))])
                    (assert-equal 2 (length grad))
                    ;; Both should be products (coefficient * variable)
                    (assert-true (product? (car grad)))
                    (assert-true (product? (cadr grad)))))
            
            (define-test gradient-three-vars
              ;; ∇[x + 2y + 3z] = [1, 2, 3]
              (let* ([expr (make-sum (list (var 'x)
                                           (product (num 2) (var 'y))
                                           (product (num 3) (var 'z))))]
                     [grad (gradient expr '(x y z))])
                    (assert-equal 3 (length grad))
                    (assert-equal 1 (num-val (car grad)))
                    (assert-equal 2 (num-val (cadr grad)))
                    (assert-equal 3 (num-val (caddr grad))))))

;;; ====
;;; Jacobian Matrix
;;; ====

(test-group jacobian
            (define-test jacobian-2x2
              ;; f(x,y) = [x*y, x+y]
              ;; J = [[y, x], [1, 1]]
              (let* ([f1 (product (var 'x) (var 'y))]
                     [f2 (sum (var 'x) (var 'y))]
                     [J (jacobian (list f1 f2) '(x y))])
                    (assert-equal 2 (length J))
                    ;; First row: [y, x]
                    (let ([row1 (car J)])
                         (assert-equal 2 (length row1))
                         (assert-true (var? (car row1)))
                         (assert-equal 'y (var-name (car row1)))
                         (assert-true (var? (cadr row1)))
                         (assert-equal 'x (var-name (cadr row1))))
                    ;; Second row: [1, 1]
                    (let ([row2 (cadr J)])
                         (assert-equal 2 (length row2))
                         (assert-equal 1 (num-val (car row2)))
                         (assert-equal 1 (num-val (cadr row2))))))
            
            (define-test jacobian-rectangular
              ;; f(x,y,z) = [x+y, y+z] (2x3 matrix)
              (let* ([f1 (sum (var 'x) (var 'y))]
                     [f2 (sum (var 'y) (var 'z))]
                     [J (jacobian (list f1 f2) '(x y z))])
                    (assert-equal 2 (length J))
                    ;; Each row should have 3 elements
                    (assert-equal 3 (length (car J)))
                    (assert-equal 3 (length (cadr J))))))

;;; ====
;;; Hessian Matrix
;;; ====

(test-group hessian
            (define-test hessian-quadratic
              ;; f(x,y) = x^2 + y^2
              ;; H = [[2, 0], [0, 2]]
              (let* ([x2 (power (var 'x) (num 2))]
                     [y2 (power (var 'y) (num 2))]
                     [f (sum x2 y2)]
                     [H (hessian f '(x y))])
                    (assert-equal 2 (length H))
                    ;; H[0][0] = ∂²f/∂x² = 2
                    (assert-equal 2 (num-val (caar H)))
                    ;; H[0][1] = ∂²f/∂x∂y = 0
                    (assert-equal 0 (num-val (cadar H)))
                    ;; H[1][0] = ∂²f/∂y∂x = 0
                    (assert-equal 0 (num-val (caadr H)))
                    ;; H[1][1] = ∂²f/∂y² = 2
                    (assert-equal 2 (num-val (cadadr H)))))
            
            (define-test hessian-mixed
              ;; f(x,y) = x*y
              ;; H = [[0, 1], [1, 0]]
              (let* ([f (product (var 'x) (var 'y))]
                     [H (hessian f '(x y))])
                    (assert-equal 2 (length H))
                    ;; H[0][0] = ∂²f/∂x² = 0
                    (assert-equal 0 (num-val (caar H)))
                    ;; H[0][1] = ∂²f/∂x∂y = 1
                    (assert-equal 1 (num-val (cadar H)))
                    ;; H[1][0] = ∂²f/∂y∂x = 1
                    (assert-equal 1 (num-val (caadr H)))
                    ;; H[1][1] = ∂²f/∂y² = 0
                    (assert-equal 0 (num-val (cadadr H)))))
            
            (define-test hessian-three-vars
              ;; f(x,y,z) = x^2 + y^2 + z^2
              ;; H = [[2,0,0], [0,2,0], [0,0,2]]
              (let* ([expr (make-sum (list (power (var 'x) (num 2))
                                           (power (var 'y) (num 2))
                                           (power (var 'z) (num 2))))]
                     [H (hessian expr '(x y z))])
                    (assert-equal 3 (length H))
                    ;; Check diagonal elements are 2
                    (assert-equal 2 (num-val (caar H)))
                    (assert-equal 2 (num-val (cadadr H)))
                    (assert-equal 2 (num-val (caddr (caddr H))))
                    ;; Check off-diagonal elements are 0
                    (assert-equal 0 (num-val (cadar H)))
                    (assert-equal 0 (num-val (caddr (cadr H)))))))

;;; ====
;;; Higher-Order Derivatives
;;; ====

(test-group higher-order
            (define-test second-derivative
              ;; d²/dx²[x^3] = 6x
              ;; Our implementation produces (* 3 (* 2 x)) which is mathematically correct
              ;; but not fully constant-folded to (* 6 x)
              (let ([result (deriv-n (power (var 'x) (num 3)) 'x 2)])
                   ;; Result should be a product
                   (assert-true (product? result))
                   ;; First factor should be a constant
                   (let ([f1 (car (product-factors result))])
                        (assert-true (num? f1)))))
            
            (define-test third-derivative
              ;; d³/dx³[x^4] = 24x
              (let ([result (deriv-n (power (var 'x) (num 4)) 'x 3)])
                   ;; Should be 24x
                   (assert-true (product? result))))
            
            (define-test zeroth-derivative
              ;; d⁰/dx⁰[x^2] = x^2
              (let ([expr (power (var 'x) (num 2))]
                    [result (deriv-n (power (var 'x) (num 2)) 'x 0)])
                   (assert-true (expr=? expr result)))))

;;; ====
;;; Directional Derivative
;;; ====

(test-group directional-derivative
            (define-test directional-simple
              ;; Df(x,y) in direction [1,0] for f = x^2 + y^2
              ;; ∇f = [2x, 2y], direction = [1, 0]
              ;; ∇f · [1,0] = 2x
              (let* ([f (sum (power (var 'x) (num 2))
                             (power (var 'y) (num 2)))]
                     [dir (list (num 1) (num 0))]
                     [result (directional-derivative f '(x y) dir)])
                    ;; Should be 2x (from 2x*1 + 2y*0)
                    (assert-true (product? result))))
            
            (define-test directional-diagonal
              ;; Df(x,y) in direction [1,1] for f = x + y
              ;; ∇f = [1, 1], direction = [1, 1]
              ;; ∇f · [1,1] = 1*1 + 1*1 = 2
              (let* ([f (sum (var 'x) (var 'y))]
                     [dir (list (num 1) (num 1))]
                     [result (directional-derivative f '(x y) dir)])
                    ;; Should be 2
                    (assert-true (num? result))
                    (assert-equal 2 (num-val result)))))

;;; ====
;;; Vector Calculus
;;; ====

(test-group vector-calculus
            (define-test divergence-simple
              ;; div([x, y, z]) = 1 + 1 + 1 = 3
              (let* ([field (list (var 'x) (var 'y) (var 'z))]
                     [result (divergence field '(x y z))])
                    (assert-true (num? result))
                    (assert-equal 3 (num-val result))))
            
            (define-test divergence-squares
              ;; div([x^2, y^2]) = 2x + 2y
              (let* ([field (list (power (var 'x) (num 2))
                                  (power (var 'y) (num 2)))]
                     [result (divergence field '(x y))])
                    (assert-true (sum? result))))
            
            (define-test curl-simple
              ;; curl([0, 0, x*y]) = [∂(xy)/∂y - 0, ∂(0)/∂z - ∂(xy)/∂x, 0 - 0]
              ;;                   = [x, -y, 0]
              (let* ([field (list (num 0) (num 0) (product (var 'x) (var 'y)))]
                     [result (curl field '(x y z))])
                    (assert-equal 3 (length result))
                    ;; First component should be x
                    (assert-true (var? (car result)))
                    (assert-equal 'x (var-name (car result)))))
            
            (define-test laplacian-simple
              ;; ∇²[x^2 + y^2 + z^2] = 2 + 2 + 2 = 6
              (let* ([f (make-sum (list (power (var 'x) (num 2))
                                        (power (var 'y) (num 2))
                                        (power (var 'z) (num 2))))]
                     [result (laplacian f '(x y z))])
                    (assert-true (num? result))
                    (assert-equal 6 (num-val result)))))

;;; ====
;;; Complex Expressions
;;; ====

(test-group complex-expressions
            (define-test polynomial
              ;; d/dx[x^3 + 2x^2 + 3x + 4] = 3x^2 + 4x + 3
              (let* ([poly (make-sum (list (power (var 'x) (num 3))
                                           (product (num 2) (power (var 'x) (num 2)))
                                           (product (num 3) (var 'x))
                                           (num 4)))]
                     [result (deriv poly 'x)])
                    ;; Result should be a sum
                    (assert-true (sum? result))))
            
            (define-test rational-function
              ;; d/dx[(x^2 + 1)/(x - 1)]
              (let* ([numer (sum (power (var 'x) (num 2)) (num 1))]
                     [denom (difference (var 'x) (num 1))]
                     [rat (division numer denom)]
                     [result (deriv rat 'x)])
                    ;; Result should be a quotient (from quotient rule)
                    (assert-true (quotient? result))))
            
            (define-test composition-chain
              ;; d/dx[sin(exp(x^2))]
              ;; = cos(exp(x^2)) * exp(x^2) * 2x
              (let* ([x2 (power (var 'x) (num 2))]
                     [exp-x2 (sym-exp x2)]
                     [sin-exp-x2 (sym-sin exp-x2)]
                     [result (deriv sin-exp-x2 'x)])
                    ;; Should be a product (chain rule application)
                    (assert-true (product? result)))))

(display "\nRunning tests...\n\n")
(run-all-tests)
