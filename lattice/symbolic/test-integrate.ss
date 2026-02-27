;;; lattice/symbolic/test-integrate.ss — Tests for Symbolic Integration
;;;
;;; NOTE: Run from project root: scheme --script lattice/symbolic/test-integrate.ss

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/symbolic/integrate.ss")

(display "\n")
(display "====\n")
(display "         SYMBOLIC INTEGRATION TESTS\n")
(display "====\n")

;;; Helper to convert truthy to #t
(define (truthy? x) (if x #t #f))

;;; ====
;;; Basic Integration Tests
;;; ====

(test-group basic-integrals
            (define-test integrate-constant
              (let ([result (integrate (num 5) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (product? result))))
            
            (define-test integrate-x
              (let ([result (integrate (var 'x) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (quotient? result))))
            
            (define-test integrate-y-wrt-x
              (let ([result (integrate (var 'y) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (product? result))))
            
            (define-test integrate-x-squared
              (let ([result (integrate (power (var 'x) (num 2)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (quotient? result))))
            
            (define-test integrate-x-cubed
              (let ([result (integrate (power (var 'x) (num 3)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (quotient? result)))))

;;; ====
;;; Trigonometric Integration Tests
;;; ====

(test-group trig-integrals
            (define-test integrate-sin
              (let ([result (integrate (sym-sin (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   ;; Result should be -cos(x)
                   (assert-true (difference? result))))
            
            (define-test integrate-cos
              (let ([result (integrate (sym-cos (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (app? result))
                   (assert-equal 'sin (app-fn result)))))

;;; ====
;;; Exponential/Logarithm Integration Tests
;;; ====

(test-group exp-log-integrals
            (define-test integrate-exp
              (let ([result (integrate (sym-exp (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (app? result))
                   (assert-equal 'exp (app-fn result))))
            
            (define-test integrate-one-over-x
              (let ([result (integrate (division (num 1) (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (app? result))
                   (assert-equal 'log (app-fn result))))
            
            (define-test integrate-ln
              (let ([result (integrate (sym-log (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (difference? result)))))

;;; ====
;;; Sum Rule Tests
;;; ====

(test-group sum-rule
            (define-test integrate-sum-of-powers
              (let ([result (integrate (sum (var 'x) (power (var 'x) (num 2))) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (sum? result))))
            
            (define-test integrate-sum-of-trig
              (let ([result (integrate (sum (sym-sin (var 'x)) (sym-cos (var 'x))) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (sum? result)))))

;;; ====
;;; Constant Multiple Rule Tests
;;; ====

(test-group constant-multiple
            (define-test integrate-3x
              (let ([result (integrate (product (num 3) (var 'x)) 'x)])
                   (assert-true (truthy? result))))
            
            (define-test integrate-5-sin
              (let ([result (integrate (product (num 5) (sym-sin (var 'x))) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (product? result)))))

;;; ====
;;; Power Rule Tests
;;; ====

(test-group power-rule
            (define-test integrate-x-inverse
              (let ([result (integrate (power (var 'x) (num -1)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (app? result))
                   (assert-equal 'log (app-fn result))))
            
            (define-test integrate-sqrt
              (let ([result (integrate (sym-sqrt (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (product? result)))))

;;; ====
;;; Difference Rule Tests
;;; ====

(test-group difference-rule
            (define-test integrate-diff-of-powers
              (let ([result (integrate (difference (power (var 'x) (num 2)) (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (difference? result))))
            
            (define-test integrate-negation
              (let ([result (integrate (make-neg (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (difference? result)))))

;;; ====
;;; Definite Integral Tests
;;; ====

(test-group definite-integrals
            (define-test definite-x-from-0-to-1
              (let ([result (definite-integral (var 'x) 'x (num 0) (num 1))])
                   (assert-true (truthy? result))
                   (let ([simplified (simplify result)])
                        (assert-true (num? simplified))
                        (assert-equal 1/2 (num-val simplified)))))
            
            (define-test definite-constant
              (let ([result (definite-integral (num 5) 'x (var 'a) (var 'b))])
                   (assert-true (truthy? result))
                   ;; Result is 5*b - 5*a, a difference expression
                   (assert-true (difference? result)))))

;;; ====
;;; Hyperbolic Function Tests
;;; ====

(test-group hyperbolic-integrals
            (define-test integrate-sinh
              (let ([result (integrate (sym-sinh (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (app? result))
                   (assert-equal 'cosh (app-fn result))))
            
            (define-test integrate-cosh
              (let ([result (integrate (sym-cosh (var 'x)) 'x)])
                   (assert-true (truthy? result))
                   (assert-true (app? result))
                   (assert-equal 'sinh (app-fn result)))))

;;; ====
;;; Verification Tests (derivative of integral = original)
;;; ====

(test-group verify-integration
            (define-test verify-x-squared
              (let* ([f (power (var 'x) (num 2))]
                     [F (integrate f 'x)]
                     [dF (and F (deriv F 'x))]
                     [simplified (and dF (simplify dF))])
                    (assert-true (truthy? simplified))
                    (assert-true (expr=? simplified f))))
            
            (define-test verify-sin
              (let* ([f (sym-sin (var 'x))]
                     [F (integrate f 'x)]
                     [dF (and F (deriv F 'x))]
                     [simplified (and dF (simplify dF))])
                    (assert-true (truthy? simplified))
                    (assert-true (expr=? simplified f))))
            
            (define-test verify-exp
              (let* ([f (sym-exp (var 'x))]
                     [F (integrate f 'x)]
                     [dF (and F (deriv F 'x))]
                     [simplified (and dF (simplify dF))])
                    (assert-true (truthy? simplified))
                    (assert-true (expr=? simplified f)))))

;;; ====
;;; Edge Cases
;;; ====

(test-group edge-cases
            (define-test integrate-zero
              (let ([result (integrate (num 0) 'x)])
                   (assert-true (truthy? result))
                   (let ([simplified (simplify result)])
                        (assert-true (num? simplified))
                        (assert-equal 0 (num-val simplified)))))
            
            (define-test integrate-one
              (let ([result (integrate (num 1) 'x)])
                   (assert-true (truthy? result))
                   (let ([simplified (simplify result)])
                        (assert-true (var? simplified))
                        (assert-equal 'x (var-name simplified))))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
