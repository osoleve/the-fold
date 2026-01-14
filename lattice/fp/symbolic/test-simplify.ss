;;; lattice/fp/symbolic/test-simplify.ss — Tests for Algebraic Simplification
;;;
;;; NOTE: Run from project root: scheme --script lattice/fp/symbolic/test-simplify.ss

(load "core/test-framework.ss")
(load "lattice/fp/symbolic/simplify.ss")

(display "\n")
(display "====\n")
(display "         ALGEBRAIC SIMPLIFICATION TESTS\n")
(display "====\n")

;;; ====
;;; Basic Simplification Tests
;;; ====

(test-group basic-simplification
            (define-test simplify-constant
              (let ([result (simplify (num 5))])
                   (assert-true (num? result))
                   (assert-equal 5 (num-val result))))
            
            (define-test simplify-variable
              (let ([result (simplify (var 'x))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test simplify-zero-plus-x
              (let ([result (simplify (sum (num 0) (var 'x)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test simplify-x-plus-zero
              (let ([result (simplify (sum (var 'x) (num 0)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test simplify-zero-times-x
              (let ([result (simplify (product (num 0) (var 'x)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test simplify-one-times-x
              (let ([result (simplify (product (num 1) (var 'x)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test simplify-x-minus-zero
              (let ([result (simplify (difference (var 'x) (num 0)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test simplify-x-minus-x
              (let ([result (simplify (difference (var 'x) (var 'x)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test simplify-x-over-x
              (let ([result (simplify (quotient (var 'x) (var 'x)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test simplify-x-power-zero
              (let ([result (simplify (power (var 'x) (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test simplify-x-power-one
              (let ([result (simplify (power (var 'x) (num 1)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result)))))

;;; ====
;;; Collecting Like Terms Tests
;;; ====

(test-group collect-terms
            (define-test x-plus-x
              (let ([result (simplify (sum (var 'x) (var 'x)))])
                   (assert-true (product? result))))
            
            (define-test two-x-plus-three-x
              (let ([result (simplify (sum (product (num 2) (var 'x))
                                           (product (num 3) (var 'x))))])
                   (assert-true (product? result))))
            
            (define-test numeric-folding
              (let ([result (simplify (sum (num 2) (num 3)))])
                   (assert-true (num? result))
                   (assert-equal 5 (num-val result)))))

;;; ====
;;; Power Collection Tests
;;; ====

(test-group power-collection
            (define-test x-times-x
              (let ([result (simplify (product (var 'x) (var 'x)))])
                   (assert-true (power? result))
                   (assert-true (num? (pow-exp result)))
                   (assert-equal 2 (num-val (pow-exp result)))))
            
            (define-test x-times-x-times-x
              (let ([result (simplify (make-product (list (var 'x) (var 'x) (var 'x))))])
                   (assert-true (power? result))
                   (assert-true (num? (pow-exp result)))
                   (assert-equal 3 (num-val (pow-exp result)))))
            
            (define-test x2-times-x
              (let ([result (simplify (product (power (var 'x) (num 2)) (var 'x)))])
                   (assert-true (power? result))
                   (assert-true (num? (pow-exp result)))
                   (assert-equal 3 (num-val (pow-exp result)))))
            
            (define-test x2-times-x3
              (let ([result (simplify (product (power (var 'x) (num 2))
                                               (power (var 'x) (num 3))))])
                   (assert-true (power? result))
                   (assert-true (num? (pow-exp result)))
                   (assert-equal 5 (num-val (pow-exp result))))))

;;; ====
;;; Expansion Tests
;;; ====

(test-group expansion
            (define-test expand-a-plus-b-times-c
              (let ([result (expand (product (sum (var 'a) (var 'b)) (var 'c)))])
                   (assert-true (sum? result))))
            
            (define-test expand-two-times-sum
              (let ([result (expand (product (num 2) (sum (var 'x) (var 'y))))])
                   (assert-true (sum? result)))))

;;; ====
;;; Factoring Tests
;;; ====

(test-group factoring
            (define-test factor-common
              (let ([result (factor (sum (product (num 2) (var 'x))
                                         (product (num 2) (var 'y))))])
                   (assert-true (product? result))))
            
            (define-test factor-diff-of-squares
              (let ([result (factor (difference (power (var 'x) (num 2))
                                                (power (var 'y) (num 2))))])
                   (assert-true (product? result)))))

;;; ====
;;; Trigonometric Simplification Tests
;;; ====

(test-group trig-simplify
            (define-test sin-zero
              (let ([result (simplify (sym-sin (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test cos-zero
              (let ([result (simplify (sym-cos (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test sin-over-cos-is-tan
              (let ([result (simplify-trig (quotient (sym-sin (var 'x))
                                                     (sym-cos (var 'x))))])
                   (assert-true (app? result))
                   (assert-equal 'tan (app-fn result))))
            
            (define-test pythagorean-identity
              (let ([result (simplify-trig
                             (sum (power (sym-sin (var 'x)) (num 2))
                                  (power (sym-cos (var 'x)) (num 2))))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result)))))

;;; ====
;;; Exponential/Logarithm Tests
;;; ====

(test-group exp-log
            (define-test exp-zero
              (let ([result (simplify (sym-exp (num 0)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test log-one
              (let ([result (simplify (sym-log (num 1)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test log-e
              (let ([result (simplify (sym-log (var 'e)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test exp-of-log
              (let ([result (simplify (sym-exp (sym-log (var 'x))))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test log-of-exp
              (let ([result (simplify (sym-log (sym-exp (var 'x))))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test log-of-power
              (let ([result (simplify (sym-log (power (var 'x) (num 3))))])
                   (assert-true (product? result)))))

;;; ====
;;; Quotient Simplification Tests
;;; ====

(test-group quotient-simplify
            (define-test zero-over-x
              (let ([result (simplify (quotient (num 0) (var 'x)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test x-over-one
              (let ([result (simplify (quotient (var 'x) (num 1)))])
                   (assert-true (var? result))
                   (assert-equal 'x (var-name result))))
            
            (define-test six-over-three
              (let ([result (simplify (quotient (num 6) (num 3)))])
                   (assert-true (num? result))
                   (assert-equal 2 (num-val result)))))

;;; ====
;;; Power Simplification Tests
;;; ====

(test-group power-simplify
            (define-test zero-squared
              (let ([result (simplify (power (num 0) (num 2)))])
                   (assert-true (num? result))
                   (assert-equal 0 (num-val result))))
            
            (define-test one-to-any-power
              (let ([result (simplify (power (num 1) (num 100)))])
                   (assert-true (num? result))
                   (assert-equal 1 (num-val result))))
            
            (define-test two-cubed
              (let ([result (simplify (power (num 2) (num 3)))])
                   (assert-true (num? result))
                   (assert-equal 8 (num-val result))))
            
            (define-test power-of-power
              (let ([result (simplify (power (power (var 'x) (num 2)) (num 3)))])
                   (assert-true (power? result))
                   (assert-true (num? (pow-exp result)))
                   (assert-equal 6 (num-val (pow-exp result))))))

;;; ====
;;; Canonical Form Tests
;;; ====

(test-group canonical
            (define-test canonical-sorts-sums
              (let ([result (to-canonical (sum (power (var 'x) (num 2)) (var 'x)))])
                   (assert-true (sum? result))))
            
            (define-test canonical-sorts-products
              (let ([result (to-canonical (product (sym-sin (var 'x)) (num 2)))])
                   (assert-true (product? result)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
