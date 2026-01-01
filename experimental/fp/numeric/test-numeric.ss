;;; fabric/stitches/fp/test-numeric.ss -- Tests for Numeric Type Classes

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/numeric/numeric.ss")

(display "
")
(display "==============================================================
")
(display "         NUMERIC TYPE CLASS TOWER TESTS
")
(display "==============================================================
")

;;; Helper: assert-within for floating-point tolerance
(define (assert-within tolerance expected actual)
  (unless (< (abs (- expected actual)) tolerance)
          (error 'assert-within
                 (format "Expected ~a to be within ~a of ~a, but got difference ~a"
                         actual tolerance expected (abs (- expected actual))))))

;;; ============================================================
;;; Num Type Class Tests
;;; ============================================================

(test-group num-typeclass
            (define-test num-creation-test
              (assert-true (num? integer-num))
              (assert-true (num? flonum-num))
              (assert-true (num? scheme-num)))
            
            (define-test integer-num-plus-test
              (assert-equal 5 (num+ integer-num 2 3))
              (assert-equal 0 (num+ integer-num -5 5))
              (assert-equal -10 (num+ integer-num -3 -7)))
            
            (define-test integer-num-minus-test
              (assert-equal 2 (num- integer-num 5 3))
              (assert-equal -8 (num- integer-num -5 3))
              (assert-equal 0 (num- integer-num 5 5)))
            
            (define-test integer-num-times-test
              (assert-equal 6 (num* integer-num 2 3))
              (assert-equal -15 (num* integer-num -5 3))
              (assert-equal 0 (num* integer-num 0 100)))
            
            (define-test integer-num-negate-test
              (assert-equal -5 (num-neg integer-num 5))
              (assert-equal 5 (num-neg integer-num -5))
              (assert-equal 0 (num-neg integer-num 0)))
            
            (define-test integer-num-abs-test
              (assert-equal 5 ((num-abs integer-num) 5))
              (assert-equal 5 ((num-abs integer-num) -5))
              (assert-equal 0 ((num-abs integer-num) 0)))
            
            (define-test integer-num-signum-test
              (assert-equal 1 ((num-signum integer-num) 5))
              (assert-equal -1 ((num-signum integer-num) -5))
              (assert-equal 0 ((num-signum integer-num) 0)))
            
            (define-test integer-from-integer-test
              (assert-equal 42 ((num-from-integer integer-num) 42))
              (assert-equal -7 ((num-from-integer integer-num) -7)))
            
            (define-test flonum-num-plus-test
              (assert-within 0.001 5.5 (num+ flonum-num 2.0 3.5))
              (assert-within 0.001 0.0 (num+ flonum-num -5.0 5.0)))
            
            (define-test flonum-num-times-test
              (assert-within 0.001 7.0 (num* flonum-num 2.0 3.5))
              (assert-within 0.001 -15.0 (num* flonum-num -5.0 3.0)))
            
            (define-test num-sum-test
              (assert-equal 15 (num-sum integer-num '(1 2 3 4 5)))
              (assert-equal 0 (num-sum integer-num '()))
              (assert-within 0.001 15.0 (num-sum flonum-num '(1.0 2.0 3.0 4.0 5.0))))
            
            (define-test num-product-test
              (assert-equal 120 (num-product integer-num '(1 2 3 4 5)))
              (assert-equal 1 (num-product integer-num '()))
              (assert-within 0.001 120.0 (num-product flonum-num '(1.0 2.0 3.0 4.0 5.0)))))

;;; ============================================================
;;; Integral Type Class Tests
;;; ============================================================

(test-group integral-typeclass
            (define-test integral-creation-test
              (assert-true (integral? integer-integral))
              (assert-true (integral? scheme-integral)))
            
            (define-test integer-quotient-test
              (assert-equal 3 (quot integer-integral 7 2))
              (assert-equal -3 (quot integer-integral -7 2))
              (assert-equal -3 (quot integer-integral 7 -2))
              (assert-equal 3 (quot integer-integral -7 -2)))
            
            (define-test integer-remainder-test
              (assert-equal 1 (rem integer-integral 7 2))
              (assert-equal -1 (rem integer-integral -7 2))
              (assert-equal 1 (rem integer-integral 7 -2))
              (assert-equal -1 (rem integer-integral -7 -2)))
            
            (define-test integer-div-test
              (assert-equal 3 (int-div integer-integral 7 2))
              (assert-equal -4 (int-div integer-integral -7 2)))
            
            (define-test integer-mod-test
              (assert-equal 1 (int-mod integer-integral 7 2))
              (assert-equal 1 (int-mod integer-integral -7 2)))
            
            (define-test quot-rem-test
              (let ([result (quot-rem integer-integral 7 3)])
                   (assert-equal 2 (car result))
                   (assert-equal 1 (cdr result))))
            
            (define-test div-mod-test
              (let ([result (div-mod integer-integral 7 3)])
                   (assert-equal 2 (car result))
                   (assert-equal 1 (cdr result))))
            
            (define-test int-even-odd-test
              (assert-true (int-even? integer-integral 4))
              (assert-false (int-even? integer-integral 5))
              (assert-true (int-odd? integer-integral 5))
              (assert-false (int-odd? integer-integral 4))
              (assert-true (int-even? integer-integral 0)))
            
            (define-test int-gcd-test
              (assert-equal 6 (int-gcd integer-integral 12 18))
              (assert-equal 1 (int-gcd integer-integral 7 11))
              (assert-equal 5 (int-gcd integer-integral 0 5))
              (assert-equal 5 (int-gcd integer-integral 5 0))
              (assert-equal 4 (int-gcd integer-integral -12 8)))
            
            (define-test int-lcm-test
              (assert-equal 36 (int-lcm integer-integral 12 18))
              (assert-equal 77 (int-lcm integer-integral 7 11))
              (assert-equal 0 (int-lcm integer-integral 0 5))))

;;; ============================================================
;;; Fractional Type Class Tests
;;; ============================================================

(test-group fractional-typeclass
            (define-test fractional-creation-test
              (assert-true (fractional? flonum-fractional))
              (assert-true (fractional? scheme-fractional)))
            
            (define-test flonum-div-test
              (assert-within 0.001 2.5 (frac/ flonum-fractional 5.0 2.0))
              (assert-within 0.001 -2.5 (frac/ flonum-fractional -5.0 2.0))
              (assert-within 0.001 0.5 (frac/ flonum-fractional 1.0 2.0)))
            
            (define-test flonum-recip-test
              (assert-within 0.001 0.5 (frac-recip flonum-fractional 2.0))
              (assert-within 0.001 0.25 (frac-recip flonum-fractional 4.0))
              (assert-within 0.001 -0.5 (frac-recip flonum-fractional -2.0)))
            
            (define-test scheme-div-test
              (assert-equal 5/2 (frac/ scheme-fractional 5 2))
              (assert-equal 1/3 (frac/ scheme-fractional 1 3))))

;;; ============================================================
;;; RealFrac Type Class Tests
;;; ============================================================

(test-group realfrac-typeclass
            (define-test realfrac-creation-test
              (assert-true (realfrac? flonum-realfrac))
              (assert-true (realfrac? scheme-realfrac)))
            
            (define-test flonum-truncate-test
              (assert-within 0.001 3.0 (frac-truncate flonum-realfrac 3.7))
              (assert-within 0.001 -3.0 (frac-truncate flonum-realfrac -3.7))
              (assert-within 0.001 3.0 (frac-truncate flonum-realfrac 3.2)))
            
            (define-test flonum-round-test
              (assert-within 0.001 4.0 (frac-round flonum-realfrac 3.7))
              (assert-within 0.001 3.0 (frac-round flonum-realfrac 3.2))
              (assert-within 0.001 -4.0 (frac-round flonum-realfrac -3.7)))
            
            (define-test flonum-ceiling-test
              (assert-within 0.001 4.0 (frac-ceiling flonum-realfrac 3.2))
              (assert-within 0.001 -3.0 (frac-ceiling flonum-realfrac -3.7))
              (assert-within 0.001 3.0 (frac-ceiling flonum-realfrac 3.0)))
            
            (define-test flonum-floor-test
              (assert-within 0.001 3.0 (frac-floor flonum-realfrac 3.7))
              (assert-within 0.001 -4.0 (frac-floor flonum-realfrac -3.7))
              (assert-within 0.001 3.0 (frac-floor flonum-realfrac 3.0))))

;;; ============================================================
;;; Floating Type Class Tests
;;; ============================================================

(test-group floating-typeclass
            (define-test floating-creation-test
              (assert-true (floating? flonum-floating))
              (assert-true (floating? scheme-floating)))
            
            (define-test floating-pi-test
              (assert-within 0.0001 3.14159 (float-pi flonum-floating))
              (assert-within 0.0001 3.14159 (float-pi scheme-floating)))
            
            (define-test floating-exp-log-test
              (assert-within 0.001 2.718 (float-exp flonum-floating 1.0))
              (assert-within 0.001 1.0 (float-log flonum-floating 2.718281828))
              (assert-within 0.001 0.0 (float-log flonum-floating 1.0)))
            
            (define-test floating-sqrt-test
              (assert-within 0.001 2.0 (float-sqrt flonum-floating 4.0))
              (assert-within 0.001 3.0 (float-sqrt flonum-floating 9.0))
              (assert-within 0.001 1.414 (float-sqrt flonum-floating 2.0)))
            
            (define-test floating-power-test
              (assert-within 0.001 8.0 (float-power flonum-floating 2.0 3.0))
              (assert-within 0.001 27.0 (float-power flonum-floating 3.0 3.0))
              (assert-within 0.001 1.0 (float-power flonum-floating 5.0 0.0)))
            
            (define-test floating-sin-cos-test
              (assert-within 0.001 0.0 (float-sin flonum-floating 0.0))
              (assert-within 0.001 1.0 (float-cos flonum-floating 0.0))
              (assert-within 0.001 1.0 (float-sin flonum-floating (/ (float-pi flonum-floating) 2.0)))
              (assert-within 0.001 0.0 (float-cos flonum-floating (/ (float-pi flonum-floating) 2.0))))
            
            (define-test floating-tan-test
              (assert-within 0.001 0.0 (float-tan flonum-floating 0.0))
              (assert-within 0.001 1.0 (float-tan flonum-floating (/ (float-pi flonum-floating) 4.0))))
            
            (define-test floating-asin-acos-atan-test
              (assert-within 0.001 0.0 (float-asin flonum-floating 0.0))
              (assert-within 0.001 (/ (float-pi flonum-floating) 2.0) (float-acos flonum-floating 0.0))
              (assert-within 0.001 0.0 (float-atan flonum-floating 0.0)))
            
            (define-test floating-sinh-cosh-test
              (assert-within 0.001 0.0 (float-sinh flonum-floating 0.0))
              (assert-within 0.001 1.0 (float-cosh flonum-floating 0.0))
              (assert-within 0.001 1.175 (float-sinh flonum-floating 1.0))
              (assert-within 0.001 1.543 (float-cosh flonum-floating 1.0)))
            
            (define-test floating-log-base-test
              (assert-within 0.001 3.0 (float-log-base flonum-floating 2.0 8.0))
              (assert-within 0.001 2.0 (float-log-base flonum-floating 10.0 100.0))))

;;; ============================================================
;;; Complex Number Tests
;;; ============================================================

(test-group complex-numbers
            (define-test complex-num-creation-test
              (assert-true (num? complex-num)))
            
            (define-test complex-plus-test
              (let ([a (make-rectangular 1 2)]
                    [b (make-rectangular 3 4)])
                   (let ([result (num+ complex-num a b)])
                        (assert-within 0.001 4.0 (real-part result))
                        (assert-within 0.001 6.0 (imag-part result)))))
            
            (define-test complex-minus-test
              (let ([a (make-rectangular 5 7)]
                    [b (make-rectangular 2 3)])
                   (let ([result (num- complex-num a b)])
                        (assert-within 0.001 3.0 (real-part result))
                        (assert-within 0.001 4.0 (imag-part result)))))
            
            (define-test complex-times-test
              (let ([a (make-rectangular 1 2)]   ; 1 + 2i
                    [b (make-rectangular 3 4)])  ; 3 + 4i
                   ; (1+2i)(3+4i) = 3 + 4i + 6i + 8i^2 = 3 + 10i - 8 = -5 + 10i
                   (let ([result (num* complex-num a b)])
                        (assert-within 0.001 -5.0 (real-part result))
                        (assert-within 0.001 10.0 (imag-part result)))))
            
            (define-test complex-negate-test
              (let ([a (make-rectangular 3 -4)])
                   (let ([result (num-neg complex-num a)])
                        (assert-within 0.001 -3.0 (real-part result))
                        (assert-within 0.001 4.0 (imag-part result))))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(test-group utility-functions
            (define-test generic-sum-test
              (assert-equal 15 (generic-sum integer-num '(1 2 3 4 5)))
              (assert-within 0.001 15.0 (generic-sum flonum-num '(1.0 2.0 3.0 4.0 5.0))))
            
            (define-test generic-product-test
              (assert-equal 120 (generic-product integer-num '(1 2 3 4 5)))
              (assert-within 0.001 120.0 (generic-product flonum-num '(1.0 2.0 3.0 4.0 5.0))))
            
            (define-test generic-mean-test
              (assert-within 0.001 3.0 (generic-mean flonum-fractional '(1.0 2.0 3.0 4.0 5.0)))
              (assert-within 0.001 2.5 (generic-mean flonum-fractional '(1.0 2.0 3.0 4.0)))
              (assert-within 0.001 0.0 (generic-mean flonum-fractional '())))
            
            (define-test generic-variance-test
              ; Variance of (1, 2, 3, 4, 5) = 2.5
              (assert-within 0.001 2.5 (generic-variance flonum-floating '(1.0 2.0 3.0 4.0 5.0))))
            
            (define-test generic-stddev-test
              ; Stddev of (1, 2, 3, 4, 5) = sqrt(2.5) ~ 1.58
              (assert-within 0.01 1.58 (generic-stddev flonum-floating '(1.0 2.0 3.0 4.0 5.0))))
            
            (define-test from-int-test
              (assert-equal 42 (from-int integer-num 42))
              (assert-within 0.001 42.0 (from-int flonum-num 42)))
            
            (define-test num-zero-one-test
              (assert-equal 0 (num-zero integer-num))
              (assert-equal 1 (num-one integer-num))
              (assert-within 0.001 0.0 (num-zero flonum-num))
              (assert-within 0.001 1.0 (num-one flonum-num))))

;;; ============================================================
;;; Laws Tests (Algebraic Properties)
;;; ============================================================

(test-group numeric-laws
            ;; Num laws: (+) forms a commutative group
            (define-test num-plus-commutative
              (assert-equal (num+ integer-num 3 5) (num+ integer-num 5 3))
              (assert-within 0.001 (num+ flonum-num 3.5 2.1) (num+ flonum-num 2.1 3.5)))
            
            (define-test num-plus-associative
              (assert-equal
               (num+ integer-num (num+ integer-num 1 2) 3)
               (num+ integer-num 1 (num+ integer-num 2 3))))
            
            (define-test num-plus-identity
              (assert-equal 5 (num+ integer-num 5 (num-zero integer-num)))
              (assert-equal 5 (num+ integer-num (num-zero integer-num) 5)))
            
            (define-test num-plus-inverse
              (assert-equal (num-zero integer-num)
                            (num+ integer-num 5 (num-neg integer-num 5))))
            
            ;; (*) forms a commutative monoid
            (define-test num-times-commutative
              (assert-equal (num* integer-num 3 5) (num* integer-num 5 3)))
            
            (define-test num-times-associative
              (assert-equal
               (num* integer-num (num* integer-num 2 3) 4)
               (num* integer-num 2 (num* integer-num 3 4))))
            
            (define-test num-times-identity
              (assert-equal 5 (num* integer-num 5 (num-one integer-num)))
              (assert-equal 5 (num* integer-num (num-one integer-num) 5)))
            
            ;; Distributive law
            (define-test num-distributive
              (assert-equal
               (num* integer-num 2 (num+ integer-num 3 4))
               (num+ integer-num (num* integer-num 2 3) (num* integer-num 2 4)))))

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
[SUCCESS] All numeric type class tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
