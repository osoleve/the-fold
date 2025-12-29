;;; fabric/stitches/fp/test-interval-elem.ss -- Tests for Interval Elementary Functions

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/interval-elem.ss")

(display "
")
(display "==============================================================
")
(display "          INTERVAL ELEMENTARY FUNCTION TESTS
")
(display "==============================================================
")

;;; Helper for floating-point comparison
(define (assert-within tolerance expected actual)
  (unless (< (abs (- expected actual)) tolerance)
          (error 'assert-within
                 (format "Expected ~a +/- ~a, got ~a (diff: ~a)"
                         expected tolerance actual (abs (- expected actual))))))

;;; Helper to check interval contains a value
(define (assert-interval-contains iv x tolerance)
  (unless (and (<= (- (interval-lo iv) tolerance) x)
               (<= x (+ (interval-hi iv) tolerance)))
          (error 'assert-interval-contains
                 (format "Expected ~a to contain ~a (bounds: [~a, ~a])"
                         (interval->string iv) x
                         (interval-lo iv) (interval-hi iv)))))

;;; ============================================================
;;; Constants Tests
;;; ============================================================

(test-group constants
            (define-test interval-pi-test
              (assert-interval-contains interval-pi 3.14159265358979 0.00001))
            
            (define-test interval-e-test
              (assert-interval-contains interval-e 2.71828182845905 0.00001))
            
            (define-test interval-pi/2-test
              (assert-interval-contains interval-pi/2 1.5707963267949 0.00001)))

;;; ============================================================
;;; Square Root Tests
;;; ============================================================

(test-group sqrt-tests
            ;; sqrt([4, 9]) = [2, 3]
            (define-test sqrt-positive-test
              (let ([result (interval-sqrt (make-interval 4 9))])
                   (assert-true (just? result))
                   (assert-within 0.001 2.0 (interval-lo (from-just result)))
                   (assert-within 0.001 3.0 (interval-hi (from-just result)))))
            
            ;; sqrt([0, 4]) = [0, 2]
            (define-test sqrt-includes-zero-test
              (let ([result (interval-sqrt (make-interval 0 4))])
                   (assert-true (just? result))
                   (assert-within 0.001 0.0 (interval-lo (from-just result)))
                   (assert-within 0.001 2.0 (interval-hi (from-just result)))))
            
            ;; sqrt([-4, -1]) = nothing
            (define-test sqrt-negative-test
              (let ([result (interval-sqrt (make-interval -4 -1))])
                   (assert-true (nothing? result))))
            
            ;; sqrt([-1, 4]) = [0, 2] (clamps negative part)
            (define-test sqrt-mixed-test
              (let ([result (interval-sqrt (make-interval -1 4))])
                   (assert-true (just? result))
                   (assert-within 0.001 0.0 (interval-lo (from-just result)))
                   (assert-within 0.001 2.0 (interval-hi (from-just result)))))
            
            ;; sqrt([1, 1]) = [1, 1]
            (define-test sqrt-singleton-test
              (let ([result (interval-sqrt (interval-singleton 1))])
                   (assert-true (just? result))
                   (assert-true (interval-singleton? (from-just result))))))

;;; ============================================================
;;; Exponential Tests
;;; ============================================================

(test-group exp-tests
            ;; exp([0, 1]) = [1, e]
            (define-test exp-basic-test
              (let ([result (interval-exp (make-interval 0 1))])
                   (assert-within 0.001 1.0 (interval-lo result))
                   (assert-within 0.001 2.71828 (interval-hi result))))
            
            ;; exp([-1, 1]) = [1/e, e]
            (define-test exp-symmetric-test
              (let ([result (interval-exp (make-interval -1 1))])
                   (assert-within 0.001 0.36788 (interval-lo result))
                   (assert-within 0.001 2.71828 (interval-hi result))))
            
            ;; exp([0, 0]) = [1, 1]
            (define-test exp-zero-test
              (let ([result (interval-exp (interval-singleton 0))])
                   (assert-within 0.001 1.0 (interval-lo result))
                   (assert-within 0.001 1.0 (interval-hi result)))))

;;; ============================================================
;;; Logarithm Tests
;;; ============================================================

(test-group log-tests
            ;; log([1, e]) = [0, 1]
            (define-test log-basic-test
              (let ([result (interval-log (make-interval 1 2.71828))])
                   (assert-true (just? result))
                   (assert-within 0.001 0.0 (interval-lo (from-just result)))
                   (assert-within 0.001 1.0 (interval-hi (from-just result)))))
            
            ;; log([e, e^2]) = [1, 2]
            (define-test log-higher-test
              (let ([result (interval-log (make-interval 2.71828 7.38906))])
                   (assert-true (just? result))
                   (assert-within 0.001 1.0 (interval-lo (from-just result)))
                   (assert-within 0.001 2.0 (interval-hi (from-just result)))))
            
            ;; log([-1, -0.5]) = nothing
            (define-test log-negative-test
              (let ([result (interval-log (make-interval -1 -0.5))])
                   (assert-true (nothing? result))))
            
            ;; log([0.5, 2]) contains log(0.5) and log(2)
            (define-test log-unit-crossing-test
              (let ([result (interval-log (make-interval 0.5 2))])
                   (assert-true (just? result))
                   (assert-within 0.001 -0.693 (interval-lo (from-just result)))
                   (assert-within 0.001 0.693 (interval-hi (from-just result))))))

;;; ============================================================
;;; Trigonometric Tests
;;; ============================================================

(test-group trig-tests
            ;; sin([0, pi/2]) = [0, 1]
            (define-test sin-first-quadrant-test
              (let ([result (interval-sin (make-interval 0 1.5708))])
                   (assert-within 0.001 0.0 (interval-lo result))
                   (assert-within 0.001 1.0 (interval-hi result))))
            
            ;; sin([0, pi]) = [0, 1] (contains maximum)
            (define-test sin-half-period-test
              (let ([result (interval-sin (make-interval 0 3.14159))])
                   (assert-within 0.01 0.0 (interval-lo result))
                   (assert-within 0.01 1.0 (interval-hi result))))
            
            ;; sin([0, 2pi]) = [-1, 1] (full period)
            (define-test sin-full-period-test
              (let ([result (interval-sin (make-interval 0 6.28318))])
                   (assert-within 0.01 -1.0 (interval-lo result))
                   (assert-within 0.01 1.0 (interval-hi result))))
            
            ;; cos([0, pi/2]) = [0, 1]
            (define-test cos-first-quadrant-test
              (let ([result (interval-cos (make-interval 0 1.5708))])
                   (assert-within 0.01 0.0 (interval-lo result))
                   (assert-within 0.01 1.0 (interval-hi result))))
            
            ;; cos([0, pi]) = [-1, 1] (contains minimum)
            (define-test cos-half-period-test
              (let ([result (interval-cos (make-interval 0 3.14159))])
                   (assert-within 0.01 -1.0 (interval-lo result))
                   (assert-within 0.01 1.0 (interval-hi result))))
            
            ;; tan([0, 0.5]) - safe range
            (define-test tan-safe-range-test
              (let ([result (interval-tan (make-interval 0 0.5))])
                   (assert-true (just? result))
                   (assert-within 0.01 0.0 (interval-lo (from-just result)))
                   (assert-within 0.01 0.546 (interval-hi (from-just result)))))
            
            ;; tan([0, 2]) - crosses pi/2, should return nothing
            (define-test tan-crosses-asymptote-test
              (let ([result (interval-tan (make-interval 0 2))])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Inverse Trigonometric Tests
;;; ============================================================

(test-group inverse-trig-tests
            ;; asin([0, 1]) = [0, pi/2]
            (define-test asin-basic-test
              (let ([result (interval-asin (make-interval 0 1))])
                   (assert-true (just? result))
                   (assert-within 0.01 0.0 (interval-lo (from-just result)))
                   (assert-within 0.01 1.5708 (interval-hi (from-just result)))))
            
            ;; asin([-1, 1]) = [-pi/2, pi/2]
            (define-test asin-full-domain-test
              (let ([result (interval-asin (make-interval -1 1))])
                   (assert-true (just? result))
                   (assert-within 0.01 -1.5708 (interval-lo (from-just result)))
                   (assert-within 0.01 1.5708 (interval-hi (from-just result)))))
            
            ;; asin([2, 3]) = nothing (outside domain)
            (define-test asin-outside-domain-test
              (let ([result (interval-asin (make-interval 2 3))])
                   (assert-true (nothing? result))))
            
            ;; acos([0, 1]) = [0, pi/2]
            (define-test acos-basic-test
              (let ([result (interval-acos (make-interval 0 1))])
                   (assert-true (just? result))
                   (assert-within 0.01 0.0 (interval-lo (from-just result)))
                   (assert-within 0.01 1.5708 (interval-hi (from-just result)))))
            
            ;; atan([-1, 1]) = [-pi/4, pi/4]
            (define-test atan-basic-test
              (let ([result (interval-atan (make-interval -1 1))])
                   (assert-within 0.01 -0.7854 (interval-lo result))
                   (assert-within 0.01 0.7854 (interval-hi result)))))

;;; ============================================================
;;; Hyperbolic Tests
;;; ============================================================

(test-group hyperbolic-tests
            ;; sinh([0, 1]) = [0, sinh(1)]
            (define-test sinh-basic-test
              (let ([result (interval-sinh (make-interval 0 1))])
                   (assert-within 0.01 0.0 (interval-lo result))
                   (assert-within 0.01 1.1752 (interval-hi result))))
            
            ;; sinh is odd: sinh([-1, 1]) = [-sinh(1), sinh(1)]
            (define-test sinh-symmetric-test
              (let ([result (interval-sinh (make-interval -1 1))])
                   (assert-within 0.01 -1.1752 (interval-lo result))
                   (assert-within 0.01 1.1752 (interval-hi result))))
            
            ;; cosh([0, 1]) = [1, cosh(1)]
            (define-test cosh-positive-test
              (let ([result (interval-cosh (make-interval 0 1))])
                   (assert-within 0.01 1.0 (interval-lo result))
                   (assert-within 0.01 1.5431 (interval-hi result))))
            
            ;; cosh([-1, 1]) = [1, cosh(1)] (minimum at 0)
            (define-test cosh-symmetric-test
              (let ([result (interval-cosh (make-interval -1 1))])
                   (assert-within 0.01 1.0 (interval-lo result))
                   (assert-within 0.01 1.5431 (interval-hi result))))
            
            ;; tanh([0, 1]) = [0, tanh(1)]
            (define-test tanh-basic-test
              (let ([result (interval-tanh (make-interval 0 1))])
                   (assert-within 0.01 0.0 (interval-lo result))
                   (assert-within 0.01 0.7616 (interval-hi result)))))

;;; ============================================================
;;; Inverse Hyperbolic Tests
;;; ============================================================

(test-group inverse-hyperbolic-tests
            ;; asinh([0, 1]) = [0, asinh(1)]
            (define-test asinh-basic-test
              (let ([result (interval-asinh (make-interval 0 1))])
                   (assert-within 0.01 0.0 (interval-lo result))
                   (assert-within 0.01 0.8814 (interval-hi result))))
            
            ;; acosh([1, 2]) = [0, acosh(2)]
            (define-test acosh-basic-test
              (let ([result (interval-acosh (make-interval 1 2))])
                   (assert-true (just? result))
                   (assert-within 0.01 0.0 (interval-lo (from-just result)))
                   (assert-within 0.01 1.3170 (interval-hi (from-just result)))))
            
            ;; acosh([0, 0.5]) = nothing (outside domain)
            (define-test acosh-outside-domain-test
              (let ([result (interval-acosh (make-interval 0 0.5))])
                   (assert-true (nothing? result))))
            
            ;; atanh([-0.5, 0.5]) = [atanh(-0.5), atanh(0.5)]
            (define-test atanh-basic-test
              (let ([result (interval-atanh (make-interval -0.5 0.5))])
                   (assert-true (just? result))
                   (assert-within 0.01 -0.5493 (interval-lo (from-just result)))
                   (assert-within 0.01 0.5493 (interval-hi (from-just result)))))
            
            ;; atanh([-1, 1]) = nothing (touches domain boundary)
            (define-test atanh-boundary-test
              (let ([result (interval-atanh (make-interval -1 1))])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Power Function Tests
;;; ============================================================

(test-group power-tests
            ;; [2, 3]^2 = [4, 9] using interval-pow-real
            (define-test pow-square-test
              (let ([result (interval-pow-real (make-interval 2 3) 2)])
                   (assert-within 0.01 4.0 (interval-lo result))
                   (assert-within 0.01 9.0 (interval-hi result))))
            
            ;; [4, 9]^0.5 = [2, 3]
            (define-test pow-sqrt-test
              (let ([result (interval-pow-real (make-interval 4 9) 0.5)])
                   (assert-within 0.01 2.0 (interval-lo result))
                   (assert-within 0.01 3.0 (interval-hi result))))
            
            ;; x^0 = 1
            (define-test pow-zero-exp-test
              (let ([result (interval-pow-real (make-interval 2 5) 0)])
                   (assert-within 0.01 1.0 (interval-lo result))
                   (assert-within 0.01 1.0 (interval-hi result)))))

;;; ============================================================
;;; Composition Tests
;;; ============================================================

(test-group composition-tests
            ;; sqrt(exp([0, 1])) should contain sqrt(1) to sqrt(e)
            (define-test sqrt-exp-composition-test
              (let ([result (interval-sqrt-unsafe (interval-exp (make-interval 0 1)))])
                   (assert-within 0.01 1.0 (interval-lo result))
                   (assert-within 0.01 1.6487 (interval-hi result))))
            
            ;; log(exp([1, 2])) should be close to [1, 2]
            (define-test log-exp-inverse-test
              (let ([result (interval-log-unsafe (interval-exp (make-interval 1 2)))])
                   (assert-within 0.001 1.0 (interval-lo result))
                   (assert-within 0.001 2.0 (interval-hi result))))
            
            ;; sin^2 + cos^2 should contain 1
            (define-test pythagorean-identity-test
              (let* ([iv (make-interval 0 1)]
                     [sin-sq (interval-square (interval-sin iv))]
                     [cos-sq (interval-square (interval-cos iv))]
                     [sum (interval-add sin-sq cos-sq)])
                    (assert-interval-contains sum 1.0 0.01))))

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
[SUCCESS] All interval elementary function tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
