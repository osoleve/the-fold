;;; lattice/autodiff/test-interval-autodiff.ss --- Tests for Interval Autodiff
;;;
;;; Tests for interval-valued automatic differentiation including:
;;;   - Basic interval arithmetic operations
;;;   - Interval gradient computation
;;;   - Monotonicity analysis
;;;   - Integration with interval global optimization

(load "core/testing/test-framework.ss")
(load "lattice/autodiff/interval-autodiff.ss")

;;; ============================================================================
;;; Helper Functions
;;; ============================================================================

;;; approx-interval=? : Interval x Interval x Real -> Boolean
;;; Check if two intervals are approximately equal.
(define (approx-interval=? iv1 iv2 tol)
  (and (< (abs (- (interval-lo iv1) (interval-lo iv2))) tol)
       (< (abs (- (interval-hi iv1) (interval-hi iv2))) tol)))

;;; interval-contains-value? : Interval x Real -> Boolean
(define (interval-contains-value? iv x)
  (and (<= (interval-lo iv) x)
       (<= x (interval-hi iv))))

;;; ============================================================================
;;; Test Group: Elementary Interval Functions
;;; ============================================================================

(test-group "interval-elementary-functions"

  (define-test "interval-exp is monotonically increasing"
    (let* ([iv (interval -1 1)]
           [result (interval-exp iv)])
      (assert-true (< (interval-lo result) (interval-hi result)))
      ;; exp(-1) = 0.367..., exp(1) = 2.718...
      (assert-true (< 0.36 (interval-lo result) 0.37))
      (assert-true (< 2.71 (interval-hi result) 2.72))))

  (define-test "interval-log is monotonically increasing"
    (let* ([iv (interval 1 10)]
           [result (interval-log iv)])
      ;; log(1) = 0, log(10) = 2.302...
      (assert-true (< -0.01 (interval-lo result) 0.01))
      (assert-true (< 2.30 (interval-hi result) 2.31))))

  (define-test "interval-sin handles full period"
    (let* ([pi 3.141592653589793]
           [iv (interval 0 (* 2 pi))]
           [result (interval-sin iv)])
      ;; Full period: should be [-1, 1]
      (assert-true (< (interval-lo result) -0.99))
      (assert-true (> (interval-hi result) 0.99))))

  (define-test "interval-cos handles full period"
    (let* ([pi 3.141592653589793]
           [iv (interval 0 (* 2 pi))]
           [result (interval-cos iv)])
      ;; Full period: should be [-1, 1]
      (assert-true (< (interval-lo result) -0.99))
      (assert-true (> (interval-hi result) 0.99))))

  (define-test "interval-sin on small range"
    (let* ([iv (interval 0 0.5)]  ; sin is increasing here
           [result (interval-sin iv)])
      ;; sin(0) = 0, sin(0.5) = 0.479...
      (assert-true (< -0.01 (interval-lo result) 0.01))
      (assert-true (< 0.47 (interval-hi result) 0.49))))

  (define-test "interval-sinh is monotonically increasing"
    (let* ([iv (interval -1 1)]
           [result (interval-sinh iv)])
      ;; sinh(-1) = -1.175..., sinh(1) = 1.175...
      (assert-true (< -1.18 (interval-lo result) -1.17))
      (assert-true (< 1.17 (interval-hi result) 1.18))))

  (define-test "interval-cosh minimum at zero"
    (let* ([iv (interval -1 1)]
           [result (interval-cosh iv)])
      ;; cosh has minimum 1 at x=0, cosh(+/-1) = 1.543...
      (assert-true (< 0.99 (interval-lo result) 1.01))  ; Minimum near 1
      (assert-true (< 1.54 (interval-hi result) 1.55)))))

;;; ============================================================================
;;; Test Group: Interval-Traced Operations
;;; ============================================================================

(test-group "interval-traced-operations"

  (define-test "iv-traced-add computes correct interval"
    (let* ([tape (make-interval-tape)]
           [a (make-interval-traced-var (interval 1 2) tape)]
           [b (make-interval-traced-var (interval 3 4) tape)]
           [result (iv-traced-add a b)]
           [iv (interval-traced-value result)])
      ;; [1,2] + [3,4] = [4,6]
      (assert-equal 4 (interval-lo iv))
      (assert-equal 6 (interval-hi iv))))

  (define-test "iv-traced-mul computes correct interval"
    (let* ([tape (make-interval-tape)]
           [a (make-interval-traced-var (interval 1 2) tape)]
           [b (make-interval-traced-var (interval 3 4) tape)]
           [result (iv-traced-mul a b)]
           [iv (interval-traced-value result)])
      ;; [1,2] * [3,4] = [3,8]
      (assert-equal 3 (interval-lo iv))
      (assert-equal 8 (interval-hi iv))))

  (define-test "iv-traced-sqr handles zero-crossing"
    (let* ([tape (make-interval-tape)]
           [a (make-interval-traced-var (interval -2 1) tape)]
           [result (iv-traced-sqr a)]
           [iv (interval-traced-value result)])
      ;; [-2,1]^2 = [0,4] (minimum at 0)
      (assert-equal 0 (interval-lo iv))
      (assert-equal 4 (interval-hi iv))))

  (define-test "iv-traced-sqrt computes correct interval"
    (let* ([tape (make-interval-tape)]
           [a (make-interval-traced-var (interval 1 4) tape)]
           [result (iv-traced-sqrt a)]
           [iv (interval-traced-value result)])
      ;; sqrt([1,4]) = [1,2]
      (assert-equal 1 (interval-lo iv))
      (assert-equal 2 (interval-hi iv))))

  (define-test "iv-traced-exp computes correct interval"
    (let* ([tape (make-interval-tape)]
           [a (make-interval-traced-var (interval 0 1) tape)]
           [result (iv-traced-exp a)]
           [iv (interval-traced-value result)])
      ;; exp([0,1]) = [1, e]
      (assert-true (< 0.99 (interval-lo iv) 1.01))
      (assert-true (< 2.71 (interval-hi iv) 2.72)))))

;;; ============================================================================
;;; Test Group: Interval Gradient Computation
;;; ============================================================================

(test-group "interval-gradient-computation"

  (define-test "gradient of linear function x"
    ;; f(x) = x, df/dx = 1
    (let* ([f (lambda (inputs) (car inputs))]
           [box (list (interval 0 1))]
           [grad (interval-gradient f box)])
      ;; Gradient should be [1,1]
      (assert-equal 1 (length grad))
      (assert-equal 1 (interval-lo (car grad)))
      (assert-equal 1 (interval-hi (car grad)))))

  (define-test "gradient of x^2"
    ;; f(x) = x^2, df/dx = 2x
    (let* ([f (lambda (inputs) (iv-traced-sqr (car inputs)))]
           [box (list (interval 1 2))]
           [grad (interval-gradient f box)])
      ;; Gradient df/dx = 2x over [1,2] is [2,4]
      (assert-equal 1 (length grad))
      (assert-equal 2 (interval-lo (car grad)))
      (assert-equal 4 (interval-hi (car grad)))))

  (define-test "gradient of x^2 + y^2"
    ;; f(x,y) = x^2 + y^2, df/dx = 2x, df/dy = 2y
    (let* ([f (lambda (inputs)
                (let ([x (car inputs)]
                      [y (cadr inputs)])
                  (iv-traced-add (iv-traced-sqr x) (iv-traced-sqr y))))]
           [box (list (interval 1 2) (interval 3 4))]
           [grad (interval-gradient f box)])
      ;; df/dx = 2x over [1,2] is [2,4]
      ;; df/dy = 2y over [3,4] is [6,8]
      (assert-equal 2 (length grad))
      (assert-equal 2 (interval-lo (car grad)))
      (assert-equal 4 (interval-hi (car grad)))
      (assert-equal 6 (interval-lo (cadr grad)))
      (assert-equal 8 (interval-hi (cadr grad)))))

  (define-test "gradient of x*y"
    ;; f(x,y) = x*y, df/dx = y, df/dy = x
    (let* ([f (lambda (inputs)
                (let ([x (car inputs)]
                      [y (cadr inputs)])
                  (iv-traced-mul x y)))]
           [box (list (interval 1 2) (interval 3 4))]
           [grad (interval-gradient f box)])
      ;; df/dx = y over [3,4]
      ;; df/dy = x over [1,2]
      (assert-equal 2 (length grad))
      (assert-equal 3 (interval-lo (car grad)))
      (assert-equal 4 (interval-hi (car grad)))
      (assert-equal 1 (interval-lo (cadr grad)))
      (assert-equal 2 (interval-hi (cadr grad)))))

  (define-test "gradient of exp(x)"
    ;; f(x) = exp(x), df/dx = exp(x)
    (let* ([f (lambda (inputs) (iv-traced-exp (car inputs)))]
           [box (list (interval 0 1))]
           [grad (interval-gradient f box)])
      ;; df/dx = exp(x) over [0,1] is [1, e]
      (assert-equal 1 (length grad))
      (assert-true (< 0.99 (interval-lo (car grad)) 1.01))
      (assert-true (< 2.71 (interval-hi (car grad)) 2.72))))

  (define-test "gradient of sin(x)"
    ;; f(x) = sin(x), df/dx = cos(x)
    (let* ([f (lambda (inputs) (iv-traced-sin (car inputs)))]
           [pi 3.141592653589793]
           [box (list (interval 0 (/ pi 2)))]  ; [0, pi/2]
           [grad (interval-gradient f box)])
      ;; df/dx = cos(x) over [0, pi/2] is [0, 1]
      (assert-equal 1 (length grad))
      (assert-true (< -0.01 (interval-lo (car grad))))
      (assert-true (> 1.01 (interval-hi (car grad)))))))

;;; ============================================================================
;;; Test Group: Monotonicity Analysis
;;; ============================================================================

(test-group "monotonicity-analysis"

  (define-test "gradient-sign identifies positive intervals"
    (let ([iv (interval 1 2)])
      (assert-equal 'positive (gradient-sign iv))))

  (define-test "gradient-sign identifies negative intervals"
    (let ([iv (interval -3 -1)])
      (assert-equal 'negative (gradient-sign iv))))

  (define-test "gradient-sign identifies zero interval"
    (let ([iv (interval 0 0)])
      (assert-equal 'zero (gradient-sign iv))))

  (define-test "gradient-sign identifies mixed intervals"
    (let ([iv (interval -1 1)])
      (assert-equal 'mixed (gradient-sign iv))))

  (define-test "all-monotonic? returns true when no zeros"
    (let ([grads (list (interval 1 2) (interval 3 4))])
      (assert-true (all-monotonic? grads))))

  (define-test "all-monotonic? returns false when contains zero"
    (let ([grads (list (interval 1 2) (interval -1 1))])
      (assert-false (all-monotonic? grads))))

  (define-test "monotonic-dimension finds first monotonic dim"
    (let ([grads (list (interval -1 1)     ; Contains zero
                       (interval 2 3)      ; Positive - monotonic
                       (interval -3 -1))]) ; Negative - monotonic
      (assert-equal 1 (monotonic-dimension grads))))

  (define-test "monotonic-dimension returns #f when all contain zero"
    (let ([grads (list (interval -1 1) (interval -2 0.5))])
      (assert-false (monotonic-dimension grads)))))

;;; ============================================================================
;;; Test Group: Box Reduction by Monotonicity
;;; ============================================================================

(test-group "box-reduction"

  (define-test "reduce-box-by-monotonicity with positive gradient"
    ;; df/dx > 0: minimum at lower bound
    (let* ([box (list (interval 1 5) (interval 2 4))]
           [grads (list (interval 1 2)      ; Positive: reduce to lower
                        (interval -1 1))]   ; Mixed: keep
           [reduced (reduce-box-by-monotonicity box grads)])
      ;; First dim reduced to singleton at lower bound
      (assert-equal 1 (interval-lo (car reduced)))
      (assert-equal 1 (interval-hi (car reduced)))
      ;; Second dim unchanged
      (assert-equal 2 (interval-lo (cadr reduced)))
      (assert-equal 4 (interval-hi (cadr reduced)))))

  (define-test "reduce-box-by-monotonicity with negative gradient"
    ;; df/dx < 0: minimum at upper bound
    (let* ([box (list (interval 1 5) (interval 2 4))]
           [grads (list (interval -3 -1)    ; Negative: reduce to upper
                        (interval 0 1))]    ; Contains zero: keep
           [reduced (reduce-box-by-monotonicity box grads)])
      ;; First dim reduced to singleton at upper bound
      (assert-equal 5 (interval-lo (car reduced)))
      (assert-equal 5 (interval-hi (car reduced)))
      ;; Second dim unchanged
      (assert-equal 2 (interval-lo (cadr reduced)))
      (assert-equal 4 (interval-hi (cadr reduced)))))

  (define-test "reduce-box-by-monotonicity with all monotonic"
    ;; All gradients definite sign: reduce to corner
    (let* ([box (list (interval 0 10) (interval 0 10))]
           [grads (list (interval 1 5)       ; Positive: lower
                        (interval -5 -1))]   ; Negative: upper
           [reduced (reduce-box-by-monotonicity box grads)])
      ;; Reduced to corner (0, 10)
      (assert-equal 0 (interval-lo (car reduced)))
      (assert-equal 0 (interval-hi (car reduced)))
      (assert-equal 10 (interval-lo (cadr reduced)))
      (assert-equal 10 (interval-hi (cadr reduced))))))

;;; ============================================================================
;;; Test Group: Integration Example
;;; ============================================================================

(test-group "integration-example"

  (define-test "sphere function gradient bounds"
    ;; f(x,y) = x^2 + y^2
    ;; df/dx = 2x, df/dy = 2y
    (let ([sphere-traced (lambda (inputs)
                           (let ([x (car inputs)]
                                 [y (cadr inputs)])
                             (iv-traced-add (iv-traced-sqr x) (iv-traced-sqr y))))])
      ;; Test over box [1,2] x [3,4]
      (let* ([box (list (interval 1 2) (interval 3 4))]
             [grad (interval-gradient sphere-traced box)]
             [info (monotonicity-info grad)])
        ;; df/dx = 2x is positive over [1,2]
        ;; df/dy = 2y is positive over [3,4]
        (assert-equal 'positive (car info))
        (assert-equal 'positive (cadr info))
        ;; So minimum is at corner (1, 3)
        (let ([reduced (reduce-box-by-monotonicity box grad)])
          (assert-equal 1 (interval-lo (car reduced)))
          (assert-equal 3 (interval-lo (cadr reduced)))))))

  (define-test "sphere function gradient at origin"
    ;; f(x,y) = x^2 + y^2
    ;; At origin: df/dx = 0, df/dy = 0
    (let ([sphere-traced (lambda (inputs)
                           (let ([x (car inputs)]
                                 [y (cadr inputs)])
                             (iv-traced-add (iv-traced-sqr x) (iv-traced-sqr y))))])
      ;; Test over box [-1,1] x [-1,1]
      (let* ([box (list (interval -1 1) (interval -1 1))]
             [grad (interval-gradient sphere-traced box)]
             [info (monotonicity-info grad)])
        ;; df/dx = 2x over [-1,1] is [-2,2] (contains 0)
        ;; df/dy = 2y over [-1,1] is [-2,2] (contains 0)
        (assert-equal 'mixed (car info))
        (assert-equal 'mixed (cadr info))
        ;; No monotonicity reduction possible
        (assert-false (all-monotonic? grad))))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
