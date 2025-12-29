;;; fabric/stitches/test-reverse-diff.ss — Tests for Reverse Mode Autodiff

(load "test-framework.ss")
(load "reverse-diff.ss")

;;; Local helper for numeric comparison with tolerance
(define (assert-= actual expected tolerance)
  (unless (< (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " ± ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper: check list of gradients
(define (assert-grads-= actual expected tolerance)
  (for-each (lambda (a e) (assert-= a e tolerance))
            actual expected))

(display "
══════════════════════════════════════════════════════════
         REVERSE MODE AUTODIFF TESTS
══════════════════════════════════════════════════════════
")

;;; ============================================================
;;; Traced Value Basics
;;; ============================================================

(test-group traced-basics
            (define-test traced-value-extraction
              (let* ([tape (make-reverse-tape)]
                     [t (make-traced-var 3.14 tape)])
                    (assert-= (traced-value t) 3.14 0.0001)
                    (assert-true (traced? t))))
            
            (define-test traced-id-is-unique
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [t1 (make-traced-var 1 tape)]
                     [t2 (make-traced-var 2 tape)]
                     [t3 (make-traced-var 3 tape)])
                    (assert-true (not (= (traced-id t1) (traced-id t2))))
                    (assert-true (not (= (traced-id t2) (traced-id t3))))))
            
            (define-test plain-number-extraction
              ;; traced-value should work on plain numbers too
              (assert-= (traced-value 42) 42 0.0001)))

;;; ============================================================
;;; Single-Variable Derivatives
;;; ============================================================

(test-group single-var-derivatives
            (define-test derivative-of-identity
              ;; d/dx[x] = 1
              (assert-= (reverse-diff (lambda (x) x) 5) 1 0.0001))
            
            (define-test derivative-of-constant
              ;; d/dx[5] = 0
              (assert-= (reverse-diff (lambda (x) 5) 3) 0 0.0001))
            
            (define-test derivative-of-double
              ;; d/dx[2x] = 2
              (let ([f (lambda (x) (traced-mul x 2))])
                   (assert-= (reverse-diff f 5) 2 0.0001)))
            
            (define-test derivative-of-square
              ;; d/dx[x²] at x=3 → 6
              (let ([f (lambda (x) (traced-sq x))])
                   (assert-= (reverse-diff f 3) 6 0.0001)))
            
            (define-test derivative-of-cube
              ;; d/dx[x³] at x=2 → 12 (3x²)
              (let ([f (lambda (x) (traced-pow x 3))])
                   (assert-= (reverse-diff f 2) 12 0.0001)))
            
            (define-test derivative-of-sqrt
              ;; d/dx[√x] at x=4 → 0.25
              (let ([f (lambda (x) (traced-sqrt x))])
                   (assert-= (reverse-diff f 4) 0.25 0.0001)))
            
            (define-test derivative-of-exp
              ;; d/dx[e^x] at x=0 → 1
              (let ([f (lambda (x) (traced-exp x))])
                   (assert-= (reverse-diff f 0) 1 0.0001)))
            
            (define-test derivative-of-log
              ;; d/dx[ln(x)] at x=e → 1/e
              (let ([f (lambda (x) (traced-log x))])
                   (assert-= (reverse-diff f (exp 1)) (/ 1 (exp 1)) 0.0001)))
            
            (define-test derivative-of-sin
              ;; d/dx[sin(x)] at x=0 → cos(0) = 1
              (let ([f (lambda (x) (traced-sin x))])
                   (assert-= (reverse-diff f 0) 1 0.0001)))
            
            (define-test derivative-of-cos
              ;; d/dx[cos(x)] at x=0 → -sin(0) = 0
              (let ([f (lambda (x) (traced-cos x))])
                   (assert-= (reverse-diff f 0) 0 0.0001))))

;;; ============================================================
;;; Composite Functions (Chain Rule)
;;; ============================================================

(test-group chain-rule-tests
            (define-test chain-sin-sq
              ;; d/dx[sin(x²)] at x=1 → cos(1) * 2 ≈ 1.08
              (let ([f (lambda (x) (traced-sin (traced-sq x)))])
                   (assert-= (reverse-diff f 1) (* 2 (cos 1)) 0.001)))
            
            (define-test chain-exp-neg
              ;; d/dx[e^(-x)] at x=0 → -1
              (let ([f (lambda (x) (traced-exp (traced-neg x)))])
                   (assert-= (reverse-diff f 0) -1 0.0001)))
            
            (define-test chain-log-sq
              ;; d/dx[ln(x²)] at x=2 → 2/x = 1
              (let ([f (lambda (x) (traced-log (traced-sq x)))])
                   (assert-= (reverse-diff f 2) 1 0.0001)))
            
            (define-test chain-sqrt-add
              ;; d/dx[√(x+1)] at x=3 → 1/(2√4) = 0.25
              (let ([f (lambda (x) (traced-sqrt (traced-add x 1)))])
                   (assert-= (reverse-diff f 3) 0.25 0.0001)))
            
            (define-test chain-sq-add-sq
              ;; d/dx[(x+1)²] at x=2 → 2(x+1) = 6
              (let ([f (lambda (x) (traced-sq (traced-add x 1)))])
                   (assert-= (reverse-diff f 2) 6 0.0001))))

;;; ============================================================
;;; Multi-Variable Gradients
;;; ============================================================

(test-group gradient-tests
            (define-test gradient-sum
              ;; f(x,y) = x + y, ∇f = (1, 1)
              (let* ([f (lambda (x y) (traced-add x y))]
                     [grad (gradient f '(3 4))])
                    (assert-grads-= grad '(1 1) 0.0001)))
            
            (define-test gradient-product
              ;; f(x,y) = x*y, ∇f at (3,4) = (y, x) = (4, 3)
              (let* ([f (lambda (x y) (traced-mul x y))]
                     [grad (gradient f '(3 4))])
                    (assert-grads-= grad '(4 3) 0.0001)))
            
            (define-test gradient-difference
              ;; f(x,y) = x - y, ∇f = (1, -1)
              (let* ([f (lambda (x y) (traced-sub x y))]
                     [grad (gradient f '(3 4))])
                    (assert-grads-= grad '(1 -1) 0.0001)))
            
            (define-test gradient-quotient
              ;; f(x,y) = x/y, ∇f at (6,2) = (1/y, -x/y²) = (0.5, -1.5)
              (let* ([f (lambda (x y) (traced-div x y))]
                     [grad (gradient f '(6 2))])
                    (assert-grads-= grad '(0.5 -1.5) 0.0001)))
            
            (define-test gradient-polynomial
              ;; f(x,y) = x² + 2xy + y², ∇f at (1,2) = (2x+2y, 2x+2y) = (6, 6)
              (let* ([f (lambda (x y)
                                (traced-add
                                 (traced-add (traced-sq x)
                                             (traced-mul (traced-mul x y) 2))
                                 (traced-sq y)))]
                     [grad (gradient f '(1 2))])
                    (assert-grads-= grad '(6 6) 0.0001)))
            
            (define-test gradient-three-vars
              ;; f(x,y,z) = xyz, ∇f at (2,3,4) = (yz, xz, xy) = (12, 8, 6)
              (let* ([f (lambda (x y z) (traced-mul (traced-mul x y) z))]
                     [grad (gradient f '(2 3 4))])
                    (assert-grads-= grad '(12 8 6) 0.0001))))

;;; ============================================================
;;; Value and Gradient Together
;;; ============================================================

(test-group value-and-gradient-tests
            (define-test value-and-gradient-basic
              (let* ([f (lambda (x) (traced-sq x))])
                    (let-values ([(val grad) (value-and-gradient f '(3))])
                                (assert-= val 9 0.0001)
                                (assert-= (car grad) 6 0.0001))))
            
            (define-test value-and-gradient-multivar
              (let* ([f (lambda (x y) (traced-mul x y))])
                    (let-values ([(val grad) (value-and-gradient f '(3 4))])
                                (assert-= val 12 0.0001)
                                (assert-grads-= grad '(4 3) 0.0001)))))

;;; ============================================================
;;; Shared Subexpressions
;;; ============================================================

(test-group shared-subexpr-tests
            (define-test shared-variable-accumulation
              ;; f(x) = x + x = 2x, d/dx = 2
              (let ([f (lambda (x) (traced-add x x))])
                   (assert-= (reverse-diff f 3) 2 0.0001)))
            
            (define-test shared-variable-product
              ;; f(x) = x * x = x², d/dx = 2x
              (let ([f (lambda (x) (traced-mul x x))])
                   (assert-= (reverse-diff f 4) 8 0.0001)))
            
            (define-test shared-intermediate
              ;; f(x) = (x+1) + (x+1) = 2x+2, d/dx = 2
              (let ([f (lambda (x)
                               (let ([y (traced-add x 1)])
                                    (traced-add y y)))])
                   (assert-= (reverse-diff f 3) 2 0.0001)))
            
            (define-test shared-intermediate-product
              ;; f(x) = (x+1) * (x+1) = (x+1)², d/dx = 2(x+1)
              ;; At x=2: d/dx = 6
              (let ([f (lambda (x)
                               (let ([y (traced-add x 1)])
                                    (traced-mul y y)))])
                   (assert-= (reverse-diff f 2) 6 0.0001))))

;;; ============================================================
;;; Gradient Checking (Numerical Verification)
;;; ============================================================

(test-group gradient-check-tests
            (define-test check-polynomial-gradient
              ;; f(x,y) = x² + xy
              (let ([f (lambda (x y)
                               (traced-add (traced-sq x)
                                           (traced-mul x y)))])
                   (assert-true (check-gradient f '(2 3) 0.0001))))
            
            (define-test check-transcendental-gradient
              ;; f(x,y) = sin(x) * cos(y)
              (let ([f (lambda (x y)
                               (traced-mul (traced-sin x)
                                           (traced-cos y)))])
                   (assert-true (check-gradient f '(0.5 0.5) 0.0001))))
            
            (define-test check-complex-gradient
              ;; f(x,y) = ln(x² + y²)
              (let ([f (lambda (x y)
                               (traced-log (traced-add (traced-sq x)
                                                       (traced-sq y))))])
                   (assert-true (check-gradient f '(3 4) 0.0001)))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-case-tests
            (define-test constant-function-gradient
              ;; f(x,y) = 5, ∇f = (0, 0)
              (let* ([f (lambda (x y) 5)]
                     [grad (gradient f '(1 2))])
                    (assert-grads-= grad '(0 0) 0.0001)))
            
            (define-test single-variable-unused
              ;; f(x,y) = x (y unused), ∇f = (1, 0)
              (let* ([f (lambda (x y) x)]
                     [grad (gradient f '(3 4))])
                    (assert-grads-= grad '(1 0) 0.0001)))
            
            (define-test deeply-nested
              ;; f(x) = sin(sin(sin(x))), check it doesn't crash
              (let ([f (lambda (x)
                               (traced-sin (traced-sin (traced-sin x))))])
                   (assert-true (number? (reverse-diff f 1))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
══════════════════════════════════════════════════════════
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All reverse mode autodiff tests passed.
")
    (display "
[FAILURE] Some reverse mode autodiff tests failed.
"))
