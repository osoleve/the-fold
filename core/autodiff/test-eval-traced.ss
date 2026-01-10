;;; fabric/stitches/test-eval-traced.ss — Tests for Traced Evaluation

(load "core/test-framework.ss")
(load "core/lang/eval.ss")

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

(display "
══════════════════════════════════════════════════════════
         TRACED EVALUATION INTEGRATION TESTS
══════════════════════════════════════════════════════════
")

;;; ============================================================
;;; Basic Traced Evaluation
;;; ============================================================

(test-group basic-traced-eval
            (define-test traced-literal
              ;; Literals are constants (not traced)
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '5 empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-equal 5 (cadr result))))

            (define-test traced-quote-number
              ;; Quoted numbers are constants (not traced)
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(quote 42) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-equal 42 (cadr result))))

            (define-test traced-quote-symbol
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(quote foo) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-equal 'foo (cadr result))))

            (define-test traced-variable
              (let* ([tape (make-reverse-tape)]
                     [var (make-traced-var 3.14 tape)]
                     [env (env-extend empty-env 'pi var)]
                     [result (eval-expr-traced 'pi env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-true (traced? (cadr result)))
                    (assert-= (traced-value (cadr result)) 3.14 0.0001))))

;;; ============================================================
;;; Traced Primitives
;;; ============================================================

(test-group traced-primitives
            (define-test traced-add
              ;; Adding constants without traced vars returns constant
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'add 2 3) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    ;; Result is a plain number (no traced inputs)
                    (assert-equal 5 (cadr result))))

            (define-test traced-mul
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'mul 4 5) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 20 0.0001)))

            (define-test traced-sub
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'sub 10 3) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 7 0.0001)))

            (define-test traced-div
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'div 12 4) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 3 0.0001)))

            (define-test traced-sqrt
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'sqrt 16) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 4 0.0001)))

            (define-test traced-sin
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'sin 0) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 0 0.0001)))

            (define-test traced-cos
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'cos 0) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 1 0.0001)))

            (define-test traced-exp
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'exp 0) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 1 0.0001)))

            (define-test traced-log
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'log (prim 'exp 1)) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 1 0.0001))))

;;; ============================================================
;;; Traced Let Bindings
;;; ============================================================

(test-group traced-let
            (define-test traced-let-simple
              (let* ([tape (make-reverse-tape)]
                     [expr '(let ((x 5)) (prim 'add x 3))]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 8 0.0001)))

            (define-test traced-let-nested
              (let* ([tape (make-reverse-tape)]
                     [expr '(let ((x 2))
                             (let ((y 3))
                                  (prim 'mul x y)))]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 6 0.0001)))

            (define-test traced-let-computation
              (let* ([tape (make-reverse-tape)]
                     [expr '(let ((x (prim 'add 1 2)))
                             (prim 'mul x x))]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 9 0.0001))))

;;; ============================================================
;;; Traced Function Calls
;;; ============================================================

(test-group traced-call
            (define-test traced-identity-call
              (let* ([tape (make-reverse-tape)]
                     [expr '(call (fn (x) x) 42)]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 42 0.0001)))

            (define-test traced-square-call
              (let* ([tape (make-reverse-tape)]
                     [expr '(call (fn (x) (prim 'mul x x)) 5)]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 25 0.0001)))

            (define-test traced-add-call
              (let* ([tape (make-reverse-tape)]
                     [expr '(call (fn (x y) (prim 'add x y)) 3 4)]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 7 0.0001))))

;;; ============================================================
;;; Traced If Expressions
;;; ============================================================

(test-group traced-if
            (define-test traced-if-true
              (let* ([tape (make-reverse-tape)]
                     [expr '(if #t (prim 'add 1 2) (prim 'mul 3 4))]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 3 0.0001)))

            (define-test traced-if-false
              (let* ([tape (make-reverse-tape)]
                     [expr '(if #f (prim 'add 1 2) (prim 'mul 3 4))]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 12 0.0001)))

            (define-test traced-if-numeric-condition
              ;; Non-zero numbers are truthy
              (let* ([tape (make-reverse-tape)]
                     [expr '(if (prim 'add 1 1) 10 20)]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 10 0.0001))))

;;; ============================================================
;;; eval-and-grad High-Level API
;;; ============================================================

(test-group eval-and-grad
            (define-test eval-and-grad-identity
              ;; f(x) = x, df/dx = 1
              (let-values ([(val grads) (eval-and-grad 'x empty-env '(x) '(5) 1000)])
                          (assert-= val 5 0.0001)
                          (assert-= (car grads) 1 0.0001)))

            (define-test eval-and-grad-square
              ;; f(x) = x², df/dx = 2x, at x=3 → 6
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'mul x x)
                                         empty-env
                                         '(x)
                                         '(3)
                                         1000)])
                          (assert-= val 9 0.0001)
                          (assert-= (car grads) 6 0.0001)))

            (define-test eval-and-grad-sum
              ;; f(x,y) = x + y, ∇f = (1, 1)
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'add x y)
                                         empty-env
                                         '(x y)
                                         '(3 4)
                                         1000)])
                          (assert-= val 7 0.0001)
                          (assert-= (car grads) 1 0.0001)
                          (assert-= (cadr grads) 1 0.0001)))

            (define-test eval-and-grad-product
              ;; f(x,y) = x*y, ∇f at (3,4) = (y, x) = (4, 3)
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'mul x y)
                                         empty-env
                                         '(x y)
                                         '(3 4)
                                         1000)])
                          (assert-= val 12 0.0001)
                          (assert-= (car grads) 4 0.0001)
                          (assert-= (cadr grads) 3 0.0001)))

            (define-test eval-and-grad-polynomial
              ;; f(x,y) = x² + 2xy + y², ∇f at (1,2) = (2x+2y, 2x+2y) = (6, 6)
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'add
                                           (prim 'add
                                                 (prim 'mul x x)
                                                 (prim 'mul (prim 'mul x y) 2))
                                           (prim 'mul y y))
                                         empty-env
                                         '(x y)
                                         '(1 2)
                                         1000)])
                          (assert-= val 9 0.0001)
                          (assert-= (car grads) 6 0.0001)
                          (assert-= (cadr grads) 6 0.0001)))

            (define-test eval-and-grad-transcendental
              ;; f(x) = sin(x), df/dx = cos(x), at x=0 → 1
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'sin x)
                                         empty-env
                                         '(x)
                                         '(0)
                                         1000)])
                          (assert-= val 0 0.0001)
                          (assert-= (car grads) 1 0.0001)))

            (define-test eval-and-grad-chain-rule
              ;; f(x) = sin(x²), df/dx = cos(x²) * 2x, at x=0 → 0
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'sin (prim 'mul x x))
                                         empty-env
                                         '(x)
                                         '(0)
                                         1000)])
                          (assert-= val 0 0.0001)
                          (assert-= (car grads) 0 0.0001)))

            (define-test eval-and-grad-with-let
              ;; f(x) = let y = x² in y + y = 2x², df/dx = 4x, at x=2 → 8
              (let-values ([(val grads) (eval-and-grad
                                         '(let ((y (prim 'mul x x)))
                                           (prim 'add y y))
                                         empty-env
                                         '(x)
                                         '(2)
                                         1000)])
                          (assert-= val 16 0.0001)
                          (assert-= (car grads) 8 0.0001)))

            (define-test eval-and-grad-with-function
              ;; f(x) = (λy.x*y)(2) = x*2, df/dx = 2
              (let-values ([(val grads) (eval-and-grad
                                         '(call (fn (y) (prim 'mul x y)) 2)
                                         empty-env
                                         '(x)
                                         '(3)
                                         1000)])
                          (assert-= val 6 0.0001)
                          (assert-= (car grads) 2 0.0001))))

;;; ============================================================
;;; Fuel Consumption
;;; ============================================================

(test-group traced-fuel
            (define-test traced-fuel-consumption
              ;; Basic operation consumes more fuel than normal eval
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'add 1 2) empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    ;; Should have consumed: 1 (eval) + 3 (add with tracing)
                    (assert-true (< (caddr result) 997))))

            (define-test traced-fuel-exhaustion
              ;; Should suspend when fuel runs out
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'add 1 2) empty-env 2 tape)])
                    ;; 2 fuel is not enough (needs at least 4: 1 eval + 3 add)
                    (assert-equal 'suspended (car result))))

            (define-test traced-complex-fuel
              ;; (x + y) * (x - y) should consume appropriate fuel
              (let* ([tape (make-reverse-tape)]
                     [env (env-extend (env-extend empty-env 'x (make-traced-var 5 tape))
                                      'y (make-traced-var 3 tape))]
                     [expr '(prim 'mul (prim 'add x y) (prim 'sub x y))]
                     [result (eval-expr-traced expr env 1000 tape)])
                    (assert-equal 'ok (car result))
                    ;; Should have consumed significant fuel (3 ops × ~3 fuel each + overhead)
                    (assert-true (< (caddr result) 990)))))

;;; ============================================================
;;; Mixed Traced/Constant Values
;;; ============================================================

(test-group mixed-values
            (define-test mixed-traced-and-constant
              ;; Adding a traced variable and a constant
              (let* ([tape (make-reverse-tape)]
                     [var (make-traced-var 5 tape)]
                     [env (env-extend empty-env 'x var)]
                     [expr '(prim 'add x 3)]
                     [result (eval-expr-traced expr env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 8 0.0001)))

            (define-test constant-times-traced
              (let* ([tape (make-reverse-tape)]
                     [var (make-traced-var 4 tape)]
                     [env (env-extend empty-env 'x var)]
                     [expr '(prim 'mul 2 x)]
                     [result (eval-expr-traced expr env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-= (traced-value (cadr result)) 8 0.0001))))

;;; ============================================================
;;; Non-Differentiable Primitives
;;; ============================================================

(test-group non-differentiable-prims
            (define-test traced-non-diff-string-append
              ;; String operations extract primals
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'string-append "hello" " world")
                                               empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (assert-equal "hello world" (cadr result))))

            (define-test traced-non-diff-list-ops
              ;; List operations work normally, constants stay constants
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'cons 1 (prim 'cons 2 '()))
                                               empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (let ([lst (cadr result)])
                         (assert-true (pair? lst))
                         (assert-equal 1 (car lst)))))

            (define-test traced-mixed-numeric-and-non-numeric
              ;; Compute then use in non-numeric context (constants stay constants)
              (let* ([tape (make-reverse-tape)]
                     [expr '(let ((n (prim 'add 1 2)))
                             (prim 'cons n '()))]
                     [result (eval-expr-traced expr empty-env 1000 tape)])
                    (assert-equal 'ok (car result))
                    (let ([lst (cadr result)])
                         (assert-true (pair? lst))
                         (assert-equal 3 (car lst))))))

;;; ============================================================
;;; Gradient Correctness (Integration with reverse-diff)
;;; ============================================================

(test-group gradient-correctness
            (define-test gradient-correct-square
              ;; Compare eval-and-grad with reverse-diff
              (let-values ([(val grad) (eval-and-grad
                                        '(prim 'mul x x)
                                        empty-env
                                        '(x)
                                        '(3)
                                        1000)])
                          ;; Should match reverse-diff result
                          (let ([ref-grad (reverse-diff (lambda (x) (traced-sq x)) 3)])
                               (assert-= (car grad) ref-grad 0.0001))))

            (define-test gradient-correct-polynomial
              ;; f(x,y) = x*y + x²
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'add
                                           (prim 'mul x y)
                                           (prim 'mul x x))
                                         empty-env
                                         '(x y)
                                         '(2 3)
                                         1000)])
                          ;; f(2,3) = 2*3 + 2² = 10
                          (assert-= val 10 0.0001)
                          ;; df/dx = y + 2x = 3 + 4 = 7
                          ;; df/dy = x = 2
                          (assert-= (car grads) 7 0.0001)
                          (assert-= (cadr grads) 2 0.0001)))

            (define-test gradient-correct-chain
              ;; f(x) = exp(x²)
              (let-values ([(val grads) (eval-and-grad
                                         '(prim 'exp (prim 'mul x x))
                                         empty-env
                                         '(x)
                                         '(0)
                                         1000)])
                          ;; f(0) = exp(0) = 1
                          (assert-= val 1 0.0001)
                          ;; df/dx = exp(x²) * 2x, at x=0 → 0
                          (assert-= (car grads) 0 0.0001))))

;;; ============================================================
;;; Error Handling
;;; ============================================================

(test-group traced-errors
            (define-test traced-div-by-zero
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced '(prim 'div 1 0) empty-env 1000 tape)])
                    (assert-equal 'error (car result))
                    (assert-equal 'div-by-zero (cadr result))))

            (define-test traced-unbound-variable
              (let* ([tape (make-reverse-tape)]
                     [result (eval-expr-traced 'undefined empty-env 1000 tape)])
                    (assert-equal 'error (car result))
                    (assert-equal 'unbound-variable (cadr result)))))

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
[SUCCESS] All traced evaluation integration tests passed.
")
    (display "
[FAILURE] Some traced evaluation integration tests failed.
"))
