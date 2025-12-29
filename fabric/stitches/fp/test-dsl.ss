;;; fabric/stitches/fp/test-dsl.ss — Tests for DSL Builder

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/dsl.ss")

(display "
")
(display "==============================================================
")
(display "         DSL BUILDER TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Instruction Tests
;;; ============================================================

(test-group instruction
            (define-test make-instruction-test
              (let ([instr (make-instruction 'test 'payload identity)])
                   (assert-equal 'test (instruction-tag instr))
                   (assert-equal 'payload (instruction-payload instr))))
            
            (define-test instruction-cont-test
              (let* ([k (lambda (x) (dsl-pure (* x 2)))]
                     [instr (make-instruction 'double 21 k)])
                    (assert-equal 'double (instruction-tag instr))
                    (assert-equal 21 (instruction-payload instr))
                    ;; continuation should work
                    (let ([result (dsl-pure-value ((instruction-cont instr) 21))])
                         (assert-equal 42 result)))))

;;; ============================================================
;;; DSL Program Tests
;;; ============================================================

(test-group dsl-program
            (define-test dsl-pure-test
              (let ([prog (dsl-pure 42)])
                   (assert-true (dsl-pure? prog))
                   (assert-equal 42 (dsl-pure-value prog))))
            
            (define-test dsl-bind-pure-test
              (let* ([prog1 (dsl-pure 21)]
                     [prog2 (dsl-bind prog1 (lambda (x) (dsl-pure (* x 2))))])
                    (assert-true (dsl-pure? prog2))
                    (assert-equal 42 (dsl-pure-value prog2))))
            
            (define-test dsl-map-test
              (let* ([prog (dsl-pure 21)]
                     [mapped (dsl-map (lambda (x) (* x 2)) prog)])
                    (assert-equal 42 (dsl-pure-value mapped))))
            
            (define-test dsl-emit-creates-suspended-test
              (let ([prog (dsl-emit 'test 'payload)])
                   (assert-true (dsl-suspended? prog))))
            
            (define-test dsl-request-creates-suspended-test
              (let ([prog (dsl-request 'get 100)])
                   (assert-true (dsl-suspended? prog)))))

;;; ============================================================
;;; Interpreter Tests
;;; ============================================================

(test-group interpreter
            (define-test make-interpreter-test
              (let ([interp (make-interpreter (lambda (tag payload) payload))])
                   (assert-true (interpreter? interp))))
            
            (define-test run-dsl-pure-test
              (let ([interp (make-interpreter (lambda (tag payload) payload))]
                    [prog (dsl-pure 42)])
                   (assert-equal 42 (run-dsl interp prog))))
            
            (define-test simple-echo-interpreter-test
              (let* ([interp (make-interpreter (lambda (tag payload) payload))]
                     [prog (dsl-bind (dsl-request 'get 100)
                                     (lambda (x) (dsl-pure (* x 2))))])
                    (assert-equal 200 (run-dsl interp prog)))))

;;; ============================================================
;;; Layered Interpreter Tests
;;; ============================================================

(test-group layered-interpreter
            (define-test layered-single-handler-test
              (let* ([interp (layered-interpreter
                              (list (cons 'double (lambda (x) (* x 2)))))]
                     [prog (dsl-request 'double 21)])
                    (assert-equal 42 (run-dsl interp prog))))
            
            (define-test layered-multiple-handlers-test
              (let* ([interp (layered-interpreter
                              (list (cons 'add (lambda (p) (+ (car p) (cdr p))))
                                    (cons 'mul (lambda (p) (* (car p) (cdr p))))))]
                     [prog (dsl-bind (dsl-request 'add (cons 10 5))
                                     (lambda (sum)
                                             (dsl-request 'mul (cons sum 2))))])
                    (assert-equal 30 (run-dsl interp prog)))))

;;; ============================================================
;;; Expression DSL Tests
;;; ============================================================

(test-group expression-dsl
            (define-test expr-lit-test
              (let ([e (expr-lit 42)])
                   (assert-true (expr-lit? e))
                   (assert-equal 42 (expr-lit-value e))))
            
            (define-test expr-var-test
              (let ([e (expr-var 'x)])
                   (assert-true (expr-var? e))
                   (assert-equal 'x (expr-var-name e))))
            
            (define-test expr-binop-test
              (let ([e (expr-binop '+ (expr-lit 1) (expr-lit 2))])
                   (assert-true (expr-binop? e))
                   (assert-equal '+ (expr-binop-op e))))
            
            (define-test expr-if-test
              (let ([e (expr-if (expr-lit #t) (expr-lit 1) (expr-lit 2))])
                   (assert-true (expr-if? e))))
            
            (define-test expr-let-test
              (let ([e (expr-let 'x (expr-lit 10) (expr-var 'x))])
                   (assert-true (expr-let? e))
                   (assert-equal 'x (expr-let-name e))))
            
            (define-test expr-lambda-test
              (let ([e (expr-lambda '(x y) (expr-binop '+ (expr-var 'x) (expr-var 'y)))])
                   (assert-true (expr-lambda? e))
                   (assert-equal '(x y) (expr-lambda-params e)))))

;;; ============================================================
;;; Expression Evaluation Tests
;;; ============================================================

(test-group eval-expr
            (define-test eval-lit-test
              (assert-equal 42 (eval-expr '() (expr-lit 42))))
            
            (define-test eval-var-test
              (assert-equal 10 (eval-expr '((x . 10)) (expr-var 'x))))
            
            (define-test eval-binop-add-test
              (assert-equal 30 (eval-expr '()
                                          (expr-binop '+ (expr-lit 10) (expr-lit 20)))))
            
            (define-test eval-binop-mul-test
              (assert-equal 50 (eval-expr '()
                                          (expr-binop '* (expr-lit 5) (expr-lit 10)))))
            
            (define-test eval-binop-sub-test
              (assert-equal 5 (eval-expr '()
                                         (expr-binop '- (expr-lit 10) (expr-lit 5)))))
            
            (define-test eval-binop-comparison-test
              (assert-true (eval-expr '() (expr-binop '< (expr-lit 5) (expr-lit 10))))
              (assert-false (eval-expr '() (expr-binop '> (expr-lit 5) (expr-lit 10)))))
            
            (define-test eval-if-true-test
              (assert-equal 1 (eval-expr '()
                                         (expr-if (expr-lit #t)
                                                  (expr-lit 1)
                                                  (expr-lit 2)))))
            
            (define-test eval-if-false-test
              (assert-equal 2 (eval-expr '()
                                         (expr-if (expr-lit #f)
                                                  (expr-lit 1)
                                                  (expr-lit 2)))))
            
            (define-test eval-let-test
              (assert-equal 15 (eval-expr '()
                                          (expr-let 'x (expr-lit 10)
                                                    (expr-binop '+ (expr-var 'x) (expr-lit 5))))))
            
            (define-test eval-nested-let-test
              (assert-equal 30 (eval-expr '()
                                          (expr-let 'x (expr-lit 10)
                                                    (expr-let 'y (expr-lit 20)
                                                              (expr-binop '+ (expr-var 'x) (expr-var 'y)))))))
            
            (define-test eval-lambda-test
              (let ([result (eval-expr '()
                                       (expr-app
                                        (expr-lambda '(x) (expr-binop '* (expr-var 'x) (expr-lit 2)))
                                        (list (expr-lit 21))))])
                   (assert-equal 42 result)))
            
            (define-test eval-lambda-two-args-test
              (let ([result (eval-expr '()
                                       (expr-app
                                        (expr-lambda '(x y) (expr-binop '+ (expr-var 'x) (expr-var 'y)))
                                        (list (expr-lit 10) (expr-lit 32))))])
                   (assert-equal 42 result)))
            
            (define-test eval-unop-not-test
              (assert-true (eval-expr '() (expr-unop 'not (expr-lit #f))))
              (assert-false (eval-expr '() (expr-unop 'not (expr-lit #t)))))
            
            (define-test eval-unop-neg-test
              (assert-equal -5 (eval-expr '() (expr-unop 'neg (expr-lit 5))))))

;;; ============================================================
;;; Statement Tests
;;; ============================================================

(test-group statement
            (define-test stmt-assign-test
              (let ([s (stmt-assign 'x (expr-lit 10))])
                   (assert-true (stmt-assign? s))
                   (assert-equal 'x (stmt-assign-var s))))
            
            (define-test stmt-seq-test
              (let ([s (stmt-seq (list (stmt-assign 'x (expr-lit 1))
                                       (stmt-assign 'y (expr-lit 2))))])
                   (assert-true (stmt-seq? s))))
            
            (define-test stmt-if-test
              (let ([s (stmt-if (expr-lit #t)
                                (stmt-assign 'x (expr-lit 1))
                                (stmt-assign 'x (expr-lit 2)))])
                   (assert-true (stmt-if? s))))
            
            (define-test stmt-while-test
              (let ([s (stmt-while (expr-var 'running)
                                   (stmt-assign 'x (expr-lit 0)))])
                   (assert-true (stmt-while? s))))
            
            (define-test stmt-return-test
              (let ([s (stmt-return (expr-lit 42))])
                   (assert-true (stmt-return? s)))))

;;; ============================================================
;;; Statement Execution Tests
;;; ============================================================

(test-group run-stmt
            (define-test run-assign-test
              (let ([result (run-stmt '() (stmt-assign 'x (expr-lit 10)))])
                   (assert-equal 10 (cdr (assoc 'x (car result))))
                   (assert-true (nothing? (cdr result)))))
            
            (define-test run-seq-test
              (let ([result (run-stmt '()
                                      (stmt-seq (list (stmt-assign 'x (expr-lit 10))
                                                      (stmt-assign 'y (expr-lit 20)))))])
                   (assert-equal 10 (cdr (assoc 'x (car result))))
                   (assert-equal 20 (cdr (assoc 'y (car result))))))
            
            (define-test run-if-true-test
              (let ([result (run-stmt '()
                                      (stmt-if (expr-lit #t)
                                               (stmt-assign 'x (expr-lit 1))
                                               (stmt-assign 'x (expr-lit 2))))])
                   (assert-equal 1 (cdr (assoc 'x (car result))))))
            
            (define-test run-if-false-test
              (let ([result (run-stmt '()
                                      (stmt-if (expr-lit #f)
                                               (stmt-assign 'x (expr-lit 1))
                                               (stmt-assign 'x (expr-lit 2))))])
                   (assert-equal 2 (cdr (assoc 'x (car result))))))
            
            (define-test run-return-test
              (let ([result (run-stmt '() (stmt-return (expr-lit 42)))])
                   (assert-true (just? (cdr result)))
                   (assert-equal 42 (from-just (cdr result)))))
            
            (define-test run-while-simple-test
              ;; Count down from 3 to 0
              (let ([result (run-stmt '((n . 3) (sum . 0))
                                      (stmt-while (expr-binop '> (expr-var 'n) (expr-lit 0))
                                                  (stmt-seq (list
                                                             (stmt-assign 'sum
                                                                          (expr-binop '+ (expr-var 'sum) (expr-var 'n)))
                                                             (stmt-assign 'n
                                                                          (expr-binop '- (expr-var 'n) (expr-lit 1)))))))])
                   (assert-equal 0 (cdr (assoc 'n (car result))))
                   (assert-equal 6 (cdr (assoc 'sum (car result)))))))  ; 3+2+1=6

;;; ============================================================
;;; DSL Combinator Tests
;;; ============================================================

(test-group dsl-combinators
            (define-test dsl-sequence-empty-test
              (let ([result (dsl-pure-value (dsl-sequence '()))])
                   (assert-equal '() result)))
            
            (define-test dsl-sequence-test
              (let* ([progs (list (dsl-pure 1) (dsl-pure 2) (dsl-pure 3))]
                     [result (dsl-pure-value (dsl-sequence progs))])
                    (assert-equal '(1 2 3) result)))
            
            (define-test dsl-fold-test
              (let ([result (dsl-pure-value
                             (dsl-fold (lambda (acc x) (dsl-pure (+ acc x)))
                                       0
                                       '(1 2 3 4 5)))])
                   (assert-equal 15 result)))
            
            (define-test dsl-when-true-test
              (let ([result (dsl-pure-value
                             (dsl-when #t (dsl-pure 'executed)))])
                   (assert-equal 'executed result)))
            
            (define-test dsl-when-false-test
              (let ([result (dsl-pure-value
                             (dsl-when #f (dsl-pure 'not-executed)))])
                   (assert-equal '() result)))
            
            (define-test dsl-replicate-test
              (let ([result (dsl-pure-value
                             (dsl-replicate 3 (dsl-pure 'x)))])
                   (assert-equal '(x x x) result))))

;;; ============================================================
;;; Calculator DSL Tests
;;; ============================================================

(test-group calculator-dsl
            (define-test calc-push-pop-test
              (let ([result (run-calc
                             (dsl-bind (calc-push! 42)
                                       (lambda (_) (calc-pop!))))])
                   (assert-equal 42 result)))
            
            (define-test calc-add-test
              (let ([result (run-calc
                             (dsl-bind (calc-push! 10)
                                       (lambda (_)
                                               (dsl-bind (calc-push! 20)
                                                         (lambda (_)
                                                                 (dsl-bind (calc-add!)
                                                                           (lambda (_)
                                                                                   (calc-pop!))))))))])
                   (assert-equal 30 result)))
            
            (define-test calc-sub-test
              (let ([result (run-calc
                             (dsl-bind (calc-push! 10)
                                       (lambda (_)
                                               (dsl-bind (calc-push! 3)
                                                         (lambda (_)
                                                                 (dsl-bind (calc-sub!)
                                                                           (lambda (_)
                                                                                   (calc-pop!))))))))])
                   (assert-equal 7 result)))  ; 10 - 3 = 7
            
            (define-test calc-mul-test
              (let ([result (run-calc
                             (dsl-bind (calc-push! 6)
                                       (lambda (_)
                                               (dsl-bind (calc-push! 7)
                                                         (lambda (_)
                                                                 (dsl-bind (calc-mul!)
                                                                           (lambda (_)
                                                                                   (calc-pop!))))))))])
                   (assert-equal 42 result)))
            
            (define-test calc-dup-test
              (let ([result (run-calc
                             (dsl-bind (calc-push! 21)
                                       (lambda (_)
                                               (dsl-bind (calc-dup!)
                                                         (lambda (_)
                                                                 (dsl-bind (calc-add!)
                                                                           (lambda (_)
                                                                                   (calc-pop!))))))))])
                   (assert-equal 42 result)))  ; 21 + 21 = 42
            
            (define-test calc-swap-test
              (let ([result (run-calc
                             (dsl-bind (calc-push! 10)
                                       (lambda (_)
                                               (dsl-bind (calc-push! 3)
                                                         (lambda (_)
                                                                 (dsl-bind (calc-swap!)
                                                                           (lambda (_)
                                                                                   (dsl-bind (calc-sub!)
                                                                                             (lambda (_)
                                                                                                     (calc-pop!))))))))))])
                   (assert-equal -7 result)))  ; 3 - 10 = -7 (after swap)
            
            (define-test calc-complex-test
              ;; Compute (5 + 3) * 2 = 16
              (let ([result (run-calc
                             (dsl-bind (calc-push! 5)
                                       (lambda (_)
                                               (dsl-bind (calc-push! 3)
                                                         (lambda (_)
                                                                 (dsl-bind (calc-add!)
                                                                           (lambda (_)
                                                                                   (dsl-bind (calc-push! 2)
                                                                                             (lambda (_)
                                                                                                     (dsl-bind (calc-mul!)
                                                                                                               (lambda (_)
                                                                                                                       (calc-pop!))))))))))))])
                   (assert-equal 16 result))))

;;; ============================================================
;;; Turtle DSL Tests
;;; ============================================================

(test-group turtle-dsl
            (define-test turtle-getpos-initial-test
              (let ([result (run-turtle (getpos!))])
                   (assert-equal 0 (car result))
                   (assert-equal 0 (cdr result))))
            
            (define-test turtle-forward-test
              (let ([result (run-turtle
                             (dsl-bind (forward! 100)
                                       (lambda (_) (getpos!))))])
                   ;; Should move 100 units in direction 0 (east)
                   (assert-true (> (car result) 99))  ; x close to 100
                   (assert-true (< (cdr result) 1))))  ; y close to 0
            
            (define-test turtle-turn-and-forward-test
              (let ([result (run-turtle
                             (dsl-bind (right! 90)
                                       (lambda (_)
                                               (dsl-bind (forward! 100)
                                                         (lambda (_) (getpos!))))))])
                   ;; After turning right 90, forward should move in negative y
                   (assert-true (< (car result) 1))    ; x close to 0
                   (assert-true (< (cdr result) -99)))) ; y close to -100
            
            (define-test turtle-square-returns-to-origin-test
              ;; Draw a square and return close to origin
              (let ([result (run-turtle
                             (dsl-bind (forward! 100)
                                       (lambda (_)
                                               (dsl-bind (right! 90)
                                                         (lambda (_)
                                                                 (dsl-bind (forward! 100)
                                                                           (lambda (_)
                                                                                   (dsl-bind (right! 90)
                                                                                             (lambda (_)
                                                                                                     (dsl-bind (forward! 100)
                                                                                                               (lambda (_)
                                                                                                                       (dsl-bind (right! 90)
                                                                                                                                 (lambda (_)
                                                                                                                                         (dsl-bind (forward! 100)
                                                                                                                                                   (lambda (_)
                                                                                                                                                           (getpos!))))))))))))))))])
                   ;; Should be back near origin
                   (assert-true (< (abs (car result)) 1))
                   (assert-true (< (abs (cdr result)) 1)))))

;;; ============================================================
;;; Practical Examples
;;; ============================================================

(test-group practical-examples
            ;; Factorial using expression DSL
            (define-test expr-factorial-test
              ;; fact(n) = if n <= 1 then 1 else n * fact(n-1)
              ;; We'll compute 5! iteratively using let
              (let ([result (eval-expr '()
                                       (expr-let 'result (expr-lit 1)
                                                 (expr-let 'n (expr-lit 5)
                                                           ;; Can't do recursion easily, compute 5*4*3*2*1 manually
                                                           (expr-binop '*
                                                                       (expr-binop '*
                                                                                   (expr-binop '*
                                                                                               (expr-binop '* (expr-lit 5) (expr-lit 4))
                                                                                               (expr-lit 3))
                                                                                   (expr-lit 2))
                                                                       (expr-lit 1)))))])
                   (assert-equal 120 result)))
            
            ;; Fibonacci-like computation using statement DSL
            (define-test stmt-fib-test
              ;; Compute fib(6) = 8 using iterative approach
              (let ([result (run-stmt '((n . 6) (a . 0) (b . 1) (i . 0))
                                      (stmt-while (expr-binop '< (expr-var 'i) (expr-var 'n))
                                                  (stmt-seq (list
                                                             (stmt-assign 'temp (expr-binop '+ (expr-var 'a) (expr-var 'b)))
                                                             (stmt-assign 'a (expr-var 'b))
                                                             (stmt-assign 'b (expr-var 'temp))
                                                             (stmt-assign 'i (expr-binop '+ (expr-var 'i) (expr-lit 1)))))))])
                   (assert-equal 8 (cdr (assoc 'a (car result)))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All DSL builder tests passed.
")
    (display "
[FAILURE] Some DSL builder tests failed.
"))
