;;; fabric/stitches/fp/test-dsl.ss — Tests for DSL Builder

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "lattice/fp/meta/dsl.ss")

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
;;; New Feature Tests
;;; ============================================================

(define-instruction (move-x! n) 'move-x)

(test-group dsl-enhancements
            (define-test define-instruction-test
              (let* ([prog (move-x! 10)]
                     [instr (dsl-instruction prog)])
                    (assert-equal 'move-x (instruction-tag instr))
                    (assert-equal '(10) (instruction-payload instr))))
            
            (define-test run-dsl-state-test
              (let* ([handler (lambda (tag payload state)
                                      (case tag
                                            [(add) (cons '() (+ state payload))]
                                            [(get) (cons state state)]))]
                     [prog (dsl-bind (dsl-emit 'add 10)
                                     (lambda (_) (dsl-request 'get '())))]
                     [result (run-dsl-state handler 0 prog)])
                    (assert-equal 10 (car result))
                    (assert-equal 10 (cdr result))))
            
            (define-test composed-interpreter-test
              (let* ([int1 (layered-interpreter (list (cons 'a (lambda (p) 'from-1))))]
                     [int2 (layered-interpreter (list (cons 'b (lambda (p) 'from-2))))]
                     [comp (composed-interpreter int1 int2)])
                    (assert-equal 'from-1 (run-dsl comp (dsl-request 'a '())))
                    (assert-equal 'from-2 (run-dsl comp (dsl-request 'b '()))))))

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
;;; Do-Notation Tests
;;; ============================================================

(test-group dsl-do-notation
            (define-test dsl-do-basic-test
              ;; Simple binding test
              (let ([result (dsl-pure-value
                             (dsl-do
                              (x <- (dsl-pure 5))
                              (dsl-pure (* x 2))))])
                   (assert-equal 10 result)))
            
            (define-test dsl-do-multiple-bindings-test
              ;; Multiple bindings
              (let ([result (dsl-pure-value
                             (dsl-do
                              (x <- (dsl-pure 10))
                              (y <- (dsl-pure 20))
                              (z <- (dsl-pure 12))
                              (dsl-pure (+ x y z))))])
                   (assert-equal 42 result)))
            
            (define-test dsl-do-discard-test
              ;; Discarding intermediate results
              (let ([result (dsl-pure-value
                             (dsl-do
                              (dsl-pure 'ignored)
                              (x <- (dsl-pure 42))
                              (dsl-pure x)))])
                   (assert-equal 42 result)))
            
            (define-test dsl-do-calc-test
              ;; Using dsl-do with calculator DSL
              (let ([result (run-calc
                             (dsl-do
                              (_ <- (calc-push! 10))
                              (_ <- (calc-push! 20))
                              (_ <- (calc-add!))
                              (calc-pop!)))])
                   (assert-equal 30 result)))
            
            (define-test dsl-do-complex-calc-test
              ;; (5 + 3) * 2 using dsl-do
              (let ([result (run-calc
                             (dsl-do
                              (_ <- (calc-push! 5))
                              (_ <- (calc-push! 3))
                              (_ <- (calc-add!))
                              (_ <- (calc-push! 2))
                              (_ <- (calc-mul!))
                              (calc-pop!)))])
                   (assert-equal 16 result)))
            
            (define-test dsl-do-nested-test
              ;; Nested dsl-do
              (let ([result (dsl-pure-value
                             (dsl-do
                              (x <- (dsl-do
                                     (a <- (dsl-pure 5))
                                     (dsl-pure (* a 2))))
                              (dsl-pure (+ x 10))))])
                   (assert-equal 20 result))))

;;; ============================================================
;;; Error Handling Tests
;;; ============================================================

(test-group error-handling
            (define-test dsl-ok-test
              (let ([r (dsl-ok 42)])
                   (assert-true (dsl-ok? r))
                   (assert-false (dsl-err? r))
                   (assert-equal 42 (dsl-ok-value r))))
            
            (define-test dsl-err-test
              (let ([r (dsl-err 'test-error "Something went wrong")])
                   (assert-false (dsl-ok? r))
                   (assert-true (dsl-err? r))
                   (assert-equal 'test-error (dsl-err-tag r))
                   (assert-equal "Something went wrong" (dsl-err-message r))))
            
            (define-test dsl-result-map-ok-test
              (let ([r (dsl-result-map (lambda (x) (* x 2)) (dsl-ok 21))])
                   (assert-true (dsl-ok? r))
                   (assert-equal 42 (dsl-ok-value r))))
            
            (define-test dsl-result-map-err-test
              (let ([r (dsl-result-map (lambda (x) (* x 2)) (dsl-err 'e "msg"))])
                   (assert-true (dsl-err? r))
                   (assert-equal 'e (dsl-err-tag r))))
            
            (define-test run-dsl-safe-success-test
              (let ([result (run-dsl-safe (make-calc-interpreter)
                                          (dsl-do
                                           (_ <- (calc-push! 5))
                                           (_ <- (calc-push! 3))
                                           (_ <- (calc-add!))
                                           (calc-pop!)))])
                   (assert-true (dsl-ok? result))
                   (assert-equal 8 (dsl-ok-value result))))
            
            (define-test dsl-assert-true-test
              (let ([result (dsl-pure-value
                             (dsl-do
                              (_ <- (dsl-assert #t 'test "Should not fail"))
                              (dsl-pure 'passed)))])
                   (assert-equal 'passed result)))
            
            (define-test dsl-fail-test
              ;; dsl-fail creates a suspended program
              (let ([prog (dsl-fail 'test-error "Test failure")])
                   (assert-true (dsl-suspended? prog)))))

;;; ============================================================
;;; Debugging Tools Tests
;;; ============================================================

(test-group debugging-tools
            (define-test dsl-peek-pure-test
              (let ([peeked (dsl-peek (dsl-pure 42))])
                   (assert-equal 'pure (car peeked))
                   (assert-equal 42 (cdr peeked))))
            
            (define-test dsl-peek-instruction-test
              (let ([peeked (dsl-peek (calc-push! 10))])
                   (assert-equal 'calc-push (car peeked))
                   (assert-equal 10 (cdr peeked))))
            
            (define-test dsl-count-instructions-test
              (let* ([prog (dsl-do
                            (_ <- (calc-push! 1))
                            (_ <- (calc-push! 2))
                            (_ <- (calc-add!))
                            (calc-pop!))]
                     [count (dsl-count-instructions prog 100)])
                    (assert-equal 4 count)))
            
            (define-test dsl-count-instructions-fuel-test
              ;; Should stop at fuel limit
              (let* ([prog (dsl-do
                            (_ <- (calc-push! 1))
                            (_ <- (calc-push! 2))
                            (_ <- (calc-push! 3))
                            (_ <- (calc-push! 4))
                            (calc-pop!))]
                     [count (dsl-count-instructions prog 2)])
                    (assert-equal 2 count)))
            
            (define-test dsl-pretty-print-pure-test
              (let ([str (dsl-pretty-print (dsl-pure 42))])
                   (assert-true (string? str))))
            
            (define-test dsl-pretty-print-instr-test
              (let ([str (dsl-pretty-print (calc-push! 10))])
                   (assert-true (string? str))))
            
            (define-test dsl-collect-tags-test
              (let ([tags (dsl-collect-tags
                           (dsl-do
                            (_ <- (calc-push! 1))
                            (_ <- (calc-add!))
                            (calc-pop!))
                           100)])
                   (assert-equal 3 (length tags))
                   (assert-equal 'calc-push (car tags))
                   (assert-equal 'calc-add (cadr tags))
                   (assert-equal 'calc-pop (caddr tags)))))

;;; ============================================================
;;; DSL Combination Tests
;;; ============================================================

(test-group dsl-combination
            (define-test dsl-inject-left-test
              (let* ([prog (calc-push! 10)]
                     [lifted (dsl-inject-left prog)]
                     [peeked (dsl-peek lifted)])
                    (assert-true (dsl-left-tag? (car peeked)))))
            
            (define-test dsl-inject-right-test
              (let* ([prog (forward! 100)]
                     [lifted (dsl-inject-right prog)]
                     [peeked (dsl-peek lifted)])
                    (assert-true (dsl-right-tag? (car peeked)))))
            
            (define-test dsl-inject-pure-test
              ;; Pure values should not be wrapped
              (let* ([prog (dsl-pure 42)]
                     [lifted (dsl-inject-left prog)])
                    (assert-true (dsl-pure? lifted))
                    (assert-equal 42 (dsl-pure-value lifted))))
            
            (define-test union-interpreter-test
              (let* ([calc-interp (make-calc-interpreter)]
                     [combined (union-interpreter calc-interp calc-interp)]
                     [prog (dsl-do
                            (_ <- (dsl-inject-left (calc-push! 5)))
                            (_ <- (dsl-inject-left (calc-push! 3)))
                            (_ <- (dsl-inject-left (calc-add!)))
                            (dsl-inject-left (calc-pop!)))]
                     [result (run-dsl combined prog)])
                    (assert-equal 8 result))))

;;; ============================================================
;;; Validation Tests
;;; ============================================================

(test-group validation
            (define-test dsl-schema-test
              (assert-true (dsl-schema? calc-schema))
              (assert-true (pair? (dsl-schema-instructions calc-schema))))
            
            (define-test dsl-validate-valid-test
              (let* ([prog (dsl-do
                            (_ <- (calc-push! 5))
                            (_ <- (calc-add!))
                            (calc-pop!))]
                     [result (dsl-validate calc-schema prog 100)])
                    (assert-true (dsl-ok? result))))
            
            (define-test dsl-validate-pure-test
              (let ([result (dsl-validate calc-schema (dsl-pure 42) 100)])
                   (assert-true (dsl-ok? result))))
            
            (define-test dsl-validate-invalid-test
              (let* ([prog (forward! 100)]  ; turtle instruction
                     [result (dsl-validate calc-schema prog 100)])
                    (assert-true (dsl-err? result))
                    (assert-equal 'invalid-instruction (dsl-err-tag result))))
            
            (define-test turtle-schema-test
              (let* ([prog (dsl-do
                            (_ <- (forward! 100))
                            (_ <- (right! 90))
                            (getpos!))]
                     [result (dsl-validate turtle-schema prog 100)])
                    (assert-true (dsl-ok? result)))))

;;; ============================================================
;;; Resource Management Tests
;;; ============================================================

(test-group resource-management
            (define-test dsl-finally-test
              ;; Verify finally runs after main program
              (let* ([cleanup-ran #f]
                     [interp (make-interpreter
                              (lambda (tag payload)
                                      (case tag
                                            [(set-flag) (set! cleanup-ran #t) '()]
                                            [(get-value) 42]
                                            [else '()])))]
                     [prog (dsl-finally
                            (dsl-emit 'set-flag '())
                            (dsl-request 'get-value '()))]
                     [result (run-dsl interp prog)])
                    (assert-equal 42 result)
                    (assert-true cleanup-ran)))
            
            (define-test dsl-bracket-test
              ;; Verify bracket acquires, uses, then releases
              (let* ([log '()]
                     [interp (make-interpreter
                              (lambda (tag payload)
                                      (case tag
                                            [(acquire)
                                             (set! log (cons 'acquired log))
                                             'resource]
                                            [(use)
                                             (set! log (cons 'used log))
                                             'result]
                                            [(release)
                                             (set! log (cons 'released log))
                                             '()]
                                            [else '()])))]
                     [prog (dsl-bracket
                            (dsl-request 'acquire '())
                            (lambda (r) (dsl-emit 'release r))
                            (lambda (r) (dsl-request 'use r)))]
                     [result (run-dsl interp prog)])
                    (assert-equal 'result result)
                    ;; Log should be in reverse order: acquired, used, released
                    (assert-equal 'released (car log))
                    (assert-equal 'used (cadr log))
                    (assert-equal 'acquired (caddr log)))))

;;; ============================================================
;;; Effects Integration Tests
;;; ============================================================

(test-group effects-integration
            (define-test dsl-to-eff-pure-test
              (let* ([prog (dsl-pure 42)]
                     [eff (dsl-to-eff prog)])
                    ;; Should produce an eff-pure
                    (assert-true (eff-pure? eff))
                    (assert-equal 42 (eff-pure-value eff))))
            
            (define-test dsl-to-eff-instruction-test
              (let* ([prog (calc-push! 10)]
                     [eff (dsl-to-eff prog)])
                    ;; Should produce an eff-op
                    (assert-true (eff-op? eff)))))

;;; ============================================================
;;; Applicative Combinator Tests
;;; ============================================================

(test-group applicative-combinators
            (define-test dsl-ap-basic-test
              ;; Apply a function to a value
              (let* ([mf (dsl-pure (lambda (x) (* x 2)))]
                     [ma (dsl-pure 21)]
                     [result (dsl-pure-value (dsl-ap mf ma))])
                    (assert-equal 42 result)))
            
            (define-test dsl-ap2-test
              ;; Lift binary function
              (let ([result (dsl-pure-value
                             (dsl-ap2 + (dsl-pure 10) (dsl-pure 32)))])
                   (assert-equal 42 result)))
            
            (define-test dsl-ap3-test
              ;; Lift ternary function
              (let ([result (dsl-pure-value
                             (dsl-ap3 (lambda (a b c) (+ a b c))
                                      (dsl-pure 10)
                                      (dsl-pure 20)
                                      (dsl-pure 12)))])
                   (assert-equal 42 result)))
            
            (define-test dsl-join-test
              ;; Flatten nested DSL
              (let* ([nested (dsl-pure (dsl-pure 42))]
                     [result (dsl-pure-value (dsl-join nested))])
                    (assert-equal 42 result)))
            
            (define-test dsl-kleisli-test
              ;; Kleisli composition
              (let* ([f (lambda (x) (dsl-pure (* x 2)))]
                     [g (lambda (x) (dsl-pure (+ x 1)))]
                     [h (dsl-kleisli f g)]  ; f >=> g = \x -> f(x) >>= g
                     [result (dsl-pure-value (h 20))])  ; (* 20 2) then (+ 40 1) = 41
                    (assert-equal 41 result)))
            
            (define-test dsl-compose-test
              ;; Reversed Kleisli (<=<)
              (let* ([f (lambda (x) (dsl-pure (* x 2)))]
                     [g (lambda (x) (dsl-pure (+ x 1)))]
                     [h (dsl-compose g f)]  ; g <=< f = f >=> g
                     [result (dsl-pure-value (h 20))])
                    (assert-equal 41 result))))

;;; ============================================================
;;; Tagless Final Tests
;;; ============================================================

(test-group tagless-final
            (define-test make-tagless-dsl-test
              (let ([dsl (make-tagless-dsl '())])
                   (assert-true (tagless-dsl? dsl))))
            
            (define-test instantiate-tagless-test
              ;; Create a simple arithmetic DSL
              (let* ([arith-dsl (make-tagless-dsl
                                 (list (cons 'lit (lambda (i) identity))
                                       (cons 'add (lambda (i) +))
                                       (cons 'mul (lambda (i) *))))]
                     [arith-eval (instantiate-tagless arith-dsl 'eval)]
                     [lit (tagless-op arith-eval 'lit)]
                     [add (tagless-op arith-eval 'add)])
                    ;; (+ 5 3) = 8
                    (assert-equal 8 (add (lit 5) (lit 3)))))
            
            (define-test tagless-complex-expression-test
              ;; (5 + 3) * 2 = 16
              (let* ([arith-dsl (make-tagless-dsl
                                 (list (cons 'lit (lambda (i) identity))
                                       (cons 'add (lambda (i) +))
                                       (cons 'mul (lambda (i) *))))]
                     [arith-eval (instantiate-tagless arith-dsl 'eval)]
                     [lit (tagless-op arith-eval 'lit)]
                     [add (tagless-op arith-eval 'add)]
                     [mul (tagless-op arith-eval 'mul)])
                    (assert-equal 16 (mul (add (lit 5) (lit 3)) (lit 2))))))

;;; ============================================================
;;; Source Location Tests
;;; ============================================================

(test-group source-locations
            (define-test make-source-pos-test
              (let ([pos (make-source-pos 10 5 100)])
                   (assert-true (source-pos? pos))
                   (assert-equal 10 (source-pos-line pos))
                   (assert-equal 5 (source-pos-col pos))
                   (assert-equal 100 (source-pos-offset pos))))
            
            (define-test no-source-pos-test
              (assert-true (source-pos? no-source-pos))
              (assert-equal 0 (source-pos-line no-source-pos)))
            
            (define-test make-located-test
              (let* ([pos (make-source-pos 1 1 0)]
                     [loc (make-located 'value pos)])
                    (assert-true (located? loc))
                    (assert-equal 'value (located-value loc))
                    (assert-equal pos (located-pos loc))))
            
            (define-test format-source-pos-test
              (let ([pos (make-source-pos 42 10 500)])
                   (assert-equal "42:10" (format-source-pos pos))))
            
            (define-test dsl-emit-at-test
              (let* ([pos (make-source-pos 5 3 20)]
                     [prog (dsl-emit-at 'test 'payload pos)])
                    (assert-true (dsl-suspended? prog))
                    (let* ([instr (dsl-instruction prog)]
                           [instr-pos (instruction-pos instr)])
                          (assert-equal 5 (source-pos-line instr-pos))))))

;;; ============================================================
;;; Middleware Tests
;;; ============================================================

(test-group middleware
            (define-test middleware-identity-test
              (let* ([base-interp (make-interpreter (lambda (tag payload) payload))]
                     [wrapped (middleware-identity base-interp)]
                     [prog (dsl-request 'echo 42)])
                    (assert-equal 42 (run-dsl wrapped prog))))
            
            (define-test middleware-compose-test
              (let* ([log1 '()]
                     [log2 '()]
                     [m1 (lambda (interp)
                                 (make-interpreter
                                  (lambda (tag payload)
                                          (set! log1 (cons 'm1 log1))
                                          ((interpreter-handler interp) tag payload))))]
                     [m2 (lambda (interp)
                                 (make-interpreter
                                  (lambda (tag payload)
                                          (set! log2 (cons 'm2 log2))
                                          ((interpreter-handler interp) tag payload))))]
                     [composed (middleware-compose m1 m2)]
                     [base-interp (make-interpreter (lambda (tag payload) payload))]
                     [wrapped (composed base-interp)]
                     [prog (dsl-request 'test 'value)])
                    (run-dsl wrapped prog)
                    ;; Both middlewares should have been called
                    (assert-equal '(m1) log1)
                    (assert-equal '(m2) log2)))
            
            (define-test middleware-stack-test
              (let* ([call-order '()]
                     [make-logging-mw (lambda (name)
                                              (lambda (interp)
                                                      (make-interpreter
                                                       (lambda (tag payload)
                                                               (set! call-order (cons name call-order))
                                                               ((interpreter-handler interp) tag payload)))))]
                     [stack (middleware-stack (list (make-logging-mw 'a)
                                                    (make-logging-mw 'b)
                                                    (make-logging-mw 'c)))]
                     [base-interp (make-interpreter (lambda (tag payload) payload))]
                     [wrapped (stack base-interp)]
                     [prog (dsl-request 'test 'value)])
                    (run-dsl wrapped prog)
                    ;; Should be called in order a -> b -> c
                    (assert-equal '(c b a) call-order)))  ; reversed due to cons
            
            (define-test middleware-logging-test
              (let* ([log '()]
                     [log-fn (lambda (tag payload result)
                                     (set! log (cons (list tag payload result) log)))]
                     [mw (middleware-logging log-fn)]
                     [base-interp (make-interpreter (lambda (tag payload) (* payload 2)))]
                     [wrapped (mw base-interp)]
                     [prog (dsl-request 'double 21)])
                    (run-dsl wrapped prog)
                    (assert-equal 1 (length log))
                    (let ([entry (car log)])
                         (assert-equal 'double (car entry))
                         (assert-equal 21 (cadr entry))
                         (assert-equal 42 (caddr entry)))))
            
            (define-test middleware-guard-test
              (let* ([allowed-tags '(safe)]
                     [guard-pred (lambda (tag payload) (memq tag allowed-tags))]
                     [fallback (lambda (tag payload) 'blocked)]
                     [mw (middleware-guard guard-pred fallback)]
                     [base-interp (make-interpreter (lambda (tag payload) 'allowed))]
                     [wrapped (mw base-interp)])
                    ;; Safe tag should pass through
                    (assert-equal 'allowed (run-dsl wrapped (dsl-request 'safe '())))
                    ;; Unsafe tag should be blocked
                    (assert-equal 'blocked (run-dsl wrapped (dsl-request 'unsafe '())))))
            
            (define-test middleware-transform-test
              (let* ([xform (lambda (tag payload)
                                    ;; Uppercase the tag symbol name (simulate)
                                    (cons tag (* payload 10)))]  ; transform payload
                     [mw (middleware-transform xform)]
                     [base-interp (make-interpreter (lambda (tag payload) payload))]
                     [wrapped (mw base-interp)])
                    (assert-equal 50 (run-dsl wrapped (dsl-request 'test 5))))))

;;; ============================================================
;;; Introspection Tests
;;; ============================================================

(test-group introspection
            (define-test dsl-instruction-list-test
              (let* ([prog (dsl-do
                            (_ <- (calc-push! 5))
                            (_ <- (calc-push! 3))
                            (_ <- (calc-add!))
                            (calc-pop!))]
                     [instrs (dsl-instruction-list prog 100)])
                    (assert-equal 4 (length instrs))
                    (assert-equal 'calc-push (caar instrs))
                    (assert-equal 'calc-pop (car (list-ref instrs 3)))))
            
            (define-test dsl-program-depth-test
              (let* ([prog (dsl-do
                            (_ <- (calc-push! 1))
                            (_ <- (calc-push! 2))
                            (_ <- (calc-add!))
                            (calc-pop!))]
                     [depth (dsl-program-depth prog 100)])
                    (assert-equal 4 depth)))
            
            (define-test dsl-uses-tag-test
              (let ([prog (dsl-do
                           (_ <- (calc-push! 1))
                           (_ <- (calc-add!))
                           (calc-pop!))])
                   (assert-true (dsl-uses-tag? 'calc-push prog 100))
                   (assert-true (dsl-uses-tag? 'calc-add prog 100))
                   (assert-false (dsl-uses-tag? 'calc-mul prog 100))))
            
            (define-test dsl-tag-histogram-test
              (let* ([prog (dsl-do
                            (_ <- (calc-push! 1))
                            (_ <- (calc-push! 2))
                            (_ <- (calc-push! 3))
                            (_ <- (calc-add!))
                            (_ <- (calc-add!))
                            (calc-pop!))]
                     [hist (dsl-tag-histogram prog 100)]
                     [push-count (cdr (assoc 'calc-push hist))]
                     [add-count (cdr (assoc 'calc-add hist))]
                     [pop-count (cdr (assoc 'calc-pop hist))])
                    (assert-equal 3 push-count)
                    (assert-equal 2 add-count)
                    (assert-equal 1 pop-count))))

;;; ============================================================
;;; Trampoline Tests
;;; ============================================================

(test-group trampolines
            (define-test make-done-test
              (let ([t (make-done 42)])
                   (assert-true (done? t))
                   (assert-false (bounce? t))
                   (assert-equal 42 (done-value t))))
            
            (define-test make-bounce-test
              (let ([t (make-bounce (lambda () (make-done 42)))])
                   (assert-false (done? t))
                   (assert-true (bounce? t))))
            
            (define-test run-trampoline-done-test
              (let ([result (run-trampoline (make-done 42))])
                   (assert-equal 42 result)))
            
            (define-test run-trampoline-bounce-test
              (let* ([t (make-bounce (lambda () (make-done 42)))]
                     [result (run-trampoline t)])
                    (assert-equal 42 result)))
            
            (define-test run-trampoline-chain-test
              ;; Chain of bounces
              (let* ([t (make-bounce
                         (lambda ()
                                 (make-bounce
                                  (lambda ()
                                          (make-bounce
                                           (lambda ()
                                                   (make-done 42)))))))]
                     [result (run-trampoline t)])
                    (assert-equal 42 result)))
            
            (define-test run-dsl-trampolined-test
              (let* ([interp (make-interpreter (lambda (tag payload) payload))]
                     [prog (dsl-do
                            (x <- (dsl-request 'get 10))
                            (y <- (dsl-request 'get 20))
                            (dsl-pure (+ x y)))]
                     [result (run-dsl-trampolined interp prog)])
                    (assert-equal 30 result))))

;;; ============================================================
;;; Testing Utilities Tests
;;; ============================================================

(test-group testing-utilities
            (define-test make-mock-interpreter-test
              (let* ([mock (make-mock-interpreter
                            (list (cons 'add (cons 'fn (lambda (args) (apply + args))))
                                  (cons 'get 42)))]
                     [result1 (run-dsl mock (dsl-request 'get '()))]
                     [result2 (run-dsl mock (dsl-request 'add '(1 2 3)))])
                    (assert-equal 42 result1)
                    (assert-equal 6 result2)))
            
            (define-test dsl-programs-equiv-test
              (let ([interp (make-interpreter (lambda (tag payload) payload))])
                   ;; Same semantics, different structure
                   (assert-true (dsl-programs-equiv?
                                 interp
                                 (dsl-pure 42)
                                 (dsl-bind (dsl-pure 21)
                                           (lambda (x) (dsl-pure (* x 2))))))))
            
            (define-test make-recording-interpreter-test
              (let* ([base (make-interpreter (lambda (tag payload) payload))]
                     [recording (make-recording-interpreter base)]
                     [wrapped (car recording)]
                     [get-log (cdr recording)]
                     [prog (dsl-do
                            (a <- (dsl-request 'x 10))
                            (b <- (dsl-request 'y 20))
                            (dsl-pure (+ a b)))])
                    (run-dsl wrapped prog)
                    (let ([log (get-log)])
                         (assert-equal 2 (length log))
                         (assert-equal 'x (caar log))
                         (assert-equal 'y (caadr log)))))
            
            (define-test verify-instruction-sequence-test
              (let* ([base (make-interpreter (lambda (tag payload) '()))]
                     [prog (dsl-do
                            (_ <- (dsl-emit 'a '()))
                            (_ <- (dsl-emit 'b '()))
                            (dsl-emit 'c '()))])
                    (assert-true (verify-instruction-sequence
                                  prog base '(a b c)))
                    (assert-false (verify-instruction-sequence
                                   prog base '(a c b))))))

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
