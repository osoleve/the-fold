;;; Test harness for core/typed-eval.ss — Typed Evaluation

(load "block.ss")
(load "prim.ss")
(load "eval.ss")
(load "types.ss")
(load "kinds.ss")
(load "infer.ss")
(load "annotate.ss")
(load "typed-eval.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
        (display "✗\n    expected: ")
        (display expected)
        (display "\n    got: ")
        (display actual)))
  (newline))

(define (test-typed name expected-type expected-value result)
  (display "  ")
  (display name)
  (display ": ")
  (cond
    [(not (eq? (car result) 'ok))
     (begin
       (display "✗ not ok: ")
       (display result))]
    [(not (typed? (cadr result)))
     (begin
       (display "✗ not typed: ")
       (display (cadr result)))]
    [else
     (let ([tv (cadr result)])
       (if (and (type=? expected-type (typed-type tv))
                (equal? expected-value (typed-value tv)))
           (display "✓")
           (begin
             (display "✗\n    expected: ")
             (display expected-value)
             (display " : ")
             (display (type->string expected-type))
             (display "\n    got: ")
             (display (typed-value tv))
             (display " : ")
             (display (type->string (typed-type tv))))))])
  (newline))

(define (test-typed-value name expected-value result)
  (display "  ")
  (display name)
  (display ": ")
  (cond
    [(not (eq? (car result) 'ok))
     (begin
       (display "✗ not ok: ")
       (display result))]
    [else
     (let ([val (if (typed? (cadr result))
                    (typed-value (cadr result))
                    (cadr result))])
       (if (equal? expected-value val)
           (display "✓")
           (begin
             (display "✗\n    expected: ")
             (display expected-value)
             (display "\n    got: ")
             (display val))))])
  (newline))

(define (test-error name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result) (eq? (car result) 'error))
      (display "✓")
      (begin
        (display "✗ expected error, got: ")
        (display result)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Type-Check Then Evaluate
;;; ============================================================
(test-section "typecheck-eval")

(test-typed "integer" 'Int 42
  (typecheck-eval '42 100))

(test-typed "boolean" 'Bool #t
  (typecheck-eval '#t 100))

(test-typed "string" 'String "hello"
  (typecheck-eval '"hello" 100))

(test-typed "add" 'Int 5
  (typecheck-eval '(prim 'add 2 3) 100))

(test-typed "neg" 'Int -7
  (typecheck-eval '(prim 'neg 7) 100))

(test-typed "comparison" 'Bool #t
  (typecheck-eval '(prim 'lt? 1 2) 100))

;;; ============================================================
;;; Type Errors Caught Before Evaluation
;;; ============================================================
(test-section "Type Errors (caught before eval)")

(test-error "unbound variable"
  (typecheck-eval 'undefined 100))

;; These should fail type checking, not evaluation
(test-error "applying non-function"
  (typecheck-eval '(42 1 2) 100))

;;; ============================================================
;;; Lambda and Application
;;; ============================================================
(test-section "Lambda and Application")

(test-typed-value "apply identity" 42
  (typecheck-eval '((fn (x) x) 42) 100))

(test-typed-value "apply add-one" 6
  (typecheck-eval '((fn (x) (prim 'add x 1)) 5) 100))

(test-typed-value "curried add" 7
  (typecheck-eval '(((fn (x) (fn (y) (prim 'add x y))) 3) 4) 200))

;;; ============================================================
;;; Let Bindings
;;; ============================================================
(test-section "Let Bindings")

(test-typed-value "simple let" 42
  (typecheck-eval '(let ((x 42)) x) 100))

(test-typed-value "let with computation" 10
  (typecheck-eval '(let ((x 3) (y 7)) (prim 'add x y)) 100))

(test-typed-value "nested let" 6
  (typecheck-eval '(let ((x 1))
                     (let ((y 2))
                       (let ((z 3))
                         (prim 'add x (prim 'add y z)))))
                  100))

;;; ============================================================
;;; If Conditional
;;; ============================================================
(test-section "If Conditional")

(test-typed-value "if true" 1
  (typecheck-eval '(if #t 1 2) 100))

(test-typed-value "if false" 2
  (typecheck-eval '(if #f 1 2) 100))

(test-typed-value "if computed" 'yes
  (typecheck-eval '(if (prim 'lt? 1 2) 'yes 'no) 100))

;;; ============================================================
;;; Recursion (Fix)
;;; ============================================================
(test-section "Recursion (Fix)")

(test-typed-value "factorial 5" 120
  (typecheck-eval
    '(let ((fact (fix fact (fn (n)
                             (if (prim 'zero? n)
                                 1
                                 (prim 'mul n (fact (prim 'sub n 1))))))))
       (fact 5))
    500))

(test-typed-value "fibonacci 6" 8
  (typecheck-eval
    '(let ((fib (fix fib (fn (n)
                           (if (prim 'lt? n 2)
                               n
                               (prim 'add (fib (prim 'sub n 1))
                                          (fib (prim 'sub n 2))))))))
       (fib 6))
    1000))


;;; ============================================================
;;; Case Expressions
;;; ============================================================
(test-section "Case Expressions")

;; Test case with blocks
(test-typed-value "case on True block" 1
  (typecheck-eval
    '(let ((b (prim 'make-block 'True (prim 'string->utf8 "") (prim 'list->vec (quote ())))))
       (case b
         ((True) 1)
         ((False) 0)))
    100))

(test-typed-value "case on False block" 0
  (typecheck-eval
    '(let ((b (prim 'make-block 'False (prim 'string->utf8 "") (prim 'list->vec (quote ())))))
       (case b
         ((True) 1)
         ((False) 0)))
    100))

;; Case with different tag (Pair)
(test-typed-value "case with binding var" 1
  (typecheck-eval
    '(let ((b (prim 'make-block 'Pair (prim 'string->utf8 "") (prim 'list->vec (quote ())))))
       (case b
         ((True) 0) ((Pair) 1)))
    100))

;;; ============================================================
;;; Typed Run (Full Pipeline)
;;; ============================================================
(test-section "typed-run (Full Pipeline)")

(test-typed "typed-run int" 'Int 42
  (typed-run '42 100))

(test-typed "typed-run bool" 'Bool #f
  (typed-run '#f 100))

(test-typed-value "typed-run computation" 25
  (typed-run '(prim 'mul 5 5) 100))

;;; ============================================================
;;; Typed Values
;;; ============================================================
(test-section "Typed Values")

(let ([tv (typed 'Int 42)])
  (test "typed?" #t (typed? tv))
  (test "typed-type" 'Int (typed-type tv))
  (test "typed-value" 42 (typed-value tv)))

;;; ============================================================
;;; Display Functions
;;; ============================================================
(test-section "Display (visual inspection)")

(display "\n--- show-typed ---\n")
(display (show-typed (typed 'Int 42)))
(newline)
(display (show-typed (typed '(-> Int Int) '<closure>)))
(newline)

(display "\n--- typed-repl-eval ---\n")
(typed-repl-eval '42)
(typed-repl-eval '(prim 'add 10 20))
(typed-repl-eval '(fn (x) (prim 'mul x 2)))
(typed-repl-eval '((fn (x) x) "hello"))

(display "\n--- type error example ---\n")
(typed-repl-eval 'undefined)

;;; ============================================================
;;; Typed Prelude
;;; ============================================================
(test-section "Typed Prelude")

(test-typed-value "prelude id" 42
  (typed-run-prelude '(id 42) 100))

(test-typed-value "prelude inc" 6
  (typed-run-prelude '(inc 5) 100))

(test-typed-value "prelude double" 10
  (typed-run-prelude '(double 5) 100))

(test-typed-value "prelude square" 25
  (typed-run-prelude '(square 5) 100))

(test-typed-value "prelude bool-not" #f
  (typed-run-prelude '(bool-not #t) 100))

(test-typed-value "prelude compose" 12
  (typed-run-prelude '(((compose double) inc) 5) 200))

(newline)
(display "✓ All typed evaluation tests complete.\n")
