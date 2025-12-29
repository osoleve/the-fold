;;; fabric/stitches/test-compile.ss — Tests for the unified compilation pipeline
;;;
;;; Tests the compile.ss pipeline integration.

(load "fabric/stitches/compile.ss")

(display "
")
(display "╔══════════════════════════════════════════════════════════════════╗
")
(display "║                Unified Compile Pipeline Tests                     ║
")
(display "╚══════════════════════════════════════════════════════════════════╝
")
(display "
")

;;; ============================================================
;;; Test Helpers
;;; ============================================================

(define *test-pass* 0)
(define *test-fail* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *test-pass* (+ *test-pass* 1))
       (display "✓
"))
      (begin
       (set! *test-fail* (+ *test-fail* 1))
       (display "✗
")
       (display "    expected: ") (display expected) (newline)
       (display "    actual:   ") (display actual) (newline))))

(define (test-ok name result)
  (test name #t (result-ok? result)))

(define (test-error name result)
  (test name #t (result-error? result)))

(define (test-value name expected result)
  (if (result-ok? result)
      (test name expected (result-value result))
      (test name expected result)))

;;; ============================================================
;;; Result Type Tests
;;; ============================================================

(display "Result Type
")
(display "───────────
")

(let ([ok (result-ok 42)])
     (test "result-ok creates ok" #t (result-ok? ok))
     (test "result-ok value" 42 (result-value ok)))

(let ([err (result-error 'test 'some-error 'details)])
     (test "result-error creates error" #t (result-error? err))
     (test "result-error phase" 'test (result-phase err))
     (test "result-error type" 'some-error (result-error-type err)))

(let ([susp (result-suspended 'expr '())])
     (test "result-suspended creates suspended" #t (result-suspended? susp)))

(display "
")

;;; ============================================================
;;; Result Threading Tests
;;; ============================================================

(display "Result Threading
")
(display "────────────────
")

(let* ([r1 (result-ok 10)]
       [r2 (result-bind r1 (lambda (x) (result-ok (* x 2))))])
      (test "bind ok -> ok" 20 (result-value r2)))

(let* ([r1 (result-error 'test 'fail)]
       [r2 (result-bind r1 (lambda (x) (result-ok (* x 2))))])
      (test "bind error -> error" #t (result-error? r2)))

(let* ([r1 (result-ok 5)]
       [r2 (result-map r1 (lambda (x) (+ x 1)))])
      (test "map ok" 6 (result-value r2)))

(display "
")

;;; ============================================================
;;; Parse Phase Tests
;;; ============================================================

(display "Parse Phase
")
(display "───────────
")

(test-ok "parse simple number" (compile "42" 'to 'parse))
(test-value "parse number value" 42 (compile "42" 'to 'parse))

(test-ok "parse symbol" (compile "x" 'to 'parse))
(test-value "parse symbol value" 'x (compile "x" 'to 'parse))

(test-ok "parse list" (compile "(+ 1 2)" 'to 'parse))
(test-value "parse list value" '(+ 1 2) (compile "(+ 1 2)" 'to 'parse))

(test-ok "parse lambda" (compile "(fn (x) x)" 'to 'parse))

(display "
")

;;; ============================================================
;;; Type Inference Tests
;;; ============================================================

(display "Type Inference
")
(display "──────────────
")

(let ([r (compile "42" 'to 'infer)])
     (test "infer number is ok" #t (result-ok? r))
     (test "number has Int type" 'Int (result-value r)))

(let ([r (compile "#t" 'to 'infer)])
     (test "infer bool is ok" #t (result-ok? r))
     (test "bool has Bool type" 'Bool (result-value r)))

(let ([r (compile "\"hello\"" 'to 'infer)])
     (test "infer string is ok" #t (result-ok? r))
     (test "string has String type" 'String (result-value r)))

(display "
")

;;; ============================================================
;;; Evaluation Tests
;;; ============================================================

(display "Evaluation
")
(display "──────────
")

(test-value "eval number" 42 (compile "42" 'to 'eval))
(test-value "eval string" "hello" (compile "\"hello\"" 'to 'eval))
(test-value "eval bool true" #t (compile "#t" 'to 'eval))
(test-value "eval bool false" #f (compile "#f" 'to 'eval))

;; Arithmetic via prim (primitives are named add, mul, not +, *)
(let ([r (compile "(prim 'add 1 2)" 'to 'eval)])
     (test "eval add is ok" #t (result-ok? r))
     (test "eval add value" 3 (result-value r)))

(let ([r (compile "(prim 'mul 3 4)" 'to 'eval)])
     (test "eval mul is ok" #t (result-ok? r))
     (test "eval mul value" 12 (result-value r)))

(display "
")

;;; ============================================================
;;; Lambda and Application Tests
;;; ============================================================

(display "Lambda and Application
")
(display "──────────────────────
")

;; Identity function - use implicit application (f arg) not (call f arg)
;; Type inference uses implicit application, eval supports both
(let ([r (compile "((fn (x) x) 42)" 'to 'eval)])
     (test "identity application is ok" #t (result-ok? r))
     (test "identity returns arg" 42 (result-value r)))

;; K combinator (returns first arg)
(let ([r (compile "(((fn (x) (fn (y) x)) 1) 2)" 'to 'eval)])
     (test "K combinator is ok" #t (result-ok? r))
     (test "K returns first" 1 (result-value r)))

(display "
")

;;; ============================================================
;;; Let Binding Tests
;;; ============================================================

(display "Let Bindings
")
(display "────────────
")

(let ([r (compile "(let ((x 10)) x)" 'to 'eval)])
     (test "let binding is ok" #t (result-ok? r))
     (test "let binding value" 10 (result-value r)))

(let ([r (compile "(let ((x 10)) (let ((y 20)) (prim 'add x y)))" 'to 'eval)])
     (test "nested let is ok" #t (result-ok? r))
     (test "nested let value" 30 (result-value r)))

(display "
")

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(display "Convenience Functions
")
(display "─────────────────────
")

(test "typeof number" 'Int (typeof "42"))
(test "typeof string" 'String (typeof "\"hi\""))
(test "typeof bool" 'Bool (typeof "#t"))

(test "eval-string number" 42 (eval-string "42"))
(test "eval-string with prim" 7 (eval-string "(prim 'add 3 4)"))

(display "
")

;;; ============================================================
;;; Error Handling Tests
;;; ============================================================

(display "Error Handling
")
(display "──────────────
")

;; Unbound variable
(let ([r (compile "undefined-var" 'to 'infer)])
     (test "unbound var is error" #t (result-error? r))
     (test "unbound var phase" 'infer (result-phase r)))

;; Empty input
(let ([r (compile "" 'to 'parse)])
     (test "empty input is error" #t (result-error? r)))

(display "
")

;;; ============================================================
;;; Summary
;;; ============================================================

(display "────────────────────────────────────────────────────────
")
(display (format "  Total: ~a tests
" (+ *test-pass* *test-fail*)))
(display (format "  Passed: ~a
" *test-pass*))
(display (format "  Failed: ~a
" *test-fail*))
(display "────────────────────────────────────────────────────────
")

(if (= *test-fail* 0)
    (display "✓ All compile pipeline tests passed!
")
    (display "✗ Some tests failed.
"))
