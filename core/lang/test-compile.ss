;;; fabric/stitches/test-compile.ss — Tests for the unified compilation pipeline
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/lang/compile.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group result-type
  (define-test "result-ok creates ok"
    (let ([ok (result-ok 42)])
      (assert-true (result-ok? ok))))

  (define-test "result-ok value"
    (let ([ok (result-ok 42)])
      (assert-equal 42 (result-value ok))))

  (define-test "result-error creates error"
    (let ([err (result-error 'test 'some-error 'details)])
      (assert-true (result-error? err))))

  (define-test "result-error phase"
    (let ([err (result-error 'test 'some-error 'details)])
      (assert-equal 'test (result-phase err))))

  (define-test "result-error type"
    (let ([err (result-error 'test 'some-error 'details)])
      (assert-equal 'some-error (result-error-type err))))

  (define-test "result-suspended creates suspended"
    (let ([susp (result-suspended 'expr '())])
      (assert-true (result-suspended? susp)))))

(test-group result-threading
  (define-test "bind ok -> ok"
    (let* ([r1 (result-ok 10)]
           [r2 (result-bind r1 (lambda (x) (result-ok (* x 2))))])
      (assert-equal 20 (result-value r2))))

  (define-test "bind error -> error"
    (let* ([r1 (result-error 'test 'fail)]
           [r2 (result-bind r1 (lambda (x) (result-ok (* x 2))))])
      (assert-true (result-error? r2))))

  (define-test "map ok"
    (let* ([r1 (result-ok 5)]
           [r2 (result-map r1 (lambda (x) (+ x 1)))])
      (assert-equal 6 (result-value r2)))))

(test-group parse-phase
  (define-test "parse simple number ok"
    (assert-true (result-ok? (compile "42" 'to 'parse))))

  (define-test "parse number value"
    (assert-equal 42 (result-value (compile "42" 'to 'parse))))

  (define-test "parse symbol ok"
    (assert-true (result-ok? (compile "x" 'to 'parse))))

  (define-test "parse symbol value"
    (assert-equal 'x (result-value (compile "x" 'to 'parse))))

  (define-test "parse list ok"
    (assert-true (result-ok? (compile "(+ 1 2)" 'to 'parse))))

  (define-test "parse list value"
    (assert-equal '(+ 1 2) (result-value (compile "(+ 1 2)" 'to 'parse))))

  (define-test "parse lambda ok"
    (assert-true (result-ok? (compile "(fn (x) x)" 'to 'parse)))))

(test-group type-inference
  (define-test "infer number is ok"
    (let ([r (compile "42" 'to 'infer)])
      (assert-true (result-ok? r))))

  (define-test "number has Int type"
    (let ([r (compile "42" 'to 'infer)])
      (assert-equal 'Int (result-value r))))

  (define-test "infer bool is ok"
    (let ([r (compile "#t" 'to 'infer)])
      (assert-true (result-ok? r))))

  (define-test "bool has Bool type"
    (let ([r (compile "#t" 'to 'infer)])
      (assert-equal 'Bool (result-value r))))

  (define-test "infer string is ok"
    (let ([r (compile "\"hello\"" 'to 'infer)])
      (assert-true (result-ok? r))))

  (define-test "string has String type"
    (let ([r (compile "\"hello\"" 'to 'infer)])
      (assert-equal 'String (result-value r)))))

(test-group evaluation
  (define-test "eval number"
    (assert-equal 42 (result-value (compile "42" 'to 'eval))))

  (define-test "eval string"
    (assert-equal "hello" (result-value (compile "\"hello\"" 'to 'eval))))

  (define-test "eval bool true"
    (assert-equal #t (result-value (compile "#t" 'to 'eval))))

  (define-test "eval bool false"
    (assert-equal #f (result-value (compile "#f" 'to 'eval))))

  (define-test "eval add is ok"
    (let ([r (compile "(prim 'add 1 2)" 'to 'eval)])
      (assert-true (result-ok? r))))

  (define-test "eval add value"
    (let ([r (compile "(prim 'add 1 2)" 'to 'eval)])
      (assert-equal 3 (result-value r))))

  (define-test "eval mul is ok"
    (let ([r (compile "(prim 'mul 3 4)" 'to 'eval)])
      (assert-true (result-ok? r))))

  (define-test "eval mul value"
    (let ([r (compile "(prim 'mul 3 4)" 'to 'eval)])
      (assert-equal 12 (result-value r)))))

(test-group lambda-and-application
  (define-test "identity application is ok"
    (let ([r (compile "((fn (x) x) 42)" 'to 'eval)])
      (assert-true (result-ok? r))))

  (define-test "identity returns arg"
    (let ([r (compile "((fn (x) x) 42)" 'to 'eval)])
      (assert-equal 42 (result-value r))))

  (define-test "K combinator is ok"
    (let ([r (compile "(((fn (x) (fn (y) x)) 1) 2)" 'to 'eval)])
      (assert-true (result-ok? r))))

  (define-test "K returns first"
    (let ([r (compile "(((fn (x) (fn (y) x)) 1) 2)" 'to 'eval)])
      (assert-equal 1 (result-value r)))))

(test-group let-bindings
  (define-test "let binding is ok"
    (let ([r (compile "(let ((x 10)) x)" 'to 'eval)])
      (assert-true (result-ok? r))))

  (define-test "let binding value"
    (let ([r (compile "(let ((x 10)) x)" 'to 'eval)])
      (assert-equal 10 (result-value r))))

  (define-test "nested let is ok"
    (let ([r (compile "(let ((x 10)) (let ((y 20)) (prim 'add x y)))" 'to 'eval)])
      (assert-true (result-ok? r))))

  (define-test "nested let value"
    (let ([r (compile "(let ((x 10)) (let ((y 20)) (prim 'add x y)))" 'to 'eval)])
      (assert-equal 30 (result-value r)))))

(test-group convenience-functions
  (define-test "typeof number"
    (assert-equal 'Int (typeof "42")))

  (define-test "typeof string"
    (assert-equal 'String (typeof "\"hi\"")))

  (define-test "typeof bool"
    (assert-equal 'Bool (typeof "#t")))

  (define-test "eval-string number"
    (assert-equal 42 (eval-string "42")))

  (define-test "eval-string with prim"
    (assert-equal 7 (eval-string "(prim 'add 3 4)"))))

(test-group error-handling
  (define-test "unbound var is error"
    (let ([r (compile "undefined-var" 'to 'infer)])
      (assert-true (result-error? r))))

  (define-test "unbound var phase"
    (let ([r (compile "undefined-var" 'to 'infer)])
      (assert-equal 'infer (result-phase r))))

  (define-test "empty input is error"
    (let ([r (compile "" 'to 'parse)])
      (assert-true (result-error? r)))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
