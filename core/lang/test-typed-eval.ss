;;; core/lang/test-typed-eval.ss — Tests for Typed Evaluation
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/blocks/block.ss")
(load "core/lang/prim.ss")
(load "core/lang/eval.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/infer.ss")
(load "core/types/annotate.ss")
(load "core/lang/typed-eval.ss")

;;; ============================================================================
;;; Domain-specific assertions
;;; ============================================================================

(define (assert-typed-result name expected-type expected-value result)
  "Assert result is (ok typed-value) with expected type and value"
  (cond
    [(not (eq? (car result) 'ok))
     (inc-failed!)
     (display "    ") (display (ctx-name)) (display " - ")
     (display name) (display ": not ok, got ")
     (write result) (newline)
     #f]
    [(not (typed? (cadr result)))
     (inc-failed!)
     (display "    ") (display (ctx-name)) (display " - ")
     (display name) (display ": not typed, got ")
     (write (cadr result)) (newline)
     #f]
    [else
     (let ([tv (cadr result)])
       (if (and (type=? expected-type (typed-type tv))
                (equal? expected-value (typed-value tv)))
           #t
           (begin
             (inc-failed!)
             (display "    ") (display (ctx-name)) (display " - ")
             (display name) (display ": expected ")
             (write expected-value) (display " : ")
             (display (type->string expected-type))
             (display ", got ")
             (write (typed-value tv)) (display " : ")
             (display (type->string (typed-type tv)))
             (newline)
             #f)))]))

(define (assert-typed-value-only name expected-value result)
  "Assert result is (ok ...) with expected value (ignoring type)"
  (cond
    [(not (eq? (car result) 'ok))
     (inc-failed!)
     (display "    ") (display (ctx-name)) (display " - ")
     (display name) (display ": not ok, got ")
     (write result) (newline)
     #f]
    [else
     (let ([val (if (typed? (cadr result))
                    (typed-value (cadr result))
                    (cadr result))])
       (if (equal? expected-value val)
           #t
           (begin
             (inc-failed!)
             (display "    ") (display (ctx-name)) (display " - ")
             (display name) (display ": expected ")
             (write expected-value) (display ", got ")
             (write val) (newline)
             #f)))]))

(define (assert-error-result name result)
  "Assert result is an error"
  (if (and (pair? result) (eq? (car result) 'error))
      #t
      (begin
        (inc-failed!)
        (display "    ") (display (ctx-name)) (display " - ")
        (display name) (display ": expected error, got ")
        (write result) (newline)
        #f)))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group typecheck-eval-basic
  (define-test "integer"
    (assert-typed-result "integer" 'Int 42
                         (typecheck-eval '42 100)))

  (define-test "boolean"
    (assert-typed-result "boolean" 'Bool #t
                         (typecheck-eval '#t 100)))

  (define-test "string"
    (assert-typed-result "string" 'String "hello"
                         (typecheck-eval '"hello" 100)))

  (define-test "add"
    (assert-typed-result "add" 'Int 5
                         (typecheck-eval '(prim 'add 2 3) 100)))

  (define-test "neg"
    (assert-typed-result "neg" 'Int -7
                         (typecheck-eval '(prim 'neg 7) 100)))

  (define-test "comparison"
    (assert-typed-result "comparison" 'Bool #t
                         (typecheck-eval '(prim 'lt? 1 2) 100))))

(test-group type-errors
  (define-test "unbound variable"
    (assert-error-result "unbound variable"
                         (typecheck-eval 'undefined 100)))

  (define-test "applying non-function"
    (assert-error-result "applying non-function"
                         (typecheck-eval '(42 1 2) 100))))

(test-group lambda-and-application
  (define-test "apply identity"
    (assert-typed-value-only "apply identity" 42
                             (typecheck-eval '((fn (x) x) 42) 100)))

  (define-test "apply add-one"
    (assert-typed-value-only "apply add-one" 6
                             (typecheck-eval '((fn (x) (prim 'add x 1)) 5) 100)))

  (define-test "curried add"
    (assert-typed-value-only "curried add" 7
                             (typecheck-eval '(((fn (x) (fn (y) (prim 'add x y))) 3) 4) 200))))

(test-group let-bindings
  (define-test "simple let"
    (assert-typed-value-only "simple let" 42
                             (typecheck-eval '(let ((x 42)) x) 100)))

  (define-test "let with computation"
    (assert-typed-value-only "let with computation" 10
                             (typecheck-eval '(let ((x 3) (y 7)) (prim 'add x y)) 100)))

  (define-test "nested let"
    (assert-typed-value-only "nested let" 6
                             (typecheck-eval '(let ((x 1))
                                                (let ((y 2))
                                                  (let ((z 3))
                                                    (prim 'add x (prim 'add y z)))))
                                             100))))

(test-group if-conditional
  (define-test "if true"
    (assert-typed-value-only "if true" 1
                             (typecheck-eval '(if #t 1 2) 100)))

  (define-test "if false"
    (assert-typed-value-only "if false" 2
                             (typecheck-eval '(if #f 1 2) 100)))

  (define-test "if computed"
    (assert-typed-value-only "if computed" 'yes
                             (typecheck-eval '(if (prim 'lt? 1 2) 'yes 'no) 100))))

(test-group recursion-fix
  (define-test "factorial 5"
    (assert-typed-value-only "factorial 5" 120
                             (typecheck-eval
                              '(let ((fact (fix fact (fn (n)
                                                       (if (prim 'zero? n)
                                                           1
                                                           (prim 'mul n (fact (prim 'sub n 1))))))))
                                 (fact 5))
                              500)))

  (define-test "fibonacci 6"
    (assert-typed-value-only "fibonacci 6" 8
                             (typecheck-eval
                              '(let ((fib (fix fib (fn (n)
                                                     (if (prim 'lt? n 2)
                                                         n
                                                         (prim 'add (fib (prim 'sub n 1))
                                                               (fib (prim 'sub n 2))))))))
                                 (fib 6))
                              1000))))

(test-group case-expressions
  (define-test "case on True block"
    (assert-typed-value-only "case on True block" 1
                             (typecheck-eval
                              '(let ((b (prim 'make-block 'True (prim 'string->utf8 "") (prim 'list->vec (quote ())))))
                                 (case b
                                   ((True) 1)
                                   ((False) 0)))
                              100)))

  (define-test "case on False block"
    (assert-typed-value-only "case on False block" 0
                             (typecheck-eval
                              '(let ((b (prim 'make-block 'False (prim 'string->utf8 "") (prim 'list->vec (quote ())))))
                                 (case b
                                   ((True) 1)
                                   ((False) 0)))
                              100)))

  (define-test "case with Pair tag"
    (assert-typed-value-only "case with Pair tag" 1
                             (typecheck-eval
                              '(let ((b (prim 'make-block 'Pair (prim 'string->utf8 "") (prim 'list->vec (quote ())))))
                                 (case b
                                   ((True) 0) ((Pair) 1)))
                              100))))

(test-group typed-run-pipeline
  (define-test "typed-run int"
    (assert-typed-result "typed-run int" 'Int 42
                         (typed-run '42 100)))

  (define-test "typed-run bool"
    (assert-typed-result "typed-run bool" 'Bool #f
                         (typed-run '#f 100)))

  (define-test "typed-run computation"
    (assert-typed-value-only "typed-run computation" 25
                             (typed-run '(prim 'mul 5 5) 100))))

(test-group typed-values
  (define-test "typed?"
    (let ([tv (typed 'Int 42)])
      (assert-true (typed? tv))))

  (define-test "typed-type"
    (let ([tv (typed 'Int 42)])
      (assert-equal 'Int (typed-type tv))))

  (define-test "typed-value"
    (let ([tv (typed 'Int 42)])
      (assert-equal 42 (typed-value tv)))))

(test-group show-typed-fn
  (define-test "show-typed int"
    (assert-equal "42 : Int"
                  (show-typed (typed 'Int 42))))

  (define-test "show-typed function"
    (assert-equal "<closure> : (Int → Int)"
                  (show-typed (typed '(-> Int Int) '<closure>)))))

(test-group typed-prelude
  (define-test "prelude id"
    (assert-typed-value-only "prelude id" 42
                             (typed-run-prelude '(id 42) 100)))

  (define-test "prelude inc"
    (assert-typed-value-only "prelude inc" 6
                             (typed-run-prelude '(inc 5) 100)))

  (define-test "prelude double"
    (assert-typed-value-only "prelude double" 10
                             (typed-run-prelude '(double 5) 100)))

  (define-test "prelude square"
    (assert-typed-value-only "prelude square" 25
                             (typed-run-prelude '(square 5) 100)))

  (define-test "prelude bool-not"
    (assert-typed-value-only "prelude bool-not" #f
                             (typed-run-prelude '(bool-not #t) 100)))

  (define-test "prelude compose"
    (assert-typed-value-only "prelude compose" 12
                             (typed-run-prelude '(((compose double) inc) 5) 200))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
