;;; core/lang/test-fold-parse.ss — Tests for Fold Parser
;;; Standardized to use test-framework.ss
;;;
;;; Tests the spanned parser combinators and Fold syntax parser.

(load "core/testing/test-framework.ss")
(load "core/lang/fold-parse.ss")

;;; ============================================================================
;;; Domain-specific assertions
;;; ============================================================================

(define (assert-parse name expected input)
  "Assert that parsing input produces expected AST (stripped of spans)"
  (let ([result (parse-fold-expr input)])
    (if (eq? (car result) 'ok)
        (if (equal? expected (strip-spans (cadr result)))
            #t
            (begin
              (inc-failed!)
              (display "    ") (display (ctx-name)) (display " - ")
              (display name) (display ": expected ")
              (write expected) (display ", got ")
              (write (strip-spans (cadr result))) (newline)
              #f))
        (begin
          (inc-failed!)
          (display "    ") (display (ctx-name)) (display " - ")
          (display name) (display " parse failed: ")
          (write result) (newline)
          #f))))

(define (assert-parse-error name input)
  "Assert that parsing input fails"
  (let ([result (parse-fold-expr input)])
    (if (eq? (car result) 'error)
        #t
        (begin
          (inc-failed!)
          (display "    ") (display (ctx-name)) (display " - ")
          (display name) (display " should have failed, got: ")
          (write result) (newline)
          #f))))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group spans
  (define-test "make-span"
    (assert-equal '(span "test.ss" 1 5 2 10)
                  (make-span "test.ss" 1 5 2 10)))

  (define-test "span?"
    (assert-true (span? (make-span "x" 1 1 1 1))))

  (define-test "span-file"
    (assert-equal "test.ss"
                  (span-file (make-span "test.ss" 1 5 2 10))))

  (define-test "span-line"
    (assert-equal 1
                  (span-line (make-span "test.ss" 1 5 2 10))))

  (define-test "span-column"
    (assert-equal 5
                  (span-column (make-span "test.ss" 1 5 2 10))))

  (define-test "format-span"
    (assert-equal "test.ss:1:5"
                  (format-span (make-span "test.ss" 1 5 2 10)))))

(test-group parser-state
  (define-test "initial-state"
    (assert-true (state? (initial-state "hello"))))

  (define-test "state-input"
    (assert-equal "hello"
                  (state-input (initial-state "hello"))))

  (define-test "state-line initial"
    (assert-equal 1
                  (state-line (initial-state "hello"))))

  (define-test "state-column initial"
    (assert-equal 1
                  (state-column (initial-state "hello"))))

  (define-test "advance-state input"
    (let ([s (advance-state (initial-state "ab") #\a)])
      (assert-equal "b" (state-remaining-input s))))

  (define-test "advance-state column"
    (let ([s (advance-state (initial-state "ab") #\a)])
      (assert-equal 2 (state-column s))))

  (define-test "advance-state newline line"
    (let* ([s (advance-state (initial-state "a\nb") #\a)]
           [s2 (advance-state s #\newline)])
      (assert-equal 2 (state-line s2))))

  (define-test "advance-state newline column"
    (let* ([s (advance-state (initial-state "a\nb") #\a)]
           [s2 (advance-state s #\newline)])
      (assert-equal 1 (state-column s2)))))

(test-group basic-parsers
  (define-test "s-item success"
    (let ([result (run-spanned s-item "abc")])
      (assert-equal #\a (spanned-value result))))

  (define-test "s-item remaining"
    (let ([result (run-spanned s-item "abc")])
      (assert-equal "bc" (state-remaining-input (spanned-state result)))))

  (define-test "s-item empty fails"
    (let ([result (run-spanned s-item "")])
      (assert-true (spanned-err? result))))

  (define-test "s-eof at end"
    (let ([result (run-spanned s-eof "")])
      (assert-true (spanned-ok? result))))

  (define-test "s-eof not at end"
    (let ([result (run-spanned s-eof "x")])
      (assert-true (spanned-err? result)))))

(test-group numbers
  (define-test "integer"
    (assert-parse "integer" 42 "42"))

  (define-test "negative integer"
    (assert-parse "negative integer" -17 "-17"))

  (define-test "zero"
    (assert-parse "zero" 0 "0"))

  (define-test "float"
    (assert-parse "float" 3.14 "3.14"))

  (define-test "negative float"
    (assert-parse "negative float" -2.5 "-2.5")))

(test-group strings
  (define-test "empty string"
    (assert-parse "empty string" "" "\"\""))

  (define-test "simple string"
    (assert-parse "simple string" "hello" "\"hello\""))

  (define-test "string with spaces"
    (assert-parse "string with spaces" "hello world" "\"hello world\""))

  (define-test "string with escape"
    (assert-parse "string with escape" "a\nb" "\"a\\nb\""))

  (define-test "string with quote"
    (assert-parse "string with quote" "say \"hi\"" "\"say \\\"hi\\\"\"")))

(test-group booleans
  (define-test "true"
    (assert-parse "true" #t "#t"))

  (define-test "false"
    (assert-parse "false" #f "#f")))

(test-group symbols
  (define-test "simple symbol"
    (assert-parse "simple symbol" 'foo "foo"))

  (define-test "symbol with dash"
    (assert-parse "symbol with dash" 'foo-bar "foo-bar"))

  (define-test "symbol with numbers"
    (assert-parse "symbol with numbers" 'x123 "x123"))

  (define-test "symbol with special"
    (assert-parse "symbol with special" 'list? "list?"))

  (define-test "plus symbol"
    (assert-parse "plus symbol" '+ "+"))

  (define-test "arrow symbol"
    (assert-parse "arrow symbol" '-> "->")))

(test-group lists
  (define-test "empty list"
    (assert-parse "empty list" '() "()"))

  (define-test "single element"
    (assert-parse "single element" '(x) "(x)"))

  (define-test "multiple elements"
    (assert-parse "multiple elements" '(a b c) "(a b c)"))

  (define-test "nested list"
    (assert-parse "nested list" '((a b) c) "((a b) c)"))

  (define-test "mixed types"
    (assert-parse "mixed types" '(1 "hi" x) "(1 \"hi\" x)"))

  (define-test "lambda"
    (assert-parse "lambda" '(fn (x) x) "(fn (x) x)"))

  (define-test "let"
    (assert-parse "let" '(let ((x 1)) x) "(let ((x 1)) x)")))

(test-group quotes
  (define-test "quoted symbol"
    (assert-parse "quoted symbol" '(quote x) "'x"))

  (define-test "quoted list"
    (assert-parse "quoted list" '(quote (1 2 3)) "'(1 2 3)"))

  (define-test "quasiquote"
    (assert-parse "quasiquote" '(quasiquote x) "`x"))

  (define-test "unquote"
    (assert-parse "unquote" '(unquote x) ",x"))

  (define-test "unquote-splicing"
    (assert-parse "unquote-splicing" '(unquote-splicing xs) ",@xs")))

(test-group comments
  (define-test "after comment"
    (assert-parse "after comment" 42 "; comment\n42"))

  (define-test "inline comment"
    (assert-parse "inline comment" 42 "42 ; comment")))

(test-group whitespace
  (define-test "leading space"
    (assert-parse "leading space" 42 "  42"))

  (define-test "trailing space"
    (assert-parse "trailing space" 42 "42  "))

  (define-test "multiline"
    (assert-parse "multiline" '(a b c) "(a\n  b\n  c)"))

  (define-test "tabs"
    (assert-parse "tabs" '(a b) "(\ta\tb\t)")))

(test-group span-tracking
  (define-test "span preserved"
    (let ([result (parse-fold-expr "42" "test.ss")])
      (if (eq? (car result) 'ok)
          (assert-true (spanned-value? (cadr result)))
          (begin (inc-failed!) #f))))

  (define-test "span file"
    (let ([result (parse-fold-expr "42" "test.ss")])
      (if (eq? (car result) 'ok)
          (assert-equal "test.ss" (span-file (get-value-span (cadr result))))
          (begin (inc-failed!) #f))))

  (define-test "span line"
    (let ([result (parse-fold-expr "42" "test.ss")])
      (if (eq? (car result) 'ok)
          (assert-equal 1 (span-line (get-value-span (cadr result))))
          (begin (inc-failed!) #f))))

  (define-test "span column"
    (let ([result (parse-fold-expr "42" "test.ss")])
      (if (eq? (car result) 'ok)
          (assert-equal 1 (span-column (get-value-span (cadr result))))
          (begin (inc-failed!) #f))))

  (define-test "span after whitespace column"
    (let ([result (parse-fold-expr "  42" "test.ss")])
      (if (eq? (car result) 'ok)
          (assert-equal 3 (span-column (get-value-span (cadr result))))
          (begin (inc-failed!) #f)))))

(test-group error-cases
  (define-test "unclosed string"
    (assert-parse-error "unclosed string" "\"hello"))

  (define-test "unclosed paren"
    (assert-parse-error "unclosed paren" "(a b"))

  ;; Note: "a)" parses 'a' successfully, leaving ')' - this is correct single-expr behavior
  (define-test "partial input"
    (assert-parse "partial input" 'a "a)"))

  (define-test "empty input"
    (assert-parse-error "empty input" "")))

(test-group complex-examples
  (define-test "factorial"
    (assert-parse "factorial"
                  '(fix fact (fn (n) (if (= n 0) 1 (* n (fact (- n 1))))))
                  "(fix fact (fn (n) (if (= n 0) 1 (* n (fact (- n 1))))))"))

  (define-test "let with multiple bindings"
    (assert-parse "let with multiple bindings"
                  '(let ((x 1) (y 2)) (+ x y))
                  "(let ((x 1) (y 2)) (+ x y))"))

  (define-test "case expression"
    (assert-parse "case expression"
                  '(case x ((True) 1) ((False) 0))
                  "(case x ((True) 1) ((False) 0))")))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
