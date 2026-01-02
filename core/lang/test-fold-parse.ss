;;; fabric/stitches/test-fold-parse.ss — Tests for Fold Parser
;;;
;;; Tests the spanned parser combinators and Fold syntax parser.

(load "core/lang/fold-parse.ss")

(display "Fold Parser Tests\n")
(display "=================\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  ✓ ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  ✗ ") (display name)
       (display " — expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(define (test-parse name input expected)
  (let ([result (parse-fold-expr input)])
       (if (eq? (car result) 'ok)
           (test name expected (strip-spans (cadr result)))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (display "  ✗ ") (display name)
            (display " — parse failed: ") (write result)
            (newline)))))

(define (test-parse-error name input)
  (let ([result (parse-fold-expr input)])
       (if (eq? (car result) 'error)
           (begin
            (set! tests-passed (+ tests-passed 1))
            (display "  ✓ ") (display name) (display " (correctly failed)") (newline))
           (begin
            (set! tests-failed (+ tests-failed 1))
            (display "  ✗ ") (display name)
            (display " — should have failed, got: ") (write result)
            (newline)))))

;;; ============================================================
;;; Span Tests
;;; ============================================================

(display "Spans:\n")

(test "make-span"
      '(span "test.ss" 1 5 2 10)
      (make-span "test.ss" 1 5 2 10))

(test "span?"
      #t
      (span? (make-span "x" 1 1 1 1)))

(test "span-file"
      "test.ss"
      (span-file (make-span "test.ss" 1 5 2 10)))

(test "span-line"
      1
      (span-line (make-span "test.ss" 1 5 2 10)))

(test "span-column"
      5
      (span-column (make-span "test.ss" 1 5 2 10)))

(test "format-span"
      "test.ss:1:5"
      (format-span (make-span "test.ss" 1 5 2 10)))

;;; ============================================================
;;; State Tests
;;; ============================================================

(display "\nParser State:\n")

(test "initial-state"
      #t
      (state? (initial-state "hello")))

(test "state-input"
      "hello"
      (state-input (initial-state "hello")))

(test "state-line initial"
      1
      (state-line (initial-state "hello")))

(test "state-column initial"
      1
      (state-column (initial-state "hello")))

(let ([s (advance-state (initial-state "ab") #\a)])
     (test "advance-state input"
           "b"
           (state-input s))
     (test "advance-state column"
           2
           (state-column s)))

(let ([s (advance-state (initial-state "a\nb") #\a)])
     (let ([s2 (advance-state s #\newline)])
          (test "advance-state newline line"
                2
                (state-line s2))
          (test "advance-state newline column"
                1
                (state-column s2))))

;;; ============================================================
;;; Basic Parser Tests
;;; ============================================================

(display "\nBasic Parsers:\n")

(let ([result (run-spanned s-item "abc")])
     (test "s-item success"
           #\a
           (spanned-value result))
     (test "s-item remaining"
           "bc"
           (state-input (spanned-state result))))

(let ([result (run-spanned s-item "")])
     (test "s-item empty fails"
           #t
           (spanned-err? result)))

(let ([result (run-spanned s-eof "")])
     (test "s-eof at end"
           #t
           (spanned-ok? result)))

(let ([result (run-spanned s-eof "x")])
     (test "s-eof not at end"
           #t
           (spanned-err? result)))

;;; ============================================================
;;; Number Parsing
;;; ============================================================

(display "\nNumbers:\n")

(test-parse "integer" "42" 42)
(test-parse "negative integer" "-17" -17)
(test-parse "zero" "0" 0)
(test-parse "float" "3.14" 3.14)
(test-parse "negative float" "-2.5" -2.5)

;;; ============================================================
;;; String Parsing
;;; ============================================================

(display "\nStrings:\n")

(test-parse "empty string" "\"\"" "")
(test-parse "simple string" "\"hello\"" "hello")
(test-parse "string with spaces" "\"hello world\"" "hello world")
(test-parse "string with escape" "\"a\\nb\"" "a\nb")
(test-parse "string with quote" "\"say \\\"hi\\\"\"" "say \"hi\"")

;;; ============================================================
;;; Boolean Parsing
;;; ============================================================

(display "\nBooleans:\n")

(test-parse "true" "#t" #t)
(test-parse "false" "#f" #f)

;;; ============================================================
;;; Symbol Parsing
;;; ============================================================

(display "\nSymbols:\n")

(test-parse "simple symbol" "foo" 'foo)
(test-parse "symbol with dash" "foo-bar" 'foo-bar)
(test-parse "symbol with numbers" "x123" 'x123)
(test-parse "symbol with special" "list?" 'list?)
(test-parse "plus symbol" "+" '+)
(test-parse "arrow symbol" "->" '->)

;;; ============================================================
;;; List Parsing
;;; ============================================================

(display "\nLists:\n")

(test-parse "empty list" "()" '())
(test-parse "single element" "(x)" '(x))
(test-parse "multiple elements" "(a b c)" '(a b c))
(test-parse "nested list" "((a b) c)" '((a b) c))
(test-parse "mixed types" "(1 \"hi\" x)" '(1 "hi" x))
(test-parse "lambda" "(fn (x) x)" '(fn (x) x))
(test-parse "let" "(let ((x 1)) x)" '(let ((x 1)) x))

;;; ============================================================
;;; Quote Parsing
;;; ============================================================

(display "\nQuotes:\n")

(test-parse "quoted symbol" "'x" '(quote x))
(test-parse "quoted list" "'(1 2 3)" '(quote (1 2 3)))
(test-parse "quasiquote" "`x" '(quasiquote x))
(test-parse "unquote" ",x" '(unquote x))
(test-parse "unquote-splicing" ",@xs" '(unquote-splicing xs))

;;; ============================================================
;;; Comments
;;; ============================================================

(display "\nComments:\n")

(test-parse "after comment" "; comment\n42" 42)
(test-parse "inline comment" "42 ; comment" 42)

;;; ============================================================
;;; Whitespace Handling
;;; ============================================================

(display "\nWhitespace:\n")

(test-parse "leading space" "  42" 42)
(test-parse "trailing space" "42  " 42)
(test-parse "multiline" "(a\n  b\n  c)" '(a b c))
(test-parse "tabs" "(\ta\tb\t)" '(a b))

;;; ============================================================
;;; Span Tracking
;;; ============================================================

(display "\nSpan Tracking:\n")

(let ([result (parse-fold-expr "42" "test.ss")])
     (if (eq? (car result) 'ok)
         (let ([ast (cadr result)])
              (test "span preserved"
                    #t
                    (spanned-value? ast))
              (test "span file"
                    "test.ss"
                    (span-file (get-value-span ast)))
              (test "span line"
                    1
                    (span-line (get-value-span ast)))
              (test "span column"
                    1
                    (span-column (get-value-span ast))))
         (begin
          (set! tests-failed (+ tests-failed 1))
          (display "  ✗ span tracking failed to parse\n"))))

(let ([result (parse-fold-expr "  42" "test.ss")])
     (if (eq? (car result) 'ok)
         (let ([ast (cadr result)])
              (test "span after whitespace column"
                    3
                    (span-column (get-value-span ast))))
         (begin
          (set! tests-failed (+ tests-failed 1))
          (display "  ✗ span after whitespace failed to parse\n"))))

;;; ============================================================
;;; Error Cases
;;; ============================================================

(display "\nError Cases:\n")

(test-parse-error "unclosed string" "\"hello")
(test-parse-error "unclosed paren" "(a b")
;; Note: "a)" parses 'a' successfully, leaving ')' - this is correct single-expr behavior
(test-parse "partial input" "a)" 'a)
(test-parse-error "empty input" "")

;;; ============================================================
;;; Complex Examples
;;; ============================================================

(display "\nComplex Examples:\n")

(test-parse "factorial"
            "(fix fact (fn (n) (if (= n 0) 1 (* n (fact (- n 1))))))"
            '(fix fact (fn (n) (if (= n 0) 1 (* n (fact (- n 1)))))))

(test-parse "let with multiple bindings"
            "(let ((x 1) (y 2)) (+ x y))"
            '(let ((x 1) (y 2)) (+ x y)))

(test-parse "case expression"
            "(case x ((True) 1) ((False) 0))"
            '(case x ((True) 1) ((False) 0)))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "==================\n")
(display (string-append "Passed: " (number->string tests-passed) "\n"))
(display (string-append "Failed: " (number->string tests-failed) "\n"))

(if (= tests-failed 0)
    (display "\n✓ All Fold parser tests passed!\n")
    (display "\n✗ Some tests failed!\n"))
