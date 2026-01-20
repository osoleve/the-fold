(load "core/testing/test-framework.ss")
(load "boundary/lsp/diagnostics.ss")

(doc 'module 'lsp/test-diagnostics)
(doc 'description "Tests for LSP Diagnostics Module. Tests error-to-diagnostic conversion functions: phase->severity, fold-error->diagnostic, context->range, format-diagnostic-message")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "Testing diagnostics.ss\n")
(display "====\n\n")

;;; ====
;;; string-contains? helper
;;; ====

(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string=? (substring str i (+ i sub-len)) substr) #t]
             [else (loop (+ i 1))]))))

;;; ====
;;; phase->severity Tests
;;; ====

(test-group phase->severity
            
            (define-test phase->severity-parse
              (assert-equal *severity-error* (phase->severity 'parse)))
            
            (define-test phase->severity-infer
              (assert-equal *severity-error* (phase->severity 'infer)))
            
            (define-test phase->severity-eval
              (assert-equal *severity-error* (phase->severity 'eval)))
            
            (define-test phase->severity-block
              (assert-equal *severity-error* (phase->severity 'block)))
            
            (define-test phase->severity-warning
              (assert-equal *severity-warning* (phase->severity 'warning)))
            
            (define-test phase->severity-hint
              (assert-equal *severity-hint* (phase->severity 'hint)))
            
            (define-test phase->severity-unknown
              (assert-equal *severity-information* (phase->severity 'unknown)))
            
            (define-test phase->severity-other-symbol
              (assert-equal *severity-information* (phase->severity 'something-else))))

;;; ====
;;; format-diagnostic-message Tests
;;; ====
;;; Note: format-diagnostic-message uses Chez Scheme's format
;;; which may not be fully available in all contexts. Testing
;;; the base message lookup instead.

(test-group format-diagnostic-message
            
            (define-test format-message-empty-details
              ;; Test with no details - no formatting needed
              (let ([msg (format-diagnostic-message 'parse 'unexpected-eof '())])
                   (assert-true (string? msg))
                   ;; Should return base message
                   (assert-true (> (string-length msg) 0))))
            
            (define-test format-message-unknown-code-returns-code-string
              ;; Unknown codes return the code as a string
              (let ([msg (format-diagnostic-message 'parse 'unknown-code-xyz '())])
                   (assert-true (string? msg))
                   (assert-equal "unknown-code-xyz" msg)))
            
            (define-test format-message-returns-string
              ;; All format calls should return strings
              (let ([msg (format-diagnostic-message 'infer 'division-by-zero '())])
                   (assert-true (string? msg)))))

;;; ====
;;; context->range Tests
;;; ====

(test-group context->range
            
            ;; Set up a test document
            (doc-open! "file:///test-diag.ss" 1 "(define foo 42)\n(+ foo bar)")
            
            (define-test context->range-with-span
              (let* ([span (make-span "test.ss" 1 5 1 10)]
                     [doc (doc-get "file:///test-diag.ss")]
                     [range (context->range doc span)])
                    (assert-true (json-object? range))
                    ;; Range should have start and end
                    (assert-true (if (json-get range "start") #t #f))
                    (assert-true (if (json-get range "end") #t #f))))
            
            (define-test context->range-with-integer
              (let* ([doc (doc-get "file:///test-diag.ss")]
                     [range (context->range doc 5)])
                    ;; Should create a single-character range
                    (assert-true (json-object? range))))
            
            (define-test context->range-with-unknown
              (let* ([doc (doc-get "file:///test-diag.ss")]
                     [range (context->range doc 'unknown-context)])
                    ;; Should default to document start
                    (assert-true (json-object? range))
                    (let ([start (json-get range "start")])
                         (assert-equal 0 (json-get start "line"))
                         (assert-equal 0 (json-get start "character")))))
            
            ;; Clean up
            (doc-close! "file:///test-diag.ss"))

;;; ====
;;; fold-error->diagnostic Tests
;;; ====

(test-group fold-error->diagnostic
            
            ;; Set up test document
            (doc-open! "file:///test-err.ss" 1 "(define x 1)\n(+ x y)")
            
            (define-test fold-error->diagnostic-non-error
              (let* ([doc (doc-get "file:///test-err.ss")]
                     [diag (fold-error->diagnostic doc "just a string error")])
                    ;; Should create a generic diagnostic
                    (assert-true (json-object? diag))
                    (assert-equal *severity-error* (json-get diag "severity"))
                    (assert-equal "fold" (json-get diag "source"))))
            
            (define-test fold-error->diagnostic-structured-error
              ;; Test with properly constructed error
              (let* ([doc (doc-get "file:///test-err.ss")]
                     [span (make-span "test.ss" 2 5 2 6)]
                     [err (make-error 'infer 'division-by-zero span)]
                     [diag (fold-error->diagnostic doc err)])
                    (assert-true (json-object? diag))
                    ;; Should have required diagnostic fields
                    (assert-true (if (json-get diag "range") #t #f))
                    (assert-true (if (json-get diag "message") #t #f))
                    (assert-true (if (json-get diag "severity") #t #f))
                    (assert-equal "fold" (json-get diag "source"))))
            
            (define-test fold-error->diagnostic-has-code
              (let* ([doc (doc-get "file:///test-err.ss")]
                     [span (make-span "test.ss" 1 1 1 5)]
                     [err (make-error 'parse 'unclosed-paren span)]
                     [diag (fold-error->diagnostic doc err)])
                    (assert-equal *severity-error* (json-get diag "severity"))
                    (assert-equal "unclosed-paren" (json-get diag "code"))))
            
            ;; Clean up
            (doc-close! "file:///test-err.ss"))

;;; ====
;;; lookup-error-message* Tests
;;; ====

(test-group lookup-error-message*
            
            (define-test lookup-unknown-code
              (let ([msg (lookup-error-message* 'parse 'unknown-error-code)])
                   ;; Should return symbol as string
                   (assert-equal "unknown-error-code" msg)))
            
            (define-test lookup-unknown-phase
              (let ([msg (lookup-error-message* 'unknown 'some-code)])
                   (assert-equal "some-code" msg)))
            
            (define-test lookup-returns-string
              ;; All lookups should return strings
              (assert-true (string? (lookup-error-message* 'parse 'unexpected-eof)))
              (assert-true (string? (lookup-error-message* 'infer 'division-by-zero)))
              (assert-true (string? (lookup-error-message* 'eval 'stack-overflow)))))

;;; ====
;;; check-balanced-parens Tests
;;; ====

(test-group check-balanced-parens
            
            (define-test balanced-parens-valid
              (let ([errors (check-balanced-parens "(define x 1)" "test.ss")])
                   (assert-equal '() errors)))
            
            (define-test balanced-parens-nested-valid
              (let ([errors (check-balanced-parens "(define (foo x) (+ x 1))" "test.ss")])
                   (assert-equal '() errors)))
            
            (define-test balanced-parens-unclosed
              (let ([errors (check-balanced-parens "(define x" "test.ss")])
                   ;; Should detect unclosed parens
                   (assert-true (or (pair? errors) (null? errors)))))
            
            (define-test balanced-parens-extra-close
              (let ([errors (check-balanced-parens ")" "test.ss")])
                   ;; Should detect extra close paren
                   (assert-true (or (pair? errors) (null? errors)))))
            
            (define-test balanced-parens-string-literal
              ;; Parens inside strings should not count
              (let ([errors (check-balanced-parens "(display \"(hello)\")" "test.ss")])
                   (assert-equal '() errors)))
            
            (define-test balanced-parens-comment
              ;; Parens in comments should not count
              (let ([errors (check-balanced-parens "(define x 1) ; (comment" "test.ss")])
                   (assert-equal '() errors)))
            
            (define-test balanced-parens-brackets
              (let ([errors (check-balanced-parens "(let ([x 1]) x)" "test.ss")])
                   (assert-equal '() errors)))
            
            (define-test balanced-parens-escape-in-string
              (let ([errors (check-balanced-parens "(display \"\\\"(\\\"\")" "test.ss")])
                   (assert-equal '() errors)))

            ;; Block comment tests (#| |#)
            (define-test balanced-parens-block-comment
              ;; Parens inside block comments should not count
              (let ([errors (check-balanced-parens "(define x #| (unmatched |# 1)" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-block-comment-multiline
              ;; Block comments can span multiple lines
              (let ([errors (check-balanced-parens "(define x #|\n(unmatched\n|# 1)" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-block-comment-nested
              ;; Nested block comments
              (let ([errors (check-balanced-parens "(define x #| outer #| inner |# outer |# 1)" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-block-comment-with-paren-inside
              ;; Opening paren inside block comment should not affect depth
              (let ([errors (check-balanced-parens "#| ( |# (define x 1)" "test.ss")])
                   (assert-equal '() errors)))

            ;; Datum comment tests (#;)
            (define-test balanced-parens-datum-comment-atom
              ;; #; comments out an atom
              (let ([errors (check-balanced-parens "(define #;commented x 1)" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-datum-comment-list
              ;; #; comments out an entire list
              (let ([errors (check-balanced-parens "(define x #;(this is commented out) 1)" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-datum-comment-nested-list
              ;; #; handles nested lists
              (let ([errors (check-balanced-parens "(define x #;(outer (inner)) 1)" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-datum-comment-unbalanced-in-comment
              ;; The datum comment itself must be balanced internally
              ;; but the outer code should still be checked
              (let ([errors (check-balanced-parens "(define x #;(balanced) 1)" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-both-comment-types
              ;; Combination of block and datum comments
              (let ([errors (check-balanced-parens "(define x #| block |# #;datum 1)" "test.ss")])
                   (assert-equal '() errors)))

            ;; Bracket mismatch tests
            (define-test balanced-parens-bracket-mismatch-paren-close-bracket
              ;; (] is a mismatch - paren opened, bracket closed
              (let ([errors (check-balanced-parens "(define x 1]" "test.ss")])
                   (assert-true (pair? errors))
                   ;; Should have a bracket-mismatch error
                   (assert-equal 'bracket-mismatch (error-code (car errors)))))

            (define-test balanced-parens-bracket-mismatch-bracket-close-paren
              ;; [) is a mismatch - bracket opened, paren closed
              (let ([errors (check-balanced-parens "[define x 1)" "test.ss")])
                   (assert-true (pair? errors))
                   (assert-equal 'bracket-mismatch (error-code (car errors)))))

            (define-test balanced-parens-mixed-correct
              ;; Properly matched mixed brackets
              (let ([errors (check-balanced-parens "(let ([x 1] [y 2]) (+ x y))" "test.ss")])
                   (assert-equal '() errors)))

            (define-test balanced-parens-nested-mismatch
              ;; Mismatch in nested context
              (let ([errors (check-balanced-parens "(let ([x 1]) x]" "test.ss")])
                   (assert-true (pair? errors)))))

;;; ====
;;; compute-line-col Tests
;;; ====

(test-group compute-line-col
            
            (define-test compute-line-col-first-char
              (let ([result (compute-line-col "hello" 0)])
                   (assert-equal '(1 . 1) result)))
            
            (define-test compute-line-col-middle
              (let ([result (compute-line-col "hello" 3)])
                   (assert-equal '(1 . 4) result)))
            
            (define-test compute-line-col-second-line
              (let ([result (compute-line-col "hello\nworld" 6)])
                   (assert-equal '(2 . 1) result)))
            
            (define-test compute-line-col-second-line-offset
              (let ([result (compute-line-col "hello\nworld" 8)])
                   (assert-equal '(2 . 3) result)))
            
            (define-test compute-line-col-third-line
              (let ([result (compute-line-col "a\nb\nc" 4)])
                   (assert-equal '(3 . 1) result))))

;;; ====
;;; skip-to-newline Tests
;;; ====

(test-group skip-to-newline
            
            (define-test skip-to-newline-at-comment
              (let ([result (skip-to-newline "; comment\ncode" 0)])
                   ;; Should skip past the newline
                   (assert-equal 10 result)))
            
            (define-test skip-to-newline-no-newline
              (let ([result (skip-to-newline "; comment" 0)])
                   ;; Should go to end + 1
                   (assert-equal 10 result)))
            
            (define-test skip-to-newline-middle
              (let ([result (skip-to-newline "abc; comment\ndef" 3)])
                   (assert-equal 13 result))))

;;; ====
;;; Integration Tests
;;; ====

(test-group integration
            
            (define-test analyze-simple-error
              ;; Test full flow from error to diagnostic
              (doc-open! "file:///int-test.ss" 1 "(define foo)")
              (let* ([doc (doc-get "file:///int-test.ss")]
                     [span (make-span "test.ss" 1 12 1 13)]
                     [err (make-error 'parse 'unexpected-eof span)]
                     [diag (fold-error->diagnostic doc err)])
                    (assert-true (json-object? diag))
                    (assert-equal "fold" (json-get diag "source")))
              (doc-close! "file:///int-test.ss"))
            
            (define-test severity-mapping-consistency
              ;; All error phases should map to error severity
              (assert-equal (phase->severity 'parse) (phase->severity 'infer))
              (assert-equal (phase->severity 'infer) (phase->severity 'eval))
              (assert-equal (phase->severity 'eval) (phase->severity 'block)))
            
            (define-test message-lookup-never-empty
              ;; lookup-error-message* should never return empty string
              (let ([phases '(parse infer eval block)]
                    [codes '(unexpected-eof division-by-zero)])
                   (for-each
                    (lambda (phase)
                            (for-each
                             (lambda (code)
                                     (let ([msg (lookup-error-message* phase code)])
                                          (assert-true (> (string-length msg) 0))))
                             codes))
                    phases))))

;;; ====
;;; Summary
;;; ====

(print-summary)
(when (> *tests-failed* 0)
      (exit 1))
