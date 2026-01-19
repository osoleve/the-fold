;;; boundary/tools/test-ast-format.ss — Tests for AST-aware formatter
;;;
;;; Tests for style profiles, special form handling, and output correctness.

(load "core/testing/test-framework.ss")
(load "boundary/tools/ast-format.ss")

;;; Helper for checking substring
(define (string-contains? str substr)
  (let ([slen (string-length str)]
        [plen (string-length substr)])
       (if (< slen plen)
           #f
           (let loop ([i 0])
                (cond
                 [(> (+ i plen) slen) #f]
                 [(string=? (substring str i (+ i plen)) substr) #t]
                 [else (loop (+ i 1))])))))

(test-group ast-format
            
            ;; Profile tests
            (define-test profile-creation
              (let ([p (make-style-profile 100 4 50 #t #f #t)])
                   (assert-equal 100 (style-width p))
                   (assert-equal 4 (style-indent p))
                   (assert-equal 50 (style-break-threshold p))
                   (assert-equal #t (style-one-per-line p))
                   (assert-equal #f (style-align-bindings p))
                   (assert-equal #t (style-collapse-simple p))))
            
            (define-test predefined-profiles-exist
              (assert-true (style-profile? *profile-compact*))
              (assert-true (style-profile? *profile-verbose*))
              (assert-true (style-profile? *profile-diff-friendly*))
              (assert-true (style-profile? *profile-canonical*)))
            
            (define-test profile-lookup
              (assert-equal *profile-compact* (get-profile 'compact))
              (assert-equal *profile-verbose* (get-profile 'verbose))
              (assert-equal *profile-diff-friendly* (get-profile 'diff-friendly))
              (assert-equal *profile-diff-friendly* (get-profile 'diff))
              (assert-equal *profile-canonical* (get-profile 'canonical))
              (assert-equal *profile-compact* (get-profile 'unknown)))  ; default
            
            ;; Basic formatting tests
            (define-test format-atom
              (assert-equal "foo" (ast-format-expr 'foo))
              (assert-equal "42" (ast-format-expr 42))
              (assert-equal "#t" (ast-format-expr #t))
              (assert-equal "#f" (ast-format-expr #f)))
            
            (define-test format-empty-list
              (assert-equal "()" (ast-format-expr '())))
            
            (define-test format-simple-list
              (assert-equal "(+ 1 2)" (ast-format-expr '(+ 1 2))))
            
            (define-test format-nested-list
              (assert-equal "(+ (* 2 3) 4)" (ast-format-expr '(+ (* 2 3) 4))))
            
            ;; Special form tests
            (define-test format-define
              (let ([result (ast-format-expr '(define x 1))])
                   (assert-true (string? result))
                   (assert-true (> (string-length result) 0))))
            
            (define-test format-lambda
              (let ([result (ast-format-expr '(lambda (x) x))])
                   (assert-true (string? result))))
            
            (define-test format-let
              (let ([result (ast-format-expr '(let ((x 1)) x))])
                   (assert-true (string? result))))
            
            (define-test format-if
              (let ([result (ast-format-expr '(if #t 1 2))])
                   (assert-true (string? result))))
            
            ;; Diff-friendly mode tests
            (define-test diff-friendly-one-per-line
              (let ([result (ast-format-expr '(a b c d e) *profile-diff-friendly*)])
                   ;; Should have newlines
                   (assert-true (string-contains? result "\n"))))
            
            ;; String formatting tests
            (define-test format-string-round-trip
              (let* ([code "(define (f x) x)"]
                     [formatted (ast-format-string code)])
                    (assert-true (string? formatted))
                    (assert-true (> (string-length formatted) 0))))
            
            (define-test format-preserves-semantics
              ;; Format and read back - should be equivalent
              (let* ([code "(+ 1 2 3)"]
                     [formatted (ast-format-string code)]
                     [original-expr (car (read-all-from-string code))]
                     [formatted-expr (car (read-all-from-string formatted))])
                    (assert-equal original-expr formatted-expr)))
            
            ;; Utility tests
            (define-test string-split-lines-test
              (let ([lines (string-split-lines "a\nb\nc")])
                   (assert-equal 3 (length lines))
                   (assert-equal "a" (car lines))
                   (assert-equal "b" (cadr lines))
                   (assert-equal "c" (caddr lines))))
            
            (define-test take-n-test
              (assert-equal '(1 2 3) (take-n '(1 2 3 4 5) 3))
              (assert-equal '(1 2) (take-n '(1 2) 5))
              (assert-equal '() (take-n '() 3)))
            
            (define-test drop-n-test
              (assert-equal '(4 5) (drop-n '(1 2 3 4 5) 3))
              (assert-equal '() (drop-n '(1 2) 5))
              (assert-equal '() (drop-n '() 3)))
            
            (define-test simple-form-detection
              (assert-true (simple-form? '(+ 1 2)))
              (assert-true (simple-form? '()))
              (assert-true (simple-form? 'x))
              ;; Complex forms (too many elements)
              (assert-false (simple-form? '(a b c d e f g h i j))))
            
            ;; Edge case tests (from Gemini QA review)
            (define-test dotted-pair-simple
              ;; (a . b) should format without crashing
              (let ([result (ast-format-expr '(a . b))])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "."))))
            
            (define-test dotted-pair-multiple
              ;; (a b . c) should format correctly
              (let ([result (ast-format-expr '(a b . c))])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "."))))
            
            (define-test quote-shorthand
              ;; 'x should remain 'x, not (quote x)
              (let ([result (ast-format-expr ''x)])
                   (assert-equal "'x" result)))
            
            (define-test quasiquote-shorthand
              ;; `x should remain `x
              (let ([result (ast-format-expr '`x)])
                   (assert-equal "`x" result)))
            
            (define-test unquote-shorthand
              ;; ,x should remain ,x
              (let ([result (ast-format-expr ',x)])
                   (assert-equal ",x" result)))
            
            (define-test unquote-splicing-shorthand
              ;; ,@x should remain ,@x
              (let ([result (ast-format-expr ',@x)])
                   (assert-equal ",@x" result)))
            
            (define-test nested-quote
              ;; '(a b c) should use shorthand
              (let ([result (ast-format-expr ''(a b c))])
                   (assert-true (string? result))
                   (assert-true (> (string-length result) 0))
                   ;; Should start with '
                   (assert-equal #\' (string-ref result 0))))
            
            (define-test proper-list-check
              (assert-true (proper-list? '()))
              (assert-true (proper-list? '(a)))
              (assert-true (proper-list? '(a b c)))
              (assert-false (proper-list? '(a . b)))
              (assert-false (proper-list? '(a b . c)))))

(display "\n=== AST Format Tests Complete ===\n")
(display (format "Passed: ~a  Failed: ~a  Total: ~a\n"
                 *tests-passed* *tests-failed* *tests-run*))
(when (> *tests-failed* 0)
      (exit 1))
