;;; boundary/tests/test-error-context.ss — Tests for error-context.ss
;;; Validates format-error-detail, capture-error!, and with-context.
;;;
;;; Note: error-context.ss must load BEFORE test-framework.ss because both
;;; define `current-context` — the framework's parameter version must win.

(load "core/base/prelude.ss")
(define *quiet* #t)  ; suppress error-context startup banner
(load "boundary/repl/error-context.ss")
(load "core/testing/test-framework.ss")

(test-group format-error-detail
  (define-test "message with matching irritants renders formatted"
    (let ([detail
           (guard (e [#t (format-error-detail e)])
             (error 'test "expected ~s but got ~s" 'foo 'bar))])
      (assert-true (string-contains? detail "expected foo but got bar"))))

  (define-test "message with single irritant renders formatted"
    (let ([detail
           (guard (e [#t (format-error-detail e)])
             (error 'test "value is ~a" 42))])
      (assert-true (string-contains? detail "value is 42"))))

  (define-test "message with no irritants shows plain text"
    (let ([detail
           (guard (e [#t (format-error-detail e)])
             (error 'test "plain error message"))])
      (assert-true (string-contains? detail "plain error message"))))

  (define-test "message with mismatched arity falls back to template"
    ;; More format directives than irritants — guard catches format error
    (let ([detail
           (guard (e [#t (format-error-detail e)])
             (error 'test "~s and ~s and ~s" 'only-one))])
      ;; Should contain the raw template (fallback), not crash
      (assert-true (string? detail))))

  (define-test "who field is displayed"
    (let ([detail
           (guard (e [#t (format-error-detail e)])
             (error 'my-function "something broke"))])
      (assert-true (string-contains? detail "my-function"))))

  (define-test "irritants field is displayed"
    (let ([detail
           (guard (e [#t (format-error-detail e)])
             (error 'test "msg: ~s" '(a b c)))])
      (assert-true (string-contains? detail "Irritants:")))))

(test-group capture-and-last-error
  (define-test "capture-error! stores condition"
    (set! *last-error* #f)
    (guard (e [#t (capture-error! e)])
      (error 'test "captured"))
    (assert-true (pair? *last-error*)))

  (define-test "clear-error! clears stored error"
    (set! *last-error* (list 'dummy '() 0))
    ;; clear-error! prints, so capture output
    (with-output-to-string clear-error!)
    (assert-false *last-error*)))

(test-group with-context-macro
  (define-test "with-context returns value on success"
    (set! *last-error* #f)
    (let ([result (with-context "test-ctx" (+ 1 2))])
      (assert-equal 3 result)))

  (define-test "with-context captures error with context label"
    (set! *last-error* #f)
    (guard (e [#t (void)])
      (with-context "my-label"
        (error 'test "context test")))
    ;; Error was captured with context
    (assert-true (pair? *last-error*))))

(run-all-tests-and-exit)
