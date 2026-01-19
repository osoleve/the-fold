;;; boundary/testing/ansi-test-output.ss — Colored Test Output
;;;
;;; Enhances the test framework with ANSI colored output.
;;; Load this file AFTER test-framework.ss to enable colors.
;;;
;;; Usage:
;;;   (load "core/testing/test-framework.ss")
;;;   (load "boundary/testing/ansi-test-output.ss")
;;;   ;; Now all test output will be colored
;;;
;;; This is Shell code: handles display formatting.

(load "boundary/ui/ansi.ss")

;;; ====
;;; ANSI Output Helpers
;;; ====

;;; pass-indicator : String
;;; Green checkmark for passing tests.
(define pass-indicator
  (string-append (style->ansi (style-fg color-green)) "✓" ansi-reset))

;;; fail-indicator : String
;;; Red X for failing tests.
(define fail-indicator
  (string-append (style->ansi (style-fg color-red)) "✗" ansi-reset))

;;; colored-text : Style × String → String
(define (colored-text style str)
  (string-append (style->ansi style) str ansi-reset))

;;; ====
;;; Override Test Framework Output
;;; ====

;;; We redefine key functions from test-framework.ss with colors.
;;; This works because Scheme allows redefinition at the top level.

;;; name->string : Any → String
;;; Convert a test name to string (handles both symbols and strings).
(define (name->string name)
  (if (symbol? name)
      (symbol->string name)
      (format "~a" name)))

;;; run-test : Symbol × (Unit → Unit) → Unit
;;; Run a single test with colored output.
(set! run-test
  (lambda (name test-thunk)
    (set-name! name)
    (let ([initial-fail-count (ctx-failed)])
      (guard (exn [else
                   (inc-failed!)
                   (display "    ")
                   (display fail-indicator)
                   (display " ")
                   (display (colored-text (style-fg color-white) (name->string name)))
                   (display (colored-text style-dim " — EXCEPTION: "))
                   (display (colored-text (style-fg color-red)
                             (if (condition? exn)
                                 (condition-message exn)
                                 (format "~a" exn))))
                   (newline)])
             (test-thunk))
      ;; If no new failures, the test passed
      (when (= initial-fail-count (ctx-failed))
            (inc-passed!)
            (display "    ")
            (display pass-indicator)
            (display " ")
            (display (name->string name))
            (newline)))))

;;; run-group : Symbol → Unit
;;; Run all tests in a group with colored header.
(set! run-group
  (lambda (group-name)
    (let ([group-entry (assq group-name *test-registry*)])
      (if group-entry
          (let ([tests (reverse (cdr group-entry))])
            (display "  ")
            (display (colored-text (make-style color-cyan #f #t #f #f #f)
                       (string-append "Group: " (symbol->string group-name))))
            (newline)
            (for-each (lambda (test)
                        (run-test (car test) (cdr test)))
                      tests))
          (begin
            (display "  ")
            (display (colored-text (style-fg color-yellow) "Warning: "))
            (display "No tests found for group ")
            (display group-name)
            (newline))))))

;;; print-summary : Unit → Unit
;;; Print test summary with colors.
(set! print-summary
  (lambda ()
    (newline)
    (display (colored-text (make-style color-blue #f #t #f #f #f)
               "╔══════════════════════════════════════════════════════════╗"))
    (newline)
    (display (colored-text (make-style color-blue #f #t #f #f #f)
               "║                    TEST SUMMARY                          ║"))
    (newline)
    (display (colored-text (make-style color-blue #f #t #f #f #f)
               "╚══════════════════════════════════════════════════════════╝"))
    (newline)
    (display "  Total:  ")
    (display (ctx-run))
    (display " tests")
    (newline)
    (display "  Passed: ")
    (display (colored-text (style-fg color-green) (number->string (ctx-passed))))
    (newline)
    (display "  Failed: ")
    (display (colored-text (style-fg (if (> (ctx-failed) 0) color-red color-green))
               (number->string (ctx-failed))))
    (newline)
    (newline)
    (if (= (ctx-failed) 0)
        (display (colored-text (make-style color-green #f #t #f #f #f)
                   "✓ All tests passed!"))
        (display (colored-text (make-style color-red #f #t #f #f #f)
                   "✗ Some tests failed!")))
    (newline)))

;;; ====
;;; Override Assertion Output
;;; ====

;;; assert-equal with colored output
(set! assert-equal
  (lambda (expected actual)
    (unless (equal? expected actual)
      (inc-failed!)
      (display "    ")
      (display fail-indicator)
      (display " ")
      (display (ctx-name))
      (newline)
      (display (colored-text style-dim "      Expected: "))
      (write expected)
      (newline)
      (display (colored-text style-dim "      Got:      "))
      (display (colored-text (style-fg color-yellow) (format "~s" actual)))
      (newline))))

;;; assert-true with colored output
(set! assert-true
  (lambda (expr)
    (unless (eq? #t expr)
      (inc-failed!)
      (display "    ")
      (display fail-indicator)
      (display " ")
      (display (ctx-name))
      (newline)
      (display (colored-text style-dim "      Expected: "))
      (display "#t")
      (newline)
      (display (colored-text style-dim "      Got:      "))
      (display (colored-text (style-fg color-yellow) (format "~s" expr)))
      (newline))))

;;; assert-false with colored output
(set! assert-false
  (lambda (expr)
    (unless (eq? #f expr)
      (inc-failed!)
      (display "    ")
      (display fail-indicator)
      (display " ")
      (display (ctx-name))
      (newline)
      (display (colored-text style-dim "      Expected: "))
      (display "#f")
      (newline)
      (display (colored-text style-dim "      Got:      "))
      (display (colored-text (style-fg color-yellow) (format "~s" expr)))
      (newline))))

;;; assert-error with colored output
(set! assert-error
  (lambda (thunk)
    (guard (exn [else #t])
      (thunk)
      (inc-failed!)
      (display "    ")
      (display fail-indicator)
      (display " ")
      (display (ctx-name))
      (newline)
      (display (colored-text (style-fg color-yellow)
                 "      Expected error, but none was raised"))
      (newline))))

;;; assert-ok with colored output
(set! assert-ok
  (lambda (result)
    (unless (ok? result)
      (inc-failed!)
      (display "    ")
      (display fail-indicator)
      (display " ")
      (display (ctx-name))
      (newline)
      (display (colored-text style-dim "      Expected (ok ...), got: "))
      (display (colored-text (style-fg color-yellow) (format "~s" result)))
      (newline))))

;;; Legacy test function with colors
(set! test
  (lambda (name expected actual)
    (set-name! name)
    (inc-run!)
    (if (equal? expected actual)
        (begin
          (inc-passed!)
          (display "  ")
          (display pass-indicator)
          (display " ")
          (display name)
          (newline))
        (begin
          (inc-failed!)
          (display "  ")
          (display fail-indicator)
          (display " ")
          (display name)
          (display (colored-text style-dim " — expected "))
          (write expected)
          (display (colored-text style-dim ", got "))
          (display (colored-text (style-fg color-yellow) (format "~s" actual)))
          (newline)))))

;;; ====
;;; Module Loaded Message
;;; ====

(display (colored-text (style-fg color-cyan) "ANSI test output enabled."))
(newline)
