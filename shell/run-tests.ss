;;; shell/run-tests.ss — Test Runner for Shell Modules
;;;
;;; Runs all shell/ tests.
;;; Must be invoked from ccverse root directory.
;;;
;;; Usage:
;;;   scheme --script shell/run-tests.ss   (from ccverse root)
;;;
;;; Dependencies: Requires core/ modules to be loadable

;;; ============================================================
;;; Setup: Ensure we can find core/ and shell/ modules
;;; ============================================================

;;; Add core to source-directories for shell dependencies
(source-directories (cons "core" (source-directories)))

;;; ============================================================
;;; Test Runner
;;; ============================================================

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(define (run-test-file filename)
  (display (string-append "
=== Running " filename " ===
"))
  (set! test-count (+ test-count 1))
  (guard (exn [else
               (set! fail-count (+ fail-count 1))
               (display "FAILED: ")
               (display (format-condition exn))
               (newline)])
         (load (string-append "shell/" filename))
         (set! pass-count (+ pass-count 1))))

;;; ============================================================
;;; Run Shell Tests
;;; ============================================================

(display "╔══════════════════════════════════════════════════════════╗
")
(display "║           The Fold — Shell Test Suite                    ║
")
(display "╚══════════════════════════════════════════════════════════╝
")
(display (string-append "Working directory: " (current-directory) "
"))

;;; Validation
(run-test-file "test-validate.ss")

;;; Indexing
(run-test-file "test-block-index.ss")

;;; Note: Additional tests excluded from automated run:
;;;   - test-block-navigator.ss (memory-intensive)
;;;   - test-text.ss, test-fs.ss, test-color.ss, test-layout.ss (require setup)

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(display "╔══════════════════════════════════════════════════════════╗
")
(display "║                    TEST SUMMARY                          ║
")
(display "╚══════════════════════════════════════════════════════════╝
")
(display (string-append "  Total:  " (number->string test-count) " test files
"))
(display (string-append "  Passed: " (number->string pass-count) "
"))
(display (string-append "  Failed: " (number->string fail-count) "
"))

(if (= fail-count 0)
    (display "
✓ All shell tests passed!
")
    (begin
     (display "
✗ Some tests failed!
")
     (exit 1)))
