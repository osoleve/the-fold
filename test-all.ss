;;; test-all.ss — Full Test Suite Runner for The Fold
;;;
;;; Runs both core/ and shell/ test suites.
;;; Invoke from ccverse root directory.
;;;
;;; Usage:
;;;   scheme --script test-all.ss

;;; ============================================================
;;; Setup
;;; ============================================================

;;; Add core to source-directories
(source-directories (cons "core" (source-directories)))

;;; ============================================================
;;; Test Runner Infrastructure
;;; ============================================================

(define total-files 0)
(define total-passed 0)
(define total-failed 0)

(define (run-test-file base-dir filename)
  (display (string-append "  " filename "... "))
  (set! total-files (+ total-files 1))
  (guard (exn [else
               (set! total-failed (+ total-failed 1))
               (display "FAILED\n")
               (display "    Error: ")
               (display (condition-message exn))
               (newline)])
    (load (string-append base-dir "/" filename))
    (set! total-passed (+ total-passed 1))
    (display "ok\n")))

;;; ============================================================
;;; Main
;;; ============================================================

(display "\n")
(display "╔══════════════════════════════════════════════════════════════╗\n")
(display "║         THE FOLD — COMPLETE TEST SUITE                       ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n")
(display (string-append "\nWorking directory: " (current-directory) "\n\n"))

;;; === Core Tests ===
(display "────────────────────────────────────────────────────────────────\n")
(display "  CORE TESTS (core/)\n")
(display "────────────────────────────────────────────────────────────────\n")

;;; Layer 0: Foundation
(run-test-file "core" "test-prelude.ss")
(run-test-file "core" "test-sha256.ss")

;;; Layer 1: Block System
(run-test-file "core" "test-block.ss")
(run-test-file "core" "test-cas.ss")

;;; Layer 2: Language Core
(run-test-file "core" "test-normalize.ss")
(run-test-file "core" "test-prim.ss")
(run-test-file "core" "test-parse.ss")

;;; Layer 3: Type System
(run-test-file "core" "test-types.ss")
(run-test-file "core" "test-kinds.ss")

;;; Layer 4: Type Inference
(run-test-file "core" "test-infer.ss")
(run-test-file "core" "test-resolve.ss")
(run-test-file "core" "test-annotate.ss")

;;; Layer 5: Evaluation
(run-test-file "core" "test-eval.ss")
(run-test-file "core" "test-typed-eval.ss")

;;; === Shell Tests ===
(display "\n────────────────────────────────────────────────────────────────\n")
(display "  SHELL TESTS (shell/)\n")
(display "────────────────────────────────────────────────────────────────\n")

(run-test-file "shell" "test-validate.ss")
(run-test-file "shell" "test-block-index.ss")

;;; Note: Additional shell tests excluded from automated run:
;;;   - test-block-navigator.ss (memory-intensive)
;;;   - test-text.ss, test-fs.ss, test-color.ss, test-layout.ss (require setup)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n╔══════════════════════════════════════════════════════════════╗\n")
(display "║                      FINAL SUMMARY                           ║\n")
(display "╚══════════════════════════════════════════════════════════════╝\n")
(display (string-append "\n  Total test files: " (number->string total-files) "\n"))
(display (string-append "  Passed:           " (number->string total-passed) "\n"))
(display (string-append "  Failed:           " (number->string total-failed) "\n\n"))

(if (= total-failed 0)
    (begin
      (display "╔══════════════════════════════════════════════════════════════╗\n")
      (display "║              ✓ ALL TESTS PASSED                              ║\n")
      (display "╚══════════════════════════════════════════════════════════════╝\n"))
    (begin
      (display "╔══════════════════════════════════════════════════════════════╗\n")
      (display "║              ✗ SOME TESTS FAILED                             ║\n")
      (display "╚══════════════════════════════════════════════════════════════╝\n")
      (exit 1)))

(newline)
