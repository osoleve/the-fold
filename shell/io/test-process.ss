;;; shell/io/test-process.ss — Tests for Process Utilities
;;;
;;; Run with: scheme --script shell/io/test-process.ss

(load "core/testing/test-framework.ss")
(load "shell/io/process.ss")

(display "\n====\n")
(display "Process Utilities Tests\n")
(display "====\n\n")

;;; ====
;;; Shell Escape Tests
;;; ====

(test-group shell-escape-tests

  (define-test test-escape-simple
    (assert-equal "hello" (shell-escape "hello")))

  (define-test test-escape-with-spaces
    (assert-equal "hello world" (shell-escape "hello world")))

  (define-test test-escape-single-quote
    ;; Single quote becomes '\'' (end quote, escaped quote, start quote)
    (assert-equal "don'\\''t" (shell-escape "don't")))

  (define-test test-escape-multiple-quotes
    (assert-equal "it'\\''s a '\\''test'\\''!" (shell-escape "it's a 'test'!")))

  (define-test test-escape-empty
    (assert-equal "" (shell-escape "")))
)

;;; ====
;;; Shell Capture Tests
;;; ====

(test-group shell-capture-tests

  (define-test test-capture-echo
    (assert-equal "hello" (shell-capture "echo hello")))

  (define-test test-capture-multiline
    (let ([output (shell-capture "echo -e 'line1\\nline2'")])
      (assert-true (string-contains? output "line1"))
      (assert-true (string-contains? output "line2"))))

  (define-test test-capture-command-substitution
    (let ([output (shell-capture "echo $(echo nested)")])
      (assert-equal "nested" output)))
)

;;; ====
;;; Shell Capture Result Tests
;;; ====

(test-group shell-capture-result-tests

  (define-test test-result-success
    (let ([result (shell-capture-result "echo ok")])
      (assert-true (process-ok? result))
      (assert-equal "ok" (process-stdout result))
      (assert-equal 0 (process-exit-code result))))

  (define-test test-result-failure
    (let ([result (shell-capture-result "exit 42")])
      (assert-false (process-ok? result))
      (assert-equal 42 (process-exit-code result))))

  (define-test test-result-output-with-failure
    (let ([result (shell-capture-result "echo output && exit 5")])
      (assert-false (process-ok? result))
      (assert-equal "output" (process-stdout result))
      (assert-equal 5 (process-exit-code result))))

  (define-test test-result-stderr
    (let ([result (shell-capture-result "echo err >&2")])
      (assert-true (process-ok? result))
      (assert-true (string-contains? (process-stderr result) "err"))))

  ;; Edge cases identified by QA
  (define-test test-result-no-trailing-newline
    ;; printf without newline - tests off-by-one fix
    (let ([result (shell-capture-result "printf 'foo'")])
      (assert-true (process-ok? result))
      (assert-equal "foo" (process-stdout result))))

  (define-test test-result-empty-output
    ;; Command with no output
    (let ([result (shell-capture-result "true")])
      (assert-true (process-ok? result))
      (assert-equal "" (process-stdout result))))

  (define-test test-result-stderr-only
    ;; Command that only writes to stderr
    (let ([result (shell-capture-result "echo error >&2 && exit 0")])
      (assert-true (process-ok? result))
      (assert-equal "" (process-stdout result))
      (assert-true (string-contains? (process-stderr result) "error"))))

  (define-test test-result-marker-in-output
    ;; Output containing the marker string (should not confuse parser)
    ;; The marker is at the END, so earlier occurrences shouldn't matter
    (let ([result (shell-capture-result "echo 'text with __EXIT__ in it'")])
      (assert-true (process-ok? result))
      ;; Should capture the full output including the fake marker
      (assert-true (string-contains? (process-stdout result) "__EXIT__"))))
)

;;; ====
;;; Shell Run Tests
;;; ====

(test-group shell-run-tests

  (define-test test-run-success
    (assert-true (shell-run "true")))

  (define-test test-run-failure
    (assert-false (shell-run "false")))

  (define-test test-run-test-file-exists
    (assert-true (shell-run "test -f /etc/passwd")))

  (define-test test-run-test-file-missing
    (assert-false (shell-run "test -f /nonexistent/file")))
)

;;; ====
;;; Summary
;;; ====

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
  (exit 1))

(display "\n✓ All tests passed!\n")
