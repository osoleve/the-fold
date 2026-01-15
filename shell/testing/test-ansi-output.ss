;;; shell/testing/test-ansi-output.ss — Test the ANSI test output
;;;
;;; Run with: scheme --script shell/testing/test-ansi-output.ss

(load "core/testing/test-framework.ss")
(load "shell/testing/ansi-test-output.ss")

;;; Quick demo tests
(define-test "passing test"
  (assert-equal 4 (+ 2 2)))

(define-test "another pass"
  (assert-true #t))

(define-test "string test"
  (assert-equal "hello" "hello"))

;; Uncomment to see failure output:
;; (define-test "intentional failure"
;;   (assert-equal 5 (+ 2 2)))

(run-all-tests)
