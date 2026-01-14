;;; shell/test-git.ss — Tests for Git Operations
;;;
;;; Validates git utility functions and tier enforcement logic.

;;; Mock dependencies
(define (read-session) #f)
(define (msg . args) #f)
(define (system . args) 0)

(load "core/base/prelude.ss")
(load "shell/git/git.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

;;; ====
;;; Truncate Commit Title Tests
;;; ====

(printf "\n=== Testing truncate-commit-title ===\n")

(assert-equal "short title"
              (truncate-commit-title "Fix bug")
              "Fix bug")

(assert-equal "exact length title"
              (truncate-commit-title (make-string 50 #\a))
              (make-string 50 #\a))

(assert-equal "long title truncation"
              (truncate-commit-title "This is a very long commit message that should definitely be truncated because it exceeds fifty characters")
              "This is a very long commit message that should ...")

(assert-equal "multi-line title"
              (truncate-commit-title "First line\nSecond line")
              "First line")

;;; ====
;;; Tier Enforcement Tests
;;; ====

(printf "\n=== Testing tier enforcement ===\n")

;; Mock read-session for Opus
(set! read-session (lambda () '((tier . shepherd) (name . opus-bot))))
(assert-equal "shepherd? for opus" (shepherd?) #t)

;; Mock read-session for Sonnet
(set! read-session (lambda () '((tier . builder) (name . sonnet-bot))))
(assert-equal "shepherd? for sonnet" (shepherd?) #f)

;; Mock read-session for no session
(set! read-session (lambda () #f))
(assert-equal "shepherd? for no session" (shepherd?) #f)

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
