(doc 'section 'mock-dependencies)
(define (system . args) 0)
(define (open-output-string) "")
(define (get-output-string . args) "")
(define (confirm . args) #t)
(define (current-time . args) (list 0 0))
(define (time-second . args) 0)

(load "boundary/git/git-workflow.ss")

(doc 'module 'test-git-workflow)
(doc 'description "Tests for Git Workflow Helpers")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Validates conflict detection and workflow logic")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(doc assert-equal 'description "Test assertion helper")
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

(doc 'section 'test-conflict-detection)

(printf "\n=== Testing has-conflicts? ===\n")

(assert-equal "no conflicts"
              (has-conflicts? "Already up to date.")
              #f)

(assert-equal "has conflicts"
              (has-conflicts? "Auto-merging file.ss\nCONFLICT (content): Merge conflict in file.ss")
              #t)

(assert-equal "non-string result"
              (has-conflicts? #f)
              #f)

(doc 'section 'test-summary)

(printf "\n====\n")
(printf "Test Results:\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

(doc 'note "Return success/failure")
(= fail-count 0)
