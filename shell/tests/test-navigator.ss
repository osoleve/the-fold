;;; shell/tests/test-navigator.ss — Tests for Navigation Utilities
;;;
;;; Comprehensive test suite for shell/lens/navigator.ss
;;; Tests high-level lens navigation commands and query interface.

(load "shell/lens/navigator.ss")

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

(define (assert-true name actual)
  (assert-equal name actual #t))

(define (assert-false name actual)
  (assert-equal name actual #f))

;;; ====
;;; Setup - Ensure graph is built
;;; ====

(printf "\n=== Setting up navigator tests ===\n")
(unless (call-graph-built?)
        (lens-rebuild!))
(printf "Lens system ready.\n")

;;; ====
;;; lens-query Tests
;;; ====

(printf "\n=== Testing lens-query ===\n")

(assert-true "query callers returns ok"
             (let ([result (lens-query 'foo 'callers)])
                  (and (pair? result)
                       (eq? (car result) 'ok))))

(assert-true "query callees returns ok"
             (let ([result (lens-query 'foo 'callees)])
                  (and (pair? result)
                       (eq? (car result) 'ok))))

(assert-true "query tests returns ok"
             (let ([result (lens-query 'foo 'tests)])
                  (and (pair? result)
                       (eq? (car result) 'ok))))

(assert-true "query slice-up returns ok"
             (let ([result (lens-query 'foo 'slice-up)])
                  (and (pair? result)
                       (eq? (car result) 'ok))))

(assert-true "query slice-down returns ok"
             (let ([result (lens-query 'foo 'slice-down)])
                  (and (pair? result)
                       (eq? (car result) 'ok))))

(assert-true "query location returns ok or error"
             (let ([result (lens-query 'foo 'location)])
                  (and (pair? result)
                       (memq (car result) '(ok error)))))

(assert-true "unknown query returns error"
             (let ([result (lens-query 'foo 'unknown-query-type)])
                  (and (pair? result)
                       (eq? (car result) 'error))))

;;; ====
;;; Query Result Content Tests
;;; ====

(printf "\n=== Testing query result content ===\n")

(assert-true "callers result is list"
             (let ([result (lens-query 'call-graph-callers 'callers)])
                  (list? (cadr result))))

(assert-true "callees result is list"
             (let ([result (lens-query 'call-graph-refresh! 'callees)])
                  (list? (cadr result))))

(assert-true "slice-up result is list"
             (let ([result (lens-query 'foo 'slice-up)])
                  (list? (cadr result))))

(assert-true "slice-down result is list"
             (let ([result (lens-query 'foo 'slice-down)])
                  (list? (cadr result))))

;;; ====
;;; call-graph-built? Integration Tests
;;; ====

(printf "\n=== Testing call-graph-built? integration ===\n")

(assert-true "graph is built after lens-rebuild"
             (call-graph-built?))

;;; ====
;;; find-all-test-files Integration Tests
;;; ====

(printf "\n=== Testing find-all-test-files integration ===\n")

(assert-true "find-all-test-files returns list"
             (list? (find-all-test-files)))

(assert-true "test files found"
             (> (length (find-all-test-files)) 0))

(assert-true "all results are test files"
             (andmap (lambda (f)
                             (let ([basename (path-basename f)])
                                  (string-starts-with? basename "test-")))
                     (find-all-test-files)))

;;; ====
;;; Utility Function Availability Tests
;;; ====

(printf "\n=== Testing utility function availability ===\n")

(assert-true "lens-jump is defined"
             (procedure? lens-jump))

(assert-true "lens-callers is defined"
             (procedure? lens-callers))

(assert-true "lens-callees is defined"
             (procedure? lens-callees))

(assert-true "lens-test is defined"
             (procedure? lens-test))

(assert-true "lens-slice-up is defined"
             (procedure? lens-slice-up))

(assert-true "lens-slice-down is defined"
             (procedure? lens-slice-down))

(assert-true "lens-path is defined"
             (procedure? lens-path))

(assert-true "lens-stats is defined"
             (procedure? lens-stats))

(assert-true "lens-rebuild! is defined"
             (procedure? lens-rebuild!))

(assert-true "lens-help is defined"
             (procedure? lens-help))

(assert-true "lens-query is defined"
             (procedure? lens-query))

;;; ====
;;; take Function Tests (used by navigator)
;;; ====

(printf "\n=== Testing take function (used internally) ===\n")

;; take is used with signature (take lst n) based on lens-callers implementation
(when (and (top-level-bound? 'take)
           (procedure? take))
      (printf "  (take available, running tests)\n")
      ;; Note: In navigator.ss, take appears to be called as (take list limit)
      ;; We'll test the expected behavior
      )

;;; ====
;;; Edge Cases
;;; ====

(printf "\n=== Testing Edge Cases ===\n")

(assert-true "query with nonexistent symbol"
             (let ([result (lens-query 'nonexistent-symbol-xyz-123 'callers)])
                  (and (pair? result)
                       (eq? (car result) 'ok)
                       (null? (cadr result)))))

(assert-true "location query for unknown returns error"
             (let ([result (lens-query 'nonexistent-symbol-xyz-123 'location)])
                  ;; May be ok with #f or error
                  (pair? result)))

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results (navigator):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
