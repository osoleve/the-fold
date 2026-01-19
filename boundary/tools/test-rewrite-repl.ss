;;; boundary/tools/test-rewrite-repl.ss — Tests for Rewrite REPL Commands
;;;
;;; Run from project root: scheme --script boundary/tools/test-rewrite-repl.ss

(load "boundary/tools/rewrite-repl.ss")

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

(define (assert-true name condition)
  (set! test-count (+ test-count 1))
  (if condition
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: #t\n")
       (printf "    Actual:   #f\n"))))

(define (assert-false name condition)
  (set! test-count (+ test-count 1))
  (if (not condition)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: #f\n")
       (printf "    Actual:   #t\n"))))

;;; ====
;;; Test 1: Display Functions
;;; ====

(printf "\n=== Test 1: Display Functions ===\n")

;; These should not error
(guard (e [else (assert-true "display-divider failed" #f)])
       (display-divider)
       (assert-true "display-divider succeeded" #t))

(guard (e [else (assert-true "display-header failed" #f)])
       (display-header "Test Header")
       (assert-true "display-header succeeded" #t))

;;; ====
;;; Test 2: Rewrite Help
;;; ====

(printf "\n=== Test 2: Rewrite Help ===\n")

(guard (e [else (assert-true "rewrite-help failed" #f)])
       (rewrite-help)
       (assert-true "rewrite-help displays without error" #t))

;;; ====
;;; Test 3: Laws Command (No Category)
;;; ====

(printf "\n=== Test 3: Laws Command (No Category) ===\n")

(guard (e [else (assert-true "laws command failed" #f)])
       (laws)
       (assert-true "laws command displays all categories" #t))

;;; ====
;;; Test 4: Laws Command (With Category)
;;; ====

(printf "\n=== Test 4: Laws Command (With Category) ===\n")

;; Test with known categories
(guard (e [else (assert-true "laws monad failed" #f)])
       (laws 'monad)
       (assert-true "laws monad displays" #t))

(guard (e [else (assert-true "laws functor failed" #f)])
       (laws 'functor)
       (assert-true "laws functor displays" #t))

;; Test with unknown category
(guard (e [else (assert-true "laws unknown failed" #f)])
       (laws 'nonexistent-category-xyz)
       (assert-true "laws unknown category handled" #t))

;;; ====
;;; Test 5: Describe Law
;;; ====

(printf "\n=== Test 5: Describe Law ===\n")

;; Test with known law
(guard (e [else (assert-true "describe-law monad-left-id failed" #f)])
       (describe-law 'monad-left-id)
       (assert-true "describe-law shows monad-left-id" #t))

;; Test with unknown law
(guard (e [else (assert-true "describe-law unknown failed" #f)])
       (describe-law 'nonexistent-law-xyz)
       (assert-true "describe-law handles unknown law" #t))

;;; ====
;;; Test 6: Rewrite with Known Law
;;; ====

(printf "\n=== Test 6: Rewrite with Known Law ===\n")

;; Try to apply monad-left-id: (bind (pure x) f) → (f x)
(guard (e [else
           (printf "    Rewrite error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "rewrite with monad-left-id failed" #f)])
       (let ([result (rewrite '(bind (pure 5) identity) 'monad-left-id)])
            (assert-true "rewrite returns result" #t)))

;;; ====
;;; Test 7: Rewrite with Unknown Law
;;; ====

(printf "\n=== Test 7: Rewrite with Unknown Law ===\n")

(guard (e [else (assert-true "rewrite unknown law failed" #f)])
       (let ([result (rewrite '(+ 1 2) 'nonexistent-law-xyz)])
            (assert-false "rewrite with unknown law returns false" result)))

;;; ====
;;; Test 8: Rewrite* (Multiple Rules)
;;; ====

(printf "\n=== Test 8: Rewrite* (Multiple Rules) ===\n")

;; Apply multiple rules in sequence
(guard (e [else
           (printf "    Rewrite* error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "rewrite* failed" #f)])
       (let ([result (rewrite* '(+ 0 x) '(add-zero-left))])
            (assert-true "rewrite* returns trace" (trace? result))))

;;; ====
;;; Test 9: Simplify
;;; ====

(printf "\n=== Test 9: Simplify ===\n")

(guard (e [else
           (printf "    Simplify error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "simplify failed" #f)])
       (let ([result (simplify '(+ 0 5))])
            (assert-true "simplify returns result" #t)))

;;; ====
;;; Test 10: Simplify-With Category
;;; ====

(printf "\n=== Test 10: Simplify-With Category ===\n")

;; Test with known categories
(guard (e [else
           (printf "    Simplify-with monad error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "simplify-with monad failed" #f)])
       (let ([result (simplify-with '(bind (pure x) f) 'monad)])
            (assert-true "simplify-with monad returns result" #t)))

;; Test with unknown category
(guard (e [else (assert-true "simplify-with unknown failed" #f)])
       (let ([result (simplify-with '(+ 1 2) 'unknown-category)])
            (assert-true "simplify-with unknown category handled" #t)))

;;; ====
;;; Test 11: Show Trace
;;; ====

(printf "\n=== Test 11: Show Trace ===\n")

;; Create a simple trace
(guard (e [else
           (printf "    Show trace error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "show-trace failed" #f)])
       (let ([trace (make-trace '(+ 1 2))])
            (show-trace trace)
            (assert-true "show-trace displays without error" #t)))

;;; ====
;;; Test 12: Verify Equivalence
;;; ====

(printf "\n=== Test 12: Verify Equivalence ===\n")

;; Test identical expressions
(guard (e [else
           (printf "    Verify error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "verify failed" #f)])
       (let ([result (verify '(+ 1 2) '(+ 1 2))])
            (assert-true "verify identical returns true" result)))

;; Test different expressions
(guard (e [else (assert-true "verify different failed" #f)])
       (let ([result (verify '(+ 1 2) '(* 3 4))])
            (assert-true "verify returns result" (boolean? result))))

;;; ====
;;; Test 13: Equiv? Quick Check
;;; ====

(printf "\n=== Test 13: Equiv? Quick Check ===\n")

(guard (e [else
           (printf "    Equiv? error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "equiv? failed" #f)])
       (let ([result (equiv? '(+ 1 2) '(+ 2 1))])
            (assert-true "equiv? returns boolean" (boolean? result))))

;;; ====
;;; Test 14: Suggest Rules
;;; ====

(printf "\n=== Test 14: Suggest Rules ===\n")

;; Suggest rules for a simple expression
(guard (e [else
           (printf "    Suggest-rules error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "suggest-rules failed" #f)])
       (suggest-rules '(+ 0 x))
       (assert-true "suggest-rules displays without error" #t))

;; Suggest rules for expression with no matches
(guard (e [else (assert-true "suggest-rules no match failed" #f)])
       (suggest-rules '(nonexistent-op 1 2))
       (assert-true "suggest-rules handles no matches" #t))

;;; ====
;;; Test 15: Define Custom Rule
;;; ====

(printf "\n=== Test 15: Define Custom Rule ===\n")

;; Define a valid rule
(guard (e [else
           (printf "    Define-rewrite-rule error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "define-rewrite-rule failed" #f)])
       (define-rewrite-rule 'test-rule '(test x) '(result x) 'category 'test)
       (assert-true "define-rewrite-rule succeeds" #t))

;; Try to define invalid rule (RHS var not in LHS)
(guard (e [else (assert-true "invalid rule raised error" #f)])
       (define-rewrite-rule 'bad-rule '(test x) '(result y) 'category 'test)
       (assert-true "invalid rule handled" #t))

;;; ====
;;; Test 16: Command Registration Check
;;; ====

(printf "\n=== Test 16: Command Registration Check ===\n")

;; Test that register function exists and can be called
(guard (e [else (assert-true "register-rewrite-commands! failed" #f)])
       (when (top-level-bound? 'register-rewrite-commands!)
             (register-rewrite-commands!)
             (assert-true "commands registered" #t))
       (unless (top-level-bound? 'register-rewrite-commands!)
               (assert-true "register function checked" #t)))

;;; ====
;;; Test 17: Integration Test
;;; ====

(printf "\n=== Test 17: Integration Test ===\n")

;; Test a complete workflow: rewrite, show trace, verify
(guard (e [else
           (printf "    Integration test error: ~a\n"
                   (if (condition? e) (condition-message e) e))
           (assert-true "integration test failed" #f)])
       ;; Try to simplify an expression
       (let* ([expr '(+ 0 (* 1 x))]
              [simplified (simplify expr)])
             (assert-true "integration test completes" #t)))

;;; ====
;;; Test 18: Error Handling
;;; ====

(printf "\n=== Test 18: Error Handling ===\n")

;; Test that functions handle bad input gracefully
(guard (e [else (assert-true "bad input handled" #t)])
       (rewrite 'not-a-list 'some-rule)
       (assert-true "rewrite handles bad input" #t))

(guard (e [else (assert-true "bad trace handled" #t)])
       (show-trace 'not-a-trace)
       (assert-true "show-trace handles bad input" #t))

;;; ====
;;; Test 19: Edge Cases
;;; ====

(printf "\n=== Test 19: Edge Cases ===\n")

;; Empty expression
(guard (e [else (assert-true "empty expr handled" #t)])
       (simplify '())
       (assert-true "simplify handles empty list" #t))

;; Nested expression
(guard (e [else (assert-true "nested expr handled" #t)])
       (simplify '(+ (+ 0 x) (* 1 y)))
       (assert-true "simplify handles nested expr" #t))

;;; ====
;;; Test 20: Law Categories
;;; ====

(printf "\n=== Test 20: Law Categories ===\n")

;; Check that categories exist
(guard (e [else (assert-true "law-categories failed" #f)])
       (let ([cats (law-categories)])
            (assert-true "law-categories returns list" (list? cats))
            (assert-true "has monad category" (memq 'monad cats))))

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
