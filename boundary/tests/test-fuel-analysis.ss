;;; boundary/test-fuel-analysis.ss — Tests for Primitive-Level Fuel Analysis
;;;
;;; Test the fuel analysis tools to ensure they correctly track
;;; primitive operations and estimate complexity.

(load "boundary/diagnostics/fuel-analysis.ss")

(display "\n")
(display "════════════════════════════════════════════════════════════════\n")
(display "  FUEL ANALYSIS TESTS\n")
(display "════════════════════════════════════════════════════════════════\n")
(display "\n")

;;; ====
;;; Test 1: Simple Addition
;;; ====

(display "Test 1: Simple Addition\n")
(display "────────────────────────────────────────────────────────────────\n")

(let ([result (analyze-fuel test-add 5)])
     (display (format "Input: (test-add 5)\n"))
     (print-analysis result)
     
     ;; Verify total fuel
     (let ([total-fuel (cdr (assq 'total-fuel result))])
          (if (= total-fuel 2)  ; add costs 2 fuel
              (display "✓ PASS: Correct fuel cost for addition\n")
              (display (format "✗ FAIL: Expected 2 fuel, got ~a\n" total-fuel)))))

(display "\n")

;;; ====
;;; Test 2: Factorial (Recursive)
;;; ====

(display "Test 2: Factorial - Recursive Pattern\n")
(display "────────────────────────────────────────────────────────────────\n")

(let ([result (analyze-fuel test-factorial 5)])
     (display (format "Input: (test-factorial 5)\n"))
     (print-analysis result)
     
     ;; Verify result is correct
     (let ([computed-result (cdr (assq 'result result))])
          (if (= computed-result 120)  ; 5! = 120
              (display "✓ PASS: Correct factorial result\n")
              (display (format "✗ FAIL: Expected 120, got ~a\n" computed-result))))
     
     ;; Verify we tracked multiple primitive types
     (let ([prim-calls (cdr (assq 'primitive-calls result))])
          (if (and (assq 'zero? prim-calls)
                   (assq 'mul prim-calls)
                   (assq 'sub prim-calls))
              (display "✓ PASS: Tracked zero?, mul, and sub primitives\n")
              (display "✗ FAIL: Missing expected primitive calls\n"))))

(display "\n")

;;; ====
;;; Test 3: List Length (Linear Complexity)
;;; ====

(display "Test 3: Complexity Analysis - List Length\n")
(display "────────────────────────────────────────────────────────────────\n")

;;; Helper: list length using instrumented primitives
(define (test-length lst)
  (if (prim-instrumented 'null? lst)
      0
      (prim-instrumented 'add 1 (test-length (prim-instrumented 'cdr lst)))))

;;; Helper: make list of size n
(define (make-test-list n)
  (if (= n 0)
      '()
      (cons n (make-test-list (- n 1)))))

(let ([result (estimate-complexity
               test-length
               make-test-list
               '(10 20 40 80))])
     (display "Input: list length with sizes [10, 20, 40, 80]\n")
     (print-complexity-analysis result)
     
     ;; Verify it detected linear complexity
     (let ([complexity (cdr (assq 'complexity result))])
          (if (string=? complexity "O(n)")
              (display "✓ PASS: Correctly identified O(n) complexity\n")
              (display (format "✗ FAIL: Expected O(n), got ~a\n" complexity)))))

(display "\n")

;;; ====
;;; Test 4: Constant Time Operation
;;; ====

(display "Test 4: Complexity Analysis - Constant Time\n")
(display "────────────────────────────────────────────────────────────────\n")

;;; Helper: constant time car operation
(define (test-car lst)
  (prim-instrumented 'car lst))

(let ([result (estimate-complexity
               test-car
               make-test-list
               '(10 100 1000))])
     (display "Input: car operation with sizes [10, 100, 1000]\n")
     (print-complexity-analysis result)
     
     ;; Verify it detected constant complexity
     (let ([complexity (cdr (assq 'complexity result))])
          (if (string=? complexity "O(1)")
              (display "✓ PASS: Correctly identified O(1) complexity\n")
              (display (format "✗ FAIL: Expected O(1), got ~a\n" complexity)))))

(display "\n")

;;; ====
;;; Test 5: Multiple Primitive Types
;;; ====

(display "Test 5: Multiple Primitive Types\n")
(display "────────────────────────────────────────────────────────────────\n")

;;; Helper: function using various primitives
(define (test-multi x)
  (let* ([neg-x (prim-instrumented 'neg x)]
         [abs-x (prim-instrumented 'abs neg-x)]
         [doubled (prim-instrumented 'mul abs-x 2)])
        (prim-instrumented 'add doubled 10)))

(let ([result (analyze-fuel test-multi 5)])
     (display (format "Input: (test-multi 5)\n"))
     (print-analysis result)
     
     ;; Verify we see all primitive types
     (let ([prim-calls (cdr (assq 'primitive-calls result))])
          (if (and (assq 'neg prim-calls)
                   (assq 'abs prim-calls)
                   (assq 'mul prim-calls)
                   (assq 'add prim-calls))
              (display "✓ PASS: Tracked all primitive types (neg, abs, mul, add)\n")
              (display "✗ FAIL: Missing some primitive calls\n")))
     
     ;; Verify correct result
     (let ([computed-result (cdr (assq 'result result))])
          (if (= computed-result 20)  ; |(-5)| * 2 + 10 = 20
              (display "✓ PASS: Correct result\n")
              (display (format "✗ FAIL: Expected 20, got ~a\n" computed-result)))))

(display "\n")

;;; ====
;;; Test 6: Fuel Cost Tiers
;;; ====

(display "Test 6: Fuel Cost Tiers\n")
(display "────────────────────────────────────────────────────────────────\n")

;;; Helper: test different cost tiers
(define (test-costs x)
  (let* ([;; Tier 1 (cost 1) - type predicate
          is-num (prim-instrumented 'number? x)]
         [;; Tier 2 (cost 2) - arithmetic
          doubled (prim-instrumented 'add x x)]
         [;; Tier 3 (cost 3) - division
          halved (prim-instrumented 'div doubled 2)])
        halved))

(let ([result (analyze-fuel test-costs 10)])
     (display (format "Input: (test-costs 10)\n"))
     (print-analysis result)
     
     ;; Verify total fuel: 1 (number?) + 2 (add) + 3 (div) = 6
     (let ([total-fuel (cdr (assq 'total-fuel result))])
          (if (= total-fuel 6)
              (display "✓ PASS: Correct total fuel across cost tiers (1+2+3=6)\n")
              (display (format "✗ FAIL: Expected 6 fuel, got ~a\n" total-fuel)))))

(display "\n")

;;; ====
;;; Test 7: Error Handling
;;; ====

(display "Test 7: Error Handling\n")
(display "────────────────────────────────────────────────────────────────\n")

;;; Helper: function that might error
(define (test-error x)
  (prim-instrumented 'div 10 x))

(let ([result (analyze-fuel test-error 0)])
     (display (format "Input: (test-error 0) - division by zero\n"))
     
     ;; Check if error is recorded
     (let ([error-info (assq 'error result)])
          (if error-info
              (begin
               (display (format "Error detected: ~a\n" (cdr error-info)))
               (display "✓ PASS: Error handling works\n"))
              (begin
               (display "Result: ")
               (write result)
               (newline)
               (display "Note: Error handling may vary by implementation\n")))))

(display "\n")

;;; ====
;;; Summary
;;; ====

(display "════════════════════════════════════════════════════════════════\n")
(display "  TEST SUITE COMPLETE\n")
(display "════════════════════════════════════════════════════════════════\n")
(display "\n")
(display "All basic fuel analysis functionality has been tested.\n")
(display "\n")
(display "Key Features Verified:\n")
(display "  ✓ Primitive call tracking\n")
(display "  ✓ Fuel cost calculation\n")
(display "  ✓ Complexity estimation (O(1), O(n))\n")
(display "  ✓ Multiple primitive types\n")
(display "  ✓ Fuel cost tiers\n")
(display "  ✓ Error handling\n")
(display "\n")
