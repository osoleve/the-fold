;;; boundary/fuel-analysis-demo.ss — Demonstration of Fuel Analysis Tools
;;;
;;; This demonstrates how to use the fuel analysis tools to:
;;;   1. Analyze fuel costs of individual operations
;;;   2. Estimate computational complexity
;;;   3. Compare different algorithms

(load "boundary/diagnostics/fuel-analysis.ss")

(display "\n")
(display "════════════════════════════════════════════════════════════════\n")
(display "  FUEL ANALYSIS DEMONSTRATION\n")
(display "════════════════════════════════════════════════════════════════\n")
(display "\n")

;;; ====
;;; Example 1: Analyzing a Simple Function
;;; ====

(display "Example 1: Analyzing a Simple Function\n")
(display "────────────────────────────────────────────────────────────────\n")
(display "\n")

;;; A function that computes (x + 5) * 2
(define (compute-simple x)
  (prim-instrumented 'mul
                     (prim-instrumented 'add x 5)
                     2))

(display "Function: (compute-simple x) = (x + 5) * 2\n")
(print-analysis (analyze-fuel compute-simple 10))
(display "\n")

;;; ====
;;; Example 2: Comparing List Operations
;;; ====

(display "Example 2: Comparing List Operations\n")
(display "────────────────────────────────────────────────────────────────\n")
(display "\n")

;;; Get first element (constant time)
(define (get-first lst)
  (prim-instrumented 'car lst))

;;; Get last element (linear time)
(define (get-last lst)
  (if (prim-instrumented 'null? (prim-instrumented 'cdr lst))
      (prim-instrumented 'car lst)
      (get-last (prim-instrumented 'cdr lst))))

;;; Helper to make test lists
(define (make-list n)
  (if (= n 0)
      '()
      (cons n (make-list (- n 1)))))

(display "Comparing car (first element) vs getting last element:\n\n")

(display "Operation: car (get first)\n")
(print-complexity-analysis
 (estimate-complexity get-first make-list '(10 100 1000)))
(display "\n")

(display "Operation: get-last\n")
(print-complexity-analysis
 (estimate-complexity get-last make-list '(10 100 1000)))
(display "\n")

;;; ====
;;; Example 3: Recursive vs Iterative
;;; ====

(display "Example 3: Recursive Sum\n")
(display "────────────────────────────────────────────────────────────────\n")
(display "\n")

;;; Sum using recursion
(define (sum-recursive lst)
  (if (prim-instrumented 'null? lst)
      0
      (prim-instrumented 'add
                         (prim-instrumented 'car lst)
                         (sum-recursive (prim-instrumented 'cdr lst)))))

(display "Function: sum-recursive - sums all elements in a list\n")
(print-complexity-analysis
 (estimate-complexity sum-recursive make-list '(5 10 20 40)))
(display "\n")

;;; ====
;;; Example 4: Expensive Operations
;;; ====

(display "Example 4: Detecting Expensive Operations\n")
(display "────────────────────────────────────────────────────────────────\n")
(display "\n")

;;; A function that uses division (more expensive than addition)
(define (avg-with-10 x)
  (prim-instrumented 'div
                     (prim-instrumented 'add x 10)
                     2))

(display "Function: avg-with-10 - computes (x + 10) / 2\n")
(display "Note: division (cost 3) is more expensive than addition (cost 2)\n")
(print-analysis (analyze-fuel avg-with-10 20))
(display "\n")

;;; ====
;;; Example 5: Analyzing Algorithm Efficiency
;;; ====

(display "Example 5: Algorithm Efficiency\n")
(display "────────────────────────────────────────────────────────────────\n")
(display "\n")

;;; Count elements (linear)
(define (count-elements lst)
  (if (prim-instrumented 'null? lst)
      0
      (prim-instrumented 'add 1
                         (count-elements (prim-instrumented 'cdr lst)))))

;;; Check if empty (constant)
(define (is-empty lst)
  (prim-instrumented 'null? lst))

(display "Comparing:\n")
(display "  - count-elements: walks entire list\n")
(display "  - is-empty: checks only the head\n\n")

(display "count-elements complexity:\n")
(print-complexity-analysis
 (estimate-complexity count-elements make-list '(10 20 40)))
(display "\n")

(display "is-empty complexity:\n")
(print-complexity-analysis
 (estimate-complexity is-empty make-list '(10 100 1000)))
(display "\n")

;;; ====
;;; Summary
;;; ====

(display "════════════════════════════════════════════════════════════════\n")
(display "  KEY TAKEAWAYS\n")
(display "════════════════════════════════════════════════════════════════\n")
(display "\n")
(display "1. Fuel costs are based on primitive operation complexity:\n")
(display "   - Predicates (zero?, null?): 1 fuel\n")
(display "   - Arithmetic (add, sub, mul): 2 fuel\n")
(display "   - Division/modulo: 3 fuel\n")
(display "   - Linear operations (length, reverse): 5 fuel\n")
(display "   - Allocations (string-append): 10 fuel\n")
(display "   - Cryptographic (sha256): 100 fuel\n")
(display "\n")
(display "2. Complexity estimation helps identify:\n")
(display "   - O(1): Constant time operations\n")
(display "   - O(n): Linear growth with input size\n")
(display "   - O(n²): Quadratic growth (nested loops)\n")
(display "\n")
(display "3. Use these tools to:\n")
(display "   - Profile hotspots in your code\n")
(display "   - Compare algorithm efficiency\n")
(display "   - Set appropriate fuel budgets\n")
(display "   - Detect potential performance issues\n")
(display "\n")
