;;; core/types/test-termination.ss --- Tests for Termination Checking
;;;
;;; Tests for the termination analysis of type-level computation.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/termination.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-ok name result)
  (test name #t (and (pair? result) (eq? (car result) 'ok))))

(define (test-error name result)
  (test name #t (and (pair? result) (eq? (car result) 'error))))

;;; ============================================================
;;; Size Relation Tests
;;; ============================================================

(define (run-size-relation-tests)
  (display "
--- Size Relation Tests ---
")
  
  ;; size-relation?
  (test-true "< is a size relation" (size-relation? '<))
  (test-true "<= is a size relation" (size-relation? '<=))
  (test-true "? is a size relation" (size-relation? '?))
  (test-false "= is not a size relation" (size-relation? '=))
  
  ;; size-rel-compose
  (test "< compose <= = <" '< (size-rel-compose '< '<=))
  (test "<= compose <= = <=" '<= (size-rel-compose '<= '<=))
  (test "< compose < = <" '< (size-rel-compose '< '<))
  (test "? compose < = ?" '? (size-rel-compose '? '<))
  (test "< compose ? = ?" '? (size-rel-compose '< '?))
  (test "? compose ? = ?" '? (size-rel-compose '? '?))
  
  ;; size-rel-meet
  (test "< meet <= = <" '< (size-rel-meet '< '<=))
  (test "<= meet < = <" '< (size-rel-meet '<= '<))
  (test "<= meet <= = <=" '<= (size-rel-meet '<= '<=))
  (test "? meet < = <" '< (size-rel-meet '? '<))
  (test "< meet ? = <" '< (size-rel-meet '< '?))
  (test "? meet <= = <=" '<= (size-rel-meet '? '<=)))

;;; ============================================================
;;; Call Graph Tests
;;; ============================================================

(define (run-call-graph-tests)
  (display "
--- Call Graph Tests ---
")
  
  ;; call-edge construction
  (let ([edge (make-call-edge 'f 'g '((0 . (0 . <))))])
       (test-true "call-edge? on call-edge" (call-edge? edge))
       (test "call-edge-source" 'f (call-edge-source edge))
       (test "call-edge-target" 'g (call-edge-target edge)))
  
  ;; call-graph construction
  (let ([graph (make-call-graph 'factorial '(n) '())])
       (test-true "call-graph? on call-graph" (call-graph? graph))
       (test "call-graph-name" 'factorial (call-graph-name graph))
       (test "call-graph-params" '(n) (call-graph-params graph))))

;;; ============================================================
;;; Structural Subterm Tests
;;; ============================================================

(define (run-structural-subterm-tests)
  (display "
--- Structural Subterm Tests ---
")
  
  ;; Direct variable is not strictly smaller
  (test-false "x is not subterm of x" (structural-subterm? 'x 'x '()))
  
  ;; Destructors create subterms
  (test-true "(fst x) is subterm of x" (structural-subterm? '(fst x) 'x '()))
  (test-true "(snd x) is subterm of x" (structural-subterm? '(snd x) 'x '()))
  (test-true "(car x) is subterm of x" (structural-subterm? '(car x) 'x '()))
  (test-true "(cdr x) is subterm of x" (structural-subterm? '(cdr x) 'x '()))
  (test-true "(pred n) is subterm of n" (structural-subterm? '(pred n) 'n '()))
  
  ;; Nested destructors
  (test-true "(fst (snd x)) is subterm of x" (structural-subterm? '(fst (snd x)) 'x '()))
  (test-true "(cdr (cdr xs)) is subterm of xs" (structural-subterm? '(cdr (cdr xs)) 'xs '()))
  
  ;; Different variable is not subterm
  (test-false "(fst y) is not subterm of x" (structural-subterm? '(fst y) 'x '()))
  
  ;; Context-based subterm (from pattern matching)
  (let ([ctx '((m . (n . <)))])  ; m is marked as subterm of n
       (test-true "m is subterm of n via context" (structural-subterm? 'm 'n ctx))))

;;; ============================================================
;;; SCG Tests
;;; ============================================================

(define (run-scg-tests)
  (display "
--- SCG Tests ---
")
  
  ;; Basic SCG construction
  (let ([scg (make-scg '(n) '(n) '((<)))])
       (test-true "scg? on scg" (scg? scg))
       (test "scg-source-params" '(n) (scg-source-params scg))
       (test "scg-edges" '((<)) (scg-edges scg)))
  
  ;; SCG with diagonal decrease terminates
  (let ([scg (make-scg '(n) '(n) '((<)))])
       (test-true "SCG with < diagonal terminates" (scg-has-decrease? scg)))
  
  ;; SCG without decrease doesn't terminate
  (let ([scg (make-scg '(n) '(n) '((<=)))])
       (test-false "SCG with <= diagonal does not decrease" (scg-has-decrease? scg)))
  
  ;; SCG idempotent check
  (let ([scg1 (make-scg '(n) '(n) '((<)))]
        [scg2 (make-scg '(n) '(m) '((<)))])
       (test-true "Same source/target is idempotent" (scg-idempotent? scg1))
       (test-false "Different source/target is not idempotent" (scg-idempotent? scg2))))

;;; ============================================================
;;; Termination Check Tests
;;; ============================================================

(define (run-termination-tests)
  (display "
--- Termination Check Tests ---
")
  
  ;; Type-level termination for simple types
  (test-ok "Int terminates" (check-type-termination 'Int))
  (test-ok "Bool terminates" (check-type-termination 'Bool))
  (test-ok "(-> Int Int) terminates" (check-type-termination '(-> Int Int)))
  
  ;; Type family applications
  (test-ok "(Vec 5 Int) terminates" (check-type-termination '(Vec 5 Int)))
  (test-ok "(List Int) terminates" (check-type-termination '(List Int)))
  
  ;; Pi types
  (test-ok "Pi type terminates"
           (check-type-termination '(Π ((n : Nat)) (Vec n Int))))
  
  ;; Conditional types
  (test-ok "Conditional type terminates"
           (check-type-termination '(if #t Int Bool)))
  
  ;; Nested types
  (test-ok "Nested type terminates"
           (check-type-termination '(Π ((A : Type)) (Π ((B : Type)) (-> A B))))))

;;; ============================================================
;;; Structural Recursion Validation Tests
;;; ============================================================

;;; Helper: create a mock function definition
(define (make-func-def name params body)
  `(define (,name ,@params) ,body))

(define (run-structural-recursion-tests)
  (display "
--- Structural Recursion Validation Tests ---
")
  
  ;; Properly terminating factorial (conceptual)
  ;; factorial n = case n of { zero -> 1; succ m -> n * factorial m }
  (let ([def (make-func-def 'factorial '(n)
                            '(case n
                              ((zero) 1)
                              ((succ m) (* n (factorial m)))))])
       (test "factorial params" '(n) (func-def-params def))
       (test "factorial name" 'factorial (func-def-name def)))
  
  ;; Check recursion violation detection
  ;; f(n) where n is the recursive param IS a violation (not decreasing)
  (let ([violations (find-recursion-violations 'f 'n '(f n) '())])
       (test-true "f(n) IS a violation (same arg, not decreasing)" (pair? violations)))
  
  ;; Subterm recursion should not be a violation
  (let ([violations (find-recursion-violations 'f 'n '(f (pred n)) '())])
       (test-true "f(pred n) is not a violation" (null? violations))))

;;; ============================================================
;;; Helper Function Tests
;;; ============================================================

(define (run-helper-tests)
  (display "
--- Helper Function Tests ---
")
  
  ;; filter-map
  (test "filter-map keeps mapped values"
        '(2 4 6)
        (filter-map (lambda (x) (if (even? x) x #f)) '(1 2 3 4 5 6)))
  
  ;; append-map
  (test "append-map flattens results"
        '(1 1 2 2 3 3)
        (append-map (lambda (x) (list x x)) '(1 2 3)))
  
  ;; all
  (test-true "all positive in (1 2 3)" (all positive? '(1 2 3)))
  (test-false "not all positive in (1 -2 3)" (all positive? '(1 -2 3)))
  (test-true "all on empty list" (all positive? '()))
  
  ;; ormap
  (test-true "some negative in (1 -2 3)" (ormap negative? '(1 -2 3)))
  (test-false "no negative in (1 2 3)" (ormap negative? '(1 2 3)))
  (test-false "ormap on empty list" (ormap negative? '())))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(define (run-integration-tests)
  (display "
--- Integration Tests ---
")
  
  ;; Test with-termination-check
  (test-true "termination checking enabled by default" termination-check-enabled?)
  
  (without-termination-check
   (lambda ()
           (test-false "termination checking disabled inside without-"
                       termination-check-enabled?)))
  
  (test-true "termination checking re-enabled after without-"
             termination-check-enabled?)
  
  ;; Main entry point
  (test-ok "termination-check on empty params" (termination-check 'x '()))
  (test-ok "termination-check simple body" (termination-check '(+ 1 2) '(n))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(define (run-tests)
  (display "
=== Termination Checking Test Suite ===

")
  
  (run-size-relation-tests)
  (run-call-graph-tests)
  (run-structural-subterm-tests)
  (run-scg-tests)
  (run-termination-tests)
  (run-structural-recursion-tests)
  (run-helper-tests)
  (run-integration-tests)
  
  (display "
=== Test Summary ===
")
  (display "Total: ") (display *test-count*)
  (display ", Passed: ") (display *pass-count*)
  (display ", Failed: ") (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;;; Run tests when loaded
(run-tests)
