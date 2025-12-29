;;; fabric/stitches/test-dep-infer.ss — Tests for Dependent Type Inference
;;;
;;; Tests for Pi types, Sigma types, and dependent inference.

(load "prelude.ss")
(load "types.ss")
(load "dep-types.ss")
(load "nbe.ss")
(load "infer.ss")
(load "dep-infer.ss")

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

(define (test-type name expr expected-type)
  (let ([result (dep-typeof expr)])
       (if (and (pair? result) (eq? (car result) 'error))
           (test name expected-type result)
           (test name expected-type result))))

(define (test-error name expr)
  (set! *test-count* (+ *test-count* 1))
  (let ([result (dep-typeof expr)])
       (if (and (pair? result) (eq? (car result) 'error))
           (begin
            (set! *pass-count* (+ *pass-count* 1))
            (display "✓ ")
            (display name)
            (display " (correctly errors)")
            (newline))
           (begin
            (set! *fail-count* (+ *fail-count* 1))
            (display "✗ ")
            (display name)
            (display " (expected error, got: ")
            (write result)
            (display ")")
            (newline)))))

(define (test-checks name expr type)
  (set! *test-count* (+ *test-count* 1))
  (let ([result (dep-typecheck expr type)])
       (if result
           (begin
            (set! *pass-count* (+ *pass-count* 1))
            (display "✓ ")
            (display name)
            (newline))
           (begin
            (set! *fail-count* (+ *fail-count* 1))
            (display "✗ ")
            (display name)
            (display " (failed to typecheck)")
            (newline)))))

(define (run-tests)
  (display "
=== Dependent Type Inference Test Suite ===

")
  
  ;; Universe synthesis
  (display "--- Universe Synthesis ---
")
  (test-type "Type : Type1" 'Type '(Type 1))
  (test-type "(Type 1) : (Type 2)" '(Type 1) '(Type 2))
  (test-type "Nat : Type" 'Nat 'Type)
  (test-type "Int : Type" 'Int 'Type)
  (test-type "Bool : Type" 'Bool 'Type)
  
  ;; Literal synthesis
  (display "
--- Literal Synthesis ---
")
  (test-type "42 : Int" 42 'Int)
  (test-type "#t : Bool" #t 'Bool)
  (test-type "\"hello\" : String" "hello" 'String)
  (test-type "'foo : Symbol" ''foo 'Symbol)
  
  ;; Arrow type synthesis
  (display "
--- Arrow Type Synthesis ---
")
  (test-type "(-> Int Int) : Type" '(-> Int Int) 'Type)
  (test-type "(-> Nat Bool String) : Type" '(-> Nat Bool String) 'Type)
  
  ;; Pi type synthesis
  (display "
--- Pi Type Synthesis ---
")
  (test-type "simple Pi type"
             '(Π ((n : Nat)) Nat)
             'Type)
  (test-type "Pi with dependent body"
             '(Π ((A : Type)) A)
             '(Type 1))
  
  ;; Sigma type synthesis
  (display "
--- Sigma Type Synthesis ---
")
  (test-type "simple Sigma type"
             '(Σ ((n : Nat)) Nat)
             'Type)
  (test-type "Sigma with dependent body"
             '(Σ ((A : Type)) A)
             '(Type 1))
  
  ;; Product type synthesis
  (display "
--- Product Type Synthesis ---
")
  (test-type "(× Int Bool) : Type" '(× Int Bool) 'Type)
  
  ;; Lambda with annotation
  (display "
--- Lambda Synthesis ---
")
  (test-type "annotated identity"
             '(fn ((x : Int)) x)
             '(Π ((x : Int)) Int))
  (test-type "const function"
             '(fn ((x : Int)) 42)
             '(Π ((x : Int)) Int))
  
  ;; Pair synthesis
  (display "
--- Pair Synthesis ---
")
  (test-type "simple pair"
             '(pair 1 #t)
             '(Σ ((_ : Int)) Bool))
  (test-type "nested pair"
             '(pair 1 (pair 2 3))
             '(Σ ((_ : Int)) (Σ ((_ : Int)) Int)))
  
  ;; Projection synthesis
  (display "
--- Projection Synthesis ---
")
  ;; For projections, we need expressions in context
  ;; Skip for now as they need pair values
  
  ;; Type checking
  (display "
--- Type Checking ---
")
  (test-checks "check 42 : Int" 42 'Int)
  (test-checks "check #t : Bool" #t 'Bool)
  (test-checks "check identity lambda"
               '(fn ((x : Int)) x)
               '(Π ((x : Int)) Int))
  
  ;; Lambda against Pi type
  (display "
--- Lambda vs Pi Checking ---
")
  (test-checks "lambda checks against Pi"
               '(fn (x) x)
               '(Π ((x : Int)) Int))
  (test-checks "lambda checks against arrow"
               '(fn (x) x)
               '(-> Int Int))
  
  ;; Pair against Sigma type
  (display "
--- Pair vs Sigma Checking ---
")
  (test-checks "pair checks against Sigma"
               '(pair 1 #t)
               '(Σ ((n : Int)) Bool))
  
  ;; Error cases
  (display "
--- Error Cases ---
")
  (test-error "unbound variable" 'unbound-xyz)
  (test-error "unannotated lambda" '(fn (x) x))
  
  ;; Summary
  (display "
=== Test Summary ===
")
  (display "Total: ")
  (display *test-count*)
  (display ", Passed: ")
  (display *pass-count*)
  (display ", Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;; Run tests
(run-tests)
