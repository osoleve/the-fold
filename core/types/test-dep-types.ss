;;; fabric/stitches/test-dep-types.ss — Tests for Dependent Types
;;;
;;; Tests for Pi types, Sigma types, and NbE normalization.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/lang/nbe.ss")

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

(define (run-tests)
  (display "
=== Dependent Types Test Suite ===

")
  
  ;; Pi type construction
  (display "--- Pi Type Construction ---
")
  (test "t-pi creates Pi type"
        '(Π ((n : Nat)) (List Nat))
        (t-pi 'n 'Nat '(List Nat)))
  
  (test "t-pi* creates nested Pi"
        '(Π ((a : Type)) (Π ((x : a)) a))
        (t-pi* '((a . Type) (x . a)) 'a))
  
  ;; Pi type predicates
  (display "
--- Pi Type Predicates ---
")
  (test-true "pi-type? on Pi" (pi-type? '(Π ((n : Nat)) (List Nat))))
  (test-false "pi-type? on arrow" (pi-type? '(-> Nat Nat)))
  (test "pi-var extracts variable" 'n (pi-var '(Π ((n : Nat)) (List Nat))))
  (test "pi-domain extracts domain" 'Nat (pi-domain '(Π ((n : Nat)) (List Nat))))
  (test "pi-codomain extracts codomain" '(List Nat) (pi-codomain '(Π ((n : Nat)) (List Nat))))
  
  ;; Sigma type construction
  (display "
--- Sigma Type Construction ---
")
  (test "t-sigma creates Sigma type"
        '(Σ ((n : Nat)) (List Nat))
        (t-sigma 'n 'Nat '(List Nat)))
  
  ;; Sigma type predicates
  (display "
--- Sigma Type Predicates ---
")
  (test-true "sigma-type? on Sigma" (sigma-type? '(Σ ((n : Nat)) (List Nat))))
  (test-false "sigma-type? on product" (sigma-type? '(× Nat Nat)))
  (test "sigma-var extracts variable" 'n (sigma-var '(Σ ((n : Nat)) (List Nat))))
  (test "sigma-fst-type extracts first type" 'Nat (sigma-fst-type '(Σ ((n : Nat)) (List Nat))))
  (test "sigma-snd-type extracts second type" '(List Nat) (sigma-snd-type '(Σ ((n : Nat)) (List Nat))))
  
  ;; Universe types
  (display "
--- Universe Types ---
")
  (test-true "universe-type? on Type" (universe-type? 'Type))
  (test-true "universe-type? on (Type 1)" (universe-type? '(Type 1)))
  (test-false "universe-type? on Nat" (universe-type? 'Nat))
  (test "universe-level of Type" 0 (universe-level 'Type))
  (test "universe-level of (Type 2)" 2 (universe-level '(Type 2)))
  (test "t-type creates Type" 'Type (t-type))
  (test "t-type 1 creates (Type 1)" '(Type 1) (t-type 1))
  
  ;; Type display
  (display "
--- Type Display ---
")
  (test "dep-type->string for Pi"
        "Π(n : Nat). (List Nat)"
        (dep-type->string '(Π ((n : Nat)) (List Nat))))
  (test "dep-type->string for Sigma"
        "Σ(n : Nat). (List Nat)"
        (dep-type->string '(Σ ((n : Nat)) (List Nat))))
  (test "dep-type->string for Type"
        "Type"
        (dep-type->string 'Type))
  (test "dep-type->string for (Type 1)"
        "Type1"
        (dep-type->string '(Type 1)))
  
  ;; NbE Values
  (display "
--- NbE Values ---
")
  (test-true "V-lam creates lambda" (V-lam? (V-lam 'x 'x '())))
  (test-true "V-pi creates Pi" (V-pi? (V-pi (V-base 'Nat) (make-closure 'n '(List Nat) '()))))
  (test-true "V-sigma creates Sigma" (V-sigma? (V-sigma (V-base 'Nat) (make-closure 'n '(List Nat) '()))))
  (test-true "V-type creates Type" (V-type? (V-type 0)))
  (test-true "V-pair creates pair" (V-pair? (V-pair (V-base 3) (V-base 'hello))))
  (test-true "V-neutral creates neutral" (V-neutral? (V-neutral (N-var 0))))
  
  ;; NbE Evaluation
  (display "
--- NbE Evaluation ---
")
  (test-true "eval number to V-base" (V-base? (nbe-eval 42 nbe-empty-env)))
  (test "eval number value" 42 (V-base-val (nbe-eval 42 nbe-empty-env)))
  (test-true "eval lambda to V-lam" (V-lam? (nbe-eval '(fn (x) x) nbe-empty-env)))
  (test-true "eval Type to V-type" (V-type? (nbe-eval 'Type nbe-empty-env)))
  (test "eval Type level" 0 (V-type-level (nbe-eval 'Type nbe-empty-env)))
  (test "eval (Type 1) level" 1 (V-type-level (nbe-eval '(Type 1) nbe-empty-env)))
  
  ;; NbE Normalization
  (display "
--- NbE Normalization ---
")
  (test "normalize Type" 'Type (normalize-closed 'Type))
  (test "normalize base type" 'Nat (normalize-closed 'Nat))
  (test "normalize identity lambda"
        '(fn (x0) x0)
        (normalize-closed '(fn (x) x)))
  
  ;; Conversion checking
  (display "
--- Conversion Checking ---
")
  (test-true "Type = Type"
             (types-equal? 'Type 'Type))
  (test-true "Nat = Nat"
             (types-equal? 'Nat 'Nat))
  (test-false "Nat ≠ Int"
              (types-equal? 'Nat 'Int))
  (test-true "(-> Nat Nat) = (-> Nat Nat)"
             (types-equal? '(-> Nat Nat) '(-> Nat Nat)))
  (test-false "(-> Nat Int) ≠ (-> Nat Nat)"
              (types-equal? '(-> Nat Int) '(-> Nat Nat)))
  
  ;; Lambda η-equivalence
  (display "
--- η-Equivalence ---
")
  ;; Note: η-equivalence requires careful handling in NbE
  ;; For now just test basic lambda normalization
  
  ;; Truly dependent types (body references bound variable)
  (display "
--- Truly Dependent Types ---
")
  ;; Vec n Int - length-indexed vector, the canonical dependent type example
  (test "t-pi with dependent body (Vec n Int)"
        '(Π ((n : Nat)) (Vec n Int))
        (t-pi 'n 'Nat '(Vec n Int)))
  (test "t-sigma with dependent body (Vec n Int)"
        '(Σ ((n : Nat)) (Vec n Int))
        (t-sigma 'n 'Nat '(Vec n Int)))
  ;; Multi-parameter dependent Pi
  (test "nested dependent Pi (curried Vec constructor)"
        '(Π ((n : Nat)) (Π ((A : Type)) (Vec n A)))
        (t-pi* '((n . Nat) (A . Type)) '(Vec n A)))
  ;; Matrix with dependent dimensions
  (test "Matrix with dependent dimensions"
        '(Π ((m : Nat)) (Π ((n : Nat)) (Matrix m n Int)))
        (t-pi* '((m . Nat) (n . Nat)) '(Matrix m n Int)))
  ;; Identity type - classic dependent type
  (test "dependent identity type (propositional equality)"
        '(Π ((A : Type)) (Π ((x : A)) (Π ((y : A)) (Id A x y))))
        (t-pi* '((A . Type) (x . A) (y . A)) '(Id A x y)))
  
  ;; Wellformedness checks
  (display "
--- Wellformedness Tests ---
")
  (test-true "well-formed Vec type" (well-formed-dep-type? '(Vec 3 Int)))
  (test-true "well-formed Matrix type" (well-formed-dep-type? '(Matrix 2 3 Int)))
  (test-true "well-formed Pi type" (well-formed-dep-type? '(Π ((n : Nat)) (Vec n Int))))
  (test-true "well-formed nested Pi" (well-formed-dep-type? '(Π ((A : Type)) (Π ((x : A)) A))))
  
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
