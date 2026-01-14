;;; core/types/test-refinement.ss — Tests for Refinement Types
;;;
;;; Tests for refinement types: {x : T | P}
;;;   - Type formation
;;;   - Introduction and elimination
;;;   - Type display

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-infer.ss")

;;; ====
;;; Test Framework
;;; ====

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

;;; ====
;;; Tests
;;; ====

(define (run-tests)
  (display "
=== Refinement Types Test Suite ===

")
  
  ;; ----
  ;; Refinement Type Predicates
  ;; ----
  (display "--- Refinement Type Predicates ---
")
  
  (test-true "refinement-type? on valid refinement"
             (refinement-type? '(refine ((n : Nat)) (> n 0))))
  (test-false "refinement-type? on Nat"
              (refinement-type? 'Nat))
  (test-false "refinement-type? on Pi type"
              (refinement-type? '(Π ((x : Nat)) Nat)))
  (test-true "refinement-type-well-formed? basic"
             (refinement-type-well-formed? '(refine ((n : Nat)) (> n 0))))
  (test-false "refinement-type-well-formed? wrong length"
              (refinement-type-well-formed? '(refine ((n : Nat)))))
  
  ;; ----
  ;; Refinement Type Operations
  ;; ----
  (display "
--- Refinement Type Operations ---
")
  
  (test "refinement-var extracts variable"
        'n
        (refinement-var '(refine ((n : Nat)) (> n 0))))
  (test "refinement-base-type extracts base type"
        'Nat
        (refinement-base-type '(refine ((n : Nat)) (> n 0))))
  (test "refinement-predicate extracts predicate"
        '(> n 0)
        (refinement-predicate '(refine ((n : Nat)) (> n 0))))
  
  ;; ----
  ;; Refinement Type Construction
  ;; ----
  (display "
--- Refinement Type Construction ---
")
  
  (test "t-refine constructs refinement type"
        '(refine ((n : Nat)) (> n 0))
        (t-refine 'n 'Nat '(> n 0)))
  (test "t-refine with complex predicate"
        '(refine ((x : Int)) (and (>= x 0) (< x 100)))
        (t-refine 'x 'Int '(and (>= x 0) (< x 100))))
  
  ;; ----
  ;; Type Display
  ;; ----
  (display "
--- Type Display ---
")
  
  (test "dep-type->string for refinement"
        "{n : Nat | (> n 0)}"
        (dep-type->string '(refine ((n : Nat)) (> n 0))))
  (test "dep-type->string for refinement with complex base"
        "{v : (List Int) | (sorted v)}"
        (dep-type->string '(refine ((v : (List Int))) (sorted v))))
  
  ;; ----
  ;; Wellformedness
  ;; ----
  (display "
--- Wellformedness ---
")
  
  (test-true "well-formed-dep-type? refinement type"
             (well-formed-dep-type? '(refine ((n : Nat)) (> n 0))))
  (test-true "well-formed-dep-type? refinement with Pi base"
             (well-formed-dep-type? '(refine ((f : (Π ((x : Nat)) Nat))) (total f))))
  
  ;; ----
  ;; Type Synthesis - Refinement Type Formation
  ;; ----
  (display "
--- Type Synthesis: Refinement Type Formation ---
")
  
  ;; Note: Synthesis of the refinement type itself requires the predicate
  ;; to be well-typed. Full type formation tests require a richer context.
  ;; For now, we test the basic structure.
  
  ;; ----
  ;; Type Synthesis - Refinement Elimination
  ;; ----
  (display "
--- Type Synthesis: Refinement Elimination ---
")
  
  (let ([ctx (dep-ctx-extend
              empty-dep-ctx
              'refined-val
              '(refine ((n : Nat)) (> n 0)))])
       
       (test-ok "synth (refine-elim refined-val)"
                (dep-synth '(refine-elim refined-val) ctx))
       
       (test "synth (refine-elim refined-val) yields Nat"
             '(ok Nat)
             (dep-synth '(refine-elim refined-val) ctx)))
  
  ;; Test error case
  (let ([ctx (dep-ctx-extend empty-dep-ctx 'x 'Nat)])
       (test-error "synth (refine-elim x) with non-refinement"
                   (dep-synth '(refine-elim x) ctx)))
  
  ;; ----
  ;; Dependent Types Integration
  ;; ----
  (display "
--- Dependent Types Integration ---
")
  
  ;; Refinement type can be used in Pi types
  (test "Pi type with refinement domain"
        '(Π ((n : (refine ((x : Nat)) (> x 0)))) Nat)
        (t-pi 'n '(refine ((x : Nat)) (> x 0)) 'Nat))
  
  ;; Refinement of dependent type
  (test "refinement of Vec type"
        '(refine ((v : (Vec 3 Int))) (sorted v))
        (t-refine 'v '(Vec 3 Int) '(sorted v)))
  
  ;; ----
  ;; Summary
  ;; ----
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
