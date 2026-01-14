;;; core/types/test-equality.ss — Tests for Propositional Equality Types
;;;
;;; Tests for the Martin-Löf identity type implementation:
;;;   - Equality type formation (= A x y)
;;;   - Introduction via refl
;;;   - Elimination via J
;;;   - Derived operations: sym, trans, cong, subst

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
=== Equality Types Test Suite ===

")
  
  ;; ----
  ;; Equality Type Predicates
  ;; ----
  (display "--- Equality Type Predicates ---
")
  
  (test-true "equality-type? on (= Nat 1 1)"
             (equality-type? '(= Nat 1 1)))
  (test-false "equality-type? on Nat"
              (equality-type? 'Nat))
  (test-false "equality-type? on arrow"
              (equality-type? '(-> Nat Nat)))
  (test-true "equality-type-well-formed? basic"
             (equality-type-well-formed? '(= Nat x y)))
  (test-false "equality-type-well-formed? wrong length"
              (equality-type-well-formed? '(= Nat x)))
  
  ;; ----
  ;; Equality Type Operations
  ;; ----
  (display "
--- Equality Type Operations ---
")
  
  (test "equality-carrier extracts type"
        'Nat
        (equality-carrier '(= Nat x y)))
  (test "equality-lhs extracts left term"
        'x
        (equality-lhs '(= Nat x y)))
  (test "equality-rhs extracts right term"
        'y
        (equality-rhs '(= Nat x y)))
  
  ;; ----
  ;; Equality Type Construction
  ;; ----
  (display "
--- Equality Type Construction ---
")
  
  (test "t-eq constructs equality type"
        '(= Nat x y)
        (t-eq 'Nat 'x 'y))
  (test "t-eq with complex carrier"
        '(= (List Int) xs ys)
        (t-eq '(List Int) 'xs 'ys))
  
  ;; ----
  ;; Refl Proof Term Predicates
  ;; ----
  (display "
--- Refl Proof Term Predicates ---
")
  
  (test-true "refl-term? on bare refl"
             (refl-term? 'refl))
  (test-true "refl-term? on annotated refl"
             (refl-term? '(refl Nat 5)))
  (test-false "refl-term? on other term"
              (refl-term? '(sym p)))
  (test "refl-carrier extracts type"
        'Nat
        (refl-carrier '(refl Nat 5)))
  (test "refl-witness extracts term"
        5
        (refl-witness '(refl Nat 5)))
  (test "refl-carrier on bare refl"
        #f
        (refl-carrier 'refl))
  
  ;; ----
  ;; J Eliminator Predicate
  ;; ----
  (display "
--- J Eliminator Predicate ---
")
  
  (test-true "j-term? on J expression"
             (j-term? '(J Nat P d x y p)))
  (test-false "j-term? on other expression"
              (j-term? '(refl Nat 5)))
  
  ;; ----
  ;; Type Display
  ;; ----
  (display "
--- Type Display ---
")
  
  (test "dep-type->string for equality"
        "x ≡ y : Nat"
        (dep-type->string '(= Nat x y)))
  (test "dep-type->string for reflexive equality"
        "5 ≡ 5 : Int"
        (dep-type->string '(= Int 5 5)))
  
  ;; ----
  ;; Wellformedness
  ;; ----
  (display "
--- Wellformedness ---
")
  
  (test-true "well-formed-dep-type? equality type"
             (well-formed-dep-type? '(= Nat x y)))
  (test-true "well-formed-dep-type? equality with expressions"
             (well-formed-dep-type? '(= Int (+ 1 2) 3)))
  
  ;; ----
  ;; Type Synthesis - Equality Type Formation
  ;; ----
  (display "
--- Type Synthesis: Equality Type Formation ---
")
  
  ;; Build a context with some variables
  (let ([ctx (dep-ctx-extend
              (dep-ctx-extend empty-dep-ctx 'x 'Nat)
              'y 'Nat)])
       
       (test-ok "synth equality type with valid terms"
                (dep-synth '(= Nat x y) ctx))
       
       (test "synth equality type yields Type"
             '(ok Type)
             (dep-synth '(= Nat x y) ctx))
       
       ;; Unbound variable should fail
       (test-error "synth equality with unbound var"
                   (dep-synth '(= Nat x z) ctx)))
  
  ;; ----
  ;; Type Synthesis - refl
  ;; ----
  (display "
--- Type Synthesis: refl ---
")
  
  (let ([ctx (dep-ctx-extend empty-dep-ctx 'x 'Nat)])
       
       (test-ok "synth (refl Nat x) in context"
                (dep-synth '(refl Nat x) ctx))
       
       (test "synth (refl Nat x) yields (= Nat x x)"
             '(ok (= Nat x x))
             (dep-synth '(refl Nat x) ctx))
       
       ;; With literal
       (test "synth (refl Int 5) yields (= Int 5 5)"
             '(ok (= Int 5 5))
             (dep-synth '(refl Int 5) empty-dep-ctx))
       
       ;; Bare refl needs annotation
       (test-error "synth bare refl without annotation"
                   (dep-synth '(refl) empty-dep-ctx)))
  
  ;; ----
  ;; Type Synthesis - sym (symmetry)
  ;; ----
  (display "
--- Type Synthesis: sym (symmetry) ---
")
  
  (let ([ctx (dep-ctx-extend
              (dep-ctx-extend
               (dep-ctx-extend empty-dep-ctx 'x 'Nat)
               'y 'Nat)
              'p '(= Nat x y))])
       
       (test-ok "synth (sym p)"
                (dep-synth '(sym p) ctx))
       
       (test "synth (sym p) yields (= Nat y x)"
             '(ok (= Nat y x))
             (dep-synth '(sym p) ctx))
       
       ;; Non-equality argument should fail
       (let ([ctx2 (dep-ctx-extend empty-dep-ctx 'n 'Nat)])
            (test-error "synth (sym n) with non-equality arg"
                        (dep-synth '(sym n) ctx2))))
  
  ;; ----
  ;; Type Synthesis - trans (transitivity)
  ;; ----
  (display "
--- Type Synthesis: trans (transitivity) ---
")
  
  (let ([ctx (dep-ctx-extend
              (dep-ctx-extend
               (dep-ctx-extend
                (dep-ctx-extend
                 (dep-ctx-extend empty-dep-ctx 'x 'Nat)
                 'y 'Nat)
                'z 'Nat)
               'p '(= Nat x y))
              'q '(= Nat y z))])
       
       (test-ok "synth (trans p q)"
                (dep-synth '(trans p q) ctx))
       
       (test "synth (trans p q) yields (= Nat x z)"
             '(ok (= Nat x z))
             (dep-synth '(trans p q) ctx)))
  
  ;; ----
  ;; Type Synthesis - cong (congruence)
  ;; ----
  (display "
--- Type Synthesis: cong (congruence) ---
")
  
  (let ([ctx (dep-ctx-extend
              (dep-ctx-extend
               (dep-ctx-extend
                (dep-ctx-extend empty-dep-ctx 'x 'Nat)
                'y 'Nat)
               'p '(= Nat x y))
              'f '(-> Nat Int))])
       
       (test-ok "synth (cong f p)"
                (dep-synth '(cong f p) ctx))
       
       (test "synth (cong f p) yields (= Int (f x) (f y))"
             '(ok (= Int (f x) (f y)))
             (dep-synth '(cong f p) ctx)))
  
  ;; ----
  ;; Type Synthesis - subst (substitution)
  ;; ----
  (display "
--- Type Synthesis: subst (substitution) ---
")
  
  (let ([ctx (dep-ctx-extend
              (dep-ctx-extend
               (dep-ctx-extend
                (dep-ctx-extend
                 (dep-ctx-extend empty-dep-ctx 'x 'Nat)
                 'y 'Nat)
                'p '(= Nat x y))
               'P '(-> Nat Type))
              'prf '(P x))])
       
       (test-ok "synth (subst P p prf)"
                (dep-synth '(subst P p prf) ctx))
       
       (test "synth (subst P p prf) yields (P y)"
             '(ok (P y))
             (dep-synth '(subst P p prf) ctx)))
  
  ;; ----
  ;; Dependent Types Integration
  ;; ----
  (display "
--- Dependent Types Integration ---
")
  
  ;; Equality in Pi type (identity function preserves equality)
  (test "Pi type with equality in codomain"
        '(Π ((x : Nat)) (Π ((y : Nat)) (Π ((p : (= Nat x y))) (= Nat x y))))
        (t-pi* '((x . Nat) (y . Nat) (p . (= Nat x y))) '(= Nat x y)))
  
  ;; Vec length equality
  (let ([vec-eq-type '(= Nat n m)])
       (test-true "equality of vector lengths"
                  (equality-type? vec-eq-type)))
  
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
