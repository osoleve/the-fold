;;; core/types/test-autodiff-types.ss — Tests for Autodiff Type Integration
;;;
;;; Tests for the integration of automatic differentiation with
;;; the type system, including Diff/Grad types, type inference,
;;; and differentiability tracking in annotated ASTs.

(load "core/types/annotate.ss")

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
  (display "\n=== Autodiff Type Integration Tests ===\n\n")
  
  ;;; ============================================================
  ;;; Real Base Type
  ;;; ============================================================
  
  (display "--- Real Base Type ---\n")
  
  (test-true "Real is a base type" (base-type? 'Real))
  (test-true "Real is a type" (type? 'Real))
  (test-true "Real is numeric" (numeric-type? 'Real))
  (test-true "Int is numeric" (numeric-type? 'Int))
  (test-true "Nat is numeric" (numeric-type? 'Nat))
  (test-false "Bool is not numeric" (numeric-type? 'Bool))
  (test-false "String is not numeric" (numeric-type? 'String))
  (test-true "(Vec 3 Real) is numeric" (numeric-type? '(Vec 3 Real)))
  (test-true "(Matrix 2 3 Real) is numeric" (numeric-type? '(Matrix 2 3 Real)))
  
  ;;; ============================================================
  ;;; Diff Type Constructor
  ;;; ============================================================
  
  (display "\n--- Diff Type Constructor ---\n")
  
  ;; Construction
  (test "t-diff Real" '(Diff Real) (t-diff 'Real))
  (test "t-diff Int" '(Diff Int) (t-diff 'Int))
  (test "t-diff (Vec 3 Real)" '(Diff (Vec 3 Real)) (t-diff '(Vec 3 Real)))
  
  ;; type? predicate
  (test-true "(Diff Real) is a type" (type? '(Diff Real)))
  (test-true "(Diff Int) is a type" (type? '(Diff Int)))
  (test-true "(Diff (Vec 3 Real)) is a type" (type? '(Diff (Vec 3 Real))))
  
  ;; diff-type? predicate
  (test-true "(Diff Real) is diff-type" (diff-type? '(Diff Real)))
  (test-false "Real is not diff-type" (diff-type? 'Real))
  (test-false "(Grad Real) is not diff-type" (diff-type? '(Grad Real)))
  
  ;; diff-inner-type accessor
  (test "diff-inner-type (Diff Real)" 'Real (diff-inner-type '(Diff Real)))
  (test "diff-inner-type (Diff (Vec Int))" '(Vec Int) (diff-inner-type '(Diff (Vec Int))))
  (test "diff-inner-type on non-Diff" 'Int (diff-inner-type 'Int))
  
  ;;; ============================================================
  ;;; Grad Type Constructor
  ;;; ============================================================
  
  (display "\n--- Grad Type Constructor ---\n")
  
  ;; Construction
  (test "t-grad Real" '(Grad Real) (t-grad 'Real))
  (test "t-grad (Vec 3 Real)" '(Grad (Vec 3 Real)) (t-grad '(Vec 3 Real)))
  
  ;; type? predicate
  (test-true "(Grad Real) is a type" (type? '(Grad Real)))
  (test-true "(Grad (Vec 3 Real)) is a type" (type? '(Grad (Vec 3 Real))))
  
  ;; grad-type? predicate
  (test-true "(Grad Real) is grad-type" (grad-type? '(Grad Real)))
  (test-false "Real is not grad-type" (grad-type? 'Real))
  (test-false "(Diff Real) is not grad-type" (grad-type? '(Diff Real)))
  
  ;; grad-inner-type accessor
  (test "grad-inner-type (Grad Real)" 'Real (grad-inner-type '(Grad Real)))
  (test "grad-inner-type on non-Grad" 'Int (grad-inner-type 'Int))
  
  ;;; ============================================================
  ;;; Type Equality
  ;;; ============================================================
  
  (display "\n--- Type Equality ---\n")
  
  (test-true "(Diff Real) = (Diff Real)" (type=? '(Diff Real) '(Diff Real)))
  (test-false "(Diff Real) ≠ (Diff Int)" (type=? '(Diff Real) '(Diff Int)))
  (test-false "(Diff Real) ≠ (Grad Real)" (type=? '(Diff Real) '(Grad Real)))
  (test-true "(Grad Real) = (Grad Real)" (type=? '(Grad Real) '(Grad Real)))
  
  ;; Nested types
  (test-true "(-> (Diff Real) (Diff Real)) equality"
             (type=? '(-> (Diff Real) (Diff Real))
                     '(-> (Diff Real) (Diff Real))))
  (test-false "(-> (Diff Real) (Diff Real)) ≠ (-> (Diff Real) Real)"
              (type=? '(-> (Diff Real) (Diff Real))
                      '(-> (Diff Real) Real)))
  
  ;;; ============================================================
  ;;; Type->String
  ;;; ============================================================
  
  (display "\n--- Type->String ---\n")
  
  (test "type->string Real" "Real" (type->string 'Real))
  (test "type->string (Diff Real)" "(Diff Real)" (type->string '(Diff Real)))
  (test "type->string (Grad Real)" "(Grad Real)" (type->string '(Grad Real)))
  (test "type->string (-> (Diff Real) (Diff Real))"
        "((Diff Real) → (Diff Real))"
        (type->string '(-> (Diff Real) (Diff Real))))
  
  ;;; ============================================================
  ;;; Free Type Variables
  ;;; ============================================================
  
  (display "\n--- Free Type Variables ---\n")
  
  (test "free-tvars in (Diff a)" '(a) (free-tvars '(Diff a)))
  (test "free-tvars in (Grad a)" '(a) (free-tvars '(Grad a)))
  (test "free-tvars in (-> (Diff a) (Grad b))" '(a b)
        (free-tvars '(-> (Diff a) (Grad b))))
  (test "free-tvars in (Diff Real)" '() (free-tvars '(Diff Real)))
  
  ;;; ============================================================
  ;;; Type Substitution
  ;;; ============================================================
  
  (display "\n--- Type Substitution ---\n")
  
  (test "subst a → Real in (Diff a)"
        '(Diff Real)
        (subst-type '(Diff a) 'a 'Real))
  (test "subst a → Int in (Grad a)"
        '(Grad Int)
        (subst-type '(Grad a) 'a 'Int))
  (test "subst a → Real in (-> (Diff a) (Grad a))"
        '(-> (Diff Real) (Grad Real))
        (subst-type '(-> (Diff a) (Grad a)) 'a 'Real))
  
  ;;; ============================================================
  ;;; Autodiff Primitive Types
  ;;; ============================================================
  
  (display "\n--- Autodiff Primitive Types ---\n")
  
  ;; Check that primitives are registered
  (test-true "lift has type" (and (lookup-prim-type 'lift) #t))
  (test-true "primal has type" (and (lookup-prim-type 'primal) #t))
  (test-true "gradient has type" (and (lookup-prim-type 'gradient) #t))
  (test-true "grad has type" (and (lookup-prim-type 'grad) #t))
  (test-true "d+ has type" (and (lookup-prim-type 'd+) #t))
  (test-true "d* has type" (and (lookup-prim-type 'd*) #t))
  (test-true "d-exp has type" (and (lookup-prim-type 'd-exp) #t))
  
  ;; Check Real arithmetic primitives
  (test-true "sqrt has type" (and (lookup-prim-type 'sqrt) #t))
  (test-true "exp has type" (and (lookup-prim-type 'exp) #t))
  (test-true "sin has type" (and (lookup-prim-type 'sin) #t))
  (test-true "cos has type" (and (lookup-prim-type 'cos) #t))
  
  ;;; ============================================================
  ;;; Type Inference for Diff Types
  ;;; ============================================================
  
  (display "\n--- Type Inference for Diff Types ---\n")
  
  ;; Infer type of (prim 'lift 5)
  (let ([result (typeof '(prim 'lift 5))])
       (test-true "typeof (prim 'lift 5) is (Diff ...)"
                  (and (pair? result)
                       (eq? (car result) 'Diff))))
  
  ;; Infer type of transcendental functions
  (let ([result (typeof '(prim 'sqrt 2.0))])
       (test "typeof (prim 'sqrt 2.0)" 'Real result))
  
  (let ([result (typeof '(prim 'exp 1.0))])
       (test "typeof (prim 'exp 1.0)" 'Real result))
  
  ;;; ============================================================
  ;;; Numeric Constraint Enforcement (Negative Tests)
  ;;; ============================================================
  
  (display "\n--- Numeric Constraint Enforcement ---\n")
  
  ;; lift with non-numeric types should fail
  (let ([result (typeof '(prim 'lift "hello"))])
       (test-true "lift String rejected"
                  (and (pair? result)
                       (eq? (car result) 'error)
                       (eq? (cadr result) 'non-numeric-diff-type))))
  
  (let ([result (typeof '(prim 'lift #t))])
       (test-true "lift Bool rejected"
                  (and (pair? result)
                       (eq? (car result) 'error)
                       (eq? (cadr result) 'non-numeric-diff-type))))
  
  (let ([result (typeof '(prim 'lift 'symbol))])
       (test-true "lift Symbol rejected"
                  (and (pair? result)
                       (eq? (car result) 'error)
                       (eq? (cadr result) 'non-numeric-diff-type))))
  
  ;; Valid numeric types should still work
  (let ([result (typeof '(prim 'lift 42))])
       (test-true "lift Int accepted"
                  (and (pair? result)
                       (eq? (car result) 'Diff))))
  
  (let ([result (typeof '(prim 'lift 3.14))])
       (test-true "lift Real accepted"
                  (and (pair? result)
                       (eq? (car result) 'Diff))))
  
  ;;; ============================================================
  ;;; Unification with Diff Types
  ;;; ============================================================
  
  (display "\n--- Unification with Diff Types ---\n")
  
  ;; Same types unify
  (let ([result (unify '(Diff Real) '(Diff Real))])
       (test-true "unify (Diff Real) (Diff Real) succeeds"
                  (eq? (car result) 'ok)))
  
  ;; Different inner types don't unify
  (let ([result (unify '(Diff Real) '(Diff Int))])
       (test-true "unify (Diff Real) (Diff Int) fails"
                  (eq? (car result) 'error)))
  
  ;; Type variable unification
  (let ([result (unify '(Diff a) '(Diff Real))])
       (test-true "unify (Diff a) (Diff Real) succeeds"
                  (eq? (car result) 'ok))
       (when (eq? (car result) 'ok)
             (let ([subst (cadr result)])
                  (test "substitution maps a to Real"
                        'Real
                        (apply-subst subst 'a)))))
  
  ;; Nested type variables
  (let ([result (unify '(-> (Diff a) (Diff a)) '(-> (Diff Real) (Diff Real)))])
       (test-true "unify (-> (Diff a) (Diff a)) with concrete type"
                  (eq? (car result) 'ok)))
  
  ;;; ============================================================
  ;;; Differentiability Detection in Types
  ;;; ============================================================
  
  (display "\n--- Differentiability Detection ---\n")
  
  (test-true "type-contains-diff? (Diff Real)" (type-contains-diff? '(Diff Real)))
  (test-false "type-contains-diff? Real" (type-contains-diff? 'Real))
  (test-true "type-contains-diff? (-> (Diff Real) Int)"
             (type-contains-diff? '(-> (Diff Real) Int)))
  (test-true "type-contains-diff? (List (Diff Real))"
             (type-contains-diff? '(List (Diff Real))))
  
  (test-true "type-contains-grad? (Grad Real)" (type-contains-grad? '(Grad Real)))
  (test-false "type-contains-grad? Real" (type-contains-grad? 'Real))
  (test-false "type-contains-grad? (Diff Real)" (type-contains-grad? '(Diff Real)))
  
  ;;; ============================================================
  ;;; wrap-in-diff / unwrap-diff
  ;;; ============================================================
  
  (display "\n--- wrap-in-diff / unwrap-diff ---\n")
  
  (test "wrap-in-diff Real" '(Diff Real) (wrap-in-diff 'Real))
  (test "wrap-in-diff (Diff Real) is idempotent"
        '(Diff Real) (wrap-in-diff '(Diff Real)))
  
  (test "unwrap-diff (Diff Real)" 'Real (unwrap-diff '(Diff Real)))
  (test "unwrap-diff Real" 'Real (unwrap-diff 'Real))
  
  ;;; ============================================================
  ;;; extract-diff-type / extract-grad-type
  ;;; ============================================================
  
  (display "\n--- extract-diff-type / extract-grad-type ---\n")
  
  (test "extract-diff-type (Diff Real)" 'Real (extract-diff-type '(Diff Real)))
  (test-false "extract-diff-type Real" (extract-diff-type 'Real))
  (test "extract-diff-type (Diff (Vec Int))" '(Vec Int)
        (extract-diff-type '(Diff (Vec Int))))
  
  (test "extract-grad-type (Grad Real)" 'Real (extract-grad-type '(Grad Real)))
  (test-false "extract-grad-type Real" (extract-grad-type 'Real))
  
  ;;; ============================================================
  ;;; T-real Constant
  ;;; ============================================================
  
  (display "\n--- T-real Constant ---\n")
  
  (test "T-real is Real" 'Real T-real)
  (test-true "T-real is base type" (base-type? T-real))
  (test-true "T-real is numeric" (numeric-type? T-real))
  
  ;;; ============================================================
  ;;; Summary
  ;;; ============================================================
  
  (display "\n=== Test Summary ===\n")
  (display "Total:  ")
  (display *test-count*)
  (newline)
  (display "Passed: ")
  (display *pass-count*)
  (newline)
  (display "Failed: ")
  (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "\n✓ All tests passed!\n")
      (begin
       (display "\n✗ Some tests failed.\n")
       (exit 1))))

;; Run tests when loaded
(run-tests)
