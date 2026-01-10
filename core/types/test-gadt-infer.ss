;;; core/types/test-gadt-infer.ss — Tests for GADT Type Inference
;;;
;;; Tests for the inference algorithms in gadt-infer.ss:
;;;   - gadt-infer-synth (GADT declaration synthesis)
;;;   - gadt-infer-synth-case (GADT case synthesis with type refinement)
;;;   - gadt-infer-check-ctors (constructor type checking)
;;;   - gadt-infer-synth-clause (clause synthesis)

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/lang/nbe.ss")
(load "core/types/infer.ss")
(load "core/types/dep-infer.ss")

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
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
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
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result) (eq? (car result) 'ok))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected: (ok ...)
")
       (display "  Actual:   ")
       (write result)
       (newline))))

(define (test-ok-type name result expected-type)
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result)
           (eq? (car result) 'ok)
           (equal? (cadr result) expected-type))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected: (ok ")
       (write expected-type)
       (display ")
")
       (display "  Actual:   ")
       (write result)
       (newline))))

(define (test-error name result expected-error-tag)
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result)
           (eq? (car result) 'error)
           (eq? (cadr result) expected-error-tag))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (display " (correctly errors: ")
       (display expected-error-tag)
       (display ")")
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected error: ")
       (write expected-error-tag)
       (newline)
       (display "  Actual:   ")
       (write result)
       (newline))))

(define (test-any-error name result)
  (set! *test-count* (+ *test-count* 1))
  (if (and (pair? result) (eq? (car result) 'error))
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ")
       (display *pass-count*)
       (display ". ")
       (display name)
       (display " (correctly errors)")
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected: (error ...)
")
       (display "  Actual:   ")
       (write result)
       (newline))))

;;; ============================================================
;;; Test Setup
;;; ============================================================

;;; Helper to synthesize with empty context
(define (synth-empty expr)
  (dep-synth expr empty-dep-ctx))

;;; Helper to synthesize with a given context
(define (synth-with-ctx expr ctx)
  (dep-synth expr ctx))

;;; Build a context with some bindings
(define (make-test-ctx . bindings)
  (let loop ([bindings bindings] [ctx empty-dep-ctx])
       (if (null? bindings)
           ctx
           (loop (cddr bindings)
                 (dep-ctx-extend ctx (car bindings) (cadr bindings))))))

;;; ============================================================
;;; Example GADTs for Testing
;;; ============================================================

;;; Simple expression GADT (without forall for now)
(define expr-gadt
  '(gadt (Expr a)
    [Lit  : (-> Int (Expr Int))]
    [Add  : (-> (Expr Int) (Expr Int) (Expr Int))]
    [Eq   : (-> (Expr Int) (Expr Int) (Expr Bool))]))

;;; Length-indexed vector GADT
(define vec-gadt
  '(gadt (Vec (n : Nat) a)
    [VNil  : (Vec 0 a)]
    [VCons : (forall (m) (-> a (Vec m a) (Vec (succ m) a)))]))

;;; Simple Maybe GADT
(define maybe-gadt
  '(gadt (Maybe a)
    [Nothing : (Maybe a)]
    [Just    : (-> a (Maybe a))]))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(define (run-tests)
  (display "
=== GADT Type Inference Test Suite ===

")
  
  ;; Reset registry before tests
  (reset-gadt-registry!)
  
  ;; --------------------------------------------------------
  ;; GADT Declaration Synthesis (gadt-infer-synth)
  ;; --------------------------------------------------------
  (display "--- GADT Declaration Synthesis ---
")
  
  ;; Malformed GADT - empty
  (test-any-error "malformed GADT: empty"
                  (synth-empty '(gadt)))
  
  ;; GADT declaration synthesis (fixed in fold-7knq)
  (reset-gadt-registry!)
  (test-ok "Maybe GADT declaration synthesizes" (synth-empty maybe-gadt))
  (reset-gadt-registry!)
  (test-ok "Expr GADT declaration synthesizes" (synth-empty expr-gadt))
  
  ;; --------------------------------------------------------
  ;; GADT Constructor Validation
  ;; --------------------------------------------------------
  (display "
--- GADT Constructor Validation ---
")
  
  ;; Constructor that doesn't return the GADT type - should fail
  (let ([bad-gadt '(gadt (Bad a)
                    [Wrong : (-> a Int)])])  ; Returns Int, not (Bad a)
       (test-any-error "constructor returns wrong type"
                       (synth-empty bad-gadt)))
  
  ;; --------------------------------------------------------
  ;; Registry Operations
  ;; --------------------------------------------------------
  (display "
--- Registry Operations ---
")
  
  ;; Lookup non-existent GADT
  (test-false "lookup non-existent GADT"
              (lookup-gadt 'NonExistent))
  
  ;; Manual registration works (bypassing inference)
  (register-gadt! maybe-gadt)
  (test-true "manual registration works"
             (not (not (lookup-gadt 'Maybe))))
  
  ;; Reset registry clears all GADTs
  (reset-gadt-registry!)
  (test-false "reset clears Maybe"
              (lookup-gadt 'Maybe))
  
  ;; --------------------------------------------------------
  ;; GADT Case Synthesis with Manual Registration
  ;; --------------------------------------------------------
  (display "
--- GADT Case Synthesis (manual registration) ---
")
  
  ;; Manually register GADTs to test case synthesis
  (register-gadt! maybe-gadt)
  (register-gadt! expr-gadt)
  
  ;; Non-GADT scrutinee - should error
  (let ([ctx (make-test-ctx 'x 'Int)])
       (let ([case-expr '(gadt-case x
                          ((Nothing) 0))])
            (test-error "scrutinee not GADT type"
                        (synth-with-ctx case-expr ctx)
                        'scrutinee-not-gadt-type)))
  
  ;; NOTE: Maybe-style GADTs have type variables in constructor return types,
  ;; which causes unreachable-gadt-branch errors due to how extract-type-equations
  ;; works. Expr-style GADTs (where constructors return concrete types) work.
  ;; Tests below use Expr GADT for case synthesis validation.
  
  ;; Empty case (no clauses) - using Expr instead of Maybe
  (let ([ctx (make-test-ctx 'e '(Expr Int))])
       (let ([case-expr '(gadt-case e)])
            (test-error "empty gadt-case"
                        (synth-with-ctx case-expr ctx)
                        'empty-gadt-case)))
  
  ;; Unknown constructor in pattern
  (let ([ctx (make-test-ctx 'e '(Expr Int))])
       (let ([case-expr '(gadt-case e
                          ((Unknown) 0))])
            (test-error "unknown constructor"
                        (synth-with-ctx case-expr ctx)
                        'unknown-constructor)))
  
  ;; Pattern arity mismatch (Lit takes 1 arg)
  (let ([ctx (make-test-ctx 'e '(Expr Int))])
       (let ([case-expr '(gadt-case e
                          ((Lit a b) 0))])  ; Lit takes 1 arg, not 2
            (test-error "pattern arity mismatch"
                        (synth-with-ctx case-expr ctx)
                        'pattern-arity-mismatch)))
  
  ;; --------------------------------------------------------
  ;; Type Refinement Tests
  ;; --------------------------------------------------------
  (display "
--- Type Refinement ---
")
  
  ;; Expr GADT: matching Lit refines type to Int
  (let ([ctx (make-test-ctx 'e '(Expr Int))])
       (let ([case-expr '(gadt-case e
                          ((Lit n) n))])
            (test-ok-type "Lit pattern gives Int binding"
                          (synth-with-ctx case-expr ctx)
                          'Int)))
  
  ;; Expr GADT: multiple Lit/Add branches
  (let ([ctx (make-test-ctx 'e '(Expr Int))])
       (let ([case-expr '(gadt-case e
                          ((Lit n) n)
                          ((Add x y) 42))])
            (test-ok-type "Lit and Add branches both return Int"
                          (synth-with-ctx case-expr ctx)
                          'Int)))
  
  ;; --------------------------------------------------------
  ;; Integration Tests
  ;; --------------------------------------------------------
  (display "
--- Integration Tests ---
")
  
  ;; TODO: Nested GADT case blocked by Maybe variable resolution issue
  ;; (let ([ctx (make-test-ctx 'mx '(Maybe (Maybe Int)))])
  ;;      (let ([case-expr '(gadt-case mx ...)])
  ;;           (test-ok-type "nested GADT case" ...)))
  
  ;; --------------------------------------------------------
  ;; Summary
  ;; --------------------------------------------------------
  (display "
=== Summary ===
")
  (display "Total: ") (display *test-count*) (newline)
  (display "Passed: ") (display *pass-count*) (newline)
  (display "Failed: ") (display *fail-count*) (newline)
  
  (if (= *fail-count* 0)
      (display "
All tests passed!
")
      (begin
       (display "
SOME TESTS FAILED!
")
       (exit 1))))

;; Run tests
(run-tests)
