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
  
  ;; Capture-avoiding substitution tests
  (display "
--- Capture-Avoiding Substitution Tests ---
")
  ;; Basic substitution
  (test "subst in simple type"
        'Int
        (dep-subst-type 'x 'x 'Int))
  (test "subst preserves other vars"
        'y
        (dep-subst-type 'y 'x 'Int))
  (test "subst in arrow type"
        '(-> Int Int)
        (dep-subst-type '(-> x x) 'x 'Int))
  
  ;; Shadowing works correctly
  ;; When x shadows, the domain is still substituted (x isn't bound in the domain)
  ;; but x in the body is shadowed
  (test "subst respects Pi shadowing - domain still subst"
        '(Π ((x : Nat)) x)  ; Nat not x, so no substitution; body x is shadowed
        (dep-subst-type '(Π ((x : Nat)) x) 'x 'Int))
  (test "subst in Pi domain before shadow"
        '(Π ((y : Int) (x : y)) x)
        (dep-subst-type '(Π ((y : x) (x : y)) x) 'x 'Int))
  
  ;; Capture-avoiding: substituting a term with free var y into a binder for y
  ;; (Π ((y : Nat)) (f y)) [x := y] should become (Π ((y0 : Nat)) (f y0)) with y substituted
  (test "capture-avoiding Pi substitution"
        (let ([result (dep-subst-type '(Π ((y : Nat)) (f y x)) 'x 'y)])
             ;; Result should be (Π ((y0 : Nat)) (f y0 y)) - y0 is fresh
             ;; Check that bound var is renamed and body has correct substitution
             (and (pi-type? result)
                  (not (eq? (pi-var result) 'y))  ; y must be renamed
                  (let* ([renamed-var (pi-var result)]
                         [body (pi-codomain result)])
                        ;; Body should be (f <renamed-var> y)
                        (and (equal? (car body) 'f)
                             (equal? (cadr body) renamed-var)
                             (equal? (caddr body) 'y)))))
        #t)
  
  ;; Capture-avoiding in Sigma
  (test "capture-avoiding Sigma substitution"
        (let ([result (dep-subst-type '(Σ ((y : Nat)) (Pair y x)) 'x 'y)])
             ;; y must be renamed to avoid capture
             (and (sigma-type? result)
                  (not (eq? (sigma-var result) 'y))))
        #t)
  
  ;; Multi-parameter Pi - all bindings should be processed
  (test "multi-param Pi substitution processes all bindings"
        (let ([result (dep-subst-type '(Π ((a : x) (b : x) (c : x)) (f a b c x)) 'x 'Int)])
             ;; All x's should be replaced with Int
             (and (pi-type? result)
                  (let ([bindings (pi-bindings result)])
                       (and (equal? (cdr (car bindings)) 'Int)
                            (equal? (cdr (cadr bindings)) 'Int)
                            (equal? (cdr (caddr bindings)) 'Int)))))
        #t)
  
  ;; Dependent binding where later bindings use earlier bound vars
  (test "multi-param Pi with dependency between bindings"
        (dep-subst-type '(Π ((A : Type) (x : A)) (Vec A x)) 'T 'Nat)
        '(Π ((A : Type) (x : A)) (Vec A x)))  ; T not in type, no change
  
  ;; Heterogeneous Equality Types
  (display "
--- Heterogeneous Equality Types ---
")
  ;; HEq type predicates
  (test-true "heq-type? on HEq" (heq-type? '(HEq A B a b)))
  (test-false "heq-type? on homogeneous =" (heq-type? '(= A a b)))
  (test-false "heq-type? on non-equality" (heq-type? '(-> A B)))
  
  ;; HEq wellformedness
  (test-true "heq-type-well-formed?" (heq-type-well-formed? '(HEq A B a b)))
  (test-false "heq-type-well-formed? wrong arity" (heq-type-well-formed? '(HEq A B a)))
  (test-false "heq-type-well-formed? not HEq" (heq-type-well-formed? '(= A a b)))
  
  ;; HEq extractors
  (test "heq-left-type extracts A" 'A (heq-left-type '(HEq A B a b)))
  (test "heq-right-type extracts B" 'B (heq-right-type '(HEq A B a b)))
  (test "heq-lhs extracts a" 'a (heq-lhs '(HEq A B a b)))
  (test "heq-rhs extracts b" 'b (heq-rhs '(HEq A B a b)))
  
  ;; Complex HEq types
  (test "heq-left-type with complex type"
        '(Vec n Int)
        (heq-left-type '(HEq (Vec n Int) (Vec m Int) xs ys)))
  (test "heq-right-type with complex type"
        '(Vec m Int)
        (heq-right-type '(HEq (Vec n Int) (Vec m Int) xs ys)))
  
  ;; make-heq-type constructor
  (test "make-heq-type with different types"
        '(HEq A B a b)
        (make-heq-type 'A 'B 'a 'b))
  (test "make-heq-type degenerates to = when types equal"
        '(= A a b)
        (make-heq-type 'A 'A 'a 'b))
  (test "make-heq-type with complex equal types"
        '(= (Vec 3 Int) xs ys)
        (make-heq-type '(Vec 3 Int) '(Vec 3 Int) 'xs 'ys))
  (test "make-heq-type with complex different types"
        '(HEq (Vec n Int) (Vec m Int) xs ys)
        (make-heq-type '(Vec n Int) '(Vec m Int) 'xs 'ys))
  
  ;; HEq extractors on non-HEq return sensible defaults
  (test "heq-left-type on non-HEq" 'Void (heq-left-type '(= A a b)))
  (test "heq-lhs on non-HEq" #f (heq-lhs '(= A a b)))
  
  (display "
--- Differentiable Type Tests ---
")
  
  ;; Diff type predicates
  (test-true "diff-type? on (Diff Float Float)"
             (diff-type? '(Diff Float Float)))
  (test-true "diff-type? on (Diff (Vec n Float) Float)"
             (diff-type? '(Diff (Vec n Float) Float)))
  (test-true "diff-type? on (Diff (Vec n Float) (Vec m Float))"
             (diff-type? '(Diff (Vec n Float) (Vec m Float))))
  (test-false "diff-type? on arrow type"
              (diff-type? '(-> Float Float)))
  (test-false "diff-type? on symbol"
              (diff-type? 'Float))
  
  ;; Diff type well-formedness
  (test-true "diff-type-well-formed? valid"
             (diff-type-well-formed? '(Diff Float Float)))
  (test-true "diff-type-well-formed? with Vec"
             (diff-type-well-formed? '(Diff (Vec 3 Float) Float)))
  (test-false "diff-type-well-formed? wrong arity"
              (diff-type-well-formed? '(Diff Float)))
  (test-false "diff-type-well-formed? extra arg"
              (diff-type-well-formed? '(Diff Float Float Float)))
  
  ;; Diff type extractors
  (test "diff-domain extracts domain"
        'Float
        (diff-domain '(Diff Float Int)))
  (test "diff-domain with Vec"
        '(Vec n Float)
        (diff-domain '(Diff (Vec n Float) Float)))
  (test "diff-codomain extracts codomain"
        '(Vec m Float)
        (diff-codomain '(Diff Float (Vec m Float))))
  (test "diff-domain on non-Diff"
        'Void
        (diff-domain '(-> Float Float)))
  
  ;; make-diff-type constructor
  (test "make-diff-type constructs Diff"
        '(Diff Float Float)
        (make-diff-type 'Float 'Float))
  (test "make-diff-type with Vec"
        '(Diff (Vec n Float) (Vec m Float))
        (make-diff-type '(Vec n Float) '(Vec m Float)))
  
  ;; Grad type predicates
  (test-true "grad-type? on (Grad Float)"
             (grad-type? '(Grad Float)))
  (test-true "grad-type? on (Grad (Vec n Float))"
             (grad-type? '(Grad (Vec n Float))))
  (test-false "grad-type? on Diff"
              (grad-type? '(Diff Float Float)))
  (test-false "grad-type? on symbol"
              (grad-type? 'Float))
  
  ;; Grad type well-formedness
  (test-true "grad-type-well-formed? valid"
             (grad-type-well-formed? '(Grad Float)))
  (test-false "grad-type-well-formed? wrong arity"
              (grad-type-well-formed? '(Grad)))
  (test-false "grad-type-well-formed? extra arg"
              (grad-type-well-formed? '(Grad Float Float)))
  
  ;; Grad type extractors
  (test "grad-inner-type extracts inner"
        'Float
        (grad-inner-type '(Grad Float)))
  (test "grad-inner-type with Vec"
        '(Vec n Float)
        (grad-inner-type '(Grad (Vec n Float))))
  (test "grad-inner-type on non-Grad"
        'Void
        (grad-inner-type '(Diff Float Float)))
  
  ;; make-grad-type constructor
  (test "make-grad-type constructs Grad"
        '(Grad Float)
        (make-grad-type 'Float))
  (test "make-grad-type with Vec"
        '(Grad (Vec n Float))
        (make-grad-type '(Vec n Float)))
  
  ;; numeric-type? predicate
  (test-true "numeric-type? on Float" (numeric-type? 'Float))
  (test-true "numeric-type? on Int" (numeric-type? 'Int))
  (test-true "numeric-type? on Nat" (numeric-type? 'Nat))
  (test-true "numeric-type? on Real" (numeric-type? 'Real))
  (test-true "numeric-type? on (Vec n Float)"
             (numeric-type? '(Vec n Float)))
  (test-true "numeric-type? on (Matrix m n Float)"
             (numeric-type? '(Matrix m n Float)))
  (test-false "numeric-type? on Bool" (numeric-type? 'Bool))
  (test-false "numeric-type? on String" (numeric-type? 'String))
  (test-false "numeric-type? on (Vec n Bool)"
              (numeric-type? '(Vec n Bool)))
  
  ;; scalar-type? predicate
  (test-true "scalar-type? on Float" (scalar-type? 'Float))
  (test-true "scalar-type? on Int" (scalar-type? 'Int))
  (test-false "scalar-type? on Vec" (scalar-type? '(Vec n Float)))
  (test-false "scalar-type? on Matrix" (scalar-type? '(Matrix m n Float)))
  
  ;; Dimension extraction
  (test "diff-input-dim scalar"
        'scalar
        (diff-input-dim '(Diff Float Float)))
  (test "diff-input-dim Vec"
        'n
        (diff-input-dim '(Diff (Vec n Float) Float)))
  (test "diff-input-dim numeric n"
        3
        (diff-input-dim '(Diff (Vec 3 Float) Float)))
  (test "diff-output-dim scalar"
        'scalar
        (diff-output-dim '(Diff Float Float)))
  (test "diff-output-dim Vec"
        'm
        (diff-output-dim '(Diff Float (Vec m Float))))
  
  ;; Gradient type computation
  (test "diff-grad-type scalar"
        '(Grad Float)
        (diff-grad-type '(Diff Float Float)))
  (test "diff-grad-type Vec input"
        '(Grad (Vec n Float))
        (diff-grad-type '(Diff (Vec n Float) Float)))
  
  ;; Jacobian type computation
  (test "diff-jacobian-type scalar->scalar"
        'Float
        (diff-jacobian-type '(Diff Float Float)))
  (test "diff-jacobian-type Vec->scalar"
        '(Vec n Float)
        (diff-jacobian-type '(Diff (Vec n Float) Float)))
  (test "diff-jacobian-type Vec->Vec"
        '(Matrix m n Float)
        (diff-jacobian-type '(Diff (Vec n Float) (Vec m Float))))
  
  ;; Hessian type computation
  (test "diff-hessian-type scalar->scalar"
        'Float
        (diff-hessian-type '(Diff Float Float)))
  (test "diff-hessian-type Vec->scalar"
        '(Matrix n n Float)
        (diff-hessian-type '(Diff (Vec n Float) Float)))
  
  ;; Composition check
  (test-true "diff-composable? matching types"
             (diff-composable? '(Diff Float Float) '(Diff Float Float)))
  (test-true "diff-composable? Vec chain"
             (diff-composable? '(Diff (Vec m Float) (Vec p Float))
                               '(Diff (Vec n Float) (Vec m Float))))
  (test-false "diff-composable? mismatched types"
              (diff-composable? '(Diff Float Float) '(Diff Float (Vec n Float))))
  (test-false "diff-composable? non-Diff first arg"
              (diff-composable? '(-> Float Float) '(Diff Float Float)))
  
  ;; dep-type? includes Diff and Grad
  (test-true "dep-type? on Diff" (dep-type? '(Diff Float Float)))
  (test-true "dep-type? on Grad" (dep-type? '(Grad Float)))
  
  ;; well-formed-dep-type? includes Diff and Grad
  (test-true "well-formed-dep-type? on Diff"
             (well-formed-dep-type? '(Diff Float Float)))
  (test-true "well-formed-dep-type? on Grad"
             (well-formed-dep-type? '(Grad (Vec n Float))))
  
  ;; dep-type->string for Diff and Grad
  (test "dep-type->string Diff"
        "Diff(Float → Float)"
        (dep-type->string '(Diff Float Float)))
  (test "dep-type->string Diff with Vec"
        "Diff(Vec(n, Float) → Float)"
        (dep-type->string '(Diff (Vec n Float) Float)))
  (test "dep-type->string Grad"
        "∇(Float)"
        (dep-type->string '(Grad Float)))
  (test "dep-type->string Grad with Vec"
        "∇(Vec(n, Float))"
        (dep-type->string '(Grad (Vec n Float))))
  
  ;; Free variables helper
  (test "dep-free-vars basic"
        '(x)
        (dep-free-vars 'x))
  (test "dep-free-vars arrow"
        (sort (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
              (dep-free-vars '(-> x y)))
        '(x y))
  (test "dep-free-vars Pi binds correctly"
        (dep-free-vars '(Π ((x : Nat)) x))
        '())  ; x is bound
  (test "dep-free-vars Pi free in domain"
        (dep-free-vars '(Π ((x : y)) x))
        '(y))  ; y is free in domain
  
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
