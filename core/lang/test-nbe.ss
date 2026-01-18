;;; Test harness for core/lang/nbe.ss — Normalization by Evaluation

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/lang/nbe.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

(define (test-true name expr)
  (display "  ")
  (display name)
  (display ": ")
  (if (eq? #t expr)
      (display "✓")
      (begin
       (display "✗
    expected: #t
    got: ")
       (display expr)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Value Construction Tests
;;; ====
(test-section "Value Construction")

;; Lambda value
(define lam1 (V-lam 'x 'x '()))
(test-true "V-lam? recognizes lambda" (V-lam? lam1))
(test "V-lam-param extracts parameter" 'x (V-lam-param lam1))
(test "V-lam-body extracts body" 'x (V-lam-body lam1))

;; Pi type value
(define pi1 (V-pi (V-base 'Int) (make-const-closure (V-base 'Int))))
(test-true "V-pi? recognizes pi type" (V-pi? pi1))

;; Sigma type value
(define sigma1 (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool))))
(test-true "V-sigma? recognizes sigma type" (V-sigma? sigma1))

;; Type universe
(define type0 (V-type 0))
(test-true "V-type? recognizes universe" (V-type? type0))
(test "V-type-level extracts level" 0 (V-type-level type0))

;; Pair value
(define pair1 (V-pair (V-base 1) (V-base 2)))
(test-true "V-pair? recognizes pair" (V-pair? pair1))
(test "V-pair-fst extracts first" (V-base 1) (V-pair-fst pair1))
(test "V-pair-snd extracts second" (V-base 2) (V-pair-snd pair1))

;; Vec value
(define vec1 (V-vec (V-base 3) (V-base 'Int)))
(test-true "V-vec? recognizes vector type" (V-vec? vec1))

;; Matrix value
(define mat1 (V-matrix (V-base 2) (V-base 3) (V-base 'Double)))
(test-true "V-matrix? recognizes matrix type" (V-matrix? mat1))

;; Neutral value
(define neutral1 (V-neutral (N-var 'x)))
(test-true "V-neutral? recognizes neutral" (V-neutral? neutral1))

;; Base value
(define base1 (V-base 42))
(test-true "V-base? recognizes base" (V-base? base1))
(test "V-base-val extracts value" 42 (V-base-val base1))

;;; ====
;;; Neutral Value Tests
;;; ====
(test-section "Neutral Values")

(define nvar (N-var 'x))
(test-true "N-var? recognizes variable" (N-var? nvar))
(test "N-var-level extracts level" 'x (N-var-level nvar))

(define napp (N-app nvar (V-base 1)))
(test-true "N-app? recognizes application" (N-app? napp))

(define nfst (N-fst nvar))
(test-true "N-fst? recognizes first projection" (N-fst? nfst))

(define nsnd (N-snd nvar))
(test-true "N-snd? recognizes second projection" (N-snd? nsnd))

;;; ====
;;; Closure Tests
;;; ====
(test-section "Closures")

(define clo1 (make-closure 'x 'x '()))
(test-true "closure? recognizes closure" (closure? clo1))
(test "closure-param extracts parameter" 'x (closure-param clo1))
(test "closure-body extracts body" 'x (closure-body clo1))

(define const-clo (make-const-closure (V-base 42)))
(test-true "const-closure? recognizes const closure" (const-closure? const-clo))
(test "const-closure-value extracts value" (V-base 42) (const-closure-value const-clo))

;; Test apply-closure with const closure
(test "apply-closure const returns value"
      (V-base 42)
      (apply-closure const-clo (V-base 999)))

;;; ====
;;; Environment Tests
;;; ====
(test-section "Environments")

(test "nbe-empty-env is empty" '() nbe-empty-env)

(define env1 (env-extend nbe-empty-env 'x (V-base 10)))
(test "env-extend adds binding" '((x . (V-base 10))) env1)

(test "env-lookup finds binding" (V-base 10) (env-lookup env1 'x))

;; Unknown variable becomes neutral
(define unknown (env-lookup nbe-empty-env 'y))
(test-true "env-lookup unknown becomes neutral" (V-neutral? unknown))

;;; ====
;;; Basic Evaluation Tests
;;; ====
(test-section "Basic Evaluation")

;; Literals
(test "eval number" (V-base 42) (nbe-eval 42 nbe-empty-env))
(test "eval boolean true" (V-base #t) (nbe-eval #t nbe-empty-env))
(test "eval boolean false" (V-base #f) (nbe-eval #f nbe-empty-env))
(test "eval string" (V-base "hello") (nbe-eval "hello" nbe-empty-env))

;; Type universe
(test "eval Type" (V-type 0) (nbe-eval 'Type nbe-empty-env))
(test "eval (Type 1)" (V-type 1) (nbe-eval '(Type 1) nbe-empty-env))

;; Base types
(test "eval base type Int" (V-base 'Int) (nbe-eval 'Int nbe-empty-env))
(test "eval base type Bool" (V-base 'Bool) (nbe-eval 'Bool nbe-empty-env))

;; Variables (become neutral)
(define var-result (nbe-eval 'x nbe-empty-env))
(test-true "eval free variable becomes neutral" (V-neutral? var-result))

;; Lambda
(define lam-result (nbe-eval '(fn (x) x) nbe-empty-env))
(test-true "eval lambda creates V-lam" (V-lam? lam-result))
(test "lambda body is preserved" 'x (V-lam-body lam-result))

;; Lambda with typed parameter
(define lam-typed (nbe-eval '(λ ((x : Int)) x) nbe-empty-env))
(test-true "eval typed lambda creates V-lam" (V-lam? lam-typed))
(test "typed lambda extracts param name" 'x (V-lam-param lam-typed))

;;; ====
;;; Application Tests
;;; ====
(test-section "Application")

;; Identity function applied
(define id-app (nbe-eval '((fn (x) x) 42) nbe-empty-env))
(test "apply identity to 42" (V-base 42) id-app)

;; Constant function
(define const-app (nbe-eval '((fn (x) ((fn (y) x) 999)) 42) nbe-empty-env))
(test "apply const function" (V-base 42) const-app)

;; Multi-argument application (curried)
(define add-app (nbe-eval '((fn (x) (fn (y) (+ x y))) 3) nbe-empty-env))
(test-true "partial application creates lambda" (V-lam? add-app))

;; Apply neutral function (creates neutral application)
(define neutral-app (nbe-apply (V-neutral (N-var 'f)) (V-base 1)))
(test-true "apply neutral creates neutral" (V-neutral? neutral-app))

;;; ====
;;; Pi Type Tests
;;; ====
(test-section "Pi Types")

;; Non-dependent function type: Int -> Bool
(define arrow-result (nbe-eval '(-> Int Bool) nbe-empty-env))
(test-true "arrow creates V-pi" (V-pi? arrow-result))
(test "arrow domain is Int" (V-base 'Int) (V-pi-domain arrow-result))

;; Multi-argument arrow: Int -> Bool -> String
(define arrow3 (nbe-eval '(-> Int Bool String) nbe-empty-env))
(test-true "multi-arrow creates V-pi" (V-pi? arrow3))
(test "multi-arrow domain is Int" (V-base 'Int) (V-pi-domain arrow3))

;; Dependent Pi type: (Π ((n : Nat)) (Vec n Int))
(define pi-dep (nbe-eval '(Π ((n : Nat)) (Vec n Int)) nbe-empty-env))
(test-true "dependent Pi creates V-pi" (V-pi? pi-dep))
(test "Pi domain" (V-base 'Nat) (V-pi-domain pi-dep))

;;; ====
;;; Sigma Type Tests
;;; ====
(test-section "Sigma Types")

;; Non-dependent product: Int × Bool
(define product-result (nbe-eval '(× Int Bool) nbe-empty-env))
(test-true "product creates V-sigma" (V-sigma? product-result))
(test "product fst type is Int" (V-base 'Int) (V-sigma-fst-type product-result))

;; Multi-element product: Int × Bool × String
(define product3 (nbe-eval '(× Int Bool String) nbe-empty-env))
(test-true "multi-product creates V-sigma" (V-sigma? product3))

;; Dependent Sigma: (Σ ((n : Nat)) (Vec n Int))
(define sigma-dep (nbe-eval '(Σ ((n : Nat)) (Vec n Int)) nbe-empty-env))
(test-true "dependent Sigma creates V-sigma" (V-sigma? sigma-dep))
(test "Sigma fst type" (V-base 'Nat) (V-sigma-fst-type sigma-dep))

;;; ====
;;; Pair Tests
;;; ====
(test-section "Pairs")

;; Pair construction
(define pair-val (nbe-eval '(pair 1 2) nbe-empty-env))
(test-true "pair creates V-pair" (V-pair? pair-val))
(test "pair fst is 1" (V-base 1) (V-pair-fst pair-val))
(test "pair snd is 2" (V-base 2) (V-pair-snd pair-val))

;; Pair projections
(test "fst projection" (V-base 1) (nbe-eval '(fst (pair 1 2)) nbe-empty-env))
(test "snd projection" (V-base 2) (nbe-eval '(snd (pair 1 2)) nbe-empty-env))

;; Projection from neutral
(define neutral-pair (V-neutral (N-var 'p)))
(test-true "fst of neutral is neutral" (V-neutral? (nbe-fst neutral-pair)))
(test-true "snd of neutral is neutral" (V-neutral? (nbe-snd neutral-pair)))

;;; ====
;;; Vec and Matrix Types
;;; ====
(test-section "Vec and Matrix Types")

;; Vec type
(define vec-type (nbe-eval '(Vec 5 Int) nbe-empty-env))
(test-true "Vec creates V-vec" (V-vec? vec-type))
(test "Vec length" (V-base 5) (V-vec-n vec-type))
(test "Vec element type" (V-base 'Int) (V-vec-A vec-type))

;; Matrix type (use Int - a base type; Double is not in base-types)
(define mat-type (nbe-eval '(Matrix 2 3 Int) nbe-empty-env))
(test-true "Matrix creates V-matrix" (V-matrix? mat-type))
(test "Matrix rows" (V-base 2) (V-matrix-m mat-type))
(test "Matrix cols" (V-base 3) (V-matrix-n mat-type))
(test "Matrix element type" (V-base 'Int) (V-matrix-A mat-type))

;;; ====
;;; Type-Level Computation Tests
;;; ====
(test-section "Type-Level Computation")

;; Arithmetic
(test "type-level add" (V-base 5) (nbe-eval '(+ 2 3) nbe-empty-env))
(test "type-level sub" (V-base 7) (nbe-eval '(- 10 3) nbe-empty-env))
(test "type-level mul" (V-base 12) (nbe-eval '(* 3 4) nbe-empty-env))

;; Comparison
(test "type-level =" (V-base #t) (nbe-eval '(= 5 5) nbe-empty-env))
(test "type-level =" (V-base #f) (nbe-eval '(= 5 6) nbe-empty-env))
(test "type-level <" (V-base #t) (nbe-eval '(< 3 5) nbe-empty-env))
(test "type-level <" (V-base #f) (nbe-eval '(< 5 3) nbe-empty-env))
(test "type-level >" (V-base #t) (nbe-eval '(> 5 3) nbe-empty-env))

;; Type-level conditional
(test "if true branch" (V-base 'Int) (nbe-eval '(if #t Int Bool) nbe-empty-env))
(test "if false branch" (V-base 'Bool) (nbe-eval '(if #f Int Bool) nbe-empty-env))

;; Stuck computation (neutral condition)
(define stuck-if (nbe-eval '(if x Int Bool) nbe-empty-env))
(test-true "if with neutral condition creates V-type-prim" (V-type-prim? stuck-if))

;; Stuck arithmetic
(define stuck-add (nbe-eval '(+ x 3) nbe-empty-env))
(test-true "add with neutral creates V-type-prim" (V-type-prim? stuck-add))

;;; ====
;;; Readback Tests
;;; ====
(test-section "Readback")

;; Readback literals
(test "readback number" 42 (readback (V-base 42) 0))
(test "readback boolean" #t (readback (V-base #t) 0))
(test "readback string" "hello" (readback (V-base "hello") 0))

;; Readback type universe
(test "readback Type" 'Type (readback (V-type 0) 0))
(test "readback (Type 2)" '(Type 2) (readback (V-type 2) 0))

;; Readback lambda
(define lam-nf (readback (V-lam 'x 'x nbe-empty-env) 0))
(test "readback lambda form" 'fn (car lam-nf))
(test "readback lambda param" '(x0) (cadr lam-nf))

;; Readback pair
(test "readback pair" '(pair 1 2)
      (readback (V-pair (V-base 1) (V-base 2)) 0))

;; Readback Vec
(test "readback Vec" '(Vec 3 Int)
      (readback (V-vec (V-base 3) (V-base 'Int)) 0))

;; Readback Matrix
(test "readback Matrix" '(Matrix 2 3 Double)
      (readback (V-matrix (V-base 2) (V-base 3) (V-base 'Double)) 0))

;; Readback non-dependent Pi (becomes arrow)
(define pi-nondep (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool))))
(test "readback non-dependent Pi" '(-> Int Bool) (readback pi-nondep 0))

;; Readback non-dependent Sigma (becomes product)
(define sigma-nondep (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool))))
(test "readback non-dependent Sigma" '(× Int Bool) (readback sigma-nondep 0))

;; Readback neutral variable
(test "readback neutral var" 'x (readback (V-neutral (N-var 'x)) 0))
(test "readback neutral level" 'x5
      (readback (V-neutral (N-var 5)) 0))

;;; ====
;;; Normalization Tests
;;; ====
(test-section "Normalization")

;; Identity function normalization
(test "normalize identity" '(fn (x0) x0)
      (nbe-type-normalize-closed '(fn (x) x)))

;; Beta reduction: ((λx.x) 42) → 42
(test "normalize identity applied" 42
      (nbe-type-normalize-closed '((fn (x) x) 42)))

;; Constant function: ((λx.λy.x) 1 2) → 1
(test "normalize const function" 1
      (nbe-type-normalize-closed '((fn (x) ((fn (y) x) 2)) 1)))

;; Nested lambda normalization
(test "normalize nested lambda" '(fn (x0) (fn (x1) x0))
      (nbe-type-normalize-closed '(fn (x) (fn (y) x))))

;; Type normalization
(test "normalize simple type" 'Type (nbe-type-normalize-closed 'Type))
(test "normalize arrow type" '(-> Int Bool)
      (nbe-type-normalize-closed '(-> Int Bool)))

;; Normalize pair projections
(test "normalize fst (pair a b)" 'a
      (nbe-type-normalize-closed '(fst (pair a b))))
(test "normalize snd (pair a b)" 'b
      (nbe-type-normalize-closed '(snd (pair a b))))

;; Normalize type-level computation
(test "normalize type-level add" 7
      (nbe-type-normalize-closed '(+ 3 4)))
(test "normalize type-level conditional" 'Int
      (nbe-type-normalize-closed '(if #t Int Bool)))

;; Normalize with environment
(define env-with-x (env-extend nbe-empty-env 'x (V-base 10)))
(test "normalize with env" 10 (nbe-type-normalize 'x env-with-x))
(test "normalize computation with env" 15
      (nbe-type-normalize '(+ x 5) env-with-x))

;;; ====
;;; Alpha Equivalence Tests
;;; ====
(test-section "Alpha Equivalence (via Conversion)")

;; Same term, different variable names should be equal
(define lam1-val (nbe-eval '(fn (x) x) nbe-empty-env))
(define lam2-val (nbe-eval '(fn (y) y) nbe-empty-env))
(test-true "alpha equivalent lambdas" (convert? lam1-val lam2-val 0))

;; Different terms should not be equal
(define lam3-val (nbe-eval '(fn (x) (fn (y) x)) nbe-empty-env))
(define lam4-val (nbe-eval '(fn (x) (fn (y) y)) nbe-empty-env))
(test "non-equivalent lambdas" #f (convert? lam3-val lam4-val 0))

;; Eta equivalence: λx.(f x) ≡ f when x not free in f
(define eta1 (nbe-eval '(fn (x) (f x)) nbe-empty-env))
(define eta2 (nbe-eval 'f nbe-empty-env))
(test-true "eta equivalence" (convert? eta1 eta2 0))

;;; ====
;;; Conversion Checking Tests
;;; ====
(test-section "Conversion Checking")

;; Type universes
(test-true "Type ≡ Type" (convert? (V-type 0) (V-type 0) 0))
(test "Type ≢ (Type 1)" #f (convert? (V-type 0) (V-type 1) 0))

;; Base values
(test-true "42 ≡ 42" (convert? (V-base 42) (V-base 42) 0))
(test "42 ≢ 43" #f (convert? (V-base 42) (V-base 43) 0))

;; Pi types: domains and codomains must match
(define pi1-val (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool))))
(define pi2-val (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool))))
(define pi3-val (V-pi (V-base 'Int) (make-const-closure (V-base 'String))))
(test-true "equivalent Pi types" (convert? pi1-val pi2-val 0))
(test "non-equivalent Pi types" #f (convert? pi1-val pi3-val 0))

;; Sigma types
(define sig1-val (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool))))
(define sig2-val (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool))))
(test-true "equivalent Sigma types" (convert? sig1-val sig2-val 0))

;; Pairs: component-wise comparison
(define pair1-val (V-pair (V-base 1) (V-base 2)))
(define pair2-val (V-pair (V-base 1) (V-base 2)))
(define pair3-val (V-pair (V-base 1) (V-base 3)))
(test-true "equivalent pairs" (convert? pair1-val pair2-val 0))
(test "non-equivalent pairs" #f (convert? pair1-val pair3-val 0))

;; Vec types
(define vec1-val (V-vec (V-base 3) (V-base 'Int)))
(define vec2-val (V-vec (V-base 3) (V-base 'Int)))
(define vec3-val (V-vec (V-base 4) (V-base 'Int)))
(test-true "equivalent Vec types" (convert? vec1-val vec2-val 0))
(test "non-equivalent Vec types" #f (convert? vec1-val vec3-val 0))

;; Matrix types
(define mat1-val (V-matrix (V-base 2) (V-base 3) (V-base 'Int)))
(define mat2-val (V-matrix (V-base 2) (V-base 3) (V-base 'Int)))
(define mat3-val (V-matrix (V-base 2) (V-base 4) (V-base 'Int)))
(test-true "equivalent Matrix types" (convert? mat1-val mat2-val 0))
(test "non-equivalent Matrix types" #f (convert? mat1-val mat3-val 0))

;; Neutral variables
(define nvar1 (V-neutral (N-var 'x)))
(define nvar2 (V-neutral (N-var 'x)))
(define nvar3 (V-neutral (N-var 'y)))
(test-true "equivalent neutral vars" (convert? nvar1 nvar2 0))
(test "non-equivalent neutral vars" #f (convert? nvar1 nvar3 0))

;; Neutral applications
(define napp1 (V-neutral (N-app (N-var 'f) (V-base 1))))
(define napp2 (V-neutral (N-app (N-var 'f) (V-base 1))))
(define napp3 (V-neutral (N-app (N-var 'f) (V-base 2))))
(test-true "equivalent neutral apps" (convert-neutral? (V-neutral-term napp1) (V-neutral-term napp2) 0))
(test "non-equivalent neutral apps" #f (convert-neutral? (V-neutral-term napp1) (V-neutral-term napp3) 0))

;;; ====
;;; Type Equality Tests
;;; ====
(test-section "Type Equality")

;; Convenience function
(test-true "types-equal? Int Int" (types-equal? 'Int 'Int))
(test "types-equal? Int Bool" #f (types-equal? 'Int 'Bool))

;; Arrow types
(test-true "types-equal? arrows"
           (types-equal? '(-> Int Bool) '(-> Int Bool)))
(test "types-equal? different arrows" #f
      (types-equal? '(-> Int Bool) '(-> Bool Int)))

;; Product types
(test-true "types-equal? products"
           (types-equal? '(× Int Bool) '(× Int Bool)))

;; Vec types with computation
(test-true "types-equal? computed Vec"
           (types-equal? '(Vec (+ 2 3) Int) '(Vec 5 Int)))

;; Normal form convenience
(test "type-nf identity" '(-> Int Int) (type-nf '(-> Int Int)))
(test "type-nf computation" 7 (type-nf '(+ 3 4)))

;;; ====
;;; Kind Tests
;;; ====
(test-section "Kinds")

;; Kind value construction
(test-true "KV-star? recognizes *" (KV-star? (KV-star)))
(test-true "KV-constraint? recognizes Constraint" (KV-constraint? (KV-constraint)))
(test-true "KV-row? recognizes Row" (KV-row? (KV-row)))

(define ksort (KV-sort 0))
(test-true "KV-sort? recognizes sort" (KV-sort? ksort))
(test "KV-sort-level extracts level" 0 (KV-sort-level ksort))

;; Kind arrow
(define karrow (KV-arrow (KV-star) (make-kind-const-closure (KV-star))))
(test-true "KV-arrow? recognizes arrow" (KV-arrow? karrow))

;; Kind Pi
(define kpi (KV-pi (KV-star) (make-kind-const-closure (KV-star))))
(test-true "KV-pi? recognizes pi" (KV-pi? kpi))

;; Kind neutral
(define kneutral (KV-neutral (KN-var 'k)))
(test-true "KV-neutral? recognizes neutral" (KV-neutral? kneutral))

;; Kind evaluation
(test "eval-kind *" (KV-star) (eval-kind '* kind-empty-env))
(test "eval-kind Constraint" (KV-constraint) (eval-kind 'Constraint kind-empty-env))
(test "eval-kind Row" (KV-row) (eval-kind 'Row kind-empty-env))
(test "eval-kind □" (KV-sort 0) (eval-kind '□ kind-empty-env))
(test "eval-kind (□ 2)" (KV-sort 2) (eval-kind '(□ 2) kind-empty-env))

;; Kind arrow evaluation
(define karrow-val (eval-kind '(⇒ * *) kind-empty-env))
(test-true "eval-kind arrow creates KV-arrow" (KV-arrow? karrow-val))

;; Kind readback
(test "kind-readback *" '* (kind-readback 0 (KV-star)))
(test "kind-readback Constraint" 'Constraint (kind-readback 0 (KV-constraint)))
(test "kind-readback Row" 'Row (kind-readback 0 (KV-row)))
(test "kind-readback □" '□ (kind-readback 0 (KV-sort 0)))
(test "kind-readback (□ 3)" '(□ 3) (kind-readback 0 (KV-sort 3)))

;; Kind normalization
(test "kind-normalize-closed *" '* (kind-normalize-closed '*))
(test "kind-normalize-closed arrow" '(⇒ * *)
      (kind-normalize-closed '(⇒ * *)))

;; Kind equivalence
(test-true "kind-equiv? * *" (kind-equiv? 0 (KV-star) (KV-star)))
(test "kind-equiv? * Constraint" #f
      (kind-equiv? 0 (KV-star) (KV-constraint)))

;; Kind equality convenience
(test-true "kinds-equal? * *" (kinds-equal? '* '*))
(test "kinds-equal? * Constraint" #f (kinds-equal? '* 'Constraint))
(test-true "kinds-equal? arrows" (kinds-equal? '(⇒ * *) '(⇒ * *)))

;; Kind normal form
(test "kind-nf *" '* (kind-nf '*))
(test "kind-nf arrow" '(⇒ * *) (kind-nf '(⇒ * *)))

;;; ====
;;; Complex Normalization Tests
;;; ====
(test-section "Complex Normalization")

;; Church numerals normalization
(define church-0 '(fn (f) (fn (x) x)))
(define church-0-nf (nbe-type-normalize-closed church-0))
(test "normalize church 0" '(fn (x0) (fn (x1) x1)) church-0-nf)

;; Compose function
(define compose '(fn (f) (fn (g) (fn (x) (f (g x))))))
(define compose-nf (nbe-type-normalize-closed compose))
(test "normalize compose starts with fn" 'fn (car compose-nf))

;; Y combinator: Cannot normalize - causes infinite expansion
;; This is expected: Y combinator has no normal form in call-by-value
;; Just verify it evaluates to a lambda value (don't try to read back)
(define y-comb '(fn (f) ((fn (x) (f (x x))) (fn (x) (f (x x))))))
(define y-val (nbe-eval y-comb nbe-empty-env))
(test-true "Y combinator evaluates to lambda" (V-lam? y-val))

;; Dependent type example: (n : Nat) → Vec n Int
(define dep-fn-type '(Π ((n : Nat)) (Vec n Int)))
(define dep-fn-nf (nbe-type-normalize-closed dep-fn-type))
(test "normalize dependent function type starts with Π" 'Π (car dep-fn-nf))

;; Dependent pair type: (n : Nat) × Vec n Int
(define dep-pair-type '(Σ ((n : Nat)) (Vec n Int)))
(define dep-pair-nf (nbe-type-normalize-closed dep-pair-type))
(test "normalize dependent pair type starts with Σ" 'Σ (car dep-pair-nf))

;; Type-level function application in types
(define vec-computed '(Vec (+ 2 3) Int))
(test "normalize Vec with type-level computation" '(Vec 5 Int)
      (nbe-type-normalize-closed vec-computed))

;; Matrix with computed dimensions (use Int - a base type)
(define mat-computed '(Matrix (* 2 3) (+ 4 1) Int))
(test "normalize Matrix with computations" '(Matrix 6 5 Int)
      (nbe-type-normalize-closed mat-computed))

;; Conditional type
(define cond-type '(if (< 3 5) Int Bool))
(test "normalize conditional type" 'Int (nbe-type-normalize-closed cond-type))

;;; ====
;;; Edge Cases and Stress Tests
;;; ====
(test-section "Edge Cases")

;; Empty environment lookups
(define free-var (nbe-eval 'z nbe-empty-env))
(test-true "free variable becomes neutral" (V-neutral? free-var))

;; Multiple projections
(test "nested fst" '(fst (fst p))
      (nbe-type-normalize-closed '(fst (fst p))))

;; Application to neutral
(define app-neutral (nbe-eval '(x 1 2 3) nbe-empty-env))
(test-true "application to neutral stays neutral" (V-neutral? app-neutral))

;; Deeply nested lambdas
(define nested-3 '(fn (a) (fn (b) (fn (c) a))))
(define nested-3-nf (nbe-type-normalize-closed nested-3))
(test "deeply nested lambda normalizes" '(fn (x0) (fn (x1) (fn (x2) x0)))
      nested-3-nf)

;; Type-level computation that stays stuck
(define stuck-vec '(Vec x Int))
(define stuck-vec-nf (nbe-type-normalize-closed stuck-vec))
(test "stuck Vec stays structured" 'Vec (car stuck-vec-nf))

;; Arithmetic with mixed concrete/symbolic
(define mixed-add '(+ 5 x))
(define mixed-nf (nbe-eval mixed-add nbe-empty-env))
(test-true "mixed arithmetic becomes type-prim" (V-type-prim? mixed-nf))

;; Fresh name generation
(test "fresh-name 0" 'x0 (fresh-name 0))
(test "fresh-name 5" 'x5 (fresh-name 5))
(test "fresh-name 100" 'x100 (fresh-name 100))

;; Kind fresh names
(test "kind-fresh-name 0" 'k0 (kind-fresh-name 0))
(test "kind-fresh-name 7" 'k7 (kind-fresh-name 7))

;; Level to name conversion
(test "level->name number" 'x3 (level->name 3))
(test "level->name symbol" 'foo (level->name 'foo))

;; Kind level to name
(test "kind-level->name number" 'k4 (kind-level->name 4))
(test "kind-level->name symbol" 'kappa (kind-level->name 'kappa))

;;; ====
;;; Summary
;;; ====

(newline)
(display "✓ All NBE tests complete.
")
