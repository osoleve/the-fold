;;; Test harness for core/lang/nbe.ss — Normalization by Evaluation
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/lang/nbe.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group value-construction
  (define-test "V-lam? recognizes lambda"
    (let ([lam1 (V-lam 'x 'x '())])
      (assert-true (V-lam? lam1))))

  (define-test "V-lam-param extracts parameter"
    (let ([lam1 (V-lam 'x 'x '())])
      (assert-equal 'x (V-lam-param lam1))))

  (define-test "V-lam-body extracts body"
    (let ([lam1 (V-lam 'x 'x '())])
      (assert-equal 'x (V-lam-body lam1))))

  (define-test "V-pi? recognizes pi type"
    (let ([pi1 (V-pi (V-base 'Int) (make-const-closure (V-base 'Int)))])
      (assert-true (V-pi? pi1))))

  (define-test "V-sigma? recognizes sigma type"
    (let ([sigma1 (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool)))])
      (assert-true (V-sigma? sigma1))))

  (define-test "V-type? recognizes universe"
    (let ([type0 (V-type 0)])
      (assert-true (V-type? type0))))

  (define-test "V-type-level extracts level"
    (let ([type0 (V-type 0)])
      (assert-equal 0 (V-type-level type0))))

  (define-test "V-pair? recognizes pair"
    (let ([pair1 (V-pair (V-base 1) (V-base 2))])
      (assert-true (V-pair? pair1))))

  (define-test "V-pair-fst extracts first"
    (let ([pair1 (V-pair (V-base 1) (V-base 2))])
      (assert-equal (V-base 1) (V-pair-fst pair1))))

  (define-test "V-pair-snd extracts second"
    (let ([pair1 (V-pair (V-base 1) (V-base 2))])
      (assert-equal (V-base 2) (V-pair-snd pair1))))

  (define-test "V-vec? recognizes vector type"
    (let ([vec1 (V-vec (V-base 3) (V-base 'Int))])
      (assert-true (V-vec? vec1))))

  (define-test "V-matrix? recognizes matrix type"
    (let ([mat1 (V-matrix (V-base 2) (V-base 3) (V-base 'Double))])
      (assert-true (V-matrix? mat1))))

  (define-test "V-neutral? recognizes neutral"
    (let ([neutral1 (V-neutral (N-var 'x))])
      (assert-true (V-neutral? neutral1))))

  (define-test "V-base? recognizes base"
    (let ([base1 (V-base 42)])
      (assert-true (V-base? base1))))

  (define-test "V-base-val extracts value"
    (let ([base1 (V-base 42)])
      (assert-equal 42 (V-base-val base1)))))

(test-group neutral-values
  (define-test "N-var? recognizes variable"
    (let ([nvar (N-var 'x)])
      (assert-true (N-var? nvar))))

  (define-test "N-var-level extracts level"
    (let ([nvar (N-var 'x)])
      (assert-equal 'x (N-var-level nvar))))

  (define-test "N-app? recognizes application"
    (let* ([nvar (N-var 'x)]
           [napp (N-app nvar (V-base 1))])
      (assert-true (N-app? napp))))

  (define-test "N-fst? recognizes first projection"
    (let* ([nvar (N-var 'x)]
           [nfst (N-fst nvar)])
      (assert-true (N-fst? nfst))))

  (define-test "N-snd? recognizes second projection"
    (let* ([nvar (N-var 'x)]
           [nsnd (N-snd nvar)])
      (assert-true (N-snd? nsnd)))))

(test-group closures
  (define-test "closure? recognizes closure"
    (let ([clo1 (make-closure 'x 'x '())])
      (assert-true (closure? clo1))))

  (define-test "closure-param extracts parameter"
    (let ([clo1 (make-closure 'x 'x '())])
      (assert-equal 'x (closure-param clo1))))

  (define-test "closure-body extracts body"
    (let ([clo1 (make-closure 'x 'x '())])
      (assert-equal 'x (closure-body clo1))))

  (define-test "const-closure? recognizes const closure"
    (let ([const-clo (make-const-closure (V-base 42))])
      (assert-true (const-closure? const-clo))))

  (define-test "const-closure-value extracts value"
    (let ([const-clo (make-const-closure (V-base 42))])
      (assert-equal (V-base 42) (const-closure-value const-clo))))

  (define-test "apply-closure const returns value"
    (let ([const-clo (make-const-closure (V-base 42))])
      (assert-equal (V-base 42) (apply-closure const-clo (V-base 999))))))

(test-group environments
  (define-test "nbe-empty-env is empty"
    (assert-equal '() nbe-empty-env))

  (define-test "env-extend adds binding"
    (let ([env1 (env-extend nbe-empty-env 'x (V-base 10))])
      (assert-equal '((x . (V-base 10))) env1)))

  (define-test "env-lookup finds binding"
    (let ([env1 (env-extend nbe-empty-env 'x (V-base 10))])
      (assert-equal (V-base 10) (env-lookup env1 'x))))

  (define-test "env-lookup unknown becomes neutral"
    (let ([unknown (env-lookup nbe-empty-env 'y)])
      (assert-true (V-neutral? unknown)))))

(test-group basic-evaluation
  (define-test "eval number"
    (assert-equal (V-base 42) (nbe-eval 42 nbe-empty-env)))

  (define-test "eval boolean true"
    (assert-equal (V-base #t) (nbe-eval #t nbe-empty-env)))

  (define-test "eval boolean false"
    (assert-equal (V-base #f) (nbe-eval #f nbe-empty-env)))

  (define-test "eval string"
    (assert-equal (V-base "hello") (nbe-eval "hello" nbe-empty-env)))

  (define-test "eval Type"
    (assert-equal (V-type 0) (nbe-eval 'Type nbe-empty-env)))

  (define-test "eval (Type 1)"
    (assert-equal (V-type 1) (nbe-eval '(Type 1) nbe-empty-env)))

  (define-test "eval base type Int"
    (assert-equal (V-base 'Int) (nbe-eval 'Int nbe-empty-env)))

  (define-test "eval base type Bool"
    (assert-equal (V-base 'Bool) (nbe-eval 'Bool nbe-empty-env)))

  (define-test "eval free variable becomes neutral"
    (let ([var-result (nbe-eval 'x nbe-empty-env)])
      (assert-true (V-neutral? var-result))))

  (define-test "eval lambda creates V-lam"
    (let ([lam-result (nbe-eval '(fn (x) x) nbe-empty-env)])
      (assert-true (V-lam? lam-result))))

  (define-test "lambda body is preserved"
    (let ([lam-result (nbe-eval '(fn (x) x) nbe-empty-env)])
      (assert-equal 'x (V-lam-body lam-result))))

  (define-test "eval typed lambda creates V-lam"
    (let ([lam-typed (nbe-eval '(λ ((x : Int)) x) nbe-empty-env)])
      (assert-true (V-lam? lam-typed))))

  (define-test "typed lambda extracts param name"
    (let ([lam-typed (nbe-eval '(λ ((x : Int)) x) nbe-empty-env)])
      (assert-equal 'x (V-lam-param lam-typed)))))

(test-group application
  (define-test "apply identity to 42"
    (let ([id-app (nbe-eval '((fn (x) x) 42) nbe-empty-env)])
      (assert-equal (V-base 42) id-app)))

  (define-test "apply const function"
    (let ([const-app (nbe-eval '((fn (x) ((fn (y) x) 999)) 42) nbe-empty-env)])
      (assert-equal (V-base 42) const-app)))

  (define-test "partial application creates lambda"
    (let ([add-app (nbe-eval '((fn (x) (fn (y) (+ x y))) 3) nbe-empty-env)])
      (assert-true (V-lam? add-app))))

  (define-test "apply neutral creates neutral"
    (let ([neutral-app (nbe-apply (V-neutral (N-var 'f)) (V-base 1))])
      (assert-true (V-neutral? neutral-app)))))

(test-group pi-types
  (define-test "arrow creates V-pi"
    (let ([arrow-result (nbe-eval '(-> Int Bool) nbe-empty-env)])
      (assert-true (V-pi? arrow-result))))

  (define-test "arrow domain is Int"
    (let ([arrow-result (nbe-eval '(-> Int Bool) nbe-empty-env)])
      (assert-equal (V-base 'Int) (V-pi-domain arrow-result))))

  (define-test "multi-arrow creates V-pi"
    (let ([arrow3 (nbe-eval '(-> Int Bool String) nbe-empty-env)])
      (assert-true (V-pi? arrow3))))

  (define-test "multi-arrow domain is Int"
    (let ([arrow3 (nbe-eval '(-> Int Bool String) nbe-empty-env)])
      (assert-equal (V-base 'Int) (V-pi-domain arrow3))))

  (define-test "dependent Pi creates V-pi"
    (let ([pi-dep (nbe-eval '(Π ((n : Nat)) (Vec n Int)) nbe-empty-env)])
      (assert-true (V-pi? pi-dep))))

  (define-test "Pi domain"
    (let ([pi-dep (nbe-eval '(Π ((n : Nat)) (Vec n Int)) nbe-empty-env)])
      (assert-equal (V-base 'Nat) (V-pi-domain pi-dep)))))

(test-group sigma-types
  (define-test "product creates V-sigma"
    (let ([product-result (nbe-eval '(× Int Bool) nbe-empty-env)])
      (assert-true (V-sigma? product-result))))

  (define-test "product fst type is Int"
    (let ([product-result (nbe-eval '(× Int Bool) nbe-empty-env)])
      (assert-equal (V-base 'Int) (V-sigma-fst-type product-result))))

  (define-test "multi-product creates V-sigma"
    (let ([product3 (nbe-eval '(× Int Bool String) nbe-empty-env)])
      (assert-true (V-sigma? product3))))

  (define-test "dependent Sigma creates V-sigma"
    (let ([sigma-dep (nbe-eval '(Σ ((n : Nat)) (Vec n Int)) nbe-empty-env)])
      (assert-true (V-sigma? sigma-dep))))

  (define-test "Sigma fst type"
    (let ([sigma-dep (nbe-eval '(Σ ((n : Nat)) (Vec n Int)) nbe-empty-env)])
      (assert-equal (V-base 'Nat) (V-sigma-fst-type sigma-dep)))))

(test-group pairs
  (define-test "pair creates V-pair"
    (let ([pair-val (nbe-eval '(pair 1 2) nbe-empty-env)])
      (assert-true (V-pair? pair-val))))

  (define-test "pair fst is 1"
    (let ([pair-val (nbe-eval '(pair 1 2) nbe-empty-env)])
      (assert-equal (V-base 1) (V-pair-fst pair-val))))

  (define-test "pair snd is 2"
    (let ([pair-val (nbe-eval '(pair 1 2) nbe-empty-env)])
      (assert-equal (V-base 2) (V-pair-snd pair-val))))

  (define-test "fst projection"
    (assert-equal (V-base 1) (nbe-eval '(fst (pair 1 2)) nbe-empty-env)))

  (define-test "snd projection"
    (assert-equal (V-base 2) (nbe-eval '(snd (pair 1 2)) nbe-empty-env)))

  (define-test "fst of neutral is neutral"
    (let ([neutral-pair (V-neutral (N-var 'p))])
      (assert-true (V-neutral? (nbe-fst neutral-pair)))))

  (define-test "snd of neutral is neutral"
    (let ([neutral-pair (V-neutral (N-var 'p))])
      (assert-true (V-neutral? (nbe-snd neutral-pair))))))

(test-group vec-and-matrix-types
  (define-test "Vec creates V-vec"
    (let ([vec-type (nbe-eval '(Vec 5 Int) nbe-empty-env)])
      (assert-true (V-vec? vec-type))))

  (define-test "Vec length"
    (let ([vec-type (nbe-eval '(Vec 5 Int) nbe-empty-env)])
      (assert-equal (V-base 5) (V-vec-n vec-type))))

  (define-test "Vec element type"
    (let ([vec-type (nbe-eval '(Vec 5 Int) nbe-empty-env)])
      (assert-equal (V-base 'Int) (V-vec-A vec-type))))

  (define-test "Matrix creates V-matrix"
    (let ([mat-type (nbe-eval '(Matrix 2 3 Int) nbe-empty-env)])
      (assert-true (V-matrix? mat-type))))

  (define-test "Matrix rows"
    (let ([mat-type (nbe-eval '(Matrix 2 3 Int) nbe-empty-env)])
      (assert-equal (V-base 2) (V-matrix-m mat-type))))

  (define-test "Matrix cols"
    (let ([mat-type (nbe-eval '(Matrix 2 3 Int) nbe-empty-env)])
      (assert-equal (V-base 3) (V-matrix-n mat-type))))

  (define-test "Matrix element type"
    (let ([mat-type (nbe-eval '(Matrix 2 3 Int) nbe-empty-env)])
      (assert-equal (V-base 'Int) (V-matrix-A mat-type)))))

(test-group type-level-computation
  (define-test "type-level add"
    (assert-equal (V-base 5) (nbe-eval '(+ 2 3) nbe-empty-env)))

  (define-test "type-level sub"
    (assert-equal (V-base 7) (nbe-eval '(- 10 3) nbe-empty-env)))

  (define-test "type-level mul"
    (assert-equal (V-base 12) (nbe-eval '(* 3 4) nbe-empty-env)))

  (define-test "type-level = true"
    (assert-equal (V-base #t) (nbe-eval '(= 5 5) nbe-empty-env)))

  (define-test "type-level = false"
    (assert-equal (V-base #f) (nbe-eval '(= 5 6) nbe-empty-env)))

  (define-test "type-level < true"
    (assert-equal (V-base #t) (nbe-eval '(< 3 5) nbe-empty-env)))

  (define-test "type-level < false"
    (assert-equal (V-base #f) (nbe-eval '(< 5 3) nbe-empty-env)))

  (define-test "type-level > true"
    (assert-equal (V-base #t) (nbe-eval '(> 5 3) nbe-empty-env)))

  (define-test "if true branch"
    (assert-equal (V-base 'Int) (nbe-eval '(if #t Int Bool) nbe-empty-env)))

  (define-test "if false branch"
    (assert-equal (V-base 'Bool) (nbe-eval '(if #f Int Bool) nbe-empty-env)))

  (define-test "if with neutral condition creates V-type-prim"
    (let ([stuck-if (nbe-eval '(if x Int Bool) nbe-empty-env)])
      (assert-true (V-type-prim? stuck-if))))

  (define-test "add with neutral creates V-type-prim"
    (let ([stuck-add (nbe-eval '(+ x 3) nbe-empty-env)])
      (assert-true (V-type-prim? stuck-add)))))

(test-group readback
  (define-test "readback number"
    (assert-equal 42 (readback (V-base 42) 0)))

  (define-test "readback boolean"
    (assert-equal #t (readback (V-base #t) 0)))

  (define-test "readback string"
    (assert-equal "hello" (readback (V-base "hello") 0)))

  (define-test "readback Type"
    (assert-equal 'Type (readback (V-type 0) 0)))

  (define-test "readback (Type 2)"
    (assert-equal '(Type 2) (readback (V-type 2) 0)))

  (define-test "readback lambda form"
    (let ([lam-nf (readback (V-lam 'x 'x nbe-empty-env) 0)])
      (assert-equal 'fn (car lam-nf))))

  (define-test "readback lambda param"
    (let ([lam-nf (readback (V-lam 'x 'x nbe-empty-env) 0)])
      (assert-equal '(x0) (cadr lam-nf))))

  (define-test "readback pair"
    (assert-equal '(pair 1 2)
                  (readback (V-pair (V-base 1) (V-base 2)) 0)))

  (define-test "readback Vec"
    (assert-equal '(Vec 3 Int)
                  (readback (V-vec (V-base 3) (V-base 'Int)) 0)))

  (define-test "readback Matrix"
    (assert-equal '(Matrix 2 3 Double)
                  (readback (V-matrix (V-base 2) (V-base 3) (V-base 'Double)) 0)))

  (define-test "readback non-dependent Pi"
    (let ([pi-nondep (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))])
      (assert-equal '(-> Int Bool) (readback pi-nondep 0))))

  (define-test "readback non-dependent Sigma"
    (let ([sigma-nondep (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool)))])
      (assert-equal '(× Int Bool) (readback sigma-nondep 0))))

  (define-test "readback neutral var"
    (assert-equal 'x (readback (V-neutral (N-var 'x)) 0)))

  (define-test "readback neutral level"
    (assert-equal 'x5 (readback (V-neutral (N-var 5)) 0))))

(test-group normalization
  (define-test "normalize identity"
    (assert-equal '(fn (x0) x0)
                  (nbe-type-normalize-closed '(fn (x) x))))

  (define-test "normalize identity applied"
    (assert-equal 42
                  (nbe-type-normalize-closed '((fn (x) x) 42))))

  (define-test "normalize const function"
    (assert-equal 1
                  (nbe-type-normalize-closed '((fn (x) ((fn (y) x) 2)) 1))))

  (define-test "normalize nested lambda"
    (assert-equal '(fn (x0) (fn (x1) x0))
                  (nbe-type-normalize-closed '(fn (x) (fn (y) x)))))

  (define-test "normalize simple type"
    (assert-equal 'Type (nbe-type-normalize-closed 'Type)))

  (define-test "normalize arrow type"
    (assert-equal '(-> Int Bool)
                  (nbe-type-normalize-closed '(-> Int Bool))))

  (define-test "normalize fst (pair a b)"
    (assert-equal 'a
                  (nbe-type-normalize-closed '(fst (pair a b)))))

  (define-test "normalize snd (pair a b)"
    (assert-equal 'b
                  (nbe-type-normalize-closed '(snd (pair a b)))))

  (define-test "normalize type-level add"
    (assert-equal 7
                  (nbe-type-normalize-closed '(+ 3 4))))

  (define-test "normalize type-level conditional"
    (assert-equal 'Int
                  (nbe-type-normalize-closed '(if #t Int Bool))))

  (define-test "normalize with env"
    (let ([env-with-x (env-extend nbe-empty-env 'x (V-base 10))])
      (assert-equal 10 (nbe-type-normalize 'x env-with-x))))

  (define-test "normalize computation with env"
    (let ([env-with-x (env-extend nbe-empty-env 'x (V-base 10))])
      (assert-equal 15 (nbe-type-normalize '(+ x 5) env-with-x)))))

(test-group alpha-equivalence
  (define-test "alpha equivalent lambdas"
    (let ([lam1-val (nbe-eval '(fn (x) x) nbe-empty-env)]
          [lam2-val (nbe-eval '(fn (y) y) nbe-empty-env)])
      (assert-true (convert? lam1-val lam2-val 0))))

  (define-test "non-equivalent lambdas"
    (let ([lam3-val (nbe-eval '(fn (x) (fn (y) x)) nbe-empty-env)]
          [lam4-val (nbe-eval '(fn (x) (fn (y) y)) nbe-empty-env)])
      (assert-false (convert? lam3-val lam4-val 0))))

  (define-test "eta equivalence"
    (let ([eta1 (nbe-eval '(fn (x) (f x)) nbe-empty-env)]
          [eta2 (nbe-eval 'f nbe-empty-env)])
      (assert-true (convert? eta1 eta2 0)))))

(test-group conversion-checking
  (define-test "Type ≡ Type"
    (assert-true (convert? (V-type 0) (V-type 0) 0)))

  (define-test "Type ≢ (Type 1)"
    (assert-false (convert? (V-type 0) (V-type 1) 0)))

  (define-test "42 ≡ 42"
    (assert-true (convert? (V-base 42) (V-base 42) 0)))

  (define-test "42 ≢ 43"
    (assert-false (convert? (V-base 42) (V-base 43) 0)))

  (define-test "equivalent Pi types"
    (let ([pi1-val (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))]
          [pi2-val (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))])
      (assert-true (convert? pi1-val pi2-val 0))))

  (define-test "non-equivalent Pi types"
    (let ([pi1-val (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))]
          [pi3-val (V-pi (V-base 'Int) (make-const-closure (V-base 'String)))])
      (assert-false (convert? pi1-val pi3-val 0))))

  (define-test "equivalent Sigma types"
    (let ([sig1-val (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool)))]
          [sig2-val (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool)))])
      (assert-true (convert? sig1-val sig2-val 0))))

  (define-test "equivalent pairs"
    (let ([pair1-val (V-pair (V-base 1) (V-base 2))]
          [pair2-val (V-pair (V-base 1) (V-base 2))])
      (assert-true (convert? pair1-val pair2-val 0))))

  (define-test "non-equivalent pairs"
    (let ([pair1-val (V-pair (V-base 1) (V-base 2))]
          [pair3-val (V-pair (V-base 1) (V-base 3))])
      (assert-false (convert? pair1-val pair3-val 0))))

  (define-test "equivalent Vec types"
    (let ([vec1-val (V-vec (V-base 3) (V-base 'Int))]
          [vec2-val (V-vec (V-base 3) (V-base 'Int))])
      (assert-true (convert? vec1-val vec2-val 0))))

  (define-test "non-equivalent Vec types"
    (let ([vec1-val (V-vec (V-base 3) (V-base 'Int))]
          [vec3-val (V-vec (V-base 4) (V-base 'Int))])
      (assert-false (convert? vec1-val vec3-val 0))))

  (define-test "equivalent Matrix types"
    (let ([mat1-val (V-matrix (V-base 2) (V-base 3) (V-base 'Int))]
          [mat2-val (V-matrix (V-base 2) (V-base 3) (V-base 'Int))])
      (assert-true (convert? mat1-val mat2-val 0))))

  (define-test "non-equivalent Matrix types"
    (let ([mat1-val (V-matrix (V-base 2) (V-base 3) (V-base 'Int))]
          [mat3-val (V-matrix (V-base 2) (V-base 4) (V-base 'Int))])
      (assert-false (convert? mat1-val mat3-val 0))))

  (define-test "equivalent neutral vars"
    (let ([nvar1 (V-neutral (N-var 'x))]
          [nvar2 (V-neutral (N-var 'x))])
      (assert-true (convert? nvar1 nvar2 0))))

  (define-test "non-equivalent neutral vars"
    (let ([nvar1 (V-neutral (N-var 'x))]
          [nvar3 (V-neutral (N-var 'y))])
      (assert-false (convert? nvar1 nvar3 0))))

  (define-test "equivalent neutral apps"
    (let ([napp1 (V-neutral (N-app (N-var 'f) (V-base 1)))]
          [napp2 (V-neutral (N-app (N-var 'f) (V-base 1)))])
      (assert-true (convert-neutral? (V-neutral-term napp1) (V-neutral-term napp2) 0))))

  (define-test "non-equivalent neutral apps"
    (let ([napp1 (V-neutral (N-app (N-var 'f) (V-base 1)))]
          [napp3 (V-neutral (N-app (N-var 'f) (V-base 2)))])
      (assert-false (convert-neutral? (V-neutral-term napp1) (V-neutral-term napp3) 0)))))

(test-group type-equality
  (define-test "types-equal? Int Int"
    (assert-true (types-equal? 'Int 'Int)))

  (define-test "types-equal? Int Bool"
    (assert-false (types-equal? 'Int 'Bool)))

  (define-test "types-equal? arrows"
    (assert-true (types-equal? '(-> Int Bool) '(-> Int Bool))))

  (define-test "types-equal? different arrows"
    (assert-false (types-equal? '(-> Int Bool) '(-> Bool Int))))

  (define-test "types-equal? products"
    (assert-true (types-equal? '(× Int Bool) '(× Int Bool))))

  (define-test "types-equal? computed Vec"
    (assert-true (types-equal? '(Vec (+ 2 3) Int) '(Vec 5 Int))))

  (define-test "type-nf identity"
    (assert-equal '(-> Int Int) (type-nf '(-> Int Int))))

  (define-test "type-nf computation"
    (assert-equal 7 (type-nf '(+ 3 4)))))

(test-group kinds
  (define-test "KV-star? recognizes *"
    (assert-true (KV-star? (KV-star))))

  (define-test "KV-constraint? recognizes Constraint"
    (assert-true (KV-constraint? (KV-constraint))))

  (define-test "KV-row? recognizes Row"
    (assert-true (KV-row? (KV-row))))

  (define-test "KV-sort? recognizes sort"
    (let ([ksort (KV-sort 0)])
      (assert-true (KV-sort? ksort))))

  (define-test "KV-sort-level extracts level"
    (let ([ksort (KV-sort 0)])
      (assert-equal 0 (KV-sort-level ksort))))

  (define-test "KV-arrow? recognizes arrow"
    (let ([karrow (KV-arrow (KV-star) (make-kind-const-closure (KV-star)))])
      (assert-true (KV-arrow? karrow))))

  (define-test "KV-pi? recognizes pi"
    (let ([kpi (KV-pi (KV-star) (make-kind-const-closure (KV-star)))])
      (assert-true (KV-pi? kpi))))

  (define-test "KV-neutral? recognizes neutral"
    (let ([kneutral (KV-neutral (KN-var 'k))])
      (assert-true (KV-neutral? kneutral))))

  (define-test "eval-kind *"
    (assert-equal (KV-star) (eval-kind '* kind-empty-env)))

  (define-test "eval-kind Constraint"
    (assert-equal (KV-constraint) (eval-kind 'Constraint kind-empty-env)))

  (define-test "eval-kind Row"
    (assert-equal (KV-row) (eval-kind 'Row kind-empty-env)))

  (define-test "eval-kind □"
    (assert-equal (KV-sort 0) (eval-kind '□ kind-empty-env)))

  (define-test "eval-kind (□ 2)"
    (assert-equal (KV-sort 2) (eval-kind '(□ 2) kind-empty-env)))

  (define-test "eval-kind arrow creates KV-arrow"
    (let ([karrow-val (eval-kind '(⇒ * *) kind-empty-env)])
      (assert-true (KV-arrow? karrow-val))))

  (define-test "kind-readback *"
    (assert-equal '* (kind-readback 0 (KV-star))))

  (define-test "kind-readback Constraint"
    (assert-equal 'Constraint (kind-readback 0 (KV-constraint))))

  (define-test "kind-readback Row"
    (assert-equal 'Row (kind-readback 0 (KV-row))))

  (define-test "kind-readback □"
    (assert-equal '□ (kind-readback 0 (KV-sort 0))))

  (define-test "kind-readback (□ 3)"
    (assert-equal '(□ 3) (kind-readback 0 (KV-sort 3))))

  (define-test "kind-normalize-closed *"
    (assert-equal '* (kind-normalize-closed '*)))

  (define-test "kind-normalize-closed arrow"
    (assert-equal '(⇒ * *) (kind-normalize-closed '(⇒ * *))))

  (define-test "kind-equiv? * *"
    (assert-true (kind-equiv? 0 (KV-star) (KV-star))))

  (define-test "kind-equiv? * Constraint"
    (assert-false (kind-equiv? 0 (KV-star) (KV-constraint))))

  (define-test "kinds-equal? * *"
    (assert-true (kinds-equal? '* '*)))

  (define-test "kinds-equal? * Constraint"
    (assert-false (kinds-equal? '* 'Constraint)))

  (define-test "kinds-equal? arrows"
    (assert-true (kinds-equal? '(⇒ * *) '(⇒ * *))))

  (define-test "kind-nf *"
    (assert-equal '* (kind-nf '*)))

  (define-test "kind-nf arrow"
    (assert-equal '(⇒ * *) (kind-nf '(⇒ * *)))))

(test-group complex-normalization
  (define-test "normalize church 0"
    (let ([church-0 '(fn (f) (fn (x) x))])
      (assert-equal '(fn (x0) (fn (x1) x1)) (nbe-type-normalize-closed church-0))))

  (define-test "normalize compose starts with fn"
    (let* ([compose '(fn (f) (fn (g) (fn (x) (f (g x)))))]
           [compose-nf (nbe-type-normalize-closed compose)])
      (assert-equal 'fn (car compose-nf))))

  (define-test "Y combinator evaluates to lambda"
    (let* ([y-comb '(fn (f) ((fn (x) (f (x x))) (fn (x) (f (x x)))))]
           [y-val (nbe-eval y-comb nbe-empty-env)])
      (assert-true (V-lam? y-val))))

  (define-test "normalize dependent function type starts with Π"
    (let ([dep-fn-nf (nbe-type-normalize-closed '(Π ((n : Nat)) (Vec n Int)))])
      (assert-equal 'Π (car dep-fn-nf))))

  (define-test "normalize dependent pair type starts with Σ"
    (let ([dep-pair-nf (nbe-type-normalize-closed '(Σ ((n : Nat)) (Vec n Int)))])
      (assert-equal 'Σ (car dep-pair-nf))))

  (define-test "normalize Vec with type-level computation"
    (assert-equal '(Vec 5 Int)
                  (nbe-type-normalize-closed '(Vec (+ 2 3) Int))))

  (define-test "normalize Matrix with computations"
    (assert-equal '(Matrix 6 5 Int)
                  (nbe-type-normalize-closed '(Matrix (* 2 3) (+ 4 1) Int))))

  (define-test "normalize conditional type"
    (assert-equal 'Int
                  (nbe-type-normalize-closed '(if (< 3 5) Int Bool)))))

(test-group edge-cases
  (define-test "free variable becomes neutral"
    (let ([free-var (nbe-eval 'z nbe-empty-env)])
      (assert-true (V-neutral? free-var))))

  (define-test "nested fst"
    (assert-equal '(fst (fst p))
                  (nbe-type-normalize-closed '(fst (fst p)))))

  (define-test "application to neutral stays neutral"
    (let ([app-neutral (nbe-eval '(x 1 2 3) nbe-empty-env)])
      (assert-true (V-neutral? app-neutral))))

  (define-test "deeply nested lambda normalizes"
    (assert-equal '(fn (x0) (fn (x1) (fn (x2) x0)))
                  (nbe-type-normalize-closed '(fn (a) (fn (b) (fn (c) a))))))

  (define-test "stuck Vec stays structured"
    (let ([stuck-vec-nf (nbe-type-normalize-closed '(Vec x Int))])
      (assert-equal 'Vec (car stuck-vec-nf))))

  (define-test "mixed arithmetic becomes type-prim"
    (let ([mixed-nf (nbe-eval '(+ 5 x) nbe-empty-env)])
      (assert-true (V-type-prim? mixed-nf))))

  (define-test "fresh-name 0"
    (assert-equal 'x0 (fresh-name 0)))

  (define-test "fresh-name 5"
    (assert-equal 'x5 (fresh-name 5)))

  (define-test "fresh-name 100"
    (assert-equal 'x100 (fresh-name 100)))

  (define-test "kind-fresh-name 0"
    (assert-equal 'k0 (kind-fresh-name 0)))

  (define-test "kind-fresh-name 7"
    (assert-equal 'k7 (kind-fresh-name 7)))

  (define-test "level->name number"
    (assert-equal 'x3 (level->name 3)))

  (define-test "level->name symbol"
    (assert-equal 'foo (level->name 'foo)))

  (define-test "kind-level->name number"
    (assert-equal 'k4 (kind-level->name 4)))

  (define-test "kind-level->name symbol"
    (assert-equal 'kappa (kind-level->name 'kappa))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
