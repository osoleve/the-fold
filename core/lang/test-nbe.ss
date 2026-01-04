;;; core/lang/test-nbe.ss — Tests for Normalization by Evaluation
;;;
;;; Comprehensive tests for NbE-based dependent type checking.

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
  (display "\n=== Normalization by Evaluation Test Suite ===\n\n")
  
  ;;; ============================================================
  ;;; Value Constructors and Predicates
  ;;; ============================================================
  
  (display "--- Value Constructors ---\n")
  
  ;; V-lam
  (let ([lam (V-lam 'x '(+ x 1) '())])
       (test-true "V-lam?: lambda is V-lam" (V-lam? lam))
       (test "V-lam-param: extracts param" 'x (V-lam-param lam))
       (test "V-lam-body: extracts body" '(+ x 1) (V-lam-body lam))
       (test "V-lam-env: extracts env" '() (V-lam-env lam)))
  
  ;; V-pi
  (let* ([domain (V-base 'Int)]
         [codomain-clo (make-const-closure (V-base 'Bool))]
         [pi (V-pi domain codomain-clo)])
        (test-true "V-pi?: pi type is V-pi" (V-pi? pi))
        (test "V-pi-domain: extracts domain" domain (V-pi-domain pi))
        (test "V-pi-codomain-closure: extracts closure" codomain-clo (V-pi-codomain-closure pi)))
  
  ;; V-sigma
  (let* ([fst-type (V-base 'Int)]
         [snd-type-clo (make-const-closure (V-base 'Bool))]
         [sigma (V-sigma fst-type snd-type-clo)])
        (test-true "V-sigma?: sigma type is V-sigma" (V-sigma? sigma))
        (test "V-sigma-fst-type: extracts first type" fst-type (V-sigma-fst-type sigma))
        (test "V-sigma-snd-type-closure: extracts closure" snd-type-clo (V-sigma-snd-type-closure sigma)))
  
  ;; V-type
  (let ([t0 (V-type 0)]
        [t1 (V-type 1)])
       (test-true "V-type?: Type is V-type" (V-type? t0))
       (test "V-type-level: level 0" 0 (V-type-level t0))
       (test "V-type-level: level 1" 1 (V-type-level t1)))
  
  ;; V-pair
  (let ([p (V-pair (V-base 1) (V-base 2))])
       (test-true "V-pair?: pair is V-pair" (V-pair? p))
       (test "V-pair-fst: extracts first" (V-base 1) (V-pair-fst p))
       (test "V-pair-snd: extracts second" (V-base 2) (V-pair-snd p)))
  
  ;; V-vec
  (let ([v (V-vec (V-base 3) (V-base 'Int))])
       (test-true "V-vec?: vec is V-vec" (V-vec? v))
       (test "V-vec-n: extracts length" (V-base 3) (V-vec-n v))
       (test "V-vec-A: extracts element type" (V-base 'Int) (V-vec-A v)))
  
  ;; V-matrix
  (let ([m (V-matrix (V-base 2) (V-base 3) (V-base 'Float))])
       (test-true "V-matrix?: matrix is V-matrix" (V-matrix? m))
       (test "V-matrix-m: extracts rows" (V-base 2) (V-matrix-m m))
       (test "V-matrix-n: extracts cols" (V-base 3) (V-matrix-n m))
       (test "V-matrix-A: extracts element type" (V-base 'Float) (V-matrix-A m)))
  
  ;; V-neutral
  (let ([n (V-neutral (N-var 0))])
       (test-true "V-neutral?: neutral is V-neutral" (V-neutral? n))
       (test "V-neutral-term: extracts neutral term" (N-var 0) (V-neutral-term n)))
  
  ;; V-base
  (let ([b (V-base 42)])
       (test-true "V-base?: base is V-base" (V-base? b))
       (test "V-base-val: extracts value" 42 (V-base-val b)))
  
  ;;; ============================================================
  ;;; Neutral Value Constructors
  ;;; ============================================================
  
  (display "\n--- Neutral Value Constructors ---\n")
  
  ;; N-var
  (let ([v (N-var 5)])
       (test-true "N-var?: is N-var" (N-var? v))
       (test "N-var-level: extracts level" 5 (N-var-level v)))
  
  ;; N-app
  (let ([app (N-app (N-var 0) (V-base 1))])
       (test-true "N-app?: is N-app" (N-app? app)))
  
  ;; N-fst
  (let ([fst (N-fst (N-var 0))])
       (test-true "N-fst?: is N-fst" (N-fst? fst)))
  
  ;; N-snd
  (let ([snd (N-snd (N-var 0))])
       (test-true "N-snd?: is N-snd" (N-snd? snd)))
  
  ;;; ============================================================
  ;;; Closures
  ;;; ============================================================
  
  (display "\n--- Closures ---\n")
  
  ;; Regular closure
  (let ([clo (make-closure 'x '(+ x 1) '())])
       (test-true "closure?: is closure" (closure? clo))
       (test "closure-param: extracts param" 'x (closure-param clo))
       (test "closure-body: extracts body" '(+ x 1) (closure-body clo))
       (test "closure-env: extracts env" '() (closure-env clo)))
  
  ;; Constant closure
  (let ([clo (make-const-closure (V-base 42))])
       (test-true "const-closure?: is const-closure" (const-closure? clo))
       (test "const-closure-value: extracts value" (V-base 42) (const-closure-value clo)))
  
  ;; Apply constant closure
  (let ([clo (make-const-closure (V-base 42))])
       (test "apply-closure const: ignores argument"
             (V-base 42) (apply-closure clo (V-base 999))))
  
  ;;; ============================================================
  ;;; Environments
  ;;; ============================================================
  
  (display "\n--- Environments ---\n")
  
  (test "nbe-empty-env: is empty" '() nbe-empty-env)
  
  (let* ([env1 (env-extend nbe-empty-env 'x (V-base 1))]
         [env2 (env-extend env1 'y (V-base 2))])
        (test "env-lookup: finds x" (V-base 1) (env-lookup env2 'x))
        (test "env-lookup: finds y" (V-base 2) (env-lookup env2 'y))
        ;; Unknown var becomes neutral
        (test-true "env-lookup: unknown is neutral"
                   (V-neutral? (env-lookup env2 'z))))
  
  ;;; ============================================================
  ;;; Basic Evaluation
  ;;; ============================================================
  
  (display "\n--- Basic Evaluation ---\n")
  
  ;; Literals
  (test "nbe-eval: number" (V-base 42) (nbe-eval 42 nbe-empty-env))
  (test "nbe-eval: boolean #t" (V-base #t) (nbe-eval #t nbe-empty-env))
  (test "nbe-eval: boolean #f" (V-base #f) (nbe-eval #f nbe-empty-env))
  (test "nbe-eval: string" (V-base "hello") (nbe-eval "hello" nbe-empty-env))
  
  ;; Type
  (test "nbe-eval: Type" (V-type 0) (nbe-eval 'Type nbe-empty-env))
  (test "nbe-eval: (Type 1)" (V-type 1) (nbe-eval '(Type 1) nbe-empty-env))
  
  ;; Base types
  (test-true "nbe-eval: Int is base" (V-base? (nbe-eval 'Int nbe-empty-env)))
  (test-true "nbe-eval: Bool is base" (V-base? (nbe-eval 'Bool nbe-empty-env)))
  
  ;; Variables
  (let ([env (env-extend nbe-empty-env 'x (V-base 42))])
       (test "nbe-eval: variable lookup" (V-base 42) (nbe-eval 'x env)))
  
  ;;; ============================================================
  ;;; Lambda Evaluation
  ;;; ============================================================
  
  (display "\n--- Lambda Evaluation ---\n")
  
  ;; Simple lambda
  (let ([lam (nbe-eval '(fn (x) x) nbe-empty-env)])
       (test-true "nbe-eval: lambda is V-lam" (V-lam? lam))
       (test "nbe-eval: lambda param" 'x (V-lam-param lam))
       (test "nbe-eval: lambda body" 'x (V-lam-body lam)))
  
  ;; Lambda with typed param
  (let ([lam (nbe-eval '(λ ((x : Int)) x) nbe-empty-env)])
       (test-true "nbe-eval: typed lambda is V-lam" (V-lam? lam))
       (test "nbe-eval: typed lambda param" 'x (V-lam-param lam)))
  
  ;;; ============================================================
  ;;; Application
  ;;; ============================================================
  
  (display "\n--- Application ---\n")
  
  ;; Apply identity
  (let ([result (nbe-eval '((fn (x) x) 42) nbe-empty-env)])
       (test "nbe-eval: identity application" (V-base 42) result))
  
  ;; Apply to neutral (produces neutral)
  (let* ([env (env-extend nbe-empty-env 'f (V-neutral (N-var 0)))]
         [result (nbe-eval '(f 1) env)])
        (test-true "nbe-eval: application to neutral" (V-neutral? result)))
  
  ;;; ============================================================
  ;;; Pi Types
  ;;; ============================================================
  
  (display "\n--- Pi Types ---\n")
  
  ;; Simple Pi type
  (let ([pi (nbe-eval '(Π ((x : Int)) Bool) nbe-empty-env)])
       (test-true "nbe-eval: Pi type is V-pi" (V-pi? pi))
       (test-true "nbe-eval: Pi domain is base" (V-base? (V-pi-domain pi))))
  
  ;; Arrow type (non-dependent)
  (let ([arrow (nbe-eval '(-> Int Bool) nbe-empty-env)])
       (test-true "nbe-eval: arrow is V-pi" (V-pi? arrow))
       (test-true "nbe-eval: arrow domain is base" (V-base? (V-pi-domain arrow))))
  
  ;; Multi-arg arrow
  (let ([arrow (nbe-eval '(-> Int Int Bool) nbe-empty-env)])
       (test-true "nbe-eval: multi-arrow is V-pi" (V-pi? arrow)))
  
  ;;; ============================================================
  ;;; Sigma Types
  ;;; ============================================================
  
  (display "\n--- Sigma Types ---\n")
  
  ;; Simple Sigma type
  (let ([sigma (nbe-eval '(Σ ((x : Int)) Bool) nbe-empty-env)])
       (test-true "nbe-eval: Sigma type is V-sigma" (V-sigma? sigma))
       (test-true "nbe-eval: Sigma first type is base" (V-base? (V-sigma-fst-type sigma))))
  
  ;; Product type (non-dependent)
  (let ([prod (nbe-eval '(× Int Bool) nbe-empty-env)])
       (test-true "nbe-eval: product is V-sigma" (V-sigma? prod)))
  
  ;;; ============================================================
  ;;; Pairs
  ;;; ============================================================
  
  (display "\n--- Pairs ---\n")
  
  ;; Pair construction
  (let ([p (nbe-eval '(pair 1 2) nbe-empty-env)])
       (test-true "nbe-eval: pair is V-pair" (V-pair? p))
       (test "nbe-eval: pair first" (V-base 1) (V-pair-fst p))
       (test "nbe-eval: pair second" (V-base 2) (V-pair-snd p)))
  
  ;; fst projection
  (let ([result (nbe-eval '(fst (pair 1 2)) nbe-empty-env)])
       (test "nbe-eval: fst projection" (V-base 1) result))
  
  ;; snd projection
  (let ([result (nbe-eval '(snd (pair 1 2)) nbe-empty-env)])
       (test "nbe-eval: snd projection" (V-base 2) result))
  
  ;; fst of neutral
  (let* ([env (env-extend nbe-empty-env 'p (V-neutral (N-var 0)))]
         [result (nbe-eval '(fst p) env)])
        (test-true "nbe-eval: fst of neutral is neutral" (V-neutral? result)))
  
  ;;; ============================================================
  ;;; Vec and Matrix Types
  ;;; ============================================================
  
  (display "\n--- Vec and Matrix Types ---\n")
  
  ;; Vec type
  (let ([v (nbe-eval '(Vec 3 Int) nbe-empty-env)])
       (test-true "nbe-eval: Vec is V-vec" (V-vec? v))
       (test "nbe-eval: Vec length" (V-base 3) (V-vec-n v)))
  
  ;; Matrix type
  (let ([m (nbe-eval '(Matrix 2 3 Float) nbe-empty-env)])
       (test-true "nbe-eval: Matrix is V-matrix" (V-matrix? m))
       (test "nbe-eval: Matrix rows" (V-base 2) (V-matrix-m m))
       (test "nbe-eval: Matrix cols" (V-base 3) (V-matrix-n m)))
  
  ;;; ============================================================
  ;;; Type-Level Computation
  ;;; ============================================================
  
  (display "\n--- Type-Level Computation ---\n")
  
  ;; Type-level addition
  (let ([result (nbe-eval '(+ 2 3) nbe-empty-env)])
       (test "type-level +: 2 + 3 = 5" (V-base 5) result))
  
  ;; Type-level subtraction
  (let ([result (nbe-eval '(- 10 3) nbe-empty-env)])
       (test "type-level -: 10 - 3 = 7" (V-base 7) result))
  
  ;; Type-level multiplication
  (let ([result (nbe-eval '(* 4 5) nbe-empty-env)])
       (test "type-level *: 4 * 5 = 20" (V-base 20) result))
  
  ;; Type-level comparison
  (let ([result (nbe-eval '(< 3 5) nbe-empty-env)])
       (test "type-level <: 3 < 5" (V-base #t) result))
  
  (let ([result (nbe-eval '(> 3 5) nbe-empty-env)])
       (test "type-level >: 3 > 5" (V-base #f) result))
  
  (let ([result (nbe-eval '(= 5 5) nbe-empty-env)])
       (test "type-level =: 5 = 5" (V-base #t) result))
  
  ;; Type-level if (true branch)
  (let ([result (nbe-eval '(if #t Int Bool) nbe-empty-env)])
       (test-true "type-level if: true branch is base" (V-base? result))
       (test "type-level if: true branch" 'Int (V-base-val result)))
  
  ;; Type-level if (false branch)
  (let ([result (nbe-eval '(if #f Int Bool) nbe-empty-env)])
       (test-true "type-level if: false branch is base" (V-base? result))
       (test "type-level if: false branch" 'Bool (V-base-val result)))
  
  ;;; ============================================================
  ;;; Readback
  ;;; ============================================================
  
  (display "\n--- Readback ---\n")
  
  ;; Readback base value
  (test "readback: number" 42 (readback (V-base 42) 0))
  (test "readback: boolean" #t (readback (V-base #t) 0))
  (test "readback: symbol" 'Int (readback (V-base 'Int) 0))
  
  ;; Readback Type
  (test "readback: Type level 0" 'Type (readback (V-type 0) 0))
  (test "readback: Type level 1" '(Type 1) (readback (V-type 1) 0))
  
  ;; Readback pair
  (test "readback: pair" '(pair 1 2) (readback (V-pair (V-base 1) (V-base 2)) 0))
  
  ;; Readback Vec
  (test "readback: Vec" '(Vec 3 Int)
        (readback (V-vec (V-base 3) (V-base 'Int)) 0))
  
  ;; Readback Matrix
  (test "readback: Matrix" '(Matrix 2 3 Float)
        (readback (V-matrix (V-base 2) (V-base 3) (V-base 'Float)) 0))
  
  ;; Readback neutral variable
  (test "readback: neutral var" 'x0 (readback (V-neutral (N-var 0)) 0))
  (test "readback: neutral var level 1" 'x1 (readback (V-neutral (N-var 1)) 0))
  
  ;; Readback lambda (η-expansion)
  (let* ([id-lam (V-lam 'x 'x nbe-empty-env)]
         [nf (readback id-lam 0)])
        (test "readback: lambda structure" 'fn (car nf)))
  
  ;; Readback non-dependent arrow
  (let* ([arrow (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))]
         [nf (readback arrow 0)])
        (test "readback: arrow uses ->" '-> (car nf)))
  
  ;;; ============================================================
  ;;; Normalization
  ;;; ============================================================
  
  (display "\n--- Normalization ---\n")
  
  ;; Normalize identity application
  (test "normalize: identity app" 42 (normalize-closed '((fn (x) x) 42)))
  
  ;; Normalize nested application
  (test "normalize: nested app" 42
        (normalize-closed '((fn (x) ((fn (y) y) x)) 42)))
  
  ;; Normalize pair projection
  (test "normalize: fst (pair 1 2)" 1 (normalize-closed '(fst (pair 1 2))))
  (test "normalize: snd (pair 1 2)" 2 (normalize-closed '(snd (pair 1 2))))
  
  ;; Normalize type-level arithmetic
  (test "normalize: (+ 2 3)" 5 (normalize-closed '(+ 2 3)))
  (test "normalize: (* 3 (+ 1 2))" 9 (normalize-closed '(* 3 (+ 1 2))))
  
  ;; Normalize conditional
  (test "normalize: if true" 'Int (normalize-closed '(if #t Int Bool)))
  (test "normalize: if false" 'Bool (normalize-closed '(if #f Int Bool)))
  
  ;;; ============================================================
  ;;; Conversion Checking
  ;;; ============================================================
  
  (display "\n--- Conversion Checking ---\n")
  
  ;; Base values
  (test-true "convert?: equal numbers"
             (convert? (V-base 42) (V-base 42) 0))
  (test-false "convert?: different numbers"
              (convert? (V-base 42) (V-base 43) 0))
  
  ;; Type values
  (test-true "convert?: equal Type levels"
             (convert? (V-type 0) (V-type 0) 0))
  (test-false "convert?: different Type levels"
              (convert? (V-type 0) (V-type 1) 0))
  
  ;; Pairs
  (test-true "convert?: equal pairs"
             (convert? (V-pair (V-base 1) (V-base 2))
                       (V-pair (V-base 1) (V-base 2)) 0))
  (test-false "convert?: different pairs"
              (convert? (V-pair (V-base 1) (V-base 2))
                        (V-pair (V-base 1) (V-base 3)) 0))
  
  ;; Vec types
  (test-true "convert?: equal Vec types"
             (convert? (V-vec (V-base 3) (V-base 'Int))
                       (V-vec (V-base 3) (V-base 'Int)) 0))
  (test-false "convert?: different Vec lengths"
              (convert? (V-vec (V-base 3) (V-base 'Int))
                        (V-vec (V-base 4) (V-base 'Int)) 0))
  
  ;; Matrix types
  (test-true "convert?: equal Matrix types"
             (convert? (V-matrix (V-base 2) (V-base 3) (V-base 'Float))
                       (V-matrix (V-base 2) (V-base 3) (V-base 'Float)) 0))
  (test-false "convert?: different Matrix dims"
              (convert? (V-matrix (V-base 2) (V-base 3) (V-base 'Float))
                        (V-matrix (V-base 3) (V-base 3) (V-base 'Float)) 0))
  
  ;; Neutral values
  (test-true "convert?: same neutral var"
             (convert? (V-neutral (N-var 0)) (V-neutral (N-var 0)) 0))
  (test-false "convert?: different neutral vars"
              (convert? (V-neutral (N-var 0)) (V-neutral (N-var 1)) 0))
  
  ;; Lambda η-equality (both lambdas that apply the same way)
  (let ([lam1 (V-lam 'x 'x nbe-empty-env)]
        [lam2 (V-lam 'y 'y nbe-empty-env)])
       (test-true "convert?: alpha-equivalent lambdas"
                  (convert? lam1 lam2 0)))
  
  ;; Pi types
  (let ([pi1 (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))]
        [pi2 (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))])
       (test-true "convert?: equal Pi types"
                  (convert? pi1 pi2 0)))
  
  (let ([pi1 (V-pi (V-base 'Int) (make-const-closure (V-base 'Bool)))]
        [pi2 (V-pi (V-base 'Int) (make-const-closure (V-base 'Int)))])
       (test-false "convert?: different Pi codomains"
                   (convert? pi1 pi2 0)))
  
  ;; Sigma types
  (let ([sigma1 (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool)))]
        [sigma2 (V-sigma (V-base 'Int) (make-const-closure (V-base 'Bool)))])
       (test-true "convert?: equal Sigma types"
                  (convert? sigma1 sigma2 0)))
  
  ;;; ============================================================
  ;;; types-equal? Convenience Function
  ;;; ============================================================
  
  (display "\n--- types-equal? ---\n")
  
  (test-true "types-equal?: Int = Int"
             (types-equal? 'Int 'Int))
  
  (test-false "types-equal?: Int ≠ Bool"
              (types-equal? 'Int 'Bool))
  
  (test-true "types-equal?: (-> Int Bool) = (-> Int Bool)"
             (types-equal? '(-> Int Bool) '(-> Int Bool)))
  
  (test-false "types-equal?: (-> Int Bool) ≠ (-> Bool Int)"
              (types-equal? '(-> Int Bool) '(-> Bool Int)))
  
  (test-true "types-equal?: Type = Type"
             (types-equal? 'Type 'Type))
  
  ;; Normalization makes these equal
  (test-true "types-equal?: (+ 2 3) = 5"
             (types-equal? '(+ 2 3) 5))
  
  (test-true "types-equal?: (Vec (+ 1 2) Int) = (Vec 3 Int)"
             (types-equal? '(Vec (+ 1 2) Int) '(Vec 3 Int)))
  
  ;;; ============================================================
  ;;; Fresh Names
  ;;; ============================================================
  
  (display "\n--- Fresh Names ---\n")
  
  (test "fresh-name: level 0" 'x0 (fresh-name 0))
  (test "fresh-name: level 5" 'x5 (fresh-name 5))
  (test "fresh-name: level 10" 'x10 (fresh-name 10))
  
  (test "level->name: numeric" 'x3 (level->name 3))
  (test "level->name: symbol passthrough" 'foo (level->name 'foo))
  
  ;;; ============================================================
  ;;; mentions-var?
  ;;; ============================================================
  
  (display "\n--- mentions-var? ---\n")
  
  (test-true "mentions-var?: symbol matches" (mentions-var? 'x 'x))
  (test-false "mentions-var?: symbol doesn't match" (mentions-var? 'x 'y))
  (test-true "mentions-var?: in list" (mentions-var? '(+ x 1) 'x))
  (test-false "mentions-var?: not in list" (mentions-var? '(+ y 1) 'x))
  (test-true "mentions-var?: nested" (mentions-var? '((fn (y) x) z) 'x))
  (test-false "mentions-var?: number" (mentions-var? 42 'x))
  
  ;;; ============================================================
  ;;; Kind Value Constructors
  ;;; ============================================================
  
  (display "\n--- Kind Value Constructors ---\n")
  
  ;; KV-star
  (let ([k (KV-star)])
       (test-true "KV-star?: is KV-star" (KV-star? k)))
  
  ;; KV-constraint
  (let ([k (KV-constraint)])
       (test-true "KV-constraint?: is KV-constraint" (KV-constraint? k)))
  
  ;; KV-row
  (let ([k (KV-row)])
       (test-true "KV-row?: is KV-row" (KV-row? k)))
  
  ;; KV-sort
  (let ([k (KV-sort 0)])
       (test-true "KV-sort?: is KV-sort" (KV-sort? k))
       (test "KV-sort-level: extracts level" 0 (KV-sort-level k)))
  
  ;; KV-arrow
  (let ([k (KV-arrow (KV-star) (make-kind-const-closure (KV-star)))])
       (test-true "KV-arrow?: is KV-arrow" (KV-arrow? k))
       (test-true "KV-arrow-domain: is KV-star" (KV-star? (KV-arrow-domain k))))
  
  ;; KV-pi
  (let ([k (KV-pi (KV-star) (make-kind-const-closure (KV-star)))])
       (test-true "KV-pi?: is KV-pi" (KV-pi? k)))
  
  ;; KV-neutral
  (let ([k (KV-neutral (KN-var 0))])
       (test-true "KV-neutral?: is KV-neutral" (KV-neutral? k)))
  
  ;;; ============================================================
  ;;; Kind Neutral Constructors
  ;;; ============================================================
  
  (display "\n--- Kind Neutral Constructors ---\n")
  
  (let ([n (KN-var 3)])
       (test-true "KN-var?: is KN-var" (KN-var? n))
       (test "KN-var-level: extracts level" 3 (KN-var-level n)))
  
  (let ([n (KN-app (KN-var 0) (KV-star))])
       (test-true "KN-app?: is KN-app" (KN-app? n)))
  
  ;;; ============================================================
  ;;; Kind Closures
  ;;; ============================================================
  
  (display "\n--- Kind Closures ---\n")
  
  (let ([clo (make-kind-closure 'k '* kind-empty-env)])
       (test-true "kind-closure?: is kind-closure" (kind-closure? clo))
       (test "kind-closure-param: extracts param" 'k (kind-closure-param clo))
       (test "kind-closure-body: extracts body" '* (kind-closure-body clo)))
  
  (let ([clo (make-kind-const-closure (KV-star))])
       (test-true "kind-const-closure?: is kind-const-closure" (kind-const-closure? clo))
       (test-true "kind-const-closure-value: is KV-star"
                  (KV-star? (kind-const-closure-value clo))))
  
  ;; Apply kind constant closure
  (let ([clo (make-kind-const-closure (KV-star))])
       (test-true "apply-kind-closure const: ignores arg"
                  (KV-star? (apply-kind-closure clo (KV-sort 0)))))
  
  ;;; ============================================================
  ;;; Kind Evaluation
  ;;; ============================================================
  
  (display "\n--- Kind Evaluation ---\n")
  
  (test-true "eval-kind: * is KV-star" (KV-star? (eval-kind '* kind-empty-env)))
  (test-true "eval-kind: Constraint is KV-constraint"
             (KV-constraint? (eval-kind 'Constraint kind-empty-env)))
  (test-true "eval-kind: Row is KV-row"
             (KV-row? (eval-kind 'Row kind-empty-env)))
  (test-true "eval-kind: □ is KV-sort" (KV-sort? (eval-kind '□ kind-empty-env)))
  (test "eval-kind: (□ 1) level" 1 (KV-sort-level (eval-kind '(□ 1) kind-empty-env)))
  
  ;; Kind arrow
  (let ([k (eval-kind '(⇒ * *) kind-empty-env)])
       (test-true "eval-kind: arrow is KV-arrow" (KV-arrow? k))
       (test-true "eval-kind: arrow domain is *" (KV-star? (KV-arrow-domain k))))
  
  ;; Kind variable becomes neutral
  (let ([k (eval-kind 'k kind-empty-env)])
       (test-true "eval-kind: unknown var is neutral" (KV-neutral? k)))
  
  ;;; ============================================================
  ;;; Kind Readback
  ;;; ============================================================
  
  (display "\n--- Kind Readback ---\n")
  
  (test "kind-readback: *" '* (kind-readback 0 (KV-star)))
  (test "kind-readback: Constraint" 'Constraint (kind-readback 0 (KV-constraint)))
  (test "kind-readback: Row" 'Row (kind-readback 0 (KV-row)))
  (test "kind-readback: □" '□ (kind-readback 0 (KV-sort 0)))
  (test "kind-readback: (□ 1)" '(□ 1) (kind-readback 0 (KV-sort 1)))
  
  ;; Readback neutral kind variable
  (test "kind-readback: neutral var" 'k0 (kind-readback 0 (KV-neutral (KN-var 0))))
  
  ;;; ============================================================
  ;;; Kind Normalization
  ;;; ============================================================
  
  (display "\n--- Kind Normalization ---\n")
  
  (test "kind-normalize-closed: *" '* (kind-normalize-closed '*))
  (test "kind-normalize-closed: □" '□ (kind-normalize-closed '□))
  
  ;; Arrow normalizes to arrow
  (let ([nf (kind-normalize-closed '(⇒ * *))])
       (test "kind-normalize-closed: (⇒ * *)" '⇒ (car nf)))
  
  ;;; ============================================================
  ;;; Kind Equivalence
  ;;; ============================================================
  
  (display "\n--- Kind Equivalence ---\n")
  
  (test-true "kind-equiv?: * = *"
             (kind-equiv? 0 (KV-star) (KV-star)))
  
  (test-true "kind-equiv?: Constraint = Constraint"
             (kind-equiv? 0 (KV-constraint) (KV-constraint)))
  
  (test-true "kind-equiv?: Row = Row"
             (kind-equiv? 0 (KV-row) (KV-row)))
  
  (test-false "kind-equiv?: * ≠ Constraint"
              (kind-equiv? 0 (KV-star) (KV-constraint)))
  
  (test-true "kind-equiv?: □ = □"
             (kind-equiv? 0 (KV-sort 0) (KV-sort 0)))
  
  (test-false "kind-equiv?: (□ 0) ≠ (□ 1)"
              (kind-equiv? 0 (KV-sort 0) (KV-sort 1)))
  
  ;; Arrow equivalence
  (let ([arr1 (KV-arrow (KV-star) (make-kind-const-closure (KV-star)))]
        [arr2 (KV-arrow (KV-star) (make-kind-const-closure (KV-star)))])
       (test-true "kind-equiv?: equal arrows"
                  (kind-equiv? 0 arr1 arr2)))
  
  ;; Neutral equivalence
  (test-true "kind-equiv?: same neutral var"
             (kind-equiv? 0 (KV-neutral (KN-var 0)) (KV-neutral (KN-var 0))))
  (test-false "kind-equiv?: different neutral vars"
              (kind-equiv? 0 (KV-neutral (KN-var 0)) (KV-neutral (KN-var 1))))
  
  ;;; ============================================================
  ;;; kinds-equal? Convenience Function
  ;;; ============================================================
  
  (display "\n--- kinds-equal? ---\n")
  
  (test-true "kinds-equal?: * = *" (kinds-equal? '* '*))
  (test-false "kinds-equal?: * ≠ □" (kinds-equal? '* '□))
  (test-true "kinds-equal?: (⇒ * *) = (⇒ * *)"
             (kinds-equal? '(⇒ * *) '(⇒ * *)))
  (test-false "kinds-equal?: (⇒ * *) ≠ (⇒ * □)"
              (kinds-equal? '(⇒ * *) '(⇒ * □)))
  
  ;;; ============================================================
  ;;; Kind Fresh Names
  ;;; ============================================================
  
  (display "\n--- Kind Fresh Names ---\n")
  
  (test "kind-fresh-name: level 0" 'k0 (kind-fresh-name 0))
  (test "kind-fresh-name: level 3" 'k3 (kind-fresh-name 3))
  
  (test "kind-level->name: numeric" 'k5 (kind-level->name 5))
  (test "kind-level->name: symbol passthrough" 'kappa (kind-level->name 'kappa))
  
  ;;; ============================================================
  ;;; kind-mentions-var?
  ;;; ============================================================
  
  (display "\n--- kind-mentions-var? ---\n")
  
  (test-true "kind-mentions-var?: symbol matches"
             (kind-mentions-var? 'k 'k))
  (test-false "kind-mentions-var?: symbol doesn't match"
              (kind-mentions-var? 'k 'j))
  (test-true "kind-mentions-var?: in arrow"
             (kind-mentions-var? '(⇒ k *) 'k))
  (test-false "kind-mentions-var?: not in arrow"
              (kind-mentions-var? '(⇒ * *) 'k))
  
  ;;; ============================================================
  ;;; Type-Level Primitive Values
  ;;; ============================================================
  
  (display "\n--- Type-Level Primitives ---\n")
  
  ;; V-type-prim constructor
  (let ([p (V-type-prim '+ (list (V-neutral (N-var 0)) (V-base 1)))])
       (test-true "V-type-prim?: is V-type-prim" (V-type-prim? p))
       (test "V-type-prim-op: extracts op" '+ (V-type-prim-op p))
       (test "V-type-prim-args: extracts args count" 2 (length (V-type-prim-args p))))
  
  ;; type-level-reduce with stuck expressions
  (let ([result (type-level-reduce '+ (list (V-neutral (N-var 0)) (V-base 1)))])
       (test-true "type-level-reduce: stuck + is V-type-prim" (V-type-prim? result)))
  
  ;; type-level-if with stuck condition
  (let ([result (type-level-if (V-neutral (N-var 0)) (V-base 'Int) (V-base 'Bool))])
       (test-true "type-level-if: stuck condition is V-type-prim" (V-type-prim? result)))
  
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
