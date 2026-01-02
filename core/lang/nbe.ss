;;; fabric/stitches/nbe.ss — Normalization by Evaluation
;;;
;;; The heart of dependent type checking.
;;;
;;; NbE works by:
;;;   1. Evaluating expressions to semantic values (closures, neutrals)
;;;   2. Reading values back to normal form expressions
;;;   3. Comparing normal forms for definitional equality
;;;
;;; This enables checking if two types are equal by normalizing
;;; and comparing structurally.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;
;;; References:
;;;   - "Checking Dependent Types with NbE" (Christiansen)
;;;   - "Mini-TT" (Coquand et al.)

(load "core/base/prelude.ss")
(load "core/types/types.ss")

;;; ============================================================
;;; Semantic Values
;;; ============================================================

;;; Values are the semantic domain for NbE.
;;; They represent fully evaluated (normalized) expressions.

;;; Value tags:
;;;   V-lam      ; Closure (λ with captured environment)
;;;   V-pi       ; Pi type value
;;;   V-sigma    ; Sigma type value
;;;   V-type     ; Universe value
;;;   V-pair     ; Pair value
;;;   V-neutral  ; Stuck computation (variable + eliminations)
;;;   V-base     ; Base type or base value (Int, Bool, etc.)

;;; Value representation: (tag payload)
;;; or for closures: (tag param body env)

;;; V-lam : Symbol × Expr × Env → Value
;;; A lambda closure captures its environment.
(define (V-lam param body env)
  `(V-lam ,param ,body ,env))

(define (V-lam? v)
  (and (pair? v) (eq? (car v) 'V-lam)))

(define (V-lam-param v) (cadr v))
(define (V-lam-body v) (caddr v))
(define (V-lam-env v) (cadddr v))

;;; V-pi : Value × Closure → Value
;;; A Pi type: domain is a Value, codomain is a closure.
;;; Closure takes a Value and produces the codomain Value.
(define (V-pi domain codomain-closure)
  `(V-pi ,domain ,codomain-closure))

(define (V-pi? v)
  (and (pair? v) (eq? (car v) 'V-pi)))

(define (V-pi-domain v) (cadr v))
(define (V-pi-codomain-closure v) (caddr v))

;;; V-sigma : Value × Closure → Value
;;; A Sigma type: first type is a Value, second is a closure.
(define (V-sigma fst-type snd-type-closure)
  `(V-sigma ,fst-type ,snd-type-closure))

(define (V-sigma? v)
  (and (pair? v) (eq? (car v) 'V-sigma)))

(define (V-sigma-fst-type v) (cadr v))
(define (V-sigma-snd-type-closure v) (caddr v))

;;; V-type : Nat → Value
;;; A universe at level n.
(define (V-type level)
  `(V-type ,level))

(define (V-type? v)
  (and (pair? v) (eq? (car v) 'V-type)))

(define (V-type-level v)
  (if (V-type? v) (cadr v) 0))

;;; V-pair : Value × Value → Value
;;; A dependent pair value.
(define (V-pair fst snd)
  `(V-pair ,fst ,snd))

(define (V-pair? v)
  (and (pair? v) (eq? (car v) 'V-pair)))

;;; V-pair-fst v) (cadr v))
(define (V-pair-snd v) (caddr v))

;;; V-vec : Value × Value → Value
;;; A length-indexed vector type: (Vec n A).
(define (V-vec n A)
  `(V-vec ,n ,A))

(define (V-vec? v)
  (and (pair? v) (eq? (car v) 'V-vec)))

(define (V-vec-n v) (cadr v))
(define (V-vec-A v) (caddr v))

;;; V-matrix : Value × Value × Value → Value
;;; A dimension-indexed matrix type: (Matrix m n A).
(define (V-matrix m n A)
  `(V-matrix ,m ,n ,A))

(define (V-matrix? v)
  (and (pair? v) (eq? (car v) 'V-matrix)))

(define (V-matrix-m v) (cadr v))
(define (V-matrix-n v) (caddr v))
(define (V-matrix-A v) (cadddr v))

;;; V-neutral : Neutral → Value
;;; A stuck computation that cannot reduce further.
(define (V-neutral neutral)
  `(V-neutral ,neutral))

(define (V-neutral? v)
  (and (pair? v) (eq? (car v) 'V-neutral)))

(define (V-neutral-term v) (cadr v))

;;; V-base : Any → Value
;;; Base values (integers, booleans, symbols, etc.)
(define (V-base val)
  `(V-base ,val))

(define (V-base? v)
  (and (pair? v) (eq? (car v) 'V-base)))

(define (V-base-val v) (cadr v))

;;; ============================================================
;;; Neutral Values
;;; ============================================================

;;; A neutral value represents a stuck computation:
;;;   - A variable that can't be reduced
;;;   - An application of a neutral to values
;;;   - A projection from a neutral pair
;;;   - A case on a neutral scrutinee

;;; Neutral = (N-var level)
;;;         | (N-app neutral value)
;;;         | (N-fst neutral)
;;;         | (N-snd neutral)
;;;         | (N-case neutral arms)

(define (N-var level)
  `(N-var ,level))

(define (N-var? n)
  (and (pair? n) (eq? (car n) 'N-var)))

(define (N-var-level n) (cadr n))

(define (N-app func arg)
  `(N-app ,func ,arg))

(define (N-app? n)
  (and (pair? n) (eq? (car n) 'N-app)))

(define (N-fst neutral)
  `(N-fst ,neutral))

(define (N-fst? n)
  (and (pair? n) (eq? (car n) 'N-fst)))

(define (N-snd neutral)
  `(N-snd ,neutral))

(define (N-snd? n)
  (and (pair? n) (eq? (car n) 'N-snd)))

;;; ============================================================
;;; Closures
;;; ============================================================

;;; A closure packages an expression with its environment.
;;; When applied to a value, it substitutes and continues evaluation.

(define (make-closure param body env)
  `(closure ,param ,body ,env))

(define (closure? c)
  (and (pair? c) (eq? (car c) 'closure)))

(define (closure-param c) (cadr c))
(define (closure-body c) (caddr c))
(define (closure-env c) (cadddr c))

;;; apply-closure : Closure × Value → Value
;;; Apply a closure to a value, producing a new value.
(define (apply-closure clo val)
  (cond
   ;; Constant closure just returns its stored value
   [(const-closure? clo) (const-closure-value clo)]
   ;; Regular closure evaluates body with extended env
   [else
    (let ([param (closure-param clo)]
          [body (closure-body clo)]
          [env (closure-env clo)])
         (nbe-eval body (env-extend env param val)))]))

;;; Constant closures - for non-dependent types
(define (make-const-closure value)
  `(const-closure ,value))

(define (const-closure? c)
  (and (pair? c) (eq? (car c) 'const-closure)))

(define (const-closure-value c)
  (cadr c))

;;; ============================================================
;;; Environments
;;; ============================================================

;;; NbE environments map de Bruijn levels to values.
;;; We use an alist for simplicity.

(define nbe-empty-env '())

(define (env-extend env name val)
  (cons (cons name val) env))

(define (env-lookup env name)
  (let ([entry (assq name env)])
       (if entry
           (cdr entry)
           ;; Unknown variable becomes a neutral
           (V-neutral (N-var name)))))

;;; ============================================================
;;; Evaluation (Expr → Value)
;;; ============================================================

;;; nbe-eval : Expr × Env → Value
;;; Evaluate an expression to a value in the given environment.
(define (nbe-eval expr env)
  (cond
   ;; Universe Type
   [(eq? expr 'Type) (V-type 0)]
   
   ;; Variables and base types
   [(symbol? expr)
    (if (base-type? expr)
        (V-base expr)
        (env-lookup env expr))]
   
   ;; Literals
   [(number? expr) (V-base expr)]
   [(boolean? expr) (V-base expr)]
   [(string? expr) (V-base expr)]
   
   [(not (pair? expr))
    (V-base expr)]
   
   ;; Universe
   [(eq? (car expr) 'Type)
    (V-type (if (= (length expr) 1) 0 (cadr expr)))]
   
   ;; Lambda: (fn (x) body) or (λ ((x : T)) body)
   [(or (eq? (car expr) 'fn) (eq? (car expr) 'λ))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           ;; Extract param name (handle both (x) and ((x : T)))
           [param (if (pair? (car params))
                      (caar params)
                      (car params))])
          (V-lam param body env))]
   
   ;; Pi type: (Π ((x : A)) B)
   [(eq? (car expr) 'Π)
    (let* ([binding (car (cadr expr))]
           [var (car binding)]
           [domain-expr (caddr binding)]
           [codomain-expr (caddr expr)]
           [domain-val (nbe-eval domain-expr env)]
           [codomain-clo (make-closure var codomain-expr env)])
          (V-pi domain-val codomain-clo))]
   
   ;; Sigma type: (Σ ((x : A)) B)
   [(eq? (car expr) 'Σ)
    (let* ([binding (car (cadr expr))]
           [var (car binding)]
           [fst-type-expr (caddr binding)]
           [snd-type-expr (caddr expr)]
           [fst-type-val (nbe-eval fst-type-expr env)]
           [snd-type-clo (make-closure var snd-type-expr env)])
          (V-sigma fst-type-val snd-type-clo))]
   
   ;; Vector type: (Vec n A)
   [(eq? (car expr) 'Vec)
    (let ([n (nbe-eval (cadr expr) env)]
          [A (nbe-eval (caddr expr) env)])
         (V-vec n A))]
   
   ;; Matrix type: (Matrix m n A)
   [(eq? (car expr) 'Matrix)
    (let ([m (nbe-eval (cadr expr) env)]
          [n (nbe-eval (caddr expr) env)]
          [A (nbe-eval (cadddr expr) env)])
         (V-matrix m n A))]
   
   ;; Pair: (pair a b)
   [(eq? (car expr) 'pair)
    (let ([fst (nbe-eval (cadr expr) env)]
          [snd (nbe-eval (caddr expr) env)])
         (V-pair fst snd))]
   
   ;; First projection: (fst p)
   [(eq? (car expr) 'fst)
    (let ([p (nbe-eval (cadr expr) env)])
         (nbe-fst p))]
   
   ;; Second projection: (snd p)
   [(eq? (car expr) 'snd)
    (let ([p (nbe-eval (cadr expr) env)])
         (nbe-snd p))]
   
   ;; Non-dependent function type: (-> A B)
   [(eq? (car expr) '->)
    (nbe-eval-arrow (cdr expr) env)]
   
   ;; Non-dependent product type: (× A B)
   [(eq? (car expr) '×)
    (nbe-eval-product (cdr expr) env)]
   
   ;; Type-level conditional: (if cond then else)
   [(eq? (car expr) 'if)
    (let ([cond-val (nbe-eval (cadr expr) env)]
          [then-val (nbe-eval (caddr expr) env)]
          [else-val (nbe-eval (cadddr expr) env)])
         (type-level-if cond-val then-val else-val))]
   
   ;; Type-level primitives: (prim op args...)
   [(eq? (car expr) 'prim)
    (let* ([op (cadr (cadr expr))]  ; Extract symbol from quoted
           [args (map (lambda (e) (nbe-eval e env)) (cddr expr))])
          (type-level-reduce op args))]
   
   ;; Type-level arithmetic shorthand: (+ a b), (- a b), (* a b)
   [(memq (car expr) '(+ - * < > = == eq))
    (let ([args (map (lambda (e) (nbe-eval e env)) (cdr expr))])
         (type-level-reduce (car expr) args))]
   
   ;; Application: (f x)
   [else
    (let ([func (nbe-eval (car expr) env)]
          [args (map (lambda (e) (nbe-eval e env)) (cdr expr))])
         (fold-left nbe-apply func args))]))

;;; nbe-apply : Value × Value → Value
;;; Apply a function value to an argument value.
(define (nbe-apply func arg)
  (cond
   [(V-lam? func)
    (let ([param (V-lam-param func)]
          [body (V-lam-body func)]
          [env (V-lam-env func)])
         (nbe-eval body (env-extend env param arg)))]
   [(V-neutral? func)
    (V-neutral (N-app (V-neutral-term func) arg))]
   [else
    ;; Type error, but we continue with neutral
    (V-neutral (N-app func arg))]))

;;; nbe-fst : Value → Value
;;; Project the first component of a pair.
(define (nbe-fst v)
  (cond
   [(V-pair? v) (V-pair-fst v)]
   [(V-neutral? v) (V-neutral (N-fst (V-neutral-term v)))]
   [else (V-neutral (N-fst v))]))

;;; nbe-snd : Value → Value
;;; Project the second component of a pair.
(define (nbe-snd v)
  (cond
   [(V-pair? v) (V-pair-snd v)]
   [(V-neutral? v) (V-neutral (N-snd (V-neutral-term v)))]
   [else (V-neutral (N-snd v))]))

;;; nbe-eval-arrow : (List Expr) × Env → Value
;;; Evaluate a non-dependent arrow type (-> A B C ...).
;;; Desugars to nested Pi types with unused parameter.
(define (nbe-eval-arrow types env)
  (if (= (length types) 2)
      (let ([domain (nbe-eval (car types) env)]
            [codomain (nbe-eval (cadr types) env)])
           ;; Non-dependent: create a constant closure that ignores its argument
           (V-pi domain (make-const-closure codomain)))
      (let ([domain (nbe-eval (car types) env)]
            [rest-expr `(-> ,@(cdr types))])
           (V-pi domain (make-closure '_ rest-expr env)))))

;;; nbe-eval-product : (List Expr) × Env → Value
;;; Evaluate a non-dependent product type (× A B C ...).
(define (nbe-eval-product types env)
  (if (= (length types) 2)
      (let ([fst-type (nbe-eval (car types) env)]
            [snd-type (nbe-eval (cadr types) env)])
           (V-sigma fst-type (make-const-closure snd-type)))
      (let ([fst-type (nbe-eval (car types) env)]
            [rest-expr `(× ,@(cdr types))])
           (V-sigma fst-type (make-closure '_ rest-expr env)))))

;;; ============================================================
;;; Readback (Value → Expr)
;;; ============================================================

;;; readback : Value × Int → Expr
;;; Convert a value back to a normal form expression.
;;; The level tracks how many binders we're under.
(define (readback val level)
  (cond
   [(V-lam? val)
    (let* ([x (fresh-name level)]
           [x-val (V-neutral (N-var level))]
           [body-val (nbe-apply val x-val)]
           [body-nf (readback body-val (+ level 1))])
          `(fn (,x) ,body-nf))]
   
   [(V-pi? val)
    (let* ([x (fresh-name level)]
           [x-val (V-neutral (N-var level))]
           [domain-nf (readback (V-pi-domain val) level)]
           [codomain-val (apply-closure (V-pi-codomain-closure val) x-val)]
           [codomain-nf (readback codomain-val (+ level 1))])
          (if (mentions-var? codomain-nf x)
              ;; Dependent: use Pi syntax
              `(Π ((,x : ,domain-nf)) ,codomain-nf)
              ;; Non-dependent: use arrow syntax
              `(-> ,domain-nf ,codomain-nf)))]
   
   [(V-sigma? val)
    (let* ([x (fresh-name level)]
           [x-val (V-neutral (N-var level))]
           [fst-type-nf (readback (V-sigma-fst-type val) level)]
           [snd-type-val (apply-closure (V-sigma-snd-type-closure val) x-val)]
           [snd-type-nf (readback snd-type-val (+ level 1))])
          (if (mentions-var? snd-type-nf x)
              ;; Dependent: use Sigma syntax
              `(Σ ((,x : ,fst-type-nf)) ,snd-type-nf)
              ;; Non-dependent: use product syntax
              `(× ,fst-type-nf ,snd-type-nf)))]
   
   [(V-vec? val)
    `(Vec ,(readback (V-vec-n val) level)
      ,(readback (V-vec-A val) level))]
   
   [(V-matrix? val)
    `(Matrix ,(readback (V-matrix-m val) level)
      ,(readback (V-matrix-n val) level)
      ,(readback (V-matrix-A val) level))]
   
   [(V-type? val)
    (let ([n (V-type-level val)])
         (if (= n 0) 'Type `(Type ,n)))]
   
   [(V-pair? val)
    `(pair ,(readback (V-pair-fst val) level)
      ,(readback (V-pair-snd val) level))]
   
   [(V-neutral? val)
    (readback-neutral (V-neutral-term val) level)]
   
   [(V-base? val)
    (V-base-val val)]
   
   ;; Type-level primitives: read back as operation
   [(V-type-prim? val)
    (let ([op (V-type-prim-op val)]
          [args (map (lambda (a) (readback a level)) (V-type-prim-args val))])
         (cons op args))]
   
   [else val]))

;;; readback-neutral : Neutral × Int → Expr
(define (readback-neutral neutral level)
  (cond
   [(N-var? neutral)
    (level->name (N-var-level neutral))]
   
   [(N-app? neutral)
    (let ([func-nf (readback-neutral (cadr neutral) level)]
          [arg-nf (readback (caddr neutral) level)])
         `(,func-nf ,arg-nf))]
   
   [(N-fst? neutral)
    `(fst ,(readback-neutral (cadr neutral) level))]
   
   [(N-snd? neutral)
    `(snd ,(readback-neutral (cadr neutral) level))]
   
   [else neutral]))

;;; ============================================================
;;; Fresh Names
;;; ============================================================

;;; fresh-name : Int → Symbol
;;; Generate a fresh variable name from a de Bruijn level.
(define (fresh-name level)
  (string->symbol (string-append "x" (number->string level))))

;;; level->name : Int|Symbol → Symbol
;;; Convert a de Bruijn level back to a name.
;;; If already a symbol (named variable), return as-is.
(define (level->name level)
  (if (symbol? level)
      level
      (string->symbol (string-append "x" (number->string level)))))

;;; mentions-var? : Expr × Symbol → Boolean
;;; Check if an expression mentions a variable (for deciding dependent vs non-dependent).
(define (mentions-var? expr var)
  (cond
   [(symbol? expr) (eq? expr var)]
   [(not (pair? expr)) #f]
   [else (ormap (lambda (e) (mentions-var? e var)) expr)]))

;;; ============================================================
;;; Normalization
;;; ============================================================

;;; normalize : Expr × Env → Expr
;;; Normalize an expression to its normal form.
(define (normalize expr env)
  (readback (nbe-eval expr env) 0))

;;; normalize-closed : Expr → Expr
;;; Normalize a closed expression.
(define (normalize-closed expr)
  (normalize expr nbe-empty-env))

;;; ============================================================
;;; Conversion Checking
;;; ============================================================

;;; convert? : Value × Value × Int → Boolean
;;; Check if two values are definitionally equal.
;;; This is the core of type equality checking.
(define (convert? v1 v2 level)
  (cond
   ;; Both lambdas: η-expand and compare bodies
   [(and (V-lam? v1) (V-lam? v2))
    (let* ([x (V-neutral (N-var level))]
           [b1 (nbe-apply v1 x)]
           [b2 (nbe-apply v2 x)])
          (convert? b1 b2 (+ level 1)))]
   
   ;; Lambda vs non-lambda: η-expand the non-lambda
   [(V-lam? v1)
    (let* ([x (V-neutral (N-var level))]
           [b1 (nbe-apply v1 x)]
           [b2 (nbe-apply v2 x)])  ; This will create N-app if v2 is neutral
          (convert? b1 b2 (+ level 1)))]
   [(V-lam? v2)
    (let* ([x (V-neutral (N-var level))]
           [b1 (nbe-apply v1 x)]
           [b2 (nbe-apply v2 x)])
          (convert? b1 b2 (+ level 1)))]
   
   ;; Pi types: compare domains and codomains
   [(and (V-pi? v1) (V-pi? v2))
    (and (convert? (V-pi-domain v1) (V-pi-domain v2) level)
         (let* ([x (V-neutral (N-var level))]
                [c1 (apply-closure (V-pi-codomain-closure v1) x)]
                [c2 (apply-closure (V-pi-codomain-closure v2) x)])
               (convert? c1 c2 (+ level 1))))]
   
   ;; Sigma types: compare first types and second types
   [(and (V-sigma? v1) (V-sigma? v2))
    (and (convert? (V-sigma-fst-type v1) (V-sigma-fst-type v2) level)
         (let* ([x (V-neutral (N-var level))]
                [c1 (apply-closure (V-sigma-snd-type-closure v1) x)]
                [c2 (apply-closure (V-sigma-snd-type-closure v2) x)])
               (convert? c1 c2 (+ level 1))))]
   
   ;; Universe types: compare levels
   [(and (V-type? v1) (V-type? v2))
    (= (V-type-level v1) (V-type-level v2))]
   
   ;; Vec types: compare length and element type
   [(and (V-vec? v1) (V-vec? v2))
    (and (convert? (V-vec-n v1) (V-vec-n v2) level)
         (convert? (V-vec-A v1) (V-vec-A v2) level))]
   
   ;; Matrix types: compare dimensions and element type
   [(and (V-matrix? v1) (V-matrix? v2))
    (and (convert? (V-matrix-m v1) (V-matrix-m v2) level)
         (convert? (V-matrix-n v1) (V-matrix-n v2) level)
         (convert? (V-matrix-A v1) (V-matrix-A v2) level))]
   
   ;; Pairs: η-expand and compare components
   [(and (V-pair? v1) (V-pair? v2))
    (and (convert? (V-pair-fst v1) (V-pair-fst v2) level)
         (convert? (V-pair-snd v1) (V-pair-snd v2) level))]
   
   ;; Pair vs non-pair: η-expand
   [(V-pair? v1)
    (and (convert? (V-pair-fst v1) (nbe-fst v2) level)
         (convert? (V-pair-snd v1) (nbe-snd v2) level))]
   [(V-pair? v2)
    (and (convert? (nbe-fst v1) (V-pair-fst v2) level)
         (convert? (nbe-snd v1) (V-pair-snd v2) level))]
   
   ;; Neutral values: compare structurally
   [(and (V-neutral? v1) (V-neutral? v2))
    (convert-neutral? (V-neutral-term v1) (V-neutral-term v2) level)]
   
   ;; Base values: compare directly
   [(and (V-base? v1) (V-base? v2))
    (equal? (V-base-val v1) (V-base-val v2))]
   
   ;; Type-level primitives: compare structurally
   [(and (V-type-prim? v1) (V-type-prim? v2))
    (and (eq? (V-type-prim-op v1) (V-type-prim-op v2))
         (= (length (V-type-prim-args v1)) (length (V-type-prim-args v2)))
         (andmap (lambda (a b) (convert? a b level))
                 (V-type-prim-args v1)
                 (V-type-prim-args v2)))]
   
   [else #f]))

;;; convert-neutral? : Neutral × Neutral × Int → Boolean
(define (convert-neutral? n1 n2 level)
  (cond
   [(and (N-var? n1) (N-var? n2))
    ;; Use equal? to handle both numeric and symbolic levels
    (equal? (N-var-level n1) (N-var-level n2))]
   
   [(and (N-app? n1) (N-app? n2))
    (and (convert-neutral? (cadr n1) (cadr n2) level)
         (convert? (caddr n1) (caddr n2) level))]
   
   [(and (N-fst? n1) (N-fst? n2))
    (convert-neutral? (cadr n1) (cadr n2) level)]
   
   [(and (N-snd? n1) (N-snd? n2))
    (convert-neutral? (cadr n1) (cadr n2) level)]
   
   [else #f]))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; types-equal? : Type × Type → Boolean
;;; Check if two types are definitionally equal.
(define (types-equal? t1 t2)
  (let ([v1 (nbe-eval t1 nbe-empty-env)]
        [v2 (nbe-eval t2 nbe-empty-env)])
       (convert? v1 v2 0)))

;;; type-nf : Type → Type
;;; Get the normal form of a type.
(define (type-nf t)
  (normalize-closed t))

;;; ============================================================
;;; Type-Level Computation
;;; ============================================================

;;; The following extends NbE to handle type-level primitives.
;;; These allow computation within types:
;;;   - (+ n m) → computes if n,m are base values
;;;   - (if b A B) → reduces when b is a base boolean
;;;   - (type-fn ...) → type-level functions

;;; V-type-prim : Symbol × (List Value) → Value
;;; A type-level primitive waiting for reduction.
(define (V-type-prim op args)
  `(V-type-prim ,op ,args))

(define (V-type-prim? v)
  (and (pair? v) (eq? (car v) 'V-type-prim)))

(define (V-type-prim-op v) (cadr v))
(define (V-type-prim-args v) (caddr v))

;;; type-level-reduce : Symbol × (List Value) → Value
;;; Try to reduce a type-level primitive.
(define (type-level-reduce op args)
  (case op
        ;; Arithmetic
        [(+)
         (if (and (pair? args)
                  (pair? (cdr args))
                  (V-base? (car args))
                  (V-base? (cadr args))
                  (number? (V-base-val (car args)))
                  (number? (V-base-val (cadr args))))
             (V-base (+ (V-base-val (car args)) (V-base-val (cadr args))))
             (V-type-prim op args))]
        
        [(-)
         (if (and (pair? args)
                  (pair? (cdr args))
                  (V-base? (car args))
                  (V-base? (cadr args))
                  (number? (V-base-val (car args)))
                  (number? (V-base-val (cadr args))))
             (V-base (- (V-base-val (car args)) (V-base-val (cadr args))))
             (V-type-prim op args))]
        
        [(*)
         (if (and (pair? args)
                  (pair? (cdr args))
                  (V-base? (car args))
                  (V-base? (cadr args))
                  (number? (V-base-val (car args)))
                  (number? (V-base-val (cadr args))))
             (V-base (* (V-base-val (car args)) (V-base-val (cadr args))))
             (V-type-prim op args))]
        
        ;; Comparison (for conditional types)
        [(= == eq)
         (if (and (pair? args)
                  (pair? (cdr args))
                  (V-base? (car args))
                  (V-base? (cadr args)))
             (V-base (equal? (V-base-val (car args)) (V-base-val (cadr args))))
             (V-type-prim op args))]
        
        [(<)
         (if (and (pair? args)
                  (pair? (cdr args))
                  (V-base? (car args))
                  (V-base? (cadr args))
                  (number? (V-base-val (car args)))
                  (number? (V-base-val (cadr args))))
             (V-base (< (V-base-val (car args)) (V-base-val (cadr args))))
             (V-type-prim op args))]
        
        [(>)
         (if (and (pair? args)
                  (pair? (cdr args))
                  (V-base? (car args))
                  (V-base? (cadr args))
                  (number? (V-base-val (car args)))
                  (number? (V-base-val (cadr args))))
             (V-base (> (V-base-val (car args)) (V-base-val (cadr args))))
             (V-type-prim op args))]
        
        [else (V-type-prim op args)]))

;;; type-level-if : Value × Value × Value → Value
;;; Reduce a type-level conditional.
(define (type-level-if condition then-val else-val)
  (cond
   [(and (V-base? condition) (eq? (V-base-val condition) #t))
    then-val]
   [(and (V-base? condition) (eq? (V-base-val condition) #f))
    else-val]
   ;; Stuck: condition is not a known boolean
   [else (V-type-prim 'if (list condition then-val else-val))]))

;;; N-type-prim : Symbol × (List Value) → Neutral
;;; A stuck type-level primitive.
(define (N-type-prim op args)
  `(N-type-prim ,op ,args))

(define (N-type-prim? n)
  (and (pair? n) (eq? (car n) 'N-type-prim)))

;;; ============================================================
;;; Kind NbE - Phase 2: Kind Normalization
;;; ============================================================
;;;
;;; Extends NbE to handle kinds. This enables:
;;;   - Normalizing kind expressions
;;;   - Checking kind equivalence
;;;   - Handling dependent kinds (K-pi)
;;;
;;; Kind semantic values mirror type semantic values:
;;;   KV-star    : The kind * as a value
;;;   KV-arrow   : Kind arrow as a closure
;;;   KV-pi      : Dependent kind Πκ as a closure
;;;   KV-sort    : Sort □ (kind of kinds) as a value
;;;   KV-neutral : Stuck kind computation

;;; ============================================================
;;; Kind Semantic Values
;;; ============================================================

;;; KV-star : → KindValue
;;; The base kind * as a semantic value.
(define (KV-star)
  '(KV-star))

(define (KV-star? v)
  (and (pair? v) (eq? (car v) 'KV-star)))

;;; KV-constraint : → KindValue
;;; The Constraint kind as a semantic value.
(define (KV-constraint)
  '(KV-constraint))

(define (KV-constraint? v)
  (and (pair? v) (eq? (car v) 'KV-constraint)))

;;; KV-row : → KindValue
;;; The Row kind as a semantic value.
(define (KV-row)
  '(KV-row))

(define (KV-row? v)
  (and (pair? v) (eq? (car v) 'KV-row)))

;;; KV-arrow : KindValue × KindClosure → KindValue
;;; A kind arrow ⇒. The codomain is a closure to handle potential
;;; dependency (though standard arrows are non-dependent).
(define (KV-arrow domain codomain-clo)
  `(KV-arrow ,domain ,codomain-clo))

(define (KV-arrow? v)
  (and (pair? v) (eq? (car v) 'KV-arrow)))

(define (KV-arrow-domain v) (cadr v))
(define (KV-arrow-codomain-clo v) (caddr v))

;;; KV-pi : KindValue × KindClosure → KindValue
;;; A dependent kind Πκ. The codomain depends on the input.
(define (KV-pi domain codomain-clo)
  `(KV-pi ,domain ,codomain-clo))

(define (KV-pi? v)
  (and (pair? v) (eq? (car v) 'KV-pi)))

(define (KV-pi-domain v) (cadr v))
(define (KV-pi-codomain-clo v) (caddr v))

;;; KV-sort : Nat → KindValue
;;; The sort □ (kind of kinds) at a given level.
(define (KV-sort level)
  `(KV-sort ,level))

(define (KV-sort? v)
  (and (pair? v) (eq? (car v) 'KV-sort)))

(define (KV-sort-level v) (cadr v))

;;; KV-neutral : KindNeutral → KindValue
;;; A stuck kind computation.
(define (KV-neutral neutral)
  `(KV-neutral ,neutral))

(define (KV-neutral? v)
  (and (pair? v) (eq? (car v) 'KV-neutral)))

(define (KV-neutral-term v) (cadr v))

;;; ============================================================
;;; Kind Neutral Values
;;; ============================================================

;;; KN-var : Level → KindNeutral
;;; A kind variable (stuck).
(define (KN-var level)
  `(KN-var ,level))

(define (KN-var? n)
  (and (pair? n) (eq? (car n) 'KN-var)))

(define (KN-var-level n) (cadr n))

;;; KN-app : KindNeutral × KindValue → KindNeutral
;;; Application of a neutral kind to a value.
(define (KN-app func arg)
  `(KN-app ,func ,arg))

(define (KN-app? n)
  (and (pair? n) (eq? (car n) 'KN-app)))

;;; ============================================================
;;; Kind Closures
;;; ============================================================

;;; make-kind-closure : Symbol × KindExpr × KindEnv → KindClosure
(define (make-kind-closure param body env)
  `(kind-closure ,param ,body ,env))

(define (kind-closure? c)
  (and (pair? c) (eq? (car c) 'kind-closure)))

(define (kind-closure-param c) (cadr c))
(define (kind-closure-body c) (caddr c))
(define (kind-closure-env c) (cadddr c))

;;; make-kind-const-closure : KindValue → KindClosure
;;; A constant closure that ignores its argument.
(define (make-kind-const-closure value)
  `(kind-const-closure ,value))

(define (kind-const-closure? c)
  (and (pair? c) (eq? (car c) 'kind-const-closure)))

(define (kind-const-closure-value c) (cadr c))

;;; apply-kind-closure : KindClosure × KindValue → KindValue
;;; Apply a kind closure to a kind value.
(define (apply-kind-closure clo val)
  (cond
   [(kind-const-closure? clo)
    (kind-const-closure-value clo)]
   [(kind-closure? clo)
    (let ([param (kind-closure-param clo)]
          [body (kind-closure-body clo)]
          [env (kind-closure-env clo)])
         (eval-kind body (kind-env-extend env param val)))]
   [else
    (error 'apply-kind-closure "invalid closure" clo)]))

;;; ============================================================
;;; Kind Environments
;;; ============================================================

(define kind-empty-env '())

(define (kind-env-extend env name val)
  (cons (cons name val) env))

(define (kind-env-lookup env name)
  (let ([entry (assq name env)])
       (if entry
           (cdr entry)
           ;; Unknown kind variable becomes a neutral
           (KV-neutral (KN-var name)))))

;;; ============================================================
;;; Kind Evaluation (Kind → KindValue)
;;; ============================================================

;;; eval-kind : Kind × KindEnv → KindValue
;;; Evaluate a kind expression to a kind semantic value.
(define (eval-kind kind env)
  (cond
   ;; Base kind *
   [(eq? kind '*)
    (KV-star)]
   
   ;; Constraint kind
   [(eq? kind 'Constraint)
    (KV-constraint)]
   
   ;; Row kind
   [(eq? kind 'Row)
    (KV-row)]
   
   ;; Sort □ (bare)
   [(eq? kind '□)
    (KV-sort 0)]
   
   ;; Kind variable — look up or become neutral
   [(symbol? kind)
    (kind-env-lookup env kind)]
   
   [(not (pair? kind))
    (error 'eval-kind "invalid kind" kind)]
   
   ;; Leveled sort: (□ n)
   [(eq? (car kind) '□)
    (KV-sort (cadr kind))]
   
   ;; Kind arrow: (⇒ K1 K2)
   [(eq? (car kind) '⇒)
    (let* ([domain-kind (cadr kind)]
           [codomain-kind (caddr kind)]
           [domain-val (eval-kind domain-kind env)]
           ;; Non-dependent: create constant closure
           [codomain-clo (make-kind-const-closure (eval-kind codomain-kind env))])
          (KV-arrow domain-val codomain-clo))]
   
   ;; Dependent kind: (Πκ ((var : domain)) codomain)
   [(eq? (car kind) 'Πκ)
    (let* ([binding (car (cadr kind))]
           [var (car binding)]
           [domain-kind (caddr binding)]
           [codomain-kind (caddr kind)]
           [domain-val (eval-kind domain-kind env)]
           [codomain-clo (make-kind-closure var codomain-kind env)])
          (KV-pi domain-val codomain-clo))]
   
   ;; Kind polymorphism: (κ∀ (vars) body)
   ;; For now, treat as the body (assuming vars are in scope)
   [(eq? (car kind) 'κ∀)
    (let ([vars (cadr kind)]
          [body (caddr kind)])
         ;; Extend env with neutral variables for each bound var
         (let loop ([vs vars] [e env])
              (if (null? vs)
                  (eval-kind body e)
                  (loop (cdr vs)
                        (kind-env-extend e (car vs)
                                         (KV-neutral (KN-var (car vs))))))))]
   
   [else
    (error 'eval-kind "unknown kind form" kind)]))

;;; ============================================================
;;; Kind Readback (KindValue → Kind)
;;; ============================================================

;;; kind-readback : Level × KindValue → Kind
;;; Convert a kind semantic value back to a normal form kind expression.
(define (kind-readback level kval)
  (cond
   [(KV-star? kval)
    '*]
   
   [(KV-constraint? kval)
    'Constraint]
   
   [(KV-row? kval)
    'Row]
   
   [(KV-sort? kval)
    (let ([n (KV-sort-level kval)])
         (if (= n 0) '□ `(□ ,n)))]
   
   [(KV-arrow? kval)
    (let* ([x-name (kind-fresh-name level)]
           [x-val (KV-neutral (KN-var level))]
           [domain-nf (kind-readback level (KV-arrow-domain kval))]
           [codomain-val (apply-kind-closure (KV-arrow-codomain-clo kval) x-val)]
           [codomain-nf (kind-readback (+ level 1) codomain-val)])
          ;; Check if codomain mentions the variable
          (if (kind-mentions-var? codomain-nf x-name)
              ;; Dependent: use Πκ syntax
              `(Πκ ((,x-name : ,domain-nf)) ,codomain-nf)
              ;; Non-dependent: use ⇒ syntax
              `(⇒ ,domain-nf ,codomain-nf)))]
   
   [(KV-pi? kval)
    (let* ([x-name (kind-fresh-name level)]
           [x-val (KV-neutral (KN-var level))]
           [domain-nf (kind-readback level (KV-pi-domain kval))]
           [codomain-val (apply-kind-closure (KV-pi-codomain-clo kval) x-val)]
           [codomain-nf (kind-readback (+ level 1) codomain-val)])
          `(Πκ ((,x-name : ,domain-nf)) ,codomain-nf))]
   
   [(KV-neutral? kval)
    (kind-readback-neutral level (KV-neutral-term kval))]
   
   [else
    (error 'kind-readback "unknown kind value" kval)]))

;;; kind-readback-neutral : Level × KindNeutral → Kind
(define (kind-readback-neutral level neutral)
  (cond
   [(KN-var? neutral)
    (kind-level->name (KN-var-level neutral))]
   
   [(KN-app? neutral)
    (let ([func-nf (kind-readback-neutral level (cadr neutral))]
          [arg-nf (kind-readback level (caddr neutral))])
         `(,func-nf ,arg-nf))]
   
   [else neutral]))

;;; kind-fresh-name : Level → Symbol
;;; Generate a fresh kind variable name from a de Bruijn level.
(define (kind-fresh-name level)
  (string->symbol (string-append "k" (number->string level))))

;;; kind-level->name : Level|Symbol → Symbol
;;; Convert a de Bruijn level to a name (or pass through symbol).
(define (kind-level->name level)
  (if (symbol? level)
      level
      (string->symbol (string-append "k" (number->string level)))))

;;; kind-mentions-var? : Kind × Symbol → Boolean
;;; Check if a kind expression mentions a variable.
(define (kind-mentions-var? kind var)
  (cond
   [(symbol? kind) (eq? kind var)]
   [(not (pair? kind)) #f]
   [else (ormap (lambda (k) (kind-mentions-var? k var)) kind)]))

;;; ============================================================
;;; Kind Normalization
;;; ============================================================

;;; kind-normalize : Kind × KindEnv → Kind
;;; Normalize a kind expression.
(define (kind-normalize kind env)
  (kind-readback 0 (eval-kind kind env)))

;;; kind-normalize-closed : Kind → Kind
;;; Normalize a closed kind expression.
(define (kind-normalize-closed kind)
  (kind-normalize kind kind-empty-env))

;;; ============================================================
;;; Kind Equivalence (Conversion Checking)
;;; ============================================================

;;; kind-equiv? : Level × KindValue × KindValue → Boolean
;;; Check if two kind values are definitionally equal.
(define (kind-equiv? level kv1 kv2)
  (cond
   ;; Both are *
   [(and (KV-star? kv1) (KV-star? kv2))
    #t]
   
   ;; Both are Constraint
   [(and (KV-constraint? kv1) (KV-constraint? kv2))
    #t]
   
   ;; Both are Row
   [(and (KV-row? kv1) (KV-row? kv2))
    #t]
   
   ;; Both are sorts: compare levels
   [(and (KV-sort? kv1) (KV-sort? kv2))
    (= (KV-sort-level kv1) (KV-sort-level kv2))]
   
   ;; Both are arrows: compare domains and codomains
   [(and (KV-arrow? kv1) (KV-arrow? kv2))
    (and (kind-equiv? level (KV-arrow-domain kv1) (KV-arrow-domain kv2))
         (let* ([x (KV-neutral (KN-var level))]
                [c1 (apply-kind-closure (KV-arrow-codomain-clo kv1) x)]
                [c2 (apply-kind-closure (KV-arrow-codomain-clo kv2) x)])
               (kind-equiv? (+ level 1) c1 c2)))]
   
   ;; Both are dependent kinds: compare domains and codomains
   [(and (KV-pi? kv1) (KV-pi? kv2))
    (and (kind-equiv? level (KV-pi-domain kv1) (KV-pi-domain kv2))
         (let* ([x (KV-neutral (KN-var level))]
                [c1 (apply-kind-closure (KV-pi-codomain-clo kv1) x)]
                [c2 (apply-kind-closure (KV-pi-codomain-clo kv2) x)])
               (kind-equiv? (+ level 1) c1 c2)))]
   
   ;; Arrow vs Pi: they can be equivalent if Pi is non-dependent
   [(and (KV-arrow? kv1) (KV-pi? kv2))
    (and (kind-equiv? level (KV-arrow-domain kv1) (KV-pi-domain kv2))
         (let* ([x (KV-neutral (KN-var level))]
                [c1 (apply-kind-closure (KV-arrow-codomain-clo kv1) x)]
                [c2 (apply-kind-closure (KV-pi-codomain-clo kv2) x)])
               (kind-equiv? (+ level 1) c1 c2)))]
   [(and (KV-pi? kv1) (KV-arrow? kv2))
    (kind-equiv? level kv2 kv1)]  ; Symmetric case
   
   ;; Both are neutrals: compare structurally
   [(and (KV-neutral? kv1) (KV-neutral? kv2))
    (kind-convert-neutral? level (KV-neutral-term kv1) (KV-neutral-term kv2))]
   
   [else #f]))

;;; kind-convert-neutral? : Level × KindNeutral × KindNeutral → Boolean
;;; Check if two neutral kind terms are structurally equal.
(define (kind-convert-neutral? level n1 n2)
  (cond
   [(and (KN-var? n1) (KN-var? n2))
    (equal? (KN-var-level n1) (KN-var-level n2))]
   
   [(and (KN-app? n1) (KN-app? n2))
    (and (kind-convert-neutral? level (cadr n1) (cadr n2))
         (kind-equiv? level (caddr n1) (caddr n2)))]
   
   [else #f]))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; kinds-equal? : Kind × Kind → Boolean
;;; Check if two kinds are definitionally equal.
(define (kinds-equal? k1 k2)
  (let ([v1 (eval-kind k1 kind-empty-env)]
        [v2 (eval-kind k2 kind-empty-env)])
       (kind-equiv? 0 v1 v2)))

;;; kind-nf : Kind → Kind
;;; Get the normal form of a kind.
(define (kind-nf k)
  (kind-normalize-closed k))
