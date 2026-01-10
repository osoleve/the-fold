;;; core/types/rank-n-infer.ss — Rank-N Type Inference
;;;
;;; Full Rank-N bidirectional type inference with impredicative polymorphism,
;;; combining approaches from:
;;;   - "Complete and Easy Bidirectional Typechecking for Higher-Rank
;;;     Polymorphism" (Dunfield & Krishnaswami, 2013)
;;;   - "A Quick Look at Impredicativity" (Serrano et al., ICFP 2020)
;;;
;;; What Rank-N Types Enable:
;;; =========================
;;; - First-class polymorphic functions: functions that take or return
;;;   polymorphic functions
;;; - ST monad style: runST :: (forall s. ST s a) -> a (NO annotation needed)
;;; - Lens types: Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t
;;; - Existential types via CPS: exists a. T =~= forall r. (forall a. T -> r) -> r
;;;
;;; Key Components:
;;; ===============
;;; 1. Forall predicates & accessors: forall-type?, forall-vars, forall-body
;;; 2. Instantiation: rank-n-instantiate replaces forall vars with concrete types
;;; 3. Generalization: rank-n-generalize wraps free type vars in forall
;;; 4. Subsumption: rank-n-subsumes? checks higher-rank subtyping
;;; 5. Synthesis: rank-n-infer-synth synthesizes types (↑ direction)
;;; 6. Checking: rank-n-check verifies expressions against types (↓ direction)
;;; 7. Quick Look: peek-argument-structure, should-delay-instantiation?
;;; 8. Impredicative unification: impredicative-unify-with with skolem escape checking
;;;
;;; Bidirectional Approach:
;;; =======================
;;; - Synthesis (↑): Expression → Type
;;;   Infers a type from the expression structure
;;; - Checking (↓): Expression × Type → Success/Failure
;;;   Verifies an expression has the expected type
;;;
;;; For higher-rank types, checking uses:
;;;   1. Lambda against function type: extend env, check body
;;;   2. Expression against ∀: introduce skolems, check against body
;;;   3. Otherwise: synthesize then use impredicative unification
;;;
;;; Quick Look (Guided Instantiation):
;;; ==================================
;;; Instead of eagerly instantiating polymorphic functions, we analyze
;;; argument structure first:
;;;   - Lambda argument → delay instantiation (let checking handle it)
;;;   - Polymorphic variable → delay instantiation
;;;   - Otherwise → instantiate eagerly
;;; This enables runST-style inference without annotations.
;;;
;;; Limitation: Polymorphic Recursion
;;; =================================
;;; True polymorphic recursion (where f calls itself at different type
;;; arguments, e.g., length :: [a] -> Int calling length :: [[a]] -> Int)
;;; is UNDECIDABLE. Use explicit type annotations for such cases:
;;;   (fix (name : Type) body)
;;;
;;; Integration with rank-n.ss:
;;; ===========================
;;; This module builds on rank-n.ss which provides:
;;;   - Type rank calculation (type-rank, rank-n?, rank-1?)
;;;   - Subsumption with skolemization (subsumes, subsumes-with)
;;;   - Capture-avoiding substitution (apply-subst-rankn)
;;;   - Deep instantiation/skolemization
;;;   - Impredicative unification
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - dep-types.ss
;;;   - rank-n.ss (subsumption, instantiation, skolemization)
;;;   - infer.ss (unification, type environments)

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/rank-n.ss")
(load "core/types/infer.ss")

;;; ============================================================
;;; Forall Type Predicates
;;; ============================================================


;;; forall-well-formed? : Type -> Boolean
;;; Check if a forall type is well-formed: (forall (vars...) body)
(define (forall-well-formed? t)
  (and (forall-type? t)
       (>= (length t) 3)
       (list? (cadr t))
       (not (null? (cadr t)))))

;;; ============================================================
;;; Forall Type Accessors
;;; ============================================================

;;; forall-vars : ForallType -> (List Symbol)
;;; Extract the bound type variable names.
(define (forall-vars t)
  (if (forall-type? t)
      (let ([bindings (cadr t)])
           (map (lambda (b)
                        (if (symbol? b)
                            b
                            (car b)))  ; Handle (a : Kind) form
                bindings))
      '()))

;;; forall-var-kinds : ForallType -> (List (Symbol . Kind))
;;; Extract bound type variables with their kinds.
(define (forall-var-kinds t)
  (if (forall-type? t)
      (let ([bindings (cadr t)])
           (map (lambda (b)
                        (if (symbol? b)
                            (cons b 'Type)
                            (cons (car b) (caddr b))))
                bindings))
      '()))

;;; forall-body : ForallType -> Type
;;; Extract the body type (may reference bound vars).
(define (forall-body t)
  (if (forall-type? t)
      (caddr t)
      t))

;;; ============================================================
;;; Instantiation
;;; ============================================================

;;; rank-n-instantiate : ForallType x (List Type) -> Type
;;; Instantiate a forall type with concrete types.
(define (rank-n-instantiate forall-t concrete-types)
  (if (not (forall-type? forall-t))
      forall-t
      (let ([vars (forall-vars forall-t)]
            [body (forall-body forall-t)])
           (if (not (= (length vars) (length concrete-types)))
               `(error arity-mismatch (expected ,(length vars)) (got ,(length concrete-types)))
               (fold-left (lambda (t pair)
                                  (subst-type t (car pair) (cdr pair)))
                          body
                          (map cons vars concrete-types))))))

;;; ============================================================
;;; Generalization
;;; ============================================================

;;; rank-n-generalize : Type x Context -> ForallType
;;; Generalize a type over free type variables not bound in the context.
;;; Context is an alist of (var . type) bindings.
;;;
;;; This implements let-generalization: variables that are free in the type
;;; but not free in the context are universally quantified.
;;;
;;; Example:
;;;   (rank-n-generalize '(-> a a) '())         → (∀ (a) (-> a a))
;;;   (rank-n-generalize '(-> a b) '((x . a)))  → (∀ (b) (-> a b))
(define (rank-n-generalize type ctx)
  (let* (;; Free variables in the type
         [type-fvs (type-free-vars type)]
         ;; Free variables in the context (from all types in the environment)
         [ctx-fvs (apply append
                         (map (lambda (binding)
                                      (type-free-vars (cdr binding)))
                              ctx))]
         ;; Variables to generalize: free in type but not in context
         [gen-vars (filter (lambda (v) (not (memq v ctx-fvs)))
                           (unique type-fvs))])
        (if (null? gen-vars)
            type
            `(∀ ,gen-vars ,type))))

;;; ============================================================
;;; Subsumption
;;; ============================================================

;;; rank-n-subsumes? : Type x Type x Context -> Boolean
;;; Check if type1 is at least as general as (subsumes) type2.
;;;
;;; Uses proper higher-rank subtyping with:
;;;   - Contravariance in function arguments
;;;   - Covariance in return types
;;;   - Skolemization for ∀ on the right
;;;   - Instantiation for ∀ on the left
;;;
;;; The context is used to determine which type variables are in scope.
;;; (Currently context is unused but kept for API compatibility.)
(define (rank-n-subsumes? type1 type2 ctx)
  (subsumes type1 type2))

;;; rank-n-subsumes-result : Type x Type x Context -> (Result Unit Error)
;;; Like rank-n-subsumes? but returns detailed error information.
(define (rank-n-subsumes-result type1 type2 ctx)
  (reset-subsume-fresh!)
  (subsumes-with type1 type2 '()))

;;; ============================================================
;;; Fresh Type Variables for Rank-N Inference
;;; ============================================================

(define *rank-n-fresh-counter* 0)

;;; reset-rank-n-fresh! : → Unit
(define (reset-rank-n-fresh!)
  (set! *rank-n-fresh-counter* 0))

;;; fresh-rank-n-var : → Symbol
;;; Generate a fresh type variable for rank-N inference.
(define (fresh-rank-n-var)
  (set! *rank-n-fresh-counter* (+ *rank-n-fresh-counter* 1))
  (string->symbol (string-append "ρ" (number->string *rank-n-fresh-counter*))))

;;; ============================================================
;;; Quick Look Infrastructure (Guided Instantiation)
;;; ============================================================

;;; Quick Look (Serrano et al., ICFP 2020) guides instantiation decisions
;;; by analyzing argument structure before deciding whether to instantiate
;;; polymorphic function types.

;;; peek-argument-structure : Expr × TEnv → 'lambda | 'poly-var | 'mono
;;; Analyze argument to guide instantiation decisions.
(define (peek-argument-structure arg env)
  (cond
   ;; Lambda: definitely don't instantiate param type
   [(and (pair? arg) (eq? (car arg) 'fn)) 'lambda]
   ;; Annotated expression: check if annotation is polymorphic
   [(and (pair? arg) (eq? (car arg) ':))
    (if (forall-type? (caddr arg)) 'poly-var 'mono)]
   ;; Variable: look up in environment
   [(symbol? arg)
    (let ([t (tenv-lookup env arg)])
         (if (and t (forall-type? t)) 'poly-var 'mono))]
   ;; Default: monomorphic
   [else 'mono]))

;;; should-delay-instantiation? : Type × (List Expr) × TEnv → Boolean
;;; Aggressive: delay if ANY argument suggests polymorphism expected.
(define (should-delay-instantiation? fn-type args env)
  (and (forall-type? fn-type)
       (not (null? args))
       (let ([structures (map (lambda (a) (peek-argument-structure a env)) args)])
            ;; Delay if ANY arg is lambda or poly-var
            (ormap (lambda (s) (memq s '(lambda poly-var))) structures))))

;;; ============================================================
;;; Skolem Detection and Escape Checking
;;; ============================================================

;;; skolem? : Type → Boolean
;;; Check if a type is a skolem constant (rigid type variable).
;;; Skolems are prefixed with ⊥ in our implementation.
(define (skolem? t)
  (and (symbol? t)
       (let ([s (symbol->string t)])
            (and (> (string-length s) 0)
                 (char=? (string-ref s 0) #\⊥)))))

;;; type-contains-skolem? : Type → Boolean
;;; Check if a type contains any skolem constants.
(define (type-contains-skolem? t)
  (cond
   [(skolem? t) #t]
   [(not (pair? t)) #f]
   [(eq? (car t) '∀)
    ;; Check body but skolems bound by this ∀ don't count
    (type-contains-skolem? (caddr t))]
   [else (ormap type-contains-skolem? (cdr t))]))

;;; is-inference-var? : Symbol → Boolean
(define (is-inference-var? s)
  (and (symbol? s)
       (let ([str (symbol->string s)])
            (and (> (string-length str) 0)
                 (char=? (string-ref str 0) #\ρ)))))

;;; subst-has-skolem-escape? : Subst → Boolean
;;; Check if any substitution binding would let a skolem escape its scope.
;;; This prevents unsound unifications where rigid skolems leak.
;;; Note: We only check inference variables (ρ). Local instantiation vars (σ)
;;; are allowed to bind to skolems as they are scoped within the check.
(define (subst-has-skolem-escape? s)
  (ormap (lambda (binding)
                 (let ([var (car binding)]
                       [type (cdr binding)])
                      ;; Problem: non-skolem metavar bound to type containing skolem
                      (and (type-var? var)
                           (not (skolem? var))
                           (is-inference-var? var)  ;; Only check inference vars
                           (type-contains-skolem? type))))
         s))

;;; ============================================================
;;; Impredicative Unification with Scope Safety
;;; ============================================================

;;; impredicative-unify-with : Subst × Type × Type → (Result Subst Error)
;;; Compose existing substitution with impredicative unification.
;;; CRITICAL: Check for skolem escape before allowing unification.
(define (impredicative-unify-with s t1 t2)
  (let* ([t1-applied (apply-subst-rankn s t1)]
         [t2-applied (apply-subst-rankn s t2)]
         [result (impredicative-unify t1-applied t2-applied)])
        (if (eq? (car result) 'ok)
            (let ([new-subst (cadr result)])
                 ;; Verify no skolem escape in the new substitution
                 (if (subst-has-skolem-escape? new-subst)
                     `(error skolem-escape ,t1-applied ,t2-applied)
                     `(ok ,(compose-subst new-subst s))))
            result)))

;;; ============================================================
;;; Bidirectional Type Inference for Rank-N
;;; ============================================================

;;; The key insight from Dunfield & Krishnaswami:
;;;   - Synthesis (↑): Expression → Type
;;;   - Checking (↓): Expression × Type → Success/Failure
;;;
;;; For higher-rank types, we need subsumption-based checking:
;;;   To check e ⇐ A:
;;;     1. Synthesize e ⇒ B
;;;     2. Check B <: A (subsumption)

;;; rank-n-infer-synth : Expr × TEnv → (Result (× Type Subst) Error)
;;; Synthesize a type for an expression.
;;;
;;; This extends basic inference to handle higher-rank polymorphism.
;;; Key extensions:
;;;   - Annotations with ∀ types are preserved, not immediately instantiated
;;;   - Applications of polymorphic functions may require subsumption
;;;   - Let-bindings generalize over free type variables
(define (rank-n-infer-synth expr env)
  (cond
   ;; Literals
   [(number? expr) `(ok Int ,empty-subst)]
   [(boolean? expr) `(ok Bool ,empty-subst)]
   [(string? expr) `(ok String ,empty-subst)]
   [(and (pair? expr) (eq? (car expr) 'quote))
    (rank-n-infer-quoted (cadr expr))]
   
   ;; Variable: lookup in environment
   [(symbol? expr)
    (let ([type (tenv-lookup env expr)])
         (if type
             ;; For rank-N, we DON'T immediately instantiate
             ;; The caller decides when to instantiate based on context
             `(ok ,type ,empty-subst)
             `(error unbound-variable ,expr)))]
   
   [(not (pair? expr))
    `(error unknown-expression ,expr)]
   
   ;; Type annotation: (: expr type)
   ;; This is critical for rank-N: user provides the type, we check
   [(eq? (car expr) ':)
    (let* ([e (cadr expr)]
           [annot-type (caddr expr)]
           [result (rank-n-check e annot-type env)])
          (if (eq? (car result) 'ok)
              `(ok ,annot-type ,(cadr result))
              result))]
   
   ;; Lambda: (fn (x) body)
   ;; Without annotation, infer with fresh type variables
   [(eq? (car expr) 'fn)
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [annotated-params (extract-param-annotations params)]
           [param-types (map (lambda (p)
                                     (if (cdr p)
                                         (cdr p)  ; Use annotation
                                         (fresh-rank-n-var)))  ; Fresh var
                             annotated-params)]
           [param-names (map car annotated-params)]
           [new-env (append (map cons param-names param-types) env)]
           [result (rank-n-infer-synth body new-env)])
          (if (eq? (car result) 'ok)
              (let ([body-type (cadr result)]
                    [s (caddr result)])
                   `(ok ,(apply-subst-rankn s
                                            (make-function-type param-types body-type))
                     ,s))
              result))]
   
   ;; Application: (f arg ...)
   [(not (rank-n-special-form? (car expr)))
    (rank-n-infer-app (car expr) (cdr expr) env)]
   
   ;; Let: (let ((x e1) ...) body)
   [(eq? (car expr) 'let)
    (rank-n-infer-let (cadr expr) (caddr expr) env)]
   
   ;; Fix: (fix name fn-expr) or (fix (name : Type) fn-expr)
   ;; NOTE: True polymorphic recursion is UNDECIDABLE. This handles:
   ;;   - Monomorphic recursion (f calls f at same type)
   ;;   - Polymorphic definitions used at multiple types externally
   ;; For f calling f at DIFFERENT types, use annotation: (fix (name : Type) body)
   [(eq? (car expr) 'fix)
    (let* ([name-part (cadr expr)]
           [var (if (pair? name-part) (car name-part) name-part)]
           [body (caddr expr)]
           [fix-type (fresh-rank-n-var)]
           [new-env (cons (cons var fix-type) env)]
           [result (rank-n-infer-synth body new-env)])
          (if (eq? (car result) 'ok)
              (let* ([body-type (cadr result)]
                     [s1 (caddr result)]
                     [unify-result (unify-with s1 fix-type body-type)])
                    (if (eq? (car unify-result) 'ok)
                        `(ok ,(apply-subst-rankn (cadr unify-result) body-type)
                          ,(cadr unify-result))
                        unify-result))
              result))]
   
   ;; If: (if test then else)
   [(eq? (car expr) 'if)
    (rank-n-infer-if (cadr expr) (caddr expr) (cadddr expr) env)]
   
   [else `(error unsupported-expression ,expr)]))

;;; ============================================================
;;; Application Inference for Rank-N
;;; ============================================================

;;; rank-n-infer-app : Expr × (List Expr) × TEnv → (Result (× Type Subst) Error)
;;; Infer type of function application.
;;;
;;; For rank-N, we use the "Quick Look" approach:
;;;   1. Infer function type
;;;   2. If polymorphic, instantiate based on argument structure
;;;   3. Check arguments against expected types
;;;   4. Return the return type
(define (rank-n-infer-app fn args env)
  (let ([fn-result (rank-n-infer-synth fn env)])
       (if (not (eq? (car fn-result) 'ok))
           fn-result
           (let* ([fn-type (cadr fn-result)]
                  [s1 (caddr fn-result)]
                  ;; Quick Look: guided instantiation instead of eager
                  ;; If argument structure suggests polymorphism, delay instantiation
                  [inst-fn-type (if (should-delay-instantiation? fn-type args env)
                                    fn-type  ;; Keep polymorphic!
                                    (deep-instantiate fn-type))])
                 (rank-n-infer-app-args inst-fn-type args s1 env)))))

;;; rank-n-infer-app-args : Type × (List Expr) × Subst × TEnv → (Result (× Type Subst) Error)
(define (rank-n-infer-app-args fn-type args s env)
  (if (null? args)
      `(ok ,(apply-subst-rankn s fn-type) ,s)
      (let ([fn-type (apply-subst-rankn s fn-type)])
           (cond
            ;; Function type: check args and return result type
            [(function-type? fn-type)
             (let* ([param-types (function-param-types fn-type)]
                    [return-type (function-return-type fn-type)])
                   (if (not (= (length args) (length param-types)))
                       `(error arity-mismatch
                         (expected ,(length param-types))
                         (got ,(length args)))
                       (let ([result (rank-n-check-args args param-types s env)])
                            (if (eq? (car result) 'ok)
                                `(ok ,(apply-subst-rankn (cadr result) return-type)
                                  ,(cadr result))
                                result))))]
            ;; Type variable: create function type
            [(type-var? fn-type)
             (let* ([arg-types (map (lambda (_) (fresh-rank-n-var)) args)]
                    [ret-type (fresh-rank-n-var)]
                    [expected-fn-type (make-function-type arg-types ret-type)]
                    [unify-result (unify-with s fn-type expected-fn-type)])
                   (if (eq? (car unify-result) 'ok)
                       (let ([s2 (cadr unify-result)])
                            (let ([result (rank-n-check-args args arg-types s2 env)])
                                 (if (eq? (car result) 'ok)
                                     `(ok ,(apply-subst-rankn (cadr result) ret-type)
                                       ,(cadr result))
                                     result)))
                       unify-result))]
            ;; Forall type: instantiate to get to the function type
            ;; This handles cases where we delayed instantiation but now need to apply
            [(forall-type? fn-type)
             (rank-n-infer-app-args (deep-instantiate fn-type) args s env)]
            [else `(error not-a-function ,fn-type)]))))

;;; rank-n-check-args : (List Expr) × (List Type) × Subst × TEnv → (Result Subst Error)
;;; Check arguments against parameter types, using impredicative unification.
;;; For polymorphic parameters, use checking (which triggers skolemization).
;;; For monomorphic parameters, synthesize and unify.
(define (rank-n-check-args args types s env)
  (if (null? args)
      `(ok ,s)
      (let* ([param-type (apply-subst-rankn s (car types))]
             [result (if (forall-type? param-type)
                         ;; Polymorphic param: use checking (triggers skolemization)
                         (rank-n-check (car args) param-type env)
                         ;; Monomorphic: synthesize and use impredicative unify
                         (let ([synth (rank-n-infer-synth (car args) env)])
                              (if (eq? (car synth) 'ok)
                                  (let ([arg-type (apply-subst-rankn (caddr synth) (cadr synth))])
                                       (impredicative-unify-with s arg-type param-type))
                                  synth)))])
            (if (eq? (car result) 'ok)
                (rank-n-check-args (cdr args) (cdr types)
                                   (compose-subst (cadr result) s) env)
                result))))

;;; ============================================================
;;; Type Checking for Rank-N
;;; ============================================================

;;; check-against-forall-infer : Expr × Type × TEnv → (Result Subst Error)
;;; Check an expression against a universally quantified type (with synthesis fallback).
(define (check-against-forall-infer expr type env)
  (if (forall-type? type)
      (let* ([vars (forall-vars type)]
             [body (forall-body type)]
             ;; Introduce skolems
             [skolems (map (lambda (v)
                                   (string->symbol
                                    (string-append "⊥" (symbol->string v))))
                           vars)]
             [s (map cons vars skolems)]
             [skolem-body (apply-subst-rankn s body)])
        ;; Recurse using rank-n-check
        (rank-n-check expr skolem-body env))
      `(error not-a-forall ,type)))

;;; rank-n-check : Expr × Type × TEnv → (Result Subst Error)
;;; Check that an expression has the expected type.
;;;
;;; For rank-N types, checking follows these rules:
;;;   1. (λx.e) ⇐ (A → B): extend env with x:A, check e ⇐ B
;;;   2. e ⇐ (∀a.A): introduce a as skolem, check e ⇐ A
;;;   3. Otherwise: synthesize e ⇒ B, check B <: A (subsumption)
(define (rank-n-check expr expected env)
  (cond
   ;; Lambda against function type
   [(and (pair? expr) (eq? (car expr) 'fn) (function-type? expected))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (function-param-types expected)]
           [return-type (function-return-type expected)]
           [annotated-params (extract-param-annotations params)]
           [param-names (map car annotated-params)])
          (if (not (= (length param-names) (length param-types)))
              `(error arity-mismatch
                (expected ,(length param-types))
                (got ,(length param-names)))
              (let ([new-env (append (map cons param-names param-types) env)])
                   (rank-n-check body return-type new-env))))]

   ;; Check against ∀ type: introduce skolems
   [(forall-type? expected)
    (check-against-forall-infer expr expected env)]

   ;; Fall back to synthesis + impredicative unification
   [else
    (let ([result (rank-n-infer-synth expr env)])
         (if (eq? (car result) 'ok)
             (let* ([inferred (cadr result)]
                    [s1 (caddr result)]
                    [inferred-applied (apply-subst-rankn s1 inferred)]
                    [expected-applied (apply-subst-rankn s1 expected)])
                   ;; If inferred type is polymorphic, instantiate before unifying
                   (let ([inferred-inst (if (and (pair? inferred-applied)
                                                 (eq? (car inferred-applied) '∀))
                                            (deep-instantiate inferred-applied)
                                            inferred-applied)])
                        ;; Use impredicative unification with skolem escape checking
                        (let ([unify-result (impredicative-unify-with s1
                                                                      inferred-inst
                                                                      expected-applied)])
                             (if (eq? (car unify-result) 'ok)
                                 `(ok ,(cadr unify-result))
                                 `(error type-mismatch
                                   (inferred ,inferred-inst)
                                   (expected ,expected-applied))))))
             result))]))

;;; ============================================================
;;; Let Inference for Rank-N
;;; ============================================================

;;; rank-n-infer-let : (List (× Symbol Expr)) × Expr × TEnv → (Result (× Type Subst) Error)
;;; Infer type of a let expression with proper generalization.
(define (rank-n-infer-let bindings body env)
  (rank-n-infer-let-bindings bindings body env empty-subst))

(define (rank-n-infer-let-bindings bindings body env s)
  (if (null? bindings)
      ;; All bindings processed, infer body
      (let ([result (rank-n-infer-synth body env)])
           (if (eq? (car result) 'ok)
               `(ok ,(cadr result) ,(compose-subst (caddr result) s))
               result))
      ;; Process next binding
      (let* ([binding (car bindings)]
             [var (car binding)]
             [init (cadr binding)]
             [init-result (rank-n-infer-synth init env)])
            (if (eq? (car init-result) 'ok)
                (let* ([init-type (cadr init-result)]
                       [s1 (caddr init-result)]
                       [combined-s (compose-subst s1 s)]
                       ;; Apply substitution before generalizing
                       [init-type-applied (apply-subst-rankn combined-s init-type)]
                       ;; Apply substitution to env for proper generalization
                       [subst-env (map (lambda (p)
                                               (cons (car p)
                                                     (apply-subst-rankn combined-s (cdr p))))
                                       env)]
                       ;; Generalize over free type variables
                       [gen-type (rank-n-generalize init-type-applied subst-env)]
                       [new-env (cons (cons var gen-type) env)])
                      (rank-n-infer-let-bindings (cdr bindings) body new-env combined-s))
                init-result))))

;;; ============================================================
;;; If Inference for Rank-N
;;; ============================================================

;;; rank-n-infer-if : Expr × Expr × Expr × TEnv → (Result (× Type Subst) Error)
(define (rank-n-infer-if test then-expr else-expr env)
  (let ([test-result (rank-n-check test 'Bool env)])
       (if (not (eq? (car test-result) 'ok))
           test-result
           (let* ([s1 (cadr test-result)]
                  [then-result (rank-n-infer-synth then-expr env)])
                 (if (not (eq? (car then-result) 'ok))
                     then-result
                     (let* ([then-type (cadr then-result)]
                            [s2 (compose-subst (caddr then-result) s1)]
                            [else-result (rank-n-infer-synth else-expr env)])
                           (if (not (eq? (car else-result) 'ok))
                               else-result
                               (let* ([else-type (cadr else-result)]
                                      [s3 (compose-subst (caddr else-result) s2)]
                                      ;; Unify branch types
                                      [branch-unify (unify-with s3
                                                                (apply-subst-rankn s3 then-type)
                                                                (apply-subst-rankn s3 else-type))])
                                     (if (eq? (car branch-unify) 'ok)
                                         (let ([s4 (cadr branch-unify)])
                                              `(ok ,(apply-subst-rankn s4 then-type) ,s4))
                                         branch-unify)))))))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; extract-param-annotations : (List Param) → (List (× Symbol (Option Type)))
;;; Extract parameter names and optional type annotations.
;;; Supports: (x) or (x : Type) or just x
(define (extract-param-annotations params)
  (map (lambda (p)
               (cond
                [(symbol? p) (cons p #f)]
                [(and (pair? p) (= (length p) 3) (eq? (cadr p) ':))
                 (cons (car p) (caddr p))]
                [(pair? p) (cons (car p) #f)]
                [else (cons p #f)]))
       params))

;;; make-function-type : (List Type) × Type → Type
(define (make-function-type param-types return-type)
  (cons '-> (append param-types (list return-type))))

;;; rank-n-special-form? : Symbol → Boolean
(define (rank-n-special-form? s)
  (and (symbol? s)
       (memq s '(fn let fix if case prim quote :))))

;;; rank-n-infer-quoted : Any → (Result (× Type Subst) Error)
(define (rank-n-infer-quoted datum)
  (cond
   [(symbol? datum) `(ok Symbol ,empty-subst)]
   [(number? datum) `(ok Int ,empty-subst)]
   [(string? datum) `(ok String ,empty-subst)]
   [(null? datum) `(ok (List ?) ,empty-subst)]
   [(pair? datum)
    (let ([elem-result (rank-n-infer-quoted (car datum))])
         (if (eq? (car elem-result) 'ok)
             `(ok (List ,(cadr elem-result)) ,empty-subst)
             `(ok (List ?) ,empty-subst)))]
   [else `(ok ? ,empty-subst)]))

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; rank-n-typeof : Expr → Type | Error
;;; Infer the type of an expression using rank-N inference.
(define (rank-n-typeof expr)
  (reset-rank-n-fresh!)
  (reset-subsume-fresh!)
  (let ([result (rank-n-infer-synth expr '())])
       (if (eq? (car result) 'ok)
           (let* ([type (cadr result)]
                  [s (caddr result)]
                  [applied-type (apply-subst-rankn s type)])
                 (rank-n-generalize applied-type '()))
           result)))

;;; rank-n-typecheck : Expr × Type → Boolean | Error
;;; Check that an expression has the given type.
(define (rank-n-typecheck expr type)
  (reset-rank-n-fresh!)
  (reset-subsume-fresh!)
  (let ([result (rank-n-check expr type '())])
       (if (eq? (car result) 'ok)
           #t
           result)))
