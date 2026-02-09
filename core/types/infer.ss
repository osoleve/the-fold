(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")

(doc 'module 'infer)
(doc 'description "Bidirectional Type Inference. Types flow in two directions: Inference (↑) synthesizes types from expressions, Checking (↓) verifies against expected types. Based on Dunfield & Krishnaswami 2013.")
(doc 'layer 'core)
(doc 'note "resolve.ss must be loaded before using constraint-solving functions (solve-constraints, typeof-constrained). The base inference functions work without resolve.ss.")

(doc 'section 'type-environment)
(doc 'note "A type environment maps term variables to types: (List (Pair Symbol Type)).")

(doc empty-tenv 'type TEnv)
(doc empty-tenv 'description "Empty type environment.")
(doc empty-tenv 'export #t)
(define empty-tenv '())

(define (tenv-lookup env var)
  (doc 'type (-> TEnv Symbol (Maybe Type)))
  (doc 'description "Look up a variable in the type environment.")
  (doc 'export #t)
  (let ([entry (assq var env)])
       (if entry (cdr entry) #f)))

(define (tenv-extend env var type)
  (doc 'type (-> TEnv Symbol Type TEnv))
  (doc 'description "Extend environment with a single binding.")
  (doc 'export #t)
  (cons (cons var type) env))

(define (tenv-extend* env bindings)
  (doc 'type (-> TEnv (List (Pair Symbol Type)) TEnv))
  (doc 'description "Extend environment with multiple bindings.")
  (doc 'export #t)
  (append bindings env))

(define (type-ground? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Does this type contain no type variables or holes? Ground types are unaffected by substitution.")
  (doc 'export #t)
  (cond
   [(base-type? t) #t]
   [(type-var? t) #f]
   [(eq? t '?) #f]
   ;; Inline hole-var check (hole-var? defined later in file)
   [(and (symbol? t)
         (let ([s (symbol->string t)])
           (and (> (string-length s) 1)
                (char=? (string-ref s 0) #\?)))) #f]
   [(not (pair? t)) #t]
   [(memq (car t) '(∀ ?)) #f]
   [else (andmap type-ground? t)]))

(define (apply-subst-to-env s env)
  (doc 'type (-> Subst TEnv TEnv))
  (doc 'description "Apply a substitution to all types in a type environment. Skips ground types since substitution is a no-op on them.")
  (doc 'export #t)
  (if (null? s)
      env
      (map (lambda (binding)
             (let ([ty (cdr binding)])
               (if (type-ground? ty)
                   binding
                   (cons (car binding) (apply-subst s ty)))))
           env)))

(doc 'section 'fresh-type-variables)
(doc 'note "For inference, we need fresh type variables. We use a counter embedded in the inference state.")

(define *fresh-counter* 0)
(define *hole-counter* 0)

(define (reset-fresh!)
  (doc 'type (-> Void))
  (doc 'description "Reset both type variable and hole variable counters.")
  (doc 'export #t)
  (set! *fresh-counter* 0)
  (set! *hole-counter* 0))

(define (fresh-tvar)
  (doc 'type (-> Symbol))
  (doc 'description "Generate a fresh type variable τ1, τ2, etc.")
  (doc 'export #t)
  (set! *fresh-counter* (+ *fresh-counter* 1))
  (string->symbol (string-append "τ" (number->string *fresh-counter*))))

(define (fresh-tvar-named prefix)
  (doc 'type (-> String Symbol))
  (doc 'description "Generate a fresh type variable with a given prefix.")
  (doc 'export #t)
  (set! *fresh-counter* (+ *fresh-counter* 1))
  (string->symbol (string-append prefix (number->string *fresh-counter*))))

(doc 'section 'hole-variables)
(doc 'note "Gradual typing support. Holes (? and (? name)) are converted to hole variables for constraint tracking. Named holes (? x) → ?x are consistent across uses; anonymous holes (?) → ?1, ?2, etc. are fresh each time.")

(define (reset-holes!)
  (doc 'type (-> Void))
  (doc 'description "Standalone reset for holes only (use reset-fresh! for full reset).")
  (doc 'export #t)
  (set! *hole-counter* 0))

(define (fresh-hole-var)
  (doc 'type (-> Symbol))
  (doc 'description "Generate a fresh hole variable for anonymous holes.")
  (doc 'export #t)
  (set! *hole-counter* (+ *hole-counter* 1))
  (string->symbol (string-append "?" (number->string *hole-counter*))))

(define (hole->var h)
  (doc 'type (-> Any Symbol))
  (doc 'description "Convert a hole to its corresponding hole variable.")
  (doc 'export #t)
  (if (and (pair? h) (eq? (car h) '?))
      (string->symbol (string-append "?" (symbol->string (cadr h))))
      (fresh-hole-var)))

(define (hole-var? t)
  (doc 'type (-> Any Boolean))
  (doc 'description "Is this a hole variable (symbol starting with '?' but not '?' itself)?")
  (doc 'export #t)
  (and (symbol? t)
       (let ([s (symbol->string t)])
         (and (> (string-length s) 1)
              (char=? (string-ref s 0) #\?)))))

(define (hole-var-name hv)
  (doc 'type (-> Symbol String))
  (doc 'description "Extract the name part of a hole variable (?foo → \"foo\").")
  (doc 'export #t)
  (substring (symbol->string hv) 1 (string-length (symbol->string hv))))

(define (hole-constraints s)
  (doc 'type (-> Subst Subst))
  (doc 'description "Extract only the hole variable bindings from a substitution.")
  (doc 'export #t)
  (filter (lambda (p) (hole-var? (car p))) s))

(define (type-var-constraints s)
  (doc 'type (-> Subst Subst))
  (doc 'description "Extract only the type variable bindings from a substitution.")
  (doc 'export #t)
  (filter (lambda (p) (and (type-var? (car p)) (not (hole-var? (car p))))) s))

(doc 'section 'substitution)
(doc 'note "A substitution maps type variables to types. We apply substitutions to types to instantiate variables.")

(doc empty-subst 'type Subst)
(doc empty-subst 'description "Empty substitution.")
(doc empty-subst 'export #t)
(define empty-subst '())

(define (subst-lookup s var)
  (doc 'type (-> Subst Symbol (Maybe Type)))
  (doc 'description "Look up a variable in a substitution.")
  (doc 'export #t)
  (let ([entry (assq var s)])
       (if entry (cdr entry) #f)))

(define (subst-extend s var type)
  (doc 'type (-> Subst Symbol Type Subst))
  (doc 'description "Extend a substitution with a new binding.")
  (doc 'export #t)
  (cons (cons var type) s))

(define (apply-subst s type)
  (doc 'type (-> Subst Type Type))
  (doc 'description "Apply a substitution to a type, replacing variables with their bindings.")
  (doc 'export #t)
  (cond
   ;; Type variables: look up in substitution
   [(type-var? type)
    (let ([replacement (subst-lookup s type)])
         (if replacement
             (apply-subst s replacement)  ; Chase chains
             type))]
   ;; Hole variables: look up like type variables
   [(hole-var? type)
    (let ([replacement (subst-lookup s type)])
         (if replacement
             (apply-subst s replacement)  ; Chase chains
             type))]
   ;; Named holes: convert to hole var and look up
   [(and (pair? type) (eq? (car type) '?))
    (let* ([hv (string->symbol (string-append "?" (symbol->string (cadr type))))]
           [replacement (subst-lookup s hv)])
          (if replacement
              (apply-subst s replacement)
              type))]  ; Keep as named hole if not yet constrained
   ;; Anonymous holes: unchanged (they get fresh vars during unification)
   [(eq? type '?) type]
   [(base-type? type) type]
   [(not (pair? type)) type]
   ;; Don't substitute bound variables
   [(eq? (car type) '∀)
    (let* ([bound-raw (cadr type)]
           [body (caddr type)]
           ;; Extract var names from both simple (a) and kinded ((a : *)) forms
           [bound-names (map (lambda (v)
                                     (if (kinded-tvar? v)
                                         (kinded-tvar-name v)
                                         v))
                             bound-raw)]
           ;; Remove bound vars from substitution
           [s* (filter (lambda (p) (not (memq (car p) bound-names))) s)])
          `(∀ ,bound-raw ,(apply-subst s* body)))]
   [(eq? (car type) 'μ)
    (let ([var (cadr type)]
          [body (caddr type)])
         (let ([s* (filter (lambda (p) (not (eq? (car p) var))) s)])
              `(μ ,var ,(apply-subst s* body))))]
   [else
    (cons (car type)
          (map (lambda (t) (apply-subst s t)) (cdr type)))]))

(define (compose-subst s1 s2)
  (doc 'type (-> Subst Subst Subst))
  (doc 'description "Compose two substitutions.")
  (doc 'export #t)
  (append
   (map (lambda (p) (cons (car p) (apply-subst s1 (cdr p)))) s2)
   s1))

(doc 'section 'unification)
(doc 'note "Unification finds a substitution that makes two types equal. Returns (ok subst) or (error message).")

(define (unify t1 t2)
  (doc 'type (-> Type Type (Result Subst Error)))
  (doc 'description "Unify two types, returning a substitution or error.")
  (doc 'export #t)
  (unify-with empty-subst t1 t2))

(define (unify-with s t1 t2)
  (doc 'type (-> Subst Type Type (Result Subst Error)))
  (doc 'description "Unify two types with an existing substitution.")
  (doc 'export #t)
  (let ([t1 (apply-subst s t1)]
        [t2 (apply-subst s t2)])
       (cond
        ;; Same type
        [(type=? t1 t2) `(ok ,s)]
        
        ;; Type variable on left
        [(type-var? t1)
         (if (occurs? t1 t2)
             `(error occurs-check ,t1 ,t2)
             `(ok ,(subst-extend s t1 t2)))]
        
        ;; Type variable on right
        [(type-var? t2)
         (if (occurs? t2 t1)
             `(error occurs-check ,t2 ,t1)
             `(ok ,(subst-extend s t2 t1)))]
        
        ;; Hole variables unify like type variables (constraint recording)
        ;; This enables tracking what types holes unified with for:
        ;;   - Consistent typing of named holes across uses
        ;;   - Inference of hole types for better error messages
        ;;   - Potential runtime cast generation for gradual typing
        [(hole-var? t1)
         (cond
          ;; Hole var unifies with itself
          [(and (hole-var? t2) (eq? t1 t2)) `(ok ,s)]
          ;; Hole var unifies with another hole var: unify them
          [(hole-var? t2) `(ok ,(subst-extend s t1 t2))]
          ;; Hole var unifies with a type: record the constraint
          [(occurs? t1 t2)
           `(error occurs-check ,t1 ,t2)]
          [else
           `(ok ,(subst-extend s t1 t2))])]
        [(hole-var? t2)
         (if (occurs? t2 t1)
             `(error occurs-check ,t2 ,t1)
             `(ok ,(subst-extend s t2 t1)))]

        ;; Holes: convert to hole variables and unify
        ;; Named holes (? x) become consistent hole vars (?x)
        ;; Anonymous holes (?) become fresh hole vars (?1, ?2, etc.)
        [(hole? t1)
         (let ([hv (hole->var t1)])
           (cond
            [(hole? t2)
             ;; Both are holes: convert both and unify their vars
             (let ([hv2 (hole->var t2)])
               `(ok ,(subst-extend s hv hv2)))]
            [(hole-var? t2)
             ;; Hole meets hole-var: unify them
             `(ok ,(subst-extend s hv t2))]
            [(occurs? hv t2)
             `(error occurs-check ,hv ,t2)]
            [else
             `(ok ,(subst-extend s hv t2))]))]
        [(hole? t2)
         (let ([hv (hole->var t2)])
           (if (occurs? hv t1)
               `(error occurs-check ,hv ,t1)
               `(ok ,(subst-extend s hv t1))))]
        
        ;; Both are compound types with same constructor
        [(and (pair? t1) (pair? t2) (eq? (car t1) (car t2)))
         (unify-lists s (cdr t1) (cdr t2))]
        
        ;; Type application on both sides: (@ F1 Args1...) ~ (@ F2 Args2...)
        [(and (type-app? t1) (type-app? t2))
         (let ([h1 (type-app-head t1)]
               [h2 (type-app-head t2)]
               [a1 (type-app-args t1)]
               [a2 (type-app-args t2)])
              (let ([result (unify-with s h1 h2)])
                   (if (eq? (car result) 'ok)
                       (unify-lists (cadr result) a1 a2)
                       result)))]
        
        ;; HKT: Type application on left, concrete type on right
        ;; (@ f a) ~ (List Int) → f = List, a = Int
        [(and (type-app? t1) (pair? t2) (not (type-app? t2)))
         (let ([head (type-app-head t1)]
               [args (type-app-args t1)]
               [constr (car t2)]
               [constr-args (cdr t2)])
              ;; Try to unify head with constructor, args with constructor args
              (if (= (length args) (length constr-args))
                  (let ([head-result (unify-with s head constr)])
                       (if (eq? (car head-result) 'ok)
                           (unify-lists (cadr head-result) args constr-args)
                           head-result))
                  `(error hkt-arity-mismatch ,t1 ,t2)))]
        
        ;; HKT: Concrete type on left, type application on right
        [(and (pair? t1) (not (type-app? t1)) (type-app? t2))
         (unify-with s t2 t1)]  ;; Swap and retry
        
        [else `(error type-mismatch ,t1 ,t2)])))

(define (unify-lists s ts1 ts2)
  (doc 'type (-> Subst (List Type) (List Type) (Result Subst Error)))
  (doc 'description "Unify two lists of types pairwise.")
  (doc 'export #t)
  (cond
   [(and (null? ts1) (null? ts2)) `(ok ,s)]
   [(or (null? ts1) (null? ts2))
    `(error arity-mismatch ,ts1 ,ts2)]
   [else
    (let ([result (unify-with s (car ts1) (car ts2))])
         (if (eq? (car result) 'ok)
             (unify-lists (cadr result) (cdr ts1) (cdr ts2))
             result))]))

(define (occurs? var type)
  (doc 'type (-> Symbol Type Boolean))
  (doc 'description "Check if a variable (type var or hole var) occurs in a type.")
  (doc 'export #t)
  (cond
   [(type-var? type) (eq? var type)]
   [(hole-var? type) (eq? var type)]
   ;; Anonymous hole: never contains a variable
   [(eq? type '?) #f]
   ;; Named hole: check if var matches the hole's variable
   [(and (pair? type) (eq? (car type) '?))
    (eq? var (string->symbol (string-append "?" (symbol->string (cadr type)))))]
   [(base-type? type) #f]
   [(not (pair? type)) #f]
   [(eq? (car type) '∀)
    ;; Extract var names from both simple (a) and kinded ((a : *)) forms
    (let ([bound-names (map (lambda (v)
                                    (if (kinded-tvar? v)
                                        (kinded-tvar-name v)
                                        v))
                            (cadr type))])
         (if (memq var bound-names)
             #f  ; Bound, doesn't count
             (occurs? var (caddr type))))]
   [(eq? (car type) 'μ)
    (if (eq? var (cadr type))
        #f
        (occurs? var (caddr type)))]
   [else (ormap (lambda (t) (occurs? var t)) (cdr type))]))

(doc 'section 'instantiation)
(doc 'note "Instantiate a polymorphic type with fresh type variables: (∀ (a b) (-> a b a)) → (-> τ1 τ2 τ1).")

(define (instantiate type)
  (doc 'type (-> Type Type))
  (doc 'description "Instantiate a polymorphic type with fresh type variables.")
  (doc 'export #t)
  (if (and (pair? type) (eq? (car type) '∀))
      (let* ([vars (cadr type)]
             [body (caddr type)]
             [fresh-vars (map (lambda (_) (fresh-tvar)) vars)]
             [s (map cons vars fresh-vars)])
            (apply-subst s body))
      type))

(doc 'section 'generalization)
(doc 'note "Generalize a type by quantifying over free type variables not in the environment.")

(define (generalize env type)
  (doc 'type (-> TEnv Type Type))
  (doc 'description "Generalize a type by quantifying over free type variables not in env.")
  (doc 'export #t)
  (let* ([env-vars (append-map (lambda (p) (free-tvars (cdr p))) env)]
         [type-vars (free-tvars type)]
         [gen-vars (unique (filter (lambda (v) (not (memq v env-vars))) type-vars))])
        (if (null? gen-vars)
            type
            `(∀ ,gen-vars ,type))))

(doc 'section 'bidirectional-type-inference)

(define (infer expr env)
  (doc 'type (-> Expr TEnv (Result Type Subst)))
  (doc 'description "Synthesize a type for an expression.")
  (doc 'export #t)
  (cond
   ;; Literals
   ;; All numeric literals infer as Int for practical reasons:
   ;; - Nat exists but requires explicit annotation (: 42 Nat)
   ;; - Without subtyping (Nat ⊆ Int), Nat literals wouldn't work with
   ;;   arithmetic primitives which expect Int
   ;; - Floats also become Int (no Real type yet — future work)
   [(number? expr) `(ok Int ,empty-subst)]
   [(boolean? expr) `(ok Bool ,empty-subst)]
   [(string? expr) `(ok String ,empty-subst)]
   [(and (pair? expr) (eq? (car expr) 'quote))
    (infer-quoted (cadr expr))]
   
   ;; Variable: check env first, then declared types (from doc annotations)
   [(symbol? expr)
    (let ([type (or (tenv-lookup env expr)
                    (lookup-declared-type expr))])
         (if type
             `(ok ,(instantiate type) ,empty-subst)
             `(error unbound-variable ,expr)))]
   
   [(not (pair? expr))
    `(error unknown-expression ,expr)]
   
   ;; Type annotation: (: expr type)
   [(eq? (car expr) ':)
    (let* ([e (cadr expr)]
           [annot-type (caddr expr)]
           [result (check e annot-type env)])
          (if (eq? (car result) 'ok)
              `(ok ,annot-type ,(cadr result))
              result))]
   
   ;; Lambda: (fn (x) body) — need expected type to check
   ;; Without annotation, we infer with fresh type variables
   [(eq? (car expr) 'fn)
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (map (lambda (_) (fresh-tvar)) params)]
           [new-env (tenv-extend* env (map cons params param-types))]
           [result (infer body new-env)])
          (if (eq? (car result) 'ok)
              (let ([body-type (cadr result)]
                    [s (caddr result)])
                   `(ok ,(apply-subst s (cons '-> (append param-types (list body-type))))
                     ,s))
              result))]
   
   ;; Application: (f arg ...)
   [(not (special-form? (car expr)))
    (infer-app (car expr) (cdr expr) env)]
   
   ;; Let: (let ((x e1) ...) body)
   [(eq? (car expr) 'let)
    (infer-let (cadr expr) (caddr expr) env empty-subst)]
   
   ;; Fix: (fix name fn-expr) or (fix (name) fn-expr)
   [(eq? (car expr) 'fix)
    (let* ([name-part (cadr expr)]
           [var (if (pair? name-part) (car name-part) name-part)]
           [body (caddr expr)]
           [fix-type (fresh-tvar)]
           [new-env (tenv-extend env var fix-type)]
           [result (infer body new-env)])
          (if (eq? (car result) 'ok)
              (let* ([body-type (cadr result)]
                     [s1 (caddr result)]
                     [unify-result (unify-with s1 fix-type body-type)])
                    (if (eq? (car unify-result) 'ok)
                        `(ok ,(apply-subst (cadr unify-result) body-type)
                          ,(cadr unify-result))
                        unify-result))
              result))]
   
   ;; If: (if test then else)
   [(eq? (car expr) 'if)
    (infer-if (cadr expr) (caddr expr) (cadddr expr) env)]
   
   ;; Case: (case expr ((Tag vars...) body) ...)
   [(eq? (car expr) 'case)
    (infer-case (cadr expr) (cddr expr) env)]
   
   ;; Primitive call: (prim 'op args...)
   [(eq? (car expr) 'prim)
    (let ([op (cadr expr)])
         ;; Handle quoted operator: (prim 'neg x) → op is (quote neg)
         (let ([op-sym (if (and (pair? op) (eq? (car op) 'quote))
                           (cadr op)
                           op)])
              (infer-prim op-sym (cddr expr) env)))]
   
   [else `(error unsupported-expression ,expr)]))

(doc 'section 'let-inference)

(define (infer-let bindings body env subst)
  (doc 'type (-> (List (Pair Symbol Expr)) Expr TEnv Subst (Result Type Error)))
  (doc 'description "Infer type for let with multiple bindings.")
  (doc 'export #t)
  (if (null? bindings)
      ;; All bindings processed, infer body
      (let ([result (infer body env)])
           (if (eq? (car result) 'ok)
               `(ok ,(cadr result) ,(compose-subst (caddr result) subst))
               result))
      ;; Process next binding
      (let* ([binding (car bindings)]
             [var (car binding)]
             [init (cadr binding)]
             [init-result (infer init env)])
            (if (eq? (car init-result) 'ok)
                (let* ([init-type (cadr init-result)]
                       [s1 (caddr init-result)]
                       [combined-subst (compose-subst s1 subst)]
                       ;; Apply substitution to env before generalizing to avoid
                       ;; over-generalization of constrained type variables
                       [subst-env (map (lambda (p)
                                               (cons (car p)
                                                     (apply-subst combined-subst (cdr p))))
                                       env)]
                       [gen-type (generalize subst-env (apply-subst combined-subst init-type))]
                       [new-env (tenv-extend env var gen-type)])
                      (infer-let (cdr bindings) body new-env combined-subst))
                init-result))))

(doc 'section 'if-inference)

(define (infer-if test then-expr else-expr env)
  (doc 'type (-> Expr Expr Expr TEnv (Result Type Error)))
  (doc 'description "Infer type for if expression, checking test is Bool and branches unify.")
  (doc 'export #t)
  (let ([test-result (infer test env)])
       (if (not (eq? (car test-result) 'ok))
           test-result
           (let* ([test-type (cadr test-result)]
                  [s1 (caddr test-result)]
                  ;; Check test is Bool
                  [bool-unify (unify-with s1 test-type 'Bool)])
                 (if (not (eq? (car bool-unify) 'ok))
                     `(error if-test-not-bool ,test-type)
                     (let* ([s2 (cadr bool-unify)]
                            [then-result (infer then-expr env)])
                           (if (not (eq? (car then-result) 'ok))
                               then-result
                               (let* ([then-type (cadr then-result)]
                                      [s3 (compose-subst (caddr then-result) s2)]
                                      [else-result (infer else-expr env)])
                                     (if (not (eq? (car else-result) 'ok))
                                         else-result
                                         (let* ([else-type (cadr else-result)]
                                                [s4 (compose-subst (caddr else-result) s3)]
                                                ;; Unify branches
                                                [branch-unify (unify-with s4
                                                                          (apply-subst s4 then-type)
                                                                          (apply-subst s4 else-type))])
                                               (if (eq? (car branch-unify) 'ok)
                                                   (let ([s5 (cadr branch-unify)])
                                                        `(ok ,(apply-subst s5 then-type) ,s5))
                                                   branch-unify)))))))))))

(doc 'section 'case-inference)
(doc 'note "Pattern matching on blocks. Scrutinee must be a Block type, each clause binds refs to variables, all bodies must have the same type.")

(define (infer-case scrutinee clauses env)
  (doc 'type (-> Expr (List Clause) TEnv (Result Type Error)))
  (doc 'description "Infer type for case expression.")
  (doc 'export #t)
  (let ([scrut-result (infer scrutinee env)])
       (if (not (eq? (car scrut-result) 'ok))
           scrut-result
           (let* ([scrut-type (cadr scrut-result)]
                  [s1 (caddr scrut-result)])
                 ;; Scrutinee must be a block type or type variable
                 ;; For now, we allow any type and check at runtime
                 (infer-case-clauses clauses s1 env #f)))))

(define (infer-case-clauses clauses subst env result-type)
  (doc 'type (-> (List Clause) Subst TEnv (Maybe Type) (Result Type Error)))
  (doc 'description "Process each clause, accumulating substitution and result type.")
  (if (null? clauses)
      (if result-type
          `(ok ,(apply-subst subst result-type) ,subst)
          `(error no-clauses-in-case))
      (let* ([clause (car clauses)]
             [pattern (car clause)]
             [body (cadr clause)]
             [tag (car pattern)]
             [vars (cdr pattern)]
             ;; Each bound var gets a fresh type (refs are hashes)
             [var-types (map (lambda (_) 'Hash) vars)]
             [clause-env (tenv-extend* env (map cons vars var-types))]
             [body-result (infer body clause-env)])
            (if (not (eq? (car body-result) 'ok))
                body-result
                (let* ([body-type (cadr body-result)]
                       [s2 (compose-subst (caddr body-result) subst)])
                      (if result-type
                          ;; Unify with previous clause result type
                          (let ([unify-result (unify-with s2 body-type result-type)])
                               (if (eq? (car unify-result) 'ok)
                                   (infer-case-clauses
                                    (cdr clauses)
                                    (cadr unify-result)
                                    env
                                    (apply-subst (cadr unify-result) body-type))
                                   unify-result))
                          ;; First clause establishes result type
                          (infer-case-clauses
                           (cdr clauses) s2 env body-type)))))))

(doc 'section 'application-inference)

(define (infer-app fn args env)
  (doc 'type (-> Expr (List Expr) TEnv (Result Type Error)))
  (doc 'description "Infer type for function application.")
  (doc 'export #t)
  (let ([fn-result (infer fn env)])
       (if (eq? (car fn-result) 'ok)
           (let* ([fn-type (cadr fn-result)]
                  [s1 (caddr fn-result)])
                 (infer-app-args fn-type args s1 env))
           fn-result)))

(define (infer-app-args fn-type args s env)
  (doc 'type (-> Type (List Expr) Subst TEnv (Result Type Error)))
  (doc 'description "Infer application with known function type.")
  (if (null? args)
      `(ok ,(apply-subst s fn-type) ,s)
      (let ([fn-type (apply-subst s fn-type)])
           (cond
            ;; Function type
            [(function-type? fn-type)
             (let* ([param-types (function-param-types fn-type)]
                    [return-type (function-return-type fn-type)])
                   (if (= (length args) (length param-types))
                       (let ([result (check-args args param-types s env)])
                            (if (eq? (car result) 'ok)
                                `(ok ,(apply-subst (cadr result) return-type)
                                  ,(cadr result))
                                result))
                       `(error arity-mismatch
                         (expected ,(length param-types))
                         (got ,(length args)))))]
            ;; Type variable — create function type
            [(type-var? fn-type)
             (let* ([arg-types (map (lambda (_) (fresh-tvar)) args)]
                    [ret-type (fresh-tvar)]
                    [expected-fn-type (cons '-> (append arg-types (list ret-type)))]
                    [unify-result (unify-with s fn-type expected-fn-type)])
                   (if (eq? (car unify-result) 'ok)
                       (let ([s2 (cadr unify-result)])
                            (let ([result (check-args args arg-types s2 env)])
                                 (if (eq? (car result) 'ok)
                                     `(ok ,(apply-subst (cadr result) ret-type)
                                       ,(cadr result))
                                     result)))
                       unify-result))]
            [else `(error not-a-function ,fn-type)]))))

(define (check-args args types s env)
  (doc 'type (-> (List Expr) (List Type) Subst TEnv (Result Subst Error)))
  (doc 'description "Check a list of arguments against expected types.")
  (if (null? args)
      `(ok ,s)
      ;; Apply current substitution to env so inner inference sees resolved types.
      ;; Without this, double-application (f (f x)) fails to unify:
      ;; inner (f x) re-infers f with fresh vars instead of seeing f's resolved type.
      (let ([result (check (car args) (apply-subst s (car types))
                           (apply-subst-to-env s env))])
           (if (eq? (car result) 'ok)
               (check-args (cdr args) (cdr types) (compose-subst (cadr result) s) env)
               result))))

(doc 'section 'type-checking)

(define (check expr expected env)
  (doc 'type (-> Expr Type TEnv (Result Subst Error)))
  (doc 'description "Check that an expression has the expected type (downward checking).")
  (doc 'export #t)
  (cond
   ;; Lambda against function type
   [(and (pair? expr) (eq? (car expr) 'fn) (function-type? expected))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (function-param-types expected)]
           [return-type (function-return-type expected)])
          (if (= (length params) (length param-types))
              (let* ([new-env (tenv-extend* env (map cons params param-types))]
                     [result (check body return-type new-env)])
                    result)
              `(error arity-mismatch
                (expected ,(length param-types))
                (got ,(length params)))))]
   
   ;; Polymorphic type — instantiate and check
   [(and (pair? expected) (eq? (car expected) '∀))
    (check expr (instantiate expected) env)]
   
   ;; Fall back to inference and unification
   [else
    (let ([result (infer expr env)])
         (if (eq? (car result) 'ok)
             (let* ([inferred (cadr result)]
                    [s1 (caddr result)]
                    [unify-result (unify-with s1 inferred expected)])
                   (if (eq? (car unify-result) 'ok)
                       `(ok ,(cadr unify-result))
                       unify-result))
             result))]))

(doc 'section 'primitive-type-inference)

(doc prim-types 'type (List (Pair Symbol Type)))
(doc prim-types 'description "Type signatures for built-in primitives.")
(doc prim-types 'export #t)
(define prim-types
  `(;; Arithmetic
    (add . (-> Int Int Int))
    (sub . (-> Int Int Int))
    (mul . (-> Int Int Int))
    (div . (-> Int Int Int))
    (mod . (-> Int Int Int))
    (neg . (-> Int Int))
    (abs . (-> Int Int))
    
    ;; Comparison
    (eq? . (∀ (a) (-> a a Bool)))
    (lt? . (-> Int Int Bool))
    (le? . (-> Int Int Bool))
    (gt? . (-> Int Int Bool))
    (ge? . (-> Int Int Bool))
    (zero? . (-> Int Bool))
    (positive? . (-> Int Bool))
    (negative? . (-> Int Bool))
    
    ;; Bitwise
    (bitand . (-> Int Int Int))
    (bitor . (-> Int Int Int))
    (bitxor . (-> Int Int Int))
    (bitnot . (-> Int Int))
    (shl . (-> Int Int Int))
    (shr . (-> Int Int Int))
    
    ;; Boolean
    (not . (-> Bool Bool))
    (and . (-> Bool Bool Bool))
    (or . (-> Bool Bool Bool))
    
    ;; List operations
    (cons . (∀ (a) (-> a (List a) (List a))))
    (car . (∀ (a) (-> (List a) a)))
    (cdr . (∀ (a) (-> (List a) (List a))))
    (null? . (∀ (a) (-> (List a) Bool)))
    (pair? . (∀ (a) (-> a Bool)))
    (length . (∀ (a) (-> (List a) Int)))
    (append . (∀ (a) (-> (List a) (List a) (List a))))
    (reverse . (∀ (a) (-> (List a) (List a))))
    (list-ref . (∀ (a) (-> (List a) Int a)))
    (memq . (∀ (a) (-> a (List a) (List a))))
    (assq . (∀ (a b) (-> a (List (× a b)) (× a b))))
    
    ;; Higher-order list operations (core FP primitives)
    (map . (∀ (a b) (-> (-> a b) (List a) (List b))))
    (filter . (∀ (a) (-> (-> a Bool) (List a) (List a))))
    (foldr . (∀ (a b) (-> (-> a b b) b (List a) b)))
    (foldl . (∀ (a b) (-> (-> b a b) b (List a) b)))
    (andmap . (∀ (a) (-> (-> a Bool) (List a) Bool)))
    (ormap . (∀ (a) (-> (-> a Bool) (List a) Bool)))
    (find . (∀ (a) (-> (-> a Bool) (List a) a)))  ; May fail if not found
    (take . (∀ (a) (-> Int (List a) (List a))))
    (drop . (∀ (a) (-> Int (List a) (List a))))
    (zip . (∀ (a b) (-> (List a) (List b) (List (× a b)))))
    
    ;; Vector operations
    (vec-ref . (∀ (a) (-> (Vector a) Int a)))
    (vec-length . (∀ (a) (-> (Vector a) Int)))
    (vec->list . (∀ (a) (-> (Vector a) (List a))))
    (list->vec . (∀ (a) (-> (List a) (Vector a))))
    
    ;; String operations
    (string-length . (-> String Int))
    (string-ref . (-> String Int Char))
    (string-append . (-> String String String))
    (substring . (-> String Int Int String))
    (string=? . (-> String String Bool))
    (string<? . (-> String String Bool))
    (string>? . (-> String String Bool))
    (string->list . (-> String (List Char)))
    (list->string . (-> (List Char) String))
    (string->utf8 . (-> String Bytes))
    (utf8->string . (-> Bytes String))
    (symbol->string . (-> Symbol String))
    (string->symbol . (-> String Symbol))
    
    ;; Character operations
    (char->integer . (-> Char Int))
    (integer->char . (-> Int Char))
    (char=? . (-> Char Char Bool))
    (char<? . (-> Char Char Bool))
    (char-alphabetic? . (-> Char Bool))
    (char-numeric? . (-> Char Bool))
    (char-whitespace? . (-> Char Bool))
    (char-upper-case? . (-> Char Bool))
    (char-lower-case? . (-> Char Bool))
    (char-upcase . (-> Char Char))
    (char-downcase . (-> Char Char))
    
    ;; Bytevector operations
    (bv-length . (-> Bytes Int))
    (bv-ref . (-> Bytes Int Int))
    (bv-slice . (-> Bytes Int Int Bytes))
    
    ;; Block operations
    (block-tag . (∀ (t p) (-> (Block t p) Symbol)))
    (block-payload . (∀ (t p) (-> (Block t p) p)))
    (block-refs . (∀ (t p) (-> (Block t p) (Vector Hash))))
    (block-ref . (∀ (t p) (-> (Block t p) Int Hash)))
    (block->bytes . (∀ (t p) (-> (Block t p) Bytes)))
    (block? . (∀ (a) (-> a Bool)))
    (make-block . (∀ (t p) (-> Symbol p (Vector Hash) (Block t p))))
    (bytes->block . (∀ (t p) (-> Bytes (Block t p))))
    
    ;; Hash operations
    (sha256 . (-> Bytes Hash))
    (hash-block . (∀ (t p) (-> (Block t p) Hash)))
    (hash->hex . (-> Hash String))
    (hex->hash . (-> String Hash))
    
    ;; Type predicates
    (number? . (∀ (a) (-> a Bool)))
    (integer? . (∀ (a) (-> a Bool)))
    (symbol? . (∀ (a) (-> a Bool)))
    (string? . (∀ (a) (-> a Bool)))
    (char? . (∀ (a) (-> a Bool)))
    (bytevector? . (∀ (a) (-> a Bool)))
    (vector? . (∀ (a) (-> a Bool)))
    (list? . (∀ (a) (-> a Bool)))
    (boolean? . (∀ (a) (-> a Bool)))
    (procedure? . (∀ (a) (-> a Bool)))))

(define (lookup-prim-type op)
  (doc 'type (-> Symbol (Maybe Type)))
  (doc 'description "Look up the type of a primitive operation.")
  (doc 'export #t)
  (let ([entry (assq op prim-types)])
       (if entry (cdr entry) #f)))

(doc 'section 'declared-types)
(doc 'note "Support for explicit type declarations from (doc symbol 'type ...) annotations. These are authoritative when present, taking precedence over inference.")

(doc *declared-types* 'type '(Hashtable Symbol Type))
(doc *declared-types* 'description "Types declared via doc annotations. Maps symbol -> type.")
(define *declared-types* (make-eq-hashtable))

(doc register-declared-type! 'type '(-> Symbol Type Void))
(doc register-declared-type! 'description "Register an explicitly declared type for a symbol.")
(doc register-declared-type! 'export #t)
(define (register-declared-type! sym type)
  (hashtable-set! *declared-types* sym type))

(doc lookup-declared-type 'type '(-> Symbol (Maybe Type)))
(doc lookup-declared-type 'description "Look up an explicitly declared type.")
(doc lookup-declared-type 'export #t)
(define (lookup-declared-type sym)
  (let ([found (hashtable-ref *declared-types* sym #f)])
    found))

(doc clear-declared-types! 'type '(-> Void))
(doc clear-declared-types! 'description "Clear all declared types (for testing/reset).")
(doc clear-declared-types! 'export #t)
(define (clear-declared-types!)
  (set! *declared-types* (make-eq-hashtable)))

(doc build-tenv-from-declared-types 'type '(-> (List Symbol) TEnv))
(doc build-tenv-from-declared-types 'description "Build a type environment from declared types for the given symbols.")
(doc build-tenv-from-declared-types 'export #t)
(define (build-tenv-from-declared-types syms)
  (let loop ([syms syms] [env empty-tenv])
    (if (null? syms)
        env
        (let* ([sym (car syms)]
               [declared (lookup-declared-type sym)])
          (if declared
              (loop (cdr syms) (tenv-extend env sym declared))
              (loop (cdr syms) env))))))

(doc register-doc-types! 'type '(-> (List (Pair Symbol Type)) Void))
(doc register-doc-types! 'description "Register multiple declared types from a list of (symbol . type) pairs. Type can be quoted (from doc annotation) or direct.")
(doc register-doc-types! 'export #t)
(define (register-doc-types! type-decls)
  (for-each
   (lambda (decl)
     (let* ([sym (car decl)]
            [type-expr (cdr decl)]
            ;; Handle quoted types: '(-> Int Int) -> (-> Int Int)
            [type (if (and (pair? type-expr) (eq? (car type-expr) 'quote))
                      (cadr type-expr)
                      type-expr)])
       (register-declared-type! sym type)))
   type-decls))

(define (infer-prim op args env)
  (doc 'type (-> Symbol (List Expr) TEnv (Result Type Error)))
  (doc 'description "Infer type for a primitive call.")
  (doc 'export #t)
  (let ([prim-type (lookup-prim-type op)])
       (if prim-type
           (let ([inst-type (instantiate prim-type)])
                (infer-app-args inst-type args empty-subst env))
           `(error unknown-primitive ,op))))

(doc 'section 'quoted-literals)

(define (infer-quoted datum)
  (doc 'type (-> Any (Result Type Subst)))
  (doc 'description "Infer type for a quoted literal.")
  (doc 'export #t)
  (cond
   [(symbol? datum) `(ok Symbol ,empty-subst)]
   [(number? datum) `(ok Int ,empty-subst)]
   [(string? datum) `(ok String ,empty-subst)]
   [(null? datum) `(ok (List ?) ,empty-subst)]
   [(pair? datum)
    ;; List of things — try to infer element type
    (let ([elem-result (infer-quoted (car datum))])
         (if (eq? (car elem-result) 'ok)
             `(ok (List ,(cadr elem-result)) ,empty-subst)
             `(ok (List ?) ,empty-subst)))]
   [else `(ok ? ,empty-subst)]))

(doc 'section 'special-forms)

(define (special-form? s)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a symbol is a special form.")
  (doc 'export #t)
  (and (symbol? s)
       (memq s '(fn let fix if case prim quote :))))

(doc 'section 'convenience-api)

(define (typeof expr)
  (doc 'type (-> Expr (Either Type Error)))
  (doc 'description "Infer the type of an expression in the empty environment.")
  (doc 'export #t)
  (reset-fresh!)
  (let ([result (infer expr empty-tenv)])
       (if (eq? (car result) 'ok)
           (let ([type (cadr result)]
                 [s (caddr result)])
                (generalize '() (apply-subst s type)))
           result)))

(define (typecheck expr type)
  (doc 'type (-> Expr Type (Either Boolean Error)))
  (doc 'description "Check that an expression has the given type.")
  (doc 'export #t)
  (reset-fresh!)
  (let ([result (check expr type empty-tenv)])
       (if (eq? (car result) 'ok)
           #t
           result)))

(doc 'section 'pretty-error-messages)

(define (format-type-error err)
  (doc 'type (-> Error String))
  (doc 'description "Format a type error as a human-readable string.")
  (doc 'export #t)
  (case (cadr err)
        [(unbound-variable)
         (format "Unbound variable: ~a" (caddr err))]
        [(type-mismatch)
         (format "Type mismatch: expected ~a, got ~a"
                 (type->string (caddr err))
                 (type->string (cadddr err)))]
        [(arity-mismatch)
         (format "Arity mismatch: ~a" (cddr err))]
        [(occurs-check)
         (format "Infinite type: ~a occurs in ~a"
                 (caddr err) (type->string (cadddr err)))]
        [(not-a-function)
         (format "Not a function: ~a" (type->string (caddr err)))]
        [(unknown-primitive)
         (format "Unknown primitive: ~a" (caddr err))]
        [(no-instance-found)
         (format "No type class instance found for constraint: ~a" (caddr err))]
        [else (format "~s" err)]))

(doc 'section 'constraint-handling)
(doc 'note "Type class constraint solving integration with resolve.ss. Flow: 1) Extract constraints from (=> C T) types, 2) Apply substitution, 3) Resolve via instance database, 4) Return evidence for runtime dispatch.")

(define (instantiate-constrained type)
  (doc 'type (-> Type (Pair Type (List Constraint))))
  (doc 'description "Instantiate a possibly-constrained polymorphic type.")
  (doc 'export #t)
  (if (and (pair? type) (eq? (car type) '∀))
      (let* ([vars (cadr type)]
             [body (caddr type)]
             [fresh-vars (map (lambda (v)
                                      (if (kinded-tvar? v)
                                          (fresh-tvar)
                                          (fresh-tvar)))
                              vars)]
             [var-names (map (lambda (v)
                                     (if (kinded-tvar? v)
                                         (kinded-tvar-name v)
                                         v))
                             vars)]
             [s (map cons var-names fresh-vars)]
             [inst-body (apply-subst s body)])
            ;; Check if instantiated body has constraints
            (if (constrained-type? inst-body)
                (list (get-underlying-type inst-body)
                      (get-constraints inst-body))
                (list inst-body '())))
      ;; Not polymorphic — check for constraints
      (if (constrained-type? type)
          (list (get-underlying-type type) (get-constraints type))
          (list type '()))))

(define (apply-subst-to-constraint s c)
  (doc 'type (-> Subst Constraint Constraint))
  (doc 'description "Apply a substitution to a single constraint.")
  (list (constraint-class c)
        (apply-subst s (constraint-type c))))

(define (apply-subst-to-constraints s cs)
  (doc 'type (-> Subst (List Constraint) (List Constraint)))
  (doc 'description "Apply a substitution to a list of constraints.")
  (doc 'export #t)
  (map (lambda (c) (apply-subst-to-constraint s c)) cs))

(doc 'section 'constraint-solving)

(define (solve-constraints constraints idb)
  (doc 'type (-> (List Constraint) IDB (Result (List Evidence) Error)))
  (doc 'description "Solve constraints using the instance database.")
  (doc 'export #t)
  (if (null? constraints)
      '(ok ())
      (let ([result (resolve (car constraints) idb)])
           (if (eq? (car result) 'ok)
               (let ([rest-result (solve-constraints (cdr constraints) idb)])
                    (if (eq? (car rest-result) 'ok)
                        `(ok ,(cons (cadr result) (cadr rest-result)))
                        rest-result))
               result))))

(doc 'section 'inference-with-constraints)

(define (infer-with-constraints expr env)
  (doc 'type (-> Expr TEnv (Result (List Type Subst (List Constraint)) Error)))
  (doc 'description "Infer type along with collected constraints.")
  (doc 'export #t)
  (let ([result (infer expr env)])
       (if (eq? (car result) 'ok)
           `(ok ,(cadr result) ,(caddr result) ())
           result)))

(define (typeof-constrained expr idb)
  (doc 'type (-> Expr IDB (Result (Pair Type (List Evidence)) Error)))
  (doc 'description "Infer type with constraint solving, returning evidence.")
  (doc 'export #t)
  (reset-fresh!)
  (let ([result (infer-with-constraints expr empty-tenv)])
       (if (eq? (car result) 'ok)
           (let* ([type (cadr result)]
                  [s (caddr result)]
                  [constraints (cadddr result)]
                  [solved-constraints (apply-subst-to-constraints s constraints)]
                  [solve-result (solve-constraints solved-constraints idb)])
                 (if (eq? (car solve-result) 'ok)
                     ;; Success: return generalized type with evidence
                     (let ([gen-type (generalize '() (apply-subst s type))])
                          `(ok ,gen-type ,(cadr solve-result)))
                     solve-result))
           result)))

(doc 'section 'constrained-type-environment)
(doc 'note "For type checking with type classes, we need a richer environment that includes class method types.")

(define (make-class-env class-db)
  (doc 'type (-> ClassDB TEnv))
  (doc 'description "Build a type environment from a class database.")
  (doc 'export #t)
  (append-map (lambda (class-entry)
                      (let* ([name (car class-entry)]
                             [tc (cdr class-entry)]
                             [methods (typeclass-methods tc)])
                            (map (lambda (m)
                                         (cons (car m) (cdr m)))
                                 methods)))
              class-db))

(define (get-standard-class-tenv)
  (doc 'type (-> TEnv))
  (doc 'description "Get a type environment containing standard type class methods.")
  (doc 'export #t)
  (make-class-env standard-classes))
