(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/infer.ss")

(doc 'module 'rank-n)
(doc 'description "Extends the type system to support higher-rank polymorphism, allowing universal quantifiers (∀) to appear in contravariant positions.")
(doc 'layer 'core)

(doc 'note "Rank-1: ∀a. a → a (Quantifier at top level only)")
(doc 'note "Rank-2: (∀a. a → a) → Int (Quantifier in argument)")
(doc 'note "Rank-N: Arbitrary nesting (Full impredicativity)")

(doc 'note "Key use cases enabled:")
(doc 'note "1. Proper Lens encoding: type Lens s t a b = ∀f. Functor f => (a → f b) → s → f t")
(doc 'note "2. ST monad (safe mutation): runST : (∀s. ST s a) → a")
(doc 'note "3. Continuation-based APIs: callCC : ((∀b. a → Cont r b) → Cont r a) → Cont r a")

(doc 'note "Implementation based on: Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism (Dunfield & Krishnaswami, 2013)")
(doc 'note "This is Core code: pure, total, assumes perfect input.")

(doc 'section 'forall-type-predicates)

(define (forall-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if this is a universally quantified type.")
  (doc 'export #t)
  (and (pair? t)
       (or (eq? (car t) 'forall)
           (eq? (car t) (string->symbol (string (integer->char 8704)))))))

(doc 'section 'type-rank-calculation)

(doc 'note "The rank of a type determines how nested its quantifiers are:")
(doc 'note "Rank 0: No quantifiers (monomorphic)")
(doc 'note "Rank 1: Quantifiers only at top level")
(doc 'note "Rank 2: Quantifiers in argument positions of rank-1 types")
(doc 'note "Rank N: Maximum nesting depth of quantifiers in negative positions")

(define (type-rank type)
  (doc 'type (-> Type Nat))
  (doc 'description "Calculate the rank of a type. Rank measures how deeply nested ∀ quantifiers appear in negative positions.")
  (doc 'export #t)
  (type-rank-with type 0))

(define (type-rank-with type neg-depth)
  (doc 'type (-> Type Nat Nat))
  (doc 'description "Calculate rank tracking depth of negative positions. neg-depth is the count of argument positions traversed.")
  (doc 'export #t)
  (cond
   [(or (base-type? type) (type-var? type) (hole? type))
    0]
   [(not (pair? type)) 0]

   [(eq? (car type) '∀)
    (let ([body-rank (type-rank-with (caddr type) neg-depth)]
          [this-rank (+ 1 neg-depth)])
         (max this-rank body-rank))]

   [(eq? (car type) '->)
    (let* ([args (function-param-types type)]
           [ret (function-return-type type)]
           [arg-ranks (map (lambda (a) (type-rank-with a (+ 1 neg-depth))) args)]
           [ret-rank (type-rank-with ret neg-depth)])
          (apply max (cons ret-rank arg-ranks)))]

   [(eq? (car type) '×)
    (apply max 0 (map (lambda (t) (type-rank-with t neg-depth)) (cdr type)))]

   [(eq? (car type) '+)
    (apply max 0 (map (lambda (v)
                              (apply max 0 (map (lambda (t) (type-rank-with t neg-depth)) (cdr v))))
                      (cdr type)))]

   [(eq? (car type) '@)
    (apply max 0 (map (lambda (t) (type-rank-with t neg-depth)) (cdr type)))]

   [(memq (car type) '(List Vector Ref))
    (type-rank-with (cadr type) neg-depth)]

   [(eq? (car type) 'Cap)
    (type-rank-with (caddr type) neg-depth)]

   [(eq? (car type) 'μ)
    (type-rank-with (caddr type) neg-depth)]

   [(eq? (car type) 'Π)
    (let* ([bindings (cadr type)]
           [body (caddr type)]
           [domain-ranks (map (lambda (b)
                                      (type-rank-with (caddr b) (+ 1 neg-depth)))
                              bindings)]
           [body-rank (type-rank-with body neg-depth)])
          (apply max (cons body-rank domain-ranks)))]

   [else 0]))

(define (flip-polarity p)
  (doc 'type (-> Symbol Symbol))
  (doc 'export #t)
  (if (eq? p 'positive) 'negative 'positive))

(define (rank-n? type)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a higher-rank type (rank > 1)?")
  (doc 'export #t)
  (> (type-rank type) 1))

(define (rank-1? type)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a rank-1 polymorphic type?")
  (doc 'export #t)
  (= (type-rank type) 1))

(define (monomorphic? type)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a monomorphic type (rank 0)?")
  (doc 'export #t)
  (= (type-rank type) 0))

(doc 'section 'subsumption)

(doc 'note "Subsumption captures when one type is at least as polymorphic as another.")
(doc 'note "Key rules:")
(doc 'note "1. A <: A (reflexivity)")
(doc 'note "2. ∀a.A <: [τ/a]A (instantiation - left)")
(doc 'note "3. A <: ∀a.B when A <: B with a fresh (generalization - right)")
(doc 'note "4. A→B <: A'→B' when A' <: A and B <: B' (contravariance)")

(define *subsume-fresh* 0)

(define (fresh-subsume-var)
  (doc 'type (-> Symbol))
  (doc 'export #t)
  (set! *subsume-fresh* (+ *subsume-fresh* 1))
  (string->symbol (string-append "σ" (number->string *subsume-fresh*))))

(define (reset-subsume-fresh!)
  (doc 'type (-> Unit))
  (doc 'export #t)
  (set! *subsume-fresh* 0))

(define (subsumes t1 t2)
  (doc 'type (-> Type Type Boolean))
  (doc 'description "Does the first type subsume (is at least as general as) the second? subsumes(A, B) means A can be used where B is expected.")
  (doc 'export #t)
  (reset-subsume-fresh!)
  (let ([result (subsumes-with t1 t2 '())])
       (eq? (car result) 'ok)))

(define (subsumes-with t1 t2 skolems)
  (doc 'type (-> Type Type (List Symbol) (Result Unit Error)))
  (doc 'description "Check subsumption with a list of skolem variables that must remain rigid.")
  (doc 'export #t)
  (cond
   [(type=? t1 t2) '(ok)]

   [(type-var? t1)
    (if (memq t1 skolems)
        (if (type=? t1 t2)
            '(ok)
            `(error skolem-escape ,t1 ,t2))
        '(ok))]

   [(type-var? t2)
    (if (memq t2 skolems)
        (if (type=? t1 t2)
            '(ok)
            `(error skolem-escape ,t2 ,t1))
        '(ok))]

   [(or (hole? t1) (hole? t2)) '(ok)]

   [(and (pair? t1) (eq? (car t1) '∀))
    (let* ([vars (forall-vars t1)]
           [body (caddr t1)]
           [fresh-vars (map (lambda (_) (fresh-subsume-var)) vars)]
           [s (map cons vars fresh-vars)]
           [inst-body (apply-subst-rankn s body)])
          (subsumes-with inst-body t2 skolems))]

   [(and (pair? t2) (eq? (car t2) '∀))
    (let* ([vars (forall-vars t2)]
           [body (caddr t2)]
           [fresh-skolems (map (lambda (v)
                                       (string->symbol
                                        (string-append "⊥" (symbol->string v))))
                               vars)]
           [s (map cons vars fresh-skolems)]
           [inst-body (apply-subst-rankn s body)]
           [new-skolems (append fresh-skolems skolems)])
          (subsumes-with t1 inst-body new-skolems))]

   [(and (function-type? t1) (function-type? t2))
    (let* ([params1 (function-param-types t1)]
           [params2 (function-param-types t2)]
           [ret1 (function-return-type t1)]
           [ret2 (function-return-type t2)])
          (if (not (= (length params1) (length params2)))
              `(error arity-mismatch ,t1 ,t2)
              (let loop ([ps1 params1] [ps2 params2])
                   (if (null? ps1)
                       (subsumes-with ret1 ret2 skolems)
                       (let ([result (subsumes-with (car ps2) (car ps1) skolems)])
                            (if (eq? (car result) 'ok)
                                (loop (cdr ps1) (cdr ps2))
                                result))))))]

   [(and (product-type? t1) (product-type? t2))
    (let ([ts1 (product-types t1)]
          [ts2 (product-types t2)])
         (if (not (= (length ts1) (length ts2)))
             `(error arity-mismatch ,t1 ,t2)
             (let loop ([ts1 ts1] [ts2 ts2])
                  (if (null? ts1)
                      '(ok)
                      (let ([result (subsumes-with (car ts1) (car ts2) skolems)])
                           (if (eq? (car result) 'ok)
                               (loop (cdr ts1) (cdr ts2))
                               result))))))]

   [(and (pair? t1) (pair? t2) (eq? (car t1) (car t2)))
    (let loop ([ts1 (cdr t1)] [ts2 (cdr t2)])
         (cond
          [(and (null? ts1) (null? ts2)) '(ok)]
          [(or (null? ts1) (null? ts2)) `(error arity-mismatch ,t1 ,t2)]
          [else
           (let ([result (subsumes-with (car ts1) (car ts2) skolems)])
                (if (eq? (car result) 'ok)
                    (loop (cdr ts1) (cdr ts2))
                    result))]))]

   [else `(error type-mismatch ,t1 ,t2)]))

(define (apply-subst-rankn s type)
  (doc 'type (-> Subst Type Type))
  (doc 'description "Apply substitution for rank-n types with capture-avoiding renaming. When substituting into a quantified type, renames bound variables if they would capture free variables in the substitution range.")
  (doc 'export #t)
  (cond
   [(type-var? type)
    (let ([replacement (assq type s)])
         (if replacement
             (cdr replacement)
             type))]
   [(or (base-type? type) (hole? type)) type]
   [(not (pair? type)) type]

   [(eq? (car type) '∀)
    (let* ([vars (forall-vars type)]
           [body (caddr type)]
           [subst-free-vars (subst-range-free-vars s)]
           [capturing-vars (filter (lambda (v) (memq v subst-free-vars)) vars)]
           [rename-subst (map (lambda (v) (cons v (fresh-rename-var v))) capturing-vars)]
           [renamed-vars (map (lambda (v)
                                      (let ([r (assq v rename-subst)])
                                           (if r (cdr r) v)))
                              vars)]
           [body-with-renames (apply-subst-rankn rename-subst body)]
           [s* (filter (lambda (p) (not (memq (car p) renamed-vars))) s)])
          `(∀ ,renamed-vars ,(apply-subst-rankn s* body-with-renames)))]

   [(eq? (car type) 'μ)
    (let* ([var (cadr type)]
           [body (caddr type)]
           [subst-free-vars (subst-range-free-vars s)]
           [new-var (if (memq var subst-free-vars)
                        (fresh-rename-var var)
                        var)]
           [body* (if (eq? var new-var)
                      body
                      (apply-subst-rankn (list (cons var new-var)) body))]
           [s* (filter (lambda (p) (not (eq? (car p) new-var))) s)])
          `(μ ,new-var ,(apply-subst-rankn s* body*)))]

   [else
    (cons (car type)
          (map (lambda (t) (apply-subst-rankn s t)) (cdr type)))]))

(define (subst-range-free-vars s)
  (doc 'type (-> Subst (List Symbol)))
  (doc 'description "Collect all free type variables from the range of a substitution.")
  (doc 'export #t)
  (apply append (map (lambda (p) (type-free-vars (cdr p))) s)))

(define (type-free-vars type)
  (doc 'type (-> Type (List Symbol)))
  (doc 'description "Collect free type variables in a type.")
  (doc 'export #t)
  (cond
   [(type-var? type) (list type)]
   [(or (base-type? type) (hole? type) (not (pair? type))) '()]
   [(eq? (car type) '∀)
    (let ([vars (forall-vars type)]
          [body (caddr type)])
         (filter (lambda (v) (not (memq v vars))) (type-free-vars body)))]
   [(eq? (car type) 'μ)
    (let ([var (cadr type)]
          [body (caddr type)])
         (filter (lambda (v) (not (eq? v var))) (type-free-vars body)))]
   [else
    (apply append (map type-free-vars (cdr type)))]))

(define fresh-rename-counter 0)

(define (fresh-rename-var var)
  (doc 'type (-> Symbol Symbol))
  (doc 'description "Generate a fresh variable name based on the original.")
  (doc 'export #t)
  (set! fresh-rename-counter (+ 1 fresh-rename-counter))
  (string->symbol
   (string-append (symbol->string var) "$" (number->string fresh-rename-counter))))

(doc 'section 'deep-instantiation)

(doc 'note "For rank-N types, we may need to instantiate quantifiers that appear inside the type, not just at the top level.")

(define (deep-instantiate type)
  (doc 'type (-> Type Type))
  (doc 'description "Instantiate all top-level ∀ quantifiers.")
  (doc 'export #t)
  (if (and (pair? type) (eq? (car type) '∀))
      (let* ([vars (forall-vars type)]
             [body (caddr type)]
             [fresh-vars (map (lambda (_) (fresh-subsume-var)) vars)]
             [s (map cons vars fresh-vars)])
            (deep-instantiate (apply-subst-rankn s body)))
      type))

(define (deep-skolemize type)
  (doc 'type (-> Type (× Type (List Symbol))))
  (doc 'description "Replace all top-level ∀ bound variables with skolem constants. Returns the skolemized type and the list of skolems introduced.")
  (doc 'export #t)
  (deep-skolemize-with type '()))

(define (deep-skolemize-with type skolems)
  (doc 'type (-> Type (List Symbol) (× Type (List Symbol))))
  (doc 'export #t)
  (if (and (pair? type) (eq? (car type) '∀))
      (let* ([vars (forall-vars type)]
             [body (caddr type)]
             [fresh-skolems (map (lambda (v)
                                         (string->symbol
                                          (string-append "⊥" (symbol->string v)
                                                         (number->string (length skolems)))))
                                 vars)]
             [s (map cons vars fresh-skolems)]
             [new-body (apply-subst-rankn s body)])
            (deep-skolemize-with new-body (append fresh-skolems skolems)))
      (list type skolems)))

(doc 'section 'instantiation-at-application)

(doc 'note "When applying a polymorphic function, we need to instantiate its type. For rank-1, this is straightforward. For rank-N, we use the Quick Look approach: peek at arguments to guide instantiation.")

(define (instantiate-for-app fn-type arg-types)
  (doc 'type (-> Type (List Type) Type))
  (doc 'description "Instantiate a function type based on the types of arguments. This implements a simplified Quick Look approach.")
  (doc 'export #t)
  (cond
   [(and (pair? fn-type) (eq? (car fn-type) '∀))
    (let* ([vars (forall-vars fn-type)]
           [body (caddr fn-type)])
          (if (function-type? body)
              (let* ([fresh-vars (map (lambda (_) (fresh-subsume-var)) vars)]
                     [s (map cons vars fresh-vars)])
                    (apply-subst-rankn s body))
              fn-type))]
   [else fn-type]))

(doc 'section 'annotation-requirement-detection)

(doc 'note "Full inference is undecidable for rank > 1, so we need type annotations. This function detects when annotations are required.")

(define (needs-annotation? expr expected-type)
  (doc 'type (-> Expr Type Boolean))
  (doc 'description "Does this expression need a type annotation to check at the given type?")
  (doc 'export #t)
  (and (rank-n? expected-type)
       (and (pair? expr)
            (eq? (car expr) 'fn)
            (not (has-param-annotations? expr)))))

(define (has-param-annotations? expr)
  (doc 'type (-> Expr Boolean))
  (doc 'description "Does this lambda have parameter type annotations?")
  (doc 'export #t)
  (if (and (pair? expr) (eq? (car expr) 'fn))
      (let ([params (cadr expr)])
           (and (pair? params)
                (pair? (car params))
                (eq? (cadr (car params)) ':)))
      #f))

(doc 'section 'higher-rank-checking-extension)

(doc 'note "Key rules:")
(doc 'note "1. To check (λx.e) against (∀a.A), check (λx.e) against A with a skolem")
(doc 'note "2. To check (λx.e) against (A → B), check e against B with x:A")
(doc 'note "3. To check e against ∀a.A, introduce a as skolem and check e against A")
(doc 'note "4. Otherwise, infer and use subsumption")

(define (check-against-forall expr type env)
  (doc 'type (-> Expr Type TEnv (Result Subst Error)))
  (doc 'description "Check an expression against a universally quantified type.")
  (doc 'export #t)
  (if (forall-type? type)
      (let* ([vars (forall-vars type)]
             [body (caddr type)]
             [skolems (map (lambda (v)
                                   (string->symbol
                                    (string-append "⊥" (symbol->string v))))
                           vars)]
             [s (map cons vars skolems)]
             [skolem-body (apply-subst-rankn s body)])
            (check-rank-n-body expr skolem-body env skolems))
      `(error not-a-forall ,type)))

(define (check-rank-n-body expr type env skolems)
  (doc 'type (-> Expr Type TEnv (List Symbol) (Result Subst Error)))
  (doc 'description "Check expression against type, tracking skolems.")
  (doc 'export #t)
  (cond
   [(and (pair? expr) (eq? (car expr) 'fn) (function-type? type))
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [param-types (function-param-types type)]
           [return-type (function-return-type type)])
          (if (= (length params) (length param-types))
              (let* ([new-env (tenv-extend* env (map cons params param-types))])
                    (check-rank-n-body body return-type new-env skolems))
              `(error arity-mismatch
                (expected ,(length param-types))
                (got ,(length params)))))]

   [(and (pair? type) (eq? (car type) '∀))
    (check-against-forall expr type env)]

   [(and (pair? expr) (eq? (car expr) 'fn))
    `(error type-mismatch
      (expression lambda)
      (expected ,type)
      (reason "lambda requires function type"))]

   [else '(ok ())]))

(define (tenv-extend* env bindings)
  (doc 'type (-> TEnv (List (× Symbol Type)) TEnv))
  (doc 'export #t)
  (append bindings env))

(doc 'section 'type-annotations-for-higher-rank)

(doc 'note "For practical higher-rank programming, users must annotate lambdas with higher-rank parameter types.")
(doc 'note "Syntax: (fn ((x : (∀ (a) (-> a a)))) body)")
(doc 'note "This is checked against, not inferred.")

(define (extract-param-type param)
  (doc 'type (-> Any (+ Type #f)))
  (doc 'description "Extract the type annotation from a parameter, if present.")
  (doc 'export #t)
  (if (and (pair? param)
           (= (length param) 3)
           (eq? (cadr param) ':))
      (caddr param)
      #f))

(define (annotated-fn? expr)
  (doc 'type (-> Expr Boolean))
  (doc 'description "Is this a lambda with type-annotated parameters?")
  (doc 'export #t)
  (and (pair? expr)
       (eq? (car expr) 'fn)
       (let ([params (cadr expr)])
            (and (pair? params)
                 (andmap (lambda (p) (extract-param-type p)) params)))))

(doc 'section 'impredicativity)

(doc 'note "Impredicative polymorphism allows type variables to be instantiated with polymorphic types:")
(doc 'note "id : ∀a. a → a")
(doc 'note "id (λx.x) : (∀b. b → b) → (∀b. b → b)")
(doc 'note "Here, a is instantiated with ∀b. b → b.")
(doc 'note "This requires special handling during unification and instantiation.")

(define (is-polymorphic? type)
  (doc 'type (-> Type Boolean))
  (doc 'description "Does this type contain any ∀ quantifiers?")
  (doc 'export #t)
  (cond
   [(and (pair? type) (eq? (car type) '∀)) #t]
   [(not (pair? type)) #f]
   [else (ormap is-polymorphic? (cdr type))]))

(define (impredicative-unify t1 t2)
  (doc 'type (-> Type Type (Result Subst Error)))
  (doc 'description "Unification that allows impredicative instantiation. WARNING: This makes unification more complex and potentially non-terminating for pathological cases. Use with care.")
  (doc 'export #t)
  (cond
   [(type=? t1 t2) `(ok ())]

   [(type-var? t1)
    (if (occurs-check t1 t2)
        `(error occurs-check ,t1 ,t2)
        `(ok ((,t1 . ,t2))))]

   [(type-var? t2)
    (if (occurs-check t2 t1)
        `(error occurs-check ,t2 ,t1)
        `(ok ((,t2 . ,t1))))]

   [(and (pair? t1) (eq? (car t1) '∀)
         (pair? t2) (eq? (car t2) '∀))
    (let* ([vars1 (forall-vars t1)]
           [vars2 (forall-vars t2)]
           [body1 (caddr t1)]
           [body2 (caddr t2)])
          (if (= (length vars1) (length vars2))
              (let* ([s (map cons vars2 vars1)]
                     [renamed-body2 (apply-subst-rankn s body2)])
                    (impredicative-unify body1 renamed-body2))
              `(error arity-mismatch ,t1 ,t2)))]

   [(and (function-type? t1) (function-type? t2))
    (let* ([params1 (function-param-types t1)]
           [params2 (function-param-types t2)]
           [ret1 (function-return-type t1)]
           [ret2 (function-return-type t2)])
          (if (= (length params1) (length params2))
              (impredicative-unify-lists
               (append params1 (list ret1))
               (append params2 (list ret2)))
              `(error arity-mismatch ,t1 ,t2)))]

   [(and (pair? t1) (pair? t2) (eq? (car t1) (car t2)))
    (impredicative-unify-lists (cdr t1) (cdr t2))]

   [else `(error type-mismatch ,t1 ,t2)]))

(define (impredicative-unify-lists ts1 ts2)
  (doc 'type (-> (List Type) (List Type) (Result Subst Error)))
  (doc 'export #t)
  (cond
   [(and (null? ts1) (null? ts2)) '(ok ())]
   [(or (null? ts1) (null? ts2)) `(error arity-mismatch ,ts1 ,ts2)]
   [else
    (let ([result (impredicative-unify (car ts1) (car ts2))])
         (if (eq? (car result) 'ok)
             (let* ([s (cadr result)]
                    [rest-ts1 (map (lambda (t) (apply-subst-rankn s t)) (cdr ts1))]
                    [rest-ts2 (map (lambda (t) (apply-subst-rankn s t)) (cdr ts2))]
                    [rest-result (impredicative-unify-lists rest-ts1 rest-ts2)])
                   (if (eq? (car rest-result) 'ok)
                       `(ok ,(append s (cadr rest-result)))
                       rest-result))
             result))]))

(define (occurs-check var type)
  (doc 'type (-> Symbol Type Boolean))
  (doc 'description "Does the variable occur in the type?")
  (doc 'export #t)
  (cond
   [(type-var? type) (eq? var type)]
   [(or (base-type? type) (hole? type)) #f]
   [(not (pair? type)) #f]
   [(eq? (car type) '∀)
    (let ([vars (forall-vars type)])
         (if (memq var vars)
             #f
             (occurs-check var (caddr type))))]
   [(eq? (car type) 'μ)
    (if (eq? var (cadr type))
        #f
        (occurs-check var (caddr type)))]
   [else (ormap (lambda (t) (occurs-check var t)) (cdr type))]))

(doc 'section 'pretty-printing)

(define (rank-n-type->string type)
  (doc 'type (-> Type String))
  (doc 'description "Pretty-print a higher-rank type with proper parenthesization.")
  (doc 'export #t)
  (cond
   [(symbol? type) (symbol->string type)]
   [(eq? type '?) "?"]

   [(and (pair? type) (eq? (car type) '∀))
    (let ([vars (forall-vars type)]
          [body (caddr type)])
         (string-append "∀"
                        (join-strings " " (map symbol->string vars))
                        ". "
                        (rank-n-type->string body)))]

   [(and (pair? type) (eq? (car type) '->))
    (let ([args (function-param-types type)]
          [ret (function-return-type type)])
         (string-append "("
                        (join-strings " → "
                                      (map (lambda (a)
                                                   (if (and (pair? a) (eq? (car a) '∀))
                                                       (string-append "(" (rank-n-type->string a) ")")
                                                       (rank-n-type->string a)))
                                           args))
                        " → "
                        (rank-n-type->string ret)
                        ")"))]

   [(pair? type)
    (string-append "("
                   (join-strings " " (map rank-n-type->string type))
                   ")")]

   [else (format "~s" type)]))

(doc 'section 'convenience-api)

(define (requires-annotation type)
  (doc 'type (-> Type Boolean))
  (doc 'description "Does this type require annotation for inference to work?")
  (doc 'export #t)
  (> (type-rank type) 1))

(define (can-infer? type)
  (doc 'type (-> Type Boolean))
  (doc 'description "Can we fully infer expressions of this type?")
  (doc 'export #t)
  (<= (type-rank type) 1))

(doc 'section 'examples-and-type-signatures)

(doc 'note "Common higher-rank type patterns:")

(doc type-runST 'type Type)
(doc type-runST 'description "Example [Rank-2] - runST : (∀s. ST s a) → a")
(doc type-runST 'export #t)
(define type-runST
  '(-> (∀ (s) (ST s a)) a))

(doc type-lens 'type (-> Type Type Type Type Type))
(doc type-lens 'description "Example [Rank-2] - Proper Lens: Lens s t a b = ∀f. Functor f => (a → f b) → s → f t")
(doc type-lens 'export #t)
(define (type-lens s t a b)
  `(∀ (f) (-> (-> ,a (,'@ f ,b)) (-> ,s (,'@ f ,t)))))

(doc type-callCC 'type (-> Type Type Type))
(doc type-callCC 'description "Example [Rank-2] - callCC : ((∀b. a → Cont r b) → Cont r a) → Cont r a")
(doc type-callCC 'export #t)
(define (type-callCC a r)
  `(-> (-> (∀ (b) (-> ,a (Cont ,r b))) (Cont ,r ,a)) (Cont ,r ,a)))

(doc type-id 'type Type)
(doc type-id 'description "Identity function (rank-1 for comparison)")
(doc type-id 'export #t)
(define type-id
  '(∀ (a) (-> a a)))

(doc type-church-pair 'type (-> Type Type Type))
(doc type-church-pair 'description "Church-encoded pairs (rank-2): pair : a → b → (∀r. (a → b → r) → r)")
(doc type-church-pair 'export #t)
(define (type-church-pair a b)
  `(-> ,a (-> ,b (∀ (r) (-> (-> ,a (-> ,b r)) r)))))

(doc 'section 'rank-n-type-inference)

(doc 'note "Full Rank-N bidirectional type inference with impredicative polymorphism.")
(doc 'note "Combining approaches from:")
(doc 'note "- Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism (Dunfield & Krishnaswami, 2013)")
(doc 'note "- A Quick Look at Impredicativity (Serrano et al., ICFP 2020)")
(doc 'note "The following inference functions were previously in rank-n-infer.ss.")

(doc 'section 'forall-type-accessors)

(define (forall-well-formed? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if a forall type is well-formed: (forall (vars...) body)")
  (doc 'export #t)
  (and (forall-type? t)
       (>= (length t) 3)
       (list? (cadr t))
       (not (null? (cadr t)))))

(define (forall-vars t)
  (doc 'type (-> ForallType (List Symbol)))
  (doc 'description "Extract the bound type variable names.")
  (doc 'export #t)
  (if (forall-type? t)
      (let ([bindings (cadr t)])
           (map (lambda (b)
                        (if (symbol? b)
                            b
                            (car b)))
                bindings))
      '()))

(define (forall-var-kinds t)
  (doc 'type (-> ForallType (List (Symbol . Kind))))
  (doc 'description "Extract bound type variables with their kinds.")
  (doc 'export #t)
  (if (forall-type? t)
      (let ([bindings (cadr t)])
           (map (lambda (b)
                        (if (symbol? b)
                            (cons b 'Type)
                            (cons (car b) (caddr b))))
                bindings))
      '()))

(define (forall-body t)
  (doc 'type (-> ForallType Type))
  (doc 'description "Extract the body type (may reference bound vars).")
  (doc 'export #t)
  (if (forall-type? t)
      (caddr t)
      t))

(doc 'section 'instantiation)

(define (rank-n-instantiate forall-t concrete-types)
  (doc 'type (-> ForallType (List Type) Type))
  (doc 'description "Instantiate a forall type with concrete types.")
  (doc 'export #t)
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

(doc 'section 'generalization)

(define (rank-n-generalize type ctx)
  (doc 'type (-> Type Context ForallType))
  (doc 'description "Generalize a type over free type variables not bound in the context. Context is an alist of (var . type) bindings. This implements let-generalization: variables that are free in the type but not free in the context are universally quantified.")
  (doc 'export #t)
  (let* ([type-fvs (type-free-vars type)]
         [ctx-fvs (apply append
                         (map (lambda (binding)
                                      (type-free-vars (cdr binding)))
                              ctx))]
         [gen-vars (filter (lambda (v) (not (memq v ctx-fvs)))
                           (unique type-fvs))])
        (if (null? gen-vars)
            type
            `(∀ ,gen-vars ,type))))

(doc 'section 'subsumption)

(define (rank-n-subsumes? type1 type2 ctx)
  (doc 'type (-> Type Type Context Boolean))
  (doc 'description "Check if type1 is at least as general as (subsumes) type2. Uses proper higher-rank subtyping with contravariance in function arguments, covariance in return types, skolemization for ∀ on the right, and instantiation for ∀ on the left. The context is used to determine which type variables are in scope.")
  (doc 'export #t)
  (subsumes type1 type2))

(define (rank-n-subsumes-result type1 type2 ctx)
  (doc 'type (-> Type Type Context (Result Unit Error)))
  (doc 'description "Like rank-n-subsumes? but returns detailed error information.")
  (doc 'export #t)
  (reset-subsume-fresh!)
  (subsumes-with type1 type2 '()))

(doc 'section 'fresh-type-variables-for-rank-n-inference)

(define *rank-n-fresh-counter* 0)

(define (reset-rank-n-fresh!)
  (doc 'type (-> Unit))
  (doc 'export #t)
  (set! *rank-n-fresh-counter* 0))

(define (fresh-rank-n-var)
  (doc 'type (-> Symbol))
  (doc 'description "Generate a fresh type variable for rank-N inference.")
  (doc 'export #t)
  (set! *rank-n-fresh-counter* (+ *rank-n-fresh-counter* 1))
  (string->symbol (string-append "ρ" (number->string *rank-n-fresh-counter*))))

(doc 'section 'quick-look-infrastructure)

(doc 'note "Quick Look (Serrano et al., ICFP 2020) guides instantiation decisions by analyzing argument structure before deciding whether to instantiate polymorphic function types.")

(define (peek-argument-structure arg env)
  (doc 'type (-> Expr TEnv Symbol))
  (doc 'description "Analyze argument to guide instantiation decisions. Returns 'lambda, 'poly-var, or 'mono.")
  (doc 'export #t)
  (cond
   [(and (pair? arg) (eq? (car arg) 'fn)) 'lambda]
   [(and (pair? arg) (eq? (car arg) ':))
    (if (forall-type? (caddr arg)) 'poly-var 'mono)]
   [(symbol? arg)
    (let ([t (tenv-lookup env arg)])
         (if (and t (forall-type? t)) 'poly-var 'mono))]
   [else 'mono]))

(define (should-delay-instantiation? fn-type args env)
  (doc 'type (-> Type (List Expr) TEnv Boolean))
  (doc 'description "Aggressive: delay if ANY argument suggests polymorphism expected.")
  (doc 'export #t)
  (and (forall-type? fn-type)
       (not (null? args))
       (let ([structures (map (lambda (a) (peek-argument-structure a env)) args)])
            (ormap (lambda (s) (memq s '(lambda poly-var))) structures))))

(doc 'section 'skolem-detection-and-escape-checking)

(define (rank-n-skolem? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if a type is a skolem constant (rigid type variable). Skolems are prefixed with ⊥ in our implementation.")
  (doc 'export #t)
  (and (symbol? t)
       (let ([s (symbol->string t)])
            (and (> (string-length s) 0)
                 (char=? (string-ref s 0) #\⊥)))))

(define (type-contains-skolem? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Check if a type contains any skolem constants.")
  (doc 'export #t)
  (cond
   [(rank-n-skolem? t) #t]
   [(not (pair? t)) #f]
   [(eq? (car t) '∀)
    (type-contains-skolem? (caddr t))]
   [else (ormap type-contains-skolem? (cdr t))]))

(define (is-inference-var? s)
  (doc 'type (-> Symbol Boolean))
  (doc 'export #t)
  (and (symbol? s)
       (let ([str (symbol->string s)])
            (and (> (string-length str) 0)
                 (char=? (string-ref str 0) #\ρ)))))

(define (subst-has-skolem-escape? s)
  (doc 'type (-> Subst Boolean))
  (doc 'description "Check if any substitution binding would let a skolem escape its scope. This prevents unsound unifications where rigid skolems leak. Note: We only check inference variables (ρ). Local instantiation vars (σ) are allowed to bind to skolems as they are scoped within the check.")
  (doc 'export #t)
  (ormap (lambda (binding)
                 (let ([var (car binding)]
                       [type (cdr binding)])
                      (and (type-var? var)
                           (not (rank-n-skolem? var))
                           (is-inference-var? var)
                           (type-contains-skolem? type))))
         s))

(doc 'section 'impredicative-unification-with-scope-safety)

(define (impredicative-unify-with s t1 t2)
  (doc 'type (-> Subst Type Type (Result Subst Error)))
  (doc 'description "Compose existing substitution with impredicative unification. CRITICAL: Check for skolem escape before allowing unification.")
  (doc 'export #t)
  (let* ([t1-applied (apply-subst-rankn s t1)]
         [t2-applied (apply-subst-rankn s t2)]
         [result (impredicative-unify t1-applied t2-applied)])
        (if (eq? (car result) 'ok)
            (let ([new-subst (cadr result)])
                 (if (subst-has-skolem-escape? new-subst)
                     `(error skolem-escape ,t1-applied ,t2-applied)
                     `(ok ,(compose-subst new-subst s))))
            result)))

(doc 'section 'bidirectional-type-inference-for-rank-n)

(doc 'note "The key insight from Dunfield & Krishnaswami:")
(doc 'note "- Synthesis (↑): Expression → Type")
(doc 'note "- Checking (↓): Expression × Type → Success/Failure")
(doc 'note "For higher-rank types, we need subsumption-based checking:")
(doc 'note "To check e ⇐ A:")
(doc 'note "1. Synthesize e ⇒ B")
(doc 'note "2. Check B <: A (subsumption)")

(define (rank-n-infer-synth expr env)
  (doc 'type (-> Expr TEnv (Result (× Type Subst) Error)))
  (doc 'description "Synthesize a type for an expression. This extends basic inference to handle higher-rank polymorphism. Key extensions: Annotations with ∀ types are preserved, not immediately instantiated. Applications of polymorphic functions may require subsumption. Let-bindings generalize over free type variables.")
  (doc 'export #t)
  (cond
   [(number? expr) `(ok Int ,empty-subst)]
   [(boolean? expr) `(ok Bool ,empty-subst)]
   [(string? expr) `(ok String ,empty-subst)]
   [(and (pair? expr) (eq? (car expr) 'quote))
    (rank-n-infer-quoted (cadr expr))]

   [(symbol? expr)
    (let ([type (tenv-lookup env expr)])
         (if type
             `(ok ,type ,empty-subst)
             `(error unbound-variable ,expr)))]

   [(not (pair? expr))
    `(error unknown-expression ,expr)]

   [(eq? (car expr) ':)
    (let* ([e (cadr expr)]
           [annot-type (caddr expr)]
           [result (rank-n-check e annot-type env)])
          (if (eq? (car result) 'ok)
              `(ok ,annot-type ,(cadr result))
              result))]

   [(eq? (car expr) 'fn)
    (let* ([params (cadr expr)]
           [body (caddr expr)]
           [annotated-params (extract-param-annotations params)]
           [param-types (map (lambda (p)
                                     (if (cdr p)
                                         (cdr p)
                                         (fresh-rank-n-var)))
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

   [(not (rank-n-special-form? (car expr)))
    (rank-n-infer-app (car expr) (cdr expr) env)]

   [(eq? (car expr) 'let)
    (rank-n-infer-let (cadr expr) (caddr expr) env)]

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

   [(eq? (car expr) 'if)
    (rank-n-infer-if (cadr expr) (caddr expr) (cadddr expr) env)]

   [else `(error unsupported-expression ,expr)]))

(doc 'section 'application-inference-for-rank-n)

(define (rank-n-infer-app fn args env)
  (doc 'type (-> Expr (List Expr) TEnv (Result (× Type Subst) Error)))
  (doc 'description "Infer type of function application. For rank-N, we use the Quick Look approach: 1. Infer function type 2. If polymorphic, instantiate based on argument structure 3. Check arguments against expected types 4. Return the return type")
  (doc 'export #t)
  (let ([fn-result (rank-n-infer-synth fn env)])
       (if (not (eq? (car fn-result) 'ok))
           fn-result
           (let* ([fn-type (cadr fn-result)]
                  [s1 (caddr fn-result)]
                  [inst-fn-type (if (should-delay-instantiation? fn-type args env)
                                    fn-type
                                    (deep-instantiate fn-type))])
                 (rank-n-infer-app-args inst-fn-type args s1 env)))))

(define (rank-n-infer-app-args fn-type args s env)
  (doc 'type (-> Type (List Expr) Subst TEnv (Result (× Type Subst) Error)))
  (doc 'export #t)
  (if (null? args)
      `(ok ,(apply-subst-rankn s fn-type) ,s)
      (let ([fn-type (apply-subst-rankn s fn-type)])
           (cond
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
            [(forall-type? fn-type)
             (rank-n-infer-app-args (deep-instantiate fn-type) args s env)]
            [else `(error not-a-function ,fn-type)]))))

(define (rank-n-check-args args types s env)
  (doc 'type (-> (List Expr) (List Type) Subst TEnv (Result Subst Error)))
  (doc 'description "Check arguments against parameter types, using impredicative unification. For polymorphic parameters, use checking (which triggers skolemization). For monomorphic parameters, synthesize and unify.")
  (doc 'export #t)
  (if (null? args)
      `(ok ,s)
      (let* ([param-type (apply-subst-rankn s (car types))]
             [result (if (forall-type? param-type)
                         (rank-n-check (car args) param-type env)
                         (let ([synth (rank-n-infer-synth (car args) env)])
                              (if (eq? (car synth) 'ok)
                                  (let ([arg-type (apply-subst-rankn (caddr synth) (cadr synth))])
                                       (impredicative-unify-with s arg-type param-type))
                                  synth)))])
            (if (eq? (car result) 'ok)
                (rank-n-check-args (cdr args) (cdr types)
                                   (compose-subst (cadr result) s) env)
                result))))

(doc 'section 'type-checking-for-rank-n)

(define (check-against-forall-infer expr type env)
  (doc 'type (-> Expr Type TEnv (Result Subst Error)))
  (doc 'description "Check an expression against a universally quantified type (with synthesis fallback).")
  (doc 'export #t)
  (if (forall-type? type)
      (let* ([vars (forall-vars type)]
             [body (forall-body type)]
             [skolems (map (lambda (v)
                                   (string->symbol
                                    (string-append "⊥" (symbol->string v))))
                           vars)]
             [s (map cons vars skolems)]
             [skolem-body (apply-subst-rankn s body)])
            (rank-n-check expr skolem-body env))
      `(error not-a-forall ,type)))

(define (rank-n-check expr expected env)
  (doc 'type (-> Expr Type TEnv (Result Subst Error)))
  (doc 'description "Check that an expression has the expected type. For rank-N types, checking follows these rules: 1. (λx.e) ⇐ (A → B): extend env with x:A, check e ⇐ B. 2. e ⇐ (∀a.A): introduce a as skolem, check e ⇐ A. 3. Otherwise: synthesize e ⇒ B, check B <: A (subsumption)")
  (doc 'export #t)
  (cond
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

   [(forall-type? expected)
    (check-against-forall-infer expr expected env)]

   [else
    (let ([result (rank-n-infer-synth expr env)])
         (if (eq? (car result) 'ok)
             (let* ([inferred (cadr result)]
                    [s1 (caddr result)]
                    [inferred-applied (apply-subst-rankn s1 inferred)]
                    [expected-applied (apply-subst-rankn s1 expected)])
                   (let ([inferred-inst (if (and (pair? inferred-applied)
                                                 (eq? (car inferred-applied) '∀))
                                            (deep-instantiate inferred-applied)
                                            inferred-applied)])
                        (let ([unify-result (impredicative-unify-with s1
                                                                      inferred-inst
                                                                      expected-applied)])
                             (if (eq? (car unify-result) 'ok)
                                 `(ok ,(cadr unify-result))
                                 `(error type-mismatch
                                   (inferred ,inferred-inst)
                                   (expected ,expected-applied))))))
             result))]))

(doc 'section 'let-inference-for-rank-n)

(define (rank-n-infer-let bindings body env)
  (doc 'type (-> (List (× Symbol Expr)) Expr TEnv (Result (× Type Subst) Error)))
  (doc 'description "Infer type of a let expression with proper generalization.")
  (doc 'export #t)
  (rank-n-infer-let-bindings bindings body env empty-subst))

(define (rank-n-infer-let-bindings bindings body env s)
  (doc 'type (-> (List (× Symbol Expr)) Expr TEnv Subst (Result (× Type Subst) Error)))
  (doc 'export #t)
  (if (null? bindings)
      (let ([result (rank-n-infer-synth body env)])
           (if (eq? (car result) 'ok)
               `(ok ,(cadr result) ,(compose-subst (caddr result) s))
               result))
      (let* ([binding (car bindings)]
             [var (car binding)]
             [init (cadr binding)]
             [init-result (rank-n-infer-synth init env)])
            (if (eq? (car init-result) 'ok)
                (let* ([init-type (cadr init-result)]
                       [s1 (caddr init-result)]
                       [combined-s (compose-subst s1 s)]
                       [init-type-applied (apply-subst-rankn combined-s init-type)]
                       [subst-env (map (lambda (p)
                                               (cons (car p)
                                                     (apply-subst-rankn combined-s (cdr p))))
                                       env)]
                       [gen-type (rank-n-generalize init-type-applied subst-env)]
                       [new-env (cons (cons var gen-type) env)])
                      (rank-n-infer-let-bindings (cdr bindings) body new-env combined-s))
                init-result))))

(doc 'section 'if-inference-for-rank-n)

(define (rank-n-infer-if test then-expr else-expr env)
  (doc 'type (-> Expr Expr Expr TEnv (Result (× Type Subst) Error)))
  (doc 'export #t)
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
                                      [branch-unify (unify-with s3
                                                                (apply-subst-rankn s3 then-type)
                                                                (apply-subst-rankn s3 else-type))])
                                     (if (eq? (car branch-unify) 'ok)
                                         (let ([s4 (cadr branch-unify)])
                                              `(ok ,(apply-subst-rankn s4 then-type) ,s4))
                                         branch-unify)))))))))

(doc 'section 'helper-functions)

(define (extract-param-annotations params)
  (doc 'type (-> (List Param) (List (× Symbol (Option Type)))))
  (doc 'description "Extract parameter names and optional type annotations. Supports: (x) or (x : Type) or just x")
  (doc 'export #t)
  (map (lambda (p)
               (cond
                [(symbol? p) (cons p #f)]
                [(and (pair? p) (= (length p) 3) (eq? (cadr p) ':))
                 (cons (car p) (caddr p))]
                [(pair? p) (cons (car p) #f)]
                [else (cons p #f)]))
       params))

(define (make-function-type param-types return-type)
  (doc 'type (-> (List Type) Type Type))
  (doc 'export #t)
  (cons '-> (append param-types (list return-type))))

(define (rank-n-special-form? s)
  (doc 'type (-> Symbol Boolean))
  (doc 'export #t)
  (and (symbol? s)
       (memq s '(fn let fix if case prim quote :))))

(define (rank-n-infer-quoted datum)
  (doc 'type (-> Any (Result (× Type Subst) Error)))
  (doc 'export #t)
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

(doc 'section 'convenience-api)

(define (rank-n-typeof expr)
  (doc 'type (-> Expr (+ Type Error)))
  (doc 'description "Infer the type of an expression using rank-N inference.")
  (doc 'export #t)
  (reset-rank-n-fresh!)
  (reset-subsume-fresh!)
  (let ([result (rank-n-infer-synth expr '())])
       (if (eq? (car result) 'ok)
           (let* ([type (cadr result)]
                  [s (caddr result)]
                  [applied-type (apply-subst-rankn s type)])
                 (rank-n-generalize applied-type '()))
           result)))

(define (rank-n-typecheck expr type)
  (doc 'type (-> Expr Type (+ Boolean Error)))
  (doc 'description "Check that an expression has the given type.")
  (doc 'export #t)
  (reset-rank-n-fresh!)
  (reset-subsume-fresh!)
  (let ([result (rank-n-check expr type '())])
       (if (eq? (car result) 'ok)
           #t
           result)))
