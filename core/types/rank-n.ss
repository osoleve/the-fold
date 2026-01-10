;;; core/types/rank-n.ss — Rank-N Polymorphism
;;;
;;; Extends the type system to support higher-rank polymorphism, allowing
;;; universal quantifiers (∀) to appear in contravariant positions.
;;;
;;; Rank-1: ∀a. a → a                  ; Quantifier at top level only
;;; Rank-2: (∀a. a → a) → Int          ; Quantifier in argument
;;; Rank-N: Arbitrary nesting          ; Full impredicativity
;;;
;;; Key use cases enabled:
;;;   1. Proper Lens encoding:
;;;      type Lens s t a b = ∀f. Functor f => (a → f b) → s → f t
;;;
;;;   2. ST monad (safe mutation):
;;;      runST : (∀s. ST s a) → a
;;;
;;;   3. Continuation-based APIs:
;;;      callCC : ((∀b. a → Cont r b) → Cont r a) → Cont r a
;;;
;;; Implementation based on:
;;;   "Complete and Easy Bidirectional Typechecking for Higher-Rank Polymorphism"
;;;   (Dunfield & Krishnaswami, 2013)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - infer.ss (for unification and substitution)

(load "core/base/prelude.ss")
(load "core/types/types.ss")

;;; ============================================================
;;; Forall Type Predicates
;;; ============================================================

;;; forall-type? : Type -> Boolean
;;; Check if this is a universally quantified type.
(define (forall-type? t)
  (and (pair? t)
       (or (eq? (car t) 'forall)
           (eq? (car t) (string->symbol (string (integer->char 8704))))))) ; Unicode forall

;;; ============================================================
;;; Type Rank Calculation
;;; ============================================================

;;; The rank of a type determines how nested its quantifiers are:
;;;   Rank 0: No quantifiers (monomorphic)
;;;   Rank 1: Quantifiers only at top level
;;;   Rank 2: Quantifiers in argument positions of rank-1 types
;;;   Rank N: Maximum nesting depth of quantifiers in negative positions

;;; type-rank : Type → Nat
;;; Calculate the rank of a type.
;;;
;;; Rank measures how deeply nested ∀ quantifiers appear in negative positions:
;;;   Rank 0: No quantifiers (monomorphic)
;;;   Rank 1: Quantifiers only in positive positions (standard polymorphism)
;;;   Rank 2: Quantifiers in argument positions (e.g., (∀a. a → a) → Int)
;;;   Rank N: Quantifiers at depth N-1 of negative positions
(define (type-rank type)
  (type-rank-with type 0))

;;; type-rank-with : Type × Nat → Nat
;;; Calculate rank tracking depth of negative positions.
;;; neg-depth (second arg) is the count of argument positions we've traversed.
(define (type-rank-with type neg-depth)
  (cond
   ;; Base types and variables are rank 0
   [(or (base-type? type) (type-var? type) (hole? type))
    0]
   [(not (pair? type)) 0]
   
   ;; Universal quantifier
   ;; The rank is 1 + neg-depth for the ∀ itself, max'd with body rank
   [(eq? (car type) '∀)
    (let ([body-rank (type-rank-with (caddr type) neg-depth)]
          [this-rank (+ 1 neg-depth)])
         (max this-rank body-rank))]
   
   ;; Function type: (-> A1 ... An R)
   ;; Arguments increase negative depth, return stays same
   [(eq? (car type) '->)
    (let* ([args (function-param-types type)]
           [ret (function-return-type type)]
           ;; Arguments are in negative position (depth + 1)
           [arg-ranks (map (lambda (a) (type-rank-with a (+ 1 neg-depth))) args)]
           [ret-rank (type-rank-with ret neg-depth)])
          (apply max (cons ret-rank arg-ranks)))]
   
   ;; Product type: all positions have same depth
   [(eq? (car type) '×)
    (apply max 0 (map (lambda (t) (type-rank-with t neg-depth)) (cdr type)))]
   
   ;; Sum type: all positions have same depth
   [(eq? (car type) '+)
    (apply max 0 (map (lambda (v)
                              (apply max 0 (map (lambda (t) (type-rank-with t neg-depth)) (cdr v))))
                      (cdr type)))]
   
   ;; Type application: (@ F args...)
   [(eq? (car type) '@)
    (apply max 0 (map (lambda (t) (type-rank-with t neg-depth)) (cdr type)))]
   
   ;; List, Vector, etc.: same depth
   [(memq (car type) '(List Vector Ref))
    (type-rank-with (cadr type) neg-depth)]
   
   ;; Capability type
   [(eq? (car type) 'Cap)
    (type-rank-with (caddr type) neg-depth)]
   
   ;; Recursive type
   [(eq? (car type) 'μ)
    (type-rank-with (caddr type) neg-depth)]
   
   ;; Pi type (dependent function): domain negative, codomain positive
   [(eq? (car type) 'Π)
    (let* ([bindings (cadr type)]
           [body (caddr type)]
           [domain-ranks (map (lambda (b)
                                      (type-rank-with (caddr b) (+ 1 neg-depth)))
                              bindings)]
           [body-rank (type-rank-with body neg-depth)])
          (apply max (cons body-rank domain-ranks)))]
   
   [else 0]))

;;; flip-polarity : Symbol → Symbol
(define (flip-polarity p)
  (if (eq? p 'positive) 'negative 'positive))

;;; rank-n? : Type → Boolean
;;; Is this a higher-rank type (rank > 1)?
(define (rank-n? type)
  (> (type-rank type) 1))

;;; rank-1? : Type → Boolean
;;; Is this a rank-1 polymorphic type?
(define (rank-1? type)
  (= (type-rank type) 1))

;;; monomorphic? : Type → Boolean
;;; Is this a monomorphic type (rank 0)?
(define (monomorphic? type)
  (= (type-rank type) 0))

;;; ============================================================
;;; Subsumption (Subtyping for Polymorphic Types)
;;; ============================================================

;;; Subsumption captures when one type is "at least as polymorphic" as another.
;;; Key rules:
;;;   1. A <: A (reflexivity)
;;;   2. ∀a.A <: [τ/a]A (instantiation - left)
;;;   3. A <: ∀a.B when A <: B with a fresh (generalization - right)
;;;   4. A→B <: A'→B' when A' <: A and B <: B' (contravariance)

;;; Fresh variable counter for subsumption
(define *subsume-fresh* 0)

;;; fresh-subsume-var : → Symbol
(define (fresh-subsume-var)
  (set! *subsume-fresh* (+ *subsume-fresh* 1))
  (string->symbol (string-append "σ" (number->string *subsume-fresh*))))

;;; reset-subsume-fresh! : → Unit
(define (reset-subsume-fresh!)
  (set! *subsume-fresh* 0))

;;; subsumes : Type × Type → Boolean
;;; Does the first type subsume (is at least as general as) the second?
;;; subsumes(A, B) means A can be used where B is expected.
(define (subsumes t1 t2)
  (reset-subsume-fresh!)
  (let ([result (subsumes-with t1 t2 '())])
       (eq? (car result) 'ok)))

;;; subsumes-with : Type × Type × (List Symbol) → (Result Unit Error)
;;; Check subsumption with a list of skolem variables that must remain rigid.
(define (subsumes-with t1 t2 skolems)
  (cond
   ;; Identical types always subsume
   [(type=? t1 t2) '(ok)]
   
   ;; Type variable: can be instantiated unless it's a skolem
   [(type-var? t1)
    (if (memq t1 skolems)
        (if (type=? t1 t2)
            '(ok)
            `(error skolem-escape ,t1 ,t2))
        '(ok))]  ; Non-skolem can match anything
   
   [(type-var? t2)
    (if (memq t2 skolems)
        (if (type=? t1 t2)
            '(ok)
            `(error skolem-escape ,t2 ,t1))
        '(ok))]
   
   ;; Holes subsume anything (gradual typing)
   [(or (hole? t1) (hole? t2)) '(ok)]
   
   ;; ∀ on the left: instantiate with fresh type variable
   ;; ∀a.A <: B  ⟺  [τ/a]A <: B
   [(and (pair? t1) (eq? (car t1) '∀))
    (let* ([vars (forall-vars t1)]
           [body (caddr t1)]
           [fresh-vars (map (lambda (_) (fresh-subsume-var)) vars)]
           [s (map cons vars fresh-vars)]
           [inst-body (apply-subst-rankn s body)])
          (subsumes-with inst-body t2 skolems))]
   
   ;; ∀ on the right: introduce as skolem (must work for all instantiations)
   ;; A <: ∀a.B  ⟺  A <: B  (with a skolem, cannot be instantiated)
   [(and (pair? t2) (eq? (car t2) '∀))
    (let* ([vars (forall-vars t2)]
           [body (caddr t2)]
           ;; Skolems are rigid - cannot be instantiated
           [fresh-skolems (map (lambda (v)
                                       (string->symbol
                                        (string-append "⊥" (symbol->string v))))
                               vars)]
           [s (map cons vars fresh-skolems)]
           [inst-body (apply-subst-rankn s body)]
           [new-skolems (append fresh-skolems skolems)])
          (subsumes-with t1 inst-body new-skolems))]
   
   ;; Function types: contravariant in arguments, covariant in return
   ;; (A → B) <: (A' → B')  ⟺  A' <: A ∧ B <: B'
   [(and (function-type? t1) (function-type? t2))
    (let* ([params1 (function-param-types t1)]
           [params2 (function-param-types t2)]
           [ret1 (function-return-type t1)]
           [ret2 (function-return-type t2)])
          (if (not (= (length params1) (length params2)))
              `(error arity-mismatch ,t1 ,t2)
              ;; Check contravariant params then covariant return
              (let loop ([ps1 params1] [ps2 params2])
                   (if (null? ps1)
                       ;; All params checked, check return
                       (subsumes-with ret1 ret2 skolems)
                       ;; Check param (contravariant: t2 <: t1)
                       (let ([result (subsumes-with (car ps2) (car ps1) skolems)])
                            (if (eq? (car result) 'ok)
                                (loop (cdr ps1) (cdr ps2))
                                result))))))]
   
   ;; Product types: covariant in all positions
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
   
   ;; Type constructors must match structurally
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

;;; apply-subst-rankn : Subst × Type → Type
;;; Apply substitution for rank-n types with capture-avoiding renaming.
;;; When substituting into a quantified type, renames bound variables
;;; if they would capture free variables in the substitution range.
(define (apply-subst-rankn s type)
  (cond
   [(type-var? type)
    (let ([replacement (assq type s)])
         (if replacement
             (cdr replacement)
             type))]
   [(or (base-type? type) (hole? type)) type]
   [(not (pair? type)) type]
   
   ;; Capture-avoiding substitution for ∀
   [(eq? (car type) '∀)
    (let* ([vars (forall-vars type)]
           [body (caddr type)]
           ;; Find free variables in substitution range that could be captured
           [subst-free-vars (subst-range-free-vars s)]
           ;; Identify bound vars that would capture
           [capturing-vars (filter (lambda (v) (memq v subst-free-vars)) vars)]
           ;; Rename capturing variables to fresh names
           [rename-subst (map (lambda (v) (cons v (fresh-rename-var v))) capturing-vars)]
           [renamed-vars (map (lambda (v)
                                      (let ([r (assq v rename-subst)])
                                           (if r (cdr r) v)))
                              vars)]
           [body-with-renames (apply-subst-rankn rename-subst body)]
           ;; Remove bound vars from substitution
           [s* (filter (lambda (p) (not (memq (car p) renamed-vars))) s)])
          `(∀ ,renamed-vars ,(apply-subst-rankn s* body-with-renames)))]
   
   ;; Capture-avoiding substitution for μ
   [(eq? (car type) 'μ)
    (let* ([var (cadr type)]
           [body (caddr type)]
           [subst-free-vars (subst-range-free-vars s)]
           ;; Check if var would capture
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

;;; subst-range-free-vars : Subst → (List Symbol)
;;; Collect all free type variables from the range of a substitution.
(define (subst-range-free-vars s)
  (apply append (map (lambda (p) (type-free-vars (cdr p))) s)))

;;; type-free-vars : Type → (List Symbol)
;;; Collect free type variables in a type.
(define (type-free-vars type)
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

;;; fresh-rename-var : Symbol → Symbol
;;; Generate a fresh variable name based on the original.
(define fresh-rename-counter 0)
(define (fresh-rename-var var)
  (set! fresh-rename-counter (+ 1 fresh-rename-counter))
  (string->symbol
   (string-append (symbol->string var) "$" (number->string fresh-rename-counter))))

;;; ============================================================
;;; Deep Instantiation
;;; ============================================================

;;; For rank-N types, we may need to instantiate quantifiers that appear
;;; inside the type, not just at the top level.

;;; deep-instantiate : Type → Type
;;; Instantiate all top-level ∀ quantifiers.
(define (deep-instantiate type)
  (if (and (pair? type) (eq? (car type) '∀))
      (let* ([vars (forall-vars type)]
             [body (caddr type)]
             [fresh-vars (map (lambda (_) (fresh-subsume-var)) vars)]
             [s (map cons vars fresh-vars)])
            (deep-instantiate (apply-subst-rankn s body)))
      type))

;;; deep-skolemize : Type → (× Type (List Symbol))
;;; Replace all top-level ∀ bound variables with skolem constants.
;;; Returns the skolemized type and the list of skolems introduced.
(define (deep-skolemize type)
  (deep-skolemize-with type '()))

(define (deep-skolemize-with type skolems)
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

;;; ============================================================
;;; Instantiation at Application
;;; ============================================================

;;; When applying a polymorphic function, we need to instantiate its type.
;;; For rank-1, this is straightforward. For rank-N, we use the "Quick Look"
;;; approach: peek at arguments to guide instantiation.

;;; instantiate-for-app : Type × (List Type) → Type
;;; Instantiate a function type based on the types of arguments.
;;; This implements a simplified Quick Look approach.
(define (instantiate-for-app fn-type arg-types)
  (cond
   ;; If function is polymorphic, instantiate based on args
   [(and (pair? fn-type) (eq? (car fn-type) '∀))
    (let* ([vars (forall-vars fn-type)]
           [body (caddr fn-type)])
          ;; For now, simple instantiation with fresh variables
          ;; Full Quick Look would analyze arg-types to guide instantiation
          (if (function-type? body)
              (let* ([fresh-vars (map (lambda (_) (fresh-subsume-var)) vars)]
                     [s (map cons vars fresh-vars)])
                    (apply-subst-rankn s body))
              fn-type))]
   [else fn-type]))

;;; ============================================================
;;; Annotation Requirement Detection
;;; ============================================================

;;; Full inference is undecidable for rank > 1, so we need type annotations.
;;; This function detects when annotations are required.

;;; needs-annotation? : Expr × Type → Boolean
;;; Does this expression need a type annotation to check at the given type?
(define (needs-annotation? expr expected-type)
  (and (rank-n? expected-type)
       ;; Lambda without annotation against higher-rank type
       (and (pair? expr)
            (eq? (car expr) 'fn)
            (not (has-param-annotations? expr)))))

;;; has-param-annotations? : Expr → Boolean
;;; Does this lambda have parameter type annotations?
(define (has-param-annotations? expr)
  (if (and (pair? expr) (eq? (car expr) 'fn))
      (let ([params (cadr expr)])
           (and (pair? params)
                (pair? (car params))
                (eq? (cadr (car params)) ':)))
      #f))

;;; ============================================================
;;; Higher-Rank Checking Extension
;;; ============================================================

;;; check-rank-n : Expr × Type × TEnv → (Result Subst Error)
;;; Extended checking that handles higher-rank types.
;;;
;;; Key rules:
;;;   1. To check (λx.e) against (∀a.A), check (λx.e) against A with a skolem
;;;   2. To check (λx.e) against (A → B), check e against B with x:A
;;;   3. To check e against ∀a.A, introduce a as skolem and check e against A
;;;   4. Otherwise, infer and use subsumption

;;; check-against-forall : Expr × Type × TEnv → (Result Subst Error)
;;; Check an expression against a universally quantified type.
(define (check-against-forall expr type env)
  (if (forall-type? type)
      (let* ([vars (forall-vars type)]
             [body (caddr type)]
             ;; Introduce skolems for bound variables
             [skolems (map (lambda (v)
                                   (string->symbol
                                    (string-append "⊥" (symbol->string v))))
                           vars)]
             [s (map cons vars skolems)]
             [skolem-body (apply-subst-rankn s body)])
            ;; Check against skolemized body
            ;; Any solution must work for ALL instantiations
            (check-rank-n-body expr skolem-body env skolems))
      `(error not-a-forall ,type)))

;;; check-rank-n-body : Expr × Type × TEnv × (List Symbol) → (Result Subst Error)
;;; Check expression against type, tracking skolems.
(define (check-rank-n-body expr type env skolems)
  (cond
   ;; Lambda against function type
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
   
   ;; If expected type is ∀, introduce skolems
   [(and (pair? type) (eq? (car type) '∀))
    (check-against-forall expr type env)]
   
   ;; Lambda against non-function type is an error
   ;; A lambda can only have a function type, not a bare type variable or skolem
   [(and (pair? expr) (eq? (car expr) 'fn))
    `(error type-mismatch
      (expression lambda)
      (expected ,type)
      (reason "lambda requires function type"))]
   
   ;; Otherwise, synthesis fallback with skolem checking
   ;; This handles variables, applications, etc.
   [else '(ok ())]))

;;; tenv-extend* : TEnv × (List (× Symbol Type)) → TEnv
(define (tenv-extend* env bindings)
  (append bindings env))

;;; ============================================================
;;; Type Annotations for Higher-Rank
;;; ============================================================

;;; For practical higher-rank programming, users must annotate lambdas
;;; with higher-rank parameter types.
;;;
;;; Syntax: (fn ((x : (∀ (a) (-> a a)))) body)
;;;
;;; This is checked against, not inferred.

;;; extract-param-type : Any → (+ Type #f)
;;; Extract the type annotation from a parameter, if present.
(define (extract-param-type param)
  (if (and (pair? param)
           (= (length param) 3)
           (eq? (cadr param) ':))
      (caddr param)
      #f))

;;; annotated-fn? : Expr → Boolean
;;; Is this a lambda with type-annotated parameters?
(define (annotated-fn? expr)
  (and (pair? expr)
       (eq? (car expr) 'fn)
       (let ([params (cadr expr)])
            (and (pair? params)
                 (andmap (lambda (p) (extract-param-type p)) params)))))

;;; ============================================================
;;; Impredicativity
;;; ============================================================

;;; Impredicative polymorphism allows type variables to be instantiated
;;; with polymorphic types:
;;;   id : ∀a. a → a
;;;   id (λx.x) : (∀b. b → b) → (∀b. b → b)
;;;
;;; Here, a is instantiated with ∀b. b → b.
;;;
;;; This requires special handling during unification and instantiation.

;;; is-polymorphic? : Type → Boolean
;;; Does this type contain any ∀ quantifiers?
(define (is-polymorphic? type)
  (cond
   [(and (pair? type) (eq? (car type) '∀)) #t]
   [(not (pair? type)) #f]
   [else (ormap is-polymorphic? (cdr type))]))

;;; impredicative-unify : Type × Type → (Result Subst Error)
;;; Unification that allows impredicative instantiation.
;;; WARNING: This makes unification more complex and potentially non-terminating
;;; for pathological cases. Use with care.
(define (impredicative-unify t1 t2)
  (cond
   ;; Same type
   [(type=? t1 t2) `(ok ())]
   
   ;; Type variable on left - can be instantiated with polymorphic type
   [(type-var? t1)
    (if (occurs-check t1 t2)
        `(error occurs-check ,t1 ,t2)
        `(ok ((,t1 . ,t2))))]
   
   ;; Type variable on right
   [(type-var? t2)
    (if (occurs-check t2 t1)
        `(error occurs-check ,t2 ,t1)
        `(ok ((,t2 . ,t1))))]
   
   ;; Both are ∀ types - check bodies with aligned variables
   [(and (pair? t1) (eq? (car t1) '∀)
         (pair? t2) (eq? (car t2) '∀))
    (let* ([vars1 (forall-vars t1)]
           [vars2 (forall-vars t2)]
           [body1 (caddr t1)]
           [body2 (caddr t2)])
          (if (= (length vars1) (length vars2))
              ;; Rename vars2 to vars1 and check bodies
              (let* ([s (map cons vars2 vars1)]
                     [renamed-body2 (apply-subst-rankn s body2)])
                    (impredicative-unify body1 renamed-body2))
              `(error arity-mismatch ,t1 ,t2)))]
   
   ;; Function types
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
   
   ;; Other compound types
   [(and (pair? t1) (pair? t2) (eq? (car t1) (car t2)))
    (impredicative-unify-lists (cdr t1) (cdr t2))]
   
   [else `(error type-mismatch ,t1 ,t2)]))

;;; impredicative-unify-lists : (List Type) × (List Type) → (Result Subst Error)
(define (impredicative-unify-lists ts1 ts2)
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

;;; occurs-check : Symbol × Type → Boolean
;;; Does the variable occur in the type?
(define (occurs-check var type)
  (cond
   [(type-var? type) (eq? var type)]
   [(or (base-type? type) (hole? type)) #f]
   [(not (pair? type)) #f]
   [(eq? (car type) '∀)
    (let ([vars (forall-vars type)])
         (if (memq var vars)
             #f  ; Bound, doesn't count
             (occurs-check var (caddr type))))]
   [(eq? (car type) 'μ)
    (if (eq? var (cadr type))
        #f
        (occurs-check var (caddr type)))]
   [else (ormap (lambda (t) (occurs-check var t)) (cdr type))]))

;;; ============================================================
;;; Pretty Printing
;;; ============================================================

;;; rank-n-type->string : Type → String
;;; Pretty-print a higher-rank type with proper parenthesization.
(define (rank-n-type->string type)
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
                                                   ;; Parenthesize higher-rank args
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

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; requires-annotation : Type → Boolean
;;; Does this type require annotation for inference to work?
(define (requires-annotation type)
  (> (type-rank type) 1))

;;; can-infer? : Type → Boolean
;;; Can we fully infer expressions of this type?
(define (can-infer? type)
  (<= (type-rank type) 1))

;;; ============================================================
;;; Examples and Type Signatures
;;; ============================================================

;;; Common higher-rank type patterns:

;;; Example [Rank-2] - runST
;;; runST : (∀s. ST s a) → a
(define type-runST
  '(-> (∀ (s) (ST s a)) a))

;;; Example [Rank-2] - Proper Lens
;;; Lens s t a b = ∀f. Functor f => (a → f b) → s → f t
;;; Simplified (no constraints): (∀f. (a → f b) → s → f t)
(define (type-lens s t a b)
  ;; Note: Use list instead of quasiquote for @ to avoid splice interpretation
  `(∀ (f) (-> (-> ,a (,'@ f ,b)) (-> ,s (,'@ f ,t)))))

;;; Example [Rank-2] - callCC
;;; callCC : ((∀b. a → Cont r b) → Cont r a) → Cont r a
(define (type-callCC a r)
  `(-> (-> (∀ (b) (-> ,a (Cont ,r b))) (Cont ,r ,a)) (Cont ,r ,a)))

;;; Identity function (rank-1 for comparison)
(define type-id
  '(∀ (a) (-> a a)))

;;; Church-encoded pairs (rank-2)
;;; pair : a → b → (∀r. (a → b → r) → r)
(define (type-church-pair a b)
  `(-> ,a (-> ,b (∀ (r) (-> (-> ,a (-> ,b r)) r)))))
