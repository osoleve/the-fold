;;; core/types/kinds.ss — Higher-Kinded Types for The Fold
;;; @module kinds
;;; @requires prelude types
;;;
;;; Types have types. We call them Kinds.
;;;
;;;   *           : The kind of ordinary types (Nat, Bool, List Nat)
;;;   * → *       : The kind of type constructors (List, Option, Vector)
;;;   * → * → *   : Binary type constructors (Either, Pair, →)
;;;   (* → *) → * : Higher-order kinds (Fix, Free)
;;;   Constraint  : The kind of type class constraints (Functor, Monad)
;;;
;;; With kinds, we can express:
;;;   - Functor : (* → *) → Constraint
;;;   - map : ∀ (f : * → *). Functor f → ∀ a b. (a → b) → f a → f b
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")

;;; Kind Grammar:
;;;
;;;   Kind ::= *                    ; Type
;;;          | (⇒ Kind Kind)        ; Kind arrow (type constructor)
;;;          | Constraint           ; Type class constraint
;;;          | Row                  ; Row kind (for extensible records)
;;;          | KVar                 ; Kind variable
;;;          | (κ∀ (KVar ...) Kind) ; Kind polymorphism
;;;
;;; Notation: We use ⇒ for kind arrows to distinguish from → (type arrows)

;;; ============================================================
;;; Kind Representation
;;; ============================================================

;;; The base kind: ordinary types
(define K* '*)

;;; The constraint kind: type class evidence
(define K-constraint 'Constraint)

;;; The row kind: for extensible records/variants
(define K-row 'Row)

;;; kind-arrow : Kind × Kind → Kind
;;; Construct a kind arrow.
(define (K=> k1 k2)
  `(⇒ ,k1 ,k2))

;;; Multi-argument kind arrow (right-associative)
(define (K=>* . kinds)
  (if (null? (cdr kinds))
      (car kinds)
      (K=> (car kinds) (apply K=>* (cdr kinds)))))

;;; kind-forall : (List Symbol) × Kind → Kind
(define (K-forall vars body)
  `(κ∀ ,vars ,body))

;;; ============================================================
;;; Dependent Kinds (Phase 1 Foundation)
;;; ============================================================

;;; Dependent Kind Grammar Extension:
;;;
;;;   Kind ::= ...
;;;          | (Πκ ((var : Kind)) Kind)   ; Dependent kind (Pi at kind level)
;;;          | □                          ; Sort (kind of kinds) - level 0
;;;          | (□ n)                      ; Leveled sort at level n
;;;
;;; Dependent kinds enable:
;;;   - Type-level functions that compute kinds
;;;   - Vector n : Nat → * → * (kind depends on value!)
;;;   - More expressive type constructors

;;; K-pi : Symbol × Kind × Kind → Kind
;;; Construct a dependent kind: Π(var : domain). codomain
;;; The codomain may mention var.
(define (K-pi var domain codomain)
  `(Πκ ((,var : ,domain)) ,codomain))

;;; K-sort : → Kind | Nat → Kind
;;; Construct the sort (kind of kinds).
;;; Without argument: □ (level 0)
;;; With argument n: □n (explicit level)
(define (K-sort . args)
  (if (null? args)
      '□
      (let ([level (car args)])
           (if (and (integer? level) (>= level 0))
               `(□ ,level)
               (error 'K-sort "level must be non-negative integer" level)))))

;;; sort? : Any → Boolean
;;; Returns true if k is a sort (□ or □n).
(define (sort? k)
  (or (eq? k '□)
      (and (pair? k)
           (eq? (car k) '□)
           (= (length k) 2)
           (integer? (cadr k))
           (>= (cadr k) 0))))

;;; sort-level : Kind → Nat
;;; Extract the level from a sort. □ has level 0.
(define (sort-level k)
  (cond
   [(eq? k '□) 0]
   [(and (pair? k) (eq? (car k) '□)) (cadr k)]
   [else (error 'sort-level "not a sort" k)]))

;;; dep-kind? : Kind → Boolean
;;; Returns true only for Πκ kinds.
(define (dep-kind? k)
  (and (pair? k)
       (eq? (car k) 'Πκ)
       (= (length k) 3)
       (let ([binding (cadr k)])
            (and (list? binding)
                 (= (length binding) 1)
                 (let ([b (car binding)])
                      (and (list? b)
                           (= (length b) 3)
                           (symbol? (car b))
                           (eq? (cadr b) ':)))))))

;;; dep-kind-var : Kind → Symbol
;;; Extract the bound variable from a dependent kind.
(define (dep-kind-var k)
  (if (dep-kind? k)
      (caar (cadr k))
      (error 'dep-kind-var "not a dependent kind" k)))

;;; dep-kind-domain : Kind → Kind
;;; Extract the domain from a dependent kind.
(define (dep-kind-domain k)
  (if (dep-kind? k)
      (caddar (cadr k))
      (error 'dep-kind-domain "not a dependent kind" k)))

;;; dep-kind-codomain : Kind → Kind
;;; Extract the codomain from a dependent kind.
(define (dep-kind-codomain k)
  (if (dep-kind? k)
      (caddr k)
      (error 'dep-kind-codomain "not a dependent kind" k)))

;;; ============================================================
;;; Kind Predicates
;;; ============================================================

;;; kind? : Any → Boolean
(define (kind? k)
  (cond
   [(eq? k '*) #t]
   [(eq? k 'Constraint) #t]
   [(eq? k 'Row) #t]
   ;; Sort (kind of kinds) - bare □
   [(eq? k '□) #t]
   ;; Kind variable (κ-prefixed symbols)
   [(kind-var? k) #t]
   [(not (pair? k)) #f]
   ;; Kind arrow
   [(eq? (car k) '⇒)
    (and (= (length k) 3)
         (kind? (cadr k))
         (kind? (caddr k)))]
   ;; Kind polymorphism
   [(eq? (car k) 'κ∀)
    (and (= (length k) 3)
         (list? (cadr k))
         (andmap symbol? (cadr k))
         (kind? (caddr k)))]
   ;; Dependent kind: (Πκ ((var : domain)) codomain)
   [(eq? (car k) 'Πκ)
    (and (= (length k) 3)
         (dep-kind? k)  ; Validates structure
         (kind? (dep-kind-domain k))
         (kind? (dep-kind-codomain k)))]
   ;; Leveled sort: (□ n)
   [(eq? (car k) '□)
    (and (= (length k) 2)
         (integer? (cadr k))
         (>= (cadr k) 0))]
   [else #f]))

;;; kind-var? : Any → Boolean
;;; Kind variables start with κ
(define (kind-var? k)
  (and (symbol? k)
       (let ([s (symbol->string k)])
            (and (> (string-length s) 1)
                 (char=? (string-ref s 0) #\κ)))))

;;; kind-arrow? : Kind → Boolean
(define (kind-arrow? k)
  (and (pair? k) (eq? (car k) '⇒)))

;;; kind-param : Kind → Kind
(define (kind-param k)
  (if (kind-arrow? k) (cadr k) #f))

;;; kind-result : Kind → Kind
(define (kind-result k)
  (if (kind-arrow? k) (caddr k) #f))

;;; ============================================================
;;; Kind Equality
;;; ============================================================

;;; kind=? : Kind × Kind → Boolean
(define (kind=? k1 k2)
  (cond
   [(and (symbol? k1) (symbol? k2)) (eq? k1 k2)]
   [(and (pair? k1) (pair? k2))
    (and (= (length k1) (length k2))
         (andmap (lambda (pair) (kind=? (car pair) (cdr pair)))
                 (map cons k1 k2)))]
   [else #f]))

;;; ============================================================
;;; Type Constructor Representation
;;; ============================================================

;;; A type constructor is a type-level function.
;;; We represent application as (@ F Arg1 Arg2 ...)
;;;
;;; Examples:
;;;   List         : * → *                (unapplied)
;;;   (@ List Nat) : *                    (fully applied = List Nat)
;;;   Either       : * → * → *            (unapplied)
;;;   (@ Either String) : * → *           (partially applied)
;;;   (@ Either String Nat) : *           (fully applied)

;;; type-app : Type × Type ... → Type
;;; Construct a type-level application.
(define (T@ f . args)
  `(@ ,f ,@args))

;;; type-app? : Type → Boolean
(define (type-app? t)
  (and (pair? t) (eq? (car t) '@)))

;;; type-app-head : Type → Type
(define (type-app-head t)
  (if (type-app? t) (cadr t) t))

;;; type-app-args : Type → (List Type)
(define (type-app-args t)
  (if (type-app? t) (cddr t) '()))

;;; ============================================================
;;; Built-in Type Constructor Kinds
;;; ============================================================

;;; The kind environment for built-in type constructors
(define builtin-kinds
  `((List    . ,(K=> K* K*))
    (Vector  . ,(K=> K* K*))
    (Option  . ,(K=> K* K*))
    (Ref     . ,(K=> K* K*))
    (->      . ,(K=>* K* K* K*))      ; Binary for now
    (×       . ,(K=>* K* K* K*))      ; Binary for now
    (+       . ,(K=>* K* K* K*))      ; Binary for now
    (Either  . ,(K=>* K* K* K*))
    (Pair    . ,(K=>* K* K* K*))
    (Result  . ,(K=>* K* K* K*))
    (Block   . ,(K=> 'Symbol (K=> K* K*)))  ; Block : Symbol → * → *
    (Cap     . ,(K=> 'Symbol (K=> K* K*)))  ; Cap : Symbol → * → *
    ;; Base types have kind *
    (Nat     . ,K*)
    (Int     . ,K*)
    (Bool    . ,K*)
    (Symbol  . ,K*)
    (String  . ,K*)
    (Bytes   . ,K*)
    (Unit    . ,K*)
    (Void    . ,K*)
    (Hash    . ,K*)))

;;; lookup-kind : Symbol → Kind | #f
(define (lookup-kind name)
  (let ([entry (assq name builtin-kinds)])
       (if entry (cdr entry) #f)))

;;; ============================================================
;;; Kind Inference
;;; ============================================================

;;; infer-kind : Type × KindEnv → Kind | Error
;;; Infer the kind of a type expression.
(define (infer-kind type kenv)
  (cond
   ;; Type variable — look up in environment
   [(symbol? type)
    (let ([builtin (lookup-kind type)])
         (if builtin
             builtin
             (let ([env-entry (assq type kenv)])
                  (if env-entry
                      (cdr env-entry)
                      `(error unknown-type ,type)))))]
   
   ;; Hole — kind is unknown, represented as kind hole
   [(eq? type '?) 'κ?]
   [(and (pair? type) (eq? (car type) '?)) 'κ?]
   
   [(not (pair? type))
    `(error invalid-type ,type)]
   
   ;; Type application: (@ F Args...)
   [(eq? (car type) '@)
    (infer-app-kind (cadr type) (cddr type) kenv)]
   
   ;; Universal quantification: (∀ (vars) body)
   ;; Vars can be simple (kind *) or kinded: (name : kind)
   ;; Quantified type has kind * if body has kind *
   [(eq? (car type) '∀)
    (let* ([vars (cadr type)]
           [body (caddr type)]
           ;; Extract variable names and their kinds
           [var-kinds (map (lambda (v)
                                   (if (and (pair? v)
                                            (= (length v) 3)
                                            (eq? (cadr v) ':))
                                       ;; Kinded var: (name : kind)
                                       (cons (car v) (caddr v))
                                       ;; Simple var: assume kind *
                                       (cons v K*)))
                           vars)]
           ;; Extend environment with variable kinds
           [new-env (append var-kinds kenv)]
           [body-kind (infer-kind body new-env)])
          (if (kind=? body-kind K*)
              K*
              `(error quantified-body-not-type ,body-kind)))]
   
   ;; Recursive type: (μ var body)
   [(eq? (car type) 'μ)
    (let* ([var (cadr type)]
           [body (caddr type)]
           [new-env (cons (cons var K*) kenv)]
           [body-kind (infer-kind body new-env)])
          (if (kind=? body-kind K*)
              K*
              `(error recursive-body-not-type ,body-kind)))]
   
   ;; Function type: (-> T1 ... Tn Tresult)
   [(eq? (car type) '->)
    (let ([arg-kinds (map (lambda (t) (infer-kind t kenv)) (cdr type))])
         (if (andmap (lambda (k) (kind=? k K*)) arg-kinds)
             K*
             `(error function-args-not-types ,arg-kinds)))]
   
   ;; Product type: (× T1 ... Tn)
   [(eq? (car type) '×)
    (let ([elem-kinds (map (lambda (t) (infer-kind t kenv)) (cdr type))])
         (if (andmap (lambda (k) (kind=? k K*)) elem-kinds)
             K*
             `(error product-elems-not-types ,elem-kinds)))]
   
   ;; Sum type: (+ (Tag T...) ...)
   [(eq? (car type) '+)
    (let ([variant-kinds
           (map (lambda (variant)
                        (map (lambda (t) (infer-kind t kenv)) (cdr variant)))
                (cdr type))])
         (if (andmap (lambda (ks) (andmap (lambda (k) (kind=? k K*)) ks)) variant-kinds)
             K*
             `(error sum-variants-not-types ,variant-kinds)))]
   
   ;; List, Vector, etc. — sugar for application
   [(eq? (car type) 'List)
    (infer-app-kind 'List (cdr type) kenv)]
   [(eq? (car type) 'Vector)
    (infer-app-kind 'Vector (cdr type) kenv)]
   [(eq? (car type) 'Ref)
    (infer-app-kind 'Ref (cdr type) kenv)]
   [(eq? (car type) 'Block)
    (let ([tag-kind (if (symbol? (cadr type)) 'Symbol `(error expected-symbol ,(cadr type)))]
          [payload-kind (infer-kind (caddr type) kenv)])
         (if (and (eq? tag-kind 'Symbol) (kind=? payload-kind K*))
             K*
             `(error block-kind-error ,tag-kind ,payload-kind)))]
   [(eq? (car type) 'Cap)
    (let ([cap-kind (if (symbol? (cadr type)) 'Symbol `(error expected-symbol ,(cadr type)))]
          [inner-kind (infer-kind (caddr type) kenv)])
         (if (and (eq? cap-kind 'Symbol) (kind=? inner-kind K*))
             K*
             `(error cap-kind-error ,cap-kind ,inner-kind)))]
   
   [else `(error unknown-type-form ,type)]))

;;; infer-app-kind : Type × (List Type) × KindEnv → Kind | Error
;;; Infer the kind of a type application.
(define (infer-app-kind head args kenv)
  (let ([head-kind (infer-kind head kenv)])
       (if (and (pair? head-kind) (eq? (car head-kind) 'error))
           head-kind
           (apply-kinds head-kind args kenv))))

;;; apply-kinds : Kind × (List Type) × KindEnv → Kind | Error
;;; Apply a kind to type arguments.
(define (apply-kinds kind args kenv)
  (if (null? args)
      kind
      (if (kind-arrow? kind)
          (let* ([param-kind (kind-param kind)]
                 [result-kind (kind-result kind)]
                 [arg-kind (infer-kind (car args) kenv)])
                (if (kind=? param-kind arg-kind)
                    (apply-kinds result-kind (cdr args) kenv)
                    `(error kind-mismatch
                      (expected ,param-kind)
                      (got ,arg-kind)
                      (in ,(car args)))))
          `(error not-a-type-constructor ,kind ,args))))

;;; ============================================================
;;; Type Classes (Higher-Kinded)
;;; ============================================================

;;; A type class is a constraint on types or type constructors.
;;;
;;; Functor : (* → *) → Constraint
;;;   requires: fmap : ∀ a b. (a → b) → f a → f b
;;;
;;; Applicative : (* → *) → Constraint
;;;   requires: Functor f
;;;             pure : ∀ a. a → f a
;;;             <*>  : ∀ a b. f (a → b) → f a → f b
;;;
;;; Monad : (* → *) → Constraint
;;;   requires: Applicative f
;;;             >>= : ∀ a b. f a → (a → f b) → f b

;;; Type class definition structure
(define-record-type typeclass
  (fields
   name        ; Symbol
   kind        ; Kind (e.g., (* → *) → Constraint)
   supers      ; (List Symbol) — superclass constraints
   methods))   ; (List (name . type-scheme))

;;; make-typeclass : convenient constructor
;;; (Already defined by define-record-type)

;;; Built-in type class definitions
(define TC-Functor
  (make-typeclass
   'Functor
   (K=> (K=> K* K*) K-constraint)
   '()  ; no superclasses
   `((fmap . (∀ (f a b)
                (=> (Functor f)
                    (-> (-> a b) (@ f a) (@ f b))))))))

(define TC-Applicative
  (make-typeclass
   'Applicative
   (K=> (K=> K* K*) K-constraint)
   '(Functor)
   `((pure . (∀ (f a)
                (=> (Applicative f)
                    (-> a (@ f a)))))
     (<*>  . (∀ (f a b)
                (=> (Applicative f)
                    (-> (@ f (-> a b)) (@ f a) (@ f b))))))))

(define TC-Monad
  (make-typeclass
   'Monad
   (K=> (K=> K* K*) K-constraint)
   '(Applicative)
   `((>>= . (∀ (m a b)
               (=> (Monad m)
                   (-> (@ m a) (-> a (@ m b)) (@ m b)))))
     (return . (∀ (m a)
                  (=> (Monad m)
                      (-> a (@ m a))))))))

;;; ============================================================
;;; Value-Level Type Classes
;;; ============================================================

;;; Eq : * → Constraint
;;;   == : a → a → Bool
(define TC-Eq
  (make-typeclass
   'Eq
   (K=> K* K-constraint)
   '()
   `((== . (∀ (a)
              (=> (Eq a)
                  (-> a a Bool))))
     (/= . (∀ (a)
              (=> (Eq a)
                  (-> a a Bool)))))))

;;; Ord : * → Constraint
;;;   requires: Eq a
;;;   compare : a → a → Ordering
;;;   <, <=, >, >= : a → a → Bool
(define TC-Ord
  (make-typeclass
   'Ord
   (K=> K* K-constraint)
   '(Eq)
   `((compare . (∀ (a)
                   (=> (Ord a)
                       (-> a a Symbol))))  ; Returns 'LT, 'EQ, or 'GT
     (<  . (∀ (a) (=> (Ord a) (-> a a Bool))))
     (<= . (∀ (a) (=> (Ord a) (-> a a Bool))))
     (>  . (∀ (a) (=> (Ord a) (-> a a Bool))))
     (>= . (∀ (a) (=> (Ord a) (-> a a Bool)))))))

;;; Show : * → Constraint
;;;   show : a → String
(define TC-Show
  (make-typeclass
   'Show
   (K=> K* K-constraint)
   '()
   `((show . (∀ (a)
                (=> (Show a)
                    (-> a String)))))))

;;; Semigroup : * → Constraint
;;;   <> : a → a → a
(define TC-Semigroup
  (make-typeclass
   'Semigroup
   (K=> K* K-constraint)
   '()
   `((<> . (∀ (a)
              (=> (Semigroup a)
                  (-> a a a)))))))

;;; Monoid : * → Constraint
;;;   requires: Semigroup a
;;;   mempty : a
(define TC-Monoid
  (make-typeclass
   'Monoid
   (K=> K* K-constraint)
   '(Semigroup)
   `((mempty . (∀ (a)
                  (=> (Monoid a) a))))))

;;; ============================================================
;;; Contravariant and Bifunctors
;;; ============================================================

;;; Contravariant : (* → *) → Constraint
;;; The dual of Functor — consumes rather than produces values.
;;; contramap : (a → b) → f b → f a   (note the flip!)
(define TC-Contravariant
  (make-typeclass
   'Contravariant
   (K=> (K=> K* K*) K-constraint)
   '()
   `((contramap . (∀ (f a b)
                     (=> (Contravariant f)
                         (-> (-> a b) (@ f b) (@ f a))))))))

;;; Bifunctor : (* → * → *) → Constraint
;;; Functor in two arguments — can map over both.
;;; bimap : (a → b) → (c → d) → p a c → p b d
;;; first : (a → b) → p a c → p b c
;;; second : (c → d) → p a c → p a d
(define TC-Bifunctor
  (make-typeclass
   'Bifunctor
   (K=> (K=>* K* K* K*) K-constraint)  ; (* → * → *) → Constraint
   '()
   `((bimap  . (∀ (p a b c d)
                  (=> (Bifunctor p)
                      (-> (-> a b) (-> c d) (@ (@ p a) c) (@ (@ p b) d)))))
     (first  . (∀ (p a b c)
                  (=> (Bifunctor p)
                      (-> (-> a b) (@ (@ p a) c) (@ (@ p b) c)))))
     (second . (∀ (p a c d)
                  (=> (Bifunctor p)
                      (-> (-> c d) (@ (@ p a) c) (@ (@ p a) d))))))))

;;; ============================================================
;;; Category Theory Type Classes
;;; ============================================================

;;; Category : (* → * → *) → Constraint
;;; A category has objects (types) and morphisms between them.
;;; id : cat a a                         (identity morphism)
;;; . : cat b c → cat a b → cat a c      (composition)
(define TC-Category
  (make-typeclass
   'Category
   (K=> (K=>* K* K* K*) K-constraint)  ; (* → * → *) → Constraint
   '()
   `((id . (∀ (cat a)
              (=> (Category cat)
                  (@ (@ cat a) a))))
     (∘  . (∀ (cat a b c)
              (=> (Category cat)
                  (-> (@ (@ cat b) c) (@ (@ cat a) b) (@ (@ cat a) c))))))))

;;; Profunctor : (* → * → *) → Constraint
;;; Contravariant in first argument, covariant in second.
;;; dimap : (a → b) → (c → d) → p b c → p a d
(define TC-Profunctor
  (make-typeclass
   'Profunctor
   (K=> (K=>* K* K* K*) K-constraint)
   '()
   `((dimap . (∀ (p a b c d)
                 (=> (Profunctor p)
                     (-> (-> a b) (-> c d) (@ (@ p b) c) (@ (@ p a) d)))))
     (lmap  . (∀ (p a b c)
                 (=> (Profunctor p)
                     (-> (-> a b) (@ (@ p b) c) (@ (@ p a) c)))))
     (rmap  . (∀ (p a c d)
                 (=> (Profunctor p)
                     (-> (-> c d) (@ (@ p a) c) (@ (@ p a) d))))))))

;;; Arrow : (* → * → *) → Constraint
;;; Generalization of functions with structured computation.
(define TC-Arrow
  (make-typeclass
   'Arrow
   (K=> (K=>* K* K* K*) K-constraint)
   '(Category)
   `((arr    . (∀ (arr a b)
                  (=> (Arrow arr)
                      (-> (-> a b) (@ (@ arr a) b)))))
     (first* . (∀ (arr a b c)
                  (=> (Arrow arr)
                      (-> (@ (@ arr a) b) (@ (@ arr (× a c)) (× b c))))))
     (second* . (∀ (arr a b c)
                   (=> (Arrow arr)
                       (-> (@ (@ arr a) b) (@ (@ arr (× c a)) (× c b))))))
     (***    . (∀ (arr a b c d)
                  (=> (Arrow arr)
                      (-> (@ (@ arr a) b) (@ (@ arr c) d)
                          (@ (@ arr (× a c)) (× b d))))))
     (&&&   . (∀ (arr a b c)
                 (=> (Arrow arr)
                     (-> (@ (@ arr a) b) (@ (@ arr a) c)
                         (@ (@ arr a) (× b c)))))))))

;;; ============================================================
;;; Multi-Parameter Type Classes with Functional Dependencies
;;; ============================================================

;;; Multi-parameter type classes constrain relationships between
;;; multiple type parameters. Functional dependencies guide type
;;; inference by declaring which parameters determine others.
;;;
;;; Example:
;;;   class MonadReader r m | m -> r where
;;;     ask :: m r
;;;     local :: (r -> r) -> m a -> m a
;;;
;;; The fundep "m -> r" means: knowing m determines r uniquely.
;;; This enables the type checker to infer r from m.

;;; Multi-parameter type class with functional dependencies
(define-record-type mparam-typeclass
  (fields
   name        ; Symbol
   params      ; (List Symbol) — type parameters (e.g., (r m))
   param-kinds ; (List Kind) — kind of each parameter
   fundeps     ; (List (List Symbol × List Symbol)) — functional deps
   supers      ; (List Constraint)
   methods))   ; (List (name . type-scheme))

;;; make-mparam-typeclass is provided by define-record-type

;;; fundep : (List Symbol) × (List Symbol) → FunDep
;;; Create a functional dependency: lhs -> rhs
(define (fundep lhs rhs)
  (cons lhs rhs))

;;; fundep-lhs : FunDep → (List Symbol)
(define (fundep-lhs fd) (car fd))

;;; fundep-rhs : FunDep → (List Symbol)
(define (fundep-rhs fd) (cdr fd))

;;; ============================================================
;;; Multi-Parameter Type Class Examples
;;; ============================================================

;;; Convertible a b — convert between types
;;; No fundeps: a and b are independent
(define TC-Convertible
  (make-mparam-typeclass
   'Convertible
   '(a b)
   (list K* K*)
   '()  ; no functional dependencies
   '()
   `((convert . (∀ (a b)
                   (=> (Convertible a b)
                       (-> a b)))))))

;;; Collection c e | c -> e — collections with element type
;;; The fundep c -> e means the collection type determines element type
(define TC-Collection
  (make-mparam-typeclass
   'Collection
   '(c e)
   (list K* K*)
   (list (fundep '(c) '(e)))  ; c determines e
   '()
   `((empty   . (∀ (c e)
                   (=> (Collection c e) c)))
     (insert  . (∀ (c e)
                   (=> (Collection c e)
                       (-> e c c))))
     (member? . (∀ (c e)
                   (=> ((Collection c e) (Eq e))
                       (-> e c Bool)))))))

;;; MonadReader r m | m -> r — reader monad class
;;; The monad type determines the environment type
(define TC-MonadReader
  (make-mparam-typeclass
   'MonadReader
   '(r m)
   (list K* (K=> K* K*))  ; r : *, m : * → *
   (list (fundep '(m) '(r)))
   '(Monad)
   `((ask   . (∀ (r m)
                 (=> (MonadReader r m)
                     (@ m r))))
     (local . (∀ (r m a)
                 (=> (MonadReader r m)
                     (-> (-> r r) (@ m a) (@ m a))))))))

;;; MonadState s m | m -> s — state monad class
(define TC-MonadState
  (make-mparam-typeclass
   'MonadState
   '(s m)
   (list K* (K=> K* K*))
   (list (fundep '(m) '(s)))
   '(Monad)
   `((get    . (∀ (s m)
                  (=> (MonadState s m)
                      (@ m s))))
     (put    . (∀ (s m)
                  (=> (MonadState s m)
                      (-> s (@ m Unit)))))
     (modify . (∀ (s m)
                  (=> (MonadState s m)
                      (-> (-> s s) (@ m Unit))))))))

;;; MonadWriter w m | m -> w — writer monad class
(define TC-MonadWriter
  (make-mparam-typeclass
   'MonadWriter
   '(w m)
   (list K* (K=> K* K*))
   (list (fundep '(m) '(w)))
   '(Monad Monoid)  ; w must be a Monoid
   `((tell   . (∀ (w m)
                  (=> (MonadWriter w m)
                      (-> w (@ m Unit)))))
     (listen . (∀ (w m a)
                  (=> (MonadWriter w m)
                      (-> (@ m a) (@ m (× a w))))))
     (pass   . (∀ (w m a)
                  (=> (MonadWriter w m)
                      (-> (@ m (× a (-> w w))) (@ m a))))))))

;;; ============================================================
;;; Functional Dependency Utilities
;;; ============================================================

;;; apply-fundep : FunDep × TypeSubst → TypeSubst
;;; Given known type bindings, derive new bindings from fundep.
(define (apply-fundep fd subst)
  ;; If all LHS params are bound in subst, RHS params
  ;; can be determined (in actual resolution, looked up from instance)
  (let* ([lhs (fundep-lhs fd)]
         [rhs (fundep-rhs fd)]
         [lhs-bound? (andmap (lambda (p) (assq p subst)) lhs)])
        (if lhs-bound?
            ;; LHS determines RHS — mark RHS as determinable
            (map (lambda (p) (cons p 'determined)) rhs)
            '())))

;;; fundeps-closure : (List FunDep) × (Set Symbol) → (Set Symbol)
;;; Compute the closure of known parameters under functional deps.
(define (fundeps-closure fundeps known)
  ;; Add new elements to set without duplicates
  (define (add-new acc new-elems)
    (fold-left (lambda (a e)
                       (if (memq e a) a (cons e a)))
               acc
               new-elems))
  (let loop ([known known])
       (let ([new-known
              (fold-left
               (lambda (acc fd)
                       (if (andmap (lambda (p) (memq p acc)) (fundep-lhs fd))
                           (add-new acc (fundep-rhs fd))
                           acc))
               known
               fundeps)])
            (if (= (length new-known) (length known))
                known
                (loop new-known)))))

;;; check-fundep-consistency : (List FunDep) × Instance × Instance → Bool
;;; Check that two instances don't violate functional dependencies.
;;; If instances agree on LHS types, they must agree on RHS types.
(define (check-fundep-consistency fundeps inst1 inst2)
  ;; Placeholder — full implementation requires type unification
  #t)

;;; ============================================================
;;; Type Class Instances
;;; ============================================================

;;; An instance provides implementations of a type class for a specific type.
;;;
;;; instance Functor List where
;;;   fmap f xs = ...
;;;
;;; Represented as:
;;;   (instance Functor List ((fmap . <implementation>)))

(define-record-type instance
  (fields
   class       ; Symbol — which type class
   type        ; Type — for which type (e.g., List, (@ Either e))
   context     ; (List Constraint) — required constraints
   methods))   ; (List (name . expr))

;;; Note: Instance implementations are deferred.
;;; The infrastructure for instances exists (make-instance, instance-*)
;;; but actual instances will be implemented when the evaluator can
;;; handle type class dictionaries.
;;;
;;; See forum/engineering/0010-adr-001-type-classes-deferred.sexp

;;; ============================================================
;;; Constrained Types
;;; ============================================================

;;; We need a way to express constraints in types:
;;;   (=> (Functor f) (-> (-> a b) (@ f a) (@ f b)))
;;;
;;; The => form introduces type class constraints.

;;; t-constrained : (List Constraint) × Type → Type
(define (t=> constraints type)
  `(=> ,constraints ,type))

;;; constrained-type? : Type → Boolean
(define (constrained-type? t)
  (and (pair? t) (eq? (car t) '=>)))

;;; get-constraints : Type → (List Constraint)
(define (get-constraints t)
  (if (constrained-type? t)
      (cadr t)
      '()))

;;; get-underlying-type : Type → Type
(define (get-underlying-type t)
  (if (constrained-type? t)
      (caddr t)
      t))

;;; ============================================================
;;; Kind Unification (for HKT inference)
;;; ============================================================

;;; Kind unification finds a substitution that makes two kinds equal.
;;; This is needed when inferring HKT type variables.

;;; kind-subst-lookup : KindSubst × Symbol → Kind | #f
(define (kind-subst-lookup s var)
  (let ([entry (assq var s)])
       (if entry (cdr entry) #f)))

;;; kind-subst-extend : KindSubst × Symbol × Kind → KindSubst
(define (kind-subst-extend s var kind)
  (cons (cons var kind) s))

;;; apply-kind-subst : KindSubst × Kind → Kind
(define (apply-kind-subst s kind)
  (cond
   [(kind-var? kind)
    (let ([replacement (kind-subst-lookup s kind)])
         (if replacement
             (apply-kind-subst s replacement)
             kind))]
   [(not (pair? kind)) kind]
   [(eq? (car kind) '⇒)
    (K=> (apply-kind-subst s (cadr kind))
         (apply-kind-subst s (caddr kind)))]
   [(eq? (car kind) 'κ∀)
    ;; Remove bound vars from substitution
    (let* ([bound (cadr kind)]
           [body (caddr kind)]
           [s* (filter (lambda (p) (not (memq (car p) bound))) s)])
          `(κ∀ ,bound ,(apply-kind-subst s* body)))]
   [else kind]))

;;; kind-occurs? : Symbol × Kind → Boolean
(define (kind-occurs? var kind)
  (cond
   [(kind-var? kind) (eq? var kind)]
   [(not (pair? kind)) #f]
   [(eq? (car kind) '⇒)
    (or (kind-occurs? var (cadr kind))
        (kind-occurs? var (caddr kind)))]
   [(eq? (car kind) 'κ∀)
    (if (memq var (cadr kind))
        #f
        (kind-occurs? var (caddr kind)))]
   [else #f]))

;;; unify-kinds : Kind × Kind → (ok KindSubst) | (error ...)
;;; Unify two kinds, returning a substitution.
(define (unify-kinds k1 k2)
  (unify-kinds-with '() k1 k2))

(define (unify-kinds-with s k1 k2)
  (let ([k1 (apply-kind-subst s k1)]
        [k2 (apply-kind-subst s k2)])
       (cond
        ;; Same kind
        [(kind=? k1 k2) `(ok ,s)]
        
        ;; Kind variable on left
        [(kind-var? k1)
         (if (kind-occurs? k1 k2)
             `(error kind-occurs-check ,k1 ,k2)
             `(ok ,(kind-subst-extend s k1 k2)))]
        
        ;; Kind variable on right
        [(kind-var? k2)
         (if (kind-occurs? k2 k1)
             `(error kind-occurs-check ,k2 ,k1)
             `(ok ,(kind-subst-extend s k2 k1)))]
        
        ;; Both are kind arrows
        [(and (kind-arrow? k1) (kind-arrow? k2))
         (let ([result (unify-kinds-with s (kind-param k1) (kind-param k2))])
              (if (eq? (car result) 'ok)
                  (unify-kinds-with (cadr result) (kind-result k1) (kind-result k2))
                  result))]
        
        [else `(error kind-mismatch ,k1 ,k2)])))

;;; ============================================================
;;; Kind Display
;;; ============================================================

;;; kind->string : Kind → String
(define (kind->string k)
  (cond
   [(eq? k '*) "*"]
   [(eq? k 'Constraint) "Constraint"]
   [(eq? k 'Row) "Row"]
   ;; Sort (kind of kinds)
   [(eq? k '□) "[]"]
   [(and (pair? k) (eq? (car k) '□))
    (string-append "[]" (number->string (cadr k)))]
   ;; Kind variable
   [(kind-var? k) (symbol->string k)]
   ;; Kind arrow
   [(kind-arrow? k)
    (let ([param (kind->string (kind-param k))]
          [result (kind->string (kind-result k))])
         (if (kind-arrow? (kind-param k))
             (string-append "(" param ") ⇒ " result)
             (string-append param " ⇒ " result)))]
   ;; Dependent kind: Pi(x : K). K'
   [(dep-kind? k)
    (let ([var (symbol->string (dep-kind-var k))]
          [domain (kind->string (dep-kind-domain k))]
          [codomain (kind->string (dep-kind-codomain k))])
         (string-append "Pi(" var " : " domain "). " codomain))]
   ;; Arbitrary symbol (used in dependent kind domains like 'Nat)
   [(symbol? k) (symbol->string k)]
   [(pair? k) (format "~s" k)]
   [else "?"]))

;;; Note: Utilities (andmap, etc.) are provided by prelude.ss
