;;; core/types/gadt.ss — Generalized Algebraic Data Types
;;;
;;; GADTs extend inductive types with type indices that can be refined
;;; by pattern matching. The key innovation is that each constructor can
;;; specify a more precise return type, and matching on that constructor
;;; brings the type refinement into scope.
;;;
;;; Example:
;;;   (gadt (Expr a)
;;;     [Lit  : (-> Int (Expr Int))]
;;;     [Add  : (-> (Expr Int) (Expr Int) (Expr Int))]
;;;     [Eq   : (-> (Expr Int) (Expr Int) (Expr Bool))]
;;;     [If   : (∀ (b) (-> (Expr Bool) (Expr b) (Expr b) (Expr b)))])
;;;
;;; When pattern matching on a GADT, the type index becomes an equality:
;;;   (gadt-case (e : (Expr a))
;;;     ((Lit n) ...)      ; Here we know a ~ Int
;;;     ((Eq x y) ...)     ; Here we know a ~ Bool
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - dep-types.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")

;;; ====
;;; GADT Grammar
;;; ====
;;;
;;;   GADTDecl ::= (gadt TypeHead CtorDecl ...)
;;;
;;;   TypeHead ::= (Name IndexDecl ...)
;;;   IndexDecl ::= Symbol                     ; Implicit kind Type
;;;               | (Symbol : Kind)            ; Explicit kind
;;;
;;;   CtorDecl ::= [CtorName : CtorType]
;;;   CtorType ::= Type                        ; Must return applied GADT type
;;;
;;; The return type of each constructor must be of the form (Name idx ...),
;;; where the indices can be specific types (not just variables), enabling
;;; type refinement.

;;; ====
;;; GADT Type Predicates
;;; ====

;;; gadt-type? : SExpr → Boolean
;;; Check if this is a GADT declaration.
(define (gadt-type? t)
  (and (pair? t) (eq? (car t) 'gadt)))

;;; gadt-applied? : SExpr → Boolean
;;; Check if this is an applied GADT type: (Name idx ...)
;;; Returns #t if the head is a known GADT name.
(define (gadt-applied? t registry)
  (and (pair? t)
       (symbol? (car t))
       (assq (car t) registry)))

;;; gadt-ctor-decl? : SExpr → Boolean
;;; Check if this is a valid constructor declaration: [Name : Type]
(define (gadt-ctor-decl? d)
  (and (list? d)
       (= (length d) 3)
       (symbol? (car d))
       (eq? (cadr d) ':)))

;;; gadt-well-formed? : SExpr → Boolean
;;; Check if a GADT declaration is well-formed.
(define (gadt-well-formed? t)
  (and (gadt-type? t)
       (>= (length t) 2)
       (pair? (cadr t))           ; Type head must be a list
       (symbol? (caadr t))        ; Type name
       (andmap gadt-index-decl? (cdadr t))  ; Index declarations
       (andmap gadt-ctor-decl? (cddr t))))  ; Constructor declarations

;;; gadt-index-decl? : SExpr → Boolean
;;; Check if this is a valid index declaration.
(define (gadt-index-decl? d)
  (or (symbol? d)                 ; Implicit kind Type
      (and (list? d)              ; Explicit kind
           (= (length d) 3)
           (symbol? (car d))
           (eq? (cadr d) ':))))

;;; ====
;;; GADT Accessors
;;; ====

;;; gadt-name : GADTDecl → Symbol
;;; Extract the type name from a GADT declaration.
(define (gadt-name t)
  (if (gadt-type? t)
      (caadr t)
      #f))

;;; gadt-index-vars : GADTDecl → (List Symbol)
;;; Extract index variable names from a GADT declaration.
(define (gadt-index-vars t)
  (if (gadt-type? t)
      (map (lambda (d)
                   (if (symbol? d)
                       d
                       (car d)))
           (cdadr t))
      '()))

;;; gadt-index-kinds : GADTDecl → (List (Symbol . Kind))
;;; Extract index variables with their kinds.
(define (gadt-index-kinds t)
  (if (gadt-type? t)
      (map (lambda (d)
                   (if (symbol? d)
                       (cons d 'Type)
                       (cons (car d) (caddr d))))
           (cdadr t))
      '()))

;;; gadt-constructors : GADTDecl → (List (Symbol . Type))
;;; Extract constructors as (name . type) pairs.
(define (gadt-constructors t)
  (if (gadt-type? t)
      (map (lambda (d) (cons (car d) (caddr d)))
           (cddr t))
      '()))

;;; gadt-ctor-names : GADTDecl → (List Symbol)
;;; Extract just the constructor names.
(define (gadt-ctor-names t)
  (map car (gadt-constructors t)))

;;; gadt-ctor-type : GADTDecl × Symbol → Type | #f
;;; Look up a constructor's type by name.
(define (gadt-ctor-type t ctor-name)
  (let ([ctors (gadt-constructors t)])
       (let ([entry (assq ctor-name ctors)])
            (if entry (cdr entry) #f))))

;;; ====
;;; GADT Type Construction
;;; ====

;;; t-gadt : Symbol × (List (Symbol . Kind)) × (List (Symbol . Type)) → GADTDecl
;;; Construct a GADT declaration from its parts.
(define (t-gadt name index-kinds ctor-types)
  `(gadt (,name ,@(map (lambda (ik)
                               (if (eq? (cdr ik) 'Type)
                                   (car ik)
                                   `(,(car ik) : ,(cdr ik))))
                       index-kinds))
    ,@(map (lambda (ct) `(,(car ct) : ,(cdr ct)))
           ctor-types)))

;;; gadt-make-applied : Symbol × (List Type) → Type
;;; Construct an applied GADT type.
(define (gadt-make-applied name indices)
  (if (null? indices)
      name
      `(,name ,@indices)))

;;; ====
;;; Constructor Type Analysis
;;; ====

;;; gadt-ctor-return-type : Type → Type
;;; Extract the return type from a constructor type.
;;; For (-> A B ... R), returns R.
;;; For (∀ (vars) T), recurses into T.
;;; For (Π ((x : A)) B), recurses into B.
(define (gadt-ctor-return-type t)
  (cond
   [(function-type? t)
    (function-return-type t)]
   [(and (pair? t) (eq? (car t) '∀))
    (gadt-ctor-return-type (caddr t))]
   [(pi-type? t)
    (gadt-ctor-return-type (pi-codomain t))]
   [else t]))

;;; gadt-ctor-param-types : Type → (List Type)
;;; Extract the parameter types from a constructor type.
;;; For (-> A B ... R), returns (A B ...) without R.
;;; For (∀ (vars) T), recurses into T.
;;; For (Π ((x : A)) B), returns A followed by params of B.
(define (gadt-ctor-param-types t)
  (cond
   [(function-type? t)
    (function-param-types t)]
   [(and (pair? t) (eq? (car t) '∀))
    (gadt-ctor-param-types (caddr t))]
   [(pi-type? t)
    (cons (pi-domain t) (gadt-ctor-param-types (pi-codomain t)))]
   [else '()]))

;;; gadt-ctor-return-indices : Type × Symbol → (List Type)
;;; Extract the type indices from a constructor's return type.
;;; Given a constructor type and the GADT name, extracts the index values.
;;; For return type (Expr Int), returns (Int).
;;; For return type (Vec 0 a), returns (0 a).
(define (gadt-ctor-return-indices ctor-type gadt-name)
  (let ([ret-type (gadt-ctor-return-type ctor-type)])
       (if (and (pair? ret-type)
                (eq? (car ret-type) gadt-name))
           (cdr ret-type)
           (if (eq? ret-type gadt-name)
               '()
               (error 'gadt-ctor-return-indices
                      "constructor doesn't return GADT type" ctor-type gadt-name)))))

;;; gadt-ctor-forall-vars : Type → (List Symbol)
;;; Extract universally quantified variables from a constructor type.
(define (gadt-ctor-forall-vars t)
  (cond
   [(and (pair? t) (eq? (car t) '∀))
    (append (forall-vars t) (gadt-ctor-forall-vars (caddr t)))]
   [(pi-type? t)
    (gadt-ctor-forall-vars (pi-codomain t))]
   [else '()]))

;;; ====
;;; Type Refinement (The Core GADT Innovation)
;;; ====

;;; When we match on a GADT constructor, we learn type equalities.
;;; If we're matching a value of type (Expr a) against constructor Lit
;;; which returns (Expr Int), we learn that a = Int.

;;; extract-type-equations : Type × Type × (List Symbol) → (List (Symbol . Type)) | 'mismatch
;;; Given the scrutinee's indices and constructor's return indices,
;;; compute the type variable assignments that would unify them.
;;;
;;; Example:
;;;   scrutinee type: (Expr a)    → indices: (a)
;;;   constructor returns: (Expr Int) → indices: (Int)
;;;   result: ((a . Int))
;;;
;;; Type variables are lowercase symbols; type constructors are uppercase.
;;; Returns 'mismatch if concrete types don't match (unreachable branch).
(define (extract-type-equations scrutinee-indices ctor-indices index-vars)
  (let loop ([scrut scrutinee-indices]
             [ctor ctor-indices]
             [eqns '()])
       (cond
        [(or (null? scrut) (null? ctor))
         (reverse eqns)]
        [else
         (let ([s (car scrut)]
               [c (car ctor)])
              (cond
               ;; If scrutinee index is a type variable (lowercase symbol), refine it
               [(and (symbol? s) (type-variable? s))
                (loop (cdr scrut) (cdr ctor) (cons (cons s c) eqns))]
               ;; If constructor index is a type variable, unify it with scrutinee index
               [(and (symbol? c) (type-variable? c))
                (loop (cdr scrut) (cdr ctor) (cons (cons c s) eqns))]
               ;; If they're structurally equal, no equation needed
               [(equal? s c)
                (loop (cdr scrut) (cdr ctor) eqns)]
               ;; Structural mismatch - this branch is unreachable
               [else 'mismatch]))])))

;;; type-variable? : Symbol → Boolean
;;; Type variables are lowercase; type constructors are uppercase.
(define (type-variable? sym)
  (let ([s (symbol->string sym)])
       (and (> (string-length s) 0)
            (char-lower-case? (string-ref s 0)))))

;;; apply-refinements : Type × (List (Symbol . Type)) → Type
;;; Apply type refinements to a type via substitution.
(define (apply-refinements type refinements)
  (fold-left (lambda (t ref)
                     (subst-type t (car ref) (cdr ref)))
             type
             refinements))

;;; ====
;;; GADT Pattern Matching Support
;;; ====

;;; gadt-case-expr? : SExpr → Boolean
;;; Check if this is a GADT case expression.
(define (gadt-case-expr? e)
  (and (pair? e) (eq? (car e) 'gadt-case)))

;;; gadt-case-scrutinee : Expr → Expr
;;; Extract the scrutinee from a gadt-case expression.
(define (gadt-case-scrutinee e)
  (if (gadt-case-expr? e)
      (cadr e)
      #f))

;;; gadt-case-clauses : Expr → (List Clause)
;;; Extract the clauses from a gadt-case expression.
(define (gadt-case-clauses e)
  (if (gadt-case-expr? e)
      (cddr e)
      '()))

;;; gadt-clause-pattern : Clause → Pattern
;;; Extract the pattern from a clause.
(define (gadt-clause-pattern clause)
  (car clause))

;;; gadt-clause-body : Clause → Expr
;;; Extract the body from a clause.
(define (gadt-clause-body clause)
  (cadr clause))

;;; gadt-pattern-ctor : Pattern → Symbol
;;; Extract the constructor name from a pattern.
(define (gadt-pattern-ctor pattern)
  (car pattern))

;;; gadt-pattern-vars : Pattern → (List Symbol)
;;; Extract the bound variables from a pattern.
(define (gadt-pattern-vars pattern)
  (cdr pattern))

;;; ====
;;; GADT Registry
;;; ====

;;; A registry maps GADT names to their declarations.
;;; This is used during type checking to look up constructor types.

;;; make-gadt-registry : (List GADTDecl) → Registry
(define (make-gadt-registry decls)
  (map (lambda (d) (cons (gadt-name d) d)) decls))

;;; gadt-registry-lookup : Registry × Symbol → GADTDecl | #f
(define (gadt-registry-lookup registry name)
  (let ([entry (assq name registry)])
       (if entry (cdr entry) #f)))

;;; gadt-registry-add : Registry × GADTDecl → Registry
(define (gadt-registry-add registry decl)
  (cons (cons (gadt-name decl) decl) registry))

;;; ====
;;; GADT Type Wellformedness
;;; ====

;;; gadt-ctor-returns-gadt? : Type × Symbol → Boolean
;;; Check that a constructor type returns the GADT type.
(define (gadt-ctor-returns-gadt? ctor-type gadt-name)
  (let ([ret (gadt-ctor-return-type ctor-type)])
       (or (eq? ret gadt-name)
           (and (pair? ret)
                (eq? (car ret) gadt-name)))))

;;; gadt-all-ctors-return-gadt? : GADTDecl → Boolean
;;; Check that all constructors return the GADT type.
(define (gadt-all-ctors-return-gadt? decl)
  (let ([name (gadt-name decl)]
        [ctors (gadt-constructors decl)])
       (andmap (lambda (ct)
                       (gadt-ctor-returns-gadt? (cdr ct) name))
               ctors)))

;;; ====
;;; GADT Type Inference
;;; ====
;;;
;;; The following inference functions are used by dep-infer.ss.
;;; They depend on dep-synth, dep-check, etc. being defined when called.

;;; ====
;;; GADT Registry (Global State)
;;; ====

(define *gadt-registry* '())

;;; reset-gadt-registry! : -> Unit
(define (reset-gadt-registry!)
  (set! *gadt-registry* '()))

;;; register-gadt! : GADTDecl -> Unit
(define (register-gadt! decl)
  (set! *gadt-registry* (gadt-registry-add *gadt-registry* decl)))

;;; lookup-gadt : Symbol -> GADTDecl | #f
(define (lookup-gadt name)
  (gadt-registry-lookup *gadt-registry* name))

;;; ====
;;; GADT Type Constructor Kind
;;; ====

;;; gadt-type-constructor-kind : (List (Symbol . Kind)) -> Kind
;;; Build the kind of a GADT type constructor from its index kinds.
;;; E.g., for (gadt (Maybe a) ...), returns (-> Type Type)
;;; For (gadt (Vec (n : Nat) a) ...), returns (-> Nat Type Type)
;;; For no indices, returns Type.
(define (gadt-type-constructor-kind index-kinds)
  (if (null? index-kinds)
      'Type
      (let ([param-kinds (map cdr index-kinds)])
           (fold-right (lambda (k acc) `(-> ,k ,acc))
                       'Type
                       param-kinds))))

;;; ====
;;; GADT Declaration Synthesis
;;; ====

;;; gadt-infer-synth : GADTDecl x Context x (Expr x Ctx -> Result) x (Expr x Type x Ctx -> Result) x (Type x Ctx -> Result)
;;;                    -> (Result Type Error)
;;; Type-check a GADT declaration and register it.
;;; Takes dep-synth, dep-check, dep-check-type as parameters for dependency injection.
(define (gadt-infer-synth decl ctx dep-synth dep-check dep-check-type)
  (if (not (gadt-well-formed? decl))
      `(error malformed-gadt-declaration ,decl)
      (let* ([name (gadt-name decl)]
             [index-kinds (gadt-index-kinds decl)]
             [ctors (gadt-constructors decl)]
             ;; Build the GADT type constructor's kind (e.g., Type -> Type for Maybe a)
             ;; and add it to context BEFORE checking constructor types
             [gadt-kind (gadt-type-constructor-kind index-kinds)]
             ;; Add type parameters (e.g., 'a : Type) to context
             [ctx-with-params (fold-left (lambda (c ik)
                                                 (cons (list (car ik) (cdr ik)) c))
                                         ctx
                                         index-kinds)]
             ;; Add the GADT type constructor itself
             [ctx-with-gadt (cons (list name gadt-kind) ctx-with-params)])
            ;; Check all constructor types in the extended context
            (let ([ctor-checks (gadt-infer-check-ctors name index-kinds ctors ctx-with-gadt dep-check-type)])
                 (if (not (eq? (car ctor-checks) 'ok))
                     ctor-checks
                     (begin
                      (register-gadt! decl)
                      `(ok (gadt-type ,name))))))))

;;; gadt-infer-check-ctors : Symbol x (List (Symbol . Kind)) x (List (Symbol . Type)) x Context x (Type x Ctx -> Result)
;;;                          -> (ok) | (error ...)
(define (gadt-infer-check-ctors gadt-name index-kinds ctors ctx dep-check-type)
  (if (null? ctors)
      '(ok)
      (let* ([ctor (car ctors)]
             [ctor-name (car ctor)]
             [ctor-type (cdr ctor)]
             [type-check (dep-check-type ctor-type ctx)])
            (if (not (eq? (car type-check) 'ok))
                `(error ctor-type-invalid ,ctor-name ,type-check)
                (if (not (gadt-ctor-returns-gadt? ctor-type gadt-name))
                    `(error ctor-wrong-return-type ,ctor-name ,ctor-type ,gadt-name)
                    (gadt-infer-check-ctors gadt-name index-kinds (cdr ctors) ctx dep-check-type))))))

;;; ====
;;; GADT Case Synthesis
;;; ====

;;; gadt-infer-synth-case : Expr x Context x (Expr x Ctx -> Result) x (Expr x Type x Ctx -> Result) x (Type x Type x Ctx -> Bool)
;;;                          x (Ctx x Symbol x Type -> Ctx) x (Ctx x Symbol x Type x Val -> Ctx)
;;;                          -> (Result Type Error)
;;; Type-check a gadt-case expression with type refinement.
(define (gadt-infer-synth-case expr ctx dep-synth dep-check dep-types-equal?
                               dep-ctx-extend dep-ctx-extend-def)
  (let* ([scrutinee (gadt-case-scrutinee expr)]
         [clauses (gadt-case-clauses expr)]
         [scrut-synth (dep-synth scrutinee ctx)])
        (if (not (eq? (car scrut-synth) 'ok))
            scrut-synth
            (let ([scrut-type (cadr scrut-synth)])
                 (let ([gadt-info (gadt-infer-decompose-type scrut-type)])
                      (if (not gadt-info)
                          `(error scrutinee-not-gadt-type ,scrut-type)
                          (let* ([gadt-name (car gadt-info)]
                                 [scrut-indices (cdr gadt-info)]
                                 [gadt-decl (lookup-gadt gadt-name)])
                                (if (not gadt-decl)
                                    `(error unknown-gadt ,gadt-name)
                                    (gadt-infer-synth-clauses gadt-decl scrut-indices clauses ctx
                                                              dep-synth dep-types-equal?
                                                              dep-ctx-extend dep-ctx-extend-def)))))))))

;;; gadt-infer-decompose-type : Type -> (Symbol . (List Type)) | #f
(define (gadt-infer-decompose-type t)
  (cond
   [(symbol? t)
    (if (lookup-gadt t)
        (cons t '())
        #f)]
   [(and (pair? t) (symbol? (car t)))
    (if (lookup-gadt (car t))
        (cons (car t) (cdr t))
        #f)]
   [else #f]))

;;; gadt-infer-synth-clauses : GADTDecl x (List Type) x (List Clause) x Context
;;;                            x (Expr x Ctx -> Result) x (Type x Type x Ctx -> Bool)
;;;                            x (Ctx x Symbol x Type -> Ctx) x (Ctx x Symbol x Type x Val -> Ctx)
;;;                            -> (Result Type Error)
(define (gadt-infer-synth-clauses gadt-decl scrut-indices clauses ctx
                                  dep-synth dep-types-equal?
                                  dep-ctx-extend dep-ctx-extend-def)
  (if (null? clauses)
      `(error empty-gadt-case)
      (let loop ([clauses clauses] [types '()])
           (if (null? clauses)
               ;; All clauses checked, unify return types
               (gadt-infer-unify-return-types (reverse types) ctx dep-types-equal?)
               (let ([result (gadt-infer-synth-clause gadt-decl scrut-indices (car clauses) ctx
                                                      dep-synth dep-ctx-extend dep-ctx-extend-def)])
                    (if (not (eq? (car result) 'ok))
                        result
                        (loop (cdr clauses) (cons (cadr result) types))))))))

;;; gadt-infer-synth-clause : GADTDecl x (List Type) x Clause x Context
;;;                           x (Expr x Ctx -> Result) x (Ctx x Symbol x Type -> Ctx)
;;;                           x (Ctx x Symbol x Type x Val -> Ctx)
;;;                           -> (Result Type Error)
(define (gadt-infer-synth-clause gadt-decl scrut-indices clause ctx
                                 dep-synth dep-ctx-extend dep-ctx-extend-def)
  (let* ([pattern (gadt-clause-pattern clause)]
         [body (gadt-clause-body clause)]
         [ctor-name (gadt-pattern-ctor pattern)]
         [pattern-vars (gadt-pattern-vars pattern)]
         [gadt-name (gadt-name gadt-decl)]
         [index-vars (gadt-index-vars gadt-decl)]
         [ctor-type (gadt-ctor-type gadt-decl ctor-name)])
        (if (not ctor-type)
            `(error unknown-constructor ,ctor-name ,gadt-name)
            (let* ([ctor-return-indices (gadt-ctor-return-indices ctor-type gadt-name)]
                   [refinements (extract-type-equations scrut-indices ctor-return-indices index-vars)])
                  ;; Check for structural mismatch (unreachable branch)
                  (if (eq? refinements 'mismatch)
                      `(error unreachable-gadt-branch
                        (constructor ,ctor-name)
                        (scrutinee-indices ,scrut-indices)
                        (ctor-indices ,ctor-return-indices))
                      (let* ([raw-param-types (gadt-ctor-param-types ctor-type)]
                             [param-types (map (lambda (t) (apply-refinements t refinements)) raw-param-types)]
                             [expected-arity (length param-types)]
                             [actual-arity (length pattern-vars)])
                            (if (not (= expected-arity actual-arity))
                                `(error pattern-arity-mismatch
                                  (constructor ,ctor-name)
                                  (expected ,expected-arity)
                                  (got ,actual-arity))
                                ;; Build refined context
                                (let* ([ctx-with-refs (gadt-infer-apply-refinements ctx refinements dep-ctx-extend-def)]
                                       [ctx-with-vars (gadt-infer-bind-pattern-vars ctx-with-refs pattern-vars param-types dep-ctx-extend)])
                                      (dep-synth body ctx-with-vars)))))))))

;;; gadt-infer-apply-refinements : Context x (List (Symbol . Type)) x (Ctx x Symbol x Type x Val -> Ctx) -> Context
(define (gadt-infer-apply-refinements ctx refinements dep-ctx-extend-def)
  (fold-left (lambda (c ref)
                     (dep-ctx-extend-def c (car ref) 'Type (cdr ref)))
             ctx
             refinements))

;;; gadt-infer-bind-pattern-vars : Context x (List Symbol) x (List Type) x (Ctx x Symbol x Type -> Ctx) -> Context
(define (gadt-infer-bind-pattern-vars ctx vars types dep-ctx-extend)
  (fold-left (lambda (c pair)
                     (dep-ctx-extend c (car pair) (cdr pair)))
             ctx
             (map cons vars types)))

;;; gadt-infer-unify-return-types : (List Type) x Context x (Type x Type x Ctx -> Bool) -> (Result Type Error)
(define (gadt-infer-unify-return-types types ctx dep-types-equal?)
  (if (null? types)
      `(error empty-gadt-case)
      (let ([first-type (car types)])
           (let loop ([remaining (cdr types)])
                (if (null? remaining)
                    `(ok ,first-type)
                    (if (dep-types-equal? first-type (car remaining) ctx)
                        (loop (cdr remaining))
                        `(error gadt-case-branch-type-mismatch
                          (first ,first-type)
                          (other ,(car remaining)))))))))

;;; ====
;;; Test GADT Setup Helper
;;; ====

;;; gadt-define-test-gadts! : -> Unit
;;; Helper to set up test GADTs.
(define (gadt-define-test-gadts!)
  (reset-gadt-registry!)
  (register-gadt!
   '(gadt (Expr a)
     [Lit  : (-> Int (Expr Int))]
     [Add  : (-> (Expr Int) (Expr Int) (Expr Int))]
     [Eq   : (-> (Expr Int) (Expr Int) (Expr Bool))]
     [If   : (forall (b) (-> (Expr Bool) (Expr b) (Expr b) (Expr b)))]))
  (register-gadt!
   '(gadt (Vec (n : Nat) a)
     [VNil  : (Vec 0 a)]
     [VCons : (forall (m) (-> a (Vec m a) (Vec (succ m) a)))])))
