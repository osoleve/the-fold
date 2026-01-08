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

;;; ============================================================
;;; GADT Grammar
;;; ============================================================
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

;;; ============================================================
;;; GADT Type Predicates
;;; ============================================================

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

;;; ============================================================
;;; GADT Accessors
;;; ============================================================

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

;;; ============================================================
;;; GADT Type Construction
;;; ============================================================

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

;;; ============================================================
;;; Constructor Type Analysis
;;; ============================================================

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

;;; ============================================================
;;; Type Refinement (The Core GADT Innovation)
;;; ============================================================

;;; When we match on a GADT constructor, we learn type equalities.
;;; If we're matching a value of type (Expr a) against constructor Lit
;;; which returns (Expr Int), we learn that a = Int.

;;; extract-type-equations : Type × Type × (List Symbol) → (List (Symbol . Type))
;;; Given the scrutinee's indices and constructor's return indices,
;;; compute the type variable assignments that would unify them.
;;;
;;; Example:
;;;   scrutinee type: (Expr a)    → indices: (a)
;;;   constructor returns: (Expr Int) → indices: (Int)
;;;   result: ((a . Int))
;;;
;;; The 'index-vars' parameter lists which symbols are refineable.
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
               ;; If scrutinee index is a refineable variable, record equation
               [(and (symbol? s) (memq s index-vars))
                (loop (cdr scrut) (cdr ctor) (cons (cons s c) eqns))]
               ;; If they're structurally equal, no equation needed
               [(equal? s c)
                (loop (cdr scrut) (cdr ctor) eqns)]
               ;; Otherwise, no equation (might be a type error elsewhere)
               [else
                (loop (cdr scrut) (cdr ctor) eqns)]))])))

;;; apply-refinements : Type × (List (Symbol . Type)) → Type
;;; Apply type refinements to a type via substitution.
(define (apply-refinements type refinements)
  (fold-left (lambda (t ref)
                     (subst-type t (car ref) (cdr ref)))
             type
             refinements))

;;; ============================================================
;;; GADT Pattern Matching Support
;;; ============================================================

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

;;; ============================================================
;;; GADT Registry
;;; ============================================================

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

;;; ============================================================
;;; GADT Type Wellformedness
;;; ============================================================

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
