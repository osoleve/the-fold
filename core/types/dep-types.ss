;;; core/types/dep-types.ss — Dependent Type Extensions
;;;
;;; Extends the base type system with dependent types:
;;;   - Pi types (Π) — Dependent function types
;;;   - Sigma types (Σ) — Dependent pair types
;;;   - Universe hierarchy (Type₀, Type₁, ...)
;;;
;;; This module provides the syntax and predicates for dependent types.
;;; Normalization and conversion checking is in nbe.ss.
;;; Type checking rules are in dep-infer.ss.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")

;;; ====
;;; Extended Type Grammar
;;; ====
;;;
;;; In addition to types.ss grammar:
;;;
;;;   Type ::= ...
;;;          | (Π ((x : Type)) Type)    ; Pi type (dependent function)
;;;          | (Σ ((x : Type)) Type)    ; Sigma type (dependent pair)
;;;          | Type                      ; Universe (synonym for Type₀)
;;;          | (Type n)                  ; Universe at level n
;;;          | (data (Name params...) [ctor : type] ...)  ; Inductive type
;;;
;;; The arrow (-> A B) is sugar for (Π ((_ : A)) B) when _ is unused.
;;; The product (× A B) is sugar for (Σ ((_ : A)) B) when _ is unused.
;;;
;;; Inductive Types:
;;;
;;;   (data (Name params...) [ctor : type] ...)
;;;
;;; Where:
;;;   - Name is the type name (symbol)
;;;   - params are type parameters: bare symbols (implicit Type) or (x : T)
;;;   - ctor is a constructor name
;;;   - type is the constructor's type (must return the inductive type)
;;;
;;; Example:
;;;   (data (Vec A)
;;;     [nil  : (Vec A)]
;;;     [cons : (Π ((x : A)) (Π ((xs : (Vec A))) (Vec A)))])
;;;
;;;   (data (Nat)
;;;     [zero : Nat]
;;;     [succ : (Π ((n : Nat)) Nat)])
;;;
;;; Indexed Families:
;;;   (data (Vec (n : Nat) A)
;;;     [nil  : (Vec 0 A)]
;;;     [cons : (Π ((x : A)) (Π ((m : Nat)) (Π ((xs : (Vec m A))) (Vec (succ m) A))))])
;;;
;;; Module Signatures (dependent types integration):
;;;
;;;   (sig Name
;;;     [type T : Kind]           ; Abstract type declaration
;;;     [type T = Type]           ; Type alias (transparent)
;;;     [val name : Type]         ; Value export
;;;     [data DataDecl]           ; Inductive type export
;;;     [include SigName])        ; Signature inclusion
;;;
;;; Example:
;;;   (sig Stack
;;;     [type T : Type]                              ; Element type (abstract)
;;;     [type Stack : (-> Type Type)]                ; Stack type constructor
;;;     [val empty : (Π ((A : Type)) (Stack A))]     ; Polymorphic empty
;;;     [val push : (Π ((A : Type)) (Π ((x : A)) (Π ((s : (Stack A))) (Stack A))))]
;;;     [val pop : (Π ((A : Type)) (Π ((s : (Stack A))) (Stack A)))])
;;;
;;; Quick API Reference (constructors defined below):
;;;
;;;   (t-pi 'n 'Nat '(Vec Int n))         → (Π ((n : Nat)) (Vec Int n))
;;;   (t-sigma 'x 'Int '(> x 0))          → (Σ ((x : Int)) (> x 0))
;;;   (t-type 1)                          → (Type 1)
;;;   (t-eq 'Nat 'x 'y)                   → (= Nat x y)
;;;   (t-refine 'n 'Nat '(> n 0))         → (refine ((n : Nat)) (> n 0))
;;;
;;; Predicates: pi-type?, sigma-type?, universe-type?, equality-type?,
;;;             refinement-type?, data-type?, sig-type?, dep-type?

;;; ====
;;; Dependent Type Predicates
;;; ====

;;; pi-type? : Type → Boolean
(define (pi-type? t)
  (and (pair? t) (eq? (car t) 'Π)))

;;; sigma-type? : Type → Boolean
(define (sigma-type? t)
  (and (pair? t) (eq? (car t) 'Σ)))

;;; vec-type? : Type → Boolean
(define (vec-type? t)
  (and (pair? t) (eq? (car t) 'Vec)))

;;; matrix-type? : Type → Boolean
(define (matrix-type? t)
  (and (pair? t) (eq? (car t) 'Matrix)))

;;; universe-type? : Type → Boolean
(define (universe-type? t)
  (or (eq? t 'Type)
      (and (pair? t) (eq? (car t) 'Type))))

;;; equality-type? : Type → Boolean
;;; Check if this is a propositional equality type: (= A x y)
(define (equality-type? t)
  (and (pair? t) (eq? (car t) '=)))

;;; refinement-type? : Type → Boolean
;;; Check if this is a refinement type: {x : T | P}
;;; Syntax: (refine ((x : T)) P)
(define (refinement-type? t)
  (and (pair? t) (eq? (car t) 'refine)))

;;; data-type? : SExpr → Boolean
;;; Check if this is an inductive type definition: (data (Name ...) [ctor : type] ...)
(define (data-type? t)
  (and (pair? t) (eq? (car t) 'data)))

;;; dep-type? : Type → Boolean
;;; Is this a dependent type construct?
(define (dep-type? t)
  (or (pi-type? t) (sigma-type? t) (universe-type? t)
      (vec-type? t) (matrix-type? t) (equality-type? t) (heq-type? t)
      (refinement-type? t) (data-type? t) (sig-type? t)
      (diff-type? t) (grad-type? t)))

;;; ====
;;; Pi Type Operations
;;; ====

;;; pi-type-well-formed? : SExpr → Boolean
;;; Check if a Pi type expression is well-formed.
;;; Form: (Π ((x : A)) B) or (Π ((x : A) (y : B)) C) for multiple params
(define (pi-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Π)
       (= (length t) 3)
       (let ([bindings (cadr t)]
             [body (caddr t)])
            (and (list? bindings)
                 (not (null? bindings))
                 (andmap typed-binding? bindings)))))

;;; pi-var : Type → Symbol
;;; Get the first bound variable of a Pi type.
(define (pi-var t)
  (if (pi-type? t)
      (caar (cadr t))
      #f))

;;; pi-domain : Type → Type
;;; Get the domain type of a Pi type.
(define (pi-domain t)
  (if (pi-type? t)
      (caddar (cadr t))  ; Third element of first binding
      'Void))

;;; pi-codomain : Type → Type
;;; Get the codomain type of a Pi type (may reference the bound var).
(define (pi-codomain t)
  (if (pi-type? t)
      (caddr t)
      'Void))

;;; pi-bindings : Type → (List (Symbol . Type))
;;; Extract all bindings from a Pi type as alist.
(define (pi-bindings t)
  (if (pi-type? t)
      (map (lambda (b) (cons (car b) (caddr b)))
           (cadr t))
      '()))

;;; ====
;;; Sigma Type Operations
;;; ====

;;; sigma-type-well-formed? : SExpr → Boolean
(define (sigma-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Σ)
       (= (length t) 3)
       (let ([binding (cadr t)]
             [body (caddr t)])
            (and (list? binding)
                 (= (length binding) 1)
                 (typed-binding? (car binding))))))

;;; sigma-var : Type → Symbol
(define (sigma-var t)
  (if (sigma-type? t)
      (caar (cadr t))
      #f))

;;; sigma-fst-type : Type → Type
(define (sigma-fst-type t)
  (if (sigma-type? t)
      (caddar (cadr t))
      'Void))

;;; sigma-snd-type : Type → Type
(define (sigma-snd-type t)
  (if (sigma-type? t)
      (caddr t)
      'Void))

;;; ====
;;; Equality Type Operations
;;; ====
;;;
;;; Propositional equality: (= A x y)
;;;   - A is the type at which equality is asserted
;;;   - x and y are terms of type A being compared
;;;
;;; Introduction:
;;;   refl : (Π ((A : Type)) (Π ((x : A)) (= A x x)))
;;;
;;; Elimination (J, "eliminator" or "transport"):
;;;   J : (Π ((A : Type))
;;;        (Π ((P : (Π ((x : A)) (Π ((y : A)) (Π ((_ : (= A x y))) Type)))))
;;;         (Π ((d : (Π ((x : A)) (P x x refl))))
;;;          (Π ((x : A))
;;;           (Π ((y : A))
;;;            (Π ((p : (= A x y)))
;;;             (P x y p)))))))
;;;
;;; Computation rule:
;;;   J A P d x x refl ≡ d x
;;;
;;; This implements propositional equality as found in Martin-Löf Type Theory.

;;; equality-type-well-formed? : SExpr → Boolean
;;; Check if an equality type expression is well-formed: (= A x y)
(define (equality-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) '=)
       (= (length t) 4)))

;;; equality-carrier : Type → Type
;;; Get the carrier type A from (= A x y)
(define (equality-carrier t)
  (if (equality-type? t)
      (cadr t)
      'Void))

;;; equality-lhs : Type → Expr
;;; Get the left-hand side x from (= A x y)
(define (equality-lhs t)
  (if (equality-type? t)
      (caddr t)
      #f))

;;; equality-rhs : Type → Expr
;;; Get the right-hand side y from (= A x y)
(define (equality-rhs t)
  (if (equality-type? t)
      (cadddr t)
      #f))

;;; ====
;;; Heterogeneous Equality Operations
;;; ====
;;;
;;; Heterogeneous equality (HEq) relates terms of potentially different types.
;;; Syntax: (HEq A B a b)
;;;   - A is the type of a
;;;   - B is the type of b
;;;   - a is the left term
;;;   - b is the right term
;;;
;;; When A ≡ B, we have (HEq A B a b) ≡ (= A a b).
;;;
;;; This is needed for dependent congruence:
;;;   For f : Π(x:A).B(x) and p : x ≡ y : A,
;;;   (cong f p) : HEq B(x) B(y) (f x) (f y)
;;;
;;; In HoTT, this corresponds to PathOver or dependent paths.

;;; heq-type? : Type → Boolean
;;; Check if this is a heterogeneous equality type: (HEq A B a b)
(define (heq-type? t)
  (and (pair? t) (eq? (car t) 'HEq)))

;;; heq-type-well-formed? : SExpr → Boolean
;;; Check if a heterogeneous equality type expression is well-formed
(define (heq-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'HEq)
       (= (length t) 5)))

;;; heq-left-type : Type → Type
;;; Get the left type A from (HEq A B a b)
(define (heq-left-type t)
  (if (heq-type? t)
      (cadr t)
      'Void))

;;; heq-right-type : Type → Type
;;; Get the right type B from (HEq A B a b)
(define (heq-right-type t)
  (if (heq-type? t)
      (caddr t)
      'Void))

;;; heq-lhs : Type → Expr
;;; Get the left term a from (HEq A B a b)
(define (heq-lhs t)
  (if (heq-type? t)
      (cadddr t)
      #f))

;;; heq-rhs : Type → Expr
;;; Get the right term b from (HEq A B a b)
(define (heq-rhs t)
  (if (heq-type? t)
      (car (cddddr t))
      #f))

;;; make-heq-type : Type × Type × Expr × Expr → Type
;;; Construct a heterogeneous equality type (HEq A B a b)
;;; If A and B are definitionally equal, returns homogeneous equality instead.
(define (make-heq-type A B a b)
  (if (equal? A B)
      `(= ,A ,a ,b)  ; Reduce to homogeneous when types match
      `(HEq ,A ,B ,a ,b)))

;;; ====
;;; Refinement Type Operations
;;; ====
;;;
;;; Refinement types are types annotated with predicates.
;;; Syntax: (refine ((x : T)) P)
;;;   - x is the bound variable
;;;   - T is the base type
;;;   - P is a predicate that must hold for values of this type
;;;
;;; Example:
;;;   (refine ((n : Nat)) (> n 0))  ; Positive natural numbers
;;;   (refine ((v : (Vec n Int))) (sorted v))  ; Sorted vectors
;;;
;;; Mathematical notation: {x : T | P}
;;;
;;; Subtyping:
;;;   {x:T | P} <: {x:T | Q}  when  ∀x:T. P(x) ⟹ Q(x)
;;;   {x:T | P} <: T          always (forgetting the refinement)
;;;
;;; Introduction:
;;;   To construct a value of type {x:T | P}, provide a value v:T
;;;   and a proof that P[v/x] holds.
;;;
;;; Elimination:
;;;   From a value of type {x:T | P}, you can:
;;;   1. Extract the underlying value of type T
;;;   2. Use P as an assumption

;;; refinement-type-well-formed? : SExpr → Boolean
;;; Check if a refinement type is well-formed: (refine ((x : T)) P)
(define (refinement-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'refine)
       (= (length t) 3)
       (let ([binding (cadr t)]
             [predicate (caddr t)])
            (and (list? binding)
                 (= (length binding) 1)
                 (typed-binding? (car binding))))))

;;; refinement-var : Type → Symbol
;;; Get the bound variable from a refinement type.
(define (refinement-var t)
  (if (refinement-type? t)
      (caar (cadr t))
      #f))

;;; refinement-base-type : Type → Type
;;; Get the base type T from (refine ((x : T)) P).
(define (refinement-base-type t)
  (if (refinement-type? t)
      (caddar (cadr t))  ; Third element of first binding
      'Void))

;;; refinement-predicate : Type → Expr
;;; Get the predicate P from (refine ((x : T)) P).
(define (refinement-predicate t)
  (if (refinement-type? t)
      (caddr t)
      #f))

;;; ====
;;; Differentiable Type Operations
;;; ====
;;;
;;; The Diff type constructor represents differentiable functions with
;;; dependent dimension tracking for automatic differentiation.
;;;
;;; Syntax: (Diff A B)
;;;   - A is the domain type (input type)
;;;   - B is the codomain type (output type)
;;;   - Both A and B should be numeric types (scalars, Vec, Matrix)
;;;
;;; Examples:
;;;   (Diff Float Float)                     ; R → R differentiable function
;;;   (Diff (Vec n Float) Float)             ; R^n → R (gradient computable)
;;;   (Diff (Vec n Float) (Vec m Float))     ; R^n → R^m (Jacobian computable)
;;;
;;; The type system uses Diff to:
;;;   1. Track which functions support automatic differentiation
;;;   2. Verify dimension compatibility of gradient operations
;;;   3. Enable type-directed mode selection (forward vs reverse AD)
;;;
;;; Type rules:
;;;   Γ ⊢ A : Type    Γ ⊢ B : Type    Num A    Num B
;;;   ───────────────────────────────────────────────
;;;              Γ ⊢ (Diff A B) : Type
;;;
;;; Related operations have types:
;;;   grad     : (Diff (Vec n α) α) → (Vec n α) → (Vec n α)
;;;   jacobian : (Diff (Vec n α) (Vec m α)) → (Vec n α) → (Matrix m n α)
;;;   hessian  : (Diff (Vec n α) α) → (Vec n α) → (Matrix n n α)
;;;   compose  : (Diff B C) → (Diff A B) → (Diff A C)
;;;   lift     : A → (Diff A A)  ; lift constant to differentiable
;;;   primal   : (Diff A B) → (A → B)  ; extract the underlying function
;;;
;;; The Grad type represents gradient values (same shape as domain):
;;;   (Grad A) ≅ A  ; for scalars
;;;   (Grad (Vec n α)) ≅ (Vec n α)  ; gradient of n-input function
;;;   (Grad (Matrix m n α)) ≅ (Matrix m n α)  ; gradient of matrix-input

;;; diff-type? : Type → Boolean
;;; Check if this is a differentiable function type: (Diff A B)
(define (diff-type? t)
  (and (pair? t) (eq? (car t) 'Diff)))

;;; diff-type-well-formed? : SExpr → Boolean
;;; Check if a Diff type expression is well-formed: (Diff Domain Codomain)
(define (diff-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Diff)
       (= (length t) 3)))

;;; diff-domain : Type → Type
;;; Get the domain type A from (Diff A B)
(define (diff-domain t)
  (if (diff-type? t)
      (cadr t)
      'Void))

;;; diff-codomain : Type → Type
;;; Get the codomain type B from (Diff A B)
(define (diff-codomain t)
  (if (diff-type? t)
      (caddr t)
      'Void))

;;; make-diff-type : Type × Type → Type
;;; Construct a differentiable function type (Diff A B)
(define (make-diff-type domain codomain)
  `(Diff ,domain ,codomain))

;;; grad-type? : Type → Boolean
;;; Check if this is a gradient type: (Grad T)
(define (grad-type? t)
  (and (pair? t) (eq? (car t) 'Grad)))

;;; grad-type-well-formed? : SExpr → Boolean
;;; Check if a Grad type expression is well-formed: (Grad T)
(define (grad-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'Grad)
       (= (length t) 2)))

;;; grad-inner-type : Type → Type
;;; Get the inner type T from (Grad T)
(define (grad-inner-type t)
  (if (grad-type? t)
      (cadr t)
      'Void))

;;; make-grad-type : Type → Type
;;; Construct a gradient type (Grad T)
(define (make-grad-type inner)
  `(Grad ,inner))

;;; ====
;;; Differentiable Type Utilities
;;; ====

;;; numeric-type? : Type → Boolean
;;; Check if a type is numeric (supports differentiation).
;;; Numeric types are: Nat, Int, Float, (Vec n T), (Matrix m n T) where T is numeric.
(define (numeric-type? t)
  (cond
   [(symbol? t) (if (memq t '(Nat Int Float Real Number)) #t #f)]
   [(not (pair? t)) #f]
   [(eq? (car t) 'Vec) (and (>= (length t) 2)
                            (if (>= (length t) 3)
                                (numeric-type? (caddr t))
                                #t))]  ; (Vec n) without element type defaults to numeric
   [(eq? (car t) 'Matrix) (and (>= (length t) 3)
                               (if (>= (length t) 4)
                                   (numeric-type? (cadddr t))
                                   #t))]  ; (Matrix m n) without element type defaults to numeric
   [else #f]))

;;; scalar-type? : Type → Boolean
;;; Check if a type is a scalar numeric type.
(define (scalar-type? t)
  (and (symbol? t) (if (memq t '(Nat Int Float Real Number)) #t #f)))

;;; diff-composable? : Type × Type → Boolean
;;; Check if two Diff types can be composed: (Diff B C) ∘ (Diff A B)
(define (diff-composable? diff1 diff2)
  (and (diff-type? diff1)
       (diff-type? diff2)
       ;; diff1 domain must match diff2 codomain
       (equal? (diff-domain diff1) (diff-codomain diff2))))

;;; ====
;;; Dimension Extraction for Autodiff
;;; ====

;;; diff-input-dim : Type → Nat | 'scalar | #f
;;; Extract the input dimension from a Diff type.
;;; Returns 'scalar for scalar input, n for (Vec n _), or #f if unknown.
(define (diff-input-dim t)
  (if (not (diff-type? t))
      #f
      (let ([domain (diff-domain t)])
           (cond
            [(scalar-type? domain) 'scalar]
            [(and (pair? domain) (eq? (car domain) 'Vec) (>= (length domain) 2))
             (cadr domain)]  ; Returns n from (Vec n _)
            [else #f]))))

;;; diff-output-dim : Type → Nat | 'scalar | #f
;;; Extract the output dimension from a Diff type.
;;; Returns 'scalar for scalar output, m for (Vec m _), or #f if unknown.
(define (diff-output-dim t)
  (if (not (diff-type? t))
      #f
      (let ([codomain (diff-codomain t)])
           (cond
            [(scalar-type? codomain) 'scalar]
            [(and (pair? codomain) (eq? (car codomain) 'Vec) (>= (length codomain) 2))
             (cadr codomain)]  ; Returns m from (Vec m _)
            [else #f]))))

;;; diff-grad-type : Type → Type
;;; Compute the gradient type for a (Diff A B) type.
;;; For R^n → R, gradient is R^n.
;;; For R^n → R^m, gradient per component is R^n, Jacobian is Matrix m n.
(define (diff-grad-type t)
  (if (not (diff-type? t))
      'Void
      (let ([domain (diff-domain t)])
           (make-grad-type domain))))

;;; diff-jacobian-type : Type → Type
;;; Compute the Jacobian type for a (Diff A B) type.
;;; For (Diff (Vec n α) (Vec m α)), returns (Matrix m n α).
(define (diff-jacobian-type t)
  (if (not (diff-type? t))
      'Void
      (let ([domain (diff-domain t)]
            [codomain (diff-codomain t)])
           (cond
            ;; (Diff (Vec n α) (Vec m α)) → (Matrix m n α)
            [(and (pair? domain) (eq? (car domain) 'Vec)
                  (pair? codomain) (eq? (car codomain) 'Vec))
             (let ([n (cadr domain)]
                   [m (cadr codomain)]
                   [elem-type (if (>= (length domain) 3) (caddr domain) 'Float)])
                  `(Matrix ,m ,n ,elem-type))]
            ;; (Diff (Vec n α) α) → (Vec n α) (same as gradient)
            [(and (pair? domain) (eq? (car domain) 'Vec)
                  (scalar-type? codomain))
             domain]
            ;; Scalar → Scalar: Jacobian is just the derivative (scalar)
            [(and (scalar-type? domain) (scalar-type? codomain))
             domain]
            [else 'Void]))))

;;; diff-hessian-type : Type → Type
;;; Compute the Hessian type for a (Diff A B) type.
;;; For (Diff (Vec n α) α), returns (Matrix n n α).
(define (diff-hessian-type t)
  (if (not (diff-type? t))
      'Void
      (let ([domain (diff-domain t)]
            [codomain (diff-codomain t)])
           (cond
            ;; (Diff (Vec n α) α) → (Matrix n n α)
            [(and (pair? domain) (eq? (car domain) 'Vec)
                  (scalar-type? codomain))
             (let ([n (cadr domain)]
                   [elem-type (if (>= (length domain) 3) (caddr domain) 'Float)])
                  `(Matrix ,n ,n ,elem-type))]
            ;; Scalar → Scalar: Hessian is just scalar
            [(and (scalar-type? domain) (scalar-type? codomain))
             domain]
            [else 'Void]))))

;;; ====
;;; Inductive Type Operations
;;; ====
;;;
;;; Inductive types (data declarations) define new types with constructors.
;;;
;;; Syntax: (data (Name params...) [ctor : type] ...)
;;;
;;; Structure:
;;;   car = 'data
;;;   cadr = (Name params...) - type header with name and parameters
;;;   cddr = ([ctor : type] ...) - list of constructors
;;;
;;; Parameters can be:
;;;   - Bare symbols: implicit Type parameter (e.g., A means A : Type)
;;;   - Typed bindings: (x : T) for explicit parameter types
;;;
;;; Constructors are:
;;;   - [ctor-name : ctor-type]
;;;   - ctor-type must return the inductive type (possibly with different indices)
;;;
;;; Examples:
;;;   (data (Bool)
;;;     [true : Bool]
;;;     [false : Bool])
;;;
;;;   (data (Nat)
;;;     [zero : Nat]
;;;     [succ : (Π ((n : Nat)) Nat)])
;;;
;;;   (data (List A)
;;;     [nil : (List A)]
;;;     [cons : (Π ((x : A)) (Π ((xs : (List A))) (List A)))])

;;; data-type-well-formed? : SExpr → Boolean
;;; Check if a data type definition is well-formed.
(define (data-type-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'data)
       (>= (length t) 2)
       (pair? (cadr t))              ; Header must be a list
       (symbol? (caadr t))           ; First element is the type name
       (not (constructor-decl? (cadr t)))  ; Header is not a constructor decl
       (andmap constructor-decl? (cddr t))))  ; Rest are constructor declarations

;;; constructor-decl? : SExpr → Boolean
;;; Check if this is a valid constructor declaration: [name : type]
(define (constructor-decl? d)
  (and (list? d)
       (= (length d) 3)
       (symbol? (car d))
       (eq? (cadr d) ':)))

;;; data-name : DataType → Symbol
;;; Get the name of an inductive type.
(define (data-name t)
  (if (data-type? t)
      (caadr t)
      #f))

;;; data-header : DataType → (List SExpr)
;;; Get the full header (name and parameters) of a data type.
(define (data-header t)
  (if (data-type? t)
      (cadr t)
      '()))

;;; data-params : DataType → (List (Symbol | Binding))
;;; Get the parameters of an inductive type (everything after the name).
(define (data-params t)
  (if (data-type? t)
      (cdadr t)
      '()))

;;; data-constructors : DataType → (List (name . type))
;;; Get the constructors of an inductive type as an alist.
(define (data-constructors t)
  (if (data-type? t)
      (map (lambda (c) (cons (car c) (caddr c)))
           (cddr t))
      '()))

;;; data-constructor-decls : DataType → (List [name : type])
;;; Get the raw constructor declarations.
(define (data-constructor-decls t)
  (if (data-type? t)
      (cddr t)
      '()))

;;; data-constructor-names : DataType → (List Symbol)
;;; Get just the constructor names.
(define (data-constructor-names t)
  (map car (data-constructors t)))

;;; data-constructor-type : DataType × Symbol → Type | #f
;;; Get the type of a specific constructor by name.
(define (data-constructor-type t ctor-name)
  (let ([ctors (data-constructors t)])
       (let ([entry (assq ctor-name ctors)])
            (if entry (cdr entry) #f))))

;;; param-binding : Param → (Symbol . Type)
;;; Convert a parameter to a binding pair.
;;; Bare symbols get implicit Type binding.
(define (param-binding p)
  (if (typed-binding? p)
      (cons (binding-var p) (binding-type p))
      (cons p 'Type)))  ; Implicit Type for bare symbols

;;; data-param-bindings : DataType → (List (Symbol . Type))
;;; Get parameters as an alist of bindings.
(define (data-param-bindings t)
  (map param-binding (data-params t)))

;;; data-applied-type : DataType → Type
;;; Get the type that constructors return (type name applied to parameters).
;;; E.g., for (data (List A) ...) returns (List A)
(define (data-applied-type t)
  (let ([name (data-name t)]
        [params (data-params t)])
       (if (null? params)
           name
           (cons name (map (lambda (p)
                                   (if (typed-binding? p)
                                       (binding-var p)
                                       p))
                           params)))))

;;; ====
;;; Inductive Type Instance Predicate
;;; ====
;;;
;;; An inductive type instance is an application of a data type name
;;; to arguments: (TypeName args...)
;;;
;;; This is used to check if a type is an instance of a declared
;;; inductive type (after the data declaration has been processed).

;;; make-data-type-predicate : Symbol → (Type → Boolean)
;;; Create a predicate that checks if a type is an instance of
;;; the given inductive type name.
(define (make-data-type-predicate name)
  (lambda (t)
          (cond
           [(eq? t name) #t]
           [(and (pair? t) (eq? (car t) name)) #t]
           [else #f])))

;;; ====
;;; Module Signature Types
;;; ====
;;;
;;; Module signatures describe the interface of a module with
;;; support for dependent types.
;;;
;;; Syntax: (sig Name decl ...)
;;;
;;; Declaration forms:
;;;   [type T : Kind]     ; Abstract type with kind
;;;   [type T = Type]     ; Type alias (transparent)
;;;   [val name : Type]   ; Value declaration
;;;   [data DataDecl]     ; Inductive type export
;;;   [include SigName]   ; Include another signature
;;;
;;; Structure:
;;;   car = 'sig
;;;   cadr = signature name (symbol)
;;;   cddr = list of declarations

;;; sig-type? : SExpr → Boolean
;;; Check if this is a module signature.
(define (sig-type? t)
  (and (pair? t) (eq? (car t) 'sig)))

;;; sig-well-formed? : SExpr → Boolean
;;; Check if a signature is well-formed.
(define (sig-well-formed? t)
  (and (pair? t)
       (eq? (car t) 'sig)
       (>= (length t) 2)
       (symbol? (cadr t))
       (andmap sig-decl? (cddr t))))

;;; sig-decl? : SExpr → Boolean
;;; Check if this is a valid signature declaration.
(define (sig-decl? d)
  (and (list? d)
       (>= (length d) 2)
       (memq (car d) '(type val data include))))

;;; sig-name : Signature → Symbol
;;; Get the name of a signature.
(define (sig-name sig)
  (if (sig-type? sig)
      (cadr sig)
      #f))

;;; sig-decls : Signature → (List Decl)
;;; Get the declarations of a signature.
(define (sig-decls sig)
  (if (sig-type? sig)
      (cddr sig)
      '()))

;;; ====
;;; Signature Declaration Types
;;; ====

;;; sig-type-decl? : Decl → Boolean
;;; Check if this is a type declaration: [type T : Kind] or [type T = Type]
(define (sig-type-decl? d)
  (and (pair? d) (eq? (car d) 'type)))

;;; sig-val-decl? : Decl → Boolean
;;; Check if this is a value declaration: [val name : Type]
(define (sig-val-decl? d)
  (and (pair? d) (eq? (car d) 'val)))

;;; sig-data-decl? : Decl → Boolean
;;; Check if this is an inductive type export: [data DataDecl]
(define (sig-data-decl? d)
  (and (pair? d) (eq? (car d) 'data)))

;;; sig-include-decl? : Decl → Boolean
;;; Check if this is an include: [include SigName]
(define (sig-include-decl? d)
  (and (pair? d) (eq? (car d) 'include)))

;;; ====
;;; Signature Declaration Operations
;;; ====

;;; sig-type-decl-name : Decl → Symbol
;;; Get the name from a type declaration.
(define (sig-type-decl-name d)
  (if (sig-type-decl? d) (cadr d) #f))

;;; sig-type-decl-abstract? : Decl → Boolean
;;; Check if this is an abstract type declaration [type T : Kind]
(define (sig-type-decl-abstract? d)
  (and (sig-type-decl? d)
       (= (length d) 4)
       (eq? (caddr d) ':)))

;;; sig-type-decl-alias? : Decl → Boolean
;;; Check if this is a type alias [type T = Type]
(define (sig-type-decl-alias? d)
  (and (sig-type-decl? d)
       (= (length d) 4)
       (eq? (caddr d) '=)))

;;; sig-type-decl-kind : Decl → Kind | #f
;;; Get the kind from an abstract type declaration.
(define (sig-type-decl-kind d)
  (if (sig-type-decl-abstract? d)
      (cadddr d)
      #f))

;;; sig-type-decl-def : Decl → Type | #f
;;; Get the definition from a type alias.
(define (sig-type-decl-def d)
  (if (sig-type-decl-alias? d)
      (cadddr d)
      #f))

;;; sig-val-decl-name : Decl → Symbol
;;; Get the name from a value declaration.
(define (sig-val-decl-name d)
  (if (sig-val-decl? d) (cadr d) #f))

;;; sig-val-decl-type : Decl → Type
;;; Get the type from a value declaration.
(define (sig-val-decl-type d)
  (if (and (sig-val-decl? d) (= (length d) 4) (eq? (caddr d) ':))
      (cadddr d)
      #f))

;;; sig-data-decl-data : Decl → DataDecl
;;; Get the data declaration from a data export.
(define (sig-data-decl-data d)
  (if (sig-data-decl? d) (cadr d) #f))

;;; sig-include-decl-name : Decl → Symbol
;;; Get the signature name from an include.
(define (sig-include-decl-name d)
  (if (sig-include-decl? d) (cadr d) #f))

;;; ====
;;; Signature Construction
;;; ====

;;; t-sig : Symbol × (List Decl) → Signature
;;; Construct a module signature.
(define (t-sig name decls)
  `(sig ,name ,@decls))

;;; t-sig-type-abstract : Symbol × Kind → Decl
;;; Construct an abstract type declaration.
(define (t-sig-type-abstract name kind)
  `(type ,name : ,kind))

;;; t-sig-type-alias : Symbol × Type → Decl
;;; Construct a type alias.
(define (t-sig-type-alias name type)
  `(type ,name = ,type))

;;; t-sig-val : Symbol × Type → Decl
;;; Construct a value declaration.
(define (t-sig-val name type)
  `(val ,name : ,type))

;;; t-sig-data : DataDecl → Decl
;;; Construct an inductive type export.
(define (t-sig-data data-decl)
  `(data ,data-decl))

;;; t-sig-include : Symbol → Decl
;;; Construct a signature inclusion.
(define (t-sig-include sig-name)
  `(include ,sig-name))

;;; ====
;;; Signature Extraction
;;; ====

;;; sig-type-names : Signature → (List Symbol)
;;; Get all type names declared in a signature.
(define (sig-type-names sig)
  (filter symbol?
          (map sig-type-decl-name
               (filter sig-type-decl? (sig-decls sig)))))

;;; sig-val-names : Signature → (List Symbol)
;;; Get all value names declared in a signature.
(define (sig-val-names sig)
  (filter symbol?
          (map sig-val-decl-name
               (filter sig-val-decl? (sig-decls sig)))))

;;; sig-data-names : Signature → (List Symbol)
;;; Get all inductive type names declared in a signature.
(define (sig-data-names sig)
  (filter symbol?
          (map (lambda (d)
                       (let ([data (sig-data-decl-data d)])
                            (if (data-type? data)
                                (data-name data)
                                #f)))
               (filter sig-data-decl? (sig-decls sig)))))

;;; sig-includes : Signature → (List Symbol)
;;; Get all included signature names.
(define (sig-includes sig)
  (filter symbol?
          (map sig-include-decl-name
               (filter sig-include-decl? (sig-decls sig)))))

;;; sig-exports : Signature → (List Symbol)
;;; Get all exported names (types, values, data constructors).
(define (sig-exports sig)
  (append (sig-type-names sig)
          (sig-val-names sig)
          (sig-data-names sig)))

;;; ====
;;; Universe Operations
;;; ====

;;; universe-level : Type → Nat
(define (universe-level t)
  (cond
   [(eq? t 'Type) 0]
   [(and (pair? t) (eq? (car t) 'Type)) (cadr t)]
   [else 0]))

;;; universe-max : Type × Type → Type
;;; The maximum universe level of two types.
(define (universe-max u1 u2)
  (let ([l1 (universe-level u1)]
        [l2 (universe-level u2)])
       (if (= (max l1 l2) 0)
           'Type
           `(Type ,(max l1 l2)))))

;;; universe-succ : Type → Type
;;; The successor universe.
(define (universe-succ u)
  `(Type ,(+ 1 (universe-level u))))

;;; ====
;;; Typed Binding Helpers
;;; ====

;;; typed-binding? : SExpr → Boolean
;;; Check if this is a valid typed binding: (x : T)
(define (typed-binding? b)
  (and (list? b)
       (= (length b) 3)
       (symbol? (car b))
       (eq? (cadr b) ':)))

;;; binding-var : Binding → Symbol
(define (binding-var b)
  (car b))

;;; binding-type : Binding → Type
(define (binding-type b)
  (caddr b))

;;; ====
;;; Dependent Type Constructors
;;; ====

;;; t-pi : Symbol × Type × Type → Type
;;; Construct a Pi type (dependent function type).
;;; (t-pi 'n 'Nat '(Vec Int n)) → (Π ((n : Nat)) (Vec Int n))
(define (t-pi var domain codomain)
  `(Π ((,var : ,domain)) ,codomain))

;;; t-pi* : ((Symbol . Type) ...) × Type → Type
;;; Construct a multi-parameter Pi type from an alist of bindings.
(define (t-pi* bindings body)
  (if (null? bindings)
      body
      (let ([binding (car bindings)])
           (t-pi (car binding) (cdr binding)
                 (t-pi* (cdr bindings) body)))))

;;; t-sigma : Symbol × Type × Type → Type
;;; Construct a Sigma type (dependent pair type).
(define (t-sigma var fst-type snd-type)
  `(Σ ((,var : ,fst-type)) ,snd-type))

;;; t-type : [Nat] → Type
;;; Construct a universe type.
(define (t-type . args)
  (if (null? args)
      'Type
      (if (= (car args) 0)
          'Type
          `(Type ,(car args)))))

;;; t-eq : Type × Expr × Expr → Type
;;; Construct an equality type (propositional equality).
;;; (t-eq 'Nat 'x 'y) → (= Nat x y)
(define (t-eq carrier lhs rhs)
  `(= ,carrier ,lhs ,rhs))

;;; t-refine : Symbol × Type × Expr → Type
;;; Construct a refinement type.
;;; (t-refine 'n 'Nat '(> n 0)) → (refine ((n : Nat)) (> n 0))
(define (t-refine var base-type predicate)
  `(refine ((,var : ,base-type)) ,predicate))

;;; t-data : Symbol × (List Param) × (List (Symbol . Type)) → DataType
;;; Construct an inductive type declaration.
;;; (t-data 'Nat '() '((zero . Nat) (succ . (Π ((n : Nat)) Nat))))
;;; → (data (Nat) [zero : Nat] [succ : (Π ((n : Nat)) Nat)])
(define (t-data name params constructors)
  (let ([header (cons name params)]
        [ctor-decls (map (lambda (c) `(,(car c) : ,(cdr c)))
                         constructors)])
       `(data ,header ,@ctor-decls)))

;;; t-constructor : Symbol × Type → Constructor-Declaration
;;; Construct a constructor declaration.
;;; (t-constructor 'zero 'Nat) → [zero : Nat]
(define (t-constructor name type)
  `(,name : ,type))

;;; t-data-simple : Symbol × (List (Symbol . Type)) → DataType
;;; Construct a simple inductive type with no parameters.
;;; (t-data-simple 'Bool '((true . Bool) (false . Bool)))
;;; → (data (Bool) [true : Bool] [false : Bool])
(define (t-data-simple name constructors)
  (t-data name '() constructors))

;;; ====
;;; Eliminator Generation
;;; ====
;;;
;;; For each inductive type, we generate an eliminator (recursor).
;;;
;;; For a type D with constructors c₁, c₂, ..., cₙ:
;;;   elim-D : (P : D → Type) →
;;;            (method₁ : ...) →
;;;            (method₂ : ...) →
;;;            ...
;;;            (x : D) →
;;;            (P x)
;;;
;;; Each method corresponds to a constructor and handles that case.
;;;
;;; Example for Nat:
;;;   elim-Nat : (P : Nat → Type) →
;;;              (z : (P zero)) →                     ; zero case
;;;              (s : (Π ((n : Nat)) (Π ((_ : (P n))) (P (succ n))))) →  ; succ case
;;;              (n : Nat) →
;;;              (P n)
;;;
;;; Computation rules:
;;;   (elim-Nat P z s zero) = z
;;;   (elim-Nat P z s (succ n)) = (s n (elim-Nat P z s n))

;;; generate-eliminator-type : DataType → Type
;;; Generate the type of the eliminator for an inductive type.
(define (generate-eliminator-type data-decl)
  (let* ([name (data-name data-decl)]
         [params (data-param-bindings data-decl)]
         [applied-type (data-applied-type data-decl)]
         [ctors (data-constructors data-decl)]
         ;; Motive: P : D → Type
         [motive-type (t-pi 'x applied-type 'Type)]
         ;; Build method types for each constructor
         [method-types (map (lambda (c)
                                    (generate-method-type c applied-type 'P))
                            ctors)]
         ;; Target: x : D
         ;; Result: (P x)
         [result-type `(P x)])
        ;; Combine: Π(P : D → Type). Π(m₁). ... Π(mₙ). Π(x : D). (P x)
        (let ([inner (t-pi 'x applied-type result-type)])
             (fold-right (lambda (method-type body)
                                 (t-pi (car method-type) (cdr method-type) body))
                         inner
                         (cons (cons 'P motive-type) method-types)))))

;;; generate-method-type : (Symbol . Type) × Type × Symbol → (Symbol . Type)
;;; Generate the method type for a constructor in the eliminator.
;;; For a constructor c : A₁ → A₂ → ... → D, the method type is:
;;;   (Π ((a₁ : A₁)) (Π ((a₂ : A₂)) ... (Π ((ih : P aᵢ)) ...) (P (c a₁ a₂ ...))))
;;; where ih bindings are added for recursive occurrences.
(define (generate-method-type ctor applied-type motive)
  (let* ([ctor-name (car ctor)]
         [ctor-type (cdr ctor)]
         [method-name (string->symbol (string-append "m-" (symbol->string ctor-name)))])
        ;; For now, generate a simplified method type
        ;; Full implementation would analyze recursive positions
        (cons method-name (generate-method-type-body ctor-type applied-type motive '()))))

;;; generate-method-type-body : Type × Type × Symbol × (List Symbol) → Type
;;; Recursively build the method body type.
(define (generate-method-type-body ctor-type applied-type motive args)
  (cond
   ;; If we've reached the return type (applied inductive type)
   [(types-match-head? ctor-type applied-type)
    ;; Result: (P (ctor args...))
    `(,motive ,(if (null? args)
                   ctor-type
                   (cons (get-type-head applied-type) (reverse args))))]
   ;; If it's a Pi type, add the parameter
   [(pi-type? ctor-type)
    (let* ([var (pi-var ctor-type)]
           [domain (pi-domain ctor-type)]
           [codomain (pi-codomain ctor-type)]
           [is-recursive (types-match-head? domain applied-type)])
          (if is-recursive
              ;; Add induction hypothesis for recursive argument
              (t-pi var domain
                    (t-pi (fresh-ih-var var) `(,motive ,var)
                          (generate-method-type-body codomain applied-type motive
                                                     (cons var args))))
              ;; Non-recursive argument
              (t-pi var domain
                    (generate-method-type-body codomain applied-type motive
                                               (cons var args)))))]
   ;; Base case - shouldn't normally reach here
   [else `(,motive ctor-type)]))

;;; types-match-head? : Type × Type → Boolean
;;; Check if two types have the same head (type name).
(define (types-match-head? t1 t2)
  (equal? (get-type-head t1) (get-type-head t2)))

;;; get-type-head : Type → Symbol | #f
;;; Get the head symbol of a type application.
(define (get-type-head t)
  (cond
   [(symbol? t) t]
   [(and (pair? t) (symbol? (car t))) (car t)]
   [else #f]))

;;; fresh-ih-var : Symbol → Symbol
;;; Generate a fresh variable name for an induction hypothesis.
(define (fresh-ih-var base)
  (string->symbol (string-append "ih-" (symbol->string base))))

;;; eliminator-name : Symbol → Symbol
;;; Generate the eliminator name for a data type.
(define (eliminator-name data-name)
  (string->symbol (string-append "elim-" (symbol->string data-name))))

;;; ====
;;; Equality Proof Term Predicates
;;; ====
;;;
;;; refl is the canonical proof of (= A x x)
;;; J is the eliminator for equality proofs

;;; refl-term? : Expr → Boolean
;;; Check if this is a refl proof term: (refl A x) or just 'refl
(define (refl-term? e)
  (or (eq? e 'refl)
      (and (pair? e) (eq? (car e) 'refl))))

;;; refl-type : Expr → Type | #f
;;; Get the type annotation from (refl A x), returns #f for bare 'refl
(define (refl-carrier e)
  (cond
   [(eq? e 'refl) #f]
   [(and (pair? e) (eq? (car e) 'refl) (>= (length e) 2))
    (cadr e)]
   [else #f]))

;;; refl-witness : Expr → Expr | #f
;;; Get the witnessed term from (refl A x), returns #f for bare 'refl
(define (refl-witness e)
  (cond
   [(eq? e 'refl) #f]
   [(and (pair? e) (eq? (car e) 'refl) (= (length e) 3))
    (caddr e)]
   [else #f]))

;;; j-term? : Expr → Boolean
;;; Check if this is a J eliminator application: (J A P d x y p)
(define (j-term? e)
  (and (pair? e) (eq? (car e) 'J)))

;;; ====
;;; Free Variables with Dependent Types
;;; ====

;;; dep-free-tvars : Type → (List Symbol)
;;; Collect free type variables, handling dependent types.
(define (dep-free-tvars t)
  (dep-free-tvars-with '() t))

(define (dep-free-tvars-with bound t)
  (cond
   [(type-var? t)
    (if (memq t bound) '() (list t))]
   [(or (base-type? t) (hole? t) (universe-type? t)) '()]
   [(not (pair? t)) '()]
   
   ;; Pi type: (Π ((x : A)) B) - x is bound in B
   [(eq? (car t) 'Π)
    (let* ([bindings (cadr t)]
           [body (caddr t)]
           [domain-vars (apply append
                               (map (lambda (b)
                                            (dep-free-tvars-with bound (binding-type b)))
                                    bindings))]
           [bound-vars (map binding-var bindings)]
           [new-bound (append bound-vars bound)]
           [body-vars (dep-free-tvars-with new-bound body)])
          (append domain-vars body-vars))]
   
   ;; Sigma type: (Σ ((x : A)) B) - x is bound in B
   [(eq? (car t) 'Σ)
    (let* ([binding (car (cadr t))]
           [body (caddr t)]
           [domain-vars (dep-free-tvars-with bound (binding-type binding))]
           [new-bound (cons (binding-var binding) bound)]
           [body-vars (dep-free-tvars-with new-bound body)])
          (append domain-vars body-vars))]
   
   ;; Fall back to base type handling
   [(eq? (car t) '∀)
    (let ([new-bound (append (cadr t) bound)])
         (dep-free-tvars-with new-bound (caddr t)))]
   [(eq? (car t) 'μ)
    (let ([new-bound (cons (cadr t) bound)])
         (dep-free-tvars-with new-bound (caddr t)))]
   [else
    (apply append (map (lambda (sub) (dep-free-tvars-with bound sub)) (cdr t)))]))

;;; ====
;;; Substitution with Dependent Types (Capture-Avoiding)
;;; ====

;;; fresh-var : Symbol × (List Symbol) → Symbol
;;; Generate a fresh variable name that is not in the avoid list.
(define (fresh-var base avoid)
  (let loop ([n 0])
       (let ([candidate (string->symbol
                         (string-append (symbol->string base)
                                        (number->string n)))])
            (if (memq candidate avoid)
                (loop (+ n 1))
                candidate))))

;;; dep-free-vars : Type → (List Symbol)
;;; Collect all free variables in a type (not just type vars, but term vars too).
(define (dep-free-vars t)
  (dep-free-vars-with '() t))

(define (dep-free-vars-with bound t)
  (cond
   [(symbol? t)
    (if (or (memq t bound) (base-type? t))
        '()
        (list t))]
   [(not (pair? t)) '()]
   [(or (hole? t) (universe-type? t)) '()]
   
   ;; Pi type: (Π ((x : A) ...) B) - bindings are bound in later bindings and body
   [(eq? (car t) 'Π)
    (let* ([bindings (cadr t)]
           [body (caddr t)])
          (dep-free-vars-in-bindings-and-body bound bindings body))]
   
   ;; Sigma type: (Σ ((x : A)) B) - x is bound in B
   [(eq? (car t) 'Σ)
    (let* ([binding (car (cadr t))]
           [body (caddr t)]
           [domain-vars (dep-free-vars-with bound (binding-type binding))]
           [new-bound (cons (binding-var binding) bound)]
           [body-vars (dep-free-vars-with new-bound body)])
          (append domain-vars body-vars))]
   
   ;; Forall: (∀ (vars ...) body)
   [(eq? (car t) '∀)
    (let ([new-bound (append (cadr t) bound)])
         (dep-free-vars-with new-bound (caddr t)))]
   
   ;; Mu: (μ var body)
   [(eq? (car t) 'μ)
    (let ([new-bound (cons (cadr t) bound)])
         (dep-free-vars-with new-bound (caddr t)))]
   
   ;; Lambda: (fn (x) body) or (lambda (x) body)
   [(or (eq? (car t) 'fn) (eq? (car t) 'lambda))
    (let ([params (cadr t)]
          [body (caddr t)])
         (let ([new-bound (if (list? params)
                              (append params bound)
                              (cons params bound))])
              (dep-free-vars-with new-bound body)))]
   
   [else
    (apply append (map (lambda (sub) (dep-free-vars-with bound sub)) (cdr t)))]))

;;; dep-free-vars-in-bindings-and-body : (List Symbol) × (List Binding) × Type → (List Symbol)
;;; Helper for Pi types: each binding's var is bound in subsequent bindings and body.
(define (dep-free-vars-in-bindings-and-body bound bindings body)
  (if (null? bindings)
      (dep-free-vars-with bound body)
      (let* ([b (car bindings)]
             [bvar (binding-var b)]
             [btype (binding-type b)]
             [domain-vars (dep-free-vars-with bound btype)]
             [new-bound (cons bvar bound)]
             [rest-vars (dep-free-vars-in-bindings-and-body new-bound (cdr bindings) body)])
            (append domain-vars rest-vars))))

;;; dep-subst-type : Type × Symbol × Type → Type
;;; Substitute var with replacement in type, with capture-avoiding substitution.
;;; When substituting into a binder, if the bound variable would capture free
;;; variables in the replacement, we alpha-rename the binder first.
(define (dep-subst-type type var replacement)
  (cond
   [(eq? type var) replacement]
   [(or (base-type? type) (hole? type) (universe-type? type)) type]
   [(symbol? type) type]  ; Other symbols (not the var we're substituting)
   [(not (pair? type)) type]
   
   ;; Pi type: substitute in bindings and body with capture-avoidance
   [(eq? (car type) 'Π)
    (let* ([bindings (cadr type)]
           [body (caddr type)]
           [replacement-fv (dep-free-vars replacement)])
          (dep-subst-pi-bindings bindings body var replacement replacement-fv))]
   
   ;; Sigma type: substitute with capture-avoidance
   [(eq? (car type) 'Σ)
    (let* ([binding (car (cadr type))]
           [body (caddr type)]
           [bound-var (binding-var binding)]
           [domain (binding-type binding)]
           [replacement-fv (dep-free-vars replacement)])
          (cond
           ;; var is shadowed - substitute only in domain
           [(eq? var bound-var)
            `(Σ ((,bound-var : ,(dep-subst-type domain var replacement)))
              ,body)]
           ;; bound-var would capture - alpha-rename first
           [(memq bound-var replacement-fv)
            (let* ([all-vars (append replacement-fv (dep-free-vars body))]
                   [fresh (fresh-var bound-var all-vars)]
                   [renamed-body (dep-subst-type body bound-var fresh)]
                   [new-domain (dep-subst-type domain var replacement)]
                   [new-body (dep-subst-type renamed-body var replacement)])
                  `(Σ ((,fresh : ,new-domain)) ,new-body))]
           ;; Safe to substitute
           [else
            `(Σ ((,bound-var : ,(dep-subst-type domain var replacement)))
              ,(dep-subst-type body var replacement))]))]
   
   ;; Forall - handle binding with capture-avoidance
   [(eq? (car type) '∀)
    (let ([bound-vars (cadr type)]
          [body (caddr type)])
         (if (memq var bound-vars)
             type  ; var is shadowed
             (let ([replacement-fv (dep-free-vars replacement)])
                  (if (ormap (lambda (bv) (memq bv replacement-fv)) bound-vars)
                      ;; Would capture - need to rename
                      (let* ([all-vars (append replacement-fv (dep-free-vars body))]
                             [renamed-vars-body
                              (fold-left
                               (lambda (acc bv)
                                       (let ([vars (car acc)]
                                             [b (cdr acc)])
                                            (if (memq bv replacement-fv)
                                                (let ([fresh (fresh-var bv all-vars)])
                                                     (cons (cons fresh vars)
                                                           (dep-subst-type b bv fresh)))
                                                (cons (cons bv vars) b))))
                               (cons '() body)
                               bound-vars)]
                             [new-vars (reverse (car renamed-vars-body))]
                             [renamed-body (cdr renamed-vars-body)])
                            `(∀ ,new-vars ,(dep-subst-type renamed-body var replacement)))
                      ;; Safe to substitute
                      `(∀ ,bound-vars ,(dep-subst-type body var replacement))))))]
   
   ;; Mu - handle binding with capture-avoidance
   [(eq? (car type) 'μ)
    (let ([bound-var (cadr type)]
          [body (caddr type)])
         (cond
          [(eq? var bound-var) type]  ; var is shadowed
          [(memq bound-var (dep-free-vars replacement))
           ;; Would capture - rename
           (let* ([all-vars (append (dep-free-vars replacement) (dep-free-vars body))]
                  [fresh (fresh-var bound-var all-vars)]
                  [renamed-body (dep-subst-type body bound-var fresh)])
                 `(μ ,fresh ,(dep-subst-type renamed-body var replacement)))]
          [else
           `(μ ,bound-var ,(dep-subst-type body var replacement))]))]
   
   ;; Default recursive case
   [else
    (cons (car type)
          (map (lambda (sub) (dep-subst-type sub var replacement)) (cdr type)))]))

;;; dep-subst-pi-bindings : (List Binding) × Type × Symbol × Type × (List Symbol) → Type
;;; Substitute through Pi type bindings with proper capture-avoidance.
;;; Each binding's var becomes bound for subsequent bindings and the body.
(define (dep-subst-pi-bindings bindings body var replacement replacement-fv)
  (if (null? bindings)
      ;; No more bindings - just substitute in body
      `(Π () ,(dep-subst-type body var replacement))
      (let* ([b (car bindings)]
             [bvar (binding-var b)]
             [btype (binding-type b)]
             [rest (cdr bindings)])
            (cond
             ;; var is shadowed by this binding
             [(eq? var bvar)
              ;; Substitute only in this binding's type, leave rest alone
              `(Π ,(cons (list bvar ': (dep-subst-type btype var replacement))
                         rest)
                ,body)]
             ;; bvar would capture free vars in replacement - alpha-rename
             [(memq bvar replacement-fv)
              (let* ([all-vars (append replacement-fv
                                       (dep-free-vars body)
                                       (apply append (map (lambda (b) (dep-free-vars (binding-type b))) rest)))]
                     [fresh (fresh-var bvar all-vars)]
                     ;; Rename bvar to fresh in rest of bindings and body
                     [renamed-rest (map (lambda (rb)
                                                (list (binding-var rb) ':
                                                      (dep-subst-type (binding-type rb) bvar fresh)))
                                        rest)]
                     [renamed-body (dep-subst-type body bvar fresh)]
                     ;; Now substitute in this binding's type
                     [new-btype (dep-subst-type btype var replacement)]
                     ;; Recursively process rest with fresh var in replacement-fv
                     [new-replacement-fv (cons fresh (remove bvar replacement-fv))]
                     [result (dep-subst-pi-bindings renamed-rest renamed-body var replacement new-replacement-fv)])
                    ;; Prepend this binding to result
                    `(Π ,(cons (list fresh ': new-btype) (cadr result))
                      ,(caddr result)))]
             ;; Safe to substitute
             [else
              (let* ([new-btype (dep-subst-type btype var replacement)]
                     [result (dep-subst-pi-bindings rest body var replacement replacement-fv)])
                    `(Π ,(cons (list bvar ': new-btype) (cadr result))
                      ,(caddr result)))]))))

;;; remove : α × (List α) → (List α)
;;; Remove first occurrence of element from list.
(define (remove x lst)
  (cond
   [(null? lst) '()]
   [(equal? x (car lst)) (cdr lst)]
   [else (cons (car lst) (remove x (cdr lst)))]))

;;; ====
;;; Type Display with Dependent Types
;;; ====

;;; dep-type->string : Type → String
;;; Pretty-print a type, including dependent types.
(define (dep-type->string t)
  (cond
   [(symbol? t) (symbol->string t)]
   [(eq? t '?) "?"]
   [(and (pair? t) (eq? (car t) '?))
    (string-append "?" (symbol->string (cadr t)))]
   
   ;; Pi type: Π(x : A). B
   [(pi-type? t)
    (let ([bindings (cadr t)]
          [body (caddr t)])
         (string-append "Π"
                        (join-strings ""
                                      (map (lambda (b)
                                                   (string-append "(" (symbol->string (binding-var b))
                                                                  " : " (dep-type->string (binding-type b)) ")"))
                                           bindings))
                        ". "
                        (dep-type->string body)))]
   
   ;; Sigma type: Σ(x : A). B
   [(sigma-type? t)
    (let ([binding (car (cadr t))]
          [body (caddr t)])
         (string-append "Σ(" (symbol->string (binding-var binding))
                        " : " (dep-type->string (binding-type binding))
                        "). " (dep-type->string body)))]
   
   ;; Universe
   [(universe-type? t)
    (if (eq? t 'Type)
        "Type"
        (string-append "Type" (number->string (universe-level t))))]
   
   ;; Vec n A
   [(vec-type? t)
    (string-append "Vec(" (format "~a" (cadr t)) ", " (dep-type->string (caddr t)) ")")]
   
   ;; Matrix m n A
   [(matrix-type? t)
    (string-append "Matrix(" (format "~a" (cadr t)) "x" (format "~a" (caddr t)) ", " (dep-type->string (cadddr t)) ")")]
   
   ;; Equality type (= A x y)
   [(equality-type? t)
    (string-append (format "~a" (equality-lhs t))
                   " ≡ "
                   (format "~a" (equality-rhs t))
                   " : "
                   (dep-type->string (equality-carrier t)))]
   
   ;; Refinement type (refine ((x : T)) P)
   [(refinement-type? t)
    (let ([var (refinement-var t)]
          [base (refinement-base-type t)]
          [pred (refinement-predicate t)])
         (string-append "{"
                        (symbol->string var)
                        " : "
                        (dep-type->string base)
                        " | "
                        (format "~a" pred)
                        "}"))]
   
   ;; Diff type (Diff A B)
   [(diff-type? t)
    (string-append "Diff("
                   (dep-type->string (diff-domain t))
                   " → "
                   (dep-type->string (diff-codomain t))
                   ")")]
   
   ;; Grad type (Grad T)
   [(grad-type? t)
    (string-append "∇(" (dep-type->string (grad-inner-type t)) ")")]
   
   ;; Inductive type (data (Name params...) [ctor : type] ...)
   [(data-type? t)
    (let ([name (data-name t)]
          [params (data-params t)]
          [ctors (data-constructors t)])
         (string-append "data "
                        (symbol->string name)
                        (if (null? params)
                            ""
                            (string-append "("
                                           (join-strings ", "
                                                         (map (lambda (p)
                                                                      (if (typed-binding? p)
                                                                          (string-append (symbol->string (binding-var p))
                                                                                         " : "
                                                                                         (dep-type->string (binding-type p)))
                                                                          (symbol->string p)))
                                                              params))
                                           ")"))
                        " { "
                        (join-strings " | "
                                      (map (lambda (c)
                                                   (string-append (symbol->string (car c))
                                                                  " : "
                                                                  (dep-type->string (cdr c))))
                                           ctors))
                        " }"))]
   
   ;; Module signature (sig Name decl ...)
   [(sig-type? t)
    (let ([name (sig-name t)]
          [decls (sig-decls t)])
         (string-append "sig "
                        (symbol->string name)
                        " { "
                        (join-strings "; "
                                      (map (lambda (d)
                                                   (cond
                                                    [(sig-type-decl-abstract? d)
                                                     (string-append "type " (symbol->string (sig-type-decl-name d))
                                                                    " : " (dep-type->string (sig-type-decl-kind d)))]
                                                    [(sig-type-decl-alias? d)
                                                     (string-append "type " (symbol->string (sig-type-decl-name d))
                                                                    " = " (dep-type->string (sig-type-decl-def d)))]
                                                    [(sig-val-decl? d)
                                                     (string-append "val " (symbol->string (sig-val-decl-name d))
                                                                    " : " (dep-type->string (sig-val-decl-type d)))]
                                                    [(sig-data-decl? d)
                                                     (string-append "data " (dep-type->string (sig-data-decl-data d)))]
                                                    [(sig-include-decl? d)
                                                     (string-append "include " (symbol->string (sig-include-decl-name d)))]
                                                    [else (format "~s" d)]))
                                           decls))
                        " }"))]
   
   ;; Arrow type
   [(and (pair? t) (eq? (car t) '->))
    (string-append "("
                   (join-strings " → " (map dep-type->string (cdr t)))
                   ")")]
   
   ;; Product type
   [(and (pair? t) (eq? (car t) '×))
    (string-append "("
                   (join-strings " × " (map dep-type->string (cdr t)))
                   ")")]
   
   ;; Other compound types
   [(pair? t)
    (string-append "("
                   (join-strings " " (map dep-type->string t))
                   ")")]
   
   [else (format "~s" t)]))

;;; ====
;;; Type Wellformedness with Dependent Types
;;; ====

;;; dep-type? predicate (extended version of type?)
;;; Note: This is a simple syntax check, not a kinding check.
(define (well-formed-dep-type? t)
  (cond
   [(base-type? t) #t]
   [(eq? t '?) #t]
   [(eq? t 'Type) #t]
   [(and (pair? t) (eq? (car t) '?)) #t]
   [(type-var? t) #t]
   [(not (pair? t)) #f]
   
   [(eq? (car t) 'Π) (pi-type-well-formed? t)]
   [(eq? (car t) 'Σ) (sigma-type-well-formed? t)]
   [(eq? (car t) 'Vec) (and (= (length t) 3) (well-formed-dep-type? (caddr t)))]
   [(eq? (car t) 'Matrix) (and (= (length t) 4) (well-formed-dep-type? (cadddr t)))]
   [(eq? (car t) 'Type) (and (= (length t) 2) (integer? (cadr t)) (>= (cadr t) 0))]
   [(eq? (car t) '=) (equality-type-well-formed? t)]
   [(eq? (car t) 'refine) (refinement-type-well-formed? t)]
   [(eq? (car t) 'data) (data-type-well-formed? t)]
   [(eq? (car t) 'sig) (sig-well-formed? t)]
   [(eq? (car t) 'Diff) (diff-type-well-formed? t)]
   [(eq? (car t) 'Grad) (grad-type-well-formed? t)]
   
   ;; Delegate to base type? for other forms
   [else (type? t)]))
