;;; lattice/fp/data/generic-zipper.ss — Generic Zipper Derivation
;;;
;;; Derives zippers automatically from algebraic data type definitions
;;; using the calculus of types. The derivative of a type T with respect
;;; to a type variable a gives the type of "one-hole contexts" - structures
;;; with exactly one hole where an `a` used to be.
;;;
;;; Key insight (McBride, Huet):
;;;   Zipper T = T' × T
;;;   where T' is the derivative of T (the context type)
;;;
;;; This module provides:
;;;   - Type DSL for representing algebraic data types
;;;   - Symbolic type differentiation
;;;   - Generic zipper construction
;;;   - Navigation and modification operations
;;;
;;; References:
;;;   - McBride, "The Derivative of a Regular Type is its Type of One-Hole Contexts"
;;;   - Huet, "The Zipper" (1997)
;;;   - Abbott, Altenkirch, Ghani, "Derivatives of Containers"
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - combinators.ss

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

;;; ====
;;; Type Expression DSL
;;; ====
;;;
;;; We represent types as symbolic S-expressions that can be
;;; differentiated. This is a "shallow embedding" of types.
;;;
;;; Type grammar:
;;;   T ::= 0           ; empty type (void)
;;;       | 1           ; unit type
;;;       | (var a)     ; type variable
;;;       | (+ T U ...)  ; sum type (tagged union)
;;;       | (* T U ...)  ; product type (tuple)
;;;       | (mu X T)    ; recursive type (μX. T)
;;;       | (list T)    ; list of T (sugar for μL. 1 + T×L)
;;;       | (maybe T)   ; optional T (sugar for 1 + T)
;;;       | (tree T)    ; rose tree (sugar for μR. T × (list R))

;;; ====
;;; Type Constructors
;;; ====

;;; type-zero : Type
;;; The empty/void type. Has no inhabitants.
(define type-zero 0)

;;; type-one : Type
;;; The unit type. Has exactly one inhabitant.
(define type-one 1)

;;; type-zero? : Type → Bool
(define (type-zero? t) (equal? t 0))

;;; type-one? : Type → Bool
(define (type-one? t) (equal? t 1))

;;; type-var : Symbol → Type
;;; A type variable.
(define (type-var name)
  (list 'var name))

;;; type-var? : Type → Bool
(define (type-var? t)
  (and (pair? t) (eq? (car t) 'var)))

;;; type-var-name : Type → Symbol
(define (type-var-name t)
  (cadr t))

;;; type-sum : Type × Type × ... → Type
;;; Sum type (tagged union). Represents A + B + C + ...
(define (type-sum . types)
  (cond
    [(null? types) type-zero]
    [(null? (cdr types)) (car types)]
    [else (cons '+ types)]))

;;; type-sum? : Type → Bool
(define (type-sum? t)
  (and (pair? t) (eq? (car t) '+)))

;;; type-sum-components : Type → (List Type)
(define (type-sum-components t)
  (if (type-sum? t) (cdr t) (list t)))

;;; type-prod : Type × Type × ... → Type
;;; Product type (tuple). Represents A × B × C × ...
(define (type-prod . types)
  (cond
    [(null? types) type-one]
    [(null? (cdr types)) (car types)]
    [else (cons '* types)]))

;;; type-prod? : Type → Bool
(define (type-prod? t)
  (and (pair? t) (eq? (car t) '*)))

;;; type-prod-components : Type → (List Type)
(define (type-prod-components t)
  (if (type-prod? t) (cdr t) (list t)))

;;; type-rec : Symbol × Type → Type
;;; Recursive type μX. T where X may appear in T.
(define (type-rec var body)
  (list 'mu var body))

;;; type-rec? : Type → Bool
(define (type-rec? t)
  (and (pair? t) (eq? (car t) 'mu)))

;;; type-rec-var : Type → Symbol
(define (type-rec-var t)
  (cadr t))

;;; type-rec-body : Type → Type
(define (type-rec-body t)
  (caddr t))

;;; ====
;;; Common Type Patterns (Sugar)
;;; ====

;;; type-list : Type → Type
;;; List a = μL. 1 + a × L
(define (type-list elem-type)
  (type-rec 'L
    (type-sum type-one
              (type-prod elem-type (type-var 'L)))))

;;; type-maybe : Type → Type
;;; Maybe a = 1 + a
(define (type-maybe elem-type)
  (type-sum type-one elem-type))

;;; type-pair : Type × Type → Type
(define (type-pair a b)
  (type-prod a b))

;;; type-either : Type × Type → Type
(define (type-either a b)
  (type-sum a b))

;;; type-rose-tree : Type → Type
;;; Rose tree (n-ary tree): Tree a = μT. a × List(T)
(define (type-rose-tree elem-type)
  (type-rec 'T
    (type-prod elem-type (type-list (type-var 'T)))))

;;; type-binary-tree : Type → Type
;;; Binary tree: BTree a = μT. 1 + a × T × T
(define (type-binary-tree elem-type)
  (type-rec 'T
    (type-sum type-one
              (type-prod elem-type (type-var 'T) (type-var 'T)))))

;;; ====
;;; Type Derivative (Core Algorithm)
;;; ====
;;;
;;; The derivative of a type T with respect to variable a gives
;;; the type of one-hole contexts where an `a` was removed.
;;;
;;; Differentiation rules:
;;;   D[0]/a = 0           (void has no holes)
;;;   D[1]/a = 0           (unit has no holes)
;;;   D[a]/a = 1           (variable has exactly one hole)
;;;   D[b]/a = 0 (b≠a)     (other vars have no holes)
;;;   D[T+U]/a = DT/a + DU/a   (sum rule)
;;;   D[T×U]/a = DT/a × U + T × DU/a   (product/Leibniz rule)
;;;   D[μX.F]/a = ...      (recursive types are more complex)

;;; type-deriv : Type × Symbol → Type
;;; Compute the derivative of type t with respect to variable v.
(define (type-deriv t v)
  (cond
    ;; Constants have no holes
    [(type-zero? t) type-zero]
    [(type-one? t) type-zero]

    ;; Variable rule
    [(type-var? t)
     (if (eq? (type-var-name t) v)
         type-one          ; d(a)/da = 1
         type-zero)]       ; d(b)/da = 0

    ;; Sum rule: d(A + B)/da = dA/da + dB/da
    [(type-sum? t)
     (apply type-sum
            (map (lambda (comp) (type-deriv comp v))
                 (type-sum-components t)))]

    ;; Product rule (Leibniz): d(A × B)/da = dA/da × B + A × dB/da
    [(type-prod? t)
     (let ([components (type-prod-components t)])
       (type-deriv-prod components v))]

    ;; Recursive type: d(μX.F)/da
    ;; This creates a "stack" of contexts leading to the hole
    [(type-rec? t)
     (let* ([rec-var (type-rec-var t)]
            [body (type-rec-body t)]
            [dF/da (type-deriv body v)]
            [dF/dX (type-deriv body rec-var)])
       ;; The derivative of μX.F(X,a) with respect to a is:
       ;; List(dF/dX[μX.F/X]) × dF/da[μX.F/X]
       ;; This represents: a path of contexts down to the hole
       (type-prod (type-list (type-subst dF/dX rec-var t))
                  (type-subst dF/da rec-var t)))]

    ;; Unknown type form
    [else type-zero]))

;;; type-deriv-prod : (List Type) × Symbol → Type
;;; Product rule for n-ary products.
;;; d(T1 × T2 × ... × Tn)/da =
;;;   dT1/da × T2 × ... × Tn
;;;   + T1 × dT2/da × T3 × ... × Tn
;;;   + ...
;;;   + T1 × T2 × ... × dTn/da
(define (type-deriv-prod components v)
  (let ([n (length components)])
    (apply type-sum
           (map (lambda (i)
                  (apply type-prod
                         (map (lambda (j comp)
                                (if (= i j)
                                    (type-deriv comp v)
                                    comp))
                              (iota n)
                              components)))
                (iota n)))))

;;; type-subst : Type × Symbol × Type → Type
;;; Substitute type `replacement` for variable `v` in type `t`.
(define (type-subst t v replacement)
  (cond
    [(type-zero? t) type-zero]
    [(type-one? t) type-one]

    [(type-var? t)
     (if (eq? (type-var-name t) v)
         replacement
         t)]

    [(type-sum? t)
     (apply type-sum
            (map (lambda (c) (type-subst c v replacement))
                 (type-sum-components t)))]

    [(type-prod? t)
     (apply type-prod
            (map (lambda (c) (type-subst c v replacement))
                 (type-prod-components t)))]

    [(type-rec? t)
     (let ([rec-var (type-rec-var t)]
            [body (type-rec-body t)])
       ;; Don't substitute if shadowed by the binding
       (if (eq? rec-var v)
           t
           (type-rec rec-var (type-subst body v replacement))))]

    [else t]))

;;; ====
;;; Type Simplification
;;; ====

;;; type-simplify : Type → Type
;;; Simplify type expressions by eliminating 0s and 1s.
(define (type-simplify t)
  (cond
    [(or (type-zero? t) (type-one? t)) t]
    [(type-var? t) t]

    [(type-sum? t)
     (let* ([simplified (map type-simplify (type-sum-components t))]
            [non-zero (filter (lambda (c) (not (type-zero? c))) simplified)])
       (cond
         [(null? non-zero) type-zero]
         [(null? (cdr non-zero)) (car non-zero)]
         [else (apply type-sum non-zero)]))]

    [(type-prod? t)
     (let* ([simplified (map type-simplify (type-prod-components t))]
            [has-zero (any type-zero? simplified)]
            [non-one (filter (lambda (c) (not (type-one? c))) simplified)])
       (cond
         [has-zero type-zero]  ; 0 × T = 0
         [(null? non-one) type-one]
         [(null? (cdr non-one)) (car non-one)]
         [else (apply type-prod non-one)]))]

    [(type-rec? t)
     (type-rec (type-rec-var t)
               (type-simplify (type-rec-body t)))]

    [else t]))

;;; any : (α → Bool) × (List α) → Bool
(define (any pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

;;; ====
;;; Zipper Type Construction
;;; ====

;;; make-zipper-type : Type × Symbol → Type
;;; Construct the zipper type for type T focused on variable v.
;;; Zipper T = T' × T where T' is the derivative (context).
(define (make-zipper-type t v)
  (type-prod (type-simplify (type-deriv t v)) t))

;;; context-type : Type × Symbol → Type
;;; Get just the context type (derivative) for a type.
(define (context-type t v)
  (type-simplify (type-deriv t v)))

;;; ====
;;; Generic Zipper Runtime Structure
;;; ====
;;;
;;; A generic zipper at runtime is represented as:
;;;   (generic-zipper focus contexts)
;;; where:
;;;   - focus: the focused element
;;;   - contexts: list of context frames (path from root to focus)

(define generic-zipper-tag 'generic-zipper)

;;; make-generic-zipper : α × (List Context) → (GenericZipper α)
(define (make-generic-zipper focus contexts)
  (list generic-zipper-tag focus contexts))

;;; generic-zipper? : α → Bool
(define (generic-zipper? z)
  (and (pair? z)
       (eq? (car z) generic-zipper-tag)
       (= (length z) 3)))

;;; generic-zipper-focus : (GenericZipper α) → α
(define (generic-zipper-focus z)
  (if (generic-zipper? z)
      (list-ref z 1)
      (error 'generic-zipper-focus "not a generic zipper")))

;;; generic-zipper-contexts : (GenericZipper α) → (List Context)
(define (generic-zipper-contexts z)
  (if (generic-zipper? z)
      (list-ref z 2)
      (error 'generic-zipper-contexts "not a generic zipper")))

;;; ====
;;; Context Frame Structure
;;; ====
;;;
;;; A context frame represents one "layer" of the one-hole context.
;;; For a product type A × B × C with hole in B:
;;;   (context-frame 'prod 1 (a-value #f c-value))
;;;
;;; For a sum type A + B + C where B has the hole:
;;;   (context-frame 'sum 1 ())

(define context-frame-tag 'context-frame)

;;; make-context-frame : Symbol × Nat × (List Val) → ContextFrame
;;; - kind: 'prod or 'sum
;;; - position: which component has the hole (0-indexed)
;;; - siblings: values of non-hole components (for products)
(define (make-context-frame kind position siblings)
  (list context-frame-tag kind position siblings))

;;; context-frame? : α → Bool
(define (context-frame? f)
  (and (pair? f)
       (eq? (car f) context-frame-tag)))

;;; context-frame-kind : ContextFrame → Symbol
(define (context-frame-kind f) (list-ref f 1))

;;; context-frame-position : ContextFrame → Nat
(define (context-frame-position f) (list-ref f 2))

;;; context-frame-siblings : ContextFrame → (List Val)
(define (context-frame-siblings f) (list-ref f 3))

;;; ====
;;; Generic Zipper Operations
;;; ====

;;; generic-zipper-at-root? : (GenericZipper α) → Bool
(define (generic-zipper-at-root? z)
  (null? (generic-zipper-contexts z)))

;;; generic-zipper-depth : (GenericZipper α) → Nat
(define (generic-zipper-depth z)
  (length (generic-zipper-contexts z)))

;;; generic-zipper-set : (GenericZipper α) × α → (GenericZipper α)
;;; Replace the focused value.
(define (generic-zipper-set z new-focus)
  (make-generic-zipper new-focus (generic-zipper-contexts z)))

;;; generic-zipper-modify : (GenericZipper α) × (α → α) → (GenericZipper α)
;;; Apply a function to the focused value.
(define (generic-zipper-modify z f)
  (make-generic-zipper (f (generic-zipper-focus z))
                       (generic-zipper-contexts z)))

;;; ====
;;; Type Display
;;; ====

;;; type->string : Type → String
;;; Pretty-print a type expression.
(define (type->string t)
  (cond
    [(type-zero? t) "0"]
    [(type-one? t) "1"]
    [(type-var? t) (symbol->string (type-var-name t))]

    [(type-sum? t)
     (let ([parts (map type->string (type-sum-components t))])
       (string-append "(" (string-join parts " + ") ")"))]

    [(type-prod? t)
     (let ([parts (map type->string (type-prod-components t))])
       (string-append "(" (string-join parts " × ") ")"))]

    [(type-rec? t)
     (string-append "μ" (symbol->string (type-rec-var t))
                    "." (type->string (type-rec-body t)))]

    [else "?"]))

;;; string-join : (List String) × String → String
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s)
                   (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; ====
;;; Derived Zipper Examples
;;; ====

;;; Example: List derivative
;;; List a = μL. 1 + a × L
;;; d(List a)/da = List(L) × (1 + L × L)  -- simplified to List × List
;;; This matches the (left, right) structure of list zipper!

;;; Example: Binary tree derivative
;;; BTree a = μT. 1 + a × T × T
;;; d(BTree a)/da = List(T × T + T × T) × (1 + T × T)
;;; The context has: a path of (left child, right child) pairs

;;; list-context-type : Type
;;; The context type for List a, derived automatically.
(define list-context-type
  (context-type (type-list (type-var 'a)) 'a))

;;; binary-tree-context-type : Type
;;; The context type for BTree a, derived automatically.
(define binary-tree-context-type
  (context-type (type-binary-tree (type-var 'a)) 'a))

;;; ====
;;; iota helper (if not available)
;;; ====

(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))
