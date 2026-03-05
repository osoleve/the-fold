;;; lattice/fp/data/generic-zipper.ss — Generic Zipper Derivation
;;; @module generic-zipper
;;; @requires prelude
;;; @purity mixed
;;; @stability stable

(require 'prelude)

(doc 'module 'generic-zipper)
(doc 'description "Generic Zipper Derivation

Derives zippers automatically from algebraic data type definitions
using the calculus of types. The derivative of a type T with respect
to a type variable a gives the type of one-hole contexts - structures
with exactly one hole where an `a` used to be.

Key insight (McBride, Huet):
  Zipper T = T' × T
  where T' is the derivative of T (the context type)

This module provides:
  - Type DSL for representing algebraic data types
  - Symbolic type differentiation
  - Generic zipper construction
  - Navigation and modification operations

References:
  - McBride, 'The Derivative of a Regular Type is its Type of One-Hole Contexts'
  - Huet, 'The Zipper' (1997)
  - Abbott, Altenkirch, Ghani, 'Derivatives of Containers'")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'type-expression-dsl)
(doc 'note "We represent types as symbolic S-expressions that can be
differentiated. This is a 'shallow embedding' of types.

Type grammar:
  T ::= 0           ; empty type (void)
      | 1           ; unit type
      | (var a)     ; type variable
      | (+ T U ...)  ; sum type (tagged union)
      | (* T U ...)  ; product type (tuple)
      | (mu X T)    ; recursive type (μX. T)
      | (list T)    ; list of T (sugar for μL. 1 + T×L)
      | (maybe T)   ; optional T (sugar for 1 + T)
      | (tree T)    ; rose tree (sugar for μR. T × (list R))")

(doc 'section 'type-constructors)

(define type-zero 0)
(doc type-zero 'export #t)
(doc type-zero 'type 'Type)
(doc type-zero 'description "The empty/void type. Has no inhabitants.")

(define type-one 1)
(doc type-one 'export #t)
(doc type-one 'type 'Type)
(doc type-one 'description "The unit type. Has exactly one inhabitant.")

(define (type-zero? t)
  (doc 'export #t)
  (doc 'type '(-> Type Bool))
  (equal? t 0))

(define (type-one? t)
  (doc 'export #t)
  (doc 'type '(-> Type Bool))
  (equal? t 1))

(define (type-var name)
  (doc 'export #t)
  (doc 'type '(-> Symbol Type))
  (doc 'description "A type variable.")
  (list 'var name))

(define (type-var? t)
  (doc 'export #t)
  (doc 'type '(-> Type Bool))
  (and (pair? t) (eq? (car t) 'var)))

(define (type-var-name t)
  (doc 'export #t)
  (doc 'type '(-> Type Symbol))
  (cadr t))

(define (type-sum . types)
  (doc 'export #t)
  (doc 'type '(-> Type ... Type))
  (doc 'description "Sum type (tagged union). Represents A + B + C + ...")
  (cond
    [(null? types) type-zero]
    [(null? (cdr types)) (car types)]
    [else (cons '+ types)]))

(define (type-sum? t)
  (doc 'export #t)
  (doc 'type '(-> Type Bool))
  (and (pair? t) (eq? (car t) '+)))

(define (type-sum-components t)
  (doc 'export #t)
  (doc 'type '(-> Type (List Type)))
  (if (type-sum? t) (cdr t) (list t)))

(define (type-prod . types)
  (doc 'export #t)
  (doc 'type '(-> Type ... Type))
  (doc 'description "Product type (tuple). Represents A × B × C × ...")
  (cond
    [(null? types) type-one]
    [(null? (cdr types)) (car types)]
    [else (cons '* types)]))

(define (type-prod? t)
  (doc 'export #t)
  (doc 'type '(-> Type Bool))
  (and (pair? t) (eq? (car t) '*)))

(define (type-prod-components t)
  (doc 'export #t)
  (doc 'type '(-> Type (List Type)))
  (if (type-prod? t) (cdr t) (list t)))

(define (type-rec var body)
  (doc 'export #t)
  (doc 'type '(-> Symbol Type Type))
  (doc 'description "Recursive type μX. T where X may appear in T.")
  (list 'mu var body))

(define (type-rec? t)
  (doc 'export #t)
  (doc 'type '(-> Type Bool))
  (and (pair? t) (eq? (car t) 'mu)))

(define (type-rec-var t)
  (doc 'export #t)
  (doc 'type '(-> Type Symbol))
  (cadr t))

(define (type-rec-body t)
  (doc 'export #t)
  (doc 'type '(-> Type Type))
  (caddr t))

(doc 'section 'common-type-patterns)

(define (type-list elem-type)
  (doc 'export #t)
  (doc 'type '(-> Type Type))
  (doc 'description "List a = μL. 1 + a × L")
  (type-rec 'L
    (type-sum type-one
              (type-prod elem-type (type-var 'L)))))

(define (type-maybe elem-type)
  (doc 'export #t)
  (doc 'type '(-> Type Type))
  (doc 'description "Maybe a = 1 + a")
  (type-sum type-one elem-type))

(define (type-pair a b)
  (doc 'export #t)
  (doc 'type '(-> Type Type Type))
  (type-prod a b))

(define (type-either a b)
  (doc 'export #t)
  (doc 'type '(-> Type Type Type))
  (type-sum a b))

(define (type-rose-tree elem-type)
  (doc 'export #t)
  (doc 'type '(-> Type Type))
  (doc 'description "Rose tree (n-ary tree): Tree a = μT. a × List(T)")
  (type-rec 'T
    (type-prod elem-type (type-list (type-var 'T)))))

(define (type-binary-tree elem-type)
  (doc 'export #t)
  (doc 'type '(-> Type Type))
  (doc 'description "Binary tree: BTree a = μT. 1 + a × T × T")
  (type-rec 'T
    (type-sum type-one
              (type-prod elem-type (type-var 'T) (type-var 'T)))))

(doc 'section 'type-derivative)
(doc 'note "The derivative of a type T with respect to variable a gives
the type of one-hole contexts where an `a` was removed.

Differentiation rules:
  D[0]/a = 0           (void has no holes)
  D[1]/a = 0           (unit has no holes)
  D[a]/a = 1           (variable has exactly one hole)
  D[b]/a = 0 (b≠a)     (other vars have no holes)
  D[T+U]/a = DT/a + DU/a   (sum rule)
  D[T×U]/a = DT/a × U + T × DU/a   (product/Leibniz rule)
  D[μX.F]/a = ...      (recursive types are more complex)")

(define (type-deriv t v)
  (doc 'export #t)
  (doc 'type '(-> Type Symbol Type))
  (doc 'description "Compute the derivative of type t with respect to variable v.")
  (cond
    [(type-zero? t) type-zero]
    [(type-one? t) type-zero]

    [(type-var? t)
     (if (eq? (type-var-name t) v)
         type-one
         type-zero)]

    [(type-sum? t)
     (apply type-sum
            (map (lambda (comp) (type-deriv comp v))
                 (type-sum-components t)))]

    [(type-prod? t)
     (let ([components (type-prod-components t)])
       (type-deriv-prod components v))]

    [(type-rec? t)
     (let* ([rec-var (type-rec-var t)]
            [body (type-rec-body t)]
            [dF/da (type-deriv body v)]
            [dF/dX (type-deriv body rec-var)])
       (type-prod (type-list (type-subst dF/dX rec-var t))
                  (type-subst dF/da rec-var t)))]

    [else type-zero]))

(define (type-deriv-prod components v)
  (doc 'export #t)
  (doc 'type '(-> (List Type) Symbol Type))
  (doc 'description "Product rule for n-ary products.
d(T1 × T2 × ... × Tn)/da =
  dT1/da × T2 × ... × Tn
  + T1 × dT2/da × T3 × ... × Tn
  + ...
  + T1 × T2 × ... × dTn/da")
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

(define (type-subst t v replacement)
  (doc 'export #t)
  (doc 'type '(-> Type Symbol Type Type))
  (doc 'description "Substitute type `replacement` for variable `v` in type `t`.")
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
       (if (eq? rec-var v)
           t
           (type-rec rec-var (type-subst body v replacement))))]

    [else t]))

(doc 'section 'type-simplification)

(define (type-simplify t)
  (doc 'export #t)
  (doc 'type '(-> Type Type))
  (doc 'description "Simplify type expressions by eliminating 0s and 1s.")
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
         [has-zero type-zero]
         [(null? non-one) type-one]
         [(null? (cdr non-one)) (car non-one)]
         [else (apply type-prod non-one)]))]

    [(type-rec? t)
     (type-rec (type-rec-var t)
               (type-simplify (type-rec-body t)))]

    [else t]))

(define (any pred lst)
  (doc 'export #t)
  (doc 'type '(-> (-> α Bool) (List α) Bool))
  (cond
    [(null? lst) #f]
    [(pred (car lst)) #t]
    [else (any pred (cdr lst))]))

(doc 'section 'zipper-type-construction)

(define (make-zipper-type t v)
  (doc 'export #t)
  (doc 'type '(-> Type Symbol Type))
  (doc 'description "Construct the zipper type for type T focused on variable v.
Zipper T = T' × T where T' is the derivative (context).")
  (type-prod (type-simplify (type-deriv t v)) t))

(define (context-type t v)
  (doc 'export #t)
  (doc 'type '(-> Type Symbol Type))
  (doc 'description "Get just the context type (derivative) for a type.")
  (type-simplify (type-deriv t v)))

(doc 'section 'generic-zipper-runtime)
(doc 'note "A generic zipper at runtime is represented as:
  (generic-zipper focus contexts)
where:
  - focus: the focused element
  - contexts: list of context frames (path from root to focus)")

(define generic-zipper-tag 'generic-zipper)

(define (make-generic-zipper focus contexts)
  (doc 'export #t)
  (doc 'type '(-> α (List Context) (GenericZipper α)))
  (list generic-zipper-tag focus contexts))

(define (generic-zipper? z)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? z)
       (eq? (car z) generic-zipper-tag)
       (= (length z) 3)))

(define (generic-zipper-focus z)
  (doc 'export #t)
  (doc 'type '(-> (GenericZipper α) α))
  (if (generic-zipper? z)
      (list-ref z 1)
      (error 'generic-zipper-focus "not a generic zipper")))

(define (generic-zipper-contexts z)
  (doc 'export #t)
  (doc 'type '(-> (GenericZipper α) (List Context)))
  (if (generic-zipper? z)
      (list-ref z 2)
      (error 'generic-zipper-contexts "not a generic zipper")))

(doc 'section 'context-frame-structure)
(doc 'note "A context frame represents one 'layer' of the one-hole context.
For a product type A × B × C with hole in B:
  (context-frame 'prod 1 (a-value #f c-value))

For a sum type A + B + C where B has the hole:
  (context-frame 'sum 1 ())")

(define context-frame-tag 'context-frame)

(define (make-context-frame kind position siblings)
  (doc 'export #t)
  (doc 'type '(-> Symbol Nat (List Val) ContextFrame))
  (doc 'param 'kind "'prod or 'sum")
  (doc 'param 'position "which component has the hole (0-indexed)")
  (doc 'param 'siblings "values of non-hole components (for products)")
  (list context-frame-tag kind position siblings))

(define (context-frame? f)
  (doc 'export #t)
  (doc 'type '(-> α Bool))
  (and (pair? f)
       (eq? (car f) context-frame-tag)))

(define (context-frame-kind f)
  (doc 'export #t)
  (doc 'type '(-> ContextFrame Symbol))
  (list-ref f 1))

(define (context-frame-position f)
  (doc 'export #t)
  (doc 'type '(-> ContextFrame Nat))
  (list-ref f 2))

(define (context-frame-siblings f)
  (doc 'export #t)
  (doc 'type '(-> ContextFrame (List Val)))
  (list-ref f 3))

(doc 'section 'generic-zipper-operations)

(define (generic-zipper-at-root? z)
  (doc 'export #t)
  (doc 'type '(-> (GenericZipper α) Bool))
  (null? (generic-zipper-contexts z)))

(define (generic-zipper-depth z)
  (doc 'export #t)
  (doc 'type '(-> (GenericZipper α) Nat))
  (length (generic-zipper-contexts z)))

(define (generic-zipper-set z new-focus)
  (doc 'export #t)
  (doc 'type '(-> (GenericZipper α) α (GenericZipper α)))
  (doc 'description "Replace the focused value.")
  (make-generic-zipper new-focus (generic-zipper-contexts z)))

(define (generic-zipper-modify z f)
  (doc 'export #t)
  (doc 'type '(-> (GenericZipper α) (-> α α) (GenericZipper α)))
  (doc 'description "Apply a function to the focused value.")
  (make-generic-zipper (f (generic-zipper-focus z))
                       (generic-zipper-contexts z)))

(doc 'section 'type-display)

(define (type->string t)
  (doc 'export #t)
  (doc 'type '(-> Type String))
  (doc 'description "Pretty-print a type expression.")
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

;; string-join is provided by prelude

(doc 'section 'derived-zipper-examples)
(doc 'note "Example: List derivative
List a = μL. 1 + a × L
d(List a)/da = List(L) × (1 + L × L)  -- simplified to List × List
This matches the (left, right) structure of list zipper!

Example: Binary tree derivative
BTree a = μT. 1 + a × T × T
d(BTree a)/da = List(T × T + T × T) × (1 + T × T)
The context has: a path of (left child, right child) pairs")

(define list-context-type
  (context-type (type-list (type-var 'a)) 'a))
(doc list-context-type 'type 'Type)
(doc list-context-type 'description "The context type for List a, derived automatically.")

(define binary-tree-context-type
  (context-type (type-binary-tree (type-var 'a)) 'a))
(doc binary-tree-context-type 'type 'Type)
(doc binary-tree-context-type 'description "The context type for BTree a, derived automatically.")

(doc 'section 'helpers)

;; iota is provided by prelude
