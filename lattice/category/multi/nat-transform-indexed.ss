;;; @module nat-transform-indexed
;;; @requires functor-general

(require 'functor-general)

(doc 'module 'nat-transform-indexed)
(doc 'description "Indexed Natural Transformations

A natural transformation η : F ⟹ G between functors F, G : C → D
assigns to each object A ∈ Obj(C) a morphism η_A : F(A) → G(A) in D.

STANDARD ENCODING (natural-transform.ss):
  component : ∀A. F(A) → G(A)
  A single polymorphic function - works for endofunctors where
  the component doesn't need to know which object it's at.

INDEXED ENCODING (this module):
  component-at : Obj(C) → (F(A) → G(A))
  A family of functions indexed by objects - necessary when the
  component needs access to the object (e.g., for counit evaluation).

KEY INSIGHT: The counit ε_A : Free(U(A)) → A for Free ⊣ Forgetful
needs to EVALUATE a term using algebra A's operations. The parametric
encoding can't access A; the indexed encoding provides it explicitly.

CURRYING: component-at returns Obj → (Value → Value)
This enables:
  - Partial application: (component-at A) is a function
  - Caching: Can precompute component at frequently-used objects
  - Composition: Natural transformation composition works cleanly")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Indexed Natural Transformation Definition
;;; ====

(doc 'section 'nat-indexed-definition)

;;; make-nat-indexed : Symbol × FunctorGeneral × FunctorGeneral ×
;;;                    (Obj → (Value → Value)) → NatIndexed
;;; Create an indexed natural transformation.
;;;
;;; For η : F ⟹ G where F, G : C → D:
;;;   component-at : Obj(C) → (F(A) → G(A))
;;;   component-at(A) is a morphism in D from F(A) to G(A)
;;;
;;; The function is CURRIED: component-at(A) returns a function,
;;; not component-at(A, x) taking two arguments directly.
(define (make-nat-indexed name source target component-at)
  (doc 'export #t)
  (doc 'type '(-> Symbol FunctorGeneral FunctorGeneral
                  (-> Obj (-> Value Value))
                  NatIndexed))
  (list 'nat-indexed name source target component-at))

(define (nat-indexed? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'nat-indexed)
       (= (length x) 5)))

(define (nat-indexed-name η)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed Symbol))
  (if (nat-indexed? η) (cadr η) 'unknown))

(define (nat-indexed-source η)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed FunctorGeneral))
  (if (nat-indexed? η) (caddr η) #f))

(define (nat-indexed-target η)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed FunctorGeneral))
  (if (nat-indexed? η) (cadddr η) #f))

(define (nat-indexed-component-at η)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed (-> Obj (-> Value Value))))
  (if (nat-indexed? η) (car (cddddr η)) (lambda (_) identity)))

;;; ====
;;; Application
;;; ====

;;; nat-indexed-at : NatIndexed × Obj → (Value → Value)
;;; Get the component at a specific object.
;;; Returns η_A : F(A) → G(A) as a function.
(define (nat-indexed-at η obj)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed Obj (-> Value Value)))
  (doc 'description "Get the component η_A at object A.
Returns a function from F(A) to G(A).")
  ((nat-indexed-component-at η) obj))

;;; nat-indexed-apply : NatIndexed × Obj × Value → Value
;;; Apply the transformation at object A to a value.
;;; η_A(x) for x : F(A)
(define (nat-indexed-apply η obj val)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed Obj Value Value))
  (doc 'description "Apply η_A to a value x : F(A).
Equivalent to ((nat-indexed-at η obj) val).")
  ((nat-indexed-at η obj) val))

;;; ====
;;; Identity Indexed Transformation
;;; ====

;;; nat-indexed-id : FunctorGeneral → NatIndexed
;;; The identity natural transformation id_F : F ⟹ F.
;;; (id_F)_A = id_{F(A)} for all A.
(define (nat-indexed-id F)
  (doc 'export #t)
  (doc 'type '(-> FunctorGeneral NatIndexed))
  (doc 'description "Identity transformation id_F : F ⟹ F.
All components are identity functions.")
  (make-nat-indexed
   'id
   F
   F
   (lambda (obj) identity)))

;;; ====
;;; Vertical Composition
;;; ====

(doc 'section 'vertical-composition)
(doc 'description "Vertical Composition

Given η : F ⟹ G and ε : G ⟹ H, their vertical composition
(ε ∘ η) : F ⟹ H has components:
  (ε ∘ η)_A = ε_A ∘ η_A

This is straightforward: compose the components at each object.")

;;; nat-indexed-compose : NatIndexed × NatIndexed → NatIndexed
;;; Vertical composition: (ε ∘ η)_A = ε_A ∘ η_A
(define (nat-indexed-compose ε η)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed NatIndexed NatIndexed))
  (doc 'description "Vertical composition ε ∘ η : F ⟹ H.
Requires target(η) = source(ε).")
  (make-nat-indexed
   (string->symbol
    (format "~a∘~a" (nat-indexed-name ε) (nat-indexed-name η)))
   (nat-indexed-source η)  ; F
   (nat-indexed-target ε)  ; H
   ;; (ε ∘ η)_A = ε_A ∘ η_A
   (lambda (obj)
     (let ([η-A (nat-indexed-at η obj)]
           [ε-A (nat-indexed-at ε obj)])
       (lambda (x) (ε-A (η-A x)))))))

;;; ====
;;; Horizontal Composition
;;; ====

(doc 'section 'horizontal-composition)
(doc 'description "Horizontal Composition (Godement Product)

Given η : F ⟹ G (functors C → D) and ε : H ⟹ K (functors D → E),
their horizontal composition (ε * η) : H∘F ⟹ K∘G has components:
  (ε * η)_A = K(η_A) ∘ ε_{F(A)} = ε_{G(A)} ∘ H(η_A)

CRITICAL: The component ε is applied at F(A), not A!
This requires threading the source object through F.")

;;; nat-indexed-horizontal : NatIndexed × NatIndexed → NatIndexed
;;; Horizontal composition: (ε * η)_A = K(η_A) ∘ ε_{F(A)}
;;; η : F ⟹ G (C → D), ε : H ⟹ K (D → E)
;;; Result: H∘F ⟹ K∘G (C → E)
(define (nat-indexed-horizontal ε η)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed NatIndexed NatIndexed))
  (doc 'description "Horizontal composition ε * η.
(ε * η)_A = K(η_A) ∘ ε_{F(A)}")
  (let* ([F (nat-indexed-source η)]
         [G (nat-indexed-target η)]
         [H (nat-indexed-source ε)]
         [K (nat-indexed-target ε)]
         ;; Get on-mor for K (to apply K to η_A)
         [K-on-mor (functor-general-on-mor K)]
         ;; Get on-obj for F (to get F(A) for indexing ε)
         [F-on-obj (functor-general-on-obj F)])
    (make-nat-indexed
     (string->symbol
      (format "~a*~a" (nat-indexed-name ε) (nat-indexed-name η)))
     (functor-compose-general H F)  ; H∘F
     (functor-compose-general K G)  ; K∘G
     ;; (ε * η)_A = K(η_A) ∘ ε_{F(A)}
     (lambda (A)
       (let* ([F-A (F-on-obj A)]           ; F(A) - object in D
              [η-A (nat-indexed-at η A)]    ; η_A : F(A) → G(A)
              [ε-FA (nat-indexed-at ε F-A)] ; ε_{F(A)} : H(F(A)) → K(F(A))
              [K-η-A (K-on-mor η-A)])        ; K(η_A) : K(F(A)) → K(G(A))
         (lambda (x)
           ;; x : H(F(A))
           ;; ε-FA(x) : K(F(A))
           ;; K-η-A(ε-FA(x)) : K(G(A))
           (K-η-A (ε-FA x))))))))

;;; ====
;;; Whiskering
;;; ====

(doc 'section 'whiskering)
(doc 'description "Whiskering Operations

Right whiskering (η ◁ H): Given η : F ⟹ G and H : E → C,
produces (η ◁ H) : F∘H ⟹ G∘H with (η ◁ H)_A = η_{H(A)}

Left whiskering (K ▷ η): Given K : D → E and η : F ⟹ G (C → D),
produces (K ▷ η) : K∘F ⟹ K∘G with (K ▷ η)_A = K(η_A)")

;;; nat-indexed-whisker-right : NatIndexed × FunctorGeneral → NatIndexed
;;; Right whiskering: (η ◁ H)_A = η_{H(A)}
(define (nat-indexed-whisker-right η H)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed FunctorGeneral NatIndexed))
  (doc 'description "Right whiskering η ◁ H.
Precomposes H with both functors.")
  (let ([F (nat-indexed-source η)]
        [G (nat-indexed-target η)]
        [H-on-obj (functor-general-on-obj H)])
    (make-nat-indexed
     (string->symbol (format "~a◁~a"
                             (nat-indexed-name η)
                             (functor-general-name H)))
     (functor-compose-general F H)  ; F∘H
     (functor-compose-general G H)  ; G∘H
     ;; (η ◁ H)_A = η_{H(A)}
     (lambda (A)
       (nat-indexed-at η (H-on-obj A))))))

;;; nat-indexed-whisker-left : FunctorGeneral × NatIndexed → NatIndexed
;;; Left whiskering: (K ▷ η)_A = K(η_A)
(define (nat-indexed-whisker-left K η)
  (doc 'export #t)
  (doc 'type '(-> FunctorGeneral NatIndexed NatIndexed))
  (doc 'description "Left whiskering K ▷ η.
Postcomposes K with both functors.")
  (let ([F (nat-indexed-source η)]
        [G (nat-indexed-target η)]
        [K-on-mor (functor-general-on-mor K)])
    (make-nat-indexed
     (string->symbol (format "~a▷~a"
                             (functor-general-name K)
                             (nat-indexed-name η)))
     (functor-compose-general K F)  ; K∘F
     (functor-compose-general K G)  ; K∘G
     ;; (K ▷ η)_A = K(η_A)
     (lambda (A)
       (K-on-mor (nat-indexed-at η A))))))

;;; ====
;;; Naturality Verification
;;; ====

(doc 'section 'verification)
(doc 'description "Naturality Verification

For η : F ⟹ G (functors C → D), naturality requires:
For every morphism f : A → B in C:
  G(f) ∘ η_A = η_B ∘ F(f)

In diagram form:
       F(A) ---η_A---> G(A)
        |               |
      F(f)            G(f)
        |               |
        v               v
       F(B) ---η_B---> G(B)")

;;; check-naturality-indexed : NatIndexed × Obj × Obj × Mor × Value → Boolean
;;; Check naturality for a specific morphism f : A → B at a test value.
(define (check-naturality-indexed η A B f test-value)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed Obj Obj Mor Value Boolean))
  (doc 'description "Verify G(f) ∘ η_A = η_B ∘ F(f) at test value.")
  (let* ([F (nat-indexed-source η)]
         [G (nat-indexed-target η)]
         [F-f (functor-apply-mor F f)]
         [G-f (functor-apply-mor G f)]
         [η-A (nat-indexed-at η A)]
         [η-B (nat-indexed-at η B)]
         ;; Left side: G(f) ∘ η_A
         [left (G-f (η-A test-value))]
         ;; Right side: η_B ∘ F(f)
         [right (η-B (F-f test-value))])
    (equal? left right)))

;;; ====
;;; Bridge to Parametric Transformations
;;; ====

(doc 'section 'bridge)
(doc 'description "Bridge Functions

Convert between parametric (standard) and indexed natural
transformations. Parametric transforms have trivial indexing;
indexed transforms can be specialized to specific indices.")

;;; nat-transform->nat-indexed : NatTransform → NatIndexed
;;; Lift a parametric transformation to an indexed one.
;;; The component-at ignores the object index.
;;;
;;; NOTE: This requires the source/target functors to be lifted
;;; via functor->functor-general first.
(define (nat-transform->nat-indexed η F-gen G-gen)
  (doc 'export #t)
  (doc 'type '(-> NatTransform FunctorGeneral FunctorGeneral NatIndexed))
  (doc 'description "Convert parametric to indexed transformation.
The component function ignores the object index.")
  (let ([name (if (and (pair? η) (eq? (car η) 'nat-transform))
                  (cadr η)
                  'η)]
        [component (if (and (pair? η) (eq? (car η) 'nat-transform))
                       (car (cddddr η))
                       identity)])
    (make-nat-indexed
     name
     F-gen
     G-gen
     ;; Trivial indexing: ignore the object
     (lambda (obj) component))))

;;; nat-indexed->nat-transform-at : NatIndexed × Obj → NatTransform
;;; Extract a parametric transformation specialized at a specific object.
;;; Useful when you need the standard interface at a known index.
(define (nat-indexed->nat-transform-at η obj)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed Obj NatTransform))
  (doc 'description "Extract parametric transform at specific object.
Returns a standard nat-transform with component = η_obj.")
  (let ([source (nat-indexed-source η)]
        [target (nat-indexed-target η)]
        [component (nat-indexed-at η obj)])
    (list 'nat-transform
          (nat-indexed-name η)
          (functor-general->functor source)
          (functor-general->functor target)
          component)))

;;; ====
;;; Display
;;; ====

;;; nat-indexed->string : NatIndexed → String
(define (nat-indexed->string η)
  (doc 'export #t)
  (doc 'type '(-> NatIndexed String))
  (if (nat-indexed? η)
      (format "~a : ~a ⟹ ~a (indexed)"
              (nat-indexed-name η)
              (functor-general-name (nat-indexed-source η))
              (functor-general-name (nat-indexed-target η)))
      "Not an indexed nat-transform"))

;;; ====
;;; Exports
;;; ====

(doc 'section 'exports)
(doc 'description "Exports

Definition:
  make-nat-indexed, nat-indexed?
  nat-indexed-name, nat-indexed-source, nat-indexed-target
  nat-indexed-component-at

Application:
  nat-indexed-at, nat-indexed-apply

Identity:
  nat-indexed-id

Vertical Composition:
  nat-indexed-compose

Horizontal Composition:
  nat-indexed-horizontal

Whiskering:
  nat-indexed-whisker-right, nat-indexed-whisker-left

Verification:
  check-naturality-indexed

Bridge:
  nat-transform->nat-indexed, nat-indexed->nat-transform-at

Display:
  nat-indexed->string")
