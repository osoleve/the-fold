;;; lattice/fp/category/multi/adjunction-inter.ss — Inter-Category Adjunctions
;;; @module adjunction-inter
;;; @requires nat-transform-indexed

(load "lattice/fp/category/multi/nat-transform-indexed.ss")

(doc 'module 'adjunction-inter)
(doc 'description "Inter-Category Adjunctions

An adjunction L ⊣ R between categories C and D consists of:
  - Left adjoint L : C → D
  - Right adjoint R : D → C
  - Unit η : Id_C ⟹ R∘L (indexed by objects of C)
  - Counit ε : L∘R ⟹ Id_D (indexed by objects of D)

Triangle identities:
  (ε ◁ L) ∘ (L ▷ η) = id_L   (left triangle)
  (R ▷ ε) ∘ (η ◁ R) = id_R   (right triangle)

CRITICAL DISTINCTION:
Standard adjunctions (adjunction.ss) use parametric natural
transformations. The counit component can't access the object index.

Inter-category adjunctions use INDEXED natural transformations.
The counit ε_A : L(R(A)) → A has access to algebra A for evaluation.

This is essential for Free ⊣ Forgetful where:
  ε_A(term) = eval-term A term  (requires algebra A's operations)")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Inter-Category Adjunction Definition
;;; ====

(doc 'section 'adjunction-inter-definition)

;;; make-adjunction-inter : Symbol × FunctorGeneral × FunctorGeneral ×
;;;                         NatIndexed × NatIndexed → AdjunctionInter
;;; Create an adjunction between (possibly different) categories.
;;;
;;; Parameters:
;;;   name: Identifying symbol
;;;   left: Left adjoint L : C → D
;;;   right: Right adjoint R : D → C
;;;   unit: η : Id_C ⟹ R∘L (indexed by C objects)
;;;   counit: ε : L∘R ⟹ Id_D (indexed by D objects)
(define (make-adjunction-inter name left right unit counit)
  (doc 'type '(-> Symbol FunctorGeneral FunctorGeneral
                  NatIndexed NatIndexed
                  AdjunctionInter))
  (list 'adjunction-inter name left right unit counit))

(define (adjunction-inter? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (eq? (car x) 'adjunction-inter)
       (= (length x) 6)))

(define (adjunction-inter-name adj)
  (doc 'type '(-> AdjunctionInter Symbol))
  (if (adjunction-inter? adj) (cadr adj) 'unknown))

(define (adjunction-inter-left adj)
  (doc 'type '(-> AdjunctionInter FunctorGeneral))
  (if (adjunction-inter? adj) (caddr adj) #f))

(define (adjunction-inter-right adj)
  (doc 'type '(-> AdjunctionInter FunctorGeneral))
  (if (adjunction-inter? adj) (cadddr adj) #f))

(define (adjunction-inter-unit adj)
  (doc 'type '(-> AdjunctionInter NatIndexed))
  (if (adjunction-inter? adj) (car (cddddr adj)) #f))

(define (adjunction-inter-counit adj)
  (doc 'type '(-> AdjunctionInter NatIndexed))
  (if (adjunction-inter? adj) (cadr (cddddr adj)) #f))

;;; ====
;;; Counit Application
;;; ====

(doc 'section 'counit-application)
(doc 'description "Counit Application

The counit ε_A : L(R(A)) → A is the key to evaluation.
For Free ⊣ Forgetful:
  ε_A : Free(U(A)) → A
  ε_A(term) = evaluate term using algebra A's operations

This is the 'fold' or 'catamorphism' - interpreting a free
structure using a concrete algebra.")

;;; counit-at : AdjunctionInter × Obj_D → (L(R(A)) → A)
;;; Get the counit component at a specific object.
(define (counit-at adj obj)
  (doc 'type '(-> AdjunctionInter Obj (-> Value Value)))
  (doc 'description "Get counit component ε_A at object A.
Returns a function from L(R(A)) to A.")
  (nat-indexed-at (adjunction-inter-counit adj) obj))

;;; apply-counit : AdjunctionInter × Obj_D × Value → Value
;;; Apply the counit at object A to a value.
(define (apply-counit adj obj val)
  (doc 'type '(-> AdjunctionInter Obj Value Value))
  (doc 'description "Apply ε_A to a value in L(R(A)).")
  (nat-indexed-apply (adjunction-inter-counit adj) obj val))

;;; unit-at : AdjunctionInter × Obj_C → (A → R(L(A)))
;;; Get the unit component at a specific object.
(define (unit-at adj obj)
  (doc 'type '(-> AdjunctionInter Obj (-> Value Value)))
  (doc 'description "Get unit component η_A at object A.
Returns a function from A to R(L(A)).")
  (nat-indexed-at (adjunction-inter-unit adj) obj))

;;; apply-unit : AdjunctionInter × Obj_C × Value → Value
;;; Apply the unit at object A to a value.
(define (apply-unit adj obj val)
  (doc 'type '(-> AdjunctionInter Obj Value Value))
  (doc 'description "Apply η_A to a value in A.")
  (nat-indexed-apply (adjunction-inter-unit adj) obj val))

;;; ====
;;; Triangle Identities
;;; ====

(doc 'section 'triangle-identities)
(doc 'description "Triangle Identities

The unit and counit must satisfy two triangle identities:

LEFT TRIANGLE: For each A ∈ Obj(C):
  ε_{L(A)} ∘ L(η_A) = id_{L(A)}

In components at A:
  Starting with x : L(A)
  L(η_A)(x) : L(R(L(A)))
  ε_{L(A)}(L(η_A)(x)) = x

RIGHT TRIANGLE: For each B ∈ Obj(D):
  R(ε_B) ∘ η_{R(B)} = id_{R(B)}

In components at B:
  Starting with y : R(B)
  η_{R(B)}(y) : R(L(R(B)))
  R(ε_B)(η_{R(B)}(y)) = y")

;;; verify-triangle-left-inter : AdjunctionInter × Obj_C × Value [× (a → a → Bool)] → Boolean
;;; Verify left triangle: ε_{L(A)} ∘ L(η_A) = id_{L(A)}
(define (verify-triangle-left-inter adj A test-value . opts)
  (doc 'type '(-> AdjunctionInter Obj Value [(-> a a Boolean)] Boolean))
  (doc 'description "Verify left triangle identity at object A.
Tests that ε_{L(A)}(L(η_A)(x)) = x for test value x : L(A).
Optional equality predicate for opaque carriers.")
  (let* ([L (adjunction-inter-left adj)]
         [η (adjunction-inter-unit adj)]
         [ε (adjunction-inter-counit adj)]
         [eq? (if (null? opts) equal? (car opts))]
         ;; L(A) - apply L to object A
         [L-A (functor-apply-obj L A)]
         ;; η_A : A → R(L(A))
         [η-A (nat-indexed-at η A)]
         ;; L(η_A) : L(A) → L(R(L(A)))
         [L-η-A (functor-apply-mor L η-A)]
         ;; ε_{L(A)} : L(R(L(A))) → L(A)
         [ε-LA (nat-indexed-at ε L-A)])
    ;; Verify: ε_{L(A)}(L(η_A)(x)) = x
    (eq? (ε-LA (L-η-A test-value))
         test-value)))

;;; verify-triangle-right-inter : AdjunctionInter × Obj_D × Value [× (a → a → Bool)] → Boolean
;;; Verify right triangle: R(ε_B) ∘ η_{R(B)} = id_{R(B)}
(define (verify-triangle-right-inter adj B test-value . opts)
  (doc 'type '(-> AdjunctionInter Obj Value [(-> a a Boolean)] Boolean))
  (doc 'description "Verify right triangle identity at object B.
Tests that R(ε_B)(η_{R(B)}(y)) = y for test value y : R(B).
Optional equality predicate for opaque carriers.")
  (let* ([R (adjunction-inter-right adj)]
         [η (adjunction-inter-unit adj)]
         [ε (adjunction-inter-counit adj)]
         [eq? (if (null? opts) equal? (car opts))]
         ;; R(B) - apply R to object B
         [R-B (functor-apply-obj R B)]
         ;; ε_B : L(R(B)) → B
         [ε-B (nat-indexed-at ε B)]
         ;; R(ε_B) : R(L(R(B))) → R(B)
         [R-ε-B (functor-apply-mor R ε-B)]
         ;; η_{R(B)} : R(B) → R(L(R(B)))
         [η-RB (nat-indexed-at η R-B)])
    ;; Verify: R(ε_B)(η_{R(B)}(y)) = y
    (eq? (R-ε-B (η-RB test-value))
         test-value)))

;;; verify-adjunction-inter : AdjunctionInter × Obj_C × Obj_D × Value × Value [× (a → a → Bool)] → Boolean
;;; Verify both triangle identities.
(define (verify-adjunction-inter adj A B val-left val-right . opts)
  (doc 'type '(-> AdjunctionInter Obj Obj Value Value [(-> a a Boolean)] Boolean))
  (doc 'description "Verify both triangle identities.
Optional equality predicate for opaque carriers.")
  (let ([eq? (if (null? opts) equal? (car opts))])
    (and (verify-triangle-left-inter adj A val-left eq?)
         (verify-triangle-right-inter adj B val-right eq?))))

;;; ====
;;; Hom-Set Bijection
;;; ====

(doc 'section 'hom-bijection)
(doc 'description "Hom-Set Bijection

An adjunction L ⊣ R is equivalently characterized by a natural
bijection between hom-sets:
  D(L(A), B) ≅ C(A, R(B))

Transpose-left: f : L(A) → B  ↦  R(f) ∘ η_A : A → R(B)
Transpose-right: g : A → R(B) ↦  ε_B ∘ L(g) : L(A) → B

These are mutually inverse.")

;;; transpose-left-inter : AdjunctionInter × Obj_C × Obj_D × (L(A) → B) → (A → R(B))
;;; Transpose a morphism from D to C.
;;; f : L(A) → B  ↦  R(f) ∘ η_A : A → R(B)
(define (transpose-left-inter adj A B f)
  (doc 'type '(-> AdjunctionInter Obj Obj (-> LA B) (-> A RB)))
  (doc 'description "Transpose f : L(A) → B to R(f) ∘ η_A : A → R(B).")
  (let* ([R (adjunction-inter-right adj)]
         [η (adjunction-inter-unit adj)]
         ;; R(f) : R(L(A)) → R(B)
         [R-f (functor-apply-mor R f)]
         ;; η_A : A → R(L(A))
         [η-A (nat-indexed-at η A)])
    ;; g = R(f) ∘ η_A
    (lambda (x) (R-f (η-A x)))))

;;; transpose-right-inter : AdjunctionInter × Obj_C × Obj_D × (A → R(B)) → (L(A) → B)
;;; Transpose a morphism from C to D.
;;; g : A → R(B)  ↦  ε_B ∘ L(g) : L(A) → B
(define (transpose-right-inter adj A B g)
  (doc 'type '(-> AdjunctionInter Obj Obj (-> A RB) (-> LA B)))
  (doc 'description "Transpose g : A → R(B) to ε_B ∘ L(g) : L(A) → B.")
  (let* ([L (adjunction-inter-left adj)]
         [ε (adjunction-inter-counit adj)]
         ;; L(g) : L(A) → L(R(B))
         [L-g (functor-apply-mor L g)]
         ;; ε_B : L(R(B)) → B
         [ε-B (nat-indexed-at ε B)])
    ;; f = ε_B ∘ L(g)
    (lambda (x) (ε-B (L-g x)))))

;;; ====
;;; Adjunction Composition
;;; ====

(doc 'section 'composition)
(doc 'description "Adjunction Composition

Given adjunctions F₁ ⊣ G₁ : C ⇆ D and F₂ ⊣ G₂ : D ⇆ E,
their composition is F₂∘F₁ ⊣ G₁∘G₂ : C ⇆ E.

NOTE: Right adjoints compose in reverse order!

Unit: η' = (G₁ ▷ η₂ ◁ F₁) ∘ η₁
     where η₁ : Id_C → G₁F₁ and η₂ : Id_D → G₂F₂

Counit: ε' = ε₂ ∘ (F₂ ▷ ε₁ ◁ G₂)
       where ε₁ : F₁G₁ → Id_D and ε₂ : F₂G₂ → Id_E")

;;; adjunction-compose-inter : AdjunctionInter × AdjunctionInter → AdjunctionInter
;;; Compose two adjunctions.
;;; adj1: F₁ ⊣ G₁ : C ⇆ D
;;; adj2: F₂ ⊣ G₂ : D ⇆ E
;;; Result: F₂∘F₁ ⊣ G₁∘G₂ : C ⇆ E
(define (adjunction-compose-inter adj2 adj1)
  (doc 'type '(-> AdjunctionInter AdjunctionInter AdjunctionInter))
  (doc 'description "Compose adjunctions F₂∘F₁ ⊣ G₁∘G₂.
Note: right adjoints compose in reverse order.")
  (let* ([F1 (adjunction-inter-left adj1)]
         [G1 (adjunction-inter-right adj1)]
         [η1 (adjunction-inter-unit adj1)]
         [ε1 (adjunction-inter-counit adj1)]
         [F2 (adjunction-inter-left adj2)]
         [G2 (adjunction-inter-right adj2)]
         [η2 (adjunction-inter-unit adj2)]
         [ε2 (adjunction-inter-counit adj2)]
         ;; Composed functors
         [F2-F1 (functor-compose-general F2 F1)]  ; F₂∘F₁
         [G1-G2 (functor-compose-general G1 G2)]  ; G₁∘G₂
         ;; Composed unit: η' = (G₁ ▷ η₂ ◁ F₁) ∘ η₁
         ;; First: η₂ ◁ F₁ (right whisker η₂ by F₁)
         [η2-F1 (nat-indexed-whisker-right η2 F1)]
         ;; Then: G₁ ▷ (η₂ ◁ F₁) (left whisker by G₁)
         [G1-η2-F1 (nat-indexed-whisker-left G1 η2-F1)]
         ;; Finally: compose with η₁
         [η-prime (nat-indexed-compose G1-η2-F1 η1)]
         ;; Composed counit: ε' = ε₂ ∘ (F₂ ▷ ε₁ ◁ G₂)
         ;; First: ε₁ ◁ G₂ (right whisker ε₁ by G₂)
         [ε1-G2 (nat-indexed-whisker-right ε1 G2)]
         ;; Then: F₂ ▷ (ε₁ ◁ G₂) (left whisker by F₂)
         [F2-ε1-G2 (nat-indexed-whisker-left F2 ε1-G2)]
         ;; Finally: compose with ε₂
         [ε-prime (nat-indexed-compose ε2 F2-ε1-G2)])
    (make-adjunction-inter
     (string->symbol (format "~a∘~a"
                             (adjunction-inter-name adj2)
                             (adjunction-inter-name adj1)))
     F2-F1
     G1-G2
     η-prime
     ε-prime)))

;;; ====
;;; Bridge to Standard Adjunctions
;;; ====

(doc 'section 'bridge)
(doc 'description "Bridge Functions

Convert between standard adjunctions (with parametric transforms)
and inter-category adjunctions (with indexed transforms).")

;;; adjunction->adjunction-inter : Adjunction → AdjunctionInter
;;; Lift a standard adjunction to an inter-category adjunction.
;;; The indexed components ignore their object index.
(define (adjunction->adjunction-inter adj)
  (doc 'type '(-> Adjunction AdjunctionInter))
  (doc 'description "Convert standard adjunction to inter-category.
Unit/counit get trivial indexing (ignore object index).")
  (if (not (and (pair? adj) (eq? (car adj) 'adjunction)))
      (error 'adjunction->adjunction-inter "expected adjunction")
      (let* ([name (cadr adj)]
             [left (caddr adj)]      ; standard Functor
             [right (cadddr adj)]    ; standard Functor
             [unit (car (cddddr adj))]   ; NatTransform
             [counit (cadr (cddddr adj))] ; NatTransform
             ;; Lift functors
             [L-gen (functor->functor-general left)]
             [R-gen (functor->functor-general right)]
             ;; Lift unit (trivial indexing)
             [η-indexed (nat-transform->nat-indexed unit L-gen R-gen)]
             ;; Lift counit (trivial indexing)
             [ε-indexed (nat-transform->nat-indexed counit R-gen L-gen)])
        (make-adjunction-inter
         name
         L-gen
         R-gen
         η-indexed
         ε-indexed))))

;;; ====
;;; Display
;;; ====

;;; adjunction-inter->string : AdjunctionInter → String
(define (adjunction-inter->string adj)
  (doc 'type '(-> AdjunctionInter String))
  (if (adjunction-inter? adj)
      (format "~a : ~a ⊣ ~a (indexed)"
              (adjunction-inter-name adj)
              (functor-general-name (adjunction-inter-left adj))
              (functor-general-name (adjunction-inter-right adj)))
      "Not an adjunction"))

;;; ====
;;; Exports
;;; ====

(doc 'section 'exports)
(doc 'description "Exports

Definition:
  make-adjunction-inter, adjunction-inter?
  adjunction-inter-name, adjunction-inter-left, adjunction-inter-right
  adjunction-inter-unit, adjunction-inter-counit

Unit/Counit Application:
  counit-at, apply-counit
  unit-at, apply-unit

Triangle Identities:
  verify-triangle-left-inter, verify-triangle-right-inter
  verify-adjunction-inter

Hom-Set Bijection:
  transpose-left-inter, transpose-right-inter

Composition:
  adjunction-compose-inter

Bridge:
  adjunction->adjunction-inter

Display:
  adjunction-inter->string")
