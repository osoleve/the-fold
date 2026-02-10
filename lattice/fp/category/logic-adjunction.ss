;;; @module logic-adjunction
;;; @requires adjunction

(require 'adjunction)

(doc 'module 'logic-adjunction)
(doc 'description "Logical Connectives as Adjunctions

This module encodes logical operations through adjunction chains,
following Lawvere's insight that logic arises from categorical structure.

Key adjunction chains:

  × ⊣ Δ ⊣ +     Product and coproduct as adjoints to diagonal
  Σ ⊣ f* ⊣ Π    Quantifiers as adjoints to substitution

The Curry-Howard correspondence emerges naturally:
  - Product (×) corresponds to conjunction (∧)
  - Coproduct (+) corresponds to disjunction (∨)
  - Pi (Π) corresponds to universal quantification (∀)
  - Sigma (Σ) corresponds to existential quantification (∃)

References:
  - Lawvere, Adjointness in Foundations (1969)
  - Awodey, Category Theory Ch. 9
  - nLab: hyperdoctrine")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'diagonal-adjunction)
(doc 'description "Part 1: The Diagonal Adjunction × ⊣ Δ ⊣ +

The diagonal functor Δ : C → C×C sends each object A to the pair (A, A)
and each morphism f to the pair (f, f).

This functor has both a left and right adjoint:
  - Left adjoint: Coproduct (+)  gives  + ⊣ Δ
  - Right adjoint: Product (×)   gives  Δ ⊣ ×

These adjunctions express the universal properties of products/coproducts:
  - Hom((A,B), Δ(C)) ≅ Hom(A+B, C)   coproduct is universal for copairing
  - Hom(Δ(C), (A,B)) ≅ Hom(C, A×B)   product is universal for pairing")

(doc 'section 'pair-category-ops)
(doc 'description "Pair Category Operations

Objects in C×C are pairs (A, B).
Morphisms are pairs of morphisms (f, g) : (A, B) → (A', B').")

(define (make-pair-obj a b)
  (doc 'type '(-> A B (Pair A B)))
  (doc 'description "Create a pair object (A, B)")
  (cons a b))

(define (pair-fst p)
  (doc 'type '(-> (Pair A B) A))
  (doc 'description "Extract first component of pair")
  (car p))

(define (pair-snd p)
  (doc 'type '(-> (Pair A B) B))
  (doc 'description "Extract second component of pair")
  (cdr p))

(define (make-pair-mor f g)
  (doc 'type '(-> (-> A A*) (-> B B*) (-> (Pair A B) (Pair A* B*))))
  (doc 'description "Lift morphisms to pair morphism")
  (lambda (p)
    (make-pair-obj (f (pair-fst p)) (g (pair-snd p)))))

(doc 'section 'diagonal-functor)
(doc 'description "Diagonal Functor Δ : C → C×C")

(define (diagonal-obj a)
  (doc 'type '(-> A (Pair A A)))
  (doc 'description "Diagonal on objects: a ↦ (a, a)")
  (make-pair-obj a a))

(define (diagonal-mor f)
  (doc 'type '(-> (-> A B) (-> (Pair A A) (Pair B B))))
  (doc 'description "Diagonal on morphisms: f ↦ (f, f)")
  (make-pair-mor f f))

(doc functor-diagonal 'type 'Functor)
(doc functor-diagonal 'description "The diagonal functor Δ : C → C×C")
(define functor-diagonal
  (make-named-functor 'Δ diagonal-mor))

(doc 'section 'product-functor)
(doc 'description "Product Functor × : C×C → C

The product functor sends (A, B) to A×B.
At the value level, this is just pairing.")

(define (product-obj pair-of-sets)
  (doc 'type '(-> (Pair A B) (* A B)))
  (doc 'description "Product on objects: (A, B) ↦ A×B
We represent A×B as a pair (Scheme cons).
For concrete values, this is identity (pairs are products)")
  pair-of-sets)

(define (product-mor pair-of-fns)
  (doc 'type '(-> (Pair (-> A A*) (-> B B*)) (-> (* A B) (* A* B*))))
  (doc 'description "Product on morphisms: (f, g) ↦ f×g")
  (let ([f (pair-fst pair-of-fns)]
        [g (pair-snd pair-of-fns)])
    (lambda (ab)
      (make-pair-obj (f (pair-fst ab)) (g (pair-snd ab))))))

(doc functor-product 'type 'Functor)
(doc functor-product 'description "The product functor × : C×C → C")
(define functor-product
  (make-named-functor '× product-mor))

(doc 'section 'coproduct-functor)
(doc 'description "Coproduct Functor + : C×C → C

The coproduct (sum) functor sends (A, B) to A+B.
We represent sums as tagged values: ('left . a) or ('right . b).")

(define (make-left a)
  (doc 'type '(-> A (+ A B)))
  (doc 'description "Left injection into coproduct")
  (cons 'left a))

(define (make-right b)
  (doc 'type '(-> B (+ A B)))
  (doc 'description "Right injection into coproduct")
  (cons 'right b))

(define (left? x)
  (doc 'type '(-> (+ A B) Boolean))
  (doc 'description "Test if coproduct value is left")
  (and (pair? x) (eq? (car x) 'left)))

(define (right? x)
  (doc 'type '(-> (+ A B) Boolean))
  (doc 'description "Test if coproduct value is right")
  (and (pair? x) (eq? (car x) 'right)))

(define (from-left x)
  (doc 'type '(-> (+ A B) A))
  (doc 'description "Extract value from left injection")
  (if (left? x) (cdr x) (error 'from-left "Not a left value")))

(define (from-right x)
  (doc 'type '(-> (+ A B) B))
  (doc 'description "Extract value from right injection")
  (if (right? x) (cdr x) (error 'from-right "Not a right value")))

(define (coproduct-mor pair-of-fns)
  (doc 'type '(-> (Pair (-> A A*) (-> B B*)) (-> (+ A B) (+ A* B*))))
  (doc 'description "Coproduct on morphisms: (f, g) ↦ f+g")
  (let ([f (pair-fst pair-of-fns)]
        [g (pair-snd pair-of-fns)])
    (lambda (x)
      (if (left? x)
          (make-left (f (from-left x)))
          (make-right (g (from-right x)))))))

(doc functor-coproduct 'type 'Functor)
(doc functor-coproduct 'description "The coproduct functor + : C×C → C")
(define functor-coproduct
  (make-named-functor '+ coproduct-mor))

(doc 'section 'adjunction-diagonal-product)
(doc 'description "Adjunction: Δ ⊣ × (Diagonal left adjoint to Product)

Unit η : Id → ×∘Δ
  η_A : A → A×A   (diagonal embedding)

Counit ε : Δ∘× → Id
  ε_{(A,B)} : (A×B, A×B) → (A, B)   (projections)")

(define (unit-diagonal-product a)
  (doc 'type '(-> A (* A A)))
  (doc 'description "The diagonal map: a ↦ (a, a)")
  (make-pair-obj a a))

(define (counit-diagonal-product pair-of-products)
  (doc 'type '(-> (Pair (* A B) (* A B)) (Pair A B)))
  (doc 'description "Project out components: ((a,b), (a,b)) ↦ (a, b)")
  (let ([ab1 (pair-fst pair-of-products)])
    ab1))

(doc nat-unit-Δ× 'type 'NatTransform)
(doc nat-unit-Δ× 'description "Unit natural transformation η : Id ⟹ Δ")
(define nat-unit-Δ×
  (make-nat-transform 'η-Δ× functor-id functor-diagonal
                      unit-diagonal-product))

(doc nat-counit-Δ× 'type 'NatTransform)
(doc nat-counit-Δ× 'description "Counit natural transformation ε : Δ ⟹ Id")
(define nat-counit-Δ×
  (make-nat-transform 'ε-Δ× functor-diagonal functor-id
                      counit-diagonal-product))

(doc adj-diagonal-product 'type 'Adjunction)
(doc adj-diagonal-product 'description "The adjunction Δ ⊣ × expressing the universal property of products")
(define adj-diagonal-product
  (make-adjunction 'Δ⊣× functor-diagonal functor-product
                   nat-unit-Δ× nat-counit-Δ×))

(doc 'section 'adjunction-coproduct-diagonal)
(doc 'description "Adjunction: + ⊣ Δ (Coproduct left adjoint to Diagonal)

Unit η : Id → Δ∘+
  η_{(A,B)} : (A, B) → (A+B, A+B)   (injections)

Counit ε : +∘Δ → Id
  ε_A : A+A → A   (codiagonal/fold)")

(define (unit-coproduct-diagonal pair)
  (doc 'type '(-> (Pair A B) (Pair (+ A B) (+ A B))))
  (doc 'description "Inject into coproduct: (a, b) ↦ (left(a), right(b))

The unit for + ⊣ Δ at (A,B) is the pair (inl, inr) : (A,B) → (A+B, A+B)")
  (make-pair-obj (make-left (pair-fst pair))
                 (make-right (pair-snd pair))))

(define (counit-coproduct-diagonal x)
  (doc 'type '(-> (+ A A) A))
  (doc 'description "The codiagonal (fold): combine two copies")
  (if (left? x)
      (from-left x)
      (from-right x)))

(doc nat-unit-+Δ 'type 'NatTransform)
(doc nat-unit-+Δ 'description "Unit η : Id ⟹ Δ for coproduct adjunction")
(define nat-unit-+Δ
  (make-nat-transform 'η-+Δ functor-id functor-diagonal
                      unit-coproduct-diagonal))

(doc nat-counit-+Δ 'type 'NatTransform)
(doc nat-counit-+Δ 'description "Counit ε : + ⟹ Id for coproduct adjunction")
(define nat-counit-+Δ
  (make-nat-transform 'ε-+Δ functor-coproduct functor-id
                      counit-coproduct-diagonal))

(doc adj-coproduct-diagonal 'type 'Adjunction)
(doc adj-coproduct-diagonal 'description "The adjunction + ⊣ Δ expressing the universal property of coproducts")
(define adj-coproduct-diagonal
  (make-adjunction '+⊣Δ functor-coproduct functor-diagonal
                   nat-unit-+Δ nat-counit-+Δ))

(doc 'section 'quantifiers-as-adjoints)
(doc 'description "Part 2: Quantifiers as Adjoints to Substitution

In a hyperdoctrine (categorical model of logic), we have:
  - A base category C (contexts/types)
  - For each object I in C, a category (or poset) Pred(I) of predicates
  - For each morphism f : I → J, a reindexing functor f* : Pred(J) → Pred(I)

The quantifiers arise as adjoints to reindexing:
  Σ_f ⊣ f* ⊣ Π_f

Where:
  - Σ_f : Pred(I) → Pred(J) is existential quantification along f
  - Π_f : Pred(I) → Pred(J) is universal quantification along f

For dependent types:
  - Pred(I) is the category of types indexed over I
  - f* is substitution
  - Σ corresponds to dependent sum (Σ types)
  - Π corresponds to dependent product (Π types)")

(doc 'section 'indexed-families)
(doc 'description "Families over a Type (Indexed Types)

A family over type I is a function I → Type.
We represent this as a record with the index type and the family function.")

(define (make-family index-type type-fn)
  (doc 'type '(-> Type (-> I Type) Family))
  (doc 'description "Create a type family over index type")
  (list 'family index-type type-fn))

(define (family? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a family")
  (and (pair? x) (eq? (car x) 'family)))

(define (family-index fam)
  (doc 'type '(-> Family Type))
  (doc 'description "Get the index type of a family")
  (if (family? fam) (cadr fam) #f))

(define (family-at fam i)
  (doc 'type '(-> Family I Type))
  (doc 'description "Get the type of the family at index i")
  (if (family? fam)
      ((caddr fam) i)
      #f))

(doc 'section 'substitution-functor)
(doc 'description "Substitution Functor f* : Fam(J) → Fam(I)

Given f : I → J and a family P over J, the substitution f*(P) is
the family over I defined by (f*P)(i) = P(f(i)).")

(define (subst-family f fam-over-j i-type)
  (doc 'type '(-> (-> I J) (Family J) Type (Family I)))
  (doc 'description "Reindex a family along a morphism")
  (make-family i-type
               (lambda (i)
                 (family-at fam-over-j (f i)))))

(define (make-subst-functor f i-type)
  (doc 'type '(-> (-> I J) Type Functor))
  (doc 'description "Create the substitution functor f* for a given f : I → J

For objects: f*(P) at i = P at f(i)
For morphisms: given h : P → Q in Fam(J), produce f*h : f*P → f*Q
               where (f*h)_i = h_{f(i)}")
  (make-named-functor
   (string->symbol (format "~a*" f))
   (lambda (h fam-over-j)
     (make-family i-type
                  (lambda (i) (h (family-at fam-over-j (f i))))))))

(doc 'section 'sigma-types)
(doc 'description "Sigma (Existential) as Left Adjoint to Substitution

Given f : I → J and family P over I:
  (Σ_f P)(j) = Σ(i : I | f(i) = j). P(i)

This is a dependent sum: pairs (i, p) where f(i) = j and p : P(i).

At the type level:
  Σ types are dependent pairs (x : A, B(x))")

(define (make-sigma-type var domain codomain-fn)
  (doc 'type '(-> Symbol Type (-> A Type) ΣType))
  (doc 'description "Construct a sigma type (Σ ((x : A)) B(x))")
  (list 'Σ (list (list var ': domain)) codomain-fn))

(define (sigma-fst pair)
  (doc 'type '(-> (Σ A (-> A B)) A))
  (doc 'description "First projection from a sigma type inhabitant")
  (car pair))

(define (sigma-snd pair)
  (doc 'type '(-> (Σ A (-> A B)) B))
  (doc 'description "Second projection from a sigma type inhabitant")
  (cdr pair))

(define (make-sigma-pair a b)
  (doc 'type '(-> A B (Σ A (-> A B))))
  (doc 'description "Construct a sigma pair (a, b) where b : B(a)")
  (cons a b))

(define (sigma-along f fam-over-i j-type)
  (doc 'type '(-> (-> I J) (Family I) Type (Family J)))
  (doc 'description "Existential quantification along f.
(Σ_f P)(j) represents exists i. f(i)=j ∧ P(i)")
  (make-family j-type
               (lambda (j)
                 (list 'Σ-fiber j f (family-index fam-over-i)
                       (lambda (i) (family-at fam-over-i i))))))

(doc 'section 'pi-types)
(doc 'description "Pi (Universal) as Right Adjoint to Substitution

Given f : I → J and family P over I:
  (Π_f P)(j) = Π(i : I | f(i) = j). P(i)

This is a dependent product: functions sending each i in the fiber to P(i).

At the type level:
  Π types are dependent functions (x : A) → B(x)")

(define (make-pi-type var domain codomain-fn)
  (doc 'type '(-> Symbol Type (-> A Type) ΠType))
  (doc 'description "Construct a pi type (Π ((x : A)) B(x))")
  (list 'Π (list (list var ': domain)) codomain-fn))

(define (pi-apply f a)
  (doc 'type '(-> (Π A (-> A B)) A B))
  (doc 'description "Apply a pi type inhabitant (dependent function) to an argument")
  (f a))

(define (make-pi-fn f)
  (doc 'type '(-> (-> A B) (Π A (-> A B))))
  (doc 'description "Construct a pi term from a Scheme function")
  f)

(define (pi-along f fam-over-i j-type)
  (doc 'type '(-> (-> I J) (Family I) Type (Family J)))
  (doc 'description "Universal quantification along f.
(Π_f P)(j) represents forall i. f(i)=j → P(i)")
  (make-family j-type
               (lambda (j)
                 (list 'Π-fiber j f (family-index fam-over-i)
                       (lambda (i) (family-at fam-over-i i))))))

(doc 'section 'beck-chevalley)
(doc 'description "Part 3: Beck-Chevalley Condition

For a pullback square:

    I' --g'--> J'
    |          |
   f'          f
    v          v
    I ---g---> J

The Beck-Chevalley condition states:
  g'* ∘ Σ_f ≅ Σ_f' ∘ g*
  g'* ∘ Π_f ≅ Π_f' ∘ g*

This says substitution commutes with quantification along pullback squares.
This is automatic in many categories (locally cartesian closed categories).")

(doc 'section 'frobenius-reciprocity)
(doc 'description "Frobenius Reciprocity

For the Σ adjunction:
  Σ_f(f*(Q) ∧ P) ≅ Q ∧ Σ_f(P)

This is the categorical form of the logical equivalence:
  ∃x. (Q ∧ P(x)) ↔ Q ∧ ∃x. P(x)   (when Q doesn't depend on x)")

(doc 'section 'curry-howard)
(doc 'description "Part 4: Curry-Howard Correspondence

The adjunctions above give rise to the Curry-Howard correspondence:

| Logic          | Type Theory      | Category Theory  |
|----------------|------------------|------------------|
| Proposition    | Type             | Object           |
| Proof          | Term             | Morphism         |
| True (⊤)       | Unit             | Terminal object  |
| False (⊥)      | Void             | Initial object   |
| And (∧)        | Product (×)      | Product          |
| Or (∨)         | Sum (+)          | Coproduct        |
| Implies (→)    | Function (→)     | Exponential      |
| Forall (∀)     | Pi (Π)           | Right adjoint    |
| Exists (∃)     | Sigma (Σ)        | Left adjoint     |

The fact that × ⊣ Δ ⊣ + means:
  - Proofs of A∧B are exactly pairs of proofs
  - Proofs of A∨B are exactly tagged proofs (left or right)

The fact that Σ ⊣ f* ⊣ Π means:
  - Proofs of ∃x.P(x) are pairs (witness, proof)
  - Proofs of ∀x.P(x) are functions from witnesses to proofs")

(doc 'section 'logical-connectives)

(define (conj-intro a b)
  (doc 'type '(-> A B (∧ A B)))
  (doc 'description "Conjunction introduction (pair)")
  (make-pair-obj a b))

(define (conj-elim-left ab)
  (doc 'type '(-> (∧ A B) A))
  (doc 'description "Conjunction elimination (left projection)")
  (pair-fst ab))

(define (conj-elim-right ab)
  (doc 'type '(-> (∧ A B) B))
  (doc 'description "Conjunction elimination (right projection)")
  (pair-snd ab))

(define (disj-intro-left a)
  (doc 'type '(-> A (∨ A B)))
  (doc 'description "Disjunction introduction (left injection)")
  (make-left a))

(define (disj-intro-right b)
  (doc 'type '(-> B (∨ A B)))
  (doc 'description "Disjunction introduction (right injection)")
  (make-right b))

(define (disj-elim ab f g)
  (doc 'type '(-> (∨ A B) (-> A C) (-> B C) C))
  (doc 'description "Disjunction elimination (case analysis)")
  (if (left? ab)
      (f (from-left ab))
      (g (from-right ab))))

(doc exists-intro 'type '(-> A B (∃ A B)))
(doc exists-intro 'description "Existential introduction (dependent pair)")
(define exists-intro make-sigma-pair)

(define (exists-elim witness-proof f)
  (doc 'type '(-> (∃ A B) (-> A B C) C))
  (doc 'description "Existential elimination (with proof-irrelevant conclusion)")
  (let ([witness (sigma-fst witness-proof)]
        [proof (sigma-snd witness-proof)])
    (f witness proof)))

(doc forall-intro 'type '(-> (-> A B) (∀ A B)))
(doc forall-intro 'description "Universal introduction (lambda)")
(define forall-intro make-pi-fn)

(doc forall-elim 'type '(-> (∀ A B) A B))
(doc forall-elim 'description "Universal elimination (application)")
(define forall-elim pi-apply)

(doc 'section 'exports)
(doc 'description "Exports Summary

Pair category operations:
  make-pair-obj, pair-fst, pair-snd, make-pair-mor

Diagonal functor:
  diagonal-obj, diagonal-mor, functor-diagonal

Product/Coproduct functors:
  functor-product, functor-coproduct
  make-left, make-right, left?, right?, from-left, from-right

Adjunctions:
  adj-diagonal-product (Δ ⊣ ×)
  adj-coproduct-diagonal (+ ⊣ Δ)

Dependent type operations:
  make-family, family?, family-index, family-at
  subst-family, make-subst-functor
  sigma-along, pi-along
  make-sigma-type, make-sigma-pair, sigma-fst, sigma-snd
  make-pi-type, make-pi-fn, pi-apply

Logical connectives:
  conj-intro, conj-elim-left, conj-elim-right
  disj-intro-left, disj-intro-right, disj-elim
  exists-intro, exists-elim
  forall-intro, forall-elim")
