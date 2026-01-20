;;; lattice/fp/category/logic-adjunction.ss — Logical Connectives as Adjunctions
;;;
;;; This module encodes logical operations through adjunction chains,
;;; following Lawvere's insight that logic arises from categorical structure.
;;;
;;; Key adjunction chains:
;;;
;;;   × ⊣ Δ ⊣ +     Product and coproduct as adjoints to diagonal
;;;   Σ ⊣ f* ⊣ Π    Quantifiers as adjoints to substitution
;;;
;;; The Curry-Howard correspondence emerges naturally:
;;;   - Product (×) corresponds to conjunction (∧)
;;;   - Coproduct (+) corresponds to disjunction (∨)
;;;   - Pi (Π) corresponds to universal quantification (∀)
;;;   - Sigma (Σ) corresponds to existential quantification (∃)
;;;
;;; References:
;;;   - Lawvere, "Adjointness in Foundations" (1969)
;;;   - Awodey, "Category Theory" Ch. 9
;;;   - nLab: "hyperdoctrine"
;;;
;;; Dependencies:
;;;   - adjunction.ss
;;;   - natural-transform.ss

(load "lattice/fp/category/adjunction.ss")

;;; ============================================================
;;; Part 1: The Diagonal Adjunction × ⊣ Δ ⊣ +
;;; ============================================================
;;;
;;; The diagonal functor Δ : C → C×C sends each object A to the pair (A, A)
;;; and each morphism f to the pair (f, f).
;;;
;;; This functor has both a left and right adjoint:
;;;   - Left adjoint: Coproduct (+)  gives  + ⊣ Δ
;;;   - Right adjoint: Product (×)   gives  Δ ⊣ ×
;;;
;;; These adjunctions express the universal properties of products/coproducts:
;;;   - Hom((A,B), Δ(C)) ≅ Hom(A+B, C)   "coproduct is universal for copairing"
;;;   - Hom(Δ(C), (A,B)) ≅ Hom(C, A×B)   "product is universal for pairing"

;;; ====
;;; Pair Category Operations
;;; ====
;;;
;;; Objects in C×C are pairs (A, B).
;;; Morphisms are pairs of morphisms (f, g) : (A, B) → (A', B').

;;; make-pair-obj : A × B → (A, B)
(define (make-pair-obj a b)
  (cons a b))

;;; pair-fst : (A, B) → A
(define (pair-fst p) (car p))

;;; pair-snd : (A, B) → B
(define (pair-snd p) (cdr p))

;;; make-pair-mor : (A → A') × (B → B') → ((A, B) → (A', B'))
(define (make-pair-mor f g)
  (lambda (p)
    (make-pair-obj (f (pair-fst p)) (g (pair-snd p)))))

;;; ====
;;; Diagonal Functor Δ : C → C×C
;;; ====

;;; diagonal-obj : A → (A, A)
(define (diagonal-obj a)
  (make-pair-obj a a))

;;; diagonal-mor : (A → B) → ((A, A) → (B, B))
(define (diagonal-mor f)
  (make-pair-mor f f))

;;; functor-diagonal : Functor
;;; The diagonal functor Δ : C → C×C
(define functor-diagonal
  (make-named-functor 'Δ diagonal-mor))

;;; ====
;;; Product Functor × : C×C → C
;;; ====
;;;
;;; The product functor sends (A, B) to A×B.
;;; At the value level, this is just pairing.

;;; product-obj : (A, B) → A×B
;;; We represent A×B as a pair (Scheme cons).
(define (product-obj pair-of-sets)
  ;; Given (A, B) representing sets, A×B is represented by pairs
  ;; For concrete values, this is identity (pairs are products)
  pair-of-sets)

;;; product-mor : ((A, B) → (A', B')) → (A×B → A'×B')
;;; Given morphisms (f, g), produce f×g.
(define (product-mor pair-of-fns)
  ;; pair-of-fns is (f . g) where f : A → A', g : B → B'
  (let ([f (pair-fst pair-of-fns)]
        [g (pair-snd pair-of-fns)])
    (lambda (ab)
      (make-pair-obj (f (pair-fst ab)) (g (pair-snd ab))))))

;;; functor-product : Functor
;;; The product functor × : C×C → C
(define functor-product
  (make-named-functor '× product-mor))

;;; ====
;;; Coproduct Functor + : C×C → C
;;; ====
;;;
;;; The coproduct (sum) functor sends (A, B) to A+B.
;;; We represent sums as tagged values: ('left . a) or ('right . b).

;;; make-left : A → A+B
(define (make-left a)
  (cons 'left a))

;;; make-right : B → A+B
(define (make-right b)
  (cons 'right b))

;;; left? : A+B → Boolean
(define (left? x)
  (and (pair? x) (eq? (car x) 'left)))

;;; right? : A+B → Boolean
(define (right? x)
  (and (pair? x) (eq? (car x) 'right)))

;;; from-left : A+B → A
(define (from-left x)
  (if (left? x) (cdr x) (error 'from-left "Not a left value")))

;;; from-right : A+B → B
(define (from-right x)
  (if (right? x) (cdr x) (error 'from-right "Not a right value")))

;;; coproduct-mor : ((A, B) → (A', B')) → (A+B → A'+B')
;;; Given morphisms (f, g), produce f+g.
(define (coproduct-mor pair-of-fns)
  (let ([f (pair-fst pair-of-fns)]
        [g (pair-snd pair-of-fns)])
    (lambda (x)
      (if (left? x)
          (make-left (f (from-left x)))
          (make-right (g (from-right x)))))))

;;; functor-coproduct : Functor
;;; The coproduct functor + : C×C → C
(define functor-coproduct
  (make-named-functor '+ coproduct-mor))

;;; ====
;;; Adjunction: Δ ⊣ × (Diagonal left adjoint to Product)
;;; ====
;;;
;;; Unit η : Id → ×∘Δ
;;;   η_A : A → A×A   (diagonal embedding)
;;;
;;; Counit ε : Δ∘× → Id
;;;   ε_{(A,B)} : (A×B, A×B) → (A, B)   (projections)

;;; unit-diagonal-product : A → A×A
;;; The diagonal map: a ↦ (a, a)
(define (unit-diagonal-product a)
  (make-pair-obj a a))

;;; counit-diagonal-product : (A×B, A×B) → (A, B)
;;; Project out components: ((a,b), (a,b)) ↦ (a, b)
(define (counit-diagonal-product pair-of-products)
  (let ([ab1 (pair-fst pair-of-products)])
    ;; ab1 is (a . b), we want (a, b) which is the same
    ab1))

(define nat-unit-Δ×
  (make-nat-transform 'η-Δ× functor-id functor-diagonal
                      unit-diagonal-product))

(define nat-counit-Δ×
  (make-nat-transform 'ε-Δ× functor-diagonal functor-id
                      counit-diagonal-product))

;;; adj-diagonal-product : Adjunction
;;; The adjunction Δ ⊣ × expressing the universal property of products.
(define adj-diagonal-product
  (make-adjunction 'Δ⊣× functor-diagonal functor-product
                   nat-unit-Δ× nat-counit-Δ×))

;;; ====
;;; Adjunction: + ⊣ Δ (Coproduct left adjoint to Diagonal)
;;; ====
;;;
;;; Unit η : Id → Δ∘+
;;;   η_{(A,B)} : (A, B) → (A+B, A+B)   (injections)
;;;
;;; Counit ε : +∘Δ → Id
;;;   ε_A : A+A → A   (codiagonal/fold)

;;; unit-coproduct-diagonal : (A, B) → (A+B, A+B)
;;; Inject into coproduct: (a, b) ↦ (left(a), right(b)) lifted to diagonal
;;; Actually, the unit sends (A, B) in C×C to Δ(A+B) = (A+B, A+B)
;;; The component at (A, B) takes a value from the "set" (A, B)
;;; But for the adjunction, we work with morphisms...
;;;
;;; For a concrete implementation at the element level:
;;; Given a pair (a, b), we need to produce (A+B, A+B) somehow.
;;; The unit η sends objects, but at the morphism level:
;;; For any (f, g) : (C, C) → (A, B), we transpose to C → A+B
(define (unit-coproduct-diagonal pair)
  ;; (a, b) ↦ (inl(a), inr(b)) both in A+B
  ;; But actually we need to pick one... this is tricky at value level
  ;; The categorical unit η_{A,B} : (A,B) → Δ(A+B) sends
  ;; (a ∈ A, b ∈ B) to... but that's not how adjunction units work.
  ;;
  ;; The unit for + ⊣ Δ at (A,B) is the pair (inl, inr) : (A,B) → (A+B, A+B)
  ;; This means: given a ∈ A, produce inl(a) ∈ A+B
  ;;             given b ∈ B, produce inr(b) ∈ A+B
  (make-pair-obj (make-left (pair-fst pair))
                 (make-right (pair-snd pair))))

;;; counit-coproduct-diagonal : A+A → A
;;; The codiagonal (fold): combine two copies
(define (counit-coproduct-diagonal x)
  (if (left? x)
      (from-left x)
      (from-right x)))

(define nat-unit-+Δ
  (make-nat-transform 'η-+Δ functor-id functor-diagonal
                      unit-coproduct-diagonal))

(define nat-counit-+Δ
  (make-nat-transform 'ε-+Δ functor-coproduct functor-id
                      counit-coproduct-diagonal))

;;; adj-coproduct-diagonal : Adjunction
;;; The adjunction + ⊣ Δ expressing the universal property of coproducts.
(define adj-coproduct-diagonal
  (make-adjunction '+⊣Δ functor-coproduct functor-diagonal
                   nat-unit-+Δ nat-counit-+Δ))

;;; ============================================================
;;; Part 2: Quantifiers as Adjoints to Substitution
;;; ============================================================
;;;
;;; In a hyperdoctrine (categorical model of logic), we have:
;;;   - A base category C (contexts/types)
;;;   - For each object I in C, a category (or poset) Pred(I) of predicates
;;;   - For each morphism f : I → J, a reindexing functor f* : Pred(J) → Pred(I)
;;;
;;; The quantifiers arise as adjoints to reindexing:
;;;   Σ_f ⊣ f* ⊣ Π_f
;;;
;;; Where:
;;;   - Σ_f : Pred(I) → Pred(J) is existential quantification along f
;;;   - Π_f : Pred(I) → Pred(J) is universal quantification along f
;;;
;;; For dependent types:
;;;   - Pred(I) is the category of types indexed over I
;;;   - f* is substitution
;;;   - Σ corresponds to dependent sum (Σ types)
;;;   - Π corresponds to dependent product (Π types)

;;; ====
;;; Families over a Type (Indexed Types)
;;; ====
;;;
;;; A family over type I is a function I → Type.
;;; We represent this as a record with the index type and the family function.

;;; make-family : Type × (I → Type) → Family
(define (make-family index-type type-fn)
  (list 'family index-type type-fn))

;;; family? : Any → Boolean
(define (family? x)
  (and (pair? x) (eq? (car x) 'family)))

;;; family-index : Family → Type
(define (family-index fam)
  (if (family? fam) (cadr fam) #f))

;;; family-at : Family × I → Type
;;; Get the type of the family at index i.
(define (family-at fam i)
  (if (family? fam)
      ((caddr fam) i)
      #f))

;;; ====
;;; Substitution Functor f* : Fam(J) → Fam(I)
;;; ====
;;;
;;; Given f : I → J and a family P over J, the substitution f*(P) is
;;; the family over I defined by (f*P)(i) = P(f(i)).

;;; subst-family : (I → J) × Family(J) → Family(I)
;;; Reindex a family along a morphism.
(define (subst-family f fam-over-j i-type)
  (make-family i-type
               (lambda (i)
                 (family-at fam-over-j (f i)))))

;;; make-subst-functor : (I → J) × Type → Functor
;;; Create the substitution functor f* for a given f : I → J.
;;;
;;; For objects: f*(P) at i = P at f(i)  (see subst-family)
;;; For morphisms: given h : P → Q in Fam(J), produce f*h : f*P → f*Q
;;;                where (f*h)_i = h_{f(i)}
(define (make-subst-functor f i-type)
  (make-named-functor
   (string->symbol (format "~a*" f))
   ;; fmap: transform family morphisms
   ;; h : P → Q is a function mapping types in family P to types in family Q
   ;; f*h : f*P → f*Q maps types at index i by applying h at index f(i)
   (lambda (h fam-over-j)
     ;; Return a properly wrapped family over I
     (make-family i-type
                  (lambda (i) (h (family-at fam-over-j (f i))))))))

;;; ====
;;; Sigma (Existential) as Left Adjoint to Substitution
;;; ====
;;;
;;; Given f : I → J and family P over I:
;;;   (Σ_f P)(j) = Σ(i : I | f(i) = j). P(i)
;;;
;;; This is a dependent sum: pairs (i, p) where f(i) = j and p : P(i).
;;;
;;; At the type level:
;;;   Σ types are dependent pairs (x : A, B(x))

;;; make-sigma-type : Symbol × Type × (A → Type) → ΣType
;;; Construct a sigma type (Σ ((x : A)) B(x))
(define (make-sigma-type var domain codomain-fn)
  (list 'Σ (list (list var ': domain)) codomain-fn))

;;; sigma-projection-a : (Σ (x:A) B(x)) → A
;;; First projection from a sigma type inhabitant.
(define (sigma-fst pair)
  (car pair))

;;; sigma-snd : (Σ (x:A) B(x)) → B(fst(pair))
;;; Second projection from a sigma type inhabitant.
(define (sigma-snd pair)
  (cdr pair))

;;; make-sigma-pair : A × B(a) → (Σ (x:A) B(x))
;;; Construct a sigma pair (a, b) where b : B(a).
(define (make-sigma-pair a b)
  (cons a b))

;;; sigma-along : (I → J) × Family(I) → Family(J)
;;; Existential quantification along f.
;;; (Σ_f P)(j) represents "exists i. f(i)=j ∧ P(i)"
(define (sigma-along f fam-over-i j-type)
  (make-family j-type
               (lambda (j)
                 ;; The type at j is the sigma over preimage
                 ;; Σ(i : I | f(i) = j). family-at(P, i)
                 ;; We represent this as a symbolic sigma type
                 (list 'Σ-fiber j f (family-index fam-over-i)
                       (lambda (i) (family-at fam-over-i i))))))

;;; ====
;;; Pi (Universal) as Right Adjoint to Substitution
;;; ====
;;;
;;; Given f : I → J and family P over I:
;;;   (Π_f P)(j) = Π(i : I | f(i) = j). P(i)
;;;
;;; This is a dependent product: functions sending each i in the fiber to P(i).
;;;
;;; At the type level:
;;;   Π types are dependent functions (x : A) → B(x)

;;; make-pi-type : Symbol × Type × (A → Type) → ΠType
;;; Construct a pi type (Π ((x : A)) B(x))
(define (make-pi-type var domain codomain-fn)
  (list 'Π (list (list var ': domain)) codomain-fn))

;;; pi-apply : (Π (x:A) B(x)) × A → B(a)
;;; Apply a pi type inhabitant (dependent function) to an argument.
(define (pi-apply f a)
  (f a))

;;; make-pi-fn : (∀a:A. B(a)) → (Π (x:A) B(x))
;;; Construct a pi term from a Scheme function.
(define (make-pi-fn f)
  f)

;;; pi-along : (I → J) × Family(I) → Family(J)
;;; Universal quantification along f.
;;; (Π_f P)(j) represents "forall i. f(i)=j → P(i)"
(define (pi-along f fam-over-i j-type)
  (make-family j-type
               (lambda (j)
                 ;; The type at j is the pi over preimage
                 ;; Π(i : I | f(i) = j). family-at(P, i)
                 (list 'Π-fiber j f (family-index fam-over-i)
                       (lambda (i) (family-at fam-over-i i))))))

;;; ============================================================
;;; Part 3: Beck-Chevalley Condition
;;; ============================================================
;;;
;;; For a pullback square:
;;;
;;;     I' --g'--> J'
;;;     |          |
;;;    f'          f
;;;     v          v
;;;     I ---g---> J
;;;
;;; The Beck-Chevalley condition states:
;;;   g'* ∘ Σ_f ≅ Σ_f' ∘ g*
;;;   g'* ∘ Π_f ≅ Π_f' ∘ g*
;;;
;;; This says substitution commutes with quantification along pullback squares.
;;; This is automatic in many categories (locally cartesian closed categories).

;;; ====
;;; Frobenius Reciprocity
;;; ====
;;;
;;; For the Σ adjunction:
;;;   Σ_f(f*(Q) ∧ P) ≅ Q ∧ Σ_f(P)
;;;
;;; This is the categorical form of the logical equivalence:
;;;   ∃x. (Q ∧ P(x)) ↔ Q ∧ ∃x. P(x)   (when Q doesn't depend on x)

;;; ============================================================
;;; Part 4: Curry-Howard Correspondence
;;; ============================================================
;;;
;;; The adjunctions above give rise to the Curry-Howard correspondence:
;;;
;;; | Logic          | Type Theory      | Category Theory  |
;;; |----------------|------------------|------------------|
;;; | Proposition    | Type             | Object           |
;;; | Proof          | Term             | Morphism         |
;;; | True (⊤)       | Unit             | Terminal object  |
;;; | False (⊥)      | Void             | Initial object   |
;;; | And (∧)        | Product (×)      | Product          |
;;; | Or (∨)         | Sum (+)          | Coproduct        |
;;; | Implies (→)    | Function (→)     | Exponential      |
;;; | Forall (∀)     | Pi (Π)           | Right adjoint    |
;;; | Exists (∃)     | Sigma (Σ)        | Left adjoint     |
;;;
;;; The fact that × ⊣ Δ ⊣ + means:
;;;   - Proofs of A∧B are exactly pairs of proofs
;;;   - Proofs of A∨B are exactly tagged proofs (left or right)
;;;
;;; The fact that Σ ⊣ f* ⊣ Π means:
;;;   - Proofs of ∃x.P(x) are pairs (witness, proof)
;;;   - Proofs of ∀x.P(x) are functions from witnesses to proofs

;;; ====
;;; Logical Connective Constructors
;;; ====

;;; conj-intro : A × B → A∧B
;;; Conjunction introduction (pair)
(define (conj-intro a b)
  (make-pair-obj a b))

;;; conj-elim-left : A∧B → A
;;; Conjunction elimination (left projection)
(define (conj-elim-left ab)
  (pair-fst ab))

;;; conj-elim-right : A∧B → B
;;; Conjunction elimination (right projection)
(define (conj-elim-right ab)
  (pair-snd ab))

;;; disj-intro-left : A → A∨B
;;; Disjunction introduction (left injection)
(define (disj-intro-left a)
  (make-left a))

;;; disj-intro-right : B → A∨B
;;; Disjunction introduction (right injection)
(define (disj-intro-right b)
  (make-right b))

;;; disj-elim : A∨B × (A → C) × (B → C) → C
;;; Disjunction elimination (case analysis)
(define (disj-elim ab f g)
  (if (left? ab)
      (f (from-left ab))
      (g (from-right ab))))

;;; exists-intro : A × B(a) → ∃x:A.B(x)
;;; Existential introduction (dependent pair)
(define exists-intro make-sigma-pair)

;;; exists-elim : (∃x:A.B(x)) × (∀x:A. B(x) → C) → C
;;; Existential elimination (with proof-irrelevant conclusion)
(define (exists-elim witness-proof f)
  (let ([witness (sigma-fst witness-proof)]
        [proof (sigma-snd witness-proof)])
    (f witness proof)))

;;; forall-intro : (∀x:A. B(x)) → Π(x:A).B(x)
;;; Universal introduction (lambda)
(define forall-intro make-pi-fn)

;;; forall-elim : (Π(x:A).B(x)) × A → B(a)
;;; Universal elimination (application)
(define forall-elim pi-apply)

;;; ============================================================
;;; Exports Summary
;;; ============================================================
;;;
;;; Pair category operations:
;;;   make-pair-obj, pair-fst, pair-snd, make-pair-mor
;;;
;;; Diagonal functor:
;;;   diagonal-obj, diagonal-mor, functor-diagonal
;;;
;;; Product/Coproduct functors:
;;;   functor-product, functor-coproduct
;;;   make-left, make-right, left?, right?, from-left, from-right
;;;
;;; Adjunctions:
;;;   adj-diagonal-product (Δ ⊣ ×)
;;;   adj-coproduct-diagonal (+ ⊣ Δ)
;;;
;;; Dependent type operations:
;;;   make-family, family?, family-index, family-at
;;;   subst-family, make-subst-functor
;;;   sigma-along, pi-along
;;;   make-sigma-type, make-sigma-pair, sigma-fst, sigma-snd
;;;   make-pi-type, make-pi-fn, pi-apply
;;;
;;; Logical connectives:
;;;   conj-intro, conj-elim-left, conj-elim-right
;;;   disj-intro-left, disj-intro-right, disj-elim
;;;   exists-intro, exists-elim
;;;   forall-intro, forall-elim
