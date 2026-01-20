((name "category")
 (purpose "Category theory abstractions for functional programming")
 (description
  "Categorical structures and transformations including natural transformations,
   functor morphisms, Kan extensions, and compositional operations. Builds on
   the functor and type class infrastructure in lattice/fp/templates.ss and
   core/types/kinds.ss.")

 (modules
  ((natural-transform.ss "Natural transformations between functors with composition")
   (adjunction.ss "Adjoint functors, triangle identities, and Galois connections")
   (free-algebra.ss "Generalized Free -| Forgetful adjunctions for algebraic signatures")
   (logic-adjunction.ss "Logical connectives as adjunctions: × ⊣ Δ ⊣ + and Σ ⊣ f* ⊣ Π")
   (monad-derivation.ss "Unified monad derivation from adjunctions (F -| G yields monad on G.F)")
   (comonad.ss "Comonad type class, Store/Env/Traced comonads, adjunction derivation")
   (kan-extension.ss "Left/Right Kan extensions and Codensity monad")
   (effect-category.ss "Categorical foundations for algebraic effects: Free Effect ⊣ Forgetful")
   (state-store-adjunction.ss "State-Store adjunction relating State monad and Store comonad")))

 (key-concepts
  ((natural-transformation "Morphism between functors: eta : F ==> G with components eta_A : F(A) -> G(A)")
   (naturality-condition "G(f) . eta_A = eta_B . F(f) for all f : A -> B")
   (vertical-composition "eta . eps: compose transformations F ==> G ==> H")
   (horizontal-composition "eta * eps: Godement product for functor composition")
   (whiskering "Compose transformation with functor: F |> eta and eta <| G")
   (natural-isomorphism "Invertible natural transformation")
   (adjunction "F -| G: Unit eta and Counit eps satisfying triangle identities")
   (triangle-identities "(eps <| F) . (F |> eta) = id_F and (G |> eps) . (eta <| G) = id_G")
   (galois-connection "Adjunction between preorders (monotone maps)")
   (comonad "Dual of monad: extract/extend/duplicate for context decomposition")
   (store-comonad "Store s a = (s->a, s): position in space with universal access")
   (env-comonad "Env e a = (e, a): value with environment (dual of Reader)")
   (traced-comonad "Traced m a = m->a: function from monoidal accumulator")
   (monad-from-adjunction "Every adjunction F -| G yields monad: return=eta, join=G(eps_F)")
   (free-algebra "Term algebra over generators from a carrier set, modulo equational laws")
   (algebraic-signature "Operations (with arities) + equational laws defining an algebraic theory")
   (free-forgetful-adjunction "Free -| Forgetful: builds term algebra from set, forgets to carrier")
   (right-kan-extension "(Ran K F) a = forall b. (a -> K b) -> F b")
   (left-kan-extension "(Lan K F) a = exists b. (K b -> a, F b)")
   (codensity-monad "Ran_Id M: gives O(1) amortized bind for any monad M")
   ;; Logic as adjunctions
   (diagonal-functor "Δ : C → C×C sending A ↦ (A, A) and f ↦ (f, f)")
   (product-adjunction "× ⊣ Δ: product as right adjoint to diagonal")
   (coproduct-adjunction "+ ⊣ Δ: coproduct as left adjoint to diagonal")
   (curry-howard "Logic ↔ Type Theory ↔ Category Theory correspondence")
   (substitution-functor "f* : Fam(J) → Fam(I): reindexing along f : I → J")
   (sigma-adjunction "Σ ⊣ f*: existential quantification as left adjoint")
   (pi-adjunction "f* ⊣ Π: universal quantification as right adjoint")
   (beck-chevalley "Substitution commutes with quantification along pullbacks")
   ;; Effect category concepts
   (effect-signature-as-algebra "Effect signatures are algebraic signatures with operations as term constructors")
   (handler-as-algebra "A handler is an algebra over the effect signature: return-case + effect-cases")
   (free-effect-adjunction "Free_Σ ⊣ Forgetful_Σ where Free builds Eff terms, Forgetful extracts carrier")
   (handler-composition-adjunction "Nesting handlers h₁(h₂(eff)) = composing adjunctions F₂∘F₁ ⊣ G₁∘G₂")
   (effect-rows-functors "Effect rows as objects in functor category [Set,Set], extension as coproduct injection")
   (continuations-kan "Captured continuations k : Response → Eff e a are left Kan extensions Lan_η")))

 (dependencies (fp/templates fp/meta/combinators base))

 (exports
  ((natural-transform.ss
    make-nat-transform nat-transform? nat-transform-name
    nat-transform-source nat-transform-target nat-transform-component
    nat-apply nat-id
    nat-compose nat-compose2
    nat-horizontal nat-horizontal2
    nat-whisker-right nat-whisker-right2
    nat-whisker-left nat-whisker-left2
    make-nat-iso nat-iso? nat-iso-forward nat-iso-inverse
    check-naturality verify-naturality
    nat-head nat-singleton nat-maybe-to-either nat-either-to-maybe
    nat-concat nat-pure-list nat-pure-maybe
    nat-transform->string)
   (adjunction.ss
    make-adjunction adjunction? adjunction-name
    adjunction-left adjunction-right adjunction-unit adjunction-counit
    functor-id
    verify-triangle-left verify-triangle-right verify-adjunction
    adjunction-transpose-left adjunction-transpose-right
    adjunction-compose
    adj-free-list
    make-galois galois? galois-lower galois-upper
    galois-closure galois-kernel galois-floor-ceil
    adjunction->string)
   (free-algebra.ss
    ;; Signatures (algebraic theories)
    make-signature signature? signature-name
    signature-operations signature-laws signature-op-arity signature-has-op?
    ;; Algebras (concrete implementations)
    make-algebra algebra? algebra-signature algebra-carrier algebra-ops algebra-op
    ;; Algebra validation
    validate-algebra algebra-valid? make-validated-algebra
    ;; Algebra homomorphisms
    make-algebra-hom algebra-hom? algebra-hom-source algebra-hom-target
    algebra-hom-function algebra-hom-apply verify-homomorphism
    compose-algebra-hom identity-algebra-hom
    ;; Terms (syntax trees in free algebra)
    make-gen gen? gen-value term-op? term?
    ;; Term operations
    free-fmap normalize-term eval-term eval-in-algebra make-algebra-evaluator
    ;; Adjunction construction
    make-free-functor make-forgetful-functor forget-carrier
    make-free-unit make-free-counit make-free-adjunction
    ;; Pre-built signatures
    sig-magma sig-semigroup sig-monoid sig-commutative-monoid sig-group sig-abelian-group
    ;; Pre-built algebras
    alg-list-monoid alg-sum-monoid alg-product-monoid alg-integer-group
    ;; Pre-built adjunctions
    adj-free-magma adj-free-semigroup adj-free-monoid adj-free-group
    ;; Display
    signature->string algebra->string term->string)
   (logic-adjunction.ss
    ;; Pair category operations
    make-pair-obj pair-fst pair-snd make-pair-mor
    ;; Diagonal functor
    diagonal-obj diagonal-mor functor-diagonal
    ;; Product/coproduct functors
    functor-product functor-coproduct
    make-left make-right left? right? from-left from-right
    ;; Adjunctions
    adj-diagonal-product adj-coproduct-diagonal
    unit-diagonal-product counit-diagonal-product
    unit-coproduct-diagonal counit-coproduct-diagonal
    ;; Dependent type families
    make-family family? family-index family-at
    subst-family make-subst-functor
    sigma-along pi-along
    make-sigma-type make-sigma-pair sigma-fst sigma-snd
    make-pi-type make-pi-fn pi-apply
    ;; Curry-Howard logical connectives
    conj-intro conj-elim-left conj-elim-right
    disj-intro-left disj-intro-right disj-elim
    exists-intro exists-elim forall-intro forall-elim)
   (monad-derivation.ss
    ;; MonadOps record
    make-monad-ops monad-ops? monad-ops-name
    monad-ops-return monad-ops-fmap monad-ops-join monad-ops-bind
    ;; Core derivation
    monad-from-adjunction
    ;; Example derivations
    monad-list-derived
    make-state-adjunction adj-state-example monad-state-derived
    ;; State-like utilities
    run-state eval-state exec-state
    ;; Law verification
    verify-left-identity verify-right-identity verify-associativity
    verify-monad-laws verify-functor-identity verify-functor-composition
    ;; Display
    monad-ops->string)
   (comonad.ss
    make-comonad comonad? comonad-functor comonad-extract comonad-extend
    duplicate-with extract-with extend-with coflatmap =>>
    comonad-from-adjunction
    ;; Store comonad
    make-store store? store-accessor store-position
    store-peek store-seek store-seeks store-experiment
    store-extract store-extend store-duplicate
    store-functor store-comonad
    ;; Env comonad
    make-env env? env-environment env-value env-ask env-local
    env-extract env-extend env-duplicate env-functor env-comonad
    ;; Traced comonad
    make-traced traced? traced-run traced-monoid run-traced
    traced-extract traced-extend traced-duplicate
    traced-functor traced-comonad
    ;; Copeek operations (for composition)
    store-copeek env-copeek
    ;; Law verification
    verify-comonad-law-1 verify-comonad-law-2 verify-comonad-law-3
    verify-comonad-laws
    ;; Composition (requires distributive law)
    compose-comonads-with-dist* compose-comonads-with-dist
    compose-comonads  ; DEPRECATED - use compose-comonads-with-dist
    ;; Display
    comonad->string store->string env->string)
   (effect-category.ss
    ;; Effect signatures as algebraic signatures
    effect-sig-to-signature
    sig-effect-state sig-effect-reader sig-effect-writer
    sig-effect-exception sig-effect-nondet
    ;; Handlers and algebras (bidirectional conversion)
    handler-to-algebra algebra-to-handler
    ;; Counit evaluation (explicit algebra-parameterized evaluation)
    evaluate-counit evaluate-with-algebra
    ;; Effect adjunctions
    make-effect-adjunction
    adj-effect-state adj-effect-reader adj-effect-writer
    adj-effect-exception adj-effect-nondet
    ;; Adjunction composition
    compose-effect-adjunctions handlers-commute?
    ;; Effect rows as functors
    make-row-functor row-extension-morphism
    ;; Continuations as Kan extensions
    continuation-as-kan kan-extension? kan-resume kan-factor
    ;; Verification
    verify-effect-unit verify-effect-naturality
    ;; Display
    effect-adjunction->string)
   (kan-extension.ss
    ;; Right Kan Extension
    make-ran ran? ran-k ran-f ran-computation ran-apply
    ran-fmap functor-ran ran-lift
    ;; Left Kan Extension
    make-lan lan? lan-k lan-f lan-morphism lan-value
    lan-fmap functor-lan lan-inject lan-lower
    ;; Codensity Monad
    make-codensity codensity? codensity-return-fn codensity-run
    codensity-return codensity-bind codensity-map
    codensity-lift codensity-lower
    ;; Codensity List
    codensity-list-singleton codensity-list-append codensity-list-lower
    ;; True Difference Lists (O(1) append)
    make-dlist dlist? dlist-prepend dlist-empty
    dlist-singleton dlist-from-list dlist-append
    dlist-cons dlist-snoc dlist-to-list dlist-concat
    ;; Codensity Maybe
    codensity-maybe-return codensity-maybe-bind codensity-maybe-fail
    ;; Generic Builder
    make-codensity-monad codensity-monad-return codensity-monad-bind
    codensity-monad-lift codensity-monad-lower)))

 (theory-notes
  ((kan-extensions-universal
    "Kan extensions are the most universal constructions in category theory.
     Every concept in category theory (limits, colimits, adjoints, ends, coends)
     can be expressed as a Kan extension.")
   (codensity-performance
    "The Codensity monad transforms any monad M into an equivalent monad with
     O(1) amortized bind. This is exactly what free-queue in free.ss and
     eff-queue in effects.ss implement - they ARE Codensity in disguise.")
   (continuation-queue-pattern
    "Instead of nested bind structure (left-associative, O(n^2)),
     Codensity accumulates continuations in a queue and applies them
     all at once during lowering (right-associative, O(n)).
     The 'queue' is a defunctionalized continuation representation.")
   (free-algebras-and-effects
    "Every algebraic signature yields a Free -| Forgetful adjunction.
     The monad derived from this adjunction is the 'free monad' for that
     algebraic theory. Free Monoid yields List monad, Free Group yields
     formal word monad. This is the foundation of algebraic effects:
     effect signatures are algebraic theories, handlers are algebras.")
   (effect-handler-ordering
    "Handler composition corresponds to adjunction composition: h₁(h₂(eff))
     gives F₂∘F₁ ⊣ G₁∘G₂. The reversal in right adjoints explains why
     effect handler order matters semantically. State-inside-Exception vs
     Exception-inside-State give different rollback behaviors.")
   (logic-as-adjunctions
    "Lawvere's insight: logical connectives arise from adjunctions.
     The diagonal Δ has product (×) as right adjoint and coproduct (+) as left.
     This gives × ⊣ Δ ⊣ + which corresponds to ∧ ⊣ Δ ⊣ ∨ in logic.
     For dependent types, substitution f* has Σ as left adjoint (∃) and
     Π as right adjoint (∀), giving Σ ⊣ f* ⊣ Π. The Curry-Howard
     correspondence emerges directly from these adjunctions.")))

 (future-work
  ((functor-categories "Categories with functors as objects, nat transforms as morphisms")
   (yoneda "Yoneda lemma and embedding")
   (ends-coends "End and coend as universal constructions via Kan")
   (cofree-comonad "Cofree f a = (a, f (Cofree f a)) dual of Free monad"))))
