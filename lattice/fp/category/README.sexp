((name "category")
 (purpose "Category theory abstractions for functional programming")
 (description
  "Categorical structures and transformations including natural transformations,
   functor morphisms, and compositional operations. Builds on the functor and
   type class infrastructure in lattice/fp/templates.ss and core/types/kinds.ss.")

 (modules
  ((natural-transform.ss "Natural transformations between functors with composition")
   (adjunction.ss "Adjoint functors, triangle identities, and Galois connections")))

 (key-concepts
  ((natural-transformation "Morphism between functors: η : F ⟹ G with components η_A : F(A) → G(A)")
   (naturality-condition "G(f) ∘ η_A = η_B ∘ F(f) for all f : A → B")
   (vertical-composition "η ∘ ε: compose transformations F ⟹ G ⟹ H")
   (horizontal-composition "η * ε: Godement product for functor composition")
   (whiskering "Compose transformation with functor: F ▷ η and η ◁ G")
   (natural-isomorphism "Invertible natural transformation")
   (adjunction "F ⊣ G: Unit η and Counit ε satisfying triangle identities")
   (triangle-identities "(ε ◁ F) ∘ (F ▷ η) = id_F and (G ▷ ε) ∘ (η ◁ G) = id_G")
   (galois-connection "Adjunction between preorders (monotone maps)")))

 (dependencies (fp/templates fp/meta/combinators base))

 (exports
  ((natural-transform.ss
    make-nat-transform nat-transform? nat-transform-name
    nat-transform-source nat-transform-target nat-transform-component
    nat-apply nat-id
    nat-compose nat-∘
    nat-horizontal nat-*
    nat-whisker-right nat-◁
    nat-whisker-left nat-▷
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
    adjunction->string)))

 (future-work
  ((functor-categories "Categories with functors as objects, nat transforms as morphisms")
   (yoneda "Yoneda lemma and embedding")
   (kan-extensions "Left and right Kan extensions"))))