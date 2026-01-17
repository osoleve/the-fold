((name "category")
 (purpose "Category theory abstractions for functional programming")
 (description
  "Categorical structures and transformations including natural transformations,
   functor morphisms, and compositional operations. Builds on the functor and
   type class infrastructure in lattice/fp/templates.ss and core/types/kinds.ss.")

 (modules
  ((natural-transform.ss "Natural transformations between functors with composition")))

 (key-concepts
  ((natural-transformation "Morphism between functors: η : F ⟹ G with components η_A : F(A) → G(A)")
   (naturality-condition "G(f) ∘ η_A = η_B ∘ F(f) for all f : A → B")
   (vertical-composition "η ∘ ε: compose transformations F ⟹ G ⟹ H")
   (horizontal-composition "η * ε: Godement product for functor composition")
   (whiskering "Compose transformation with functor: F ▷ η and η ◁ G")
   (natural-isomorphism "Invertible natural transformation")))

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
    nat-transform->string)))

 (future-work
  ((adjunctions "Unit/counit formulation, hom-set bijection")
   (functor-categories "Categories with functors as objects, nat transforms as morphisms")
   (yoneda "Yoneda lemma and embedding")
   (kan-extensions "Left and right Kan extensions"))))
