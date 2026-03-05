;;; skills/category.sexp — Skill curation file

(skill category
  (description
    "Category theory foundations applied to programming. Natural transformations,\nadjunctions with triangle identities, Kan extensions, comonads, monad derivation\nfrom adjunctions, abstract interpretation via Galois connections, and an\ninter-category framework with first-class categories and indexed transforms.\nFree algebras bridge to the rewrite system for operational semantics.")

  (keywords (category-theory adjunction natural-transformation kan-extension comonad abstract-interpretation galois-connection monad-derivation free-algebra inter-category))

  (aliases (category cat-theory))

  (concepts (category-theory))

  (modules
    natural-transform
    adjunction
    monad-derivation
    kleisli
    common-monads
    comonad
    free-algebra
    effect-category
    kan-extension
    limits
    logic-adjunction
    state-store-adjunction
    abstract-interp
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
