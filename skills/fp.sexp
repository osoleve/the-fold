;;; skills/fp.sexp — Skill curation file

(skill fp
  (description
    "A comprehensive functional programming library implementing Haskell-style\ntype classes and abstractions in Scheme. Uses dictionary-passing style\nfor type class polymorphism, maintaining purity and explicit dependencies.\n\nKey design principles:\n- Type classes as first-class values (dictionaries)\n- Explicit dictionary passing (no global instances)\n- Composability through higher-order functions\n- Lawful abstractions verified via property-based testing")

  (keywords (functional-programming type-classes monads parsers streams logic-programming protocols units-of-measure algebraic-effects))

  (aliases (fp functional))

  (concepts (functional-programming type-classes monadic-programming logic-programming formal-languages))

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
