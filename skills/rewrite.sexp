;;; skills/rewrite.sexp — Skill curation file

(skill rewrite
  (description
    "Term rewriting systems and equational reasoning. Provides pattern matching\nand rewriting engine, algebraic law definitions, fusion optimization rules,\nproof tactics for equational reasoning, program sketching with holes, and\nS-expression zipper navigation. Infrastructure for symbolic manipulation\nused by category theory (free algebras) and DSL template systems.")

  (keywords (term-rewriting pattern-matching proof-tactics fusion equational-reasoning program-sketching rewrite-rules))

  (aliases (rewrite term-rewrite))

  (concepts (rewriting))

  (modules
    rule
    trace
    engine
    rewrite/sexp-zipper
    verify
    goals
    rewrite/laws
    rewrite/fusion-rules
    rewrite/proof-tactics
    rewrite/sketch
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
