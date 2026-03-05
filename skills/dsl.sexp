;;; skills/dsl.sexp — Skill curation file

(skill dsl
  (description
    "Tools for building domain-specific languages using tagless final style,\n    pattern matching compilation, partial evaluation, functor composition,\n    reader macros, quasiquotation, and multi-stage programming.")

  (keywords (dsl tagless-final pattern-matching partial-evaluation functor-composition reader-macros quasiquote staging meta-programming language-design interpreters))

  (aliases (meta-dsl language-tools tagless))

  (concepts (meta-programming domain-specific-languages partial-evaluation multi-stage-programming))

  (modules
    tagless
    match
    partial-eval
    compose
    reader
    quasi
    staging
    template
    template-zipper
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
