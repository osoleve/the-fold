;;; skills/markdown.sexp — Skill curation file

(skill markdown
  (description
    "Markdown parser and HTML renderer using parser combinators.\n    Parses CommonMark-subset markdown into an S-expression AST,\n    then renders to HTML. Dogfooding the FP toolkit.")

  (keywords (markdown parser html renderer documentation dsl))

  (aliases (md markdown-parser))

  (concepts (markdown-processing))

  (modules
    ast
    inline-parser
    block-parser
    html
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
