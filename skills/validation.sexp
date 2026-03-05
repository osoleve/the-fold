;;; skills/validation.sexp — Skill curation file

(skill validation
  (description
    "Content-addressed validation contracts. Contracts are declarative\n    specifications of data shape expressed as S-expressions. Each contract\n    gets a SHA256 hash from its canonical form, enabling versioning,\n    provenance tracking, and CAS storage. Provides checking (predicate)\n    and validation (structured error) modes.")

  (keywords (validation contract content-addressed schema predicate boundary defensive checking))

  (aliases (contracts))

  (concepts (validation-contracts))

  (modules
    contract
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
