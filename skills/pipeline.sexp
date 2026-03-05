;;; skills/pipeline.sexp — Skill curation file

(skill pipeline
  (description
    "Agent workflow pipelines using an arrow-based stage abstraction. Provides\n    composable stages with monadic/arrow combinators, effect handling (LLM, shell,\n    store, HTTP, git), council-based multi-model deliberation, and a DSL for\n    defining pipelines declaratively.")

  (keywords (pipeline agent workflow arrow monad effects llm council deliberation multi-model automation orchestration))

  (aliases (workflow agent-pipeline stages arrows))

  (concepts (agent-orchestration))

  (modules
    stage
    effects
    council
    dsl
    context
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
