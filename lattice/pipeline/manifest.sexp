;;; lattice/pipeline/manifest.sexp — Agent Pipeline Skill Manifest

(skill pipeline
  (version "0.1.0")
  (tier 2)
  (path "lattice/pipeline")
  (purity partial)
  (stability experimental)
  (fuel-bound "O(n) for sequential stages, O(1) per effect, unbounded for LLM calls")
  (deps (data fp))

  (description
   "Agent workflow pipelines using an arrow-based stage abstraction. Provides
    composable stages with monadic/arrow combinators, effect handling (LLM, shell,
    store, HTTP, git), council-based multi-model deliberation, and a DSL for
    defining pipelines declaratively.")

  (keywords (pipeline agent workflow arrow monad effects llm council
             deliberation multi-model automation orchestration))
  (aliases (workflow agent-pipeline stages arrows))

  (exports
   ;; No exports annotated with (doc 'export #t) yet
   )

  (modules
   (stage "stage.ss" "Core stage abstraction with arrow combinators")
   (effects "effects.ss" "Effect definitions for LLM, shell, store, HTTP, git")
   (council "council.ss" "Multi-model council deliberation")
   (dsl "dsl.ss" "Declarative pipeline DSL")
   (context "context.ss" "Pipeline context, state, and configuration")))
