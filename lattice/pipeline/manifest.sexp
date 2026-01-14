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
   (stage stage-ok stage-err stage-retry stage-skip stage-halt stage-await
          make-stage run-stage stage-pure stage-ask stage-fail
          stage-compose stage->>> stage-<<< stage-first stage-second
          stage-fanout stage-&&& stage-left stage-right stage-choice
          stage-bind stage-map stage-sequence stage-traverse stage-fold
          stage-if stage-when stage-unless stage-case stage-guard
          stage-catch stage-recover stage-default pipeline)
   (effects llm llm-with-system llm-json llm-structured fold-eval fold-call
            shell shell-with-stdin store-put store-get store-has
            log-info log-warn log-error checkpoint restore
            http-get http-post http-json git-status git-diff git-commit git-push
            bbs-create bbs-create-full bbs-update bbs-close bbs-ready bbs-show
            discord-post discord-chat discord-reply await-discord-mention)
   (council make-council-config council-sequential council-parallel
            council-vote council-debate council-consensus
            extract-consensus count-votes majority-position
            architectural-council quick-review-council diverse-perspectives)
   (dsl define-pipeline* named-stage stage* --> <-- pipe-into
        parse-json to-json split-lines join-lines
        on-success on-failure try-all retry with-retry-policy gate
        map-stage filter-stage reduce-stage parallel race all-of any-of
        fsm-pipeline ask-llm run-fold run-shell chain branch switch tap log)
   (context make-pipeline-context empty-context ctx-with-config ctx-with-persona
            ctx-with-fuel ctx-env-ref make-pipeline-state empty-state
            make-persona make-retry-policy retry-exponential retry-linear))

  (modules
   (stage "stage.ss" "Core stage abstraction with arrow combinators")
   (effects "effects.ss" "Effect definitions for LLM, shell, store, HTTP, git")
   (council "council.ss" "Multi-model council deliberation")
   (dsl "dsl.ss" "Declarative pipeline DSL")
   (context "context.ss" "Pipeline context, state, and configuration")))
