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
   (stage
    ;; StageResult constructors and predicates
    stage-ok stage-err stage-retry stage-skip stage-halt stage-await
    stage-result? stage-result-tag stage-result-value
    stage-ok? stage-err? stage-retry? stage-skip? stage-halt? stage-await?
    stage-err-code stage-err-message stage-err-data
    stage-retry-reason stage-retry-delay
    ;; Stage construction
    make-stage stage? stage-name stage-run-fn run-stage
    stage-pure stage-read stage-ask stage-asks stage-local
    stage-fail stage-halt-with stage-skip-with
    stage-arr stage-arr-ctx
    ;; Arrow composition
    stage-compose stage->>> stage-<<<
    stage-first stage-second stage-split stage-***
    stage-fanout stage-&&&
    stage-left stage-right stage-choice stage-+++
    ;; Conditionals
    stage-if stage-when stage-unless stage-case stage-guard
    ;; Monadic interface
    stage-bind stage-map stage-ap stage-sequence stage-traverse
    ;; Iteration
    stage-fold stage-for-each stage-repeat stage-while
    ;; Effects
    make-effect-stage stage-effect? stage-effect-type
    stage-effect-payload stage-effect-input
    fanout-effect? fanout-effect-left fanout-effect-right
    ;; Utilities
    stage-id stage-const stage-dup stage-swap stage-fst stage-snd
    pipeline stage-tap stage-trace)

   (effects
    ;; Core effect constructor
    effect
    ;; LLM effects
    llm llm-with-system llm-json llm-structured
    ;; Fold effects
    fold-eval fold-call fold-load
    forum-post forum-digest forum-browse
    ;; Shell effects
    shell shell-with-stdin shell-env shell-bg
    ;; Store effects
    store-put store-get store-has store-pin
    ;; Log effects
    log-info log-warn log-error log-debug log-metric
    ;; Checkpoint effects
    checkpoint checkpoint-with restore checkpoint-exists checkpoint-clear
    ;; HTTP effects
    http-get http-post http-json http-with-headers
    ;; Await effects
    await-tag await-file await-timeout await-signal
    ;; BBS effects
    bbs-create bbs-create-full bbs-update bbs-close bbs-ready bbs-show
    ;; Git effects
    git-status git-diff git-commit git-push
    ;; Sub-pipeline effects
    invoke-pipeline invoke-with-fuel spawn-pipeline await-pipeline
    ;; RLM effects
    rlm rlm-with-input rlm-effect?
    ;; Template expansion
    expand-template
    ;; Effect predicates
    llm-effect? fold-effect? shell-effect? store-effect?
    log-effect? checkpoint-effect? http-effect? await-effect?
    bbs-effect? git-effect? pipeline-effect?)

   (council
    make-council-config council-config?
    council-models council-mode council-rounds council-moderator
    council-topic-template council-round-prompts council-synthesis-prompt
    council-require-consensus council-timeout
    make-council-result council-result?
    result-responses result-synthesis result-consensus
    result-dissent result-votes result-history
    council-sequential council-sequential-with-prompts
    council-parallel council-parallel-with-prompt
    council-vote council-debate council-consensus
    with-council-moderator with-council-timeout
    extract-consensus extract-dissent count-votes majority-position
    architectural-council quick-review-council diverse-perspectives
    technical-debate
    council council-then council-on-consensus
    council-escalate-on-no-consensus
    council-effect? council-effect-mode council-effect-config council-effect-topic)

   (context
    make-pipeline-context pipeline-context?
    ctx-config ctx-persona ctx-session-id ctx-run-id ctx-env ctx-fuel
    empty-context
    ctx-with-config ctx-with-persona ctx-with-session ctx-with-run-id
    ctx-with-env ctx-extend-env ctx-with-fuel ctx-consume-fuel
    ctx-env-ref ctx-config-ref
    make-pipeline-state pipeline-state?
    state-log state-artifacts state-checkpoints state-metrics state-cache
    empty-state
    state-add-log state-add-artifact state-set-checkpoint state-get-checkpoint
    state-add-metric state-cache-put state-cache-get
    make-log-entry log-entry-level log-entry-message log-entry-data log-entry-timestamp
    make-run-record run-record?
    run-pipeline-name run-id-of run-input run-output
    run-final-state run-status run-started-at run-finished-at run-parent-id
    make-pipeline-def pipeline-def?
    pipeline-def-name pipeline-def-stage pipeline-def-config
    make-persona persona?
    persona-name persona-system-prompt persona-config persona-tier persona-model persona-channels
    make-cron-schedule make-interval-schedule make-tag-schedule
    make-event-schedule make-manual-schedule
    schedule? schedule-type schedule-spec
    make-retry-policy retry-policy?
    retry-max-attempts retry-delay-fn retry-on-predicate retry-on-exhaust
    retry-none retry-exponential retry-linear retry-immediate
    make-ctx-state ctx-of state-of update-state update-ctx)

   (dsl
    define-pipeline* named-stage stage* config
    with-model with-fuel with-persona with-env
    --> <--
    pipe-into
    parse-json to-json split-lines join-lines
    on-success on-failure try-all retry with-retry-policy gate
    map-stage filter-stage reduce-stage take-stage drop-stage
    parallel race all-of any-of
    fsm-pipeline find-transition
    ask-llm run-fold run-shell post-to fetch-url
    chain branch switch tap
    log debug warn save ask-rlm load-checkpoint))

  (modules
   (stage "stage.ss" "Core stage abstraction with arrow combinators")
   (effects "effects.ss" "Effect definitions for LLM, shell, store, HTTP, git")
   (council "council.ss" "Multi-model council deliberation")
   (dsl "dsl.ss" "Declarative pipeline DSL")
   (context "context.ss" "Pipeline context, state, and configuration")))
