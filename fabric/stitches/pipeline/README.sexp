((title . "Pipeline Framework")
 (version . "0.1.0")
 (status . "alpha")

 (description . "Composable pipeline framework for multi-stage agent workflows.
Pipelines are pure S-expressions composed via Arrow-style operators.
Effects are interpreted by the shell layer.")

 (modules
  ((stage.ss . "Core stage algebra - StageResult, composition operators")
   (effects.ss . "Effect definitions - LLM, shell, Fold, HTTP, etc.")
   (context.ss . "Pipeline context and state records")
   (council.ss . "Multi-model deliberation primitives")
   (dsl.ss . "User-facing pipeline DSL")))

 (shell-modules
  ((interpreter.ss . "Effect interpreter - executes pipelines with real IO")))

 (key-concepts
  ((Stage . "ctx -> input -> StageResult output")
   (StageResult . "ok | err | retry | skip | halt | await")
   (>>> . "Sequential composition")
   (&&& . "Parallel fanout")
   (||| . "Choice routing")
   (Effect . "Staged IO operation for interpreter")
   (Council . "Multi-model deliberation pattern")))

 (quick-start
  "
;; Load the DSL
(load \"fabric/stitches/pipeline/dsl.ss\")

;; Define a simple pipeline
(define my-pipeline
  (define-pipeline* 'example
    (config (cons 'model 'sonnet))
    (chain
      (log \"Starting\")
      (ask-llm 'sonnet \"Process: ${input}\")
      (log \"Done\"))))

;; Run it
(run-pipeline my-pipeline \"Hello world\")
  ")

 (composition-examples
  ((sequential . "(>>> stage1 stage2 stage3)")
   (parallel . "(&&& score-safety score-depth)")
   (conditional . "(stage-if pred? then-stage else-stage)")
   (error-handling . "(stage-catch handler risky-stage)")
   (mapping . "(map-stage (ask-llm 'haiku \"Summarize: ${input}\"))")))

 (council-patterns
  ((sequential . "(council-sequential '(opus gemini-3) 3 'opus)")
   (parallel . "(council-parallel '(sonnet gemini-3 kimi) 'opus)")
   (debate . "(council-debate 'opus 'gemini-3 'sonnet)")
   (vote . "(council-vote '(opus sonnet haiku) options)")
   (consensus . "(council-consensus '(opus gemini-3) 5)")))

 (effect-types
  ((llm . "Call language models")
   (fold . "Execute Fold expressions")
   (shell . "Run shell commands")
   (store . "Content-addressed storage")
   (log . "Pipeline logging")
   (checkpoint . "Save/restore state")
   (http . "HTTP requests")
   (await . "Wait for external signals")
   (beads . "Issue tracker integration")
   (git . "Git operations")
   (pipeline . "Invoke sub-pipelines")))

 (fsm-example
  "
(define my-fsm
  (fsm-pipeline
   (list
    (list 'start start-stage (cons 'ok 'process))
    (list 'process process-stage
          (cons 'ok 'done)
          (cons 'error 'start))
    (list 'done done-stage (cons 'ok 'accept)))
   'start
   '(accept)))
  ")

 (concrete-pipelines
  ((arxiv.ss . "ArXiv paper ingestion - fetch/filter/summarize/post")
   (feature-dev.ss . "Feature development FSM - plan/dev/QA/test/dogfood/ship")
   (tech-debt.ss . "Tech debt review - periodic code health checks")
   (council.ss . "General council wrapper for deliberation")))

 (testing . "scheme --script fabric/stitches/pipeline/tests/test-stage.ss")

 (dependencies
  ((internal . ("prelude.ss" "fp/combinators.ss"))
   (shell . ("thimble/fs.ss" "thimble/repl-daemon.ss"))))

 (tier . shepherd)
 (authority . "May be modified by Shepherd tier and above"))
