(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

(load "boundary/pipeline/effects/llm.ss")
(load "boundary/pipeline/effects/shell.ss")
(load "boundary/pipeline/effects/http.ss")
(load "boundary/pipeline/effects/fold.ss")
(load "boundary/pipeline/effects/discord.ss")
(load "boundary/pipeline/effects/misc.ss")
(load "boundary/pipeline/checkpoint.ss")

(define-syntax doc
  (syntax-rules ()
    [(_ . rest) (void)]))

(doc 'module 'interpreter)
(doc 'description "Pipeline effect interpreter - the impure shell that executes pipeline effects")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "This is Shell code: handles IO, may fail, contains defensive logic")
(doc 'features '("Effect interpretation (LLM, shell, Fold, HTTP, etc.)"
                 "State management during execution"
                 "Logging and metrics collection"
                 "Checkpoint persistence"
                 "Error recovery"))

(doc 'section 'main-interpreter-entry-point)

(define (run-pipeline pipeline-def input)
  (doc 'description "Execute a pipeline with input, return result and final state")
  (doc 'type '(-> PipelineDef Any (Pair StageResult PipelineState)))
  (doc 'param 'pipeline-def "Pipeline definition")
  (doc 'param 'input "Initial input value")
  (let* ([stage (pipeline-def-stage pipeline-def)]
         [config (pipeline-def-config pipeline-def)]
         [ctx (build-context-from-config config)]
         [state empty-state])
        (interpret-pipeline stage ctx state input)))

(define (run-pipeline-with-context stage ctx input)
  (doc 'description "Execute pipeline with provided context")
  (doc 'type '(-> Stage PipelineContext Any (Pair StageResult PipelineState)))
  (doc 'param 'stage "Pipeline stage")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'input "Initial input")
  (interpret-pipeline stage ctx empty-state input))

(doc 'section 'pipeline-interpretation-loop)

(define (interpret-pipeline stage ctx state input)
  (doc 'description "Main pipeline interpretation loop")
  (doc 'type '(-> Stage Context State Input (Pair Result State)))
  (doc 'param 'stage "Stage to interpret")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (doc 'param 'input "Current input")
  (let ([result (run-stage stage ctx input)])
       (interpret-result result ctx state input)))

(define (interpret-result result ctx state input)
  (doc 'description "Interpret stage result or effect")
  (doc 'type '(-> (Or StageResult Effect) Context State Input (Pair Result State)))
  (doc 'param 'result "Result or effect to interpret")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (doc 'param 'input "Current input")
  (cond
   ;; Pure StageResult - return as-is
   [(stage-result? result)
    (cons result state)]
   
   ;; Stage effect - interpret it
   [(stage-effect? result)
    (interpret-effect result ctx state)]
   
   ;; Council effect - special handling
   [(council-effect? result)
    (interpret-council-effect result ctx state)]
   
   ;; Race effect - parallel execution
   [(and (pair? result) (eq? (car result) 'race-effect))
    (interpret-race-effect result ctx state)]
   
   ;; Unknown - wrap as error
   [else
    (cons (stage-err 'unknown-result
                     "Unrecognized result type"
                     result)
          state)]))

(doc 'section 'effect-interpretation-dispatcher)

(define (interpret-effect effect ctx state)
  (doc 'description "Dispatch effect interpretation to appropriate handler")
  (doc 'type '(-> Effect Context State (Pair Result State)))
  (doc 'param 'effect "Effect to interpret")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (let ([type (stage-effect-type effect)]
        [payload (stage-effect-payload effect)]
        [input (stage-effect-input effect)])
       (case type
             [(llm) (interpret-llm-effect payload ctx state input)]
             [(fold) (interpret-fold-effect payload ctx state input)]
             [(shell) (interpret-shell-effect payload ctx state input)]
             [(store) (interpret-store-effect payload ctx state input)]
             [(log) (interpret-log-effect payload ctx state input)]
             [(checkpoint) (interpret-checkpoint-effect payload ctx state input)]
             [(http) (interpret-http-effect payload ctx state input)]
             [(await) (interpret-await-effect payload ctx state input)]
             [(bbs beads) (interpret-bbs-effect payload ctx state input)]
             [(git) (interpret-git-effect payload ctx state input)]
             [(pipeline) (interpret-pipeline-effect payload ctx state input)]
             [(discord) (interpret-discord-effect payload ctx state input)]
             [else
              (cons (stage-err 'unknown-effect
                               (format "Unknown effect type: ~a" type)
                               effect)
                    state)])))

(doc 'section 'pipeline-effect-interpretation)

(define (interpret-pipeline-effect payload ctx state input)
  (doc 'description "Interpret pipeline nesting effect - allows pipelines to invoke other pipelines")
  (doc 'type '(-> Payload Context State Input (Pair Result State)))
  (doc 'param 'payload "Effect payload")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (doc 'param 'input "Current input")
  (let ([op (car payload)])
       (case op
             [(invoke)
              (let* ([pipeline (cadr payload)]
                     ;; Inherit fuel from parent, minus some for the invocation
                     [child-fuel (max 0 (- (ctx-fuel ctx) 100))]
                     [child-ctx (ctx-with-fuel ctx child-fuel)]
                     [result (interpret-pipeline pipeline child-ctx empty-state input)])
                    ;; Merge child state into parent
                    (let ([child-result (car result)]
                          [child-state (cdr result)])
                         (cons child-result
                               (merge-states state child-state))))]
             [(invoke-fuel)
              (let* ([fuel (cadr payload)]
                     [pipeline (caddr payload)]
                     [child-ctx (ctx-with-fuel ctx fuel)]
                     [result (interpret-pipeline pipeline child-ctx empty-state input)])
                    (cons (car result)
                          (merge-states state (cdr result))))]
             [(spawn)
              ;; Async execution - return job ID
              (let* ([pipeline (cadr payload)]
                     [job-id (generate-job-id)])
                    ;; Queue the pipeline for background execution
                    (queue-pipeline-job job-id pipeline ctx input)
                    (cons (stage-ok job-id) state))]
             [(await)
              (let* ([job-id (cadr payload)]
                     [result (await-pipeline-job job-id)])
                    (if result
                        (cons (stage-ok result) state)
                        (cons (stage-err 'pipeline-timeout
                                         "Timed out waiting for pipeline"
                                         job-id)
                              state)))]
             [else
              (cons (stage-err 'unknown-pipeline-op
                               (format "Unknown pipeline op: ~a" op)
                               payload)
                    state)])))

(doc 'section 'council-effect-interpretation)

(define (interpret-council-effect effect ctx state)
  (doc 'description "Interpret council effect - multi-agent deliberation")
  (doc 'type '(-> CouncilEffect Context State (Pair Result State)))
  (doc 'param 'effect "Council effect")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (let ([mode (council-effect-mode effect)]
        [config (council-effect-config effect)]
        [topic (council-effect-topic effect)])
       (case mode
             [(sequential) (run-sequential-council config topic ctx state)]
             [(parallel) (run-parallel-council config topic ctx state)]
             [(vote) (run-vote-council config topic ctx state)]
             [(debate) (run-debate-council config topic ctx state)]
             [(consensus) (run-consensus-council config topic ctx state)]
             [else
              (cons (stage-err 'unknown-council-mode
                               (format "Unknown council mode: ~a" mode)
                               effect)
                    state)])))

(define (run-sequential-council config topic ctx state)
  (doc 'description "Run sequential council - models take turns in rounds")
  (doc 'type '(-> Config Topic Context State (Pair Result State)))
  (doc 'param 'config "Council configuration")
  (doc 'param 'topic "Discussion topic")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (let ([models (council-models config)]
        [rounds (council-rounds config)]
        [moderator (council-moderator config)]
        [round-prompts (council-round-prompts config)]
        [synthesis-prompt (council-synthesis-prompt config)])
       (let loop ([round 1]
                  [history '()]
                  [current-state state])
            (if (> round rounds)
                ;; All rounds complete, synthesize
                (let ([synthesis-result
                       (call-llm-api moderator
                                     ""
                                     (format "~a\n\nDiscussion:\n~a"
                                             synthesis-prompt
                                             (format-history history)))])
                     (if (llm-response-ok? synthesis-result)
                         (cons (stage-ok (make-council-result
                                          history
                                          (llm-response-text synthesis-result)
                                          #f  ; consensus not checked
                                          '()
                                          '()
                                          history))
                               current-state)
                         (cons (stage-err 'council-synthesis-failed
                                          (llm-response-error synthesis-result)
                                          synthesis-result)
                               current-state)))
                ;; Run this round
                (let ([round-prompt (if (< round (length round-prompts))
                                        (list-ref round-prompts (- round 1))
                                        (car (reverse round-prompts)))])
                     (let round-loop ([remaining-models models]
                                      [round-responses '()]
                                      [s current-state])
                          (if (null? remaining-models)
                              ;; Round complete
                              (loop (+ round 1)
                                    (append history (list (cons round (reverse round-responses))))
                                    s)
                              ;; Get response from next model
                              (let* ([model (car remaining-models)]
                                     [prompt (format "Topic: ~a\n\n~a\n\nPrior responses:\n~a"
                                                     topic
                                                     round-prompt
                                                     (format-responses round-responses))]
                                     [response (call-llm-api model "" prompt)])
                                    (if (llm-response-ok? response)
                                        (round-loop (cdr remaining-models)
                                                    (cons (cons model (llm-response-text response))
                                                          round-responses)
                                                    s)
                                        ;; Model failed, continue with others
                                        (round-loop (cdr remaining-models)
                                                    (cons (cons model "(no response)")
                                                          round-responses)
                                                    s))))))))))

(define (run-parallel-council config topic ctx state)
  (doc 'description "Run parallel council - all models respond simultaneously")
  (doc 'type '(-> Config Topic Context State (Pair Result State)))
  (doc 'param 'config "Council configuration")
  (doc 'param 'topic "Discussion topic")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (let ([models (council-models config)]
        [synthesizer (council-moderator config)]
        [prompt (car (council-round-prompts config))]
        [synthesis-prompt (council-synthesis-prompt config)])
       ;; Get all responses in parallel (sequentially for now)
       (let loop ([remaining models]
                  [responses '()])
            (if (null? remaining)
                ;; All responses collected, synthesize
                (let* ([full-prompt (format "~a\n\nPerspectives:\n~a"
                                            synthesis-prompt
                                            (format-responses (reverse responses)))]
                       [synthesis (call-llm-api synthesizer "" full-prompt)])
                      (if (llm-response-ok? synthesis)
                          (cons (stage-ok (make-council-result
                                           responses
                                           (llm-response-text synthesis)
                                           #f
                                           '()
                                           '()
                                           (list responses)))
                                state)
                          (cons (stage-err 'council-synthesis-failed
                                           (llm-response-error synthesis)
                                           synthesis)
                                state)))
                ;; Get next response
                (let* ([model (car remaining)]
                       [full-prompt (format "~a\n\nTopic: ~a"
                                            (expand-template prompt (list (cons 'topic topic)))
                                            topic)]
                       [response (call-llm-api model "" full-prompt)])
                      (loop (cdr remaining)
                            (cons (cons model
                                        (if (llm-response-ok? response)
                                            (llm-response-text response)
                                            "(no response)"))
                                  responses)))))))

;;; Placeholder implementations for other council modes
(define (run-vote-council config topic ctx state)
  (run-parallel-council config topic ctx state))

(define (run-debate-council config topic ctx state)
  (run-sequential-council config topic ctx state))

(define (run-consensus-council config topic ctx state)
  (run-sequential-council config topic ctx state))

(doc 'section 'race-effect-interpretation)

(define (interpret-race-effect effect ctx state)
  (doc 'description "Interpret race effect - parallel execution with first-to-finish semantics")
  (doc 'type '(-> RaceEffect Context State (Pair Result State)))
  (doc 'param 'effect "Race effect")
  (doc 'param 'ctx "Pipeline context")
  (doc 'param 'state "Current state")
  (let ([stages (list-ref effect 1)]
        [input (list-ref effect 2)])
       ;; For now, just run first stage
       (if (null? stages)
           (cons (stage-err 'race-empty "No stages to race" '()) state)
           (interpret-pipeline (car stages) ctx state input))))

(doc 'section 'helper-functions)

(define (build-context-from-config config)
  (doc 'description "Build pipeline context from configuration alist")
  (doc 'type '(-> Alist PipelineContext))
  (doc 'param 'config "Configuration alist")
  (let ([fuel (or (assq-ref config 'fuel) 10000)]
        [model (or (assq-ref config 'model) 'sonnet)])
       (ctx-extend-env
        (ctx-with-fuel empty-context fuel)
        (list (cons 'default-model model)))))

(define (assq-ref alist key)
  (doc 'description "Lookup key in alist, return value or #f")
  (doc 'type '(-> Alist Symbol (Maybe Any)))
  (doc 'param 'alist "Association list")
  (doc 'param 'key "Key to lookup")
  (let ([entry (assq key alist)])
       (if entry (cdr entry) #f)))

(define (merge-states parent child)
  (doc 'description "Merge child state into parent state")
  (doc 'type '(-> State State State))
  (doc 'param 'parent "Parent state")
  (doc 'param 'child "Child state to merge")
  (make-pipeline-state
   (append (state-log child) (state-log parent))
   (append (state-artifacts child) (state-artifacts parent))
   (append (state-checkpoints child) (state-checkpoints parent))
   (append (state-metrics child) (state-metrics parent))
   (state-cache parent)))  ; Don't merge cache

;;; format-history : List (Round . List (Model . Response)) -> String
(define (format-history history)
  (apply string-append
         (map (lambda (round-entry)
                      (format "Round ~a:\n~a\n"
                              (car round-entry)
                              (format-responses (cdr round-entry))))
              history)))

;;; format-responses : List (Model . Response) -> String
(define (format-responses responses)
  (apply string-append
         (map (lambda (r)
                      (format "  ~a: ~a\n" (car r) (cdr r)))
              responses)))

;;; generate-job-id : -> String
(define (generate-job-id)
  (format "job-~a" (random 1000000)))

;;; queue-pipeline-job : JobId -> Pipeline -> Context -> Input -> ()
(define (queue-pipeline-job job-id pipeline ctx input)
  ;; TODO: Queue for background execution
  (void))

;;; await-pipeline-job : JobId -> Any
(define (await-pipeline-job job-id)
  ;; TODO: Wait for job completion
  #f)
