;;; shell/pipeline/interpreter.ss — Pipeline Effect Interpreter
;;;
;;; This is the impure shell that executes pipeline effects.
;;; Effects from core/pipeline/effects.ss are interpreted here.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.
;;;
;;; Features:
;;;   - Effect interpretation (LLM, shell, Fold, HTTP, etc.)
;;;   - State management during execution
;;;   - Logging and metrics collection
;;;   - Checkpoint persistence
;;;   - Error recovery
;;;
;;; Dependencies:
;;;   - core/pipeline/stage.ss
;;;   - core/pipeline/effects.ss
;;;   - core/pipeline/context.ss
;;;   - shell/fs.ss (for file operations)
;;;
;;; NOTE: Standard string utilities provided by core/prelude.ss.
;;;       string-rindex is unique to this module.

(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

;;; ============================================================
;;; Interpreter State
;;; ============================================================

;;; Current session for Fold IPC
(define *pipeline-session* (make-parameter "pipeline"))

;;; ============================================================
;;; Main Interpreter Entry Point
;;; ============================================================

;;; run-pipeline : PipelineDef -> Any -> (StageResult . PipelineState)
;;; Execute a pipeline with input, return result and final state.
(define (run-pipeline pipeline-def input)
  (let* ([stage (pipeline-def-stage pipeline-def)]
         [config (pipeline-def-config pipeline-def)]
         [ctx (build-context-from-config config)]
         [state empty-state])
        (interpret-pipeline stage ctx state input)))

;;; run-pipeline-with-context : Stage -> PipelineContext -> Any -> (StageResult . PipelineState)
;;; Execute pipeline with provided context.
(define (run-pipeline-with-context stage ctx input)
  (interpret-pipeline stage ctx empty-state input))

;;; ============================================================
;;; Pipeline Interpretation Loop
;;; ============================================================

;;; interpret-pipeline : Stage -> Context -> State -> Input -> (Result . State)
(define (interpret-pipeline stage ctx state input)
  (let ([result (run-stage stage ctx input)])
       (interpret-result result ctx state input)))

;;; interpret-result : StageResult|Effect -> Context -> State -> Input -> (Result . State)
(define (interpret-result result ctx state input)
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

;;; ============================================================
;;; Effect Interpretation
;;; ============================================================

;;; interpret-effect : Effect -> Context -> State -> (Result . State)
(define (interpret-effect effect ctx state)
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
             [(beads) (interpret-beads-effect payload ctx state input)]
             [(git) (interpret-git-effect payload ctx state input)]
             [(pipeline) (interpret-pipeline-effect payload ctx state input)]
             [(discord) (interpret-discord-effect payload ctx state input)]
             [else
              (cons (stage-err 'unknown-effect
                               (format "Unknown effect type: ~a" type)
                               effect)
                    state)])))

;;; ============================================================
;;; LLM Effect Interpretation
;;; ============================================================

;;; interpret-llm-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-llm-effect payload ctx state input)
  (let ([op (car payload)]
        [model (cadr payload)]
        [prompt-template (caddr payload)])
       (case op
             [(call)
              (let* ([prompt (expand-template-with-ctx prompt-template ctx input)]
                     [system-prompt (get-system-prompt ctx)]
                     [response (call-llm-api model system-prompt prompt)])
                    (if (llm-response-ok? response)
                        (let ([new-state (state-add-log state
                                                        (make-log-entry 'debug
                                                                        (format "LLM ~a called" model)
                                                                        '()))])
                             (cons (stage-ok (llm-response-text response)) new-state))
                        (cons (stage-err 'llm-error
                                         (llm-response-error response)
                                         response)
                              state)))]
             [(call-with-system)
              (let* ([system-prompt (caddr payload)]
                     [user-prompt (cadddr payload)]
                     [expanded-user (expand-template-with-ctx user-prompt ctx input)]
                     [response (call-llm-api model system-prompt expanded-user)])
                    (if (llm-response-ok? response)
                        (cons (stage-ok (llm-response-text response)) state)
                        (cons (stage-err 'llm-error
                                         (llm-response-error response)
                                         response)
                              state)))]
             [(call-json)
              (let* ([prompt (expand-template-with-ctx prompt-template ctx input)]
                     [system-prompt (string-append (get-system-prompt ctx)
                                                   "\n\nRespond with valid JSON only.")]
                     [response (call-llm-api model system-prompt prompt)])
                    (if (llm-response-ok? response)
                        (let ([parsed (parse-json-string (llm-response-text response))])
                             (if parsed
                                 (cons (stage-ok parsed) state)
                                 (cons (stage-err 'json-parse-error
                                                  "Failed to parse LLM response as JSON"
                                                  (llm-response-text response))
                                       state)))
                        (cons (stage-err 'llm-error
                                         (llm-response-error response)
                                         response)
                              state)))]
             [else
              (cons (stage-err 'unknown-llm-op
                               (format "Unknown LLM operation: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Fold Effect Interpretation
;;; ============================================================

;;; interpret-fold-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-fold-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(eval)
              (let* ([expr-template (cadr payload)]
                     [expr (expand-template-with-ctx expr-template ctx input)]
                     [result (fold-ipc-eval expr)])
                    (if (fold-result-ok? result)
                        (cons (stage-ok (fold-result-value result)) state)
                        (cons (stage-err 'fold-error
                                         (fold-result-error result)
                                         result)
                              state)))]
             [(call)
              (let* ([fn-name (cadr payload)]
                     [args (caddr payload)]
                     [expr (format "(~a ~a)" fn-name
                                   (apply string-append
                                          (map (lambda (a) (format " ~s" a)) args)))]
                     [result (fold-ipc-eval expr)])
                    (if (fold-result-ok? result)
                        (cons (stage-ok (fold-result-value result)) state)
                        (cons (stage-err 'fold-error
                                         (fold-result-error result)
                                         result)
                              state)))]
             [(load)
              (let* ([path (cadr payload)]
                     [result (fold-ipc-eval (format "(load ~s)" path))])
                    (if (fold-result-ok? result)
                        (cons (stage-ok '()) state)
                        (cons (stage-err 'fold-load-error
                                         (fold-result-error result)
                                         result)
                              state)))]
             [(forum-post)
              (let* ([channel (cadr payload)]
                     [title (caddr payload)]
                     [body (cadddr payload)]
                     [expr (format "(msg '~a ~s ~s)" channel title body)]
                     [result (fold-ipc-eval expr)])
                    (if (fold-result-ok? result)
                        (cons (stage-ok (fold-result-value result)) state)
                        (cons (stage-err 'forum-error
                                         (fold-result-error result)
                                         result)
                              state)))]
             [else
              (cons (stage-err 'unknown-fold-op
                               (format "Unknown Fold operation: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Shell Effect Interpretation
;;; ============================================================

;;; interpret-shell-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-shell-effect payload ctx state input)
  (let ([op (car payload)]
        [cmd-template (cadr payload)])
       (case op
             [(run)
              (let* ([cmd (expand-template-with-ctx cmd-template ctx input)]
                     [result (shell-exec cmd)])
                    (if (shell-result-ok? result)
                        (let ([new-state (state-add-log state
                                                        (make-log-entry 'debug
                                                                        (format "Shell: ~a" cmd)
                                                                        '()))])
                             (cons (stage-ok (shell-result-stdout result)) new-state))
                        (cons (stage-err 'shell-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(run-stdin)
              (let* ([cmd (expand-template-with-ctx cmd-template ctx input)]
                     [result (shell-exec-with-stdin cmd input)])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'shell-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(run-env)
              (let* ([env-vars (cadr payload)]
                     [cmd (expand-template-with-ctx (caddr payload) ctx input)]
                     [result (shell-exec-with-env env-vars cmd)])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'shell-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [else
              (cons (stage-err 'unknown-shell-op
                               (format "Unknown shell operation: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Log Effect Interpretation
;;; ============================================================

;;; interpret-log-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-log-effect payload ctx state input)
  (let ([level (car payload)]
        [message (cadr payload)])
       (let* ([expanded (expand-template-with-ctx message ctx input)]
              [entry (make-log-entry level expanded input)]
              [new-state (state-add-log state entry)])
             ;; Also write to pipeline log file
             (write-pipeline-log entry)
             (cons (stage-ok input) new-state))))

;;; ============================================================
;;; Checkpoint Effect Interpretation
;;; ============================================================

;;; interpret-checkpoint-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-checkpoint-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(save)
              (let* ([name (cadr payload)]
                     [new-state (state-set-checkpoint state name input)]
                     [run-id (ctx-run-id ctx)])
                    ;; Persist to CAS
                    (when run-id
                          (persist-checkpoint run-id name input))
                    (cons (stage-ok input) new-state))]
             [(save-value)
              (let* ([name (cadr payload)]
                     [value (caddr payload)]
                     [new-state (state-set-checkpoint state name value)])
                    (cons (stage-ok input) new-state))]
             [(restore)
              (let* ([name (cadr payload)]
                     [value (state-get-checkpoint state name)])
                    (if value
                        (cons (stage-ok value) state)
                        ;; Try loading from CAS
                        (let ([run-id (ctx-run-id ctx)])
                             (if run-id
                                 (let ([persisted (load-checkpoint run-id name)])
                                      (if persisted
                                          (cons (stage-ok persisted) state)
                                          (cons (stage-err 'checkpoint-not-found
                                                           (format "No checkpoint: ~a" name)
                                                           name)
                                                state)))
                                 (cons (stage-err 'checkpoint-not-found
                                                  (format "No checkpoint: ~a" name)
                                                  name)
                                       state)))))]
             [(exists)
              (let* ([name (cadr payload)]
                     [value (state-get-checkpoint state name)])
                    (cons (stage-ok (if value #t #f)) state))]
             [(clear)
              (let* ([name (cadr payload)]
                     [new-state (state-set-checkpoint state name #f)])
                    (cons (stage-ok '()) new-state))]
             [else
              (cons (stage-err 'unknown-checkpoint-op
                               (format "Unknown checkpoint op: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; HTTP Effect Interpretation
;;; ============================================================

;;; interpret-http-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-http-effect payload ctx state input)
  (let ([op (car payload)]
        [url-template (cadr payload)])
       (case op
             [(get)
              (let* ([url (expand-template-with-ctx url-template ctx input)]
                     [result (http-fetch-get url)])
                    (if (http-result-ok? result)
                        (cons (stage-ok (http-result-body result)) state)
                        (cons (stage-err 'http-error
                                         (http-result-error result)
                                         result)
                              state)))]
             [(get-json)
              (let* ([url (expand-template-with-ctx url-template ctx input)]
                     [result (http-fetch-get url)])
                    (if (http-result-ok? result)
                        (let ([parsed (parse-json-string (http-result-body result))])
                             (if parsed
                                 (cons (stage-ok parsed) state)
                                 (cons (stage-err 'json-parse-error
                                                  "Failed to parse HTTP response as JSON"
                                                  (http-result-body result))
                                       state)))
                        (cons (stage-err 'http-error
                                         (http-result-error result)
                                         result)
                              state)))]
             [else
              (cons (stage-err 'unknown-http-op
                               (format "Unknown HTTP op: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Beads Effect Interpretation
;;; ============================================================

;;; interpret-beads-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-beads-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(create)
              (let* ([title (cadr payload)]
                     [result (shell-exec (format "bd create ~s" title))])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'beads-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(create-full)
              (let* ([title (cadr payload)]
                     [description (caddr payload)]
                     [type (cadddr payload)]
                     [priority (list-ref payload 4)]
                     [cmd (format "bd create ~s -d ~s -t ~a -p ~a"
                                  title description type priority)]
                     [result (shell-exec cmd)])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'beads-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(close)
              (let* ([id (cadr payload)]
                     [result (shell-exec (format "bd close ~a" id))])
                    (if (shell-result-ok? result)
                        (cons (stage-ok '()) state)
                        (cons (stage-err 'beads-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(ready)
              (let ([result (shell-exec "bd ready --json")])
                   (if (shell-result-ok? result)
                       (let ([parsed (parse-json-string (shell-result-stdout result))])
                            (cons (stage-ok (or parsed '())) state))
                       (cons (stage-err 'beads-error
                                        (shell-result-stderr result)
                                        result)
                             state)))]
             [else
              (cons (stage-err 'unknown-beads-op
                               (format "Unknown beads op: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Git Effect Interpretation
;;; ============================================================

;;; interpret-git-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-git-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(status)
              (let ([result (shell-exec "git status --porcelain")])
                   (if (shell-result-ok? result)
                       (cons (stage-ok (shell-result-stdout result)) state)
                       (cons (stage-err 'git-error
                                        (shell-result-stderr result)
                                        result)
                             state)))]
             [(diff)
              (let ([result (shell-exec "git diff")])
                   (if (shell-result-ok? result)
                       (cons (stage-ok (shell-result-stdout result)) state)
                       (cons (stage-err 'git-error
                                        (shell-result-stderr result)
                                        result)
                             state)))]
             [(commit)
              (let* ([message (cadr payload)]
                     [result (shell-exec (format "git add -A && git commit -m ~s" message))])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'git-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(push)
              (let ([result (shell-exec "git push")])
                   (if (shell-result-ok? result)
                       (cons (stage-ok '()) state)
                       (cons (stage-err 'git-error
                                        (shell-result-stderr result)
                                        result)
                             state)))]
             [else
              (cons (stage-err 'unknown-git-op
                               (format "Unknown git op: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Pipeline Effect Interpretation (Nesting)
;;; ============================================================

;;; interpret-pipeline-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-pipeline-effect payload ctx state input)
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

;;; ============================================================
;;; Council Effect Interpretation
;;; ============================================================

;;; interpret-council-effect : CouncilEffect -> Context -> State -> (Result . State)
(define (interpret-council-effect effect ctx state)
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

;;; run-sequential-council : Config -> Topic -> Context -> State -> (Result . State)
(define (run-sequential-council config topic ctx state)
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

;;; run-parallel-council : Config -> Topic -> Context -> State -> (Result . State)
(define (run-parallel-council config topic ctx state)
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

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; expand-template-with-ctx : String -> Context -> Input -> String
(define (expand-template-with-ctx template ctx input)
  (let ([bindings (append (list (cons "input" input))
                          (map (lambda (p) (cons (symbol->string (car p)) (cdr p)))
                               (ctx-env ctx)))])
       (expand-template template bindings)))

;;; get-system-prompt : Context -> String
(define (get-system-prompt ctx)
  (let ([persona (ctx-persona ctx)])
       (if persona
           (persona-system-prompt persona)
           "")))

;;; build-context-from-config : Alist -> PipelineContext
(define (build-context-from-config config)
  (let ([fuel (or (assq-ref config 'fuel) 10000)]
        [model (or (assq-ref config 'model) 'sonnet)])
       (ctx-extend-env
        (ctx-with-fuel empty-context fuel)
        (list (cons 'default-model model)))))

;;; assq-ref : Alist -> Symbol -> Any
(define (assq-ref alist key)
  (let ([entry (assq key alist)])
       (if entry (cdr entry) #f)))

;;; merge-states : State -> State -> State
(define (merge-states parent child)
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

;;; ============================================================
;;; External API Stubs (to be implemented)
;;; ============================================================

;;; *llm-api-key-file* : String
;;; Path to file containing Anthropic API key
(define *llm-api-key-file* ".env.agents")

;;; get-anthropic-api-key : -> String | #f
;;; Read the Anthropic API key from .env.agents file.
(define (get-anthropic-api-key)
  (guard (ex [else #f])
         (if (file-exists? *llm-api-key-file*)
             (call-with-input-file *llm-api-key-file*
                                   (lambda (p)
                                           (let loop ()
                                                (let ([line (get-line p)])
                                                     (cond
                                                      [(eof-object? line) #f]
                                                      [(and (>= (string-length line) 18)
                                                            (string=? (substring line 0 18) "ANTHROPIC_API_KEY="))
                                                       (substring line 18 (string-length line))]
                                                      [else (loop)])))))
             #f)))

;;; model-symbol->api-model : Symbol -> String
;;; Convert model symbol to Anthropic API model ID.
(define (model-symbol->api-model sym)
  (case sym
        [(opus) "claude-opus-4-20250514"]
        [(sonnet) "claude-sonnet-4-20250514"]
        [(haiku) "claude-3-5-haiku-20241022"]
        [(claude-3-opus) "claude-3-opus-20240229"]
        [(claude-3-sonnet) "claude-3-5-sonnet-20241022"]
        [(claude-3-haiku) "claude-3-5-haiku-20241022"]
        [else (symbol->string sym)]))

;;; call-llm-api : Symbol -> String -> String -> LLMResponse
;;; Call the Anthropic Claude API with given model, system prompt, and user prompt.
;;; Returns LLMResponse: (llm-response ok? text error)
(define (call-llm-api model system-prompt user-prompt)
  (let ([api-key (get-anthropic-api-key)])
       (if (not api-key)
           (list 'llm-response #f #f "No ANTHROPIC_API_KEY found in .env.agents")
           (guard (ex [else
                       (list 'llm-response #f #f
                             (format "LLM API error: ~a"
                                     (if (message-condition? ex)
                                         (condition-message ex)
                                         "unknown error")))])
                  (let* ([api-model (model-symbol->api-model model)]
                         ;; Build JSON request body
                         [request-body (format "{\"model\": ~s, \"max_tokens\": 4096, \"system\": ~s, \"messages\": [{\"role\": \"user\", \"content\": ~s}]}"
                                               api-model
                                               (json-escape system-prompt)
                                               (json-escape user-prompt))]
                         ;; Call curl with the request
                         [result (shell-exec-with-stdin
                                  (format "curl -sS -X POST https://api.anthropic.com/v1/messages -H 'Content-Type: application/json' -H 'x-api-key: ~a' -H 'anthropic-version: 2023-06-01' -d @-"
                                          api-key)
                                  request-body)])
                        (if (shell-result-ok? result)
                            (let ([response-json (shell-result-stdout result)])
                                 ;; Extract the text from the response
                                 (let ([text (extract-llm-response-text response-json)])
                                      (if text
                                          (list 'llm-response #t text #f)
                                          ;; Try to extract error
                                          (let ([error (extract-llm-error response-json)])
                                               (list 'llm-response #f #f (or error "Failed to parse response"))))))
                            (list 'llm-response #f #f (shell-result-stderr result))))))))

;;; json-escape : String -> String
;;; Escape a string for JSON (handle quotes, newlines, backslashes).
(define (json-escape s)
  (let loop ([chars (string->list s)]
             [result '()])
       (if (null? chars)
           (list->string (reverse result))
           (let ([c (car chars)])
                (cond
                 [(char=? c #\") (loop (cdr chars) (append '(#\" #\\) result))]
                 [(char=? c #\\) (loop (cdr chars) (append '(#\\ #\\) result))]
                 [(char=? c #\newline) (loop (cdr chars) (append (list #\n #\\) result))]
                 [(char=? c #\return) (loop (cdr chars) (append (list #\r #\\) result))]
                 [(char=? c #\tab) (loop (cdr chars) (append (list #\t #\\) result))]
                 [else (loop (cdr chars) (cons c result))])))))

;;; extract-llm-response-text : String -> String | #f
;;; Extract the text content from an Anthropic API response JSON.
;;; Looks for: "content": [{"type": "text", "text": "..."}]
(define (extract-llm-response-text json-str)
  (let ([text-start (string-search json-str "\"text\": \"")])
       (if text-start
           (let* ([content-start (+ text-start 9)]
                  [content-end (find-json-string-end json-str content-start)])
                 (if content-end
                     (json-unescape (substring json-str content-start content-end))
                     #f))
           #f)))

;;; extract-llm-error : String -> String | #f
;;; Extract error message from API response.
(define (extract-llm-error json-str)
  (let ([error-start (string-search json-str "\"message\": \"")])
       (if error-start
           (let* ([msg-start (+ error-start 12)]
                  [msg-end (find-json-string-end json-str msg-start)])
                 (if msg-end
                     (json-unescape (substring json-str msg-start msg-end))
                     #f))
           #f)))

;;; string-search : String -> String -> Integer | #f
;;; Find first occurrence of needle in haystack.
(define (string-search haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) i]
             [else (loop (+ i 1))]))))

;;; find-json-string-end : String -> Integer -> Integer | #f
;;; Find the end of a JSON string starting at pos (after opening quote).
;;; Handles escape sequences.
(define (find-json-string-end str start)
  (let ([len (string-length str)])
       (let loop ([i start])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) #\\)
              ;; Skip escaped character
              (loop (+ i 2))]
             [(char=? (string-ref str i) #\")
              i]
             [else (loop (+ i 1))]))))

;;; json-unescape : String -> String
;;; Unescape a JSON string (handle \\, \", \n, \r, \t).
(define (json-unescape s)
  (let ([len (string-length s)])
       (let loop ([i 0]
                  [result '()])
            (cond
             [(>= i len) (list->string (reverse result))]
             [(and (char=? (string-ref s i) #\\) (< (+ i 1) len))
              (let ([next (string-ref s (+ i 1))])
                   (case next
                         [(#\n) (loop (+ i 2) (cons #\newline result))]
                         [(#\r) (loop (+ i 2) (cons #\return result))]
                         [(#\t) (loop (+ i 2) (cons #\tab result))]
                         [(#\" #\\) (loop (+ i 2) (cons next result))]
                         [else (loop (+ i 2) (cons next result))]))]
             [else (loop (+ i 1) (cons (string-ref s i) result))]))))

(define (llm-response-ok? r) (list-ref r 1))
(define (llm-response-text r) (list-ref r 2))
(define (llm-response-error r) (list-ref r 3))

;;; fold-ipc-eval : String -> FoldResult
;;; Evaluate an expression via the Fold REPL daemon IPC.
;;; Uses file-based IPC: write to requests/, poll responses/
(define (fold-ipc-eval expr)
  (let ([session-id (*pipeline-session*)]
        [req-dir ".fold-repl/requests"]
        [resp-dir ".fold-repl/responses"])
       (guard (ex [else
                   (list 'fold-result #f #f
                         (format "fold-ipc-eval error: ~a"
                                 (if (message-condition? ex)
                                     (condition-message ex)
                                     "unknown error")))])
              (let ([req-path (string-append req-dir "/" session-id ".ss")]
                    [resp-path (string-append resp-dir "/" session-id ".txt")])
                   ;; Clear any stale response
                   (when (file-exists? resp-path)
                         (delete-file resp-path))
                   ;; Write request
                   (call-with-output-file req-path
                                          (lambda (p)
                                                  (display (format "~s"
                                                                   `((session-id . ,session-id)
                                                                     (expression . ,expr)
                                                                     (timestamp . ,(time-second (current-time)))))
                                                           p)))
                   ;; Poll for response (max 30 seconds)
                   (let loop ([attempts 300])
                        (cond
                         [(file-exists? resp-path)
                          (let ([response (call-with-input-file resp-path get-string-all)])
                               (delete-file resp-path)
                               ;; Check if response indicates error
                               (if (and (>= (string-length response) 6)
                                        (string=? (substring response 0 6) "ERROR:"))
                                   (list 'fold-result #f #f response)
                                   (list 'fold-result #t response #f)))]
                         [(= attempts 0)
                          (list 'fold-result #f #f "Timeout waiting for daemon response")]
                         [else
                          (sleep (make-time 'time-duration 100000000 0))  ; 100ms
                          (loop (- attempts 1))]))))))

(define (fold-result-ok? r) (list-ref r 1))
(define (fold-result-value r) (list-ref r 2))
(define (fold-result-error r) (list-ref r 3))

;;; shell-exec : String -> ShellResult
;;; Execute a shell command and return result with stdout/stderr.
;;; Uses Chez Scheme's open-process-ports for subprocess handling.
(define (shell-exec cmd)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         (let-values ([(to-stdin from-stdout from-stderr process-id)
                       (open-process-ports cmd
                                           (buffer-mode block)
                                           (native-transcoder))])
                     (close-port to-stdin)
                     (let ([stdout-all (get-string-all from-stdout)]
                           [stderr-all (get-string-all from-stderr)])
                          (close-port from-stdout)
                          (close-port from-stderr)
                          ;; Convert eof objects to empty strings
                          (let ([stdout-str (if (eof-object? stdout-all) "" stdout-all)]
                                [stderr-str (if (eof-object? stderr-all) "" stderr-all)])
                               ;; Consider it successful if stderr is empty
                               (list 'shell-result
                                     (string=? stderr-str "")
                                     stdout-str
                                     stderr-str))))))

;;; shell-exec-with-stdin : String -> String -> ShellResult
;;; Execute a shell command with stdin input.
(define (shell-exec-with-stdin cmd stdin-content)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec-with-stdin error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         (let-values ([(to-stdin from-stdout from-stderr process-id)
                       (open-process-ports cmd
                                           (buffer-mode block)
                                           (native-transcoder))])
                     ;; Write stdin content
                     (put-string to-stdin stdin-content)
                     (close-port to-stdin)
                     (let ([stdout-all (get-string-all from-stdout)]
                           [stderr-all (get-string-all from-stderr)])
                          (close-port from-stdout)
                          (close-port from-stderr)
                          (let ([stdout-str (if (eof-object? stdout-all) "" stdout-all)]
                                [stderr-str (if (eof-object? stderr-all) "" stderr-all)])
                               (list 'shell-result
                                     (string=? stderr-str "")
                                     stdout-str
                                     stderr-str))))))

;;; shell-exec-with-env : Alist -> String -> ShellResult
;;; Execute a shell command with environment variables.
;;; env is an alist of (name . value) pairs.
(define (shell-exec-with-env env cmd)
  (let ([env-prefix (apply string-append
                           (map (lambda (pair)
                                        (format "~a=~a " (car pair) (cdr pair)))
                                env))])
       (shell-exec (string-append "env " env-prefix cmd))))

(define (shell-result-ok? r) (list-ref r 1))
(define (shell-result-stdout r) (list-ref r 2))
(define (shell-result-stderr r) (list-ref r 3))

;;; http-fetch-get : String -> HTTPResult
;;; Fetch content from a URL using curl.
;;; Returns HTTPResult with body on success, error on failure.
(define (http-fetch-get url)
  (guard (ex [else
              (list 'http-result #f #f
                    (format "http-fetch-get error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Use curl with silent mode and fail on HTTP errors
         (let ([result (shell-exec (format "curl -sS -f ~s" url))])
              (if (shell-result-ok? result)
                  (list 'http-result #t (shell-result-stdout result) #f)
                  (list 'http-result #f #f (shell-result-stderr result))))))

(define (http-result-ok? r) (list-ref r 1))
(define (http-result-body r) (list-ref r 2))
(define (http-result-error r) (list-ref r 3))

;;; parse-json-string : String -> Any
;;; Parse a JSON string into Scheme data structures.
;;; Objects become alists, arrays become lists.
(define (parse-json-string s)
  (guard (ex [else #f])
         (if (or (not s) (string=? s ""))
             #f
             (let ([trimmed (string-trim s)])
                  (if (string=? trimmed "")
                      #f
                      (let-values ([(result rest) (parse-json-value trimmed 0)])
                                  result))))))

;;; string-trim : String -> String
;;; Remove leading/trailing whitespace.
(define (string-trim s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                     (if (and (< i len) (char-whitespace? (string-ref s i)))
                         (loop (+ i 1))
                         i))]
         [end (let loop ([i (- len 1)])
                   (if (and (>= i start) (char-whitespace? (string-ref s i)))
                       (loop (- i 1))
                       (+ i 1)))])
        (if (>= start end)
            ""
            (substring s start end))))

;;; parse-json-value : String -> Integer -> (Values Any Integer)
;;; Parse a JSON value starting at position, return value and end position.
(define (parse-json-value s pos)
  (let ([pos (skip-whitespace s pos)])
       (if (>= pos (string-length s))
           (values #f pos)
           (let ([c (string-ref s pos)])
                (cond
                 [(char=? c #\{) (parse-json-object s pos)]
                 [(char=? c #\[) (parse-json-array s pos)]
                 [(char=? c #\") (parse-json-string-value s pos)]
                 [(or (char=? c #\-) (char-numeric? c)) (parse-json-number s pos)]
                 [(char=? c #\t) (parse-json-true s pos)]
                 [(char=? c #\f) (parse-json-false s pos)]
                 [(char=? c #\n) (parse-json-null s pos)]
                 [else (values #f pos)])))))

;;; skip-whitespace : String -> Integer -> Integer
(define (skip-whitespace s pos)
  (let ([len (string-length s)])
       (let loop ([i pos])
            (if (and (< i len) (char-whitespace? (string-ref s i)))
                (loop (+ i 1))
                i))))

;;; parse-json-object : String -> Integer -> (Values Alist Integer)
(define (parse-json-object s pos)
  (let ([pos (+ pos 1)])  ; skip '{'
       (let loop ([pos (skip-whitespace s pos)]
                  [result '()])
            (if (or (>= pos (string-length s)) (char=? (string-ref s pos) #\}))
                (values (reverse result) (+ pos 1))
                (let-values ([(key pos2) (parse-json-string-value s pos)])
                            (let ([pos3 (skip-whitespace s pos2)])
                                 (if (and (< pos3 (string-length s)) (char=? (string-ref s pos3) #\:))
                                     (let-values ([(val pos4) (parse-json-value s (+ pos3 1))])
                                                 (let ([pos5 (skip-whitespace s pos4)])
                                                      (if (and (< pos5 (string-length s)) (char=? (string-ref s pos5) #\,))
                                                          (loop (skip-whitespace s (+ pos5 1))
                                                                (cons (cons (string->symbol key) val) result))
                                                          (loop pos5 (cons (cons (string->symbol key) val) result)))))
                                     (values (reverse result) pos3))))))))

;;; parse-json-array : String -> Integer -> (Values List Integer)
(define (parse-json-array s pos)
  (let ([pos (+ pos 1)])  ; skip '['
       (let loop ([pos (skip-whitespace s pos)]
                  [result '()])
            (if (or (>= pos (string-length s)) (char=? (string-ref s pos) #\]))
                (values (reverse result) (+ pos 1))
                (let-values ([(val pos2) (parse-json-value s pos)])
                            (let ([pos3 (skip-whitespace s pos2)])
                                 (if (and (< pos3 (string-length s)) (char=? (string-ref s pos3) #\,))
                                     (loop (skip-whitespace s (+ pos3 1)) (cons val result))
                                     (loop pos3 (cons val result)))))))))

;;; parse-json-string-value : String -> Integer -> (Values String Integer)
(define (parse-json-string-value s pos)
  (let ([pos (+ pos 1)])  ; skip opening quote
       (let loop ([i pos]
                  [chars '()])
            (if (>= i (string-length s))
                (values (list->string (reverse chars)) i)
                (let ([c (string-ref s i)])
                     (cond
                      [(char=? c #\")
                       (values (list->string (reverse chars)) (+ i 1))]
                      [(char=? c #\\)
                       (if (< (+ i 1) (string-length s))
                           (let ([next (string-ref s (+ i 1))])
                                (case next
                                      [(#\n) (loop (+ i 2) (cons #\newline chars))]
                                      [(#\r) (loop (+ i 2) (cons #\return chars))]
                                      [(#\t) (loop (+ i 2) (cons #\tab chars))]
                                      [(#\" #\\ #\/) (loop (+ i 2) (cons next chars))]
                                      [else (loop (+ i 2) (cons next chars))]))
                           (values (list->string (reverse chars)) i))]
                      [else (loop (+ i 1) (cons c chars))]))))))

;;; parse-json-number : String -> Integer -> (Values Number Integer)
(define (parse-json-number s pos)
  (let ([len (string-length s)])
       (let loop ([i pos]
                  [chars '()])
            (if (and (< i len)
                     (let ([c (string-ref s i)])
                          (or (char-numeric? c)
                              (char=? c #\-)
                              (char=? c #\+)
                              (char=? c #\.)
                              (char=? c #\e)
                              (char=? c #\E))))
                (loop (+ i 1) (cons (string-ref s i) chars))
                (values (string->number (list->string (reverse chars))) i)))))

;;; parse-json-true : String -> Integer -> (Values #t Integer)
(define (parse-json-true s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "true"))
      (values #t (+ pos 4))
      (values #f pos)))

;;; parse-json-false : String -> Integer -> (Values #f Integer)
(define (parse-json-false s pos)
  (if (and (<= (+ pos 5) (string-length s))
           (string=? (substring s pos (+ pos 5)) "false"))
      (values #f (+ pos 5))
      (values #f pos)))

;;; parse-json-null : String -> Integer -> (Values '() Integer)
(define (parse-json-null s pos)
  (if (and (<= (+ pos 4) (string-length s))
           (string=? (substring s pos (+ pos 4)) "null"))
      (values '() (+ pos 4))
      (values #f pos)))

;;; *pipeline-log-dir* : String
;;; Directory for pipeline logs.
(define *pipeline-log-dir* "logs/pipelines")

;;; *checkpoint-dir* : String
;;; Directory for pipeline checkpoints.
(define *checkpoint-dir* ".fold-checkpoints")

;;; write-pipeline-log : LogEntry -> ()
;;; Append a log entry to the pipeline log file.
(define (write-pipeline-log entry)
  (guard (ex [else (void)])  ; Silently fail if logging fails
         (ensure-directory! *pipeline-log-dir*)
         (let* ([timestamp (get-timestamp)]
                [log-file (string-append *pipeline-log-dir* "/pipeline.log")]
                [level (log-entry-level entry)]
                [message (log-entry-message entry)])
               (call-with-output-file log-file
                                      (lambda (p)
                                              (display (format "[~a] [~a] ~a\n" timestamp level message) p))
                                      '(append)))))

;;; persist-checkpoint : RunId -> Name -> Value -> ()
;;; Persist a checkpoint value to disk.
;;; Checkpoints are stored as: .fold-checkpoints/<run-id>/<name>.sexp
(define (persist-checkpoint run-id name value)
  (guard (ex [else (void)])  ; Silently fail if persistence fails
         (let ([run-dir (string-append *checkpoint-dir* "/" run-id)])
              (ensure-directory! run-dir)
              (let ([checkpoint-file (string-append run-dir "/" (symbol->string name) ".sexp")])
                   (call-with-output-file checkpoint-file
                                          (lambda (p)
                                                  (pretty-print value p)))))))

;;; load-checkpoint : RunId -> Name -> Any
;;; Load a checkpoint value from disk.
;;; Returns #f if checkpoint doesn't exist.
(define (load-checkpoint run-id name)
  (guard (ex [else #f])
         (let ([checkpoint-file (string-append *checkpoint-dir* "/" run-id "/" (symbol->string name) ".sexp")])
              (if (file-exists? checkpoint-file)
                  (call-with-input-file checkpoint-file read)
                  #f))))

;;; queue-pipeline-job : JobId -> Pipeline -> Context -> Input -> ()
(define (queue-pipeline-job job-id pipeline ctx input)
  ;; TODO: Queue for background execution
  (void))

;;; await-pipeline-job : JobId -> Any
(define (await-pipeline-job job-id)
  ;; TODO: Wait for job completion
  #f)

;;; interpret-await-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-await-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(timeout)
              ;; Pause execution for specified milliseconds
              (let* ([ms (cadr payload)]
                     [ns (* ms 1000000)])  ; Convert ms to nanoseconds
                    (sleep (make-time 'time-duration ns 0))
                    (cons (stage-ok input) state))]
             [(forum-tag)
              ;; TODO: Poll forum for tag
              (cons (stage-await (cadr payload)) state)]
             [(file)
              ;; TODO: Wait for file
              (cons (stage-await (list 'file (cadr payload))) state)]
             [(signal)
              (cons (stage-await (list 'signal (cadr payload))) state)]
             ;; Discord await operations
             [(discord-mention)
              ;; Wait for @agent mention in Discord
              ;; The daemon polls for trigger files written by bot.js
              (let* ([agent-name (cadr payload)]
                     [trigger-pattern (format "~a-discord-trigger" agent-name)])
                    ;; Return await result for daemon to poll
                    (cons (stage-await (list 'discord-mention agent-name)) state))]
             [(discord-reaction)
              ;; Wait for specific reaction on a message
              (let ([message-id (cadr payload)]
                    [emoji (caddr payload)])
                   (cons (stage-await (list 'discord-reaction message-id emoji)) state))]
             [(discord-reply)
              ;; Wait for reply to a message
              (let ([message-id (cadr payload)])
                   (cons (stage-await (list 'discord-reply message-id)) state))]
             [else
              (cons (stage-err 'unknown-await-op
                               (format "Unknown await op: ~a" op)
                               payload)
                    state)])))

;;; *cas-store-dir* : String
;;; Directory for content-addressed storage.
(define *cas-store-dir* ".store")

;;; cas-hash : Any -> String
;;; Compute a hash of a value for CAS storage.
;;; Uses a simple string hash for now.
(define (cas-hash value)
  (let* ([str (format "~s" value)]
         [hash (simple-string-hash str)])
        (format "~16,'0x" hash)))

;;; simple-string-hash : String -> Integer
;;; A simple hash function for strings.
(define (simple-string-hash str)
  (let ([len (string-length str)])
       (let loop ([i 0]
                  [hash 5381])
            (if (>= i len)
                (modulo hash (expt 2 64))
                (loop (+ i 1)
                      (+ (* hash 33) (char->integer (string-ref str i))))))))

;;; cas-path-for-hash : String -> String
;;; Get the file path for a hash in the CAS.
(define (cas-path-for-hash hash)
  (let ([prefix (substring hash 0 2)])
       (string-append *cas-store-dir* "/" prefix "/" hash)))

;;; cas-persist : Any -> String
;;; Store a value in CAS and return its hash.
(define (cas-persist value)
  (let* ([hash (cas-hash value)]
         [path (cas-path-for-hash hash)]
         [dir (path-directory path)])
        (ensure-directory! dir)
        (call-with-output-file path
                               (lambda (p)
                                       (pretty-print value p)))
        hash))

;;; cas-load : String -> Any | #f
;;; Load a value from CAS by hash.
(define (cas-load hash)
  (let ([path (cas-path-for-hash hash)])
       (if (file-exists? path)
           (guard (ex [else #f])
                  (call-with-input-file path read))
           #f)))

;;; cas-has? : String -> Boolean
;;; Check if a hash exists in CAS.
(define (cas-has? hash)
  (file-exists? (cas-path-for-hash hash)))

;;; cas-get : String -> Any | #f
;;; Alias for cas-load.
(define (cas-get hash)
  (cas-load hash))

;;; interpret-store-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-store-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(put)
              ;; Store input in CAS, return hash
              (guard (ex [else
                          (cons (stage-err 'store-error
                                           (format "Failed to store: ~a"
                                                   (if (message-condition? ex)
                                                       (condition-message ex)
                                                       "unknown error"))
                                           input)
                                state)])
                     (let ([hash (cas-persist input)])
                          (cons (stage-ok hash) state)))]
             [(get)
              (let* ([hash (cadr payload)]
                     [value (cas-load hash)])
                    (if value
                        (cons (stage-ok value) state)
                        (cons (stage-err 'store-not-found
                                         (format "Hash not found: ~a" hash)
                                         hash)
                              state)))]
             [(has)
              (let ([hash (cadr payload)])
                   (cons (stage-ok (cas-has? hash)) state))]
             [(pin)
              ;; Pin is a no-op for now (no garbage collection)
              (cons (stage-ok '()) state)]
             [else
              (cons (stage-err 'unknown-store-op
                               (format "Unknown store op: ~a" op)
                               payload)
                    state)])))

;;; interpret-race-effect : RaceEffect -> Context -> State -> (Result . State)
(define (interpret-race-effect effect ctx state)
  (let ([stages (list-ref effect 1)]
        [input (list-ref effect 2)])
       ;; For now, just run first stage
       (if (null? stages)
           (cons (stage-err 'race-empty "No stages to race" '()) state)
           (interpret-pipeline (car stages) ctx state input))))

;;; ============================================================
;;; Discord Effect Interpretation
;;; ============================================================
;;;
;;; Discord effects write to the outbox directory where bridge.js watches.
;;; The outbox path is: .fold-repl/discord-outbox/*.json
;;;
;;; Each file contains a JSON object with:
;;;   - channel: Fold channel name ('engineering, 'philosophy, etc.)
;;;   - title: Optional post title (null for chat)
;;;   - body: Message content
;;;   - author: Agent/user name
;;;   - tier: Fold tier (shepherd, builder, player)
;;;   - timestamp: ISO timestamp
;;;   - discord_message_id: Optional, for replies
;;;   - embed: Optional embed spec

;;; Discord outbox path relative to project root
(define *discord-outbox-dir* ".fold-repl/discord-outbox")

;;; interpret-discord-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-discord-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(post)
              (let* ([channel (cadr payload)]
                     [title (caddr payload)]
                     [body (cadddr payload)]
                     [expanded-body (expand-template-with-ctx body ctx input)])
                    (discord-queue-post channel
                                        title
                                        expanded-body
                                        ctx)
                    (cons (stage-ok '()) state))]
             [(post-embed)
              (let* ([channel (cadr payload)]
                     [embed-spec (caddr payload)])
                    (discord-queue-embed channel embed-spec ctx)
                    (cons (stage-ok '()) state))]
             [(chat)
              (let* ([channel (cadr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-post channel #f body ctx)
                    (cons (stage-ok '()) state))]
             [(reply)
              (let* ([message-id (cadr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-reply message-id body ctx)
                    (cons (stage-ok '()) state))]
             [(react)
              (let* ([message-id (cadr payload)]
                     [emoji (caddr payload)])
                    (discord-queue-react message-id emoji ctx)
                    (cons (stage-ok '()) state))]
             [(thread)
              (let* ([message-id (cadr payload)]
                     [thread-name (caddr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-thread message-id thread-name body ctx)
                    ;; Thread creation returns thread-id (mock for now)
                    (cons (stage-ok (format "thread-~a" message-id)) state))]
             [(dm)
              (let* ([user-id (cadr payload)]
                     [body (if (string? input)
                               input
                               (format "~a" input))])
                    (discord-queue-dm user-id body ctx)
                    (cons (stage-ok '()) state))]
             [else
              (cons (stage-err 'unknown-discord-op
                               (format "Unknown Discord operation: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Discord Queue Helpers
;;; ============================================================
;;; These write JSON files to the outbox for bridge.js to pick up.

;;; discord-queue-post : Symbol -> Maybe String -> String -> Context -> ()
;;; Queue a post (titled or chat) for Discord.
(define (discord-queue-post channel title body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [tier (get-agent-tier ctx)]
         [post-data `((channel . ,(symbol->string channel))
                      (title . ,title)
                      (body . ,body)
                      (author . ,author)
                      (tier . ,tier)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-embed : Symbol -> Alist -> Context -> ()
;;; Queue an embed post for Discord.
(define (discord-queue-embed channel embed-spec ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [tier (get-agent-tier ctx)]
         [post-data `((channel . ,(symbol->string channel))
                      (embed . ,embed-spec)
                      (author . ,author)
                      (tier . ,tier)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-reply : String -> String -> Context -> ()
;;; Queue a reply to a Discord message.
(define (discord-queue-reply message-id body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [post-data `((reply_to . ,message-id)
                      (body . ,body)
                      (author . ,author)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-react : String -> String -> Context -> ()
;;; Queue a reaction to a Discord message.
(define (discord-queue-react message-id emoji ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [post-data `((react_to . ,message-id)
                      (emoji . ,emoji)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-thread : String -> String -> String -> Context -> ()
;;; Queue thread creation from a message.
(define (discord-queue-thread message-id thread-name body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [post-data `((create_thread_from . ,message-id)
                      (thread_name . ,thread-name)
                      (body . ,body)
                      (author . ,author)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; discord-queue-dm : String -> String -> Context -> ()
;;; Queue a direct message.
(define (discord-queue-dm user-id body ctx)
  (let* ([outbox-file (make-outbox-filename)]
         [author (get-agent-name ctx)]
         [post-data `((dm_to . ,user-id)
                      (body . ,body)
                      (author . ,author)
                      (timestamp . ,(current-iso-timestamp)))])
        (write-outbox-json outbox-file post-data)))

;;; make-outbox-filename : -> String
;;; Generate unique filename for outbox JSON.
(define (make-outbox-filename)
  (let ([timestamp (current-milliseconds)]
        [random-suffix (random 100000)])
       (format "~a/~a-~a.json" *discord-outbox-dir* timestamp random-suffix)))

;;; write-outbox-json : String -> Alist -> ()
;;; Write alist as JSON to outbox file.
(define (write-outbox-json path data)
  ;; Ensure outbox directory exists
  (let ([dir (path-directory path)])
       (unless (file-exists? dir)
               (make-directories dir)))
  ;; Write JSON
  (with-output-to-file path
                       (lambda ()
                               (display (alist->json data)))))

;;; alist->json : Alist -> String
;;; Convert alist to JSON string (simple implementation).
(define (alist->json alist)
  (string-append
   "{\n"
   (apply string-append
          (intersperse
           ",\n"
           (map (lambda (pair)
                        (format "  ~s: ~a"
                                (symbol->string (car pair))
                                (json-value (cdr pair))))
                alist)))
   "\n}"))

;;; json-value : Any -> String
;;; Convert value to JSON representation.
(define (json-value v)
  (cond
   [(string? v) (format "~s" v)]
   [(number? v) (format "~a" v)]
   [(boolean? v) (if v "true" "false")]
   [(null? v) "null"]
   [(symbol? v) (format "~s" (symbol->string v))]
   [(pair? v)
    (if (and (pair? (car v)) (symbol? (caar v)))
        ;; Nested alist
        (alist->json v)
        ;; List/array
        (string-append
         "["
         (apply string-append
                (intersperse ", " (map json-value v)))
         "]"))]
   [else (format "~s" (format "~a" v))]))

;;; intersperse : String -> List String -> List String
(define (intersperse sep lst)
  (cond
   [(null? lst) '()]
   [(null? (cdr lst)) lst]
   [else (cons (car lst)
               (cons sep (intersperse sep (cdr lst))))]))

;;; get-agent-name : Context -> String
;;; Get agent name from context.
(define (get-agent-name ctx)
  (let ([persona (ctx-persona ctx)])
       (if persona
           (persona-name persona)
           "pipeline")))

;;; get-agent-tier : Context -> String
;;; Get agent tier from context.
(define (get-agent-tier ctx)
  (let ([persona (ctx-persona ctx)])
       (if persona
           (symbol->string (persona-tier persona))
           "builder")))

;;; get-timestamp : -> String
;;; Get current time as a human-readable timestamp.
(define (get-timestamp)
  (let* ([t (current-time)]
         [seconds (time-second t)])
        ;; Return as ISO-style timestamp
        (format "~a" seconds)))

;;; current-iso-timestamp : -> String
;;; Get current time in ISO 8601 format.
(define (current-iso-timestamp)
  (let* ([t (current-time)]
         [d (time-utc->date t)])
        (format "~a-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
                (date-year d)
                (date-month d)
                (date-day d)
                (date-hour d)
                (date-minute d)
                (date-second d))))

;;; current-milliseconds : -> Integer
;;; Get current time in milliseconds since epoch.
(define (current-milliseconds)
  (let ([t (current-time)])
       (+ (* (time-second t) 1000)
          (quotient (time-nanosecond t) 1000000))))

;;; path-directory : String -> String
;;; Get directory portion of path.
(define (path-directory path)
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path 0 idx)
           ".")))

;;; string-rindex : String -> Char -> Maybe Integer
;;; Find last occurrence of char in string.
(define (string-rindex str ch)
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) ch) i]
        [else (loop (- i 1))])))

;;; directory-exists? : String -> Boolean
;;; Check if a directory exists.
(define (directory-exists? path)
  (file-directory? path))

;;; ensure-directory! : String -> ()
;;; Create directory and all parent directories if they don't exist.
(define (ensure-directory! path)
  (unless (file-exists? path)
          (let ([parent (path-directory path)])
               (when (and (not (string=? parent "."))
                          (not (string=? parent path)))
                     (ensure-directory! parent)))
          (guard (ex [else (void)])  ; Ignore if already exists
                 (mkdir path))))

;;; make-directories : String -> ()
;;; Create directory and all parent directories.
;;; Alias for ensure-directory!
(define (make-directories path)
  (ensure-directory! path))

;;; Note: file-exists? is provided by Chez Scheme's (rnrs io simple)
;;; It returns #t if the file or directory exists.
