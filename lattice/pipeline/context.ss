(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @requires prelude
(require 'prelude)

(doc 'module 'pipeline/context)
(doc 'description "Pipeline Context and State. Context is the read-only environment for pipeline execution. State is the mutable accumulator threaded through stages.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(prelude.ss))
(doc 'features "PipelineContext: config, persona, session, fuel; PipelineState: log, artifacts, checkpoints, metrics; Context accessors and builders; State threading helpers")

(doc 'section 'pipeline-context)
(doc 'description "Read-Only Environment. Context flows through the pipeline unchanged (unless stage-local). Contains configuration, session info, and resource limits.")

(doc 'type '(-> Alist Persona String Symbol Alist Nat PipelineContext))
(define (make-pipeline-context config persona session-id run-id env fuel)
  (doc 'export #t)
  (list 'pipeline-context config persona session-id run-id env fuel))

(doc 'type '(-> Any Boolean))
(define (pipeline-context? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'pipeline-context)))

(doc 'section 'accessors)
(define (ctx-config ctx) (list-ref ctx 1))
(define (ctx-persona ctx) (list-ref ctx 2))
(define (ctx-session-id ctx) (list-ref ctx 3))
(define (ctx-run-id ctx) (list-ref ctx 4))
(define (ctx-env ctx) (list-ref ctx 5))
(define (ctx-fuel ctx) (list-ref ctx 6))

(doc 'section 'context-builders)

(doc 'type 'PipelineContext)
(define empty-context
  (make-pipeline-context '() #f "default" #f '() 10000))

(doc 'type '(-> PipelineContext Alist PipelineContext))
(define (ctx-with-config ctx config)
  (doc 'export #t)
  (make-pipeline-context config
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

(doc 'type '(-> PipelineContext Persona PipelineContext))
(define (ctx-with-persona ctx persona)
  (doc 'export #t)
  (make-pipeline-context (ctx-config ctx)
                         persona
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

(doc 'type '(-> PipelineContext String PipelineContext))
(define (ctx-with-session ctx session-id)
  (doc 'export #t)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         session-id
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

(doc 'type '(-> PipelineContext String PipelineContext))
(define (ctx-with-run-id ctx run-id)
  (doc 'export #t)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         run-id
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

(doc 'type '(-> PipelineContext Alist PipelineContext))
(define (ctx-with-env ctx env)
  (doc 'export #t)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         env
                         (ctx-fuel ctx)))

(doc 'type '(-> PipelineContext Alist PipelineContext))
(doc 'description "Add to existing environment")
(define (ctx-extend-env ctx additions)
  (doc 'export #t)
  (ctx-with-env ctx (append additions (ctx-env ctx))))

(doc 'type '(-> PipelineContext Nat PipelineContext))
(define (ctx-with-fuel ctx fuel)
  (doc 'export #t)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         fuel))

(doc 'type '(-> PipelineContext Nat PipelineContext))
(doc 'description "Decrease fuel by amount")
(define (ctx-consume-fuel ctx amount)
  (doc 'export #t)
  (ctx-with-fuel ctx (max 0 (- (ctx-fuel ctx) amount))))

(doc 'type '(-> PipelineContext Symbol Any))
(doc 'description "Look up environment variable")
(define (ctx-env-ref ctx key)
  (doc 'export #t)
  (let ([entry (assq key (ctx-env ctx))])
       (if entry (cdr entry) #f)))

(doc 'type '(-> PipelineContext Symbol Any))
(doc 'description "Look up config value")
(define (ctx-config-ref ctx key)
  (doc 'export #t)
  (let ([entry (assq key (ctx-config ctx))])
       (if entry (cdr entry) #f)))

(doc 'section 'pipeline-state)
(doc 'description "Mutable Accumulator. State accumulates as pipeline runs: logs, artifacts, checkpoints. Unlike context, state changes between stages.")

(doc 'type '(-> List List List List List PipelineState))
(define (make-pipeline-state log artifacts checkpoints metrics cache)
  (doc 'export #t)
  (list 'pipeline-state log artifacts checkpoints metrics cache))

(doc 'type '(-> Any Boolean))
(define (pipeline-state? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'pipeline-state)))

(doc 'section 'state-accessors)
(define (state-log st) (list-ref st 1))
(define (state-artifacts st) (list-ref st 2))
(define (state-checkpoints st) (list-ref st 3))
(define (state-metrics st) (list-ref st 4))
(define (state-cache st) (list-ref st 5))

(doc 'type 'PipelineState)
(define empty-state
  (make-pipeline-state '() '() '() '() '()))

(doc 'section 'state-updaters)
(doc 'description "Return new state, don't mutate")

(doc 'type '(-> PipelineState LogEntry PipelineState))
(define (state-add-log st entry)
  (doc 'export #t)
  (make-pipeline-state (cons entry (state-log st))
                       (state-artifacts st)
                       (state-checkpoints st)
                       (state-metrics st)
                       (state-cache st)))

;;; state-add-artifact : PipelineState -> Symbol -> Any -> PipelineState
(define (state-add-artifact st name value)
  (doc 'export #t)
  (make-pipeline-state (state-log st)
                       (cons (cons name value) (state-artifacts st))
                       (state-checkpoints st)
                       (state-metrics st)
                       (state-cache st)))

;;; state-set-checkpoint : PipelineState -> Symbol -> Any -> PipelineState
(define (state-set-checkpoint st name value)
  (doc 'export #t)
  (make-pipeline-state (state-log st)
                       (state-artifacts st)
                       (cons (cons name value)
                             (filter (lambda (p) (not (eq? (car p) name)))
                                     (state-checkpoints st)))
                       (state-metrics st)
                       (state-cache st)))

;;; state-get-checkpoint : PipelineState -> Symbol -> Any
(define (state-get-checkpoint st name)
  (doc 'export #t)
  (let ([entry (assq name (state-checkpoints st))])
       (if entry (cdr entry) #f)))

;;; state-add-metric : PipelineState -> Symbol -> Number -> PipelineState
(define (state-add-metric st name value)
  (doc 'export #t)
  (make-pipeline-state (state-log st)
                       (state-artifacts st)
                       (state-checkpoints st)
                       (cons (cons name value) (state-metrics st))
                       (state-cache st)))

;;; state-cache-put : PipelineState -> Any -> Any -> PipelineState
;;; Add to cache (key-value).
(define (state-cache-put st key value)
  (doc 'export #t)
  (make-pipeline-state (state-log st)
                       (state-artifacts st)
                       (state-checkpoints st)
                       (state-metrics st)
                       (cons (cons key value) (state-cache st))))

;;; state-cache-get : PipelineState -> Any -> Any
(define (state-cache-get st key)
  (doc 'export #t)
  (let ([entry (assoc key (state-cache st))])
       (if entry (cdr entry) #f)))

(doc 'section 'log-entry-types)

(doc 'type '(-> Symbol String Any LogEntry))
(define (make-log-entry level message data)
  (doc 'export #t)
  (list 'log-entry level message data (current-timestamp)))

(doc 'description "Simple timestamp (seconds since epoch approximation)")
(define (current-timestamp)
  (doc 'export #t)
  (doc 'note "Interpreter will provide real timestamp")
  0)

(doc 'type '(-> LogEntry Symbol))
(define (log-entry-level e) (list-ref e 1))

;;; log-entry-message : LogEntry -> String
(define (log-entry-message e) (list-ref e 2))

;;; log-entry-data : LogEntry -> Any
(define (log-entry-data e) (list-ref e 3))

;;; log-entry-timestamp : LogEntry -> Number
(define (log-entry-timestamp e) (list-ref e 4))

(doc 'section 'run-record)
(doc 'description "Complete Pipeline Execution. Captures everything about a pipeline run for persistence/resume.")

(doc 'type '(-> Symbol Symbol Any Any PipelineState Symbol Nat Nat Symbol RunRecord))
(define (make-run-record pipeline-name
                         run-id
                         input
                         output
                         final-state
                         status
                         started-at
                         finished-at
                         parent-run-id)
  (list 'run-record
        pipeline-name run-id input output final-state
        status started-at finished-at parent-run-id))

;;; run-record? : Any -> Boolean
(define (run-record? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'run-record)))

;;; Run record accessors
(define (run-pipeline-name r) (list-ref r 1))
(define (run-id-of r) (list-ref r 2))
(define (run-input r) (list-ref r 3))
(define (run-output r) (list-ref r 4))
(define (run-final-state r) (list-ref r 5))
(define (run-status r) (list-ref r 6))
(define (run-started-at r) (list-ref r 7))
(define (run-finished-at r) (list-ref r 8))
(define (run-parent-id r) (list-ref r 9))

(doc 'note "Run statuses: 'pending (not started), 'running (in progress), 'success (completed successfully), 'failed (completed with error), 'halted (stopped intentionally), 'awaiting (waiting for external signal)")

(doc 'section 'pipeline-definition)
(doc 'description "Named pipeline with configuration")

(doc 'type '(-> Symbol Stage Alist PipelineDef))
(define (make-pipeline-def name stage config)
  (doc 'export #t)
  (list 'pipeline-def name stage config))

;;; pipeline-def? : Any -> Boolean
(define (pipeline-def? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'pipeline-def)))

;;; Pipeline def accessors
(define (pipeline-def-name p) (list-ref p 1))
(define (pipeline-def-stage p) (list-ref p 2))
(define (pipeline-def-config p) (list-ref p 3))

(doc 'note "Pipeline config keys: 'model (default LLM model), 'fuel (default fuel limit), 'timeout (overall timeout ms), 'retry-policy (retry configuration), 'require-approval (needs human approval to start), 'schedule (cron/interval spec), 'tags (labels for organization)")

(doc 'section 'persona-integration)
(doc 'description "Personas provide system prompts and behavioral configuration")

(doc 'type '(-> Symbol String Alist Persona))
(define (make-persona name system-prompt config)
  (doc 'export #t)
  (list 'persona name system-prompt config))

;;; persona? : Any -> Boolean
(define (persona? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'persona)))

;;; Persona accessors
(define (persona-name p) (list-ref p 1))
(define (persona-system-prompt p) (list-ref p 2))
(define (persona-config p) (list-ref p 3))

;;; persona-tier : Persona -> Symbol
;;; Get the tier (shepherd, builder, player).
(define (persona-tier p)
  (doc 'export #t)
  (let ([cfg (persona-config p)])
       (let ([entry (assq 'tier cfg)])
            (if entry (cdr entry) 'player))))

;;; persona-model : Persona -> Symbol
;;; Get preferred model.
(define (persona-model p)
  (doc 'export #t)
  (let ([cfg (persona-config p)])
       (let ([entry (assq 'model cfg)])
            (if entry (cdr entry) 'sonnet))))

;;; persona-channels : Persona -> (List Symbol . List Symbol)
;;; Get (read-channels . write-channels).
(define (persona-channels p)
  (doc 'export #t)
  (let ([cfg (persona-config p)])
       (let ([read (assq 'read-channels cfg)]
             [write (assq 'write-channels cfg)])
            (cons (if read (cdr read) '())
                  (if write (cdr write) '())))))

(doc 'section 'schedule-specification)

(doc 'type '(-> String Schedule))
(define (make-cron-schedule cron-expr)
  (doc 'export #t)
  (list 'schedule 'cron cron-expr))

;;; make-interval-schedule : Nat -> Schedule
;;; Interval in seconds.
(define (make-interval-schedule seconds)
  (doc 'export #t)
  (list 'schedule 'interval seconds))

;;; make-tag-schedule : String -> Schedule
;;; Trigger on forum tag.
(define (make-tag-schedule tag-pattern)
  (doc 'export #t)
  (list 'schedule 'tag tag-pattern))

;;; make-event-schedule : Symbol -> Schedule
;;; Trigger on named event.
(define (make-event-schedule event-name)
  (doc 'export #t)
  (list 'schedule 'event event-name))

;;; make-manual-schedule : Schedule
;;; Only run manually.
(define make-manual-schedule
  (list 'schedule 'manual))

;;; schedule? : Any -> Boolean
(define (schedule? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'schedule)))

;;; schedule-type : Schedule -> Symbol
(define (schedule-type s) (list-ref s 1))

;;; schedule-spec : Schedule -> Any
(define (schedule-spec s)
  (doc 'export #t)
  (if (> (length s) 2)
      (list-ref s 2)
      #f))

(doc 'section 'retry-policy)

(doc 'type '(-> Nat (-> Nat Nat) (-> Symbol Boolean) Symbol RetryPolicy))
(define (make-retry-policy max-attempts delay-fn retry-on on-exhaust)
  (doc 'export #t)
  (list 'retry-policy max-attempts delay-fn retry-on on-exhaust))

;;; retry-policy? : Any -> Boolean
(define (retry-policy? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'retry-policy)))

;;; Retry policy accessors
(define (retry-max-attempts p) (list-ref p 1))
(define (retry-delay-fn p) (list-ref p 2))
(define (retry-on-predicate p) (list-ref p 3))
(define (retry-on-exhaust p) (list-ref p 4))

;;; Standard retry policies

;;; retry-none : RetryPolicy
;;; No retries.
(define retry-none
  (make-retry-policy 0 (lambda (n) 0) (lambda (c) #f) 'fail))

;;; retry-exponential : RetryPolicy
;;; Exponential backoff, 3 attempts.
(define retry-exponential
  (make-retry-policy
   3
   (lambda (n) (* 1000 (expt 2 n)))  ; 1s, 2s, 4s
   (lambda (code) (memq code '(timeout rate-limit transient)))
   'halt))

;;; retry-linear : RetryPolicy
;;; Linear delay, 5 attempts.
(define retry-linear
  (make-retry-policy
   5
   (lambda (n) (* 1000 n))  ; 1s, 2s, 3s, 4s, 5s
   (lambda (code) (memq code '(timeout rate-limit transient)))
   'halt))

;;; retry-immediate : RetryPolicy
;;; Immediate retry, 3 times.
(define retry-immediate
  (make-retry-policy
   3
   (lambda (n) 0)
   (lambda (code) #t)  ; Retry all errors
   'fail))

(doc 'section 'ctx-state-pair)
(doc 'description "Context/State Pair for Threading")

(doc 'type '(-> PipelineContext PipelineState (Pair Context State)))
(define (make-ctx-state ctx state)
  (doc 'export #t)
  (cons ctx state))

;;; ctx-of : (Context . State) -> Context
(define ctx-of car)

;;; state-of : (Context . State) -> State
(define state-of cdr)

;;; update-state : (Context . State) -> (State -> State) -> (Context . State)
(define (update-state cs f)
  (doc 'export #t)
  (cons (ctx-of cs) (f (state-of cs))))

;;; update-ctx : (Context . State) -> (Context -> Context) -> (Context . State)
(define (update-ctx cs f)
  (doc 'export #t)
  (cons (f (ctx-of cs)) (state-of cs)))

