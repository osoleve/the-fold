;;; fabric/stitches/pipeline/context.ss — Pipeline Context and State
;;;
;;; Context is the read-only environment for pipeline execution.
;;; State is the mutable accumulator threaded through stages.
;;;
;;; This is Core code: pure record definitions.
;;;
;;; Features:
;;;   - PipelineContext: config, persona, session, fuel
;;;   - PipelineState: log, artifacts, checkpoints, metrics
;;;   - Context accessors and builders
;;;   - State threading helpers
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "fabric/stitches/prelude.ss")

;;; ============================================================
;;; PipelineContext — Read-Only Environment
;;; ============================================================
;;;
;;; Context flows through the pipeline unchanged (unless stage-local).
;;; Contains configuration, session info, and resource limits.

;;; make-pipeline-context : Record fields
(define (make-pipeline-context config persona session-id run-id env fuel)
  (list 'pipeline-context config persona session-id run-id env fuel))

;;; pipeline-context? : Any -> Boolean
(define (pipeline-context? x)
  (and (pair? x) (eq? (car x) 'pipeline-context)))

;;; Accessors
(define (ctx-config ctx) (list-ref ctx 1))
(define (ctx-persona ctx) (list-ref ctx 2))
(define (ctx-session-id ctx) (list-ref ctx 3))
(define (ctx-run-id ctx) (list-ref ctx 4))
(define (ctx-env ctx) (list-ref ctx 5))
(define (ctx-fuel ctx) (list-ref ctx 6))

;;; Context builders

;;; empty-context : PipelineContext
(define empty-context
  (make-pipeline-context '() #f "default" #f '() 10000))

;;; ctx-with-config : PipelineContext -> Alist -> PipelineContext
(define (ctx-with-config ctx config)
  (make-pipeline-context config
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

;;; ctx-with-persona : PipelineContext -> Persona -> PipelineContext
(define (ctx-with-persona ctx persona)
  (make-pipeline-context (ctx-config ctx)
                         persona
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

;;; ctx-with-session : PipelineContext -> String -> PipelineContext
(define (ctx-with-session ctx session-id)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         session-id
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

;;; ctx-with-run-id : PipelineContext -> String -> PipelineContext
(define (ctx-with-run-id ctx run-id)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         run-id
                         (ctx-env ctx)
                         (ctx-fuel ctx)))

;;; ctx-with-env : PipelineContext -> Alist -> PipelineContext
(define (ctx-with-env ctx env)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         env
                         (ctx-fuel ctx)))

;;; ctx-extend-env : PipelineContext -> Alist -> PipelineContext
;;; Add to existing environment.
(define (ctx-extend-env ctx additions)
  (ctx-with-env ctx (append additions (ctx-env ctx))))

;;; ctx-with-fuel : PipelineContext -> Nat -> PipelineContext
(define (ctx-with-fuel ctx fuel)
  (make-pipeline-context (ctx-config ctx)
                         (ctx-persona ctx)
                         (ctx-session-id ctx)
                         (ctx-run-id ctx)
                         (ctx-env ctx)
                         fuel))

;;; ctx-consume-fuel : PipelineContext -> Nat -> PipelineContext
;;; Decrease fuel by amount.
(define (ctx-consume-fuel ctx amount)
  (ctx-with-fuel ctx (max 0 (- (ctx-fuel ctx) amount))))

;;; ctx-env-ref : PipelineContext -> Symbol -> Any
;;; Look up environment variable.
(define (ctx-env-ref ctx key)
  (let ([entry (assq key (ctx-env ctx))])
       (if entry (cdr entry) #f)))

;;; ctx-config-ref : PipelineContext -> Symbol -> Any
;;; Look up config value.
(define (ctx-config-ref ctx key)
  (let ([entry (assq key (ctx-config ctx))])
       (if entry (cdr entry) #f)))

;;; ============================================================
;;; PipelineState — Mutable Accumulator
;;; ============================================================
;;;
;;; State accumulates as pipeline runs: logs, artifacts, checkpoints.
;;; Unlike context, state changes between stages.

;;; make-pipeline-state : Fields -> PipelineState
(define (make-pipeline-state log artifacts checkpoints metrics cache)
  (list 'pipeline-state log artifacts checkpoints metrics cache))

;;; pipeline-state? : Any -> Boolean
(define (pipeline-state? x)
  (and (pair? x) (eq? (car x) 'pipeline-state)))

;;; Accessors
(define (state-log st) (list-ref st 1))
(define (state-artifacts st) (list-ref st 2))
(define (state-checkpoints st) (list-ref st 3))
(define (state-metrics st) (list-ref st 4))
(define (state-cache st) (list-ref st 5))

;;; empty-state : PipelineState
(define empty-state
  (make-pipeline-state '() '() '() '() '()))

;;; State updaters (return new state, don't mutate)

;;; state-add-log : PipelineState -> LogEntry -> PipelineState
(define (state-add-log st entry)
  (make-pipeline-state (cons entry (state-log st))
                       (state-artifacts st)
                       (state-checkpoints st)
                       (state-metrics st)
                       (state-cache st)))

;;; state-add-artifact : PipelineState -> Symbol -> Any -> PipelineState
(define (state-add-artifact st name value)
  (make-pipeline-state (state-log st)
                       (cons (cons name value) (state-artifacts st))
                       (state-checkpoints st)
                       (state-metrics st)
                       (state-cache st)))

;;; state-set-checkpoint : PipelineState -> Symbol -> Any -> PipelineState
(define (state-set-checkpoint st name value)
  (make-pipeline-state (state-log st)
                       (state-artifacts st)
                       (cons (cons name value)
                             (filter (lambda (p) (not (eq? (car p) name)))
                                     (state-checkpoints st)))
                       (state-metrics st)
                       (state-cache st)))

;;; state-get-checkpoint : PipelineState -> Symbol -> Any
(define (state-get-checkpoint st name)
  (let ([entry (assq name (state-checkpoints st))])
       (if entry (cdr entry) #f)))

;;; state-add-metric : PipelineState -> Symbol -> Number -> PipelineState
(define (state-add-metric st name value)
  (make-pipeline-state (state-log st)
                       (state-artifacts st)
                       (state-checkpoints st)
                       (cons (cons name value) (state-metrics st))
                       (state-cache st)))

;;; state-cache-put : PipelineState -> Any -> Any -> PipelineState
;;; Add to cache (key-value).
(define (state-cache-put st key value)
  (make-pipeline-state (state-log st)
                       (state-artifacts st)
                       (state-checkpoints st)
                       (state-metrics st)
                       (cons (cons key value) (state-cache st))))

;;; state-cache-get : PipelineState -> Any -> Any
(define (state-cache-get st key)
  (let ([entry (assoc key (state-cache st))])
       (if entry (cdr entry) #f)))

;;; ============================================================
;;; Log Entry Types
;;; ============================================================

;;; make-log-entry : Symbol -> String -> Any -> LogEntry
(define (make-log-entry level message data)
  (list 'log-entry level message data (current-timestamp)))

;;; Simple timestamp (seconds since epoch approximation)
(define (current-timestamp)
  0)  ; Interpreter will provide real timestamp

;;; log-entry-level : LogEntry -> Symbol
(define (log-entry-level e) (list-ref e 1))

;;; log-entry-message : LogEntry -> String
(define (log-entry-message e) (list-ref e 2))

;;; log-entry-data : LogEntry -> Any
(define (log-entry-data e) (list-ref e 3))

;;; log-entry-timestamp : LogEntry -> Number
(define (log-entry-timestamp e) (list-ref e 4))

;;; ============================================================
;;; Run Record — Complete Pipeline Execution
;;; ============================================================
;;;
;;; Captures everything about a pipeline run for persistence/resume.

;;; make-run-record : Fields -> RunRecord
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

;;; Run statuses
;;;   'pending - not started
;;;   'running - in progress
;;;   'success - completed successfully
;;;   'failed - completed with error
;;;   'halted - stopped intentionally
;;;   'awaiting - waiting for external signal

;;; ============================================================
;;; Pipeline Definition Record
;;; ============================================================
;;;
;;; Named pipeline with configuration.

;;; make-pipeline-def : Symbol -> Stage -> Alist -> PipelineDef
(define (make-pipeline-def name stage config)
  (list 'pipeline-def name stage config))

;;; pipeline-def? : Any -> Boolean
(define (pipeline-def? x)
  (and (pair? x) (eq? (car x) 'pipeline-def)))

;;; Pipeline def accessors
(define (pipeline-def-name p) (list-ref p 1))
(define (pipeline-def-stage p) (list-ref p 2))
(define (pipeline-def-config p) (list-ref p 3))

;;; Pipeline config keys:
;;;   'model - default LLM model
;;;   'fuel - default fuel limit
;;;   'timeout - overall timeout (ms)
;;;   'retry-policy - retry configuration
;;;   'require-approval - needs human approval to start
;;;   'schedule - cron/interval spec
;;;   'tags - labels for organization

;;; ============================================================
;;; Persona Integration
;;; ============================================================
;;;
;;; Personas provide system prompts and behavioral configuration.

;;; make-persona : Symbol -> String -> Alist -> Persona
(define (make-persona name system-prompt config)
  (list 'persona name system-prompt config))

;;; persona? : Any -> Boolean
(define (persona? x)
  (and (pair? x) (eq? (car x) 'persona)))

;;; Persona accessors
(define (persona-name p) (list-ref p 1))
(define (persona-system-prompt p) (list-ref p 2))
(define (persona-config p) (list-ref p 3))

;;; persona-tier : Persona -> Symbol
;;; Get the tier (shepherd, builder, player).
(define (persona-tier p)
  (let ([cfg (persona-config p)])
       (let ([entry (assq 'tier cfg)])
            (if entry (cdr entry) 'player))))

;;; persona-model : Persona -> Symbol
;;; Get preferred model.
(define (persona-model p)
  (let ([cfg (persona-config p)])
       (let ([entry (assq 'model cfg)])
            (if entry (cdr entry) 'sonnet))))

;;; persona-channels : Persona -> (List Symbol . List Symbol)
;;; Get (read-channels . write-channels).
(define (persona-channels p)
  (let ([cfg (persona-config p)])
       (let ([read (assq 'read-channels cfg)]
             [write (assq 'write-channels cfg)])
            (cons (if read (cdr read) '())
                  (if write (cdr write) '())))))

;;; ============================================================
;;; Schedule Specification
;;; ============================================================

;;; make-cron-schedule : String -> Schedule
(define (make-cron-schedule cron-expr)
  (list 'schedule 'cron cron-expr))

;;; make-interval-schedule : Nat -> Schedule
;;; Interval in seconds.
(define (make-interval-schedule seconds)
  (list 'schedule 'interval seconds))

;;; make-tag-schedule : String -> Schedule
;;; Trigger on forum tag.
(define (make-tag-schedule tag-pattern)
  (list 'schedule 'tag tag-pattern))

;;; make-event-schedule : Symbol -> Schedule
;;; Trigger on named event.
(define (make-event-schedule event-name)
  (list 'schedule 'event event-name))

;;; make-manual-schedule : Schedule
;;; Only run manually.
(define make-manual-schedule
  (list 'schedule 'manual))

;;; schedule? : Any -> Boolean
(define (schedule? x)
  (and (pair? x) (eq? (car x) 'schedule)))

;;; schedule-type : Schedule -> Symbol
(define (schedule-type s) (list-ref s 1))

;;; schedule-spec : Schedule -> Any
(define (schedule-spec s)
  (if (> (length s) 2)
      (list-ref s 2)
      #f))

;;; ============================================================
;;; Retry Policy
;;; ============================================================

;;; make-retry-policy : Nat -> (Nat -> Nat) -> (Symbol -> Boolean) -> Symbol -> RetryPolicy
(define (make-retry-policy max-attempts delay-fn retry-on on-exhaust)
  (list 'retry-policy max-attempts delay-fn retry-on on-exhaust))

;;; retry-policy? : Any -> Boolean
(define (retry-policy? x)
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

;;; ============================================================
;;; Context/State Pair for Threading
;;; ============================================================

;;; make-ctx-state : PipelineContext -> PipelineState -> (Context . State)
(define (make-ctx-state ctx state)
  (cons ctx state))

;;; ctx-of : (Context . State) -> Context
(define ctx-of car)

;;; state-of : (Context . State) -> State
(define state-of cdr)

;;; update-state : (Context . State) -> (State -> State) -> (Context . State)
(define (update-state cs f)
  (cons (ctx-of cs) (f (state-of cs))))

;;; update-ctx : (Context . State) -> (Context -> Context) -> (Context . State)
(define (update-ctx cs f)
  (cons (f (ctx-of cs)) (state-of cs)))
