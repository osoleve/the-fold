;;; fabric/stitches/pipeline/tests/test-context.ss — Comprehensive Tests for Pipeline Context
;;;
;;; Tests context and state management for pipelines.

(load "core/testing/test-framework.ss")
(load "lattice/pipeline/context.ss")

;;; ====
;;; PipelineContext Tests
;;; ====

(test-group "PipelineContext Construction"
  ;; Test make-pipeline-context
  (define-test "make-pipeline-context creates context"
    (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
      (assert-true (pipeline-context? ctx))))

  (define-test "ctx-config extracts config"
    (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
      (assert-equal '((a . 1)) (ctx-config ctx))))

  (define-test "ctx-persona extracts persona"
    (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
      (assert-equal 'persona (ctx-persona ctx))))

  (define-test "ctx-session-id extracts session-id"
    (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
      (assert-equal "session" (ctx-session-id ctx))))

  (define-test "ctx-run-id extracts run-id"
    (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
      (assert-equal "run" (ctx-run-id ctx))))

  (define-test "ctx-env extracts env"
    (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
      (assert-equal '((x . 10)) (ctx-env ctx))))

  (define-test "ctx-fuel extracts fuel"
    (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
      (assert-equal 5000 (ctx-fuel ctx))))

  ;; Test empty-context
  (define-test "empty-context is a context"
    (assert-true (pipeline-context? empty-context)))

  (define-test "empty-context has empty config"
    (assert-equal '() (ctx-config empty-context)))

  (define-test "empty-context has #f persona"
    (assert-equal #f (ctx-persona empty-context)))

  (define-test "empty-context has default session-id"
    (assert-equal "default" (ctx-session-id empty-context)))

  (define-test "empty-context has 10000 fuel"
    (assert-equal 10000 (ctx-fuel empty-context)))

  (define-test "pipeline-context? false for non-context"
    (assert-equal #f (pipeline-context? '(not a context)))))

;;; ====
;;; Context Builders
;;; ====

(test-group "Context Builders"
  ;; Test ctx-with-config
  (define-test "ctx-with-config sets config"
    (let* ([ctx empty-context]
           [updated (ctx-with-config ctx '((key . value)))])
      (assert-equal '((key . value)) (ctx-config updated))))

  (define-test "ctx-with-config preserves other fields"
    (let* ([ctx empty-context]
           [updated (ctx-with-config ctx '((key . value)))])
      (assert-equal (ctx-fuel ctx) (ctx-fuel updated))))

  ;; Test ctx-with-persona
  (define-test "ctx-with-persona sets persona"
    (let* ([ctx empty-context]
           [updated (ctx-with-persona ctx 'test-persona)])
      (assert-equal 'test-persona (ctx-persona updated))))

  ;; Test ctx-with-session
  (define-test "ctx-with-session sets session-id"
    (let* ([ctx empty-context]
           [updated (ctx-with-session ctx "my-session")])
      (assert-equal "my-session" (ctx-session-id updated))))

  ;; Test ctx-with-run-id
  (define-test "ctx-with-run-id sets run-id"
    (let* ([ctx empty-context]
           [updated (ctx-with-run-id ctx "run-123")])
      (assert-equal "run-123" (ctx-run-id updated))))

  ;; Test ctx-with-env
  (define-test "ctx-with-env sets env"
    (let* ([ctx empty-context]
           [updated (ctx-with-env ctx '((a . 1) (b . 2)))])
      (assert-equal '((a . 1) (b . 2)) (ctx-env updated))))

  ;; Test ctx-extend-env
  (define-test "ctx-extend-env adds to env"
    (let* ([ctx (ctx-with-env empty-context '((x . 10)))]
           [updated (ctx-extend-env ctx '((y . 20)))])
      (assert-equal '((y . 20) (x . 10)) (ctx-env updated))))

  ;; Test ctx-with-fuel
  (define-test "ctx-with-fuel sets fuel"
    (let* ([ctx empty-context]
           [updated (ctx-with-fuel ctx 1000)])
      (assert-equal 1000 (ctx-fuel updated))))

  ;; Test ctx-consume-fuel
  (define-test "ctx-consume-fuel reduces fuel"
    (let* ([ctx (ctx-with-fuel empty-context 1000)]
           [updated (ctx-consume-fuel ctx 100)])
      (assert-equal 900 (ctx-fuel updated))))

  (define-test "ctx-consume-fuel bottoms out at 0"
    (let* ([ctx (ctx-with-fuel empty-context 50)]
           [updated (ctx-consume-fuel ctx 100)])
      (assert-equal 0 (ctx-fuel updated)))))

;;; ====
;;; Context Lookup
;;; ====

(test-group "Context Lookup"
  ;; Test ctx-env-ref
  (define-test "ctx-env-ref finds existing key"
    (let ([ctx (ctx-with-env empty-context '((a . 1) (b . 2) (c . 3)))])
      (assert-equal 2 (ctx-env-ref ctx 'b))))

  (define-test "ctx-env-ref returns #f for missing key"
    (let ([ctx (ctx-with-env empty-context '((a . 1) (b . 2) (c . 3)))])
      (assert-equal #f (ctx-env-ref ctx 'missing))))

  ;; Test ctx-config-ref
  (define-test "ctx-config-ref finds existing key"
    (let ([ctx (ctx-with-config empty-context '((model . opus) (timeout . 30000)))])
      (assert-equal 'opus (ctx-config-ref ctx 'model))))

  (define-test "ctx-config-ref returns #f for missing key"
    (let ([ctx (ctx-with-config empty-context '((model . opus) (timeout . 30000)))])
      (assert-equal #f (ctx-config-ref ctx 'missing)))))

;;; ====
;;; PipelineState Tests
;;; ====

(test-group "PipelineState Construction"
  ;; Test make-pipeline-state
  (define-test "make-pipeline-state creates state"
    (let ([state (make-pipeline-state '(log-entry) '((art . val)) '((cp . data)) '((m . 1)) '((k . v)))])
      (assert-true (pipeline-state? state))))

  (define-test "state-log extracts log"
    (let ([state (make-pipeline-state '(log-entry) '((art . val)) '((cp . data)) '((m . 1)) '((k . v)))])
      (assert-equal '(log-entry) (state-log state))))

  (define-test "state-artifacts extracts artifacts"
    (let ([state (make-pipeline-state '(log-entry) '((art . val)) '((cp . data)) '((m . 1)) '((k . v)))])
      (assert-equal '((art . val)) (state-artifacts state))))

  (define-test "state-checkpoints extracts checkpoints"
    (let ([state (make-pipeline-state '(log-entry) '((art . val)) '((cp . data)) '((m . 1)) '((k . v)))])
      (assert-equal '((cp . data)) (state-checkpoints state))))

  (define-test "state-metrics extracts metrics"
    (let ([state (make-pipeline-state '(log-entry) '((art . val)) '((cp . data)) '((m . 1)) '((k . v)))])
      (assert-equal '((m . 1)) (state-metrics state))))

  (define-test "state-cache extracts cache"
    (let ([state (make-pipeline-state '(log-entry) '((art . val)) '((cp . data)) '((m . 1)) '((k . v)))])
      (assert-equal '((k . v)) (state-cache state))))

  ;; Test empty-state
  (define-test "empty-state is a state"
    (assert-true (pipeline-state? empty-state)))

  (define-test "empty-state has empty log"
    (assert-equal '() (state-log empty-state)))

  (define-test "empty-state has empty artifacts"
    (assert-equal '() (state-artifacts empty-state)))

  (define-test "pipeline-state? false for non-state"
    (assert-equal #f (pipeline-state? '(not a state)))))

;;; ====
;;; State Updaters
;;; ====

(test-group "State Updaters"
  ;; Test state-add-log
  (define-test "state-add-log adds entry"
    (let* ([state empty-state]
           [updated (state-add-log state 'entry1)])
      (assert-equal '(entry1) (state-log updated))))

  (define-test "state-add-log prepends entries"
    (let* ([state empty-state]
           [updated (state-add-log state 'entry1)]
           [updated2 (state-add-log updated 'entry2)])
      (assert-equal '(entry2 entry1) (state-log updated2))))

  ;; Test state-add-artifact
  (define-test "state-add-artifact adds artifact"
    (let* ([state empty-state]
           [updated (state-add-artifact state 'output "result")])
      (assert-equal '((output . "result")) (state-artifacts updated))))

  ;; Test state-set-checkpoint
  (define-test "state-set-checkpoint adds checkpoint"
    (let* ([state empty-state]
           [updated (state-set-checkpoint state 'cp1 "data1")])
      (assert-equal '((cp1 . "data1")) (state-checkpoints updated))))

  (define-test "state-set-checkpoint multiple"
    (let* ([state empty-state]
           [updated (state-set-checkpoint state 'cp1 "data1")]
           [updated2 (state-set-checkpoint updated 'cp2 "data2")])
      (assert-equal '((cp2 . "data2") (cp1 . "data1")) (state-checkpoints updated2))))

  (define-test "state-set-checkpoint replaces existing"
    (let* ([state empty-state]
           [updated (state-set-checkpoint state 'cp1 "data1")]
           [updated2 (state-set-checkpoint updated 'cp2 "data2")]
           [updated3 (state-set-checkpoint updated2 'cp1 "data1-new")])
      (assert-equal '((cp1 . "data1-new") (cp2 . "data2")) (state-checkpoints updated3))))

  ;; Test state-get-checkpoint
  (define-test "state-get-checkpoint finds existing"
    (let ([state (state-set-checkpoint empty-state 'my-cp '(some data))])
      (assert-equal '(some data) (state-get-checkpoint state 'my-cp))))

  (define-test "state-get-checkpoint returns #f for missing"
    (let ([state (state-set-checkpoint empty-state 'my-cp '(some data))])
      (assert-equal #f (state-get-checkpoint state 'unknown))))

  ;; Test state-add-metric
  (define-test "state-add-metric adds metric"
    (let* ([state empty-state]
           [updated (state-add-metric state 'latency 42.5)])
      (assert-equal '((latency . 42.5)) (state-metrics updated))))

  ;; Test state-cache-put
  (define-test "state-cache-put adds to cache"
    (let* ([state empty-state]
           [updated (state-cache-put state "key1" "value1")])
      (assert-equal '(("key1" . "value1")) (state-cache updated))))

  ;; Test state-cache-get
  (define-test "state-cache-get finds existing"
    (let ([state (state-cache-put empty-state "my-key" '(cached data))])
      (assert-equal '(cached data) (state-cache-get state "my-key"))))

  (define-test "state-cache-get returns #f for missing"
    (let ([state (state-cache-put empty-state "my-key" '(cached data))])
      (assert-equal #f (state-cache-get state "unknown")))))

;;; ====
;;; Log Entry Tests
;;; ====

(test-group "Log Entry"
  (define-test "log-entry-level extracts level"
    (let ([entry (make-log-entry 'info "Test message" '(data))])
      (assert-equal 'info (log-entry-level entry))))

  (define-test "log-entry-message extracts message"
    (let ([entry (make-log-entry 'info "Test message" '(data))])
      (assert-equal "Test message" (log-entry-message entry))))

  (define-test "log-entry-data extracts data"
    (let ([entry (make-log-entry 'info "Test message" '(data))])
      (assert-equal '(data) (log-entry-data entry))))

  (define-test "log-entry-timestamp returns number"
    (let ([entry (make-log-entry 'info "Test message" '(data))])
      (assert-equal 0 (log-entry-timestamp entry)))))  ; current-timestamp returns 0 in core

;;; ====
;;; Run Record Tests
;;; ====

(test-group "Run Record"
  (define-test "make-run-record creates record"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-true (run-record? record))))

  (define-test "run-pipeline-name extracts name"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal 'my-pipeline (run-pipeline-name record))))

  (define-test "run-id-of extracts run-id"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal "run-123" (run-id-of record))))

  (define-test "run-input extracts input"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal "input" (run-input record))))

  (define-test "run-output extracts output"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal "output" (run-output record))))

  (define-test "run-final-state is state"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-true (pipeline-state? (run-final-state record)))))

  (define-test "run-status extracts status"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal 'success (run-status record))))

  (define-test "run-started-at extracts start time"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal 1000 (run-started-at record))))

  (define-test "run-finished-at extracts finish time"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal 2000 (run-finished-at record))))

  (define-test "run-parent-id extracts parent"
    (let ([record (make-run-record 'my-pipeline "run-123" "input" "output" empty-state 'success 1000 2000 "parent-run")])
      (assert-equal "parent-run" (run-parent-id record))))

  (define-test "run-record? false for non-record"
    (assert-equal #f (run-record? '(not a record)))))

;;; ====
;;; Pipeline Definition Tests
;;; ====

(test-group "Pipeline Definition"
  (define-test "make-pipeline-def creates def"
    (let ([def (make-pipeline-def 'test-pipeline 'stage-placeholder '((model . opus)))])
      (assert-true (pipeline-def? def))))

  (define-test "pipeline-def-name extracts name"
    (let ([def (make-pipeline-def 'test-pipeline 'stage-placeholder '((model . opus)))])
      (assert-equal 'test-pipeline (pipeline-def-name def))))

  (define-test "pipeline-def-stage extracts stage"
    (let ([def (make-pipeline-def 'test-pipeline 'stage-placeholder '((model . opus)))])
      (assert-equal 'stage-placeholder (pipeline-def-stage def))))

  (define-test "pipeline-def-config extracts config"
    (let ([def (make-pipeline-def 'test-pipeline 'stage-placeholder '((model . opus)))])
      (assert-equal '((model . opus)) (pipeline-def-config def))))

  (define-test "pipeline-def? false for non-def"
    (assert-equal #f (pipeline-def? '(not a def)))))

;;; ====
;;; Persona Tests
;;; ====

(test-group "Persona"
  (define-test "make-persona creates persona"
    (let ([persona (make-persona 'sentinel "You are a code reviewer" '((tier . shepherd) (model . opus)))])
      (assert-true (persona? persona))))

  (define-test "persona-name extracts name"
    (let ([persona (make-persona 'sentinel "You are a code reviewer" '((tier . shepherd) (model . opus)))])
      (assert-equal 'sentinel (persona-name persona))))

  (define-test "persona-system-prompt extracts prompt"
    (let ([persona (make-persona 'sentinel "You are a code reviewer" '((tier . shepherd) (model . opus)))])
      (assert-equal "You are a code reviewer" (persona-system-prompt persona))))

  (define-test "persona-config extracts config"
    (let ([persona (make-persona 'sentinel "You are a code reviewer" '((tier . shepherd) (model . opus)))])
      (assert-equal '((tier . shepherd) (model . opus)) (persona-config persona))))

  (define-test "persona-tier extracts tier from config"
    (let ([persona (make-persona 'sentinel "You are a code reviewer" '((tier . shepherd) (model . opus)))])
      (assert-equal 'shepherd (persona-tier persona))))

  (define-test "persona-model extracts model from config"
    (let ([persona (make-persona 'sentinel "You are a code reviewer" '((tier . shepherd) (model . opus)))])
      (assert-equal 'opus (persona-model persona))))

  ;; Test defaults
  (define-test "persona-tier defaults to player"
    (let ([persona (make-persona 'basic "Prompt" '())])
      (assert-equal 'player (persona-tier persona))))

  (define-test "persona-model defaults to sonnet"
    (let ([persona (make-persona 'basic "Prompt" '())])
      (assert-equal 'sonnet (persona-model persona))))

  ;; Test channels
  (define-test "persona-channels returns read/write"
    (let ([persona (make-persona 'channeled "Prompt"
                                '((read-channels . (engineering design))
                                  (write-channels . (design))))])
      (assert-equal '((engineering design) . (design)) (persona-channels persona))))

  (define-test "persona? false for non-persona"
    (assert-equal #f (persona? '(not a persona)))))

;;; ====
;;; Schedule Tests
;;; ====

(test-group "Schedule"
  ;; Test cron schedule
  (define-test "make-cron-schedule creates schedule"
    (let ([sched (make-cron-schedule "0 6 * * *")])
      (assert-true (schedule? sched))))

  (define-test "schedule-type is cron"
    (let ([sched (make-cron-schedule "0 6 * * *")])
      (assert-equal 'cron (schedule-type sched))))

  (define-test "schedule-spec is cron expression"
    (let ([sched (make-cron-schedule "0 6 * * *")])
      (assert-equal "0 6 * * *" (schedule-spec sched))))

  ;; Test interval schedule
  (define-test "interval schedule-type is interval"
    (let ([sched (make-interval-schedule 3600)])
      (assert-equal 'interval (schedule-type sched))))

  (define-test "interval schedule-spec is seconds"
    (let ([sched (make-interval-schedule 3600)])
      (assert-equal 3600 (schedule-spec sched))))

  ;; Test tag schedule
  (define-test "tag schedule-type is tag"
    (let ([sched (make-tag-schedule "@opus")])
      (assert-equal 'tag (schedule-type sched))))

  (define-test "tag schedule-spec is pattern"
    (let ([sched (make-tag-schedule "@opus")])
      (assert-equal "@opus" (schedule-spec sched))))

  ;; Test event schedule
  (define-test "event schedule-type is event"
    (let ([sched (make-event-schedule 'commit)])
      (assert-equal 'event (schedule-type sched))))

  (define-test "event schedule-spec is event name"
    (let ([sched (make-event-schedule 'commit)])
      (assert-equal 'commit (schedule-spec sched))))

  ;; Test manual schedule
  (define-test "make-manual-schedule is schedule"
    (assert-true (schedule? make-manual-schedule)))

  (define-test "manual schedule-type is manual"
    (assert-equal 'manual (schedule-type make-manual-schedule)))

  (define-test "manual schedule-spec is #f"
    (assert-equal #f (schedule-spec make-manual-schedule)))

  (define-test "schedule? false for non-schedule"
    (assert-equal #f (schedule? '(not a schedule)))))

;;; ====
;;; Retry Policy Tests
;;; ====

(test-group "Retry Policy"
  (define-test "make-retry-policy creates policy"
    (let ([policy (make-retry-policy 3 (lambda (n) (* n 1000)) (lambda (c) #t) 'fail)])
      (assert-true (retry-policy? policy))))

  (define-test "retry-max-attempts extracts max"
    (let ([policy (make-retry-policy 3 (lambda (n) (* n 1000)) (lambda (c) #t) 'fail)])
      (assert-equal 3 (retry-max-attempts policy))))

  (define-test "retry-delay-fn works"
    (let ([policy (make-retry-policy 3 (lambda (n) (* n 1000)) (lambda (c) #t) 'fail)])
      (assert-equal 2000 ((retry-delay-fn policy) 2))))

  (define-test "retry-on-predicate works"
    (let ([policy (make-retry-policy 3 (lambda (n) (* n 1000)) (lambda (c) #t) 'fail)])
      (assert-true ((retry-on-predicate policy) 'any-code))))

  (define-test "retry-on-exhaust extracts action"
    (let ([policy (make-retry-policy 3 (lambda (n) (* n 1000)) (lambda (c) #t) 'fail)])
      (assert-equal 'fail (retry-on-exhaust policy))))

  ;; Test standard policies
  (define-test "retry-none is policy"
    (assert-true (retry-policy? retry-none)))

  (define-test "retry-none has 0 attempts"
    (assert-equal 0 (retry-max-attempts retry-none)))

  (define-test "retry-exponential is policy"
    (assert-true (retry-policy? retry-exponential)))

  (define-test "retry-exponential has 3 attempts"
    (assert-equal 3 (retry-max-attempts retry-exponential)))

  (define-test "retry-exponential delay is exponential"
    (assert-equal 4000 ((retry-delay-fn retry-exponential) 2)))  ;; 1000 * 2^2

  (define-test "retry-linear is policy"
    (assert-true (retry-policy? retry-linear)))

  (define-test "retry-linear has 5 attempts"
    (assert-equal 5 (retry-max-attempts retry-linear)))

  (define-test "retry-linear delay is linear"
    (assert-equal 3000 ((retry-delay-fn retry-linear) 3)))

  (define-test "retry-immediate is policy"
    (assert-true (retry-policy? retry-immediate)))

  (define-test "retry-immediate has 0 delay"
    (assert-equal 0 ((retry-delay-fn retry-immediate) 5)))

  (define-test "retry-policy? false for non-policy"
    (assert-equal #f (retry-policy? '(not a policy)))))

;;; ====
;;; Context/State Pair Tests
;;; ====

(test-group "Context/State Pair"
  (define-test "ctx-of extracts context"
    (let ([cs (make-ctx-state empty-context empty-state)])
      (assert-equal empty-context (ctx-of cs))))

  (define-test "state-of extracts state"
    (let ([cs (make-ctx-state empty-context empty-state)])
      (assert-equal empty-state (state-of cs))))

  ;; Test update-state
  (define-test "update-state modifies state"
    (let* ([cs (make-ctx-state empty-context empty-state)]
           [updated (update-state cs (lambda (s) (state-add-log s 'entry)))])
      (assert-equal '(entry) (state-log (state-of updated)))))

  (define-test "update-state preserves context"
    (let* ([cs (make-ctx-state empty-context empty-state)]
           [updated (update-state cs (lambda (s) (state-add-log s 'entry)))])
      (assert-equal empty-context (ctx-of updated))))

  ;; Test update-ctx
  (define-test "update-ctx modifies context"
    (let* ([cs (make-ctx-state empty-context empty-state)]
           [updated (update-ctx cs (lambda (c) (ctx-with-fuel c 500)))])
      (assert-equal 500 (ctx-fuel (ctx-of updated)))))

  (define-test "update-ctx preserves state"
    (let* ([cs (make-ctx-state empty-context empty-state)]
           [updated (update-ctx cs (lambda (c) (ctx-with-fuel c 500)))])
      (assert-equal empty-state (state-of updated)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
