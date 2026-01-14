;;; fabric/stitches/pipeline/tests/test-context.ss — Comprehensive Tests for Pipeline Context
;;;
;;; Tests context and state management for pipelines.

(load "lattice/pipeline/context.ss")

;;; ====
;;; Test Framework (minimal)
;;; ====

(define *test-count* 0)
(define *test-passed* 0)
(define *test-failed* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *test-passed* (+ *test-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *test-failed* (+ *test-failed* 1))
       (display (format "  FAIL: ~a\n    Expected: ~s\n    Actual:   ~s\n"
                        name expected actual)))))

(define (test-pred name pred actual)
  (set! *test-count* (+ *test-count* 1))
  (if (pred actual)
      (begin
       (set! *test-passed* (+ *test-passed* 1))
       (display (format "  PASS: ~a\n" name)))
      (begin
       (set! *test-failed* (+ *test-failed* 1))
       (display (format "  FAIL: ~a\n    Value did not satisfy predicate: ~s\n"
                        name actual)))))

(define (run-tests name thunk)
  (display (format "\n=== ~a ===\n" name))
  (thunk))

(define (test-summary)
  (display (format "\n=== Summary ===\n"))
  (display (format "Total: ~a  Passed: ~a  Failed: ~a\n"
                   *test-count* *test-passed* *test-failed*))
  (if (= *test-failed* 0)
      (display "All tests passed!\n")
      (begin
       (display "SOME TESTS FAILED\n")
       (exit 1))))

;;; ====
;;; PipelineContext Tests
;;; ====

(run-tests "PipelineContext Construction"
           (lambda ()
                   ;; Test make-pipeline-context
                   (let ([ctx (make-pipeline-context '((a . 1)) 'persona "session" "run" '((x . 10)) 5000)])
                        (test-pred "make-pipeline-context creates context"
                                   pipeline-context?
                                   ctx)
                        
                        (test "ctx-config extracts config"
                              '((a . 1))
                              (ctx-config ctx))
                        
                        (test "ctx-persona extracts persona"
                              'persona
                              (ctx-persona ctx))
                        
                        (test "ctx-session-id extracts session-id"
                              "session"
                              (ctx-session-id ctx))
                        
                        (test "ctx-run-id extracts run-id"
                              "run"
                              (ctx-run-id ctx))
                        
                        (test "ctx-env extracts env"
                              '((x . 10))
                              (ctx-env ctx))
                        
                        (test "ctx-fuel extracts fuel"
                              5000
                              (ctx-fuel ctx)))
                   
                   ;; Test empty-context
                   (test-pred "empty-context is a context"
                              pipeline-context?
                              empty-context)
                   
                   (test "empty-context has empty config"
                         '()
                         (ctx-config empty-context))
                   
                   (test "empty-context has #f persona"
                         #f
                         (ctx-persona empty-context))
                   
                   (test "empty-context has default session-id"
                         "default"
                         (ctx-session-id empty-context))
                   
                   (test "empty-context has 10000 fuel"
                         10000
                         (ctx-fuel empty-context))
                   
                   (test "pipeline-context? false for non-context"
                         #f
                         (pipeline-context? '(not a context)))))

;;; ====
;;; Context Builders
;;; ====

(run-tests "Context Builders"
           (lambda ()
                   ;; Test ctx-with-config
                   (let* ([ctx empty-context]
                          [updated (ctx-with-config ctx '((key . value)))])
                         (test "ctx-with-config sets config"
                               '((key . value))
                               (ctx-config updated))
                         
                         (test "ctx-with-config preserves other fields"
                               (ctx-fuel ctx)
                               (ctx-fuel updated)))
                   
                   ;; Test ctx-with-persona
                   (let* ([ctx empty-context]
                          [updated (ctx-with-persona ctx 'test-persona)])
                         (test "ctx-with-persona sets persona"
                               'test-persona
                               (ctx-persona updated)))
                   
                   ;; Test ctx-with-session
                   (let* ([ctx empty-context]
                          [updated (ctx-with-session ctx "my-session")])
                         (test "ctx-with-session sets session-id"
                               "my-session"
                               (ctx-session-id updated)))
                   
                   ;; Test ctx-with-run-id
                   (let* ([ctx empty-context]
                          [updated (ctx-with-run-id ctx "run-123")])
                         (test "ctx-with-run-id sets run-id"
                               "run-123"
                               (ctx-run-id updated)))
                   
                   ;; Test ctx-with-env
                   (let* ([ctx empty-context]
                          [updated (ctx-with-env ctx '((a . 1) (b . 2)))])
                         (test "ctx-with-env sets env"
                               '((a . 1) (b . 2))
                               (ctx-env updated)))
                   
                   ;; Test ctx-extend-env
                   (let* ([ctx (ctx-with-env empty-context '((x . 10)))]
                          [updated (ctx-extend-env ctx '((y . 20)))])
                         (test "ctx-extend-env adds to env"
                               '((y . 20) (x . 10))
                               (ctx-env updated)))
                   
                   ;; Test ctx-with-fuel
                   (let* ([ctx empty-context]
                          [updated (ctx-with-fuel ctx 1000)])
                         (test "ctx-with-fuel sets fuel"
                               1000
                               (ctx-fuel updated)))
                   
                   ;; Test ctx-consume-fuel
                   (let* ([ctx (ctx-with-fuel empty-context 1000)]
                          [updated (ctx-consume-fuel ctx 100)])
                         (test "ctx-consume-fuel reduces fuel"
                               900
                               (ctx-fuel updated)))
                   
                   (let* ([ctx (ctx-with-fuel empty-context 50)]
                          [updated (ctx-consume-fuel ctx 100)])
                         (test "ctx-consume-fuel bottoms out at 0"
                               0
                               (ctx-fuel updated)))))

;;; ====
;;; Context Lookup
;;; ====

(run-tests "Context Lookup"
           (lambda ()
                   ;; Test ctx-env-ref
                   (let ([ctx (ctx-with-env empty-context '((a . 1) (b . 2) (c . 3)))])
                        (test "ctx-env-ref finds existing key"
                              2
                              (ctx-env-ref ctx 'b))
                        
                        (test "ctx-env-ref returns #f for missing key"
                              #f
                              (ctx-env-ref ctx 'missing)))
                   
                   ;; Test ctx-config-ref
                   (let ([ctx (ctx-with-config empty-context '((model . opus) (timeout . 30000)))])
                        (test "ctx-config-ref finds existing key"
                              'opus
                              (ctx-config-ref ctx 'model))
                        
                        (test "ctx-config-ref returns #f for missing key"
                              #f
                              (ctx-config-ref ctx 'missing)))))

;;; ====
;;; PipelineState Tests
;;; ====

(run-tests "PipelineState Construction"
           (lambda ()
                   ;; Test make-pipeline-state
                   (let ([state (make-pipeline-state '(log-entry) '((art . val)) '((cp . data)) '((m . 1)) '((k . v)))])
                        (test-pred "make-pipeline-state creates state"
                                   pipeline-state?
                                   state)
                        
                        (test "state-log extracts log"
                              '(log-entry)
                              (state-log state))
                        
                        (test "state-artifacts extracts artifacts"
                              '((art . val))
                              (state-artifacts state))
                        
                        (test "state-checkpoints extracts checkpoints"
                              '((cp . data))
                              (state-checkpoints state))
                        
                        (test "state-metrics extracts metrics"
                              '((m . 1))
                              (state-metrics state))
                        
                        (test "state-cache extracts cache"
                              '((k . v))
                              (state-cache state)))
                   
                   ;; Test empty-state
                   (test-pred "empty-state is a state"
                              pipeline-state?
                              empty-state)
                   
                   (test "empty-state has empty log"
                         '()
                         (state-log empty-state))
                   
                   (test "empty-state has empty artifacts"
                         '()
                         (state-artifacts empty-state))
                   
                   (test "pipeline-state? false for non-state"
                         #f
                         (pipeline-state? '(not a state)))))

;;; ====
;;; State Updaters
;;; ====

(run-tests "State Updaters"
           (lambda ()
                   ;; Test state-add-log
                   (let* ([state empty-state]
                          [updated (state-add-log state 'entry1)]
                          [updated2 (state-add-log updated 'entry2)])
                         (test "state-add-log adds entry"
                               '(entry1)
                               (state-log updated))
                         
                         (test "state-add-log prepends entries"
                               '(entry2 entry1)
                               (state-log updated2)))
                   
                   ;; Test state-add-artifact
                   (let* ([state empty-state]
                          [updated (state-add-artifact state 'output "result")])
                         (test "state-add-artifact adds artifact"
                               '((output . "result"))
                               (state-artifacts updated)))
                   
                   ;; Test state-set-checkpoint
                   (let* ([state empty-state]
                          [updated (state-set-checkpoint state 'cp1 "data1")]
                          [updated2 (state-set-checkpoint updated 'cp2 "data2")]
                          [updated3 (state-set-checkpoint updated2 'cp1 "data1-new")])
                         (test "state-set-checkpoint adds checkpoint"
                               '((cp1 . "data1"))
                               (state-checkpoints updated))
                         
                         (test "state-set-checkpoint multiple"
                               '((cp2 . "data2") (cp1 . "data1"))
                               (state-checkpoints updated2))
                         
                         (test "state-set-checkpoint replaces existing"
                               '((cp1 . "data1-new") (cp2 . "data2"))
                               (state-checkpoints updated3)))
                   
                   ;; Test state-get-checkpoint
                   (let ([state (state-set-checkpoint empty-state 'my-cp '(some data))])
                        (test "state-get-checkpoint finds existing"
                              '(some data)
                              (state-get-checkpoint state 'my-cp))
                        
                        (test "state-get-checkpoint returns #f for missing"
                              #f
                              (state-get-checkpoint state 'unknown)))
                   
                   ;; Test state-add-metric
                   (let* ([state empty-state]
                          [updated (state-add-metric state 'latency 42.5)])
                         (test "state-add-metric adds metric"
                               '((latency . 42.5))
                               (state-metrics updated)))
                   
                   ;; Test state-cache-put
                   (let* ([state empty-state]
                          [updated (state-cache-put state "key1" "value1")])
                         (test "state-cache-put adds to cache"
                               '(("key1" . "value1"))
                               (state-cache updated)))
                   
                   ;; Test state-cache-get
                   (let ([state (state-cache-put empty-state "my-key" '(cached data))])
                        (test "state-cache-get finds existing"
                              '(cached data)
                              (state-cache-get state "my-key"))
                        
                        (test "state-cache-get returns #f for missing"
                              #f
                              (state-cache-get state "unknown")))))

;;; ====
;;; Log Entry Tests
;;; ====

(run-tests "Log Entry"
           (lambda ()
                   (let ([entry (make-log-entry 'info "Test message" '(data))])
                        (test "log-entry-level extracts level"
                              'info
                              (log-entry-level entry))
                        
                        (test "log-entry-message extracts message"
                              "Test message"
                              (log-entry-message entry))
                        
                        (test "log-entry-data extracts data"
                              '(data)
                              (log-entry-data entry))
                        
                        (test "log-entry-timestamp returns number"
                              0  ; current-timestamp returns 0 in core
                              (log-entry-timestamp entry)))))

;;; ====
;;; Run Record Tests
;;; ====

(run-tests "Run Record"
           (lambda ()
                   (let ([record (make-run-record 'my-pipeline
                                                  "run-123"
                                                  "input"
                                                  "output"
                                                  empty-state
                                                  'success
                                                  1000
                                                  2000
                                                  "parent-run")])
                        (test-pred "make-run-record creates record"
                                   run-record?
                                   record)
                        
                        (test "run-pipeline-name extracts name"
                              'my-pipeline
                              (run-pipeline-name record))
                        
                        (test "run-id-of extracts run-id"
                              "run-123"
                              (run-id-of record))
                        
                        (test "run-input extracts input"
                              "input"
                              (run-input record))
                        
                        (test "run-output extracts output"
                              "output"
                              (run-output record))
                        
                        (test-pred "run-final-state is state"
                                   pipeline-state?
                                   (run-final-state record))
                        
                        (test "run-status extracts status"
                              'success
                              (run-status record))
                        
                        (test "run-started-at extracts start time"
                              1000
                              (run-started-at record))
                        
                        (test "run-finished-at extracts finish time"
                              2000
                              (run-finished-at record))
                        
                        (test "run-parent-id extracts parent"
                              "parent-run"
                              (run-parent-id record)))
                   
                   (test "run-record? false for non-record"
                         #f
                         (run-record? '(not a record)))))

;;; ====
;;; Pipeline Definition Tests
;;; ====

(run-tests "Pipeline Definition"
           (lambda ()
                   (let ([def (make-pipeline-def 'test-pipeline 'stage-placeholder '((model . opus)))])
                        (test-pred "make-pipeline-def creates def"
                                   pipeline-def?
                                   def)
                        
                        (test "pipeline-def-name extracts name"
                              'test-pipeline
                              (pipeline-def-name def))
                        
                        (test "pipeline-def-stage extracts stage"
                              'stage-placeholder
                              (pipeline-def-stage def))
                        
                        (test "pipeline-def-config extracts config"
                              '((model . opus))
                              (pipeline-def-config def)))
                   
                   (test "pipeline-def? false for non-def"
                         #f
                         (pipeline-def? '(not a def)))))

;;; ====
;;; Persona Tests
;;; ====

(run-tests "Persona"
           (lambda ()
                   (let ([persona (make-persona 'sentinel "You are a code reviewer" '((tier . shepherd) (model . opus)))])
                        (test-pred "make-persona creates persona"
                                   persona?
                                   persona)
                        
                        (test "persona-name extracts name"
                              'sentinel
                              (persona-name persona))
                        
                        (test "persona-system-prompt extracts prompt"
                              "You are a code reviewer"
                              (persona-system-prompt persona))
                        
                        (test "persona-config extracts config"
                              '((tier . shepherd) (model . opus))
                              (persona-config persona))
                        
                        (test "persona-tier extracts tier from config"
                              'shepherd
                              (persona-tier persona))
                        
                        (test "persona-model extracts model from config"
                              'opus
                              (persona-model persona)))
                   
                   ;; Test defaults
                   (let ([persona (make-persona 'basic "Prompt" '())])
                        (test "persona-tier defaults to player"
                              'player
                              (persona-tier persona))
                        
                        (test "persona-model defaults to sonnet"
                              'sonnet
                              (persona-model persona)))
                   
                   ;; Test channels
                   (let ([persona (make-persona 'channeled "Prompt"
                                                '((read-channels . (engineering design))
                                                  (write-channels . (design))))])
                        (test "persona-channels returns read/write"
                              '((engineering design) . (design))
                              (persona-channels persona)))
                   
                   (test "persona? false for non-persona"
                         #f
                         (persona? '(not a persona)))))

;;; ====
;;; Schedule Tests
;;; ====

(run-tests "Schedule"
           (lambda ()
                   ;; Test cron schedule
                   (let ([sched (make-cron-schedule "0 6 * * *")])
                        (test-pred "make-cron-schedule creates schedule"
                                   schedule?
                                   sched)
                        
                        (test "schedule-type is cron"
                              'cron
                              (schedule-type sched))
                        
                        (test "schedule-spec is cron expression"
                              "0 6 * * *"
                              (schedule-spec sched)))
                   
                   ;; Test interval schedule
                   (let ([sched (make-interval-schedule 3600)])
                        (test "interval schedule-type is interval"
                              'interval
                              (schedule-type sched))
                        
                        (test "interval schedule-spec is seconds"
                              3600
                              (schedule-spec sched)))
                   
                   ;; Test tag schedule
                   (let ([sched (make-tag-schedule "@opus")])
                        (test "tag schedule-type is tag"
                              'tag
                              (schedule-type sched))
                        
                        (test "tag schedule-spec is pattern"
                              "@opus"
                              (schedule-spec sched)))
                   
                   ;; Test event schedule
                   (let ([sched (make-event-schedule 'commit)])
                        (test "event schedule-type is event"
                              'event
                              (schedule-type sched))
                        
                        (test "event schedule-spec is event name"
                              'commit
                              (schedule-spec sched)))
                   
                   ;; Test manual schedule
                   (test-pred "make-manual-schedule is schedule"
                              schedule?
                              make-manual-schedule)
                   
                   (test "manual schedule-type is manual"
                         'manual
                         (schedule-type make-manual-schedule))
                   
                   (test "manual schedule-spec is #f"
                         #f
                         (schedule-spec make-manual-schedule))
                   
                   ;; Test Discord schedules
                   (let ([sched (make-discord-mention-schedule 'catalyst)])
                        (test "discord-mention schedule-type"
                              'discord-mention
                              (schedule-type sched))
                        
                        (test "discord-mention schedule-spec"
                              'catalyst
                              (schedule-spec sched)))
                   
                   (let ([sched (make-discord-reaction-schedule ":fire:")])
                        (test "discord-reaction schedule-type"
                              'discord-reaction
                              (schedule-type sched)))
                   
                   (let ([sched (make-discord-keyword-schedule "help")])
                        (test "discord-keyword schedule-type"
                              'discord-keyword
                              (schedule-type sched)))
                   
                   (test "schedule? false for non-schedule"
                         #f
                         (schedule? '(not a schedule)))))

;;; ====
;;; Retry Policy Tests
;;; ====

(run-tests "Retry Policy"
           (lambda ()
                   (let ([policy (make-retry-policy 3 (lambda (n) (* n 1000)) (lambda (c) #t) 'fail)])
                        (test-pred "make-retry-policy creates policy"
                                   retry-policy?
                                   policy)
                        
                        (test "retry-max-attempts extracts max"
                              3
                              (retry-max-attempts policy))
                        
                        (test "retry-delay-fn works"
                              2000
                              ((retry-delay-fn policy) 2))
                        
                        (test "retry-on-predicate works"
                              #t
                              ((retry-on-predicate policy) 'any-code))
                        
                        (test "retry-on-exhaust extracts action"
                              'fail
                              (retry-on-exhaust policy)))
                   
                   ;; Test standard policies
                   (test-pred "retry-none is policy"
                              retry-policy?
                              retry-none)
                   
                   (test "retry-none has 0 attempts"
                         0
                         (retry-max-attempts retry-none))
                   
                   (test-pred "retry-exponential is policy"
                              retry-policy?
                              retry-exponential)
                   
                   (test "retry-exponential has 3 attempts"
                         3
                         (retry-max-attempts retry-exponential))
                   
                   (test "retry-exponential delay is exponential"
                         4000  ;; 1000 * 2^2
                         ((retry-delay-fn retry-exponential) 2))
                   
                   (test-pred "retry-linear is policy"
                              retry-policy?
                              retry-linear)
                   
                   (test "retry-linear has 5 attempts"
                         5
                         (retry-max-attempts retry-linear))
                   
                   (test "retry-linear delay is linear"
                         3000
                         ((retry-delay-fn retry-linear) 3))
                   
                   (test-pred "retry-immediate is policy"
                              retry-policy?
                              retry-immediate)
                   
                   (test "retry-immediate has 0 delay"
                         0
                         ((retry-delay-fn retry-immediate) 5))
                   
                   (test "retry-policy? false for non-policy"
                         #f
                         (retry-policy? '(not a policy)))))

;;; ====
;;; Context/State Pair Tests
;;; ====

(run-tests "Context/State Pair"
           (lambda ()
                   (let ([cs (make-ctx-state empty-context empty-state)])
                        (test "ctx-of extracts context"
                              empty-context
                              (ctx-of cs))
                        
                        (test "state-of extracts state"
                              empty-state
                              (state-of cs)))
                   
                   ;; Test update-state
                   (let* ([cs (make-ctx-state empty-context empty-state)]
                          [updated (update-state cs (lambda (s) (state-add-log s 'entry)))])
                         (test "update-state modifies state"
                               '(entry)
                               (state-log (state-of updated)))
                         
                         (test "update-state preserves context"
                               empty-context
                               (ctx-of updated)))
                   
                   ;; Test update-ctx
                   (let* ([cs (make-ctx-state empty-context empty-state)]
                          [updated (update-ctx cs (lambda (c) (ctx-with-fuel c 500)))])
                         (test "update-ctx modifies context"
                               500
                               (ctx-fuel (ctx-of updated)))
                         
                         (test "update-ctx preserves state"
                               empty-state
                               (state-of updated)))))

;;; ====
;;; Discord Context Tests
;;; ====

(run-tests "Discord Context"
           (lambda ()
                   ;; Test make-discord-context
                   (let* ([trigger-data '((message-id . "123")
                                          (channel-id . "456")
                                          (author-id . "789")
                                          (author . "TestUser")
                                          (guild-id . "guild-1")
                                          (body . "Hello!"))]
                          [dc (make-discord-context trigger-data)])
                         (test "make-discord-context creates alist"
                               #t
                               (pair? dc))
                         
                         (test "discord context has message-id"
                               "123"
                               (cdr (assq 'discord-message-id dc)))
                         
                         (test "discord context has channel-id"
                               "456"
                               (cdr (assq 'discord-channel-id dc)))
                         
                         (test "discord context has author-id"
                               "789"
                               (cdr (assq 'discord-author-id dc)))
                         
                         (test "discord context has author-name"
                               "TestUser"
                               (cdr (assq 'discord-author-name dc)))
                         
                         (test "discord context has content"
                               "Hello!"
                               (cdr (assq 'discord-content dc))))
                   
                   ;; Test ctx-with-discord
                   (let* ([trigger-data '((message-id . "msg-1"))]
                          [ctx (ctx-with-discord empty-context trigger-data)])
                         (test "ctx-with-discord adds discord context"
                               "msg-1"
                               (ctx-env-ref ctx 'discord-message-id)))
                   
                   ;; Test discord context accessors
                   (let* ([trigger-data '((message-id . "m1")
                                          (channel-id . "c1")
                                          (author-id . "a1")
                                          (author . "User1")
                                          (guild-id . "g1")
                                          (body . "Content"))]
                          [ctx (ctx-with-discord empty-context trigger-data)])
                         (test "ctx-discord-message-id"
                               "m1"
                               (ctx-discord-message-id ctx))
                         
                         (test "ctx-discord-channel-id"
                               "c1"
                               (ctx-discord-channel-id ctx))
                         
                         (test "ctx-discord-author-id"
                               "a1"
                               (ctx-discord-author-id ctx))
                         
                         (test "ctx-discord-author-name"
                               "User1"
                               (ctx-discord-author-name ctx))
                         
                         (test "ctx-discord-guild-id"
                               "g1"
                               (ctx-discord-guild-id ctx))
                         
                         (test "ctx-discord-content"
                               "Content"
                               (ctx-discord-content ctx))
                         
                         (test "ctx-is-discord-triggered? true"
                               #t
                               (ctx-is-discord-triggered? ctx)))
                   
                   ;; Test non-discord context
                   (test "ctx-discord-message-id returns #f for non-discord"
                         #f
                         (ctx-discord-message-id empty-context))
                   
                   (test "ctx-is-discord-triggered? false for non-discord"
                         #f
                         (ctx-is-discord-triggered? empty-context))))

;;; ====
;;; Run All Tests
;;; ====

(test-summary)
