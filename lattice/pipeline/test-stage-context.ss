(load "core/testing/test-framework.ss")
(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/context.ss")

(doc 'module 'test-stage-context)
(doc 'description "Tests for pipeline stage algebra and context types")

;;; ====
;;; StageResult constructors and predicates
;;; ====

(test-group stage-result-types

  (define-test "stage-ok creates ok result"
    (let ([r (stage-ok 42)])
      (assert-true (stage-ok? r))
      (assert-equal 42 (stage-result-value r))))

  (define-test "stage-err creates error result"
    (let ([r (stage-err 'timeout "timed out" '())])
      (assert-true (stage-err? r))
      (assert-equal 'timeout (stage-err-code r))
      (assert-equal "timed out" (stage-err-message r))))

  (define-test "stage-retry creates retry result"
    (let ([r (stage-retry "busy" 1000)])
      (assert-true (stage-retry? r))
      (assert-equal "busy" (stage-retry-reason r))
      (assert-equal 1000 (stage-retry-delay r))))

  (define-test "stage-skip creates skip result"
    (let ([r (stage-skip "not applicable")])
      (assert-true (stage-skip? r))))

  (define-test "stage-halt creates halt result"
    (let ([r (stage-halt "emergency")])
      (assert-true (stage-halt? r))))

  (define-test "stage-await creates await result"
    (let ([r (stage-await 'approval-needed)])
      (assert-true (stage-await? r))))

  (define-test "predicates are mutually exclusive"
    (let ([ok (stage-ok 1)]
          [err (stage-err 'e "e" '())]
          [skip (stage-skip "s")])
      (assert-false (stage-err? ok))
      (assert-false (stage-ok? err))
      (assert-false (stage-halt? skip))))

  (define-test "stage-result? identifies results"
    (assert-true (stage-result? (stage-ok 1)))
    (assert-false (stage-result? '(not a result))))

  (define-test "stage-result-tag extracts tag"
    (assert-equal 'ok (stage-result-tag (stage-ok 1)))
    (assert-equal 'err (stage-result-tag (stage-err 'x "x" '())))))

;;; ====
;;; Stage constructors
;;; ====

(test-group stage-constructors

  (define-test "stage-pure injects value"
    (let ([r (run-stage (stage-pure 99) 'ctx 'ignored)])
      (assert-true (stage-ok? r))
      (assert-equal 99 (stage-result-value r))))

  (define-test "stage-read passes input through"
    (let ([r (run-stage stage-read 'ctx "hello")])
      (assert-true (stage-ok? r))
      (assert-equal "hello" (stage-result-value r))))

  (define-test "stage-ask returns context"
    (let ([r (run-stage stage-ask 'my-context 'input)])
      (assert-true (stage-ok? r))
      (assert-equal 'my-context (stage-result-value r))))

  (define-test "stage-asks projects context"
    (let ([s (stage-asks (lambda (ctx) (+ ctx 10)))])
      (assert-equal 15 (stage-result-value (run-stage s 5 'input)))))

  (define-test "stage-fail produces error"
    (let ([r (run-stage (stage-fail 'bad "oops") 'ctx 'input)])
      (assert-true (stage-err? r))
      (assert-equal 'bad (stage-err-code r))))

  (define-test "stage-halt-with produces halt"
    (let ([r (run-stage (stage-halt-with "stop") 'ctx 'input)])
      (assert-true (stage-halt? r))))

  (define-test "stage? predicate"
    (assert-true (stage? (stage-pure 1)))
    (assert-false (stage? '(not a stage))))

  (define-test "stage-name returns name"
    (assert-equal 'pure (stage-name (stage-pure 1))))

  (define-test "stage-arr lifts pure function"
    (let ([r (run-stage (stage-arr (lambda (x) (* x 2))) 'ctx 5)])
      (assert-equal 10 (stage-result-value r))))

  (define-test "stage-local modifies context"
    (let ([local-s (stage-local (lambda (ctx) (+ ctx 100)) stage-ask)])
      (assert-equal 105 (stage-result-value (run-stage local-s 5 'input))))))

;;; ====
;;; Arrow composition
;;; ====

(test-group stage-composition

  (define-test "stage->>> composes left to right"
    (let* ([double (stage-arr (lambda (x) (* x 2)))]
           [inc (stage-arr (lambda (x) (+ x 1)))]
           [composed (stage->>> double inc)])
      (assert-equal 11 (stage-result-value (run-stage composed 'ctx 5)))))

  (define-test "stage-<<< composes right to left"
    (let* ([double (stage-arr (lambda (x) (* x 2)))]
           [inc (stage-arr (lambda (x) (+ x 1)))]
           [composed (stage-<<< double inc)])
      (assert-equal 12 (stage-result-value (run-stage composed 'ctx 5)))))

  (define-test "error propagates through >>>"
    (let* ([fail (stage-fail 'boom "exploded")]
           [inc (stage-arr (lambda (x) (+ x 1)))]
           [composed (stage->>> fail inc)])
      (assert-true (stage-err? (run-stage composed 'ctx 5)))))

  (define-test "stage-first applies to car of pair"
    (let* ([double (stage-arr (lambda (x) (* x 2)))]
           [s (stage-first double)]
           [r (run-stage s 'ctx (cons 3 "kept"))])
      (assert-equal (cons 6 "kept") (stage-result-value r))))

  (define-test "stage-second applies to cdr of pair"
    (let* ([double (stage-arr (lambda (x) (* x 2)))]
           [s (stage-second double)]
           [r (run-stage s 'ctx (cons "kept" 4))])
      (assert-equal (cons "kept" 8) (stage-result-value r))))

  (define-test "stage-fanout pairs two results"
    (let* ([s1 (stage-arr (lambda (x) (* x 2)))]
           [s2 (stage-arr (lambda (x) (+ x 10)))]
           [s (stage-&&& s1 s2)]
           [r (run-stage s 'ctx 5)])
      (assert-equal (cons 10 15) (stage-result-value r)))))

;;; ====
;;; Arrow choice
;;; ====

(test-group stage-choice

  (define-test "stage-left applies to left values"
    (let* ([double (stage-arr (lambda (x) (* x 2)))]
           [s (stage-left double)]
           [r (run-stage s 'ctx (left 5))])
      (assert-true (left? (stage-result-value r)))
      (assert-equal 10 (from-left (stage-result-value r)))))

  (define-test "stage-left passes through right values"
    (let* ([double (stage-arr (lambda (x) (* x 2)))]
           [s (stage-left double)]
           [r (run-stage s 'ctx (right "hello"))])
      (assert-true (right? (stage-result-value r)))))

  (define-test "stage-choice routes left"
    (let* ([s (stage-choice (stage-arr (lambda (x) (* x 2)))
                            (stage-arr (lambda (x) (+ x 100))))]
           [r (run-stage s 'ctx (left 5))])
      (assert-equal 10 (stage-result-value r))))

  (define-test "stage-choice routes right"
    (let* ([s (stage-choice (stage-arr (lambda (x) (* x 2)))
                            (stage-arr (lambda (x) (+ x 100))))]
           [r (run-stage s 'ctx (right 5))])
      (assert-equal 105 (stage-result-value r)))))

;;; ====
;;; Conditional stages
;;; ====

(test-group stage-conditionals

  (define-test "stage-if selects then branch"
    (let* ([s (stage-if (lambda (x) (> x 0))
                        (stage-pure 'positive)
                        (stage-pure 'non-positive))]
           [r (run-stage s 'ctx 5)])
      (assert-equal 'positive (stage-result-value r))))

  (define-test "stage-if selects else branch"
    (let* ([s (stage-if (lambda (x) (> x 0))
                        (stage-pure 'positive)
                        (stage-pure 'non-positive))]
           [r (run-stage s 'ctx -3)])
      (assert-equal 'non-positive (stage-result-value r))))

  (define-test "stage-when runs on true"
    (let* ([s (stage-when (lambda (x) (> x 0))
                          (stage-arr (lambda (x) (* x 10))))]
           [r (run-stage s 'ctx 5)])
      (assert-equal 50 (stage-result-value r))))

  (define-test "stage-when passes through on false"
    (let* ([s (stage-when (lambda (x) (> x 0))
                          (stage-arr (lambda (x) (* x 10))))]
           [r (run-stage s 'ctx -3)])
      (assert-equal -3 (stage-result-value r))))

  (define-test "stage-guard passes on true"
    (let ([r (run-stage (stage-guard positive? "must be positive") 'ctx 5)])
      (assert-true (stage-ok? r))
      (assert-equal 5 (stage-result-value r))))

  (define-test "stage-guard fails on false"
    (let ([r (run-stage (stage-guard positive? "must be positive") 'ctx -1)])
      (assert-true (stage-err? r))))

  (define-test "stage-case matches first true predicate"
    (let* ([s (stage-case
                (list (cons (lambda (x) (< x 0)) (stage-pure 'negative))
                      (cons (lambda (x) (= x 0)) (stage-pure 'zero)))
                (stage-pure 'positive))]
           [r (run-stage s 'ctx 0)])
      (assert-equal 'zero (stage-result-value r))))

  (define-test "stage-case falls through to default"
    (let* ([s (stage-case
                (list (cons (lambda (x) (< x 0)) (stage-pure 'negative)))
                (stage-pure 'non-negative))]
           [r (run-stage s 'ctx 5)])
      (assert-equal 'non-negative (stage-result-value r)))))

;;; ====
;;; Monadic interface
;;; ====

(test-group stage-monadic

  (define-test "stage-bind chains computation"
    (let* ([s (stage-bind (stage-pure 5)
                          (lambda (x) (stage-pure (* x 3))))]
           [r (run-stage s 'ctx 'ignored)])
      (assert-equal 15 (stage-result-value r))))

  (define-test "stage-bind propagates error"
    (let* ([s (stage-bind (stage-fail 'e "err")
                          (lambda (x) (stage-pure "unreachable")))]
           [r (run-stage s 'ctx 'input)])
      (assert-true (stage-err? r))))

  (define-test "stage-map transforms output"
    (let* ([s (stage-map (lambda (x) (string-append x "!")) (stage-pure "hi"))]
           [r (run-stage s 'ctx 'ignored)])
      (assert-equal "hi!" (stage-result-value r))))

  (define-test "stage-sequence collects results"
    (let* ([stages (list (stage-pure 1) (stage-pure 2) (stage-pure 3))]
           [s (stage-sequence stages)]
           [r (run-stage s 'ctx 'ignored)])
      (assert-equal '(1 2 3) (stage-result-value r))))

  (define-test "stage-sequence empty list"
    (let ([r (run-stage (stage-sequence '()) 'ctx 'ignored)])
      (assert-equal '() (stage-result-value r)))))

;;; ====
;;; Iteration
;;; ====

(test-group stage-iteration

  (define-test "stage-repeat applies n times"
    (let* ([inc (stage-arr (lambda (x) (+ x 1)))]
           [s (stage-repeat 5 inc)]
           [r (run-stage s 'ctx 0)])
      (assert-equal 5 (stage-result-value r))))

  (define-test "stage-repeat 0 times is identity"
    (let* ([inc (stage-arr (lambda (x) (+ x 1)))]
           [s (stage-repeat 0 inc)]
           [r (run-stage s 'ctx 42)])
      (assert-equal 42 (stage-result-value r))))

  (define-test "stage-while loops until predicate fails"
    (let* ([s (stage-while (lambda (x) (< x 10))
                           (stage-arr (lambda (x) (+ x 3))))]
           [r (run-stage s 'ctx 0)])
      (assert-equal 12 (stage-result-value r))))

  (define-test "stage-while respects fuel limit"
    (let* ([s (stage-while (lambda (x) #t)
                           (stage-arr (lambda (x) (+ x 1))))]
           [r (run-stage s 'ctx 0)])
      (assert-true (stage-err? r))))

  (define-test "stage-fold accumulates"
    (let* ([s (stage-fold
                (lambda (acc x) (stage-pure (+ acc x)))
                0
                '(1 2 3 4 5))]
           [r (run-stage s 'ctx 'ignored)])
      (assert-equal 15 (stage-result-value r)))))

;;; ====
;;; Error handling
;;; ====

(test-group stage-error-handling

  (define-test "stage-catch recovers from error"
    (let* ([s (stage-catch
                (lambda (err) (stage-pure 'recovered))
                (stage-fail 'boom "exploded"))]
           [r (run-stage s 'ctx 'input)])
      (assert-true (stage-ok? r))
      (assert-equal 'recovered (stage-result-value r))))

  (define-test "stage-catch passes through ok"
    (let* ([s (stage-catch
                (lambda (err) (stage-pure 'recovered))
                (stage-pure 42))]
           [r (run-stage s 'ctx 'input)])
      (assert-equal 42 (stage-result-value r))))

  (define-test "stage-default returns default on error"
    (let* ([s (stage-default 'fallback (stage-fail 'e "err"))]
           [r (run-stage s 'ctx 'input)])
      (assert-equal 'fallback (stage-result-value r))))

  (define-test "stage-optional wraps ok in some"
    (let* ([s (stage-optional (stage-pure 42))]
           [r (run-stage s 'ctx 'input)])
      (assert-equal '(some 42) (stage-result-value r))))

  (define-test "stage-optional returns empty on error"
    (let* ([s (stage-optional (stage-fail 'e "err"))]
           [r (run-stage s 'ctx 'input)])
      (assert-equal '() (stage-result-value r)))))

;;; ====
;;; Utility stages
;;; ====

(test-group stage-utilities

  (define-test "stage-id passes through"
    (assert-equal 7 (stage-result-value (run-stage stage-id 'ctx 7))))

  (define-test "stage-dup duplicates"
    (assert-equal (cons 3 3) (stage-result-value (run-stage stage-dup 'ctx 3))))

  (define-test "stage-swap swaps pair"
    (assert-equal (cons 'b 'a) (stage-result-value (run-stage stage-swap 'ctx (cons 'a 'b)))))

  (define-test "stage-fst extracts car"
    (assert-equal 1 (stage-result-value (run-stage stage-fst 'ctx (cons 1 2)))))

  (define-test "stage-snd extracts cdr"
    (assert-equal 2 (stage-result-value (run-stage stage-snd 'ctx (cons 1 2))))))

;;; ====
;;; Effects
;;; ====

(test-group stage-effects

  (define-test "make-effect-stage creates effect"
    (let ([r (run-stage (make-effect-stage 'llm-call '(prompt "hi")) 'ctx 'input)])
      (assert-true (stage-effect? r))
      (assert-equal 'llm-call (stage-effect-type r))
      (assert-equal '(prompt "hi") (stage-effect-payload r))))

  (define-test "stage-effect-input captures input"
    (let ([r (run-stage (make-effect-stage 'log '()) 'ctx 'my-input)])
      (assert-equal 'my-input (stage-effect-input r)))))

;;; ====
;;; Pipeline construction
;;; ====

(test-group pipeline-construction

  (define-test "pipeline sequences stages"
    (let* ([p (pipeline 'my-pipe
                (stage-arr (lambda (x) (* x 2)))
                (stage-arr (lambda (x) (+ x 1))))]
           [r (run-stage p 'ctx 5)])
      (assert-equal 11 (stage-result-value r))))

  (define-test "pipeline short-circuits on error"
    (let* ([p (pipeline 'fail-pipe
                (stage-fail 'e "err")
                (stage-arr (lambda (x) "unreachable")))]
           [r (run-stage p 'ctx 'input)])
      (assert-true (stage-err? r))))

  (define-test "empty pipeline passes through"
    (let* ([p (pipeline 'empty)]
           [r (run-stage p 'ctx 42)])
      (assert-equal 42 (stage-result-value r)))))

;;; ====
;;; PipelineContext
;;; ====

(test-group pipeline-context

  (define-test "make-pipeline-context creates context"
    (let ([ctx (make-pipeline-context '((model . gpt4)) #f "sess-1" 'run-1 '() 5000)])
      (assert-true (pipeline-context? ctx))
      (assert-equal "sess-1" (ctx-session-id ctx))
      (assert-equal 5000 (ctx-fuel ctx))))

  (define-test "empty-context defaults"
    (assert-true (pipeline-context? empty-context))
    (assert-equal "default" (ctx-session-id empty-context))
    (assert-equal 10000 (ctx-fuel empty-context)))

  (define-test "ctx-with-config replaces config"
    (let ([ctx (ctx-with-config empty-context '((model . opus)))])
      (assert-equal 'opus (ctx-config-ref ctx 'model))))

  (define-test "ctx-with-fuel sets fuel"
    (let ([ctx (ctx-with-fuel empty-context 500)])
      (assert-equal 500 (ctx-fuel ctx))))

  (define-test "ctx-consume-fuel decreases fuel"
    (let ([ctx (ctx-consume-fuel (ctx-with-fuel empty-context 100) 30)])
      (assert-equal 70 (ctx-fuel ctx))))

  (define-test "ctx-consume-fuel floors at 0"
    (let ([ctx (ctx-consume-fuel (ctx-with-fuel empty-context 10) 100)])
      (assert-equal 0 (ctx-fuel ctx))))

  (define-test "ctx-with-env sets environment"
    (let ([ctx (ctx-with-env empty-context '((debug . #t)))])
      (assert-equal #t (ctx-env-ref ctx 'debug))))

  (define-test "ctx-extend-env adds to environment"
    (let* ([ctx (ctx-with-env empty-context '((a . 1)))]
           [ctx2 (ctx-extend-env ctx '((b . 2)))])
      (assert-equal 1 (ctx-env-ref ctx2 'a))
      (assert-equal 2 (ctx-env-ref ctx2 'b))))

  (define-test "ctx-env-ref returns #f for missing key"
    (assert-false (ctx-env-ref empty-context 'nonexistent)))

  (define-test "ctx-with-session replaces session"
    (let ([ctx (ctx-with-session empty-context "new-session")])
      (assert-equal "new-session" (ctx-session-id ctx)))))

;;; ====
;;; PipelineState
;;; ====

(test-group pipeline-state

  (define-test "empty-state has empty collections"
    (assert-true (pipeline-state? empty-state))
    (assert-equal '() (state-log empty-state))
    (assert-equal '() (state-artifacts empty-state))
    (assert-equal '() (state-metrics empty-state)))

  (define-test "state-add-log prepends entry"
    (let* ([st (state-add-log empty-state '(info "test" ()))]
           [st2 (state-add-log st '(warn "alert" ()))])
      (assert-equal 2 (length (state-log st2)))))

  (define-test "state-add-artifact stores named value"
    (let ([st (state-add-artifact empty-state 'result "42")])
      (assert-equal 1 (length (state-artifacts st)))
      (assert-equal "42" (cdar (state-artifacts st)))))

  (define-test "state-set-checkpoint overwrites by name"
    (let* ([st (state-set-checkpoint empty-state 'step1 'v1)]
           [st2 (state-set-checkpoint st 'step1 'v2)])
      (assert-equal 1 (length (state-checkpoints st2)))
      (assert-equal 'v2 (state-get-checkpoint st2 'step1))))

  (define-test "state-get-checkpoint returns #f for missing"
    (assert-false (state-get-checkpoint empty-state 'nonexistent)))

  (define-test "state-add-metric stores value"
    (let ([st (state-add-metric empty-state 'latency 42)])
      (assert-equal 42 (cdar (state-metrics st)))))

  (define-test "state-cache round-trip"
    (let* ([st (state-cache-put empty-state 'key1 'val1)]
           [st2 (state-cache-put st 'key2 'val2)])
      (assert-equal 'val1 (state-cache-get st2 'key1))
      (assert-equal 'val2 (state-cache-get st2 'key2))))

  (define-test "state-cache-get returns #f for missing"
    (assert-false (state-cache-get empty-state 'missing))))

;;; ====
;;; Persona
;;; ====

(test-group persona-types

  (define-test "make-persona creates persona"
    (let ([p (make-persona 'analyst "You are an analyst" '((tier . shepherd) (model . opus)))])
      (assert-true (persona? p))
      (assert-equal 'analyst (persona-name p))
      (assert-equal "You are an analyst" (persona-system-prompt p))))

  (define-test "persona-tier extracts tier"
    (let ([p (make-persona 'test "prompt" '((tier . builder)))])
      (assert-equal 'builder (persona-tier p))))

  (define-test "persona-tier defaults to player"
    (let ([p (make-persona 'test "prompt" '())])
      (assert-equal 'player (persona-tier p))))

  (define-test "persona-model extracts model"
    (let ([p (make-persona 'test "prompt" '((model . haiku)))])
      (assert-equal 'haiku (persona-model p))))

  (define-test "persona-model defaults to sonnet"
    (let ([p (make-persona 'test "prompt" '())])
      (assert-equal 'sonnet (persona-model p)))))

;;; ====
;;; Schedule types
;;; ====

(test-group schedule-types

  (define-test "cron schedule"
    (let ([s (make-cron-schedule "0 * * * *")])
      (assert-true (schedule? s))
      (assert-equal 'cron (schedule-type s))
      (assert-equal "0 * * * *" (schedule-spec s))))

  (define-test "interval schedule"
    (let ([s (make-interval-schedule 300)])
      (assert-equal 'interval (schedule-type s))
      (assert-equal 300 (schedule-spec s))))

  (define-test "tag schedule"
    (let ([s (make-tag-schedule "#review")])
      (assert-equal 'tag (schedule-type s))))

  (define-test "event schedule"
    (let ([s (make-event-schedule 'commit-pushed)])
      (assert-equal 'event (schedule-type s))))

  (define-test "manual schedule"
    (assert-true (schedule? make-manual-schedule))
    (assert-equal 'manual (schedule-type make-manual-schedule))))

;;; ====
;;; Retry policies
;;; ====

(test-group retry-policies

  (define-test "retry-none has 0 attempts"
    (assert-true (retry-policy? retry-none))
    (assert-equal 0 (retry-max-attempts retry-none)))

  (define-test "retry-exponential has 3 attempts"
    (assert-equal 3 (retry-max-attempts retry-exponential)))

  (define-test "retry-exponential delay doubles"
    (let ([delay-fn (retry-delay-fn retry-exponential)])
      (assert-equal 1000 (delay-fn 0))
      (assert-equal 2000 (delay-fn 1))
      (assert-equal 4000 (delay-fn 2))))

  (define-test "retry-linear delay increases linearly"
    (let ([delay-fn (retry-delay-fn retry-linear)])
      (assert-equal 1000 (delay-fn 1))
      (assert-equal 3000 (delay-fn 3))))

  (define-test "retry-immediate has 0 delay"
    (let ([delay-fn (retry-delay-fn retry-immediate)])
      (assert-equal 0 (delay-fn 5)))))

;;; ====
;;; Context/State pair threading
;;; ====

(test-group ctx-state-pair

  (define-test "make-ctx-state creates pair"
    (let ([cs (make-ctx-state empty-context empty-state)])
      (assert-true (pipeline-context? (ctx-of cs)))
      (assert-true (pipeline-state? (state-of cs)))))

  (define-test "update-state applies function"
    (let* ([cs (make-ctx-state empty-context empty-state)]
           [cs2 (update-state cs (lambda (st) (state-add-metric st 'x 1)))])
      (assert-equal 1 (length (state-metrics (state-of cs2))))))

  (define-test "update-ctx applies function"
    (let* ([cs (make-ctx-state empty-context empty-state)]
           [cs2 (update-ctx cs (lambda (ctx) (ctx-with-fuel ctx 999)))])
      (assert-equal 999 (ctx-fuel (ctx-of cs2))))))

;;; ====
;;; Run record
;;; ====

(test-group run-record

  (define-test "make-run-record creates record"
    (let ([r (make-run-record 'my-pipe 'run-1 "input" "output"
                               empty-state 'success 0 100 #f)])
      (assert-true (run-record? r))
      (assert-equal 'my-pipe (run-pipeline-name r))
      (assert-equal 'success (run-status r)))))

;;; ====
;;; Pipeline definition
;;; ====

(test-group pipeline-definition

  (define-test "make-pipeline-def creates def"
    (let ([p (make-pipeline-def 'test-pipe (stage-pure 1) '((model . opus)))])
      (assert-true (pipeline-def? p))
      (assert-equal 'test-pipe (pipeline-def-name p)))))

(run-all-tests-and-exit)
