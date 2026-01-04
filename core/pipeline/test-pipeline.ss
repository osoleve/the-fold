;;; core/pipeline/test-pipeline.ss — Tests for Pipeline Subsystem
;;;
;;; Tests stage algebra, context/state management, FSM, and composition.

(load "core/test-framework.ss")
(load "core/pipeline/context.ss")
(load "core/pipeline/stage.ss")
(load "core/pipeline/dsl.ss")

;;; ============================================================
;;; Stage Result Tests
;;; ============================================================

(test-group "StageResult constructors and predicates"
            
            (define-test stage-ok-creates-ok-result
              (let ([r (stage-ok 42)])
                   (assert-true (stage-result? r))
                   (assert-true (stage-ok? r))
                   (assert-equal 42 (stage-result-value r))))
            
            (define-test stage-err-creates-error-result
              (let ([r (stage-err 'not-found "Item not found" '())])
                   (assert-true (stage-result? r))
                   (assert-true (stage-err? r))
                   (assert-equal 'not-found (stage-err-code r))
                   (assert-equal "Item not found" (stage-err-message r))))
            
            (define-test stage-skip-creates-skip-result
              (let ([r (stage-skip "condition not met")])
                   (assert-true (stage-result? r))
                   (assert-true (stage-skip? r))))
            
            (define-test stage-halt-creates-halt-result
              (let ([r (stage-halt "critical failure")])
                   (assert-true (stage-result? r))
                   (assert-true (stage-halt? r))))
            
            (define-test stage-retry-creates-retry-result
              (let ([r (stage-retry "rate limited" 1000)])
                   (assert-true (stage-result? r))
                   (assert-true (stage-retry? r))
                   (assert-equal "rate limited" (stage-retry-reason r))
                   (assert-equal 1000 (stage-retry-delay r))))
            
            (define-test stage-await-creates-await-result
              (let ([r (stage-await 'external-signal)])
                   (assert-true (stage-result? r))
                   (assert-true (stage-await? r)))))

;;; ============================================================
;;; Basic Stage Tests
;;; ============================================================

(test-group "Basic stage constructors"
            
            (define-test stage-pure-ignores-input
              (let* ([s (stage-pure 99)]
                     [r (run-stage s empty-context 'anything)])
                    (assert-true (stage-ok? r))
                    (assert-equal 99 (stage-result-value r))))
            
            (define-test stage-read-passes-input-through
              (let* ([r (run-stage stage-read empty-context 'hello)])
                    (assert-true (stage-ok? r))
                    (assert-equal 'hello (stage-result-value r))))
            
            (define-test stage-ask-returns-context
              (let* ([ctx (make-pipeline-context '((key . value)) #f "sess" #f '() 100)]
                     [r (run-stage stage-ask ctx 'ignored)])
                    (assert-true (stage-ok? r))
                    (assert-true (pipeline-context? (stage-result-value r)))
                    (assert-equal '((key . value)) (ctx-config (stage-result-value r)))))
            
            (define-test stage-asks-extracts-from-context
              (let* ([ctx (make-pipeline-context '() #f "my-session" #f '() 100)]
                     [s (stage-asks ctx-session-id)]
                     [r (run-stage s ctx 'ignored)])
                    (assert-true (stage-ok? r))
                    (assert-equal "my-session" (stage-result-value r))))
            
            (define-test stage-arr-applies-function
              (let* ([s (stage-arr (lambda (x) (* x 2)))]
                     [r (run-stage s empty-context 21)])
                    (assert-true (stage-ok? r))
                    (assert-equal 42 (stage-result-value r)))))

;;; ============================================================
;;; Stage Composition Tests
;;; ============================================================

(test-group "Stage composition"
            
            (define-test stage-compose-chains-stages
              (let* ([s1 (stage-arr (lambda (x) (+ x 1)))]
                     [s2 (stage-arr (lambda (x) (* x 2)))]
                     [composed (stage->>> s1 s2)]
                     [r (run-stage composed empty-context 5)])
                    (assert-true (stage-ok? r))
                    (assert-equal 12 (stage-result-value r))))  ; (5+1)*2
            
            (define-test stage-compose-propagates-errors
              (let* ([s1 (make-stage 'fail (lambda (ctx i) (stage-err 'oops "error" '())))]
                     [s2 (stage-arr (lambda (x) (* x 2)))]
                     [composed (stage->>> s1 s2)]
                     [r (run-stage composed empty-context 5)])
                    (assert-true (stage-err? r))
                    (assert-equal 'oops (stage-err-code r))))
            
            (define-test stage-fanout-pairs-results
              (let* ([s1 (stage-arr (lambda (x) (+ x 1)))]
                     [s2 (stage-arr (lambda (x) (* x 2)))]
                     [fanout (stage-&&& s1 s2)]
                     [r (run-stage fanout empty-context 5)])
                    (assert-true (stage-ok? r))
                    (assert-equal '(6 . 10) (stage-result-value r))))  ; (5+1, 5*2)
            
            (define-test stage-fanout-handles-effects-from-both-branches
              (let* ([s1 (make-stage 'eff1 (lambda (ctx i) (list 'stage-effect 'llm "prompt1" i)))]
                     [s2 (make-stage 'eff2 (lambda (ctx i) (list 'stage-effect 'shell "cmd" i)))]
                     [fanout (stage-&&& s1 s2)]
                     [r (run-stage fanout empty-context "input")])
                    (assert-true (stage-effect? r))
                    (assert-equal 'fanout (list-ref r 1))))
            
            (define-test stage-fanout-handles-effect-plus-ok
              (let* ([s1 (make-stage 'eff (lambda (ctx i) (list 'stage-effect 'llm "prompt" i)))]
                     [s2 (stage-arr (lambda (x) (* x 2)))]
                     [fanout (stage-&&& s1 s2)]
                     [r (run-stage fanout empty-context 5)])
                    (assert-true (stage-effect? r))
                    (assert-equal 'fanout (list-ref r 1))))
            
            (define-test stage-catch-handles-errors
              (let* ([s (make-stage 'fail (lambda (ctx i) (stage-err 'oops "error" '())))]
                     [handler (lambda (err) (stage-pure 'recovered))]
                     [caught (stage-catch handler s)]
                     [r (run-stage caught empty-context 'anything)])
                    (assert-true (stage-ok? r))
                    (assert-equal 'recovered (stage-result-value r)))))

;;; ============================================================
;;; Context Tests
;;; ============================================================

(test-group "Pipeline context"
            
            (define-test empty-context-has-default-fuel
              (assert-equal 10000 (ctx-fuel empty-context)))
            
            (define-test ctx-with-fuel-updates-fuel
              (let ([ctx (ctx-with-fuel empty-context 500)])
                   (assert-equal 500 (ctx-fuel ctx))))
            
            (define-test ctx-consume-fuel-decreases-fuel
              (let ([ctx (ctx-consume-fuel empty-context 100)])
                   (assert-equal 9900 (ctx-fuel ctx))))
            
            (define-test ctx-extend-env-adds-to-environment
              (let ([ctx (ctx-extend-env empty-context '((key . value)))])
                   (assert-equal 'value (ctx-env-ref ctx 'key))))
            
            (define-test ctx-env-ref-returns-false-for-missing-key
              (assert-false (ctx-env-ref empty-context 'missing))))

;;; ============================================================
;;; Pipeline State Tests
;;; ============================================================

(test-group "Pipeline state"
            
            (define-test empty-state-has-empty-collections
              (assert-true (null? (state-log empty-state)))
              (assert-true (null? (state-artifacts empty-state)))
              (assert-true (null? (state-cache empty-state))))
            
            (define-test state-cache-put-adds-entry
              (let ([st (state-cache-put empty-state 'key 'value)])
                   (assert-equal 'value (state-cache-get st 'key))))
            
            (define-test state-cache-put-replaces-existing-entry
              (let* ([st1 (state-cache-put empty-state 'key 'old-value)]
                     [st2 (state-cache-put st1 'key 'new-value)])
                    (assert-equal 'new-value (state-cache-get st2 'key))
                    ;; Verify no duplicate entries (memory leak fix)
                    (assert-equal 1 (length (state-cache st2)))))
            
            (define-test state-set-checkpoint-replaces-entry
              (let* ([st1 (state-set-checkpoint empty-state 'cp 'v1)]
                     [st2 (state-set-checkpoint st1 'cp 'v2)])
                    (assert-equal 'v2 (state-get-checkpoint st2 'cp))
                    (assert-equal 1 (length (state-checkpoints st2))))))

;;; ============================================================
;;; FSM Pipeline Tests
;;; ============================================================

(test-group "FSM pipeline"
            
            (define-test fsm-pipeline-runs-through-states
              (let* ([states `((start . (,(stage-arr (lambda (x) (+ x 1))) (ok . middle)))
                               (middle . (,(stage-arr (lambda (x) (* x 2))) (ok . end)))
                               (end . (,(stage-arr (lambda (x) (- x 1))))))]
                     [fsm (fsm-pipeline states 'start '(end))]
                     [r (run-stage fsm empty-context 5)])
                    (assert-true (stage-ok? r))
                    (assert-equal 11 (stage-result-value r))))  ; ((5+1)*2)-1 = 11
            
            (define-test fsm-pipeline-respects-accepting-states
              (let* ([states `((start . (,(stage-arr (lambda (x) (+ x 1))) (ok . final)))
                               (final . (,(stage-arr (lambda (x) (* x 100))))))]
                     [fsm (fsm-pipeline states 'start '(final))]
                     [r (run-stage fsm empty-context 5)])
                    (assert-true (stage-ok? r))
                    ;; Should run final stage, then stop
                    (assert-equal 600 (stage-result-value r))))  ; (5+1)*100
            
            (define-test fsm-pipeline-uses-context-fuel
              (let* ([states `((loop . (,stage-read (ok . loop))))]
                     [fsm (fsm-pipeline states 'loop '())]
                     [ctx (ctx-with-fuel empty-context 5)]
                     [r (run-stage fsm ctx 'data)])
                    (assert-true (stage-err? r))
                    (assert-true (eq? (stage-err-code r) 'fsm-exhausted))))
            
            (define-test fsm-pipeline-errors-on-invalid-state
              (let* ([states `((start . (,(stage-arr (lambda (x) x)) (ok . nonexistent))))]
                     [fsm (fsm-pipeline states 'start '())]
                     [r (run-stage fsm empty-context 'data)])
                    (assert-true (stage-err? r))
                    (assert-equal 'fsm-invalid-state (stage-err-code r)))))

;;; ============================================================
;;; DSL Utilities Tests
;;; ============================================================

(test-group "DSL utilities"
            
            (define-test retry-retries-on-failure
              (let* ([call-count 0]
                     [flaky (make-stage 'flaky
                                        (lambda (ctx i)
                                                (set! call-count (+ call-count 1))
                                                (if (< call-count 3)
                                                    (stage-err 'transient "failed" '())
                                                    (stage-ok 'success))))]
                     [with-retry (retry 5 flaky)]
                     [r (run-stage with-retry empty-context 'data)])
                    (assert-true (stage-ok? r))
                    (assert-equal 'success (stage-result-value r))
                    (assert-equal 3 call-count)))
            
            (define-test try-all-tries-alternatives
              (let* ([s1 (make-stage 'fail1 (lambda (ctx i) (stage-err 'e1 "error 1" '())))]
                     [s2 (make-stage 'fail2 (lambda (ctx i) (stage-err 'e2 "error 2" '())))]
                     [s3 (stage-pure 'success)]
                     [pipeline (try-all (list s1 s2 s3))]
                     [r (run-stage pipeline empty-context 'data)])
                    (assert-true (stage-ok? r))
                    (assert-equal 'success (stage-result-value r))))
            
            (define-test gate-blocks-on-false-predicate
              (let* ([s (stage-arr (lambda (x) (* x 2)))]
                     [gated (gate (lambda (x) (> x 10)) s)]
                     [r (run-stage gated empty-context 5)])
                    (assert-true (stage-skip? r))))
            
            (define-test gate-allows-on-true-predicate
              (let* ([s (stage-arr (lambda (x) (* x 2)))]
                     [gated (gate (lambda (x) (> x 10)) s)]
                     [r (run-stage gated empty-context 20)])
                    (assert-true (stage-ok? r))
                    (assert-equal 40 (stage-result-value r))))
            
            (define-test map-stage-processes-list
              (let* ([s (stage-arr (lambda (x) (* x 2)))]
                     [mapped (map-stage s)]
                     [r (run-stage mapped empty-context '(1 2 3 4))])
                    (assert-true (stage-ok? r))
                    (assert-equal '(2 4 6 8) (stage-result-value r)))))

;;; ============================================================
;;; Effect Tagging Tests
;;; ============================================================

(test-group "Effect tagging consistency"
            
            (define-test race-uses-stage-effect-tag
              (let* ([s1 (stage-pure 'a)]
                     [s2 (stage-pure 'b)]
                     [r (run-stage (race (list s1 s2)) empty-context 'input)])
                    (assert-true (stage-effect? r))
                    (assert-equal 'race (list-ref r 1))))
            
            (define-test make-effect-stage-uses-stage-effect-tag
              (let* ([s (make-effect-stage 'custom "payload")]
                     [r (run-stage s empty-context 'input)])
                    (assert-true (list? r))
                    (assert-equal 'stage-effect (car r))
                    (assert-equal 'custom (list-ref r 1)))))

;;; ============================================================
;;; Run tests
;;; ============================================================

(run-tests 'pipeline)
