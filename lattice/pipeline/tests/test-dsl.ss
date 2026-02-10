;;; fabric/stitches/pipeline/tests/test-dsl.ss — Comprehensive Tests for Pipeline DSL
;;;
;;; Tests the user-facing pipeline DSL.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/pipeline/dsl.ss")

;;; ====
;;; Test Context
;;; ====

(define test-ctx empty-context)

;;; ====
;;; Pipeline Definition Tests
;;; ====

(test-group "Pipeline Definition"
  (define-test "define-pipeline* creates pipeline-def"
    (let ([pipe (define-pipeline* 'my-pipeline
                  '((model . opus) (fuel . 5000))
                  (stage-pure 42))])
      (assert-true (pipeline-def? pipe))))

  (define-test "pipeline has correct name"
    (let ([pipe (define-pipeline* 'my-pipeline
                  '((model . opus) (fuel . 5000))
                  (stage-pure 42))])
      (assert-equal 'my-pipeline (pipeline-def-name pipe))))

  (define-test "pipeline has correct config"
    (let ([pipe (define-pipeline* 'my-pipeline
                  '((model . opus) (fuel . 5000))
                  (stage-pure 42))])
      (assert-equal '((model . opus) (fuel . 5000)) (pipeline-def-config pipe))))

  (define-test "pipeline stage is stage"
    (let ([pipe (define-pipeline* 'my-pipeline
                  '((model . opus) (fuel . 5000))
                  (stage-pure 42))])
      (assert-true (stage? (pipeline-def-stage pipe))))))

;;; ====
;;; Stage Naming Tests
;;; ====

(test-group "Stage Naming"
  (define-test "named-stage creates stage"
    (let ([s (named-stage 'my-named-stage (stage-pure 42))])
      (assert-true (stage? s))))

  (define-test "named-stage sets name"
    (let ([s (named-stage 'my-named-stage (stage-pure 42))])
      (assert-equal 'my-named-stage (stage-name s))))

  (define-test "named-stage preserves behavior"
    (let ([s (named-stage 'my-named-stage (stage-pure 42))])
      (assert-equal 42 (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "stage* is alias for named-stage"
    (let ([s (stage* 'aliased-stage stage-read)])
      (assert-equal 'aliased-stage (stage-name s)))))

;;; ====
;;; Configuration Helpers Tests
;;; ====

(test-group "Configuration Helpers"
  (define-test "config creates alist"
    (assert-equal '((a . 1) (b . 2))
                  (config (cons 'a 1) (cons 'b 2))))

  (define-test "with-model sets default-model in env"
    (let ([s (with-model 'opus
                         (stage-asks (lambda (ctx)
                                             (ctx-env-ref ctx 'default-model))))])
      (assert-equal 'opus (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "with-fuel sets fuel"
    (let ([s (with-fuel 1000 (stage-asks ctx-fuel))])
      (assert-equal 1000 (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "with-persona sets persona"
    (let* ([persona (make-persona 'test-persona "prompt" '())]
           [s (with-persona persona (stage-asks ctx-persona))])
      (assert-equal persona (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "with-env extends environment"
    (let ([s (with-env '((custom-key . custom-value))
                       (stage-asks (lambda (ctx)
                                           (ctx-env-ref ctx 'custom-key))))])
      (assert-equal 'custom-value (stage-result-value (run-stage s test-ctx "input"))))))

;;; ====
;;; Operator Aliases Tests
;;; ====

(test-group "Operator Aliases"
  (define-test "--> is alias for stage->>>"
    (let ([double (stage-arr (lambda (x) (* x 2)))]
          [add-one (stage-arr (lambda (x) (+ x 1)))])
      (assert-equal 11 (stage-result-value (run-stage (--> double add-one) test-ctx 5)))))

  (define-test "<-- is alias for stage-<<<"
    (let ([double (stage-arr (lambda (x) (* x 2)))]
          [add-one (stage-arr (lambda (x) (+ x 1)))])
      (assert-equal 11 (stage-result-value (run-stage (<-- add-one double) test-ctx 5)))))

  (define-test "pipe-into pipes value into stage constructor"
    (assert-equal 42 (stage-result-value (run-stage (pipe-into 42 stage-pure) test-ctx "ignored")))))

;;; ====
;;; Common Patterns Tests
;;; ====

(test-group "Common Patterns"
  (define-test "parse-json produces result"
    (let ([result (run-stage parse-json test-ctx "{\"key\": \"value\"}")])
      (assert-true (stage-ok? result))))

  (define-test "to-json produces result"
    (let ([result (run-stage to-json test-ctx '((key . value)))])
      (assert-true (stage-ok? result))))

  (define-test "split-lines produces result"
    (let ([result (run-stage split-lines test-ctx "line1\nline2\nline3")])
      (assert-true (stage-ok? result))))

  (define-test "join-lines produces result"
    (let ([result (run-stage join-lines test-ctx '("line1" "line2" "line3"))])
      (assert-true (stage-ok? result)))))

;;; ====
;;; Flow Control Patterns Tests
;;; ====

(test-group "Flow Control Patterns"
  (define-test "on-success chains stages"
    (let ([s (on-success (stage-pure 5) (stage-arr (lambda (x) (* x 2))))])
      (assert-equal 10 (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "on-failure uses fallback"
    (let ([s (on-failure (stage-fail 'err "message") (stage-pure 'fallback))])
      (assert-equal 'fallback (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "on-failure passes through success"
    (let ([s (on-failure (stage-pure 'success) (stage-pure 'fallback))])
      (assert-equal 'success (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "try-all finds first success"
    (let ([s (try-all (list (stage-fail 'e1 "first fails")
                            (stage-fail 'e2 "second fails")
                            (stage-pure 'third-wins)))])
      (assert-equal 'third-wins (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "try-all fails when all fail"
    (let ([s (try-all (list (stage-fail 'e1 "all fail") (stage-fail 'e2 "all fail")))])
      (assert-equal 'all-failed (stage-err-code (run-stage s test-ctx "input")))))

  (define-test "retry 0 runs once"
    (let ([s (retry 0 (stage-pure 42))])
      (assert-equal 42 (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "gate runs stage when condition met"
    (let ([s (gate positive? (stage-arr (lambda (x) (* x 2))))])
      (assert-equal 10 (stage-result-value (run-stage s test-ctx 5)))))

  (define-test "gate skips when condition not met"
    (let ([s (gate positive? (stage-arr (lambda (x) (* x 2))))])
      (assert-equal 'skip (stage-result-tag (run-stage s test-ctx -5))))))

;;; ====
;;; Collection Processing Tests
;;; ====

(test-group "Collection Processing"
  (define-test "map-stage applies to each element"
    (let ([s (map-stage (stage-arr (lambda (x) (* x 2))))])
      (assert-equal '(2 4 6 8 10) (stage-result-value (run-stage s test-ctx '(1 2 3 4 5))))))

  (define-test "map-stage on empty list"
    (let ([s (map-stage (stage-arr (lambda (x) (* x 2))))])
      (assert-equal '() (stage-result-value (run-stage s test-ctx '())))))

  (define-test "filter-stage keeps matching elements"
    (let ([s (filter-stage positive?)])
      (assert-equal '(1 3 5) (stage-result-value (run-stage s test-ctx '(-2 1 -1 3 0 5))))))

  (define-test "reduce-stage reduces list"
    (let ([s (reduce-stage (lambda (acc x) (stage-pure (+ acc x))) 0)])
      (assert-equal 15 (stage-result-value (run-stage s test-ctx '(1 2 3 4 5))))))

  (define-test "take-stage takes first n"
    (let ([s (take-stage 3)])
      (assert-equal '(1 2 3) (stage-result-value (run-stage s test-ctx '(1 2 3 4 5))))))

  (define-test "take-stage handles short list"
    (let ([s (take-stage 10)])
      (assert-equal '(1 2 3) (stage-result-value (run-stage s test-ctx '(1 2 3))))))

  (define-test "drop-stage drops first n"
    (let ([s (drop-stage 2)])
      (assert-equal '(3 4 5) (stage-result-value (run-stage s test-ctx '(1 2 3 4 5))))))

  (define-test "drop-stage handles short list"
    (let ([s (drop-stage 10)])
      (assert-equal '() (stage-result-value (run-stage s test-ctx '(1 2 3)))))))

;;; ====
;;; Parallel Execution Tests
;;; ====

(test-group "Parallel Execution"
  (define-test "parallel runs all stages"
    (let ([s (parallel (list (stage-arr (lambda (x) (* x 2)))
                             (stage-arr (lambda (x) (* x 3)))
                             (stage-arr (lambda (x) (+ x 1)))))])
      (assert-equal '(10 15 6) (stage-result-value (run-stage s test-ctx 5)))))

  (define-test "parallel with empty list"
    (let ([s (parallel '())])
      (assert-equal '() (stage-result-value (run-stage s test-ctx 5)))))

  (define-test "all-of collects all results"
    (let ([s (all-of (list (stage-pure 1) (stage-pure 2) (stage-pure 3)))])
      (assert-equal '(1 2 3) (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "race produces effect"
    (let ([s (race (list (stage-pure 'first) (stage-pure 'second)))])
      (assert-true (stage-effect? (run-stage s test-ctx "input"))))))

;;; ====
;;; FSM Pipeline Tests
;;; ====

(test-group "FSM Pipeline"
  (define-test "fsm-pipeline transitions through states"
    (let ([fsm (fsm-pipeline
                (list
                 (list 'start (stage-arr (lambda (x) (+ x 1)))
                       (cons 'ok 'processing))
                 (list 'processing (stage-arr (lambda (x) (* x 2)))
                       (cons 'ok 'done)))
                'start
                '(done))])
      (assert-equal 12 (stage-result-value (run-stage fsm test-ctx 5)))))

  (define-test "fsm-pipeline reaches done on success"
    (let ([fsm (fsm-pipeline
                (list
                 (list 'start
                       (stage-if positive?
                                 (stage-arr (lambda (x) x))
                                 (stage-fail 'negative "not positive"))
                       (cons 'ok 'done)
                       (cons 'negative 'error-state))
                 (list 'error-state
                       (stage-pure 'error-handled)
                       (cons 'ok 'handled)))
                'start
                '(done handled))])
      (assert-equal 5 (stage-result-value (run-stage fsm test-ctx 5)))))

  (define-test "fsm-pipeline transitions on error"
    (let ([fsm (fsm-pipeline
                (list
                 (list 'start
                       (stage-if positive?
                                 (stage-arr (lambda (x) x))
                                 (stage-fail 'negative "not positive"))
                       (cons 'ok 'done)
                       (cons 'negative 'error-state))
                 (list 'error-state
                       (stage-pure 'error-handled)
                       (cons 'ok 'handled)))
                'start
                '(done handled))])
      (assert-equal 'error-handled (stage-result-value (run-stage fsm test-ctx -5)))))

  (define-test "fsm-pipeline errors on invalid state"
    (let ([fsm (fsm-pipeline '() 'nonexistent '(done))])
      (assert-equal 'fsm-invalid-state (stage-err-code (run-stage fsm test-ctx 42))))))

;;; ====
;;; Convenience Constructors Tests
;;; ====

(test-group "Convenience Constructors"
  (define-test "ask-llm creates stage"
    (let ([s (ask-llm 'opus "Test prompt")])
      (assert-true (stage? s))))

  (define-test "ask-llm produces llm effect"
    (let ([s (ask-llm 'opus "Test prompt")])
      (assert-equal 'llm (stage-effect-type (run-stage s test-ctx "input")))))

  (define-test "run-fold produces fold effect"
    (let ([s (run-fold "(+ 1 2)")])
      (assert-equal 'fold (stage-effect-type (run-stage s test-ctx "input")))))

  (define-test "run-shell produces shell effect"
    (let ([s (run-shell "ls -la")])
      (assert-equal 'shell (stage-effect-type (run-stage s test-ctx "input")))))

  (define-test "fetch-url produces http effect"
    (let ([s (fetch-url "https://example.com")])
      (assert-equal 'http (stage-effect-type (run-stage s test-ctx "input"))))))

;;; ====
;;; Chain and Branch Tests
;;; ====

(test-group "Chain and Branch"
  (define-test "chain sequences stages"
    (let ([s (chain (stage-arr (lambda (x) (* x 2)))
                    (stage-arr (lambda (x) (+ x 1)))
                    (stage-arr (lambda (x) (* x 3))))])
      (assert-equal 33 (stage-result-value (run-stage s test-ctx 5)))))

  (define-test "empty chain is identity"
    (let ([s (chain)])
      (assert-equal 42 (stage-result-value (run-stage s test-ctx 42)))))

  (define-test "single-element chain"
    (let ([s (chain (stage-arr (lambda (x) (* x 2))))])
      (assert-equal 10 (stage-result-value (run-stage s test-ctx 5)))))

  (define-test "branch takes then on true"
    (let ([s (branch positive?
                     (stage-arr (lambda (x) (* x 2)))
                     (stage-arr (lambda (x) (- x))))])
      (assert-equal 10 (stage-result-value (run-stage s test-ctx 5)))))

  (define-test "branch takes else on false"
    (let ([s (branch positive?
                     (stage-arr (lambda (x) (* x 2)))
                     (stage-arr (lambda (x) (- x))))])
      (assert-equal 5 (stage-result-value (run-stage s test-ctx -5)))))

  (define-test "switch matches predicates"
    (let ([s (switch (list (cons negative? (stage-pure 'negative))
                           (cons zero? (stage-pure 'zero)))
                     (stage-pure 'positive))])
      (assert-equal 'negative (stage-result-value (run-stage s test-ctx -5)))))

  (define-test "switch falls through"
    (let ([s (switch (list (cons negative? (stage-pure 'negative))
                           (cons zero? (stage-pure 'zero)))
                     (stage-pure 'positive))])
      (assert-equal 'positive (stage-result-value (run-stage s test-ctx 5)))))

  (define-test "tap passes value through"
    (let ([side-effect-value #f])
      (assert-equal 42 (stage-result-value
                        (run-stage (tap (lambda (x) (set! side-effect-value x)))
                                   test-ctx 42)))))

  (define-test "tap executes side effect"
    (let ([side-effect-value #f])
      (stage-result-value
       (run-stage (tap (lambda (x) (set! side-effect-value x)))
                  test-ctx 42))
      (assert-equal 42 side-effect-value))))

;;; ====
;;; Logging Helpers Tests
;;; ====

(test-group "Logging Helpers"
  (define-test "log creates log effect"
    (let ([s (log "Info message")])
      (assert-equal 'log (stage-effect-type (run-stage s test-ctx "input")))))

  (define-test "debug creates debug log"
    (let* ([s (debug "Debug message")]
           [result (run-stage s test-ctx "input")])
      (assert-equal (list 'debug "Debug message") (stage-effect-payload result))))

  (define-test "warn creates warn log"
    (let* ([s (warn "Warning message")]
           [result (run-stage s test-ctx "input")])
      (assert-equal (list 'warn "Warning message") (stage-effect-payload result)))))

;;; ====
;;; Checkpoint Helpers Tests
;;; ====

(test-group "Checkpoint Helpers"
  (define-test "save creates checkpoint effect"
    (let ([s (save 'my-checkpoint)])
      (assert-equal 'checkpoint (stage-effect-type (run-stage s test-ctx "input")))))

  (define-test "save has save payload"
    (let ([s (save 'my-checkpoint)])
      (assert-equal (list 'save 'my-checkpoint) (stage-effect-payload (run-stage s test-ctx "input")))))

  (define-test "load-checkpoint creates checkpoint effect"
    (let ([s (load-checkpoint 'my-checkpoint)])
      (assert-equal 'checkpoint (stage-effect-type (run-stage s test-ctx "input")))))

  (define-test "load-checkpoint has restore payload"
    (let ([s (load-checkpoint 'my-checkpoint)])
      (assert-equal (list 'restore 'my-checkpoint) (stage-effect-payload (run-stage s test-ctx "input"))))))

;;; ====
;;; Retry Policy Integration Tests
;;; ====

(test-group "Retry Policy Integration"
  (define-test "with-retry-policy passes success through"
    (let ([s (with-retry-policy retry-exponential (stage-pure 'success))])
      (assert-equal 'success (stage-result-value (run-stage s test-ctx "input")))))

  (define-test "with-retry-policy returns retry result"
    (let* ([s (with-retry-policy retry-exponential (stage-fail 'timeout "timed out"))]
           [result (run-stage s test-ctx "input")])
      (assert-equal 'retry (stage-result-tag result))))

  (define-test "retry has correct delay"
    (let* ([s (with-retry-policy retry-exponential (stage-fail 'timeout "timed out"))]
           [result (run-stage s test-ctx "input")])
      (assert-equal 1000 (stage-retry-delay result))))

  (define-test "non-retryable error is not retried"
    (let* ([s (with-retry-policy retry-exponential (stage-fail 'validation "invalid input"))]
           [result (run-stage s test-ctx "input")])
      (assert-equal 'halt (stage-result-tag result)))))

;;; ====
;;; Run All Tests
;;; ====

(run-all-tests)
