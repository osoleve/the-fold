;;; fabric/stitches/pipeline/tests/test-dsl.ss — Comprehensive Tests for Pipeline DSL
;;;
;;; Tests the user-facing pipeline DSL.

(load "lattice/pipeline/dsl.ss")

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
;;; Test Context
;;; ====

(define test-ctx empty-context)

;;; ====
;;; Pipeline Definition Tests
;;; ====

(run-tests "Pipeline Definition"
           (lambda ()
                   (let ([pipe (define-pipeline* 'my-pipeline
                                 '((model . opus) (fuel . 5000))
                                 (stage-pure 42))])
                        (test-pred "define-pipeline* creates pipeline-def"
                                   pipeline-def?
                                   pipe)
                        
                        (test "pipeline has correct name"
                              'my-pipeline
                              (pipeline-def-name pipe))
                        
                        (test "pipeline has correct config"
                              '((model . opus) (fuel . 5000))
                              (pipeline-def-config pipe))
                        
                        (test-pred "pipeline stage is stage"
                                   stage?
                                   (pipeline-def-stage pipe)))))

;;; ====
;;; Stage Naming Tests
;;; ====

(run-tests "Stage Naming"
           (lambda ()
                   (let ([s (named-stage 'my-named-stage (stage-pure 42))])
                        (test-pred "named-stage creates stage"
                                   stage?
                                   s)
                        
                        (test "named-stage sets name"
                              'my-named-stage
                              (stage-name s))
                        
                        (test "named-stage preserves behavior"
                              42
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test stage* alias
                   (let ([s (stage* 'aliased-stage stage-read)])
                        (test "stage* is alias for named-stage"
                              'aliased-stage
                              (stage-name s)))))

;;; ====
;;; Configuration Helpers Tests
;;; ====

(run-tests "Configuration Helpers"
           (lambda ()
                   ;; Test config function
                   (test "config creates alist"
                         '((a . 1) (b . 2))
                         (config (cons 'a 1) (cons 'b 2)))
                   
                   ;; Test with-model
                   (let ([s (with-model 'opus
                                        (stage-asks (lambda (ctx)
                                                            (ctx-env-ref ctx 'default-model))))])
                        (test "with-model sets default-model in env"
                              'opus
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test with-fuel
                   (let ([s (with-fuel 1000
                                       (stage-asks ctx-fuel))])
                        (test "with-fuel sets fuel"
                              1000
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test with-persona
                   (let* ([persona (make-persona 'test-persona "prompt" '())]
                          [s (with-persona persona
                                           (stage-asks ctx-persona))])
                         (test "with-persona sets persona"
                               persona
                               (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test with-env
                   (let ([s (with-env '((custom-key . custom-value))
                                      (stage-asks (lambda (ctx)
                                                          (ctx-env-ref ctx 'custom-key))))])
                        (test "with-env extends environment"
                              'custom-value
                              (stage-result-value (run-stage s test-ctx "input"))))))

;;; ====
;;; Operator Aliases Tests
;;; ====

(run-tests "Operator Aliases"
           (lambda ()
                   (let ([double (stage-arr (lambda (x) (* x 2)))]
                         [add-one (stage-arr (lambda (x) (+ x 1)))])
                        ;; Test -->
                        (test "--> is alias for stage->>>"
                              11
                              (stage-result-value
                               (run-stage (--> double add-one) test-ctx 5)))
                        
                        ;; Test <--
                        (test "<-- is alias for stage-<<<"
                              11
                              (stage-result-value
                               (run-stage (<-- add-one double) test-ctx 5))))
                   
                   ;; Test pipe-into
                   (test "pipe-into pipes value into stage constructor"
                         42
                         (stage-result-value
                          (run-stage (pipe-into 42 stage-pure) test-ctx "ignored")))))

;;; ====
;;; Common Patterns Tests
;;; ====

(run-tests "Common Patterns"
           (lambda ()
                   ;; Test parse-json (just verifies structure, interpreter does actual parsing)
                   (let ([result (run-stage parse-json test-ctx "{\"key\": \"value\"}")])
                        (test-pred "parse-json produces result"
                                   stage-ok?
                                   result))
                   
                   ;; Test to-json
                   (let ([result (run-stage to-json test-ctx '((key . value)))])
                        (test-pred "to-json produces result"
                                   stage-ok?
                                   result))
                   
                   ;; Test split-lines
                   (let ([result (run-stage split-lines test-ctx "line1\nline2\nline3")])
                        (test-pred "split-lines produces result"
                                   stage-ok?
                                   result))
                   
                   ;; Test join-lines
                   (let ([result (run-stage join-lines test-ctx '("line1" "line2" "line3"))])
                        (test-pred "join-lines produces result"
                                   stage-ok?
                                   result))))

;;; ====
;;; Flow Control Patterns Tests
;;; ====

(run-tests "Flow Control Patterns"
           (lambda ()
                   ;; Test on-success
                   (let ([s (on-success (stage-pure 5)
                                        (stage-arr (lambda (x) (* x 2))))])
                        (test "on-success chains stages"
                              10
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test on-failure
                   (let ([s (on-failure (stage-fail 'err "message")
                                        (stage-pure 'fallback))])
                        (test "on-failure uses fallback"
                              'fallback
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   (let ([s (on-failure (stage-pure 'success)
                                        (stage-pure 'fallback))])
                        (test "on-failure passes through success"
                              'success
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test try-all
                   (let ([s (try-all (list (stage-fail 'e1 "first fails")
                                           (stage-fail 'e2 "second fails")
                                           (stage-pure 'third-wins)))])
                        (test "try-all finds first success"
                              'third-wins
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   (let ([s (try-all (list (stage-fail 'e1 "all fail")
                                           (stage-fail 'e2 "all fail")))])
                        (test "try-all fails when all fail"
                              'all-failed
                              (stage-err-code (run-stage s test-ctx "input"))))
                   
                   ;; Test retry
                   (let ([s (retry 0 (stage-pure 42))])
                        (test "retry 0 runs once"
                              42
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test gate
                   (let ([s (gate positive? (stage-arr (lambda (x) (* x 2))))])
                        (test "gate runs stage when condition met"
                              10
                              (stage-result-value (run-stage s test-ctx 5)))
                        
                        (test "gate skips when condition not met"
                              'skip
                              (stage-result-tag (run-stage s test-ctx -5))))))

;;; ====
;;; Collection Processing Tests
;;; ====

(run-tests "Collection Processing"
           (lambda ()
                   ;; Test map-stage
                   (let ([s (map-stage (stage-arr (lambda (x) (* x 2))))])
                        (test "map-stage applies to each element"
                              '(2 4 6 8 10)
                              (stage-result-value (run-stage s test-ctx '(1 2 3 4 5)))))
                   
                   (let ([s (map-stage (stage-arr (lambda (x) (* x 2))))])
                        (test "map-stage on empty list"
                              '()
                              (stage-result-value (run-stage s test-ctx '()))))
                   
                   ;; Test filter-stage
                   (let ([s (filter-stage positive?)])
                        (test "filter-stage keeps matching elements"
                              '(1 3 5)
                              (stage-result-value (run-stage s test-ctx '(-2 1 -1 3 0 5)))))
                   
                   ;; Test reduce-stage
                   (let ([s (reduce-stage (lambda (acc x) (stage-pure (+ acc x))) 0)])
                        (test "reduce-stage reduces list"
                              15
                              (stage-result-value (run-stage s test-ctx '(1 2 3 4 5)))))
                   
                   ;; Test take-stage
                   (let ([s (take-stage 3)])
                        (test "take-stage takes first n"
                              '(1 2 3)
                              (stage-result-value (run-stage s test-ctx '(1 2 3 4 5)))))
                   
                   (let ([s (take-stage 10)])
                        (test "take-stage handles short list"
                              '(1 2 3)
                              (stage-result-value (run-stage s test-ctx '(1 2 3)))))
                   
                   ;; Test drop-stage
                   (let ([s (drop-stage 2)])
                        (test "drop-stage drops first n"
                              '(3 4 5)
                              (stage-result-value (run-stage s test-ctx '(1 2 3 4 5)))))
                   
                   (let ([s (drop-stage 10)])
                        (test "drop-stage handles short list"
                              '()
                              (stage-result-value (run-stage s test-ctx '(1 2 3)))))))

;;; ====
;;; Parallel Execution Tests
;;; ====

(run-tests "Parallel Execution"
           (lambda ()
                   ;; Test parallel
                   (let ([s (parallel (list (stage-arr (lambda (x) (* x 2)))
                                            (stage-arr (lambda (x) (* x 3)))
                                            (stage-arr (lambda (x) (+ x 1)))))])
                        (test "parallel runs all stages"
                              '(10 15 6)  ;; Result is a proper list
                              (stage-result-value (run-stage s test-ctx 5))))
                   
                   (let ([s (parallel '())])
                        (test "parallel with empty list"
                              '()
                              (stage-result-value (run-stage s test-ctx 5))))
                   
                   ;; Test all-of
                   (let ([s (all-of (list (stage-pure 1) (stage-pure 2) (stage-pure 3)))])
                        (test "all-of collects all results"
                              '(1 2 3)
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test race (produces effect for interpreter)
                   (let ([s (race (list (stage-pure 'first) (stage-pure 'second)))])
                        (test-pred "race produces effect"
                                   stage-effect?
                                   (run-stage s test-ctx "input")))))

;;; ====
;;; FSM Pipeline Tests
;;; ====

(run-tests "FSM Pipeline"
           (lambda ()
                   ;; Simple FSM: start -> processing -> done
                   (let ([fsm (fsm-pipeline
                               (list
                                (list 'start (stage-arr (lambda (x) (+ x 1)))
                                      (cons 'ok 'processing))
                                (list 'processing (stage-arr (lambda (x) (* x 2)))
                                      (cons 'ok 'done)))
                               'start
                               '(done))])
                        (test "fsm-pipeline transitions through states"
                              12  ;; (5 + 1) * 2 = 12
                              (stage-result-value (run-stage fsm test-ctx 5))))
                   
                   ;; FSM with error transition
                   ;; Note: error-state transitions to 'handled which is accepting
                   ;; This ensures the error-state stage actually runs
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
                        
                        (test "fsm-pipeline reaches done on success"
                              5
                              (stage-result-value (run-stage fsm test-ctx 5)))
                        
                        (test "fsm-pipeline transitions on error"
                              'error-handled
                              (stage-result-value (run-stage fsm test-ctx -5))))
                   
                   ;; Test invalid state
                   (let ([fsm (fsm-pipeline '() 'nonexistent '(done))])
                        (test "fsm-pipeline errors on invalid state"
                              'fsm-invalid-state
                              (stage-err-code (run-stage fsm test-ctx 42))))))

;;; ====
;;; Convenience Constructors Tests
;;; ====

(run-tests "Convenience Constructors"
           (lambda ()
                   ;; Test ask-llm is alias for llm
                   (let ([s (ask-llm 'opus "Test prompt")])
                        (test-pred "ask-llm creates stage"
                                   stage?
                                   s)
                        
                        (test "ask-llm produces llm effect"
                              'llm
                              (stage-effect-type (run-stage s test-ctx "input"))))
                   
                   ;; Test run-fold is alias for fold-eval
                   (let ([s (run-fold "(+ 1 2)")])
                        (test "run-fold produces fold effect"
                              'fold
                              (stage-effect-type (run-stage s test-ctx "input"))))
                   
                   ;; Test run-shell is alias for shell
                   (let ([s (run-shell "ls -la")])
                        (test "run-shell produces shell effect"
                              'shell
                              (stage-effect-type (run-stage s test-ctx "input"))))
                   
                   ;; Test fetch-url is alias for http-get
                   (let ([s (fetch-url "https://example.com")])
                        (test "fetch-url produces http effect"
                              'http
                              (stage-effect-type (run-stage s test-ctx "input"))))))

;;; ====
;;; Chain and Branch Tests
;;; ====

(run-tests "Chain and Branch"
           (lambda ()
                   ;; Test chain
                   (let ([s (chain (stage-arr (lambda (x) (* x 2)))
                                   (stage-arr (lambda (x) (+ x 1)))
                                   (stage-arr (lambda (x) (* x 3))))])
                        (test "chain sequences stages"
                              33  ;; ((5 * 2) + 1) * 3 = 33
                              (stage-result-value (run-stage s test-ctx 5))))
                   
                   (let ([s (chain)])
                        (test "empty chain is identity"
                              42
                              (stage-result-value (run-stage s test-ctx 42))))
                   
                   (let ([s (chain (stage-arr (lambda (x) (* x 2))))])
                        (test "single-element chain"
                              10
                              (stage-result-value (run-stage s test-ctx 5))))
                   
                   ;; Test branch (alias for stage-if)
                   (let ([s (branch positive?
                                    (stage-arr (lambda (x) (* x 2)))
                                    (stage-arr (lambda (x) (- x))))])
                        (test "branch takes then on true"
                              10
                              (stage-result-value (run-stage s test-ctx 5)))
                        
                        (test "branch takes else on false"
                              5
                              (stage-result-value (run-stage s test-ctx -5))))
                   
                   ;; Test switch (alias for stage-case)
                   (let ([s (switch (list (cons negative? (stage-pure 'negative))
                                          (cons zero? (stage-pure 'zero)))
                                    (stage-pure 'positive))])
                        (test "switch matches predicates"
                              'negative
                              (stage-result-value (run-stage s test-ctx -5)))
                        
                        (test "switch falls through"
                              'positive
                              (stage-result-value (run-stage s test-ctx 5))))
                   
                   ;; Test tap (alias for stage-tap)
                   (let ([side-effect-value #f])
                        (test "tap passes value through"
                              42
                              (stage-result-value
                               (run-stage (tap (lambda (x) (set! side-effect-value x)))
                                          test-ctx 42)))
                        
                        (test "tap executes side effect"
                              42
                              side-effect-value))))

;;; ====
;;; Logging Helpers Tests
;;; ====

(run-tests "Logging Helpers"
           (lambda ()
                   ;; Test log (alias for log-info)
                   (let ([s (log "Info message")])
                        (test "log creates log effect"
                              'log
                              (stage-effect-type (run-stage s test-ctx "input"))))
                   
                   ;; Test debug (alias for log-debug)
                   (let ([s (debug "Debug message")])
                        (let ([result (run-stage s test-ctx "input")])
                             (test "debug creates debug log"
                                   (list 'debug "Debug message")
                                   (stage-effect-payload result))))
                   
                   ;; Test warn (alias for log-warn)
                   (let ([s (warn "Warning message")])
                        (let ([result (run-stage s test-ctx "input")])
                             (test "warn creates warn log"
                                   (list 'warn "Warning message")
                                   (stage-effect-payload result))))))

;;; ====
;;; Checkpoint Helpers Tests
;;; ====

(run-tests "Checkpoint Helpers"
           (lambda ()
                   ;; Test save (alias for checkpoint)
                   (let ([s (save 'my-checkpoint)])
                        (test "save creates checkpoint effect"
                              'checkpoint
                              (stage-effect-type (run-stage s test-ctx "input")))
                        
                        (test "save has save payload"
                              (list 'save 'my-checkpoint)
                              (stage-effect-payload (run-stage s test-ctx "input"))))
                   
                   ;; Test load-checkpoint (alias for restore)
                   (let ([s (load-checkpoint 'my-checkpoint)])
                        (test "load-checkpoint creates checkpoint effect"
                              'checkpoint
                              (stage-effect-type (run-stage s test-ctx "input")))
                        
                        (test "load-checkpoint has restore payload"
                              (list 'restore 'my-checkpoint)
                              (stage-effect-payload (run-stage s test-ctx "input"))))))

;;; ====
;;; Retry Policy Integration Tests
;;; ====

(run-tests "Retry Policy Integration"
           (lambda ()
                   ;; Test with-retry-policy on success
                   (let ([s (with-retry-policy retry-exponential
                                               (stage-pure 'success))])
                        (test "with-retry-policy passes success through"
                              'success
                              (stage-result-value (run-stage s test-ctx "input"))))
                   
                   ;; Test with-retry-policy on retryable error
                   (let ([s (with-retry-policy retry-exponential
                                               (stage-fail 'timeout "timed out"))])
                        (let ([result (run-stage s test-ctx "input")])
                             (test "with-retry-policy returns retry result"
                                   'retry
                                   (stage-result-tag result))
                             
                             ;; First retry should have 1s delay (1000ms)
                             (test "retry has correct delay"
                                   1000
                                   (stage-retry-delay result))))
                   
                   ;; Test with-retry-policy on non-retryable error
                   (let ([s (with-retry-policy retry-exponential
                                               (stage-fail 'validation "invalid input"))])
                        (let ([result (run-stage s test-ctx "input")])
                             ;; validation is not in the retry-on predicate list
                             (test "non-retryable error is not retried"
                                   'halt
                                   (stage-result-tag result))))))

;;; ====
;;; Run All Tests
;;; ====

(test-summary)
