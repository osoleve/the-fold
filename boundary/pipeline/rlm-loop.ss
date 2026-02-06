;;; boundary/pipeline/rlm-loop.ss — Core RLM Agent Loop
;;;
;;; The main orchestrator: builds prompts, calls LLM, parses responses,
;;; evaluates code in a sandboxed REPL session, records trajectory as
;;; CAS block DAGs, detects loops, and dispatches sub-agent spawns.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "lattice/pipeline/rlm.ss")
(load "boundary/pipeline/rlm-client.ss")
(load "boundary/pipeline/rlm-env.ss")
(load "boundary/pipeline/effects/fold.ss")  ; for fold-ipc-eval, *pipeline-session*
(load "core/blocks/cas.ss")
(load "boundary/storage/cas-persist.ss")

(doc 'module 'rlm-loop)
(doc 'description "Core RLM agent loop: prompt → LLM → parse → eval → record → repeat")
(doc 'layer 'boundary)
(doc 'purity 'impure)
(doc 'dependencies '(lattice/pipeline/rlm.ss boundary/pipeline/rlm-client.ss
                     boundary/pipeline/rlm-env.ss boundary/pipeline/effects/fold.ss
                     core/blocks/cas.ss boundary/storage/cas-persist.ss))

;;; ====
;;; Main Entry Point
;;; ====

;;; rlm-run : RlmConfig -> String -> Any -> RlmRunResult
;;;
;;; Execute an RLM loop.
;;;   config : RLM configuration (provider, system prompt, limits)
;;;   task   : The task description / initial prompt for the model
;;;   input  : Initial input value (stored in env as 'input)
;;;
;;; Returns: (list 'rlm-run-result status output trajectory-hash env)
;;;   status: 'completed | 'exhausted | 'error | 'loop-detected
(define (rlm-run config task input)
  (rlm-run-at-depth config task input 0))

;;; rlm-run-at-depth : RlmConfig -> String -> Any -> Nat -> RlmRunResult
;;; Internal: runs the loop at a given recursion depth.
(define (rlm-run-at-depth config task input depth)
  (let* ([run-id (generate-run-id)]
         [session-id (format "rlm-~a" run-id)]
         [started (current-iso8601)]
         ;; Initialize environment with input
         [env0 (make-rlm-env)]
         [env+input (car (rlm-env-store! env0 'input input 'sexpr))]
         [env+task (car (rlm-env-store! env+input 'task task 'text))])
    ;; Parameterize the pipeline session for Fold IPC
    (parameterize ([*pipeline-session* session-id])
      (let loop ([step-num 0]
                 [env env+task]
                 [fuel-remaining (rlm-config-max-fuel config)]
                 [history '()]       ; list of step hashes (most recent first)
                 [fingerprints '()]  ; for loop detection
                 [prev-step-hash #f] ; CAS hash of previous step block
                 [messages (initial-messages config task)])
        (cond
          ;; Max steps exceeded
          [(>= step-num (rlm-config-max-steps config))
           (finalize-run config env 'exhausted
                         "Max steps reached" started depth
                         prev-step-hash step-num fuel-remaining)]
          ;; Fuel exhausted
          [(<= fuel-remaining 0)
           (finalize-run config env 'exhausted
                         "Fuel exhausted" started depth
                         prev-step-hash step-num 0)]
          [else
           ;; Step 1: Build prompt with env summary
           (let* ([env-summary (rlm-env-summary env)]
                  [tool-docs (rlm-tool-docs depth config)]
                  [user-msg (build-step-prompt env-summary tool-docs step-num)])
             ;; Step 2: Call LLM
             (let ([response (rlm-chat (rlm-config-provider config)
                                       (append messages
                                               (list (make-msg "user" user-msg)))
                                       4096 0.7)])
               (cond
                 ;; LLM call failed
                 [(rlm-chat-err? response)
                  (finalize-run config env 'error
                                (rlm-chat-error-msg response) started depth
                                prev-step-hash step-num fuel-remaining)]
                 [else
                  (let* ([response-text (rlm-chat-text response)]
                         ;; Step 3: Parse response
                         [blocks (rlm-parse-response response-text)]
                         ;; Check for DONE marker
                         [done-block (find-done-block blocks)])
                    (if done-block
                        ;; Model says it's done
                        (let ([final-value (rlm-block-content done-block)])
                          (let ([env* (car (rlm-env-store! env 'output final-value 'result))])
                            (finalize-run config env* 'completed
                                          final-value started depth
                                          prev-step-hash (+ step-num 1) fuel-remaining)))
                        ;; Step 4: Execute code blocks
                        (let-values ([(env* results fuel-used)
                                      (execute-blocks blocks env fuel-remaining
                                                      depth config)])
                          (let* ([fuel-after (- fuel-remaining fuel-used)]
                                 ;; Step 5: Loop detection
                                 [code-text (extract-all-code blocks)]
                                 [fp (rlm-step-fingerprint env-summary code-text)]
                                 [looping? (rlm-loop-detected? fingerprints fp
                                             (rlm-config-loop-window config))])
                            ;; Step 6: Record step as CAS block
                            (let* ([step-hash (record-step! step-num
                                                             (rlm-config-provider config)
                                                             fuel-used
                                                             env env*
                                                             prev-step-hash)]
                                   ;; Build updated message history
                                   [messages* (append messages
                                                (list (make-msg "user" user-msg)
                                                      (make-msg "assistant" response-text)
                                                      (make-msg "user"
                                                        (format-results results looping?))))])
                              (if looping?
                                  ;; Inject loop-break message but continue
                                  (loop (+ step-num 1)
                                        env*
                                        fuel-after
                                        (cons step-hash history)
                                        '()  ; reset fingerprints after break
                                        step-hash
                                        messages*)
                                  (loop (+ step-num 1)
                                        env*
                                        fuel-after
                                        (cons step-hash history)
                                        (cons fp fingerprints)
                                        step-hash
                                        messages*)))))))])))])))))

;;; ====
;;; Message Construction
;;; ====

(define (make-msg role content)
  `((role . ,role) (content . ,content)))

(define (initial-messages config task)
  (list (make-msg "system" (rlm-config-system-prompt config))
        (make-msg "user" task)))

(define (build-step-prompt env-summary tool-docs step-num)
  (string-append
    "\n--- Step " (number->string step-num) " ---\n\n"
    env-summary "\n\n"
    tool-docs "\n\n"
    "Emit Fold/Scheme code in ```scheme blocks to make progress on the task.\n"
    "Use (rlm-env-put! 'key value) to store results in context.\n"
    "Use (rlm-env-get 'key) to retrieve context values.\n"
    "Use (rlm-env-peek 'key n) to preview the first n characters.\n"
    "When done, write DONE(your-final-answer) on its own line.\n"))

(define (rlm-tool-docs depth config)
  (let ([base-docs
          (string-append
            "Available tools:\n"
            "  (rlm-env-put! 'key value)  — Store a value in context\n"
            "  (rlm-env-get 'key)          — Retrieve a value from context\n"
            "  (rlm-env-peek 'key n)       — Preview first n chars of a value\n"
            "  (rlm-env-keys)              — List all context keys\n"
            "  Any valid Fold/Scheme expression — evaluated in sandbox\n")])
    (if (< depth (rlm-config-max-depth config))
        (string-append base-docs
          "  (rlm-spawn \"sub-task\" '(key1 key2))  — Spawn sub-agent with sliced context\n")
        base-docs)))

;;; ====
;;; Code Execution
;;; ====

;;; execute-blocks : (List RlmBlock) -> RlmEnv -> Nat -> Nat -> RlmConfig
;;;                  -> (Values RlmEnv (List Result) Nat)
;;; Execute all code/template blocks. Returns updated env, results, fuel used.
(define (execute-blocks blocks env fuel-remaining depth config)
  (let loop ([bs blocks]
             [env env]
             [results '()]
             [fuel-used 0])
    (if (null? bs)
        (values env (reverse results) fuel-used)
        (let ([block (car bs)])
          (cond
            ;; Code block — evaluate in sandbox
            [(rlm-block-code? block)
             (let-values ([(env* result fu)
                           (execute-code (rlm-block-content block)
                                         env fuel-remaining depth config)])
               (loop (cdr bs) env*
                     (cons result results)
                     (+ fuel-used fu)))]
            ;; Template block — parse via tp-batch then evaluate
            [(rlm-block-template? block)
             (let-values ([(env* result fu)
                           (execute-template (rlm-block-content block)
                                             env fuel-remaining depth config)])
               (loop (cdr bs) env*
                     (cons result results)
                     (+ fuel-used fu)))]
            ;; Thought or done — skip
            [else
             (loop (cdr bs) env results fuel-used)])))))

;;; execute-code : String -> RlmEnv -> Nat -> Nat -> RlmConfig
;;;               -> (Values RlmEnv Result Nat)
(define (execute-code code-text env fuel-remaining depth config)
  (guard (ex [else
              (values env
                      (list 'error (format "Code execution error: ~a"
                                           (if (message-condition? ex)
                                               (condition-message ex)
                                               "unknown")))
                      1)])
    ;; Check for special RLM commands
    (cond
      ;; rlm-spawn: sub-agent delegation
      [(rlm-spawn-call? code-text)
       (let-values ([(sub-prompt context-keys) (parse-spawn-call code-text)])
         (if (>= depth (rlm-config-max-depth config))
             (values env
                     (list 'error "Max recursion depth reached, cannot spawn sub-agent")
                     1)
             (let* ([child-fuel (min (quotient fuel-remaining 2)
                                     (rlm-config-max-fuel config))]
                    [child-env (slice-env env context-keys)]
                    [child-config (make-rlm-config
                                    (rlm-config-provider config)
                                    (rlm-config-system-prompt config)
                                    (rlm-config-max-steps config)
                                    child-fuel
                                    (rlm-config-chunk-size config)
                                    (rlm-config-max-depth config)
                                    (rlm-config-loop-window config))]
                    [child-result (rlm-run-at-depth child-config sub-prompt "" (+ depth 1))]
                    [child-output (rlm-run-result-output child-result)]
                    [child-traj-hash (rlm-run-result-trajectory-hash child-result)]
                    ;; Store child result in parent env
                    [result-key (string->symbol
                                  (format "sub-result-~a" (length (rlm-env-keys env))))]
                    [env* (car (rlm-env-store! env result-key child-output 'sub-result))])
               (values env*
                       (list 'ok (format "Sub-agent completed. Result stored as '~a'" result-key))
                       child-fuel))))]
      ;; rlm-env-put!: store in environment
      [(rlm-env-put-call? code-text)
       (let-values ([(key value) (parse-env-put-call code-text env)])
         (if key
             (let ([env* (car (rlm-env-store! env key value 'sexpr))])
               (values env* (list 'ok (format "Stored ~a in context" key)) 1))
             (values env (list 'error "Failed to parse rlm-env-put! call") 1)))]
      ;; Regular code — eval via Fold IPC
      [else
       (let ([result (fold-ipc-eval code-text)])
         (if (fold-result-ok? result)
             (values env (list 'ok (fold-result-value result)) 10)
             (values env (list 'error (fold-result-error result)) 5)))])))

;;; execute-template : String -> RlmEnv -> Nat -> Nat -> RlmConfig
;;;                   -> (Values RlmEnv Result Nat)
(define (execute-template template-text env fuel-remaining depth config)
  ;; Parse template via tp-batch, then evaluate the result
  (guard (ex [else
              (values env
                      (list 'error (format "Template parse error: ~a"
                                           (if (message-condition? ex)
                                               (condition-message ex)
                                               "unknown")))
                      1)])
    ;; tp-batch returns an s-expression
    (let* ([expr (tp-batch template-text)]
           [code-text (format "~s" expr)])
      (execute-code code-text env fuel-remaining depth config))))

;;; ====
;;; Special Call Detection
;;; ====

(define (rlm-spawn-call? code)
  (and (>= (string-length code) 11)
       (let ([trimmed (string-trim-left code)])
         (and (>= (string-length trimmed) 11)
              (string=? (substring trimmed 0 10) "(rlm-spawn")))))

(define (rlm-env-put-call? code)
  (let ([trimmed (string-trim-left code)])
    (and (>= (string-length trimmed) 14)
         ;; Match "(rlm-env-put!" — 13 chars including the !
         (string=? (substring trimmed 0 13) "(rlm-env-put!")
         )))

(define (parse-spawn-call code)
  ;; Parse (rlm-spawn "sub-prompt" '(key1 key2))
  ;; Returns (values sub-prompt context-keys)
  (guard (ex [else (values "analyze" '())])
    (let* ([expr (read (open-input-string code))]
           [sub-prompt (cadr expr)]
           [context-keys (if (> (length expr) 2)
                             (let ([keys-expr (caddr expr)])
                               (if (and (pair? keys-expr) (eq? (car keys-expr) 'quote))
                                   (cadr keys-expr)
                                   (list keys-expr)))
                             '())])
      (values sub-prompt context-keys))))

(define (parse-env-put-call code env)
  ;; Parse (rlm-env-put! 'key value) — evaluate value via IPC
  (guard (ex [else (values #f #f)])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)]
           ;; Evaluate the value expression
           [value-code (format "~s" (caddr expr))]
           [result (fold-ipc-eval value-code)])
      (if (fold-result-ok? result)
          (values key (fold-result-value result))
          (values key (caddr expr))))))  ; store unevaluated if eval fails

;;; Slice environment: extract only named keys
(define (slice-env env keys)
  (if (null? keys)
      env  ; empty key list = pass everything
      (let loop ([ks keys] [sliced (make-rlm-env)])
        (if (null? ks)
            sliced
            (let ([entry (rlm-env-get env (car ks))])
              (if entry
                  (loop (cdr ks) (cons entry sliced))
                  (loop (cdr ks) sliced)))))))

;;; ====
;;; Trajectory Recording
;;; ====

(define (record-step! step-num provider fuel-used env-before env-after prev-step-hash)
  (let* ([env-snap-hash (rlm-env-snapshot! env-after)]
         [payload `((step . ,step-num)
                    (model . ,(rlm-provider-model-id provider))
                    (fuel-used . ,fuel-used)
                    (timestamp . ,(current-iso8601))
                    (env-keys-before . ,(map car env-before))
                    (env-keys-after . ,(map car env-after)))]
         [refs (if prev-step-hash
                   (vector (hex->hash prev-step-hash)
                           (hex->hash env-snap-hash))
                   (vector (hex->hash env-snap-hash)))]
         [blk (make-block 'rlm/step
                           (string->utf8 (format "~s" payload))
                           refs)]
         [hash (store-persistent! blk)])
    (hash->hex hash)))

(define (finalize-run config env status output started depth
                      last-step-hash total-steps fuel-remaining)
  (let* ([finished (current-iso8601)]
         ;; Store output
         [output-pair (rlm-env-store! env 'output output 'result)]
         [env* (car output-pair)]
         [output-hex (cdr output-pair)]
         ;; Build trajectory record
         [config-hex (store-config! config)]
         [traj (make-rlm-trajectory config-hex "" output-hex
                                     total-steps
                                     (- (rlm-config-max-fuel config) fuel-remaining)
                                     status started finished depth)]
         [traj-blk (make-block 'rlm/trajectory
                                (string->utf8 (format "~s" traj))
                                (if last-step-hash
                                    (vector (hex->hash last-step-hash)
                                            (hex->hash output-hex))
                                    (vector (hex->hash output-hex))))]
         [traj-hash (store-persistent! traj-blk)]
         [traj-hex (hash->hex traj-hash)])
    (list 'rlm-run-result status output traj-hex env*)))

(define (store-config! config)
  (let* ([blk (make-block 'rlm/config
                           (string->utf8 (format "~s" config))
                           (make-vector 0))]
         [hash (store-persistent! blk)])
    (hash->hex hash)))

;;; ====
;;; Result Formatting
;;; ====

(define (format-results results looping?)
  (let ([result-text
          (apply string-append
                 (map (lambda (r)
                        (cond
                          [(and (pair? r) (eq? (car r) 'ok))
                           (format "Result: ~a\n" (cadr r))]
                          [(and (pair? r) (eq? (car r) 'error))
                           (format "Error: ~a\n" (cadr r))]
                          [else (format "~a\n" r)]))
                      results))])
    (if looping?
        (string-append result-text
          "\n[LOOP DETECTED] You are repeating yourself. "
          "Try a different approach or use different tools.\n")
        result-text)))

(define (extract-all-code blocks)
  (apply string-append
         (map (lambda (b)
                (if (or (rlm-block-code? b) (rlm-block-template? b))
                    (rlm-block-content b)
                    ""))
              blocks)))

(define (find-done-block blocks)
  (let loop ([bs blocks])
    (cond
      [(null? bs) #f]
      [(rlm-block-done? (car bs)) (car bs)]
      [else (loop (cdr bs))])))

;;; ====
;;; Run Result Accessors
;;; ====

(define (rlm-run-result? x)
  (and (pair? x) (eq? (car x) 'rlm-run-result)))

(define (rlm-run-result-status r)          (list-ref r 1))
(define (rlm-run-result-output r)          (list-ref r 2))
(define (rlm-run-result-trajectory-hash r) (list-ref r 3))
(define (rlm-run-result-env r)             (list-ref r 4))

;;; ====
;;; Utilities
;;; ====

(define (generate-run-id)
  (format "~a-~a"
          (time-second (current-time))
          (random 100000)))

(define (current-iso8601)
  (let* ([t (current-time)]
         [s (time-second t)])
    ;; Simple ISO8601-ish timestamp
    (format "~a" s)))
