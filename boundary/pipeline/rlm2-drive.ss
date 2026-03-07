;;; boundary/pipeline/rlm2-drive.ss — RLM v2 Driver
;;;
;;; The impure driver loop: act → execute → reflect → update → record.
;;; The agent is a pure function State → Action. The driver handles IO.

(load "lattice/pipeline/rlm2.ss")
(load "lattice/pipeline/rlm2-parse.ss")
(load "lattice/pipeline/rlm2-hud.ss")
(load "boundary/pipeline/rlm-client.ss")
(load "boundary/pipeline/rlm-env.ss")
(load "boundary/pipeline/effects/fold.ss")
(load "core/blocks/cas.ss")
(load "boundary/storage/cas-persist.ss")
(load "boundary/io/atomic.ss")
(load "boundary/io/file-lock.ss")

(doc 'module 'rlm2-drive)
(doc 'description "RLM v2 driver: HUD-based state machine with act/reflect loop, action dispatch, CAS trajectory recording, and telemetry.")
(doc 'layer 'boundary)
(doc 'purity 'impure)

;;; ====
;;; Progress File (sideband for external consumers)
;;; ====
;;; When *rlm2-progress-file* is set, each completed step appends a
;;; tab-delimited line: step\taction-type\tok?\tnote-preview
;;; Consumers (e.g. Python brain) poll this file for live trajectory.

(define *rlm2-progress-file* #f)

(define (rlm2-emit-progress! step-num action-type ok? note)
  (when *rlm2-progress-file*
    (guard (ex [else (void)])  ; never let progress I/O break the run
      (let ([p (open-file-output-port *rlm2-progress-file*
                 (file-options no-fail no-truncate)
                 (buffer-mode line)
                 (native-transcoder))])
        (set-port-position! p (port-length p))
        (put-string p (format "~a\t~a\t~a\t~a\n"
                        step-num action-type
                        (if ok? "ok" "err")
                        (let ([s (if (string? note) note (format "~a" note))])
                          (if (> (string-length s) 200)
                              (substring s 0 200)
                              s))))
        (close-port p)))))

;;; ====
;;; Step Timing (gated by RLM_TIMING env var)
;;; ====

(define (rlm2-time-ms)
  (let ([t (current-time)])
    (+ (* (time-second t) 1000)
       (quotient (time-nanosecond t) 1000000))))

(define *rlm2-timing-enabled?*
  (and (getenv "RLM_TIMING") #t))

;;; Step viewer — RLM_STEP_VIEW=1 dumps full context at each step
(define *rlm2-step-view?*
  (and (getenv "RLM_STEP_VIEW") #t))

(define (rlm2-step-view-header step-num action-type fuel max-fuel)
  (let ([bar (make-string 47 #\x2550)])  ; ═
    (format (current-output-port)
      "\n~a\n STEP ~a  |  ~a  |  fuel: ~a/~a\n~a\n"
      bar step-num action-type fuel max-fuel bar)
    (flush-output-port)))

(define (rlm2-step-view-section header text)
  (let ([pad (make-string (max 0 (- 40 (string-length header))) #\x2500)])  ; ─
    (format (current-output-port) "\n-- ~a ~a\n~a\n" header pad text)
    (flush-output-port)))

;;; Act temperature — overridable via RLM_TEMPERATURE env var (default 0.7)
(define *rlm2-act-temperature*
  (let ([env (getenv "RLM_TEMPERATURE")])
    (if (and env (string->number env))
        (string->number env)
        0.7)))

;;; ====
;;; Mechanical Actions (skip LLM reflection, use template note)
;;; ====
;;; These actions are deterministic enough that an LLM reflection call
;;; adds latency without insight. Think and begin are NOT here —
;;; think is designed to feed reflection, begin hides intermediate obs.

(define *rlm2-mechanical-actions*
  '(submit store load plan! journal memorize remember recall
    lookup definition symbols outline delegate idle reframe))

;;; ====
;;; Setup Replay (pre-load modules and bind variables before the agent starts)
;;; ====
;;;
;;; Setup actions are executed silently before the main loop. They let
;;; the bench harness pre-load modules and bind variables so the agent
;;; starts with a prepared environment. No fuel cost — the model's
;;; budget starts fresh after setup.

(doc 'type '(-> Rlm2State (List Action) Rlm2Config Rlm2State))
(doc 'description "Replay setup actions (load/store/eval) silently, threading state. Returns updated state.")
(define (rlm2-replay-setup! state setup config)
  (fold-left
    (lambda (st action)
      (let ([type (car action)])
        (case type
          [(load)    (let ([result (rlm2-exec-load st action)])
                      (cadr result))]
          [(store)   (let ([result (rlm2-exec-store st action config)])
                      (cadr result))]
          [(eval)    (let ([result (rlm2-exec-eval st action config)])
                      (cadr result))]
          [(plan!)   (let ([result (rlm2-exec-plan! st action)])
                      (cadr result))]
          [(journal) (let ([result (rlm2-exec-journal st action)])
                      (cadr result))]
          [else st])))
    state
    setup))

;;; ====
;;; Main Entry Point
;;; ====

(doc 'type '(-> String Any Rlm2Config (Maybe Proc) Rlm2RunResult))
(doc 'description "Run an RLM v2 loop. Returns (rlm2-run-result status output trajectory-hash env).")
(define (rlm2-run config task input)
  (rlm2-run-at-depth config task input 0))

(define (rlm2-run-at-depth config task input depth)
  (let* ([run-id (rlm2-generate-run-id)]
         [session-id (format "rlm2-~a" run-id)]
         [started (rlm2-current-iso8601)]
         ;; Initialize environment
         [env0 (make-rlm-env)]
         ;; Auto-chunk large string inputs
         [large-input? (and (string? input)
                            (> (string-length input)
                               (rlm2-config-chunk-size config)))]
         [env+input (if large-input?
                        (rlm-env-ingest-text! env0 'input input
                                              (rlm2-config-chunk-size config))
                        (car (rlm-env-store! env0 'input input 'sexpr)))]
         [input-hex (or (rlm-env-hash env+input 'input) "")]
         [env+task (car (rlm-env-store! env+input 'task task 'text))]
         ;; Build initial state
         [initial-state (make-rlm2-state
                         task '() env+task '() '() '() '()
                         #f  ; no last-result yet
                         (rlm2-config-max-fuel config) 0)]
         ;; Build system prompt
         [sys-prompt (rlm2-build-system-prompt
                      (rlm2-config-system-prompt config))])
    ;; Parameterize pipeline session for IPC
    (parameterize ([*pipeline-session* session-id])
      ;; Load worker prelude into IPC session
      (rlm2-init-worker-prelude!)
      ;; Drive the loop (replay setup actions first if configured)
      (let loop ([state (let ([setup (rlm2-config-setup config)])
                          (if (null? setup) initial-state
                              (rlm2-replay-setup! initial-state setup config)))]
                 [fingerprints '()]
                 [prev-step-hash #f]
                 [consecutive-thinks 0]
                 [history '()])
        (cond
          ;; Success: state carries a result
          [(rlm2-state-complete? state)
           (rlm2-finalize config state 'completed
                          (format "~a" (rlm2-state-result state))
                          started depth prev-step-hash input-hex)]
          ;; Exhaustion: fuel or steps
          [(rlm2-state-exhausted? state config)
           (rlm2-finalize config state 'exhausted
                          "Resources exhausted" started depth
                          prev-step-hash input-hex)]
          [else
           ;; === ACT PHASE (with optional timing) ===
           (let* ([step-t0 (rlm2-time-ms)]  ; always capture for telemetry
                  [t0 (and *rlm2-timing-enabled?* step-t0)]
                  [hud (rlm2-render-state state
                         (rlm2-config-context-budget config))]
                  [t1 (and *rlm2-timing-enabled?* (rlm2-time-ms))]
                  ;; Call LLM with system prompt + few-shot + sliding window + HUD
                  [few-shot (rlm2-config-few-shot config)]
                  [history-msgs (rlm2-history->messages history)]
                  [messages (append
                              (list (rlm2-make-msg "system" sys-prompt))
                              few-shot
                              history-msgs
                              (list (rlm2-make-msg "user" hud)))]
                  ;; === STEP VIEW: input context ===
                  [_sv1 (when *rlm2-step-view?*
                          (rlm2-step-view-header
                            (rlm2-state-step state) "..."
                            (rlm2-state-fuel state)
                            (rlm2-config-max-fuel config))
                          (rlm2-step-view-section "HUD (user message to model)" hud)
                          (when (pair? history-msgs)
                            (rlm2-step-view-section "HISTORY"
                              (apply string-append
                                (map (lambda (m)
                                       (format "[~a]: ~a\n"
                                         (cdr (assq 'role m))
                                         (let ([c (cdr (assq 'content m))])
                                           (if (> (string-length c) 500)
                                               (string-append (substring c 0 497) "...")
                                               c))))
                                     history-msgs)))))]
                  [act-response (rlm-chat (rlm2-config-provider config)
                                          messages (rlm2-config-max-tokens config) *rlm2-act-temperature*)]
                  [t2 (and *rlm2-timing-enabled?* (rlm2-time-ms))])
             (cond
               ;; LLM call failed
               [(rlm-chat-err? act-response)
                (rlm2-finalize config state 'error
                               (rlm-chat-error-msg act-response)
                               started depth prev-step-hash input-hex)]
               [else
                (let* ([raw-text (rlm-chat-text act-response)]
                       [reasoning (rlm-chat-reasoning act-response)]
                       [_trace-reasoning
                        (when (and reasoning (getenv "RLM_TRACE"))
                          (format (current-error-port)
                            "[step ~a] reasoning=~a~%"
                            (rlm2-state-step state)
                            (if (> (string-length reasoning) 800)
                                (substring reasoning 0 800)
                                reasoning)))]
                       ;; Parse action from model output
                       [parse-result (rlm2-parse-response raw-text)]
                       [action (rlm2-parse-result-action parse-result)]
                       [raw-thought (rlm2-parse-result-thought parse-result)]
                       ;; One-shot retry: if parser fell back to (think ...)
                       ;; because a non-think action was truncated. Skip retry
                       ;; when the model intended to write a think — detected
                       ;; by raw text starting with "(think" even if truncated.
                       [intended-think? (let ([t (rlm2-parse-trim raw-text)])
                                          (and (> (string-length t) 6)
                                               (string=? (substring t 0 6) "(think")))]
                       [truncated? (and (rlm2-think? action)
                                        (not intended-think?)
                                        (rlm2-looks-truncated? raw-text))]
                       ;; Retry with higher budget if truncated
                       [retry-result (and truncated?
                                         (rlm-chat (rlm2-config-provider config)
                                                   messages (min (* 4 (rlm2-config-max-tokens config)) 8192) *rlm2-act-temperature*))]
                       ;; Use retry if it succeeded and parsed to non-think
                       [effective-text
                        (if (and retry-result (rlm-chat-ok? retry-result))
                            (let ([rt (rlm-chat-text retry-result)])
                              (let ([rp (rlm2-parse-response rt)])
                                (if (rlm2-think? (rlm2-parse-result-action rp))
                                    raw-text  ; retry also fell back, use original
                                    rt)))
                            raw-text)]
                       ;; Re-parse if we used retry text
                       [final-parse (if (eq? effective-text raw-text)
                                        parse-result
                                        (rlm2-parse-response effective-text))]
                       [action (rlm2-parse-result-action final-parse)]
                       [raw-thought (rlm2-parse-result-thought final-parse)]
                       ;; === STEP VIEW: model response ===
                       [_sv2 (when *rlm2-step-view?*
                               (rlm2-step-view-section "MODEL RESPONSE"
                                 (if (eq? effective-text raw-text)
                                     effective-text
                                     (string-append "[RETRIED] " effective-text)))
                               (rlm2-step-view-section
                                 (format "PARSED -> ~a" (rlm2-action-type action))
                                 (format "~s" action)))]
                       ;; === EXECUTE PHASE ===
                       [_trace (when (getenv "RLM_TRACE")
                                 (format (current-error-port)
                                   "[step ~a] action=~a raw=~a~%"
                                   (rlm2-state-step state)
                                   (rlm2-action-type action)
                                   (if (> (string-length effective-text) 500)
                                       (substring effective-text 0 500)
                                       effective-text)))]
                       [exec-result (rlm2-execute-action state action
                                      config depth)]
                       [observation (car exec-result)]
                       [state-after-exec (cadr exec-result)]
                       [fuel-used (caddr exec-result)]
                       [_trace2 (when (getenv "RLM_TRACE")
                                  (let ([obs-text (rlm2-observation-value observation)]
                                        [obs-ok (rlm2-observation-ok? observation)])
                                    (format (current-error-port)
                                      "  => ok=~a obs=~a~%"
                                      obs-ok
                                      (if (and (string? obs-text)
                                               (> (string-length obs-text) 300))
                                          (substring obs-text 0 300)
                                          obs-text))))]
                       ;; === STEP VIEW: execution result ===
                       [_sv3 (when *rlm2-step-view?*
                               (let ([obs-text (rlm2-observation-value observation)]
                                     [obs-ok (rlm2-observation-ok? observation)])
                                 (rlm2-step-view-section "RESULT"
                                   (format "ok: ~a\n~a" obs-ok
                                     (if (string? obs-text) obs-text
                                         (format "~a" obs-text))))))]
                       [t3 (and *rlm2-timing-enabled?* (rlm2-time-ms))]
                       ;; === REFLECT PHASE ===
                       ;; Skip LLM reflection for mechanical actions
                       [action-type (rlm2-action-type action)]
                       [mechanical? (memq action-type
                                         *rlm2-mechanical-actions*)]
                       [note (if mechanical?
                                 (rlm2-mechanical-note action observation
                                   (rlm2-state-step state))
                                 (rlm2-reflect config raw-thought action
                                   observation (rlm2-state-step state)))]
                       ;; Prepend alias correction if an alias was resolved
                       [alias-info (rlm2-parse-result-alias-info final-parse)]
                       [note (if alias-info
                                 (string-append
                                   (rlm2-alias-correction-note alias-info)
                                   " " note)
                                 note)]
                       ;; === STEP VIEW: reflect note ===
                       [_sv4 (when *rlm2-step-view?*
                               (rlm2-step-view-section
                                 (if mechanical? "REFLECT [mechanical]" "REFLECT")
                                 note))]
                       [t4 (and *rlm2-timing-enabled?* (rlm2-time-ms))]
                       ;; Emit timing to stderr if enabled
                       [_timing (when *rlm2-timing-enabled?*
                                  (format (current-error-port)
                                    "  step ~a: hud=~ams act=~ams exec=~ams reflect=~ams~a~%"
                                    (rlm2-state-step state)
                                    (- t1 t0) (- t2 t1) (- t3 t2)
                                    (- t4 t3)
                                    (if mechanical? " [mechanical]" "")))]
                       ;; === UPDATE STATE ===
                       [state* (rlm2-update-state state-after-exec
                                 action observation note fuel-used)]
                       ;; === LOOP DETECTION ===
                       [fp (rlm2-semantic-fingerprint state*
                             action-type)]
                       [looping? (rlm2-loop-detected? fingerprints fp
                                   (rlm2-config-loop-window config))]
                       ;; Inject loop/think warnings into last-result (transient, not notes)
                       [state** (if looping?
                                    (rlm2-state-with-last-result state*
                                      (rlm2-format-diagnostic (rlm2-warning-loop)))
                                    state*)]
                       ;; === THINK-SPAM DETECTION ===
                       [thinks (if (eq? action-type 'think)
                                   (+ consecutive-thinks 1)
                                   0)]
                       [state** (cond
                                  [(>= thinks 3)
                                   (rlm2-state-with-last-result state**
                                     (rlm2-format-diagnostic
                                       (rlm2-nudge-think-streak thinks)))]
                                  [else state**])]
                       ;; === RECORD STEP ===
                       [step-hash (rlm2-record-step!
                                   (rlm2-state-step state)
                                   action observation note
                                   (rlm2-config-provider config)
                                   fuel-used prev-step-hash)]
                       ;; Add episodic entry
                       [state*** (rlm2-state-add-episodic state**
                                   (rlm2-state-step state)
                                   step-hash action-type
                                   (rlm2-observation-ok? observation))]
                       ;; === PROGRESS SIDEBAND ===
                       [_progress (rlm2-emit-progress!
                                   (rlm2-state-step state) action-type
                                   (rlm2-observation-ok? observation) note)]
                       ;; === TELEMETRY ===
                       [step-duration-ms (- (rlm2-time-ms) step-t0)]
                       [_telem (rlm2-record-telemetry!
                                (rlm-provider-model-id
                                 (rlm2-config-provider config))
                                action-type
                                (rlm2-state-step state)
                                step-duration-ms
                                (rlm2-observation-ok? observation)
                                effective-text final-parse)]
                       ;; Update fingerprints
                       [fps* (if looping?
                                 '()  ; reset after loop break
                                 (cons fp fingerprints))]
                       ;; === SLIDING WINDOW ===
                       ;; Thinks go to notes via reflect — don't echo full text
                       ;; in history (causes recursive escape nesting)
                       [action-text (if (eq? action-type 'think)
                                        "(think)"
                                        (format "~s" action))]
                       [obs-summary (rlm2-observation-summary observation)]
                       [history* (rlm2-history-push history
                                   action-text obs-summary 3)])
                  (loop state*** fps* step-hash thinks history*))]))])))))

;;; ====
;;; Action Execution
;;; ====
;;;
;;; execute-action : State -> Action -> Config -> Nat -> (List Observation State FuelUsed)
;;; Dispatches on action type. Returns (list observation updated-state fuel-used).

(define (rlm2-execute-action state action config depth)
  (guard (ex [else
              (let ([error-msg (if (message-condition? ex)
                                   (condition-message ex)
                                   "unknown")])
                (list (make-rlm2-observation
                       (if (pair? action) (car action) 'unknown)
                       #f
                       (rlm2-format-diagnostic
                         (rlm2-error 'exception
                           (list 'detail (format "~a" error-msg))
                           (list 'expr (let ([s (format "~s" action)])
                                       (if (> (string-length s) 200)
                                           (string-append (substring s 0 197) "...")
                                           s)))
                           (list 'suggestion "Check your expression for syntax errors and retry.")))
                       #f)
                      state 1))])
    (let ([type (rlm2-action-type action)])
      (case type
        [(search)   (rlm2-exec-search state action)]
        [(inspect)  (rlm2-exec-inspect state action)]
        [(exports)  (rlm2-exec-exports state action)]
        [(load)     (rlm2-exec-load state action)]
        [(eval)     (rlm2-exec-eval state action config)]
        [(store)    (rlm2-exec-store state action config)]
        [(peek)     (rlm2-exec-peek state action)]
        [(grep)     (rlm2-exec-grep state action)]
        [(slice)    (rlm2-exec-slice state action)]
        [(recall-step) (rlm2-exec-recall-step state action)]
        [(submit)   (rlm2-exec-submit state action config)]
        [(think)    (rlm2-exec-think state action)]
        [(plan!)    (rlm2-exec-plan! state action)]
        [(map-chunks) (rlm2-exec-map-chunks state action config)]
        [(journal)  (rlm2-exec-journal state action)]
        [(recall)   (rlm2-exec-recall state action)]
        [(memorize) (rlm2-exec-memorize state action)]
        [(remember) (rlm2-exec-remember state action)]
        [(begin)    (rlm2-exec-begin state action config depth)]
        [(lookup)     (rlm2-exec-lookup state action)]
        [(definition) (rlm2-exec-definition state action)]
        [(symbols)    (rlm2-exec-symbols state action)]
        [(outline)    (rlm2-exec-outline state action)]
        [(delegate)   (rlm2-exec-delegate state action config depth)]
        [(idle)       (rlm2-exec-idle state action)]
        [(reframe)    (rlm2-exec-reframe state action)]
        [else
         (list (make-rlm2-observation type #f
                 (rlm2-format-diagnostic
                   (rlm2-error-unknown-action
                     (format "~a" type)
                     (map symbol->string *rlm2-action-types*)))
                 #f)
               state 1)]))))

;;; --- IPC Value Parsing ---
;;;
;;; IPC returns all values as strings (the wire protocol is text-based).
;;; Before storing in the CAS-backed env, parse them back to structured data
;;; so that rlm-env-store!/fetch round-trips correctly.
;;; Without this, (format "~s" string) double-quotes the value.

(define (rlm2-parse-ipc-value str)
  (if (not (string? str))
      str  ; already structured, pass through
      (guard (ex [else str])  ; if read fails, keep as string
        (let ([val (read (open-input-string str))])
          (if (eof-object? val) str val)))))

;;; --- Individual Action Handlers ---

(define (rlm2-exec-search state action)
  (let ([query (rlm2-search-query action)])
    ;; Validate: query must be a string to prevent code injection
    (if (not (string? query))
        (list (make-rlm2-observation 'search query
                (rlm2-format-diagnostic
                  (rlm2-error-type-mismatch "string" query "search query"))
                #f)
              state 1)
        ;; Workers have lattice meta via repl.ss → lattice-init-quiet!
        (let* ([escaped (rlm2-escape-string query)]
               [result (fold-ipc-eval (format "(lf ~a)" escaped))])
          (list (make-rlm2-observation 'search query
                  (if (fold-result-ok? result)
                      (let ([v (fold-result-value result)])
                        (if (or (not v) (string=? v ""))
                            "No matches found."
                            v))
                      (rlm2-format-diagnostic
                        (rlm2-error 'search-error
                          (list 'given query)
                          (list 'detail (fold-result-error result))
                          (list 'suggestion "Try different search terms or check spelling."))))
                  (fold-result-ok? result))
                state 1)))))

(define (rlm2-exec-inspect state action)
  (let* ([skill (rlm2-inspect-skill action)]
         [result (fold-ipc-eval (format "(li '~a)" skill))])
    (list (make-rlm2-observation 'inspect skill
            (if (fold-result-ok? result)
                (let ([v (fold-result-value result)])
                  (if (or (not v) (string=? v ""))
                      (rlm2-format-diagnostic
                        (rlm2-error 'skill-not-found
                          (list 'given (format "~a" skill))
                          (list 'suggestion (format "Use (search \"~a\") to find available skills." skill))))
                      v))
                (rlm2-format-diagnostic
                  (rlm2-error 'inspect-error
                    (list 'given (format "~a" skill))
                    (list 'detail (fold-result-error result))
                    (list 'suggestion "Check the skill name and try (search ...) to discover skills."))))
            (fold-result-ok? result))
          state 1)))

(define (rlm2-exec-exports state action)
  (let* ([skill (rlm2-exports-skill action)]
         [result (fold-ipc-eval (format "(le '~a)" skill))])
    (list (make-rlm2-observation 'exports skill
            (if (fold-result-ok? result)
                (let ([v (fold-result-value result)])
                  (if (or (not v) (string=? v ""))
                      (rlm2-format-diagnostic
                        (rlm2-error 'skill-not-found
                          (list 'given (format "~a" skill))
                          (list 'suggestion (format "Use (search \"~a\") to find available skills." skill))))
                      v))
                (rlm2-format-diagnostic
                  (rlm2-error 'exports-error
                    (list 'given (format "~a" skill))
                    (list 'detail (fold-result-error result))
                    (list 'suggestion "Check the skill name and try (search ...) to discover skills."))))
            (fold-result-ok? result))
          state 1)))

;;; Escape a string for safe embedding in Scheme expressions
(define (rlm2-escape-string s)
  (let ([out (open-output-string)])
    (write s out)
    (get-output-string out)))

(define (rlm2-exec-load state action)
  (let* ([mod (rlm2-load-module action)]
         [code (format "(require '~a)" mod)]
         [result (fold-ipc-eval code)])
    (if (fold-result-ok? result)
        (let ([loaded* (if (memq mod (rlm2-state-loaded state))
                           (rlm2-state-loaded state)
                           (cons mod (rlm2-state-loaded state)))])
          (list (make-rlm2-observation 'load mod
                  (format "Module ~a loaded." mod) #t)
                (rlm2-state-with-loaded state loaded*) 1))
        (list (make-rlm2-observation 'load mod
                (rlm2-format-diagnostic
                  (rlm2-error-require mod (fold-result-error result)))
                #f)
              state 1))))

(define (rlm2-exec-eval state action config)
  (let* ([expr (rlm2-eval-expr action)]
         [env (rlm2-state-env state)]
         ;; Pre-expand retrieve/peek/grep calls in the expression
         [expanded (rlm2-expand-env-refs expr env)]
         [code-text (format "~s" expanded)]
         ;; Split multi-expression begins
         [exprs (rlm2-split-code-exprs code-text)]
         ;; Execute each expression
         [final (let exec-loop ([es exprs]
                                [last-val ""]
                                [total-fuel 0])
                  (if (null? es)
                      (list 'ok last-val total-fuel)
                      (let ([result (fold-ipc-eval (car es))])
                        (if (fold-result-ok? result)
                            (exec-loop (cdr es)
                                       (fold-result-value result)
                                       (+ total-fuel 10))
                            (list 'err
                                  (rlm2-format-diagnostic
                                    (rlm2-error-eval (car es) (fold-result-error result)))
                                  (+ total-fuel 5))))))]
         [ok? (eq? 'ok (car final))]
         [result-val (if ok?
                        (rlm2-parse-ipc-value (cadr final))
                        (cadr final))]
         [fuel (caddr final)]
         ;; Auto-store result in env
         [result-key (string->symbol
                      (format "step-~a-result" (rlm2-state-step state)))]
         [env* (if ok?
                   (car (rlm-env-store! env result-key result-val 'sexpr))
                   env)]
         [state* (if ok? (rlm2-state-with-env state env*) state)])
    (list (make-rlm2-observation 'eval (rlm2-eval-expr action)
            result-val ok?)
          state* fuel)))

(define (rlm2-exec-store state action config)
  (let* ([key (rlm2-store-key action)]
         [expr (rlm2-store-expr action)]
         [env (rlm2-state-env state)]
         ;; Expand and eval the expression
         [expanded (rlm2-expand-env-refs expr env)]
         [code-text (format "~s" expanded)]
         [result (fold-ipc-eval code-text)])
    (if (fold-result-ok? result)
        (let* ([value (rlm2-parse-ipc-value (fold-result-value result))]
               [env* (car (rlm-env-store! env key value 'sexpr))]
               [state* (rlm2-state-with-env state env*)])
          (list (make-rlm2-observation 'store key
                  (format "Stored ~a" key) #t)
                state* 1))
        (list (make-rlm2-observation 'store key
                (rlm2-format-diagnostic
                  (rlm2-error-eval (rlm2-store-expr action) (fold-result-error result)))
                #f)
              state 1))))

(define (rlm2-exec-peek state action)
  (let* ([key (rlm2-peek-key action)]
         [n (rlm2-peek-n action)]   ; #f when (peek key) — means full fetch
         [env (rlm2-state-env state)]
         [entry (rlm-env-get env key)])
    (if (not entry)
        (list (make-rlm2-observation 'peek key
                (rlm2-format-diagnostic (rlm2-error-key-not-found key)) #f)
              state 1)
        (let ([tag (cadr entry)] [size (caddr entry)])
          (cond
            ;; Truncated peek with explicit n
            [n (let ([result (rlm-env-peek env key n)])
                 (list (make-rlm2-observation 'peek key
                         (or result (rlm2-format-diagnostic (rlm2-error-key-not-found key)))
                         (if result #t #f))
                       state 1))]
            ;; Full fetch, but value is chunked — advise to use (peek key n) or (grep ...)
            [(eq? tag 'chunks)
             (list (make-rlm2-observation 'peek key
                     (format "Value '~a' is chunked (~a chars). Use (peek '~a n) or (grep '~a \"pattern\" k)."
                             key size key key)
                     #t)
                   state 1)]
            ;; Full fetch of non-chunked value
            [else
             (let ([value (rlm-env-fetch env key)])
               (list (make-rlm2-observation 'peek key
                       (if value (format "~s" value) "CAS fetch failed")
                       (if value #t #f))
                     state 1))])))))

(define (rlm2-exec-grep state action)
  (let* ([key (rlm2-grep-key action)]
         [pattern (rlm2-grep-pattern action)]
         [k (rlm2-grep-k action)]
         [results (rlm-env-grep (rlm2-state-env state) key pattern k)])
    (list (make-rlm2-observation 'grep key
            (if results
                (rlm2-format-grep-results results pattern)
                (rlm2-format-diagnostic
                  (rlm2-error 'grep-error
                    (list 'given (format "~a" key))
                    (list 'detail "Key not found or value is not chunked")
                    (list 'suggestion "Check that the key exists in env and is a chunked value."))))
            (if results #t #f))
          state 1)))

(define (rlm2-exec-slice state action)
  (let* ([key (rlm2-slice-key action)]
         [start (rlm2-slice-start action)]
         [end (rlm2-slice-end action)]
         [env (rlm2-state-env state)])
    ;; Read chunks in range [start, end)
    (let loop ([idx start] [acc '()])
      (if (>= idx end)
          (let ([text (apply string-append (reverse acc))])
            (list (make-rlm2-observation 'slice key
                    (if (string=? text "")
                        (format "No chunks found in range [~a, ~a)" start end)
                        text)
                    (not (string=? text "")))
                  state 1))
          (let ([chunk (rlm-env-read-chunk env key idx)])
            (if chunk
                (loop (+ idx 1) (cons chunk acc))
                (loop (+ idx 1) acc)))))))

(define (rlm2-exec-recall-step state action)
  (let* ([n (rlm2-recall-step-n action)]
         [episodic (rlm2-state-episodic state)]
         [entry (assv n episodic)])
    (if entry
        (let* ([hash (cadr entry)]
               [blk (fetch-persistent (hex->hash hash))])
          (if blk
              (let ([payload (utf8->string (block-payload blk))])
                (list (make-rlm2-observation 'recall-step n payload #t)
                      state 1))
              (list (make-rlm2-observation 'recall-step n
                      (rlm2-format-diagnostic
                        (rlm2-error 'recall-error
                          (list 'given (format "step ~a" n))
                          (list 'detail "Step record not found in CAS")
                          (list 'suggestion "The step may not have been recorded yet.")))
                      #f)
                    state 1)))
        (list (make-rlm2-observation 'recall-step n
                (rlm2-format-diagnostic
                  (rlm2-error 'recall-error
                    (list 'given (format "step ~a" n))
                    (list 'detail "Step not in episodic log")
                    (list 'suggestion "Check (episodic ...) in your HUD for available step numbers.")))
                #f)
              state 1))))

(define (rlm2-exec-submit state action config)
  (let* ([expr (rlm2-submit-expr action)]
         [env (rlm2-state-env state)]
         ;; Evaluate the submit expression
         [expanded (rlm2-expand-env-refs expr env)]
         [code-text (format "~s" expanded)]
         [result (fold-ipc-eval code-text)])
    (if (fold-result-ok? result)
        (let* ([value (fold-result-value result)]
               ;; Run verifier if present
               [verifier (rlm2-config-verifier config)]
               [verified? (if verifier
                              (guard (ex [else #f])
                                (verifier value))
                              #t)])
          (if verified?
              ;; Success — set state result
              (list (make-rlm2-observation 'submit expr
                      (format "Answer accepted: ~a" value) #t)
                    (rlm2-state-with-last-result state
                      (make-rlm2-result value))
                    1)
              ;; Verifier rejected
              (list (make-rlm2-observation 'submit expr
                      (rlm2-format-diagnostic
                        (rlm2-error 'submit-rejected
                          (list 'detail "Answer rejected by verifier")
                          (list 'suggestion "Review your answer and try a different approach.")))
                      #f)
                    state 1)))
        ;; Eval failed
        (list (make-rlm2-observation 'submit expr
                (rlm2-format-diagnostic
                  (rlm2-error-eval expr (fold-result-error result)))
                #f)
              state 1))))

(define (rlm2-exec-think state action)
  ;; Think is ephemeral — no state change, observation is just an ack
  (list (make-rlm2-observation 'think 'think
          "ok" #t)
        state 0))

(define (rlm2-exec-plan! state action)
  (let ([items (rlm2-plan!-items action)])
    ;; Validate: items must be a list (ideally of pairs, but tolerate atoms)
    (if (not (list? items))
        (list (make-rlm2-observation 'plan! items
                (rlm2-format-diagnostic
                  (rlm2-error-type-mismatch "list of (item . status) pairs" items "plan!"))
                #f)
              state 0)
        ;; Normalize: ensure each item is a pair. Wrap bare atoms as (atom . pending).
        (let ([normalized (map (lambda (item)
                                 (if (pair? item) item (cons item 'pending)))
                               items)])
          (list (make-rlm2-observation 'plan! normalized
                  (format "Plan updated: ~a items" (length normalized)) #t)
                (rlm2-state-with-plan state normalized) 0)))))

(define (rlm2-normalize-expr-escaping s)
  ;; Desire path: models over-escape quotes in map-chunks expressions.
  ;; Collapse \" → " repeatedly until stable.
  (let loop ([prev s])
    (let ([next (let walk ([cs (string->list prev)] [acc '()])
                  (cond
                    [(null? cs) (list->string (reverse acc))]
                    [(and (char=? (car cs) #\\)
                          (pair? (cdr cs))
                          (char=? (cadr cs) #\"))
                     (walk (cddr cs) (cons #\" acc))]
                    [else (walk (cdr cs) (cons (car cs) acc))]))])
      (if (string=? next prev)
          next
          (loop next)))))

(define (rlm2-exec-map-chunks state action config)
  (let* ([key (rlm2-map-chunks-key action)]
         [expr-text (rlm2-normalize-expr-escaping
                      (rlm2-map-chunks-expr action))]
         [env (rlm2-state-env state)]
         [entry (rlm-env-get env key)])
    (cond
      [(not entry)
       (list (make-rlm2-observation 'map-chunks key
               (rlm2-format-diagnostic (rlm2-error-key-not-found key)) #f)
             state 1)]
      [(not (eq? (cadr entry) 'chunks))
       (list (make-rlm2-observation 'map-chunks key
               (rlm2-format-diagnostic
                 (rlm2-error 'not-chunked
                   (list 'given (format "~a" key))
                   (list 'detail "Value is not chunked")
                   (list 'suggestion "Use (eval ...) directly for small values.")))
               #f)
             state 1)]
      [(not (string? expr-text))
       (list (make-rlm2-observation 'map-chunks key
               (rlm2-format-diagnostic
                 (rlm2-error-type-mismatch "string" expr-text "map-chunks expression"))
               #f)
             state 1)]
      [else
       ;; Fetch manifest and iterate over chunks
       (let ([manifest (rlm-env-fetch env key)])
         (if (not (and manifest (chunk-manifest? manifest)))
             (list (make-rlm2-observation 'map-chunks key
                     (rlm2-format-diagnostic
                       (rlm2-error 'manifest-missing
                         (list 'given (format "~a" key))
                         (list 'detail "Chunk manifest not found in CAS")
                         (list 'suggestion "The data may be corrupted. Try re-storing the value.")))
                     #f)
                   state 1)
             (let loop ([hashes (chunk-manifest-hashes manifest)]
                        [results '()]
                        [errors 0]
                        [idx 0])
               (if (null? hashes)
                   ;; All chunks processed — parse results, flatten, store as 'map-result
                   (let* ([raw-results (reverse results)]
                          [parsed (map (lambda (x)
                                         (if (string? x)
                                             (guard (ex [else (or (string->number x) x)])
                                               (let ([val (read (open-input-string x))])
                                                 (if (eof-object? val) x val)))
                                             x))
                                       raw-results)]
                          [flattened parsed]
                          [n-chunks (length parsed)]
                          [env* (car (rlm-env-store! env 'map-result flattened 'sexpr))]
                          [state* (rlm2-state-with-env state env*)]
                          [ok? (= errors 0)])
                     (list (make-rlm2-observation 'map-chunks key
                             (format "Processed ~a chunks (~a errors). ~a results stored as 'map-result.\nValues: ~s"
                                     n-chunks errors
                                     (length flattened)
                                     flattened)
                             ok?)
                           state* (max 1 n-chunks)))
                   ;; Process one chunk
                   (let* ([chunk-blk (fetch-persistent (hex->hash (car hashes)))]
                          [chunk-text (if chunk-blk
                                         (utf8->string (block-payload chunk-blk))
                                         "")]
                          [code (format "(let ([*chunk* ~s]) ~a)" chunk-text expr-text)]
                          [result (fold-ipc-eval code)]
                          [failed? (not (fold-result-ok? result))]
                          [val (if failed?
                                   (format "error:~a" (fold-result-error result))
                                   (fold-result-value result))]
                          ;; Normalize void/empty to "()"
                          [val* (if (and (string? val)
                                         (or (string=? val "#<void>")
                                             (string=? val "")))
                                    "()"
                                    val)])
                     (loop (cdr hashes) (cons val* results)
                           (+ errors (if failed? 1 0)) (+ idx 1)))))))])))

;;; --- Memory Actions ---

(define (rlm2-exec-journal state action)
  (let ([tag (rlm2-journal-tag action)]
        [text (rlm2-journal-text action)])
    (if (not (string? text))
        (list (make-rlm2-observation 'journal tag
                (rlm2-format-diagnostic
                  (rlm2-error-type-mismatch "string" text "journal text"))
                #f)
              state 0)
        (let ([state* (rlm2-state-add-journal state tag text)])
          (list (make-rlm2-observation 'journal tag
                  (format "Logged to journal under '~a'" tag) #t)
                state* 0)))))

(define (rlm2-exec-recall state action)
  (let* ([tag (rlm2-recall-tag action)]
         [journal (rlm2-state-journal state)]
         [matches (filter (lambda (entry) (eq? (car entry) tag)) journal)])
    (if (null? matches)
        (list (make-rlm2-observation 'recall tag
                (format "No journal entries tagged '~a'" tag) #t)
              state 0)
        (let ([texts (map cdr matches)])
          (list (make-rlm2-observation 'recall tag
                  (format "~a entries:\n~a"
                          (length texts)
                          (rlm2-join-journal-entries texts))
                  #t)
                state 0)))))

(define (rlm2-join-strings sep texts)
  (if (null? texts)
      ""
      (let loop ([rest (cdr texts)] [acc (car texts)])
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

(define (rlm2-join-journal-entries texts)
  (rlm2-join-strings "\n" texts))


(load "boundary/pipeline/rlm2-memory.ss")

(define (rlm2-exec-begin state action config depth)
  ;; Enforce max nesting depth
  (if (>= depth (rlm2-config-max-depth config))
      (list (make-rlm2-observation 'begin #f
              (rlm2-format-diagnostic
                (rlm2-error 'nesting-limit
                  (list 'detail (format "begin nesting depth ~a exceeds max-depth ~a"
                                        depth (rlm2-config-max-depth config)))
                  (list 'suggestion "Flatten your begin block or use sequential actions.")))
              #f)
            state 1)
  ;; Execute children sequentially, stop on first error or submit completion
  (let loop ([children (rlm2-begin-actions action)]
             [state state]
             [observations '()]
             [total-fuel 0])
    (if (null? children)
        ;; All succeeded — combine observations so model sees all results
        (if (null? observations)
            (list (make-rlm2-observation 'begin #f "empty begin" #t) state total-fuel)
            (let* ([obs-list (reverse observations)]
                   [values (map rlm2-observation-value obs-list)]
                   ;; Filter out trivial "noted" values to reduce noise
                   [substantive (filter (lambda (v)
                                          (not (and (string? v) (string=? v "noted"))))
                                        values)]
                   [combined (if (null? substantive)
                                 "noted"
                                 (rlm2-join-strings "\n" (map (lambda (v)
                                                                (if (string? v) v (format "~s" v)))
                                                              substantive)))]
                   [last-obs (car (reverse obs-list))])
              (list (make-rlm2-observation
                      (rlm2-observation-action-type last-obs)
                      (rlm2-observation-target last-obs)
                      combined
                      #t)
                    state total-fuel)))
        (let* ([child (car children)]
               [result (rlm2-execute-action state child config (+ depth 1))]
               [obs (car result)]
               [state* (cadr result)]
               [fuel (caddr result)])
          (cond
            ;; Submit succeeded — stop, propagate the completed state
            [(rlm2-state-complete? state*)
             (list obs state* (+ total-fuel fuel))]
            ;; Error — stop here
            [(not (rlm2-observation-ok? obs))
             (list obs state* (+ total-fuel fuel))]
            ;; Continue to next child
            [else
             (loop (cdr children) state*
                   (cons obs observations)
                   (+ total-fuel fuel))]))))))

;;; --- LSP Actions (via IPC v2 commands) ---

(define (rlm2-exec-lookup state action)
  (let* ([sym (rlm2-lookup-symbol action)]
         [sym-str (if (symbol? sym) (symbol->string sym) (format "~a" sym))]
         [result (fold-ipc-command 'lsp-lookup
                   `((symbol . ,sym-str)))])
    (list (make-rlm2-observation 'lookup sym
            (if (fold-result-ok? result)
                (or (fold-result-value result)
                    (rlm2-format-diagnostic
                      (rlm2-error 'symbol-not-found
                        (list 'given sym-str)
                        (list 'suggestion (format "Try (symbols \"~a\") to search by fragment." sym-str)))))
                (rlm2-format-diagnostic
                  (rlm2-error 'lookup-error
                    (list 'given sym-str)
                    (list 'detail (fold-result-error result))
                    (list 'suggestion "Check the symbol name and ensure the module is loaded."))))
            (fold-result-ok? result))
          state 1)))

(define (rlm2-exec-definition state action)
  (let* ([sym (rlm2-definition-symbol action)]
         [sym-str (if (symbol? sym) (symbol->string sym) (format "~a" sym))]
         [result (fold-ipc-command 'lsp-definition
                   `((symbol . ,sym-str)))])
    (list (make-rlm2-observation 'definition sym
            (if (fold-result-ok? result)
                (or (fold-result-value result)
                    (rlm2-format-diagnostic
                      (rlm2-error 'symbol-not-found
                        (list 'given sym-str)
                        (list 'suggestion (format "Try (symbols \"~a\") to search by fragment." sym-str)))))
                (rlm2-format-diagnostic
                  (rlm2-error 'definition-error
                    (list 'given sym-str)
                    (list 'detail (fold-result-error result))
                    (list 'suggestion "Check the symbol name and ensure the module is loaded."))))
            (fold-result-ok? result))
          state 1)))

(define (rlm2-exec-symbols state action)
  (let* ([query (rlm2-symbols-query action)]
         [query-str (cond
                      [(string? query) query]
                      [(symbol? query) (symbol->string query)]
                      [else (format "~a" query)])]
         [result (fold-ipc-command 'lsp-symbols
                   `((query . ,query-str)))])
    (list (make-rlm2-observation 'symbols query
            (if (fold-result-ok? result)
                (or (fold-result-value result)
                    (rlm2-format-diagnostic
                      (rlm2-error 'no-matches
                        (list 'given query-str)
                        (list 'suggestion "Try a shorter or different search fragment."))))
                (rlm2-format-diagnostic
                  (rlm2-error 'symbols-error
                    (list 'given query-str)
                    (list 'detail (fold-result-error result))
                    (list 'suggestion "Check the query string."))))
            (fold-result-ok? result))
          state 1)))

(define (rlm2-exec-outline state action)
  (let* ([file (rlm2-outline-file action)]
         [file-str (cond
                     [(string? file) file]
                     [(symbol? file) (symbol->string file)]
                     [else (format "~a" file)])]
         [result (fold-ipc-command 'lsp-outline
                   `((file . ,file-str)))])
    (list (make-rlm2-observation 'outline file
            (if (fold-result-ok? result)
                (or (fold-result-value result)
                    (rlm2-format-diagnostic
                      (rlm2-error 'no-definitions
                        (list 'given file-str)
                        (list 'suggestion "Check the file path. Use lattice paths like 'lattice/linalg/vec.ss'."))))
                (rlm2-format-diagnostic
                  (rlm2-error 'outline-error
                    (list 'given file-str)
                    (list 'detail (fold-result-error result))
                    (list 'suggestion "Check the file path."))))
            (fold-result-ok? result))
          state 1)))

;;; ====
;;; Delegate Action (recursive sub-agent)
;;; ====
;;;
;;; Spawns a sub-RLM at depth+1 to handle a focused sub-task.
;;; The sub-agent inherits the parent's system prompt and provider but
;;; gets a fraction of the remaining fuel and steps. The parent blocks
;;; synchronously while the child runs.

(define (rlm2-exec-delegate state action config depth)
  (let ([sub-task (rlm2-delegate-task action)]
        [sub-input (rlm2-delegate-input action)]
        [max-depth (rlm2-config-max-depth config)])
    ;; Enforce depth limit
    (if (>= (+ depth 1) max-depth)
        (list (make-rlm2-observation 'delegate sub-task
                (rlm2-format-diagnostic
                  (rlm2-error 'delegation-limit
                    (list 'detail (format "Depth ~a would exceed max-depth ~a"
                                          (+ depth 1) max-depth))
                    (list 'suggestion "Handle this sub-task directly instead of delegating.")))
                #f)
              state 1)
        ;; Build sub-config with reduced resources
        (let* ([remaining-fuel (rlm2-state-fuel state)]
               [remaining-steps (- (rlm2-config-max-steps config)
                                   (rlm2-state-step state))]
               [sub-fuel (min (quotient (* remaining-fuel 2) 5) 15000)]
               [sub-steps (min (max 1 (- remaining-steps 2)) 8)]
               [sub-config (make-rlm2-config
                             (rlm2-config-provider config)
                             (rlm2-config-system-prompt config)
                             sub-steps
                             sub-fuel
                             (rlm2-config-chunk-size config)
                             max-depth
                             (rlm2-config-loop-window config)
                             (rlm2-config-context-budget config)
                             #f  ; no verifier for sub-tasks
                             (rlm2-config-max-tokens config))]
               [result (guard (ex [else
                                   (list 'rlm2-run-result 'error
                                     (format "Delegate exception: ~a"
                                       (if (message-condition? ex)
                                           (condition-message ex)
                                           "unknown"))
                                     "" '())])
                         (rlm2-run-at-depth sub-config sub-task sub-input
                                            (+ depth 1)))]
               [status (rlm2-run-result-status result)]
               [output (rlm2-run-result-output result)]
               ;; Charge the full sub-fuel allocation. The parent allocated
               ;; this budget — unused fuel is not returned. The trajectory
               ;; records actual consumption for auditing.
               [output-text (if (string? output)
                                output
                                (format "~a" output))]
               [obs-text (format "[sub-agent ~a] ~a"
                                 status
                                 (if (> (string-length output-text) 1500)
                                     (string-append (substring output-text 0 1497) "...")
                                     output-text))])
          (list (make-rlm2-observation 'delegate sub-task
                  obs-text
                  (memq status '(completed)))
                state sub-fuel)))))

;;; --- Lifecycle Actions (idle, reframe) ---

(define (rlm2-exec-idle state action)
  (list (make-rlm2-observation 'idle #f "Pausing. State will be checkpointed." #t)
        (rlm2-state-with-last-result state (make-rlm2-idle-marker))
        0))

(define (rlm2-exec-reframe state action)
  (let ([new-task (rlm2-reframe-task action)])
    (if (not (string? new-task))
        (list (make-rlm2-observation 'reframe new-task
                (rlm2-format-diagnostic
                  (rlm2-error-type-mismatch "string" new-task "reframe task"))
                #f)
              state 0)
        (list (make-rlm2-observation 'reframe new-task
                (format "Task reframed to: ~a" new-task) #t)
              (rlm2-state-with-task state new-task) 0))))


(load "boundary/pipeline/rlm2-agent-state.ss")

;;; ====
;;; Wake Entry Point (continuous agent lifecycle)
;;; ====
;;;
;;; rlm2-wake : String × Rlm2Config × Nat × Nat → Rlm2WakeResult
;;;
;;; Restores agent state from CAS (or creates fresh), runs for up to
;;; max-steps with the provided fuel budget, then checkpoints on:
;;;   - (idle) action → paused
;;;   - step/fuel exhaustion → paused
;;;   - (submit) → completed
;;;   - error → error (state still checkpointed)

(doc 'type '(-> String String Rlm2Config Nat Nat Rlm2WakeResult))
(doc 'description "Wake a continuous agent. Restores state, runs for budget, checkpoints on pause.")
(define (rlm2-wake agent-id task config max-steps fuel)
  (let* ([session-id (format "rlm2-agent-~a" agent-id)]
         [started (rlm2-current-iso8601)]
         ;; Restore or create state
         [restored (rlm2-restore-state agent-id)]
         [prev-head (and restored
                         (let ([h (rlm2-read-agent-head agent-id)])
                           (and h (hash->hex h))))]
         [state0 (if restored
                     ;; Refuel and optionally update task
                     (let* ([s1 (rlm2-state-with-fuel restored
                                  (+ (rlm2-state-fuel restored) fuel))]
                            ;; Clear idle marker from last-result
                            [s2 (if (rlm2-state-idle? s1)
                                    (rlm2-state-with-last-result s1 #f)
                                    s1)]
                            ;; Update task if provided and different
                            [s3 (if (and (string? task)
                                        (not (string=? task ""))
                                        (not (string=? task (rlm2-state-task s2))))
                                    (rlm2-state-with-task s2 task)
                                    s2)])
                       s3)
                     ;; Fresh state — task must be a string
                     (make-rlm2-state
                       (or task "") '() '() '() '() '() '()
                       #f fuel 0))]
         [sys-prompt (rlm2-build-system-prompt
                       (rlm2-config-system-prompt config))])
    ;; Parameterize with persistent session ID
    (parameterize ([*pipeline-session* session-id])
      ;; Init worker prelude (idempotent if session survives)
      (rlm2-init-worker-prelude!)
      ;; Drive the loop — identical to rlm2-run-at-depth inner loop
      ;; but with different termination semantics
      (let loop ([state state0]
                 [steps-taken 0]
                 [fingerprints '()]
                 [prev-step-hash prev-head]
                 [consecutive-thinks 0]
                 [history '()])
        (cond
          ;; Success: agent submitted an answer
          [(rlm2-state-complete? state)
           (let ([state-hash (rlm2-checkpoint-state! agent-id state
                               prev-step-hash)])
             (make-rlm2-wake-result 'completed state-hash
                                    (rlm2-state-fuel state)
                                    (rlm2-state-step state)))]
          ;; Idle: voluntary pause
          [(rlm2-state-idle? state)
           (let ([state-hash (rlm2-checkpoint-state! agent-id state
                               prev-step-hash)])
             (make-rlm2-wake-result 'paused state-hash
                                    (rlm2-state-fuel state)
                                    (rlm2-state-step state)))]
          ;; Exhaustion: steps or fuel
          [(or (>= steps-taken max-steps)
               (<= (rlm2-state-fuel state) 0))
           (let ([state-hash (rlm2-checkpoint-state! agent-id state
                               prev-step-hash)])
             (make-rlm2-wake-result 'paused state-hash
                                    (rlm2-state-fuel state)
                                    (rlm2-state-step state)))]
          [else
           ;; === ACT PHASE ===
           (let* ([step-t0 (rlm2-time-ms)]
                  [hud (rlm2-render-state state
                         (rlm2-config-context-budget config))]
                  [few-shot (rlm2-config-few-shot config)]
                  [history-msgs (rlm2-history->messages history)]
                  [messages (append
                              (list (rlm2-make-msg "system" sys-prompt))
                              few-shot
                              history-msgs
                              (list (rlm2-make-msg "user" hud)))]
                  [act-response (rlm-chat (rlm2-config-provider config)
                                          messages (rlm2-config-max-tokens config) *rlm2-act-temperature*)])
             (cond
               ;; LLM call failed — checkpoint and return error
               [(rlm-chat-err? act-response)
                (let ([state-hash (rlm2-checkpoint-state! agent-id state
                                    prev-step-hash)])
                  (make-rlm2-wake-result 'error state-hash
                                         (rlm2-state-fuel state)
                                         (rlm2-state-step state)))]
               [else
                (let* ([raw-text (rlm-chat-text act-response)]
                       [reasoning (rlm-chat-reasoning act-response)]
                       ;; Parse action
                       [parse-result (rlm2-parse-response raw-text)]
                       [action (rlm2-parse-result-action parse-result)]
                       [raw-thought (rlm2-parse-result-thought parse-result)]
                       ;; Truncation retry — only when model intended a non-think
                       ;; action that got truncated and fell back to think
                       [intended-think? (let ([t (rlm2-parse-trim raw-text)])
                                          (and (> (string-length t) 6)
                                               (string=? (substring t 0 6) "(think")))]
                       [truncated? (and (rlm2-think? action)
                                        (not intended-think?)
                                        (rlm2-looks-truncated? raw-text))]
                       [retry-result (and truncated?
                                         (rlm-chat (rlm2-config-provider config)
                                                   messages
                                                   (min (* 4 (rlm2-config-max-tokens config)) 8192)
                                                   *rlm2-act-temperature*))]
                       [effective-text
                        (if (and retry-result (rlm-chat-ok? retry-result))
                            (let ([rt (rlm-chat-text retry-result)])
                              (let ([rp (rlm2-parse-response rt)])
                                (if (rlm2-think? (rlm2-parse-result-action rp))
                                    raw-text
                                    rt)))
                            raw-text)]
                       [final-parse (if (eq? effective-text raw-text)
                                        parse-result
                                        (rlm2-parse-response effective-text))]
                       [action (rlm2-parse-result-action final-parse)]
                       [raw-thought (rlm2-parse-result-thought final-parse)]
                       ;; === EXECUTE ===
                       [exec-result (rlm2-execute-action state action config 0)]
                       [observation (car exec-result)]
                       [state-after-exec (cadr exec-result)]
                       [fuel-used (caddr exec-result)]
                       ;; === REFLECT ===
                       [action-type (rlm2-action-type action)]
                       [mechanical? (memq action-type *rlm2-mechanical-actions*)]
                       [note (if mechanical?
                                 (rlm2-mechanical-note action observation
                                   (rlm2-state-step state))
                                 (rlm2-reflect config raw-thought action
                                   observation (rlm2-state-step state)))]
                       ;; Prepend alias correction if an alias was resolved
                       [alias-info (rlm2-parse-result-alias-info final-parse)]
                       [note (if alias-info
                                 (string-append
                                   (rlm2-alias-correction-note alias-info)
                                   " " note)
                                 note)]
                       ;; === UPDATE STATE ===
                       [state* (rlm2-update-state state-after-exec
                                 action observation note fuel-used)]
                       ;; === LOOP DETECTION ===
                       [fp (rlm2-semantic-fingerprint state* action-type)]
                       [looping? (rlm2-loop-detected? fingerprints fp
                                   (rlm2-config-loop-window config))]
                       [state** (if looping?
                                    (rlm2-state-with-last-result state*
                                      (rlm2-format-diagnostic (rlm2-warning-loop)))
                                    state*)]
                       ;; === THINK-SPAM ===
                       [thinks (if (eq? action-type 'think)
                                   (+ consecutive-thinks 1) 0)]
                       [state** (cond
                                  [(>= thinks 3)
                                   (rlm2-state-with-last-result state**
                                     (rlm2-format-diagnostic
                                       (rlm2-nudge-think-streak thinks)))]
                                  [else state**])]
                       ;; === RECORD STEP ===
                       [step-hash (rlm2-record-step!
                                   (rlm2-state-step state) action observation note
                                   (rlm2-config-provider config) fuel-used
                                   prev-step-hash)]
                       [state*** (rlm2-state-add-episodic state**
                                   (rlm2-state-step state) step-hash
                                   action-type
                                   (rlm2-observation-ok? observation))]
                       ;; === PROGRESS ===
                       [_progress (rlm2-emit-progress!
                                   (rlm2-state-step state) action-type
                                   (rlm2-observation-ok? observation) note)]
                       ;; Fingerprints
                       [fps* (if looping? '() (cons fp fingerprints))]
                       ;; === SLIDING WINDOW ===
                       [action-text (if (eq? action-type 'think)
                                        "(think)"
                                        (format "~s" action))]
                       [obs-summary (rlm2-observation-summary observation)]
                       [history* (rlm2-history-push history action-text obs-summary 3)])
                  (loop state*** (+ steps-taken 1) fps* step-hash
                        thinks history*))]))])))))

;;; ====
;;; Reflection Pass
;;; ====

(define (rlm2-reflect config thought action observation step-num)
  (guard (ex [else
              ;; Fallback: mechanical note
              (rlm2-mechanical-note action observation step-num)])
    (let* ([prompt (rlm2-build-reflection-prompt
                    thought action observation)]
           [messages (list (rlm2-make-msg "system"
                            "You are a note-taker. Summarize what was learned in ONE sentence. Do not solve problems or take actions.")
                          (rlm2-make-msg "user" prompt))]
           [response (rlm-chat (rlm2-config-provider config)
                               messages 256 0.3)])
      (if (rlm-chat-ok? response)
          (let ([text (rlm-chat-text response)])
            ;; Trim to one line
            (let ([first-line (rlm2-first-line text)])
              (if (string=? first-line "")
                  (rlm2-mechanical-note action observation step-num)
                  first-line)))
          ;; API error — fall back
          (rlm2-mechanical-note action observation step-num)))))

(define (rlm2-mechanical-note action observation step-num)
  (let* ([type (rlm2-action-type action)]
         [ok? (rlm2-observation-ok? observation)]
         [value (rlm2-observation-value observation)]
         [summary (if (string? value)
                      (if (> (string-length value) 200)
                          (string-append (substring value 0 197) "...")
                          value)
                      (let ([s (format "~a" value)])
                        (if (> (string-length s) 200)
                            (string-append (substring s 0 197) "...")
                            s)))])
    (format "Step ~a: ~a ~a. ~a"
            step-num type (if ok? "succeeded" "FAILED")
            summary)))

;;; ====
;;; State Update
;;; ====

(define (rlm2-update-state state action observation note fuel-used)
  ;; If exec already set an rlm2-result marker, preserve it.
  ;; For rlm2-idle, only preserve on the idle action itself —
  ;; otherwise the marker becomes sticky in rlm2-run which has
  ;; no idle-exit condition, dropping subsequent observations.
  (let* ([has-result? (or (rlm2-state-complete? state)
                          (and (rlm2-state-idle? state)
                               (rlm2-idle? action)))]
         [last-result (if has-result?
                         (rlm2-state-last-result state)
                         observation)]
         [s1 (rlm2-state-with-last-result state last-result)]
         [s2 (rlm2-state-add-note s1 note)]
         [s3 (rlm2-state-use-fuel s2 fuel-used)]
         [s4 (rlm2-state-advance-step s3)])
    s4))


(load "boundary/pipeline/rlm2-env-expand.ss")


;;; ====
;;; Grep Results Formatting
;;; ====

;;; rlm2-format-grep-results : (List (String . Number)) String -> String
;;; Extract matching lines from BM25 chunk results.
;;; Returns lines containing the pattern (case-insensitive substring match)
;;; rather than entire chunks, giving the model clean signal.
;;; Falls back to full chunk text if no lines match the pattern literally.
(define (rlm2-format-grep-results results pattern)
  (if (null? results)
      "No matches found."
      (let* ([pat-lower (string-downcase pattern)]
             [lines-from-chunk
              (lambda (chunk-text)
                (let loop ([chars (string->list chunk-text)]
                           [line '()]
                           [acc '()])
                  (cond
                    [(null? chars)
                     (let ([final (list->string (reverse line))])
                       (reverse (if (string-contains-ci final pat-lower)
                                    (cons final acc)
                                    acc)))]
                    [(char=? (car chars) #\newline)
                     (let ([l (list->string (reverse line))])
                       (loop (cdr chars) '()
                             (if (string-contains-ci l pat-lower)
                                 (cons l acc)
                                 acc)))]
                    [else (loop (cdr chars) (cons (car chars) line) acc)])))]
             [all-lines
              (let chunk-loop ([rs results] [acc '()])
                (if (null? rs)
                    (reverse acc)
                    (let ([lines (lines-from-chunk (car (car rs)))])
                      (chunk-loop (cdr rs) (append (reverse lines) acc)))))]
             ;; Deduplicate matching lines (same entry may appear in overlapping chunks)
             [unique-lines
              (let dedup ([ls all-lines] [seen '()] [acc '()])
                (cond
                  [(null? ls) (reverse acc)]
                  [(member (car ls) seen) (dedup (cdr ls) seen acc)]
                  [else (dedup (cdr ls) (cons (car ls) seen)
                               (cons (car ls) acc))]))])
        (if (null? unique-lines)
            ;; No literal matches — fall back to first chunk (BM25 fuzzy match)
            (let ([text (car (car results))])
              (if (> (string-length text) 500)
                  (substring text 0 500)
                  text))
            (let join ([strs unique-lines] [out ""])
              (if (null? strs) out
                  (join (cdr strs)
                        (if (string=? out "")
                            (car strs)
                            (string-append out "\n" (car strs))))))))))

;;; Case-insensitive substring containment check
(define (string-contains-ci str pattern)
  (let ([slen (string-length str)]
        [plen (string-length pattern)])
    (if (> plen slen)
        #f
        (let loop ([i 0])
          (cond
            [(> (+ i plen) slen) #f]
            [(string-ci-match? str i pattern plen) #t]
            [else (loop (+ i 1))])))))

(define (string-ci-match? str start pattern plen)
  (let loop ([j 0])
    (cond
      [(= j plen) #t]
      [(char-ci=? (string-ref str (+ start j))
                  (string-ref pattern j))
       (loop (+ j 1))]
      [else #f])))

;;; ====
;;; CAS Recording
;;; ====

(define (rlm2-record-step! step-num action observation note provider
                           fuel-used prev-step-hash)
  (let* ([obs-val-str (let ([v (rlm2-observation-value observation)])
                        (if (string? v) v (format "~a" v)))]
         [payload `((step . ,step-num)
                    (action . ,action)
                    (observation-type . ,(rlm2-observation-action-type observation))
                    (observation-ok . ,(rlm2-observation-ok? observation))
                    (observation-value . ,(rlm2-truncate-telemetry obs-val-str 2000))
                    (note . ,note)
                    (model . ,(rlm-provider-model-id provider))
                    (fuel-used . ,fuel-used)
                    (timestamp . ,(rlm2-current-iso8601)))]
         [refs (if prev-step-hash
                   (vector (hex->hash prev-step-hash))
                   (make-vector 0))]
         [blk (make-block 'rlm2/step
                          (string->utf8 (format "~s" payload))
                          refs)]
         [hash (store-persistent! blk)])
    (hash->hex hash)))

(define (rlm2-finalize config state status output started depth
                        last-step-hash input-hex)
  (let* ([finished (rlm2-current-iso8601)]
         [env (rlm2-state-env state)]
         ;; Store output
         [output-pair (rlm-env-store! env 'output output 'result)]
         [env* (car output-pair)]
         [output-hex (cdr output-pair)]
         ;; Store config
         [config-hex (rlm2-store-config! config)]
         ;; Build trajectory
         [traj (make-rlm2-trajectory config-hex input-hex output-hex
                                     (rlm2-state-step state)
                                     (- (rlm2-config-max-fuel config)
                                        (rlm2-state-fuel state))
                                     status started finished depth)]
         [traj-blk (make-block 'rlm2/trajectory
                               (string->utf8 (format "~s" traj))
                               (if last-step-hash
                                   (vector (hex->hash last-step-hash)
                                           (hex->hash output-hex))
                                   (vector (hex->hash output-hex))))]
         [traj-hash (store-persistent! traj-blk)]
         [traj-hex (hash->hex traj-hash)])
    ;; Cleanup: destroy the daemon session to free memory
    (guard (ex [else #f])  ; don't let cleanup errors kill the result
      (fold-ipc-eval "(bye)"))
    (list 'rlm2-run-result status output traj-hex env*)))

(define (rlm2-store-config! config)
  (let* ([blk (make-block 'rlm2/config
                          (string->utf8 (format "~s" config))
                          (make-vector 0))]
         [hash (store-persistent! blk)])
    (hash->hex hash)))

;;; ====
;;; Run Result Accessors
;;; ====

(define (rlm2-run-result? x)
  (and (pair? x) (eq? (car x) 'rlm2-run-result)))

(define (rlm2-run-result-status r)          (list-ref r 1))
(define (rlm2-run-result-output r)          (list-ref r 2))
(define (rlm2-run-result-trajectory-hash r) (list-ref r 3))
(define (rlm2-run-result-env r)             (list-ref r 4))

;;; ====
;;; Telemetry
;;; ====
;;;
;;; Lightweight: store as CAS blocks alongside trajectory.
;;; Log action type, model, timing, success, and parse failures.

(define (rlm2-record-telemetry! model-id action-type step-num
                                fuel-used ok? raw-text parse-result)
  ;; Only record parse failures and eval actions (desire-path candidates)
  (when (or (not ok?)
            (eq? action-type 'eval)
            (rlm2-think? (rlm2-parse-result-action parse-result)))
    (let* ([detail (cond
                     [(rlm2-think? (rlm2-parse-result-action parse-result))
                      ;; Parse fell through to think — log raw output, candidate, and reason
                      (let ([candidate (rlm2-parse-result-candidate parse-result)]
                            [reason (rlm2-parse-result-failure-reason parse-result)])
                        (format "~s"
                          `((raw . ,(rlm2-truncate-telemetry raw-text 500))
                            (candidate . ,(and candidate
                                               (rlm2-truncate-telemetry
                                                 (format "~s" candidate) 200)))
                            (reason . ,reason))))]
                     [(eq? action-type 'eval)
                      "eval-usage"]
                     [else "action-error"])]
           [telem (make-rlm2-telemetry model-id action-type step-num
                    fuel-used ok? detail)]
           [blk (make-block 'rlm2/telemetry
                            (string->utf8 (format "~s" telem))
                            (make-vector 0))])
      (store-persistent! blk)))
  (void))

(define (rlm2-truncate-telemetry text max-len)
  (if (<= (string-length text) max-len)
      text
      (string-append (substring text 0 (- max-len 3)) "...")))


(load "boundary/pipeline/rlm2-worker-prelude.ss")


;;; ====
;;; Truncation Detection
;;; ====
;;; Heuristic: if the raw text has more open parens than close parens,
;;; it was likely cut off by max_tokens.

(define (rlm2-looks-truncated? text)
  (let ([len (string-length text)])
    (and (> len 50)  ; don't trigger on very short outputs
         (let loop ([i 0] [depth 0])
           (if (>= i len)
               (> depth 0)  ; unclosed parens remain
               (let ([c (string-ref text i)])
                 (cond
                   [(char=? c #\() (loop (+ i 1) (+ depth 1))]
                   [(char=? c #\)) (loop (+ i 1) (- depth 1))]
                   [else           (loop (+ i 1) depth)])))))))

;;; ====
;;; Utilities
;;; ====

(define (rlm2-make-msg role content)
  `((role . ,role) (content . ,content)))

(define (rlm2-first-line text)
  (let ([len (string-length text)])
    (let loop ([i 0])
      (cond
        [(>= i len) text]
        [(char=? (string-ref text i) #\newline)
         (substring text 0 i)]
        [else (loop (+ i 1))]))))

;;; ====
;;; Sliding Window History
;;; ====
;;;
;;; Keeps the last N (action-text . observation-summary) pairs.
;;; Converted to alternating assistant/user messages for the LLM context.

(define (rlm2-observation-summary obs)
  (let* ([ok (rlm2-observation-ok? obs)]
         [val (rlm2-observation-value obs)]
         [type (rlm2-observation-action-type obs)]
         [key (rlm2-observation-target obs)]
         [text (if (string? val) val (format "~s" val))]
         ;; Cap at 1500 chars to avoid blowing context
         [text (if (> (string-length text) 1500)
                   (string-append (substring text 0 1497) "...")
                   text)])
    (format "(observation ~a ~a ok=~a\n  ~a)" type key ok text)))

(define (rlm2-history-push history action-text obs-summary max-turns)
  ;; Push newest to front, keep at most max-turns
  (let ([entry (cons action-text obs-summary)])
    (if (>= (length history) max-turns)
        (cons entry (list-head history (- max-turns 1)))
        (cons entry history))))

(define (rlm2-history->messages history)
  ;; History is newest-first. Reverse to chronological, then flatten
  ;; into alternating assistant/user messages.
  (let loop ([turns (reverse history)] [msgs '()])
    (if (null? turns)
        (reverse msgs)
        (let ([turn (car turns)])
          (loop (cdr turns)
                (cons (rlm2-make-msg "user" (cdr turn))      ; observation
                      (cons (rlm2-make-msg "assistant" (car turn))  ; action
                            msgs)))))))

(define (list-head lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (list-head (cdr lst) (- n 1)))))

(define (rlm2-generate-run-id)
  (format "~a-~a"
          (time-second (current-time))
          (random 100000)))

(define (rlm2-current-iso8601)
  (let ([d (current-date)])
    (format "~4d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year d) (date-month d) (date-day d)
            (date-hour d) (date-minute d) (date-second d))))
