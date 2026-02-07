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

(doc 'module 'rlm2-drive)
(doc 'description "RLM v2 driver: HUD-based state machine with act/reflect loop, action dispatch, CAS trajectory recording, and telemetry.")
(doc 'layer 'boundary)
(doc 'purity 'impure)

;;; ====
;;; Lattice Search (lazy-loaded in main process)
;;; ====
;;; Same pattern as v1: lattice meta can't load in IPC worker (nested loads
;;; break frame protocol). Lazy-load in main process, intercept search actions.

(define *rlm2-lattice-meta-loaded?* #f)

(define (rlm2-ensure-lattice-meta!)
  (unless *rlm2-lattice-meta-loaded?*
    (let ([sink (open-output-string)])
      (parameterize ([current-output-port sink])
        (load "lattice/meta/meta.ss")
        (lattice-init!)))
    (set! *rlm2-lattice-meta-loaded?* #t)))

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
                         task '() env+task '() '() '()
                         #f  ; no last-result yet
                         (rlm2-config-max-fuel config) 0)]
         ;; Build system prompt
         [sys-prompt (rlm2-build-system-prompt
                      (rlm2-config-system-prompt config))])
    ;; Parameterize pipeline session for IPC
    (parameterize ([*pipeline-session* session-id])
      ;; Load worker prelude into IPC session
      (rlm2-init-worker-prelude!)
      ;; Drive the loop
      (let loop ([state initial-state]
                 [fingerprints '()]
                 [prev-step-hash #f])
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
           ;; === ACT PHASE ===
           (let* ([hud (rlm2-render-state state
                         (rlm2-config-context-budget config))]
                  ;; Call LLM with system prompt + HUD as user message
                  [messages (list (rlm2-make-msg "system" sys-prompt)
                                  (rlm2-make-msg "user" hud))]
                  [act-response (rlm-chat (rlm2-config-provider config)
                                          messages 4096 0.7)])
             (cond
               ;; LLM call failed
               [(rlm-chat-err? act-response)
                (rlm2-finalize config state 'error
                               (rlm-chat-error-msg act-response)
                               started depth prev-step-hash input-hex)]
               [else
                (let* ([raw-text (rlm-chat-text act-response)]
                       ;; Parse action from model output
                       [parse-result (rlm2-parse-response raw-text)]
                       [action (rlm2-parse-result-action parse-result)]
                       [raw-thought (rlm2-parse-result-thought parse-result)]
                       ;; === EXECUTE PHASE ===
                       [exec-result (rlm2-execute-action state action
                                      config depth)]
                       [observation (car exec-result)]
                       [state-after-exec (cadr exec-result)]
                       [fuel-used (caddr exec-result)]
                       ;; === REFLECT PHASE ===
                       [note (rlm2-reflect config raw-thought action
                               observation (rlm2-state-step state))]
                       ;; === UPDATE STATE ===
                       [state* (rlm2-update-state state-after-exec
                                 action observation note fuel-used)]
                       ;; === LOOP DETECTION ===
                       [fp (rlm2-semantic-fingerprint state*
                             (rlm2-action-type action))]
                       [looping? (rlm2-loop-detected? fingerprints fp
                                   (rlm2-config-loop-window config))]
                       ;; Inject loop-break note if stuck
                       [state** (if looping?
                                    (rlm2-state-add-note state*
                                      "[LOOP] Repeated action pattern detected. Try a different approach.")
                                    state*)]
                       ;; === RECORD STEP ===
                       [step-hash (rlm2-record-step!
                                   (rlm2-state-step state)
                                   action observation note
                                   (rlm2-config-provider config)
                                   fuel-used prev-step-hash)]
                       ;; Add episodic entry
                       [state*** (rlm2-state-add-episodic state**
                                   (rlm2-state-step state)
                                   step-hash)]
                       ;; === TELEMETRY ===
                       [_telem (rlm2-record-telemetry!
                                (rlm-provider-model-id
                                 (rlm2-config-provider config))
                                (rlm2-action-type action)
                                (rlm2-state-step state)
                                fuel-used
                                (rlm2-observation-ok? observation)
                                raw-text parse-result)]
                       ;; Update fingerprints
                       [fps* (if looping?
                                 '()  ; reset after loop break
                                 (cons fp fingerprints))])
                  (loop state*** fps* step-hash))]))])))))

;;; ====
;;; Action Execution
;;; ====
;;;
;;; execute-action : State -> Action -> Config -> Nat -> (List Observation State FuelUsed)
;;; Dispatches on action type. Returns (list observation updated-state fuel-used).

(define (rlm2-execute-action state action config depth)
  (guard (ex [else
              (list (make-rlm2-observation
                     (if (pair? action) (car action) 'unknown)
                     #f
                     (format "Exception: ~a"
                             (if (message-condition? ex)
                                 (condition-message ex)
                                 "unknown"))
                     #f)
                    state 1)])
    (let ([type (rlm2-action-type action)])
      (case type
        [(search)   (rlm2-exec-search state action)]
        [(inspect)  (rlm2-exec-inspect state action)]
        [(exports)  (rlm2-exec-exports state action)]
        [(load)     (rlm2-exec-load state action)]
        [(eval)     (rlm2-exec-eval state action config)]
        [(store)    (rlm2-exec-store state action config)]
        [(retrieve) (rlm2-exec-retrieve state action)]
        [(peek)     (rlm2-exec-peek state action)]
        [(grep)     (rlm2-exec-grep state action)]
        [(slice)    (rlm2-exec-slice state action)]
        [(recall-step) (rlm2-exec-recall-step state action)]
        [(submit)   (rlm2-exec-submit state action config)]
        [(think)    (rlm2-exec-think state action)]
        [(plan!)    (rlm2-exec-plan! state action)]
        [(map-chunks) (rlm2-exec-map-chunks state action config)]
        [(begin)    (rlm2-exec-begin state action config depth)]
        [else
         (list (make-rlm2-observation type #f
                 (format "Unknown action type: ~a" type) #f)
               state 1)]))))

;;; --- Individual Action Handlers ---

(define (rlm2-exec-search state action)
  (let ([query (rlm2-search-query action)])
    ;; Validate: query must be a string to prevent code injection
    (if (not (string? query))
        (list (make-rlm2-observation 'search query
                (format "Search query must be a string, got ~a" (if (pair? query) "list" "non-string"))
                #f)
              state 1)
        (begin
          (rlm2-ensure-lattice-meta!)
          (let ([out (open-output-string)])
            (parameterize ([current-output-port out])
              (lf query))
            (let ([result (get-output-string out)])
              (list (make-rlm2-observation 'search query
                      (if (string=? result "") "No matches found." result)
                      #t)
                    state 1)))))))

(define (rlm2-exec-inspect state action)
  (let ([skill (rlm2-inspect-skill action)])
    (rlm2-ensure-lattice-meta!)
    (let ([out (open-output-string)])
      (parameterize ([current-output-port out])
        (li skill))
      (let ([result (get-output-string out)])
        (list (make-rlm2-observation 'inspect skill
                (if (string=? result "")
                    (format "Skill '~a' not found." skill)
                    result)
                (not (string=? result "")))
              state 1)))))

(define (rlm2-exec-exports state action)
  (let ([skill (rlm2-exports-skill action)])
    (rlm2-ensure-lattice-meta!)
    (let ([out (open-output-string)])
      (parameterize ([current-output-port out])
        (le skill))
      (let ([result (get-output-string out)])
        (list (make-rlm2-observation 'exports skill
                (if (string=? result "")
                    (format "Skill '~a' not found." skill)
                    result)
                (not (string=? result "")))
              state 1)))))

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
                (format "Failed to load ~a: ~a" mod (fold-result-error result)) #f)
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
                                  (format "Error: ~a" (fold-result-error result))
                                  (+ total-fuel 5))))))]
         [ok? (eq? 'ok (car final))]
         [result-val (cadr final)]
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
        (let* ([value (fold-result-value result)]
               [env* (car (rlm-env-store! env key value 'sexpr))]
               [state* (rlm2-state-with-env state env*)])
          (list (make-rlm2-observation 'store key
                  (format "Stored ~a" key) #t)
                state* 1))
        (list (make-rlm2-observation 'store key
                (format "Failed to eval expression: ~a"
                        (fold-result-error result)) #f)
              state 1))))

(define (rlm2-exec-retrieve state action)
  (let* ([key (rlm2-retrieve-key action)]
         [env (rlm2-state-env state)]
         [entry (rlm-env-get env key)])
    (if (not entry)
        (list (make-rlm2-observation 'retrieve key
                (format "Key '~a' not found" key) #f)
              state 1)
        (let ([tag (cadr entry)] [size (caddr entry)])
          (if (eq? tag 'chunks)
              ;; Chunked — advise to use peek/grep
              (list (make-rlm2-observation 'retrieve key
                      (format "Value '~a' is chunked (~a chars). Use (peek ~a n) or (grep ~a pattern k)."
                              key size key key)
                      #t)
                    state 1)
              ;; Regular — fetch from CAS
              (let ([value (rlm-env-fetch env key)])
                (list (make-rlm2-observation 'retrieve key
                        (if value (format "~s" value) "CAS fetch failed")
                        (if value #t #f))
                      state 1)))))))

(define (rlm2-exec-peek state action)
  (let* ([key (rlm2-peek-key action)]
         [n (rlm2-peek-n action)]
         [result (rlm-env-peek (rlm2-state-env state) key n)])
    (list (make-rlm2-observation 'peek key
            (or result (format "Key '~a' not found" key))
            (if result #t #f))
          state 1)))

(define (rlm2-exec-grep state action)
  (let* ([key (rlm2-grep-key action)]
         [pattern (rlm2-grep-pattern action)]
         [k (rlm2-grep-k action)]
         [results (rlm-env-grep (rlm2-state-env state) key pattern k)])
    (list (make-rlm2-observation 'grep key
            (if results
                (rlm2-format-grep-results results)
                (format "Key '~a' not found or not chunked" key))
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
        (let* ([hash (cdr entry)]
               [blk (fetch-persistent (hex->hash hash))])
          (if blk
              (let ([payload (utf8->string (block-payload blk))])
                (list (make-rlm2-observation 'recall-step n payload #t)
                      state 1))
              (list (make-rlm2-observation 'recall-step n
                      (format "Step ~a record not found in CAS" n) #f)
                    state 1)))
        (list (make-rlm2-observation 'recall-step n
                (format "Step ~a not in episodic log" n) #f)
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
                      "Answer rejected by verifier" #f)
                    state 1)))
        ;; Eval failed
        (list (make-rlm2-observation 'submit expr
                (format "Submit eval failed: ~a" (fold-result-error result)) #f)
              state 1))))

(define (rlm2-exec-think state action)
  ;; Think is ephemeral — no state change, no observation stored
  (list (make-rlm2-observation 'think (rlm2-think-text action)
          "noted" #t)
        state 0))

(define (rlm2-exec-plan! state action)
  (let ([items (rlm2-plan!-items action)])
    ;; Validate: items must be a list (ideally of pairs, but tolerate atoms)
    (if (not (list? items))
        (list (make-rlm2-observation 'plan! items
                "plan! argument must be a list of (item . status) pairs" #f)
              state 0)
        ;; Normalize: ensure each item is a pair. Wrap bare atoms as (atom . pending).
        (let ([normalized (map (lambda (item)
                                 (if (pair? item) item (cons item 'pending)))
                               items)])
          (list (make-rlm2-observation 'plan! normalized
                  (format "Plan updated: ~a items" (length normalized)) #t)
                (rlm2-state-with-plan state normalized) 0)))))

(define (rlm2-exec-map-chunks state action config)
  (let* ([key (rlm2-map-chunks-key action)]
         [expr-text (rlm2-map-chunks-expr action)]
         [env (rlm2-state-env state)]
         [entry (rlm-env-get env key)])
    (cond
      [(not entry)
       (list (make-rlm2-observation 'map-chunks key
               (format "Key '~a' not found" key) #f)
             state 1)]
      [(not (eq? (cadr entry) 'chunks))
       (list (make-rlm2-observation 'map-chunks key
               (format "Key '~a' is not chunked — use (eval ...) for small values" key) #f)
             state 1)]
      [(not (string? expr-text))
       (list (make-rlm2-observation 'map-chunks key
               "Expression must be a string (evaluated per chunk with *chunk* bound)" #f)
             state 1)]
      [else
       ;; Fetch manifest and iterate over chunks
       (let ([manifest (rlm-env-fetch env key)])
         (if (not (and manifest (chunk-manifest? manifest)))
             (list (make-rlm2-observation 'map-chunks key
                     "Chunk manifest not found in CAS" #f)
                   state 1)
             (let loop ([hashes (chunk-manifest-hashes manifest)]
                        [results '()]
                        [idx 0])
               (if (null? hashes)
                   ;; All chunks processed — parse results and store as 'map-result
                   (let* ([raw-results (reverse results)]
                          [parsed (map (lambda (x)
                                         (if (string? x)
                                             (guard (ex [else (or (string->number x) x)])
                                               (let ([val (read (open-input-string x))])
                                                 (if (eof-object? val) x val)))
                                             x))
                                       raw-results)]
                          [n-chunks (length parsed)]
                          [env* (car (rlm-env-store! env 'map-result parsed 'sexpr))]
                          [state* (rlm2-state-with-env state env*)])
                     (list (make-rlm2-observation 'map-chunks key
                             (format "Processed ~a chunks. Results stored as 'map-result.\nValues: ~s"
                                     n-chunks parsed)
                             #t)
                           state* (max 1 n-chunks)))
                   ;; Process one chunk
                   (let* ([chunk-blk (fetch-persistent (hex->hash (car hashes)))]
                          [chunk-text (if chunk-blk
                                         (utf8->string (block-payload chunk-blk))
                                         "")]
                          [code (format "(let ([*chunk* ~s]) ~a)" chunk-text expr-text)]
                          [result (fold-ipc-eval code)]
                          [val (if (fold-result-ok? result)
                                   (fold-result-value result)
                                   (format "error:~a" (fold-result-error result)))]
                          ;; Normalize void/empty to "()"
                          [val* (if (and (string? val)
                                         (or (string=? val "#<void>")
                                             (string=? val "")))
                                    "()"
                                    val)])
                     (loop (cdr hashes) (cons val* results) (+ idx 1)))))))])))

(define (rlm2-exec-begin state action config depth)
  ;; Execute children sequentially, stop on first error or submit completion
  (let loop ([children (rlm2-begin-actions action)]
             [state state]
             [observations '()]
             [total-fuel 0])
    (if (null? children)
        ;; All succeeded — return last observation
        (let ([last-obs (if (null? observations)
                            (make-rlm2-observation 'begin #f "empty begin" #t)
                            (car observations))])
          (list last-obs state total-fuel))
        (let* ([child (car children)]
               [result (rlm2-execute-action state child config depth)]
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
                   (+ total-fuel fuel))])))))

;;; ====
;;; Reflection Pass
;;; ====

(define (rlm2-reflect config thought action observation step-num)
  (guard (ex [else
              ;; Fallback: mechanical note
              (rlm2-mechanical-note action observation step-num)])
    (let* ([prompt (rlm2-build-reflection-prompt
                    thought action observation)]
           [messages (list (rlm2-make-msg "user" prompt))]
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
  (let ([type (rlm2-action-type action)]
        [ok? (rlm2-observation-ok? observation)])
    (format "Step ~a: ~a ~a"
            step-num type (if ok? "succeeded" "failed"))))

;;; ====
;;; State Update
;;; ====

(define (rlm2-update-state state action observation note fuel-used)
  ;; If exec already set an rlm2-result (submit succeeded, or submit
  ;; inside begin), preserve it instead of overwriting with observation.
  (let* ([has-result? (rlm2-state-complete? state)]
         [last-result (if has-result?
                         (rlm2-state-last-result state)
                         observation)]
         [s1 (rlm2-state-with-last-result state last-result)]
         [s2 (rlm2-state-add-note s1 note)]
         [s3 (rlm2-state-use-fuel s2 fuel-used)]
         [s4 (rlm2-state-advance-step s3)])
    s4))

;;; ====
;;; Env Reference Expansion (ported from v1)
;;; ====
;;;
;;; Inside (eval ...) and (store ...) expressions, the model may write
;;; (retrieve 'key) to reference env values. We pre-expand these to
;;; their actual values before sending to IPC.

(define (rlm2-expand-env-refs expr env)
  (cond
    ;; (retrieve 'key) -> expanded value
    [(and (pair? expr)
          (eq? (car expr) 'retrieve)
          (pair? (cdr expr))
          (rlm2-literal-arg? (cadr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [val (rlm-env-fetch env key)])
       (if (and val
                (let ([s (format "~a" val)])
                  (<= (string-length s) 2000)))
           (rlm2-quote-if-needed val)
           expr))]
    ;; (peek 'key n) -> expanded preview
    [(and (pair? expr)
          (eq? (car expr) 'peek)
          (>= (length (cdr expr)) 2)
          (rlm2-literal-arg? (cadr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [n (caddr expr)]
            [val (rlm-env-peek env key n)])
       (if val (rlm2-quote-if-needed val) expr))]
    ;; (grep 'key pattern k) -> expanded results
    [(and (pair? expr)
          (eq? (car expr) 'grep)
          (>= (length (cdr expr)) 2)
          (for-all rlm2-literal-arg? (cdr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [pattern (caddr expr)]
            [k (if (>= (length (cdr expr)) 3) (cadddr expr) 5)]
            [results (rlm-env-grep env key pattern k)])
       (if results
           (rlm2-quote-if-needed (map car results))
           expr))]
    ;; Also handle v1-style (rlm-env-get 'key) for compatibility in eval blocks
    [(and (pair? expr)
          (memq (car expr) '(rlm-env-get env-get))
          (pair? (cdr expr))
          (rlm2-literal-arg? (cadr expr)))
     (let* ([key (rlm2-unquote-key (cadr expr))]
            [val (rlm-env-fetch env key)])
       (if (and val
                (let ([s (format "~a" val)])
                  (<= (string-length s) 2000)))
           (rlm2-quote-if-needed val)
           expr))]
    ;; Recurse into sub-expressions
    [(pair? expr)
     (cons (rlm2-expand-env-refs (car expr) env)
           (rlm2-expand-env-refs (cdr expr) env))]
    [else expr]))

(define (rlm2-literal-arg? x)
  (or (number? x) (string? x) (boolean? x) (char? x)
      (null? x)
      (and (pair? x) (eq? (car x) 'quote))))

(define (rlm2-unquote-key arg)
  (if (and (pair? arg) (eq? (car arg) 'quote))
      (cadr arg)
      arg))

(define (rlm2-quote-if-needed val)
  (if (or (number? val) (string? val) (boolean? val) (char? val))
      val
      (list 'quote val)))

;;; ====
;;; Code Splitting (ported from v1)
;;; ====

(define (rlm2-split-code-exprs code-text)
  (guard (ex [else (list code-text)])
    (let ([port (open-input-string code-text)])
      (let loop ([exprs '()])
        (let skip-ws ()
          (let ([c (peek-char port)])
            (cond
              [(eof-object? c) (void)]
              [(char-whitespace? c) (read-char port) (skip-ws)]
              [(char=? c #\;)
               (let skip-comment ()
                 (let ([c (read-char port)])
                   (unless (or (eof-object? c) (char=? c #\newline))
                     (skip-comment))))
               (skip-ws)]
              [else (void)])))
        (if (eof-object? (peek-char port))
            (reverse exprs)
            (let ([expr (read port)])
              (if (eof-object? expr)
                  (reverse exprs)
                  (loop (cons (format "~s" expr) exprs)))))))))

;;; ====
;;; Grep Results Formatting
;;; ====

(define (rlm2-format-grep-results results)
  (if (null? results)
      "No matches found."
      (let loop ([rs results] [acc '()])
        (if (null? rs)
            (let join ([strs (reverse acc)] [out ""])
              (if (null? strs)
                  out
                  (join (cdr strs)
                        (if (string=? out "")
                            (car strs)
                            (string-append out "\n" (car strs))))))
            (loop (cdr rs) (cons (car (car rs)) acc))))))

;;; ====
;;; CAS Recording
;;; ====

(define (rlm2-record-step! step-num action observation note provider
                           fuel-used prev-step-hash)
  (let* ([payload `((step . ,step-num)
                    (action . ,action)
                    (observation-type . ,(rlm2-observation-action-type observation))
                    (observation-ok . ,(rlm2-observation-ok? observation))
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
                      ;; Parse fell through to think — log the raw output
                      (rlm2-truncate-telemetry raw-text 500)]
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

;;; ====
;;; Worker Prelude (reused from v1)
;;; ====
;;; Loads string utilities into the IPC worker session.
;;; Identical to v1 — the worker is the same.

(define (rlm2-init-worker-prelude!)
  (fold-ipc-eval
    (string-append
      "(begin"
      "  (define (rlm-split-lines s)"
      "    (let loop ([chars (string->list s)] [current '()] [lines '()])"
      "      (cond"
      "        [(null? chars) (reverse (cons (list->string (reverse current)) lines))]"
      "        [(char=? (car chars) #\\newline)"
      "         (loop (cdr chars) '() (cons (list->string (reverse current)) lines))]"
      "        [else (loop (cdr chars) (cons (car chars) current) lines)])))"
      "  (define (rlm-string-contains? s sub)"
      "    (let ([s-len (string-length s)] [sub-len (string-length sub)])"
      "      (if (> sub-len s-len) #f"
      "          (let loop ([i 0])"
      "            (cond [(> (+ i sub-len) s-len) #f]"
      "                  [(string=? (substring s i (+ i sub-len)) sub) #t]"
      "                  [else (loop (+ i 1))])))))"
      "  (define (rlm-extract-after s marker)"
      "    (let ([s-len (string-length s)] [m-len (string-length marker)])"
      "      (let loop ([i 0])"
      "        (cond [(> (+ i m-len) s-len) #f]"
      "              [(string=? (substring s i (+ i m-len)) marker)"
      "               (let ([rest (substring s (+ i m-len) s-len)])"
      "                 (let trim ([j 0])"
      "                   (cond [(>= j (string-length rest)) rest]"
      "                         [(char-whitespace? (string-ref rest j)) (trim (+ j 1))]"
      "                         [else (substring rest j (string-length rest))])))]"
      "              [else (loop (+ i 1))]))))"
      "  (define (rlm-flatten lst)"
      "    (cond [(null? lst) '()]"
      "          [(null? (car lst)) (rlm-flatten (cdr lst))]"
      "          [(pair? (car lst)) (append (rlm-flatten (car lst)) (rlm-flatten (cdr lst)))]"
      "          [else (cons (car lst) (rlm-flatten (cdr lst)))]))"
      "  (define (rlm-deduplicate lst)"
      "    (let loop ([l lst] [seen '()] [acc '()])"
      "      (if (null? l) (reverse acc)"
      "          (if (member (car l) seen)"
      "              (loop (cdr l) seen acc)"
      "              (loop (cdr l) (cons (car l) seen) (cons (car l) acc))))))"
      "  (define string-contains? rlm-string-contains?)"
      "  (define split-lines rlm-split-lines)"
      "  (define flatten rlm-flatten)"
      "  (define deduplicate rlm-deduplicate)"
      "  (define remove-duplicates rlm-deduplicate)"
      "  (define extract-after rlm-extract-after)"
      "  (define first car)"
      "  (define second cadr)"
      "  (define third caddr)"
      "  (define fourth cadddr)"
      "  (define (fifth lst) (car (cddddr lst)))"
      "  (define (rest lst) (cdr lst))"
      "  (define (last lst) (if (null? (cdr lst)) (car lst) (last (cdr lst))))"
      "  (define rest cdr)"
      "  (define (iota n) (let loop ([i (- n 1)] [acc '()]) (if (< i 0) acc (loop (- i 1) (cons i acc)))))"
      "  (define (build-list n f) (map f (iota n)))"
      "  (define (sorted lst pred) (sort pred lst))"
      "  (define (list<? a b)"
      "    (cond [(and (null? a) (null? b)) #f]"
      "          [(null? a) #t]"
      "          [(null? b) #f]"
      "          [(string<? (if (string? (car a)) (car a) (format \"~a\" (car a)))"
      "                     (if (string? (car b)) (car b) (format \"~a\" (car b)))) #t]"
      "          [(string=? (if (string? (car a)) (car a) (format \"~a\" (car a)))"
      "                     (if (string? (car b)) (car b) (format \"~a\" (car b))))"
      "           (list<? (cdr a) (cdr b))]"
      "          [else #f]))"
      "  (define (filter-map f lst)"
      "    (let loop ([xs lst] [acc '()])"
      "      (if (null? xs) (reverse acc)"
      "          (let ([result (f (car xs))])"
      "            (if result"
      "                (loop (cdr xs) (cons result acc))"
      "                (loop (cdr xs) acc))))))"
      "  (define (string-split str delim)"
      "    (let ([dc (if (char? delim) delim (string-ref delim 0))]"
      "          [len (string-length str)])"
      "      (let loop ([i 0] [start 0] [acc '()])"
      "        (cond [(= i len) (reverse (cons (substring str start len) acc))]"
      "              [(char=? (string-ref str i) dc)"
      "               (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]"
      "              [else (loop (+ i 1) start acc)]))))"
      "  (define (string-trim str)"
      "    (let ([len (string-length str)])"
      "      (let ([s (let loop ([i 0])"
      "                 (if (and (< i len) (char-whitespace? (string-ref str i)))"
      "                     (loop (+ i 1)) i))])"
      "        (let ([e (let loop ([i len])"
      "                   (if (and (> i s) (char-whitespace? (string-ref str (- i 1))))"
      "                       (loop (- i 1)) i))])"
      "          (substring str s e)))))"
      "  (define (string-prefix? str prefix)"
      "    (and (>= (string-length str) (string-length prefix))"
      "         (string=? (substring str 0 (string-length prefix)) prefix)))"
      "  (define (string-suffix? str suffix)"
      "    (let ([slen (string-length str)] [xlen (string-length suffix)])"
      "      (and (>= slen xlen)"
      "           (string=? (substring str (- slen xlen) slen) suffix))))"
      "  (define (take lst n)"
      "    (if (or (null? lst) (<= n 0)) '()"
      "        (cons (car lst) (take (cdr lst) (- n 1)))))"
      "  (define (drop lst n)"
      "    (if (or (null? lst) (<= n 0)) lst"
      "        (drop (cdr lst) (- n 1))))"
      "  (define (cartesian-product xs ys)"
      "    (apply append (map (lambda (x) (map (lambda (y) (list x y)) ys)) xs)))"
      "  (define (string-replace str old new)"
      "    (let ([s-len (string-length str)] [o-len (string-length old)])"
      "      (if (= o-len 0) str"
      "          (let loop ([i 0] [acc '()])"
      "            (cond [(> (+ i o-len) s-len)"
      "                   (apply string-append (reverse (cons (substring str i s-len) acc)))]"
      "                  [(string=? (substring str i (+ i o-len)) old)"
      "                   (loop (+ i o-len) (cons new acc))]"
      "                  [else (loop (+ i 1)"
      "                              (cons (string (string-ref str i)) acc))])))))"
      "  (define (string-join lst sep)"
      "    (if (null? lst) \"\""
      "        (let loop ([rest (cdr lst)] [acc (car lst)])"
      "          (if (null? rest) acc"
      "              (loop (cdr rest) (string-append acc sep (car rest)))))))"
      "  (define (string-index str ch)"
      "    (let ([len (string-length str)])"
      "      (let loop ([i 0])"
      "        (cond [(= i len) #f]"
      "              [(char=? (string-ref str i) ch) i]"
      "              [else (loop (+ i 1))]))))"
      "  (define (string-index-of str sub)"
      "    (let ([slen (string-length str)] [sublen (string-length sub)])"
      "      (let loop ([i 0])"
      "        (cond [(> (+ i sublen) slen) #f]"
      "              [(string=? (substring str i (+ i sublen)) sub) i]"
      "              [else (loop (+ i 1))]))))"
      "  (define (substr str start . rest)"
      "    (if (null? rest)"
      "        (substring str start (string-length str))"
      "        (substring str start (car rest))))"
      "  (void))"))
  (void))

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

(define (rlm2-generate-run-id)
  (format "~a-~a"
          (time-second (current-time))
          (random 100000)))

(define (rlm2-current-iso8601)
  (let ([d (current-date)])
    (format "~4d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year d) (date-month d) (date-day d)
            (date-hour d) (date-minute d) (date-second d))))
