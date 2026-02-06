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
;;; Lattice Search (lazy-loaded in main process)
;;; ====
;;; Lattice meta can't load in the IPC worker (nested loads break the frame
;;; protocol). Instead, we load it lazily in the main process and intercept
;;; lf/li/le/require commands during code execution.

(define *lattice-meta-loaded?* #f)

(define (ensure-lattice-meta!)
  (unless *lattice-meta-loaded?*
    (let ([sink (open-output-string)])
      (parameterize ([current-output-port sink])
        (load "lattice/meta/meta.ss")
        (lattice-init!)))
    (set! *lattice-meta-loaded?* #t)))

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
  (rlm-run-at-depth config task input 0 #f))

;;; rlm-run-at-depth : RlmConfig -> String -> Any -> Nat -> (Maybe RlmEnv) -> RlmRunResult
;;; Internal: runs the loop at a given recursion depth.
;;; initial-env: if provided, seeds the environment (used by rlm-spawn to pass sliced context).
(define (rlm-run-at-depth config task input depth initial-env)
  (let* ([run-id (generate-run-id)]
         [session-id (format "rlm-~a" run-id)]
         [started (current-iso8601)]
         ;; Initialize environment, seeding from initial-env if provided
         [env0 (or initial-env (make-rlm-env))]
         ;; Auto-chunk large string inputs for grep/peek support
         [large-input? (and (string? input)
                            (> (string-length input)
                               (rlm-config-chunk-size config)))]
         [env+input (if large-input?
                        (rlm-env-ingest-text! env0 'input input
                                              (rlm-config-chunk-size config))
                        (car (rlm-env-store! env0 'input input 'sexpr)))]
         [input-hex (or (rlm-env-hash env+input 'input) "")]
         [env+task (car (rlm-env-store! env+input 'task task 'text))])
    ;; Parameterize the pipeline session for Fold IPC
    (parameterize ([*pipeline-session* session-id])
      ;; Pre-load string utilities into the IPC worker so map-chunks
      ;; expressions can do real text processing.
      (rlm-init-worker-prelude!)
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
                         prev-step-hash step-num fuel-remaining input-hex)]
          ;; Fuel exhausted
          [(<= fuel-remaining 0)
           (finalize-run config env 'exhausted
                         "Fuel exhausted" started depth
                         prev-step-hash step-num 0 input-hex)]
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
                                prev-step-hash step-num fuel-remaining input-hex)]
                 [else
                  (let* ([response-text (rlm-chat-text response)]
                         ;; Step 3: Parse response
                         [blocks (rlm-parse-response response-text)])
                    ;; Step 4: Execute code blocks.
                    (let-values ([(env* results fuel-used)
                                  (execute-blocks blocks env fuel-remaining
                                                  depth config)])
                      ;; Check for answer: model stores final result via
                      ;; (rlm-env-put! 'answer value). No text-level DONE parsing.
                      (let ([answer (rlm-env-fetch env* 'answer)])
                        (if answer
                            ;; Model stored an answer — finalize
                            (let* ([final-value (format "~a" answer)]
                                   [env** (car (rlm-env-store! env* 'output final-value 'result))])
                              (finalize-run config env** 'completed
                                            final-value started depth
                                            prev-step-hash (+ step-num 1)
                                            (- fuel-remaining fuel-used) input-hex))
                            ;; No answer yet — continue loop
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
                                   ;; Build updated message history (sliding window: keep
                                   ;; system + task msgs, last 4 steps = 12 msgs max)
                                   [all-msgs (append messages
                                                (list (make-msg "user" user-msg)
                                                      (make-msg "assistant" response-text)
                                                      (make-msg "user"
                                                        (format-results results looping?))))]
                                   ;; Keep first 2 messages (system + task) + last 12
                                   [messages* (if (> (length all-msgs) 14)
                                                  (append (list (car all-msgs) (cadr all-msgs))
                                                          (list-tail all-msgs
                                                            (- (length all-msgs) 12)))
                                                  all-msgs)])
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
                                        messages*))))))))])))])))))

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
    "When you have computed the final answer, store it with:\n"
    "  (rlm-env-put! 'answer \"your final answer string\")\n"
    "The run completes automatically when 'answer is set.\n"
    "Do NOT set 'answer until your code has executed successfully\n"
    "and you have verified the result is correct.\n"))

(define (rlm-tool-docs depth config)
  (let ([base-docs
          (string-append
            "Available tools:\n"
            "  (rlm-env-put! 'key value)               — Store a value in context\n"
            "  (rlm-env-get 'key)                       — Retrieve a value from context\n"
            "  (rlm-env-peek 'key n)                    — Preview first n chars of a value\n"
            "  (rlm-env-grep 'key \"pattern\" k)          — Search chunks by keyword, top-k results\n"
            "  (rlm-env-chunk-count 'key)               — Number of chunks in a chunked value\n"
            "  (rlm-env-read-chunk 'key index)          — Read chunk by 0-based index\n"
            "  (rlm-env-map-chunks 'key \"(expr)\")       — Eval expr per chunk (*chunk* bound), store result in 'map-result\n"
            "    NOTE: The expr string is eval'd standalone per chunk. Only *chunk* is bound.\n"
            "    Do NOT reference variables defined outside the string. Example:\n"
            "    (rlm-env-map-chunks 'input \"(filter (lambda (line) (string-contains? line \\\"ENTRY\\\")) (split-lines *chunk*))\")\n"
            "  (rlm-env-keys)                           — List all context keys\n"
            "  Any valid Fold/Scheme expression          — evaluated in sandbox\n"
            "\n"
            "Built-in string & list utilities (already loaded, no require needed):\n"
            "  (string-split str delim)    — Split string by char or 1-char string delimiter\n"
            "  (split-lines str)           — Split string by newlines into list of strings\n"
            "  (string-trim str)           — Strip leading/trailing whitespace\n"
            "  (string-contains? str sub)  — Does str contain substring sub?\n"
            "  (string-index str ch)       — Find index of char ch in str, or #f\n"
            "  (string-index-of str sub)   — Find index of substring sub in str, or #f\n"
            "  (string-replace str old new) — Replace all occurrences of old with new\n"
            "  (string-join lst sep)        — Join list of strings with separator\n"
            "  (string-prefix? str pfx)    — Does str start with pfx?\n"
            "  (string-suffix? str sfx)    — Does str end with sfx?\n"
            "  (first lst) / (second lst) / (third lst) / (fourth lst) / (fifth lst) — List accessors\n"
            "  (rest lst)                 — Alias for cdr\n"
            "  (last lst)                 — Last element of list\n"
            "  (flatten lst)              — Flatten nested list\n"
            "  (iota n)                   — List of integers 0..n-1\n"
            "  (build-list n f)           — Build list by mapping f over 0..n-1\n"
            "  (take lst n) / (drop lst n) — Take/drop first n elements\n"
            "  (cartesian-product xs ys)   — All (x y) pairs\n"
            "  (deduplicate lst)           — Remove duplicates (preserves order)\n"
            "  (filter-map f lst)          — Map f, keep non-#f results\n"
            "  (extract-after str prefix)  — Get text after prefix (e.g. (extract-after \"user: U007\" \"user: \") → \"U007\")\n"
            "  (sort predicate list)         — Sort list (NOTE: predicate comes FIRST)\n"
            "  (sorted lst predicate)       — Sort list (list-first order)\n"
            "  (list<? a b)                  — Lexicographic comparison for sorting list-of-lists/pairs\n"
            "  NOTE: car/cdr/cons/null?/pair?/map/filter/append/apply/length — all standard Scheme\n"
            "  (substring str start end) — extract substring (requires start AND end)\n"
            "  (substr str start) / (substr str start end) — like substring, end defaults to length\n"
            "\n"
            "Lattice discovery (find & load additional tools):\n"
            "  (lf \"query\")        — Search the skill lattice by keyword (e.g. \"constraint\", \"sort\", \"graph\")\n"
            "  (li 'skill-name)    — Describe a skill (what it does, dependencies)\n"
            "  (le 'skill-name)    — List a skill's exported functions\n"
            "  (require 'module)   — Load a module to use its functions\n"
            "  The lattice has 36 skills covering: algebra, geometry, optimization,\n"
            "  constraint logic programming, statistics, graph algorithms, data structures,\n"
            "  linear algebra, information theory, and more.\n"
            "  If the task calls for specialized computation, search for it before\n"
            "  writing it from scratch.\n")])
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
;;; Code blocks are split into individual expressions so each gets proper routing
;;; (intercepted commands vs IPC worker).
(define (execute-blocks blocks env fuel-remaining depth config)
  (let loop ([bs blocks]
             [env env]
             [results '()]
             [fuel-used 0])
    (if (null? bs)
        (values env (reverse results) fuel-used)
        (let ([block (car bs)])
          (cond
            ;; Code block — split into expressions and evaluate each
            [(rlm-block-code? block)
             (let ([exprs (split-code-exprs (rlm-block-content block))])
               (let expr-loop ([es exprs]
                               [env env]
                               [results results]
                               [fuel-used fuel-used])
                 (if (null? es)
                     (loop (cdr bs) env results fuel-used)
                     (let-values ([(env* result fu)
                                   (execute-code (car es)
                                                 env (- fuel-remaining fuel-used)
                                                 depth config)])
                       (expr-loop (cdr es) env*
                                  (cons result results)
                                  (+ fuel-used fu))))))]
            ;; Template block — parse via tp-batch then evaluate
            [(rlm-block-template? block)
             (let-values ([(env* result fu)
                           (execute-template (rlm-block-content block)
                                             env (- fuel-remaining fuel-used)
                                             depth config)])
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
                    [child-result (rlm-run-at-depth child-config sub-prompt "" (+ depth 1) child-env)]
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
      ;; rlm-env-peek: preview a value
      [(rlm-env-peek-call? code-text)
       (let-values ([(key n) (parse-env-peek-call code-text)])
         (let ([result (rlm-env-peek env key n)])
           (if result
               (values env (list 'ok result) 1)
               (values env (list 'error (format "Key '~a' not found" key)) 1))))]
      ;; rlm-env-grep: search chunked values by keyword
      [(rlm-env-grep-call? code-text)
       (let-values ([(key pattern k) (parse-env-grep-call code-text)])
         (let ([results (rlm-env-grep env key pattern k)])
           (if results
               (values env (list 'ok (format-grep-results results)) 1)
               (values env (list 'error (format "Key '~a' not found or not chunked" key)) 1))))]
      ;; rlm-env-chunk-count: number of chunks
      [(rlm-env-chunk-count-call? code-text)
       (let* ([key (parse-env-simple-key code-text)]
              [count (rlm-env-chunk-count env key)])
         (if count
             (values env (list 'ok (format "~a" count)) 1)
             (values env (list 'error (format "Key '~a' not found or not chunked" key)) 1)))]
      ;; rlm-env-read-chunk: read chunk by index
      [(rlm-env-read-chunk-call? code-text)
       (let-values ([(key index) (parse-env-read-chunk-call code-text)])
         (let ([text (rlm-env-read-chunk env key index)])
           (if text
               (values env (list 'ok text) 1)
               (values env (list 'error (format "Chunk ~a of '~a' not found" index key)) 1))))]
      ;; rlm-env-map-chunks: eval expression per chunk, collect results.
      ;; Auto-stores results as 'map-result in the env for downstream use.
      [(rlm-env-map-chunks-call? code-text)
       (let-values ([(key expr-text) (parse-env-map-chunks-call code-text)])
         (let ([result (rlm-env-map-chunks-impl env key expr-text)])
           (if (and (pair? result) (eq? (car result) 'ok))
               (let* ([raw-results (cadr result)]
                      ;; Parse result strings back to S-expressions.
                      ;; IPC worker returns strings like "(\"a\" \"b\")" for lists.
                      ;; Parse them so 'map-result contains actual lists, not
                      ;; string representations that the model has to re-parse.
                      [parsed (map (lambda (x)
                                     (if (string? x)
                                         (guard (ex [else
                                                  (or (string->number x) x)])
                                           (let ([val (read (open-input-string x))])
                                             (if (eof-object? val) x val)))
                                         x))
                                   raw-results)]
                      [n-chunks (length parsed)]
                      ;; Store in env for programmatic access
                      [env* (car (rlm-env-store! env 'map-result parsed 'sexpr))])
                 (values env*
                         (list 'ok (format "map-chunks processed ~a chunks. Results stored as 'map-result.\nValues: ~s"
                                           n-chunks parsed))
                         (max 1 n-chunks)))
               (values env result 1))))]
      ;; rlm-env-keys: list environment contents
      [(rlm-env-keys-call? code-text)
       (values env (list 'ok (format "~s" (rlm-env-keys env))) 1)]
      ;; rlm-env-get: retrieve a value (intercepted for chunked value handling)
      [(rlm-env-get-call? code-text)
       (let* ([key (parse-env-get-key code-text)]
              [entry (rlm-env-get env key)])
         (if (not entry)
             (values env (list 'error (format "Key '~a' not found" key)) 1)
             (let ([tag (cadr entry)]
                   [size (caddr entry)])
               (if (eq? tag 'chunks)
                   ;; Chunked value — tell model to use peek/grep instead
                   (values env
                           (list 'ok (format "Value '~a' is chunked (~a chars). Use (rlm-env-peek '~a n) to preview or (rlm-env-grep '~a \"pattern\" k) to search."
                                             key size key key))
                           1)
                   ;; Regular value — fetch from CAS
                   (let ([result (rlm-env-fetch env key)])
                     (if result
                         (values env (list 'ok (format "~s" result)) 1)
                         (values env (list 'error (format "Failed to fetch '~a' from CAS" key)) 1)))))))]
      ;; Lattice search — intercepted and run in main process
      ;; (lf/li/le/require can't run in IPC worker — nested loads break frame protocol)
      ;; Note: multi-expression blocks are pre-split by execute-blocks, so code-text
      ;; here is a single expression.
      [(lattice-search-call? code-text)
       (let ([result (execute-lattice-search code-text)])
         (values env result 1))]
      ;; Regular code — eval via Fold IPC
      ;; Pre-expand any (rlm-env-get 'key) calls before sending to worker
      [else
       (let* ([expanded-code (expand-env-gets-in-code code-text env)]
              [result (fold-ipc-eval expanded-code)])
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

;;; Parse-based detection: read the first symbol from the expression.
;;; More robust than substring matching — tolerates whitespace variations.
;;; canonicalize-rlm-symbol : Symbol -> Symbol
;;; Normalize short-name aliases to canonical rlm-env-* names.
;;; Models drop the rlm-env- prefix constantly — this catches them.
(define (canonicalize-rlm-symbol sym)
  (case sym
    [(map-chunks env-map-chunks)             'rlm-env-map-chunks]
    [(env-get)                               'rlm-env-get]
    [(env-put! env-store!)                   'rlm-env-put!]
    [(env-peek)                              'rlm-env-peek]
    [(env-grep grep-chunks)                  'rlm-env-grep]
    [(env-keys)                              'rlm-env-keys]
    [(chunk-count env-chunk-count)           'rlm-env-chunk-count]
    [(read-chunk env-read-chunk)             'rlm-env-read-chunk]
    [(spawn)                                 'rlm-spawn]
    [else sym]))

(define (rlm-first-symbol code)
  (guard (ex [else #f])
    (let ([expr (read (open-input-string code))])
      (if (pair? expr) (canonicalize-rlm-symbol (car expr)) #f))))

(define (rlm-spawn-call? code)
  (eq? (rlm-first-symbol code) 'rlm-spawn))

(define (rlm-env-put-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-put!))

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
  ;; Parse (rlm-env-put! 'key value) — evaluate value via IPC.
  ;; Pre-expands (rlm-env-get 'k) calls in the value expression
  ;; so the IPC worker can evaluate the result.
  (guard (ex [else (values #f #f)])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)]
           [raw-value (caddr expr)]
           [expanded (expand-env-gets raw-value env)]
           [value-code (format "~s" expanded)]
           [result (fold-ipc-eval value-code)])
      (if (fold-result-ok? result)
          (values key (fold-result-value result))
          (values #f #f)))))  ; eval failed — don't store garbage

;;; rlm-env-peek/grep/keys/get detection
(define (rlm-env-peek-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-peek))

(define (rlm-env-grep-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-grep))

(define (rlm-env-keys-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-keys))

(define (rlm-env-get-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-get))

(define (parse-env-peek-call code)
  ;; Parse (rlm-env-peek 'key n) -> (values key n)
  (guard (ex [else (values 'input 2000)])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)]
           [n (if (> (length expr) 2) (caddr expr) 2000)])
      (values key n))))

(define (parse-env-grep-call code)
  ;; Parse (rlm-env-grep 'key "pattern" k) -> (values key pattern k)
  (guard (ex [else (values 'input "" 5)])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)]
           [pattern (caddr expr)]
           [k (if (> (length expr) 3) (cadddr expr) 5)])
      (values key pattern k))))

(define (parse-env-get-key code)
  ;; Parse (rlm-env-get 'key) -> key
  (guard (ex [else 'input])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)])
      key)))

(define (rlm-env-chunk-count-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-chunk-count))

(define (rlm-env-read-chunk-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-read-chunk))

(define (rlm-env-map-chunks-call? code)
  (eq? (rlm-first-symbol code) 'rlm-env-map-chunks))

(define (parse-env-simple-key code)
  ;; Parse (some-command 'key) -> key
  (guard (ex [else 'input])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)])
      key)))

(define (parse-env-read-chunk-call code)
  ;; Parse (rlm-env-read-chunk 'key index) -> (values key index)
  (guard (ex [else (values 'input 0)])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)]
           [index (caddr expr)])
      (values key index))))

(define (parse-env-map-chunks-call code)
  ;; Parse (rlm-env-map-chunks 'key "expr") -> (values key expr-text)
  (guard (ex [else (values 'input "*chunk*")])
    (let* ([expr (read (open-input-string code))]
           [key-expr (cadr expr)]
           [key (if (and (pair? key-expr) (eq? (car key-expr) 'quote))
                    (cadr key-expr)
                    key-expr)]
           [expr-text (caddr expr)])
      (values key expr-text))))

;;; rlm-env-map-chunks-impl : RlmEnv -> Symbol -> String -> (ok List) | (error String)
;;; Evaluate a Scheme expression for each chunk with *chunk* bound.
;;; The expression runs in the IPC worker, so it has access to the
;;; worker prelude (string-split-lines, string-contains?, etc.).
(define (rlm-env-map-chunks-impl env key expr-text)
  (let ([manifest (rlm-env-fetch env key)])
    (if (not (and manifest (chunk-manifest? manifest)))
        (list 'error (format "Key '~a' not found or not chunked" key))
        (let loop ([hashes (chunk-manifest-hashes manifest)]
                   [results '()]
                   [idx 0])
          (if (null? hashes)
              (list 'ok (reverse results))
              (let* ([chunk-blk (fetch-persistent (hex->hash (car hashes)))]
                     [chunk-text (if chunk-blk (utf8->string (block-payload chunk-blk)) "")]
                     ;; Build: (let ([*chunk* "..."]) expr)
                     [code (format "(let ([*chunk* ~s]) ~a)" chunk-text expr-text)]
                     [result (fold-ipc-eval code)])
                (let ([val (if (fold-result-ok? result)
                              (fold-result-value result)
                              (format "error:~a" (fold-result-error result)))])
                  ;; Replace void/empty results with "()" — chunks that produce
                  ;; no value (e.g. no entries found) should return empty list,
                  ;; not #<void> which is unparseable and confuses the model.
                  (loop (cdr hashes)
                        (cons (if (and (string? val)
                                       (or (string=? val "#<void>")
                                           (string=? val "")))
                                  "()"
                                  val)
                              results)
                        (+ idx 1)))))))))

;;; format-grep-results : (List (text . score)) -> String
;;; Returns matched lines, one per line, with no formatting headers.
;;; Small models struggle to parse the "--- Match ---" format;
;;; raw lines are directly processable with string-split on "|".
(define (format-grep-results results)
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
;;; Lattice Search Interception
;;; ====
;;; The model can call (lf "query"), (li 'skill), (le 'skill), or (require 'mod)
;;; to discover and load lattice tools. These run in the main process because
;;; nested loads in the IPC worker break the frame protocol.

;;; split-code-exprs : String -> (List String)
;;; Split a code block into individual S-expressions.
;;; Each expression is returned as a string. Comments are attached
;;; to the following expression. Falls back to returning the whole
;;; block as a single entry if parsing fails.
(define (split-code-exprs code-text)
  (guard (ex [else (list code-text)])  ; fallback: treat as single expr
    (let ([port (open-input-string code-text)])
      (let loop ([exprs '()])
        ;; Skip whitespace and comments
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

(define (lattice-search-call? code)
  (let ([sym (rlm-first-symbol code)])
    (and sym (memq sym '(lf li le lfe lfp lfs require)))))

;;; string-trim : String -> String
;;; Remove leading/trailing whitespace.
(define (string-trim s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                      (loop (+ i 1))
                      i))]
         [end (let loop ([i len])
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                    (loop (- i 1))
                    i))])
    (substring s start end)))

(define (execute-lattice-search code)
  (guard (ex [else
              (list 'error (format "Lattice search error: ~a"
                                   (if (message-condition? ex)
                                       (condition-message ex)
                                       "unknown")))])
    (ensure-lattice-meta!)
    (let* ([expr (read (open-input-string code))]
           [cmd (car expr)]
           [out (open-output-string)])
      (case cmd
        [(lf lfe lfp lfs)
         ;; Search commands print results — capture their output
         (parameterize ([current-output-port out])
           (eval expr))
         (let ([result (get-output-string out)])
           (if (string=? result "")
               (list 'ok "No matches found.")
               (list 'ok result)))]
        [(li le)
         ;; Inspect/exports — capture printed output
         (parameterize ([current-output-port out])
           (eval expr))
         (let ([result (get-output-string out)])
           (if (string=? result "")
               (list 'ok (format "Skill '~a' not found." (cadr expr)))
               (list 'ok result)))]
        [(require)
         ;; Module loading — run in the IPC worker so the module is available there
         (let ([result (fold-ipc-eval code)])
           (if (fold-result-ok? result)
               (list 'ok (format "Module loaded: ~a" (cadr expr)))
               (list 'error (format "Failed to load module: ~a" (fold-result-error result)))))]
        [else (list 'error (format "Unknown lattice command: ~a" cmd))]))))

;;; quote-if-needed : Any -> SExpr
;;; Wrap non-self-evaluating values in (quote ...) so they survive IPC eval.
;;; Numbers, strings, booleans, and characters are self-evaluating.
(define (quote-if-needed val)
  (if (or (number? val) (string? val) (boolean? val) (char? val))
      val
      (list 'quote val)))

;;; literal-arg? : SExpr -> Boolean
;;; True if the expression is a compile-time constant (safe to pass to
;;; expansion functions). False for variable references and compound
;;; expressions whose values depend on runtime state.
(define (literal-arg? x)
  (or (number? x) (string? x) (boolean? x) (char? x)
      (null? x)
      (and (pair? x) (eq? (car x) 'quote))))

;;; expand-env-gets : SExpr -> RlmEnv -> SExpr
;;; Recursively replace intercepted RLM calls with their evaluated results.
;;; Only expands calls whose arguments are compile-time literals (numbers,
;;; strings, quoted symbols). Runtime variable references are left intact
;;; so the expression can be evaluated properly in the IPC worker.
;;; Handles: rlm-env-get, rlm-env-peek, rlm-env-grep, rlm-env-chunk-count,
;;;          rlm-env-read-chunk, rlm-env-map-chunks, rlm-env-keys.
(define (expand-env-gets expr env)
  ;; Canonicalize short-name aliases before matching
  (let ([expr (if (and (pair? expr) (symbol? (car expr)))
                  (let ([c (canonicalize-rlm-symbol (car expr))])
                    (if (eq? c (car expr)) expr (cons c (cdr expr))))
                  expr)])
  (cond
    ;; rlm-env-get → substitute value from CAS (cap at 2000 chars to
    ;; prevent context explosion; large values stay as IPC calls)
    [(and (pair? expr)
          (eq? (car expr) 'rlm-env-get)
          (pair? (cdr expr))
          (literal-arg? (cadr expr)))
     (let* ([key-arg (cadr expr)]
            [key (if (and (pair? key-arg) (eq? (car key-arg) 'quote))
                     (cadr key-arg)
                     key-arg)]
            [val (rlm-env-fetch env key)])
       (if (and val
                (let ([s (format "~a" val)])
                  (<= (string-length s) 2000)))
           (quote-if-needed val)
           expr))]
    ;; rlm-env-peek → substitute preview string
    [(and (pair? expr)
          (eq? (car expr) 'rlm-env-peek)
          (pair? (cdr expr))
          (literal-arg? (cadr expr))
          (or (null? (cddr expr)) (literal-arg? (caddr expr))))
     (let* ([args (cdr expr)]
            [key (unquote-key (car args))]
            [n (if (pair? (cdr args)) (cadr args) 2000)]
            [val (rlm-env-peek env key n)])
       (if val (quote-if-needed val) expr))]
    ;; rlm-env-grep → substitute as list of matched text lines.
    ;; In expansion context, return a proper list so model code can
    ;; iterate directly (map, filter, etc.) without string parsing.
    [(and (pair? expr)
          (eq? (car expr) 'rlm-env-grep)
          (>= (length (cdr expr)) 2)
          (for-all literal-arg? (cdr expr)))
     (let* ([args (cdr expr)]
            [key (unquote-key (car args))]
            [pattern (cadr args)]
            [k (if (>= (length args) 3) (caddr args) 5)]
            [results (rlm-env-grep env key pattern k)])
       (if results
           (quote-if-needed (map car results))  ; list of matched text strings
           expr))]
    ;; rlm-env-chunk-count → substitute count
    [(and (pair? expr)
          (eq? (car expr) 'rlm-env-chunk-count)
          (pair? (cdr expr))
          (literal-arg? (cadr expr)))
     (let* ([key (unquote-key (cadr expr))]
            [count (rlm-env-chunk-count env key)])
       (if count count expr))]
    ;; rlm-env-read-chunk → substitute chunk text
    [(and (pair? expr)
          (eq? (car expr) 'rlm-env-read-chunk)
          (>= (length (cdr expr)) 2)
          (for-all literal-arg? (cdr expr)))
     (let* ([key (unquote-key (cadr expr))]
            [idx (caddr expr)]
            [text (rlm-env-read-chunk env key idx)])
       (if text (quote-if-needed text) expr))]
    ;; rlm-env-map-chunks → pre-evaluate, substitute result list
    ;; IPC eval returns strings; parse them back to S-expressions so the
    ;; substituted code can operate on structured data (e.g. apply append).
    ;; If any chunk has errors, DON'T substitute — return original expr
    ;; so the top-level intercept gives a clear error message.
    [(and (pair? expr)
          (eq? (car expr) 'rlm-env-map-chunks)
          (>= (length (cdr expr)) 2)
          (for-all literal-arg? (cdr expr)))
     (let* ([key (unquote-key (cadr expr))]
            [expr-text (caddr expr)]
            [result (rlm-env-map-chunks-impl env key expr-text)])
       (if (and (pair? result) (eq? (car result) 'ok))
           (let* ([raw (cadr result)]
                  ;; Check for error strings before attempting parse
                  [has-errors (exists (lambda (s)
                                       (and (string? s)
                                            (>= (string-length s) 6)
                                            (string=? (substring s 0 6) "error:")))
                                     raw)])
             (if has-errors
                 expr  ; don't substitute — let original code hit top-level intercept
                 (let ([parsed (map (lambda (s)
                                     (guard (ex [else s])
                                       (if (string? s)
                                           (read (open-input-string s))
                                           s)))
                                   raw)])
                   (quote-if-needed parsed))))
           expr))]
    ;; rlm-env-keys → substitute key list
    [(and (pair? expr) (eq? (car expr) 'rlm-env-keys))
     (quote-if-needed (rlm-env-keys env))]
    ;; Recurse into sub-expressions
    [(pair? expr)
     (cons (expand-env-gets (car expr) env)
           (expand-env-gets (cdr expr) env))]
    [else expr])))

;;; unquote-key : SExpr -> Symbol
;;; Extract the symbol from a possibly-quoted argument.
(define (unquote-key arg)
  (if (and (pair? arg) (eq? (car arg) 'quote))
      (cadr arg)
      arg))

;;; expand-env-gets-in-code : String -> RlmEnv -> String
;;; Parse code text, expand rlm-env-get calls, re-serialize.
;;; Falls back to original text if parsing fails.
(define (expand-env-gets-in-code code-text env)
  (guard (ex [else
              code-text])
    (let ([expr (read (open-input-string code-text))])
      (if (eof-object? expr)
          code-text
          (let ([expanded (expand-env-gets expr env)])
            (if (equal? expanded expr)
                code-text  ; no changes, keep original text
                (format "~s" expanded)))))))

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
                      last-step-hash total-steps fuel-remaining input-hex)
  (let* ([finished (current-iso8601)]
         ;; Store output
         [output-pair (rlm-env-store! env 'output output 'result)]
         [env* (car output-pair)]
         [output-hex (cdr output-pair)]
         ;; Build trajectory record
         [config-hex (store-config! config)]
         [traj (make-rlm-trajectory config-hex input-hex output-hex
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
                        (let ([raw (cond
                                     [(and (pair? r) (eq? (car r) 'ok))
                                      (format "Result: ~a" (cadr r))]
                                     [(and (pair? r) (eq? (car r) 'error))
                                      (format "Error: ~a" (cadr r))]
                                     [else (format "~a" r)])])
                          ;; Truncate long results to prevent context explosion
                          (if (> (string-length raw) 500)
                              (string-append (substring raw 0 500)
                                             "... [truncated, use peek/get to inspect]\n")
                              (string-append raw "\n"))))
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

;;; rlm-init-worker-prelude! : -> void
;;; Load string utilities and lattice search into the IPC worker session.
;;; String utils enable map-chunks text processing.
;;; Lattice search enables tool discovery — the model can find and load
;;; any skill from the lattice DAG rather than relying on hand-curated tools.
(define (rlm-init-worker-prelude!)
  ;; Phase 1: String utilities (always needed for text processing)
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
      "  (define (rlm-extract-field line field)"
      "    (let ([marker (string-append field \": \")])"
      "      (let ([after (rlm-extract-after line marker)])"
      "        (if (not after) #f"
      "            (let ([end (let scan ([i 0])"
      "                         (cond [(>= i (string-length after)) i]"
      "                               [(or (char=? (string-ref after i) #\\|)"
      "                                    (char=? (string-ref after i) #\\newline)) i]"
      "                               [else (scan (+ i 1))]))])"
      "              (let trim-end ([j (- end 1)])"
      "                (cond [(< j 0) \"\"]"
      "                      [(char-whitespace? (string-ref after j))"
      "                       (trim-end (- j 1))]"
      "                      [else (substring after 0 (+ j 1))])))))))  "
      ;; Standard-name aliases: models write these names naturally
      "  (define string-contains? rlm-string-contains?)"
      "  (define split-lines rlm-split-lines)"
      "  (define flatten rlm-flatten)"
      "  (define deduplicate rlm-deduplicate)"
      "  (define remove-duplicates rlm-deduplicate)"
      "  (define extract-field rlm-extract-field)"
      "  (define extract-after rlm-extract-after)"
      ;; List accessors models reach for (Python-style naming)
      "  (define first car)"
      "  (define second cadr)"
      "  (define third caddr)"
      "  (define fourth cadddr)"
      "  (define (fifth lst) (car (cddddr lst)))"
      "  (define (rest lst) (cdr lst))"
      "  (define (last lst) (if (null? (cdr lst)) (car lst) (last (cdr lst))))"
      "  (define rest cdr)"
      ;; iota / build-list: models expect (iota n) or (build-list n f)
      "  (define (iota n) (let loop ([i (- n 1)] [acc '()]) (if (< i 0) acc (loop (- i 1) (cons i acc)))))"
      "  (define (build-list n f) (map f (iota n)))"
      ;; sort: models always write (sort list pred), Chez uses (sort pred list)
      ;; sort is (sort predicate list) in Chez — models write (sort list pred).
      ;; Can't redefine built-ins in IPC worker, so provide 'sorted' alias.
      "  (define (sorted lst pred) (sort pred lst))"
      ;; list<? : lexicographic comparison for sorting list-of-lists
      ;; Models use (sort string<? pairs) where pairs are lists, not strings.
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
      ;; filter-map: models use this constantly (SRFI-1, not in Chez base)
      "  (define (filter-map f lst)"
      "    (let loop ([xs lst] [acc '()])"
      "      (if (null? xs) (reverse acc)"
      "          (let ([result (f (car xs))])"
      "            (if result"
      "                (loop (cdr xs) (cons result acc))"
      "                (loop (cdr xs) acc))))))"
      ;; Extra utilities models commonly reach for
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
  ;; Phase 2: Lattice search is handled by command interception in execute-code.
  ;; We can't load lattice meta in the IPC worker (nested loads break frame protocol).
  ;; Instead, lf/li/le/require are intercepted and run in the main process.
  (void))

(define (generate-run-id)
  (format "~a-~a"
          (time-second (current-time))
          (random 100000)))

(define (current-iso8601)
  (let ([d (current-date)])
    (format "~4d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year d) (date-month d) (date-day d)
            (date-hour d) (date-minute d) (date-second d))))
