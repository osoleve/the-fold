(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @requires rlm2
(require 'rlm2)

(doc 'module 'pipeline/rlm2-hud)
(doc 'description "RLM v2 HUD renderer: pure function from state to structured text. The HUD is the complete context the model reads — no conversation history, just current reality.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(pipeline/rlm2.ss))

;;; ====
;;; HUD Renderer
;;; ====
;;;
;;; render-state : State x Budget -> String
;;;
;;; Renders the state as a structured S-expression string.
;;; Budget controls truncation — notes are truncated first,
;;; then env entries, then plan items. The HUD always includes
;;; task, fuel, step, and last-result (non-negotiable sections).

(doc 'section 'rlm2-hud)

(doc 'type '(-> Rlm2State Nat String))
(doc 'description "Render the state as a HUD string for the model's context. Budget is char limit for the rendered output.")
(define (rlm2-render-state state budget)
  (let* (;; Fixed sections (always included, estimated sizes)
         [task-section (rlm2-render-task state)]
         [fuel-section (rlm2-render-fuel state)]
         [step-section (rlm2-render-step state)]
         [result-section (rlm2-render-last-result state)]
         [fixed-cost (+ (string-length task-section)
                        (string-length fuel-section)
                        (string-length step-section)
                        (string-length result-section)
                        100)]  ; overhead for section wrappers
         [remaining (max 0 (- budget fixed-cost))]
         ;; Variable sections — share remaining budget
         [env-budget (min (quotient remaining 4) 1500)]
         [loaded-budget (min (quotient remaining 8) 500)]
         [plan-budget (min (quotient remaining 12) 400)]
         [journal-budget (min (quotient remaining 16) 300)]  ; tags only
         [notes-budget (max 0 (- remaining env-budget
                                 loaded-budget plan-budget
                                 journal-budget))]
         ;; Render variable sections
         [env-section (rlm2-render-env state env-budget)]
         [loaded-section (rlm2-render-loaded state loaded-budget)]
         [plan-section (rlm2-render-plan state plan-budget)]
         [notes-section (rlm2-render-notes state notes-budget)]
         [journal-section (rlm2-render-journal state journal-budget)])
    (string-append
     "(rlm-state\n"
     task-section "\n"
     env-section "\n"
     loaded-section "\n"
     plan-section "\n"
     notes-section "\n"
     journal-section "\n"
     result-section "\n"
     fuel-section "\n"
     step-section ")")))

;;; ====
;;; Section Renderers
;;; ====

(define (rlm2-render-task state)
  (format "  (task ~s)" (rlm2-state-task state)))

(define (rlm2-render-plan state budget)
  (let ([plan (rlm2-state-plan state)])
    (if (null? plan)
        "  (plan ())"
        (let* ([items (map (lambda (item)
                             (if (pair? item)
                                 (format "     (~a . ~a)" (car item) (cdr item))
                                 (format "     ~a" item)))
                           plan)]
               [body (rlm2-join-lines items)]
               [truncated (rlm2-truncate-to body budget)])
          (string-append "  (plan\n    (" truncated "))")))))

(define (rlm2-render-env state budget)
  (let ([env (rlm2-state-env state)])
    (if (null? env)
        "  (env ())"
        (let* ([items (map (lambda (entry)
                             (format "     (~a ~a ~a)"
                                     (car entry)     ; key
                                     (cadr entry)    ; type
                                     (caddr entry))) ; size
                           env)]
               [body (rlm2-join-lines items)]
               [truncated (rlm2-truncate-to body budget)])
          (string-append "  (env\n    (" truncated "))")))))

(define (rlm2-render-loaded state budget)
  (let ([loaded (rlm2-state-loaded state)])
    (if (null? loaded)
        "  (loaded ())"
        (let* ([syms (map symbol->string loaded)]
               [body (rlm2-join-with-space syms)]
               [truncated (rlm2-truncate-to body budget)])
          (string-append "  (loaded (" truncated "))")))))

(define (rlm2-render-notes state budget)
  (let ([notes (rlm2-state-notes state)])
    (if (null? notes)
        "  (notes ())"
        (let* ([items (map (lambda (n)
                             (let ([truncated (rlm2-truncate-to n (quotient budget (length notes)))])
                               (format "     ~s" truncated)))
                           notes)])
          (string-append "  (notes\n    (" (rlm2-join-lines items) "))")))))

(define (rlm2-render-episodic state budget)
  (let ([episodic (rlm2-state-episodic state)])
    (if (null? episodic)
        "  (episodic ())"
        ;; Show most recent entries that fit within budget
        ;; Entry format: (step-num hash action-type ok?)
        ;; HUD format:   (step-num action-type ok|fail)
        (let loop ([entries (reverse episodic)]  ; most recent first
                   [acc '()]
                   [used 0])
          (cond
            [(null? entries)
             (let ([items (map (lambda (e)
                                 (let ([n (car e)] [act (caddr e)] [ok? (cadddr e)])
                                   (format "     (~a ~a ~a)" n act (if ok? 'ok 'fail))))
                               acc)])
               (string-append "  (episodic\n    (" (rlm2-join-lines items) "))"))]
            [else
             (let* ([entry (car entries)]
                    [n (car entry)] [act (caddr entry)] [ok? (cadddr entry)]
                    [line (format "     (~a ~a ~a)" n act (if ok? 'ok 'fail))]
                    [cost (+ (string-length line) 1)])
               (if (> (+ used cost) budget)
                   ;; Budget exhausted — render what we have with count of omitted
                   (let* ([items (map (lambda (e)
                                        (let ([n (car e)] [act (caddr e)] [ok? (cadddr e)])
                                          (format "     (~a ~a ~a)" n act (if ok? 'ok 'fail))))
                                      acc)]
                          [prefix (if (> (length entries) 0)
                                      (format "     ;; ... ~a earlier steps omitted\n" (length entries))
                                      "")])
                     (string-append "  (episodic\n    (" prefix (rlm2-join-lines items) "))"))
                   (loop (cdr entries) (cons entry acc) (+ used cost))))])))))

(define (rlm2-render-journal state budget)
  (let ([journal (rlm2-state-journal state)])
    (if (null? journal)
        "  (journal ())"
        ;; Show only unique tags — use (recall tag) to read contents
        (let* ([tags (let loop ([entries journal] [seen '()] [acc '()])
                       (if (null? entries) (reverse acc)
                           (let ([tag (caar entries)])
                             (if (memq tag seen)
                                 (loop (cdr entries) seen acc)
                                 (loop (cdr entries) (cons tag seen) (cons tag acc))))))]
               [body (rlm2-join-with-space (map symbol->string tags))]
               [truncated (rlm2-truncate-to body budget)])
          (string-append "  (journal (tags " truncated "))")))))

(define (rlm2-render-last-result state)
  (let ([result (rlm2-state-last-result state)])
    (if (not result)
        "  (last-result #f)"
        (let* ([rendered (format "~s" result)]
               ;; Cap last-result at 2000 chars to avoid context explosion
               [truncated (if (> (string-length rendered) 2000)
                              (string-append (substring rendered 0 1997) "...")
                              rendered)])
          (format "  (last-result ~a)" truncated)))))

(define (rlm2-render-fuel state)
  (format "  (fuel ~a)" (rlm2-state-fuel state)))

(define (rlm2-render-step state)
  (format "  (step ~a)" (rlm2-state-step state)))

;;; ====
;;; System Prompt Builder
;;; ====

(doc 'section 'rlm2-system-prompt)

(doc 'type '(-> String String))
(doc 'description "Build the system prompt for the agent — includes action grammar and semantics")
(define (rlm2-build-system-prompt base-prompt)
  ;; If base-prompt already contains the action grammar, it's a pre-built
  ;; or fitted prompt — return it as-is (idempotent).
  (if (and (string? base-prompt)
           (> (string-length base-prompt) 100)
           (rlm2-string-contains? base-prompt "Action grammar:"))
      base-prompt
      (string-append
   "You are an agent in The Fold. You receive a structured state (HUD) and emit "
   "a single S-expression action. Your output must be ONLY the action — no markdown, "
   "no prose before or after (use (think \"...\") for reasoning).\n\n"
   ;; --- Action Grammar (staged) ---
   (rlm2-full-action-grammar)
   ;; --- Semantics ---
   "Semantics:\n"
   "- (begin ...) stops at the first error or (submit). Later actions do not run.\n"
   "- (think ...) is ephemeral — use journal to record facts you need to keep.\n"
   "- (delegate task input) spawns a sub-agent with its own fuel budget. Use for focused sub-tasks.\n"
   "- (memorize key text) persists across agent runs. (remember query) does BM25 keyword search.\n"
   "- Eval results are auto-stored as step-N-result. Use (store key expr) to name values.\n"
   "- Plan status values: pending, done, blocked, skipped. You own plan status.\n"
   "- (idle) checkpoints your state and pauses execution. You'll resume later with fresh fuel.\n"
   "- (reframe task) changes the current task without losing env, loaded modules, or memory.\n\n"
   "Environment & stored values:\n"
   "- After (store 'entries val), you can use entries as a bare variable in later expressions:\n"
   "    (store 'entries (flatten map-result))\n"
   "    (store 'filtered (filter pred? entries))  ; bare symbol works!\n"
   "  The driver auto-expands bare symbols that match stored keys.\n"
   "- (peek 'key) shows the full value. (peek 'key n) shows first n chars.\n"
   "- Stored values survive as structured data (lists, numbers, strings).\n"
   "  You can use car, cdr, map, filter, etc. on stored values directly.\n\n"
   "Submit:\n"
   "- (submit expr) evaluates expr as Scheme code, then submits the RESULT.\n"
   "- Stored keys are bare Scheme variables. (submit answer) works directly.\n"
   "- Typical pattern: compute and store your answer, then submit it:\n"
   "    (store 'answer (map ...))\n"  ; in one step
   "    (submit answer)              ; in the next step — bare symbol, not (peek 'answer)\n"
   "- Do NOT pass a string to submit. (submit \"(some code)\") submits the string literal, NOT the result.\n"
   "- Submit the TASK ANSWER, not observations about the process. Never submit error descriptions.\n\n"
   "Error recovery:\n"
   "- Errors are normal. If an eval/store/map-chunks fails, read the error, fix the expression, retry.\n"
   "- Common fixes: escape quotes in string expressions, check variable binding order, verify data types.\n"
   "- You have up to 20 steps — use them. Do not give up after one failure.\n\n"
   ;; --- Chunked values ---
   "Chunked values:\n"
   "Large values are automatically chunked for memory efficiency. "
   "If (peek key) says \"value is chunked\", navigate with:\n"
   "  (peek key 500)          — Preview first 500 chars\n"
   "  (grep key \"pattern\" 5)  — Search chunks for pattern, top 5 matches\n"
   "  (slice key 0 3)         — Extract chunks 0, 1, 2 (0-indexed, exclusive end)\n\n"
   ;; --- Map-chunks ---
   "Map-chunks:\n"
   "The expression argument is a STRING, not a bare S-expression. "
   "It is evaluated per chunk with *chunk* bound to the chunk text.\n"
   "  (map-chunks 'input \"(filter (lambda (line) (string-contains? line \\\"RECORD\\\")) (split-lines *chunk*))\")\n"
   "Results are auto-stored as 'map-result.\n\n"
   ;; --- Common pattern ---
   "Common pattern — chain actions in a begin:\n"
   "  (begin\n"
   "    (search \"eigenvalue decomposition\")\n"
   "    (store 'result (map f data)))\n"
   "Do NOT put (think ...) inside (begin ...) — it wastes tokens and risks truncation.\n"
   "Use (think ...) only as a standalone action when you need to reason without acting.\n\n"
   ;; --- Worker prelude ---
   "Pre-loaded utilities (available in eval/store/map-chunks):\n"
   "  Lists: first second third fourth fifth rest last take drop iota build-list\n"
   "         filter-map flatten deduplicate sorted cartesian-product\n"
   "  Strings: split-lines string-contains? string-split string-trim\n"
   "           string-prefix? string-suffix? string-index string-index-of\n"
   "           substr string-join string-replace extract-after\n"
   "  Note: (sorted lst pred) wraps sort. (split-lines s) splits on newlines.\n"
   (if (and (string? base-prompt) (> (string-length base-prompt) 0))
       (string-append "\n" base-prompt)
       ""))))

;;; ====
;;; Action Grammar (full — always rendered in system prompt)
;;; ====
;;; All 24 actions are documented in the system prompt for maximum compatibility.
;;; The staged HUD (rlm2-render-state-staged) can omit groups, but the grammar
;;; is always complete so the parser accepts any action the model emits.

(define (rlm2-full-action-grammar)
  (string-append
   "Action grammar:\n"
   "\n"
   "  Core:\n"
   "  (think text)            — Reasoning (standalone only, do NOT put inside begin)\n"
   "  (eval expr)             — Evaluate Scheme expression\n"
   "  (submit expr)           — Submit final answer (terminates run)\n"
   "  (begin action ...)      — Execute actions sequentially (stops on first error or submit)\n"
   "\n"
   "  Discovery:\n"
   "  (search query)          — Search the lattice by keyword\n"
   "  (inspect skill)         — Get skill description, deps, modules\n"
   "  (exports skill)         — List a skill's exported functions\n"
   "  (load module)           — Load a lattice module into session\n"
   "  (delegate task input)   — Spawn a sub-agent for a focused sub-task\n"
   "\n"
   "  Code intel:\n"
   "  (lookup symbol)         — Type info + description + definition location\n"
   "  (definition symbol)     — File:line where symbol is defined\n"
   "  (symbols query)         — Search symbols by name fragment\n"
   "  (outline file)          — List definitions in a file\n"
   "\n"
   "  Data:\n"
   "  (store key expr)        — Evaluate expr, store result with name\n"
   "  (peek key)              — Show full value of env entry\n"
   "  (peek key n)            — Preview first n chars of env entry\n"
   "  (grep key pattern k)    — Search env entry chunks, top-k results\n"
   "  (slice key start end)   — Extract chunk range [start, end) from env entry\n"
   "  (map-chunks key expr)   — Eval string expr per chunk, store in 'map-result\n"
   "\n"
   "  Memory:\n"
   "  (plan! items)           — Propose/update plan: ((item . status) ...)\n"
   "  (journal tag text)      — Write tagged note to session journal\n"
   "  (recall tag)            — Read all journal entries matching tag\n"
   "  (recall-step n)         — Retrieve full record of step N\n"
   "  (memorize key text)     — Save to persistent memory (survives across runs)\n"
   "  (remember query)        — Search persistent memory by keyword (BM25 ranking)\n"
   "\n"
   "  Lifecycle:\n"
   "  (idle)                  — Voluntarily pause (checkpoint state, resume later with fresh fuel)\n"
   "  (reframe task)          — Change current task without resetting state\n"
   "\n"))

;;; ====
;;; Staged HUD Rendering (state-dependent action visibility)
;;; ====

;;; rlm2-render-state-staged : Rlm2State × Nat → String
;;; Like rlm2-render-state but appends action hints based on current state.
;;; Groups:
;;;   Core: always shown
;;;   Discovery: shown when loaded modules are few
;;;   Data: shown when env is non-empty
;;;   Code intel: shown when loaded modules > 0
;;;   Memory: always shown (compact)
(define (rlm2-render-state-staged state budget)
  (let* ([base (rlm2-render-state state budget)]
         [loaded (rlm2-state-loaded state)]
         [env (rlm2-state-env state)]
         [hints '()]
         ;; Always show core
         [hints (cons "  Core: think, eval, submit, begin" hints)]
         ;; Discovery when few modules loaded
         [hints (if (< (length loaded) 3)
                    (cons "  Discover: search, inspect, exports, load, symbols, delegate" hints)
                    hints)]
         ;; Data when env non-empty
         [hints (if (not (null? env))
                    (cons "  Data: peek, grep, slice, store, map-chunks" hints)
                    hints)]
         ;; Code intel when modules loaded
         [hints (if (> (length loaded) 0)
                    (cons "  Code: lookup, definition, outline" hints)
                    hints)]
         ;; Memory always
         [hints (cons "  Memory: plan!, journal, recall, memorize, remember" hints)]
         ;; Lifecycle always (for continuous agents)
         [hints (cons "  Lifecycle: idle, reframe" hints)]
         [hint-text (let loop ([hs (reverse hints)] [acc ""])
                      (if (null? hs) acc
                          (loop (cdr hs)
                                (string-append acc (car hs) "\n"))))])
    (string-append base "\n(available-actions\n" hint-text ")")))

;;; ====
;;; Reflection Prompt Builder
;;; ====

(doc 'section 'rlm2-reflection-prompt)

(doc 'type '(-> (Maybe String) Any Any String))
(doc 'description "Build the reflection prompt from thought + action + observation")
(define (rlm2-build-reflection-prompt thought action observation)
  (string-append
   "Distill the following step into a single concise note (one sentence, max 120 chars). "
   "The note should capture what was learned or accomplished — not what was done.\n\n"
   (if thought
       (string-append "Reasoning:\n" thought "\n\n")
       "")
   "Action: " (format "~s" action) "\n\n"
   "Result: " (let ([rendered (format "~s" observation)])
                (if (> (string-length rendered) 1000)
                    (string-append (substring rendered 0 997) "...")
                    rendered))
   "\n\nNote:"))

;;; ====
;;; Structured Diagnostics (fold-zy0t)
;;; ====
;;;
;;; Structured S-expression diagnostics replace raw error strings.
;;; These are machine-parseable AND human-readable when rendered.
;;; The format is:
;;;   (error (type <symbol>) (given <any>) (hint <string>) (suggestion <string>))
;;;   (nudge (type <symbol>) (count <nat>) (suggestion <string>))
;;;   (warning (type <symbol>) (detail <string>) (suggestion <string>))
;;;
;;; All builders produce S-expressions. rlm2-format-diagnostic renders
;;; them to the compact text the model actually sees in the HUD.

(doc 'section 'rlm2-diagnostics)

(doc 'type '(-> Symbol (List (Pair Symbol Any)) Rlm2Diagnostic))
(doc 'description "Build a structured error diagnostic. Type is the error category, fields is an alist of (key . value) pairs.")
(define (rlm2-error type . fields)
  (cons 'error (cons (list 'type type) fields)))

(doc 'type '(-> Any Boolean))
(doc 'description "True if x is a structured diagnostic (error, nudge, or warning)")
(define (rlm2-diagnostic? x)
  (and (pair? x)
       (memq (car x) '(error nudge warning))
       #t))

(doc 'type '(-> Symbol Nat String Rlm2Diagnostic))
(doc 'description "Build a think-streak nudge. Constructive, not punitive.")
(define (rlm2-nudge type count suggestion)
  (list 'nudge
        (list 'type type)
        (list 'count count)
        (list 'suggestion suggestion)))

(doc 'type '(-> Symbol String String Rlm2Diagnostic))
(doc 'description "Build a warning diagnostic (e.g. loop detection, fuel status).")
(define (rlm2-warning type detail suggestion)
  (list 'warning
        (list 'type type)
        (list 'detail detail)
        (list 'suggestion suggestion)))

;;; Field extraction from diagnostics
(define (rlm2-diagnostic-type d)
  (let ([entry (assq 'type (cdr d))])
    (and entry (cadr entry))))

(define (rlm2-diagnostic-field d key)
  (let ([entry (assq key (cdr d))])
    (and entry (cadr entry))))

;;; ====
;;; Diagnostic Formatting
;;; ====
;;;
;;; Renders structured diagnostics to compact, readable text for the HUD.
;;; The format is designed for small models: clear, direct, actionable.
;;; Also handles plain strings for backwards compatibility.

(doc 'type '(-> Any String))
(doc 'description "Format a structured diagnostic (or plain string) to readable HUD text. Backwards-compatible: plain strings pass through unchanged.")
(define (rlm2-format-diagnostic d)
  (cond
    [(string? d) d]  ; backwards compat: plain strings pass through
    [(not (pair? d)) (format "~a" d)]
    [(eq? (car d) 'error)
     (let ([type (rlm2-diagnostic-field d 'type)]
           [given (rlm2-diagnostic-field d 'given)]
           [hint (rlm2-diagnostic-field d 'hint)]
           [suggestion (rlm2-diagnostic-field d 'suggestion)]
           [expr (rlm2-diagnostic-field d 'expr)]
           [detail (rlm2-diagnostic-field d 'detail)])
       (string-append
        (format "[~a]" (or type 'error))
        (if given (format " Given: ~a." given) "")
        (if expr (format " Expression: ~s." expr) "")
        (if detail (format " ~a." detail) "")
        (if hint (format " ~a." hint) "")
        (if suggestion (format " Try: ~a" suggestion) "")))]
    [(eq? (car d) 'nudge)
     (let ([type (rlm2-diagnostic-field d 'type)]
           [count (rlm2-diagnostic-field d 'count)]
           [suggestion (rlm2-diagnostic-field d 'suggestion)])
       (string-append
        (format "[~a]" (or type 'nudge))
        (if count (format " Count: ~a." count) "")
        (if suggestion (format " ~a" suggestion) " Consider taking an action.")))]
    [(eq? (car d) 'warning)
     (let ([type (rlm2-diagnostic-field d 'type)]
           [detail (rlm2-diagnostic-field d 'detail)]
           [suggestion (rlm2-diagnostic-field d 'suggestion)])
       (string-append
        (format "[~a]" (or type 'warning))
        (if detail (format " ~a." detail) "")
        (if suggestion (format " ~a" suggestion) "")))]
    [else (format "~s" d)]))

;;; ====
;;; Diagnostic Constructors (convenience)
;;; ====

(doc 'type '(-> String (List String) Rlm2Diagnostic))
(doc 'description "Build an unknown-action error with the given name and list of valid actions.")
(define (rlm2-error-unknown-action given valid-actions)
  (rlm2-error 'action-unknown
    (list 'given given)
    (list 'hint (string-append "Valid actions: "
                  (rlm2-diag-join-with ", " valid-actions)))
    (list 'suggestion (rlm2-diag-suggest-closest given valid-actions))))

(doc 'type '(-> Any String Rlm2Diagnostic))
(doc 'description "Build a parse error diagnostic with the problematic input and a description.")
(define (rlm2-error-parse given detail)
  (rlm2-error 'parse-error
    (list 'given (if (string? given)
                     (if (> (string-length given) 200)
                         (string-append (substring given 0 197) "...")
                         given)
                     (let ([s (format "~s" given)])
                       (if (> (string-length s) 200)
                           (string-append (substring s 0 197) "...")
                           s))))
    (list 'detail detail)
    (list 'suggestion "Emit a single S-expression action, e.g. (search \"query\") or (eval (+ 1 2))")))

(doc 'type '(-> Any String Rlm2Diagnostic))
(doc 'description "Build an eval error diagnostic with the expression and error message.")
(define (rlm2-error-eval expr error-msg)
  (rlm2-error 'eval-error
    (list 'expr expr)
    (list 'detail error-msg)
    (list 'suggestion "Check syntax, variable bindings, and data types. Then retry.")))

(doc 'type '(-> Symbol String Rlm2Diagnostic))
(doc 'description "Build a require/load error diagnostic with module name and error.")
(define (rlm2-error-require module-name error-msg)
  (rlm2-error 'require-error
    (list 'given (format "~a" module-name))
    (list 'detail error-msg)
    (list 'suggestion (format "Use (search \"~a\") to find the correct module name." module-name))))

(doc 'type '(-> Symbol Rlm2Diagnostic))
(doc 'description "Build a key-not-found error for env operations.")
(define (rlm2-error-key-not-found key)
  (rlm2-error 'key-not-found
    (list 'given (format "~a" key))
    (list 'hint "Use (peek 'key) only for keys shown in the env section of your HUD")
    (list 'suggestion "Check (env ...) in your current state for available keys.")))

(doc 'type '(-> String Any String Rlm2Diagnostic))
(doc 'description "Build a type validation error.")
(define (rlm2-error-type-mismatch expected given context)
  (rlm2-error 'type-mismatch
    (list 'given (format "~a" given))
    (list 'detail (format "Expected ~a in ~a" expected context))
    (list 'suggestion (format "Provide a ~a value." expected))))

(doc 'type '(-> Nat Nat Rlm2Diagnostic))
(doc 'description "Build a fuel/budget status warning. Framed as budget info, not scarcity anxiety.")
(define (rlm2-warning-fuel remaining total)
  (let ([pct (if (> total 0) (quotient (* remaining 100) total) 0)])
    (rlm2-warning 'budget-status
      (format "~a/~a fuel remaining (~a%)" remaining total pct)
      (if (< pct 20)
          "Budget is low. Focus on your best approach and submit."
          "Sufficient budget remains."))))

(doc 'type '(-> Nat Rlm2Diagnostic))
(doc 'description "Build a think-streak nudge at level 1 (count >= 3).")
(define (rlm2-nudge-think-streak count)
  (rlm2-nudge 'think-streak count
    (if (>= count 5)
        "You have enough context. Compute your answer with (store 'answer expr), then (submit answer)."
        "You have enough context to act. Try: search, eval, store, or grep.")))

(doc 'type '(-> Rlm2Diagnostic))
(doc 'description "Build a loop-detected warning.")
(define (rlm2-warning-loop)
  (rlm2-warning 'loop-detected
    "Repeated action pattern detected"
    "Try a different approach: new search terms, a different module, or break the problem into smaller steps."))

;;; ====
;;; Closest-match suggestion (simple Levenshtein-free heuristic)
;;; ====
;;; Uses prefix matching and substring containment for speed.
;;; Good enough for action names (short strings).

(define (rlm2-diag-suggest-closest given candidates)
  (let* ([given-lower (string-downcase (if (string? given) given (format "~a" given)))]
         [matches (filter
                    (lambda (c)
                      (let ([cl (string-downcase c)])
                        (or (rlm2-diag-prefix? given-lower cl)
                            (rlm2-diag-prefix? cl given-lower)
                            (rlm2-diag-substring? cl given-lower)
                            (rlm2-diag-substring? given-lower cl))))
                    candidates)])
    (if (null? matches)
        "Check the action grammar above."
        (format "(~a ...)" (car matches)))))

(define (rlm2-diag-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

(define (rlm2-diag-substring? needle haystack)
  (let ([nlen (string-length needle)]
        [hlen (string-length haystack)])
    (and (<= nlen hlen)
         (let loop ([i 0])
           (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))])))))

(define (rlm2-diag-join-with sep items)
  (if (null? items) ""
      (let loop ([rest (cdr items)] [acc (car items)])
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; string-downcase: use Chez Scheme built-in (R6RS)

;;; ====
;;; String Utilities (local, no external deps)
;;; ====

;;; Simple substring containment check.
(define (rlm2-string-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (and (<= nlen hlen)
         (let loop ([i 0])
           (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))])))))

(define (rlm2-join-lines items)
  (if (null? items)
      ""
      (let loop ([rest (cdr items)] [acc (car items)])
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc "\n" (car rest)))))))

(define (rlm2-join-with-space items)
  (if (null? items)
      ""
      (let loop ([rest (cdr items)] [acc (car items)])
        (if (null? rest)
            acc
            (loop (cdr rest) (string-append acc " " (car rest)))))))

(define (rlm2-truncate-to text max-chars)
  (if (<= (string-length text) max-chars)
      text
      ;; Find last newline before budget to avoid breaking S-expressions mid-token
      (let* ([limit (max 0 (- max-chars 20))]  ; reserve room for marker
             [cut-point
              (let loop ([i (min limit (- (string-length text) 1))])
                (cond
                  [(< i 0) limit]  ; no newline found, fall back to hard cut
                  [(char=? (string-ref text i) #\newline) i]
                  [else (loop (- i 1))]))])
        (string-append (substring text 0 cut-point) "\n     ... [truncated]"))))
