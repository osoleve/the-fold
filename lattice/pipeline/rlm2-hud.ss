(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
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
         ;; Variable sections — share remaining budget (6 shares)
         [plan-budget (min (quotient remaining 6) 2000)]
         [env-budget (min (quotient remaining 6) 1500)]
         [loaded-budget (min (quotient remaining 12) 500)]
         [journal-budget (min (quotient remaining 6) 1000)]
         [notes-budget (max 0 (- remaining plan-budget env-budget
                                 loaded-budget journal-budget))]
         ;; Render variable sections
         [plan-section (rlm2-render-plan state plan-budget)]
         [env-section (rlm2-render-env state env-budget)]
         [loaded-section (rlm2-render-loaded state loaded-budget)]
         [notes-section (rlm2-render-notes state notes-budget)]
         [episodic-section (rlm2-render-episodic state)]
         [journal-section (rlm2-render-journal state journal-budget)])
    (string-append
     "(rlm-state\n"
     task-section "\n"
     plan-section "\n"
     env-section "\n"
     loaded-section "\n"
     notes-section "\n"
     episodic-section "\n"
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
        (let loop ([ns (reverse notes)]  ; most recent first for truncation
                   [acc '()]
                   [used 0])
          (cond
            [(null? ns)
             (let ([items (map (lambda (n) (format "     ~s" n)) acc)])
               (string-append "  (notes\n    (" (rlm2-join-lines items) "))"))]
            [else
             (let* ([note (car ns)]
                    [cost (+ (string-length note) 10)])  ; overhead for quoting
               (if (> (+ used cost) budget)
                   ;; Budget exceeded — add truncation marker
                   (let* ([remaining-count (length ns)]
                          [marker (format "[~a earlier notes omitted — use recall-step for details]"
                                          remaining-count)]
                          [items (map (lambda (n) (format "     ~s" n))
                                      (append (list marker) acc))])
                     (string-append "  (notes\n    (" (rlm2-join-lines items) "))"))
                   (loop (cdr ns) (cons note acc) (+ used cost))))])))))

(define (rlm2-render-episodic state)
  (let ([episodic (rlm2-state-episodic state)])
    (if (null? episodic)
        "  (episodic ())"
        (let ([items (map (lambda (entry)
                            (format "     (~a . ~s)" (car entry) (cdr entry)))
                          episodic)])
          (string-append "  (episodic\n    (" (rlm2-join-lines items) "))")))))

(define (rlm2-render-journal state budget)
  (let ([journal (rlm2-state-journal state)])
    (if (null? journal)
        "  (journal ())"
        (let loop ([entries (reverse journal)]  ; most recent first
                   [acc '()]
                   [used 0])
          (cond
            [(null? entries)
             (let ([items (map (lambda (e)
                                 (format "     (~a . ~s)" (car e) (cdr e)))
                               acc)])
               (string-append "  (journal\n    (" (rlm2-join-lines items) "))"))]
            [else
             (let* ([entry (car entries)]
                    [cost (+ (string-length (format "~a" (car entry)))
                             (string-length (cdr entry)) 15)])
               (if (> (+ used cost) budget)
                   (let* ([remaining-count (length entries)]
                          [marker (cons 'truncated
                                        (format "~a earlier entries omitted" remaining-count))]
                          [items (map (lambda (e)
                                        (format "     (~a . ~s)" (car e) (cdr e)))
                                      (cons marker acc))])
                     (string-append "  (journal\n    (" (rlm2-join-lines items) "))"))
                   (loop (cdr entries) (cons entry acc) (+ used cost))))])))))

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
  (string-append
   base-prompt
   "\n\n"
   "You are an agent in The Fold. You receive a structured state (HUD) and emit "
   "a single S-expression action. Your output must be ONLY the action — no markdown, "
   "no prose before or after (use (think \"...\") for reasoning).\n\n"
   ;; --- Action Grammar (staged) ---
   (rlm2-full-action-grammar)
   ;; --- Semantics ---
   "Semantics:\n"
   "- (begin ...) stops at the first error or (submit). Later actions do not run.\n"
   "- (think ...) is ephemeral — it is fed to reflection but not stored in notes.\n"
   "- (delegate task input) spawns a sub-agent with its own fuel budget. Use for focused sub-tasks.\n"
   "- (memorize key text) persists across agent runs. (remember query) does BM25 keyword search.\n"
   "- Eval results are auto-stored as step-N-result. Use (store key expr) to name values.\n"
   "- Plan status values: pending, done, blocked, skipped. You own plan status.\n\n"
   "Environment & stored values:\n"
   "- After (store 'entries val), you can use entries as a bare variable in later expressions:\n"
   "    (store 'entries (flatten (retrieve 'map-result)))\n"
   "    (store 'filtered (filter pred? entries))  ; bare symbol works!\n"
   "  The driver auto-expands bare symbols that match stored keys.\n"
   "- (retrieve 'key) also works and is equivalent to using the bare symbol.\n"
   "- Stored values survive as structured data (lists, numbers, strings).\n"
   "  You can use car, cdr, map, filter, etc. on stored values directly.\n"
   "- Do NOT try to call retrieve inside a string passed to eval. Use (store ...) instead.\n\n"
   "Submit:\n"
   "- (submit expr) evaluates expr as Scheme code, then submits the RESULT.\n"
   "- (retrieve 'key) inside submit is pre-expanded, just like in store/eval.\n"
   "- Typical pattern: compute and store your answer, then submit it:\n"
   "    (store 'answer (map ...))\n"  ; in one step
   "    (submit (retrieve 'answer))  ; in the next step\n"
   "- Do NOT pass a string to submit. (submit \"(some code)\") submits the string literal, NOT the result.\n"
   "- Submit the TASK ANSWER, not observations about the process. Never submit error descriptions.\n\n"
   "Error recovery:\n"
   "- Errors are normal. If an eval/store/map-chunks fails, read the error, fix the expression, retry.\n"
   "- Common fixes: escape quotes in string expressions, check variable binding order, verify data types.\n"
   "- You have up to 20 steps — use them. Do not give up after one failure.\n\n"
   ;; --- Chunked values ---
   "Chunked values:\n"
   "Large values are automatically chunked for memory efficiency. "
   "If (retrieve key) says \"value is chunked\", navigate with:\n"
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
   "    (store 'result (map f (retrieve 'data))))\n"
   "Do NOT put (think ...) inside (begin ...) — it wastes tokens and risks truncation.\n"
   "Use (think ...) only as a standalone action when you need to reason without acting.\n\n"
   ;; --- Worker prelude ---
   "Pre-loaded utilities (available in eval/store/map-chunks):\n"
   "  Lists: first second third fourth fifth rest last take drop iota build-list\n"
   "         filter-map flatten deduplicate sorted cartesian-product\n"
   "  Strings: split-lines string-contains? string-split string-trim\n"
   "           string-prefix? string-suffix? string-index string-index-of\n"
   "           substr string-join string-replace extract-after\n"
   "  Note: (sorted lst pred) wraps sort. (split-lines s) splits on newlines.\n"))

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
   "  (retrieve key)          — Fetch full content of env entry\n"
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
                    (cons "  Data: retrieve, peek, grep, slice, store, map-chunks" hints)
                    hints)]
         ;; Code intel when modules loaded
         [hints (if (> (length loaded) 0)
                    (cons "  Code: lookup, definition, outline" hints)
                    hints)]
         ;; Memory always
         [hints (cons "  Memory: plan!, journal, recall, memorize, remember" hints)]
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
;;; String Utilities (local, no external deps)
;;; ====

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
