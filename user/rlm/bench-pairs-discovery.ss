;;; user/rlm/bench-pairs-discovery.ss — Pairs benchmark with discovery prompts
;;;
;;; Tests whether the model can discover lattice tools (e.g. CLP)
;;; through search, without being told what's available.
;;;
;;; Run: scheme --script user/rlm/bench-pairs-discovery.ss
;;;
;;; Environment:
;;;   RLM_MODEL   — model ID (default: Qwen/Qwen3-Coder-Next-FP8)
;;;   RLM_PORT    — vLLM port (default: 8000)
;;;   RLM_STEPS   — max steps (default: 25)
;;;   RLM_FUEL    — max fuel (default: 60000)
;;;   RLM_PROMPT  — prompt version: v1|v2|v3 (default: v3)

(unless (top-level-bound? 'rlm2-run)
  (load "boundary/pipeline/rlm2-drive.ss"))
;; bench.ss auto-runs when RLM_INTEGRATION is set. We only need its helpers.
(load "user/rlm/bench.ss")

;;; ====
;;; Prompt variants
;;; ====

;;; V1: neutral — "consider whether any existing skill could help"
;;; Result: model never searched lattice. 0/25 steps used (search ...).
(define *prompt-v1*
  (string-append
    "You are a Fold/Scheme agent solving a structured reasoning task.\n\n"
    "The document in 'input' contains ENTRY lines scattered among filler text:\n"
    "  ENTRY | user: UNNN | date: YYYY-MM-DD | category: <cat>\n\n"
    "Dates are ISO format — string comparison works for date ordering.\n\n"
    "You have access to the full Fold lattice — a rich library of verified skills\n"
    "covering algebra, logic, constraints, data structures, and more.\n"
    "Before diving into manual computation, consider whether any existing\n"
    "lattice skill could simplify your approach. Use:\n"
    "  (search \"query\") to find relevant skills\n"
    "  (inspect 'skill) to see what a skill provides\n"
    "  (exports 'skill) to list its functions\n"
    "  (load 'module) to load it into your session\n\n"
    "Large values are chunked. Navigate with:\n"
    "  (peek key n) — preview first n chars\n"
    "  (grep key \"pattern\" k) — search chunks for pattern\n"
    "  (slice key start end) — extract chunk range\n"
    "  (map-chunks key \"expr\") — eval expr per chunk with *chunk* bound\n\n"
    "Think carefully about the structure of the problem before coding.\n"))

;;; V2: directive — search-first protocol
;;; Result: model searched 1x ("constraint") but didn't follow through.
(define *prompt-v2*
  (string-append
    "You are a Fold/Scheme agent solving a structured reasoning task.\n\n"
    "The document in 'input' contains ENTRY lines scattered among filler text:\n"
    "  ENTRY | user: UNNN | date: YYYY-MM-DD | category: <cat>\n\n"
    "Dates are ISO format — string comparison works for date ordering.\n\n"
    "IMPORTANT: You have access to the Fold lattice — hundreds of verified skills\n"
    "covering algebra, logic, constraint solving, data structures, combinatorics,\n"
    "and more. Your FIRST actions should be to search the lattice for skills\n"
    "relevant to the problem structure. Do NOT write manual algorithms when a\n"
    "lattice skill exists that handles the pattern.\n\n"
    "Search-first protocol:\n"
    "  1. (search \"relevant keywords\") — find skills that match the problem\n"
    "  2. (inspect 'skill) — read what a promising skill provides\n"
    "  3. (exports 'skill) — list its available functions\n"
    "  4. (load 'module) — load the skill into your session\n"
    "  5. Use loaded functions to solve the problem\n\n"
    "For this task, think about what mathematical/logical structure underlies\n"
    "finding constrained pairs from sets. Is this a constraint satisfaction\n"
    "problem? A combinatorial enumeration? Search for those concepts.\n\n"
    "Large values are chunked. Navigate with:\n"
    "  (peek key n) — preview first n chars\n"
    "  (grep key \"pattern\" k) — search chunks for pattern\n"
    "  (map-chunks key \"expr\") — eval expr per chunk with *chunk* bound\n\n"))

;;; V3: phased — mandatory search then extract then solve
(define *prompt-v3*
  (string-append
    "You are a Fold/Scheme agent solving a structured reasoning task.\n\n"
    "The document in 'input' contains ENTRY lines scattered among filler text:\n"
    "  ENTRY | user: UNNN | date: YYYY-MM-DD | category: <cat>\n\n"
    "Dates are ISO format — string comparison works for date ordering.\n\n"
    "=== MANDATORY WORKFLOW ===\n\n"
    "Phase 1 — TOOL DISCOVERY (steps 0-3):\n"
    "  Search the lattice for tools that help with this task's structure.\n"
    "  This task involves finding constrained pairs from two filtered sets.\n"
    "  Run: (search \"constraint\"), (search \"combinatorial pairs\"),\n"
    "  then (inspect 'skill-name) and (exports 'skill-name) for promising hits.\n"
    "  Load useful modules with (load 'module-name).\n\n"
    "Phase 2 — DATA EXTRACTION (steps 4-8):\n"
    "  Extract structured data from the input using map-chunks.\n"
    "  Use ONE map-chunks call to extract all ENTRY lines, then ONE eval/store\n"
    "  to parse them into sets. Do NOT re-extract data across multiple steps.\n\n"
    "Phase 3 — SOLVE AND SUBMIT (steps 9+):\n"
    "  Use loaded lattice tools (or eval) to compute the answer from extracted data.\n"
    "  Store the computed result, then submit: (submit (retrieve 'answer)).\n"
    "  Do NOT pass a string to submit — (submit expr) evaluates expr as code.\n\n"
    "CRITICAL: Each step costs budget. Minimize (think ...) — prefer action.\n"
    "Use (begin action1 action2 ...) to chain multiple actions per step.\n\n"
    "Large values are chunked. Navigate with:\n"
    "  (peek key n) — preview first n chars\n"
    "  (grep key \"pattern\" k) — search chunks for pattern\n"
    "  (map-chunks key \"expr\") — eval expr per chunk with *chunk* bound\n\n"))

(define *discovery-system-prompt*
  (let ([v (or (getenv "RLM_PROMPT") "v3")])
    (cond
      [(string=? v "v1") *prompt-v1*]
      [(string=? v "v2") *prompt-v2*]
      [else *prompt-v3*])))

;;; ====
;;; Benchmark runner
;;; ====

(define (run-pairs-discovery)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Coder-Next-FP8")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [max-steps (or (and (getenv "RLM_STEPS")
                             (string->number (getenv "RLM_STEPS")))
                        25)]
         [max-fuel (or (and (getenv "RLM_FUEL")
                            (string->number (getenv "RLM_FUEL")))
                       60000)]
         [provider (rlm-provider-vllm model-id port)]
         ;; Generate the same data as the standard benchmark (seed 42)
         [n-users 15]
         [entries-per-user 3]
         [target-size 30000]
         [entries (generate-pairs-entries n-users entries-per-user)]
         [haystack (build-pairs-haystack entries target-size)]
         [set-a (compute-condition-set entries *condition-a-cat* 'before *cutoff-day*)]
         [set-b (compute-condition-set entries *condition-b-cat* 'after *cutoff-day*)]
         [expected-pairs (compute-pairs set-a set-b)]
         [cutoff-date (day->date-string *cutoff-day*)]
         [task (format "Find all pairs (A, B) where A < B (alphabetical), user A has at least one 'science' entry dated before ~a, and user B has at least one 'technology' entry dated after ~a. List each pair as (UNNN, UMMM), one per line, sorted ascending."
                       cutoff-date cutoff-date)]
         [chunk-size 2000])

    (display (format "\n=== PAIRS DISCOVERY BENCHMARK ===\n"))
    (display (format "Model: ~a | Port: ~a\n" model-id port))
    (display (format "Prompt: ~a\n" (or (getenv "RLM_PROMPT") "v3")))
    (display (format "Steps: ~a | Fuel: ~a\n" max-steps max-fuel))
    (display (format "Users: ~a (~a entries/user) | Haystack: ~a chars\n"
                     n-users entries-per-user (string-length haystack)))
    (display (format "Set A (science before ~a): ~a users — ~a\n"
                     cutoff-date (length set-a) set-a))
    (display (format "Set B (tech after ~a): ~a users — ~a\n"
                     cutoff-date (length set-b) set-b))
    (display (format "Expected pairs: ~a\n" (length expected-pairs)))
    (for-each (lambda (p) (display (format "  (~a, ~a)\n" (car p) (cdr p))))
              expected-pairs)
    (flush-output-port)

    (display "\n--- Running ---\n")
    (flush-output-port)
    (let-values ([(result ms)
                  (wall-clock-ms
                   (lambda ()
                     (let ([config (append
                                     (make-rlm2-config
                                       provider *discovery-system-prompt*
                                       max-steps max-fuel chunk-size
                                       1 4 8000 #f)
                                     (list '()))])
                       (rlm2-run config task haystack))))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)])
        (let-values ([(f1 prec rec tp pred exp)
                      (compute-f1 expected-pairs output)])
          (display (format "\n=== RESULTS ===\n"))
          (display (format "  Status:    ~a\n" status))
          (display (format "  Time:      ~a ms\n" ms))
          (display (format "  F1:        ~,2f\n" f1))
          (display (format "  Precision: ~,2f\n" prec))
          (display (format "  Recall:    ~,2f\n" rec))
          (display (format "  TP=~a pred=~a exp=~a\n" tp pred exp))
          (display (format "  Output:    ~a\n" (if (> (string-length output) 500)
                                                    (string-append (substring output 0 500) "...")
                                                    output)))
          (display (format "  Trajectory: ~a\n" traj))
          (flush-output-port)

          ;; Save results
          (let ([results-file (format "user/rlm/bench-results-pairs-discovery-~a.sexp"
                                      (rlm2-current-iso8601))])
            (call-with-output-file results-file
              (lambda (port)
                (pretty-print `(pairs-discovery-results
                                 (model ,model-id)
                                 (timestamp ,(rlm2-current-iso8601))
                                 (max-steps ,max-steps)
                                 (max-fuel ,max-fuel)
                                 (prompt ,(or (getenv "RLM_PROMPT") "v3"))
                                 (status ,status)
                                 (time-ms ,ms)
                                 (f1 ,f1)
                                 (precision ,prec)
                                 (recall ,rec)
                                 (tp ,tp)
                                 (predicted ,pred)
                                 (expected ,(length expected-pairs))
                                 (output ,output)
                                 (trajectory ,traj))
                              port)))
            (display (format "\nResults saved to ~a\n" results-file))))))))

;; Always run — this is a standalone script
(run-pairs-discovery)
