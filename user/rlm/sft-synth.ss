;;; user/rlm/sft-synth.ss -- Synthetic SFT Training Data Generator
;;;
;;; Generates golden training trajectories for Nemotron SFT.
;;; Each scenario defines a task + ideal action sequence.
;;; The generator reconstructs HUD state at each step and produces
;;; JSONL in OpenAI chat format.
;;;
;;; Run: scheme --script user/rlm/sft-synth.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "boundary/io/json.ss")

;;; ====
;;; Configuration
;;; ====

(define *synth-output-path* "user/rlm/sft-training-data.jsonl")
(define *synth-context-budget* 8000)

;;; ====
;;; Base Prompts (task-domain specific, grammar appended by rlm2-build-system-prompt)
;;; ====

(define *prompt-aggregation*
  (string-append
    "You are a Fold/Scheme agent. Your task is to find and aggregate "
    "specific data from a large document.\n\n"
    "The document is stored in your environment under 'input'. It contains "
    "structured RECORD entries scattered among other text. Each record has:\n"
    "  RECORD NNNN | type: <type> | region: <region> | value: <number>\n\n"
    "IMPORTANT: grep only returns top-k matches, NOT all matches. "
    "For aggregation tasks, you MUST use map-chunks to process every chunk.\n\n"
    "Strategy:\n"
    "1. (peek 'input 500) to understand structure\n"
    "2. (map-chunks 'input \"(expression using *chunk*)\") to process ALL chunks.\n"
    "   This runs the expression for each chunk with *chunk* bound to its text.\n"
    "   Results are auto-stored as 'map-result.\n"
    "3. (store 'total (apply + (retrieve 'map-result))) to sum results\n"
    "4. (submit (retrieve 'total)) to report the answer\n\n"
    "Example map-chunks expression for summing matching values:\n"
    "  (map-chunks 'input\n"
    "    \"(let ([lines (split-lines *chunk*)])\n"
    "       (apply + (map (lambda (line)\n"
    "                       (if (and (string-contains? line \\\"type: alpha\\\")\n"
    "                                (string-contains? line \\\"region: north\\\"))\n"
    "                           (let ([v (extract-after line \\\"value: \\\")])\n"
    "                             (if v (string->number v) 0))\n"
    "                           0))\n"
    "                     lines)))\")\n\n"
    "Available string utilities (already loaded):\n"
    "  (split-lines s)            -- split string into list of lines\n"
    "  (string-contains? s sub)   -- check if s contains sub\n"
    "  (extract-after s marker)   -- get text after marker in s\n\n"
    "Be precise. Return only the numeric total.\n"))

(define *prompt-text-analysis*
  (string-append
    "You are a Fold/Scheme agent. Your task is to analyze text data.\n\n"
    "The document is stored in your environment under 'input'.\n\n"
    "Strategy:\n"
    "1. (peek 'input 500) to understand the document structure\n"
    "2. Use map-chunks to process all data, or grep for targeted search\n"
    "3. Store intermediate results with (store 'key expr)\n"
    "4. (submit result) to report the final answer\n\n"
    "Available string utilities (already loaded):\n"
    "  (split-lines s)            -- split string into list of lines\n"
    "  (string-contains? s sub)   -- check if s contains sub\n"
    "  (extract-after s marker)   -- get text after marker in s\n"
    "  (string-index-of s sub)    -- find index of substring\n\n"
    "Be precise.\n"))

(define *prompt-lattice*
  (string-append
    "You are a Fold/Scheme agent. Your task requires mathematical computation.\n\n"
    "Use the lattice to find and load appropriate tools:\n"
    "1. (search \"keyword\") to find relevant skills\n"
    "2. (inspect 'skill-name) to learn about a skill\n"
    "3. (exports 'skill-name) to see available functions\n"
    "4. (load 'module) to load a module\n"
    "5. (eval expr) to compute\n"
    "6. (submit result) to report the answer\n"))

;;; ====
;;; Scenario Data
;;; ====
;;;
;;; Each scenario: (label task base-prompt env-spec fuel steps)
;;; Each step:     (action obs-type obs-ok obs-value)

(define (make-scenario label task base-prompt env-spec fuel steps)
  `((label . ,label)
    (task . ,task)
    (base-prompt . ,base-prompt)
    (env-spec . ,env-spec)
    (fuel . ,fuel)
    (steps . ,steps)))

(define (make-step action obs-type obs-ok obs-value)
  `((action . ,action)
    (obs-type . ,obs-type)
    (obs-ok . ,obs-ok)
    (obs-value . ,obs-value)))

;; ---- Domain 1: Record Aggregation (OOLONG-style) ----

(define *scenarios*
  (list

    ;; 1. One-shot begin (mirrors real Qwen3 behavior)
    (make-scenario "synth-agg-oneshot"
      "Find the total sum of all 'value' fields from records where type is 'alpha' AND region is 'north'. Report only the numeric total."
      *prompt-aggregation*
      '((input chunks 55000) (task text 130))
      20000
      (list
        (make-step
          '(begin
             (map-chunks 'input
               "(let ([lines (split-lines *chunk*)])\n       (apply + (map (lambda (line)\n                       (if (and (string-contains? line \"type: alpha\")\n                                (string-contains? line \"region: north\"))\n                           (let ([v (extract-after line \"value: \")])\n                             (if v (string->number v) 0))\n                           0))\n                     lines)))")
             (store 'total (apply + (retrieve 'map-result)))
             (submit (retrieve 'total)))
          'submit #t "Answer accepted: 403")))

    ;; 2. Step-by-step aggregation (teaches multi-step pattern)
    (make-scenario "synth-agg-stepwise"
      "Find the total sum of all 'value' fields from records where type is 'beta' AND region is 'south'. Report only the numeric total."
      *prompt-aggregation*
      '((input chunks 55000) (task text 130))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: south | value: 18\nNoise line: lorem ipsum dolor sit amet\nRECORD 0003 | type: gamma | region: east | value: 55\nRECORD 0004 | type: beta | region: south | value: 31\n...")
        (make-step
          '(plan! (("Scan input format" . done)
                   ("Map all chunks to extract beta-south values" . pending)
                   ("Sum the results" . pending)
                   ("Submit total" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n       (apply + (map (lambda (line)\n                       (if (and (string-contains? line \"type: beta\")\n                                (string-contains? line \"region: south\"))\n                           (let ([v (extract-after line \"value: \")])\n                             (if v (string->number v) 0))\n                           0))\n                     lines)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result")
        (make-step
          '(store 'total (apply + (retrieve 'map-result)))
          'store #t "Stored: 728")
        (make-step
          '(submit (retrieve 'total))
          'submit #t "Answer accepted: 728")))

    ;; 3. Count records matching criteria
    (make-scenario "synth-agg-count"
      "Count the total number of records where type is 'gamma'. Report the count."
      *prompt-aggregation*
      '((input chunks 110000) (task text 65))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: gamma | region: west | value: 19\nRECORD 0003 | type: beta | region: south | value: 55\n...")
        (make-step
          '(map-chunks 'input
            "(length (filter (lambda (line) (string-contains? line \"type: gamma\")) (split-lines *chunk*)))")
          'map-chunks #t "Processed 110 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'count (apply + (retrieve 'map-result)))
             (submit (retrieve 'count)))
          'submit #t "Answer accepted: 47")))

    ;; 4. Maximum value
    (make-scenario "synth-agg-max"
      "Find the maximum 'value' field across all records where type is 'alpha'. Report only the number."
      *prompt-aggregation*
      '((input chunks 55000) (task text 80))
      20000
      (list
        (make-step
          '(begin
             (think "Need to find the max across all chunks. map-chunks returns per-chunk results, then I take max of those.")
             (peek 'input 500))
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: south | value: 18\n...")
        (make-step
          '(map-chunks 'input
            "(let ([vals (filter number? (map (lambda (line)\n        (if (string-contains? line \"type: alpha\")\n            (let ([v (extract-after line \"value: \")])\n              (if v (string->number v) #f))\n            #f))\n      (split-lines *chunk*)))])\n  (if (null? vals) 0 (apply max vals)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result")
        (make-step
          '(store 'max-val (apply max (retrieve 'map-result)))
          'store #t "Stored: 98")
        (make-step
          '(submit (retrieve 'max-val))
          'submit #t "Answer accepted: 98")))

    ;; 5. Two-pass aggregation (count + sum for average)
    (make-scenario "synth-agg-average"
      "Find the average 'value' for all records where region is 'east'. Report the average as a decimal number."
      *prompt-aggregation*
      '((input chunks 55000) (task text 85))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: east | value: 18\nRECORD 0003 | type: gamma | region: east | value: 55\n...")
        (make-step
          '(think "I need both the sum and count of east-region values to compute an average. I can get both in one map-chunks pass by returning a pair (sum . count) per chunk.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let ([vals (filter number? (map (lambda (line)\n        (if (string-contains? line \"region: east\")\n            (let ([v (extract-after line \"value: \")])\n              (if v (string->number v) #f))\n            #f))\n      (split-lines *chunk*)))])\n  (cons (apply + vals) (length vals)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'total-sum (apply + (map car (retrieve 'map-result))))
             (store 'total-count (apply + (map cdr (retrieve 'map-result)))))
          'store #t "Stored: 312")
        (make-step
          '(submit (/ (retrieve 'total-sum) (retrieve 'total-count)))
          'submit #t "Answer accepted: 26.0")))

    ;; ---- Domain 2: Text Analysis ----

    ;; 6. Grep + count pattern
    (make-scenario "synth-text-grep"
      "Count the total number of lines containing the word 'ERROR' in the input log file."
      *prompt-text-analysis*
      '((input chunks 30000) (task text 70))
      15000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "[2026-02-06 10:23:01] INFO  - Server started on port 8080\n[2026-02-06 10:23:02] ERROR - Failed to connect to database\n[2026-02-06 10:23:03] INFO  - Retrying connection\n[2026-02-06 10:23:04] ERROR - Connection timeout after 30s\n...")
        (make-step
          '(grep 'input "ERROR" 5)
          'grep #t "5 matches (showing top 5):\n[0.95] [2026-02-06 10:23:02] ERROR - Failed to connect to database\n[0.95] [2026-02-06 10:23:04] ERROR - Connection timeout after 30s\n[0.94] [2026-02-06 10:23:07] ERROR - Authentication failed\n...")
        (make-step
          '(think "grep shows top-k matches, not all. I need map-chunks to count every occurrence.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(length (filter (lambda (line) (string-contains? line \"ERROR\")) (split-lines *chunk*)))")
          'map-chunks #t "Processed 30 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'error-count (apply + (retrieve 'map-result)))
             (submit (retrieve 'error-count)))
          'submit #t "Answer accepted: 142")))

    ;; 7. Extract + aggregate with journal
    (make-scenario "synth-text-journal"
      "Find all unique status codes in the HTTP log and count occurrences of each. Report the status code with the highest count."
      *prompt-text-analysis*
      '((input chunks 45000) (task text 100))
      15000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "GET /api/users 200 12ms\nPOST /api/login 401 5ms\nGET /api/data 200 45ms\nGET /api/missing 404 3ms\nPOST /api/upload 500 120ms\n...")
        (make-step
          '(journal observation "Input is HTTP log. Format: METHOD PATH STATUS_CODE DURATION. Need to extract status codes and count each.")
          'journal #t "Recorded to journal: observation")
        (make-step
          '(plan! (("Understand format" . done)
                   ("Count 200s" . pending)
                   ("Count 404s" . pending)
                   ("Count other codes" . pending)
                   ("Compare and submit highest" . pending)))
          'plan #t "Plan updated: 5 items")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n  (let ([c200 (length (filter (lambda (l) (string-contains? l \" 200 \")) lines))]\n        [c404 (length (filter (lambda (l) (string-contains? l \" 404 \")) lines))]\n        [c401 (length (filter (lambda (l) (string-contains? l \" 401 \")) lines))]\n        [c500 (length (filter (lambda (l) (string-contains? l \" 500 \")) lines))])\n    (list c200 c404 c401 c500)))")
          'map-chunks #t "Processed 45 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'counts
               (let ([results (retrieve 'map-result)])
                 (list
                   (cons 200 (apply + (map first results)))
                   (cons 404 (apply + (map second results)))
                   (cons 401 (apply + (map third results)))
                   (cons 500 (apply + (map fourth results))))))
             (submit (car (sorted (retrieve 'counts) (lambda (a b) (> (cdr a) (cdr b)))))))
          'submit #t "Answer accepted: (200 . 3847)")))

    ;; 8. Simple word count
    (make-scenario "synth-text-wordcount"
      "Count the total number of lines in the input document."
      *prompt-text-analysis*
      '((input chunks 20000) (task text 50))
      10000
      (list
        (make-step
          '(map-chunks 'input "(length (split-lines *chunk*))")
          'map-chunks #t "Processed 20 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'line-count (apply + (retrieve 'map-result)))
             (submit (retrieve 'line-count)))
          'submit #t "Answer accepted: 15420")))

    ;; ---- Domain 3: Lattice Discovery ----

    ;; 9. Search + inspect + load + compute
    (make-scenario "synth-lattice-linalg"
      "Compute the dot product of vectors (1 2 3) and (4 5 6)."
      *prompt-lattice*
      '((task text 50))
      10000
      (list
        (make-step
          '(search "vector dot product")
          'search #t "Found 3 results:\n  linalg (score: 0.92) -- Linear algebra: vectors, matrices, decomposition\n  geometry (score: 0.65) -- Shapes, transforms, SDFs\n  physics (score: 0.45) -- Physics simulation")
        (make-step
          '(inspect 'linalg)
          'inspect #t "Skill: linalg\n  Description: Linear algebra - vectors, matrices, decomposition, solvers\n  Modules: linalg/vec, linalg/matrix, linalg/decomp, linalg/solve\n  Dependencies: ()")
        (make-step
          '(begin
             (load 'linalg/vec)
             (eval (vec-dot (vec 1 2 3) (vec 4 5 6))))
          'eval #t "32")
        (make-step
          '(submit 32)
          'submit #t "Answer accepted: 32")))

    ;; 10. Lattice exploration with think
    (make-scenario "synth-lattice-stats"
      "Compute the mean and standard deviation of the dataset (10 20 30 40 50)."
      *prompt-lattice*
      '((task text 70))
      10000
      (list
        (make-step
          '(begin
             (think "I need statistics functions. Let me search the lattice.")
             (search "statistics mean standard deviation"))
          'search #t "Found 2 results:\n  statistics (score: 0.95) -- Regression, GLM, time series, hypothesis testing\n  numeric (score: 0.40) -- Complex numbers, DFT, signal processing")
        (make-step
          '(exports 'statistics)
          'exports #t "Exports from statistics:\n  mean, variance, stddev, median, quantile, covariance, correlation, ...")
        (make-step
          '(begin
             (load 'statistics)
             (store 'mu (mean '(10 20 30 40 50)))
             (store 'sigma (stddev '(10 20 30 40 50))))
          'store #t "Stored: 14.142135623730951")
        (make-step
          '(submit (list (cons 'mean (retrieve 'mu))
                         (cons 'stddev (retrieve 'sigma))))
          'submit #t "Answer accepted: ((mean . 30) (stddev . 14.142135623730951))")))

    ;; ---- Domain 4: Reasoning Patterns ----

    ;; 11. Think + plan + execute
    (make-scenario "synth-reasoning-plan"
      "Find the sum of all 'value' fields from records where type is 'alpha' OR type is 'beta'. Exclude records where region is 'west'."
      *prompt-aggregation*
      '((input chunks 55000) (task text 115))
      20000
      (list
        (make-step
          '(think "This has compound conditions: (type=alpha OR type=beta) AND region!=west. I need to be careful with the filter logic.")
          'think #t #f)
        (make-step
          '(peek 'input 500)
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: west | value: 18\nRECORD 0003 | type: gamma | region: east | value: 55\nRECORD 0004 | type: alpha | region: west | value: 30\n...")
        (make-step
          '(plan! (("Understand filter criteria" . done)
                   ("Map-chunks with OR type filter and NOT west" . pending)
                   ("Sum results" . pending)
                   ("Submit total" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n  (apply + (map (lambda (line)\n    (if (and (or (string-contains? line \"type: alpha\")\n                 (string-contains? line \"type: beta\"))\n             (not (string-contains? line \"region: west\")))\n        (let ([v (extract-after line \"value: \")])\n          (if v (string->number v) 0))\n        0))\n    lines)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'total (apply + (retrieve 'map-result)))
             (submit (retrieve 'total)))
          'submit #t "Answer accepted: 1591")))

    ;; 12. Error recovery pattern
    (make-scenario "synth-reasoning-recover"
      "Sum all numeric values in the input (one number per line, with some non-numeric lines mixed in)."
      *prompt-text-analysis*
      '((input chunks 15000) (task text 85))
      15000
      (list
        (make-step
          '(peek 'input 300)
          'peek #t "42\nhello world\n17\n\n99\nnot a number\n-5\n12.5\n...")
        (make-step
          '(map-chunks 'input
            "(apply + (filter number? (map (lambda (line)\n  (let ([n (string->number line)])\n    (if n n 0)))\n  (split-lines *chunk*))))")
          'map-chunks #t "Processed 15 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'total (apply + (retrieve 'map-result)))
             (submit (retrieve 'total)))
          'submit #t "Answer accepted: 8847.5")))

    ;; 13. Slice-based exploration
    (make-scenario "synth-explore-slice"
      "Find which chunk range contains the HEADER section in the input document, and report the header text."
      *prompt-text-analysis*
      '((input chunks 40000) (task text 90))
      15000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "--- METADATA ---\nCreated: 2026-02-06\nAuthor: system\n\n--- DATA ---\nRECORD 0001 | value: 42\nRECORD 0002 | value: 18\n...")
        (make-step
          '(grep 'input "HEADER" 5)
          'grep #t "2 matches (showing top 2):\n[0.98] --- HEADER ---\n[0.87] Note: see HEADER for column definitions")
        (make-step
          '(think "The HEADER section exists. grep found it but I need the full section. Let me search for it in the early chunks since headers usually come first.")
          'think #t #f)
        (make-step
          '(slice 'input 0 3)
          'slice #t "--- METADATA ---\nCreated: 2026-02-06\nAuthor: system\n\n--- HEADER ---\nColumn 1: type (alpha, beta, gamma)\nColumn 2: region (north, south, east, west)\nColumn 3: value (integer 0-100)\n\n--- DATA ---\nRECORD 0001 | type: alpha | region: north | value: 42\n...")
        (make-step
          '(submit "Column 1: type (alpha, beta, gamma), Column 2: region (north, south, east, west), Column 3: value (integer 0-100)")
          'submit #t "Answer accepted")))

    ;; 14. Large begin with memorize
    (make-scenario "synth-memorize"
      "Find the total sum of all 'value' fields from records where type is 'alpha' AND region is 'north'. Report only the numeric total."
      *prompt-aggregation*
      '((input chunks 55000) (task text 130))
      20000
      (list
        (make-step
          '(begin
             (think "Standard aggregation task. I know the pattern: map-chunks to filter and sum per chunk, then aggregate.")
             (remember "aggregation"))
          'remember #t "No relevant memories found.")
        (make-step
          '(begin
             (map-chunks 'input
               "(let ([lines (split-lines *chunk*)])\n       (apply + (map (lambda (line)\n                       (if (and (string-contains? line \"type: alpha\")\n                                (string-contains? line \"region: north\"))\n                           (let ([v (extract-after line \"value: \")])\n                             (if v (string->number v) 0))\n                           0))\n                     lines)))")
             (store 'total (apply + (retrieve 'map-result))))
          'store #t "Stored: 403")
        (make-step
          '(begin
             (memorize alpha-north-pattern "For type+region filtering: use map-chunks with nested string-contains? checks and extract-after for value")
             (submit (retrieve 'total)))
          'submit #t "Answer accepted: 403")))

    ;; 15. Multi-pass with plan updates
    (make-scenario "synth-multipass"
      "Compare the total values for type 'alpha' vs type 'beta'. Report which type has a higher total and by how much."
      *prompt-aggregation*
      '((input chunks 55000) (task text 100))
      20000
      (list
        (make-step
          '(plan! (("Peek at data format" . pending)
                   ("Sum alpha values" . pending)
                   ("Sum beta values" . pending)
                   ("Compare and report" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(peek 'input 500)
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: south | value: 18\nRECORD 0003 | type: gamma | region: east | value: 55\n...")
        (make-step
          '(plan! (("Peek at data format" . done)
                   ("Sum alpha values" . pending)
                   ("Sum beta values" . pending)
                   ("Compare and report" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(apply + (map (lambda (line)\n  (if (string-contains? line \"type: alpha\")\n      (let ([v (extract-after line \"value: \")])\n        (if v (string->number v) 0))\n      0))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result")
        (make-step
          '(store 'alpha-total (apply + (retrieve 'map-result)))
          'store #t "Stored: 1247")
        (make-step
          '(plan! (("Peek at data format" . done)
                   ("Sum alpha values" . done)
                   ("Sum beta values" . pending)
                   ("Compare and report" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(apply + (map (lambda (line)\n  (if (string-contains? line \"type: beta\")\n      (let ([v (extract-after line \"value: \")])\n        (if v (string->number v) 0))\n      0))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result")
        (make-step
          '(store 'beta-total (apply + (retrieve 'map-result)))
          'store #t "Stored: 983")
        (make-step
          '(begin
             (store 'diff (- (retrieve 'alpha-total) (retrieve 'beta-total)))
             (submit (list 'alpha-higher-by (retrieve 'diff))))
          'submit #t "Answer accepted: (alpha-higher-by 264)")))))

;;; ====
;;; State Reconstruction
;;; ====

(define (synth-unquote-key arg)
  (if (and (pair? arg) (eq? (car arg) 'quote) (pair? (cdr arg)))
      (cadr arg)
      arg))

(define (synth-build-initial-state task env-spec fuel)
  (let ([env (fold-left
               (lambda (env entry)
                 (rlm-env-put env (car entry) "synthetic"
                              (cadr entry) (caddr entry)))
               '()
               env-spec)])
    (make-rlm2-state task '() env '() '() '() '() #f fuel 0)))

(define (synth-env-from-begin env begin-action step)
  ;; Walk sub-actions in a begin block and update env
  (fold-left
    (lambda (env sub)
      (if (not (pair? sub)) env
          (case (car sub)
            [(store)
             (if (>= (length sub) 3)
                 (rlm-env-put env (synth-unquote-key (cadr sub))
                              "computed" 'result 0)
                 env)]
            [(map-chunks)
             (rlm-env-put env 'map-result "computed" 'result 0)]
            [(eval)
             (rlm-env-put env
                          (string->symbol (format "step-~a-result" step))
                          "computed" 'result 0)]
            [else env])))
    env
    (cdr begin-action)))

(define (synth-advance-state state action obs-type obs-ok obs-value)
  (let* ([atype (if (pair? action) (car action) 'unknown)]
         [env (rlm2-state-env state)]
         [step (rlm2-state-step state)]
         ;; Env updates
         [env* (case atype
                 [(store)
                  (if (>= (length action) 3)
                      (rlm-env-put env (synth-unquote-key (cadr action))
                                   "computed" 'result 0)
                      env)]
                 [(eval)
                  (rlm-env-put env (string->symbol (format "step-~a-result" step))
                               "computed" 'result 0)]
                 [(map-chunks)
                  (rlm-env-put env 'map-result "computed" 'result 0)]
                 [(begin)
                  (synth-env-from-begin env action step)]
                 [else env])]
         ;; Plan updates (cadr extracts the items list from (plan! items))
         [plan* (cond
                  [(eq? atype 'plan!)
                   (if (>= (length action) 2) (cadr action) '())]
                  ;; Check for plan! inside begin
                  [(eq? atype 'begin)
                   (let loop ([subs (cdr action)] [p (rlm2-state-plan state)])
                     (cond
                       [(null? subs) p]
                       [(and (pair? (car subs)) (eq? (caar subs) 'plan!))
                        (loop (cdr subs) (if (>= (length (car subs)) 2)
                                             (cadar subs) '()))]
                       [else (loop (cdr subs) p)]))]
                  [else (rlm2-state-plan state)])]
         ;; Loaded updates (unquote module names)
         [loaded* (cond
                    [(eq? atype 'load)
                     (if (>= (length action) 2)
                         (cons (synth-unquote-key (cadr action))
                               (rlm2-state-loaded state))
                         (rlm2-state-loaded state))]
                    [(eq? atype 'begin)
                     (fold-left
                       (lambda (acc sub)
                         (if (and (pair? sub) (eq? (car sub) 'load)
                                  (>= (length sub) 2))
                             (cons (synth-unquote-key (cadr sub)) acc)
                             acc))
                       (rlm2-state-loaded state)
                       (cdr action))]
                    [else (rlm2-state-loaded state)])]
         ;; Notes
         [note-str (format "Step ~a: ~a ~a" step obs-type
                           (if obs-ok "ok" "error"))]
         [notes* (if (eq? atype 'think)
                     (rlm2-state-notes state)
                     (cons note-str (rlm2-state-notes state)))]
         ;; Journal
         [journal* (cond
                     [(and (eq? atype 'journal) (>= (length action) 3))
                      (cons (cons (synth-unquote-key (cadr action))
                                  (caddr action))
                            (rlm2-state-journal state))]
                     [(eq? atype 'begin)
                      (fold-left
                        (lambda (j sub)
                          (if (and (pair? sub) (eq? (car sub) 'journal)
                                   (>= (length sub) 3))
                              (cons (cons (synth-unquote-key (cadr sub))
                                          (caddr sub)) j)
                              j))
                        (rlm2-state-journal state)
                        (cdr action))]
                     [else (rlm2-state-journal state)])]
         ;; Episodic
         [episodic* (cons (cons step "synthetic")
                          (rlm2-state-episodic state))]
         ;; Last result
         [last-result* obs-value])
    (make-rlm2-state
      (rlm2-state-task state)
      plan* env* loaded* notes* episodic* journal*
      last-result*
      (max 0 (- (rlm2-state-fuel state) 100))
      (+ step 1))))

;;; ====
;;; Training Tuple Generation
;;; ====

(define (synth-format-action action)
  (let ([port (open-output-string)])
    (write action port)
    (get-output-string port)))

(define (synth-make-example sys-prompt hud action-str label step-num)
  `((messages . (((role . "system") (content . ,sys-prompt))
                 ((role . "user") (content . ,hud))
                 ((role . "assistant") (content . ,action-str))))
    (source . ,label)
    (step . ,step-num)))

(define (synth-generate-from-scenario scenario)
  (let* ([label (cdr (assq 'label scenario))]
         [task (cdr (assq 'task scenario))]
         [base-prompt (cdr (assq 'base-prompt scenario))]
         [env-spec (cdr (assq 'env-spec scenario))]
         [fuel (cdr (assq 'fuel scenario))]
         [steps (cdr (assq 'steps scenario))]
         [sys-prompt (rlm2-build-system-prompt base-prompt)]
         [initial-state (synth-build-initial-state task env-spec fuel)])
    (display (format "  ~a: " label))
    (let loop ([remaining steps]
               [state initial-state]
               [examples '()])
      (if (null? remaining)
          (let ([result (reverse examples)])
            (display (format "~a examples\n" (length result)))
            result)
          (let* ([step-data (car remaining)]
                 [action (cdr (assq 'action step-data))]
                 [obs-type (cdr (assq 'obs-type step-data))]
                 [obs-ok (cdr (assq 'obs-ok step-data))]
                 [obs-value (cdr (assq 'obs-value step-data))]
                 [hud (rlm2-render-state state *synth-context-budget*)]
                 [action-str (synth-format-action action)]
                 [example (synth-make-example
                            sys-prompt hud action-str
                            label (rlm2-state-step state))]
                 [state* (synth-advance-state
                           state action obs-type obs-ok obs-value)])
            (loop (cdr remaining) state*
                  (cons example examples)))))))

;;; ====
;;; JSONL Writer
;;; ====

(define (synth-write-jsonl! examples output-path)
  (call-with-port
    (open-file-output-port output-path
      (file-options no-fail)
      (buffer-mode block)
      (make-transcoder (utf-8-codec)))
    (lambda (port)
      (for-each
        (lambda (example)
          (put-string port (json->string example))
          (put-string port "\n"))
        examples))))

;;; ====
;;; Driver
;;; ====

(define (synth-generate-all!)
  (display "Synthetic SFT Generator\n")
  (display "=======================\n\n")
  (display (format "Generating from ~a scenarios:\n" (length *scenarios*)))

  (let ([all-examples
          (fold-left
            (lambda (acc scenario)
              (let ([examples
                      (guard (ex [else
                                  (display (format "  ERROR: ~a\n"
                                    (if (message-condition? ex)
                                        (condition-message ex)
                                        ex)))
                                  '()])
                        (synth-generate-from-scenario scenario))])
                (append acc examples)))
            '()
            *scenarios*)])
    (display (format "\nTotal: ~a training examples\n" (length all-examples)))
    (if (null? all-examples)
        (display "No examples generated.\n")
        (begin
          (synth-write-jsonl! all-examples *synth-output-path*)
          (display (format "Written to: ~a\n" *synth-output-path*))))
    all-examples))

;;; ====
;;; Main
;;; ====

(synth-generate-all!)
