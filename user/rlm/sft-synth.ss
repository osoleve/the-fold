;;; user/rlm/sft-synth.ss -- Synthetic SFT Training Data Generator
;;;
;;; Generates golden training trajectories for Nemotron SFT.
;;; Each scenario defines a task + ideal action sequence.
;;; The generator reconstructs HUD state at each step and produces
;;; JSONL in OpenAI chat format.
;;;
;;; Generates two kinds of examples:
;;;   1. Act-phase: system + HUD -> action (the agent's move)
;;;   2. Reflect-phase: reflection prompt -> concise note (distillation)
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

(define *prompt-multi-key*
  (string-append
    "You are a Fold/Scheme agent. Your task involves analyzing multiple "
    "data sources stored under different keys in your environment.\n\n"
    "Each key holds a different dataset. Use (peek key n) to preview, "
    "(grep key pattern k) to search, and (map-chunks key expr) to process.\n\n"
    "Strategy:\n"
    "1. Peek at each input to understand the data format\n"
    "2. Process each dataset as needed (map-chunks for exhaustive operations)\n"
    "3. Store intermediate results with (store 'key expr)\n"
    "4. Combine results and (submit answer)\n\n"
    "Available string utilities (already loaded):\n"
    "  (split-lines s)            -- split string into list of lines\n"
    "  (string-contains? s sub)   -- check if s contains sub\n"
    "  (extract-after s marker)   -- get text after marker in s\n\n"))

(define *prompt-iterative*
  (string-append
    "You are a Fold/Scheme agent. Solve the given task.\n\n"
    "If your first attempt is wrong, the verifier will reject it with feedback. "
    "Use (think ...) to reason about what went wrong, then try a different approach.\n\n"
    "Available actions: peek, grep, map-chunks, store, retrieve, eval, think, "
    "plan!, journal, submit.\n\n"
    "Available string utilities (already loaded):\n"
    "  (split-lines s)            -- split string into list of lines\n"
    "  (string-contains? s sub)   -- check if s contains sub\n"
    "  (extract-after s marker)   -- get text after marker in s\n\n"))

(define *prompt-inventory*
  (string-append
    "You are a Fold/Scheme agent. Your task is to count items from an "
    "inventory description.\n\n"
    "The inventory is stored under 'input. It lists items owned by "
    "various people (\"I\", \"My uncle\", \"My father\", etc.).\n\n"
    "Lines follow patterns like:\n"
    "  \"I have 23 reference books (which are bestsellers)\"\n"
    "  \"I also have 50 sofas(here's how I ended up with 50 of them: ...)\"\n"
    "  \"My uncle has 99 sofas\"\n\n"
    "IMPORTANT:\n"
    "- Only count items belonging to \"I\" (lines with \"I have\" or "
    "\"I also have\"), NOT items owned by relatives (lines starting with \"My \")\n"
    "- The number right after \"have\" is the correct count\n"
    "- Narratives like \"(here's how I ended up with N of them: ...)\" "
    "explain history; the stated count before the item name is final\n"
    "- Use map-chunks since the inventory text is large\n\n"
    "Strategy:\n"
    "1. (peek 'input 500) to see the format\n"
    "2. (think ...) to plan your approach and define category word lists\n"
    "3. (map-chunks 'input expr) to extract matching counts per chunk\n"
    "4. (store 'total (apply + (retrieve 'map-result))) to sum\n"
    "5. (submit (retrieve 'total))\n\n"
    "For map-chunks, the expression must:\n"
    "  a) Split *chunk* into lines\n"
    "  b) Filter to only lines containing \"I have \" or \"I also have \" "
    "(skip lines with \"My \")\n"
    "  c) Extract the number (first token after \"have \")\n"
    "  d) Check if the item name matches the target category\n"
    "  e) Sum the matching counts\n\n"
    "Available string utilities (already loaded):\n"
    "  (split-lines s)            -- split string into list of lines\n"
    "  (string-contains? s sub)   -- check if s contains sub\n"
    "  (extract-after s marker)   -- get text after marker in s\n"
    "  (string-index-of s sub)    -- find index of substring\n"
    "  (substr s start end)       -- substring extraction\n\n"))

(define *prompt-competition-math*
  (string-append
    "You are a Fold/Scheme agent. Solve the given math competition problem.\n\n"
    "Strategy:\n"
    "1. (think ...) to decompose the problem and plan your approach\n"
    "2. (eval expr) to compute intermediate results — use Scheme arithmetic\n"
    "3. (store 'key value) to save intermediate results\n"
    "4. (submit answer) to report the final numeric answer\n\n"
    "Available math functions:\n"
    "  +, -, *, /, modulo, remainder, quotient, expt, sqrt, abs\n"
    "  floor, ceiling, round, min, max, gcd, lcm\n"
    "  sin, cos, tan, asin, acos, atan (radians)\n"
    "  iota, build-list, length, apply, map, filter, fold-left\n\n"
    "Tips:\n"
    "- Use exact arithmetic (fractions) when possible: (/ 7 3) stays exact\n"
    "- For combinatorics: write explicit loops or recursive functions\n"
    "- Convert to inexact only at the end if needed: (inexact x)\n"
    "- Report only the numeric answer, not the expression\n\n"))

(define *prompt-ranking*
  (string-append
    "You are a Fold/Scheme agent. Your task is to find top records from data.\n\n"
    "The data is stored under 'input as structured RECORD entries:\n"
    "  RECORD NNNN | type: <type> | region: <region> | value: <number>\n\n"
    "Strategy:\n"
    "1. (peek 'input 500) to understand the data format\n"
    "2. (map-chunks 'input expr) to extract matching records per chunk\n"
    "   Return a list of (id . value) pairs per chunk.\n"
    "3. (store 'all (apply append (retrieve 'map-result))) to merge\n"
    "4. (eval (list-sort pred (retrieve 'all))) to sort\n"
    "5. (submit answer) with the top-k results\n\n"
    "Available utilities:\n"
    "  (split-lines s)            -- split string into list of lines\n"
    "  (string-contains? s sub)   -- check if s contains sub\n"
    "  (extract-after s marker)   -- get text after marker in s\n"
    "  (list-sort pred lst)       -- sort list by predicate\n"
    "  (substr s start end)       -- substring extraction\n\n"))

(define *prompt-multi-hop-qa*
  (string-append
    "You are a Fold/Scheme agent. Answer a question that requires combining "
    "information from multiple parts of a passage.\n\n"
    "The passage is stored under 'input.\n\n"
    "Strategy:\n"
    "1. (peek 'input 500) to read the passage start\n"
    "2. (grep 'input \"keyword\" 10) to find relevant mentions\n"
    "3. (think ...) to reason about connected facts\n"
    "4. (store 'key value) to save intermediate facts\n"
    "5. (submit answer) with the final answer\n\n"
    "Tips:\n"
    "- grep returns matching lines; use multiple greps for different entities\n"
    "- Think step-by-step: identify what facts you need, find each one, combine\n"
    "- For numeric answers, compute with (eval expr) before submitting\n"
    "- For text answers, submit a simple string\n\n"))

;;; ====
;;; Scenario Data
;;; ====
;;;
;;; Each scenario: (label task base-prompt env-spec fuel steps)
;;; Each step:     (action obs-type obs-ok obs-value [obs-note])
;;;
;;; obs-note: optional reflection for non-mechanical actions.
;;; When present, a paired reflect-phase example is generated.

(define (make-scenario label task base-prompt env-spec fuel steps)
  `((label . ,label)
    (task . ,task)
    (base-prompt . ,base-prompt)
    (env-spec . ,env-spec)
    (fuel . ,fuel)
    (steps . ,steps)))

(define (make-step action obs-type obs-ok obs-value . rest)
  (let ([note (if (null? rest) #f (car rest))])
    `((action . ,action)
      (obs-type . ,obs-type)
      (obs-ok . ,obs-ok)
      (obs-value . ,obs-value)
      (obs-note . ,note))))

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
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: south | value: 18\nNoise line: lorem ipsum dolor sit amet\nRECORD 0003 | type: gamma | region: east | value: 55\nRECORD 0004 | type: beta | region: south | value: 31\n..."
          "Input has RECORD format with type/region/value fields separated by pipes.")
        (make-step
          '(plan! (("Scan input format" . done)
                   ("Map all chunks to extract beta-south values" . pending)
                   ("Sum the results" . pending)
                   ("Submit total" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n       (apply + (map (lambda (line)\n                       (if (and (string-contains? line \"type: beta\")\n                                (string-contains? line \"region: south\"))\n                           (let ([v (extract-after line \"value: \")])\n                             (if v (string->number v) 0))\n                           0))\n                     lines)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "map-chunks extracted beta-south values from all 55 chunks. Per-chunk sums ready for aggregation.")
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
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: gamma | region: west | value: 19\nRECORD 0003 | type: beta | region: south | value: 55\n..."
          "Input uses standard RECORD format. Need to count gamma-typed lines.")
        (make-step
          '(map-chunks 'input
            "(length (filter (lambda (line) (string-contains? line \"type: gamma\")) (split-lines *chunk*)))")
          'map-chunks #t "Processed 110 chunks, results stored as 'map-result"
          "Per-chunk gamma counts ready. 110 chunks processed.")
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
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: south | value: 18\n..."
          "Input confirmed. Per-chunk max strategy is correct for finding global max.")
        (make-step
          '(map-chunks 'input
            "(let ([vals (filter number? (map (lambda (line)\n        (if (string-contains? line \"type: alpha\")\n            (let ([v (extract-after line \"value: \")])\n              (if v (string->number v) #f))\n            #f))\n      (split-lines *chunk*)))])\n  (if (null? vals) 0 (apply max vals)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Per-chunk max values computed. Global max = max of per-chunk maxima.")
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
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: east | value: 18\nRECORD 0003 | type: gamma | region: east | value: 55\n..."
          "East-region records present. Need sum and count for average.")
        (make-step
          '(think "I need both the sum and count of east-region values to compute an average. I can get both in one map-chunks pass by returning a pair (sum . count) per chunk.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let ([vals (filter number? (map (lambda (line)\n        (if (string-contains? line \"region: east\")\n            (let ([v (extract-after line \"value: \")])\n              (if v (string->number v) #f))\n            #f))\n      (split-lines *chunk*)))])\n  (cons (apply + vals) (length vals)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Each chunk returned (sum . count) pair. Need to aggregate both components.")
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
          'peek #t "[2026-02-06 10:23:01] INFO  - Server started on port 8080\n[2026-02-06 10:23:02] ERROR - Failed to connect to database\n[2026-02-06 10:23:03] INFO  - Retrying connection\n[2026-02-06 10:23:04] ERROR - Connection timeout after 30s\n..."
          "Log format: [timestamp] LEVEL - message. ERROR lines clearly identifiable.")
        (make-step
          '(grep 'input "ERROR" 5)
          'grep #t "5 matches (showing top 5):\n[0.95] [2026-02-06 10:23:02] ERROR - Failed to connect to database\n[0.95] [2026-02-06 10:23:04] ERROR - Connection timeout after 30s\n[0.94] [2026-02-06 10:23:07] ERROR - Authentication failed\n..."
          "grep shows top-5 but total count may be higher. Need map-chunks for exhaustive count.")
        (make-step
          '(think "grep shows top-k matches, not all. I need map-chunks to count every occurrence.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(length (filter (lambda (line) (string-contains? line \"ERROR\")) (split-lines *chunk*)))")
          'map-chunks #t "Processed 30 chunks, results stored as 'map-result"
          "Per-chunk ERROR counts computed across all 30 chunks.")
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
          'peek #t "GET /api/users 200 12ms\nPOST /api/login 401 5ms\nGET /api/data 200 45ms\nGET /api/missing 404 3ms\nPOST /api/upload 500 120ms\n..."
          "HTTP log format: METHOD PATH STATUS DURATION. Four status codes visible: 200, 401, 404, 500.")
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
          'map-chunks #t "Processed 45 chunks, results stored as 'map-result"
          "Per-chunk status code counts in (200 404 401 500) order. Ready for aggregation.")
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
          'map-chunks #t "Processed 20 chunks, results stored as 'map-result"
          "Per-chunk line counts computed.")
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
          'search #t "Found 3 results:\n  linalg (score: 0.92) -- Linear algebra: vectors, matrices, decomposition\n  geometry (score: 0.65) -- Shapes, transforms, SDFs\n  physics (score: 0.45) -- Physics simulation"
          "linalg is the top hit for vector operations. Need to inspect for specific module.")
        (make-step
          '(inspect 'linalg)
          'inspect #t "Skill: linalg\n  Description: Linear algebra - vectors, matrices, decomposition, solvers\n  Modules: linalg/vec, linalg/matrix, linalg/decomp, linalg/solve\n  Dependencies: ()"
          "linalg/vec is the right module for vector operations like dot product.")
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
          'search #t "Found 2 results:\n  statistics (score: 0.95) -- Regression, GLM, time series, hypothesis testing\n  numeric (score: 0.40) -- Complex numbers, DFT, signal processing"
          "statistics skill has mean and stddev. Load it directly.")
        (make-step
          '(exports 'statistics)
          'exports #t "Exports from statistics:\n  mean, variance, stddev, median, quantile, covariance, correlation, ..."
          "Both mean and stddev are exported from statistics module.")
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

    ;; 11. Polynomial roots
    (make-scenario "synth-lattice-polynomial"
      "Find the roots of the polynomial x^2 - 5x + 6."
      *prompt-lattice*
      '((task text 45))
      10000
      (list
        (make-step
          '(search "polynomial roots")
          'search #t "Found 2 results:\n  algebra/polynomial (score: 0.94) -- Polynomial algebra, roots, GCD\n  numeric (score: 0.35) -- Complex numbers, DFT"
          "algebra/polynomial is the right skill for root-finding.")
        (make-step
          '(inspect 'algebra/polynomial)
          'inspect #t "Skill: algebra/polynomial\n  Description: Polynomial algebra — construction, arithmetic, roots, GCD, Gröbner bases\n  Modules: algebra/polynomial\n  Dependencies: (linalg)"
          "Single module algebra/polynomial. Has root-finding functions.")
        (make-step
          '(exports 'algebra/polynomial)
          'exports #t "Exports from algebra/polynomial:\n  make-poly, poly-add, poly-mul, poly-roots, poly-eval, poly-degree, poly-gcd, ..."
          "poly-roots should find roots. make-poly constructs from coefficients.")
        (make-step
          '(begin
             (load 'algebra/polynomial)
             (eval (poly-roots (make-poly '(6 -5 1)))))
          'eval #t "(2 3)"
          "Roots found: x=2 and x=3. Verify: (2-2)(2-3)=0, (3-2)(3-3)=0.")
        (make-step
          '(submit '(2 3))
          'submit #t "Answer accepted: (2 3)")))

    ;; 12. Matrix determinant
    (make-scenario "synth-lattice-matrix-det"
      "Compute the determinant of the matrix [[1 2] [3 4]]."
      *prompt-lattice*
      '((task text 45))
      10000
      (list
        (make-step
          '(search "matrix determinant")
          'search #t "Found 2 results:\n  linalg (score: 0.93) -- Linear algebra: vectors, matrices, decomposition\n  algebra (score: 0.30) -- Groups, rings, polynomial algebra"
          "linalg skill for matrix operations.")
        (make-step
          '(inspect 'linalg)
          'inspect #t "Skill: linalg\n  Description: Linear algebra - vectors, matrices, decomposition, solvers\n  Modules: linalg/vec, linalg/matrix, linalg/decomp, linalg/solve\n  Dependencies: ()"
          "linalg/matrix module for determinant computation.")
        (make-step
          '(begin
             (load 'linalg/matrix)
             (eval (matrix-det (make-matrix 2 2 '(1 2 3 4)))))
          'eval #t "-2"
          "det([[1,2],[3,4]]) = 1*4 - 2*3 = -2. Correct.")
        (make-step
          '(submit -2)
          'submit #t "Answer accepted: -2")))

    ;; 13. Entropy computation
    (make-scenario "synth-lattice-entropy"
      "Compute the Shannon entropy of the distribution (0.25 0.25 0.25 0.25) in bits."
      *prompt-lattice*
      '((task text 70))
      10000
      (list
        (make-step
          '(search "entropy information theory")
          'search #t "Found 2 results:\n  info (score: 0.96) -- Entropy, coding, information theory\n  statistics (score: 0.40) -- Regression, GLM, time series"
          "info skill covers entropy and information theory.")
        (make-step
          '(inspect 'info)
          'inspect #t "Skill: info\n  Description: Entropy, coding, information theory\n  Modules: info/entropy, info/coding\n  Dependencies: ()"
          "info/entropy module has the entropy function.")
        (make-step
          '(exports 'info)
          'exports #t "Exports from info:\n  shannon-entropy, cross-entropy, kl-divergence, mutual-info, ..."
          "shannon-entropy is the function needed.")
        (make-step
          '(begin
             (load 'info/entropy)
             (eval (shannon-entropy '(0.25 0.25 0.25 0.25))))
          'eval #t "2.0"
          "Uniform distribution over 4 outcomes = log2(4) = 2.0 bits. Correct.")
        (make-step
          '(submit 2.0)
          'submit #t "Answer accepted: 2.0")))

    ;; 14. Graph shortest path
    (make-scenario "synth-lattice-graph"
      "Find the shortest path from node A to node D in a graph with edges A-B(1), B-C(2), A-C(4), C-D(1)."
      *prompt-lattice*
      '((task text 90))
      10000
      (list
        (make-step
          '(think "Need graph data structure and shortest path algorithm. The lattice should have graph tools.")
          'think #t #f)
        (make-step
          '(search "graph shortest path")
          'search #t "Found 2 results:\n  data/graph (score: 0.91) -- Data structures, graphs, collections\n  topology (score: 0.35) -- Simplicial complexes, homology"
          "data/graph skill has graph algorithms.")
        (make-step
          '(inspect 'data/graph)
          'inspect #t "Skill: data/graph\n  Description: Graph data structures, algorithms, community detection\n  Modules: data/graph\n  Dependencies: ()"
          "Single module data/graph covers construction and algorithms.")
        (make-step
          '(exports 'data/graph)
          'exports #t "Exports from data/graph:\n  make-graph, add-edge, add-node, shortest-path, bfs, dfs, connected-components, ..."
          "make-graph, add-edge, and shortest-path are what I need.")
        (make-step
          '(begin
             (load 'data/graph)
             (store 'g (let ([g (make-graph)])
                         (add-edge (add-edge (add-edge (add-edge g 'A 'B 1) 'B 'C 2) 'A 'C 4) 'C 'D 1))))
          'store #t "Stored graph"
          "Graph constructed with 4 weighted edges.")
        (make-step
          '(eval (shortest-path (retrieve 'g) 'A 'D))
          'eval #t "((A B C D) . 4)"
          "Shortest path A->B->C->D costs 1+2+1=4. Direct A->C->D is 4+1=5, so BCD route wins.")
        (make-step
          '(submit '((A B C D) . 4))
          'submit #t "Answer accepted: ((A B C D) . 4)")))

    ;; 15. Complex number arithmetic
    (make-scenario "synth-lattice-complex"
      "Compute (3+4i) * (1-2i) and report the result."
      *prompt-lattice*
      '((task text 50))
      10000
      (list
        (make-step
          '(search "complex number arithmetic")
          'search #t "Found 2 results:\n  numeric (score: 0.93) -- Complex numbers, DFT, signal processing\n  algebra (score: 0.30) -- Groups, rings, polynomial algebra"
          "numeric skill has complex number support.")
        (make-step
          '(inspect 'numeric)
          'inspect #t "Skill: numeric\n  Description: Complex numbers, DFT, signal processing\n  Modules: numeric/complex, numeric/dft\n  Dependencies: ()"
          "numeric/complex module for complex arithmetic.")
        (make-step
          '(begin
             (load 'numeric/complex)
             (eval (complex-mul (make-complex 3 4) (make-complex 1 -2))))
          'eval #t "(complex 11 -2)"
          "(3+4i)(1-2i) = 3-6i+4i-8i² = 3-2i+8 = 11-2i. Correct.")
        (make-step
          '(submit '(complex 11 -2))
          'submit #t "Answer accepted: (complex 11 -2)")))

    ;; ---- Domain 4: Reasoning Patterns ----

    ;; 16. Think + plan + execute
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
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: west | value: 18\nRECORD 0003 | type: gamma | region: east | value: 55\nRECORD 0004 | type: alpha | region: west | value: 30\n..."
          "Data confirmed. West-region records with alpha/beta types must be excluded.")
        (make-step
          '(plan! (("Understand filter criteria" . done)
                   ("Map-chunks with OR type filter and NOT west" . pending)
                   ("Sum results" . pending)
                   ("Submit total" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n  (apply + (map (lambda (line)\n    (if (and (or (string-contains? line \"type: alpha\")\n                 (string-contains? line \"type: beta\"))\n             (not (string-contains? line \"region: west\")))\n        (let ([v (extract-after line \"value: \")])\n          (if v (string->number v) 0))\n        0))\n    lines)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Compound filter applied: (alpha OR beta) AND NOT west. Per-chunk sums ready.")
        (make-step
          '(begin
             (store 'total (apply + (retrieve 'map-result)))
             (submit (retrieve 'total)))
          'submit #t "Answer accepted: 1591")))

    ;; 17. Error recovery pattern
    (make-scenario "synth-reasoning-recover"
      "Sum all numeric values in the input (one number per line, with some non-numeric lines mixed in)."
      *prompt-text-analysis*
      '((input chunks 15000) (task text 85))
      15000
      (list
        (make-step
          '(peek 'input 300)
          'peek #t "42\nhello world\n17\n\n99\nnot a number\n-5\n12.5\n..."
          "Mixed content: numbers and text. Need to parse each line, skip non-numeric.")
        (make-step
          '(map-chunks 'input
            "(apply + (filter number? (map (lambda (line)\n  (let ([n (string->number line)])\n    (if n n 0)))\n  (split-lines *chunk*))))")
          'map-chunks #t "Processed 15 chunks, results stored as 'map-result"
          "Per-chunk numeric sums computed, non-numeric lines returned 0.")
        (make-step
          '(begin
             (store 'total (apply + (retrieve 'map-result)))
             (submit (retrieve 'total)))
          'submit #t "Answer accepted: 8847.5")))

    ;; 18. Slice-based exploration
    (make-scenario "synth-explore-slice"
      "Find which chunk range contains the HEADER section in the input document, and report the header text."
      *prompt-text-analysis*
      '((input chunks 40000) (task text 90))
      15000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "--- METADATA ---\nCreated: 2026-02-06\nAuthor: system\n\n--- DATA ---\nRECORD 0001 | value: 42\nRECORD 0002 | value: 18\n..."
          "Peek shows METADATA and DATA sections but no HEADER yet. It may be in later chunks.")
        (make-step
          '(grep 'input "HEADER" 5)
          'grep #t "2 matches (showing top 2):\n[0.98] --- HEADER ---\n[0.87] Note: see HEADER for column definitions"
          "HEADER section exists. grep found the marker. Need slice to get the full section text.")
        (make-step
          '(think "The HEADER section exists. grep found it but I need the full section. Let me search for it in the early chunks since headers usually come first.")
          'think #t #f)
        (make-step
          '(slice 'input 0 3)
          'slice #t "--- METADATA ---\nCreated: 2026-02-06\nAuthor: system\n\n--- HEADER ---\nColumn 1: type (alpha, beta, gamma)\nColumn 2: region (north, south, east, west)\nColumn 3: value (integer 0-100)\n\n--- DATA ---\nRECORD 0001 | type: alpha | region: north | value: 42\n..."
          "Found HEADER in chunks 0-2. Three columns defined: type, region, value.")
        (make-step
          '(submit "Column 1: type (alpha, beta, gamma), Column 2: region (north, south, east, west), Column 3: value (integer 0-100)")
          'submit #t "Answer accepted")))

    ;; 19. Large begin with memorize
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

    ;; 20. Multi-pass with plan updates
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
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: south | value: 18\nRECORD 0003 | type: gamma | region: east | value: 55\n..."
          "Standard RECORD format. Two map-chunks passes needed: one for alpha, one for beta.")
        (make-step
          '(plan! (("Peek at data format" . done)
                   ("Sum alpha values" . pending)
                   ("Sum beta values" . pending)
                   ("Compare and report" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(apply + (map (lambda (line)\n  (if (string-contains? line \"type: alpha\")\n      (let ([v (extract-after line \"value: \")])\n        (if v (string->number v) 0))\n      0))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Alpha totals computed per-chunk.")
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
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Beta totals computed per-chunk.")
        (make-step
          '(store 'beta-total (apply + (retrieve 'map-result)))
          'store #t "Stored: 983")
        (make-step
          '(begin
             (store 'diff (- (retrieve 'alpha-total) (retrieve 'beta-total)))
             (submit (list 'alpha-higher-by (retrieve 'diff))))
          'submit #t "Answer accepted: (alpha-higher-by 264)")))

    ;; ---- Domain 5: Error Recovery ----

    ;; 21. Bad module name, then correct
    (make-scenario "synth-error-bad-module"
      "Compute the dot product of vectors (1 0 0) and (0 1 0)."
      *prompt-lattice*
      '((task text 50))
      10000
      (list
        (make-step
          '(search "vector dot product")
          'search #t "Found 3 results:\n  linalg (score: 0.92) -- Linear algebra: vectors, matrices, decomposition\n  geometry (score: 0.65) -- Shapes, transforms, SDFs\n  physics (score: 0.45) -- Physics simulation"
          "linalg skill is the right choice for vector operations.")
        (make-step
          '(load 'linalg)
          'load #f "Failed to load linalg: module not found. Available: linalg/vec, linalg/matrix, linalg/decomp, linalg/solve"
          "Module name was wrong — linalg is a skill, not a module. Need linalg/vec.")
        (make-step
          '(load 'linalg/vec)
          'load #t "Module linalg/vec loaded."
          "Correct module loaded. vec-dot should now be available.")
        (make-step
          '(eval (vec-dot (vec 1 0 0) (vec 0 1 0)))
          'eval #t "0"
          "Orthogonal vectors have dot product 0. Correct.")
        (make-step
          '(submit 0)
          'submit #t "Answer accepted: 0")))

    ;; 22. Eval failure, then correct
    (make-scenario "synth-error-eval-failure"
      "Compute the variance of (2 4 6 8 10)."
      *prompt-lattice*
      '((task text 40))
      10000
      (list
        (make-step
          '(search "variance statistics")
          'search #t "Found 1 result:\n  statistics (score: 0.95) -- Regression, GLM, time series, hypothesis testing"
          "statistics skill has variance function.")
        (make-step
          '(begin
             (load 'statistics)
             (eval (var '(2 4 6 8 10))))
          'eval #f "Error: var is not bound"
          "Wrong function name. Need to check exports for the correct name.")
        (make-step
          '(exports 'statistics)
          'exports #t "Exports from statistics:\n  mean, variance, stddev, median, quantile, covariance, correlation, ..."
          "The function is called 'variance', not 'var'. Let me retry.")
        (make-step
          '(eval (variance '(2 4 6 8 10)))
          'eval #t "10"
          "Variance of (2 4 6 8 10) = 10. Mean is 6, deviations squared sum to 40, /4 = 10.")
        (make-step
          '(submit 10)
          'submit #t "Answer accepted: 10")))

    ;; 23. Type mismatch, think, then correct
    (make-scenario "synth-error-type-mismatch"
      "Compute the sum of elements in the vector (10 20 30)."
      *prompt-lattice*
      '((task text 45))
      10000
      (list
        (make-step
          '(begin
             (search "vector sum")
             (load 'linalg/vec))
          'load #t "Module linalg/vec loaded."
          "linalg/vec loaded. Need to find the right function for element sum.")
        (make-step
          '(eval (apply + (vec 10 20 30)))
          'eval #f "Error: + expects numbers, got vec"
          "Can't apply + to a vec directly — vec is not a list. Need to convert or use vec-sum.")
        (make-step
          '(think "The vec type is not a list, so apply + doesn't work. I should look for a vec-sum or vec-fold function, or convert to list first.")
          'think #t #f)
        (make-step
          '(exports 'linalg)
          'exports #t "Exports from linalg:\n  vec, vec-ref, vec-dot, vec-add, vec-scale, vec-norm, vec-sum, vec->list, ..."
          "vec-sum exists. Use that instead of apply +.")
        (make-step
          '(eval (vec-sum (vec 10 20 30)))
          'eval #t "60"
          "vec-sum correctly sums vector elements: 10+20+30=60.")
        (make-step
          '(submit 60)
          'submit #t "Answer accepted: 60")))

    ;; ---- Domain 6: Underrepresented Actions ----

    ;; 24. Grep multi-pattern comparison
    (make-scenario "synth-grep-pattern"
      "Compare the frequency of 'WARNING' vs 'CRITICAL' messages in the log. Which is more common?"
      *prompt-text-analysis*
      '((input chunks 50000) (task text 80))
      15000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "[2026-02-06 12:00:01] INFO     - Normal operation\n[2026-02-06 12:00:02] WARNING  - Disk usage at 85%\n[2026-02-06 12:00:03] CRITICAL - Primary database down\n[2026-02-06 12:00:04] WARNING  - Memory pressure detected\n..."
          "Log has INFO, WARNING, and CRITICAL levels. Need to count WARNING vs CRITICAL.")
        (make-step
          '(grep 'input "WARNING" 3)
          'grep #t "3 matches (showing top 3):\n[0.96] [2026-02-06 12:00:02] WARNING  - Disk usage at 85%\n[0.95] [2026-02-06 12:00:04] WARNING  - Memory pressure detected\n[0.94] [2026-02-06 12:00:09] WARNING  - Connection pool exhausted"
          "grep shows sample WARNINGs. Need map-chunks for exact counts of both levels.")
        (make-step
          '(grep 'input "CRITICAL" 3)
          'grep #t "3 matches (showing top 3):\n[0.97] [2026-02-06 12:00:03] CRITICAL - Primary database down\n[0.96] [2026-02-06 12:00:15] CRITICAL - Replication lag exceeded threshold\n[0.95] [2026-02-06 12:00:22] CRITICAL - SSL certificate expiring"
          "Both levels present. grep top-k insufficient for counts; use map-chunks.")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n  (cons (length (filter (lambda (l) (string-contains? l \"WARNING\")) lines))\n        (length (filter (lambda (l) (string-contains? l \"CRITICAL\")) lines))))")
          'map-chunks #t "Processed 50 chunks, results stored as 'map-result"
          "Per-chunk (warning-count . critical-count) pairs collected.")
        (make-step
          '(begin
             (store 'warnings (apply + (map car (retrieve 'map-result))))
             (store 'criticals (apply + (map cdr (retrieve 'map-result))))
             (submit (if (> (retrieve 'warnings) (retrieve 'criticals))
                         (list 'WARNING (retrieve 'warnings) 'vs 'CRITICAL (retrieve 'criticals))
                         (list 'CRITICAL (retrieve 'criticals) 'vs 'WARNING (retrieve 'warnings)))))
          'submit #t "Answer accepted: (WARNING 287 vs CRITICAL 43)")))

    ;; 25. Slice-based targeted extraction
    (make-scenario "synth-slice-explore"
      "Find the configuration section in the input file and extract the 'max_connections' setting."
      *prompt-text-analysis*
      '((input chunks 30000) (task text 80))
      15000
      (list
        (make-step
          '(grep 'input "max_connections" 3)
          'grep #t "1 match:\n[0.99] max_connections = 500"
          "Setting found via grep. But need surrounding context to verify it's in [config] section.")
        (make-step
          '(grep 'input "CONFIG" 3)
          'grep #t "2 matches:\n[0.98] --- CONFIG ---\n[0.85] ; See CONFIG for server settings"
          "CONFIG section exists. Need to slice the right chunk range to see full context.")
        (make-step
          '(slice 'input 0 2)
          'slice #t "--- HEADER ---\nApp: my-server v2.1\n\n--- CONFIG ---\nhost = 0.0.0.0\nport = 8080\nmax_connections = 500\ntimeout_ms = 30000\nlog_level = info\n\n--- DATA ---\n..."
          "Found max_connections = 500 inside [CONFIG] section in chunks 0-1.")
        (make-step
          '(eval (string->number "500"))
          'eval #t "500"
          "Extracted value: 500.")
        (make-step
          '(submit 500)
          'submit #t "Answer accepted: 500")))

    ;; ---- Domain 7: Cross-Key Operations ----

    ;; 26. Two datasets, compare totals
    (make-scenario "synth-cross-key-compare"
      "You have two datasets: 'sales-q1' and 'sales-q2', each containing sales records. Compare the total revenue between Q1 and Q2."
      *prompt-multi-key*
      '((sales-q1 chunks 25000) (sales-q2 chunks 30000) (task text 100))
      20000
      (list
        (make-step
          '(begin
             (peek 'sales-q1 400)
             (peek 'sales-q2 400))
          'peek #t "sales-q1: SALE 001 | product: widget | qty: 5 | price: 10.00\nSALE 002 | product: gadget | qty: 2 | price: 25.50\n...\nsales-q2: SALE 001 | product: widget | qty: 8 | price: 10.00\nSALE 002 | product: gadget | qty: 3 | price: 25.50\n..."
          "Both datasets have same format: product, qty, price per record. Revenue = qty * price.")
        (make-step
          '(plan! (("Preview both datasets" . done)
                   ("Sum Q1 revenue via map-chunks" . pending)
                   ("Sum Q2 revenue via map-chunks" . pending)
                   ("Compare and report" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'sales-q1
            "(apply + (map (lambda (line)\n  (if (string-contains? line \"SALE\")\n      (let ([qty (extract-after line \"qty: \")]\n            [price (extract-after line \"price: \")])\n        (if (and qty price)\n            (* (string->number qty) (string->number price))\n            0))\n      0))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 25 chunks, results stored as 'map-result"
          "Q1 per-chunk revenues computed.")
        (make-step
          '(store 'q1-total (apply + (retrieve 'map-result)))
          'store #t "Stored: 45230.50")
        (make-step
          '(map-chunks 'sales-q2
            "(apply + (map (lambda (line)\n  (if (string-contains? line \"SALE\")\n      (let ([qty (extract-after line \"qty: \")]\n            [price (extract-after line \"price: \")])\n        (if (and qty price)\n            (* (string->number qty) (string->number price))\n            0))\n      0))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 30 chunks, results stored as 'map-result"
          "Q2 per-chunk revenues computed.")
        (make-step
          '(store 'q2-total (apply + (retrieve 'map-result)))
          'store #t "Stored: 52875.00")
        (make-step
          '(submit (list 'q2-higher-by (- (retrieve 'q2-total) (retrieve 'q1-total))))
          'submit #t "Answer accepted: (q2-higher-by 7644.50)")))

    ;; 27. Cross-key join (users + orders → top customer)
    (make-scenario "synth-cross-key-join"
      "You have 'users' (id, name) and 'orders' (user_id, amount). Find the user with the highest total order amount."
      *prompt-multi-key*
      '((users chunks 5000) (orders chunks 40000) (task text 80))
      20000
      (list
        (make-step
          '(begin
             (peek 'users 400)
             (peek 'orders 400))
          'peek #t "users: USER 001 | name: Alice\nUSER 002 | name: Bob\nUSER 003 | name: Charlie\n...\norders: ORDER 0001 | user_id: 002 | amount: 150.00\nORDER 0002 | user_id: 001 | amount: 75.50\nORDER 0003 | user_id: 003 | amount: 200.00\n..."
          "Users have id+name. Orders have user_id+amount. Need to sum orders by user_id, then lookup name.")
        (make-step
          '(think "Strategy: (1) map-chunks on orders to build (user_id . total) per chunk, (2) aggregate across chunks, (3) find max, (4) look up name in users.")
          'think #t #f)
        (make-step
          '(plan! (("Understand formats" . done)
                   ("Sum orders by user_id" . pending)
                   ("Find top user_id" . pending)
                   ("Look up user name" . pending)
                   ("Submit result" . pending)))
          'plan #t "Plan updated: 5 items")
        (make-step
          '(map-chunks 'orders
            "(let loop ([lines (split-lines *chunk*)] [acc '()])\n  (if (null? lines) acc\n      (let ([line (car lines)])\n        (if (string-contains? line \"ORDER\")\n            (let ([uid (extract-after line \"user_id: \")]\n                  [amt (extract-after line \"amount: \")])\n              (if (and uid amt)\n                  (let ([uid-sym (string->symbol uid)]\n                        [amt-n (string->number amt)])\n                    (let ([existing (assq uid-sym acc)])\n                      (loop (cdr lines)\n                            (if existing\n                                (map (lambda (p) (if (eq? (car p) uid-sym) (cons uid-sym (+ (cdr p) amt-n)) p)) acc)\n                                (cons (cons uid-sym amt-n) acc)))))\n                  (loop (cdr lines) acc)))\n            (loop (cdr lines) acc)))))")
          'map-chunks #t "Processed 40 chunks, results stored as 'map-result"
          "Per-chunk user totals as alist. Need to merge across chunks.")
        (make-step
          '(eval (let merge-all ([chunks (retrieve 'map-result)] [merged '()])
                   (if (null? chunks)
                       merged
                       (merge-all (cdr chunks)
                         (fold-left (lambda (acc pair)
                                      (let ([existing (assq (car pair) acc)])
                                        (if existing
                                            (map (lambda (p) (if (eq? (car p) (car pair))
                                                                 (cons (car pair) (+ (cdr p) (cdr pair)))
                                                                 p)) acc)
                                            (cons pair acc))))
                                    merged
                                    (car chunks))))))
          'eval #t "((|003| . 12450.00) (|002| . 18750.50) (|001| . 9200.00))"
          "Merged totals: user 002 (Bob) has highest at 18750.50.")
        (make-step
          '(store 'totals (retrieve 'step-4-result))
          'store #t "Stored totals")
        (make-step
          '(grep 'users "002" 1)
          'grep #t "1 match:\n[0.99] USER 002 | name: Bob"
          "User 002 is Bob.")
        (make-step
          '(submit (list 'top-customer "Bob" 'total 18750.50))
          'submit #t "Answer accepted: (top-customer Bob total 18750.50)")))

    ;; ---- Domain 8: Iterative Refinement ----

    ;; 28. First attempt wrong, verifier rejects, retry with correct approach
    (make-scenario "synth-iterative-refine"
      "Count the number of UNIQUE product names in the inventory data."
      *prompt-iterative*
      '((input chunks 35000) (task text 55))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "ITEM 001 | product: Widget A | qty: 50 | warehouse: W1\nITEM 002 | product: Widget B | qty: 30 | warehouse: W2\nITEM 003 | product: Widget A | qty: 20 | warehouse: W1\nITEM 004 | product: Gadget X | qty: 10 | warehouse: W3\n..."
          "Products repeat across items. Need UNIQUE count, not total count.")
        (make-step
          '(map-chunks 'input
            "(length (filter (lambda (line) (string-contains? line \"ITEM\")) (split-lines *chunk*)))")
          'map-chunks #t "Processed 35 chunks, results stored as 'map-result"
          "Counted all ITEM lines per chunk. But this counts total items, not unique products!")
        (make-step
          '(submit (apply + (retrieve 'map-result)))
          'submit #f "Rejected: 1750 is the total item count, not the unique product count. Products like 'Widget A' appear in multiple items."
          "The verifier caught my mistake. I counted total items, not unique products.")
        (make-step
          '(think "I need unique product names. map-chunks per chunk gives per-chunk unique sets, but products may span chunks. I need to collect all unique names across all chunks, then count.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let loop ([lines (split-lines *chunk*)] [names '()])\n  (if (null? lines) names\n      (let ([line (car lines)])\n        (if (string-contains? line \"product: \")\n            (let ([name (extract-after line \"product: \")])\n              (if (and name (not (member name names)))\n                  (loop (cdr lines) (cons name names))\n                  (loop (cdr lines) names)))\n            (loop (cdr lines) names)))))")
          'map-chunks #t "Processed 35 chunks, results stored as 'map-result"
          "Per-chunk unique product lists. Need to merge and deduplicate across chunks.")
        (make-step
          '(begin
             (store 'all-products
               (let dedup ([lists (retrieve 'map-result)] [seen '()])
                 (if (null? lists) seen
                     (dedup (cdr lists)
                       (fold-left (lambda (acc name) (if (member name acc) acc (cons name acc)))
                                  seen (car lists))))))
             (submit (length (retrieve 'all-products))))
          'submit #t "Answer accepted: 42")))

    ;; 29. Wrong aggregation, then correct
    (make-scenario "synth-iterative-refine-avg"
      "Compute the average price per item across all inventory records."
      *prompt-iterative*
      '((input chunks 35000) (task text 55))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "ITEM 001 | product: Widget A | price: 12.50\nITEM 002 | product: Widget B | price: 8.00\nITEM 003 | product: Gadget X | price: 45.00\n..."
          "Need sum of all prices divided by count of items.")
        (make-step
          '(map-chunks 'input
            "(apply + (filter number? (map (lambda (line)\n  (let ([p (extract-after line \"price: \")])\n    (if p (string->number p) #f)))\n  (split-lines *chunk*))))")
          'map-chunks #t "Processed 35 chunks, results stored as 'map-result")
        (make-step
          '(submit (apply + (retrieve 'map-result)))
          'submit #f "Rejected: 48750.00 is the total sum, not the average. You need to divide by the number of items."
          "Submitted sum instead of average. Need count too.")
        (make-step
          '(think "I computed the sum but forgot to count. I need a second pass for count, or I should have returned (sum . count) pairs per chunk in one pass.")
          'think #t #f)
        (make-step
          '(store 'total-sum (apply + (retrieve 'map-result)))
          'store #t "Stored: 48750.00")
        (make-step
          '(map-chunks 'input
            "(length (filter (lambda (line) (string-contains? line \"ITEM\")) (split-lines *chunk*)))")
          'map-chunks #t "Processed 35 chunks, results stored as 'map-result")
        (make-step
          '(begin
             (store 'total-count (apply + (retrieve 'map-result)))
             (submit (/ (retrieve 'total-sum) (retrieve 'total-count))))
          'submit #t "Answer accepted: 27.86")))

    ;; ---- Domain 9: Recall-Step and Journal Workflows ----

    ;; 30. Recall a previous step's output for verification
    (make-scenario "synth-recall-step"
      "Verify that the sum of alpha values equals the sum of beta values in the dataset."
      *prompt-aggregation*
      '((input chunks 55000) (task text 70))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "RECORD 0001 | type: alpha | value: 42\nRECORD 0002 | type: beta | value: 42\nRECORD 0003 | type: alpha | value: 18\nRECORD 0004 | type: beta | value: 18\n..."
          "Data has alpha and beta records. Need to sum both types and compare.")
        (make-step
          '(map-chunks 'input
            "(apply + (map (lambda (line)\n  (if (string-contains? line \"type: alpha\")\n      (let ([v (extract-after line \"value: \")])\n        (if v (string->number v) 0))\n      0))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Alpha per-chunk sums computed.")
        (make-step
          '(store 'alpha-sum (apply + (retrieve 'map-result)))
          'store #t "Stored: 2847")
        (make-step
          '(map-chunks 'input
            "(apply + (map (lambda (line)\n  (if (string-contains? line \"type: beta\")\n      (let ([v (extract-after line \"value: \")])\n        (if v (string->number v) 0))\n      0))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Beta per-chunk sums computed.")
        (make-step
          '(store 'beta-sum (apply + (retrieve 'map-result)))
          'store #t "Stored: 2847")
        (make-step
          '(begin
             (recall-step 2)
             (recall-step 4))
          'recall-step #t "Step 2: store alpha-sum → 2847\nStep 4: store beta-sum → 2847"
          "Both sums are 2847. The totals are equal.")
        (make-step
          '(submit (list 'equal (retrieve 'alpha-sum) (retrieve 'beta-sum)))
          'submit #t "Answer accepted: (equal 2847 2847)")))

    ;; 31. Journal-driven multi-phase analysis
    (make-scenario "synth-journal-analysis"
      "Analyze a server log: (1) identify the most common error type, (2) find the time range of errors, (3) report both."
      *prompt-text-analysis*
      '((input chunks 50000) (task text 95))
      25000
      (list
        (make-step
          '(plan! (("Phase 1: Identify error types" . pending)
                   ("Phase 2: Find time range" . pending)
                   ("Phase 3: Report" . pending)))
          'plan #t "Plan updated: 3 items")
        (make-step
          '(peek 'input 500)
          'peek #t "[2026-02-06 10:23:01] ERROR ConnTimeout - Connection timed out to db-primary\n[2026-02-06 10:23:03] ERROR AuthFail - Invalid credentials for user admin\n[2026-02-06 10:23:05] ERROR ConnTimeout - Connection timed out to db-replica\n..."
          "Errors have typed labels: ConnTimeout, AuthFail, etc. Timestamps in brackets.")
        (make-step
          '(journal observation "Error format: [timestamp] ERROR <type> - <message>. Types include ConnTimeout, AuthFail.")
          'journal #t "Recorded to journal: observation")
        (make-step
          '(map-chunks 'input
            "(let loop ([lines (split-lines *chunk*)] [counts '()])\n  (if (null? lines) counts\n      (let ([line (car lines)])\n        (if (string-contains? line \"ERROR\")\n            (let ([type (extract-after line \"ERROR \")])\n              (if type\n                  (let ([t (car (split-lines type))])\n                    (let ([existing (assoc t counts)])\n                      (loop (cdr lines)\n                            (if existing\n                                (map (lambda (p) (if (equal? (car p) t) (cons t (+ (cdr p) 1)) p)) counts)\n                                (cons (cons t 1) counts)))))\n                  (loop (cdr lines) counts)))\n            (loop (cdr lines) counts)))))")
          'map-chunks #t "Processed 50 chunks, results stored as 'map-result"
          "Per-chunk error type counts. Need to merge across chunks.")
        (make-step
          '(begin
             (store 'error-counts
               (let merge ([chunks (retrieve 'map-result)] [merged '()])
                 (if (null? chunks) merged
                     (merge (cdr chunks)
                       (fold-left (lambda (acc pair)
                                    (let ([existing (assoc (car pair) acc)])
                                      (if existing
                                          (map (lambda (p) (if (equal? (car p) (car pair))
                                                               (cons (car pair) (+ (cdr p) (cdr pair)))
                                                               p)) acc)
                                          (cons pair acc))))
                                  merged (car chunks))))))
             (journal 'phase1-result "Error type counts merged and stored in error-counts."))
          'store #t "Stored error counts")
        (make-step
          '(plan! (("Phase 1: Identify error types" . done)
                   ("Phase 2: Find time range" . pending)
                   ("Phase 3: Report" . pending)))
          'plan #t "Plan updated: 3 items")
        (make-step
          '(begin
             (grep 'input "ERROR" 1)
             (slice 'input 0 1))
          'grep #t "First ERROR at [2026-02-06 10:23:01]")
        (make-step
          '(slice 'input 48 50)
          'slice #t "...\n[2026-02-06 14:55:22] ERROR ConnTimeout - Connection timed out to cache-3\n[2026-02-06 14:55:30] INFO - Service restored"
          "Last error around 14:55. Time range: 10:23 to 14:55.")
        (make-step
          '(journal phase2-result "Error time range: 10:23:01 to 14:55:22 (about 4.5 hours)")
          'journal #t "Recorded to journal: phase2-result")
        (make-step
          '(submit (list 'most-common "ConnTimeout" 'count 287 'time-range "10:23:01 to 14:55:22"))
          'submit #t "Answer accepted")))

    ;; ---- Domain 10: CSV/Structured Numeric Data ----

    ;; 32. CSV parsing and statistical aggregation
    (make-scenario "synth-csv-stats"
      "The input is CSV data with columns: id, temperature, humidity, pressure. Compute the average temperature and the max pressure."
      *prompt-text-analysis*
      '((input chunks 20000) (task text 90))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "id,temperature,humidity,pressure\n1,22.5,45,1013.2\n2,23.1,42,1013.5\n3,21.8,48,1012.9\n4,24.0,40,1014.1\n5,22.2,46,1013.0\n..."
          "CSV with header. temperature is col 2 (0-indexed 1), pressure is col 4 (0-indexed 3).")
        (make-step
          '(think "I need to parse CSV lines. Skip the header. For each data line, split by comma, extract temperature (index 1) and pressure (index 3).")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let ([lines (filter (lambda (l) (and (not (string=? l \"\")) (not (string-contains? l \"id,temperature\")))) (split-lines *chunk*))])\n  (let loop ([ls lines] [t-sum 0] [t-count 0] [p-max 0])\n    (if (null? ls) (list t-sum t-count p-max)\n        (let* ([parts (split-csv (car ls))]\n               [temp (and (>= (length parts) 2) (string->number (list-ref parts 1)))]\n               [pres (and (>= (length parts) 4) (string->number (list-ref parts 3)))])\n          (loop (cdr ls)\n                (if temp (+ t-sum temp) t-sum)\n                (if temp (+ t-count 1) t-count)\n                (if (and pres (> pres p-max)) pres p-max))))))")
          'map-chunks #t "Processed 20 chunks, results stored as 'map-result"
          "Per-chunk (temp-sum, temp-count, max-pressure) triples collected.")
        (make-step
          '(begin
             (store 'temp-avg (/ (apply + (map car (retrieve 'map-result)))
                                 (apply + (map cadr (retrieve 'map-result)))))
             (store 'max-pressure (apply max (map caddr (retrieve 'map-result)))))
          'store #t "Stored: temp-avg=22.87, max-pressure=1015.3")
        (make-step
          '(submit (list 'avg-temperature (retrieve 'temp-avg)
                         'max-pressure (retrieve 'max-pressure)))
          'submit #t "Answer accepted: (avg-temperature 22.87 max-pressure 1015.3)")))

    ;; 33. Lattice + data processing (hybrid)
    (make-scenario "synth-hybrid-lattice-data"
      "Compute the standard deviation of all 'value' fields from records where type is 'alpha'. Use the statistics lattice skill."
      *prompt-aggregation*
      '((input chunks 55000) (task text 90))
      25000
      (list
        (make-step
          '(think "I need to extract all alpha values, then compute std dev. The lattice has a statistics skill with vec-std-dev.")
          'think #t #f)
        (make-step
          '(plan! (("Extract alpha values via map-chunks" . pending)
                   ("Load statistics module" . pending)
                   ("Compute std dev" . pending)
                   ("Submit" . pending)))
          'plan #t "Plan updated: 4 items")
        (make-step
          '(map-chunks 'input
            "(filter number? (map (lambda (line)\n  (if (string-contains? line \"type: alpha\")\n      (let ([v (extract-after line \"value: \")])\n        (if v (string->number v) #f))\n      #f))\n  (split-lines *chunk*)))")
          'map-chunks #t "Processed 55 chunks, results stored as 'map-result"
          "Per-chunk alpha value lists collected.")
        (make-step
          '(store 'all-values (apply append (retrieve 'map-result)))
          'store #t "Stored: list of 847 values")
        (make-step
          '(begin
             (search "standard deviation")
             (load 'statistics/summary-stats))
          'load #t "Module statistics/summary-stats loaded."
          "statistics skill found and summary-stats module loaded.")
        (make-step
          '(eval (vec-std-dev (retrieve 'all-values)))
          'eval #t "14.32"
          "Standard deviation computed: 14.32.")
        (make-step
          '(submit (retrieve 'step-5-result))
          'submit #t "Answer accepted: 14.32")))

    ;; ---- Domain 11: Inventory Counting (Intellect3-style) ----

    ;; 34. Count fruits I own (step-by-step, teaches the full pattern)
    (make-scenario "synth-inventory-fruits"
      "How many fruits do I have in total?"
      *prompt-inventory*
      '((input chunks 5000) (task text 200))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "I have 23 reference books (which are bestsellers). I also have 31 guavas (which are organic). I also have 20 audi a4 (which are vintage models). My uncle has 99 sofas. My father has 59 graphic novels. My neighbor has 69 apricots. My aunt has 94 cherries. I also have 35 gloves (which are in my closet). My grandfather has 49 audi a4. I also have 78 headphones..."
          "Inventory format: 'I have N <item>' or 'I also have N <item>' for my items. 'My <relative> has N <item>' for others. Some have narrative tracking in parens.")
        (make-step
          '(think "I need to count only MY fruits. Steps: (1) Filter to lines with 'I have'/'I also have' — skip lines with 'My '. (2) Extract the number right after 'have '. (3) Check if the item name matches a fruit. Fruit stems to match: apple, orange, guava, cherr, blackberr, strawberr, grape, mango, kiwi, peach, plum, persimmon, pomegranate, dates, dragonfruit, blueberr, watermelon, nectarine, lemon, lychee, breadfruit, raspberr, apricot, banana, pineapple, jonagold, envy. Use map-chunks to process all chunks.")
          'think #t #f)
        (make-step
          '(plan! (("Peek at inventory format" . done)
                   ("Plan fruit word list" . done)
                   ("map-chunks to extract my fruit counts" . pending)
                   ("Sum across chunks" . pending)
                   ("Submit total" . pending)))
          'plan #t "Plan updated: 5 items")
        (make-step
          '(map-chunks 'input
            "(let ([fruits '(\"apple\" \"orange\" \"guava\" \"cherr\" \"blackberr\" \"strawberr\" \"grape\" \"mango\" \"kiwi\" \"peach\" \"plum\" \"persimmon\" \"pomegranate\" \"dates\" \"dragonfruit\" \"blueberr\" \"watermelon\" \"nectarine\" \"lemon\" \"lychee\" \"breadfruit\" \"raspberr\" \"apricot\" \"banana\" \"pineapple\" \"jonagold\" \"envy\")])\n  (apply +\n    (map (lambda (line)\n      (if (and (or (string-contains? line \"I have \")\n                   (string-contains? line \"I also have \"))\n               (not (string-contains? line \"My \")))\n          (let* ([after (or (extract-after line \"I also have \")\n                            (extract-after line \"I have \"))]\n                 [sp (and after (string-index-of after \" \"))]\n                 [num (and sp (> sp 0) (string->number (substr after 0 sp)))])\n            (if (and num (exists (lambda (f) (string-contains? after f)) fruits))\n                num 0))\n          0))\n      (split-lines *chunk*))))")
          'map-chunks #t "Processed 5 chunks, results stored as 'map-result"
          "Per-chunk fruit sums extracted. The expression filters ownership (not 'My '), extracts number after 'have', and checks item against fruit word list.")
        (make-step
          '(store 'total (apply + (retrieve 'map-result)))
          'store #t "Stored: 501")
        (make-step
          '(submit (retrieve 'total))
          'submit #t "Answer accepted: 501")))

    ;; 35. Count clothing items I own (teaches different category word list)
    (make-scenario "synth-inventory-clothing"
      "What is the total number of clothing items that I have?"
      *prompt-inventory*
      '((input chunks 5000) (task text 200))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "I have 78 scarves. I also have 100 shoes. My grandfather has 69 mazda cx-90. I also have 75 ducks. My father has 79 swimsuits. I also have 76 opossums(here's how I ended up with 76 of them: initially I had 99, then I lost 24...). My professor has 61 plantains..."
          "Items are mixed: clothing, animals, cars, electronics, etc. Need clothing word list.")
        (make-step
          '(think "Clothing items to match: t-shirt, shoes, hat, hoodie, sweater, glove, shorts, boots, sneaker, coat, ties, dress, suit, skirt, swimsuit, pajama, socks, jeans, jacket, scarf, scarves, hoodies, coats. I'll use stems that catch plurals. Watch out: 'dressers' is furniture not clothing, but 'dress' stem matches it. Use 'dresses' or check more carefully. Actually 'suit' catches 'suits' and 'swimsuits' — both are clothing so that's fine.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let ([clothing '(\"t-shirt\" \"shoes\" \"shoe \" \"hats\" \"hat \" \"hoodie\" \"sweater\" \"glove\" \"shorts\" \"boots\" \"sneaker\" \"coats\" \"coat \" \"ties\" \"tie \" \"dresses\" \"dress \" \"suits\" \"suit \" \"skirt\" \"swimsuit\" \"pajama\" \"socks\" \"sock \" \"jeans\" \"jean \" \"jacket\" \"scarf\" \"scarves\")])\n  (apply +\n    (map (lambda (line)\n      (if (and (or (string-contains? line \"I have \")\n                   (string-contains? line \"I also have \"))\n               (not (string-contains? line \"My \")))\n          (let* ([after (or (extract-after line \"I also have \")\n                            (extract-after line \"I have \"))]\n                 [sp (and after (string-index-of after \" \"))]\n                 [num (and sp (> sp 0) (string->number (substr after 0 sp)))])\n            (if (and num (exists (lambda (f) (string-contains? after f)) clothing))\n                num 0))\n          0))\n      (split-lines *chunk*))))")
          'map-chunks #t "Processed 5 chunks, results stored as 'map-result"
          "Per-chunk clothing sums extracted. Word list uses plural forms to avoid matching 'dressers' with 'dress'.")
        (make-step
          '(store 'total (apply + (retrieve 'map-result)))
          'store #t "Stored: 729")
        (make-step
          '(submit (retrieve 'total))
          'submit #t "Answer accepted: 729")))

    ;; 36. Count fruits + clothing combined (multi-category)
    (make-scenario "synth-inventory-combined"
      "What is the total number of fruits and clothing items that I have?"
      *prompt-inventory*
      '((input chunks 5000) (task text 200))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "I have 53 jaguar f-type (which have low mileage). I also have 98 history books. I also have 92 apples (which are for a fruit salad). My mother has 98 ties. My brother has 78 jeans. I also have 83 ties. My teacher has 51 rocking chairs. I also have 48 self-help books..."
          "Mixed inventory. Need to count both fruits and clothing that belong to me.")
        (make-step
          '(think "Two categories to count: fruits and clothing. I can combine both word lists into one and do a single map-chunks pass. Fruit stems: apple, orange, guava, cherr, blackberr, strawberr, grape, mango, kiwi, peach, plum, persimmon, pomegranate, dates, dragonfruit, blueberr, watermelon, nectarine, lemon, lychee, breadfruit, raspberr, apricot, banana, pineapple, jonagold, envy. Clothing: t-shirt, shoes, hats, hoodie, sweater, glove, shorts, boots, sneaker, coats, ties, dresses, suits, skirt, swimsuit, pajama, socks, jeans, jacket, scarf.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let ([targets '(\"apple\" \"orange\" \"guava\" \"cherr\" \"blackberr\" \"strawberr\" \"grape\" \"mango\" \"kiwi\" \"peach\" \"plum\" \"persimmon\" \"pomegranate\" \"dates\" \"dragonfruit\" \"blueberr\" \"watermelon\" \"nectarine\" \"lemon\" \"lychee\" \"breadfruit\" \"raspberr\" \"apricot\" \"banana\" \"pineapple\" \"jonagold\" \"envy\" \"t-shirt\" \"shoes\" \"hats\" \"hoodie\" \"sweater\" \"glove\" \"shorts\" \"boots\" \"sneaker\" \"coats\" \"ties\" \"dresses\" \"suits\" \"skirt\" \"swimsuit\" \"pajama\" \"socks\" \"jeans\" \"jacket\" \"scarf\")])\n  (apply +\n    (map (lambda (line)\n      (if (and (or (string-contains? line \"I have \")\n                   (string-contains? line \"I also have \"))\n               (not (string-contains? line \"My \")))\n          (let* ([after (or (extract-after line \"I also have \")\n                            (extract-after line \"I have \"))]\n                 [sp (and after (string-index-of after \" \"))]\n                 [num (and sp (> sp 0) (string->number (substr after 0 sp)))])\n            (if (and num (exists (lambda (f) (string-contains? after f)) targets))\n                num 0))\n          0))\n      (split-lines *chunk*))))")
          'map-chunks #t "Processed 5 chunks, results stored as 'map-result"
          "Single pass with combined fruit+clothing word list. More efficient than two separate map-chunks.")
        (make-step
          '(store 'total (apply + (retrieve 'map-result)))
          'store #t "Stored: 664")
        (make-step
          '(submit (retrieve 'total))
          'submit #t "Answer accepted: 664")))

    ;; 37. One-shot inventory counting (compact begin pattern)
    (make-scenario "synth-inventory-oneshot"
      "How many fruits do I have in total?"
      *prompt-inventory*
      '((input chunks 5000) (task text 200))
      20000
      (list
        (make-step
          '(begin
             (map-chunks 'input
               "(let ([fruits '(\"apple\" \"orange\" \"guava\" \"cherr\" \"blackberr\" \"strawberr\" \"grape\" \"mango\" \"kiwi\" \"peach\" \"plum\" \"persimmon\" \"pomegranate\" \"dates\" \"dragonfruit\" \"blueberr\" \"watermelon\" \"nectarine\" \"lemon\" \"lychee\" \"breadfruit\" \"raspberr\" \"apricot\" \"banana\" \"pineapple\" \"jonagold\" \"envy\")])\n  (apply +\n    (map (lambda (line)\n      (if (and (or (string-contains? line \"I have \")\n                   (string-contains? line \"I also have \"))\n               (not (string-contains? line \"My \")))\n          (let* ([after (or (extract-after line \"I also have \")\n                            (extract-after line \"I have \"))]\n                 [sp (and after (string-index-of after \" \"))]\n                 [num (and sp (> sp 0) (string->number (substr after 0 sp)))])\n            (if (and num (exists (lambda (f) (string-contains? after f)) fruits))\n                num 0))\n          0))\n      (split-lines *chunk*))))")
             (store 'total (apply + (retrieve 'map-result)))
             (submit (retrieve 'total)))
          'submit #t "Answer accepted: 501")))

    ;; 38. Count cars I own (different category, exercises generalization)
    (make-scenario "synth-inventory-cars"
      "How many cars do I have in total?"
      *prompt-inventory*
      '((input chunks 5000) (task text 200))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "I have 65 mazda cx-90. My colleague has 92 wireless earbuds. My brother has 84 smartphones. I also have 68 toyota corolla (which need maintenance). I also have 82 magazines. I also have 52 stools (which match the decor). My sister has 59 nissan altima..."
          "Inventory contains car models mixed with other items. Car names are specific makes/models.")
        (make-step
          '(think "Car brands and models to match: toyota, honda, nissan, mazda, hyundai, subaru, chevrolet, ford, bmw, audi, mercedes, volkswagen, porsche, jaguar, volvo, tesla, kia, jeep, lexus. These are distinctive enough that partial matching on the brand name works — no non-car items contain 'toyota' or 'honda'.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let ([cars '(\"toyota\" \"honda\" \"nissan\" \"mazda\" \"hyundai\" \"subaru\" \"chevrolet\" \"ford\" \"bmw\" \"audi\" \"mercedes\" \"volkswagen\" \"porsche\" \"jaguar\" \"volvo\" \"tesla\" \"kia\" \"jeep\" \"lexus\")])\n  (apply +\n    (map (lambda (line)\n      (if (and (or (string-contains? line \"I have \")\n                   (string-contains? line \"I also have \"))\n               (not (string-contains? line \"My \")))\n          (let* ([after (or (extract-after line \"I also have \")\n                            (extract-after line \"I have \"))]\n                 [sp (and after (string-index-of after \" \"))]\n                 [num (and sp (> sp 0) (string->number (substr after 0 sp)))])\n            (if (and num (exists (lambda (f) (string-contains? after f)) cars))\n                num 0))\n          0))\n      (split-lines *chunk*))))")
          'map-chunks #t "Processed 5 chunks, results stored as 'map-result"
          "Car brand names are unambiguous — 'toyota', 'honda' etc. won't match non-car items.")
        (make-step
          '(store 'total (apply + (retrieve 'map-result)))
          'store #t "Stored: 297")
        (make-step
          '(submit (retrieve 'total))
          'submit #t "Answer accepted: 297")))

    ;; 39. Absolute difference between two categories (teaches structured map-chunks returns)
    (make-scenario "synth-inventory-diff"
      "What is the absolute difference between the number of clothing items and furniture items that I have?"
      *prompt-inventory*
      '((input chunks 5000) (task text 200))
      20000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "I have 88 sneakers. I also have 41 pajamas (which need washing). My mother has 46 smartwatches. I also have 22 video game consoles. My uncle has 88 boots. I also have 57 kia niro (which are in the garage). I also have 79 beds. I also have 100 parrots. I also have 15 t-shirts. My boss has 51 sofas..."
          "Mixed inventory. Need separate counts for clothing and furniture.")
        (make-step
          '(think "I need TWO counts: clothing and furniture. I can do a single map-chunks pass that returns (list clothing-sum furniture-sum) per chunk, then aggregate separately. Clothing: t-shirt, shoes, hats, hoodie, sweater, glove, shorts, boots, sneaker, coats, ties, dresses, suits, skirt, swimsuit, pajama, socks, jeans, jacket, scarf. Furniture: bed, desk, table, chair, sofa, rocking chair, bookshelf, dresser, ottoman, recliner, armchair, nightstand, bench, stool, coffee table, dining table.")
          'think #t #f)
        (make-step
          '(map-chunks 'input
            "(let ([clothing '(\"t-shirt\" \"shoes\" \"hats\" \"hoodie\" \"sweater\" \"glove\" \"shorts\" \"boots\" \"sneaker\" \"coats\" \"ties\" \"dresses\" \"suits\" \"skirt\" \"swimsuit\" \"pajama\" \"socks\" \"jeans\" \"jacket\" \"scarf\")]\n       [furniture '(\"beds\" \"bed \" \"desk\" \"table\" \"chair\" \"sofa\" \"rocking chair\" \"bookshel\" \"dresser\" \"ottoman\" \"recliner\" \"armchair\" \"nightstand\" \"bench\" \"stool\" \"coffee table\" \"dining table\")])\n  (let ([extract (lambda (line cats)\n    (if (and (or (string-contains? line \"I have \")\n                 (string-contains? line \"I also have \"))\n             (not (string-contains? line \"My \")))\n        (let* ([after (or (extract-after line \"I also have \")\n                          (extract-after line \"I have \"))]\n               [sp (and after (string-index-of after \" \"))]\n               [num (and sp (> sp 0) (string->number (substr after 0 sp)))])\n          (if (and num (exists (lambda (f) (string-contains? after f)) cats))\n              num 0))\n        0))])\n    (let ([lines (split-lines *chunk*)])\n      (list (apply + (map (lambda (l) (extract l clothing)) lines))\n            (apply + (map (lambda (l) (extract l furniture)) lines))))))")
          'map-chunks #t "Processed 5 chunks, results stored as 'map-result"
          "Returns (clothing-sum furniture-sum) per chunk for separate aggregation.")
        (make-step
          '(begin
             (store 'clothing (apply + (map car (retrieve 'map-result))))
             (store 'furniture (apply + (map cadr (retrieve 'map-result)))))
          'store #t "Stored: clothing=387, furniture=348")
        (make-step
          '(submit (abs (- (retrieve 'clothing) (retrieve 'furniture))))
          'submit #t "Answer accepted: 39")))

    ;; ---- Domain 12: Mathador (Intellect3-style arithmetic puzzles) ----

    ;; 40. Simple mathador (3 numbers → target)
    (make-scenario "synth-mathador-simple"
      "From the numbers [3, 6, 7], combine them with +, -, *, / to produce 24."
      *prompt-iterative*
      '((task text 100))
      10000
      (list
        (make-step
          '(think "I need to make 24 from 3, 6, 7. Let me try: (7 - 3) * 6 = 4 * 6 = 24. That works! In Scheme prefix notation: (* (- 7 3) 6)")
          'think #t #f)
        (make-step
          '(eval (* (- 7 3) 6))
          'eval #t "24"
          "Expression evaluates to 24. Uses all three numbers.")
        (make-step
          '(submit 24)
          'submit #t "Answer accepted: 24")))

    ;; 41. Harder mathador (4 numbers → target)
    (make-scenario "synth-mathador-4num"
      "From the numbers [1, 3, 4, 9], combine them with +, -, *, / to produce 26."
      *prompt-iterative*
      '((task text 100))
      10000
      (list
        (make-step
          '(think "Need 26 from 1, 3, 4, 9. Let me try: (9 + 4) * (3 - 1) = 13 * 2 = 26. Yes! Scheme: (* (+ 9 4) (- 3 1))")
          'think #t #f)
        (make-step
          '(eval (* (+ 9 4) (- 3 1)))
          'eval #t "26"
          "All four numbers used: 9+4=13, 3-1=2, 13*2=26.")
        (make-step
          '(submit 26)
          'submit #t "Answer accepted: 26")))

    ;; 42. Mathador with trial and error (teaches recovery from wrong attempt)
    (make-scenario "synth-mathador-retry"
      "From the numbers [2, 5, 6, 7, 7], combine them with +, -, *, / to produce 28."
      *prompt-iterative*
      '((task text 100))
      10000
      (list
        (make-step
          '(think "Need 28 from 2, 5, 6, 7, 7. First try: 7 * 7 = 49, too big. Maybe 7 * (6 - 2) = 28! But that doesn't use 5 and 7. Let me try: (7 + 7) * 2 = 28, and I haven't used 5, 6. That's OK if I don't need all numbers. Scheme: (* (+ 7 7) 2)")
          'think #t #f)
        (make-step
          '(eval (* (+ 7 7) 2))
          'eval #t "28"
          "Confirmed: (7+7)*2 = 14*2 = 28.")
        (make-step
          '(submit 28)
          'submit #t "Answer accepted: 28")))

    ;; --- Competition Math ---

    (make-scenario "synth-math-gcd"
      "Find the greatest common divisor of 1071 and 462."
      *prompt-competition-math*
      '((task text 100))
      5000
      (list
        (make-step
          '(think "GCD of 1071 and 462. Can use Euclid's algorithm or just call gcd directly.")
          'think #t #f)
        (make-step
          '(eval (gcd 1071 462))
          'eval #t "21"
          "gcd(1071, 462) = 21. Verified: 1071/21 = 51, 462/21 = 22.")
        (make-step
          '(submit 21)
          'submit #t "Answer accepted: 21")))

    (make-scenario "synth-math-modular"
      "Find the remainder when 2^100 is divided by 7."
      *prompt-competition-math*
      '((task text 100))
      8000
      (list
        (make-step
          '(think "Need 2^100 mod 7. Fermat's little theorem: 2^6 ≡ 1 (mod 7) since gcd(2,7)=1. So 2^100 = 2^(6*16 + 4) = (2^6)^16 * 2^4 ≡ 1^16 * 16 ≡ 16 mod 7 ≡ 2.")
          'think #t #f)
        (make-step
          '(eval (modulo (expt 2 100) 7))
          'eval #t "2"
          "Confirmed: 2^100 mod 7 = 2. Matches Fermat's little theorem prediction.")
        (make-step
          '(submit 2)
          'submit #t "Answer accepted: 2")))

    (make-scenario "synth-math-combinatorics"
      "How many ways can 5 people sit in a row of 5 chairs?"
      *prompt-competition-math*
      '((task text 100))
      5000
      (list
        (make-step
          '(think "5 people in 5 chairs = 5! = 120. Straightforward factorial.")
          'think #t #f)
        (make-step
          '(eval (apply * (iota 5 1)))
          'eval #t "120"
          "5! = 1*2*3*4*5 = 120.")
        (make-step
          '(submit 120)
          'submit #t "Answer accepted: 120")))

    (make-scenario "synth-math-quadratic"
      "Find the positive root of x^2 - 5x + 6 = 0."
      *prompt-competition-math*
      '((task text 100))
      8000
      (list
        (make-step
          '(think "x^2 - 5x + 6 = 0. Factor: (x-2)(x-3) = 0. Roots are 2 and 3. Both positive. Larger is 3. Wait — it says 'positive root'. Both are positive. Need to re-read: 'the positive root'. If they mean the larger one, it's 3. Let me just compute both and report the larger.")
          'think #t #f)
        (make-step
          '(begin
             (store 'disc (- (* 5 5) (* 4 6)))
             (store 'root1 (/ (+ 5 (sqrt (retrieve 'disc))) 2))
             (store 'root2 (/ (- 5 (sqrt (retrieve 'disc))) 2)))
          'begin #t "Stored disc=1, root1=3.0, root2=2.0"
          "Discriminant = 25-24 = 1. Roots: (5+1)/2 = 3, (5-1)/2 = 2.")
        (make-step
          '(submit 3)
          'submit #t "Answer accepted: 3")))

    (make-scenario "synth-math-series"
      "Find the sum of the first 100 positive integers."
      *prompt-competition-math*
      '((task text 100))
      5000
      (list
        (make-step
          '(think "Sum from 1 to 100 = n(n+1)/2 = 100*101/2 = 5050. Can also verify with apply +.")
          'think #t #f)
        (make-step
          '(eval (/ (* 100 101) 2))
          'eval #t "5050"
          "Gauss formula: 100*101/2 = 5050.")
        (make-step
          '(submit 5050)
          'submit #t "Answer accepted: 5050")))

    (make-scenario "synth-math-prime-check"
      "How many prime numbers are there between 10 and 30 (inclusive)?"
      *prompt-competition-math*
      '((task text 100))
      8000
      (list
        (make-step
          '(think "Primes between 10 and 30: 11, 13, 17, 19, 23, 29. That's 6. Let me verify by writing a primality checker.")
          'think #t #f)
        (make-step
          '(eval
             (let ([prime? (lambda (n)
                     (and (> n 1)
                          (let loop ([d 2])
                            (cond [(> (* d d) n) #t]
                                  [(zero? (modulo n d)) #f]
                                  [else (loop (+ d 1))]))))])
               (length (filter prime? (iota 21 10)))))
          'eval #t "6"
          "6 primes: 11, 13, 17, 19, 23, 29. Verified by trial division.")
        (make-step
          '(submit 6)
          'submit #t "Answer accepted: 6")))

    ;; --- Ranking/Top-K ---

    (make-scenario "synth-rank-topk"
      "Find the top 3 records by value where type is 'alpha' and region is 'north'. Report the sum of their values."
      *prompt-ranking*
      '((input chunks 50000))
      10000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\nRECORD 0002 | type: beta | region: south | value: 88\n..."
          "Data has RECORD format with type, region, value fields.")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n  (filter pair?\n    (map (lambda (line)\n      (if (and (string-contains? line \"type: alpha\")\n               (string-contains? line \"region: north\"))\n          (let ([v (extract-after line \"value: \")])\n            (if v (cons line (string->number v)) #f))\n          #f))\n      lines)))")
          'map-chunks #t "((\"RECORD 0015 ...\" . 97) (\"RECORD 0042 ...\" . 85) ...)"
          "Extracted (line . value) pairs for alpha+north records from all chunks.")
        (make-step
          '(begin
             (store 'all (apply append (retrieve 'map-result)))
             (store 'sorted-desc (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                                            (retrieve 'all)))
             (store 'top3 (let ([s (retrieve 'sorted-desc)])
                            (if (> (length s) 3)
                                (list (first s) (second s) (third s))
                                s))))
          'begin #t "Stored all, sorted-desc, top3"
          "Merged per-chunk results, sorted descending, took top 3.")
        (make-step
          '(eval (apply + (map cdr (retrieve 'top3))))
          'eval #t "267"
          "Sum of top 3 values: 97 + 85 + 85 = 267.")
        (make-step
          '(submit 267)
          'submit #t "Answer accepted: 267")))

    (make-scenario "synth-rank-median"
      "Find the median value of all 'beta' records in the 'east' region."
      *prompt-ranking*
      '((input chunks 50000))
      10000
      (list
        (make-step
          '(peek 'input 300)
          'peek #t "RECORD 0001 | type: alpha | region: north | value: 42\n..."
          "Standard RECORD format. Need to filter beta+east.")
        (make-step
          '(map-chunks 'input
            "(let ([lines (split-lines *chunk*)])\n  (filter number?\n    (map (lambda (line)\n      (if (and (string-contains? line \"type: beta\")\n               (string-contains? line \"region: east\"))\n          (let ([v (extract-after line \"value: \")])\n            (if v (string->number v) #f))\n          #f))\n      lines)))")
          'map-chunks #t "(42 88 15 67 ...)"
          "Extracted numeric values for beta+east records.")
        (make-step
          '(begin
             (store 'values (apply append (retrieve 'map-result)))
             (store 'sorted-vals (list-sort < (retrieve 'values)))
             (store 'n (length (retrieve 'sorted-vals))))
          'begin #t "Stored values, sorted-vals, n"
          "Collected and sorted all beta+east values.")
        (make-step
          '(eval (let* ([vals (retrieve 'sorted-vals)]
                        [n (retrieve 'n)]
                        [mid (quotient n 2)])
                   (if (odd? n)
                       (list-ref vals mid)
                       (/ (+ (list-ref vals (- mid 1))
                             (list-ref vals mid))
                          2))))
          'eval #t "55"
          "Median of sorted values. Even count → average of two middle values.")
        (make-step
          '(submit 55)
          'submit #t "Answer accepted: 55")))

    ;; --- Multi-hop QA ---

    (make-scenario "synth-multihop-birth-year"
      "In what year was the author of 'The Great Gatsby' born?"
      *prompt-multi-hop-qa*
      '((input chunks 5000))
      8000
      (list
        (make-step
          '(think "Need two facts: (1) who wrote The Great Gatsby, (2) when were they born. Let me grep for 'Great Gatsby' first.")
          'think #t #f)
        (make-step
          '(grep 'input "Great Gatsby" 5)
          'grep #t "The Great Gatsby is a novel by F. Scott Fitzgerald, published in 1925..."
          "Found: The Great Gatsby was written by F. Scott Fitzgerald.")
        (make-step
          '(grep 'input "Fitzgerald" 5)
          'grep #t "F. Scott Fitzgerald was born on September 24, 1896 in St. Paul, Minnesota..."
          "Found: Fitzgerald born September 24, 1896.")
        (make-step
          '(submit 1896)
          'submit #t "Answer accepted: 1896")))

    (make-scenario "synth-multihop-population"
      "What is the population of the capital of France?"
      *prompt-multi-hop-qa*
      '((input chunks 5000))
      8000
      (list
        (make-step
          '(grep 'input "capital of France" 5)
          'grep #t "Paris is the capital and most populous city of France, situated on the Seine..."
          "Capital of France = Paris.")
        (make-step
          '(grep 'input "Paris population" 5)
          'grep #t "As of the 2021 census, Paris had a population of 2,165,423 within its administrative limits..."
          "Paris population: 2,165,423.")
        (make-step
          '(submit 2165423)
          'submit #t "Answer accepted: 2165423")))

    (make-scenario "synth-multihop-comparison"
      "Which city has more people: the birthplace of Mozart or the birthplace of Beethoven?"
      *prompt-multi-hop-qa*
      '((input chunks 8000))
      10000
      (list
        (make-step
          '(think "Need: (1) Mozart's birthplace, (2) Beethoven's birthplace, (3) population of each. Two-hop for each composer.")
          'think #t #f)
        (make-step
          '(grep 'input "Mozart born" 5)
          'grep #t "Wolfgang Amadeus Mozart was born on 27 January 1756 in Salzburg..."
          "Mozart born in Salzburg.")
        (make-step
          '(grep 'input "Beethoven born" 5)
          'grep #t "Ludwig van Beethoven was born on 17 December 1770 in Bonn..."
          "Beethoven born in Bonn.")
        (make-step
          '(begin
             (grep 'input "Salzburg population" 5)
             (store 'salzburg-pop 157644))
          'begin #t "Stored salzburg-pop=157644"
          "Salzburg population ~157,644.")
        (make-step
          '(begin
             (grep 'input "Bonn population" 5)
             (store 'bonn-pop 336465))
          'begin #t "Stored bonn-pop=336465"
          "Bonn population ~336,465.")
        (make-step
          '(think "Bonn (336,465) > Salzburg (157,644). Beethoven's birthplace is more populous.")
          'think #t #f)
        (make-step
          '(submit "Bonn")
          'submit #t "Answer accepted: Bonn")))

    ;; --- Science Reasoning ---

    (make-scenario "synth-science-speed"
      "A car travels 150 km in 2.5 hours. What is its average speed in km/h?"
      *prompt-competition-math*
      '((task text 100))
      5000
      (list
        (make-step
          '(think "Speed = distance / time = 150 / 2.5 = 60 km/h.")
          'think #t #f)
        (make-step
          '(eval (/ 150 2.5))
          'eval #t "60.0"
          "150 km / 2.5 hours = 60 km/h.")
        (make-step
          '(submit 60)
          'submit #t "Answer accepted: 60")))

    (make-scenario "synth-science-density"
      "A block of iron has mass 790 g and volume 100 cm^3. What is its density in g/cm^3?"
      *prompt-competition-math*
      '((task text 100))
      5000
      (list
        (make-step
          '(think "Density = mass / volume = 790 / 100 = 7.9 g/cm^3.")
          'think #t #f)
        (make-step
          '(eval (/ 790 100))
          'eval #t "79/10"
          "790/100 = 79/10 = 7.9 g/cm^3. Exact fraction preserved.")
        (make-step
          '(submit 7.9)
          'submit #t "Answer accepted: 7.9")))

    (make-scenario "synth-science-energy"
      "Calculate the kinetic energy of a 2 kg object moving at 10 m/s. Use KE = 0.5 * m * v^2."
      *prompt-competition-math*
      '((task text 100))
      5000
      (list
        (make-step
          '(think "KE = 0.5 * m * v^2 = 0.5 * 2 * 100 = 100 J.")
          'think #t #f)
        (make-step
          '(eval (* 1/2 2 (expt 10 2)))
          'eval #t "100"
          "KE = 0.5 * 2 * 10^2 = 100 J. Used exact 1/2 fraction.")
        (make-step
          '(submit 100)
          'submit #t "Answer accepted: 100")))

    ;; --- Reading Comprehension (DROP-style numeric extraction) ---

    (make-scenario "synth-drop-diff"
      "How many more touchdowns did Team A score than Team B?"
      *prompt-multi-hop-qa*
      '((input chunks 5000))
      8000
      (list
        (make-step
          '(peek 'input 500)
          'peek #t "Game Summary: In the first quarter, Team A scored a 14-yard rushing TD. Team B kicked a 32-yard field goal..."
          "Game summary passage. Need to count TDs per team.")
        (make-step
          '(grep 'input "Team A" 10)
          'grep #t "Team A scored a 14-yard rushing TD.\nTeam A scored on a 7-yard pass TD.\nTeam A kicked a 45-yard field goal.\nTeam A scored a 1-yard QB sneak TD."
          "Team A: 3 TDs (rushing, pass, QB sneak) + 1 field goal.")
        (make-step
          '(grep 'input "Team B" 10)
          'grep #t "Team B kicked a 32-yard field goal.\nTeam B scored a 28-yard interception return TD.\nTeam B kicked a 50-yard field goal."
          "Team B: 1 TD (interception return) + 2 field goals.")
        (make-step
          '(think "Team A: 3 TDs. Team B: 1 TD. Difference: 3 - 1 = 2.")
          'think #t #f)
        (make-step
          '(submit 2)
          'submit #t "Answer accepted: 2")))

    (make-scenario "synth-drop-percentage"
      "What percent of the population were not Hispanic or Latino?"
      *prompt-multi-hop-qa*
      '((input chunks 5000))
      8000
      (list
        (make-step
          '(grep 'input "Hispanic" 5)
          'grep #t "The racial makeup of the county was 89.2% White, 5.1% Black, 0.3% Native American. Hispanic or Latino of any race were 8.7% of the population."
          "Hispanic or Latino = 8.7% of population.")
        (make-step
          '(eval (- 100 8.7))
          'eval #t "91.3"
          "100% - 8.7% = 91.3% were not Hispanic or Latino.")
        (make-step
          '(submit 91.3)
          'submit #t "Answer accepted: 91.3")))

    ))

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
    (step . ,step-num)
    (phase . "act")))

(define *synth-mechanical-actions*
  '(submit store load plan! journal memorize remember recall))

(define (synth-make-reflect-example action obs-value note label step-num)
  ;; Generates a reflect-phase training example.
  ;; Input: reflection prompt (action + observation).
  ;; Output: concise note.
  (let* ([action-str (synth-format-action action)]
         [obs-str (if (string? obs-value)
                      (if (> (string-length obs-value) 500)
                          (string-append (substring obs-value 0 497) "...")
                          obs-value)
                      (format "~a" obs-value))]
         [reflect-prompt (string-append
                           "Distill the following step into a single concise note "
                           "(one sentence, max 120 chars). "
                           "The note should capture what was learned or accomplished.\n\n"
                           "Action: " action-str "\n\n"
                           "Result: " obs-str "\n\n"
                           "Note:")])
    `((messages . (((role . "user") (content . ,reflect-prompt))
                   ((role . "assistant") (content . ,note))))
      (source . ,label)
      (step . ,step-num)
      (phase . "reflect"))))

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
                 [obs-note (cdr (assq 'obs-note step-data))]
                 [hud (rlm2-render-state state *synth-context-budget*)]
                 [action-str (synth-format-action action)]
                 ;; Act-phase example (always generated)
                 [act-example (synth-make-example
                                sys-prompt hud action-str
                                label (rlm2-state-step state))]
                 ;; Reflect-phase example (when note provided and action is non-mechanical)
                 [action-type (if (pair? action) (car action) 'unknown)]
                 [mechanical? (memq action-type *synth-mechanical-actions*)]
                 [reflect-example
                   (and obs-note
                        (not mechanical?)
                        (not (eq? action-type 'think))
                        (synth-make-reflect-example
                          action obs-value obs-note
                          label (rlm2-state-step state)))]
                 [new-examples (if reflect-example
                                   (list act-example reflect-example)
                                   (list act-example))]
                 [state* (synth-advance-state
                           state action obs-type obs-ok obs-value)])
            (loop (cdr remaining) state*
                  (append (reverse new-examples) examples)))))))

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

    ;; Report distribution
    (let* ([act-count (length (filter (lambda (e)
                                        (let ([p (assq 'phase e)])
                                          (and p (string=? (cdr p) "act"))))
                                      all-examples))]
           [reflect-count (length (filter (lambda (e)
                                            (let ([p (assq 'phase e)])
                                              (and p (string=? (cdr p) "reflect"))))
                                          all-examples))])
      (display (format "\nTotal: ~a training examples (~a act, ~a reflect)\n"
                       (length all-examples) act-count reflect-count)))

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
