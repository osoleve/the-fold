;;; user/rlm/bench-v1-v2.ss — RLM v1 vs v2 Benchmark
;;;
;;; Runs identical OOLONG tasks through both v1 and v2 drivers,
;;; collects metrics, and produces a side-by-side comparison.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/bench-v1-v2.ss

(unless (getenv "RLM_INTEGRATION")
  (display "Skipping benchmark (set RLM_INTEGRATION=1 to enable)\n")
  (exit 0))

;;; Load both drivers
(load "boundary/pipeline/rlm-loop.ss")   ; v1
(load "boundary/pipeline/rlm2-drive.ss") ; v2

;;; ====
;;; Shared Task Generation
;;; ====

(define *filler-paragraphs*
  '#("The process of photosynthesis converts carbon dioxide and water into glucose and oxygen using sunlight. Chlorophyll in plant cells absorbs light energy, which drives the light-dependent reactions in the thylakoid membranes. The Calvin cycle then uses the products of these reactions to fix carbon into organic molecules."

     "Medieval European castles evolved from simple wooden motte-and-bailey constructions to sophisticated stone fortifications over several centuries. Concentric castle design, with multiple rings of walls, became common after the Crusades introduced Western Europeans to Byzantine and Islamic military architecture."

     "The preparation of a proper French bechamel sauce begins with a roux: equal parts butter and flour cooked together until the raw flour taste disappears. Whole milk is then added gradually while whisking constantly to prevent lumps. The sauce should simmer for at least twenty minutes to reach the proper consistency."

     "The Mariana Trench in the western Pacific Ocean reaches a depth of approximately 11,034 meters at its deepest point, the Challenger Deep. The water pressure at this depth exceeds 1,000 atmospheres, creating an environment that few organisms can survive in without specialized adaptations."

     "Quantum entanglement occurs when pairs of particles interact in ways that make the quantum state of each particle dependent on the state of the other, regardless of the distance between them. Einstein famously described this phenomenon as spooky action at a distance, questioning the completeness of quantum mechanics."

     "The Fibonacci sequence appears throughout nature in surprising ways. The arrangement of leaves around a stem, the spiral pattern of seeds in a sunflower head, and the branching of trees all follow Fibonacci numbers. This pattern optimizes the plant's exposure to sunlight and rain."

     "Traditional Japanese pottery techniques have been refined over more than a thousand years. The wabi-sabi aesthetic embraces imperfection, finding beauty in irregular shapes, asymmetry, and the marks left by the firing process. Raku pottery involves removing pieces from the kiln while still glowing hot."

     "Continental drift theory, first proposed by Alfred Wegener in 1912, was initially rejected by most geologists. It was not until the discovery of seafloor spreading in the 1960s that the theory gained widespread acceptance, eventually evolving into the modern theory of plate tectonics."

     "The London Underground, opened in 1863, was the world's first underground railway. The Metropolitan Railway initially used steam locomotives, which filled the tunnels with smoke and soot. The system gradually converted to electric traction starting in 1890 with the City and South London Railway."

     "Bayesian statistics provides a framework for updating probability estimates as new evidence becomes available. Unlike frequentist approaches, Bayesian methods explicitly incorporate prior knowledge through prior probability distributions, which are updated via Bayes' theorem to produce posterior distributions."))

(define *n-fillers* (vector-length *filler-paragraphs*))

(define *types* '#("alpha" "beta" "gamma" "delta"))
(define *regions* '#("north" "south" "east" "west"))
(define *target-type* "alpha")
(define *target-region* "north")

(define (make-entry id type region value)
  (format "RECORD ~4,'0d | type: ~a | region: ~a | value: ~a"
          id type region value))

(define (generate-entries n)
  (random-seed 42)
  (let loop ([i 0] [entries '()])
    (if (>= i n)
        (reverse entries)
        (let* ([type (vector-ref *types* (random (vector-length *types*)))]
               [region (vector-ref *regions* (random (vector-length *regions*)))]
               [value (+ 10 (random 91))]
               [text (make-entry (+ i 1) type region value)])
          (loop (+ i 1) (cons (list text value type region) entries))))))

(define (compute-answer entries target-type target-region)
  (let loop ([es entries] [total 0])
    (if (null? es)
        total
        (let* ([e (car es)]
               [val (cadr e)]
               [typ (caddr e)]
               [reg (cadddr e)])
          (if (and (string=? typ target-type) (string=? reg target-region))
              (loop (cdr es) (+ total val))
              (loop (cdr es) total))))))

(define (count-matching entries target-type target-region)
  (let loop ([es entries] [n 0])
    (if (null? es)
        n
        (let* ([e (car es)]
               [typ (caddr e)]
               [reg (cadddr e)])
          (if (and (string=? typ target-type) (string=? reg target-region))
              (loop (cdr es) (+ n 1))
              (loop (cdr es) n))))))

(define (group-entries entry-texts block-size)
  (let loop ([texts entry-texts] [current '()] [groups '()])
    (cond
      [(null? texts)
       (reverse (if (null? current) groups (cons (reverse current) groups)))]
      [(>= (length current) block-size)
       (loop texts '() (cons (reverse current) groups))]
      [else
       (loop (cdr texts) (cons (car texts) current) groups)])))

(define (join-entries entries)
  (apply string-append (map (lambda (e) (string-append e "\n")) entries)))

(define (join-paragraphs paragraphs)
  (let loop ([ps paragraphs] [acc ""])
    (if (null? ps)
        acc
        (loop (cdr ps)
              (string-append acc (car ps) "\n\n")))))

(define (build-oolong-haystack entries target-size)
  (let* ([entry-block-size 5]
         [entry-blocks (group-entries (map car entries) entry-block-size)])
    (let loop ([size 0] [paras '()] [filler-idx 0]
               [eblocks entry-blocks] [paras-since-entry 0])
      (cond
        [(and (>= size target-size) (null? eblocks))
         (join-paragraphs (reverse paras))]
        [(and (pair? eblocks) (>= paras-since-entry 8))
         (let ([block-text (string-append
                             "--- DATA RECORDS ---\n"
                             (join-entries (car eblocks))
                             "--- END RECORDS ---")])
           (loop (+ size (string-length block-text) 2)
                 (cons block-text paras)
                 filler-idx
                 (cdr eblocks)
                 0))]
        [else
         (let ([para (vector-ref *filler-paragraphs* (modulo filler-idx *n-fillers*))])
           (loop (+ size (string-length para) 2)
                 (cons para paras)
                 (+ filler-idx 1)
                 eblocks
                 (+ paras-since-entry 1)))]))))

;;; output-contains-number? : String -> Nat -> Bool
(define (output-contains-number? output expected)
  (let* ([expected-str (number->string expected)]
         [out-len (string-length output)]
         [exp-len (string-length expected-str)])
    (if (> exp-len out-len)
        #f
        (let loop ([i 0])
          (cond
            [(> (+ i exp-len) out-len) #f]
            [(and (string=? (substring output i (+ i exp-len)) expected-str)
                  (or (= i 0) (not (char-numeric? (string-ref output (- i 1)))))
                  (or (= (+ i exp-len) out-len)
                      (not (char-numeric? (string-ref output (+ i exp-len))))))
             #t]
            [else (loop (+ i 1))])))))

;;; ====
;;; Version-Specific System Prompts
;;; ====

(define *v1-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to find and aggregate "
    "specific data from a large document.\n\n"
    "The document is stored in your environment under 'input'. It contains "
    "structured RECORD entries scattered among other text. Each record has:\n"
    "  RECORD NNNN | type: <type> | region: <region> | value: <number>\n\n"
    "IMPORTANT: grep only returns top-k matches, NOT all matches. "
    "For aggregation tasks, you MUST use map-chunks to process every chunk.\n\n"
    "Strategy:\n"
    "1. Peek at the input to understand its structure\n"
    "2. Use map-chunks to process ALL chunks with a per-chunk expression:\n"
    "   (rlm-env-map-chunks 'input \"(expression using *chunk*)\")\n"
    "   This runs the expression for each chunk with *chunk* bound to its text.\n"
    "   Results are auto-stored in 'map-result as a list.\n"
    "3. Sum the results: (apply + (rlm-env-get 'map-result))\n"
    "4. Store final answer: (rlm-env-put! 'answer total)\n\n"
    "Example map-chunks expression for summing matching values:\n"
    "  (rlm-env-map-chunks 'input\n"
    "    \"(let ([lines (rlm-split-lines *chunk*)])\n"
    "       (apply + (map (lambda (line)\n"
    "                       (if (and (rlm-string-contains? line \\\"type: alpha\\\")\n"
    "                                (rlm-string-contains? line \\\"region: north\\\"))\n"
    "                           (let ([v (rlm-extract-after line \\\"value: \\\")])\n"
    "                             (if v (string->number v) 0))\n"
    "                           0))\n"
    "                     lines)))\")\n\n"
    "Useful functions available in the worker:\n"
    "  (rlm-split-lines s)           — split string into list of lines\n"
    "  (rlm-string-contains? s sub)  — check if s contains sub\n"
    "  (rlm-extract-after s marker)  — get text after marker in s\n\n"
    "Be precise. Return only the numeric total.\n"))

(define *v2-system-prompt*
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
    "  (split-lines s)            — split string into list of lines\n"
    "  (string-contains? s sub)   — check if s contains sub\n"
    "  (extract-after s marker)   — get text after marker in s\n\n"
    "Be precise. Return only the numeric total.\n"))

;;; ====
;;; Timing Utility
;;; ====

(define (wall-clock-ms thunk)
  (let* ([t0 (current-time)]
         [result (thunk)]
         [t1 (current-time)]
         [ms (+ (* 1000 (- (time-second t1) (time-second t0)))
                (quotient (- (time-nanosecond t1) (time-nanosecond t0))
                          1000000))])
    (values result ms)))

;;; ====
;;; Benchmark Runner
;;; ====

;;; run-single-benchmark : Nat -> Nat -> String -> RlmProvider -> Alist
;;; Generate task, run both versions, return comparison alist.
(define (run-single-benchmark n-entries target-size label provider)
  (let* ([entries (generate-entries n-entries)]
         [haystack (build-oolong-haystack entries target-size)]
         [expected (compute-answer entries *target-type* *target-region*)]
         [match-count (count-matching entries *target-type* *target-region*)]
         [task (format "Find the total sum of all 'value' fields from records where type is '~a' AND region is '~a'. Report only the numeric total."
                       *target-type* *target-region*)]
         [max-steps 12]
         [max-fuel 20000]
         [chunk-size 2000])

    (display (format "\n=== BENCHMARK: ~a ===\n" label))
    (display (format "Entries: ~a (~a matching) | Haystack: ~a chars | Expected: ~a\n"
                     n-entries match-count (string-length haystack) expected))
    (flush-output-port)

    ;; --- V1 Run ---
    (display "\n--- Running v1 ---\n")
    (flush-output-port)
    (let-values ([(v1-result v1-ms)
                  (wall-clock-ms
                   (lambda ()
                     (let ([config (make-rlm-config
                                    provider *v1-system-prompt*
                                    max-steps max-fuel chunk-size
                                    1    ; max-depth
                                    3)]) ; loop-window
                       (rlm-run config task haystack))))])
      (let* ([v1-status (rlm-run-result-status v1-result)]
             [v1-output (format "~a" (rlm-run-result-output v1-result))]
             [v1-correct? (output-contains-number? v1-output expected)]
             [v1-traj (rlm-run-result-trajectory-hash v1-result)])
        (display (format "  Status: ~a | Time: ~a ms | Correct: ~a\n"
                         v1-status v1-ms v1-correct?))
        (display (format "  Output: ~a\n" (if (> (string-length v1-output) 200)
                                               (string-append (substring v1-output 0 200) "...")
                                               v1-output)))
        (flush-output-port)

        ;; --- V2 Run ---
        (display "\n--- Running v2 ---\n")
        (flush-output-port)
        (let-values ([(v2-result v2-ms)
                      (wall-clock-ms
                       (lambda ()
                         (let ([config (make-rlm2-config
                                         provider *v2-system-prompt*
                                         max-steps max-fuel chunk-size
                                         1     ; max-depth
                                         3     ; loop-window
                                         8000  ; context-budget
                                         #f)]) ; no verifier
                           (rlm2-run config task haystack))))])
          (let* ([v2-status (rlm2-run-result-status v2-result)]
                 [v2-output (format "~a" (rlm2-run-result-output v2-result))]
                 [v2-correct? (output-contains-number? v2-output expected)]
                 [v2-traj (rlm2-run-result-trajectory-hash v2-result)])
            (display (format "  Status: ~a | Time: ~a ms | Correct: ~a\n"
                             v2-status v2-ms v2-correct?))
            (display (format "  Output: ~a\n" (if (> (string-length v2-output) 200)
                                                   (string-append (substring v2-output 0 200) "...")
                                                   v2-output)))
            (flush-output-port)

            ;; Build comparison record
            `((label . ,label)
              (n-entries . ,n-entries)
              (haystack-chars . ,(string-length haystack))
              (expected . ,expected)
              (match-count . ,match-count)
              (v1 . ((status . ,v1-status)
                     (time-ms . ,v1-ms)
                     (correct . ,v1-correct?)
                     (output . ,v1-output)
                     (trajectory . ,v1-traj)))
              (v2 . ((status . ,v2-status)
                     (time-ms . ,v2-ms)
                     (correct . ,v2-correct?)
                     (output . ,v2-output)
                     (trajectory . ,v2-traj))))))))))

;;; ====
;;; Report
;;; ====

(define (print-comparison results)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              RLM v1 vs v2 BENCHMARK RESULTS                ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  (display (format "~30a ~15a ~15a\n" "" "v1" "v2"))
  (display (make-string 60 #\─))
  (display "\n")
  (for-each
   (lambda (r)
     (let ([label (cdr (assq 'label r))]
           [v1 (cdr (assq 'v1 r))]
           [v2 (cdr (assq 'v2 r))]
           [expected (cdr (assq 'expected r))])
       (display (format "\n~a (expected: ~a)\n" label expected))
       (display (format "  ~28a ~15a ~15a\n" "Status"
                        (cdr (assq 'status v1))
                        (cdr (assq 'status v2))))
       (display (format "  ~28a ~15a ~15a\n" "Time (ms)"
                        (cdr (assq 'time-ms v1))
                        (cdr (assq 'time-ms v2))))
       (display (format "  ~28a ~15a ~15a\n" "Correct?"
                        (if (cdr (assq 'correct v1)) "YES" "NO")
                        (if (cdr (assq 'correct v2)) "YES" "NO")))))
   results)
  (display "\n")

  ;; Summary
  (let* ([v1-correct (length (filter (lambda (r)
                                       (cdr (assq 'correct (cdr (assq 'v1 r)))))
                                     results))]
         [v2-correct (length (filter (lambda (r)
                                       (cdr (assq 'correct (cdr (assq 'v2 r)))))
                                     results))]
         [n (length results)]
         [v1-total-ms (apply + (map (lambda (r)
                                      (cdr (assq 'time-ms (cdr (assq 'v1 r)))))
                                    results))]
         [v2-total-ms (apply + (map (lambda (r)
                                      (cdr (assq 'time-ms (cdr (assq 'v2 r)))))
                                    results))])
    (display (format "\nSummary: v1 ~a/~a correct (~a ms total) | v2 ~a/~a correct (~a ms total)\n"
                     v1-correct n v1-total-ms v2-correct n v2-total-ms))))

;;; ====
;;; Main
;;; ====

(define (run-benchmark-suite)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [provider (rlm-provider-vllm model-id port)])

    (display (format "Model: ~a | Port: ~a\n" model-id port))
    (display "Starting benchmark suite...\n")
    (flush-output-port)

    (let* ([r1 (run-single-benchmark 100 50000
                 "OOLONG 100 entries / 50K" provider)]
           [r2 (run-single-benchmark 200 100000
                 "OOLONG 200 entries / 100K" provider)])
      (print-comparison (list r1 r2))

      ;; Store results as sexp for analysis
      (let ([results-file (format "user/rlm/bench-results-~a.sexp"
                                  (rlm2-current-iso8601))])
        (call-with-output-file results-file
          (lambda (port)
            (pretty-print `(benchmark-results
                            (model ,model-id)
                            (timestamp ,(rlm2-current-iso8601))
                            (runs ,(list r1 r2)))
                          port)))
        (display (format "\nResults saved to ~a\n" results-file))))))

(run-benchmark-suite)
