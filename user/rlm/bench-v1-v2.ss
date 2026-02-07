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
;;; OOLONG-Pairs Task Generation
;;; ====

(define *pairs-categories* '#("science" "history" "sports" "technology" "arts"))
(define *n-pairs-categories* (vector-length *pairs-categories*))
(define *cutoff-day* 152)  ; ~June 1
(define *condition-a-cat* "science")
(define *condition-b-cat* "technology")

(define (day->date-string day)
  (let* ([month-days '#(31 28 31 30 31 30 31 31 30 31 30 31)]
         [month-names '#("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12")])
    (let loop ([m 0] [remaining day])
      (if (or (>= m 12) (<= remaining (vector-ref month-days m)))
          (format "2025-~a-~2,'0d"
                  (vector-ref month-names (min m 11))
                  (max 1 remaining))
          (loop (+ m 1) (- remaining (vector-ref month-days m)))))))

(define (generate-pairs-entries n-users entries-per-user)
  (random-seed 42)
  (let loop ([u 0] [entries '()])
    (if (>= u n-users)
        (reverse entries)
        (let ([user-id (format "U~3,'0d" (+ u 1))])
          (let inner ([e 0] [entries entries])
            (if (>= e entries-per-user)
                (loop (+ u 1) entries)
                (let* ([day (+ 1 (random 365))]
                       [cat-idx (random *n-pairs-categories*)]
                       [category (vector-ref *pairs-categories* cat-idx)]
                       [text (format "ENTRY | user: ~a | date: ~a | category: ~a"
                                     user-id (day->date-string day) category)])
                  (inner (+ e 1)
                         (cons (list user-id day category text) entries)))))))))

(define (pairs-deduplicate lst)
  (let loop ([l lst] [seen '()] [acc '()])
    (if (null? l) (reverse acc)
        (if (member (car l) seen)
            (loop (cdr l) seen acc)
            (loop (cdr l) (cons (car l) seen) (cons (car l) acc))))))

(define (compute-condition-set entries target-cat temporal-dir cutoff)
  (let loop ([es entries] [users '()])
    (if (null? es)
        (list-sort string<? (pairs-deduplicate users))
        (let* ([e (car es)]
               [uid (car e)]
               [day (cadr e)]
               [cat (caddr e)]
               [date-match? (if (eq? temporal-dir 'before)
                                (< day cutoff)
                                (> day cutoff))]
               [cat-match? (string=? cat target-cat)])
          (if (and cat-match? date-match?)
              (loop (cdr es) (cons uid users))
              (loop (cdr es) users))))))

(define (compute-pairs set-a set-b)
  (let ([pairs '()])
    (for-each
      (lambda (a)
        (for-each
          (lambda (b)
            (cond
              [(string<? a b)
               (set! pairs (cons (cons a b) pairs))]
              [(string<? b a)
               (set! pairs (cons (cons b a) pairs))]))
          set-b))
      set-a)
    (let ([unique (deduplicate-pairs (reverse pairs))])
      (list-sort pair<? unique))))

(define (deduplicate-pairs pairs)
  (let loop ([ps pairs] [seen '()] [acc '()])
    (if (null? ps)
        (reverse acc)
        (let ([p (car ps)])
          (if (member p seen)
              (loop (cdr ps) seen acc)
              (loop (cdr ps) (cons p seen) (cons p acc)))))))

(define (pair<? a b)
  (or (string<? (car a) (car b))
      (and (string=? (car a) (car b))
           (string<? (cdr a) (cdr b)))))

(define (build-pairs-haystack entries target-size)
  (let* ([entry-texts (map cadddr entries)]
         [entry-blocks (group-entries entry-texts 8)])
    (let loop ([size 0] [paras '()] [filler-idx 0]
               [eblocks entry-blocks] [paras-since-entry 0])
      (cond
        [(and (>= size target-size) (null? eblocks))
         (join-paragraphs (reverse paras))]
        [(and (pair? eblocks) (>= paras-since-entry 6))
         (let ([block-text (string-append
                             "--- DATA ENTRIES ---\n"
                             (join-entries (car eblocks))
                             "--- END ENTRIES ---")])
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

;;; F1 evaluation
(define (compute-f1 expected-pairs output-text)
  (let* ([predicted (parse-pairs-from-output output-text)]
         [tp (count-intersect predicted expected-pairs)]
         [precision (if (zero? (length predicted)) 0.0
                        (exact->inexact (/ tp (length predicted))))]
         [recall (if (zero? (length expected-pairs)) 0.0
                     (exact->inexact (/ tp (length expected-pairs))))]
         [f1 (if (zero? (+ precision recall)) 0.0
                 (exact->inexact (/ (* 2 precision recall) (+ precision recall))))])
    (values f1 precision recall tp (length predicted) (length expected-pairs))))

(define (count-intersect lst1 lst2)
  (let loop ([l lst1] [n 0])
    (if (null? l) n
        (if (member (car l) lst2)
            (loop (cdr l) (+ n 1))
            (loop (cdr l) n)))))

(define (parse-pairs-from-output text)
  (let* ([clean (strip-outer-quotes text)]
         [lines (string-split-lines clean)])
    (let loop ([ls lines] [pairs '()])
      (if (null? ls)
          (deduplicate-pairs (reverse pairs))
          (let ([pair (try-parse-pair (car ls))])
            (if pair
                (loop (cdr ls) (cons pair pairs))
                (loop (cdr ls) pairs)))))))

(define (strip-outer-quotes s)
  (let ([len (string-length s)])
    (if (and (>= len 2)
             (char=? (string-ref s 0) #\")
             (char=? (string-ref s (- len 1)) #\"))
        (substring s 1 (- len 1))
        (if (and (>= len 1) (char=? (string-ref s 0) #\"))
            (substring s 1 len)
            s))))

(define (try-parse-pair line)
  (let ([trimmed (string-trim line)])
    (guard (ex [else #f])
      (let ([paren-start (find-char trimmed #\()])
        (if paren-start
            (let ([paren-end (find-char-from trimmed #\) paren-start)])
              (if paren-end
                  (parse-pair-content (substring trimmed (+ paren-start 1) paren-end))
                  (parse-pair-content (substring trimmed (+ paren-start 1)
                                                (string-length trimmed)))))
            (parse-pair-content trimmed))))))

(define (parse-pair-content content)
  (let ([parts (string-split-on-comma content)])
    (if (= (length parts) 2)
        (let ([a (string-trim (car parts))]
              [b (string-trim (cadr parts))])
          (if (and (> (string-length a) 0)
                   (> (string-length b) 0)
                   (char=? (string-ref a 0) #\U)
                   (char=? (string-ref b 0) #\U))
              (cons a b)
              #f))
        #f)))

(define (find-char s ch)
  (let loop ([i 0])
    (cond [(>= i (string-length s)) #f]
          [(char=? (string-ref s i) ch) i]
          [else (loop (+ i 1))])))

(define (find-char-from s ch start)
  (let loop ([i start])
    (cond [(>= i (string-length s)) #f]
          [(char=? (string-ref s i) ch) i]
          [else (loop (+ i 1))])))

(define (string-split-on-comma s)
  (let loop ([chars (string->list s)] [current '()] [parts '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) parts))]
      [(char=? (car chars) #\,)
       (loop (cdr chars) '()
             (cons (list->string (reverse current)) parts))]
      [else
       (loop (cdr chars) (cons (car chars) current) parts)])))

(define (string-split-lines s)
  (let loop ([chars (string->list s)] [current '()] [lines '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) lines))]
      [(char=? (car chars) #\newline)
       (loop (cdr chars) '() (cons (list->string (reverse current)) lines))]
      [else
       (loop (cdr chars) (cons (car chars) current) lines)])))

(define (string-trim s)
  (let ([len (string-length s)])
    (let ([start (let loop ([i 0])
                   (if (and (< i len) (char-whitespace? (string-ref s i)))
                       (loop (+ i 1)) i))])
      (if (>= start len) ""
          (let ([end (let loop ([i (- len 1)])
                       (if (and (> i start) (char-whitespace? (string-ref s (- i 0))))
                           (loop (- i 1)) (+ i 1)))])
            (substring s start end))))))

;;; Pairs System Prompts

(define *v1-pairs-system-prompt*
  (string-append
    "You are a Fold/Scheme agent with a full REPL and a searchable skill lattice.\n\n"
    "The document in 'input' contains ENTRY lines scattered among filler text:\n"
    "  ENTRY | user: UNNN | date: YYYY-MM-DD | category: <cat>\n\n"
    "Each chunk has MULTIPLE lines. Use (rlm-split-lines *chunk*) to split,\n"
    "then filter for lines containing \"ENTRY\".\n\n"
    "Dates are ISO format — string<? works for date comparison.\n"
    "rlm-env-map-chunks auto-stores results in 'map-result.\n"
    "grep only returns top-k. Use rlm-env-map-chunks to process ALL chunks.\n\n"
    "You have access to the full Fold lattice — 36 skills covering constraint\n"
    "solving, set operations, graph algorithms, optimization, and more.\n"
    "Use (lf \"query\") to search for tools before writing code from scratch.\n"
    "Use (require 'module) to load what you find.\n\n"))

(define *v2-pairs-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task involves pairwise reasoning over "
    "structured data in a large document.\n\n"
    "The document in 'input' contains ENTRY lines scattered among filler text:\n"
    "  ENTRY | user: UNNN | date: YYYY-MM-DD | category: <cat>\n\n"
    "Each chunk has MULTIPLE lines. Use (split-lines *chunk*) inside map-chunks,\n"
    "then filter for lines containing \"ENTRY\".\n\n"
    "Dates are ISO format — string<? works for date comparison.\n"
    "map-chunks auto-stores results as 'map-result.\n"
    "grep only returns top-k. Use (map-chunks ...) to process ALL chunks.\n\n"
    "Strategy for pairwise tasks:\n"
    "1. (peek 'input 500) to understand structure\n"
    "2. (map-chunks 'input \"expr\") to extract per-chunk data\n"
    "3. (store 'key expr) to build intermediate results\n"
    "4. Compute the final answer using (eval ...) or (store ...)\n"
    "5. (submit result) when done\n\n"
    "You have access to the full Fold lattice (36 skills). Use:\n"
    "  (search \"query\") to find tools\n"
    "  (load 'module) to load what you find\n\n"))

;;; Pairs Benchmark Runner

(define (run-pairs-benchmark n-users entries-per-user target-size label provider)
  (let* ([entries (generate-pairs-entries n-users entries-per-user)]
         [haystack (build-pairs-haystack entries target-size)]
         [set-a (compute-condition-set entries *condition-a-cat* 'before *cutoff-day*)]
         [set-b (compute-condition-set entries *condition-b-cat* 'after *cutoff-day*)]
         [expected-pairs (compute-pairs set-a set-b)]
         [cutoff-date (day->date-string *cutoff-day*)]
         [task (format "Find all pairs (A, B) where A < B (alphabetical), user A has at least one 'science' entry dated before ~a, and user B has at least one 'technology' entry dated after ~a. List each pair as (UNNN, UMMM), one per line, sorted ascending."
                       cutoff-date cutoff-date)]
         [max-steps 10]
         [max-fuel 40000]
         [chunk-size 2000])

    (display (format "\n=== BENCHMARK: ~a ===\n" label))
    (display (format "Users: ~a (~a entries/user) | Haystack: ~a chars\n"
                     n-users entries-per-user (string-length haystack)))
    (display (format "Set A (science before ~a): ~a users\n" cutoff-date (length set-a)))
    (display (format "Set B (tech after ~a): ~a users\n" cutoff-date (length set-b)))
    (display (format "Expected pairs: ~a\n" (length expected-pairs)))
    (flush-output-port)

    ;; --- V1 Run ---
    (display "\n--- Running v1 ---\n")
    (flush-output-port)
    (let-values ([(v1-result v1-ms)
                  (wall-clock-ms
                   (lambda ()
                     (let ([config (make-rlm-config
                                    provider *v1-pairs-system-prompt*
                                    max-steps max-fuel chunk-size
                                    1 4)])
                       (rlm-run config task haystack))))])
      (let* ([v1-status (rlm-run-result-status v1-result)]
             [v1-output (format "~a" (rlm-run-result-output v1-result))]
             [v1-traj (rlm-run-result-trajectory-hash v1-result)])
        (let-values ([(v1-f1 v1-prec v1-rec v1-tp v1-pred v1-exp)
                      (compute-f1 expected-pairs v1-output)])
          (display (format "  Status: ~a | Time: ~a ms\n" v1-status v1-ms))
          (display (format "  F1: ~,2f (P=~,2f R=~,2f TP=~a pred=~a exp=~a)\n"
                           v1-f1 v1-prec v1-rec v1-tp v1-pred v1-exp))
          (flush-output-port)

          ;; --- V2 Run ---
          (display "\n--- Running v2 ---\n")
          (flush-output-port)
          (let-values ([(v2-result v2-ms)
                        (wall-clock-ms
                         (lambda ()
                           (let ([config (make-rlm2-config
                                           provider *v2-pairs-system-prompt*
                                           max-steps max-fuel chunk-size
                                           1 4 8000 #f)])
                             (rlm2-run config task haystack))))])
            (let* ([v2-status (rlm2-run-result-status v2-result)]
                   [v2-output (format "~a" (rlm2-run-result-output v2-result))]
                   [v2-traj (rlm2-run-result-trajectory-hash v2-result)])
              (let-values ([(v2-f1 v2-prec v2-rec v2-tp v2-pred v2-exp)
                            (compute-f1 expected-pairs v2-output)])
                (display (format "  Status: ~a | Time: ~a ms\n" v2-status v2-ms))
                (display (format "  F1: ~,2f (P=~,2f R=~,2f TP=~a pred=~a exp=~a)\n"
                                 v2-f1 v2-prec v2-rec v2-tp v2-pred v2-exp))
                (flush-output-port)

                `((label . ,label)
                  (n-users . ,n-users)
                  (entries-per-user . ,entries-per-user)
                  (haystack-chars . ,(string-length haystack))
                  (expected-pairs . ,(length expected-pairs))
                  (v1 . ((status . ,v1-status)
                         (time-ms . ,v1-ms)
                         (f1 . ,v1-f1)
                         (precision . ,v1-prec)
                         (recall . ,v1-rec)
                         (tp . ,v1-tp)
                         (predicted . ,v1-pred)
                         (output . ,v1-output)
                         (trajectory . ,v1-traj)))
                  (v2 . ((status . ,v2-status)
                         (time-ms . ,v2-ms)
                         (f1 . ,v2-f1)
                         (precision . ,v2-prec)
                         (recall . ,v2-rec)
                         (tp . ,v2-tp)
                         (predicted . ,v2-pred)
                         (output . ,v2-output)
                         (trajectory . ,v2-traj))))))))))))

(define (print-pairs-comparison results)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║            OOLONG-Pairs v1 vs v2 RESULTS                   ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  (display (format "~30a ~15a ~15a\n" "" "v1" "v2"))
  (display (make-string 60 #\─))
  (display "\n")
  (for-each
   (lambda (r)
     (let ([label (cdr (assq 'label r))]
           [v1 (cdr (assq 'v1 r))]
           [v2 (cdr (assq 'v2 r))]
           [exp (cdr (assq 'expected-pairs r))])
       (display (format "\n~a (~a expected pairs)\n" label exp))
       (display (format "  ~28a ~15a ~15a\n" "Status"
                        (cdr (assq 'status v1))
                        (cdr (assq 'status v2))))
       (display (format "  ~28a ~15a ~15a\n" "Time (ms)"
                        (cdr (assq 'time-ms v1))
                        (cdr (assq 'time-ms v2))))
       (display (format "  ~28a ~15,2f ~15,2f\n" "F1"
                        (cdr (assq 'f1 v1))
                        (cdr (assq 'f1 v2))))
       (display (format "  ~28a ~15,2f ~15,2f\n" "Precision"
                        (cdr (assq 'precision v1))
                        (cdr (assq 'precision v2))))
       (display (format "  ~28a ~15,2f ~15,2f\n" "Recall"
                        (cdr (assq 'recall v1))
                        (cdr (assq 'recall v2))))))
   results)
  (display "\n"))

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

    ;; OOLONG (linear aggregation)
    (let* ([r1 (run-single-benchmark 100 50000
                 "OOLONG 100 entries / 50K" provider)]
           [r2 (run-single-benchmark 200 100000
                 "OOLONG 200 entries / 100K" provider)])
      (print-comparison (list r1 r2))

      ;; OOLONG-Pairs (pairwise reasoning)
      (let* ([p1 (run-pairs-benchmark 15 3 30000
                   "OOLONG-Pairs 15 users" provider)])
        (print-pairs-comparison (list p1))

        ;; Store all results
        (let ([results-file (format "user/rlm/bench-results-~a.sexp"
                                    (rlm2-current-iso8601))])
          (call-with-output-file results-file
            (lambda (port)
              (pretty-print `(benchmark-results
                              (model ,model-id)
                              (timestamp ,(rlm2-current-iso8601))
                              (oolong ,(list r1 r2))
                              (pairs ,(list p1)))
                            port)))
          (display (format "\nResults saved to ~a\n" results-file)))))))

(run-benchmark-suite)
