;;; user/rlm/oolong-pairs.ss — OOLONG-Pairs (Pairwise Aggregation)
;;;
;;; Inspired by the OOLONG-Pairs task from "Recursive Language Models"
;;; (Zhang, Kraska, Khattab — arXiv:2512.24601).
;;;
;;; The model must classify structured entries, build condition sets,
;;; then compute pairwise relationships. Unlike basic OOLONG (linear),
;;; this requires quadratic reasoning: find all (A, B) pairs where
;;; A satisfies condition X and B satisfies condition Y.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/oolong-pairs.ss

(unless (getenv "RLM_INTEGRATION")
  (display "Skipping OOLONG-Pairs test (set RLM_INTEGRATION=1 to enable)\n")
  (exit 0))

(load "boundary/pipeline/rlm-loop.ss")

;;; ====
;;; Filler Corpus
;;; ====

(define *filler-paragraphs*
  '#("The process of photosynthesis converts carbon dioxide and water into glucose and oxygen using sunlight. Chlorophyll in plant cells absorbs light energy, which drives the light-dependent reactions in the thylakoid membranes."

     "Medieval European castles evolved from simple wooden motte-and-bailey constructions to sophisticated stone fortifications over several centuries."

     "The Mariana Trench in the western Pacific Ocean reaches a depth of approximately 11,034 meters at its deepest point, the Challenger Deep."

     "Quantum entanglement occurs when pairs of particles interact in ways that make the quantum state of each particle dependent on the state of the other."

     "The Fibonacci sequence appears throughout nature in surprising ways. The arrangement of leaves around a stem and the spiral pattern of seeds in a sunflower head."

     "Continental drift theory, first proposed by Alfred Wegener in 1912, was initially rejected by most geologists."

     "Bayesian statistics provides a framework for updating probability estimates as new evidence becomes available."

     "Coral reefs are sometimes called the rainforests of the sea due to their extraordinary biodiversity."))

(define *n-fillers* (vector-length *filler-paragraphs*))

;;; Seed PRNG for reproducible entry generation and haystack layout.
(random-seed 42)

;;; ====
;;; Entry Generation
;;; ====

(define *categories* '#("science" "history" "sports" "technology" "arts"))
(define *n-categories* (vector-length *categories*))

;;; Dates span 2025-01-01 to 2025-12-31.
;;; We store as day-of-year (1-365) for easy comparison.
(define (day->date-string day)
  (let* ([month-days '#(31 28 31 30 31 30 31 31 30 31 30 31)]
         [month-names '#("01" "02" "03" "04" "05" "06" "07" "08" "09" "10" "11" "12")])
    (let loop ([m 0] [remaining day])
      (if (or (>= m 12) (<= remaining (vector-ref month-days m)))
          (format "2025-~a-~2,'0d"
                  (vector-ref month-names (min m 11))
                  (max 1 remaining))
          (loop (+ m 1) (- remaining (vector-ref month-days m)))))))

;;; generate-entries : Nat -> Nat -> (List Entry)
;;; Entry: (user-id day-of-year category entry-text)
(define (generate-entries n-users entries-per-user)
  (let loop ([u 0] [entries '()])
    (if (>= u n-users)
        (reverse entries)
        (let ([user-id (format "U~3,'0d" (+ u 1))])
          (let inner ([e 0] [entries entries])
            (if (>= e entries-per-user)
                (loop (+ u 1) entries)
                (let* ([day (+ 1 (random 365))]
                       [cat-idx (random *n-categories*)]
                       [category (vector-ref *categories* cat-idx)]
                       [text (format "ENTRY | user: ~a | date: ~a | category: ~a"
                                     user-id (day->date-string day) category)])
                  (inner (+ e 1)
                         (cons (list user-id day category text) entries)))))))))

;;; ====
;;; Ground Truth Computation
;;; ====

;;; A "pair query" asks: find all pairs (A, B) where A < B,
;;; A has at least one entry with category X before cutoff date,
;;; B has at least one entry with category Y after cutoff date.

(define *cutoff-day* 152)  ; ~June 1 (day 152 of 365)
(define *condition-a-cat* "science")
(define *condition-b-cat* "technology")

;;; compute-condition-set : (List Entry) -> String -> (before|after) -> Nat -> (List String)
;;; Returns sorted list of unique user IDs meeting the condition.
(define (compute-condition-set entries target-cat temporal-dir cutoff)
  (let loop ([es entries] [users '()])
    (if (null? es)
        (list-sort string<? (deduplicate users))
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

(define (deduplicate lst)
  (let loop ([l lst] [seen '()] [acc '()])
    (if (null? l)
        (reverse acc)
        (if (member (car l) seen)
            (loop (cdr l) seen acc)
            (loop (cdr l) (cons (car l) seen) (cons (car l) acc))))))

;;; compute-pairs : (List String) -> (List String) -> (List (String . String))
;;; All pairs (a, b) where a is in set-A, b is in set-B, a < b.
;;; Also includes (b, a) if b in set-A and a in set-B (i.e., both conditions
;;; can be satisfied by different users, and we want A < B ordering).
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
    ;; Deduplicate and sort
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

;;; format-pairs : (List (String . String)) -> String
(define (format-pairs pairs)
  (apply string-append
         (map (lambda (p)
                (format "(~a, ~a)\n" (car p) (cdr p)))
              pairs)))

;;; ====
;;; Haystack Construction
;;; ====

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
                             (join-lines (car eblocks))
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

(define (group-entries texts block-size)
  (let loop ([ts texts] [current '()] [groups '()])
    (cond
      [(null? ts)
       (reverse (if (null? current) groups (cons (reverse current) groups)))]
      [(>= (length current) block-size)
       (loop ts '() (cons (reverse current) groups))]
      [else
       (loop (cdr ts) (cons (car ts) current) groups)])))

(define (join-lines texts)
  (apply string-append (map (lambda (t) (string-append t "\n")) texts)))

(define (join-paragraphs paragraphs)
  (let loop ([ps paragraphs] [acc ""])
    (if (null? ps)
        acc
        (loop (cdr ps)
              (string-append acc (car ps) "\n\n")))))

;;; ====
;;; System Prompt
;;; ====

(define *pairs-system-prompt*
  (string-append
    "You are a Fold/Scheme agent working with structured data.\n\n"
    "The document in 'input' contains ENTRY lines scattered among filler:\n"
    "  ENTRY | user: UNNN | date: YYYY-MM-DD | category: <cat>\n\n"
    "CRITICAL: Each chunk has MULTIPLE lines. Always split first:\n"
    "  (rlm-split-lines *chunk*) returns a list of lines.\n"
    "  Only lines containing \"ENTRY\" are data records.\n\n"
    "Worker functions (use STRING arguments, not symbols):\n"
    "  (rlm-split-lines s)              split into lines\n"
    "  (rlm-string-contains? s \"sub\")   substring check\n"
    "  (rlm-extract-field line \"user\")  extract field from ENTRY line\n"
    "  (rlm-flatten lst)                flatten nested lists\n"
    "  (rlm-deduplicate lst)            remove duplicates\n"
    "  (filter pred lst)                keep elements where pred is true\n\n"
    "Dates are ISO format — string<? works for date comparison.\n"
    "map-chunks auto-stores results in 'map-result.\n"
    "grep only returns top-k. Use map-chunks for ALL data.\n\n"))

;;; ====
;;; Evaluation
;;; ====

;;; F1 score on pair sets.
(define (compute-f1 expected-pairs output-text)
  (let* ([predicted (parse-pairs-from-output output-text)]
         [expected-set expected-pairs]
         [tp (count-intersect predicted expected-set)]
         [precision (if (zero? (length predicted)) 0.0
                        (/ tp (length predicted)))]
         [recall (if (zero? (length expected-set)) 0.0
                     (/ tp (length expected-set)))]
         [f1 (if (zero? (+ precision recall)) 0.0
                 (/ (* 2 precision recall) (+ precision recall)))])
    (values f1 precision recall tp (length predicted) (length expected-set))))

(define (count-intersect lst1 lst2)
  (let loop ([l lst1] [n 0])
    (if (null? l)
        n
        (if (member (car l) lst2)
            (loop (cdr l) (+ n 1))
            (loop (cdr l) n)))))

;;; parse-pairs-from-output : String -> (List (String . String))
;;; Extract (UNNN, UMMM) pairs from model output text.
;;; Robust: handles quoted strings, extra whitespace, various delimiters.
(define (parse-pairs-from-output text)
  ;; First strip outer quotes if present
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
        ;; Also handle leading quote without trailing
        (if (and (>= len 1) (char=? (string-ref s 0) #\"))
            (substring s 1 len)
            s))))

;;; try-parse-pair : String -> (String . String) | #f
;;; Parse "(U001, U002)" or "U001, U002" from a line. Tolerant of formats.
(define (try-parse-pair line)
  (let ([trimmed (string-trim line)])
    (guard (ex [else #f])
      (let ([paren-start (find-char trimmed #\()])
        (if paren-start
            ;; Has parens — extract content between them
            (let ([paren-end (find-char-from trimmed #\) paren-start)])
              (if paren-end
                  (parse-pair-content (substring trimmed (+ paren-start 1) paren-end))
                  (parse-pair-content (substring trimmed (+ paren-start 1)
                                                (string-length trimmed)))))
            ;; No parens — try parsing the whole line as "UNNN, UMMM"
            (parse-pair-content trimmed))))))

(define (parse-pair-content content)
  (let ([parts (string-split-on-comma content)])
    (if (= (length parts) 2)
        (let ([a (string-trim (car parts))]
              [b (string-trim (cadr parts))])
          (if (and (> (string-length a) 0)
                   (> (string-length b) 0)
                   ;; Verify they look like user IDs
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

;;; ====
;;; Runner
;;; ====

(define (run-oolong-pairs n-users entries-per-user target-size)
  (let* ([entries (generate-entries n-users entries-per-user)]
         [haystack (build-pairs-haystack entries target-size)]
         [set-a (compute-condition-set entries *condition-a-cat* 'before *cutoff-day*)]
         [set-b (compute-condition-set entries *condition-b-cat* 'after *cutoff-day*)]
         [expected-pairs (compute-pairs set-a set-b)]
         [cutoff-date (day->date-string *cutoff-day*)]
         [task (format "Find all pairs (A, B) where A < B (alphabetical), user A has at least one 'science' entry dated before ~a, and user B has at least one 'technology' entry dated after ~a. List each pair as (UNNN, UMMM), one per line, sorted ascending."
                       cutoff-date cutoff-date)]
         [provider (rlm-provider-vllm "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8" 8000)]
         [config (make-rlm-config
                   provider
                   *pairs-system-prompt*
                   25     ; max-steps (2-pass + pair computation + debugging)
                   40000  ; max-fuel
                   2000   ; chunk-size
                   1      ; max-depth
                   4)])   ; loop-window

    (display (format "=== OOLONG-Pairs (~a users, ~a entries/user) ===\n"
                     n-users entries-per-user))
    (display (format "Haystack: ~a chars, ~a chunks\n"
                     (string-length haystack)
                     (+ 1 (quotient (string-length haystack) 2000))))
    (display (format "Condition A: category=~a before ~a -> ~a users: ~a\n"
                     *condition-a-cat* cutoff-date (length set-a) set-a))
    (display (format "Condition B: category=~a after ~a -> ~a users: ~a\n"
                     *condition-b-cat* cutoff-date (length set-b) set-b))
    (display (format "Expected pairs: ~a\n" (length expected-pairs)))
    (when (< (length expected-pairs) 30)
      (display (format "Pairs: ~a\n" (format-pairs expected-pairs))))
    (display "\n")
    (flush-output-port)

    (let ([result (rlm-run config task haystack)])
      (display (format "\n=== Result ===\n"))
      (display (format "Status: ~a\n" (rlm-run-result-status result)))
      (display (format "Output: ~a\n" (rlm-run-result-output result)))
      (display (format "Trajectory: ~a\n" (rlm-run-result-trajectory-hash result)))

      ;; F1 evaluation
      (let-values ([(f1 prec rec tp predicted expected)
                    (compute-f1 expected-pairs
                                (format "~a" (rlm-run-result-output result)))])
        (display (format "\n>>> F1: ~,2f  (P=~,2f R=~,2f  TP=~a pred=~a exp=~a)\n"
                         f1 prec rec tp predicted expected))
        (cond
          [(>= f1 0.8)
           (display ">>> PASS: F1 >= 0.8\n")
           #t]
          [(>= f1 0.5)
           (display ">>> PARTIAL: F1 >= 0.5 but < 0.8\n")
           #f]
          [else
           (display ">>> FAIL: F1 < 0.5\n")
           #f])))))

;;; ====
;;; Run
;;; ====

(define (run-pairs-suite)
  ;; Start small: 15 users, 3 entries each = 45 entries, ~30K haystack
  ;; Then scale: 25 users, 4 entries each = 100 entries, ~60K haystack
  (let* ([r1 (run-oolong-pairs 15 3 30000)]
         [_ (display "\n")]
         [r2 (run-oolong-pairs 25 4 60000)])
    (display "\n=== Summary ===\n")
    (display (format "  15 users / 30K: ~a\n" (if r1 "PASS" "FAIL")))
    (display (format "  25 users / 60K: ~a\n" (if r2 "PASS" "FAIL")))))

(run-pairs-suite)
