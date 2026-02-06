;;; user/rlm/oolong.ss — OOLONG Classification + Aggregation
;;;
;;; Inspired by the OOLONG task from "Recursive Language Models"
;;; (Zhang, Kraska, Khattab — arXiv:2512.24601).
;;;
;;; The model must process structured entries scattered in a large text,
;;; classify them by criteria, and compute an aggregate (sum).
;;; Unlike S-NIAH, the answer requires touching ALL relevant data.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/oolong.ss

(unless (getenv "RLM_INTEGRATION")
  (display "Skipping OOLONG test (set RLM_INTEGRATION=1 to enable)\n")
  (exit 0))

(load "boundary/pipeline/rlm-loop.ss")

;;; ====
;;; Filler Corpus (shared with other RLM tests)
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

;;; ====
;;; Structured Entry Generation
;;; ====

(define *types* '#("alpha" "beta" "gamma" "delta"))
(define *regions* '#("north" "south" "east" "west"))

;;; make-entry : Nat -> String -> String -> Nat -> String
;;; Format a structured record entry.
(define (make-entry id type region value)
  (format "RECORD ~4,'0d | type: ~a | region: ~a | value: ~a"
          id type region value))

;;; generate-entries : Nat -> (List (String Nat String String))
;;; Generate n entries with seeded PRNG for reproducible distribution.
;;; Returns list of (entry-text value type region) tuples.
(define (generate-entries n)
  (random-seed 42)
  (let loop ([i 0] [entries '()])
    (if (>= i n)
        (reverse entries)
        (let* ([type (vector-ref *types* (random (vector-length *types*)))]
               [region (vector-ref *regions* (random (vector-length *regions*)))]
               [value (+ 10 (random 91))]  ; values 10-100
               [text (make-entry (+ i 1) type region value)])
          (loop (+ i 1) (cons (list text value type region) entries))))))

;;; compute-answer : (List (String Nat String String)) -> String -> String -> Nat
;;; Ground-truth sum: total value of entries matching type AND region.
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

;;; count-matching : (List (String Nat String String)) -> String -> String -> Nat
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

;;; ====
;;; Haystack Construction
;;; ====

;;; Build a haystack: filler paragraphs interspersed with entry blocks.
;;; Entries are grouped into blocks of ~5, each block separated by fillers.
(define (build-oolong-haystack entries target-size)
  (let* ([entry-block-size 5]
         [entry-blocks (group-entries (map car entries) entry-block-size)]
         [n-entry-blocks (length entry-blocks)])
    ;; Fill to target size, scattering entry blocks throughout
    (let loop ([size 0] [paras '()] [filler-idx 0]
               [eblocks entry-blocks] [paras-since-entry 0])
      (cond
        ;; Done with size AND entry blocks exhausted
        [(and (>= size target-size) (null? eblocks))
         (join-paragraphs (reverse paras))]
        ;; Time to insert next entry block (every ~8 filler paragraphs)
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
        ;; Add filler paragraph
        [else
         (let ([para (vector-ref *filler-paragraphs* (modulo filler-idx *n-fillers*))])
           (loop (+ size (string-length para) 2)
                 (cons para paras)
                 (+ filler-idx 1)
                 eblocks
                 (+ paras-since-entry 1)))]))))

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

;;; ====
;;; System Prompt
;;; ====

(define *oolong-system-prompt*
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
    "4. Report with DONE(total)\n\n"
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

;;; ====
;;; Runner
;;; ====

(define *target-type* "alpha")
(define *target-region* "north")

(define (run-oolong n-entries target-size)
  (let* ([entries (generate-entries n-entries)]
         [haystack (build-oolong-haystack entries target-size)]
         [expected (compute-answer entries *target-type* *target-region*)]
         [match-count (count-matching entries *target-type* *target-region*)]
         [task (format "Find the total sum of all 'value' fields from records where type is '~a' AND region is '~a'. Report only the numeric total."
                       *target-type* *target-region*)]
         [provider (rlm-provider-vllm "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8" 8000)]
         [config (make-rlm-config
                   provider
                   *oolong-system-prompt*
                   12     ; max-steps
                   20000  ; max-fuel
                   2000   ; chunk-size
                   1      ; max-depth
                   3)])   ; loop-window

    (display (format "=== OOLONG (~a entries) ===\n" n-entries))
    (display (format "Haystack: ~a chars, ~a chunks\n"
                     (string-length haystack)
                     (+ 1 (quotient (string-length haystack) 2000))))
    (display (format "Target: type=~a, region=~a\n" *target-type* *target-region*))
    (display (format "Matching entries: ~a out of ~a\n" match-count n-entries))
    (display (format "Expected total: ~a\n\n" expected))
    (flush-output-port)

    (let ([result (rlm-run config task haystack)])
      (display (format "\n=== Result ===\n"))
      (display (format "Status: ~a\n" (rlm-run-result-status result)))
      (display (format "Output: ~a\n" (rlm-run-result-output result)))
      (display (format "Trajectory: ~a\n" (rlm-run-result-trajectory-hash result)))

      ;; Check answer: extract numeric value from output
      (let* ([output (format "~a" (rlm-run-result-output result))]
             [expected-str (number->string expected)]
             [pass? (output-contains-number? output expected)])
        (if pass?
            (begin
              (display (format "\n>>> PASS: Correct total ~a found!\n" expected))
              #t)
            (begin
              (display (format "\n>>> FAIL: Expected ~a in output\n" expected))
              #f))))))

;;; output-contains-number? : String -> Nat -> Bool
;;; Check if the output string contains the expected number.
;;; We look for the number as a standalone value (not part of a larger number).
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
                  ;; Check boundary: not preceded/followed by a digit
                  (or (= i 0) (not (char-numeric? (string-ref output (- i 1)))))
                  (or (= (+ i exp-len) out-len)
                      (not (char-numeric? (string-ref output (+ i exp-len))))))
             #t]
            [else (loop (+ i 1))])))))

;;; ====
;;; Run
;;; ====

(define (run-oolong-suite)
  (let* ([r1 (run-oolong 100 50000)]
         [_ (display "\n")]
         [r2 (run-oolong 200 100000)])
    (display "\n=== Summary ===\n")
    (display (format "  100 entries / 50K:  ~a\n" (if r1 "PASS" "FAIL")))
    (display (format "  200 entries / 100K: ~a\n" (if r2 "PASS" "FAIL")))))

(run-oolong-suite)
