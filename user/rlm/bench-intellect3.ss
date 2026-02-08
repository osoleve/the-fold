;;; user/rlm/bench-intellect3.ss — Intellect3 benchmark
;;;
;;; Three task types from Prime Intellect's dataset:
;;;   - Logic (object_counting): Huge inventory text, count by category.
;;;     Exercises: peek, map-chunks, store, eval, think
;;;   - Logic (mathador): Find arithmetic ops on given numbers to reach target.
;;;     Exercises: think, eval
;;;   - Math: Competition math. Exercises: think, eval
;;;   - Science: Applied STEM problems. Exercises: think, eval
;;;
;;; Run: scheme --script user/rlm/bench-intellect3.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Data loading
;;; ====

(define (load-intellect3-samples path)
  (let* ([p (open-input-file path)]
         [data (read p)])
    (close-input-port p)
    (cdar data)))

;;; ====
;;; Answer matching
;;; ====

(define (i3-normalize-answer s)
  ;; Strip %, $, trailing spaces, \boxed{} wrapper
  (let* ([s (i3-strip-chars s '(#\% #\$ #\space))]
         [s (i3-strip-boxed s)])
    s))

(define (i3-strip-chars s chars)
  (list->string
    (filter (lambda (c) (not (memv c chars)))
            (string->list s))))

(define (i3-strip-boxed s)
  (let ([len (string-length s)])
    (if (and (>= len 8)
             (string=? (substring s 0 7) "\\boxed{")
             (char=? (string-ref s (- len 1)) #\}))
        (substring s 7 (- len 1))
        s)))

(define (i3-correct? output expected-str)
  (let* ([out (format "~a" output)]
         [norm-expected (i3-normalize-answer expected-str)]
         [expected-num (string->number norm-expected)])
    (if expected-num
        ;; Numeric: check if expected number appears in output
        (output-contains-number? out expected-num)
        ;; String fallback
        (i3-string-contains? out norm-expected))))

(define (i3-string-contains? hay needle)
  (let ([hlen (string-length hay)]
        [nlen (string-length needle)])
    (if (or (zero? nlen) (> nlen hlen)) #f
        (let loop ([i 0])
          (cond
            [(> (+ i nlen) hlen) #f]
            [(string=? (substring hay i (+ i nlen)) needle) #t]
            [else (loop (+ i 1))])))))

;;; ====
;;; Task classification
;;; ====

(define (i3-task-type entry)
  ;; Detect task type from entry fields
  (let ([task (assq 'task entry)]
        [domain (assq 'domain entry)])
    (cond
      [task (cdr task)]       ; logic samples have 'task field
      [domain (cdr domain)]   ; science samples have 'domain field
      [else "math"])))        ; math samples have neither

;;; ====
;;; System prompts per task type
;;; ====

(define *i3-counting-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to count objects from an "
    "inventory description.\n\n"
    "The full inventory text is stored under 'input'. It lists items owned "
    "by various people. You need to identify which items belong to 'I/me' "
    "(not relatives/friends), classify them by category, and compute totals.\n\n"
    "IMPORTANT: Some items have tracking narratives like "
    "'here's how I ended up with N of them' — the FINAL count stated is correct.\n\n"
    "Strategy:\n"
    "1. (peek 'input 1000) to understand the structure\n"
    "2. (chunk-count 'input) to see how many chunks there are\n"
    "3. (map-chunks 'input \"expr\") to extract relevant items per chunk\n"
    "   Use (split-lines *chunk*) and filter for lines starting with 'I '\n"
    "4. (store 'key value) for intermediate results\n"
    "5. (eval \"(+ ...)\") to compute totals\n"
    "6. (submit answer) with the numeric total\n\n"
    "Available: split-lines, string-contains?, extract-after, string-index-of\n\n"
    "Submit ONLY the numeric answer.\n"))

(define *i3-mathador-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to find arithmetic operations "
    "that combine given numbers to reach a target value.\n\n"
    "The problem is stored under 'input'.\n\n"
    "Strategy:\n"
    "1. (peek 'input 2000) to read the problem\n"
    "2. (think \"reasoning...\") to explore combinations\n"
    "3. (eval \"(expression)\") to verify candidates\n"
    "4. (submit answer) with the target number once you've verified it\n\n"
    "You can use +, -, *, / and parentheses. All standard Scheme arithmetic "
    "is available.\n\n"
    "Submit ONLY the numeric answer.\n"))

(define *i3-math-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to solve a math problem.\n\n"
    "The problem is stored under 'input'.\n\n"
    "Strategy:\n"
    "1. (peek 'input 2000) to read the problem\n"
    "2. (think \"reasoning about approach...\") to plan your solution\n"
    "3. (eval \"(scheme-expression)\") to compute values\n"
    "   Available: +, -, *, /, quotient, remainder, expt, sqrt, floor, ceiling,\n"
    "   round, min, max, abs, gcd, lcm, modulo, log, exp, sin, cos, tan, asin,\n"
    "   acos, atan, iota, build-list\n"
    "4. (submit answer) with the numeric answer\n\n"
    "Submit ONLY the numeric answer.\n"))

(define *i3-science-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to solve an applied science "
    "or engineering problem.\n\n"
    "The problem is stored under 'input'.\n\n"
    "Strategy:\n"
    "1. (peek 'input 2000) to read the problem\n"
    "2. (think \"identifying variables and equations...\") to plan\n"
    "3. (eval \"(scheme-expression)\") to compute values\n"
    "   Available: all standard arithmetic plus sqrt, expt, log, exp,\n"
    "   sin, cos, tan, asin, acos, atan, pi (use (acos -1) for pi)\n"
    "4. (submit answer) — numeric, may be decimal\n\n"
    "Submit ONLY the numeric answer (no units).\n"))

(define (i3-system-prompt-for entry)
  (let ([task (i3-task-type entry)])
    (cond
      [(string=? task "object_counting") *i3-counting-prompt*]
      [(string=? task "mathador") *i3-mathador-prompt*]
      [(member task '("physics" "chemistry" "economics" "biology"))
       *i3-science-prompt*]
      [else *i3-math-prompt*])))

;;; ====
;;; Runner
;;; ====

(define (i3-config-for entry provider)
  (let ([task (i3-task-type entry)])
    (if (string=? task "object_counting")
        ;; Big text: more steps, more fuel, chunking
        (make-rlm2-config provider (i3-system-prompt-for entry)
                          12 20000 2000 1 3 8000 #f)
        ;; Short problems: fewer steps
        (make-rlm2-config provider (i3-system-prompt-for entry)
                          10 10000 4000 1 3 8000 #f))))

(define (run-i3-problem entry provider i total suite-label)
  (let* ([question (cdr (assq 'question entry))]
         [expected (cdr (assq 'answer entry))]
         [task-type (i3-task-type entry)]
         [difficulty (let ([d (assq 'difficulty entry)])
                       (if d (cdr d) "?"))]
         ;; For counting tasks, derive the actual question from the end
         [task-desc (if (string=? task-type "object_counting")
                        "Count the specified items from the inventory. Report only the numeric total."
                        (string-append "Solve this problem. Report only the numeric answer.\n\n"
                                       question))]
         [haystack question]
         [config (i3-config-for entry provider)])

    (display (format "  [~a/~a] ~a (d=~a) | Expected: ~a | "
                     i total task-type difficulty expected))
    (flush-output-port)

    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task-desc haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)]
             [correct? (i3-correct? output expected)])
        (display (format "~a ~a (~as) → ~a\n"
                         (if correct? "✓" "✗")
                         status
                         (quotient ms 1000)
                         (if (> (string-length output) 80)
                             (string-append (substring output 0 80) "...")
                             output)))
        (flush-output-port)

        `((question . ,(if (> (string-length question) 80)
                           (string-append (substring question 0 80) "...")
                           question))
          (task-type . ,task-type)
          (difficulty . ,difficulty)
          (expected . ,expected)
          (status . ,status)
          (time-ms . ,ms)
          (correct . ,correct?)
          (output . ,output)
          (trajectory . ,traj)
          (suite . ,suite-label))))))

(define (run-i3-suite! suite-path suite-label)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [provider (rlm-provider-vllm model-id port)]
         [samples (load-intellect3-samples suite-path)]
         [n (length samples)])

    (display (format "\n~a (~a, ~a problems)\n" suite-label model-id n))
    (display (make-string 60 #\=))
    (display "\n")
    (flush-output-port)

    (let loop ([remaining samples] [i 1] [results '()])
      (if (null? remaining)
          (let* ([results (reverse results)]
                 [correct (length (filter (lambda (r) (cdr (assq 'correct r))) results))]
                 [total-ms (apply + (map (lambda (r) (cdr (assq 'time-ms r))) results))])
            (display (format "\n--- ~a Summary ---\n" suite-label))
            (display (format "Score: ~a/~a (~,1f%)\n" correct n
                             (* 100.0 (/ correct n))))
            (display (format "Total time: ~as\n\n" (quotient total-ms 1000)))
            (flush-output-port)
            results)
          (let ([r (run-i3-problem (car remaining) provider i n suite-label)])
            (loop (cdr remaining) (+ i 1) (cons r results)))))))

;;; ====
;;; Main
;;; ====

(define (run-all-intellect3!)
  (let* ([logic-results (run-i3-suite! "user/rlm/data/intellect3-logic-sample.sexp"
                                       "Intellect3-Logic")]
         [math-results (run-i3-suite! "user/rlm/data/intellect3-math-sample.sexp"
                                      "Intellect3-Math")]
         [science-results (run-i3-suite! "user/rlm/data/intellect3-science-sample.sexp"
                                         "Intellect3-Science")]
         [all-results (append logic-results math-results science-results)]
         [n (length all-results)]
         [correct (length (filter (lambda (r) (cdr (assq 'correct r))) all-results))]
         [total-ms (apply + (map (lambda (r) (cdr (assq 'time-ms r))) all-results))]
         [ts (rlm2-current-iso8601)]
         [model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")]
         [outpath (format "user/rlm/bench-results-intellect3-~a.sexp" ts)])

    (display "╔══════════════════════════════════════════════════════════════╗\n")
    (display "║              INTELLECT3 COMBINED RESULTS                   ║\n")
    (display "╚══════════════════════════════════════════════════════════════╝\n")
    (display (format "Overall: ~a/~a (~,1f%) in ~as\n"
                     correct n (* 100.0 (/ correct n))
                     (quotient total-ms 1000)))

    ;; Per-type breakdown
    (for-each
      (lambda (task-type)
        (let* ([sub (filter (lambda (r) (string=? (cdr (assq 'task-type r)) task-type))
                            all-results)]
               [sub-n (length sub)]
               [sub-c (length (filter (lambda (r) (cdr (assq 'correct r))) sub))])
          (when (> sub-n 0)
            (display (format "  ~a: ~a/~a (~,1f%)\n"
                             task-type sub-c sub-n
                             (* 100.0 (/ sub-c sub-n)))))))
      '("object_counting" "mathador" "math" "physics" "chemistry"
        "economics" "biology"))

    ;; Save results
    (call-with-port
      (open-file-output-port outpath
        (file-options no-fail) (buffer-mode block)
        (make-transcoder (utf-8-codec)))
      (lambda (port)
        (pretty-print
          `(benchmark-results
             (model ,model-id)
             (mode "intellect3")
             (timestamp ,ts)
             (n-problems ,n)
             (correct ,correct)
             (accuracy ,(inexact (/ correct n)))
             (total-ms ,total-ms)
             (problems ,all-results))
          port)))
    (display (format "Results: ~a\n" outpath))))

(run-all-intellect3!)
