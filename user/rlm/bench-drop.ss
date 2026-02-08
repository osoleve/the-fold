;;; user/rlm/bench-drop.ss — DROP benchmark
;;;
;;; Discrete Reasoning Over Paragraphs. NFL game recaps and historical
;;; passages where questions require counting, arithmetic, or comparison.
;;; Tests: grep/peek for extraction → eval for computation → submit.
;;; Unlike OOLONG's structured records, DROP uses natural language.
;;;
;;; Run: scheme --script user/rlm/bench-drop.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Data loading
;;; ====

(define (load-drop-samples path)
  ;; Returns list of (passage question answer answer-type) alists
  (let* ([p (open-input-file path)]
         [data (read p)])
    (close-input-port p)
    (cdar data)))

;;; ====
;;; Evaluation
;;; ====

(define (drop-correct? output expected-str)
  ;; For numeric DROP answers, check if the expected number appears in output.
  ;; DROP answers are strings — convert to number for comparison.
  (let ([expected-num (string->number expected-str)]
        [out (format "~a" output)])
    (if expected-num
        (output-contains-number? out expected-num)
        ;; Fallback: exact string match (for non-numeric answers)
        (string-contains-ci out expected-str))))

(define (string-contains-ci hay needle)
  (let ([hlen (string-length hay)]
        [nlen (string-length needle)])
    (if (> nlen hlen) #f
        (let loop ([i 0])
          (cond
            [(> (+ i nlen) hlen) #f]
            [(string=? (substring hay i (+ i nlen)) needle) #t]
            [else (loop (+ i 1))])))))

;;; ====
;;; System prompt
;;; ====

(define *drop-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to answer questions that require "
    "discrete reasoning (counting, arithmetic, comparison) over a passage.\n\n"
    "The passage is stored in your environment under 'input'.\n\n"
    "Strategy:\n"
    "1. (peek 'input 3000) to read the passage\n"
    "2. (grep 'input \"keyword\") to search for specific information\n"
    "3. (think \"reasoning...\") to work through the problem\n"
    "4. (eval \"(scheme-expression)\") if you need to compute something\n"
    "5. (submit answer) with the final answer\n\n"
    "The questions typically require:\n"
    "  - Counting events or items mentioned in the passage\n"
    "  - Arithmetic (sums, differences, averages) over numbers in the text\n"
    "  - Comparing values to determine which is larger/smaller\n"
    "  - Inferring quantities not directly stated\n\n"
    "IMPORTANT: Submit ONLY the numeric answer. No units, no explanation.\n"))

;;; ====
;;; Runner
;;; ====

(define (run-drop-problem entry provider i total)
  (let* ([passage (cdr (assq 'passage entry))]
         [question (cdr (assq 'question entry))]
         [expected (cdr (assq 'answer entry))]
         [task (string-append "Answer this question about the passage. Report only the numeric answer.\n\n"
                              "Question: " question)]
         [haystack passage]
         [config (make-rlm2-config
                   provider *drop-system-prompt*
                   10       ; max-steps
                   10000    ; fuel
                   4000     ; chunk-size (passages are ~200-500 words)
                   1 3 8000 #f)])

    (display (format "  [~a/~a] Q: ~a | Expected: ~a | "
                     i total
                     (if (> (string-length question) 50)
                         (string-append (substring question 0 50) "...")
                         question)
                     expected))
    (flush-output-port)

    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)]
             [correct? (drop-correct? output expected)])
        (display (format "~a ~a (~as) → ~a\n"
                         (if correct? "✓" "✗")
                         status
                         (quotient ms 1000)
                         (if (> (string-length output) 80)
                             (string-append (substring output 0 80) "...")
                             output)))
        (flush-output-port)

        `((question . ,question)
          (expected . ,expected)
          (status . ,status)
          (time-ms . ,ms)
          (correct . ,correct?)
          (output . ,output)
          (trajectory . ,traj))))))

(define (run-drop-suite!)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [provider (rlm-provider-vllm model-id port)]
         [samples (load-drop-samples "user/rlm/data/drop-sample.sexp")]
         [n (length samples)])

    (display (format "DROP Benchmark (~a)\n" model-id))
    (display "==========================\n")
    (display (format "Problems: ~a (numeric answers only)\n\n" n))
    (flush-output-port)

    (let loop ([remaining samples] [i 1] [results '()])
      (if (null? remaining)
          (let* ([results (reverse results)]
                 [correct (length (filter (lambda (r) (cdr (assq 'correct r))) results))]
                 [total-ms (apply + (map (lambda (r) (cdr (assq 'time-ms r))) results))])

            (display (format "\n=== SUMMARY ===\n"))
            (display (format "Score: ~a/~a (~,1f%)\n" correct n
                             (* 100.0 (/ correct n))))
            (display (format "Total time: ~as\n" (quotient total-ms 1000)))
            (flush-output-port)

            ;; Save results
            (let* ([ts (rlm2-current-iso8601)]
                   [outpath (format "user/rlm/bench-results-drop-~a.sexp" ts)])
              (call-with-port
                (open-file-output-port outpath
                  (file-options no-fail) (buffer-mode block)
                  (make-transcoder (utf-8-codec)))
                (lambda (port)
                  (pretty-print
                    `(benchmark-results
                       (model ,model-id)
                       (mode "drop")
                       (timestamp ,ts)
                       (n-problems ,n)
                       (correct ,correct)
                       (accuracy ,(inexact (/ correct n)))
                       (total-ms ,total-ms)
                       (problems ,results))
                    port)))
              (display (format "Results: ~a\n" outpath))))

          (let ([r (run-drop-problem (car remaining) provider i n)])
            (loop (cdr remaining) (+ i 1) (cons r results)))))))

;;; ====
;;; Main
;;; ====

(run-drop-suite!)
