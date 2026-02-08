;;; user/rlm/bench-gsm8k.ss — GSM8K benchmark
;;;
;;; Grade school math word problems. Tests a completely different action
;;; pattern: read problem → think → eval Scheme expression → submit number.
;;; No chunking needed — exercises eval capability directly.
;;;
;;; Run: scheme --script user/rlm/bench-gsm8k.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Data loading
;;; ====

(define (load-gsm8k-samples path)
  ;; Returns list of (question . answer) alists
  (let* ([p (open-input-file path)]
         [data (read p)])
    (close-input-port p)
    ;; data = ((gsm8k-sample entry1 entry2 ...))
    (cdar data)))

;;; ====
;;; Evaluation
;;; ====

(define (gsm8k-correct? output expected)
  ;; Check if the expected number appears in the output
  (let* ([expected-str (number->string expected)]
         [out (format "~a" output)])
    (output-contains-number? out expected)))

;;; ====
;;; System prompt
;;; ====

(define *gsm8k-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to solve math word problems.\n\n"
    "The problem is stored in your environment under 'input' (a single chunk).\n\n"
    "Strategy:\n"
    "1. (peek 'input 2000) to read the full problem\n"
    "2. (think \"reasoning about the approach...\") to plan your solution\n"
    "3. (eval \"(scheme-expression)\") to compute intermediate or final values\n"
    "   The eval result is stored as 'last-result.\n"
    "4. (submit answer) with the final numeric answer\n\n"
    "You can use (begin ...) to chain multiple steps in one action.\n"
    "All standard Scheme arithmetic is available: +, -, *, /, quotient, remainder,\n"
    "expt, sqrt, floor, ceiling, round, min, max, abs.\n\n"
    "Also available: (iota n) returns (0 1 2 ... n-1), (build-list n f) builds a list.\n\n"
    "IMPORTANT: Submit ONLY the final numeric answer. No units, no explanation.\n"))

;;; ====
;;; Runner
;;; ====

(define (run-gsm8k-problem entry provider i total)
  (let* ([question (cdr (assq 'question entry))]
         [expected (cdr (assq 'answer entry))]
         [task (string-append "Solve this math problem. Report only the final numeric answer.\n\n" question)]
         ;; GSM8K problems are short — just put the question as the haystack too
         [haystack question]
         [config (make-rlm2-config
                   provider *gsm8k-system-prompt*
                   10       ; max-steps (math shouldn't need many)
                   10000    ; fuel
                   4000     ; chunk-size (one chunk for the question)
                   1 3 8000 #f)])

    (display (format "  [~a/~a] Expected: ~a | " i total expected))
    (flush-output-port)

    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)]
             [correct? (gsm8k-correct? output expected)])
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
          (expected . ,expected)
          (status . ,status)
          (time-ms . ,ms)
          (correct . ,correct?)
          (output . ,output)
          (trajectory . ,traj))))))

(define (run-gsm8k-suite!)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [provider (rlm-provider-vllm model-id port)]
         [samples (load-gsm8k-samples "user/rlm/data/gsm8k-sample.sexp")]
         [n (length samples)])

    (display (format "GSM8K Benchmark (~a)\n" model-id))
    (display "===========================\n")
    (display (format "Problems: ~a\n\n" n))
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
                   [outpath (format "user/rlm/bench-results-gsm8k-~a.sexp" ts)])
              (call-with-port
                (open-file-output-port outpath
                  (file-options no-fail) (buffer-mode block)
                  (make-transcoder (utf-8-codec)))
                (lambda (port)
                  (pretty-print
                    `(benchmark-results
                       (model ,model-id)
                       (mode "gsm8k")
                       (timestamp ,ts)
                       (n-problems ,n)
                       (correct ,correct)
                       (accuracy ,(inexact (/ correct n)))
                       (total-ms ,total-ms)
                       (problems ,results))
                    port)))
              (display (format "Results: ~a\n" outpath))))

          (let ([r (run-gsm8k-problem (car remaining) provider i n)])
            (loop (cdr remaining) (+ i 1) (cons r results)))))))

;;; ====
;;; Main
;;; ====

(run-gsm8k-suite!)
