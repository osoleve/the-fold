;;; user/rlm/bench-math.ss — MATH (Hendrycks) competition benchmark
;;;
;;; Competition-level math problems requiring multi-step reasoning.
;;; Exercises: think, eval, store, retrieve (for intermediate results)
;;;
;;; Run: scheme --script user/rlm/bench-math.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Data loading
;;; ====

(define (load-math-samples path)
  (let* ([p (open-input-file path)]
         [data (read p)])
    (close-input-port p)
    (cdar data)))

;;; ====
;;; Answer matching
;;; ====

(define (math-normalize s)
  ;; Strip $, \boxed{}, commas, whitespace
  (let* ([s (format "~a" s)]
         [s (math-strip-boxed s)]
         [s (math-remove-chars s '(#\$ #\, #\space))])
    s))

(define (math-strip-boxed s)
  (let ([len (string-length s)])
    (cond
      [(and (>= len 8)
            (string=? (substring s 0 7) "\\boxed{")
            (char=? (string-ref s (- len 1)) #\}))
       (substring s 7 (- len 1))]
      ;; Also handle the \boxed pattern without backslash
      [(and (>= len 7)
            (string=? (substring s 0 6) "boxed{")
            (char=? (string-ref s (- len 1)) #\}))
       (substring s 6 (- len 1))]
      [else s])))

(define (math-remove-chars s chars)
  (list->string
    (filter (lambda (c) (not (memv c chars)))
            (string->list s))))

(define (math-correct? output expected-str)
  (let* ([out (math-normalize output)]
         [exp (math-normalize expected-str)]
         [exp-num (string->number exp)])
    (if exp-num
        (output-contains-number? out exp-num)
        (string=? out exp))))

;;; ====
;;; System prompt
;;; ====

(define *math-competition-prompt*
  (string-append
    "You are a Fold/Scheme agent solving a competition math problem.\n\n"
    "The problem is stored under 'input'.\n\n"
    "Strategy:\n"
    "1. (peek 'input 2000) to read the problem\n"
    "2. (think \"...\") to plan your approach step by step\n"
    "3. (eval \"(expression)\") to compute values\n"
    "4. (store 'key value) to save intermediate results\n"
    "5. (retrieve 'key) to recall saved values\n\n"
    "For multi-step problems, store intermediate results with descriptive names:\n"
    "  (store 'area-of-triangle 24)\n"
    "  (store 'perimeter 30)\n\n"
    "Available math functions: +, -, *, /, quotient, remainder, modulo, expt,\n"
    "sqrt, floor, ceiling, round, min, max, abs, gcd, lcm, log, exp,\n"
    "sin, cos, tan, asin, acos, atan, iota, build-list, sorted\n\n"
    "Submit ONLY the numeric answer.\n"))

;;; ====
;;; Runner
;;; ====

(define (run-math-problem entry provider i total)
  (let* ([question (cdr (assq 'question entry))]
         [expected (cdr (assq 'answer entry))]
         [level (let ([l (assq 'level entry)]) (if l (cdr l) "?"))]
         [subject (let ([s (assq 'subject entry)]) (if s (cdr s) "?"))]
         [task-desc (string-append
                      "Solve this competition math problem. Report only the numeric answer.\n\n"
                      question)]
         [haystack question]
         [config (make-rlm2-config provider *math-competition-prompt*
                                    15    ; max-steps (more for hard problems)
                                    15000 ; max-fuel
                                    4000  ; max-tokens
                                    1     ; temperature numerator
                                    3     ; temperature denominator (1/3)
                                    8000  ; context-window
                                    #f)]) ; no verifier

    (display (format "  [~a/~a] ~a ~a | Expected: ~a | "
                     i total level subject expected))
    (flush-output-port)

    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task-desc haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)]
             [correct? (math-correct? output expected)])
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
          (level . ,level)
          (subject . ,subject)
          (expected . ,expected)
          (status . ,status)
          (time-ms . ,ms)
          (correct . ,correct?)
          (output . ,output)
          (trajectory . ,traj))))))

(define (run-math-suite!)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [provider (rlm-provider-vllm model-id port)]
         [samples (load-math-samples "user/rlm/data/math-competition-sample.sexp")]
         [n (length samples)])

    (display (format "\nMATH Competition (~a, ~a problems)\n" model-id n))
    (display (make-string 60 #\=))
    (display "\n")
    (flush-output-port)

    (let loop ([remaining samples] [i 1] [results '()])
      (if (null? remaining)
          (let* ([results (reverse results)]
                 [correct (length (filter (lambda (r) (cdr (assq 'correct r))) results))]
                 [total-ms (apply + (map (lambda (r) (cdr (assq 'time-ms r))) results))]
                 [ts (rlm2-current-iso8601)]
                 [outpath (format "user/rlm/bench-results-math-~a.sexp" ts)])

            ;; Summary
            (display (format "\n--- MATH Competition Summary ---\n"))
            (display (format "Score: ~a/~a (~,1f%)\n" correct n
                             (* 100.0 (/ correct n))))
            (display (format "Total time: ~as\n" (quotient total-ms 1000)))

            ;; Per-subject breakdown
            (let ([subjects '()])
              (for-each
                (lambda (r)
                  (let ([s (cdr (assq 'subject r))])
                    (unless (member s subjects)
                      (set! subjects (cons s subjects)))))
                results)
              (for-each
                (lambda (subject)
                  (let* ([sub (filter (lambda (r) (string=? (cdr (assq 'subject r)) subject))
                                      results)]
                         [sub-n (length sub)]
                         [sub-c (length (filter (lambda (r) (cdr (assq 'correct r))) sub))])
                    (display (format "  ~a: ~a/~a\n" subject sub-c sub-n))))
                (list-sort string<? subjects)))

            ;; Save results
            (call-with-port
              (open-file-output-port outpath
                (file-options no-fail) (buffer-mode block)
                (make-transcoder (utf-8-codec)))
              (lambda (port)
                (pretty-print
                  `(benchmark-results
                     (model ,model-id)
                     (mode "math-competition")
                     (timestamp ,ts)
                     (n-problems ,n)
                     (correct ,correct)
                     (accuracy ,(inexact (/ correct n)))
                     (total-ms ,total-ms)
                     (problems ,results))
                  port)))

            (display (format "Results: ~a\n" outpath)))

          (let ([r (run-math-problem (car remaining) provider i n)])
            (loop (cdr remaining) (+ i 1) (cons r results)))))))

(run-math-suite!)
