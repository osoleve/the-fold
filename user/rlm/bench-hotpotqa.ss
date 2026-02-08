;;; user/rlm/bench-hotpotqa.ss — HotpotQA multi-hop benchmark
;;;
;;; Multi-hop QA with long multi-paragraph contexts.
;;; Exercises: map-chunks, grep, store, retrieve, peek, chunk-count, think
;;;
;;; The key feature: contexts are multi-paragraph documents that require
;;; chunked processing to find information across multiple sources.
;;;
;;; Run: scheme --script user/rlm/bench-hotpotqa.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Data loading
;;; ====

(define (load-hotpotqa-samples path)
  (let* ([p (open-input-file path)]
         [data (read p)])
    (close-input-port p)
    (cdar data)))

;;; ====
;;; Answer matching
;;; ====

(define (hotpot-normalize s)
  ;; Lowercase, strip leading/trailing whitespace and punctuation
  (let* ([s (format "~a" s)]
         [s (string-downcase s)]
         [s (hotpot-strip-ends s)])
    s))

(define (string-downcase s)
  (list->string
    (map char-downcase (string->list s))))

(define (hotpot-strip-ends s)
  ;; Strip leading/trailing whitespace and common punctuation
  (let* ([chars (string->list s)]
         [chars (hotpot-drop-while hotpot-trim-char? chars)]
         [chars (reverse (hotpot-drop-while hotpot-trim-char? (reverse chars)))])
    (list->string chars)))

(define (hotpot-trim-char? c)
  (or (char-whitespace? c) (memv c '(#\. #\, #\; #\: #\! #\?))))

(define (hotpot-drop-while pred lst)
  (cond
    [(null? lst) '()]
    [(pred (car lst)) (hotpot-drop-while pred (cdr lst))]
    [else lst]))

(define (hotpot-correct? output expected-str)
  (let* ([out-str (format "~a" output)]
         [norm-out (hotpot-normalize out-str)]
         [norm-exp (hotpot-normalize expected-str)])
    (cond
      ;; Exact match after normalization
      [(string=? norm-out norm-exp) #t]
      ;; Numeric match
      [(and (string->number norm-exp)
            (output-contains-number? out-str (string->number norm-exp)))
       #t]
      ;; Expected appears in output (for when model gives extra context)
      [(hotpot-string-contains? norm-out norm-exp) #t]
      ;; Output appears in expected (for partial matches)
      [(and (> (string-length norm-out) 2)
            (hotpot-string-contains? norm-exp norm-out))
       #t]
      [else #f])))

(define (hotpot-string-contains? hay needle)
  (let ([hlen (string-length hay)]
        [nlen (string-length needle)])
    (if (or (zero? nlen) (> nlen hlen)) #f
        (let loop ([i 0])
          (cond
            [(> (+ i nlen) hlen) #f]
            [(string=? (substring hay i (+ i nlen)) needle) #t]
            [else (loop (+ i 1))])))))

;;; ====
;;; System prompt
;;; ====

(define *hotpot-prompt*
  (string-append
    "You are a Fold/Scheme agent answering a multi-hop question.\n\n"
    "The question requires finding and combining information from multiple "
    "paragraphs stored under 'input'. The paragraphs come from different "
    "Wikipedia articles, labeled with [Article Title].\n\n"
    "Strategy:\n"
    "1. (peek 'input 500) to preview the document structure\n"
    "2. (chunk-count 'input) to see how many chunks are available\n"
    "3. (grep 'input \"search term\") to find relevant paragraphs\n"
    "4. (map-chunks 'input \"expr\") to scan all chunks for information\n"
    "   The variable *chunk* holds each chunk's text.\n"
    "   Example: (map-chunks 'input "
    "\"(if (string-contains? *chunk* \\\"keyword\\\") *chunk* #f)\")\n"
    "5. (store 'key value) to save facts you discover\n"
    "6. (retrieve 'key) to recall saved facts\n"
    "7. (think \"reasoning...\") to connect the dots across sources\n"
    "8. (submit answer) with the final answer\n\n"
    "Tips:\n"
    "- Multi-hop means you need facts from 2+ paragraphs\n"
    "- Use grep first to narrow down which chunks are relevant\n"
    "- Store intermediate facts with descriptive keys\n"
    "- The answer is usually a short phrase, name, or number\n\n"
    "Available: split-lines, string-contains?, string-index-of, substr\n"))

;;; ====
;;; Runner
;;; ====

(define (run-hotpot-problem entry provider i total)
  (let* ([question (cdr (assq 'question entry))]
         [expected (cdr (assq 'answer entry))]
         [context (cdr (assq 'context entry))]
         [level (let ([l (assq 'level entry)]) (if l (cdr l) "?"))]
         [task-desc (string-append
                      "Answer this multi-hop question using the stored paragraphs.\n\n"
                      "Question: " question "\n\n"
                      "Submit only the answer — a short phrase, name, or number.")]
         ;; The context is the "haystack" — it gets chunked by the RLM2 driver
         [haystack context]
         [config (make-rlm2-config provider *hotpot-prompt*
                                    15     ; max-steps (multi-hop needs more)
                                    20000  ; max-fuel
                                    2000   ; max-tokens
                                    1      ; temperature numerator
                                    3      ; temperature denominator
                                    8000   ; context-window
                                    #f)])  ; no verifier

    (display (format "  [~a/~a] ~a | Expected: ~a | "
                     i total level expected))
    (flush-output-port)

    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task-desc haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)]
             [correct? (hotpot-correct? output expected)])
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
          (level . ,level)
          (status . ,status)
          (time-ms . ,ms)
          (correct . ,correct?)
          (output . ,output)
          (trajectory . ,traj))))))

(define (run-hotpot-suite!)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [provider (rlm-provider-vllm model-id port)]
         [samples (load-hotpotqa-samples "user/rlm/data/hotpotqa-sample.sexp")]
         [n (length samples)])

    (display (format "\nHotpotQA (~a, ~a problems)\n" model-id n))
    (display (make-string 60 #\=))
    (display "\n")
    (flush-output-port)

    (let loop ([remaining samples] [i 1] [results '()])
      (if (null? remaining)
          (let* ([results (reverse results)]
                 [correct (length (filter (lambda (r) (cdr (assq 'correct r))) results))]
                 [total-ms (apply + (map (lambda (r) (cdr (assq 'time-ms r))) results))]
                 [ts (rlm2-current-iso8601)]
                 [outpath (format "user/rlm/bench-results-hotpotqa-~a.sexp" ts)])

            ;; Summary
            (display (format "\n--- HotpotQA Summary ---\n"))
            (display (format "Score: ~a/~a (~,1f%)\n" correct n
                             (* 100.0 (/ correct n))))
            (display (format "Total time: ~as\n" (quotient total-ms 1000)))
            (flush-output-port)

            ;; Save results
            (call-with-port
              (open-file-output-port outpath
                (file-options no-fail) (buffer-mode block)
                (make-transcoder (utf-8-codec)))
              (lambda (port)
                (pretty-print
                  `(benchmark-results
                     (model ,model-id)
                     (mode "hotpotqa")
                     (timestamp ,ts)
                     (n-problems ,n)
                     (correct ,correct)
                     (accuracy ,(inexact (/ correct n)))
                     (total-ms ,total-ms)
                     (problems ,results))
                  port)))

            (display (format "Results: ~a\n" outpath)))

          (let ([r (run-hotpot-problem (car remaining) provider i n)])
            (loop (cdr remaining) (+ i 1) (cons r results)))))))

(run-hotpot-suite!)
