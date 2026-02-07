;;; user/rlm/bench-fewshot-parallel.ss -- Parallel few-shot benchmark
;;;
;;; Runs N trials of OOLONG 100+200 concurrently using Chez threads.
;;; vLLM continuous-batches the concurrent requests on the GPU.
;;;
;;; Run: scheme --script user/rlm/bench-fewshot-parallel.ss
;;; Env: N_TRIALS=4 (default 3)

(import (chezscheme))

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Few-Shot Examples (same as bench-fewshot.ss)
;;; ====

(define *fewshot-budget* 8000)

(define (fewshot-render-example task env-spec fuel action)
  (let* ([env (fold-left
                (lambda (env entry)
                  (rlm-env-put env (car entry) "synthetic"
                               (cadr entry) (caddr entry)))
                '()
                env-spec)]
         [state (make-rlm2-state task '() env '() '() '() '()
                                 #f fuel 0)]
         [hud (rlm2-render-state state *fewshot-budget*)]
         [action-str (let ([p (open-output-string)])
                       (write action p)
                       (get-output-string p))])
    (cons hud action-str)))

(define *fewshot-examples*
  (list
    (fewshot-render-example
      "Count records where type is 'gamma'."
      '((input chunks 55000) (task text 40))
      20000
      '(peek 'input 500))
    (fewshot-render-example
      "Find the average value for region 'east' records."
      '((input chunks 55000) (task text 50))
      20000
      '(think "I need both sum and count to compute an average. Let me peek at the format first."))
    (fewshot-render-example
      "Sum all 'value' fields where type is 'beta' AND region is 'south'."
      '((input chunks 55000) (task text 70))
      15000
      '(map-chunks 'input
        "(let ([lines (split-lines *chunk*)])\n  (apply + (map (lambda (line)\n    (if (and (string-contains? line \"type: beta\")\n             (string-contains? line \"region: south\"))\n        (let ([v (extract-after line \"value: \")])\n          (if v (string->number v) 0))\n        0))\n    lines)))"))
    (fewshot-render-example
      "Report the total count."
      '((input chunks 55000) (task text 25) (map-result result 0))
      18000
      '(begin
         (store 'total (apply + (retrieve 'map-result)))
         (submit (retrieve 'total))))
    (fewshot-render-example
      "Find the total sum of all 'value' fields from records where type is 'alpha' AND region is 'north'. Report only the numeric total."
      '((input chunks 55000) (task text 130))
      20000
      '(begin
         (map-chunks 'input
           "(let ([lines (split-lines *chunk*)])\n       (apply + (map (lambda (line)\n                       (if (and (string-contains? line \"type: alpha\")\n                                (string-contains? line \"region: north\"))\n                           (let ([v (extract-after line \"value: \")])\n                             (if v (string->number v) 0))\n                           0))\n                     lines)))")
         (store 'total (apply + (retrieve 'map-result)))
         (submit (retrieve 'total))))))

(define *few-shot-msgs*
  (apply append
    (map (lambda (pair)
           (list `((role . "user") (content . ,(car pair)))
                 `((role . "assistant") (content . ,(cdr pair)))))
         *fewshot-examples*)))

;;; ====
;;; Thread-Safe Output
;;; ====

(define *output-mutex* (make-mutex))

(define (safe-printf fmt . args)
  (with-mutex *output-mutex*
    (apply display (list (apply format (cons fmt args))))
    (flush-output-port)))

;;; ====
;;; Single Trial
;;; ====

(define (run-single-trial trial-id n-entries target-size provider)
  (let* ([label (format "trial-~a/~a" trial-id n-entries)]
         [entries (generate-entries n-entries)]
         [haystack (build-oolong-haystack entries target-size)]
         [expected (compute-answer entries *target-type* *target-region*)]
         [match-count (count-matching entries *target-type* *target-region*)]
         [task (format "Find the total sum of all 'value' fields from records where type is '~a' AND region is '~a'. Report only the numeric total."
                       *target-type* *target-region*)]
         [config (append
                   (make-rlm2-config provider *oolong-system-prompt*
                                     12 20000 2000 1 3 8000 #f)
                   (list *few-shot-msgs*))])
    (safe-printf "  [~a] Starting: ~a entries, ~a chars, expected ~a\n"
                 label n-entries (string-length haystack) expected)
    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [correct? (output-contains-number? output expected)]
             [traj (rlm2-run-result-trajectory-hash result)])
        (safe-printf "  [~a] ~a ~ams | correct: ~a | output: ~a\n"
                     label status ms correct?
                     (if (> (string-length output) 80)
                         (string-append (substring output 0 80) "...")
                         output))
        `((label . ,label)
          (trial . ,trial-id)
          (n-entries . ,n-entries)
          (haystack-chars . ,(string-length haystack))
          (expected . ,expected)
          (match-count . ,match-count)
          (status . ,status)
          (time-ms . ,ms)
          (correct . ,correct?)
          (output . ,output)
          (trajectory . ,traj))))))

;;; ====
;;; Parallel Runner (2 concurrent per trial, trials sequential)
;;; ====

(define (run-parallel-benchmark n-trials)
  (safe-printf "Few-Shot Benchmark (Nemotron) — ~a trials\n" n-trials)
  (safe-printf "============================================\n")
  (safe-printf "Per trial: 2 concurrent agents (OOLONG 100 + 200)\n\n")

  (let* ([provider (rlm-provider-vllm
                     "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4" 8000)]
         [n-tasks (* n-trials 2)]
         [results (make-vector n-tasks #f)])

    ;; Run trials sequentially, 2 tasks per trial in parallel
    (do ([i 0 (+ i 1)])
        ((= i n-trials))
      (let ([idx-100 (* i 2)]
            [idx-200 (+ (* i 2) 1)]
            [tid (+ i 1)])
        (safe-printf "\n--- Trial ~a/~a ---\n" tid n-trials)
        (let ([t100 (fork-thread
                      (lambda ()
                        (vector-set! results idx-100
                          (guard (ex [else
                                  (safe-printf "  [trial-~a/100] ERROR: ~a\n" tid
                                    (if (message-condition? ex)
                                        (condition-message ex) ex))
                                  `((label . ,(format "trial-~a/100" tid))
                                    (correct . #f) (status . error))])
                            (run-single-trial tid 100 50000 provider)))))]
              [t200 (fork-thread
                      (lambda ()
                        (vector-set! results idx-200
                          (guard (ex [else
                                  (safe-printf "  [trial-~a/200] ERROR: ~a\n" tid
                                    (if (message-condition? ex)
                                        (condition-message ex) ex))
                                  `((label . ,(format "trial-~a/200" tid))
                                    (correct . #f) (status . error))])
                            (run-single-trial tid 200 100000 provider)))))])
          (guard (ex [else (void)]) (thread-join t100))
          (guard (ex [else (void)]) (thread-join t200)))))

    ;; Collect results
    (let* ([all (vector->list results)]
           [valid (filter (lambda (r) (and r (pair? r))) all)]
           [r100 (filter (lambda (r) (and (assq 'n-entries r)
                                          (= (cdr (assq 'n-entries r)) 100)))
                         valid)]
           [r200 (filter (lambda (r) (and (assq 'n-entries r)
                                          (= (cdr (assq 'n-entries r)) 200)))
                         valid)]
           [c100 (length (filter (lambda (r) (and (assq 'correct r)
                                                   (cdr (assq 'correct r))))
                                 r100))]
           [c200 (length (filter (lambda (r) (and (assq 'correct r)
                                                   (cdr (assq 'correct r))))
                                 r200))]
           [t100 (if (null? r100) 0
                     (quotient (apply + (map (lambda (r)
                                               (or (and (assq 'time-ms r)
                                                        (cdr (assq 'time-ms r)))
                                                   0))
                                             r100))
                               (length r100)))]
           [t200 (if (null? r200) 0
                     (quotient (apply + (map (lambda (r)
                                               (or (and (assq 'time-ms r)
                                                        (cdr (assq 'time-ms r)))
                                                   0))
                                             r200))
                               (length r200)))])

      (safe-printf "\n=== RESULTS ===\n")
      (safe-printf "OOLONG 100: ~a/~a correct (avg ~ams)\n" c100 (length r100) t100)
      (safe-printf "OOLONG 200: ~a/~a correct (avg ~ams)\n" c200 (length r200) t200)
      (safe-printf "Total: ~a/~a correct\n\n" (+ c100 c200) (+ (length r100) (length r200)))

      ;; Per-trial detail
      (for-each
        (lambda (r)
          (when (and (pair? r) (assq 'label r))
            (safe-printf "  ~a: ~a ~ams\n"
                         (cdr (assq 'label r))
                         (if (and (assq 'correct r) (cdr (assq 'correct r)))
                             "PASS" "FAIL")
                         (or (and (assq 'time-ms r) (cdr (assq 'time-ms r))) 0))))
        all)

      ;; Save results
      (let* ([ts (rlm2-current-iso8601)]
             [outpath (format "user/rlm/bench-results-fewshot-parallel-~a.sexp" ts)])
        (call-with-port
          (open-file-output-port outpath
            (file-options no-fail) (buffer-mode block)
            (make-transcoder (utf-8-codec)))
          (lambda (port)
            (pretty-print
              `(benchmark-results
                 (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
                 (mode "few-shot-parallel")
                 (n-trials ,n-trials)
                 (n-fewshot ,(length *fewshot-examples*))
                 (timestamp ,ts)
                 (summary
                   ((oolong-100 ,c100 ,(length r100) ,t100)
                    (oolong-200 ,c200 ,(length r200) ,t200)))
                 (trials ,all))
              port)))
        (safe-printf "\nResults: ~a\n" outpath)))))

;;; ====
;;; Main
;;; ====

(let ([n (or (and (getenv "N_TRIALS") (string->number (getenv "N_TRIALS")))
             3)])
  (run-parallel-benchmark n))
