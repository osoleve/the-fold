;;; user/rlm/bench-fewshot.ss -- Few-shot OOLONG benchmark for Nemotron
;;;
;;; Stuffs synthetic HUD+action examples into the message history
;;; before the real HUD, testing whether in-context learning helps
;;; Nemotron emit valid S-expression actions.
;;;
;;; Run: scheme --script user/rlm/bench-fewshot.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Build Few-Shot Examples Directly
;;; ====
;;;
;;; Rather than parsing JSONL, we render HUD+action pairs from
;;; the same state reconstruction the synth generator uses.

(define *fewshot-budget* 8000)

(define (fewshot-render-example task env-spec fuel action)
  ;; Build a state and render HUD, pair with the action string
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

;; Five diverse examples at step 0 (simplest HUDs, teaches action syntax)
(define *fewshot-examples*
  (list
    ;; 1. Simple peek
    (fewshot-render-example
      "Count records where type is 'gamma'."
      '((input chunks 55000) (task text 40))
      20000
      '(peek 'input 500))

    ;; 2. Think-then-act
    (fewshot-render-example
      "Find the average value for region 'east' records."
      '((input chunks 55000) (task text 50))
      20000
      '(think "I need both sum and count to compute an average. Let me peek at the format first."))

    ;; 3. Map-chunks (the critical action Nemotron must learn)
    (fewshot-render-example
      "Sum all 'value' fields where type is 'beta' AND region is 'south'."
      '((input chunks 55000) (task text 70))
      15000
      '(map-chunks 'input
        "(let ([lines (split-lines *chunk*)])\n  (apply + (map (lambda (line)\n    (if (and (string-contains? line \"type: beta\")\n             (string-contains? line \"region: south\"))\n        (let ([v (extract-after line \"value: \")])\n          (if v (string->number v) 0))\n        0))\n    lines)))"))

    ;; 4. Store + submit
    (fewshot-render-example
      "Report the total count."
      '((input chunks 55000) (task text 25) (map-result result 0))
      18000
      '(begin
         (store 'total (apply + (retrieve 'map-result)))
         (submit (retrieve 'total))))

    ;; 5. Full one-shot begin (the ideal pattern)
    (fewshot-render-example
      "Find the total sum of all 'value' fields from records where type is 'alpha' AND region is 'north'. Report only the numeric total."
      '((input chunks 55000) (task text 130))
      20000
      '(begin
         (map-chunks 'input
           "(let ([lines (split-lines *chunk*)])\n       (apply + (map (lambda (line)\n                       (if (and (string-contains? line \"type: alpha\")\n                                (string-contains? line \"region: north\"))\n                           (let ([v (extract-after line \"value: \")])\n                             (if v (string->number v) 0))\n                           0))\n                     lines)))")
         (store 'total (apply + (retrieve 'map-result)))
         (submit (retrieve 'total))))))

(define (fewshot->messages examples)
  (apply append
    (map (lambda (pair)
           (list `((role . "user") (content . ,(car pair)))
                 `((role . "assistant") (content . ,(cdr pair)))))
         examples)))

;;; ====
;;; Runner
;;; ====

(define (run-fewshot-benchmark!)
  (display "Few-Shot OOLONG Benchmark (Nemotron)\n")
  (display "====================================\n\n")

  (let* ([few-shot-msgs (fewshot->messages *fewshot-examples*)]
         [total-chars (apply + (map (lambda (m)
                                      (string-length (cdr (assq 'content m))))
                                    few-shot-msgs))])
    (display (format "Few-shot: ~a examples, ~a messages, ~a chars\n\n"
                     (length *fewshot-examples*)
                     (length few-shot-msgs)
                     total-chars))

    (let ([provider (rlm-provider-vllm
                      "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4" 8000)])

      ;; --- OOLONG 100 ---
      (display "--- OOLONG 100 (few-shot) ---\n")
      (let* ([entries (generate-entries 100)]
             [haystack (build-oolong-haystack entries 50000)]
             [expected (compute-answer entries *target-type* *target-region*)]
             [match-count (count-matching entries *target-type* *target-region*)]
             [task (format "Find the total sum of all 'value' fields from records where type is '~a' AND region is '~a'. Report only the numeric total."
                           *target-type* *target-region*)]
             ;; Config with few-shot messages appended
             [config (append
                       (make-rlm2-config provider *oolong-system-prompt*
                                         12 20000 2000 1 3 8000 #f)
                       (list few-shot-msgs))])

        (display (format "Entries: ~a (~a matching) | Haystack: ~a chars | Expected: ~a\n"
                         100 match-count (string-length haystack) expected))
        (flush-output-port)

        (let-values ([(result ms)
                      (wall-clock-ms (lambda () (rlm2-run config task haystack)))])
          (let* ([status (rlm2-run-result-status result)]
                 [output (format "~a" (rlm2-run-result-output result))]
                 [correct? (output-contains-number? output expected)]
                 [traj (rlm2-run-result-trajectory-hash result)])
            (display (format "  Status: ~a | Time: ~ams | Correct: ~a\n" status ms correct?))
            (display (format "  Output: ~a\n"
                             (if (> (string-length output) 300)
                                 (string-append (substring output 0 300) "...")
                                 output)))
            (display (format "  Trajectory: ~a\n\n" traj))
            (flush-output-port)

            ;; --- OOLONG 200 ---
            (display "--- OOLONG 200 (few-shot) ---\n")
            (let* ([entries2 (generate-entries 200)]
                   [haystack2 (build-oolong-haystack entries2 100000)]
                   [expected2 (compute-answer entries2 *target-type* *target-region*)]
                   [match2 (count-matching entries2 *target-type* *target-region*)]
                   [config2 (append
                              (make-rlm2-config provider *oolong-system-prompt*
                                                12 20000 2000 1 3 8000 #f)
                              (list few-shot-msgs))])

              (display (format "Entries: ~a (~a matching) | Haystack: ~a chars | Expected: ~a\n"
                               200 match2 (string-length haystack2) expected2))
              (flush-output-port)

              (let-values ([(result2 ms2)
                            (wall-clock-ms (lambda () (rlm2-run config2 task haystack2)))])
                (let* ([status2 (rlm2-run-result-status result2)]
                       [output2 (format "~a" (rlm2-run-result-output result2))]
                       [correct2? (output-contains-number? output2 expected2)]
                       [traj2 (rlm2-run-result-trajectory-hash result2)])
                  (display (format "  Status: ~a | Time: ~ams | Correct: ~a\n" status2 ms2 correct2?))
                  (display (format "  Output: ~a\n"
                                   (if (> (string-length output2) 300)
                                       (string-append (substring output2 0 300) "...")
                                       output2)))
                  (display (format "  Trajectory: ~a\n\n" traj2))

                  ;; === Summary ===
                  (display "=== SUMMARY ===\n")
                  (display (format "OOLONG 100: ~a (~ams)\n"
                                   (if correct? "CORRECT" "WRONG") ms))
                  (display (format "OOLONG 200: ~a (~ams)\n"
                                   (if correct2? "CORRECT" "WRONG") ms2))
                  (display (format "Score: ~a/2\n\n"
                                   (+ (if correct? 1 0) (if correct2? 1 0))))

                  ;; Save results
                  (let* ([ts (rlm2-current-iso8601)]
                         [results
                           `(benchmark-results
                              (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
                              (mode "few-shot")
                              (n-fewshot ,(length *fewshot-examples*))
                              (fewshot-chars ,total-chars)
                              (timestamp ,ts)
                              (oolong
                                (((label . "OOLONG 100 few-shot")
                                  (n-entries . 100) (haystack-chars . ,(string-length haystack))
                                  (expected . ,expected) (match-count . ,match-count)
                                  (status . ,status) (time-ms . ,ms)
                                  (correct . ,correct?) (output . ,output)
                                  (trajectory . ,traj))
                                 ((label . "OOLONG 200 few-shot")
                                  (n-entries . 200) (haystack-chars . ,(string-length haystack2))
                                  (expected . ,expected2) (match-count . ,match2)
                                  (status . ,status2) (time-ms . ,ms2)
                                  (correct . ,correct2?) (output . ,output2)
                                  (trajectory . ,traj2)))))]
                         [outpath (format "user/rlm/bench-results-fewshot-~a.sexp" ts)])
                    (call-with-port
                      (open-file-output-port outpath
                        (file-options no-fail) (buffer-mode block)
                        (make-transcoder (utf-8-codec)))
                      (lambda (port) (pretty-print results port)))
                    (display (format "Results: ~a\n" outpath))))))))))))

;;; ====
;;; Main
;;; ====

(run-fewshot-benchmark!)
