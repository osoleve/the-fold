;;; user/rlm/bench-rank.ss — OOLONG-Rank benchmark
;;;
;;; "Find the top-K records by value where region is X."
;;; Tests: map-chunks extraction → eval for sorting → submit ranked list.
;;; Unlike OOLONG (sum), this requires the model to collect individual
;;; records and sort them — a different action pattern.
;;;
;;; Run: scheme --script user/rlm/bench-rank.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Rank-specific evaluation
;;; ====

(define (compute-top-k entries target-type target-region k)
  ;; Returns sorted list of (id . value) pairs, descending by value
  (let* ([matching (filter
                     (lambda (e)
                       (and (string=? (caddr e) target-type)
                            (string=? (cadddr e) target-region)))
                     entries)]
         [with-ids (map (lambda (e)
                          (let* ([text (car e)]
                                 ;; Extract RECORD id: "RECORD 0042 | ..."
                                 [id-start 7]
                                 [id-end (let loop ([i id-start])
                                           (if (char=? (string-ref text i) #\space)
                                               i (loop (+ i 1))))]
                                 [id (substring text id-start id-end)])
                            (cons id (cadr e))))
                        matching)]
         [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) with-ids)])
    (if (> (length sorted) k)
        (let loop ([n 0] [rest sorted] [acc '()])
          (if (= n k) (reverse acc)
              (loop (+ n 1) (cdr rest) (cons (car rest) acc))))
        sorted)))

(define (format-expected-rank pairs)
  ;; Format as what we expect in the output: "(0042, 95)\n(0017, 88)\n..."
  (apply string-append
    (map (lambda (p)
           (format "(~a, ~a)\n" (car p) (cdr p)))
         pairs)))

(define (eval-rank-output output expected-pairs)
  ;; Check how many of the expected (id, value) pairs appear in the output.
  ;; Returns (correct total) where correct = number of expected pairs found
  ;; in the right order in the output.
  (let ([expected-ids (map car expected-pairs)])
    ;; Simple check: all expected IDs appear in the output
    (let loop ([ids expected-ids] [found 0])
      (if (null? ids)
          (values found (length expected-pairs))
          (if (string-contains-ci output (car ids))
              (loop (cdr ids) (+ found 1))
              (loop (cdr ids) found))))))

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

(define *rank-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to find and rank "
    "specific records from a large document.\n\n"
    "The document is stored in your environment under 'input'. It contains "
    "structured RECORD entries scattered among other text. Each record has:\n"
    "  RECORD NNNN | type: <type> | region: <region> | value: <number>\n\n"
    "IMPORTANT: grep only returns top-k matches, NOT all matches. "
    "For tasks requiring ALL matching records, use map-chunks.\n\n"
    "Strategy:\n"
    "1. (peek 'input 500) to understand structure\n"
    "2. (map-chunks 'input \"expr\") to extract matching records as (id . value) pairs\n"
    "3. (store 'records (apply append (retrieve 'map-result))) to flatten\n"
    "4. (eval \"(sorted records-list pred)\") or use store to sort\n"
    "5. (submit result) with the top-K pairs\n\n"
    "Available string utilities:\n"
    "  (split-lines s)            — split string into list of lines\n"
    "  (string-contains? s sub)   — check if s contains sub\n"
    "  (extract-after s marker)   — get text after marker in s\n"
    "  (sorted lst pred)          — sort a list by predicate\n\n"
    "Return results as a list of (ID, value) pairs, sorted by value descending.\n"))

;;; ====
;;; Runner
;;; ====

(define (run-rank-benchmark n-entries target-size k label provider)
  (let* ([entries (generate-entries n-entries)]
         [haystack (build-oolong-haystack entries target-size)]
         [expected (compute-top-k entries *target-type* *target-region* k)]
         [task (format "Find the top ~a records by value (highest first) where type is '~a' AND region is '~a'. Report each as (ID, value) on its own line, sorted by value descending."
                       k *target-type* *target-region*)]
         [config (make-rlm2-config
                   provider *rank-system-prompt*
                   15 30000 2000 1 3 8000 #f)])

    (display (format "\n=== BENCHMARK: ~a ===\n" label))
    (display (format "Entries: ~a | Top-~a | Haystack: ~a chars\n"
                     n-entries k (string-length haystack)))
    (display (format "Expected top-~a: ~a\n" k expected))
    (flush-output-port)

    (display "\n--- Running ---\n")
    (flush-output-port)
    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)])
        (let-values ([(found total) (eval-rank-output output expected)])
          (display (format "  Status: ~a | Time: ~a ms\n" status ms))
          (display (format "  Found: ~a/~a expected IDs in output\n" found total))
          (display (format "  Output: ~a\n" (if (> (string-length output) 300)
                                                 (string-append (substring output 0 300) "...")
                                                 output)))
          (display (format "  Trajectory: ~a\n\n" traj))
          (flush-output-port)

          `((label . ,label)
            (n-entries . ,n-entries)
            (k . ,k)
            (haystack-chars . ,(string-length haystack))
            (expected . ,expected)
            (status . ,status)
            (time-ms . ,ms)
            (found . ,found)
            (total . ,total)
            (output . ,output)
            (trajectory . ,traj)))))))

;;; ====
;;; Main
;;; ====

(define (run-rank-suite!)
  (let ([provider (rlm-provider-vllm "Qwen/Qwen3-32B-FP8" 8000)])
    (display "OOLONG-Rank Benchmark (Qwen3-32B)\n")
    (display "==================================\n")
    (flush-output-port)

    (let* ([r1 (run-rank-benchmark 100 50000 5 "Rank Top-5 / 100 entries" provider)]
           [r2 (run-rank-benchmark 200 100000 5 "Rank Top-5 / 200 entries" provider)])

      ;; Save results
      (let* ([ts (rlm2-current-iso8601)]
             [outpath (format "user/rlm/bench-results-rank-~a.sexp" ts)])
        (call-with-port
          (open-file-output-port outpath
            (file-options no-fail) (buffer-mode block)
            (make-transcoder (utf-8-codec)))
          (lambda (port)
            (pretty-print
              `(benchmark-results
                 (model "Qwen/Qwen3-32B-FP8")
                 (mode "oolong-rank")
                 (timestamp ,ts)
                 (rank ,(list r1 r2)))
              port)))
        (display (format "\nResults: ~a\n" outpath))))))

(run-rank-suite!)
