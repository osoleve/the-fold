;;; user/rlm/bench-stats.ss — OOLONG-Stats benchmark
;;;
;;; "Compute the mean and standard deviation of values where type is X."
;;; Tests: search → load → map-chunks → eval with lattice functions → submit.
;;; This is the first benchmark requiring lattice tool discovery.
;;;
;;; Run: scheme --script user/rlm/bench-stats.ss

(load "boundary/pipeline/rlm2-drive.ss")
(load "user/rlm/bench.ss")

;;; ====
;;; Stats-specific evaluation
;;; ====

(define (compute-stats entries target-type target-region)
  ;; Returns (mean . std-dev) as inexact numbers, rounded to 2 decimal places
  (let* ([values (filter-map
                   (lambda (e)
                     (and (string=? (caddr e) target-type)
                          (string=? (cadddr e) target-region)
                          (cadr e)))
                   entries)]
         [n (length values)]
         [mu (if (zero? n) 0
                 (/ (apply + values) n))]
         [var (if (<= n 1) 0
                  (/ (apply + (map (lambda (v) (expt (- v mu) 2)) values))
                     (- n 1)))]
         [sd (sqrt (inexact var))]
         [mu-inexact (inexact mu)])
    (cons (round2 mu-inexact) (round2 sd))))

(define (filter-map f lst)
  (let loop ([l lst] [acc '()])
    (if (null? l)
        (reverse acc)
        (let ([v (f (car l))])
          (if v
              (loop (cdr l) (cons v acc))
              (loop (cdr l) acc))))))

(define (round2 x)
  (/ (round (* x 100)) 100.0))

(define (eval-stats-output output expected-mean expected-sd)
  ;; Check if the output contains numbers close to the expected mean and std-dev.
  ;; Returns (mean-ok? sd-ok?)
  (let ([numbers (extract-numbers output)])
    (values
      (any-close? numbers expected-mean 0.5)
      (any-close? numbers expected-sd 1.0))))

(define (any-close? numbers target tolerance)
  (let loop ([ns numbers])
    (if (null? ns) #f
        (if (<= (abs (- (car ns) target)) tolerance)
            #t
            (loop (cdr ns))))))

(define (extract-numbers text)
  ;; Pull all numeric tokens from the output text
  (let ([len (string-length text)])
    (let loop ([i 0] [acc '()])
      (if (>= i len)
          (reverse acc)
          (let ([c (string-ref text i)])
            (if (or (char-numeric? c)
                    (and (char=? c #\-)
                         (< (+ i 1) len)
                         (char-numeric? (string-ref text (+ i 1))))
                    (char=? c #\.))
                ;; Start of a number token
                (let num-loop ([j (+ i 1)])
                  (if (and (< j len)
                           (let ([d (string-ref text j)])
                             (or (char-numeric? d) (char=? d #\.))))
                      (num-loop (+ j 1))
                      (let ([token (substring text i j)])
                        (let ([n (guard (ex [else #f])
                                   (string->number token))])
                          (if n
                              (loop j (cons n acc))
                              (loop j acc))))))
                (loop (+ i 1) acc)))))))

;;; ====
;;; System prompt
;;; ====

(define *stats-system-prompt*
  (string-append
    "You are a Fold/Scheme agent. Your task is to compute descriptive statistics "
    "over specific records from a large document.\n\n"
    "The document is stored in your environment under 'input'. It contains "
    "structured RECORD entries scattered among other text. Each record has:\n"
    "  RECORD NNNN | type: <type> | region: <region> | value: <number>\n\n"
    "IMPORTANT: grep only returns top-k matches, NOT all matches. "
    "For tasks requiring ALL matching records, use map-chunks.\n\n"
    "You have access to the full Fold lattice (36 skills). Use:\n"
    "  (search \"query\") to find tools — e.g. search for statistical functions\n"
    "  (load 'module) to load a module you discover\n"
    "  (exports 'module) to see what functions a module provides\n\n"
    "Strategy:\n"
    "1. (search \"mean standard deviation statistics\") to find stat tools\n"
    "2. (load 'statistics/summary-stats) or whatever module you find\n"
    "3. (map-chunks 'input \"expr\") to extract matching values as a list\n"
    "4. (store 'values (apply append (retrieve 'map-result))) to flatten\n"
    "5. Use the loaded statistical functions on the collected values\n"
    "6. (submit result) with the computed statistics\n\n"
    "Available string utilities:\n"
    "  (split-lines s)            — split string into list of lines\n"
    "  (string-contains? s sub)   — check if s contains sub\n"
    "  (extract-after s marker)   — get text after marker in s\n\n"
    "Report the mean and standard deviation rounded to 2 decimal places.\n"))

;;; ====
;;; Runner
;;; ====

(define (run-stats-benchmark n-entries target-size label provider)
  (let* ([entries (generate-entries n-entries)]
         [haystack (build-oolong-haystack entries target-size)]
         [expected (compute-stats entries *target-type* *target-region*)]
         [expected-mean (car expected)]
         [expected-sd (cdr expected)]
         [task (format "Compute the mean and sample standard deviation of all 'value' fields from records where type is '~a' AND region is '~a'. Report as: mean=X.XX, std-dev=Y.YY (rounded to 2 decimal places)."
                       *target-type* *target-region*)]
         [config (make-rlm2-config
                   provider *stats-system-prompt*
                   20       ; more steps — needs search+load+map+compute
                   40000 2000 1 4 8000 #f)])

    (display (format "\n=== BENCHMARK: ~a ===\n" label))
    (display (format "Entries: ~a | Haystack: ~a chars\n"
                     n-entries (string-length haystack)))
    (display (format "Expected: mean=~a, std-dev=~a\n" expected-mean expected-sd))
    (flush-output-port)

    (display "\n--- Running ---\n")
    (flush-output-port)
    (let-values ([(result ms) (wall-clock-ms (lambda () (rlm2-run config task haystack)))])
      (let* ([status (rlm2-run-result-status result)]
             [output (format "~a" (rlm2-run-result-output result))]
             [traj (rlm2-run-result-trajectory-hash result)])
        (let-values ([(mean-ok? sd-ok?) (eval-stats-output output expected-mean expected-sd)])
          (display (format "  Status: ~a | Time: ~a ms\n" status ms))
          (display (format "  Mean correct: ~a | StdDev correct: ~a\n" mean-ok? sd-ok?))
          (display (format "  Output: ~a\n" (if (> (string-length output) 300)
                                                 (string-append (substring output 0 300) "...")
                                                 output)))
          (display (format "  Trajectory: ~a\n\n" traj))
          (flush-output-port)

          `((label . ,label)
            (n-entries . ,n-entries)
            (haystack-chars . ,(string-length haystack))
            (expected-mean . ,expected-mean)
            (expected-sd . ,expected-sd)
            (status . ,status)
            (time-ms . ,ms)
            (mean-correct . ,mean-ok?)
            (sd-correct . ,sd-ok?)
            (output . ,output)
            (trajectory . ,traj)))))))

;;; ====
;;; Main
;;; ====

(define (run-stats-suite!)
  (let ([provider (rlm-provider-vllm "Qwen/Qwen3-32B-FP8" 8000)])
    (display "OOLONG-Stats Benchmark (Qwen3-32B)\n")
    (display "===================================\n")
    (flush-output-port)

    (let* ([r1 (run-stats-benchmark 100 50000 "Stats 100 entries / 50K" provider)]
           [r2 (run-stats-benchmark 200 100000 "Stats 200 entries / 100K" provider)])

      ;; Save results
      (let* ([ts (rlm2-current-iso8601)]
             [outpath (format "user/rlm/bench-results-stats-~a.sexp" ts)])
        (call-with-port
          (open-file-output-port outpath
            (file-options no-fail) (buffer-mode block)
            (make-transcoder (utf-8-codec)))
          (lambda (port)
            (pretty-print
              `(benchmark-results
                 (model "Qwen/Qwen3-32B-FP8")
                 (mode "oolong-stats")
                 (timestamp ,ts)
                 (stats ,(list r1 r2)))
              port)))
        (display (format "\nResults: ~a\n" outpath))))))

(run-stats-suite!)
