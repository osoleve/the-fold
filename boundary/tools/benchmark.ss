(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'benchmark)
(doc 'description "Benchmarking Harness for performance testing and comparison. Measures execution time, memory allocation, and throughput with statistical analysis.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'configuration)

(define *default-iterations* 1000)
(define *default-warmup-iterations* 100)
(define *time-unit* 'milliseconds)  ; or 'nanoseconds, 'microseconds

;;; Benchmark result structure
(define-record-type benchmark-result
  (fields
   name                  ; Benchmark name
   iterations            ; Number of iterations run
   samples               ; List of sample times (nanoseconds)
   mem-allocated         ; Total memory allocated (bytes)
   mean-ns               ; Mean time (nanoseconds)
   median-ns             ; Median time
   stddev-ns             ; Standard deviation
   min-ns                ; Minimum time
   max-ns                ; Maximum time
   p50-ns                ; 50th percentile
   p90-ns                ; 90th percentile
   p99-ns                ; 99th percentile
   timestamp))           ; When benchmark was run

;;; Suite result structure
(define-record-type suite-result
  (fields
   name                  ; Suite name
   benchmarks            ; List of benchmark-result records
   timestamp))

;;; ====
;;; Core Benchmarking
;;; ====

;;; benchmark : String × (→ α) × Nat → BenchmarkResult
;;; Run a benchmark with specified iterations.
(define (benchmark name thunk . args)
  (let* ([iterations (if (null? args) *default-iterations* (car args))]
         [warmup (if (or (null? args) (null? (cdr args)))
                     *default-warmup-iterations*
                     (cadr args))])
        
        ;; Warmup phase
        (do-warmup thunk warmup)
        
        ;; Force GC before measurement
        (collect)
        
        ;; Measurement phase
        (let* ([start-mem (bytes-allocated)]
               [samples (collect-samples thunk iterations)]
               [end-mem (bytes-allocated)]
               [mem-used (- end-mem start-mem)])
              
              ;; Compute statistics
              (let ([sorted (list-sort < samples)])
                   (make-benchmark-result
                    name
                    iterations
                    samples
                    mem-used
                    (mean sorted)
                    (median sorted)
                    (stddev sorted (mean sorted))
                    (list-min sorted)
                    (list-max sorted)
                    (percentile sorted 50)
                    (percentile sorted 90)
                    (percentile sorted 99)
                    (current-nanoseconds))))))

;;; do-warmup : (→ α) × Nat → void
;;; Run warmup iterations (discarded).
(define (do-warmup thunk iterations)
  (let loop ([i 0])
       (when (< i iterations)
             (thunk)
             (loop (+ i 1)))))

;;; collect-samples : (→ α) × Nat → (List Nat)
;;; Collect timing samples for each iteration.
(define (collect-samples thunk iterations)
  (let loop ([i 0] [samples '()])
       (if (>= i iterations)
           (reverse samples)
           (let* ([start (current-nanoseconds)]
                  [_ (thunk)]
                  [end (current-nanoseconds)]
                  [duration (- end start)])
                 (loop (+ i 1) (cons duration samples))))))

;;; ====
;;; Comparison Benchmarking
;;; ====

;;; benchmark-compare : (List (Pair String (→ α))) × Nat → (List BenchmarkResult)
;;; Compare multiple implementations.
(define (benchmark-compare name-thunk-pairs . args)
  (let ([iterations (if (null? args) *default-iterations* (car args))])
       (map (lambda (pair)
                    (let ([name (car pair)]
                          [thunk (cdr pair)])
                         (benchmark name thunk iterations)))
            name-thunk-pairs)))

;;; benchmark-suite : String × (List (Pair String (→ α))) → SuiteResult
;;; Run a suite of benchmarks.
(define (benchmark-suite suite-name benchmarks)
  (let ([results (map (lambda (pair)
                              (let ([name (car pair)]
                                    [thunk (cdr pair)])
                                   (benchmark name thunk)))
                      benchmarks)])
       (make-suite-result
        suite-name
        results
        (current-nanoseconds))))

;;; ====
;;; Statistical Functions
;;; ====

;;; mean : (List Nat) → Nat
(define (mean nums)
  (if (null? nums)
      0
      (quotient (apply + nums) (length nums))))

;;; median : (List Nat) → Nat
;;; Assumes list is sorted.
(define (median sorted-nums)
  (if (null? sorted-nums)
      0
      (let ([len (length sorted-nums)])
           (if (odd? len)
               (list-ref sorted-nums (quotient len 2))
               (quotient (+ (list-ref sorted-nums (quotient len 2))
                            (list-ref sorted-nums (- (quotient len 2) 1)))
                         2)))))

;;; stddev : (List Nat) × Nat → Nat
(define (stddev nums mean-val)
  (if (null? nums)
      0
      (let* ([squared-diffs (map (lambda (x)
                                         (let ([diff (- x mean-val)])
                                              (* diff diff)))
                                 nums)]
             [variance (quotient (apply + squared-diffs) (length nums))])
            (inexact->exact (floor (sqrt variance))))))

;;; percentile : (List Nat) × Nat → Nat
;;; Get nth percentile from sorted list.
(define (percentile sorted-nums p)
  (if (null? sorted-nums)
      0
      (let* ([len (length sorted-nums)]
             [idx (min (- len 1)
                       (max 0 (inexact->exact (floor (* (/ p 100.0) len)))))])
            (list-ref sorted-nums idx))))

;;; list-min : (List Nat) → Nat
(define (list-min nums)
  (if (null? nums)
      0
      (apply min nums)))

;;; list-max : (List Nat) → Nat
(define (list-max nums)
  (if (null? nums)
      0
      (apply max nums)))

;;; ====
;;; Reporting
;;; ====

;;; benchmark-report : (List BenchmarkResult) → void
;;; Display benchmark results.
(define (benchmark-report results)
  (display "
")
  (display "╔═══════════════════════════════════════════════════════════════╗
")
  (display "║                    BENCHMARK RESULTS                          ║
")
  (display "╚═══════════════════════════════════════════════════════════════╝
")
  (display "
")
  
  (for-each
   (lambda (result)
           (display "───────────────────────────────────────────────────────────────
")
           (display (format "  ~a
" (benchmark-result-name result)))
           (display "───────────────────────────────────────────────────────────────
")
           (display (format "  Iterations:    ~a
" (benchmark-result-iterations result)))
           (display (format "  Mean:          ~a
" (format-time-ns (benchmark-result-mean-ns result))))
           (display (format "  Median:        ~a
" (format-time-ns (benchmark-result-median-ns result))))
           (display (format "  Std Dev:       ~a
" (format-time-ns (benchmark-result-stddev-ns result))))
           (display (format "  Min:           ~a
" (format-time-ns (benchmark-result-min-ns result))))
           (display (format "  Max:           ~a
" (format-time-ns (benchmark-result-max-ns result))))
           (display (format "  P50:           ~a
" (format-time-ns (benchmark-result-p50-ns result))))
           (display (format "  P90:           ~a
" (format-time-ns (benchmark-result-p90-ns result))))
           (display (format "  P99:           ~a
" (format-time-ns (benchmark-result-p99-ns result))))
           (display (format "  Memory:        ~a
" (format-bytes (benchmark-result-mem-allocated result))))
           (display "
"))
   results)
  
  ;; Comparison if multiple results
  (when (> (length results) 1)
        (display "───────────────────────────────────────────────────────────────
")
        (display "  COMPARISON (relative to fastest)
")
        (display "───────────────────────────────────────────────────────────────
")
        (let* ([fastest (find-fastest results)]
               [fastest-mean (benchmark-result-mean-ns fastest)])
              (for-each
               (lambda (result)
                       (let* ([mean (benchmark-result-mean-ns result)]
                              [ratio (if (= fastest-mean 0) 1.0 (/ mean fastest-mean))])
                             (display (format "  ~a: ~ax~a
"
                                              (benchmark-result-name result)
                                              (format-ratio ratio)
                                              (if (eq? result fastest) " (fastest)" "")))))
               results))
        (display "
")))

;;; find-fastest : (List BenchmarkResult) → BenchmarkResult
(define (find-fastest results)
  (if (null? results)
      (error 'find-fastest "No results")
      (let loop ([rs (cdr results)]
                 [fastest (car results)])
           (if (null? rs)
               fastest
               (let ([current (car rs)])
                    (loop (cdr rs)
                          (if (< (benchmark-result-mean-ns current)
                                 (benchmark-result-mean-ns fastest))
                              current
                              fastest)))))))

;;; ====
;;; Formatting
;;; ====

;;; format-time-ns : Nat → String
;;; Format nanoseconds in human-readable form.
(define (format-time-ns ns)
  (cond
   [(< ns 1000)
    (format "~a ns" ns)]
   [(< ns 1000000)
    (format "~a μs" (quotient ns 1000))]
   [(< ns 1000000000)
    (format "~a ms" (quotient ns 1000000))]
   [else
    (format "~a s" (quotient ns 1000000000))]))

;;; format-bytes : Nat → String
(define (format-bytes bytes)
  (cond
   [(< bytes 1024)
    (format "~a B" bytes)]
   [(< bytes (* 1024 1024))
    (format "~a KB" (quotient bytes 1024))]
   [(< bytes (* 1024 1024 1024))
    (format "~a MB" (quotient bytes (* 1024 1024)))]
   [else
    (format "~a GB" (quotient bytes (* 1024 1024 1024)))]))

;;; format-ratio : Real → String
(define (format-ratio ratio)
  (cond
   [(< ratio 1.01) "1.00"]
   [(< ratio 10.0)
    (format "~a" (exact->inexact (/ (inexact->exact (round (* ratio 100))) 100)))]
   [else
    (format "~a" (inexact->exact (round ratio)))]))

;;; ====
;;; Persistence
;;; ====

;;; benchmark-save : BenchmarkResult × Path → void
;;; Save benchmark results to file.
(define (benchmark-save result path)
  (guard (e [else
             (display (format "Error saving benchmark: ~a
"
                              (if (condition? e)
                                  (condition-message e)
                                  e)))])
         (call-with-output-file path
                                (lambda (port)
                                        (write result port)
                                        (newline port))
                                'replace)))

;;; benchmark-load : Path → BenchmarkResult
;;; Load benchmark results from file.
(define (benchmark-load path)
  (call-with-input-file path
                        (lambda (port)
                                (read port))))

;;; ====
;;; Utility Functions
;;; ====

;;; current-nanoseconds : → Nat
(define (current-nanoseconds)
  (let ([t (current-time 'time-monotonic)])
       (+ (* (time-second t) 1000000000)
          (time-nanosecond t))))

;;; bytes-allocated : → Nat
;;; Get total bytes allocated (approximation).
(define (bytes-allocated)
  ;; Chez-specific: use statistics if available
  (guard (e [else 0])
         (let ([stats (statistics)])
              (if (list? stats)
                  (let ([mem-entry (assq 'bytes-allocated stats)])
                       (if mem-entry (cdr mem-entry) 0))
                  0))))

;;; sort : (List α) × (α × α → Bool) → (List α)
;;; Simple insertion sort.
(define (sort lst cmp)
  (if (null? lst)
      '()
      (insert (car lst) (sort (cdr lst) cmp) cmp)))

(define (insert x sorted cmp)
  (cond
   [(null? sorted) (list x)]
   [(cmp x (car sorted)) (cons x sorted)]
   [else (cons (car sorted) (insert x (cdr sorted) cmp))]))

;;; ====
;;; Examples and Presets
;;; ====

;;; Quick benchmark presets
(define (quick-bench thunk)
  "Quick benchmark with 100 iterations"
  (benchmark "quick" thunk 100))

(define (thorough-bench thunk)
  "Thorough benchmark with 10000 iterations"
  (benchmark "thorough" thunk 10000))

;;; Example benchmark suite
(define (example-benchmark-suite)
  (benchmark-suite
   "Core Operations"
   `(("cons" . ,(lambda () (cons 1 2)))
     ("list" . ,(lambda () (list 1 2 3 4 5)))
     ("append" . ,(lambda () (append '(1 2) '(3 4))))
     ("reverse" . ,(lambda () (reverse '(1 2 3 4 5)))))))

(display "
")
(display "Benchmarking harness loaded.
")
(display "
")
(display "Usage:
")
(display "  (benchmark \"name\" thunk [iterations])  - Run benchmark
")
(display "  (benchmark-compare pairs [iterations])  - Compare implementations
")
(display "  (benchmark-suite \"name\" benchmarks)     - Run suite
")
(display "  (benchmark-report results)              - Display report
")
(display "
")
(display "Quick Access:
")
(display "  (quick-bench thunk)                     - Quick test (100 iter)
")
(display "  (thorough-bench thunk)                  - Thorough test (10k iter)
")
(display "  (example-benchmark-suite)               - Example suite
")
(display "
")
