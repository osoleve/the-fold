;;; shell/introspect/timing.ss — Time Measurement for Benchmarks
;;;
;;; Measures elapsed wall-clock and CPU time for operations.
;;; This is Shell code: impure, uses system time primitives.
;;;
;;; Capabilities:
;;;   None required — time measurement is non-destructive observation.
;;;
;;; Operations:
;;;   (time-thunk thunk) → TimingResult
;;;   (benchmark name iterations thunk) → BenchmarkResult
;;;   (compare-benchmarks results...) → ComparisonReport
;;;
;;; TimingResult:
;;;   (timing elapsed-ns cpu-ns result)
;;;
;;; BenchmarkResult:
;;;   (benchmark-result name iterations timings stats)

;;; ============================================================
;;; Time Primitives
;;; ============================================================

;;; current-nanoseconds : → Nat
;;; High-resolution monotonic time in nanoseconds.
;;; Uses Chez Scheme's (current-time 'time-monotonic).
(define (current-nanoseconds)
  (let ([t (current-time 'time-monotonic)])
       (+ (* (time-second t) 1000000000)
          (time-nanosecond t))))

;;; current-cpu-nanoseconds : → Nat
;;; CPU time consumed by this process in nanoseconds.
(define (current-cpu-nanoseconds)
  (let ([t (current-time 'time-process)])
       (+ (* (time-second t) 1000000000)
          (time-nanosecond t))))

;;; ============================================================
;;; Timing Result
;;; ============================================================

(define-record-type timing-result
  (fields
   elapsed-ns    ; Wall-clock nanoseconds
   cpu-ns        ; CPU nanoseconds consumed
   value))       ; The result of the timed computation

;;; time-thunk : (→ A) → TimingResult
;;; Execute thunk and measure elapsed time.
(define (time-thunk thunk)
  (let* ([start-wall (current-nanoseconds)]
         [start-cpu (current-cpu-nanoseconds)]
         [result (thunk)]
         [end-cpu (current-cpu-nanoseconds)]
         [end-wall (current-nanoseconds)])
        (make-timing-result
         (- end-wall start-wall)
         (- end-cpu start-cpu)
         result)))

;;; ============================================================
;;; Statistics
;;; ============================================================

(define-record-type timing-stats
  (fields
   min-ns        ; Minimum elapsed time
   max-ns        ; Maximum elapsed time
   mean-ns       ; Arithmetic mean
   median-ns     ; Median value
   stddev-ns     ; Standard deviation
   total-ns))    ; Total time for all iterations

;;; compute-stats : (List Nat) → TimingStats
;;; Compute statistics from a list of timing samples.
(define (compute-stats samples)
  (let* ([sorted (list-sort < samples)]
         [n (length sorted)]
         [total (fold-left + 0 sorted)]
         [mean (if (> n 0) (/ total n) 0)]
         [median (if (> n 0)
                     (if (odd? n)
                         (list-ref sorted (quotient n 2))
                         (/ (+ (list-ref sorted (- (quotient n 2) 1))
                               (list-ref sorted (quotient n 2)))
                            2))
                     0)]
         [min-val (if (> n 0) (car sorted) 0)]
         [max-val (if (> n 0) (list-ref sorted (- n 1)) 0)]
         [variance (if (> n 1)
                       (/ (fold-left + 0
                                     (map (lambda (x) (expt (- x mean) 2)) sorted))
                          (- n 1))
                       0)]
         [stddev (sqrt variance)])
        (make-timing-stats
         min-val
         max-val
         (inexact mean)
         (inexact median)
         (inexact stddev)
         total)))

;;; ============================================================
;;; Benchmark Result
;;; ============================================================

(define-record-type benchmark-result
  (fields
   name          ; Symbol or string identifying the benchmark
   iterations    ; Number of iterations run
   timings       ; List of individual timing results
   stats))       ; Computed timing-stats

;;; benchmark : String × Nat × (→ A) → BenchmarkResult
;;; Run a benchmark for n iterations and collect statistics.
(define (benchmark name iterations thunk)
  (let* ([timings (let loop ([i 0] [acc '()])
                       (if (>= i iterations)
                           (reverse acc)
                           (loop (+ i 1)
                                 (cons (time-thunk thunk) acc))))]
         [elapsed-samples (map timing-result-elapsed-ns timings)]
         [stats (compute-stats elapsed-samples)])
        (make-benchmark-result name iterations timings stats)))

;;; ============================================================
;;; Warm-up Support
;;; ============================================================

;;; benchmark-with-warmup : String × Nat × Nat × (→ A) → BenchmarkResult
;;; Run warmup iterations (discarded), then benchmark iterations.
(define (benchmark-with-warmup name warmup-iters bench-iters thunk)
  ;; Warmup phase (results discarded)
  (let loop ([i 0])
       (when (< i warmup-iters)
             (thunk)
             (loop (+ i 1))))
  ;; Actual benchmark
  (benchmark name bench-iters thunk))

;;; ============================================================
;;; Comparison
;;; ============================================================

(define-record-type benchmark-comparison
  (fields
   baseline      ; BenchmarkResult (the reference)
   candidates    ; List of BenchmarkResult
   ratios))      ; List of (name . ratio) where ratio = candidate/baseline

;;; compare-benchmarks : BenchmarkResult × (List BenchmarkResult) → BenchmarkComparison
;;; Compare candidates against a baseline.
(define (compare-benchmarks baseline candidates)
  (let* ([baseline-mean (timing-stats-mean-ns (benchmark-result-stats baseline))]
         [ratios (map (lambda (c)
                              (let ([c-mean (timing-stats-mean-ns (benchmark-result-stats c))])
                                   (cons (benchmark-result-name c)
                                         (if (> baseline-mean 0)
                                             (/ c-mean baseline-mean)
                                             +inf.0))))
                      candidates)])
        (make-benchmark-comparison baseline candidates ratios)))

;;; ============================================================
;;; Formatting
;;; ============================================================

;;; format-ns : Nat → String
;;; Format nanoseconds in human-readable form.
(define (format-ns ns)
  (cond
   [(>= ns 1000000000) (format "~,2fs" (/ ns 1000000000.0))]
   [(>= ns 1000000) (format "~,2fms" (/ ns 1000000.0))]
   [(>= ns 1000) (format "~,2fμs" (/ ns 1000.0))]
   [else (format "~ans" ns)]))

;;; format-timing : TimingResult → String
(define (format-timing tr)
  (format "elapsed: ~a, cpu: ~a"
          (format-ns (timing-result-elapsed-ns tr))
          (format-ns (timing-result-cpu-ns tr))))

;;; format-stats : TimingStats → String
(define (format-stats stats)
  (format "min: ~a, max: ~a, mean: ~a, median: ~a, stddev: ~a"
          (format-ns (inexact->exact (round (timing-stats-min-ns stats))))
          (format-ns (inexact->exact (round (timing-stats-max-ns stats))))
          (format-ns (inexact->exact (round (timing-stats-mean-ns stats))))
          (format-ns (inexact->exact (round (timing-stats-median-ns stats))))
          (format-ns (inexact->exact (round (timing-stats-stddev-ns stats))))))

;;; format-benchmark : BenchmarkResult → String
(define (format-benchmark br)
  (format "~a (~a iterations):\n  ~a"
          (benchmark-result-name br)
          (benchmark-result-iterations br)
          (format-stats (benchmark-result-stats br))))

;;; ============================================================
;;; Display Utilities
;;; ============================================================

;;; print-benchmark : BenchmarkResult → void
(define (print-benchmark br)
  (display (format-benchmark br))
  (newline))

;;; print-comparison : BenchmarkComparison → void
(define (print-comparison cmp)
  (display "Benchmark Comparison:\n")
  (display (format "  Baseline: ~a (mean: ~a)\n"
                   (benchmark-result-name (benchmark-comparison-baseline cmp))
                   (format-ns (inexact->exact
                               (round (timing-stats-mean-ns
                                       (benchmark-result-stats
                                        (benchmark-comparison-baseline cmp))))))))
  (for-each
   (lambda (ratio-pair)
           (display (format "  ~a: ~,2fx\n"
                            (car ratio-pair)
                            (cdr ratio-pair))))
   (benchmark-comparison-ratios cmp)))

;;; ============================================================
;;; Fuel-Aware Timing (Integration with Core)
;;; ============================================================

;;; time-with-fuel : Nat × (→ A) → (TimingResult × Nat)
;;; Execute a fuel-limited computation and return timing plus fuel remaining.
;;; Note: Thunk should return (cons result fuel-remaining) for this to work.
(define (time-with-fuel initial-fuel thunk)
  (let* ([tr (time-thunk thunk)]
         [result-pair (timing-result-value tr)])
        (values tr
                (if (pair? result-pair)
                    (cdr result-pair)
                    0))))
