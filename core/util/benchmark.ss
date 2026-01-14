;;; core/util/benchmark.ss — Benchmark Harness with Regression Tracking
;;;
;;; A pure benchmark definition DSL with statistical analysis and
;;; regression detection capabilities.
;;;
;;; This is Core code: the data structures and analysis are pure.
;;; Timing primitives use Chez Scheme's time facilities.
;;;
;;; Features:
;;;   - Benchmark definition DSL (name, setup, teardown, iterations)
;;;   - Statistical analysis (mean, std dev, percentiles)
;;;   - Memory allocation tracking
;;;   - Warmup runs before measurement
;;;   - Benchmark suites and grouping
;;;   - Results serialization (S-expression format)
;;;   - Comparison between runs (regression detection)
;;;   - Baseline storage and delta reporting
;;;   - Integration with profiler (core/util/profile.ss)
;;;
;;; API:
;;;   (define-benchmark name . options)  ; DSL for benchmark definition
;;;   (make-benchmark ...)               ; Programmatic creation
;;;   (run-benchmark bench)              ; Execute benchmark
;;;   (run-suite suite)                  ; Run benchmark suite
;;;   (compare-results baseline current) ; Regression detection
;;;   (benchmark-report results)         ; Generate report
;;;   (serialize-results results)        ; S-expression output
;;;   (deserialize-results sexp)         ; Load from S-expression
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Configuration
;;; ====

(define *default-iterations* 100)
(define *default-warmup* 10)
(define *default-percentiles* '(50 90 95 99))
(define *regression-threshold* 0.10)  ; 10% regression threshold

;;; ====
;;; Timing Primitives (Chez Scheme Specific)
;;; ====

;;; current-nanoseconds : → Nat
(define (current-nanoseconds)
  (let ([t (current-time 'time-monotonic)])
       (+ (* (time-second t) 1000000000)
          (time-nanosecond t))))

;;; current-cpu-nanoseconds : → Nat
(define (current-cpu-nanoseconds)
  (let ([t (current-time 'time-process)])
       (+ (* (time-second t) 1000000000)
          (time-nanosecond t))))

;;; bytes-currently-allocated : → Nat
(define (bytes-currently-allocated)
  (guard (e [else 0])
         (bytes-allocated)))

;;; ====
;;; Benchmark Definition
;;; ====

;;; Benchmark structure:
;;;   (benchmark
;;;     (name . symbol)
;;;     (description . string)
;;;     (thunk . procedure)         ; The code to benchmark
;;;     (setup . procedure|#f)      ; Optional setup before each run
;;;     (teardown . procedure|#f)   ; Optional teardown after each run
;;;     (iterations . nat)          ; Number of iterations
;;;     (warmup . nat)              ; Warmup iterations
;;;     (tags . list))              ; Classification tags

;;; make-benchmark : Symbol × (→ α) × Alist → Benchmark
(define (make-benchmark name thunk . opts)
  (let* ([props (parse-benchmark-opts opts)]
         [description (assq-default props 'description "")]
         [setup (assq-default props 'setup #f)]
         [teardown (assq-default props 'teardown #f)]
         [iterations (assq-default props 'iterations *default-iterations*)]
         [warmup (assq-default props 'warmup *default-warmup*)]
         [tags (assq-default props 'tags '())])
        `(benchmark
          (name . ,name)
          (description . ,description)
          (thunk . ,thunk)
          (setup . ,setup)
          (teardown . ,teardown)
          (iterations . ,iterations)
          (warmup . ,warmup)
          (tags . ,tags))))

;;; parse-benchmark-opts : (List α) → Alist
(define (parse-benchmark-opts opts)
  (let loop ([rest opts] [acc '()])
       (cond
        [(null? rest) acc]
        [(null? (cdr rest)) acc]  ; Ignore unpaired
        [else
         (let ([key (car rest)]
               [val (cadr rest)])
              (loop (cddr rest)
                    (cons (cons (if (symbol? key)
                                    key
                                    (string->symbol (symbol->string key)))
                                val)
                          acc)))])))

;;; assq-default : Alist × Symbol × α → α
(define (assq-default alist key default)
  (let ([entry (assq key alist)])
       (if entry (cdr entry) default)))

;;; benchmark? : α → Boolean
(define (benchmark? x)
  (and (pair? x) (eq? (car x) 'benchmark)))

;;; benchmark-get : Benchmark × Symbol → α
(define (benchmark-get b key)
  (let ([entry (assq key (cdr b))])
       (and entry (cdr entry))))

(define (benchmark-name b) (benchmark-get b 'name))
(define (benchmark-description b) (benchmark-get b 'description))
(define (benchmark-thunk b) (benchmark-get b 'thunk))
(define (benchmark-setup b) (benchmark-get b 'setup))
(define (benchmark-teardown b) (benchmark-get b 'teardown))
(define (benchmark-iterations b) (benchmark-get b 'iterations))
(define (benchmark-warmup b) (benchmark-get b 'warmup))
(define (benchmark-tags b) (benchmark-get b 'tags))

;;; ====
;;; Benchmark Suite
;;; ====

;;; Suite structure:
;;;   (suite
;;;     (name . symbol)
;;;     (description . string)
;;;     (benchmarks . list)
;;;     (tags . list))

;;; make-suite : Symbol × (List Benchmark) × Alist → Suite
(define (make-suite name benchmarks . opts)
  (let* ([props (parse-benchmark-opts opts)]
         [description (assq-default props 'description "")]
         [tags (assq-default props 'tags '())])
        `(suite
          (name . ,name)
          (description . ,description)
          (benchmarks . ,benchmarks)
          (tags . ,tags))))

;;; suite? : α → Boolean
(define (suite? x)
  (and (pair? x) (eq? (car x) 'suite)))

;;; suite-get : Suite × Symbol → α
(define (suite-get s key)
  (let ([entry (assq key (cdr s))])
       (and entry (cdr entry))))

(define (suite-name s) (suite-get s 'name))
(define (suite-description s) (suite-get s 'description))
(define (suite-benchmarks s) (suite-get s 'benchmarks))
(define (suite-tags s) (suite-get s 'tags))

;;; ====
;;; Statistical Functions
;;; ====

;;; sum : (List Number) → Number
(define (sum nums)
  (fold-left + 0 nums))

;;; mean : (List Number) → Number
(define (mean nums)
  (if (null? nums)
      0
      (/ (sum nums) (length nums))))

;;; variance : (List Number) × Number → Number
(define (variance nums mean-val)
  (if (or (null? nums) (null? (cdr nums)))
      0
      (let ([squared-diffs (map (lambda (x)
                                        (let ([d (- x mean-val)])
                                             (* d d)))
                                nums)])
           (/ (sum squared-diffs) (- (length nums) 1)))))

;;; stddev : (List Number) × Number → Number
(define (stddev nums mean-val)
  (sqrt (variance nums mean-val)))

;;; percentile : (List Number) × Nat → Number
(define (percentile sorted-nums p)
  (if (null? sorted-nums)
      0
      (let* ([n (length sorted-nums)]
             [idx (min (- n 1)
                       (max 0 (inexact->exact (floor (* (/ p 100.0) n)))))])
            (list-ref sorted-nums idx))))

;;; percentiles : (List Number) × (List Nat) → Alist
(define (percentiles sorted-nums ps)
  (map (lambda (p)
               (cons p (percentile sorted-nums p)))
       ps))

;;; list-min : (List Number) → Number
(define (list-min nums)
  (if (null? nums)
      +inf.0
      (apply min nums)))

;;; list-max : (List Number) → Number
(define (list-max nums)
  (if (null? nums)
      -inf.0
      (apply max nums)))

;;; median : (List Number) → Number
(define (median sorted-nums)
  (if (null? sorted-nums)
      0
      (let ([n (length sorted-nums)])
           (if (odd? n)
               (list-ref sorted-nums (quotient n 2))
               (/ (+ (list-ref sorted-nums (- (quotient n 2) 1))
                     (list-ref sorted-nums (quotient n 2)))
                  2)))))

;;; iqr : (List Number) → Number
(define (iqr sorted-nums)
  (if (null? sorted-nums)
      0
      (- (percentile sorted-nums 75)
         (percentile sorted-nums 25))))

;;; ====
;;; Timing Sample Collection
;;; ====

;;; Sample structure:
;;;   (sample
;;;     (elapsed-ns . nat)
;;;     (cpu-ns . nat)
;;;     (memory-delta . integer))

;;; make-sample : Nat × Nat × Integer → Sample
(define (make-sample elapsed-ns cpu-ns memory-delta)
  `(sample
    (elapsed-ns . ,elapsed-ns)
    (cpu-ns . ,cpu-ns)
    (memory-delta . ,memory-delta)))

;;; sample? : α → Boolean
(define (sample? x)
  (and (pair? x) (eq? (car x) 'sample)))

(define (sample-elapsed-ns s)
  (cdr (assq 'elapsed-ns (cdr s))))

(define (sample-cpu-ns s)
  (cdr (assq 'cpu-ns (cdr s))))

(define (sample-memory-delta s)
  (cdr (assq 'memory-delta (cdr s))))

;;; time-thunk : (→ α) → (Values α Sample)
(define (time-thunk thunk)
  (let* ([mem-before (bytes-currently-allocated)]
         [start-wall (current-nanoseconds)]
         [start-cpu (current-cpu-nanoseconds)]
         [result (thunk)]
         [end-cpu (current-cpu-nanoseconds)]
         [end-wall (current-nanoseconds)]
         [mem-after (bytes-currently-allocated)])
        (values result
                (make-sample
                 (- end-wall start-wall)
                 (- end-cpu start-cpu)
                 (- mem-after mem-before)))))

;;; warmup-benchmark : Benchmark → Void
(define (warmup-benchmark bench)
  (let ([thunk (benchmark-thunk bench)]
        [setup (benchmark-setup bench)]
        [teardown (benchmark-teardown bench)]
        [n (benchmark-warmup bench)])
       (let loop ([i 0])
            (when (< i n)
                  (when setup (setup))
                  (thunk)
                  (when teardown (teardown))
                  (loop (+ i 1))))))

;;; collect-samples : Benchmark → (List Sample)
(define (collect-samples bench)
  (let ([thunk (benchmark-thunk bench)]
        [setup (benchmark-setup bench)]
        [teardown (benchmark-teardown bench)]
        [n (benchmark-iterations bench)])
       (let loop ([i 0] [samples '()])
            (if (>= i n)
                (reverse samples)
                (begin
                 (when setup (setup))
                 (let-values ([(_ sample) (time-thunk thunk)])
                             (when teardown (teardown))
                             (loop (+ i 1) (cons sample samples))))))))

;;; ====
;;; Benchmark Results
;;; ====

;;; BenchmarkResult structure:
;;;   (benchmark-result
;;;     (name . symbol)
;;;     (iterations . nat)
;;;     (warmup . nat)
;;;     (samples . list)          ; Raw samples
;;;     (stats . alist)           ; Computed statistics
;;;     (timestamp . nat))        ; When benchmark was run

;;; make-benchmark-result : Symbol × Nat × Nat × (List Sample) × Nat → BenchmarkResult
(define (make-benchmark-result name iterations warmup samples timestamp)
  (let* ([elapsed-times (map sample-elapsed-ns samples)]
         [cpu-times (map sample-cpu-ns samples)]
         [memory-deltas (map sample-memory-delta samples)]
         [sorted-elapsed (list-sort < elapsed-times)]
         [sorted-cpu (list-sort < cpu-times)]
         [sorted-memory (list-sort < memory-deltas)]
         [mean-elapsed (mean elapsed-times)]
         [mean-cpu (mean cpu-times)]
         [mean-memory (mean memory-deltas)])
        `(benchmark-result
          (name . ,name)
          (iterations . ,iterations)
          (warmup . ,warmup)
          (samples . ,samples)
          (stats .
                 ((elapsed-ns .
                              ((mean . ,mean-elapsed)
                               (stddev . ,(stddev elapsed-times mean-elapsed))
                               (min . ,(list-min elapsed-times))
                               (max . ,(list-max elapsed-times))
                               (median . ,(median sorted-elapsed))
                               (iqr . ,(iqr sorted-elapsed))
                               (percentiles . ,(percentiles sorted-elapsed *default-percentiles*))))
                  (cpu-ns .
                          ((mean . ,mean-cpu)
                           (stddev . ,(stddev cpu-times mean-cpu))
                           (min . ,(list-min cpu-times))
                           (max . ,(list-max cpu-times))
                           (median . ,(median sorted-cpu))))
                  (memory .
                          ((mean . ,mean-memory)
                           (total . ,(sum memory-deltas))
                           (min . ,(list-min memory-deltas))
                           (max . ,(list-max memory-deltas))))))
          (timestamp . ,timestamp))))

;;; benchmark-result? : α → Boolean
(define (benchmark-result? x)
  (and (pair? x) (eq? (car x) 'benchmark-result)))

;;; result-get : BenchmarkResult × Symbol → α
(define (result-get r key)
  (let ([entry (assq key (cdr r))])
       (and entry (cdr entry))))

(define (result-name r) (result-get r 'name))
(define (result-iterations r) (result-get r 'iterations))
(define (result-warmup r) (result-get r 'warmup))
(define (result-samples r) (result-get r 'samples))
(define (result-stats r) (result-get r 'stats))
(define (result-timestamp r) (result-get r 'timestamp))

;;; result-stat : BenchmarkResult × Symbol × Symbol → (Option Number)
(define (result-stat r category key)
  (let ([cat (assq category (result-stats r))])
       (if cat
           (let ([entry (assq key (cdr cat))])
                (if entry (cdr entry) #f))
           #f)))

;;; ====
;;; Suite Results
;;; ====

;;; make-suite-result : Symbol × (List BenchmarkResult) × Nat → SuiteResult
(define (make-suite-result name results timestamp)
  `(suite-result
    (name . ,name)
    (results . ,results)
    (timestamp . ,timestamp)))

;;; suite-result? : α → Boolean
(define (suite-result? x)
  (and (pair? x) (eq? (car x) 'suite-result)))

;;; suite-result-get : SuiteResult × Symbol → α
(define (suite-result-get sr key)
  (let ([entry (assq key (cdr sr))])
       (and entry (cdr entry))))

(define (suite-result-name sr) (suite-result-get sr 'name))
(define (suite-result-results sr) (suite-result-get sr 'results))
(define (suite-result-timestamp sr) (suite-result-get sr 'timestamp))

;;; ====
;;; Running Benchmarks
;;; ====

;;; run-benchmark : Benchmark → BenchmarkResult
(define (run-benchmark bench)
  ;; Force GC before measurement
  (collect)
  
  ;; Warmup phase
  (warmup-benchmark bench)
  
  ;; Force GC again after warmup
  (collect)
  
  ;; Collection phase
  (let ([samples (collect-samples bench)])
       (make-benchmark-result
        (benchmark-name bench)
        (benchmark-iterations bench)
        (benchmark-warmup bench)
        samples
        (current-nanoseconds))))

;;; run-suite : Suite → SuiteResult
(define (run-suite suite)
  (let ([results (map run-benchmark (suite-benchmarks suite))])
       (make-suite-result
        (suite-name suite)
        results
        (current-nanoseconds))))

;;; ====
;;; Comparison and Regression Detection
;;; ====

;;; ComparisonResult structure:
;;;   (comparison
;;;     (baseline-name . symbol)
;;;     (current-name . symbol)
;;;     (elapsed-ratio . number)    ; current/baseline (>1 = slower)
;;;     (cpu-ratio . number)
;;;     (memory-ratio . number)
;;;     (regression? . boolean)     ; True if ratio > threshold
;;;     (improvement? . boolean)    ; True if ratio < (1 - threshold)
;;;     (delta-ns . number)         ; Absolute change in elapsed ns
;;;     (delta-pct . number))       ; Percentage change

;;; compare-results : BenchmarkResult × BenchmarkResult → Comparison
(define (compare-results baseline current)
  (let* ([base-elapsed (result-stat baseline 'elapsed-ns 'mean)]
         [curr-elapsed (result-stat current 'elapsed-ns 'mean)]
         [base-cpu (result-stat baseline 'cpu-ns 'mean)]
         [curr-cpu (result-stat current 'cpu-ns 'mean)]
         [base-mem (result-stat baseline 'memory 'mean)]
         [curr-mem (result-stat current 'memory 'mean)]
         [elapsed-ratio (if (and base-elapsed (> base-elapsed 0))
                            (/ curr-elapsed base-elapsed)
                            1.0)]
         [cpu-ratio (if (and base-cpu (> base-cpu 0))
                        (/ curr-cpu base-cpu)
                        1.0)]
         [memory-ratio (if (and base-mem (not (zero? base-mem)))
                           (/ curr-mem base-mem)
                           1.0)]
         [delta-ns (- curr-elapsed base-elapsed)]
         [delta-pct (* 100.0 (- elapsed-ratio 1.0))])
        `(comparison
          (baseline-name . ,(result-name baseline))
          (current-name . ,(result-name current))
          (elapsed-ratio . ,elapsed-ratio)
          (cpu-ratio . ,cpu-ratio)
          (memory-ratio . ,memory-ratio)
          (regression? . ,(> elapsed-ratio (+ 1.0 *regression-threshold*)))
          (improvement? . ,(< elapsed-ratio (- 1.0 *regression-threshold*)))
          (delta-ns . ,delta-ns)
          (delta-pct . ,delta-pct))))

;;; comparison? : α → Boolean
(define (comparison? x)
  (and (pair? x) (eq? (car x) 'comparison)))

;;; comparison-get : Comparison × Symbol → α
(define (comparison-get c key)
  (let ([entry (assq key (cdr c))])
       (and entry (cdr entry))))

(define (comparison-regression? c) (comparison-get c 'regression?))
(define (comparison-improvement? c) (comparison-get c 'improvement?))
(define (comparison-elapsed-ratio c) (comparison-get c 'elapsed-ratio))
(define (comparison-delta-pct c) (comparison-get c 'delta-pct))

;;; compare-suite-results : SuiteResult × SuiteResult → (List Comparison)
(define (compare-suite-results baseline-suite current-suite)
  (let ([baseline-results (suite-result-results baseline-suite)]
        [current-results (suite-result-results current-suite)])
       (filter-map
        (lambda (curr)
                (let ([base (find (lambda (b)
                                          (eq? (result-name b) (result-name curr)))
                                  baseline-results)])
                     (and base (compare-results base curr))))
        current-results)))

;;; filter-map : (α → (Option β)) × (List α) → (List β)
(define (filter-map f lst)
  (fold-right (lambda (x acc)
                      (let ([result (f x)])
                           (if result
                               (cons result acc)
                               acc)))
              '()
              lst))

;;; any-regressions? : (List Comparison) → Boolean
(define (any-regressions? comparisons)
  (ormap comparison-regression? comparisons))

;;; ====
;;; Baseline Storage
;;; ====

;;; Baseline structure (for file storage):
;;;   (baseline
;;;     (version . 1)
;;;     (name . symbol)
;;;     (created . timestamp)
;;;     (results . suite-result))

;;; make-baseline : Symbol × SuiteResult → Baseline
(define (make-baseline name suite-result)
  `(baseline
    (version . 1)
    (name . ,name)
    (created . ,(current-nanoseconds))
    (results . ,suite-result)))

;;; baseline? : α → Boolean
(define (baseline? x)
  (and (pair? x) (eq? (car x) 'baseline)))

;;; baseline-get : Baseline × Symbol → α
(define (baseline-get b key)
  (let ([entry (assq key (cdr b))])
       (and entry (cdr entry))))

(define (baseline-name b) (baseline-get b 'name))
(define (baseline-created b) (baseline-get b 'created))
(define (baseline-results b) (baseline-get b 'results))

;;; ====
;;; Serialization (S-expression format)
;;; ====

;;; serialize-results : (BenchmarkResult | SuiteResult) → Sexp
(define (serialize-results results)
  (cond
   [(benchmark-result? results)
    ;; Remove raw samples for compact storage
    (let ([stripped (filter (lambda (e) (not (eq? (car e) 'samples)))
                            (cdr results))])
         (cons 'benchmark-result stripped))]
   [(suite-result? results)
    (let ([serialized-results (map serialize-results (suite-result-results results))])
         `(suite-result
           (name . ,(suite-result-name results))
           (results . ,serialized-results)
           (timestamp . ,(suite-result-timestamp results))))]
   [(baseline? results)
    results]
   [else results]))

;;; deserialize-results : Sexp → (BenchmarkResult | SuiteResult)
(define (deserialize-results sexp)
  sexp)

;;; ====
;;; Formatting Utilities
;;; ====

;;; format-ns : Number → String
(define (format-ns ns)
  (cond
   [(>= ns 1000000000) (format "~,2fs" (/ ns 1000000000.0))]
   [(>= ns 1000000) (format "~,2fms" (/ ns 1000000.0))]
   [(>= ns 1000) (format "~,2fus" (/ ns 1000.0))]
   [else (format "~ans" (inexact->exact (round ns)))]))

;;; format-bytes : Number → String
(define (format-bytes bytes)
  (let ([abs-bytes (abs bytes)]
        [sign (if (< bytes 0) "-" "")])
       (cond
        [(>= abs-bytes (* 1024 1024 1024))
         (format "~a~,2fGB" sign (/ abs-bytes (* 1024.0 1024.0 1024.0)))]
        [(>= abs-bytes (* 1024 1024))
         (format "~a~,2fMB" sign (/ abs-bytes (* 1024.0 1024.0)))]
        [(>= abs-bytes 1024)
         (format "~a~,2fKB" sign (/ abs-bytes 1024.0))]
        [else (format "~a~aB" sign abs-bytes)])))

;;; format-ratio : Number → String
(define (format-ratio ratio)
  (format "~,2fx" ratio))

;;; format-pct : Number → String
(define (format-pct pct)
  (format "~a~,1f%"
          (if (>= pct 0) "+" "")
          pct))

;;; ====
;;; Reporting
;;; ====

;;; benchmark-report : BenchmarkResult → Void
(define (benchmark-report result)
  (display "\n")
  (display "----\n")
  (display (format "  Benchmark: ~a\n" (result-name result)))
  (display "----\n")
  (display (format "  Iterations: ~a (warmup: ~a)\n"
                   (result-iterations result)
                   (result-warmup result)))
  (newline)
  
  ;; Elapsed time stats
  (display "  Elapsed Time:\n")
  (display (format "    Mean:   ~a\n"
                   (format-ns (result-stat result 'elapsed-ns 'mean))))
  (display (format "    StdDev: ~a\n"
                   (format-ns (result-stat result 'elapsed-ns 'stddev))))
  (display (format "    Min:    ~a\n"
                   (format-ns (result-stat result 'elapsed-ns 'min))))
  (display (format "    Max:    ~a\n"
                   (format-ns (result-stat result 'elapsed-ns 'max))))
  (display (format "    Median: ~a\n"
                   (format-ns (result-stat result 'elapsed-ns 'median))))
  (display (format "    IQR:    ~a\n"
                   (format-ns (result-stat result 'elapsed-ns 'iqr))))
  
  ;; Percentiles
  (let ([pcts (result-stat result 'elapsed-ns 'percentiles)])
       (when pcts
             (display "    Percentiles:\n")
             (for-each (lambda (p)
                               (display (format "      P~a: ~a\n"
                                                (car p)
                                                (format-ns (cdr p)))))
                       pcts)))
  
  ;; CPU time stats
  (display "\n  CPU Time:\n")
  (display (format "    Mean:   ~a\n"
                   (format-ns (result-stat result 'cpu-ns 'mean))))
  (display (format "    StdDev: ~a\n"
                   (format-ns (result-stat result 'cpu-ns 'stddev))))
  
  ;; Memory stats
  (display "\n  Memory:\n")
  (display (format "    Mean:  ~a/iter\n"
                   (format-bytes (result-stat result 'memory 'mean))))
  (display (format "    Total: ~a\n"
                   (format-bytes (result-stat result 'memory 'total))))
  
  (display "\n"))

;;; suite-report : SuiteResult → Void
(define (suite-report suite-result)
  (display "\n")
  (display "====\n")
  (display (format "  BENCHMARK SUITE: ~a\n" (suite-result-name suite-result)))
  (display "====\n")
  
  (for-each benchmark-report (suite-result-results suite-result))
  
  ;; Summary table
  (display "====\n")
  (display "  SUMMARY\n")
  (display "====\n")
  (display (format "  ~a~a~a~a\n"
                   (string-pad-right "Name" 25 #\space)
                   (string-pad-right "Mean" 15 #\space)
                   (string-pad-right "StdDev" 15 #\space)
                   "Memory/iter"))
  (display "  ")
  (display (make-string 70 #\-))
  (newline)
  
  (for-each
   (lambda (r)
           (display (format "  ~a~a~a~a\n"
                            (string-pad-right (symbol->string (result-name r)) 25 #\space)
                            (string-pad-right (format-ns (result-stat r 'elapsed-ns 'mean)) 15 #\space)
                            (string-pad-right (format-ns (result-stat r 'elapsed-ns 'stddev)) 15 #\space)
                            (format-bytes (result-stat r 'memory 'mean)))))
   (suite-result-results suite-result))
  
  (display "\n"))

;;; comparison-report : (List Comparison) → Void
(define (comparison-report comparisons)
  (display "\n")
  (display "====\n")
  (display "  REGRESSION ANALYSIS\n")
  (display "====\n")
  
  (let ([regressions (filter comparison-regression? comparisons)]
        [improvements (filter comparison-improvement? comparisons)]
        [neutral (filter (lambda (c)
                                 (and (not (comparison-regression? c))
                                      (not (comparison-improvement? c))))
                         comparisons)])
       
       (when (pair? regressions)
             (display "\n  REGRESSIONS (>10% slower):\n")
             (for-each
              (lambda (c)
                      (display (format "    ~a: ~a (~a)\n"
                                       (comparison-get c 'current-name)
                                       (format-ratio (comparison-elapsed-ratio c))
                                       (format-pct (comparison-delta-pct c)))))
              regressions))
       
       (when (pair? improvements)
             (display "\n  IMPROVEMENTS (>10% faster):\n")
             (for-each
              (lambda (c)
                      (display (format "    ~a: ~a (~a)\n"
                                       (comparison-get c 'current-name)
                                       (format-ratio (comparison-elapsed-ratio c))
                                       (format-pct (comparison-delta-pct c)))))
              improvements))
       
       (when (pair? neutral)
             (display "\n  STABLE (within 10%):\n")
             (for-each
              (lambda (c)
                      (display (format "    ~a: ~a (~a)\n"
                                       (comparison-get c 'current-name)
                                       (format-ratio (comparison-elapsed-ratio c))
                                       (format-pct (comparison-delta-pct c)))))
              neutral))
       
       (display "\n")
       
       (if (null? regressions)
           (display "  No regressions detected.\n")
           (display (format "  WARNING: ~a regression(s) detected!\n"
                            (length regressions))))
       
       (display "\n")))

;;; ====
;;; Profiler Integration
;;; ====

;;; benchmark-with-profiler : Benchmark × Nat → BenchmarkResult
(define (benchmark-with-profiler bench fuel)
  (let* ([thunk (benchmark-thunk bench)]
         ;; Note: Caller must load profile.ss and pass profile-expr
         [wrapped-thunk (lambda ()
                                (thunk))])
        (run-benchmark bench)))

;;; ====
;;; DSL Macros
;;; ====

;;; define-benchmark : Creates a benchmark definition.
;;;
;;; Usage:
;;;   (define-benchmark my-bench
;;;     #:thunk (lambda () (fib 20))
;;;     #:iterations 1000
;;;     #:warmup 100
;;;     #:setup (lambda () (set! state (make-state)))
;;;     #:teardown (lambda () (cleanup!))
;;;     #:description "Benchmark fibonacci"
;;;     #:tags (math recursion))

(define-syntax define-benchmark
  (syntax-rules ()
                [(_ name . options)
                 (define name
                   (parse-benchmark-definition 'name 'options))]))

;;; parse-benchmark-definition : Symbol × (List Sexp) → Benchmark
(define (parse-benchmark-definition name options)
  (let loop ([rest options]
             [thunk #f]
             [iterations *default-iterations*]
             [warmup *default-warmup*]
             [setup #f]
             [teardown #f]
             [description ""]
             [tags '()])
       (if (null? rest)
           (if thunk
               (make-benchmark name thunk
                               'iterations iterations
                               'warmup warmup
                               'setup setup
                               'teardown teardown
                               'description description
                               'tags tags)
               (error 'define-benchmark "Missing #:thunk" name))
           (let ([key (car rest)]
                 [rest2 (cdr rest)])
                (if (null? rest2)
                    (error 'define-benchmark "Odd number of keyword arguments" name)
                    (let ([val (car rest2)]
                          [rest3 (cdr rest2)])
                         (case key
                               [(#:thunk) (loop rest3 val iterations warmup setup teardown description tags)]
                               [(#:iterations) (loop rest3 thunk val warmup setup teardown description tags)]
                               [(#:warmup) (loop rest3 thunk iterations val setup teardown description tags)]
                               [(#:setup) (loop rest3 thunk iterations warmup val teardown description tags)]
                               [(#:teardown) (loop rest3 thunk iterations warmup setup val description tags)]
                               [(#:description) (loop rest3 thunk iterations warmup setup teardown val tags)]
                               [(#:tags) (loop rest3 thunk iterations warmup setup teardown description val)]
                               [else (error 'define-benchmark "Unknown option" key)])))))))

;;; quick-bench : (→ α) → BenchmarkResult
(define (quick-bench thunk)
  (run-benchmark (make-benchmark 'quick thunk
                                 'iterations 100
                                 'warmup 10)))

;;; ====
;;; CI/CD Integration Helpers
;;; ====

;;; check-no-regressions : SuiteResult × SuiteResult → Boolean
(define (check-no-regressions baseline-suite current-suite)
  (let ([comparisons (compare-suite-results baseline-suite current-suite)])
       (not (any-regressions? comparisons))))

;;; generate-ci-report : SuiteResult × SuiteResult → Sexp
(define (generate-ci-report baseline-suite current-suite)
  (let* ([comparisons (compare-suite-results baseline-suite current-suite)]
         [has-regressions (any-regressions? comparisons)])
        `(ci-report
          (status . ,(if has-regressions 'failed 'passed))
          (baseline . ,(suite-result-name baseline-suite))
          (current . ,(suite-result-name current-suite))
          (regressions . ,(length (filter comparison-regression? comparisons)))
          (improvements . ,(length (filter comparison-improvement? comparisons)))
          (comparisons . ,(map (lambda (c)
                                       `(,(comparison-get c 'current-name)
                                         (ratio . ,(comparison-elapsed-ratio c))
                                         (delta-pct . ,(comparison-delta-pct c))
                                         (regression? . ,(comparison-regression? c))
                                         (improvement? . ,(comparison-improvement? c))))
                               comparisons)))))

;;; ====
;;; Module Exports (informational)
;;; ====

;;; Primary API:
;;;   make-benchmark, make-suite
;;;   run-benchmark, run-suite
;;;   compare-results, compare-suite-results
;;;   benchmark-report, suite-report, comparison-report
;;;   serialize-results, deserialize-results
;;;   make-baseline
;;;   quick-bench
;;;   check-no-regressions, generate-ci-report
