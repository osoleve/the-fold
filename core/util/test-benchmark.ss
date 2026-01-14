;;; core/util/test-benchmark.ss — Tests for Benchmark Harness
;;;
;;; Tests benchmark functionality:
;;;   - Benchmark creation and configuration
;;;   - Suite creation and execution
;;;   - Statistical calculations
;;;   - Comparison and regression detection
;;;   - Serialization/deserialization
;;;   - Reporting functions
;;;
;;; Run from project root:
;;;   scheme --script core/util/test-benchmark.ss

(load "core/util/benchmark.ss")

(display "Benchmark Harness Tests\n")
(display "====\n\n")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  + ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  X ") (display name)
       (display " - expected ") (write expected)
       (display ", got ") (write actual)
       (newline))))

(define (test-approx name expected actual epsilon)
  (if (< (abs (- expected actual)) epsilon)
      (begin
       (set! tests-passed (+ tests-passed 1))
       (display "  + ") (display name) (newline))
      (begin
       (set! tests-failed (+ tests-failed 1))
       (display "  X ") (display name)
       (display " - expected ~") (write expected)
       (display ", got ") (write actual)
       (newline))))

;;; ====
;;; Statistical Function Tests
;;; ====

(display "Statistical Functions:\n")

;; sum
(test "sum empty list" 0 (sum '()))
(test "sum single element" 5 (sum '(5)))
(test "sum multiple elements" 15 (sum '(1 2 3 4 5)))

;; mean
(test "mean empty list" 0 (mean '()))
(test "mean single element" 5 (mean '(5)))
(test "mean multiple elements" 3 (mean '(1 2 3 4 5)))

;; variance
(test "variance empty list" 0 (variance '() 0))
(test "variance single element" 0 (variance '(5) 5))
(test-approx "variance uniform list" 0 (variance '(5 5 5 5) 5) 0.001)
(test-approx "variance varied list" 2.5 (variance '(1 2 3 4 5) 3) 0.001)

;; stddev
(test-approx "stddev empty list" 0 (stddev '() 0) 0.001)
(test-approx "stddev uniform list" 0 (stddev '(5 5 5 5) 5) 0.001)
(test-approx "stddev varied list" 1.581 (stddev '(1 2 3 4 5) 3) 0.01)

;; percentile
(test "percentile empty list" 0 (percentile '() 50))
(test "percentile p50" 3 (percentile '(1 2 3 4 5) 50))
(test "percentile p0" 1 (percentile '(1 2 3 4 5) 0))
(test "percentile p100" 5 (percentile '(1 2 3 4 5) 100))

;; median
(test "median empty list" 0 (median '()))
(test "median odd length" 3 (median '(1 2 3 4 5)))
(test "median even length" 5/2 (median '(1 2 3 4)))

;; iqr
(test "iqr empty list" 0 (iqr '()))
(test "iqr computed" 2 (iqr '(1 2 3 4 5)))

;; list-min/list-max
(test "list-min" 1 (list-min '(3 1 4 1 5)))
(test "list-max" 5 (list-max '(3 1 4 1 5)))

;;; ====
;;; Benchmark Creation Tests
;;; ====

(display "\nBenchmark Creation:\n")

(let ([b (make-benchmark 'test-bench (lambda () (+ 1 1)))])
     (test "benchmark?" #t (benchmark? b))
     (test "benchmark-name" 'test-bench (benchmark-name b))
     (test "benchmark-iterations default" *default-iterations* (benchmark-iterations b))
     (test "benchmark-warmup default" *default-warmup* (benchmark-warmup b))
     (test "benchmark-setup default" #f (benchmark-setup b))
     (test "benchmark-teardown default" #f (benchmark-teardown b))
     (test "benchmark-tags default" '() (benchmark-tags b)))

(let ([b (make-benchmark 'custom-bench (lambda () 42)
                         'iterations 500
                         'warmup 50
                         'description "A custom benchmark"
                         'tags '(math fast))])
     (test "custom iterations" 500 (benchmark-iterations b))
     (test "custom warmup" 50 (benchmark-warmup b))
     (test "custom description" "A custom benchmark" (benchmark-description b))
     (test "custom tags" '(math fast) (benchmark-tags b)))

(let* ([setup-called 0]
       [teardown-called 0]
       [b (make-benchmark 'with-hooks (lambda () 1)
                          'setup (lambda () (set! setup-called (+ setup-called 1)))
                          'teardown (lambda () (set! teardown-called (+ teardown-called 1))))])
      (test "setup is function" #t (procedure? (benchmark-setup b)))
      (test "teardown is function" #t (procedure? (benchmark-teardown b))))

;;; ====
;;; Suite Creation Tests
;;; ====

(display "\nSuite Creation:\n")

(let* ([b1 (make-benchmark 'bench1 (lambda () 1))]
       [b2 (make-benchmark 'bench2 (lambda () 2))]
       [s (make-suite 'test-suite (list b1 b2)
                      'description "Test suite"
                      'tags '(unit fast))])
      (test "suite?" #t (suite? s))
      (test "suite-name" 'test-suite (suite-name s))
      (test "suite-benchmarks count" 2 (length (suite-benchmarks s)))
      (test "suite-description" "Test suite" (suite-description s))
      (test "suite-tags" '(unit fast) (suite-tags s)))

;;; ====
;;; Sample Creation Tests
;;; ====

(display "\nSample Creation:\n")

(let ([s (make-sample 1000000 800000 1024)])
     (test "sample?" #t (sample? s))
     (test "sample-elapsed-ns" 1000000 (sample-elapsed-ns s))
     (test "sample-cpu-ns" 800000 (sample-cpu-ns s))
     (test "sample-memory-delta" 1024 (sample-memory-delta s)))

;;; ====
;;; Timing Tests
;;; ====

(display "\nTiming:\n")

(test "current-nanoseconds positive" #t (> (current-nanoseconds) 0))
(test "current-cpu-nanoseconds positive" #t (>= (current-cpu-nanoseconds) 0))

(let-values ([(_ sample) (time-thunk (lambda () (+ 1 1)))])
            (test "time-thunk returns sample" #t (sample? sample))
            (test "elapsed-ns >= 0" #t (>= (sample-elapsed-ns sample) 0)))

;;; ====
;;; Benchmark Execution Tests
;;; ====

(display "\nBenchmark Execution:\n")

;; Simple benchmark execution
(let* ([counter 0]
       [b (make-benchmark 'count-bench
                          (lambda () (set! counter (+ counter 1)))
                          'iterations 10
                          'warmup 5)]
       [_ (set! counter 0)]
       [result (run-benchmark b)])
      (test "result is benchmark-result" #t (benchmark-result? result))
      (test "result-name" 'count-bench (result-name result))
      (test "result-iterations" 10 (result-iterations result))
      (test "result-warmup" 5 (result-warmup result))
      ;; Counter should be 10 (iterations) + 5 (warmup) = 15
      (test "iterations + warmup executed" 15 counter)
      (test "has elapsed-ns stats" #t (number? (result-stat result 'elapsed-ns 'mean)))
      (test "has cpu-ns stats" #t (number? (result-stat result 'cpu-ns 'mean)))
      (test "has memory stats" #t (number? (result-stat result 'memory 'mean))))

;; Benchmark with setup/teardown
(let* ([setup-count 0]
       [teardown-count 0]
       [b (make-benchmark 'hooks-bench
                          (lambda () 1)
                          'iterations 5
                          'warmup 3
                          'setup (lambda () (set! setup-count (+ setup-count 1)))
                          'teardown (lambda () (set! teardown-count (+ teardown-count 1))))]
       [_ (begin (set! setup-count 0) (set! teardown-count 0))]
       [result (run-benchmark b)])
      ;; Setup and teardown called for each iteration including warmup
      (test "setup called correctly" 8 setup-count)      ; 5 + 3
      (test "teardown called correctly" 8 teardown-count)) ; 5 + 3

;;; ====
;;; Suite Execution Tests
;;; ====

(display "\nSuite Execution:\n")

(let* ([b1 (make-benchmark 'fast (lambda () 1) 'iterations 5 'warmup 2)]
       [b2 (make-benchmark 'slow (lambda () (let loop ([i 0]) (if (< i 100) (loop (+ i 1)) i))) 'iterations 5 'warmup 2)]
       [s (make-suite 'perf-suite (list b1 b2))]
       [result (run-suite s)])
      (test "suite-result?" #t (suite-result? result))
      (test "suite-result-name" 'perf-suite (suite-result-name result))
      (test "suite-result has 2 results" 2 (length (suite-result-results result)))
      (test "first result name" 'fast (result-name (car (suite-result-results result)))))

;;; ====
;;; Comparison Tests
;;; ====

(display "\nComparison & Regression Detection:\n")

;; Create mock results for testing
(define (make-mock-result name mean-ns)
  `(benchmark-result
    (name . ,name)
    (iterations . 100)
    (warmup . 10)
    (samples . ())
    (stats .
           ((elapsed-ns .
                        ((mean . ,mean-ns)
                         (stddev . ,(/ mean-ns 10))
                         (min . ,(* mean-ns 0.8))
                         (max . ,(* mean-ns 1.2))
                         (median . ,mean-ns)
                         (iqr . ,(/ mean-ns 5))
                         (percentiles . ((50 . ,mean-ns) (90 . ,(* mean-ns 1.1)) (95 . ,(* mean-ns 1.15)) (99 . ,(* mean-ns 1.2))))))
            (cpu-ns .
                    ((mean . ,(* mean-ns 0.9))
                     (stddev . ,(/ mean-ns 10))
                     (min . ,(* mean-ns 0.7))
                     (max . ,(* mean-ns 1.1))
                     (median . ,(* mean-ns 0.9))))
            (memory .
                    ((mean . 1024)
                     (total . 102400)
                     (min . 512)
                     (max . 2048)))))
    (timestamp . 0)))

;; Test comparison - no change
(let* ([baseline (make-mock-result 'test 1000000)]
       [current (make-mock-result 'test 1000000)]
       [comparison (compare-results baseline current)])
      (test "comparison?" #t (comparison? comparison))
      (test-approx "elapsed-ratio no change" 1.0 (comparison-elapsed-ratio comparison) 0.001)
      (test "no regression when same" #f (comparison-regression? comparison))
      (test "no improvement when same" #f (comparison-improvement? comparison)))

;; Test comparison - regression (20% slower)
(let* ([baseline (make-mock-result 'test 1000000)]
       [current (make-mock-result 'test 1200000)]
       [comparison (compare-results baseline current)])
      (test-approx "elapsed-ratio 20% slower" 1.2 (comparison-elapsed-ratio comparison) 0.001)
      (test "regression detected" #t (comparison-regression? comparison))
      (test "not improvement" #f (comparison-improvement? comparison))
      (test-approx "delta-pct +20%" 20.0 (comparison-delta-pct comparison) 0.1))

;; Test comparison - improvement (20% faster)
(let* ([baseline (make-mock-result 'test 1000000)]
       [current (make-mock-result 'test 800000)]
       [comparison (compare-results baseline current)])
      (test-approx "elapsed-ratio 20% faster" 0.8 (comparison-elapsed-ratio comparison) 0.001)
      (test "not regression" #f (comparison-regression? comparison))
      (test "improvement detected" #t (comparison-improvement? comparison))
      (test-approx "delta-pct -20%" -20.0 (comparison-delta-pct comparison) 0.1))

;; Test comparison - within threshold (5% change)
(let* ([baseline (make-mock-result 'test 1000000)]
       [current (make-mock-result 'test 1050000)]
       [comparison (compare-results baseline current)])
      (test "5% change not regression" #f (comparison-regression? comparison))
      (test "5% change not improvement" #f (comparison-improvement? comparison)))

;;; ====
;;; Suite Comparison Tests
;;; ====

(display "\nSuite Comparison:\n")

(let* ([baseline-results (list (make-mock-result 'bench1 1000000)
                               (make-mock-result 'bench2 2000000)
                               (make-mock-result 'bench3 500000))]
       [current-results (list (make-mock-result 'bench1 1200000)   ; 20% regression
                              (make-mock-result 'bench2 1600000)   ; 20% improvement
                              (make-mock-result 'bench3 500000))]  ; no change
       [baseline-suite `(suite-result (name . baseline) (results . ,baseline-results) (timestamp . 0))]
       [current-suite `(suite-result (name . current) (results . ,current-results) (timestamp . 0))]
       [comparisons (compare-suite-results baseline-suite current-suite)])
      (test "3 comparisons returned" 3 (length comparisons))
      (test "1 regression" 1 (length (filter comparison-regression? comparisons)))
      (test "1 improvement" 1 (length (filter comparison-improvement? comparisons)))
      (test "any-regressions? true" #t (any-regressions? comparisons)))

;;; ====
;;; Baseline Tests
;;; ====

(display "\nBaseline Storage:\n")

(let* ([mock-results (list (make-mock-result 'test 1000000))]
       [mock-suite `(suite-result (name . test-suite) (results . ,mock-results) (timestamp . 12345))]
       [baseline (make-baseline 'v1.0 mock-suite)])
      (test "baseline?" #t (baseline? baseline))
      (test "baseline-name" 'v1.0 (baseline-name baseline))
      (test "baseline-created is number" #t (number? (baseline-created baseline)))
      (test "baseline-results" mock-suite (baseline-results baseline)))

;;; ====
;;; Serialization Tests
;;; ====

(display "\nSerialization:\n")

(let* ([mock-result (make-mock-result 'serialize-test 1000000)]
       [serialized (serialize-results mock-result)])
      (test "serialized is still benchmark-result" #t (benchmark-result? serialized))
      ;; Samples should be stripped
      (test "samples stripped" #f (assq 'samples (cdr serialized))))

(let* ([mock-results (list (make-mock-result 'test1 1000000)
                           (make-mock-result 'test2 2000000))]
       [mock-suite `(suite-result (name . test-suite) (results . ,mock-results) (timestamp . 12345))]
       [serialized (serialize-results mock-suite)])
      (test "serialized suite-result" #t (suite-result? serialized))
      (test "serialized suite name preserved" 'test-suite (suite-result-name serialized)))

(let ([original `(benchmark-result (name . test) (iterations . 100) (stats . ()))])
     (test "deserialize is identity" original (deserialize-results original)))

;;; ====
;;; Formatting Tests
;;; ====

(display "\nFormatting:\n")

;; format-ns
(test "format-ns nanoseconds" "500ns" (format-ns 500))
(test "format-ns microseconds" "1.50us" (format-ns 1500))
(test "format-ns milliseconds" "1.50ms" (format-ns 1500000))
(test "format-ns seconds" "1.50s" (format-ns 1500000000))

;; format-bytes
(test "format-bytes bytes" "512B" (format-bytes 512))
(test "format-bytes kilobytes" "1.50KB" (format-bytes 1536))
(test "format-bytes megabytes" "1.50MB" (format-bytes (* 1536 1024)))
(test "format-bytes gigabytes" "1.50GB" (format-bytes (* 1536 1024 1024)))
(test "format-bytes negative" "-1.00KB" (format-bytes -1024))

;; format-ratio
(test "format-ratio 1x" "1.00x" (format-ratio 1.0))
(test "format-ratio 2x" "2.00x" (format-ratio 2.0))
(test "format-ratio 0.5x" "0.50x" (format-ratio 0.5))

;; format-pct
(test "format-pct positive" "+20.0%" (format-pct 20.0))
(test "format-pct negative" "-15.0%" (format-pct -15.0))
(test "format-pct zero" "+0.0%" (format-pct 0.0))

;;; ====
;;; CI Integration Tests
;;; ====

(display "\nCI Integration:\n")

(let* ([baseline-results (list (make-mock-result 'bench1 1000000))]
       [current-results (list (make-mock-result 'bench1 1000000))]
       [baseline-suite `(suite-result (name . baseline) (results . ,baseline-results) (timestamp . 0))]
       [current-suite `(suite-result (name . current) (results . ,current-results) (timestamp . 0))])
      (test "check-no-regressions passes" #t (check-no-regressions baseline-suite current-suite)))

(let* ([baseline-results (list (make-mock-result 'bench1 1000000))]
       [current-results (list (make-mock-result 'bench1 1500000))]  ; 50% regression
       [baseline-suite `(suite-result (name . baseline) (results . ,baseline-results) (timestamp . 0))]
       [current-suite `(suite-result (name . current) (results . ,current-results) (timestamp . 0))])
      (test "check-no-regressions fails" #f (check-no-regressions baseline-suite current-suite)))

(let* ([baseline-results (list (make-mock-result 'bench1 1000000)
                               (make-mock-result 'bench2 2000000))]
       [current-results (list (make-mock-result 'bench1 1100000)   ; 10% - neutral
                              (make-mock-result 'bench2 1500000))] ; 25% improvement
       [baseline-suite `(suite-result (name . baseline) (results . ,baseline-results) (timestamp . 0))]
       [current-suite `(suite-result (name . current) (results . ,current-results) (timestamp . 0))]
       [report (generate-ci-report baseline-suite current-suite)])
      (test "ci-report status passed" 'passed (cdr (assq 'status (cdr report))))
      (test "ci-report 0 regressions" 0 (cdr (assq 'regressions (cdr report))))
      (test "ci-report 1 improvement" 1 (cdr (assq 'improvements (cdr report)))))

;;; ====
;;; quick-bench Test
;;; ====

(display "\nQuick Bench:\n")

(let ([result (quick-bench (lambda () (+ 1 1)))])
     (test "quick-bench returns result" #t (benchmark-result? result))
     (test "quick-bench name is 'quick" 'quick (result-name result)))

;;; ====
;;; Summary
;;; ====

(newline)
(display "====\n")
(display (format "Tests: ~a passed, ~a failed\n" tests-passed tests-failed))
(when (> tests-failed 0)
      (exit 1))
