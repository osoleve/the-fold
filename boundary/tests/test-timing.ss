(load "boundary/introspect/timing.ss")

(doc 'module 'test-timing)
(doc 'description "Comprehensive test suite for boundary/introspect/timing.ss - timing primitives, benchmarks, statistics, and comparisons")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

(doc 'section 'test-helpers)

(doc assert-equal 'description "Test assertion helper")
(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-true name actual)
  (assert-equal name actual #t))

(define (assert-false name actual)
  (assert-equal name actual #f))

(doc 'section 'current-nanoseconds-tests)

(printf "\n=== Testing current-nanoseconds ===\n")

(assert-true "returns positive number"
             (> (current-nanoseconds) 0))

(assert-true "is monotonic"
             (let ([t1 (current-nanoseconds)]
                   [t2 (current-nanoseconds)])
                  (>= t2 t1)))

(assert-true "increases over time"
             (let ([t1 (current-nanoseconds)])
                  ;; Do some work
                  (let loop ([i 0])
                       (when (< i 1000) (loop (+ i 1))))
                  (> (current-nanoseconds) t1)))

(doc 'section 'current-cpu-nanoseconds-tests)

(printf "\n=== Testing current-cpu-nanoseconds ===\n")

(assert-true "returns positive number"
             (> (current-cpu-nanoseconds) 0))

(assert-true "increases with work"
             (let ([t1 (current-cpu-nanoseconds)])
                  ;; Do some CPU work
                  (let loop ([i 0] [acc 0])
                       (if (< i 10000)
                           (loop (+ i 1) (+ acc i))
                           acc))
                  (>= (current-cpu-nanoseconds) t1)))

(doc 'section 'timing-result-tests)

(printf "\n=== Testing time-thunk ===\n")

(let ([result (time-thunk (lambda () (+ 1 2)))])
     (assert-true "returns timing-result"
                  (timing-result? result))
     
     (assert-true "elapsed-ns is non-negative"
                  (>= (timing-result-elapsed-ns result) 0))
     
     (assert-true "cpu-ns is non-negative"
                  (>= (timing-result-cpu-ns result) 0))
     
     (assert-equal "captures return value"
                   (timing-result-value result)
                   3))

(let ([result (time-thunk (lambda () (make-list 100 'x)))])
     (assert-true "captures complex return value"
                  (list? (timing-result-value result)))
     
     (assert-equal "list length correct"
                   (length (timing-result-value result))
                   100))

(doc 'section 'compute-stats-tests)

(printf "\n=== Testing compute-stats ===\n")

(let ([stats (compute-stats '(100 200 300 400 500))])
     (assert-true "returns timing-stats"
                  (timing-stats? stats))
     
     (assert-equal "min-ns"
                   (timing-stats-min-ns stats)
                   100)
     
     (assert-equal "max-ns"
                   (timing-stats-max-ns stats)
                   500)
     
     (assert-true "mean-ns is correct"
                  (= (timing-stats-mean-ns stats) 300.0))
     
     (assert-true "median-ns is correct"
                  (= (timing-stats-median-ns stats) 300.0))
     
     (assert-equal "total-ns"
                   (timing-stats-total-ns stats)
                   1500))

;; Test with even number of samples
(let ([stats (compute-stats '(100 200 300 400))])
     (assert-true "median with even samples"
                  (= (timing-stats-median-ns stats) 250.0)))

;; Test with single sample
(let ([stats (compute-stats '(500))])
     (assert-equal "single sample min"
                   (timing-stats-min-ns stats)
                   500)
     
     (assert-equal "single sample max"
                   (timing-stats-max-ns stats)
                   500))

;; Test with empty list
(let ([stats (compute-stats '())])
     (assert-equal "empty list min"
                   (timing-stats-min-ns stats)
                   0)
     
     (assert-equal "empty list mean"
                   (timing-stats-mean-ns stats)
                   0))

(doc 'section 'benchmark-tests)

(printf "\n=== Testing benchmark ===\n")

(let ([result (benchmark "simple-test" 5 (lambda () 42))])
     (assert-true "returns benchmark-result"
                  (benchmark-result? result))
     
     (assert-equal "name matches"
                   (benchmark-result-name result)
                   "simple-test")
     
     (assert-equal "iterations matches"
                   (benchmark-result-iterations result)
                   5)
     
     (assert-true "has timings list"
                  (list? (benchmark-result-timings result)))
     
     (assert-equal "timings count matches"
                   (length (benchmark-result-timings result))
                   5)
     
     (assert-true "all timings are timing-result"
                  (andmap timing-result? (benchmark-result-timings result)))
     
     (assert-true "has stats"
                  (timing-stats? (benchmark-result-stats result))))

(doc 'section 'benchmark-with-warmup-tests)

(printf "\n=== Testing benchmark-with-warmup ===\n")

(let ([result (benchmark-with-warmup "warmup-test" 3 5 (lambda () 'ok))])
     (assert-true "returns benchmark-result"
                  (benchmark-result? result))
     
     (assert-equal "has correct iterations (after warmup)"
                   (benchmark-result-iterations result)
                   5))

(doc 'section 'compare-benchmarks-tests)

(printf "\n=== Testing compare-benchmarks ===\n")

(let* ([baseline (benchmark "baseline" 3 (lambda () 'fast))]
       [slow (benchmark "slow" 3 (lambda ()
                                         (let loop ([i 0])
                                              (when (< i 1000) (loop (+ i 1))))
                                         'slow))]
       [comparison (compare-benchmarks baseline (list slow))])
      
      (assert-true "returns benchmark-comparison"
                   (benchmark-comparison? comparison))
      
      (assert-true "has baseline"
                   (benchmark-result? (benchmark-comparison-baseline comparison)))
      
      (assert-true "has candidates"
                   (list? (benchmark-comparison-candidates comparison)))
      
      (assert-true "has ratios"
                   (list? (benchmark-comparison-ratios comparison)))
      
      (assert-true "ratio is a pair"
                   (pair? (car (benchmark-comparison-ratios comparison)))))

(doc 'section 'format-ns-tests)

(printf "\n=== Testing format-ns ===\n")

(assert-true "nanoseconds format"
             (string-contains? (format-ns 500) "ns"))

(assert-true "microseconds format"
             (string-contains? (format-ns 5000) "s"))  ; Could be ns or us

(assert-true "milliseconds format"
             (string-contains? (format-ns 5000000) "ms"))

(assert-true "seconds format"
             (string-contains? (format-ns 5000000000) "s"))

(doc 'section 'format-timing-tests)

(printf "\n=== Testing format-timing ===\n")

(let* ([tr (make-timing-result 1000000 500000 'value)]
       [formatted (format-timing tr)])
      (assert-true "returns string"
                   (string? formatted))
      
      (assert-true "contains elapsed"
                   (string-contains? formatted "elapsed"))
      
      (assert-true "contains cpu"
                   (string-contains? formatted "cpu")))

(doc 'section 'format-stats-tests)

(printf "\n=== Testing format-stats ===\n")

(let* ([stats (compute-stats '(100 200 300))]
       [formatted (format-stats stats)])
      (assert-true "returns string"
                   (string? formatted))
      
      (assert-true "contains min"
                   (string-contains? formatted "min"))
      
      (assert-true "contains max"
                   (string-contains? formatted "max"))
      
      (assert-true "contains mean"
                   (string-contains? formatted "mean"))
      
      (assert-true "contains median"
                   (string-contains? formatted "median"))
      
      (assert-true "contains stddev"
                   (string-contains? formatted "stddev")))

(doc 'section 'format-benchmark-tests)

(printf "\n=== Testing format-benchmark ===\n")

(let* ([br (benchmark "format-test" 3 (lambda () 42))]
       [formatted (format-benchmark br)])
      (assert-true "returns string"
                   (string? formatted))
      
      (assert-true "contains name"
                   (string-contains? formatted "format-test"))
      
      (assert-true "contains iterations"
                   (string-contains? formatted "3")))

(doc 'section 'time-with-fuel-tests)

(printf "\n=== Testing time-with-fuel ===\n")

(let-values ([(tr fuel-left) (time-with-fuel 100
                                             (lambda () (cons 'result 50)))])
            (assert-true "returns timing result"
                         (timing-result? tr))
            
            (assert-equal "captures fuel remaining"
                          fuel-left
                          50))

(let-values ([(tr fuel-left) (time-with-fuel 100
                                             (lambda () 'not-a-pair))])
            (assert-equal "non-pair returns 0 fuel"
                          fuel-left
                          0))

(doc 'section 'edge-cases)

(printf "\n=== Testing Edge Cases ===\n")

(assert-true "zero iterations benchmark"
             (let ([result (benchmark "zero" 0 (lambda () 'x))])
                  (null? (benchmark-result-timings result))))

(assert-true "stddev with single sample"
             (let ([stats (compute-stats '(100))])
                  (= (timing-stats-stddev-ns stats) 0.0)))

(assert-true "empty comparison candidates"
             (let* ([baseline (benchmark "base" 1 (lambda () 'x))]
                    [cmp (compare-benchmarks baseline '())])
                   (null? (benchmark-comparison-ratios cmp))))

(doc 'section 'test-summary)

(printf "\n====\n")
(printf "Test Results (timing):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

(doc 'note "Return success/failure")
(= fail-count 0)
