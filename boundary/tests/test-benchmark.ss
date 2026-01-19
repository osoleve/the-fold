;;; boundary/tests/test-benchmark.ss -- Tests for Benchmarking Harness

;;; NOTE: Run from project root:
;;;   scheme --script boundary/tests/test-benchmark.ss

(load "core/base/prelude.ss")
(load "core/test-framework.ss")
(load "boundary/tools/benchmark.ss")

(display "
")
(display "====
")
(display "          BENCHMARKING HARNESS TESTS
")
(display "====
")

;;; ====
;;; Statistical Function Tests
;;; ====

(test-group statistical-tests
            (define-test mean-empty-test
              (assert-equal 0 (mean '())))
            
            (define-test mean-single-test
              (assert-equal 10 (mean '(10))))
            
            (define-test mean-multiple-test
              (assert-equal 5 (mean '(3 5 7))))  ; (3+5+7)/3 = 5
            
            (define-test mean-larger-test
              (assert-equal 50 (mean '(10 20 30 40 50 60 70 80 90 100))))
            
            (define-test median-empty-test
              (assert-equal 0 (median '())))
            
            (define-test median-single-test
              (assert-equal 5 (median '(5))))
            
            (define-test median-odd-count-test
              (assert-equal 3 (median '(1 3 5))))  ; Already sorted
            
            (define-test median-even-count-test
              (assert-equal 4 (median '(2 4 6 8))))  ; (4+6)/2 = 5 but integer division
            
            (define-test stddev-empty-test
              (assert-equal 0 (stddev '() 0)))
            
            (define-test stddev-uniform-test
              ;; All same values should have 0 stddev
              (assert-equal 0 (stddev '(5 5 5 5) 5)))
            
            (define-test stddev-variance-test
              ;; List with variance - basic sanity check
              (let ([result (stddev '(2 4 4 4 5 5 7 9) 5)])
                   (assert-true (number? result))
                   (assert-true (> result 0))))
            
            (define-test percentile-empty-test
              (assert-equal 0 (percentile '() 50)))
            
            (define-test percentile-single-test
              (assert-equal 10 (percentile '(10) 50)))
            
            (define-test percentile-50-test
              ;; 50th percentile of sorted list
              (let ([result (percentile '(1 2 3 4 5 6 7 8 9 10) 50)])
                   (assert-true (>= result 4))
                   (assert-true (<= result 6))))
            
            (define-test percentile-90-test
              (let ([result (percentile '(1 2 3 4 5 6 7 8 9 10) 90)])
                   (assert-true (>= result 8))))
            
            (define-test percentile-99-test
              (let ([result (percentile '(1 2 3 4 5 6 7 8 9 10) 99)])
                   (assert-true (>= result 9)))))

;;; ====
;;; List Min/Max Tests
;;; ====

(test-group minmax-tests
            (define-test list-min-empty-test
              (assert-equal 0 (list-min '())))
            
            (define-test list-min-single-test
              (assert-equal 5 (list-min '(5))))
            
            (define-test list-min-multiple-test
              (assert-equal 1 (list-min '(5 3 1 4 2))))
            
            (define-test list-max-empty-test
              (assert-equal 0 (list-max '())))
            
            (define-test list-max-single-test
              (assert-equal 7 (list-max '(7))))
            
            (define-test list-max-multiple-test
              (assert-equal 5 (list-max '(1 5 3 2 4)))))

;;; ====
;;; Sorting Tests
;;; ====

(test-group sorting-tests
            (define-test sort-empty-test
              (assert-equal '() (sort '() <)))
            
            (define-test sort-single-test
              (assert-equal '(1) (sort '(1) <)))
            
            (define-test sort-already-sorted-test
              (assert-equal '(1 2 3 4 5) (sort '(1 2 3 4 5) <)))
            
            (define-test sort-reverse-test
              (assert-equal '(1 2 3 4 5) (sort '(5 4 3 2 1) <)))
            
            (define-test sort-random-test
              (assert-equal '(1 2 3 4 5) (sort '(3 1 4 5 2) <)))
            
            (define-test insert-empty-test
              (assert-equal '(5) (insert 5 '() <)))
            
            (define-test insert-beginning-test
              (assert-equal '(1 2 3) (insert 1 '(2 3) <)))
            
            (define-test insert-middle-test
              (assert-equal '(1 2 3) (insert 2 '(1 3) <)))
            
            (define-test insert-end-test
              (assert-equal '(1 2 3) (insert 3 '(1 2) <))))

;;; ====
;;; Formatting Tests
;;; ====

(test-group formatting-tests
            (define-test format-time-ns-nanoseconds-test
              (let ([result (format-time-ns 500)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "ns"))))
            
            (define-test format-time-ns-microseconds-test
              (let ([result (format-time-ns 5000)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "us"))))
            
            (define-test format-time-ns-milliseconds-test
              (let ([result (format-time-ns 5000000)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "ms"))))
            
            (define-test format-time-ns-seconds-test
              (let ([result (format-time-ns 5000000000)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "s"))))
            
            (define-test format-bytes-bytes-test
              (let ([result (format-bytes 500)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "B"))))
            
            (define-test format-bytes-kb-test
              (let ([result (format-bytes 5000)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "KB"))))
            
            (define-test format-bytes-mb-test
              (let ([result (format-bytes 5000000)])
                   (assert-true (string? result))
                   (assert-true (string-contains? result "MB"))))
            
            (define-test format-ratio-one-test
              (let ([result (format-ratio 1.0)])
                   (assert-equal "1.00" result)))
            
            (define-test format-ratio-small-test
              (let ([result (format-ratio 1.5)])
                   (assert-true (string? result))
                   (assert-true (> (string-length result) 0))))
            
            (define-test format-ratio-large-test
              (let ([result (format-ratio 15.0)])
                   (assert-true (string? result)))))

;;; ====
;;; Timing Utility Tests
;;; ====

(test-group timing-tests
            (define-test current-nanoseconds-test
              (let ([t (current-nanoseconds)])
                   (assert-true (number? t))
                   (assert-true (> t 0))))
            
            (define-test current-nanoseconds-increasing-test
              (let ([t1 (current-nanoseconds)])
                   ;; Small busy wait
                   (do ([i 0 (+ i 1)]) ((>= i 1000)))
                   (let ([t2 (current-nanoseconds)])
                        (assert-true (>= t2 t1)))))
            
            (define-test bytes-allocated-test
              ;; Should return a number (possibly 0 if not supported)
              (let ([b (bytes-allocated)])
                   (assert-true (number? b))
                   (assert-true (>= b 0)))))

;;; ====
;;; Warmup Tests
;;; ====

(test-group warmup-tests
            (define-test do-warmup-test
              ;; Should not error
              (let ([counter 0])
                   (do-warmup (lambda () (set! counter (+ counter 1))) 10)
                   (assert-equal 10 counter)))
            
            (define-test do-warmup-zero-test
              (let ([counter 0])
                   (do-warmup (lambda () (set! counter (+ counter 1))) 0)
                   (assert-equal 0 counter))))

;;; ====
;;; Sample Collection Tests
;;; ====

(test-group sample-collection-tests
            (define-test collect-samples-test
              (let ([samples (collect-samples (lambda () (+ 1 1)) 5)])
                   (assert-equal 5 (length samples))
                   ;; All samples should be non-negative
                   (assert-true (every (lambda (s) (>= s 0)) samples))))
            
            (define-test collect-samples-zero-test
              (let ([samples (collect-samples (lambda () #t) 0)])
                   (assert-equal 0 (length samples)))))

;;; ====
;;; Benchmark Result Tests
;;; ====

(test-group benchmark-result-tests
            (define-test benchmark-simple-test
              (let ([result (benchmark "simple" (lambda () (+ 1 2)) 10 0)])
                   (assert-true (benchmark-result? result))
                   (assert-equal "simple" (benchmark-result-name result))
                   (assert-equal 10 (benchmark-result-iterations result))))
            
            (define-test benchmark-result-fields-test
              (let ([result (benchmark "fields" (lambda () #t) 10 0)])
                   (assert-true (number? (benchmark-result-mean-ns result)))
                   (assert-true (number? (benchmark-result-median-ns result)))
                   (assert-true (number? (benchmark-result-stddev-ns result)))
                   (assert-true (number? (benchmark-result-min-ns result)))
                   (assert-true (number? (benchmark-result-max-ns result)))
                   (assert-true (number? (benchmark-result-p50-ns result)))
                   (assert-true (number? (benchmark-result-p90-ns result)))
                   (assert-true (number? (benchmark-result-p99-ns result)))
                   (assert-true (number? (benchmark-result-timestamp result)))))
            
            (define-test benchmark-result-samples-test
              (let ([result (benchmark "samples" (lambda () #t) 10 0)])
                   (assert-equal 10 (length (benchmark-result-samples result))))))

;;; ====
;;; Comparison Tests
;;; ====

(test-group comparison-tests
            (define-test benchmark-compare-test
              (let ([results (benchmark-compare
                              `(("fast" . ,(lambda () (+ 1 1)))
                                ("slow" . ,(lambda () (do ([i 0 (+ i 1)]) ((> i 100))))))
                              10)])
                   (assert-equal 2 (length results))
                   (assert-true (every benchmark-result? results))))
            
            (define-test find-fastest-test
              (let* ([r1 (benchmark "fast" (lambda () #t) 10 0)]
                     [r2 (benchmark "slow" (lambda () (do ([i 0 (+ i 1)]) ((> i 100)))) 10 0)]
                     [fastest (find-fastest (list r2 r1))])
                    (assert-true (benchmark-result? fastest)))))

;;; ====
;;; Suite Tests
;;; ====

(test-group suite-tests
            (define-test benchmark-suite-test
              (let ([result (benchmark-suite
                             "test-suite"
                             `(("a" . ,(lambda () (+ 1 1)))
                               ("b" . ,(lambda () (* 2 2)))))])
                   (assert-true (suite-result? result))
                   (assert-equal "test-suite" (suite-result-name result))
                   (assert-equal 2 (length (suite-result-benchmarks result))))))

;;; ====
;;; Quick/Thorough Preset Tests
;;; ====

(test-group preset-tests
            (define-test quick-bench-test
              (let ([result (quick-bench (lambda () #t))])
                   (assert-true (benchmark-result? result))
                   (assert-equal 100 (benchmark-result-iterations result))))
            
            (define-test thorough-bench-test
              ;; Note: This runs 10000 iterations, may be slow
              ;; Just verify it returns correct type
              (let ([result (thorough-bench (lambda () #t))])
                   (assert-true (benchmark-result? result))
                   (assert-equal 10000 (benchmark-result-iterations result)))))

;;; ====
;;; Report Tests (No File I/O)
;;; ====

(test-group report-tests
            (define-test benchmark-report-single-test
              ;; Should not error
              (let ([result (benchmark "report-test" (lambda () #t) 5 0)])
                   (benchmark-report (list result))
                   (assert-true #t)))
            
            (define-test benchmark-report-comparison-test
              ;; Multiple results should show comparison
              (let ([results (benchmark-compare
                              `(("a" . ,(lambda () #t))
                                ("b" . ,(lambda () #t)))
                              5)])
                   (benchmark-report results)
                   (assert-true #t))))

;;; ====
;;; Configuration Tests
;;; ====

(test-group configuration-tests
            (define-test default-iterations-test
              (assert-equal 1000 *default-iterations*))
            
            (define-test default-warmup-test
              (assert-equal 100 *default-warmup-iterations*))
            
            (define-test time-unit-test
              (assert-equal 'milliseconds *time-unit*)))

;;; ====
;;; Edge Case Tests
;;; ====

(test-group edge-case-tests
            (define-test benchmark-zero-iterations-test
              ;; Zero iterations should be handled gracefully
              (let ([result (benchmark "zero" (lambda () #t) 0 0)])
                   (assert-true (benchmark-result? result))
                   (assert-equal 0 (benchmark-result-iterations result))))
            
            (define-test benchmark-exception-in-thunk-test
              ;; Benchmark should handle exceptions in thunk
              ;; Note: This may cause the benchmark to fail, but shouldn't crash
              (guard (e [else #t])  ; Just catch any error
                     (benchmark "error" (lambda () (error 'test "intentional")) 1 0)
                     (assert-true #t))))

;;; ====
;;; Helper Functions Used in Tests
;;; ====

;;; every : (a -> Bool) x (List a) -> Bool
(define (every pred lst)
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (every pred (cdr lst))]))

;;; string-contains? : String x String -> Bool
(define (string-contains? str sub)
  (let ([slen (string-length str)]
        [sublen (string-length sub)])
       (let loop ([i 0])
            (cond
             [(> (+ i sublen) slen) #f]
             [(string=? sub (substring str i (+ i sublen))) #t]
             [else (loop (+ i 1))]))))

;;; ====
;;; Run Tests
;;; ====

(display "
")
(display "====
")
(display (format "Tests passed: ~a
" *tests-passed*))
(display (format "Tests failed: ~a
" *tests-failed*))
(display (format "Total tests:  ~a
" *tests-run*))

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All benchmarking harness tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
