;;; shell/tests/test-memory.ss — Tests for Memory Measurement
;;;
;;; Comprehensive test suite for shell/introspect/memory.ss
;;; Tests memory snapshots, deltas, benchmarks, and allocation tracking.

(load "shell/introspect/memory.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
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

;;; ====
;;; memory-snapshot Tests
;;; ====

(printf "\n=== Testing current-memory-snapshot ===\n")

(let ([snap (current-memory-snapshot)])
     (assert-true "returns memory-snapshot"
                  (memory-snapshot? snap))
     
     (assert-true "bytes-allocated is positive"
                  (> (memory-snapshot-bytes-allocated snap) 0))
     
     (assert-true "bytes-in-use is non-negative"
                  (>= (memory-snapshot-bytes-in-use snap) 0))
     
     (assert-true "gc-count is non-negative"
                  (>= (memory-snapshot-gc-count snap) 0))
     
     (assert-true "timestamp is positive"
                  (> (memory-snapshot-timestamp-ns snap) 0)))

;;; ====
;;; compute-memory-delta Tests
;;; ====

(printf "\n=== Testing compute-memory-delta ===\n")

(let* ([before (current-memory-snapshot)]
       [_ (make-list 1000 'x)]  ; Allocate something
       [after (current-memory-snapshot)]
       [delta (compute-memory-delta before after)])
      
      (assert-true "returns memory-delta"
                   (memory-delta? delta))
      
      (assert-true "bytes-allocated is non-negative"
                   (>= (memory-delta-bytes-allocated delta) 0))
      
      (assert-true "gc-triggered is non-negative"
                   (>= (memory-delta-gc-triggered delta) 0)))

;;; ====
;;; memory-thunk Tests
;;; ====

(printf "\n=== Testing memory-thunk ===\n")

(let ([result (memory-thunk (lambda () (make-list 100 42)))])
     (assert-true "returns memory-result"
                  (memory-result? result))
     
     (assert-true "has before snapshot"
                  (memory-snapshot? (memory-result-before result)))
     
     (assert-true "has after snapshot"
                  (memory-snapshot? (memory-result-after result)))
     
     (assert-true "has delta"
                  (memory-delta? (memory-result-delta result)))
     
     (assert-true "captures return value"
                  (list? (memory-result-value result)))
     
     (assert-equal "return value length"
                   (length (memory-result-value result))
                   100))

;;; ====
;;; gc-and-measure Tests
;;; ====

(printf "\n=== Testing gc-and-measure ===\n")

(let ([snap (gc-and-measure)])
     (assert-true "returns snapshot after gc"
                  (memory-snapshot? snap))
     
     (assert-true "snapshot is recent"
                  (> (memory-snapshot-timestamp-ns snap) 0)))

;;; ====
;;; memory-benchmark Tests
;;; ====

(printf "\n=== Testing memory-benchmark ===\n")

(let ([result (memory-benchmark "test-bench" 5 (lambda () (make-list 10 'x)))])
     (assert-true "returns benchmark result"
                  (memory-benchmark-result? result))
     
     (assert-equal "name matches"
                   (memory-benchmark-result-name result)
                   "test-bench")
     
     (assert-equal "iterations matches"
                   (memory-benchmark-result-iterations result)
                   5)
     
     (assert-true "has results list"
                  (list? (memory-benchmark-result-results result)))
     
     (assert-equal "results count matches iterations"
                   (length (memory-benchmark-result-results result))
                   5)
     
     (assert-true "has stats"
                  (memory-stats? (memory-benchmark-result-stats result))))

;;; ====
;;; memory-benchmark-with-gc Tests
;;; ====

(printf "\n=== Testing memory-benchmark-with-gc ===\n")

(let ([result (memory-benchmark-with-gc "gc-bench" 3 (lambda () 'ok))])
     (assert-true "returns benchmark result"
                  (memory-benchmark-result? result))
     
     (assert-equal "iterations correct"
                   (memory-benchmark-result-iterations result)
                   3))

;;; ====
;;; compute-memory-stats Tests
;;; ====

(printf "\n=== Testing compute-memory-stats ===\n")

(let* ([deltas (list (make-memory-delta 100 10 0)
                     (make-memory-delta 200 20 1)
                     (make-memory-delta 150 15 0))]
       [stats (compute-memory-stats deltas)])
      
      (assert-true "returns memory-stats"
                   (memory-stats? stats))
      
      (assert-equal "min-allocated"
                    (memory-stats-min-allocated stats)
                    100)
      
      (assert-equal "max-allocated"
                    (memory-stats-max-allocated stats)
                    200)
      
      (assert-equal "total-allocated"
                    (memory-stats-total-allocated stats)
                    450)
      
      (assert-equal "gc-count"
                    (memory-stats-gc-count stats)
                    1)
      
      (assert-true "mean is reasonable"
                   (= (memory-stats-mean-allocated stats) 150)))

;;; ====
;;; format-bytes Tests
;;; ====

(printf "\n=== Testing format-bytes ===\n")

(assert-equal "bytes"
              (format-bytes 512)
              "512B")

(assert-true "kilobytes"
             (string-contains? (format-bytes 2048) "KB"))

(assert-true "megabytes"
             (string-contains? (format-bytes (* 2 1024 1024)) "MB"))

(assert-true "gigabytes"
             (string-contains? (format-bytes (* 2 1024 1024 1024)) "GB"))

(assert-true "negative bytes handled"
             (string? (format-bytes -1024)))

;;; ====
;;; format-memory-snapshot Tests
;;; ====

(printf "\n=== Testing format-memory-snapshot ===\n")

(let* ([snap (current-memory-snapshot)]
       [formatted (format-memory-snapshot snap)])
      (assert-true "returns string"
                   (string? formatted))
      
      (assert-true "contains allocated"
                   (string-contains? formatted "allocated"))
      
      (assert-true "contains in-use"
                   (string-contains? formatted "in-use"))
      
      (assert-true "contains gc-count"
                   (string-contains? formatted "gc-count")))

;;; ====
;;; format-memory-delta Tests
;;; ====

(printf "\n=== Testing format-memory-delta ===\n")

(let* ([delta (make-memory-delta 1024 512 1)]
       [formatted (format-memory-delta delta)])
      (assert-true "returns string"
                   (string? formatted))
      
      (assert-true "contains allocated"
                   (string-contains? formatted "allocated"))
      
      (assert-true "contains freed"
                   (string-contains? formatted "freed")))

;;; ====
;;; format-memory-stats Tests
;;; ====

(printf "\n=== Testing format-memory-stats ===\n")

(let* ([stats (make-memory-stats 100 200 150 450 1)]
       [formatted (format-memory-stats stats)])
      (assert-true "returns string"
                   (string? formatted))
      
      (assert-true "contains min"
                   (string-contains? formatted "min"))
      
      (assert-true "contains max"
                   (string-contains? formatted "max"))
      
      (assert-true "contains mean"
                   (string-contains? formatted "mean")))

;;; ====
;;; allocate-bytes Tests
;;; ====

(printf "\n=== Testing allocate-bytes ===\n")

(let ([bv (allocate-bytes 1024)])
     (assert-true "returns bytevector"
                  (bytevector? bv))
     
     (assert-equal "correct length"
                   (bytevector-length bv)
                   1024))

;;; ====
;;; measure-under-pressure Tests
;;; ====

(printf "\n=== Testing measure-under-pressure ===\n")

(let ([result (measure-under-pressure (* 1024 1024)  ; 1MB pressure
                                      (lambda () 'tested))])
     (assert-true "returns memory-result"
                  (memory-result? result))
     
     (assert-equal "captures value"
                   (memory-result-value result)
                   'tested))

;;; ====
;;; Edge Cases
;;; ====

(printf "\n=== Testing Edge Cases ===\n")

(assert-true "zero iteration benchmark"
             (let ([result (memory-benchmark "zero" 0 (lambda () 'x))])
                  (null? (memory-benchmark-result-results result))))

(assert-true "empty deltas stats"
             (let ([stats (compute-memory-stats '())])
                  (and (= (memory-stats-min-allocated stats) 0)
                       (= (memory-stats-max-allocated stats) 0))))

(assert-true "allocate zero bytes"
             (= (bytevector-length (allocate-bytes 0)) 0))

;;; ====
;;; Test Summary
;;; ====

(printf "\n====\n")
(printf "Test Results (memory):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
