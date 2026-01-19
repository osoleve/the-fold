;;; boundary/tests/test-alloc-tracker.ss --- Tests for Allocation Tracker
;;;
;;; Comprehensive tests for boundary/diagnostics/alloc-tracker.ss.
;;; Validates byte formatting, allocation tracking, and tracker accumulation.

(load "core/test-framework.ss")
(load "boundary/diagnostics/alloc-tracker.ss")

(display "\n")
(display "====\n")
(display "          Allocation Tracker Tests\n")
(display "====\n")
(display "\n")

;;; ====
;;; format-bytes Tests
;;; ====

(test-group format-bytes
            
            (define-test format-bytes-zero
              (assert-equal "0 B" (format-bytes 0)))
            
            (define-test format-bytes-small
              (assert-equal "1 B" (format-bytes 1)))
            
            (define-test format-bytes-under-1k
              (assert-equal "512 B" (format-bytes 512)))
            
            (define-test format-bytes-exactly-1k
              (assert-equal "1 KB" (format-bytes 1024)))
            
            (define-test format-bytes-kilobytes
              (assert-equal "1.5 KB" (format-bytes 1536)))
            
            (define-test format-bytes-larger-kb
              (assert-equal "10 KB" (format-bytes 10240)))
            
            (define-test format-bytes-fractional-kb
              (assert-equal "2.5 KB" (format-bytes 2560)))
            
            (define-test format-bytes-exactly-1mb
              (assert-equal "1 MB" (format-bytes (* 1024 1024))))
            
            (define-test format-bytes-megabytes
              (assert-equal "5.5 MB" (format-bytes (* 1024 1024 5.5))))
            
            (define-test format-bytes-larger-mb
              (assert-equal "100 MB" (format-bytes (* 1024 1024 100))))
            
            (define-test format-bytes-exactly-1gb
              (assert-equal "1 GB" (format-bytes (* 1024 1024 1024))))
            
            (define-test format-bytes-gigabytes
              (assert-equal "2.5 GB" (format-bytes (exact (round (* 1024 1024 1024 2.5))))))
            
            (define-test format-bytes-negative
              ;; Negative bytes shouldn't happen but we handle them
              (assert-equal "-100 B" (format-bytes -100)))
            
            )

;;; ====
;;; format-decimal Tests
;;; ====

(test-group format-decimal
            
            (define-test format-decimal-whole
              (assert-equal "5" (format-decimal 5.0 1)))
            
            (define-test format-decimal-one-place
              (assert-equal "3.5" (format-decimal 3.5 1)))
            
            (define-test format-decimal-rounds-down
              (assert-equal "2.1" (format-decimal 2.14 1)))
            
            (define-test format-decimal-rounds-up
              (assert-equal "2.2" (format-decimal 2.16 1)))
            
            (define-test format-decimal-two-places
              (assert-equal "3.14" (format-decimal 3.14159 2)))
            
            )

;;; ====
;;; clean-decimal-string Tests
;;; ====

(test-group clean-decimal-string
            
            (define-test clean-trailing-zeros
              (assert-equal "1.5" (clean-decimal-string "1.50")))
            
            (define-test clean-all-zeros-after-point
              (assert-equal "2" (clean-decimal-string "2.00")))
            
            (define-test clean-no-change-needed
              (assert-equal "3.14" (clean-decimal-string "3.14")))
            
            (define-test clean-no-decimal
              (assert-equal "42" (clean-decimal-string "42")))
            
            (define-test clean-multiple-zeros
              (assert-equal "1" (clean-decimal-string "1.000")))
            
            )

;;; ====
;;; get-allocation-stats Tests
;;; ====

(test-group get-allocation-stats
            
            (define-test stats-returns-alist
              (let ([stats (get-allocation-stats)])
                   (assert-true (pair? stats))))
            
            (define-test stats-has-bytes-allocated
              (let ([stats (get-allocation-stats)])
                   (assert-true (pair? (assq 'bytes-allocated stats)))))
            
            (define-test stats-has-gc-bytes
              (let ([stats (get-allocation-stats)])
                   (assert-true (pair? (assq 'gc-bytes stats)))))
            
            (define-test stats-has-gc-count
              (let ([stats (get-allocation-stats)])
                   (assert-true (pair? (assq 'gc-count stats)))))
            
            (define-test stats-bytes-is-positive
              (let* ([stats (get-allocation-stats)]
                     [bytes (cdr (assq 'bytes-allocated stats))])
                    (assert-true (> bytes 0))))
            
            )

;;; ====
;;; track-allocations Tests
;;; ====

(test-group track-allocations
            
            (define-test track-returns-result
              (let-values ([(result bytes) (track-allocations (lambda () 42))])
                          (assert-equal 42 result)))
            
            (define-test track-returns-bytes
              (let-values ([(result bytes) (track-allocations (lambda () 42))])
                          (assert-true (integer? bytes))))
            
            (define-test track-non-negative-bytes
              (let-values ([(result bytes) (track-allocations (lambda () 42))])
                          (assert-true (>= bytes 0))))
            
            (define-test track-allocating-thunk
              ;; Create some allocations by making a large list
              (let-values ([(result bytes)
                            (track-allocations
                             (lambda ()
                                     (let loop ([n 10000] [acc '()])
                                          (if (= n 0)
                                              acc
                                              (loop (- n 1) (cons n acc))))))])
                          ;; Should have allocated something
                          (assert-true (> bytes 0))))
            
            (define-test track-list-returns-list
              (let ([result (track-allocations/list (lambda () 'ok))])
                   (assert-true (list? result))
                   (assert-equal 2 (length result))
                   (assert-equal 'ok (car result))))
            
            )

;;; ====
;;; Allocation Tracker Tests
;;; ====

(test-group alloc-tracker
            
            (define-test make-tracker
              (let ([tracker (make-alloc-tracker)])
                   (assert-true (alloc-tracker? tracker))))
            
            (define-test tracker-predicate-false
              (assert-false (alloc-tracker? 42))
              (assert-false (alloc-tracker? '()))
              (assert-false (alloc-tracker? "not a tracker")))
            
            (define-test tracker-initially-empty
              (let ([tracker (make-alloc-tracker)])
                   (assert-equal '() (alloc-tracker-entries tracker))
                   (assert-equal 0 (alloc-tracker-total tracker))))
            
            (define-test tracker-record
              (let* ([tracker (make-alloc-tracker)]
                     [updated (alloc-tracker-record! tracker "test" 1024)])
                    (assert-equal 1 (length (alloc-tracker-entries updated)))
                    (assert-equal 1024 (alloc-tracker-total updated))))
            
            (define-test tracker-multiple-records
              (let* ([t0 (make-alloc-tracker)]
                     [t1 (alloc-tracker-record! t0 "first" 100)]
                     [t2 (alloc-tracker-record! t1 "second" 200)]
                     [t3 (alloc-tracker-record! t2 "third" 300)])
                    (assert-equal 3 (length (alloc-tracker-entries t3)))
                    (assert-equal 600 (alloc-tracker-total t3))))
            
            (define-test tracker-immutable
              ;; Original tracker should be unchanged after recording
              (let* ([original (make-alloc-tracker)]
                     [updated (alloc-tracker-record! original "test" 1000)])
                    (assert-equal 0 (alloc-tracker-total original))
                    (assert-equal 1000 (alloc-tracker-total updated))))
            
            )

;;; ====
;;; Tracker Summary Tests
;;; ====

(test-group tracker-summary
            
            (define-test summary-structure
              (let* ([tracker (make-alloc-tracker)]
                     [summary (alloc-tracker-summary tracker)])
                    (assert-true (pair? (assq 'total-bytes summary)))
                    (assert-true (pair? (assq 'total-formatted summary)))
                    (assert-true (pair? (assq 'entry-count summary)))
                    (assert-true (pair? (assq 'entries summary)))))
            
            (define-test summary-values
              (let* ([t0 (make-alloc-tracker)]
                     [t1 (alloc-tracker-record! t0 "alloc1" 2048)]
                     [summary (alloc-tracker-summary t1)])
                    (assert-equal 2048 (cdr (assq 'total-bytes summary)))
                    (assert-equal "2 KB" (cdr (assq 'total-formatted summary)))
                    (assert-equal 1 (cdr (assq 'entry-count summary)))))
            
            (define-test summary-entries-in-order
              (let* ([t0 (make-alloc-tracker)]
                     [t1 (alloc-tracker-record! t0 "first" 100)]
                     [t2 (alloc-tracker-record! t1 "second" 200)]
                     [summary (alloc-tracker-summary t2)]
                     [entries (cdr (assq 'entries summary))])
                    ;; Should be in chronological order (first recorded first)
                    (assert-equal "first" (caar entries))
                    (assert-equal "second" (caadr entries))))
            
            )

;;; ====
;;; Top-N Tests
;;; ====

(test-group top-n
            
            (define-test top-n-sorts-descending
              (let* ([t0 (make-alloc-tracker)]
                     [t1 (alloc-tracker-record! t0 "small" 100)]
                     [t2 (alloc-tracker-record! t1 "medium" 500)]
                     [t3 (alloc-tracker-record! t2 "large" 1000)]
                     [top (alloc-tracker-top-n t3 3)])
                    (assert-equal "large" (caar top))
                    (assert-equal 1000 (cdar top))))
            
            (define-test top-n-limits-results
              (let* ([t0 (make-alloc-tracker)]
                     [t1 (alloc-tracker-record! t0 "a" 100)]
                     [t2 (alloc-tracker-record! t1 "b" 200)]
                     [t3 (alloc-tracker-record! t2 "c" 300)]
                     [top (alloc-tracker-top-n t3 2)])
                    (assert-equal 2 (length top))))
            
            (define-test top-n-handles-empty
              (let* ([tracker (make-alloc-tracker)]
                     [top (alloc-tracker-top-n tracker 5)])
                    (assert-equal '() top)))
            
            )

;;; ====
;;; Integration Tests
;;; ====

(test-group integration
            
            (define-test track-and-record
              ;; Use track-allocations and record result
              (let*-values ([(t0) (make-alloc-tracker)]
                            [(t1 result) (alloc-tracker-record-thunk!
                                          t0
                                          "list-creation"
                                          (lambda ()
                                                  (make-list 1000 0)))])
                           (assert-true (list? result))
                           (assert-equal 1000 (length result))
                           (assert-equal 1 (length (alloc-tracker-entries t1)))))
            
            (define-test with-tracked-macro
              (with-tracked-allocations (result bytes)
                                        (+ 1 2 3)
                                        (assert-equal 6 result)
                                        (assert-true (integer? bytes))))
            
            )

;;; ====
;;; Summary
;;; ====

(newline)
(exit-with-summary)
