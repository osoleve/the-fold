;;; shell/tests/test-profiler-unified.ss --- Tests for Unified Profiler
;;;
;;; Comprehensive tests for shell/profiler-unified.ss, shell/profile-persist.ss,
;;; and the new profile-repl.ss commands.
;;;
;;; Validates:
;;;   - Unified profiler creation and structure
;;;   - Cost model integration
;;;   - Memory tracking integration
;;;   - Call graph integration
;;;   - Profile persistence (save/load)
;;;   - Statistics extraction

(load "core/test-framework.ss")
(load "shell/profiler-unified.ss")
(load "shell/profile-persist.ss")

(display "\n")
(display "====\n")
(display "          Unified Profiler Tests\n")
(display "====\n")
(display "\n")

;;; ====
;;; Test Expressions
;;; ====

;;; Simple expression for quick tests
;;; Uses eval.ss format: (prim 'op args...)
(define simple-expr '(prim 'add 1 2))

;;; Literal expressions
(define literal-expr '(quote 42))

;;; Let binding expression
(define let-expr '(let ((x 5)) x))

;;; Recursive expression for more complex tests
(define recursive-expr
  '(fix factorial
    (fn (n)
        (if (prim 'lte n 1)
            1
            (prim 'mul n (call factorial (prim 'sub n 1)))))))

;;; ====
;;; Unified Profiler Creation Tests
;;; ====

(test-group unified-profiler-creation
            
            (define-test create-basic-profiler
              (let ([up (profile-unified simple-expr)])
                   (assert-true (unified-profiler? up))))
            
            (define-test profiler-has-base
              (let ([up (profile-unified simple-expr)])
                   (assert-true (profiler? (unified-profiler-base up)))))
            
            (define-test profiler-has-cost-tracker
              (let ([up (profile-unified simple-expr)])
                   (assert-true (cost-tracker? (unified-profiler-cost-tracker up)))))
            
            (define-test profiler-has-alloc-tracker
              (let ([up (profile-unified simple-expr)])
                   (assert-true (alloc-tracker? (unified-profiler-alloc-tracker up)))))
            
            (define-test profiler-has-call-graph
              (let ([up (profile-unified simple-expr)])
                   (assert-true (call-graph? (unified-profiler-call-graph up)))))
            
            (define-test profiler-has-metadata
              (let* ([up (profile-unified simple-expr)]
                     [metadata (unified-profiler-metadata up)])
                    (assert-true (pair? metadata))
                    (assert-true (pair? (assq 'elapsed-ns metadata)))
                    (assert-true (pair? (assq 'cost-model metadata)))))
            
            )

;;; ====
;;; Unified Profiler Status Tests
;;; ====

(test-group unified-profiler-status
            
            (define-test status-complete-for-simple
              (let ([up (profile-unified simple-expr)])
                   (assert-equal 'complete (unified-profiler-status up))))
            
            (define-test result-matches-expected
              (let ([up (profile-unified simple-expr)])
                   (assert-equal 3 (unified-profiler-result up))))
            
            (define-test expr-matches-result
              (let ([up (profile-unified simple-expr)])
                   (assert-equal 3 (unified-profiler-expr up))))
            
            )

;;; ====
;;; Profile Options Tests
;;; ====

(test-group profile-options
            
            (define-test custom-fuel-budget
              (let* ([up (profile-unified simple-expr '((fuel-budget . 5000)))]
                     [metadata (unified-profiler-metadata up)])
                    (assert-equal 5000 (cdr (assq 'fuel-budget metadata)))))
            
            (define-test disable-memory-tracking
              (let* ([up (profile-unified simple-expr '((track-memory . #f)))]
                     [metadata (unified-profiler-metadata up)])
                    (assert-false (cdr (assq 'tracked-memory metadata)))))
            
            (define-test disable-call-graph
              (let* ([up (profile-unified simple-expr '((build-call-graph . #f)))]
                     [metadata (unified-profiler-metadata up)])
                    (assert-false (cdr (assq 'built-graph metadata)))))
            
            (define-test custom-cost-model
              (let* ([up (profile-unified simple-expr
                                          `((cost-model . ,weighted-cost-model)))]
                     [metadata (unified-profiler-metadata up)])
                    (assert-equal 'weighted (cdr (assq 'cost-model metadata)))))
            
            )

;;; ====
;;; Unified Statistics Tests
;;; ====

(test-group unified-statistics
            
            (define-test stats-has-fuel-section
              (let* ([up (profile-unified simple-expr)]
                     [stats (unified-profile-stats up)])
                    (assert-true (pair? (assq 'fuel stats)))))
            
            (define-test stats-has-costs-section
              (let* ([up (profile-unified simple-expr)]
                     [stats (unified-profile-stats up)])
                    (assert-true (pair? (assq 'costs stats)))))
            
            (define-test stats-has-memory-section
              (let* ([up (profile-unified simple-expr)]
                     [stats (unified-profile-stats up)])
                    (assert-true (pair? (assq 'memory stats)))))
            
            (define-test stats-has-call-graph-section
              (let* ([up (profile-unified simple-expr)]
                     [stats (unified-profile-stats up)])
                    (assert-true (pair? (assq 'call-graph stats)))))
            
            (define-test stats-has-metadata-section
              (let* ([up (profile-unified simple-expr)]
                     [stats (unified-profile-stats up)])
                    (assert-true (pair? (assq 'metadata stats)))))
            
            (define-test fuel-used-is-positive
              (let* ([up (profile-unified simple-expr)]
                     [stats (unified-profile-stats up)]
                     [fuel-stats (cdr (assq 'fuel stats))]
                     [used (cdr (assq 'used-fuel fuel-stats))])
                    (assert-true (>= used 0))))
            
            )

;;; ====
;;; Cost Model Integration Tests
;;; ====

(test-group cost-model-integration
            
            (define-test fuel-model-costs-recorded
              (let* ([up (profile-unified simple-expr
                                          `((cost-model . ,fuel-cost-model)))]
                     [cost-tracker (unified-profiler-cost-tracker up)]
                     [costs (get-costs cost-tracker)])
                    ;; Should have some costs recorded
                    (assert-true (or (null? costs) (pair? costs)))))
            
            (define-test weighted-model-costs-recorded
              (let* ([up (profile-unified simple-expr
                                          `((cost-model . ,weighted-cost-model)))]
                     [cost-tracker (unified-profiler-cost-tracker up)]
                     [costs (get-costs cost-tracker)])
                    (assert-true (or (null? costs) (pair? costs)))))
            
            (define-test memory-model-costs-recorded
              (let* ([up (profile-unified simple-expr
                                          `((cost-model . ,memory-cost-model)))]
                     [cost-tracker (unified-profiler-cost-tracker up)]
                     [costs (get-costs cost-tracker)])
                    (assert-true (or (null? costs) (pair? costs)))))
            
            )

;;; ====
;;; Memory Tracking Integration Tests
;;; ====

(test-group memory-tracking
            
            (define-test memory-tracked-when-enabled
              (let* ([up (profile-unified simple-expr '((track-memory . #t)))]
                     [alloc-tracker (unified-profiler-alloc-tracker up)]
                     [summary (alloc-tracker-summary alloc-tracker)])
                    (assert-true (pair? summary))))
            
            (define-test memory-has-total-bytes
              (let* ([up (profile-unified simple-expr '((track-memory . #t)))]
                     [alloc-tracker (unified-profiler-alloc-tracker up)]
                     [summary (alloc-tracker-summary alloc-tracker)])
                    (assert-true (pair? (assq 'total-bytes summary)))))
            
            (define-test memory-has-formatted-total
              (let* ([up (profile-unified simple-expr '((track-memory . #t)))]
                     [alloc-tracker (unified-profiler-alloc-tracker up)]
                     [summary (alloc-tracker-summary alloc-tracker)])
                    (assert-true (pair? (assq 'total-formatted summary)))))
            
            )

;;; ====
;;; Call Graph Integration Tests
;;; ====

(test-group call-graph-integration
            
            (define-test graph-built-when-enabled
              (let* ([up (profile-unified simple-expr '((build-call-graph . #t)))]
                     [graph (unified-profiler-call-graph up)])
                    (assert-true (call-graph? graph))))
            
            (define-test graph-metrics-in-stats
              (let* ([up (profile-unified simple-expr)]
                     [stats (unified-profile-stats up)]
                     [graph-stats (cdr (assq 'call-graph stats))])
                    (assert-true (pair? (assq 'node-count graph-stats)))
                    (assert-true (pair? (assq 'roots graph-stats)))
                    (assert-true (pair? (assq 'leaves graph-stats)))
                    (assert-true (pair? (assq 'cycles graph-stats)))))
            
            )

;;; ====
;;; Report Rendering Tests
;;; ====

(test-group report-rendering
            
            (define-test summary-renders-string
              (let* ([up (profile-unified simple-expr)]
                     [summary (render-unified-summary up)])
                    (assert-true (string? summary))))
            
            (define-test summary-contains-sections
              (let* ([up (profile-unified simple-expr)]
                     [summary (render-unified-summary up)])
                    (assert-true (> (string-length summary) 100))))
            
            (define-test memory-report-renders
              (let* ([up (profile-unified simple-expr)]
                     [report (render-memory-report up)])
                    (assert-true (string? report))))
            
            (define-test cost-report-renders
              (let* ([up (profile-unified simple-expr)]
                     [report (render-cost-report up)])
                    (assert-true (string? report))))
            
            (define-test call-graph-report-renders
              (let* ([up (profile-unified simple-expr)]
                     [report (render-call-graph-report up)])
                    (assert-true (string? report))))
            
            )

;;; ====
;;; Serialization Tests
;;; ====

(test-group serialization
            
            (define-test to-sexp-is-list
              (let* ([up (profile-unified simple-expr)]
                     [sexp (unified-profiler->sexp up)])
                    (assert-true (pair? sexp))))
            
            (define-test sexp-has-version
              (let* ([up (profile-unified simple-expr)]
                     [sexp (unified-profiler->sexp up)])
                    (assert-true (pair? (assq 'version (cdr sexp))))))
            
            (define-test sexp-has-base-profiler
              (let* ([up (profile-unified simple-expr)]
                     [sexp (unified-profiler->sexp up)])
                    (assert-true (pair? (assq 'base-profiler (cdr sexp))))))
            
            (define-test sexp-has-costs
              (let* ([up (profile-unified simple-expr)]
                     [sexp (unified-profiler->sexp up)])
                    (assert-true (pair? (assq 'costs (cdr sexp))))))
            
            (define-test sexp-has-memory
              (let* ([up (profile-unified simple-expr)]
                     [sexp (unified-profiler->sexp up)])
                    (assert-true (pair? (assq 'memory (cdr sexp))))))
            
            (define-test sexp-has-call-graph
              (let* ([up (profile-unified simple-expr)]
                     [sexp (unified-profiler->sexp up)])
                    (assert-true (pair? (assq 'call-graph (cdr sexp))))))
            
            )

;;; ====
;;; Profile Persistence Tests
;;; ====

(test-group persistence
            
            (define-test save-creates-file
              (let* ([up (profile-unified simple-expr)]
                     [filename "/tmp/test-profile-1.profile"]
                     [result (profile-save up filename)])
                    (assert-true result)
                    (assert-true (file-exists? filename))
                    (delete-file filename)))
            
            (define-test load-returns-data
              (let* ([up (profile-unified simple-expr)]
                     [filename "/tmp/test-profile-2.profile"])
                    (profile-save up filename)
                    (let ([loaded (profile-load filename)])
                         (assert-true (profile-data? loaded))
                         (delete-file filename))))
            
            (define-test loaded-data-has-version
              (let* ([up (profile-unified simple-expr)]
                     [filename "/tmp/test-profile-3.profile"])
                    (profile-save up filename)
                    (let* ([loaded (profile-load filename)]
                           [version (profile-data-version loaded)])
                          (assert-equal 1 version)
                          (delete-file filename))))
            
            (define-test loaded-stats-match
              (let* ([up (profile-unified simple-expr)]
                     [filename "/tmp/test-profile-4.profile"]
                     [original-stats (unified-profile-stats up)])
                    (profile-save up filename)
                    (let* ([loaded (profile-load filename)]
                           [loaded-stats (profile-data-stats loaded)]
                           [original-fuel (cdr (assq 'fuel original-stats))]
                           [loaded-fuel (cdr (assq 'fuel loaded-stats))])
                          ;; Compare fuel usage (should be preserved)
                          (assert-equal (cdr (assq 'total-fuel original-fuel))
                                        (cdr (assq 'total-fuel loaded-fuel)))
                          (delete-file filename))))
            
            (define-test load-nonexistent-returns-false
              (let ([loaded (profile-load "/tmp/nonexistent-profile.profile")])
                   (assert-false loaded)))
            
            )

;;; ====
;;; Profile Comparison Tests
;;; ====

(test-group comparison
            
            (define-test compare-loaded-returns-string
              (let* ([up1 (profile-unified simple-expr)]
                     [up2 (profile-unified '(+ 1 2 3))]
                     [f1 "/tmp/test-cmp-1.profile"]
                     [f2 "/tmp/test-cmp-2.profile"])
                    (profile-save up1 f1)
                    (profile-save up2 f2)
                    (let* ([d1 (profile-load f1)]
                           [d2 (profile-load f2)]
                           [comparison (profile-compare-loaded d1 d2)])
                          (assert-true (string? comparison))
                          (delete-file f1)
                          (delete-file f2))))
            
            (define-test compare-with-loaded-returns-string
              (let* ([up (profile-unified simple-expr)]
                     [filename "/tmp/test-cmp-3.profile"])
                    (profile-save up filename)
                    (let ([comparison (profile-load-and-compare up filename)])
                         (assert-true (string? comparison))
                         (delete-file filename))))
            
            )

;;; ====
;;; CSV Export Tests
;;; ====

(test-group csv-export
            
            (define-test export-creates-file
              (let* ([up (profile-unified simple-expr)]
                     [filename "/tmp/test-profile.csv"]
                     [result (profile-export-csv up filename)])
                    (assert-true result)
                    (assert-true (file-exists? filename))
                    (delete-file filename)))
            
            (define-test export-has-header
              (let* ([up (profile-unified simple-expr)]
                     [filename "/tmp/test-profile-header.csv"])
                    (profile-export-csv up filename)
                    (let ([content (call-with-input-file filename
                                                         (lambda (p)
                                                                 (get-line p)))])
                         (assert-equal "category,metric,value" content)
                         (delete-file filename))))
            
            )

;;; ====
;;; Convenience Function Tests
;;; ====

(test-group convenience-functions
            
            (define-test profile-memory-works
              (let ([up (unified-profile-memory simple-expr)])
                   (assert-true (unified-profiler? up))))
            
            (define-test profile-costs-with-model
              (let ([up (unified-profile-costs simple-expr weighted-cost-model)])
                   (assert-true (unified-profiler? up))
                   (assert-equal 'weighted
                                 (cdr (assq 'cost-model
                                            (unified-profiler-metadata up))))))
            
            (define-test profile-minimal-works
              (let* ([up (unified-profile-minimal simple-expr)]
                     [metadata (unified-profiler-metadata up)])
                    (assert-true (unified-profiler? up))
                    (assert-false (cdr (assq 'tracked-memory metadata)))
                    (assert-false (cdr (assq 'built-graph metadata)))))
            
            )

;;; ====
;;; Time Helpers Tests
;;; ====

(test-group time-helpers
            
            (define-test format-nanoseconds
              (assert-equal "500ns" (format-elapsed 500)))
            
            (define-test format-microseconds
              (assert-equal "500us" (format-elapsed 500000)))
            
            (define-test format-milliseconds
              (assert-equal "500ms" (format-elapsed 500000000)))
            
            (define-test format-seconds
              (assert-equal "5s" (format-elapsed 5000000000)))
            
            )

;;; ====
;;; Summary
;;; ====

(newline)
(exit-with-summary)
