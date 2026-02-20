(load "core/testing/test-framework.ss")
(load "lattice/pipeline/rlm2.ss")
(load "lattice/pipeline/rlm2-metrics.ss")

;;; ====
;;; Test helpers
;;; ====

(define (make-test-step num action-type ok? . rest)
  (let ([target (if (pair? rest) (car rest) #f)]
        [value (if (and (pair? rest) (pair? (cdr rest))) (cadr rest) "")])
    (make-rlm2-step-record
      num
      (list action-type (or target ""))
      (make-rlm2-observation action-type target value ok?)
      "test note"
      "fake-hash")))

(define (make-submit-step num ok?)
  (make-rlm2-step-record
    num
    '(submit (+ 1 2))
    (make-rlm2-observation 'submit '(+ 1 2) (if ok? 3 "wrong") ok?)
    "submitting"
    "fake-hash"))

;;; ====
;;; Action Histogram
;;; ====

(test-group "rlm2-action-histogram"

  (define-test "empty steps returns empty histogram"
    (assert-equal '() (rlm2-action-histogram '())))

  (define-test "single step counted"
    (let ([hist (rlm2-action-histogram (list (make-test-step 0 'search #t "query")))])
      (assert-equal 1 (length hist))
      (assert-equal 'search (caar hist))
      (assert-equal 1 (cdar hist))))

  (define-test "multiple types counted separately"
    (let* ([steps (list (make-test-step 0 'search #t)
                        (make-test-step 1 'eval #t)
                        (make-test-step 2 'search #t)
                        (make-test-step 3 'store #t))]
           [hist (rlm2-action-histogram steps)])
      (assert-equal 2 (cdr (assq 'search hist)))
      (assert-equal 1 (cdr (assq 'eval hist)))
      (assert-equal 1 (cdr (assq 'store hist)))))

  (define-test "sorted by frequency descending"
    (let* ([steps (list (make-test-step 0 'eval #t)
                        (make-test-step 1 'eval #t)
                        (make-test-step 2 'eval #t)
                        (make-test-step 3 'search #t))]
           [hist (rlm2-action-histogram steps)])
      (assert-equal 'eval (caar hist))))
)

;;; ====
;;; Action Diversity
;;; ====

(test-group "rlm2-action-diversity"

  (define-test "empty returns 0"
    (assert-equal 0 (rlm2-action-diversity '())))

  (define-test "all same type returns 1"
    (let ([steps (list (make-test-step 0 'think #t)
                       (make-test-step 1 'think #t)
                       (make-test-step 2 'think #t))])
      (assert-equal 1 (rlm2-action-diversity steps))))

  (define-test "distinct types counted"
    (let ([steps (list (make-test-step 0 'search #t)
                       (make-test-step 1 'eval #t)
                       (make-test-step 2 'store #t)
                       (make-test-step 3 'submit #t))])
      (assert-equal 4 (rlm2-action-diversity steps))))
)

;;; ====
;;; Submit Analysis
;;; ====

(test-group "rlm2-submit-analysis"

  (define-test "no submits"
    (let ([steps (list (make-test-step 0 'search #t)
                       (make-test-step 1 'eval #t))])
      (assert-equal 0 (length (rlm2-submit-attempts steps)))
      (assert-equal 0 (rlm2-failed-submissions steps))
      (assert-equal 0 (rlm2-successful-submissions steps))))

  (define-test "mixed submit results"
    (let ([steps (list (make-test-step 0 'search #t)
                       (make-submit-step 1 #f)
                       (make-test-step 2 'eval #t)
                       (make-submit-step 3 #f)
                       (make-submit-step 4 #t))])
      (assert-equal 3 (length (rlm2-submit-attempts steps)))
      (assert-equal 2 (rlm2-failed-submissions steps))
      (assert-equal 1 (rlm2-successful-submissions steps))))
)

;;; ====
;;; Success Rate
;;; ====

(test-group "rlm2-success-rate"

  (define-test "overall rate all ok"
    (let ([steps (list (make-test-step 0 'search #t)
                       (make-test-step 1 'eval #t))])
      (assert-equal 1.0 (exact->inexact (rlm2-overall-success-rate steps)))))

  (define-test "overall rate mixed"
    (let ([steps (list (make-test-step 0 'search #t)
                       (make-test-step 1 'eval #f)
                       (make-test-step 2 'search #t)
                       (make-test-step 3 'eval #f))])
      (assert-equal 0.5 (exact->inexact (rlm2-overall-success-rate steps)))))

  (define-test "windowed rate focuses on recent"
    (let* ([steps (list (make-test-step 0 'eval #f)
                        (make-test-step 1 'eval #f)
                        (make-test-step 2 'eval #f)
                        (make-test-step 3 'eval #t)
                        (make-test-step 4 'eval #t))]
           [trace (rlm2-success-trace steps)])
      (assert-equal 1.0 (exact->inexact (rlm2-windowed-success-rate trace 2)))))

  (define-test "empty returns 0"
    (assert-equal 0.0 (rlm2-overall-success-rate '())))
)

;;; ====
;;; Productive Ratio
;;; ====

(test-group "rlm2-productive-ratio"

  (define-test "all productive"
    (let ([steps (list (make-test-step 0 'search #t)
                       (make-test-step 1 'eval #t))])
      (assert-equal 1.0 (exact->inexact (rlm2-productive-ratio steps)))))

  (define-test "half think"
    (let ([steps (list (make-test-step 0 'think #t)
                       (make-test-step 1 'eval #t)
                       (make-test-step 2 'think #t)
                       (make-test-step 3 'search #t))])
      (assert-equal 0.5 (exact->inexact (rlm2-productive-ratio steps)))))

  (define-test "empty returns 0"
    (assert-equal 0.0 (rlm2-productive-ratio '())))
)

;;; ====
;;; Near-Miss Detection
;;; ====

(test-group "rlm2-near-miss"

  (define-test "completed episode is not near-miss"
    (let* ([steps (list (make-test-step 0 'search #t)
                        (make-test-step 1 'eval #t)
                        (make-submit-step 2 #t))]
           [report (rlm2-near-miss-report 'completed steps 100)])
      (assert-true (rlm2-near-miss-report? report))
      (assert-true (< (rlm2-near-miss-score report) 0.4))))

  (define-test "failed submit raises score"
    (let* ([steps (list (make-test-step 0 'search #t)
                        (make-test-step 1 'eval #t)
                        (make-submit-step 2 #f)
                        (make-test-step 3 'eval #t)
                        (make-submit-step 4 #f))]
           [report (rlm2-near-miss-report 'exhausted steps 100)])
      (assert-true (>= (rlm2-near-miss-score report) 0.3))))

  (define-test "exhausted with progress is near-miss"
    (let* ([steps (list (make-test-step 0 'search #t)
                        (make-test-step 1 'eval #t)
                        (make-test-step 2 'store #t)
                        (make-test-step 3 'eval #t)
                        (make-test-step 4 'search #t)
                        (make-test-step 5 'eval #t))]
           [report (rlm2-near-miss-report 'exhausted steps 100)])
      (assert-true (rlm2-near-miss? report))))

  (define-test "all-think exhausted is not near-miss"
    (let* ([steps (list (make-test-step 0 'think #t)
                        (make-test-step 1 'think #t)
                        (make-test-step 2 'think #t))]
           [report (rlm2-near-miss-report 'exhausted steps 100)])
      (assert-false (rlm2-near-miss? report))))

  (define-test "near-miss-field extraction"
    (let* ([steps (list (make-test-step 0 'search #t)
                        (make-submit-step 1 #f))]
           [report (rlm2-near-miss-report 'exhausted steps 100)])
      (assert-equal 'exhausted (rlm2-near-miss-field report 'status))
      (assert-equal 2 (rlm2-near-miss-field report 'total-steps))
      (assert-equal 1 (rlm2-near-miss-field report 'failed-submissions))))
)

;;; ====
;;; Action Patterns
;;; ====

(test-group "rlm2-action-patterns"

  (define-test "bigrams extracted"
    (let* ([steps (list (make-test-step 0 'search #t)
                        (make-test-step 1 'eval #t)
                        (make-test-step 2 'store #t))]
           [bg (rlm2-action-bigrams steps)])
      (assert-equal 2 (length bg))
      (assert-equal '(search eval) (car bg))
      (assert-equal '(eval store) (cadr bg))))

  (define-test "empty and single return empty bigrams"
    (assert-equal '() (rlm2-action-bigrams '()))
    (assert-equal '() (rlm2-action-bigrams (list (make-test-step 0 'search #t)))))

  (define-test "max runs detected"
    (let* ([steps (list (make-test-step 0 'eval #t)
                        (make-test-step 1 'eval #t)
                        (make-test-step 2 'eval #t)
                        (make-test-step 3 'search #t)
                        (make-test-step 4 'eval #t))]
           [runs (rlm2-max-runs steps)])
      (assert-equal 3 (cdr (assq 'eval runs)))
      (assert-equal 1 (cdr (assq 'search runs)))))
)

;;; ====
;;; Aggregate Metrics
;;; ====

(test-group "rlm2-episode-metrics"

  (define-test "produces valid metrics structure"
    (let* ([steps (list (make-test-step 0 'search #t)
                        (make-test-step 1 'eval #t)
                        (make-submit-step 2 #t))]
           [m (rlm2-episode-metrics 'completed steps 100)])
      (assert-true (rlm2-episode-metrics? m))
      (assert-equal 3 (rlm2-metrics-field m 'diversity))
      (assert-equal 1 (rlm2-metrics-field m 'submit-attempts))
      (assert-equal 0 (rlm2-metrics-field m 'failed-submissions))
      (assert-true (rlm2-near-miss-report? (rlm2-metrics-field m 'near-miss)))))
)

(run-all-tests)
