(load "core/testing/test-framework.ss")
(load "core/lang/eval.ss")
(load "lattice/meta/fuel-report.ss")

;;; ====
;;; Progress Computation
;;; ====

(test-group "fuel-progress"

  (define-test "full consumption is 1.0"
    (assert-equal 1.0 (fuel-progress 100 0)))

  (define-test "no consumption is 0.0"
    (assert-equal 0.0 (fuel-progress 100 100)))

  (define-test "half consumption"
    (assert-equal 0.5 (fuel-progress 100 50)))

  (define-test "zero initial fuel"
    (assert-equal 0.0 (fuel-progress 0 0)))

  (define-test "90% consumption"
    (assert-equal 0.9 (fuel-progress 1000 100)))
)

;;; ====
;;; Near-Completion Detection
;;; ====

(test-group "fuel-near-completion"

  (define-test "95% consumed is near-complete"
    (assert-true (fuel-near-completion? 100 5)))

  (define-test "91% consumed is near-complete"
    (assert-true (fuel-near-completion? 100 9)))

  (define-test "89% consumed is NOT near-complete"
    (assert-false (fuel-near-completion? 100 11)))

  (define-test "custom threshold 0.2"
    ;; 85% consumed, threshold 0.2 means >80% is near-complete
    (assert-true (fuel-near-completion? 100 15 0.2)))

  (define-test "custom threshold 0.05"
    ;; 95% consumed, threshold 0.05 means >95% needed
    (assert-false (fuel-near-completion? 100 5 0.05)))
)

;;; ====
;;; Severity Classification
;;; ====

(test-group "fuel-severity"

  (define-test "ok result is completed"
    (assert-equal 'completed (fuel-severity 'ok 100 50)))

  (define-test "error result is error"
    (assert-equal 'error (fuel-severity 'error 100 50)))

  (define-test "suspended with >90% progress is near-miss"
    (assert-equal 'near-miss (fuel-severity 'suspended 100 5)))

  (define-test "suspended with 50% progress is partial"
    (assert-equal 'partial (fuel-severity 'suspended 100 50)))

  (define-test "suspended with 10% progress is minimal"
    (assert-equal 'minimal (fuel-severity 'suspended 100 90)))
)

;;; ====
;;; Suspended Fuel Accessor (core change)
;;; ====

(test-group "suspended-fuel-accessor"

  (define-test "suspended result includes fuel"
    ;; Run an expression with very little fuel to force suspension
    (let ([result (eval-expr '(let ((x 1)) (let ((y 2)) (let ((z 3)) z)))
                             empty-env
                             3)])
      (assert-equal 'suspended (car result))
      ;; The 4th element should be the remaining fuel
      (assert-true (number? (suspended-fuel result)))))

  (define-test "suspended-fuel returns 0 for minimal result"
    (let ([result (eval-expr '(let ((x 1)) x) empty-env 1)])
      ;; With 1 fuel, should suspend immediately
      (assert-equal 'suspended (car result))
      (assert-equal 0 (suspended-fuel result))))

  (define-test "completed result has no suspended-fuel"
    ;; suspended-fuel on a non-suspended result returns 0
    (let ([result (eval-expr '(prim 'add 1 2) empty-env 100)])
      (assert-equal 'ok (car result))
      ;; suspended-fuel should gracefully handle non-suspended results
      (assert-equal 0 (suspended-fuel result))))
)

;;; ====
;;; Fuel Reports
;;; ====

(test-group "fuel-reports"

  (define-test "report from completed evaluation"
    (let* ([result (eval-expr '(prim 'add 1 2) empty-env 100)]
           [report (make-fuel-report result 100)])
      (assert-true (fuel-report? report))
      (assert-equal 'ok (fuel-report-field report 'result-tag))
      (assert-equal 'completed (fuel-report-field report 'severity))))

  (define-test "report from suspended evaluation"
    ;; The evaluator always runs until fuel=0, so remaining is always 0.
    ;; With initial=fuel=3, progress = 100% consumed → near-miss.
    (let* ([initial 3]
           [result (eval-expr '(let ((x 1)) (let ((y 2)) (let ((z 3)) z)))
                              empty-env
                              initial)]
           [report (make-fuel-report result initial)])
      (assert-true (fuel-report? report))
      (assert-equal 'suspended (fuel-report-field report 'result-tag))
      (assert-equal 'near-miss (fuel-report-field report 'severity))))

  (define-test "synthetic minimal severity"
    ;; Minimal: remaining > 70% of initial (suspended early in multi-stage pipeline)
    (let* ([report (make-fuel-report '(suspended expr env 80) 100)])
      (assert-equal 'minimal (fuel-report-field report 'severity))
      (assert-true (< (fuel-report-field report 'progress) 0.3))))

  (define-test "report from near-miss suspension"
    ;; Craft a result that looks like a near-miss
    ;; Use a bigger expression with most of fuel consumed
    (let* ([result (eval-expr '(let ((a 1))
                                 (let ((b 2))
                                   (let ((c 3))
                                     (let ((d 4))
                                       (let ((e 5))
                                         (prim 'add a (prim 'add b (prim 'add c (prim 'add d e)))))))))
                              empty-env
                              10)]  ; enough for some but not all
           [report (make-fuel-report result 10)])
      ;; Whether it completes or suspends depends on exact fuel cost,
      ;; but the report should be structurally valid
      (assert-true (fuel-report? report))
      (assert-true (pair? (fuel-report-field report 'recommendation)))))

  (define-test "report fields are accessible"
    (let* ([result (eval-expr '(prim 'add 1 2) empty-env 100)]
           [report (make-fuel-report result 100)])
      (assert-equal 100 (fuel-report-field report 'initial-fuel))
      (assert-true (number? (fuel-report-field report 'progress)))
      (assert-true (number? (fuel-report-field report 'remaining-fuel)))))
)

;;; ====
;;; Recommendations
;;; ====

(test-group "fuel-recommendations"

  (define-test "completed needs no action"
    (let ([rec (fuel-recommendation 'completed 100 50)])
      (assert-equal 'none (cadr (assq 'strategy (cdr rec))))))

  (define-test "near-miss suggests retry with bump"
    (let ([rec (fuel-recommendation 'near-miss 100 5)])
      (assert-equal 'retry-with-bump (cadr (assq 'strategy (cdr rec))))
      (assert-true (number? (cadr (assq 'suggested-fuel (cdr rec)))))))

  (define-test "partial suggests retry with estimate"
    (let ([rec (fuel-recommendation 'partial 100 50)])
      (assert-equal 'retry-with-estimate (cadr (assq 'strategy (cdr rec))))
      (assert-true (number? (cadr (assq 'suggested-fuel (cdr rec)))))))

  (define-test "minimal suggests profiling"
    (let ([rec (fuel-recommendation 'minimal 100 90)])
      (assert-equal 'profile-first (cadr (assq 'strategy (cdr rec))))))

  (define-test "error suggests fix"
    (let ([rec (fuel-recommendation 'error 100 50)])
      (assert-equal 'fix-error (cadr (assq 'strategy (cdr rec))))))
)

;;; ====
;;; Batch Summary
;;; ====

(test-group "fuel-batch-summary"

  (define-test "empty batch"
    (let ([summary (fuel-report-summary '())])
      (assert-equal 0 (cadr (assq 'total (cdr summary))))))

  (define-test "mixed batch aggregation"
    (let* ([r1 (make-fuel-report '(ok 42 90) 100)]
           [r2 (make-fuel-report '(suspended expr env 5) 100)]
           [r3 (make-fuel-report '(error bad) 100)]
           [summary (fuel-report-summary (list r1 r2 r3))])
      (assert-equal 3 (cadr (assq 'total (cdr summary))))
      (assert-equal 1 (cadr (assq 'completed (cdr summary))))
      (assert-equal 1 (cadr (assq 'near-misses (cdr summary))))
      (assert-equal 1 (cadr (assq 'errors (cdr summary))))))
)

(run-all-tests)
