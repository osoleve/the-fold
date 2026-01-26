;;; lattice/egraph/test-scheduler.ss — Tests for Rule Scheduling
;;;
;;; Run with: scheme --script lattice/egraph/test-scheduler.ss

(load "core/testing/test-framework.ss")
(load "lattice/egraph/scheduler.ss")
(load "lattice/egraph/extract.ss")

(display "\n====\n")
(display "Scheduler Tests\n")
(display "====\n\n")

(test-group "rule-stats"

  (define-test "make-rule-stats creates stats"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      (assert-true (rule-stats? stats))
      (assert-equal rule (stats-rule stats))))

  (define-test "initial stats are zero"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      (assert-equal 0 (stats-total-matches stats))
      (assert-equal 0 (stats-last-matches stats))
      (assert-equal 0 (stats-zero-streaks stats))
      (assert-equal 1.0 (stats-priority stats))))

  (define-test "update-stats! tracks matches"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      (update-stats! stats 5)
      (assert-equal 5 (stats-total-matches stats))
      (assert-equal 5 (stats-last-matches stats))
      (update-stats! stats 3)
      (assert-equal 8 (stats-total-matches stats))
      (assert-equal 3 (stats-last-matches stats))))

  (define-test "zero matches increments streak"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      (update-stats! stats 0)
      (assert-equal 1 (stats-zero-streaks stats))
      (update-stats! stats 0)
      (assert-equal 2 (stats-zero-streaks stats))))

  (define-test "nonzero matches resets streak"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      (update-stats! stats 0)
      (update-stats! stats 0)
      (assert-equal 2 (stats-zero-streaks stats))
      (update-stats! stats 1)
      (assert-equal 0 (stats-zero-streaks stats)))))

(test-group "backoff"

  (define-test "priority decreases after threshold"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      ;; Zero matches up to threshold
      (update-stats! stats 0)
      (update-stats! stats 0)
      (update-stats! stats 0)  ; Hits threshold
      ;; Priority should decrease
      (assert-true (< (stats-priority stats) 1.0))))

  (define-test "priority recovers on match"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      ;; Back off
      (update-stats! stats 0)
      (update-stats! stats 0)
      (update-stats! stats 0)
      (let ([backed-off-priority (stats-priority stats)])
        ;; Now match something
        (update-stats! stats 1)
        (assert-true (> (stats-priority stats) backed-off-priority)))))

  (define-test "priority has minimum floor"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)])
      ;; Many zero-match iterations
      (let loop ([i 0])
        (when (< i 20)
          (update-stats! stats 0)
          (loop (+ i 1))))
      ;; Should not go below minimum
      (assert-true (>= (stats-priority stats) *min-priority*)))))

(test-group "scheduler-creation"

  (define-test "simple-scheduler exists"
    (assert-true (scheduler? simple-scheduler))
    (assert-equal 'simple (scheduler-name simple-scheduler)))

  (define-test "make-backoff-scheduler creates scheduler"
    (let ([s (make-backoff-scheduler)])
      (assert-true (scheduler? s))
      (assert-equal 'backoff (scheduler-name s))))

  (define-test "make-worklist-scheduler creates scheduler"
    (let ([s (make-worklist-scheduler)])
      (assert-true (scheduler? s))
      (assert-equal 'worklist (scheduler-name s))))

  (define-test "make-priority-scheduler creates scheduler"
    (let ([s (make-priority-scheduler)])
      (assert-true (scheduler? s))
      (assert-equal 'priority (scheduler-name s)))))

(test-group "simple-scheduler"

  (define-test "simple scheduler saturates"
    (let ([eg (make-egraph)]
          [config (default-saturation-config)])
      (egraph-add-term! eg '(+ x 0))
      (let ([result (saturate-scheduled eg arith-identity-rules
                                        simple-scheduler config)])
        (assert-true (result-saturated? result)))))

  (define-test "simple scheduler applies identity"
    (let ([eg (make-egraph)]
          [config (default-saturation-config)])
      (let ([term-id (egraph-add-term! eg '(+ x 0))]
            [x-id (egraph-add-term! eg 'x)])
        (saturate-scheduled eg arith-identity-rules simple-scheduler config)
        ;; x and (+ x 0) should be equivalent
        (assert-equal (egraph-find eg term-id) (egraph-find eg x-id))))))

(test-group "backoff-scheduler"

  (define-test "backoff scheduler saturates"
    (let ([eg (make-egraph)]
          [scheduler (make-backoff-scheduler)]
          [config (default-saturation-config)])
      (egraph-add-term! eg '(+ x 0))
      (let ([result (saturate-scheduled eg arith-identity-rules scheduler config)])
        (assert-true (result-saturated? result)))))

  (define-test "backoff scheduler applies identity"
    (let ([eg (make-egraph)]
          [scheduler (make-backoff-scheduler)]
          [config (default-saturation-config)])
      (let ([term-id (egraph-add-term! eg '(* y 1))]
            [y-id (egraph-add-term! eg 'y)])
        (saturate-scheduled eg arith-identity-rules scheduler config)
        (assert-equal (egraph-find eg term-id) (egraph-find eg y-id))))))

(test-group "priority-scheduler"

  (define-test "priority scheduler saturates"
    (let ([eg (make-egraph)]
          [scheduler (make-priority-scheduler)]
          [config (default-saturation-config)])
      (egraph-add-term! eg '(* x 0))
      (let ([result (saturate-scheduled eg arith-identity-rules scheduler config)])
        (assert-true (result-saturated? result)))))

  (define-test "priority scheduler simplifies to zero"
    (let ([eg (make-egraph)]
          [scheduler (make-priority-scheduler)]
          [config (default-saturation-config)])
      (let ([term-id (egraph-add-term! eg '(* x 0))]
            [zero-id (egraph-add-term! eg 0)])
        (saturate-scheduled eg arith-identity-rules scheduler config)
        (assert-equal (egraph-find eg term-id) (egraph-find eg zero-id))))))

(test-group "optimize-scheduled"

  (define-test "optimize with simple scheduler"
    (let ([result (optimize-scheduled '(+ x 0)
                                      arith-identity-rules
                                      ast-size-cost
                                      simple-scheduler)])
      (assert-equal 'x result)))

  (define-test "optimize with backoff scheduler"
    (let* ([scheduler (make-backoff-scheduler)]
           [result (optimize-scheduled '(* y 1)
                                       arith-identity-rules
                                       ast-size-cost
                                       scheduler)])
      (assert-equal 'y result)))

  (define-test "optimize nested with priority scheduler"
    (let* ([scheduler (make-priority-scheduler)]
           [result (optimize-scheduled '(+ (* x 1) 0)
                                       arith-identity-rules
                                       ast-size-cost
                                       scheduler)])
      (assert-equal 'x result))))

(test-group "scheduler-debugging"

  (define-test "scheduler->string formats"
    (let ([s (scheduler->string simple-scheduler)])
      (assert-true (string? s))
      (assert-true (> (string-length s) 0))))

  (define-test "rule-stats->string formats"
    (let* ([rule (make-rule '(+ ?x 0) '?x)]
           [stats (make-rule-stats rule)]
           [s (rule-stats->string stats)])
      (assert-true (string? s))
      (assert-true (> (string-length s) 0)))))

(test-group "scheduled-iteration"

  (define-test "scheduled-iteration applies rules"
    (let ([eg (make-egraph)])
      (let ([term-id (egraph-add-term! eg '(+ a 0))])
        (let ([applied (scheduled-iteration eg
                                            arith-identity-rules
                                            (list term-id)
                                            0)])
          ;; Should have applied at least one rule
          (assert-true (> applied 0))))))

  (define-test "scheduled-iteration respects limit"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg '(+ a 0))
      (egraph-add-term! eg '(+ b 0))
      (egraph-add-term! eg '(+ c 0))
      (let ([applied (scheduled-iteration eg
                                          arith-identity-rules
                                          (uf-roots (egraph-uf eg))
                                          1)])
        ;; Should stop at limit of 1
        (assert-equal 1 applied)))))

(run-all-tests)

