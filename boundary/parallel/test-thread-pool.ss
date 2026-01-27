;;; boundary/parallel/test-thread-pool.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/thread-pool.ss")

(test-group "thread-pool"

  (define-test "cpu-count returns positive integer"
    (let ([n (cpu-count)])
      (assert-true (and (integer? n) (> n 0)))))

  (define-test "default-worker-count is (max 1 (- cpu-count 1))"
    (let ([n (default-worker-count)])
      (assert-true (>= n 1))
      (assert-true (<= n (cpu-count)))))

  (define-test "make-thread-pool creates pool with workers"
    (let ([pool (make-thread-pool 2)])
      (assert-true (thread-pool? pool))
      (assert-equal 2 (pool-worker-count pool))
      (pool-shutdown! pool))))

(run-all-tests)
