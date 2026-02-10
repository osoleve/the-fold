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
      (pool-shutdown! pool)))

  (define-test "pool-start! launches workers"
    (let ([pool (make-thread-pool 2)])
      (pool-start! pool)
      (assert-true (pool-running? pool))
      (sleep (make-time 'time-duration 0 0))  ; Yield
      (pool-shutdown! pool)
      (pool-wait-shutdown! pool)))

  (define-test "pool executes submitted task"
    (let ([pool (make-thread-pool 2)]
          [result-box (box #f)])
      (pool-start! pool)
      (pool-submit! pool (make-task (lambda ()
                                      (set-box! result-box 42)
                                      42)
                                    #f))
      ;; Wait for result
      (let loop ([n 100])
        (when (and (> n 0) (not (unbox result-box)))
          (sleep (make-time 'time-duration 10000000 0))  ; 10ms
          (loop (- n 1))))
      (pool-shutdown! pool)
      (pool-wait-shutdown! pool)
      (assert-equal 42 (unbox result-box))))

  (define-test "injection queue handles concurrent submissions"
    (let* ([pool (make-thread-pool 4)]
           [counter (box 0)]
           [lock (make-mutex)]
           [n 200])
      (pool-start! pool)
      ;; Submit many tasks from multiple threads to stress the injection queue
      (let ([submitters
             (map (lambda (batch)
                    (fork-thread
                     (lambda ()
                       (for-each
                        (lambda (i)
                          (pool-submit! pool
                            (make-task (lambda ()
                                         (with-mutex lock
                                           (set-box! counter (+ 1 (unbox counter)))))
                                       #f)))
                        (iota 50)))))
                  (iota 4))])
        ;; Wait for submitters to finish
        (for-each thread-join submitters))
      ;; Wait for all tasks to complete
      (let loop ([tries 500])
        (when (and (> tries 0) (< (unbox counter) n))
          (sleep (make-time 'time-duration 10000000 0))
          (loop (- tries 1))))
      (pool-shutdown! pool)
      (pool-wait-shutdown! pool)
      (assert-equal n (unbox counter)))))

(run-all-tests)
