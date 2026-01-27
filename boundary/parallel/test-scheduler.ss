;;; boundary/parallel/test-scheduler.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/scheduler.ss")

(test-group "scheduler-api"

  (define-test "spawn returns future"
    (let ([f (spawn (lambda () 42))])
      (assert-true (future? f))))

  (define-test "await returns result"
    (let* ([f (spawn (lambda () (+ 1 2 3)))]
           [result (await f)])
      (assert-equal 6 result)))

  (define-test "spawn/await with multiple tasks"
    (let* ([f1 (spawn (lambda () 10))]
           [f2 (spawn (lambda () 20))]
           [f3 (spawn (lambda () 30))])
      (assert-equal 10 (await f1))
      (assert-equal 20 (await f2))
      (assert-equal 30 (await f3))))

  (define-test "await captures errors"
    (let* ([f (spawn (lambda () (error 'test "boom")))]
           [result (await f)])
      (assert-true (and (pair? result)
                        (eq? (car result) 'error))))))

;; Run and cleanup
(run-all-tests)
(pool-shutdown-global!)
