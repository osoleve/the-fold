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

  (define-test "await raises on task failure"
    (assert-error
      (lambda ()
        (let ([f (spawn (lambda () (error 'test "boom")))])
          (await f)))))

  (define-test "parallel-invoke runs multiple thunks"
    (let ([results (parallel-invoke
                    (lambda () 1)
                    (lambda () 2)
                    (lambda () 3))])
      (assert-equal '(1 2 3) results)))

  (define-test "parallel-map applies function to list"
    (let ([results (parallel-map (lambda (x) (* x x)) '(1 2 3 4 5))])
      (assert-equal '(1 4 9 16 25) results)))

  (define-test "parallel-map handles empty list"
    (assert-equal '() (parallel-map (lambda (x) x) '())))

  (define-test "parallel-for-each executes side effects"
    (let ([sum-box (box 0)]
          [lock (make-mutex)])
      (parallel-for-each
       (lambda (x)
         (with-mutex lock
           (set-box! sum-box (+ (unbox sum-box) x))))
       '(1 2 3 4 5))
      (assert-equal 15 (unbox sum-box)))))

;; Run and cleanup
(run-all-tests)
(pool-shutdown-global!)
