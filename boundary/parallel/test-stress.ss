;;; boundary/parallel/test-stress.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/scheduler.ss")

(test-group "stress-tests"

  (define-test "parallel-map 1000 elements"
    (let* ([xs (iota 1000)]
           [results (parallel-map (lambda (x) (* x 2)) xs)])
      (assert-equal (map (lambda (x) (* x 2)) xs) results)))

  (define-test "nested parallel-invoke"
    (let ([result (parallel-invoke
                   (lambda () (parallel-invoke
                               (lambda () 1)
                               (lambda () 2)))
                   (lambda () (parallel-invoke
                               (lambda () 3)
                               (lambda () 4))))])
      (assert-equal '((1 2) (3 4)) result)))

  (define-test "many concurrent spawns"
    (let ([futures (map (lambda (i) (spawn (lambda () i)))
                        (iota 100))])
      (assert-equal (iota 100) (map await futures))))

  (define-test "error in parallel-invoke propagates"
    (assert-error
      (lambda ()
        (parallel-invoke
         (lambda () 1)
         (lambda () (error 'test "boom"))
         (lambda () 3)))))

  (define-test "error isolation with spawn/await"
    (let* ([f1 (spawn (lambda () 1))]
           [f2 (spawn (lambda () (error 'test "boom")))]
           [f3 (spawn (lambda () 3))]
           [r1 (await f1)]
           [r3 (await f3)])
      (assert-equal 1 r1)
      (assert-equal 3 r3)
      (assert-true (future-done? f2))
      (assert-error (lambda () (await f2))))))

(run-all-tests)

;; Cleanup
(pool-shutdown-global!)
