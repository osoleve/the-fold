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

  (define-test "error isolation"
    (let ([results (parallel-invoke
                    (lambda () 1)
                    (lambda () (error 'test "boom"))
                    (lambda () 3))])
      (assert-equal 1 (car results))
      (assert-true (and (pair? (cadr results))
                        (eq? (car (cadr results)) 'error)))
      (assert-equal 3 (caddr results)))))

(run-all-tests)

;; Cleanup
(pool-shutdown-global!)
