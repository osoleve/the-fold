;;; boundary/parallel/test-task.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "boundary/parallel/task.ss")

(test-group "task-and-future"

  (define-test "make-task creates pending task"
    (let ([t (make-task (lambda () 42) 1000)])
      (assert-true (task? t))
      (assert-false (task-done? t))))

  (define-test "task-complete! sets result"
    (let ([t (make-task (lambda () 42) 1000)])
      (task-complete! t 'ok 42)
      (assert-true (task-done? t))
      (assert-equal 42 (task-result t))))

  (define-test "task-fail! sets error"
    (let ([t (make-task (lambda () (error 'oops)) 1000)])
      (task-fail! t "something went wrong")
      (assert-true (task-done? t))
      (assert-true (task-failed? t))))

  (define-test "make-future wraps task"
    (let* ([t (make-task (lambda () 42) 1000)]
           [f (make-future t)])
      (assert-true (future? f))
      (assert-equal t (future-task f)))))

(run-all-tests)
