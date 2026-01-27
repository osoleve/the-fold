;;; lattice/data/test-chase-lev-deque.ss
(load "core/base/prelude.ss")
(load "core/testing/test-framework.ss")
(load "lattice/data/chase-lev-deque.ss")

(test-group "chase-lev-deque"

  (define-test "make-chase-lev-deque creates empty deque"
    (let ([d (make-chase-lev-deque 16)])
      (assert-true (chase-lev-deque? d))
      (assert-equal 0 (deque-size d))))

  (define-test "deque-empty? returns true for new deque"
    (let ([d (make-chase-lev-deque 16)])
      (assert-true (deque-empty? d))))

  (define-test "deque-push! adds element"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'task-1)
      (assert-equal 1 (deque-size d))
      (assert-false (deque-empty? d))))

  (define-test "deque-push! multiple elements"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'a)
      (deque-push! d 'b)
      (deque-push! d 'c)
      (assert-equal 3 (deque-size d)))))

(run-all-tests)
