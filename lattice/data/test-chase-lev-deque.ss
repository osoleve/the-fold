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
      (assert-true (deque-empty? d)))))

(run-all-tests)
