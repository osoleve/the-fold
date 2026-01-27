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
      (assert-equal 3 (deque-size d))))

  (define-test "deque-pop! returns last pushed (LIFO)"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'first)
      (deque-push! d 'second)
      (deque-push! d 'third)
      (assert-equal 'third (deque-pop! d))
      (assert-equal 'second (deque-pop! d))
      (assert-equal 'first (deque-pop! d))))

  (define-test "deque-pop! on empty returns 'empty"
    (let ([d (make-chase-lev-deque 16)])
      (assert-equal 'empty (deque-pop! d))))

  (define-test "deque-pop! after emptying returns 'empty"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'x)
      (deque-pop! d)
      (assert-equal 'empty (deque-pop! d))))

  (define-test "deque-steal! returns oldest (FIFO from top)"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'first)
      (deque-push! d 'second)
      (deque-push! d 'third)
      ;; Steal takes from top (oldest)
      (assert-equal 'first (deque-steal! d))
      (assert-equal 'second (deque-steal! d))))

  (define-test "deque-steal! on empty returns 'empty"
    (let ([d (make-chase-lev-deque 16)])
      (assert-equal 'empty (deque-steal! d))))

  (define-test "deque-steal! and deque-pop! work together"
    (let ([d (make-chase-lev-deque 16)])
      (deque-push! d 'a)
      (deque-push! d 'b)
      (deque-push! d 'c)
      ;; Steal oldest, pop newest
      (assert-equal 'a (deque-steal! d))
      (assert-equal 'c (deque-pop! d))
      (assert-equal 'b (deque-pop! d))
      (assert-equal 'empty (deque-pop! d)))))

(run-all-tests)
