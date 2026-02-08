;;; lattice/fp/category/test-common-monads.ss — Tests for Maybe, Either, Reader monads
;;; Run with: scheme --script lattice/fp/category/test-common-monads.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/category/common-monads.ss")

;;; ============================================================
;;; Maybe Monad
;;; ============================================================

(test-group "maybe-construction"
  (define-test "maybe-just wraps value"
    (let ([m (maybe-just 42)])
      (assert-true (maybe-just? m))
      (assert-equal 42 (maybe-from-just m))))

  (define-test "maybe-nothing is #f"
    (assert-equal #f maybe-nothing)
    (assert-true (maybe-nothing? maybe-nothing)))

  (define-test "maybe-just? on nothing"
    (assert-false (maybe-just? maybe-nothing)))

  (define-test "maybe-nothing? on just"
    (assert-false (maybe-nothing? (maybe-just 1))))

  (define-test "maybe? validates both cases"
    (assert-true (maybe? maybe-nothing))
    (assert-true (maybe? (maybe-just 42))))

  (define-test "maybe-just #f is just, not nothing"
    ;; Tagged representation: (just #f) ≠ nothing
    (let ([m (maybe-just #f)])
      (assert-true (maybe-just? m))
      (assert-false (maybe-nothing? m))
      (assert-equal #f (maybe-from-just m)))))

(test-group "maybe-operations"
  (define-test "maybe-return wraps"
    (let ([m (maybe-return 7)])
      (assert-true (maybe-just? m))
      (assert-equal 7 (maybe-from-just m))))

  (define-test "maybe-bind on just"
    (let ([result (maybe-bind (maybe-just 3)
                              (lambda (x) (maybe-just (* x 2))))])
      (assert-true (maybe-just? result))
      (assert-equal 6 (maybe-from-just result))))

  (define-test "maybe-bind on nothing"
    (let ([result (maybe-bind maybe-nothing
                              (lambda (x) (maybe-just (* x 2))))])
      (assert-true (maybe-nothing? result))))

  (define-test "maybe-fmap on just"
    (let ([result (maybe-fmap (lambda (x) (+ x 10)) (maybe-just 5))])
      (assert-true (maybe-just? result))
      (assert-equal 15 (maybe-from-just result))))

  (define-test "maybe-fmap on nothing"
    (let ([result (maybe-fmap (lambda (x) (+ x 10)) maybe-nothing)])
      (assert-true (maybe-nothing? result))))

  (define-test "maybe-join flattens nested just"
    (let ([result (maybe-join (maybe-just (maybe-just 42)))])
      (assert-true (maybe-just? result))
      (assert-equal 42 (maybe-from-just result))))

  (define-test "maybe-join on nothing"
    (assert-true (maybe-nothing? (maybe-join maybe-nothing))))

  (define-test "maybe-join on just-nothing"
    (assert-true (maybe-nothing? (maybe-join (maybe-just maybe-nothing)))))

  (define-test "maybe-from-default on just"
    (assert-equal 42 (maybe-from-default 0 (maybe-just 42))))

  (define-test "maybe-from-default on nothing"
    (assert-equal 0 (maybe-from-default 0 maybe-nothing)))

  (define-test "maybe-or-else on just"
    (let ([result (maybe-or-else (maybe-just 1) (lambda () (maybe-just 2)))])
      (assert-equal 1 (maybe-from-just result))))

  (define-test "maybe-or-else on nothing"
    (let ([result (maybe-or-else maybe-nothing (lambda () (maybe-just 2)))])
      (assert-equal 2 (maybe-from-just result)))))

(test-group "maybe-monad-laws"
  ;; Left identity: return a >>= f  ≡  f a
  (define-test "left identity"
    (let* ([f (lambda (x) (maybe-just (* x 2)))]
           [a 5]
           [lhs (maybe-bind (maybe-return a) f)]
           [rhs (f a)])
      (assert-equal (maybe-from-just lhs) (maybe-from-just rhs))))

  ;; Right identity: m >>= return  ≡  m
  (define-test "right identity"
    (let* ([m (maybe-just 42)]
           [result (maybe-bind m maybe-return)])
      (assert-equal (maybe-from-just m) (maybe-from-just result))))

  ;; Associativity: (m >>= f) >>= g  ≡  m >>= (λx. f x >>= g)
  (define-test "associativity"
    (let* ([m (maybe-just 3)]
           [f (lambda (x) (maybe-just (* x 2)))]
           [g (lambda (x) (maybe-just (+ x 1)))]
           [lhs (maybe-bind (maybe-bind m f) g)]
           [rhs (maybe-bind m (lambda (x) (maybe-bind (f x) g)))])
      (assert-equal (maybe-from-just lhs) (maybe-from-just rhs)))))

;;; ============================================================
;;; Either Monad
;;; ============================================================

(test-group "either-construction"
  (define-test "make-right creates right"
    (let ([e (make-right 42)])
      (assert-true (either-right? e))
      (assert-false (either-left? e))
      (assert-equal 42 (either-from-right e))))

  (define-test "make-left creates left"
    (let ([e (make-left "error")])
      (assert-true (either-left? e))
      (assert-false (either-right? e))
      (assert-equal "error" (either-from-left e))))

  (define-test "either? on right"
    (assert-true (either? (make-right 1))))

  (define-test "either? on left"
    (assert-true (either? (make-left "err"))))

  (define-test "either? on non-either"
    (assert-false (either? 42))
    (assert-false (either? '(foo 1))))

  (define-test "either-value extracts from right"
    (assert-equal 42 (either-value (make-right 42))))

  (define-test "either-value extracts from left"
    (assert-equal "err" (either-value (make-left "err")))))

(test-group "either-operations"
  (define-test "either-return wraps in right"
    (let ([e (either-return 7)])
      (assert-true (either-right? e))
      (assert-equal 7 (either-from-right e))))

  (define-test "either-bind on right"
    (let ([result (either-bind (make-right 3)
                               (lambda (x) (make-right (* x 2))))])
      (assert-true (either-right? result))
      (assert-equal 6 (either-from-right result))))

  (define-test "either-bind on left propagates error"
    (let ([result (either-bind (make-left "oops")
                               (lambda (x) (make-right (* x 2))))])
      (assert-true (either-left? result))
      (assert-equal "oops" (either-from-left result))))

  (define-test "either-fmap on right"
    (let ([result (either-fmap (lambda (x) (* x 10)) (make-right 5))])
      (assert-equal 50 (either-from-right result))))

  (define-test "either-fmap on left"
    (let ([result (either-fmap (lambda (x) (* x 10)) (make-left "err"))])
      (assert-true (either-left? result))))

  (define-test "either-join flattens nested right"
    (let ([result (either-join (make-right (make-right 42)))])
      (assert-equal 42 (either-from-right result))))

  (define-test "either-join on left"
    (let ([result (either-join (make-left "err"))])
      (assert-true (either-left? result))))

  (define-test "either-from-default on right"
    (assert-equal 42 (either-from-default 0 (make-right 42))))

  (define-test "either-from-default on left"
    (assert-equal 0 (either-from-default 0 (make-left "err"))))

  (define-test "either-map-left transforms error"
    (let ([result (either-map-left string-length (make-left "err"))])
      (assert-equal 3 (either-from-left result))))

  (define-test "either-map-left leaves right alone"
    (let ([result (either-map-left string-length (make-right 42))])
      (assert-equal 42 (either-from-right result))))

  (define-test "either-fold on right"
    (assert-equal 84 (either-fold string-length (lambda (x) (* x 2)) (make-right 42))))

  (define-test "either-fold on left"
    (assert-equal 3 (either-fold string-length (lambda (x) (* x 2)) (make-left "err")))))

(test-group "either-monad-laws"
  (define-test "left identity"
    (let* ([f (lambda (x) (make-right (* x 2)))]
           [a 5]
           [lhs (either-bind (either-return a) f)]
           [rhs (f a)])
      (assert-equal (either-from-right lhs) (either-from-right rhs))))

  (define-test "right identity"
    (let* ([m (make-right 42)]
           [result (either-bind m either-return)])
      (assert-equal (either-from-right m) (either-from-right result))))

  (define-test "associativity"
    (let* ([m (make-right 3)]
           [f (lambda (x) (make-right (* x 2)))]
           [g (lambda (x) (make-right (+ x 1)))]
           [lhs (either-bind (either-bind m f) g)]
           [rhs (either-bind m (lambda (x) (either-bind (f x) g)))])
      (assert-equal (either-from-right lhs) (either-from-right rhs)))))

;;; ============================================================
;;; Reader Monad
;;; ============================================================

(test-group "reader-operations"
  (define-test "reader-return ignores environment"
    (let ([r (reader-return 42)])
      (assert-equal 42 (run-reader r 'any-env))
      (assert-equal 42 (run-reader r '()))))

  (define-test "reader-ask returns environment"
    (assert-equal '(a b c) (run-reader reader-ask '(a b c))))

  (define-test "reader-asks projects environment"
    (let ([r (reader-asks car)])
      (assert-equal 'a (run-reader r '(a b c)))))

  (define-test "reader-bind threads environment"
    (let* ([r (reader-bind reader-ask
                           (lambda (env) (reader-return (length env))))])
      (assert-equal 3 (run-reader r '(a b c)))))

  (define-test "reader-fmap transforms result"
    (let ([r (reader-fmap (lambda (x) (* x 2)) (reader-return 21))])
      (assert-equal 42 (run-reader r 'ignored))))

  (define-test "reader-join flattens nested reader"
    (let* ([inner (reader-return 42)]
           [outer (reader-return inner)]
           [result (reader-join outer)])
      (assert-equal 42 (run-reader result 'env))))

  (define-test "reader-local modifies environment"
    (let ([r (reader-local cdr reader-ask)])
      (assert-equal '(b c) (run-reader r '(a b c)))))

  (define-test "reader-local is scoped"
    ;; Original environment visible outside local
    (let* ([inner (reader-local cdr reader-ask)]
           [outer (reader-bind reader-ask
                    (lambda (full-env)
                      (reader-bind inner
                        (lambda (tail-env)
                          (reader-return (list full-env tail-env))))))])
      (assert-equal '((a b c) (b c)) (run-reader outer '(a b c))))))

(test-group "reader-monad-laws"
  (define env '(x y z))

  (define-test "left identity"
    (let* ([f (lambda (x) (reader-asks (lambda (e) (cons x e))))]
           [a 'hello]
           [lhs (run-reader (reader-bind (reader-return a) f) env)]
           [rhs (run-reader (f a) env)])
      (assert-equal lhs rhs)))

  (define-test "right identity"
    (let* ([m reader-ask]
           [lhs (run-reader (reader-bind m reader-return) env)]
           [rhs (run-reader m env)])
      (assert-equal lhs rhs)))

  (define-test "associativity"
    (let* ([m reader-ask]
           [f (lambda (x) (reader-return (length x)))]
           [g (lambda (n) (reader-return (* n 2)))]
           [lhs (run-reader (reader-bind (reader-bind m f) g) env)]
           [rhs (run-reader (reader-bind m (lambda (x) (reader-bind (f x) g))) env)])
      (assert-equal lhs rhs))))

;;; ============================================================
;;; MonadOps Records
;;; ============================================================

(test-group "monad-ops-records"
  (define-test "monad-maybe is a MonadOps"
    (assert-true (monad-ops? monad-maybe)))

  (define-test "monad-either is a MonadOps"
    (assert-true (monad-ops? monad-either)))

  (define-test "monad-reader is a MonadOps"
    (assert-true (monad-ops? monad-reader)))

  (define-test "monad-ops-name"
    (assert-equal 'maybe (monad-ops-name monad-maybe))
    (assert-equal 'either (monad-ops-name monad-either))
    (assert-equal 'reader (monad-ops-name monad-reader))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests-and-exit)
