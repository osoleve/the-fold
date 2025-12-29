;;; fabric/stitches/fp/test-reader.ss — Tests for Reader Monad

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/reader.ss")

(display "\n")
(display "==============================================================\n")
(display "         READER MONAD TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Core Reader Tests
;;; ============================================================

(test-group reader-basics
  (define-test make-reader-test
    (let ([rd (make-reader (lambda (env) 42))])
      (assert-true (reader? rd))))

  (define-test run-reader-test
    (let ([rd (make-reader (lambda (env) (+ env 10)))])
      (assert-equal 15 (run-reader rd 5))))

  (define-test reader-pure-test
    (assert-equal 42 (run-reader (reader-pure 42) 'any-env))
    (assert-equal "hello" (run-reader (reader-pure "hello") 0))))

;;; ============================================================
;;; Ask and Asks Tests
;;; ============================================================

(test-group ask-operations
  (define-test ask-test
    (assert-equal 42 (run-reader reader-ask 42))
    (assert-equal '(a b c) (run-reader reader-ask '(a b c))))

  (define-test asks-test
    (let ([rd (reader-asks (lambda (env) (+ env 10)))])
      (assert-equal 15 (run-reader rd 5))))

  (define-test asks-with-alist-test
    (let ([rd (reader-asks (lambda (env) (cdr (assoc 'name env))))])
      (assert-equal "Alice" (run-reader rd '((name . "Alice") (age . 30)))))))

;;; ============================================================
;;; Monad Law Tests
;;; ============================================================

(test-group monad-laws
  ;; Left identity: pure a >>= f  ===  f a
  (define-test left-identity-test
    (let* ([a 5]
           [f (lambda (x) (reader-pure (+ x 10)))]
           [lhs (reader-bind (reader-pure a) f)]
           [rhs (f a)])
      (assert-equal (run-reader lhs 'env) (run-reader rhs 'env))))

  ;; Right identity: m >>= pure  ===  m
  (define-test right-identity-test
    (let ([m (reader-asks add1)])
      (let ([lhs (reader-bind m reader-pure)]
            [rhs m])
        (assert-equal (run-reader lhs 5) (run-reader rhs 5)))))

  ;; Associativity: (m >>= f) >>= g  ===  m >>= (x -> f x >>= g)
  (define-test associativity-test
    (let ([m (reader-pure 1)]
          [f (lambda (x) (reader-pure (+ x 10)))]
          [g (lambda (x) (reader-pure (+ x 100)))])
      (let ([lhs (reader-bind (reader-bind m f) g)]
            [rhs (reader-bind m (lambda (x) (reader-bind (f x) g)))])
        (assert-equal (run-reader lhs 'env) (run-reader rhs 'env))))))

;;; ============================================================
;;; Bind and Map Tests
;;; ============================================================

(test-group reader-monad
  (define-test bind-test
    (let ([rd (reader-bind reader-ask
                           (lambda (env)
                             (reader-pure (+ env 10))))])
      (assert-equal 15 (run-reader rd 5))))

  (define-test bind-chain-test
    (let ([rd (reader-bind reader-ask
                           (lambda (a)
                             (reader-bind (reader-asks (lambda (env) (+ env 1)))
                                          (lambda (b)
                                            (reader-pure (cons a b))))))])
      (assert-equal (cons 5 6) (run-reader rd 5))))

  (define-test then-test
    (let ([rd (reader-then (reader-pure 'ignored)
                           (reader-asks add1))])
      (assert-equal 6 (run-reader rd 5))))

  (define-test map-test
    (let ([rd (reader-map add1 reader-ask)])
      (assert-equal 6 (run-reader rd 5))))

  (define-test ap-test
    (let ([rd (reader-ap (reader-pure add1) reader-ask)])
      (assert-equal 6 (run-reader rd 5)))))

;;; ============================================================
;;; Local Tests
;;; ============================================================

(test-group local-operations
  (define-test local-test
    (let ([rd (reader-local add1 reader-ask)])
      (assert-equal 6 (run-reader rd 5))))

  (define-test local-nested-test
    (let ([inner (reader-local add1 reader-ask)]
          [outer (reader-local (lambda (x) (+ x 10)) reader-ask)])
      ;; Inner adds 1 to env, outer adds 10 to env
      (assert-equal 6 (run-reader inner 5))
      (assert-equal 15 (run-reader outer 5))))

  (define-test local-with-alist-test
    (let* ([get-x (reader-asks (lambda (env) (cdr (assoc 'x env))))]
           [rd (reader-local (lambda (env) (cons '(x . 100) env)) get-x)])
      ;; Local should override x
      (assert-equal 100 (run-reader rd '((x . 1)))))))

;;; ============================================================
;;; Sequence Tests
;;; ============================================================

(test-group reader-sequence
  (define-test sequence-empty-test
    (assert-equal '() (run-reader (reader-sequence '()) 'env)))

  (define-test sequence-test
    (let ([readers (list (reader-pure 1)
                         (reader-pure 2)
                         (reader-pure 3))])
      (assert-equal '(1 2 3) (run-reader (reader-sequence readers) 'env))))

  (define-test sequence-with-env-test
    (let ([readers (list reader-ask
                         (reader-asks add1)
                         (reader-asks (lambda (x) (+ x 10))))])
      (assert-equal '(5 6 15) (run-reader (reader-sequence readers) 5))))

  (define-test map-m-test
    (let ([rd (reader-map-m (lambda (x) (reader-asks (lambda (env) (+ x env))))
                            '(1 2 3))])
      (assert-equal '(11 12 13) (run-reader rd 10)))))

;;; ============================================================
;;; Alist Environment Tests
;;; ============================================================

(test-group alist-environment
  (define-test get-key-missing-test
    (let ([result (run-reader (reader-get-key 'x) '())])
      (assert-true (nothing? result))))

  (define-test get-key-present-test
    (let ([result (run-reader (reader-get-key 'x) '((x . 42)))])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test get-key-default-missing-test
    (assert-equal 0 (run-reader (reader-get-key-default 'x 0) '())))

  (define-test get-key-default-present-test
    (assert-equal 42 (run-reader (reader-get-key-default 'x 0) '((x . 42)))))

  (define-test require-key-present-test
    (assert-equal 42 (run-reader (reader-require-key 'x) '((x . 42)))))

  (define-test with-key-test
    (let ([rd (reader-with-key 'y 100 (reader-get-key 'y))])
      (let ([result (run-reader rd '((x . 1)))])
        (assert-true (just? result))
        (assert-equal 100 (from-just result)))))

  (define-test with-keys-test
    (let ([rd (reader-with-keys '((a . 1) (b . 2))
                                (reader-bind (reader-get-key-default 'a 0)
                                             (lambda (a)
                                               (reader-bind (reader-get-key-default 'b 0)
                                                            (lambda (b)
                                                              (reader-pure (+ a b)))))))])
      (assert-equal 3 (run-reader rd '())))))

;;; ============================================================
;;; Conditional Tests
;;; ============================================================

(test-group conditional-reader
  (define-test when-true-test
    (let ([counter 0])
      (run-reader (reader-when #t (reader-asks (lambda (env)
                                                 (set! counter (+ counter 1)))))
                  'env)
      (assert-equal 1 counter)))

  (define-test when-false-test
    (let ([counter 0])
      (run-reader (reader-when #f (reader-asks (lambda (env)
                                                  (set! counter (+ counter 1)))))
                  'env)
      (assert-equal 0 counter)))

  (define-test unless-true-test
    (let ([counter 0])
      (run-reader (reader-unless #t (reader-asks (lambda (env)
                                                   (set! counter (+ counter 1)))))
                  'env)
      (assert-equal 0 counter)))

  (define-test unless-false-test
    (let ([counter 0])
      (run-reader (reader-unless #f (reader-asks (lambda (env)
                                                    (set! counter (+ counter 1)))))
                  'env)
      (assert-equal 1 counter))))

;;; ============================================================
;;; Lifting Tests
;;; ============================================================

(test-group lifting
  (define-test lift-test
    (let* ([add10 (reader-lift (lambda (x) (+ x 10)))]
           [rd (add10 reader-ask)])
      (assert-equal 15 (run-reader rd 5))))

  (define-test lift2-test
    (let* ([add (reader-lift2 +)]
           [rd (add reader-ask (reader-asks (lambda (x) (+ x 10))))])
      (assert-equal 20 (run-reader rd 5))))  ; 5 + 15

  (define-test lift3-test
    (let* ([sum3 (reader-lift3 (lambda (a b c) (+ a b c)))]
           [rd (sum3 (reader-pure 1) (reader-pure 2) (reader-pure 3))])
      (assert-equal 6 (run-reader rd 'env)))))

;;; ============================================================
;;; Maybe and Either Integration Tests
;;; ============================================================

(test-group maybe-either-integration
  (define-test reader-maybe-nothing-test
    (let ([rd (reader-maybe (reader-pure nothing)
                            'default
                            (lambda (x) x))])
      (assert-equal 'default (run-reader rd 'env))))

  (define-test reader-maybe-just-test
    (let ([rd (reader-maybe (reader-pure (just 42))
                            'default
                            add1)])
      (assert-equal 43 (run-reader rd 'env))))

  (define-test reader-either-left-test
    (let ([rd (reader-either (reader-pure (left "error"))
                             (lambda (e) (string-append "Got: " e))
                             add1)])
      (assert-equal "Got: error" (run-reader rd 'env))))

  (define-test reader-either-right-test
    (let ([rd (reader-either (reader-pure (right 42))
                             (lambda (e) e)
                             add1)])
      (assert-equal 43 (run-reader rd 'env)))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
  ;; Dependency injection pattern
  (define-test dependency-injection-test
    (let* ([get-multiplier (reader-get-key-default 'multiplier 1)]
           [compute (reader-bind get-multiplier
                                 (lambda (mult)
                                   (reader-bind reader-ask
                                                (lambda (env)
                                                  (reader-pure
                                                   (+ mult (cdr (assoc 'value env))))))))]
           [env '((multiplier . 10) (value . 5))])
      (assert-equal 15 (run-reader compute env))))

  ;; URL building example
  (define-test url-building-test
    (let* ([get-host (reader-asks (lambda (env) (cdr (assoc 'host env))))]
           [get-port (reader-asks (lambda (env) (cdr (assoc 'port env))))]
           [make-url (reader-bind get-host
                                  (lambda (host)
                                    (reader-bind get-port
                                                 (lambda (port)
                                                   (reader-pure
                                                    (string-append host ":"
                                                                   (number->string port)))))))]
           [env '((host . "localhost") (port . 8080))])
      (assert-equal "localhost:8080" (run-reader make-url env))))

  ;; Local override example
  (define-test local-override-test
    (let* ([get-debug (reader-get-key-default 'debug #f)]
           [with-debug (lambda (rd)
                         (reader-local (lambda (env) (cons '(debug . #t) env)) rd))]
           [env '((other . value))])
      ;; Without local: debug is #f
      (assert-equal #f (run-reader get-debug env))
      ;; With local: debug is #t
      (assert-equal #t (run-reader (with-debug get-debug) env))))

  ;; Scope example
  (define-test scope-example-test
    (let* ([get-name (reader-asks (lambda (env) (cdr (assoc 'name env))))]
           [in-user-scope (reader-with-scope 'user get-name)]
           [env '((user . ((name . "Alice") (age . 30)))
                  (settings . ((theme . dark))))])
      (assert-equal "Alice" (run-reader in-user-scope env)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All reader monad tests passed.\n")
    (display "\n[FAILURE] Some reader monad tests failed.\n"))
