;;; fabric/stitches/fp/test-transformers.ss — Tests for Monad Transformers

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/control/transformers.ss")

(display "
")
(display "==============================================================
")
(display "         MONAD TRANSFORMERS TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Identity Monad Tests
;;; ============================================================

(test-group identity-monad
            (define-test identity-pure-test
              (assert-equal 42 (identity-pure 42)))
            
            (define-test identity-bind-test
              (assert-equal 52 (identity-bind 42 (lambda (x) (+ x 10)))))
            
            (define-test identity-map-test
              (assert-equal 43 (identity-map add1 42))))

;;; ============================================================
;;; MaybeT Tests
;;; ============================================================

(test-group maybe-t-basics
            (define-test make-maybe-t-test
              (let ([mt (make-maybe-t (just 42))])
                   (assert-true (maybe-t? mt))))
            
            (define-test run-maybe-t-test
              (let ([mt (make-maybe-t (just 42))])
                   (assert-equal (just 42) (run-maybe-t mt))))
            
            (define-test maybe-t-pure-test
              (let ([mt (maybe-t-pure identity-pure 42)])
                   (let ([result (run-maybe-t mt)])
                        (assert-true (just? result))
                        (assert-equal 42 (from-just result)))))
            
            (define-test maybe-t-fail-test
              (let ([mt (maybe-t-fail identity-pure)])
                   (assert-true (nothing? (run-maybe-t mt))))))

;;; ============================================================
;;; EitherT Tests
;;; ============================================================

(test-group either-t-basics
            (define-test make-either-t-test
              (let ([et (make-either-t (right 42))])
                   (assert-true (either-t? et))))
            
            (define-test run-either-t-test
              (let ([et (make-either-t (right 42))])
                   (assert-equal (right 42) (run-either-t et))))
            
            (define-test either-t-pure-test
              (let ([et (either-t-pure identity-pure 42)])
                   (let ([result (run-either-t et)])
                        (assert-true (right? result))
                        (assert-equal 42 (from-right result)))))
            
            (define-test either-t-throw-test
              (let ([et (either-t-throw identity-pure "error")])
                   (let ([result (run-either-t et)])
                        (assert-true (left? result))
                        (assert-equal "error" (from-left result)))))
            
            (define-test either-t-bind-success-test
              (let* ([et1 (either-t-pure identity-pure 5)]
                     [et2 (either-t-bind identity-bind identity-pure et1
                                         (lambda (x)
                                                 (either-t-pure identity-pure (* x 2))))])
                    (let ([result (run-either-t et2)])
                         (assert-true (right? result))
                         (assert-equal 10 (from-right result)))))
            
            (define-test either-t-bind-failure-test
              (let* ([et1 (either-t-throw identity-pure "error")]
                     [et2 (either-t-bind identity-bind identity-pure et1
                                         (lambda (x)
                                                 (either-t-pure identity-pure (* x 2))))])
                    (let ([result (run-either-t et2)])
                         (assert-true (left? result))
                         (assert-equal "error" (from-left result)))))
            
            (define-test either-t-catch-test
              (let* ([et1 (either-t-throw identity-pure "error")]
                     [et2 (either-t-catch identity-bind identity-pure et1
                                          (lambda (e)
                                                  (either-t-pure identity-pure (string-append "recovered: " e))))])
                    (let ([result (run-either-t et2)])
                         (assert-true (right? result))
                         (assert-equal "recovered: error" (from-right result))))))

;;; ============================================================
;;; ReaderT Tests
;;; ============================================================

(test-group reader-t-basics
            (define-test make-reader-t-test
              (let ([rt (make-reader-t (lambda (env) 42))])
                   (assert-true (reader-t? rt))))
            
            (define-test run-reader-t-test
              (let ([rt (make-reader-t (lambda (env) (+ env 10)))])
                   (assert-equal 15 (run-reader-t rt 5))))
            
            (define-test reader-t-pure-test
              (let ([rt (reader-t-pure identity-pure 42)])
                   (assert-equal 42 (run-reader-t rt 'any-env))))
            
            (define-test reader-t-ask-test
              (let ([rt (reader-t-ask identity-pure)])
                   (assert-equal 5 (run-reader-t rt 5))))
            
            (define-test reader-t-asks-test
              (let ([rt (reader-t-asks identity-pure add1)])
                   (assert-equal 6 (run-reader-t rt 5))))
            
            (define-test reader-t-bind-test
              (let* ([rt1 (reader-t-ask identity-pure)]
                     [rt2 (reader-t-bind identity-bind rt1
                                         (lambda (x)
                                                 (reader-t-pure identity-pure (* x 2))))])
                    (assert-equal 10 (run-reader-t rt2 5))))
            
            (define-test reader-t-local-test
              (let* ([rt (reader-t-ask identity-pure)]
                     [rt-local (reader-t-local add1 rt)])
                    (assert-equal 6 (run-reader-t rt-local 5))))
            
            (define-test reader-t-lift-test
              (let ([rt (reader-t-lift (just 42))])
                   (let ([result (run-reader-t rt 'any-env)])
                        (assert-true (just? result))
                        (assert-equal 42 (from-just result))))))

;;; ============================================================
;;; StateT Tests
;;; ============================================================

(test-group state-t-basics
            (define-test make-state-t-test
              (let ([st (make-state-t (lambda (s) (cons 42 s)))])
                   (assert-true (state-t? st))))
            
            (define-test run-state-t-test
              (let ([st (make-state-t (lambda (s) (cons (+ s 1) (+ s 10))))])
                   (let ([result (run-state-t st 5)])
                        (assert-equal 6 (car result))
                        (assert-equal 15 (cdr result)))))
            
            (define-test state-t-pure-test
              (let ([st (state-t-pure identity-pure 42)])
                   (let ([result (run-state-t st 0)])
                        (assert-equal 42 (car result))
                        (assert-equal 0 (cdr result)))))
            
            (define-test state-t-get-test
              (let ([st (state-t-get identity-pure)])
                   (let ([result (run-state-t st 5)])
                        (assert-equal 5 (car result))
                        (assert-equal 5 (cdr result)))))
            
            (define-test state-t-put-test
              (let ([st (state-t-put identity-pure 100)])
                   (let ([result (run-state-t st 0)])
                        (assert-equal '() (car result))
                        (assert-equal 100 (cdr result)))))
            
            (define-test state-t-modify-test
              (let ([st (state-t-modify identity-pure add1)])
                   (let ([result (run-state-t st 5)])
                        (assert-equal '() (car result))
                        (assert-equal 6 (cdr result)))))
            
            (define-test state-t-gets-test
              (let ([st (state-t-gets identity-pure (lambda (s) (+ s 10)))])
                   (let ([result (run-state-t st 5)])
                        (assert-equal 15 (car result))
                        (assert-equal 5 (cdr result)))))
            
            (define-test state-t-bind-test
              (let* ([st1 (state-t-get identity-pure)]
                     [st2 (state-t-bind identity-bind st1
                                        (lambda (x)
                                                (state-t-bind identity-bind
                                                              (state-t-put identity-pure (+ x 10))
                                                              (lambda (_)
                                                                      (state-t-pure identity-pure x)))))])
                    (let ([result (run-state-t st2 5)])
                         (assert-equal 5 (car result))
                         (assert-equal 15 (cdr result))))))

;;; ============================================================
;;; WriterT Tests
;;; ============================================================

(test-group writer-t-basics
            (define-test make-writer-t-test
              (let ([wt (make-writer-t transformer-list-monoid (cons 42 '()))])
                   (assert-true (writer-t? wt))))
            
            (define-test run-writer-t-test
              (let ([wt (make-writer-t transformer-list-monoid (cons 42 '("log")))])
                   (let ([result (run-writer-t wt)])
                        (assert-equal 42 (car result))
                        (assert-equal '("log") (cdr result)))))
            
            (define-test writer-t-pure-test
              (let ([wt (writer-t-pure identity-pure transformer-list-monoid 42)])
                   (let ([result (run-writer-t wt)])
                        (assert-equal 42 (car result))
                        (assert-equal '() (cdr result)))))
            
            (define-test writer-t-tell-test
              (let ([wt (writer-t-tell identity-pure transformer-list-monoid '("message"))])
                   (let ([result (run-writer-t wt)])
                        (assert-equal '() (car result))
                        (assert-equal '("message") (cdr result))))))

;;; ============================================================
;;; ReaderT with Either Tests
;;; ============================================================

(test-group reader-either-stack
            (define-test reader-either-pure-test
              (let ([rt (reader-either-pure 42)])
                   (let ([result (run-reader-t rt 'env)])
                        (assert-true (right? result))
                        (assert-equal 42 (from-right result)))))
            
            (define-test reader-either-ask-test
              (let ([result (run-reader-t reader-either-ask 5)])
                   (assert-true (right? result))
                   (assert-equal 5 (from-right result))))
            
            (define-test reader-either-throw-test
              (let ([rt (reader-either-throw "error")])
                   (let ([result (run-reader-t rt 'env)])
                        (assert-true (left? result))
                        (assert-equal "error" (from-left result)))))
            
            (define-test reader-either-bind-test
              (let* ([rt (reader-either-bind reader-either-ask
                                             (lambda (x)
                                                     (if (> x 0)
                                                         (reader-either-pure (* x 2))
                                                         (reader-either-throw "must be positive"))))])
                    (let ([success (run-reader-t rt 5)]
                          [failure (run-reader-t rt -1)])
                         (assert-true (right? success))
                         (assert-equal 10 (from-right success))
                         (assert-true (left? failure))
                         (assert-equal "must be positive" (from-left failure))))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
            ;; Config validation with ReaderT Either
            (define-test config-validation-test
              (let* ([get-port (lambda (env) (cdr (assoc 'port env)))]
                     [validate (reader-either-bind reader-either-ask
                                                   (lambda (env)
                                                           (let ([port (get-port env)])
                                                                (if (and (number? port) (> port 0) (< port 65536))
                                                                    (reader-either-pure port)
                                                                    (reader-either-throw "Invalid port")))))])
                    (let ([good (run-reader-t validate '((port . 8080)))]
                          [bad (run-reader-t validate '((port . -1)))])
                         (assert-true (right? good))
                         (assert-equal 8080 (from-right good))
                         (assert-true (left? bad)))))
            
            ;; State with failure using StateT Maybe pattern
            (define-test stateful-lookup-test
              (let* ([lookup-st (make-state-t
                                 (lambda (alist)
                                         (let ([pair (assoc 'x alist)])
                                              (if pair
                                                  (just (cons (cdr pair) alist))
                                                  nothing))))])
                    (let ([found (run-state-t lookup-st '((x . 42) (y . 1)))]
                          [not-found (run-state-t lookup-st '((y . 1)))])
                         (assert-true (just? found))
                         (assert-equal 42 (car (from-just found)))
                         (assert-true (nothing? not-found))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All transformer tests passed.
")
    (display "
[FAILURE] Some transformer tests failed.
"))
