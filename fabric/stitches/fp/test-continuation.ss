;;; fabric/stitches/fp/test-continuation.ss — Tests for Continuation Monad

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/continuation.ss")

(display "
")
(display "==============================================================
")
(display "         CONTINUATION MONAD TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic Cont Tests
;;; ============================================================

(test-group cont-basic
            (define-test make-cont-test
              (let ([c (make-cont (lambda (k) (k 42)))])
                   (assert-true (cont? c))))
            
            (define-test run-cont-test
              (let ([c (make-cont (lambda (k) (k 42)))])
                   (assert-equal 42 (run-cont c identity))))
            
            (define-test eval-cont-test
              (let ([c (make-cont (lambda (k) (k 42)))])
                   (assert-equal 42 (eval-cont c))))
            
            (define-test cont-return-test
              (assert-equal 42 (eval-cont (cont-return 42)))
              (assert-equal "hello" (eval-cont (cont-return "hello")))))

;;; ============================================================
;;; Monad Law Tests
;;; ============================================================

(test-group monad-laws
            ;; Left identity: return a >>= f = f a
            (define-test left-identity-test
              (let* ([f (lambda (x) (cont-return (* x 2)))]
                     [lhs (cont-bind (cont-return 5) f)]
                     [rhs (f 5)])
                    (assert-equal (eval-cont lhs) (eval-cont rhs))))
            
            ;; Right identity: m >>= return = m
            (define-test right-identity-test
              (let* ([m (cont-return 42)]
                     [result (cont-bind m cont-return)])
                    (assert-equal (eval-cont m) (eval-cont result))))
            
            ;; Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
            (define-test associativity-test
              (let* ([m (cont-return 2)]
                     [f (lambda (x) (cont-return (* x 3)))]
                     [g (lambda (x) (cont-return (+ x 1)))]
                     [lhs (cont-bind (cont-bind m f) g)]
                     [rhs (cont-bind m (lambda (x) (cont-bind (f x) g)))])
                    (assert-equal (eval-cont lhs) (eval-cont rhs)))))

;;; ============================================================
;;; Functor/Applicative Tests
;;; ============================================================

(test-group functor-applicative
            (define-test cont-map-test
              (let ([result (eval-cont (cont-map add1 (cont-return 41)))])
                   (assert-equal 42 result)))
            
            (define-test cont-map-compose-test
              (let* ([double (lambda (x) (* x 2))]
                     [result (eval-cont
                              (cont-map add1 (cont-map double (cont-return 20))))])
                    (assert-equal 41 result)))
            
            (define-test cont-ap-test
              (let ([result (eval-cont
                             (cont-ap (cont-return add1)
                                      (cont-return 41)))])
                   (assert-equal 42 result)))
            
            (define-test cont-join-test
              (let* ([inner (cont-return 42)]
                     [outer (cont-return inner)]
                     [result (eval-cont (cont-join outer))])
                    (assert-equal 42 result))))

;;; ============================================================
;;; callCC Tests
;;; ============================================================

(test-group call-cc
            (define-test callcc-no-escape-test
              ;; If we don't use the escape, normal flow continues
              (let ([result (eval-cont
                             (callCC (lambda (exit)
                                             (cont-return 42))))])
                   (assert-equal 42 result)))
            
            (define-test callcc-immediate-escape-test
              ;; Using escape immediately
              (let ([result (eval-cont
                             (callCC (lambda (exit)
                                             (exit 42))))])
                   (assert-equal 42 result)))
            
            (define-test callcc-escape-from-bind-test
              ;; Escape from within a bind
              (let ([result (eval-cont
                             (callCC (lambda (exit)
                                             (cont-bind (exit 42)
                                                        (lambda (x) (cont-return (* x 2)))))))])
                   (assert-equal 42 result)))
            
            (define-test callcc-conditional-escape-test
              (let ([try-find (lambda (pred lst)
                                      (callCC (lambda (found)
                                                      (cont-fold (lambda (acc x)
                                                                         (if (pred x)
                                                                             (found (just x))
                                                                             (cont-return acc)))
                                                                 nothing
                                                                 lst))))])
                   (let ([result (eval-cont (try-find even? '(1 3 5 6 7)))])
                        (assert-true (just? result))
                        (assert-equal 6 (from-just result)))
                   (let ([result (eval-cont (try-find even? '(1 3 5 7)))])
                        (assert-true (nothing? result))))))

;;; ============================================================
;;; Early Return Pattern Tests
;;; ============================================================

(test-group early-return
            (define-test with-early-return-no-return-test
              (let ([result (with-early-return
                             (lambda (return)
                                     (cont-return 42)))])
                   (assert-equal 42 result)))
            
            (define-test with-early-return-immediate-test
              (let ([result (with-early-return
                             (lambda (return)
                                     (return 10)))])
                   (assert-equal 10 result)))
            
            (define-test with-early-return-in-loop-test
              (let ([result (with-early-return
                             (lambda (return)
                                     (cont-fold (lambda (acc x)
                                                        (if (= x 0)
                                                            (return acc)
                                                            (cont-return (+ acc x))))
                                                0
                                                '(1 2 3 0 4 5))))])
                   (assert-equal 6 result)))  ; 1+2+3 = 6, stops at 0
            
            (define-test with-early-return-nested-test
              (let ([result (with-early-return
                             (lambda (outer-return)
                                     (cont-bind (cont-return 10)
                                                (lambda (x)
                                                        (if (> x 5)
                                                            (outer-return (* x 2))
                                                            (cont-return x))))))])
                   (assert-equal 20 result))))

;;; ============================================================
;;; Loop Control Tests
;;; ============================================================

(test-group loop-control
            (define-test cont-loop-immediate-exit-test
              (let ([result (eval-cont
                             (cont-loop 0
                                        (lambda (n) (cont-return (right n)))))])
                   (assert-equal 0 result)))
            
            (define-test cont-loop-count-to-five-test
              (let ([result (eval-cont
                             (cont-loop 0
                                        (lambda (n)
                                                (if (>= n 5)
                                                    (cont-return (right n))
                                                    (cont-return (left (+ n 1)))))))])
                   (assert-equal 5 result)))
            
            (define-test cont-for-each-test
              (let ([sum 0])
                   (eval-cont
                    (cont-for-each (lambda (x)
                                           (set! sum (+ sum x))
                                           (cont-return '()))
                                   '(1 2 3 4 5)))
                   (assert-equal 15 sum)))
            
            (define-test cont-fold-test
              (let ([result (eval-cont
                             (cont-fold (lambda (acc x) (cont-return (+ acc x)))
                                        0
                                        '(1 2 3 4 5)))])
                   (assert-equal 15 result))))

;;; ============================================================
;;; Conditional Tests
;;; ============================================================

(test-group conditionals
            ;; Test cont-when returns the action result when true
            (define-test cont-when-true-test
              (let ([result (eval-cont (cont-when #t (cont-return 'executed)))])
                   (assert-equal 'executed result)))
            
            ;; Test cont-when returns unit when false (action not executed)
            (define-test cont-when-false-test
              (let ([result (eval-cont (cont-when #f (cont-return 'executed)))])
                   (assert-equal '() result)))  ; Returns unit, not 'executed
            
            ;; Test cont-unless returns unit when true (action not executed)
            (define-test cont-unless-true-test
              (let ([result (eval-cont (cont-unless #t (cont-return 'executed)))])
                   (assert-equal '() result)))
            
            ;; Test cont-unless returns action result when false
            (define-test cont-unless-false-test
              (let ([result (eval-cont (cont-unless #f (cont-return 'executed)))])
                   (assert-equal 'executed result))))

;;; ============================================================
;;; Trampoline Tests
;;; ============================================================

(test-group trampoline
            (define-test done-test
              (let ([t (make-done 42)])
                   (assert-true (done? t))
                   (assert-false (bounce? t))
                   (assert-equal 42 (done-value t))))
            
            (define-test bounce-test
              (let ([t (make-bounce (lambda () (make-done 42)))])
                   (assert-true (bounce? t))
                   (assert-false (done? t))))
            
            (define-test trampoline-done-test
              (assert-equal 42 (trampoline (make-done 42))))
            
            (define-test trampoline-bounce-test
              (assert-equal 42 (trampoline (make-bounce (lambda () (make-done 42))))))
            
            (define-test trampoline-multi-bounce-test
              (let ([t (make-bounce
                        (lambda ()
                                (make-bounce
                                 (lambda ()
                                         (make-bounce
                                          (lambda () (make-done 42)))))))])
                   (assert-equal 42 (trampoline t))))
            
            ;; Trampolined factorial
            (define (tramp-fact n acc)
              (if (= n 0)
                  (make-done acc)
                  (make-bounce (lambda () (tramp-fact (- n 1) (* n acc))))))
            
            (define-test trampoline-factorial-test
              (assert-equal 1 (trampoline (tramp-fact 0 1)))
              (assert-equal 1 (trampoline (tramp-fact 1 1)))
              (assert-equal 120 (trampoline (tramp-fact 5 1)))
              (assert-equal 720 (trampoline (tramp-fact 6 1)))))

;;; ============================================================
;;; Sequence and Map Tests
;;; ============================================================

(test-group sequence-map
            (define-test sequence-cont-empty-test
              (let ([result (eval-cont (sequence-cont '()))])
                   (assert-equal '() result)))
            
            (define-test sequence-cont-nonempty-test
              (let ([result (eval-cont (sequence-cont
                                        (list (cont-return 1)
                                              (cont-return 2)
                                              (cont-return 3))))])
                   (assert-equal '(1 2 3) result)))
            
            (define-test map-cont-test
              (let ([result (eval-cont
                             (map-cont (lambda (x) (cont-return (* x 2)))
                                       '(1 2 3 4 5)))])
                   (assert-equal '(2 4 6 8 10) result)))
            
            (define-test filter-cont-test
              (let ([result (eval-cont
                             (filter-cont (lambda (x) (cont-return (even? x)))
                                          '(1 2 3 4 5 6)))])
                   (assert-equal '(2 4 6) result))))

;;; ============================================================
;;; ContT Transformer Tests
;;; ============================================================

(test-group cont-t
            (define-test make-cont-t-test
              (let ([ct (make-cont-t (lambda (k) (k 42)))])
                   (assert-true (cont-t? ct))))
            
            (define-test cont-t-return-test
              (let ([ct (cont-t-return 42)])
                   (assert-equal 42 (run-cont-t ct identity))))
            
            (define-test cont-t-bind-test
              (let* ([ct1 (cont-t-return 21)]
                     [ct2 (cont-t-bind ct1 (lambda (x) (cont-t-return (* x 2))))])
                    (assert-equal 42 (run-cont-t ct2 identity)))))

;;; ============================================================
;;; Continuation Operators Tests
;;; ============================================================

(test-group operators
            (define-test cont>>-test
              (let ([side-effect #f])
                   (let ([result (eval-cont
                                  (cont>> (begin (set! side-effect #t)
                                                 (cont-return 'ignored))
                                          (cont-return 42)))])
                        (assert-true side-effect)
                        (assert-equal 42 result))))
            
            (define-test cont-void-test
              (assert-equal '() (eval-cont cont-void))))

;;; ============================================================
;;; Shift/Reset Tests
;;; ============================================================

(test-group shift-reset
            (define-test reset-identity-test
              (let ([result (eval-cont (cont-reset (cont-return 42)))])
                   (assert-equal 42 result)))
            
            (define-test reset-with-computation-test
              (let ([result (eval-cont
                             (cont-reset
                              (cont-bind (cont-return 21)
                                         (lambda (x) (cont-return (* x 2))))))])
                   (assert-equal 42 result))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
            ;; Find first element matching predicate using fold with early exit
            (define (find-first pred lst)
              (with-early-return
               (lambda (found)
                       (cont-fold (lambda (acc x)
                                          (if (pred x)
                                              (found (just x))
                                              (cont-return acc)))
                                  nothing
                                  lst))))
            
            (define-test find-first-found-test
              (let ([result (find-first (lambda (x) (> x 3)) '(1 2 3 4 5))])
                   (assert-true (just? result))
                   (assert-equal 4 (from-just result))))
            
            (define-test find-first-not-found-test
              (let ([result (find-first (lambda (x) (> x 10)) '(1 2 3 4 5))])
                   (assert-true (nothing? result))))
            
            ;; Sum until negative
            (define (sum-until-negative lst)
              (with-early-return
               (lambda (stop)
                       (cont-fold (lambda (acc x)
                                          (if (< x 0)
                                              (stop acc)
                                              (cont-return (+ acc x))))
                                  0
                                  lst))))
            
            (define-test sum-until-negative-test
              (assert-equal 6 (sum-until-negative '(1 2 3 -1 10 20)))
              (assert-equal 15 (sum-until-negative '(1 2 3 4 5)))
              (assert-equal 0 (sum-until-negative '(-1 2 3))))
            
            ;; Validate all with early failure using fold
            (define (validate-all pred lst)
              (with-early-return
               (lambda (fail)
                       (cont-fold (lambda (acc x)
                                          (if (pred x)
                                              (cont-return #t)
                                              (fail #f)))
                                  #t
                                  lst))))
            
            (define-test validate-all-pass-test
              (assert-true (validate-all positive? '(1 2 3 4 5))))
            
            (define-test validate-all-fail-test
              (assert-false (validate-all positive? '(1 2 -3 4 5)))))

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
[SUCCESS] All continuation monad tests passed.
")
    (display "
[FAILURE] Some continuation monad tests failed.
"))
