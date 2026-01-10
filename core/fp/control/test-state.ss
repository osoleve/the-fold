;;; fabric/stitches/fp/test-state.ss — Tests for State Monad

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/control/state.ss")

(display "
")
(display "==============================================================
")
(display "         STATE MONAD TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Core State Tests
;;; ============================================================

(test-group state-basics
            (define-test make-state-test
              (let ([st (make-state (lambda (s) (cons 42 s)))])
                   (assert-true (state? st))))
            
            (define-test run-state-test
              (let ([st (make-state (lambda (s) (cons (+ s 1) (+ s 10))))])
                   (let ([result (run-state st 5)])
                        (assert-equal 6 (car result))
                        (assert-equal 15 (cdr result)))))
            
            (define-test eval-state-test
              (let ([st (make-state (lambda (s) (cons (+ s 1) s)))])
                   (assert-equal 6 (eval-state st 5))))
            
            (define-test exec-state-test
              (let ([st (make-state (lambda (s) (cons 'ignored (+ s 100))))])
                   (assert-equal 105 (exec-state st 5)))))

;;; ============================================================
;;; Get/Put/Modify Tests
;;; ============================================================

(test-group state-operations
            (define-test get-test
              (assert-equal 42 (eval-state state-get 42))
              (assert-equal 42 (exec-state state-get 42)))
            
            (define-test put-test
              (let ([st (state-put 100)])
                   (assert-equal '() (eval-state st 0))
                   (assert-equal 100 (exec-state st 0))))
            
            (define-test modify-test
              (let ([st (state-modify add1)])
                   (assert-equal '() (eval-state st 5))
                   (assert-equal 6 (exec-state st 5))))
            
            (define-test gets-test
              (let ([st (state-gets (lambda (s) (+ s 10)))])
                   (assert-equal 15 (eval-state st 5))
                   (assert-equal 5 (exec-state st 5)))))

;;; ============================================================
;;; Monad Law Tests
;;; ============================================================

(test-group monad-laws
            ;; Left identity: pure a >>= f  ===  f a
            (define-test left-identity-test
              (let* ([a 5]
                     [f (lambda (x) (state-pure (+ x 10)))]
                     [lhs (state-bind (state-pure a) f)]
                     [rhs (f a)])
                    (assert-equal (run-state lhs 0) (run-state rhs 0))))
            
            ;; Right identity: m >>= pure  ===  m
            (define-test right-identity-test
              (let ([m (state-modify add1)])
                   (let ([lhs (state-bind m state-pure)]
                         [rhs m])
                        (assert-equal (run-state lhs 0) (run-state rhs 0)))))
            
            ;; Associativity: (m >>= f) >>= g  ===  m >>= (x -> f x >>= g)
            (define-test associativity-test
              (let ([m (state-pure 1)]
                    [f (lambda (x) (state-pure (+ x 10)))]
                    [g (lambda (x) (state-pure (+ x 100)))])
                   (let ([lhs (state-bind (state-bind m f) g)]
                         [rhs (state-bind m (lambda (x) (state-bind (f x) g)))])
                        (assert-equal (run-state lhs 0) (run-state rhs 0))))))

;;; ============================================================
;;; State-pure and State-bind Tests
;;; ============================================================

(test-group state-monad
            (define-test pure-test
              (assert-equal 42 (eval-state (state-pure 42) 'any-state))
              (assert-equal 'any-state (exec-state (state-pure 42) 'any-state)))
            
            (define-test bind-test
              (let ([st (state-bind state-get
                                    (lambda (s)
                                            (state-pure (+ s 10))))])
                   (assert-equal 15 (eval-state st 5))))
            
            (define-test bind-chain-test
              (let ([st (state-bind state-get
                                    (lambda (a)
                                            (state-bind (state-modify add1)
                                                        (lambda (_)
                                                                (state-bind state-get
                                                                            (lambda (b)
                                                                                    (state-pure (cons a b))))))))])
                   (assert-equal (cons 0 1) (eval-state st 0))))
            
            (define-test then-test
              (let ([st (state-then (state-put 10)
                                    state-get)])
                   (assert-equal 10 (eval-state st 0)))))

;;; ============================================================
;;; State-map and State-ap Tests
;;; ============================================================

(test-group state-functor-applicative
            (define-test map-test
              (let ([st (state-map add1 (state-pure 5))])
                   (assert-equal 6 (eval-state st 0))))
            
            (define-test map-with-state-test
              (let ([st (state-map add1 state-get)])
                   (assert-equal 6 (eval-state st 5))))
            
            (define-test ap-test
              (let ([st (state-ap (state-pure add1) (state-pure 5))])
                   (assert-equal 6 (eval-state st 0)))))

;;; ============================================================
;;; Sequence Tests
;;; ============================================================

(test-group state-sequence
            (define-test sequence-empty-test
              (assert-equal '() (eval-state (state-sequence '()) 0)))
            
            (define-test sequence-test
              (let ([actions (list (state-pure 1)
                                   (state-pure 2)
                                   (state-pure 3))])
                   (assert-equal '(1 2 3) (eval-state (state-sequence actions) 0))))
            
            (define-test sequence-with-state-test
              ;; Each action increments and returns old value
              (let ([actions (list state-inc state-inc state-inc)])
                   (let ([result (run-state (state-sequence actions) 0)])
                        (assert-equal '(0 1 2) (car result))
                        (assert-equal 3 (cdr result)))))
            
            (define-test map-m-test
              (let ([st (state-map-m (lambda (x) (state-pure (+ x 10)))
                                     '(1 2 3))])
                   (assert-equal '(11 12 13) (eval-state st 0)))))

;;; ============================================================
;;; Counter Tests
;;; ============================================================

(test-group counter-state
            (define-test inc-test
              (let ([result (run-state state-inc 5)])
                   (assert-equal 5 (car result))
                   (assert-equal 6 (cdr result))))
            
            (define-test dec-test
              (let ([result (run-state state-dec 5)])
                   (assert-equal 5 (car result))
                   (assert-equal 4 (cdr result))))
            
            (define-test add-test
              (assert-equal 15 (exec-state (state-add 10) 5)))
            
            (define-test fresh-test
              (let ([st (state-bind fresh
                                    (lambda (a)
                                            (state-bind fresh
                                                        (lambda (b)
                                                                (state-bind fresh
                                                                            (lambda (c)
                                                                                    (state-pure (list a b c))))))))])
                   (assert-equal '(0 1 2) (eval-state st 0))
                   (assert-equal 3 (exec-state st 0)))))

;;; ============================================================
;;; Stack Tests
;;; ============================================================

(test-group stack-state
            (define-test push-test
              (assert-equal '(1) (exec-state (state-push 1) '()))
              (assert-equal '(2 1) (exec-state (state-push 2) '(1))))
            
            (define-test pop-empty-test
              (let ([result (eval-state state-pop '())])
                   (assert-true (nothing? result))))
            
            (define-test pop-test
              (let ([result (run-state state-pop '(1 2 3))])
                   (assert-true (just? (car result)))
                   (assert-equal 1 (from-just (car result)))
                   (assert-equal '(2 3) (cdr result))))
            
            (define-test peek-empty-test
              (let ([result (eval-state state-peek '())])
                   (assert-true (nothing? result))))
            
            (define-test peek-test
              (let ([result (run-state state-peek '(1 2 3))])
                   (assert-true (just? (car result)))
                   (assert-equal 1 (from-just (car result)))
                   (assert-equal '(1 2 3) (cdr result))))
            
            (define-test stack-ops-test
              (let ([st (state-then (state-push 1)
                                    (state-then (state-push 2)
                                                (state-then (state-push 3)
                                                            state-pop)))])
                   (let ([result (run-state st '())])
                        (assert-true (just? (car result)))
                        (assert-equal 3 (from-just (car result)))
                        (assert-equal '(2 1) (cdr result))))))

;;; ============================================================
;;; Accumulator Tests
;;; ============================================================

(test-group accumulator-state
            (define-test tell-test
              (let ([st (state-then (state-tell "hello")
                                    state-listen)])
                   (assert-equal '("hello") (eval-state st '()))))
            
            (define-test tell-multiple-test
              (let ([st (state-then (state-tell "a")
                                    (state-then (state-tell "b")
                                                (state-then (state-tell "c")
                                                            state-listen)))])
                   (assert-equal '("a" "b" "c") (eval-state st '()))))
            
            (define-test tell-all-test
              (let ([st (state-then (state-tell-all '(1 2 3))
                                    state-listen)])
                   (assert-equal '(1 2 3) (eval-state st '()))))
            
            (define-test listen-test
              (let ([st (state-then (state-tell "x")
                                    state-listen)])
                   (assert-equal '("x") (eval-state st '()))))
            
            (define-test clear-test
              (let ([st (state-then (state-tell "x")
                                    (state-then (state-tell "y")
                                                state-clear))])
                   (let ([result (run-state st '())])
                        (assert-equal '("x" "y") (car result))
                        (assert-equal '() (cdr result))))))

;;; ============================================================
;;; Conditional Tests
;;; ============================================================

(test-group conditional-state
            (define-test when-true-test
              (assert-equal 6 (exec-state (state-when #t (state-modify add1)) 5)))
            
            (define-test when-false-test
              (assert-equal 5 (exec-state (state-when #f (state-modify add1)) 5)))
            
            (define-test unless-true-test
              (assert-equal 5 (exec-state (state-unless #t (state-modify add1)) 5)))
            
            (define-test unless-false-test
              (assert-equal 6 (exec-state (state-unless #f (state-modify add1)) 5))))

;;; ============================================================
;;; Alist State Tests
;;; ============================================================

(test-group alist-state
            (define-test get-key-missing-test
              (let ([result (eval-state (state-get-key 'x) '())])
                   (assert-true (nothing? result))))
            
            (define-test get-key-present-test
              (let ([result (eval-state (state-get-key 'x) '((x . 42)))])
                   (assert-true (just? result))
                   (assert-equal 42 (from-just result))))
            
            (define-test put-key-new-test
              (let ([result (exec-state (state-put-key 'x 10) '())])
                   (assert-equal '((x . 10)) result)))
            
            (define-test put-key-update-test
              (let ([result (exec-state (state-put-key 'x 20) '((x . 10) (y . 5)))])
                   (assert-equal 20 (cdr (assoc 'x result)))
                   (assert-equal 5 (cdr (assoc 'y result)))))
            
            (define-test modify-key-existing-test
              (let ([result (exec-state (state-modify-key 'x add1 0) '((x . 5)))])
                   (assert-equal 6 (cdr (assoc 'x result)))))
            
            (define-test modify-key-new-test
              (let ([result (exec-state (state-modify-key 'x add1 0) '())])
                   (assert-equal 1 (cdr (assoc 'x result))))))

;;; ============================================================
;;; Practical Example Tests
;;; ============================================================

(test-group practical-examples
            ;; Simple computation using counter state
            (define-test counter-computation-test
              (let ([st (state-bind fresh
                                    (lambda (a)
                                            (state-bind fresh
                                                        (lambda (b)
                                                                (state-bind fresh
                                                                            (lambda (c)
                                                                                    (state-pure (+ a b c))))))))])
                   ;; 0 + 1 + 2 = 3
                   (assert-equal 3 (eval-state st 0))
                   (assert-equal 3 (exec-state st 0))))
            
            ;; Tree labeling with fresh
            (define-test tree-labeling-test
              (let ([st (state-bind fresh
                                    (lambda (a)
                                            (state-bind fresh
                                                        (lambda (b)
                                                                (state-pure (list 'node a b))))))])
                   (assert-equal '(node 0 1) (eval-state st 0)))))

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
[SUCCESS] All state monad tests passed.
")
    (display "
[FAILURE] Some state monad tests failed.
"))
