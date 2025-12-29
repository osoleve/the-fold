;;; fabric/stitches/fp/test-effects.ss — Tests for Algebraic Effects

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/effects.ss")

(display "
")
(display "==============================================================
")
(display "         ALGEBRAIC EFFECTS TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Eff Structure Tests
;;; ============================================================

(test-group eff-structure
            (define-test eff-pure-test
              (let ([e (make-eff-pure 42)])
                   (assert-true (eff-pure? e))
                   (assert-false (eff-op? e))
                   (assert-equal 42 (eff-pure-value e))))
            
            (define-test eff-op-test
              (let ([e (make-eff-op (make-effect 'test 'payload) identity)])
                   (assert-true (eff-op? e))
                   (assert-false (eff-pure? e))))
            
            (define-test eff-return-test
              (let ([e (eff-return 42)])
                   (assert-true (eff-pure? e))
                   (assert-equal 42 (eff-pure-value e)))))

;;; ============================================================
;;; Effect Structure Tests
;;; ============================================================

(test-group effect-structure
            (define-test make-effect-test
              (let ([e (make-effect 'my-tag 'my-payload)])
                   (assert-true (effect? e))
                   (assert-equal 'my-tag (effect-tag e))
                   (assert-equal 'my-payload (effect-payload e)))))

;;; ============================================================
;;; Eff Monad Laws
;;; ============================================================

(test-group eff-monad-laws
            ;; Left identity: return a >>= f = f a
            (define-test left-identity-test
              (let* ([f (lambda (x) (eff-return (* x 2)))]
                     [lhs (run-state 0 (eff-bind (eff-return 5) f))]
                     [rhs (run-state 0 (f 5))])
                    (assert-equal (car lhs) (car rhs))))
            
            ;; Right identity: m >>= return = m
            (define-test right-identity-test
              (let* ([m (eff-return 42)]
                     [lhs (run-state 0 m)]
                     [rhs (run-state 0 (eff-bind m eff-return))])
                    (assert-equal (car lhs) (car rhs))))
            
            ;; Associativity
            (define-test associativity-test
              (let* ([m (eff-return 2)]
                     [f (lambda (x) (eff-return (* x 3)))]
                     [g (lambda (x) (eff-return (+ x 1)))]
                     [lhs (run-state 0 (eff-bind (eff-bind m f) g))]
                     [rhs (run-state 0 (eff-bind m (lambda (x) (eff-bind (f x) g))))])
                    (assert-equal (car lhs) (car rhs)))))

;;; ============================================================
;;; State Effect Tests
;;; ============================================================

(test-group state-effect
            (define-test state-get-test
              (let ([result (run-state 42 state-get)])
                   (assert-equal 42 (car result))
                   (assert-equal 42 (cdr result))))
            
            (define-test state-put-test
              (let ([result (run-state 0 (state-put 100))])
                   (assert-equal '() (car result))
                   (assert-equal 100 (cdr result))))
            
            (define-test state-modify-test
              (let ([result (run-state 10 (state-modify add1))])
                   (assert-equal '() (car result))
                   (assert-equal 11 (cdr result))))
            
            (define-test state-sequence-test
              (let ([result (run-state 0
                                       (eff-bind state-get
                                                 (lambda (n)
                                                         (eff-bind (state-put (+ n 10))
                                                                   (lambda (_)
                                                                           (eff-bind state-get
                                                                                     (lambda (m)
                                                                                             (eff-return (* m 2)))))))))])
                   (assert-equal 20 (car result))
                   (assert-equal 10 (cdr result))))
            
            (define-test counter-increment-test
              (let ([result (run-state 5
                                       (eff-bind counter-increment
                                                 (lambda (_)
                                                         (eff-bind counter-increment
                                                                   (lambda (_)
                                                                           state-get)))))])
                   (assert-equal 7 (car result))
                   (assert-equal 7 (cdr result)))))

;;; ============================================================
;;; Reader Effect Tests
;;; ============================================================

(test-group reader-effect
            (define-test reader-ask-test
              (let ([result (run-reader "environment" reader-ask)])
                   (assert-equal "environment" result)))
            
            (define-test reader-in-bind-test
              (let ([result (run-reader 10
                                        (eff-bind reader-ask
                                                  (lambda (x)
                                                          (eff-return (* x 2)))))])
                   (assert-equal 20 result)))
            
            (define-test reader-multiple-asks-test
              (let ([result (run-reader 5
                                        (eff-bind reader-ask
                                                  (lambda (x)
                                                          (eff-bind reader-ask
                                                                    (lambda (y)
                                                                            (eff-return (+ x y)))))))])
                   (assert-equal 10 result))))

;;; ============================================================
;;; Writer Effect Tests
;;; ============================================================

(test-group writer-effect
            (define-test writer-tell-test
              (let ([result (run-writer (writer-tell "hello"))])
                   (assert-equal '() (car result))
                   (assert-equal '("hello") (cdr result))))
            
            (define-test writer-multiple-tells-test
              (let ([result (run-writer
                             (eff-bind (writer-tell "first")
                                       (lambda (_)
                                               (eff-bind (writer-tell "second")
                                                         (lambda (_)
                                                                 (eff-return 42))))))])
                   (assert-equal 42 (car result))
                   (assert-equal '("first" "second") (cdr result))))
            
            (define-test log-info-test
              (let ([result (run-writer (log-info "test message"))])
                   (assert-equal '() (car result))
                   (assert-equal '((info "test message")) (cdr result))))
            
            (define-test multiple-log-levels-test
              (let ([result (run-writer
                             (eff-bind (log-info "info")
                                       (lambda (_)
                                               (eff-bind (log-warn "warning")
                                                         (lambda (_)
                                                                 (log-error "error"))))))])
                   (assert-equal 3 (length (cdr result))))))

;;; ============================================================
;;; Exception Effect Tests
;;; ============================================================

(test-group exception-effect
            (define-test throw-test
              (let ([result (run-exception (eff-throw "error!"))])
                   (assert-true (left? result))
                   (assert-equal "error!" (from-left result))))
            
            (define-test no-throw-test
              (let ([result (run-exception (eff-return 42))])
                   (assert-true (right? result))
                   (assert-equal 42 (from-right result))))
            
            (define-test catch-thrown-test
              (let ([result (run-exception
                             (eff-catch (eff-throw "oops")
                                        (lambda (e)
                                                (eff-return (string-append "caught: " e)))))])
                   (assert-true (right? result))
                   (assert-equal "caught: oops" (from-right result))))
            
            (define-test catch-not-thrown-test
              (let ([result (run-exception
                             (eff-catch (eff-return 42)
                                        (lambda (e) (eff-return -1))))])
                   (assert-true (right? result))
                   (assert-equal 42 (from-right result))))
            
            (define-test throw-in-bind-test
              (let ([result (run-exception
                             (eff-bind (eff-return 10)
                                       (lambda (x)
                                               (if (> x 5)
                                                   (eff-throw "too big")
                                                   (eff-return x)))))])
                   (assert-true (left? result))
                   (assert-equal "too big" (from-left result)))))

;;; ============================================================
;;; NonDet Effect Tests
;;; ============================================================

(test-group nondet-effect
            (define-test nondet-choose-single-test
              (let ([result (run-nondet (nondet-choose '(42)))])
                   (assert-equal '(42) result)))
            
            (define-test nondet-choose-multiple-test
              (let ([result (run-nondet (nondet-choose '(1 2 3)))])
                   (assert-equal '(1 2 3) result)))
            
            (define-test nondet-fail-test
              (let ([result (run-nondet nondet-fail)])
                   (assert-equal '() result)))
            
            (define-test nondet-bind-test
              (let ([result (run-nondet
                             (eff-bind (nondet-choose '(1 2))
                                       (lambda (x)
                                               (nondet-choose (list x (* x 10))))))])
                   (assert-equal '(1 10 2 20) result)))
            
            (define-test nondet-cartesian-test
              (let ([result (run-nondet
                             (eff-bind (nondet-choose '(1 2 3))
                                       (lambda (x)
                                               (eff-bind (nondet-choose '(10 20))
                                                         (lambda (y)
                                                                 (eff-return (+ x y)))))))])
                   (assert-equal '(11 21 12 22 13 23) result)))
            
            (define-test nondet-first-found-test
              (let ([result (run-nondet-first (nondet-choose '(a b c)))])
                   (assert-true (just? result))
                   (assert-equal 'a (from-just result))))
            
            (define-test nondet-first-empty-test
              (let ([result (run-nondet-first nondet-fail)])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Console Effect Tests
;;; ============================================================

(test-group console-effect
            (define-test console-print-test
              (let ([result (run-console-pure '()
                                              (console-print "hello"))])
                   (assert-equal '() (car result))
                   (assert-equal '("hello") (cdr result))))
            
            (define-test console-read-test
              (let ([result (run-console-pure '("input")
                                              console-read)])
                   (assert-equal "input" (car result))))
            
            (define-test console-interaction-test
              (let ([result (run-console-pure '("Alice")
                                              (eff-bind (console-print "What is your name?")
                                                        (lambda (_)
                                                                (eff-bind console-read
                                                                          (lambda (name)
                                                                                  (eff-bind (console-print
                                                                                             (string-append "Hello, " name))
                                                                                            (lambda (_)
                                                                                                    (eff-return name))))))))])
                   (assert-equal "Alice" (car result))
                   (assert-equal '("What is your name?" "Hello, Alice") (cdr result)))))

;;; ============================================================
;;; Async Effect Tests
;;; ============================================================

(test-group async-effect
            (define-test async-fork-await-test
              (let ([result (run-async-sync
                             (eff-bind (async-fork (eff-return 42))
                                       (lambda (future)
                                               (async-await future))))])
                   (assert-equal 42 result)))
            
            (define-test async-multiple-test
              (let ([result (run-async-sync
                             (eff-bind (async-fork (eff-return 10))
                                       (lambda (f1)
                                               (eff-bind (async-fork (eff-return 20))
                                                         (lambda (f2)
                                                                 (eff-bind (async-await f1)
                                                                           (lambda (v1)
                                                                                   (eff-bind (async-await f2)
                                                                                             (lambda (v2)
                                                                                                     (eff-return (+ v1 v2)))))))))))])
                   (assert-equal 30 result))))

;;; ============================================================
;;; Handler Tests
;;; ============================================================

(test-group handlers
            (define-test make-handler-test
              (let ([h (make-handler identity (lambda (eff k) 'handled))])
                   (assert-true (handler? h))))
            
            (define-test handle-pure-test
              (let* ([h (make-handler
                         (lambda (x) (* x 2))
                         (lambda (eff k) 'effect))]
                     [result (handle h (eff-return 21))])
                    (assert-equal 42 result))))

;;; ============================================================
;;; Combining Effects Tests
;;; ============================================================

(test-group combining-effects
            (define-test eff-sequence-test
              (let ([result (run-state 0
                                       (eff-sequence (list (state-put 10)
                                                           state-get
                                                           (state-modify add1)
                                                           state-get)))])
                   (assert-equal '(() 10 () 11) (car result))
                   (assert-equal 11 (cdr result))))
            
            (define-test eff-map-m-test
              (let ([result (run-writer
                             (eff-map-m (lambda (x)
                                                (eff-bind (writer-tell x)
                                                          (lambda (_) (eff-return (* x 2)))))
                                        '(1 2 3)))])
                   (assert-equal '(2 4 6) (car result))
                   (assert-equal '(1 2 3) (cdr result))))
            
            (define-test eff-for-each-test
              (let ([result (run-writer
                             (eff-for-each writer-tell '(a b c)))])
                   (assert-equal '(a b c) (cdr result))))
            
            (define-test eff-fold-test
              (let ([result (run-state 0
                                       (eff-fold (lambda (acc x)
                                                         (eff-bind (state-modify (lambda (s) (+ s x)))
                                                                   (lambda (_)
                                                                           (eff-return (+ acc x)))))
                                                 0
                                                 '(1 2 3 4 5)))])
                   (assert-equal 15 (car result))
                   (assert-equal 15 (cdr result)))))

;;; ============================================================
;;; eff-when / eff-unless Tests
;;; ============================================================

(test-group eff-conditionals
            (define-test eff-when-true-test
              (let ([result (run-writer
                             (eff-when #t (writer-tell "executed")))])
                   (assert-equal '("executed") (cdr result))))
            
            (define-test eff-when-false-test
              (let ([result (run-writer
                             (eff-when #f (writer-tell "not executed")))])
                   (assert-equal '() (cdr result))))
            
            (define-test eff-unless-true-test
              (let ([result (run-writer
                             (eff-unless #t (writer-tell "not executed")))])
                   (assert-equal '() (cdr result))))
            
            (define-test eff-unless-false-test
              (let ([result (run-writer
                             (eff-unless #f (writer-tell "executed")))])
                   (assert-equal '("executed") (cdr result)))))

;;; ============================================================
;;; eff-lift Tests
;;; ============================================================

(test-group eff-lift
            (define-test eff-lift-test
              (let ([result (run-state 0
                                       (eff-lift add1 (eff-return 41)))])
                   (assert-equal 42 (car result))))
            
            (define-test eff-lift2-test
              (let ([result (run-state 0
                                       (eff-lift2 + (eff-return 20) (eff-return 22)))])
                   (assert-equal 42 (car result)))))

;;; ============================================================
;;; Practical Examples
;;; ============================================================

(test-group practical-examples
            ;; Stateful computation: factorial
            (define (eff-factorial n)
              (eff-fold (lambda (acc x)
                                (eff-return (* acc x)))
                        1
                        (range 1 n)))
            
            ;; Helper: range [lo, hi]
            (define (range lo hi)
              (if (> lo hi) '()
                  (cons lo (range (+ lo 1) hi))))
            
            (define-test factorial-test
              (let ([result (run-state 0 (eff-factorial 5))])
                   (assert-equal 120 (car result))))
            
            ;; Non-deterministic search: find pairs summing to n
            (define (pairs-summing-to n max)
              (eff-bind (nondet-choose (range 1 max))
                        (lambda (x)
                                (eff-bind (nondet-choose (range x max))
                                          (lambda (y)
                                                  (if (= (+ x y) n)
                                                      (eff-return (list x y))
                                                      nondet-fail))))))
            
            (define-test pairs-summing-test
              (let ([result (run-nondet (pairs-summing-to 10 8))])
                   (assert-true (not (not (member '(2 8) result))))
                   (assert-true (not (not (member '(3 7) result))))
                   (assert-true (not (not (member '(4 6) result))))))
            
            ;; State + Writer: logging counter
            (define (logged-increment)
              (eff-bind state-get
                        (lambda (n)
                                (eff-bind (writer-tell (string-append "incrementing from "
                                                                      (number->string n)))
                                          (lambda (_)
                                                  (state-put (+ n 1)))))))
            
            ;; Note: This requires effect layering which is complex
            ;; We'll test state and writer separately instead
            
            ;; Exception in computation
            (define (safe-divide a b)
              (if (= b 0)
                  (eff-throw "division by zero")
                  (eff-return (/ a b))))
            
            (define-test safe-divide-success-test
              (let ([result (run-exception (safe-divide 10 2))])
                   (assert-true (right? result))
                   (assert-equal 5 (from-right result))))
            
            (define-test safe-divide-failure-test
              (let ([result (run-exception (safe-divide 10 0))])
                   (assert-true (left? result))
                   (assert-equal "division by zero" (from-left result)))))

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
[SUCCESS] All effects tests passed.
")
    (display "
[FAILURE] Some effects tests failed.
"))
