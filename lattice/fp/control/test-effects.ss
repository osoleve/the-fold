;;; core/fp/control/test-effects.ss --- Tests for Algebraic Effects

;;; NOTE: Run from project root

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/fp/control/effects.ss")

(display "\n")
(display "====\n")
(display "         ALGEBRAIC EFFECTS TESTS\n")
(display "====\n")

;;; ====
;;; Effect Signature Tests
;;; ====

(test-group effect-signatures
            (define-test make-effect-sig-test
              (let ([sig (make-effect-sig 'TestEffect
                                          (list (make-operation 'op1 'Int 'String)))])
                   (assert-true (effect-sig? sig))
                   (assert-equal 'TestEffect (effect-sig-name sig))
                   (assert-equal 1 (length (effect-sig-operations sig)))))
            
            (define-test make-operation-test
              (let ([op (make-operation 'myop 'Input 'Output)])
                   (assert-true (operation? op))
                   (assert-equal 'myop (operation-name op))
                   (assert-equal 'Input (operation-param-type op))
                   (assert-equal 'Output (operation-result-type op))))
            
            (define-test lookup-operation-test
              (let ([op (lookup-operation sig-State 'get)])
                   (assert-true (operation? op))
                   (assert-equal 'get (operation-name op))))
            
            (define-test lookup-operation-not-found-test
              (let ([op (lookup-operation sig-State 'nonexistent)])
                   (assert-false op)))
            
            (define-test builtin-signatures-test
              (assert-true (effect-sig? sig-State))
              (assert-true (effect-sig? sig-Reader))
              (assert-true (effect-sig? sig-Writer))
              (assert-true (effect-sig? sig-Exception))
              (assert-true (effect-sig? sig-NonDet))
              (assert-true (effect-sig? sig-Console))
              (assert-true (effect-sig? sig-Async))))

;;; ====
;;; Effect Row Tests
;;; ====

(test-group effect-rows
            (define-test empty-row-test
              (assert-true (effect-row? empty-row))
              (assert-equal '() (effect-row-effects empty-row)))
            
            (define-test row-add-test
              (let ([row (row-add empty-row 'State)])
                   (assert-true (row-contains? row 'State))
                   (assert-false (row-contains? row 'Writer))))
            
            (define-test row-add-duplicate-test
              (let* ([row1 (row-add empty-row 'State)]
                     [row2 (row-add row1 'State)])
                    ;; Should not duplicate
                    (assert-equal 1 (length (effect-row-effects row2)))))
            
            (define-test row-remove-test
              (let* ([row1 (row-add (row-add empty-row 'State) 'Writer)]
                     [row2 (row-remove row1 'State)])
                    (assert-false (row-contains? row2 'State))
                    (assert-true (row-contains? row2 'Writer))))
            
            (define-test row-union-test
              (let* ([row1 (row-add empty-row 'State)]
                     [row2 (row-add empty-row 'Writer)]
                     [combined (row-union row1 row2)])
                    (assert-true (row-contains? combined 'State))
                    (assert-true (row-contains? combined 'Writer))))
            
            (define-test row-difference-test
              (let* ([row1 (row-add (row-add empty-row 'State) 'Writer)]
                     [row2 (row-add empty-row 'State)]
                     [diff (row-difference row1 row2)])
                    (assert-true (row-contains? diff 'Writer))
                    (assert-false (row-contains? diff 'State))))
            
            (define-test row-var-test
              (let ([rv (make-row-var 'r)])
                   (assert-true (row-var? rv))
                   (assert-equal 'r (row-var-name rv)))))

;;; ====
;;; Eff Structure Tests
;;; ====

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
                   (assert-equal 42 (eff-pure-value e))))
            
            (define-test eff-ap-test
              (let* ([ef (eff-return add1)]
                     [ea (eff-return 41)]
                     [result (run-state 0 (eff-ap ef ea))])
                    (assert-equal 42 (car result)))))

;;; ====
;;; Effect Structure Tests
;;; ====

(test-group effect-structure
            (define-test make-effect-test
              (let ([e (make-effect 'my-tag 'my-payload)])
                   (assert-true (effect? e))
                   (assert-equal 'my-tag (effect-tag e))
                   (assert-equal 'my-payload (effect-payload e))))
            
            (define-test effect-label-test
              (let ([e (make-effect 'state-get '())])
                   (assert-equal 'State (effect-label e)))))

;;; ====
;;; Eff Monad Laws
;;; ====

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

;;; ====
;;; Handler Tests
;;; ====

(test-group handlers
            (define-test make-handler-test
              (let ([h (make-handler identity '() 'deep)])
                   (assert-true (handler? h))
                   (assert-equal 'deep (handler-semantics h))))
            
            (define-test deep-handler-test
              (let ([h (deep-handler identity '())])
                   (assert-equal 'deep (handler-semantics h))))
            
            (define-test shallow-handler-test
              (let ([h (shallow-handler identity '())])
                   (assert-equal 'shallow (handler-semantics h))))
            
            (define-test handle-pure-test
              (let* ([h (deep-handler
                         (lambda (x) (* x 2))
                         '())]
                     [result (handle h (eff-return 21))])
                    (assert-equal 42 result)))
            
            (define-test handler-lookup-test
              (let ([h (deep-handler identity
                                     `((test-op . ,(lambda (p k) 'handled))))])
                   (assert-true (procedure? (handler-lookup h 'test-op)))
                   (assert-false (handler-lookup h 'unknown-op)))))

;;; ====
;;; Handler Composition Tests
;;; ====

(test-group handler-composition
            (define-test compose-handlers-test
              (let* ([h1 (deep-handler add1 '())]
                     [h2 (deep-handler (lambda (x) (* x 2)) '())]
                     [h3 (compose-handlers h1 h2)]
                     [result (handle h3 (eff-return 20))])
                    ;; h1 first: 20 + 1 = 21, then h2: 21 * 2 = 42
                    (assert-equal 42 result)))
            
            (define-test nest-handlers-test
              (let* ([h1 (deep-handler add1 '())]
                     [h2 (deep-handler (lambda (x) (* x 2)) '())]
                     [h (nest-handlers (list h1 h2))]
                     [result (handle h (eff-return 20))])
                    (assert-equal 42 result)))
            
            (define-test nest-handlers-empty-test
              (let* ([h (nest-handlers '())]
                     [result (handle h (eff-return 42))])
                    (assert-equal 42 result))))

;;; ====
;;; State Effect Tests
;;; ====

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
            
            (define-test state-gets-test
              (let ([result (run-state 10 (state-gets (lambda (x) (* x 2))))])
                   (assert-equal 20 (car result))
                   (assert-equal 10 (cdr result))))  ; state unchanged
            
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
                   (assert-equal 7 (cdr result))))
            
            (define-test local-state-test
              (let ([result (run-state 100
                                       (eff-bind (local-state 0
                                                              (eff-bind counter-increment
                                                                        (lambda (_) state-get)))
                                                 (lambda (inner-result)
                                                         (eff-bind state-get
                                                                   (lambda (outer-state)
                                                                           (eff-return (list inner-result outer-state)))))))])
                   (assert-equal '(1 100) (car result))))
            
            (define-test make-state-handler-threads-state-test
              ;; Test that make-state-handler properly threads state through
              ;; This is a regression test for the bug where return case
              ;; was (lambda (a) (cons a init-state)) instead of threading state
              (let* ([handler (make-state-handler 0)]
                     [computation (eff-bind (state-put 10)
                                            (lambda (_)
                                                    (eff-bind (state-put 20)
                                                              (lambda (_)
                                                                      (eff-bind state-get
                                                                                (lambda (s)
                                                                                        (eff-return s)))))))]
                     [result-fn (handle handler computation)]
                     [result (result-fn 0)])  ; Apply the state-threading function
                    ;; Should return (20 . 20) because state was threaded through
                    ;; Bug would have returned (20 . 0) with init-state
                    (assert-equal 20 (car result))
                    (assert-equal 20 (cdr result)))))

;;; ====
;;; Reader Effect Tests
;;; ====

(test-group reader-effect
            (define-test reader-ask-test
              (let ([result (run-reader "environment" reader-ask)])
                   (assert-equal "environment" result)))
            
            (define-test reader-asks-test
              (let ([result (run-reader 10 (reader-asks (lambda (x) (* x 2))))])
                   (assert-equal 20 result)))
            
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
                   (assert-equal 10 result)))
            
            (define-test reader-local-basic-test
              ;; reader-local should modify the environment for the inner computation
              (let ([result (run-reader 10
                                        (reader-local (lambda (e) (+ e 5))
                                                      reader-ask))])
                   (assert-equal 15 result)))
            
            (define-test reader-local-restores-env-test
              ;; After reader-local completes, the original environment should be restored
              ;; This is the key test for the fix
              (let ([result (run-reader 1
                                        (eff-bind reader-ask
                                                  (lambda (outer)
                                                          (eff-bind (reader-local (lambda (e) (+ e 10))
                                                                                  reader-ask)
                                                                    (lambda (inner)
                                                                            (eff-return (cons outer inner)))))))])
                   ;; outer should be 1 (initial env)
                   ;; inner should be 11 (modified env inside reader-local)
                   (assert-equal '(1 . 11) result)))
            
            (define-test reader-local-after-continues-with-original-env-test
              ;; Verify that subsequent operations after reader-local see the original env
              (let ([result (run-reader 100
                                        (eff-bind (reader-local (lambda (e) (* e 2))
                                                                reader-ask)
                                                  (lambda (inner)
                                                          (eff-bind reader-ask
                                                                    (lambda (after)
                                                                            (eff-return (list inner after)))))))])
                   ;; inner should be 200 (modified env)
                   ;; after should be 100 (original env restored)
                   (assert-equal '(200 100) result)))
            
            (define-test reader-local-nested-test
              ;; Nested reader-local should stack and unstack correctly
              (let ([result (run-reader 1
                                        (eff-bind reader-ask
                                                  (lambda (first)
                                                          (eff-bind (reader-local add1
                                                                                  (eff-bind reader-ask
                                                                                            (lambda (second)
                                                                                                    (eff-bind (reader-local add1
                                                                                                                            reader-ask)
                                                                                                              (lambda (third)
                                                                                                                      (eff-return (list first second third)))))))
                                                                    (lambda (nested-result)
                                                                            (eff-bind reader-ask
                                                                                      (lambda (after)
                                                                                              (eff-return (cons nested-result after)))))))))])
                   ;; first = 1 (initial), second = 2 (after first local), third = 3 (after second local)
                   ;; after = 1 (back to original after all locals complete)
                   (assert-equal '((1 2 3) . 1) result))))

;;; ====
;;; Writer Effect Tests
;;; ====

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
            
            (define-test log-debug-test
              (let ([result (run-writer (log-debug "debug msg"))])
                   (assert-equal '((debug "debug msg")) (cdr result))))
            
            (define-test multiple-log-levels-test
              (let ([result (run-writer
                             (eff-bind (log-info "info")
                                       (lambda (_)
                                               (eff-bind (log-warn "warning")
                                                         (lambda (_)
                                                                 (log-error "error"))))))])
                   (assert-equal 3 (length (cdr result))))))

;;; ====
;;; Exception Effect Tests
;;; ====

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
                   (assert-equal "too big" (from-left result))))
            
            (define-test eff-try-test
              (let ([result (run-state 0 (eff-try (eff-throw "err")))])
                   (assert-true (left? (car result))))))

;;; ====
;;; NonDet Effect Tests
;;; ====

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
            
            (define-test nondet-guard-pass-test
              (let ([result (run-nondet (nondet-guard #t))])
                   (assert-equal 1 (length result))))
            
            (define-test nondet-guard-fail-test
              (let ([result (run-nondet (nondet-guard #f))])
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
                   (assert-true (nothing? result))))
            
            (define-test nondet-bounded-test
              (let ([result (run-nondet-bounded 3 (nondet-choose '(1 2 3 4 5)))])
                   (assert-equal 3 (length result))))
            
            (define-test nondet-from-maybe-just-test
              (let ([result (run-nondet (nondet-from-maybe (just 42)))])
                   (assert-equal '(42) result)))
            
            (define-test nondet-from-maybe-nothing-test
              (let ([result (run-nondet (nondet-from-maybe nothing))])
                   (assert-equal '() result))))

;;; ====
;;; Console Effect Tests
;;; ====

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
            
            (define-test console-print-line-test
              (let ([result (run-console-pure '()
                                              (console-print-line "hello"))])
                   (assert-equal '("hello\n") (cdr result))))
            
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

;;; ====
;;; Async Effect Tests
;;; ====

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
                   (assert-equal 30 result)))
            
            (define-test async-parallel-test
              (let ([result (run-async-sync
                             (async-parallel (list (eff-return 1)
                                                   (eff-return 2)
                                                   (eff-return 3))))])
                   (assert-equal '(1 2 3) result))))

;;; ====
;;; Combining Effects Tests
;;; ====

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
            
            (define-test eff-filter-m-test
              (let ([result (run-state 0
                                       (eff-filter-m (lambda (x)
                                                             (eff-return (even? x)))
                                                     '(1 2 3 4 5 6)))])
                   (assert-equal '(2 4 6) (car result))))
            
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
                   (assert-equal 15 (cdr result))))
            
            (define-test eff-fold-right-test
              (let ([result (run-writer
                             (eff-fold-right (lambda (x acc)
                                                     (eff-bind (writer-tell x)
                                                               (lambda (_)
                                                                       (eff-return (cons x acc)))))
                                             '()
                                             '(1 2 3)))])
                   (assert-equal '(1 2 3) (car result))
                   (assert-equal '(3 2 1) (cdr result))))  ; told in reverse order
            
            (define-test eff-replicate-test
              (let ([result (run-state 0
                                       (eff-replicate 5 (eff-bind state-get
                                                                  (lambda (n)
                                                                          (eff-bind (state-put (+ n 1))
                                                                                    (lambda (_)
                                                                                            (eff-return n)))))))])
                   (assert-equal '(0 1 2 3 4) (car result))
                   (assert-equal 5 (cdr result)))))

;;; ====
;;; eff-when / eff-unless Tests
;;; ====

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

;;; ====
;;; eff-lift Tests
;;; ====

(test-group eff-lift
            (define-test eff-lift-test
              (let ([result (run-state 0
                                       (eff-lift add1 (eff-return 41)))])
                   (assert-equal 42 (car result))))
            
            (define-test eff-lift2-test
              (let ([result (run-state 0
                                       (eff-lift2 + (eff-return 20) (eff-return 22)))])
                   (assert-equal 42 (car result))))
            
            (define-test eff-lift3-test
              (let ([result (run-state 0
                                       (eff-lift3 (lambda (a b c) (+ a b c))
                                                  (eff-return 10)
                                                  (eff-return 20)
                                                  (eff-return 12)))])
                   (assert-equal 42 (car result)))))

;;; ====
;;; Effect Constraints Tests
;;; ====

(test-group effect-constraints
            (define-test make-effect-constraint-test
              (let ([c (make-effect-constraint '(State Writer))])
                   (assert-true (effect-constraint? c))
                   (assert-equal '(State Writer) (effect-constraint-effects c))))
            
            (define-test check-effects-pure-test
              (assert-true (check-effects '() (eff-return 42))))
            
            (define-test check-effects-allowed-test
              (assert-true (check-effects '(State) state-get))))

;;; ====
;;; Combined Effect Handlers Tests
;;; ====

(test-group combined-handlers
            (define-test run-state-writer-test
              (let ([result (run-state-writer 0
                                              (eff-bind (state-put 10)
                                                        (lambda (_)
                                                                (eff-bind (writer-tell "set to 10")
                                                                          (lambda (_)
                                                                                  state-get)))))])
                   ;; Result is ((value . state) . log)
                   (assert-equal 10 (caar result))     ; value
                   (assert-equal 10 (cdar result))     ; state
                   (assert-equal '("set to 10") (cdr result))))  ; log
            
            (define-test run-reader-writer-test
              (let ([result (run-reader-writer "env"
                                               (eff-bind reader-ask
                                                         (lambda (e)
                                                                 (eff-bind (writer-tell e)
                                                                           (lambda (_)
                                                                                   (eff-return 42))))))])
                   (assert-equal 42 (car result))
                   (assert-equal '("env") (cdr result)))))

;;; ====
;;; Scoped Effects Tests
;;; ====

(test-group scoped-effects
            (define-test with-state-test
              (let ([result (run-state 999
                                       (with-state 0
                                                   (eff-bind counter-increment
                                                             (lambda (_) state-get))))])
                   ;; Inner state scope returns 1, outer state unchanged
                   (assert-equal 1 (car result))
                   (assert-equal 999 (cdr result))))
            
            (define-test with-writer-test
              (let ([result (run-state 0
                                       (with-writer
                                        (eff-bind (writer-tell "hi")
                                                  (lambda (_) (eff-return 42)))))])
                   ;; Returns (value . log) pair
                   (assert-equal 42 (caar result))
                   (assert-equal '("hi") (cdar result))))
            
            (define-test with-nondet-test
              (let ([result (run-state 0
                                       (with-nondet (nondet-choose '(1 2 3))))])
                   (assert-equal '(1 2 3) (car result)))))

;;; ====
;;; Resumable Continuations Tests
;;; ====

(test-group resumable-continuations
            (define-test resume-test
              (let* ([captured-k #f]
                     [eff (make-eff-op (make-effect 'test '())
                                       (lambda (resp)
                                               (eff-return (* resp 2))))]
                     [k (eff-op-cont eff)]
                     [resumed (resume k 21)])
                    (assert-equal 42 (car (run-state 0 resumed)))))
            
            (define-test abort-test
              (let ([result (run-state 0 (abort 42))])
                   (assert-equal 42 (car result)))))

;;; ====
;;; Interleaving Tests
;;; ====

(test-group interleaving
            (define-test interleave-first-completes-test
              (let ([result (run-state 0
                                       (interleave (eff-return 'a)
                                                   (eff-return 'b)))])
                   (assert-true (left? (car result)))
                   (assert-equal 'a (from-left (car result))))))

;;; ====
;;; Practical Examples
;;; ====

(test-group practical-examples
            ;; Helpers (all defines must precede define-test in R6RS)
            (define (range lo hi)
              (if (> lo hi) '()
                  (cons lo (range (+ lo 1) hi))))

            (define (eff-factorial n)
              (eff-fold (lambda (acc x)
                                (eff-return (* acc x)))
                        1
                        (range 1 n)))

            (define (pairs-summing-to n max)
              (eff-bind (nondet-choose (range 1 max))
                        (lambda (x)
                                (eff-bind (nondet-choose (range x max))
                                          (lambda (y)
                                                  (if (= (+ x y) n)
                                                      (eff-return (list x y))
                                                      nondet-fail))))))

            (define (safe-divide a b)
              (if (= b 0)
                  (eff-throw "division by zero")
                  (eff-return (/ a b))))

            (define (pythagorean-triples limit)
              (eff-bind (nondet-choose (range 1 limit))
                        (lambda (a)
                                (eff-bind (nondet-choose (range a limit))
                                          (lambda (b)
                                                  (eff-bind (nondet-choose (range b limit))
                                                            (lambda (c)
                                                                    (eff-bind (nondet-guard (= (+ (* a a) (* b b)) (* c c)))
                                                                              (lambda (_)
                                                                                      (eff-return (list a b c)))))))))))

            (define-test factorial-test
              (let ([result (run-state 0 (eff-factorial 5))])
                   (assert-equal 120 (car result))))

            (define-test pairs-summing-test
              (let ([result (run-nondet (pairs-summing-to 10 8))])
                   (assert-true (not (not (member '(2 8) result))))
                   (assert-true (not (not (member '(3 7) result))))
                   (assert-true (not (not (member '(4 6) result))))))

            (define-test safe-divide-success-test
              (let ([result (run-exception (safe-divide 10 2))])
                   (assert-true (right? result))
                   (assert-equal 5 (from-right result))))

            (define-test safe-divide-failure-test
              (let ([result (run-exception (safe-divide 10 0))])
                   (assert-true (left? result))
                   (assert-equal "division by zero" (from-left result))))

            (define-test pythagorean-triples-test
              (let ([result (run-nondet (pythagorean-triples 15))])
                   (assert-true (not (not (member '(3 4 5) result))))
                   (assert-true (not (not (member '(5 12 13) result)))))))

;;; ====
;;; Deep vs Shallow Handler Tests
;;; ====

(test-group deep-vs-shallow
            ;; Test that deep handlers wrap the entire continuation
            (define-test deep-handler-wraps-continuation-test
              (let* ([counter 0]
                     [h (deep-handler
                         identity
                         `((count . ,(lambda (payload k)
                                             (set! counter (+ counter 1))
                                             (k '())))))]
                     [eff (eff-bind (perform (make-effect 'count '()))
                                    (lambda (_)
                                            (eff-bind (perform (make-effect 'count '()))
                                                      (lambda (_)
                                                              (perform (make-effect 'count '()))))))]
                     [result (handle h eff)])
                    (assert-equal 3 counter)))
            
            ;; Test that shallow handlers only handle once
            (define-test shallow-handler-once-test
              (let* ([h (shallow-handler
                         identity
                         `((incr . ,(lambda (payload k)
                                            ;; Return the continuation as-is
                                            ;; (it won't be re-wrapped)
                                            (k (+ payload 1))))))]
                     ;; First step only
                     [eff (perform (make-effect 'incr 0))])
                    ;; Shallow handler processes just the first op
                    ;; Result is (eff-pure 1) because the return case is identity
                    (let ([result (handle h eff)])
                         (assert-true (eff-pure? result))
                         (assert-equal 1 (eff-pure-value result))))))

;;; ====
;;; Performance Tests (NonDet O(N) Collection)
;;; ====

(test-group nondet-performance
            ;; Test that run-nondet scales linearly with many branches.
            ;; With N branches each returning 1 result, old O(N^2) approach
            ;; would be slow; new O(N) approach should be fast.
            (define-test nondet-many-branches-test
              ;; 1000 branches, each returning a single value
              (let* ([n 1000]
                     [result (run-nondet (nondet-choose (iota n)))])
                    (assert-equal n (length result))
                    ;; Verify order is preserved
                    (assert-equal 0 (car result))
                    (assert-equal (- n 1) (car (reverse result)))))
            
            (define-test nondet-deep-tree-test
              ;; Create a binary tree of depth 8 = 256 leaves
              ;; This tests nested NonDet where results accumulate
              (let* ([depth 8]
                     [eff (let loop ([d depth])
                               (if (= d 0)
                                   (eff-return 'leaf)
                                   (eff-bind (nondet-choose '(left right))
                                             (lambda (choice)
                                                     (loop (- d 1))))))]
                     [result (run-nondet eff)])
                    (assert-equal 256 (length result))))  ; 2^8 = 256
            
            (define-test nondet-bounded-many-branches-test
              ;; Test bounded version with many potential branches
              (let* ([n 1000]
                     [limit 50]
                     [result (run-nondet-bounded limit (nondet-choose (iota n)))])
                    (assert-equal limit (length result))
                    ;; Should be first 50 elements in order
                    (assert-equal 0 (car result))
                    (assert-equal 49 (car (reverse result)))))
            
            (define-test nondet-cartesian-large-test
              ;; 20 x 20 = 400 results via cartesian product
              (let* ([n 20]
                     [result (run-nondet
                              (eff-bind (nondet-choose (iota n))
                                        (lambda (x)
                                                (eff-bind (nondet-choose (iota n))
                                                          (lambda (y)
                                                                  (eff-return (cons x y)))))))])
                    (assert-equal 400 (length result))
                    ;; First should be (0 . 0), last should be (19 . 19)
                    (assert-equal '(0 . 0) (car result))
                    (assert-equal '(19 . 19) (car (reverse result))))))

;; iota is provided by prelude (via test-framework.ss)

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All effects tests passed.\n")
    (display "\n[FAILURE] Some effects tests failed.\n"))
