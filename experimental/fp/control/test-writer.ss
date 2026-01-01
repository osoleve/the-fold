;;; fabric/stitches/fp/test-writer.ss — Tests for Writer Monad

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/control/writer.ss")

(display "
")
(display "==============================================================
")
(display "         WRITER MONAD TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Monoid Tests
;;; ============================================================

(test-group monoid-basics
            (define-test list-monoid-identity-test
              (let ([empty (monoid-empty list-monoid)]
                    [combine (monoid-append list-monoid)])
                   (assert-equal '() empty)
                   (assert-equal '(1 2 3) (combine empty '(1 2 3)))
                   (assert-equal '(1 2 3) (combine '(1 2 3) empty))))
            
            (define-test list-monoid-associative-test
              (let ([combine (monoid-append list-monoid)])
                   (assert-equal (combine (combine '(1) '(2)) '(3))
                                 (combine '(1) (combine '(2) '(3))))))
            
            (define-test string-monoid-test
              (let ([empty (monoid-empty string-monoid)]
                    [combine (monoid-append string-monoid)])
                   (assert-equal "" empty)
                   (assert-equal "hello" (combine empty "hello"))
                   (assert-equal "helloworld" (combine "hello" "world"))))
            
            (define-test sum-monoid-test
              (let ([empty (monoid-empty sum-monoid)]
                    [combine (monoid-append sum-monoid)])
                   (assert-equal 0 empty)
                   (assert-equal 10 (combine 3 7))))
            
            (define-test product-monoid-test
              (let ([empty (monoid-empty product-monoid)]
                    [combine (monoid-append product-monoid)])
                   (assert-equal 1 empty)
                   (assert-equal 21 (combine 3 7))))
            
            (define-test first-monoid-test
              (let ([combine (monoid-append first-monoid)])
                   (assert-true (nothing? (monoid-empty first-monoid)))
                   (assert-equal 1 (from-just (combine (just 1) (just 2))))
                   (assert-equal 2 (from-just (combine nothing (just 2))))))
            
            (define-test last-monoid-test
              (let ([combine (monoid-append last-monoid)])
                   (assert-true (nothing? (monoid-empty last-monoid)))
                   (assert-equal 2 (from-just (combine (just 1) (just 2))))
                   (assert-equal 1 (from-just (combine (just 1) nothing))))))

;;; ============================================================
;;; Core Writer Tests
;;; ============================================================

(test-group writer-basics
            (define-test make-writer-test
              (let ([wr (make-writer list-monoid 42 '("log"))])
                   (assert-true (writer? wr))))
            
            (define-test run-writer-test
              (let ([wr (make-writer list-monoid 42 '("log"))])
                   (let ([result (run-writer wr)])
                        (assert-equal 42 (car result))
                        (assert-equal '("log") (cdr result)))))
            
            (define-test eval-writer-test
              (let ([wr (make-writer list-monoid 42 '("log"))])
                   (assert-equal 42 (eval-writer wr))))
            
            (define-test exec-writer-test
              (let ([wr (make-writer list-monoid 42 '("log"))])
                   (assert-equal '("log") (exec-writer wr))))
            
            (define-test writer-pure-test
              (let ([wr (writer-pure list-monoid 42)])
                   (assert-equal 42 (eval-writer wr))
                   (assert-equal '() (exec-writer wr)))))

;;; ============================================================
;;; Tell Tests
;;; ============================================================

(test-group tell-operations
            (define-test tell-list-test
              (let ([wr (writer-tell list-monoid '("message"))])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal '("message") (exec-writer wr))))
            
            (define-test tell-string-test
              (let ([wr (writer-tell string-monoid "hello")])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal "hello" (exec-writer wr))))
            
            (define-test tell-sum-test
              (let ([wr (writer-tell sum-monoid 5)])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal 5 (exec-writer wr)))))

;;; ============================================================
;;; Monad Law Tests
;;; ============================================================

(test-group monad-laws
            ;; Left identity: pure a >>= f  ===  f a
            (define-test left-identity-test
              (let* ([a 5]
                     [f (lambda (x) (make-writer list-monoid (+ x 10) '("added")))]
                     [lhs (writer-bind (writer-pure list-monoid a) f)]
                     [rhs (f a)])
                    (assert-equal (run-writer lhs) (run-writer rhs))))
            
            ;; Right identity: m >>= pure  ===  m
            (define-test right-identity-test
              (let ([m (make-writer list-monoid 5 '("original"))])
                   (let* ([lhs (writer-bind m (lambda (x) (writer-pure list-monoid x)))]
                          [rhs m])
                         (assert-equal (run-writer lhs) (run-writer rhs)))))
            
            ;; Associativity: (m >>= f) >>= g  ===  m >>= (x -> f x >>= g)
            (define-test associativity-test
              (let ([m (writer-pure list-monoid 1)]
                    [f (lambda (x) (make-writer list-monoid (+ x 10) '("f")))]
                    [g (lambda (x) (make-writer list-monoid (+ x 100) '("g")))])
                   (let ([lhs (writer-bind (writer-bind m f) g)]
                         [rhs (writer-bind m (lambda (x) (writer-bind (f x) g)))])
                        (assert-equal (run-writer lhs) (run-writer rhs))))))

;;; ============================================================
;;; Bind and Map Tests
;;; ============================================================

(test-group writer-monad
            (define-test bind-test
              (let* ([wr1 (make-writer list-monoid 5 '("first"))]
                     [wr2 (writer-bind wr1
                                       (lambda (x)
                                               (make-writer list-monoid (* x 2) '("second"))))])
                    (assert-equal 10 (eval-writer wr2))
                    (assert-equal '("first" "second") (exec-writer wr2))))
            
            (define-test bind-chain-test
              (let* ([wr (writer-bind (writer-tell list-monoid '("a"))
                                      (lambda (_)
                                              (writer-bind (writer-tell list-monoid '("b"))
                                                           (lambda (_)
                                                                   (writer-bind (writer-tell list-monoid '("c"))
                                                                                (lambda (_)
                                                                                        (writer-pure list-monoid 'done)))))))])
                    (assert-equal 'done (eval-writer wr))
                    (assert-equal '("a" "b" "c") (exec-writer wr))))
            
            (define-test then-test
              (let ([wr (writer-then (writer-tell list-monoid '("ignored"))
                                     (make-writer list-monoid 42 '("kept")))])
                   (assert-equal 42 (eval-writer wr))
                   (assert-equal '("ignored" "kept") (exec-writer wr))))
            
            (define-test map-test
              (let ([wr (writer-map add1 (make-writer list-monoid 5 '("log")))])
                   (assert-equal 6 (eval-writer wr))
                   (assert-equal '("log") (exec-writer wr))))
            
            (define-test ap-test
              (let ([wr (writer-ap (make-writer list-monoid add1 '("fn"))
                                   (make-writer list-monoid 5 '("val")))])
                   (assert-equal 6 (eval-writer wr))
                   (assert-equal '("fn" "val") (exec-writer wr)))))

;;; ============================================================
;;; Listen and Censor Tests
;;; ============================================================

(test-group listen-censor
            (define-test listen-test
              (let* ([wr (make-writer list-monoid 42 '("log"))]
                     [listened (writer-listen wr)]
                     [result (eval-writer listened)])
                    (assert-equal 42 (car result))
                    (assert-equal '("log") (cdr result))
                    (assert-equal '("log") (exec-writer listened))))
            
            (define-test listens-test
              (let* ([wr (make-writer list-monoid 42 '("a" "b" "c"))]
                     [listened (writer-listens length wr)]
                     [result (eval-writer listened)])
                    (assert-equal 42 (car result))
                    (assert-equal 3 (cdr result))))
            
            (define-test censor-test
              (let* ([wr (make-writer list-monoid 42 '("secret" "public"))]
                     [censored (writer-censor (lambda (logs)
                                                      (filter (lambda (s) (not (equal? s "secret")))
                                                              logs))
                                              wr)])
                    (assert-equal 42 (eval-writer censored))
                    (assert-equal '("public") (exec-writer censored))))
            
            (define-test pass-test
              (let* ([wr (make-writer list-monoid
                                      (cons 42 (lambda (w) (reverse w)))
                                      '("a" "b" "c"))]
                     [passed (writer-pass wr)])
                    (assert-equal 42 (eval-writer passed))
                    (assert-equal '("c" "b" "a") (exec-writer passed)))))

;;; ============================================================
;;; Sequence Tests
;;; ============================================================

(test-group writer-sequence
            (define-test sequence-empty-test
              (let ([wr (writer-sequence list-monoid '())])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal '() (exec-writer wr))))
            
            (define-test sequence-test
              (let ([writers (list (make-writer list-monoid 1 '("one"))
                                   (make-writer list-monoid 2 '("two"))
                                   (make-writer list-monoid 3 '("three")))])
                   (let ([wr (writer-sequence list-monoid writers)])
                        (assert-equal '(1 2 3) (eval-writer wr))
                        (assert-equal '("one" "two" "three") (exec-writer wr)))))
            
            (define-test map-m-test
              (let ([wr (writer-map-m list-monoid
                                      (lambda (x)
                                              (make-writer list-monoid (* x 2) (list (number->string x))))
                                      '(1 2 3))])
                   (assert-equal '(2 4 6) (eval-writer wr))
                   (assert-equal '("1" "2" "3") (exec-writer wr)))))

;;; ============================================================
;;; Logging Helpers Tests
;;; ============================================================

(test-group logging-helpers
            (define-test log-msg-test
              (let ([wr (log-msg "Hello")])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal '("Hello") (exec-writer wr))))
            
            (define-test log-value-test
              (let ([wr (log-value "x" 42)])
                   (assert-equal 42 (eval-writer wr))
                   (assert-equal 1 (length (exec-writer wr)))))
            
            (define-test logging-chain-test
              (let ([wr (writer-bind (log-msg "Start")
                                     (lambda (_)
                                             (writer-bind (log-value "input" 5)
                                                          (lambda (x)
                                                                  (writer-bind (log-value "output" (* x 2))
                                                                               (lambda (y)
                                                                                       (writer-bind (log-msg "Done")
                                                                                                    (lambda (_)
                                                                                                            (writer-pure list-monoid y)))))))))])
                   (assert-equal 10 (eval-writer wr))
                   (assert-equal 4 (length (exec-writer wr))))))

;;; ============================================================
;;; Counting Helpers Tests
;;; ============================================================

(test-group counting-helpers
            (define-test count-one-test
              (let ([wr count-one])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal 1 (exec-writer wr))))
            
            (define-test count-n-test
              (let ([wr (count-n 5)])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal 5 (exec-writer wr))))
            
            (define-test count-chain-test
              (let ([wr (writer-bind count-one
                                     (lambda (_)
                                             (writer-bind count-one
                                                          (lambda (_)
                                                                  (writer-bind count-one
                                                                               (lambda (_)
                                                                                       (writer-pure sum-monoid 'done)))))))])
                   (assert-equal 'done (eval-writer wr))
                   (assert-equal 3 (exec-writer wr)))))

;;; ============================================================
;;; String Building Helpers Tests
;;; ============================================================

(test-group string-building
            (define-test emit-test
              (let ([wr (emit "hello")])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal "hello" (exec-writer wr))))
            
            (define-test emit-line-test
              (let ([wr (emit-line "hello")])
                   (assert-equal '() (eval-writer wr))
                   (assert-equal "hello
" (exec-writer wr))))
            
            (define-test emit-chain-test
              (let ([wr (writer-bind (emit-line "line1")
                                     (lambda (_)
                                             (writer-bind (emit-line "line2")
                                                          (lambda (_)
                                                                  (writer-pure string-monoid 'done)))))])
                   (assert-equal 'done (eval-writer wr))
                   (assert-equal "line1
line2
" (exec-writer wr)))))

;;; ============================================================
;;; Conditional Tests
;;; ============================================================

(test-group conditional-writer
            (define-test when-true-test
              (let ([wr (writer-when list-monoid #t (log-msg "logged"))])
                   (assert-equal '("logged") (exec-writer wr))))
            
            (define-test when-false-test
              (let ([wr (writer-when list-monoid #f (log-msg "logged"))])
                   (assert-equal '() (exec-writer wr))))
            
            (define-test unless-true-test
              (let ([wr (writer-unless list-monoid #t (log-msg "logged"))])
                   (assert-equal '() (exec-writer wr))))
            
            (define-test unless-false-test
              (let ([wr (writer-unless list-monoid #f (log-msg "logged"))])
                   (assert-equal '("logged") (exec-writer wr)))))

;;; ============================================================
;;; Lifting Tests
;;; ============================================================

(test-group lifting
            (define-test lift-test
              (let* ([double (writer-lift list-monoid (lambda (x) (* x 2)))]
                     [wr (double (make-writer list-monoid 5 '("input")))])
                    (assert-equal 10 (eval-writer wr))
                    (assert-equal '("input") (exec-writer wr))))
            
            (define-test lift2-test
              (let* ([add (writer-lift2 list-monoid +)]
                     [wr (add (make-writer list-monoid 3 '("a"))
                              (make-writer list-monoid 4 '("b")))])
                    (assert-equal 7 (eval-writer wr))
                    (assert-equal '("a" "b") (exec-writer wr)))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
            ;; Logging computation
            (define-test logging-computation-test
              (let* ([compute (lambda (x)
                                      (writer-bind (log-msg "Computing...")
                                                   (lambda (_)
                                                           (writer-bind (log-value "result" (* x x))
                                                                        (lambda (r)
                                                                                (log-msg "Done"))))))]
                     [wr (compute 5)])
                    (assert-equal 3 (length (exec-writer wr)))))
            
            ;; Counting recursive operations
            (define-test recursive-counting-test
              (let* ([factorial-counted (lambda (fact)
                                                (lambda (n)
                                                        (writer-bind count-one
                                                                     (lambda (_)
                                                                             (if (= n 0)
                                                                                 (writer-pure sum-monoid 1)
                                                                                 (writer-bind ((fact fact) (- n 1))
                                                                                              (lambda (r)
                                                                                                      (writer-pure sum-monoid (* n r)))))))))]
                     [fact (factorial-counted factorial-counted)]
                     [wr (fact 5)])
                    (assert-equal 120 (eval-writer wr))
                    (assert-equal 6 (exec-writer wr))))  ; 6 recursive calls (including base case)
            
            ;; HTML building - simplified test
            (define-test html-building-test
              (let* ([wr (writer-bind (emit-line "<html>")
                                      (lambda (_)
                                              (writer-bind (emit-line "</html>")
                                                           (lambda (_)
                                                                   (writer-pure string-monoid 'done)))))])
                    (assert-equal 'done (eval-writer wr))
                    (assert-true (> (string-length (exec-writer wr)) 0)))))

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
[SUCCESS] All writer monad tests passed.
")
    (display "
[FAILURE] Some writer monad tests failed.
"))
