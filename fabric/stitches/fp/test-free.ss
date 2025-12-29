;;; fabric/stitches/fp/test-free.ss — Tests for Free Monad

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/free.ss")

(display "
")
(display "==============================================================
")
(display "         FREE MONAD TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Core Constructor Tests
;;; ============================================================

(test-group free-constructors
            (define-test pure-free-test
              (let ([fr (pure-free 42)])
                   (assert-true (pure-free? fr))
                   (assert-false (free-suspended? fr))
                   (assert-equal 42 (from-pure-free fr))))
            
            (define-test free-suspended-test
              (let ([fr (free (list 'cmd 'data))])
                   (assert-true (free-suspended? fr))
                   (assert-false (pure-free? fr))
                   (assert-equal (list 'cmd 'data) (from-free fr))))
            
            (define-test pure-free-various-types-test
              (assert-equal "hello" (from-pure-free (pure-free "hello")))
              (assert-equal '(1 2 3) (from-pure-free (pure-free '(1 2 3))))
              (assert-equal #t (from-pure-free (pure-free #t)))))

;;; ============================================================
;;; Simple Functor for Testing
;;; ============================================================

;;; TestF a = ('test-cmd value next)
;;; A simple functor where next is the continuation

(define (test-fmap f cmd)
  (let ([tag (car cmd)]
        [value (cadr cmd)]
        [next (caddr cmd)])
       (list tag value (f next))))

;;; ============================================================
;;; Monad Operations Tests
;;; ============================================================

(test-group free-monad-ops
            (define-test free-map-pure-test
              (let* ([fr (pure-free 5)]
                     [result (free-map add1 test-fmap fr)])
                    (assert-true (pure-free? result))
                    (assert-equal 6 (from-pure-free result))))
            
            (define-test free-bind-pure-test
              (let* ([fr (pure-free 5)]
                     [result (free-bind test-fmap fr
                                        (lambda (x) (pure-free (* x 2))))])
                    (assert-true (pure-free? result))
                    (assert-equal 10 (from-pure-free result))))
            
            (define-test free-bind-chain-test
              (let* ([fr (free-bind test-fmap (pure-free 1)
                                    (lambda (x)
                                            (free-bind test-fmap (pure-free (+ x 10))
                                                       (lambda (y)
                                                               (pure-free (* y 2))))))])
                    (assert-true (pure-free? fr))
                    (assert-equal 22 (from-pure-free fr))))
            
            (define-test free-then-test
              (let* ([fr1 (pure-free 'ignored)]
                     [fr2 (pure-free 'kept)]
                     [result (free-then test-fmap fr1 fr2)])
                    (assert-true (pure-free? result))
                    (assert-equal 'kept (from-pure-free result))))
            
            (define-test free-ap-test
              (let* ([fr-f (pure-free add1)]
                     [fr-a (pure-free 5)]
                     [result (free-ap test-fmap fr-f fr-a)])
                    (assert-true (pure-free? result))
                    (assert-equal 6 (from-pure-free result)))))

;;; ============================================================
;;; Monad Laws Tests
;;; ============================================================

(test-group free-monad-laws
            ;; Left identity: pure a >>= f  ===  f a
            (define-test left-identity-test
              (let* ([a 5]
                     [f (lambda (x) (pure-free (* x 2)))]
                     [lhs (free-bind test-fmap (pure-free a) f)]
                     [rhs (f a)])
                    (assert-equal (from-pure-free lhs) (from-pure-free rhs))))
            
            ;; Right identity: m >>= pure  ===  m
            (define-test right-identity-test
              (let ([m (pure-free 42)])
                   (let* ([lhs (free-bind test-fmap m (lambda (x) (pure-free x)))]
                          [rhs m])
                         (assert-equal (from-pure-free lhs) (from-pure-free rhs)))))
            
            ;; Associativity: (m >>= f) >>= g  ===  m >>= (x -> f x >>= g)
            (define-test associativity-test
              (let ([m (pure-free 1)]
                    [f (lambda (x) (pure-free (+ x 10)))]
                    [g (lambda (x) (pure-free (* x 2)))])
                   (let ([lhs (free-bind test-fmap (free-bind test-fmap m f) g)]
                         [rhs (free-bind test-fmap m
                                         (lambda (x) (free-bind test-fmap (f x) g)))])
                        (assert-equal (from-pure-free lhs) (from-pure-free rhs))))))

;;; ============================================================
;;; lift-free Tests
;;; ============================================================

(test-group lift-free-tests
            (define-test lift-free-basic-test
              (let* ([wrap (lambda (x) (list 'wrapped x))]
                     [fr (lift-free wrap 42)])
                    (assert-true (free-suspended? fr))
                    (let ([inner (from-free fr)])
                         (assert-equal 'wrapped (car inner))))))

;;; ============================================================
;;; Interpreter Tests (fold-free, iter-free)
;;; ============================================================

(test-group interpreter-tests
            (define-test fold-free-pure-test
              (let ([result (fold-free
                             (lambda (x) (* x 2))  ; on-pure: double the value
                             (lambda (fx) fx)       ; on-free: pass through
                             test-fmap
                             (pure-free 5))])
                   (assert-equal 10 result)))
            
            (define-test iter-free-pure-test
              (let ([result (iter-free identity test-fmap (pure-free 42))])
                   (assert-equal 42 result))))

;;; ============================================================
;;; KV Store DSL Tests
;;; ============================================================

(test-group kv-store-dsl
            (define-test kv-get-creates-free-test
              (let ([fr (kv-get 'x)])
                   (assert-true (free-suspended? fr))
                   (let ([cmd (from-free fr)])
                        (assert-equal 'get (car cmd))
                        (assert-equal 'x (cadr cmd)))))
            
            (define-test kv-put-creates-free-test
              (let ([fr (kv-put 'x 42)])
                   (assert-true (free-suspended? fr))
                   (let ([cmd (from-free fr)])
                        (assert-equal 'put (car cmd))
                        (assert-equal 'x (cadr cmd))
                        (assert-equal 42 (caddr cmd)))))
            
            (define-test kv-delete-creates-free-test
              (let ([fr (kv-delete 'x)])
                   (assert-true (free-suspended? fr))
                   (let ([cmd (from-free fr)])
                        (assert-equal 'delete (car cmd))
                        (assert-equal 'x (cadr cmd)))))
            
            (define-test kv-fmap-get-test
              (let* ([cmd (list 'get 'x pure-free)]
                     [mapped (kv-fmap add1 cmd)])
                    (assert-equal 'get (car mapped))
                    (assert-equal 'x (cadr mapped))))
            
            (define-test kv-fmap-put-test
              (let* ([cmd (list 'put 'x 42 (pure-free '()))]
                     [mapped (kv-fmap (lambda (x) 'transformed) cmd)])
                    (assert-equal 'put (car mapped))
                    (assert-equal 'x (cadr mapped))
                    (assert-equal 42 (caddr mapped))
                    (assert-equal 'transformed (cadddr mapped))))
            
            (define-test run-kv-put-test
              (let* ([prog (kv-put 'x 42)]
                     [result ((run-kv prog) '())])
                    (assert-equal '() (car result))  ; value is ()
                    (assert-equal 42 (cdr (assoc 'x (cdr result))))))
            
            (define-test run-kv-get-found-test
              (let* ([prog (kv-get 'x)]
                     [store '((x . 42))]
                     [result ((run-kv prog) store)])
                    (let ([value (car result)])
                         (assert-true (just? value))
                         (assert-equal 42 (from-just value)))))
            
            (define-test run-kv-get-not-found-test
              (let* ([prog (kv-get 'y)]
                     [store '((x . 42))]
                     [result ((run-kv prog) store)])
                    (let ([value (car result)])
                         (assert-true (nothing? value)))))
            
            (define-test run-kv-put-get-test
              (let* ([prog (free-bind kv-fmap (kv-put 'x 100)
                                      (lambda (_) (kv-get 'x)))]
                     [result ((run-kv prog) '())])
                    (let ([value (car result)])
                         (assert-true (just? value))
                         (assert-equal 100 (from-just value)))))
            
            (define-test run-kv-delete-test
              (let* ([prog (free-bind kv-fmap (kv-put 'x 42)
                                      (lambda (_)
                                              (free-bind kv-fmap (kv-delete 'x)
                                                         (lambda (_) (kv-get 'x)))))]
                     [result ((run-kv prog) '())])
                    (let ([value (car result)])
                         (assert-true (nothing? value)))))
            
            (define-test run-kv-complex-program-test
              ;; Put x=10, y=20, get both and sum
              (let* ([prog (free-bind kv-fmap (kv-put 'x 10)
                                      (lambda (_)
                                              (free-bind kv-fmap (kv-put 'y 20)
                                                         (lambda (_)
                                                                 (free-bind kv-fmap (kv-get 'x)
                                                                            (lambda (mx)
                                                                                    (free-bind kv-fmap (kv-get 'y)
                                                                                               (lambda (my)
                                                                                                       (pure-free
                                                                                                        (+ (from-just mx)
                                                                                                           (from-just my)))))))))))]
                     [result ((run-kv prog) '())])
                    (assert-equal 30 (car result)))))

;;; ============================================================
;;; Console DSL Tests
;;; ============================================================

(test-group console-dsl
            (define-test console-print-creates-free-test
              (let ([fr (console-print "hello")])
                   (assert-true (free-suspended? fr))
                   (let ([cmd (from-free fr)])
                        (assert-equal 'print (car cmd))
                        (assert-equal "hello" (cadr cmd)))))
            
            (define-test console-read-creates-free-test
              (assert-true (free-suspended? console-read))
              (let ([cmd (from-free console-read)])
                   (assert-equal 'read (car cmd))))
            
            (define-test console-fmap-print-test
              (let* ([cmd (list 'print "msg" (pure-free '()))]
                     [mapped (console-fmap (lambda (x) 'transformed) cmd)])
                    (assert-equal 'print (car mapped))
                    (assert-equal "msg" (cadr mapped))
                    (assert-equal 'transformed (caddr mapped))))
            
            (define-test console-fmap-read-test
              (let* ([cmd (list 'read pure-free)]
                     [mapped (console-fmap (lambda (x) 'transformed) cmd)])
                    (assert-equal 'read (car mapped))))
            
            (define-test run-console-print-test
              (let* ([prog (console-print "Hello")]
                     [result (run-console-pure prog '())])
                    (assert-equal '() (car result))  ; value
                    (assert-equal '("Hello") (cdr result))))  ; output
            
            (define-test run-console-read-test
              (let* ([prog console-read]
                     [result (run-console-pure prog '("input"))])
                    (assert-equal "input" (car result))
                    (assert-equal '() (cdr result))))
            
            (define-test run-console-print-read-test
              (let* ([prog (free-bind console-fmap (console-print "Name?")
                                      (lambda (_) console-read))]
                     [result (run-console-pure prog '("Alice"))])
                    (assert-equal "Alice" (car result))
                    (assert-equal '("Name?") (cdr result))))
            
            (define-test run-console-greet-program-test
              ;; Print prompt, read name, print greeting
              (let* ([prog (free-bind console-fmap (console-print "What is your name?")
                                      (lambda (_)
                                              (free-bind console-fmap console-read
                                                         (lambda (name)
                                                                 (console-print
                                                                  (string-append "Hello, " name "!"))))))]
                     [result (run-console-pure prog '("World"))])
                    (assert-equal '() (car result))
                    (assert-equal '("What is your name?" "Hello, World!") (cdr result)))))

;;; ============================================================
;;; Free Combinators Tests
;;; ============================================================

(test-group free-combinators
            (define-test free-sequence-empty-test
              (let ([result (free-sequence test-fmap '())])
                   (assert-true (pure-free? result))
                   (assert-equal '() (from-pure-free result))))
            
            (define-test free-sequence-pure-values-test
              (let* ([frees (list (pure-free 1) (pure-free 2) (pure-free 3))]
                     [result (free-sequence test-fmap frees)])
                    (assert-true (pure-free? result))
                    (assert-equal '(1 2 3) (from-pure-free result))))
            
            (define-test free-map-m-test
              (let* ([result (free-map-m test-fmap
                                         (lambda (x) (pure-free (* x 2)))
                                         '(1 2 3))])
                    (assert-true (pure-free? result))
                    (assert-equal '(2 4 6) (from-pure-free result))))
            
            (define-test free-for-each-test
              (let* ([result (free-for-each test-fmap '(1 2 3)
                                            (lambda (x) (pure-free x)))])
                    (assert-true (pure-free? result))
                    (assert-equal '() (from-pure-free result))))
            
            (define-test free-when-true-test
              (let ([result (free-when test-fmap #t (pure-free 'executed))])
                   (assert-true (pure-free? result))
                   (assert-equal 'executed (from-pure-free result))))
            
            (define-test free-when-false-test
              (let ([result (free-when test-fmap #f (pure-free 'not-executed))])
                   (assert-true (pure-free? result))
                   (assert-equal '() (from-pure-free result))))
            
            (define-test free-unless-true-test
              (let ([result (free-unless test-fmap #t (pure-free 'not-executed))])
                   (assert-true (pure-free? result))
                   (assert-equal '() (from-pure-free result))))
            
            (define-test free-unless-false-test
              (let ([result (free-unless test-fmap #f (pure-free 'executed))])
                   (assert-true (pure-free? result))
                   (assert-equal 'executed (from-pure-free result)))))

;;; ============================================================
;;; Coyoneda Tests
;;; ============================================================

(test-group coyoneda-tests
            (define-test make-coyoneda-test
              (let ([cy (make-coyoneda '(some data))])
                   (assert-equal 'coyoneda (car cy))))
            
            (define-test coyoneda-map-test
              (let* ([cy (make-coyoneda 42)]
                     [mapped (coyoneda-map add1 cy)]
                     [mapped2 (coyoneda-map add1 mapped)])
                    ;; Maps compose without running the underlying functor
                    (assert-equal 'coyoneda (car mapped))
                    (assert-equal 'coyoneda (car mapped2))))
            
            ;; For testing lowering, we need a proper functor value
            ;; We'll use lists as our functor, with map as fmap
            (define-test coyoneda-functor-law-test
              ;; coyoneda-map id cy  ===  cy (in terms of lowered values)
              (let* ([cy (make-coyoneda '(1 2 3))]
                     [mapped (coyoneda-map id cy)])
                    ;; Both should lower to the same thing given list's map as fmap
                    (assert-equal (lower-coyoneda map cy)
                                  (lower-coyoneda map mapped))))
            
            (define-test coyoneda-composition-law-test
              ;; coyoneda-map (f . g) cy  ===  coyoneda-map f (coyoneda-map g cy)
              (let* ([f add1]
                     [g (lambda (x) (* x 2))]
                     [fg (lambda (x) (f (g x)))]  ; manual composition
                     [cy (make-coyoneda '(1 2 3))]
                     [lhs (coyoneda-map fg cy)]
                     [rhs (coyoneda-map f (coyoneda-map g cy))])
                    ;; Both should lower to the same result
                    (assert-equal (lower-coyoneda map lhs)
                                  (lower-coyoneda map rhs)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group integration-tests
            ;; Build a small KV program that demonstrates composition
            (define-test kv-multiple-operations-test
              (let* ([prog (free-bind kv-fmap (kv-put 'a 1)
                                      (lambda (_)
                                              (free-bind kv-fmap (kv-put 'b 2)
                                                         (lambda (_)
                                                                 (free-bind kv-fmap (kv-put 'c 3)
                                                                            (lambda (_)
                                                                                    (free-bind kv-fmap (kv-get 'b)
                                                                                               (lambda (mb)
                                                                                                       (pure-free (from-just mb))))))))))]
                     [result ((run-kv prog) '())])
                    (assert-equal 2 (car result))
                    ;; Store should have all three keys
                    (let ([store (cdr result)])
                         (assert-equal 1 (cdr (assoc 'a store)))
                         (assert-equal 2 (cdr (assoc 'b store)))
                         (assert-equal 3 (cdr (assoc 'c store))))))
            
            ;; Console program with multiple interactions
            (define-test console-calculator-test
              ;; Simulate a simple calculator: read two numbers, print sum
              (let* ([read-num (free-bind console-fmap console-read
                                          (lambda (s) (pure-free (string->number s))))]
                     [prog (free-bind console-fmap (console-print "Enter first number:")
                                      (lambda (_)
                                              (free-bind console-fmap read-num
                                                         (lambda (a)
                                                                 (free-bind console-fmap (console-print "Enter second number:")
                                                                            (lambda (_)
                                                                                    (free-bind console-fmap read-num
                                                                                               (lambda (b)
                                                                                                       (console-print
                                                                                                        (string-append "Sum: "
                                                                                                                       (number->string (+ a b))))))))))))]
                     [result (run-console-pure prog '("3" "4"))])
                    (assert-equal '() (car result))
                    (assert-equal '("Enter first number:" "Enter second number:" "Sum: 7")
                                  (cdr result)))))

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
[SUCCESS] All free monad tests passed.
")
    (display "
[FAILURE] Some free monad tests failed.
"))
