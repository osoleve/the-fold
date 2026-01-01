;;; fabric/stitches/fp/test-profunctor.ss — Tests for Profunctor Abstraction

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/functors/profunctor.ss")

(display "
")
(display "==============================================================
")
(display "         PROFUNCTOR TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Function Profunctor Tests
;;; ============================================================

(test-group function-profunctor
            (define-test fn-dimap-test
              (let* ([double (lambda (x) (* x 2))]
                     [str->num string->number]
                     [show number->string]
                     [f (fn-dimap str->num show double)])
                    (assert-equal "42" (f "21"))))
            
            (define-test fn-lmap-test
              (let* ([double (lambda (x) (* x 2))]
                     [str->num string->number]
                     [f (fn-lmap str->num double)])
                    (assert-equal 42 (f "21"))))
            
            (define-test fn-rmap-test
              (let* ([double (lambda (x) (* x 2))]
                     [show number->string]
                     [f (fn-rmap show double)])
                    (assert-equal "42" (f 21))))
            
            ;; Profunctor law: dimap id id = id
            (define-test fn-dimap-identity-test
              (let* ([f (lambda (x) (* x 2))]
                     [g (fn-dimap identity identity f)])
                    (assert-equal (f 5) (g 5))))
            
            ;; Profunctor law: dimap f g . dimap h k = dimap (h . f) (g . k)
            (define-test fn-dimap-composition-test
              (let* ([base (lambda (x) (+ x 10))]
                     [f add1]
                     [g (lambda (x) (* x 2))]
                     [h (lambda (x) (- x 1))]
                     [k (lambda (x) (+ x 100))]
                     [lhs (fn-dimap f g (fn-dimap h k base))]
                     [rhs (fn-dimap (lambda (x) (h (f x)))
                                    (lambda (x) (g (k x)))
                                    base)])
                    (assert-equal (lhs 10) (rhs 10)))))

;;; ============================================================
;;; Tagged Profunctor Tests
;;; ============================================================

(test-group tagged-profunctor
            (define-test make-tagged-test
              (let ([t (make-tagged 42)])
                   (assert-true (tagged? t))
                   (assert-equal 42 (untag t))))
            
            (define-test tagged-dimap-test
              (let* ([t (make-tagged 21)]
                     [t2 (tagged-dimap add1 (lambda (x) (* x 2)) t)])
                    ;; First arg is ignored
                    (assert-equal 42 (untag t2))))
            
            (define-test tagged-lmap-test
              (let* ([t (make-tagged 42)]
                     [t2 (tagged-lmap add1 t)])
                    ;; lmap is identity for Tagged
                    (assert-equal 42 (untag t2))))
            
            (define-test tagged-rmap-test
              (let* ([t (make-tagged 21)]
                     [t2 (tagged-rmap (lambda (x) (* x 2)) t)])
                    (assert-equal 42 (untag t2)))))

;;; ============================================================
;;; Star Profunctor Tests
;;; ============================================================

(test-group star-profunctor
            ;; Helper: Maybe fmap
            (define (maybe-fmap f m)
              (if (nothing? m) m (just (f (from-just m)))))
            
            (define-test make-star-test
              (let ([s (make-star (lambda (x) (just (* x 2))))])
                   (assert-true (star? s))))
            
            (define-test run-star-test
              (let ([s (make-star (lambda (x) (just (* x 2))))])
                   (let ([result (run-star s 21)])
                        (assert-true (just? result))
                        (assert-equal 42 (from-just result)))))
            
            (define-test star-safe-head-test
              (let ([safe-head (make-star (lambda (lst)
                                                  (if (null? lst)
                                                      nothing
                                                      (just (car lst)))))])
                   (let ([r1 (run-star safe-head '(1 2 3))]
                         [r2 (run-star safe-head '())])
                        (assert-true (just? r1))
                        (assert-equal 1 (from-just r1))
                        (assert-true (nothing? r2)))))
            
            (define-test star-dimap-test
              (let* ([s (make-star (lambda (x) (just (* x 2))))]
                     [s2 (star-dimap maybe-fmap
                                     string->number
                                     number->string
                                     s)])
                    (let ([result (run-star s2 "21")])
                         (assert-true (just? result))
                         (assert-equal "42" (from-just result)))))
            
            (define-test star-lmap-test
              (let* ([s (make-star (lambda (x) (just (* x 2))))]
                     [s2 (star-lmap string->number s)])
                    (let ([result (run-star s2 "21")])
                         (assert-true (just? result))
                         (assert-equal 42 (from-just result)))))
            
            (define-test star-rmap-test
              (let* ([s (make-star (lambda (x) (just (* x 2))))]
                     [s2 (star-rmap maybe-fmap number->string s)])
                    (let ([result (run-star s2 21)])
                         (assert-true (just? result))
                         (assert-equal "42" (from-just result))))))

;;; ============================================================
;;; Costar Profunctor Tests
;;; ============================================================

(test-group costar-profunctor
            (define-test make-costar-test
              (let ([cs (make-costar (lambda (lst) (apply + lst)))])
                   (assert-true (costar? cs))))
            
            (define-test run-costar-test
              (let ([cs (make-costar (lambda (lst) (apply + lst)))])
                   (assert-equal 15 (run-costar cs '(1 2 3 4 5)))))
            
            (define-test costar-dimap-test
              (let* ([cs (make-costar (lambda (lst) (apply + lst)))]
                     [cs2 (costar-dimap map
                                        string->number
                                        number->string
                                        cs)])
                    (assert-equal "15" (run-costar cs2 '("1" "2" "3" "4" "5"))))))

;;; ============================================================
;;; Strong Profunctor Tests
;;; ============================================================

(test-group strong-profunctor
            (define-test fn-first-test
              (let ([f (fn-first add1)])
                   (assert-equal (cons 6 "hello") (f (cons 5 "hello")))))
            
            (define-test fn-second-test
              (let ([f (fn-second add1)])
                   (assert-equal (cons "hello" 6) (f (cons "hello" 5)))))
            
            (define-test fn-first-preserves-second-test
              (let ([f (fn-first (lambda (x) (* x 2)))])
                   (let ([result (f (cons 5 'preserved))])
                        (assert-equal 10 (car result))
                        (assert-equal 'preserved (cdr result)))))
            
            (define-test fn-second-preserves-first-test
              (let ([f (fn-second (lambda (x) (* x 2)))])
                   (let ([result (f (cons 'preserved 5))])
                        (assert-equal 'preserved (car result))
                        (assert-equal 10 (cdr result)))))
            
            ;; Helper for Star tests
            (define (maybe-fmap f m)
              (if (nothing? m) m (just (f (from-just m)))))
            
            (define-test star-first-test
              (let* ([s (make-star (lambda (x) (just (* x 2))))]
                     [s2 (star-first maybe-fmap s)]
                     [result (run-star s2 (cons 5 "hello"))])
                    (assert-true (just? result))
                    (let ([pair (from-just result)])
                         (assert-equal 10 (car pair))
                         (assert-equal "hello" (cdr pair)))))
            
            (define-test star-second-test
              (let* ([s (make-star (lambda (x) (just (* x 2))))]
                     [s2 (star-second maybe-fmap s)]
                     [result (run-star s2 (cons "hello" 5))])
                    (assert-true (just? result))
                    (let ([pair (from-just result)])
                         (assert-equal "hello" (car pair))
                         (assert-equal 10 (cdr pair))))))

;;; ============================================================
;;; Choice Profunctor Tests
;;; ============================================================

(test-group choice-profunctor
            (define-test fn-left-on-left-test
              (let ([f (fn-left add1)])
                   (let ([result (f (left 5))])
                        (assert-true (left? result))
                        (assert-equal 6 (from-left result)))))
            
            (define-test fn-left-on-right-test
              (let ([f (fn-left add1)])
                   (let ([result (f (right "unchanged"))])
                        (assert-true (right? result))
                        (assert-equal "unchanged" (from-right result)))))
            
            (define-test fn-right-on-left-test
              (let ([f (fn-right add1)])
                   (let ([result (f (left "unchanged"))])
                        (assert-true (left? result))
                        (assert-equal "unchanged" (from-left result)))))
            
            (define-test fn-right-on-right-test
              (let ([f (fn-right add1)])
                   (let ([result (f (right 5))])
                        (assert-true (right? result))
                        (assert-equal 6 (from-right result)))))
            
            ;; Helpers for Star tests
            (define (maybe-fmap f m)
              (if (nothing? m) m (just (f (from-just m)))))
            
            (define-test star-left-on-left-test
              (let* ([s (make-star (lambda (x) (just (* x 2))))]
                     [s2 (star-left maybe-fmap just s)]
                     [result (run-star s2 (left 5))])
                    (assert-true (just? result))
                    (let ([inner (from-just result)])
                         (assert-true (left? inner))
                         (assert-equal 10 (from-left inner)))))
            
            (define-test star-left-on-right-test
              (let* ([s (make-star (lambda (x) (just (* x 2))))]
                     [s2 (star-left maybe-fmap just s)]
                     [result (run-star s2 (right "unchanged"))])
                    (assert-true (just? result))
                    (let ([inner (from-just result)])
                         (assert-true (right? inner))
                         (assert-equal "unchanged" (from-right inner)))))
            
            (define-test tagged-left-test
              (let* ([t (make-tagged 42)]
                     [t2 (tagged-left t)])
                    (let ([val (untag t2)])
                         (assert-true (left? val))
                         (assert-equal 42 (from-left val)))))
            
            (define-test tagged-right-test
              (let* ([t (make-tagged 42)]
                     [t2 (tagged-right t)])
                    (let ([val (untag t2)])
                         (assert-true (right? val))
                         (assert-equal 42 (from-right val))))))

;;; ============================================================
;;; Closed Profunctor Tests
;;; ============================================================

(test-group closed-profunctor
            (define-test fn-closed-test
              (let* ([double (lambda (x) (* x 2))]
                     [f (fn-closed double)]
                     [g (lambda (x) (+ x 10))]
                     [h (f g)])
                    ;; h should be: x -> double(g(x)) = x -> (x + 10) * 2
                    (assert-equal 30 (h 5))))  ; (5 + 10) * 2 = 30
            
            (define-test fn-closed-identity-test
              (let* ([f (fn-closed identity)]
                     [g (lambda (x) (* x 2))]
                     [h (f g)])
                    ;; Should be same as g
                    (assert-equal 10 (h 5)))))

;;; ============================================================
;;; Market Profunctor Tests
;;; ============================================================

(test-group market-profunctor
            (define-test make-market-test
              (let ([m (make-market add1 (lambda (x) (if (> x 0) (right x) (left 0))))])
                   (assert-true (market? m))))
            
            (define-test market-getter-setter-test
              (let ([m (make-market (lambda (x) (* x 2))
                                    (lambda (x) (if (even? x) (right (/ x 2)) (left x))))])
                   ;; Setter doubles
                   (assert-equal 10 ((market-setter m) 5))
                   ;; Getter halves evens
                   (let ([g (market-getter m)])
                        (let ([r1 (g 10)]
                              [r2 (g 11)])
                             (assert-true (right? r1))
                             (assert-equal 5 (from-right r1))
                             (assert-true (left? r2))
                             (assert-equal 11 (from-left r2))))))
            
            (define-test market-dimap-test
              (let* ([m (make-market identity
                                     (lambda (x) (if (> x 0) (right x) (left 0))))]
                     [m2 (market-dimap add1 (lambda (x) (* x 2)) m)])
                    ;; Setter now doubles
                    (assert-equal 10 ((market-setter m2) 5))
                    ;; Getter output gets add1
                    (let ([r ((market-getter m2) 5)])
                         (assert-true (right? r))
                         (assert-equal 6 (from-right r))))))

;;; ============================================================
;;; Exchange Profunctor Tests
;;; ============================================================

(test-group exchange-profunctor
            (define-test make-exchange-test
              (let ([e (make-exchange car (lambda (x) (cons x '())))])
                   (assert-true (exchange? e))))
            
            (define-test exchange-getter-setter-test
              (let ([e (make-exchange car (lambda (x) (cons x '())))])
                   (assert-equal 1 ((exchange-getter e) '(1 2 3)))
                   (assert-equal '(5) ((exchange-setter e) 5))))
            
            (define-test exchange-dimap-test
              (let* ([e (make-exchange car (lambda (x) (cons x '())))]
                     [e2 (exchange-dimap add1 (lambda (x) (* x 2)) e)])
                    ;; Getter now adds 1
                    (assert-equal 2 ((exchange-getter e2) '(1 2 3)))
                    ;; Setter input now gets doubled
                    (assert-equal '(10) ((exchange-setter e2) 5)))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
            ;; String parsing with profunctors
            (define-test parse-pipeline-test
              (let* ([parse-int (lambda (s) (string->number s))]
                     [show-int number->string]
                     [double (lambda (x) (* x 2))]
                     [pipeline (fn-dimap parse-int show-int double)])
                    (assert-equal "42" (pipeline "21"))))
            
            ;; Safe operations with Star
            (define (maybe-fmap f m)
              (if (nothing? m) m (just (f (from-just m)))))
            
            (define-test safe-division-test
              (let* ([safe-div (make-star (lambda (pair)
                                                  (let ([num (car pair)]
                                                        [denom (cdr pair)])
                                                       (if (= denom 0)
                                                           nothing
                                                           (just (/ num denom))))))]
                     [with-double (star-rmap maybe-fmap (lambda (x) (* x 2)) safe-div)])
                    (let ([r1 (run-star with-double (cons 10 2))]
                          [r2 (run-star with-double (cons 10 0))])
                         (assert-true (just? r1))
                         (assert-equal 10 (from-just r1))  ; (10/2)*2 = 10
                         (assert-true (nothing? r2)))))
            
            ;; Branching computation with Choice
            (define-test error-handling-pipeline-test
              (let* ([validate (lambda (x)
                                       (if (> x 0) (right x) (left "must be positive")))]
                     [process (fn-right (lambda (x) (* x 2)))]
                     [pipeline (lambda (x) (process (validate x)))])
                    (let ([r1 (pipeline 5)]
                          [r2 (pipeline -1)])
                         (assert-true (right? r1))
                         (assert-equal 10 (from-right r1))
                         (assert-true (left? r2))
                         (assert-equal "must be positive" (from-left r2)))))
            
            ;; Pair manipulation with Strong
            (define-test update-pair-first-test
              (let ([update-name (fn-first string-upcase)])
                   (assert-equal (cons "ALICE" 25) (update-name (cons "alice" 25)))))
            
            (define-test update-pair-second-test
              (let ([increment-age (fn-second add1)])
                   (assert-equal (cons "alice" 26) (increment-age (cons "alice" 25))))))

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
[SUCCESS] All profunctor tests passed.
")
    (display "
[FAILURE] Some profunctor tests failed.
"))
