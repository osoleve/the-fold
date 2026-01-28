;;; fabric/stitches/fp/test-combinators.ss — Tests for FP Combinators

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "lattice/fp/meta/combinators.ss")

;;; Helper for numeric comparison
(define (assert-= actual expected tolerance)
  (unless (<= (abs (- actual expected)) tolerance)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (display " ± ")
          (display tolerance)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper for exact equality
(define (assert-eq? actual expected)
  (unless (eq? actual expected)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

;;; Helper for structural equality
(define (assert-equal? actual expected)
  (unless (equal? actual expected)
          (set! *tests-failed* (+ *tests-failed* 1))
          (display "    ✗ ")
          (display *current-test-name*)
          (newline)
          (display "      Expected: ")
          (display expected)
          (newline)
          (display "      Got:      ")
          (display actual)
          (newline)))

(display "
══════════════════════════════════════════════════════════
         FP COMBINATORS TESTS
══════════════════════════════════════════════════════════
")

;;; ====
;;; Classic Combinators Tests
;;; ====

(test-group classic-combinators
            (define-test id-test
              (assert-= (id 42) 42 0)
              (assert-equal? (id 'hello) 'hello)
              (assert-equal? (id '(1 2 3)) '(1 2 3)))
            
            (define-test const-test
              (assert-= ((const 5) 10) 5 0)
              (assert-= ((const 5) 'anything) 5 0))
            
            (define-test flip-test
              (let ([sub (flip -)])
                   (assert-= (sub 3 10) 7 0)))  ; 10 - 3 = 7
            
            (define-test apply-fn-test
              (assert-= (apply-fn (lambda (x) (* x 2)) 5) 10 0)))

;;; ====
;;; Composition Tests
;;; ====

(test-group composition
            (define-test compose2-test
              (let ([f (compose2 (lambda (x) (* x 2))
                                 (lambda (x) (+ x 1)))])
                   (assert-= (f 5) 12 0)))  ; (5+1)*2 = 12
            
            (define-test compose-test
              (let ([f (compose (lambda (x) (* x 2))
                                (lambda (x) (+ x 1))
                                (lambda (x) (* x 3)))])
                   (assert-= (f 5) 32 0)))  ; ((5*3)+1)*2 = 32
            
            (define-test pipe2-test
              (let ([f (pipe2 (lambda (x) (+ x 1))
                              (lambda (x) (* x 2)))])
                   (assert-= (f 5) 12 0)))  ; (5+1)*2 = 12
            
            (define-test pipe-test
              (let ([f (pipe (lambda (x) (* x 3))
                             (lambda (x) (+ x 1))
                             (lambda (x) (* x 2)))])
                   (assert-= (f 5) 32 0)))  ; ((5*3)+1)*2 = 32
            
            (define-test empty-compose-test
              (assert-= ((compose) 42) 42 0))
            
            (define-test empty-pipe-test
              (assert-= ((pipe) 42) 42 0)))

;;; ====
;;; Currying Tests
;;; ====

(test-group currying
            (define-test curry2-test
              (let ([add (curry2 (lambda (x y) (+ x y)))])
                   (assert-= ((add 3) 4) 7 0)
                   (let ([add5 (add 5)])
                        (assert-= (add5 10) 15 0))))
            
            (define-test uncurry2-test
              ;; Note: uncurry2 works with (cons a b) pairs, not 2-element lists
              (let ([curried (lambda (x) (lambda (y) (+ x y)))]
                    [uncurried (uncurry2 (lambda (x) (lambda (y) (+ x y))))])
                   (assert-= (uncurried (cons 3 4)) 7 0)))
            
            (define-test curry3-test
              (let ([f (curry3 (lambda (x y z) (+ x (* y z))))])
                   (assert-= (((f 10) 3) 4) 22 0)))  ; 10 + 3*4 = 22
            
            (define-test partial-test
              (let ([add3 (partial + 3)])
                   (assert-= (add3 4) 7 0)
                   (assert-= (add3 4 5) 12 0)))
            
            (define-test partial2-test
              (let ([add7 (partial2 + 3 4)])
                   (assert-= (add7 5) 12 0))))

;;; ====
;;; Higher-Order Utilities Tests
;;; ====

(test-group higher-order
            (define-test on-test
              (let ([compare-by-length (on < length)])
                   (assert-true (compare-by-length '(1) '(1 2 3)))
                   (assert-false (compare-by-length '(1 2 3) '(1)))))
            
            ;; Note: both, pair-first, pair-second, bimap work with (cons a b) pairs
            (define-test both-test
              (let ([double-both ((both (lambda (x) (* x 2))) (cons 3 4))])
                   (assert-equal? double-both (cons 6 8))))
            
            (define-test pair-first-test
              (let ([result ((pair-first (lambda (x) (* x 2))) (cons 3 4))])
                   (assert-= (car result) 6 0)
                   (assert-= (cdr result) 4 0)))
            
            (define-test pair-second-test
              (let ([result ((pair-second (lambda (x) (* x 2))) (cons 3 4))])
                   (assert-= (car result) 3 0)
                   (assert-= (cdr result) 8 0)))
            
            (define-test bimap-test
              (let ([result ((bimap (lambda (x) (+ x 1))
                                    (lambda (x) (* x 2)))
                             (cons 3 4))])
                   (assert-equal? result (cons 4 8)))))

;;; ====
;;; Logical Combinators Tests
;;; ====

(test-group logical
            (define-test complement-test
              (let ([not-even? (complement even?)])
                   (assert-true (not-even? 3))
                   (assert-false (not-even? 4))))
            
            (define-test conjoin-test
              (let ([positive-even? (conjoin positive? even?)])
                   (assert-true (positive-even? 4))
                   (assert-false (positive-even? -4))
                   (assert-false (positive-even? 3))))
            
            (define-test disjoin-test
              (let ([extreme? (disjoin (lambda (x) (< x -10))
                                       (lambda (x) (> x 10)))])
                   (assert-true (extreme? 15))
                   (assert-true (extreme? -15))
                   (assert-false (extreme? 5)))))

;;; ====
;;; Tuple Tests
;;; ====

(test-group tuple
            ;; Note: fst, snd, swap, dup, pair work with (cons a b) pairs
            (define-test fst-snd-test
              (assert-= (fst (cons 1 2)) 1 0)
              (assert-= (snd (cons 1 2)) 2 0))
            
            (define-test swap-test
              (assert-equal? (swap (cons 1 2)) (cons 2 1)))
            
            (define-test dup-test
              (assert-equal? (dup 5) (cons 5 5)))
            
            (define-test pair-test
              (assert-equal? (pair 'a 'b) (cons 'a 'b))))

;;; ====
;;; Maybe Tests
;;; ====

(test-group maybe-type
            (define-test nothing-test
              (assert-true (nothing? nothing))
              (assert-false (just? nothing)))
            
            (define-test just-test
              (assert-true (just? (just 5)))
              (assert-false (nothing? (just 5)))
              (assert-= (from-just (just 5)) 5 0))
            
            (define-test maybe-test
              (assert-= (maybe 0 (lambda (x) (* x 2)) (just 5)) 10 0)
              (assert-= (maybe 0 (lambda (x) (* x 2)) nothing) 0 0))
            
            (define-test maybe-fmap-test
              (assert-= (from-just (maybe-fmap (lambda (x) (* x 2)) (just 5))) 10 0)
              (assert-true (nothing? (maybe-fmap (lambda (x) (* x 2)) nothing))))
            
            (define-test maybe-bind-test
              (let ([safe-div (lambda (n)
                                      (if (= n 0) nothing (just (/ 10 n))))])
                   (assert-= (from-just (maybe-bind (just 2) safe-div)) 5 0)
                   (assert-true (nothing? (maybe-bind (just 0) safe-div)))
                   (assert-true (nothing? (maybe-bind nothing safe-div)))))
            
            (define-test filter-maybe-test
              (assert-true (just? (filter-maybe positive? (just 5))))
              (assert-true (nothing? (filter-maybe positive? (just -5))))
              (assert-true (nothing? (filter-maybe positive? nothing))))
            
            (define-test cat-maybes-test
              (let ([result (cat-maybes (list (just 1) nothing (just 2) nothing (just 3)))])
                   (assert-equal? result '(1 2 3))))
            
            (define-test sequence-maybe-test
              (assert-equal? (from-just (sequence-maybe (list (just 1) (just 2) (just 3))))
                             '(1 2 3))
              (assert-true (nothing? (sequence-maybe (list (just 1) nothing (just 3)))))))

;;; ====
;;; Either Tests
;;; ====

(test-group either-type
            (define-test left-right-test
              (assert-true (left? (left 'error)))
              (assert-true (right? (right 5)))
              (assert-false (left? (right 5)))
              (assert-false (right? (left 'error))))
            
            (define-test from-left-right-test
              (assert-equal? (from-left (left 'error)) 'error)
              (assert-= (from-right (right 5)) 5 0))
            
            (define-test either-test
              (assert-= (either string-length (lambda (x) (* x 2)) (left "hi")) 2 0)
              (assert-= (either string-length (lambda (x) (* x 2)) (right 5)) 10 0))
            
            (define-test either-fmap-test
              (assert-= (from-right (either-fmap (lambda (x) (* x 2)) (right 5))) 10 0)
              (assert-equal? (from-left (either-fmap (lambda (x) (* x 2)) (left 'err))) 'err))
            
            (define-test either-fmap-left-test
              (assert-equal? (from-left (either-fmap-left (lambda (x) (cons 'wrapped x)) (left 'err)))
                             '(wrapped . err))
              (assert-= (from-right (either-fmap-left (lambda (x) (cons 'wrapped x)) (right 5))) 5 0))
            
            (define-test either-bind-test
              (let ([validate (lambda (x)
                                      (if (positive? x) (right (* x 2)) (left 'negative)))])
                   (assert-= (from-right (either-bind (right 5) validate)) 10 0)
                   (assert-equal? (from-left (either-bind (right -5) validate)) 'negative)
                   (assert-equal? (from-left (either-bind (left 'already-error) validate)) 'already-error)))
            
            (define-test rights-lefts-test
              (let ([es (list (right 1) (left 'a) (right 2) (left 'b) (right 3))])
                   (assert-equal? (rights es) '(1 2 3))
                   (assert-equal? (lefts es) '(a b))))
            
            (define-test partition-eithers-test
              (let* ([es (list (right 1) (left 'a) (right 2) (left 'b))]
                     [result (partition-eithers es)])
                    (assert-equal? (car result) '(a b))
                    (assert-equal? (cadr result) '(1 2)))))

;;; ====
;;; List Utilities Tests
;;; ====

(test-group list-utilities
            (define-test head-tail-test
              (assert-= (head '(1 2 3)) 1 0)
              (assert-equal? (tail '(1 2 3)) '(2 3)))
            
            (define-test cons-if-test
              (assert-equal? (cons-if #t 1 '(2 3)) '(1 2 3))
              (assert-equal? (cons-if #f 1 '(2 3)) '(2 3)))
            
            (define-test intersperse-test
              (assert-equal? (intersperse 0 '(1 2 3)) '(1 0 2 0 3))
              (assert-equal? (intersperse 0 '(1)) '(1))
              (assert-equal? (intersperse 0 '()) '()))
            
            (define-test intercalate-test
              (assert-equal? (intercalate '(0) '((1 2) (3 4) (5 6)))
                             '(1 2 0 3 4 0 5 6)))
            
            (define-test group-by-test
              (let ([result (group-by even? '(2 4 1 3 6 8 5))])
                   (assert-equal? result '((2 4) (1 3) (6 8) (5)))))
            
            (define-test unique-test
              (assert-equal? (unique '(1 1 2 2 2 3 3 1 1)) '(1 2 3 1)))
            
            (define-test unique-by-test
              (let ([result (unique-by even? '(2 4 1 3 6 8))])
                   (assert-equal? result '(2 1 6)))))

;;; ====
;;; Iteration Tests
;;; ====

(test-group iteration
            (define-test iterate-n-test
              (assert-equal? (iterate-n (lambda (x) (* x 2)) 1 5)
                             '(1 2 4 8 16)))
            
            (define-test unfold-test
              (let ([count-down (lambda (n)
                                        (if (= n 0)
                                            nothing
                                            (just (list n (- n 1)))))])
                   (assert-equal? (unfold count-down 5) '(5 4 3 2 1))))
            
            (define-test fix-with-tolerance-test
              ;; Find sqrt(2) by Newton's method
              (let* ([f (lambda (x) (/ (+ x (/ 2 x)) 2))]
                     [result (fix-with-tolerance f 1.0 0.0001 100)])
                    (assert-true (< (abs (- result 1.4142)) 0.001)))))

;;; ====
;;; Memoization Tests
;;; ====

(test-group memoization
            (define-test memoize-test
              (let* ([call-count 0]
                     [expensive (memoize (lambda (x)
                                                 (set! call-count (+ call-count 1))
                                                 (* x x)))])
                    (assert-= (expensive 5) 25 0)
                    (assert-= (expensive 5) 25 0)
                    (assert-= (expensive 5) 25 0)
                    (assert-= call-count 1 0)  ; Only called once
                    (assert-= (expensive 6) 36 0)
                    (assert-= call-count 2 0))))  ; Now called for 6

;;; ====
;;; Y Combinator Test
;;; ====

(test-group y-combinator
            (define-test y-factorial-test
              (let ([factorial (Y (lambda (f)
                                          (lambda (n)
                                                  (if (= n 0)
                                                      1
                                                      (* n (f (- n 1)))))))])
                   (assert-= (factorial 0) 1 0)
                   (assert-= (factorial 5) 120 0)
                   (assert-= (factorial 10) 3628800 0))))

;;; ====
;;; Do-Monad Tests
;;; ====

(test-group do-monad-tests
            ;; Test with Maybe monad
            (define-test do-monad-maybe-success-test
              (let ([result (do-monad maybe-bind
                                      [x <- (just 3)]
                                      [y <- (just 4)]
                                      (just (+ x y)))])
                   (assert-equal (just 7) result)))
            
            (define-test do-monad-maybe-failure-test
              (let ([result (do-monad maybe-bind
                                      [x <- (just 3)]
                                      [y <- nothing]
                                      (just (+ x y)))])
                   (assert-equal nothing result)))
            
            ;; Test sequencing (no binding)
            (define-test do-monad-sequence-test
              (let ([side-effect 0])
                   (define (set-effect! v)
                     (set! side-effect v)
                     (just v))
                   (let ([result (do-monad maybe-bind
                                           (set-effect! 1)
                                           [x <- (just 10)]
                                           (just (+ x side-effect)))])
                        (assert-equal (just 11) result))))
            
            ;; Test with list monad (non-determinism)
            (define-test do-monad-list-test
              (let* ([list-bind (lambda (m f) (append-map f m))]
                     [result (do-monad list-bind
                                       [x <- '(1 2)]
                                       [y <- '(10 20)]
                                       (list (+ x y)))])
                    (assert-equal '(11 21 12 22) result))))

;;; ====
;;; Summary
;;; ====

(display "
══════════════════════════════════════════════════════════
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All FP combinator tests passed.
")
    (display "
[FAILURE] Some FP combinator tests failed.
"))
