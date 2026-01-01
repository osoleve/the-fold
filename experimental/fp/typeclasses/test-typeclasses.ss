;;; fabric/stitches/fp/test-typeclasses.ss — Tests for Typeclass Hierarchy
;;;
;;; Tests for Semigroupoid, Apply, Bind, and Selective typeclasses.

(load "core/test-framework.ss")
(load "core/fp/typeclasses/typeclasses.ss")

(display "
")
(display "==============================================================
")
(display "         TYPECLASS HIERARCHY TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Semigroupoid Tests
;;; ============================================================

(test-group semigroupoid-tests
            
            (define-test semigroupoid-construction-test
              (assert-true (semigroupoid? function-semigroupoid))
              (assert-false (semigroupoid? 42))
              (assert-false (semigroupoid? '(not a semigroupoid))))
            
            (define-test function-semigroupoid-compose-test
              (let* ([f (lambda (x) (+ x 1))]
                     [g (lambda (x) (* x 2))]
                     [composed (semigroup-compose function-semigroupoid f g)])
                    ;; (f . g)(3) = f(g(3)) = f(6) = 7
                    (assert-equal 7 (composed 3))))
            
            (define-test semigroupoid-associativity-test
              (let* ([f (lambda (x) (+ x 1))]
                     [g (lambda (x) (* x 2))]
                     [h (lambda (x) (- x 3))]
                     ;; (f . g) . h
                     [left (semigroup-compose function-semigroupoid
                                              (semigroup-compose function-semigroupoid f g)
                                              h)]
                     ;; f . (g . h)
                     [right (semigroup-compose function-semigroupoid
                                               f
                                               (semigroup-compose function-semigroupoid g h))])
                    ;; Should be equal for any input
                    (assert-equal (left 5) (right 5))
                    (assert-equal (left 10) (right 10)))))

;;; ============================================================
;;; Apply Tests
;;; ============================================================

(test-group apply-tests
            
            (define-test apply-construction-test
              (assert-true (apply? list-apply))
              (assert-true (apply? maybe-apply))
              (assert-true (apply? either-apply))
              (assert-false (apply? 42)))
            
            (define-test list-apply-ap-test
              (let ([result (ap-apply list-apply (list add1 (lambda (x) (* x 2))) '(1 2 3))])
                   ;; Cartesian product: [2,3,4, 2,4,6]
                   (assert-equal '(2 3 4 2 4 6) result)))
            
            (define-test maybe-apply-ap-test
              (assert-equal (just 6) (ap-apply maybe-apply (just add1) (just 5)))
              (assert-true (nothing? (ap-apply maybe-apply nothing (just 5))))
              (assert-true (nothing? (ap-apply maybe-apply (just add1) nothing))))
            
            (define-test either-apply-ap-test
              (assert-equal (right 6) (ap-apply either-apply (right add1) (right 5)))
              (assert-true (left? (ap-apply either-apply (left "err") (right 5))))
              (assert-true (left? (ap-apply either-apply (right add1) (left "err")))))
            
            (define-test lift-apply2-test
              (let ([curried-add (lambda (a) (lambda (b) (+ a b)))])
                   (assert-equal (just 7)
                                 (lift-apply2 maybe-apply curried-add (just 3) (just 4)))
                   (assert-true (nothing? (lift-apply2 maybe-apply curried-add nothing (just 4))))))

            (define-test lift4-test
              (let ([curried-sum4 (lambda (a) (lambda (b) (lambda (c) (lambda (d) (+ a b c d)))))])
                   (assert-equal (just 10)
                                 (lift4 maybe-applicative curried-sum4 (just 1) (just 2) (just 3) (just 4)))
                   (assert-true (nothing? (lift4 maybe-applicative curried-sum4 nothing (just 2) (just 3) (just 4))))))

            (define-test lift5-test
              (let ([curried-sum5 (lambda (a) (lambda (b) (lambda (c) (lambda (d) (lambda (e) (+ a b c d e))))))])
                   (assert-equal (just 15)
                                 (lift5 maybe-applicative curried-sum5 (just 1) (just 2) (just 3) (just 4) (just 5)))
                   (assert-true (nothing? (lift5 maybe-applicative curried-sum5 (just 1) nothing (just 3) (just 4) (just 5))))))

            (define-test lift6-test
              (let ([curried-sum6 (lambda (a) (lambda (b) (lambda (c) (lambda (d) (lambda (e) (lambda (f) (+ a b c d e f)))))))])
                   (assert-equal (just 21)
                                 (lift6 maybe-applicative curried-sum6 (just 1) (just 2) (just 3) (just 4) (just 5) (just 6)))
                   (assert-true (nothing? (lift6 maybe-applicative curried-sum6 (just 1) (just 2) nothing (just 4) (just 5) (just 6)))))))

;;; ============================================================
;;; Bind Tests
;;; ============================================================

(test-group bind-tests
            
            (define-test bind-construction-test
              (assert-true (bind? list-bind))
              (assert-true (bind? maybe-bind))
              (assert-true (bind? either-bind))
              (assert-false (bind? 42)))
            
            (define-test list-bind-seq-test
              (let ([result (bind-seq list-bind '(1 2 3) (lambda (x) (list x (* x 10))))])
                   (assert-equal '(1 10 2 20 3 30) result)))
            
            (define-test maybe-bind-seq-test
              (assert-equal (just 6) (bind-seq maybe-bind (just 5) (lambda (x) (just (+ x 1)))))
              (assert-true (nothing? (bind-seq maybe-bind nothing (lambda (x) (just (+ x 1))))))
              (assert-true (nothing? (bind-seq maybe-bind (just 5) (lambda (x) nothing)))))
            
            (define-test either-bind-seq-test
              (assert-equal (right 6) (bind-seq either-bind (right 5) (lambda (x) (right (+ x 1)))))
              (assert-true (left? (bind-seq either-bind (left "err") (lambda (x) (right (+ x 1)))))))
            
            (define-test bind-then-test
              (assert-equal (just 42) (bind-then maybe-bind (just 1) (just 42)))
              (assert-true (nothing? (bind-then maybe-bind nothing (just 42)))))
            
            (define-test join-bind-test
              (assert-equal (just 5) (join-bind maybe-bind (just (just 5))))
              (assert-true (nothing? (join-bind maybe-bind (just nothing))))
              (assert-true (nothing? (join-bind maybe-bind nothing)))))

;;; ============================================================
;;; Selective Tests
;;; ============================================================

(test-group selective-tests
            
            (define-test selective-construction-test
              (assert-true (selective? maybe-selective))
              (assert-true (selective? either-selective))
              (assert-true (selective? function-selective))
              (assert-false (selective? 42)))
            
            ;; Identity law: select (pure (Right b)) f = pure b
            (define-test selective-identity-law-test
              (let ([result (select maybe-selective (just (right 42)) (just add1))])
                   (assert-true (just? result))
                   (assert-equal 42 (from-just result))))
            
            ;; Distributivity: select (pure (Left a)) (pure f) = pure (f a)
            (define-test selective-distributivity-law-test
              (let ([result (select maybe-selective (just (left 5)) (just add1))])
                   (assert-true (just? result))
                   (assert-equal 6 (from-just result))))
            
            (define-test maybe-selective-nothing-test
              (assert-true (nothing? (select maybe-selective nothing (just add1))))
              ;; When value is Right, function is not used (per identity law)
              (assert-equal (just 5) (select maybe-selective (just (right 5)) nothing))
              ;; When value is Left and function is nothing, result is nothing
              (assert-true (nothing? (select maybe-selective (just (left 5)) nothing))))
            
            (define-test either-selective-test
              ;; Right passes through
              (assert-equal (right 42) (select either-selective (right (right 42)) (right add1)))
              ;; Left applies function
              (assert-equal (right 6) (select either-selective (right (left 5)) (right add1)))
              ;; Error propagates
              (assert-true (left? (select either-selective (left "err") (right add1)))))
            
            (define-test function-selective-test
              (let* ([feab (lambda (r) (if (> r 0) (right r) (left (- r))))]
                     [fab (lambda (r) (lambda (a) (* a 2)))]
                     [sel-fn (select function-selective feab fab)])
                    ;; Positive: Right path, returns r
                    (assert-equal 5 (sel-fn 5))
                    ;; Negative: Left path, applies function
                    (assert-equal 6 (sel-fn -3)))))  ; (- -3) = 3, * 2 = 6

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
