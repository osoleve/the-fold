;;; fabric/stitches/fp/test-laws.ss — Tests for Law Checker
;;;
;;; Tests that the law checker correctly validates typeclass instances.

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/laws.ss")

(display "
")
(display "==============================================================
")
(display "         LAW CHECKER TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Test Infrastructure
;;; ============================================================

(test-group law-infrastructure-tests

            (define-test make-law-result-test
              (let ([r (make-law-result 'test-law #t "passed")])
                   (assert-true (law-result? r))
                   (assert-equal 'test-law (law-result-name r))
                   (assert-true (law-result-passed? r))
                   (assert-equal "passed" (law-result-message r))))

            (define-test all-laws-passed-empty-test
              (assert-true (all-laws-passed? '())))

            (define-test all-laws-passed-all-pass-test
              (assert-true
               (all-laws-passed?
                (list (make-law-result 'a #t "ok")
                      (make-law-result 'b #t "ok")))))

            (define-test all-laws-passed-one-fail-test
              (assert-false
               (all-laws-passed?
                (list (make-law-result 'a #t "ok")
                      (make-law-result 'b #f "fail"))))))

;;; ============================================================
;;; Functor Laws Tests
;;; ============================================================

(test-group functor-laws-tests

            (define-test maybe-functor-identity-test
              (let ([results (check-functor-laws maybe-functor gen-maybe-int maybe-eq 50)])
                   (assert-true (>= (length results) 1))
                   (assert-true (law-result-passed? (car results)))))

            (define-test list-functor-identity-test
              (let ([results (check-functor-laws list-functor gen-list-int equal? 50)])
                   (assert-true (all-laws-passed? results)))))

;;; ============================================================
;;; Applicative Laws Tests
;;; ============================================================

(test-group applicative-laws-tests

            (define-test maybe-applicative-identity-test
              (let ([results (check-applicative-laws
                              maybe-applicative
                              gen-maybe-int
                              (gen-map just (gen-pure add1))
                              maybe-eq
                              50)])
                   (assert-true (>= (length results) 1))
                   (assert-true (law-result-passed? (car results)))))

            (define-test maybe-applicative-homomorphism-test
              (let ([results (check-applicative-laws
                              maybe-applicative
                              gen-maybe-int
                              (gen-map just (gen-pure add1))
                              maybe-eq
                              50)])
                   (assert-true (>= (length results) 2)))))

;;; ============================================================
;;; Monad Laws Tests
;;; ============================================================

(test-group monad-laws-tests

            (define-test maybe-monad-left-identity-test
              (let ([results (check-monad-laws
                              maybe-monad
                              gen-maybe-int
                              (gen-pure (lambda (x) (just (add1 x))))
                              maybe-eq
                              50)])
                   (assert-true (>= (length results) 1))
                   (assert-true (law-result-passed? (car results)))))

            (define-test maybe-monad-right-identity-test
              (let ([results (check-monad-laws
                              maybe-monad
                              gen-maybe-int
                              (gen-pure (lambda (x) (just (add1 x))))
                              maybe-eq
                              50)])
                   (assert-true (>= (length results) 2))
                   (assert-true (law-result-passed? (cadr results)))))

            (define-test maybe-monad-associativity-test
              (let ([results (check-monad-laws
                              maybe-monad
                              gen-maybe-int
                              (gen-pure (lambda (x) (just (add1 x))))
                              maybe-eq
                              50)])
                   (assert-true (>= (length results) 3))
                   (assert-true (law-result-passed? (caddr results))))))

;;; ============================================================
;;; Monoid Laws Tests
;;; ============================================================

(test-group monoid-laws-tests

            (define-test list-monoid-identity-test
              (let ([results (check-monoid-laws list-monoid gen-list-int equal? 50)])
                   (assert-true (>= (length results) 2))
                   (assert-true (law-result-passed? (car results)))
                   (assert-true (law-result-passed? (cadr results)))))

            (define-test list-monoid-associativity-test
              (let ([results (check-monoid-laws list-monoid gen-list-int equal? 50)])
                   (assert-true (>= (length results) 3))
                   (assert-true (law-result-passed? (caddr results)))))

            (define-test sum-monoid-all-laws-test
              (let ([results (check-monoid-laws sum-monoid gen-int = 50)])
                   (assert-true (all-laws-passed? results))))

            (define-test product-monoid-all-laws-test
              (let ([results (check-monoid-laws product-monoid gen-int = 50)])
                   (assert-true (all-laws-passed? results)))))

;;; ============================================================
;;; Semigroup Laws Tests
;;; ============================================================

(test-group semigroup-laws-tests

            (define-test list-semigroup-associativity-test
              (let ([results (check-semigroup-laws list-semigroup gen-list-int equal? 50)])
                   (assert-true (all-laws-passed? results))))

            (define-test sum-semigroup-associativity-test
              (let ([sg (make-semigroup 'sum +)])
                   (let ([results (check-semigroup-laws sg gen-int = 50)])
                        (assert-true (all-laws-passed? results))))))

;;; ============================================================
;;; Semigroupoid Laws Tests
;;; ============================================================

(test-group semigroupoid-laws-tests

            (define-test function-semigroupoid-associativity-test
              (let ([results (check-semigroupoid-laws-fn 50)])
                   (assert-true (all-laws-passed? results)))))

;;; ============================================================
;;; Selective Laws Tests
;;; ============================================================

(test-group selective-laws-tests

            (define-test maybe-selective-identity-test
              (let ([results (check-selective-laws maybe-selective gen-int maybe-eq 50)])
                   (assert-true (>= (length results) 1))
                   (assert-true (law-result-passed? (car results)))))

            (define-test maybe-selective-distributivity-test
              (let ([results (check-selective-laws maybe-selective gen-int maybe-eq 50)])
                   (assert-true (>= (length results) 2))
                   (assert-true (law-result-passed? (cadr results))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
