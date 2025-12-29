;;; fabric/stitches/fp/test-bifunctor.ss — Tests for Bifunctor Library

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/bifunctor.ss")

(display "
")
(display "==============================================================
")
(display "         BIFUNCTOR TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Pair Bifunctor Tests
;;; ============================================================

(test-group pair-bifunctor-tests
            (define-test pair-bimap-test
              (assert-equal (cons 2 5) (pair-bimap add1 (lambda (s) (string-length s)) (cons 1 "hello"))))
            
            (define-test pair-first-test
              (assert-equal (cons 10 "x") (pair-map-first (lambda (x) (* x 2)) (cons 5 "x"))))
            
            (define-test pair-second-test
              (assert-equal (cons 1 100) (pair-map-second (lambda (x) (* x 10)) (cons 1 10))))
            
            (define-test pair-identity-law-test
              (assert-true (verify-bifunctor-identity pair-bifunctor (cons 1 2))))
            
            (define-test pair-first-identity-law-test
              (assert-true (verify-bifunctor-first-identity pair-bifunctor (cons "a" "b"))))
            
            (define-test pair-second-identity-law-test
              (assert-true (verify-bifunctor-second-identity pair-bifunctor (cons 1 2))))
            
            (define-test pair-composition-law-test
              (assert-true (verify-bifunctor-composition pair-bifunctor add1 (lambda (x) (* x 2)) (cons 5 10)))))

;;; ============================================================
;;; Either Bifunctor Tests
;;; ============================================================

(test-group either-bifunctor-tests
            (define-test either-bimap-left-test
              (assert-equal (left 10) (either-bimap (lambda (x) (* x 2)) add1 (left 5))))
            
            (define-test either-bimap-right-test
              (assert-equal (right 6) (either-bimap (lambda (x) (* x 2)) add1 (right 5))))
            
            (define-test either-map-left-test
              (assert-equal (left "ERROR") (either-map-left (lambda (s) (string-upcase s)) (left "error"))))
            
            (define-test either-map-left-noop-test
              (assert-equal (right 5) (either-map-left add1 (right 5))))
            
            (define-test either-map-right-test
              (assert-equal (right 10) (either-map-right (lambda (x) (* x 2)) (right 5))))
            
            (define-test either-map-right-noop-test
              (assert-equal (left "err") (either-map-right add1 (left "err"))))
            
            (define-test either-identity-law-left-test
              (assert-true (verify-bifunctor-identity either-bifunctor (left 1))))
            
            (define-test either-identity-law-right-test
              (assert-true (verify-bifunctor-identity either-bifunctor (right 2))))
            
            (define-test either-composition-law-test
              (assert-true (verify-bifunctor-composition either-bifunctor add1 (lambda (x) (* x 2)) (right 5)))))

;;; ============================================================
;;; Validation Bifunctor Tests
;;; ============================================================

(test-group validation-bifunctor-tests
            (define-test validation-bimap-success-test
              (assert-equal (validation-mk-success 10)
                            (validation-bimap add1 (lambda (x) (* x 2)) (validation-mk-success 5))))
            
            (define-test validation-bimap-failure-test
              (let ([result (validation-bimap (lambda (s) (string-upcase s)) add1
                                              (validation-mk-failure '("err1" "err2")))])
                   (assert-true (validation-is-failure? result))
                   (assert-equal '("ERR1" "ERR2") (from-validation-failure result))))
            
            (define-test validation-map-errors-test
              (let ([result (validation-map-errors (lambda (s) (string-append "prefix:" s))
                                                   (validation-mk-failure '("a" "b")))])
                   (assert-equal '("prefix:a" "prefix:b") (from-validation-failure result))))
            
            (define-test validation-map-success-test
              (assert-equal (validation-mk-success 25)
                            (validation-map-success (lambda (x) (* x x)) (validation-mk-success 5))))
            
            (define-test validation-identity-law-success-test
              (assert-true (verify-bifunctor-identity validation-bifunctor (validation-mk-success 42))))
            
            (define-test validation-identity-law-failure-test
              (assert-true (verify-bifunctor-identity validation-bifunctor (validation-mk-failure '("e"))))))

;;; ============================================================
;;; These Bifunctor Tests
;;; ============================================================

(test-group these-bifunctor-tests
            (define-test this-construction-test
              (let ([t (make-this 1)])
                   (assert-true (this? t))
                   (assert-false (that? t))
                   (assert-false (these? t))
                   (assert-equal 1 (from-this t))))
            
            (define-test that-construction-test
              (let ([t (make-that "x")])
                   (assert-false (this? t))
                   (assert-true (that? t))
                   (assert-false (these? t))
                   (assert-equal "x" (from-that t))))
            
            (define-test these-construction-test
              (let ([t (make-these 1 "x")])
                   (assert-false (this? t))
                   (assert-false (that? t))
                   (assert-true (these? t))
                   (assert-equal 1 (from-these-fst t))
                   (assert-equal "x" (from-these-snd t))))
            
            (define-test these-bimap-this-test
              (assert-equal (make-this 2) (these-bimap add1 (lambda (s) (string-upcase s)) (make-this 1))))
            
            (define-test these-bimap-that-test
              (assert-equal (make-that "HI") (these-bimap add1 (lambda (s) (string-upcase s)) (make-that "hi"))))
            
            (define-test these-bimap-both-test
              (assert-equal (make-these 2 "HI") (these-bimap add1 (lambda (s) (string-upcase s)) (make-these 1 "hi"))))
            
            (define-test these-identity-law-this-test
              (assert-true (verify-bifunctor-identity these-bifunctor (make-this 1))))
            
            (define-test these-identity-law-that-test
              (assert-true (verify-bifunctor-identity these-bifunctor (make-that "x"))))
            
            (define-test these-identity-law-both-test
              (assert-true (verify-bifunctor-identity these-bifunctor (make-these 1 "x")))))

;;; ============================================================
;;; Derived Operations Tests
;;; ============================================================

(test-group derived-operations-tests
            (define-test void-left-test
              (assert-equal (cons 0 "hello") (bf-void-left pair-bifunctor 0 (cons 999 "hello"))))
            
            (define-test void-right-test
              (assert-equal (cons 1 'unit) (bf-void-right pair-bifunctor 'unit (cons 1 "ignored")))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
