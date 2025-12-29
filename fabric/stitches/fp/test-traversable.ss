;;; fabric/stitches/fp/test-traversable.ss — Tests for Traversable and Foldable

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/traversable.ss")

(display "\n")
(display "==============================================================\n")
(display "         TRAVERSABLE AND FOLDABLE TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Monoid Tests
;;; ============================================================

(test-group monoids
  (define-test list-monoid-test
    (assert-equal '() (monoid-empty list-monoid))
    (assert-equal '(1 2 3 4) ((monoid-append list-monoid) '(1 2) '(3 4))))

  (define-test sum-monoid-test
    (assert-equal 0 (monoid-empty sum-monoid))
    (assert-equal 10 ((monoid-append sum-monoid) 3 7)))

  (define-test product-monoid-test
    (assert-equal 1 (monoid-empty product-monoid))
    (assert-equal 21 ((monoid-append product-monoid) 3 7)))

  (define-test and-monoid-test
    (assert-true (monoid-empty and-monoid))
    (assert-false ((monoid-append and-monoid) #t #f))
    (assert-true ((monoid-append and-monoid) #t #t)))

  (define-test or-monoid-test
    (assert-false (monoid-empty or-monoid))
    (assert-true ((monoid-append or-monoid) #t #f))
    (assert-false ((monoid-append or-monoid) #f #f)))

  (define-test first-monoid-test
    (assert-true (nothing? (monoid-empty first-monoid)))
    (let ([combined ((monoid-append first-monoid) (just 1) (just 2))])
      (assert-equal 1 (from-just combined)))
    (let ([combined ((monoid-append first-monoid) nothing (just 2))])
      (assert-equal 2 (from-just combined))))

  (define-test last-monoid-test
    (assert-true (nothing? (monoid-empty last-monoid)))
    (let ([combined ((monoid-append last-monoid) (just 1) (just 2))])
      (assert-equal 2 (from-just combined)))
    (let ([combined ((monoid-append last-monoid) (just 1) nothing)])
      (assert-equal 1 (from-just combined))))

  (define-test endo-monoid-test
    (let ([f ((monoid-append endo-monoid) add1 (lambda (x) (* x 2)))])
      ;; f(x) = add1((* x 2)) = x*2 + 1
      (assert-equal 11 (f 5))))

  (define-test dual-monoid-test
    (let ([dual (dual-monoid list-monoid)])
      (assert-equal '(3 4 1 2) ((monoid-append dual) '(1 2) '(3 4))))))

;;; ============================================================
;;; Applicative Tests
;;; ============================================================

(test-group applicatives
  (define-test identity-applicative-test
    (assert-equal 42 ((app-pure identity-applicative) 42))
    (assert-equal 6 ((app-ap identity-applicative) add1 5)))

  (define-test maybe-applicative-pure-test
    (let ([result ((app-pure maybe-applicative) 42)])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test maybe-applicative-ap-test
    (let ([result ((app-ap maybe-applicative) (just add1) (just 5))])
      (assert-true (just? result))
      (assert-equal 6 (from-just result))))

  (define-test maybe-applicative-ap-nothing-test
    (assert-true (nothing? ((app-ap maybe-applicative) nothing (just 5))))
    (assert-true (nothing? ((app-ap maybe-applicative) (just add1) nothing))))

  (define-test list-applicative-test
    (let ([result ((app-ap list-applicative)
                   (list add1 (lambda (x) (* x 2)))
                   (list 1 2))])
      ;; Cartesian: [1+1, 2+1, 1*2, 2*2] = [2, 3, 2, 4]
      (assert-equal '(2 3 2 4) result)))

  (define-test either-applicative-right-test
    (let ([result ((app-ap either-applicative) (right add1) (right 5))])
      (assert-true (right? result))
      (assert-equal 6 (from-right result))))

  (define-test either-applicative-left-test
    (assert-true (left? ((app-ap either-applicative)
                         (left "error") (right 5))))
    (assert-true (left? ((app-ap either-applicative)
                         (right add1) (left "error")))))

  (define-test app-fmap-test
    (let ([result (app-fmap maybe-applicative add1 (just 5))])
      (assert-true (just? result))
      (assert-equal 6 (from-just result))))

  (define-test app-lift2-test
    ;; app-lift2 expects curried function
    (let ([result (app-lift2 maybe-applicative
                             (lambda (a) (lambda (b) (+ a b)))
                             (just 3) (just 4))])
      (assert-true (just? result))
      (assert-equal 7 (from-just result))))

  (define-test app-lift2-uncurried-test
    ;; app-lift2-uncurried works with Scheme's multi-arg functions
    (let ([result (app-lift2-uncurried maybe-applicative + (just 3) (just 4))])
      (assert-true (just? result))
      (assert-equal 7 (from-just result)))))

;;; ============================================================
;;; Foldable List Tests
;;; ============================================================

(test-group foldable-list
  (define-test list-foldr-test
    (assert-equal 15 (list-foldr + 0 '(1 2 3 4 5)))
    (assert-equal '(1 2 3) (list-foldr cons '() '(1 2 3))))

  (define-test list-foldl-test
    (assert-equal 15 (list-foldl + 0 '(1 2 3 4 5)))
    (assert-equal '(3 2 1) (list-foldl (lambda (acc x) (cons x acc)) '() '(1 2 3))))

  (define-test list-fold-map-test
    (assert-equal 15 (list-fold-map sum-monoid identity '(1 2 3 4 5)))
    (assert-equal '(1 2 3 4 5 6) (list-fold-map list-monoid list '(1 2 3 4 5 6))))

  (define-test list-fold-test
    (assert-equal 15 (list-fold sum-monoid '(1 2 3 4 5)))
    (assert-equal "abc" (list-fold string-monoid '("a" "b" "c"))))

  (define-test list-to-list-test
    (assert-equal '(1 2 3) (list-to-list '(1 2 3)))))

;;; ============================================================
;;; Foldable Maybe Tests
;;; ============================================================

(test-group foldable-maybe
  (define-test maybe-foldr-just-test
    (assert-equal 10 (maybe-foldr + 5 (just 5))))

  (define-test maybe-foldr-nothing-test
    (assert-equal 5 (maybe-foldr + 5 nothing)))

  (define-test maybe-fold-map-test
    (assert-equal 10 (maybe-fold-map sum-monoid (lambda (x) (* x 2)) (just 5)))
    (assert-equal 0 (maybe-fold-map sum-monoid (lambda (x) (* x 2)) nothing)))

  (define-test maybe-to-list-test
    (assert-equal '(42) (maybe-to-list (just 42)))
    (assert-equal '() (maybe-to-list nothing))))

;;; ============================================================
;;; Foldable Either Tests
;;; ============================================================

(test-group foldable-either
  (define-test either-foldr-right-test
    (assert-equal 10 (either-foldr + 5 (right 5))))

  (define-test either-foldr-left-test
    (assert-equal 5 (either-foldr + 5 (left "error"))))

  (define-test either-fold-map-test
    (assert-equal 10 (either-fold-map sum-monoid identity (right 10)))
    (assert-equal 0 (either-fold-map sum-monoid identity (left "error"))))

  (define-test either-to-list-test
    (assert-equal '(42) (either-to-list (right 42)))
    (assert-equal '() (either-to-list (left "error")))))

;;; ============================================================
;;; Foldable Pair Tests
;;; ============================================================

(test-group foldable-pair
  (define-test pair-foldr-test
    (assert-equal 15 (pair-foldr + 10 (cons "ignored" 5))))

  (define-test pair-fold-map-test
    (assert-equal 10 (pair-fold-map sum-monoid identity (cons "ignored" 10)))))

;;; ============================================================
;;; Traversable List Tests
;;; ============================================================

(test-group traversable-list
  (define-test list-traverse-maybe-success-test
    (let ([result (list-traverse maybe-applicative
                                 (lambda (x) (if (> x 0) (just x) nothing))
                                 '(1 2 3))])
      (assert-true (just? result))
      (assert-equal '(1 2 3) (from-just result))))

  (define-test list-traverse-maybe-failure-test
    (let ([result (list-traverse maybe-applicative
                                 (lambda (x) (if (> x 0) (just x) nothing))
                                 '(1 -2 3))])
      (assert-true (nothing? result))))

  (define-test list-sequence-maybe-success-test
    (let ([result (list-sequence maybe-applicative
                                 (list (just 1) (just 2) (just 3)))])
      (assert-true (just? result))
      (assert-equal '(1 2 3) (from-just result))))

  (define-test list-sequence-maybe-failure-test
    (let ([result (list-sequence maybe-applicative
                                 (list (just 1) nothing (just 3)))])
      (assert-true (nothing? result))))

  (define-test list-traverse-identity-test
    (let ([result (list-traverse identity-applicative add1 '(1 2 3))])
      (assert-equal '(2 3 4) result)))

  (define-test list-traverse-list-test
    ;; Cartesian product
    (let ([result (list-traverse list-applicative
                                 (lambda (x) (list x (- x)))
                                 '(1 2))])
      ;; [[1,2], [1,-2], [-1,2], [-1,-2]]
      (assert-equal '((1 2) (1 -2) (-1 2) (-1 -2)) result)))

  (define-test list-for-test
    (let ([result (list-for maybe-applicative '(1 2 3)
                            (lambda (x) (just (* x 2))))])
      (assert-true (just? result))
      (assert-equal '(2 4 6) (from-just result))))

  (define-test list-traverse-empty-test
    (let ([result (list-traverse maybe-applicative
                                 (lambda (x) (just x))
                                 '())])
      (assert-true (just? result))
      (assert-equal '() (from-just result)))))

;;; ============================================================
;;; Traversable Maybe Tests
;;; ============================================================

(test-group traversable-maybe
  (define-test maybe-traverse-just-test
    (let ([result (maybe-traverse maybe-applicative
                                  (lambda (x) (just (* x 2)))
                                  (just 5))])
      (assert-true (just? result))
      (let ([inner (from-just result)])
        (assert-true (just? inner))
        (assert-equal 10 (from-just inner)))))

  (define-test maybe-traverse-nothing-test
    (let ([result (maybe-traverse maybe-applicative
                                  (lambda (x) (just (* x 2)))
                                  nothing)])
      (assert-true (just? result))
      (assert-true (nothing? (from-just result)))))

  (define-test maybe-sequence-test
    (let ([result (maybe-sequence maybe-applicative (just (just 42)))])
      (assert-true (just? result))
      (let ([inner (from-just result)])
        (assert-true (just? inner))
        (assert-equal 42 (from-just inner))))))

;;; ============================================================
;;; Traversable Either Tests
;;; ============================================================

(test-group traversable-either
  (define-test either-traverse-right-test
    (let ([result (either-traverse maybe-applicative
                                   (lambda (x) (just (* x 2)))
                                   (right 5))])
      (assert-true (just? result))
      (let ([inner (from-just result)])
        (assert-true (right? inner))
        (assert-equal 10 (from-right inner)))))

  (define-test either-traverse-left-test
    (let ([result (either-traverse maybe-applicative
                                   (lambda (x) (just (* x 2)))
                                   (left "error"))])
      (assert-true (just? result))
      (let ([inner (from-just result)])
        (assert-true (left? inner))
        (assert-equal "error" (from-left inner))))))

;;; ============================================================
;;; Traversable Pair Tests
;;; ============================================================

(test-group traversable-pair
  (define-test pair-traverse-test
    (let ([result (pair-traverse maybe-applicative
                                 (lambda (x) (just (* x 2)))
                                 (cons "key" 5))])
      (assert-true (just? result))
      (let ([pair (from-just result)])
        (assert-equal "key" (car pair))
        (assert-equal 10 (cdr pair))))))

;;; ============================================================
;;; Derived Foldable Operations Tests
;;; ============================================================

(test-group derived-foldable
  (define-test foldable-null-test
    (assert-true (foldable-null? list-foldr '()))
    (assert-false (foldable-null? list-foldr '(1 2 3))))

  (define-test foldable-length-test
    (assert-equal 0 (foldable-length list-foldr '()))
    (assert-equal 5 (foldable-length list-foldr '(1 2 3 4 5))))

  (define-test foldable-elem-test
    (assert-true (foldable-elem? list-foldr 3 '(1 2 3 4 5)))
    (assert-false (foldable-elem? list-foldr 6 '(1 2 3 4 5))))

  (define-test foldable-find-test
    (let ([result (foldable-find list-foldr even? '(1 3 4 5 6))])
      (assert-true (just? result))
      (assert-equal 4 (from-just result)))
    (assert-true (nothing? (foldable-find list-foldr even? '(1 3 5)))))

  (define-test foldable-all-test
    (assert-true (foldable-all list-foldr positive? '(1 2 3 4 5)))
    (assert-false (foldable-all list-foldr positive? '(1 2 -3 4 5))))

  (define-test foldable-any-test
    (assert-true (foldable-any list-foldr negative? '(1 2 -3 4 5)))
    (assert-false (foldable-any list-foldr negative? '(1 2 3 4 5))))

  (define-test foldable-sum-test
    (assert-equal 15 (foldable-sum list-foldr '(1 2 3 4 5)))
    (assert-equal 0 (foldable-sum list-foldr '())))

  (define-test foldable-product-test
    (assert-equal 120 (foldable-product list-foldr '(1 2 3 4 5)))
    (assert-equal 1 (foldable-product list-foldr '())))

  (define-test foldable-maximum-test
    (let ([result (foldable-maximum list-foldr '(3 1 4 1 5 9 2 6))])
      (assert-true (just? result))
      (assert-equal 9 (from-just result)))
    (assert-true (nothing? (foldable-maximum list-foldr '()))))

  (define-test foldable-minimum-test
    (let ([result (foldable-minimum list-foldr '(3 1 4 1 5 9 2 6))])
      (assert-true (just? result))
      (assert-equal 1 (from-just result)))
    (assert-true (nothing? (foldable-minimum list-foldr '())))))

;;; ============================================================
;;; Scan Operations Tests
;;; ============================================================

(test-group scan-operations
  (define-test scan-left-test
    (assert-equal '(0 1 3 6 10) (scan-left + 0 '(1 2 3 4))))

  (define-test scan-right-test
    (assert-equal '(10 9 7 4 0) (scan-right + 0 '(1 2 3 4)))))

;;; ============================================================
;;; Indexed Traversal Tests
;;; ============================================================

(test-group indexed-traversals
  (define-test list-itraverse-test
    (let ([result (list-itraverse maybe-applicative
                                  (lambda (i x) (just (cons i x)))
                                  '(a b c))])
      (assert-true (just? result))
      (assert-equal '((0 . a) (1 . b) (2 . c)) (from-just result))))

  (define-test list-ifoldr-test
    (let ([result (list-ifoldr
                   (lambda (i x acc) (cons (cons i x) acc))
                   '()
                   '(a b c))])
      (assert-equal '((0 . a) (1 . b) (2 . c)) result)))

  (define-test list-ifoldl-test
    (let ([result (list-ifoldl
                   (lambda (i acc x) (cons (cons i x) acc))
                   '()
                   '(a b c))])
      (assert-equal '((2 . c) (1 . b) (0 . a)) result))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
  ;; Validate a list of inputs
  (define-test validate-list-test
    (let ([validate (lambda (x)
                      (if (> x 0)
                          (right x)
                          (left "must be positive")))]
          [app either-applicative])
      (let ([good (list-traverse app validate '(1 2 3))]
            [bad (list-traverse app validate '(1 -2 3))])
        (assert-true (right? good))
        (assert-equal '(1 2 3) (from-right good))
        (assert-true (left? bad))
        (assert-equal "must be positive" (from-left bad)))))

  ;; Parse and transform data
  (define-test parse-numbers-test
    (let ([parse-num (lambda (s)
                       (let ([n (string->number s)])
                         (if n (just n) nothing)))]
          [app maybe-applicative])
      (let ([good (list-traverse app parse-num '("1" "2" "3"))]
            [bad (list-traverse app parse-num '("1" "x" "3"))])
        (assert-true (just? good))
        (assert-equal '(1 2 3) (from-just good))
        (assert-true (nothing? bad)))))

  ;; Fold to extract information
  (define-test extract-info-test
    ;; Find first even, last odd, count total
    (let* ([nums '(1 2 3 4 5 6 7)]
           [first-even (foldable-find list-foldr even? nums)]
           [sum (foldable-sum list-foldr nums)])
      (assert-equal 2 (from-just first-even))
      (assert-equal 28 sum)))

  ;; Filter with validation
  (define-test filter-with-validation-test
    (let* ([validate-positive (lambda (x)
                                (if (> x 0) (right #t) (left "negative")))]
           [result (list-filter-with-effect either-applicative
                                            validate-positive
                                            '(1 2 3))])
      (assert-true (right? result))
      (assert-equal '(1 2 3) (from-right result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All traversable tests passed.\n")
    (display "\n[FAILURE] Some traversable tests failed.\n"))
