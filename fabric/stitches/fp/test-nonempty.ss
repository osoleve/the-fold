;;; fabric/stitches/fp/test-nonempty.ss - Tests for NonEmpty List Type

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/nonempty.ss")

(display "
")
(display "==============================================================
")
(display "         NONEMPTY LIST TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Construction and Basic Operations Tests
;;; ============================================================

(test-group construction-tests
            
            (define-test make-nonempty-test
              (let ([ne (make-nonempty 1 '(2 3))])
                   (assert-true (nonempty? ne))
                   (assert-equal 1 (nonempty-head ne))
                   (assert-equal '(2 3) (nonempty-tail ne))))
            
            (define-test singleton-test
              (let ([ne (ne-singleton 42)])
                   (assert-true (nonempty? ne))
                   (assert-equal 42 (nonempty-head ne))
                   (assert-equal '() (nonempty-tail ne))))
            
            (define-test nonempty-varargs-test
              (let ([ne (nonempty 1 2 3 4)])
                   (assert-equal 1 (nonempty-head ne))
                   (assert-equal '(2 3 4) (nonempty-tail ne))))
            
            (define-test nonempty-predicate-test
              (assert-true (nonempty? (nonempty 1)))
              (assert-false (nonempty? '(1 2 3)))
              (assert-false (nonempty? 42))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group conversion-tests
            
            (define-test nonempty->list-test
              (assert-equal '(1 2 3) (nonempty->list (nonempty 1 2 3))))
            
            (define-test list->nonempty-empty-test
              (assert-true (nothing? (list->nonempty '()))))
            
            (define-test list->nonempty-nonempty-test
              (let ([result (list->nonempty '(1 2 3))])
                   (assert-true (just? result))
                   (assert-equal '(1 2 3) (nonempty->list (from-just result)))))
            
            (define-test round-trip-test
              (let* ([original '(1 2 3 4 5)]
                     [ne (ne-from-list original)]
                     [back (nonempty->list ne)])
                    (assert-equal original back))))

;;; ============================================================
;;; Basic Operations Tests
;;; ============================================================

(test-group basic-operations-tests
            
            (define-test ne-length-singleton-test
              (assert-equal 1 (ne-length (ne-singleton 1))))
            
            (define-test ne-length-multiple-test
              (assert-equal 5 (ne-length (nonempty 1 2 3 4 5))))
            
            (define-test ne-last-singleton-test
              (assert-equal 42 (ne-last (ne-singleton 42))))
            
            (define-test ne-last-multiple-test
              (assert-equal 5 (ne-last (nonempty 1 2 3 4 5))))
            
            (define-test ne-init-singleton-test
              (assert-equal '() (ne-init (ne-singleton 1))))
            
            (define-test ne-init-multiple-test
              (assert-equal '(1 2 3 4) (ne-init (nonempty 1 2 3 4 5))))
            
            (define-test ne-reverse-test
              (assert-equal '(5 4 3 2 1) (nonempty->list (ne-reverse (nonempty 1 2 3 4 5)))))
            
            (define-test ne-cons-test
              (assert-equal '(0 1 2 3) (nonempty->list (ne-cons 0 (nonempty 1 2 3)))))
            
            (define-test ne-snoc-test
              (assert-equal '(1 2 3 4) (nonempty->list (ne-snoc (nonempty 1 2 3) 4)))))

;;; ============================================================
;;; Functor Tests
;;; ============================================================

(test-group functor-tests
            
            (define-test ne-map-test
              (assert-equal '(2 4 6) (nonempty->list (ne-map (lambda (x) (* x 2)) (nonempty 1 2 3)))))
            
            (define-test functor-identity-test
              (let ([ne (nonempty 1 2 3)])
                   (assert-equal (nonempty->list ne)
                                 (nonempty->list (ne-map id ne)))))
            
            (define-test functor-composition-test
              (let ([ne (nonempty 1 2 3)]
                    [f add1]
                    [g (lambda (x) (* x 2))])
                   (assert-equal (nonempty->list (ne-map (compose g f) ne))
                                 (nonempty->list (ne-map g (ne-map f ne)))))))

;;; ============================================================
;;; Semigroup Tests
;;; ============================================================

(test-group semigroup-tests
            
            (define-test ne-append-test
              (let ([ne1 (nonempty 1 2)]
                    [ne2 (nonempty 3 4)])
                   (assert-equal '(1 2 3 4) (nonempty->list (ne-append ne1 ne2)))))
            
            (define-test semigroup-associativity-test
              (let ([a (nonempty 1 2)]
                    [b (nonempty 3 4)]
                    [c (nonempty 5 6)]
                    [op (semigroup-op nonempty-semigroup)])
                   (assert-equal (nonempty->list (op (op a b) c))
                                 (nonempty->list (op a (op b c)))))))

;;; ============================================================
;;; Foldable1 Tests
;;; ============================================================

(test-group foldable1-tests
            
            (define-test ne-foldr-test
              (assert-equal 15 (ne-foldr + 0 (nonempty 1 2 3 4 5))))
            
            (define-test ne-foldl-test
              (assert-equal 15 (ne-foldl + 0 (nonempty 1 2 3 4 5))))
            
            (define-test ne-foldr1-test
              (assert-equal 15 (ne-foldr1 + (nonempty 1 2 3 4 5))))
            
            (define-test ne-foldl1-test
              (assert-equal 15 (ne-foldl1 + (nonempty 1 2 3 4 5))))
            
            (define-test ne-foldr1-singleton-test
              (assert-equal 42 (ne-foldr1 + (ne-singleton 42))))
            
            (define-test ne-fold-map-test
              (assert-equal 15 (ne-fold-map sum-semigroup id (nonempty 1 2 3 4 5)))))

;;; ============================================================
;;; Apply Tests
;;; ============================================================

(test-group apply-tests
            
            (define-test ne-ap-test
              (let ([nef (nonempty add1 (lambda (x) (* x 2)))]
                    [nea (nonempty 1 2 3)])
                   ;; Cartesian product: [2,3,4, 2,4,6]
                   (assert-equal '(2 3 4 2 4 6) (nonempty->list (ne-ap nef nea)))))
            
            (define-test ne-lift2-test
              (let ([result (ne-lift2 (lambda (a) (lambda (b) (+ a b)))
                                      (nonempty 1 2)
                                      (nonempty 10 20))])
                   ;; 1+10, 1+20, 2+10, 2+20 = 11, 21, 12, 22
                   (assert-equal '(11 21 12 22) (nonempty->list result)))))

;;; ============================================================
;;; Bind Tests
;;; ============================================================

(test-group bind-tests
            
            (define-test ne-bind-test
              (let ([result (ne-bind (nonempty 1 2 3)
                                     (lambda (x) (nonempty x (* x 10))))])
                   (assert-equal '(1 10 2 20 3 30) (nonempty->list result))))
            
            (define-test ne-join-test
              (let ([nested (nonempty (nonempty 1 2) (nonempty 3 4))])
                   (assert-equal '(1 2 3 4) (nonempty->list (ne-join nested))))))

;;; ============================================================
;;; Utility Function Tests
;;; ============================================================

(test-group utility-tests
            
            (define-test ne-take-test
              (assert-equal '(1 2 3) (ne-take 3 (nonempty 1 2 3 4 5))))
            
            (define-test ne-drop-test
              (assert-equal '(4 5) (ne-drop 3 (nonempty 1 2 3 4 5))))
            
            (define-test ne-zip-test
              (assert-equal '((1 . a) (2 . b))
                            (nonempty->list (ne-zip (nonempty 1 2 3) (nonempty 'a 'b)))))
            
            (define-test ne-zip-with-test
              (assert-equal '(11 22 33)
                            (nonempty->list (ne-zip-with + (nonempty 1 2 3) (nonempty 10 20 30)))))
            
            (define-test ne-filter-test
              (assert-equal '(2 4) (ne-filter even? (nonempty 1 2 3 4 5))))
            
            (define-test ne-maximum-test
              (assert-equal 5 (ne-maximum (nonempty 3 1 5 2 4))))
            
            (define-test ne-minimum-test
              (assert-equal 1 (ne-minimum (nonempty 3 1 5 2 4))))
            
            (define-test ne-sum-test
              (assert-equal 15 (ne-sum (nonempty 1 2 3 4 5))))
            
            (define-test ne-product-test
              (assert-equal 120 (ne-product (nonempty 1 2 3 4 5))))
            
            (define-test ne-nub-test
              (assert-equal '(1 2 3 2 1)
                            (nonempty->list (ne-nub (nonempty 1 1 2 2 2 3 3 2 1 1))))))

;;; ============================================================
;;; Comonad Tests
;;; ============================================================

(test-group comonad-tests
            
            (define-test ne-extract-test
              (assert-equal 1 (ne-extract (nonempty 1 2 3))))
            
            (define-test ne-tails-test
              (let ([tails (ne-tails (nonempty 1 2 3))])
                   (assert-equal 3 (ne-length tails))
                   (assert-equal '(1 2 3) (nonempty->list (nonempty-head tails)))))
            
            (define-test ne-extend-test
              (let ([result (ne-extend ne-sum (nonempty 1 2 3))])
                   ;; sums of [1,2,3], [2,3], [3] = 6, 5, 3
                   (assert-equal '(6 5 3) (nonempty->list result)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
