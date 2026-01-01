;;; fabric/stitches/fp/test-algebraic.ss — Tests for Algebraic Typeclasses
;;;
;;; Tests for Semigroup, Monoid, Group and related structures.

(load "core/test-framework.ss")
(load "core/fp/typeclasses/algebraic.ss")

(display "
")
(display "==============================================================
")
(display "         ALGEBRAIC TYPECLASSES TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Semigroup Tests
;;; ============================================================

(test-group semigroup-tests
            
            (define-test semigroup-construction-test
              (let ([sg (make-semigroup 'test +)])
                   (assert-true (semigroup? sg))
                   (assert-equal 'test (semigroup-name sg))))
            
            (define-test sg-append-list-test
              (assert-equal '(1 2 3 4) (sg-append list-semigroup '(1 2) '(3 4))))
            
            (define-test sg-append-string-test
              (assert-equal "HelloWorld" (sg-append string-semigroup "Hello" "World")))
            
            (define-test sconcat-list-test
              (assert-equal '(1 2 3) (sconcat list-semigroup '((1) (2) (3)))))
            
            (define-test sconcat-string-test
              (assert-equal "abc" (sconcat string-semigroup '("a" "b" "c"))))
            
            (define-test stimes-list-test
              (assert-equal '(1 1 1) (stimes list-semigroup 3 '(1))))
            
            (define-test stimes-string-test
              (assert-equal "xxxx" (stimes string-semigroup 4 "x")))
            
            (define-test semigroup-associativity-list-test
              (assert-true (verify-semigroup-law list-semigroup '(1) '(2) '(3))))
            
            (define-test semigroup-associativity-string-test
              (assert-true (verify-semigroup-law string-semigroup "a" "b" "c")))
            
            ) ; end semigroup-tests

;;; ============================================================
;;; Monoid Tests
;;; ============================================================

(test-group monoid-tests
            
            (define-test monoid-construction-test
              (let ([m (make-monoid 'test 0 +)])
                   (assert-true (monoid? m))
                   (assert-equal 'test (monoid-name m))
                   (assert-equal 0 (monoid-empty m))))
            
            (define-test list-monoid-test
              (assert-equal '() (monoid-empty list-monoid))
              (assert-equal '(1 2 3 4) (mappend list-monoid '(1 2) '(3 4)))
              (assert-equal '(1 2 3) (mconcat list-monoid '((1) (2) (3)))))
            
            (define-test string-monoid-test
              (assert-equal "" (monoid-empty string-monoid))
              (assert-equal "foobar" (mappend string-monoid "foo" "bar"))
              (assert-equal "abc" (mconcat string-monoid '("a" "b" "c"))))
            
            (define-test sum-monoid-test
              (assert-equal 0 (monoid-empty sum-monoid))
              (assert-equal 7 (mappend sum-monoid 3 4))
              (assert-equal 15 (mconcat sum-monoid '(1 2 3 4 5))))
            
            (define-test product-monoid-test
              (assert-equal 1 (monoid-empty product-monoid))
              (assert-equal 12 (mappend product-monoid 3 4))
              (assert-equal 120 (mconcat product-monoid '(1 2 3 4 5))))
            
            (define-test all-monoid-test
              (assert-equal #t (monoid-empty all-monoid))
              (assert-equal #t (mappend all-monoid #t #t))
              (assert-equal #f (mappend all-monoid #t #f))
              (assert-equal #t (mconcat all-monoid '(#t #t #t)))
              (assert-equal #f (mconcat all-monoid '(#t #f #t))))
            
            (define-test any-monoid-test
              (assert-equal #f (monoid-empty any-monoid))
              (assert-equal #f (mappend any-monoid #f #f))
              (assert-equal #t (mappend any-monoid #f #t))
              (assert-equal #f (mconcat any-monoid '(#f #f #f)))
              (assert-equal #t (mconcat any-monoid '(#f #t #f))))
            
            (define-test max-monoid-test
              (assert-equal -inf.0 (monoid-empty max-monoid))
              (assert-equal 5 (mappend max-monoid 3 5))
              ;; mconcat starts with -inf.0, so result is flonum
              (assert-true (= 5 (mconcat max-monoid '(1 5 3 2 4)))))
            
            (define-test min-monoid-test
              (assert-equal +inf.0 (monoid-empty min-monoid))
              (assert-equal 3 (mappend min-monoid 3 5))
              ;; mconcat starts with +inf.0, so result is flonum
              (assert-true (= 1 (mconcat min-monoid '(1 5 3 2 4)))))
            
            (define-test endo-monoid-test
              (let* ([e1 (mappend endo-monoid add1 add1)]
                     [e2 (mconcat endo-monoid (list add1 add1 add1))])
                    (assert-equal 2 (e1 0))
                    (assert-equal 3 (e2 0))))
            
            (define-test monoid-identity-list-test
              (assert-true (verify-monoid-laws list-monoid '(1 2 3))))
            
            (define-test monoid-identity-sum-test
              (assert-true (verify-monoid-laws sum-monoid 42)))
            
            (define-test monoid-identity-product-test
              (assert-true (verify-monoid-laws product-monoid 7)))
            
            (define-test monoid-identity-string-test
              (assert-true (verify-monoid-laws string-monoid "test")))
            
            (define-test mconcat-empty-list-test
              (assert-equal '() (mconcat list-monoid '()))
              (assert-equal 0 (mconcat sum-monoid '()))
              (assert-equal 1 (mconcat product-monoid '())))
            
            ) ; end monoid-tests

;;; ============================================================
;;; Group Tests
;;; ============================================================

(test-group group-tests
            
            (define-test group-construction-test
              (let ([g (make-group 'test 0 + -)])
                   (assert-true (group? g))
                   (assert-equal 'test (group-name g))
                   (assert-equal 0 (group-identity g))))
            
            (define-test sum-group-test
              (assert-equal 0 (group-identity sum-group))
              (assert-equal -5 (group-inverse sum-group 5))
              (assert-equal 7 (group-diff sum-group 10 3)))
            
            (define-test nonzero-product-group-test
              (assert-equal 1 (group-identity nonzero-product-group))
              (assert-equal 1/2 (group-inverse nonzero-product-group 2))
              (assert-equal 3 (group-diff nonzero-product-group 6 2)))
            
            (define-test xor-group-test
              (assert-equal #f (group-identity xor-group))
              (assert-equal #t (group-inverse xor-group #t))
              (assert-equal #f (group-inverse xor-group #f))
              (assert-equal #f ((group-op xor-group) #t #t))
              (assert-equal #t ((group-op xor-group) #t #f)))
            
            (define-test group-inverse-sum-test
              (assert-true (verify-group-laws sum-group 42))
              (assert-true (verify-group-laws sum-group -17)))
            
            (define-test group-inverse-product-test
              (assert-true (verify-group-laws nonzero-product-group 3)))
            
            (define-test group-inverse-xor-test
              (assert-true (verify-group-laws xor-group #t))
              (assert-true (verify-group-laws xor-group #f)))
            
            (define-test power-group-positive-test
              (assert-equal 15 (power-group sum-group 3 5))
              (assert-equal 0 (power-group sum-group 0 5)))
            
            (define-test power-group-negative-test
              (assert-equal -10 (power-group sum-group -2 5)))
            
            (define-test group-to-monoid-test
              (let ([m (group->monoid sum-group)])
                   (assert-true (monoid? m))
                   (assert-equal 0 (monoid-empty m))
                   (assert-equal 7 (mappend m 3 4))))
            
            ) ; end group-tests

;;; ============================================================
;;; Wrapper Types Tests
;;; ============================================================

(test-group wrapper-types-tests
            
            (define-test dual-reverses-test
              (let ([dual-list (dual-monoid list-monoid)])
                   (assert-equal '(3 2 1)
                                 (get-dual
                                  (mconcat dual-list
                                           (list (make-dual '(1))
                                                 (make-dual '(2))
                                                 (make-dual '(3))))))))
            
            (define-test first-gets-first-test
              (let ([f1 (make-first nothing)]
                    [f2 (make-first (just 1))]
                    [f3 (make-first (just 2))])
                   (assert-equal (just 1)
                                 (get-first (mconcat first-monoid (list f1 f2 f3))))
                   (assert-equal nothing
                                 (get-first (mconcat first-monoid (list f1 f1))))))
            
            (define-test last-gets-last-test
              (let ([l1 (make-last (just 1))]
                    [l2 (make-last nothing)]
                    [l3 (make-last (just 2))])
                   (assert-equal (just 2)
                                 (get-last (mconcat last-monoid (list l1 l2 l3))))
                   (assert-equal nothing
                                 (get-last (mconcat last-monoid (list l2 l2))))))
            
            (define-test sum-wrapper-test
              (let ([s1 (make-sum 3)]
                    [s2 (make-sum 4)])
                   (assert-equal 7 (get-sum (mappend sum-wrapped-monoid s1 s2)))
                   (assert-equal 0 (get-sum (monoid-empty sum-wrapped-monoid)))))
            
            (define-test product-wrapper-test
              (let ([p1 (make-product 3)]
                    [p2 (make-product 4)])
                   (assert-equal 12 (get-product (mappend product-wrapped-monoid p1 p2)))
                   (assert-equal 1 (get-product (monoid-empty product-wrapped-monoid)))))
            
            (define-test endo-wrapper-test
              (let* ([e1 (make-endo add1)]
                     [e2 (make-endo (lambda (x) (* x 2)))]
                     [combined (mappend endo-wrapped-monoid e1 e2)])
                    (assert-equal 11 (app-endo combined 5))))  ; add1(5*2) = 11
            
            ) ; end wrapper-types-tests

;;; ============================================================
;;; Tuple Instances Tests
;;; ============================================================

(test-group tuple-instances-tests
            
            (define-test pair-monoid-test
              (let ([pm (pair-monoid sum-monoid product-monoid)])
                   (assert-equal '(0 . 1) (monoid-empty pm))
                   (assert-equal '(4 . 8) (mappend pm '(1 . 2) '(3 . 4)))
                   (assert-equal '(9 . 48) (mconcat pm '((1 . 2) (3 . 4) (5 . 6))))))
            
            (define-test triple-monoid-test
              (let ([tm (triple-monoid sum-monoid product-monoid list-monoid)])
                   (assert-equal '(0 1 ()) (monoid-empty tm))
                   (assert-equal '(4 8 (a b)) (mappend tm '(1 2 (a)) '(3 4 (b))))))
            
            ) ; end tuple-instances-tests

;;; ============================================================
;;; Maybe Instance Tests
;;; ============================================================

(test-group maybe-instances-tests
            
            (define-test maybe-semigroup-combine-test
              (let ([ms (maybe-semigroup sum-semigroup)])
                   (assert-equal (just 7) (sg-append ms (just 3) (just 4)))
                   (assert-equal (just 4) (sg-append ms nothing (just 4)))
                   (assert-equal (just 3) (sg-append ms (just 3) nothing))
                   (assert-equal nothing (sg-append ms nothing nothing))))
            
            (define-test maybe-monoid-test
              (let ([mm (maybe-monoid sum-semigroup)])
                   (assert-equal nothing (monoid-empty mm))
                   (assert-equal (just 3)
                                 (mconcat mm (list (just 1) nothing (just 2))))
                   (assert-equal nothing
                                 (mconcat mm (list nothing nothing)))))
            
            ) ; end maybe-instances-tests

;;; ============================================================
;;; Generic Folding Tests
;;; ============================================================

(test-group generic-folding-tests
            
            (define-test fold-map-sum-test
              (assert-equal 6 (fold-map sum-monoid length '((1 2) (3 4 5) (6)))))
            
            (define-test fold-map-string-test
              (assert-equal "123" (fold-map string-monoid number->string '(1 2 3))))
            
            (define-test fold-map-empty-test
              (assert-equal 0 (fold-map sum-monoid length '()))
              (assert-equal '() (fold-map list-monoid list '())))
            
            (define-test when-monoid-test
              (assert-equal 42 (when-monoid sum-monoid #t 42))
              (assert-equal 0 (when-monoid sum-monoid #f 42))
              (assert-equal '(1 2 3) (when-monoid list-monoid #t '(1 2 3)))
              (assert-equal '() (when-monoid list-monoid #f '(1 2 3))))
            
            (define-test replicate-monoid-test
              ;; list-monoid concatenates, so replicate 3 '(x) gives (x x x)
              (assert-equal '(x x x) (replicate-monoid list-monoid 3 '(x)))
              (assert-equal 15 (replicate-monoid sum-monoid 5 3))
              (assert-equal '() (replicate-monoid list-monoid 0 '(x))))
            
            ) ; end generic-folding-tests

;;; ============================================================
;;; Ordering Monoid Tests
;;; ============================================================

(test-group ordering-monoid-tests
            
            (define-test ordering-identity-test
              (assert-equal 'EQ (monoid-empty ordering-monoid)))
            
            (define-test ordering-combination-test
              (assert-equal 'LT (mappend ordering-monoid 'LT 'GT))
              (assert-equal 'GT (mappend ordering-monoid 'GT 'LT))
              (assert-equal 'LT (mappend ordering-monoid 'EQ 'LT))
              (assert-equal 'GT (mappend ordering-monoid 'EQ 'GT))
              (assert-equal 'EQ (mappend ordering-monoid 'EQ 'EQ)))
            
            (define-test lexicographic-ordering-test
              (assert-equal 'LT (mconcat ordering-monoid '(EQ EQ LT)))
              (assert-equal 'LT (mconcat ordering-monoid '(EQ LT GT))))
            
            ) ; end ordering-monoid-tests

;;; ============================================================
;;; Abelian (Commutativity) Tests
;;; ============================================================

(test-group abelian-tests
            
            (define-test sum-commutative-test
              (assert-true (verify-commutativity + 3 5))
              (assert-true (verify-commutativity + 0 42))
              (assert-true (verify-commutativity + -7 7)))
            
            (define-test product-commutative-test
              (assert-true (verify-commutativity * 3 5))
              (assert-true (verify-commutativity * 1 42)))
            
            (define-test max-min-commutative-test
              (assert-true (verify-commutativity max 3 5))
              (assert-true (verify-commutativity min 3 5)))
            
            (define-test xor-commutative-test
              (assert-true (verify-commutativity bool-xor #t #f))
              (assert-true (verify-commutativity bool-xor #t #t)))
            
            ) ; end abelian-tests

;;; ============================================================
;;; Monoid Action Tests
;;; ============================================================

(test-group monoid-action-tests
            
            (define-test list-take-action-test
              (assert-equal '(a b c) (action-act list-take-action 3 '(a b c d e)))
              (assert-equal '() (action-act list-take-action 0 '(a b c)))
              (assert-equal '(a b) (action-act list-take-action 10 '(a b))))
            
            ) ; end monoid-action-tests

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
