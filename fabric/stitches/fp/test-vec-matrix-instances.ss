;;; fabric/stitches/fp/test-vec-matrix-instances.ss -- Tests for Vec/Matrix Type Class Instances

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/vec-matrix-instances.ss")

(display "
")
(display "==============================================================
")
(display "         VEC/MATRIX TYPE CLASS INSTANCE TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Vec Functor Tests
;;; ============================================================

(test-group vec-functor-tests
            (define-test vec-fmap-empty-test
              (let ([result (vec-fmap (lambda (x) (* x 2)) (vec))])
                   (assert-equal 0 (vec-length result))))
            
            (define-test vec-fmap-singleton-test
              (let ([result (vec-fmap (lambda (x) (* x 2)) (vec 5))])
                   (assert-equal 1 (vec-length result))
                   (assert-equal 10 (vec-ref result 0))))
            
            (define-test vec-fmap-multiple-test
              (let ([result (vec-fmap (lambda (x) (* x x)) (vec 1 2 3 4))])
                   (assert-equal 4 (vec-length result))
                   (assert-equal 1 (vec-ref result 0))
                   (assert-equal 4 (vec-ref result 1))
                   (assert-equal 9 (vec-ref result 2))
                   (assert-equal 16 (vec-ref result 3))))
            
            (define-test vec-fmap-identity-test
              (let* ([v (vec 1 2 3)]
                     [result (vec-fmap identity v)])
                    (assert-equal 3 (vec-length result))
                    (assert-equal 1 (vec-ref result 0))
                    (assert-equal 2 (vec-ref result 1))
                    (assert-equal 3 (vec-ref result 2))))
            
            (define-test vec-functor-instance-test
              (assert-equal 'vec-functor (car vec-functor))
              (assert-true (procedure? (cdr vec-functor)))))

;;; ============================================================
;;; Vec Foldable Tests
;;; ============================================================

(test-group vec-foldable-tests
            (define-test vec-foldr-empty-test
              (assert-equal 0 (vec-foldr + 0 (vec))))
            
            (define-test vec-foldr-sum-test
              (assert-equal 10 (vec-foldr + 0 (vec 1 2 3 4))))
            
            (define-test vec-foldr-cons-test
              (let ([result (vec-foldr cons '() (vec 1 2 3))])
                   (assert-equal '(1 2 3) result)))
            
            (define-test vec-foldl-empty-test
              (assert-equal 0 (vec-foldl + 0 (vec))))
            
            (define-test vec-foldl-sum-test
              (assert-equal 10 (vec-foldl + 0 (vec 1 2 3 4))))
            
            (define-test vec-foldl-cons-test
              (let ([result (vec-foldl (lambda (acc x) (cons x acc)) '() (vec 1 2 3))])
                   (assert-equal '(3 2 1) result)))
            
            (define-test vec-sum-test
              (assert-equal 0 (vec-sum (vec)))
              (assert-equal 15 (vec-sum (vec 1 2 3 4 5))))
            
            (define-test vec-product-test
              (assert-equal 1 (vec-product (vec)))
              (assert-equal 120 (vec-product (vec 1 2 3 4 5))))
            
            (define-test vec-any-test
              (assert-false (vec-any (lambda (x) (> x 10)) (vec 1 2 3)))
              (assert-true (vec-any (lambda (x) (> x 2)) (vec 1 2 3))))
            
            (define-test vec-all-test
              (assert-true (vec-all (lambda (x) (> x 0)) (vec 1 2 3)))
              (assert-false (vec-all (lambda (x) (> x 2)) (vec 1 2 3))))
            
            (define-test vec-find-test
              (let ([result (vec-find (lambda (x) (> x 2)) (vec 1 2 3 4))])
                   (assert-true (just? result))
                   (assert-equal 3 (from-just result)))
              (let ([result (vec-find (lambda (x) (> x 10)) (vec 1 2 3))])
                   (assert-true (nothing? result))))
            
            (define-test vec-maximum-test
              (assert-true (nothing? (vec-maximum (vec))))
              (let ([result (vec-maximum (vec 3 1 4 1 5 9))])
                   (assert-true (just? result))
                   (assert-equal 9 (from-just result))))
            
            (define-test vec-minimum-test
              (assert-true (nothing? (vec-minimum (vec))))
              (let ([result (vec-minimum (vec 3 1 4 1 5 9))])
                   (assert-true (just? result))
                   (assert-equal 1 (from-just result)))))

;;; ============================================================
;;; Vec Applicative Tests
;;; ============================================================

(test-group vec-applicative-tests
            (define-test vec-pure-test
              (let ([result (vec-pure 42)])
                   (assert-equal 1 (vec-length result))
                   (assert-equal 42 (vec-ref result 0))))
            
            (define-test vec-ap-empty-test
              (let ([result (vec-ap (vec) (vec 1 2 3))])
                   (assert-equal 0 (vec-length result))))
            
            (define-test vec-ap-zipwith-test
              (let* ([vf (vec (lambda (x) (* x 2)) (lambda (x) (+ x 10)))]
                     [va (vec 5 3)]
                     [result (vec-ap vf va)])
                    (assert-equal 2 (vec-length result))
                    (assert-equal 10 (vec-ref result 0))  ; (* 5 2)
                    (assert-equal 13 (vec-ref result 1)))) ; (+ 3 10)
            
            (define-test vec-lift2-test
              (let ([result (vec-lift2 + (vec 1 2 3) (vec 10 20 30))])
                   (assert-equal 3 (vec-length result))
                   (assert-equal 11 (vec-ref result 0))
                   (assert-equal 22 (vec-ref result 1))
                   (assert-equal 33 (vec-ref result 2))))
            
            (define-test vec-lift2-different-lengths-test
              (let ([result (vec-lift2 * (vec 1 2 3 4 5) (vec 10 20))])
                   (assert-equal 2 (vec-length result))
                   (assert-equal 10 (vec-ref result 0))
                   (assert-equal 40 (vec-ref result 1))))
            
            (define-test vec-zip-test
              (let ([result (vec-zip (vec 1 2 3) (vec 'a 'b 'c))])
                   (assert-equal 3 (vec-length result))
                   (assert-equal '(1 . a) (vec-ref result 0))
                   (assert-equal '(2 . b) (vec-ref result 1))
                   (assert-equal '(3 . c) (vec-ref result 2)))))

;;; ============================================================
;;; Vec Traversable Tests
;;; ============================================================

(test-group vec-traversable-tests
            (define-test vec-traverse-maybe-all-just-test
              (let* ([f (lambda (x) (if (> x 0) (just x) nothing))]
                     [result (vec-traverse maybe-applicative f (vec 1 2 3))])
                    (assert-true (just? result))
                    (let ([inner (from-just result)])
                         (assert-equal 3 (vec-length inner))
                         (assert-equal 1 (vec-ref inner 0))
                         (assert-equal 2 (vec-ref inner 1))
                         (assert-equal 3 (vec-ref inner 2)))))
            
            (define-test vec-traverse-maybe-with-nothing-test
              (let* ([f (lambda (x) (if (> x 0) (just x) nothing))]
                     [result (vec-traverse maybe-applicative f (vec 1 -2 3))])
                    (assert-true (nothing? result))))
            
            (define-test vec-traverse-empty-test
              (let* ([f (lambda (x) (just (* x 2)))]
                     [result (vec-traverse maybe-applicative f (vec))])
                    (assert-true (just? result))
                    (assert-equal 0 (vec-length (from-just result)))))
            
            (define-test vec-sequence-test
              (let ([result (vec-sequence maybe-applicative (vec (just 1) (just 2) (just 3)))])
                   (assert-true (just? result))
                   (let ([inner (from-just result)])
                        (assert-equal 3 (vec-length inner)))))
            
            (define-test vec-sequence-with-nothing-test
              (let ([result (vec-sequence maybe-applicative (vec (just 1) nothing (just 3)))])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Vec Append Tests
;;; ============================================================

(test-group vec-append-tests
            (define-test vec-append-empty-left-test
              (let ([result (vec-append (vec) (vec 1 2 3))])
                   (assert-equal 3 (vec-length result))
                   (assert-equal 1 (vec-ref result 0))))
            
            (define-test vec-append-empty-right-test
              (let ([result (vec-append (vec 1 2 3) (vec))])
                   (assert-equal 3 (vec-length result))
                   (assert-equal 3 (vec-ref result 2))))
            
            (define-test vec-append-both-nonempty-test
              (let ([result (vec-append (vec 1 2) (vec 3 4 5))])
                   (assert-equal 5 (vec-length result))
                   (assert-equal 1 (vec-ref result 0))
                   (assert-equal 2 (vec-ref result 1))
                   (assert-equal 3 (vec-ref result 2))
                   (assert-equal 4 (vec-ref result 3))
                   (assert-equal 5 (vec-ref result 4)))))

;;; ============================================================
;;; Matrix Functor Tests
;;; ============================================================

(test-group matrix-functor-tests
            (define-test matrix-fmap-test
              (let* ([m (matrix-from-lists '((1 2) (3 4)))]
                     [result (matrix-fmap (lambda (x) (* x 2)) m)])
                    (assert-equal 2 (matrix-rows result))
                    (assert-equal 2 (matrix-cols result))
                    (assert-equal 2 (matrix-ref result 0 0))
                    (assert-equal 4 (matrix-ref result 0 1))
                    (assert-equal 6 (matrix-ref result 1 0))
                    (assert-equal 8 (matrix-ref result 1 1))))
            
            (define-test matrix-fmap-identity-test
              (let* ([m (matrix-from-lists '((1 2 3) (4 5 6)))]
                     [result (matrix-fmap identity m)])
                    (assert-equal 2 (matrix-rows result))
                    (assert-equal 3 (matrix-cols result))
                    (assert-equal 1 (matrix-ref result 0 0))
                    (assert-equal 6 (matrix-ref result 1 2)))))

;;; ============================================================
;;; Matrix Foldable Tests
;;; ============================================================

(test-group matrix-foldable-tests
            (define-test matrix-sum-test
              (let ([m (matrix-from-lists '((1 2) (3 4)))])
                   (assert-equal 10 (matrix-sum m))))
            
            (define-test matrix-product-test
              (let ([m (matrix-from-lists '((1 2) (3 4)))])
                   (assert-equal 24 (matrix-product m))))
            
            (define-test matrix-any-test
              (let ([m (matrix-from-lists '((1 2) (3 4)))])
                   (assert-true (matrix-any (lambda (x) (> x 3)) m))
                   (assert-false (matrix-any (lambda (x) (> x 10)) m))))
            
            (define-test matrix-all-test
              (let ([m (matrix-from-lists '((1 2) (3 4)))])
                   (assert-true (matrix-all (lambda (x) (> x 0)) m))
                   (assert-false (matrix-all (lambda (x) (> x 2)) m))))
            
            (define-test matrix-maximum-test
              (let ([result (matrix-maximum (matrix-from-lists '((3 1) (4 1) (5 9))))])
                   (assert-true (just? result))
                   (assert-equal 9 (from-just result))))
            
            (define-test matrix-minimum-test
              (let ([result (matrix-minimum (matrix-from-lists '((3 1) (4 1) (5 9))))])
                   (assert-true (just? result))
                   (assert-equal 1 (from-just result)))))

;;; ============================================================
;;; Matrix Row/Column Operations Tests
;;; ============================================================

(test-group matrix-row-col-tests
            (define-test matrix-row-sums-test
              (let* ([m (matrix-from-lists '((1 2 3) (4 5 6)))]
                     [sums (matrix-row-sums m)])
                    (assert-equal 2 (vec-length sums))
                    (assert-equal 6 (vec-ref sums 0))   ; 1+2+3
                    (assert-equal 15 (vec-ref sums 1)))) ; 4+5+6
            
            (define-test matrix-col-sums-test
              (let* ([m (matrix-from-lists '((1 2 3) (4 5 6)))]
                     [sums (matrix-col-sums m)])
                    (assert-equal 3 (vec-length sums))
                    (assert-equal 5 (vec-ref sums 0))   ; 1+4
                    (assert-equal 7 (vec-ref sums 1))   ; 2+5
                    (assert-equal 9 (vec-ref sums 2)))) ; 3+6
            
            (define-test matrix-row-maxes-test
              (let* ([m (matrix-from-lists '((1 5 2) (9 3 4)))]
                     [maxes (matrix-row-maxes m)])
                    (assert-equal 2 (vec-length maxes))
                    (assert-equal 5 (vec-ref maxes 0))
                    (assert-equal 9 (vec-ref maxes 1))))
            
            (define-test matrix-col-maxes-test
              (let* ([m (matrix-from-lists '((1 5 2) (9 3 4)))]
                     [maxes (matrix-col-maxes m)])
                    (assert-equal 3 (vec-length maxes))
                    (assert-equal 9 (vec-ref maxes 0))
                    (assert-equal 5 (vec-ref maxes 1))
                    (assert-equal 4 (vec-ref maxes 2)))))

;;; ============================================================
;;; Matrix Applicative Tests
;;; ============================================================

(test-group matrix-applicative-tests
            (define-test matrix-pure-test
              (let ([result (matrix-pure 42)])
                   (assert-equal 1 (matrix-rows result))
                   (assert-equal 1 (matrix-cols result))
                   (assert-equal 42 (matrix-ref result 0 0))))
            
            (define-test matrix-lift2-test
              (let* ([m1 (matrix-from-lists '((1 2) (3 4)))]
                     [m2 (matrix-from-lists '((10 20) (30 40)))]
                     [result (matrix-lift2 + m1 m2)])
                    (assert-equal 2 (matrix-rows result))
                    (assert-equal 2 (matrix-cols result))
                    (assert-equal 11 (matrix-ref result 0 0))
                    (assert-equal 22 (matrix-ref result 0 1))
                    (assert-equal 33 (matrix-ref result 1 0))
                    (assert-equal 44 (matrix-ref result 1 1))))
            
            (define-test matrix-lift2-multiply-test
              (let* ([m1 (matrix-from-lists '((1 2 3) (4 5 6)))]
                     [m2 (matrix-from-lists '((2 2 2) (3 3 3)))]
                     [result (matrix-lift2 * m1 m2)])
                    (assert-equal 2 (matrix-ref result 0 0))
                    (assert-equal 4 (matrix-ref result 0 1))
                    (assert-equal 6 (matrix-ref result 0 2))
                    (assert-equal 12 (matrix-ref result 1 0))
                    (assert-equal 15 (matrix-ref result 1 1))
                    (assert-equal 18 (matrix-ref result 1 2)))))

;;; ============================================================
;;; Matrix Traversable Tests
;;; ============================================================

(test-group matrix-traversable-tests
            (define-test matrix-traverse-maybe-test
              (let* ([m (matrix-from-lists '((1 2) (3 4)))]
                     [f (lambda (x) (if (> x 0) (just (* x 2)) nothing))]
                     [result (matrix-traverse maybe-applicative f m)])
                    (assert-true (just? result))
                    (let ([inner (from-just result)])
                         (assert-equal 2 (matrix-rows inner))
                         (assert-equal 2 (matrix-cols inner))
                         (assert-equal 2 (matrix-ref inner 0 0))
                         (assert-equal 8 (matrix-ref inner 1 1)))))
            
            (define-test matrix-traverse-fails-test
              (let* ([m (matrix-from-lists '((1 -2) (3 4)))]
                     [f (lambda (x) (if (> x 0) (just x) nothing))]
                     [result (matrix-traverse maybe-applicative f m)])
                    (assert-true (nothing? result))))
            
            (define-test matrix-sequence-test
              (let* ([m (matrix-from-lists (list (list (just 1) (just 2))
                                                 (list (just 3) (just 4))))]
                     [result (matrix-sequence maybe-applicative m)])
                    (assert-true (just? result)))))

;;; ============================================================
;;; Functor Laws Tests
;;; ============================================================

(test-group functor-laws
            ;; Identity: fmap id = id
            (define-test vec-functor-identity-law-test
              (let* ([v (vec 1 2 3)]
                     [result (vec-fmap identity v)])
                    (assert-equal (vec-ref v 0) (vec-ref result 0))
                    (assert-equal (vec-ref v 1) (vec-ref result 1))
                    (assert-equal (vec-ref v 2) (vec-ref result 2))))
            
            ;; Composition: fmap (f . g) = fmap f . fmap g
            (define-test vec-functor-composition-law-test
              (let* ([v (vec 1 2 3)]
                     [f (lambda (x) (+ x 1))]
                     [g (lambda (x) (* x 2))]
                     [composed (vec-fmap (lambda (x) (f (g x))) v)]
                     [sequential (vec-fmap f (vec-fmap g v))])
                    (assert-equal (vec-ref composed 0) (vec-ref sequential 0))
                    (assert-equal (vec-ref composed 1) (vec-ref sequential 1))
                    (assert-equal (vec-ref composed 2) (vec-ref sequential 2)))))

;;; ============================================================
;;; Foldable Laws Tests
;;; ============================================================

(test-group foldable-laws
            ;; foldr and foldl should produce consistent sums
            (define-test vec-fold-consistency-test
              (let ([v (vec 1 2 3 4 5)])
                   (assert-equal (vec-foldr + 0 v) (vec-foldl + 0 v))))
            
            ;; fold-map with list monoid should equal foldr cons
            (define-test vec-fold-map-consistency-test
              (let* ([v (vec 1 2 3)]
                     [folded (vec-foldr cons '() v)]
                     [mapped (vec-fold-map list-monoid list v)])
                    (assert-equal folded mapped))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "
")
(display "==============================================================
")
(display (format "Tests passed: ~a
" *tests-passed*))
(display (format "Tests failed: ~a
" *tests-failed*))
(display (format "Total tests:  ~a
" *tests-run*))

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All Vec/Matrix type class instance tests passed.
")
    (display "
[FAILURE] Some tests failed.
"))
