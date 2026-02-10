;;; core/linalg/test-dep-linalg.ss — Tests for Dependent Linear Algebra
;;;
;;; Verifies dimension-safe vector and matrix operations.

(load "core/lang/module.ss")
(load "core/test-framework.ss")
(load "lattice/linalg/dep-linalg.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-infer.ss")

;;; ====
;;; Runtime Operation Tests
;;; ====

(test-group "dep-linalg-runtime"
            
            (define-test "vec-append-typed appends vectors"
              (let ([v1 (vec 1 2 3)]
                    [v2 (vec 4 5)])
                   (assert-equal '(1 2 3 4 5)
                                 (vec->list (vec-append-typed 3 2 'Int v1 v2)))))
            
            (define-test "vec-zip-typed zips equal-length vectors"
              (let ([v1 (vec 1 2 3)]
                    [v2 (vec 'a 'b 'c)])
                   (assert-equal '((1 . a) (2 . b) (3 . c))
                                 (vec->list (vec-zip-typed 3 'Int 'Symbol v1 v2)))))
            
            (define-test "vec-head-typed gets first element"
              (let ([v (vec 10 20 30)])
                   (assert-equal 10 (vec-head-typed 2 'Int v))))
            
            (define-test "vec-tail-typed gets rest of vector"
              (let ([v (vec 10 20 30)])
                   (assert-equal '(20 30)
                                 (vec->list (vec-tail-typed 2 'Int v)))))
            
            (define-test "vec-nil-typed creates empty vector"
              (assert-equal '() (vec->list (vec-nil-typed 'Int))))
            
            (define-test "vec-cons-typed prepends element"
              (let ([v (vec 2 3)])
                   (assert-equal '(1 2 3)
                                 (vec->list (vec-cons-typed 2 'Int 1 v)))))
            
            (define-test "vec-map-typed preserves length"
              (let ([v (vec 1 2 3)])
                   (assert-equal '(2 4 6)
                                 (vec->list (vec-map-typed 3 'Int 'Int (lambda (x) (* x 2)) v)))))
            
            (define-test "vec-fold-typed accumulates"
              (let ([v (vec 1 2 3 4)])
                   (assert-equal 10
                                 (vec-fold-typed 4 'Int 'Int + 0 v))))
            
            (define-test "matrix-mul-typed multiplies matrices"
              (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
                    [m2 (matrix-from-lists '((5 6) (7 8)))])
                   (let ([result (matrix-mul-typed 2 2 2 'Int m1 m2)])
                        (assert-equal 19 (matrix-ref result 0 0))
                        (assert-equal 50 (matrix-ref result 1 1)))))
            
            (define-test "matrix-add-typed adds matrices"
              (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
                    [m2 (matrix-from-lists '((5 6) (7 8)))])
                   (let ([result (matrix-add-typed 2 2 'Int m1 m2)])
                        (assert-equal 6 (matrix-ref result 0 0))
                        (assert-equal 12 (matrix-ref result 1 1)))))
            
            (define-test "matrix-transpose-typed swaps dimensions"
              (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
                   ;; Original: row0=(1 2 3), row1=(4 5 6)
                   ;; Transposed: row0=(1 4), row1=(2 5), row2=(3 6)
                   (let ([result (matrix-transpose-typed 2 3 'Int m)])
                        (assert-equal 4 (matrix-ref result 0 1))
                        (assert-equal 2 (matrix-ref result 1 0))))))

;;; ====
;;; Type Structure Tests
;;; ====

(test-group "dep-linalg-types"
            
            (define-test "dep-linalg-types is a valid alist"
              (assert-true (list? dep-linalg-types))
              (assert-true (andmap (lambda (entry) (and (pair? entry) (symbol? (car entry))))
                                   dep-linalg-types)))
            
            (define-test "has 15 type entries (6 linalg + 9 diff)"
              (assert-equal 15 (length dep-linalg-types)))
            
            (define-test "vec-append-typed has Pi type structure"
              (let ([type (cdr (assq 'vec-append-typed dep-linalg-types))])
                   (assert-true (pi-type? type))
                   (assert-equal 'Nat (pi-domain type))))
            
            (define-test "matrix-mul-typed has 4-parameter Pi type"
              (let* ([type (cdr (assq 'matrix-mul-typed dep-linalg-types))]
                     [t1 (pi-codomain type)]
                     [t2 (pi-codomain t1)]
                     [t3 (pi-codomain t2)])
                    (assert-true (pi-type? type))
                    (assert-true (pi-type? t1))
                    (assert-true (pi-type? t2))
                    (assert-true (pi-type? t3))))
            
            (define-test "Pi type predicates work"
              (assert-true (pi-type? '(Π ((x : Nat)) Int)))
              (assert-false (pi-type? '(-> Int Int)))
              (assert-false (pi-type? 'Int))))

;;; Run the tests
(run-all-tests)
