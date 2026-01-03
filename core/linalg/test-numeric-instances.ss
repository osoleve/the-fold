;;; core/linalg/test-numeric-instances.ss --- Tests for Vec/Matrix Num instances

;;; NOTE: Run from project root

(load "core/test-framework.ss")
(load "core/linalg/numeric-instances.ss")

(display "\n")
(display "==============================================================\n")
(display "         NUMERIC INSTANCES TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Vec Num Instance Tests
;;; ============================================================

(test-group vec-num-instance
            (define-test vec-add-test
              (let ([v1 (vec 1 2 3)]
                    [v2 (vec 4 5 6)])
                   (assert-equal '#(5 7 9) (vec+ v1 v2))))
            
            (define-test vec-sub-test
              (let ([v1 (vec 5 5 5)]
                    [v2 (vec 1 2 3)])
                   (assert-equal '#(4 3 2) (vec- v1 v2))))
            
            (define-test vec-mul-test
              (let ([v1 (vec 2 3 4)]
                    [v2 (vec 3 4 5)])
                   (assert-equal '#(6 12 20) (vec* v1 v2))))
            
            (define-test vec-negate-test
              (let ([v (vec 1 -2 3)])
                   (assert-equal '#(-1 2 -3) (vec-negate v))))
            
            (define-test vec-abs-test
              (let ([v (vec -1 2 -3)])
                   (assert-equal '#(1 2 3) (vec-abs v))))
            
            (define-test vec-signum-test
              (let ([v (vec -5 0 7)])
                   (assert-equal '#(-1 0 1) (vec-signum v)))))

;;; ============================================================
;;; Vec Fractional Instance Tests
;;; ============================================================

(test-group vec-fractional-instance
            (define-test vec-div-test
              (let ([v1 (vec 10 20 30)]
                    [v2 (vec 2 4 5)])
                   (assert-equal '#(5 5 6) (vec/ v1 v2))))
            
            (define-test vec-recip-test
              (let ([v (vec 2 4 5)])
                   (assert-equal '#(1/2 1/4 1/5) (vec-recip v)))))

;;; ============================================================
;;; Vec Floating Instance Tests
;;; ============================================================

(test-group vec-floating-instance
            (define-test vec-exp-test
              (let ([v (vec 0 1)])
                   (assert-true (< (abs (- (vector-ref (vec-exp v) 0) 1)) 0.001))
                   (assert-true (< (abs (- (vector-ref (vec-exp v) 1) 2.718281828)) 0.001))))
            
            (define-test vec-log-test
              (let ([v (vec 1 2.718281828)])
                   (assert-true (< (abs (vector-ref (vec-log v) 0)) 0.001))
                   (assert-true (< (abs (- (vector-ref (vec-log v) 1) 1)) 0.001))))
            
            (define-test vec-sqrt-test
              (let ([v (vec 4 9 16)])
                   (assert-equal '#(2 3 4) (vec-sqrt v))))
            
            (define-test vec-sin-cos-test
              (let ([v (vec 0)])
                   (assert-true (< (abs (vector-ref (vec-sin v) 0)) 0.001))
                   (assert-true (< (abs (- (vector-ref (vec-cos v) 0) 1)) 0.001))))
            
            (define-test vec-sinh-cosh-test
              (let ([v (vec 0)])
                   (assert-true (< (abs (vector-ref (vec-sinh v) 0)) 0.001))
                   (assert-true (< (abs (- (vector-ref (vec-cosh v) 0) 1)) 0.001))))
            
            (define-test vec-tanh-test
              (let ([v (vec 0)])
                   (assert-true (< (abs (vector-ref (vec-tanh v) 0)) 0.001)))))

;;; ============================================================
;;; Matrix Num Instance Tests
;;; ============================================================

(test-group matrix-num-instance
            (define-test matrix-add-test
              (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
                    [m2 (matrix-from-lists '((5 6) (7 8)))])
                   (let ([result (matrix+ m1 m2)])
                        (assert-equal 6 (matrix-ref result 0 0))
                        (assert-equal 8 (matrix-ref result 0 1))
                        (assert-equal 10 (matrix-ref result 1 0))
                        (assert-equal 12 (matrix-ref result 1 1)))))
            
            (define-test matrix-sub-test
              (let ([m1 (matrix-from-lists '((5 6) (7 8)))]
                    [m2 (matrix-from-lists '((1 2) (3 4)))])
                   (let ([result (matrix- m1 m2)])
                        (assert-equal 4 (matrix-ref result 0 0))
                        (assert-equal 4 (matrix-ref result 0 1)))))
            
            (define-test matrix-hadamard-test
              (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
                    [m2 (matrix-from-lists '((2 3) (4 5)))])
                   (let ([result (matrix* m1 m2)])
                        (assert-equal 2 (matrix-ref result 0 0))
                        (assert-equal 6 (matrix-ref result 0 1))
                        (assert-equal 12 (matrix-ref result 1 0))
                        (assert-equal 20 (matrix-ref result 1 1)))))
            
            (define-test matrix-negate-test
              (let ([m (matrix-from-lists '((1 -2) (-3 4)))])
                   (let ([result (matrix-negate m)])
                        (assert-equal -1 (matrix-ref result 0 0))
                        (assert-equal 2 (matrix-ref result 0 1))
                        (assert-equal 3 (matrix-ref result 1 0))
                        (assert-equal -4 (matrix-ref result 1 1)))))
            
            (define-test matrix-abs-test
              (let ([m (matrix-from-lists '((-1 2) (-3 -4)))])
                   (let ([result (matrix-abs m)])
                        (assert-equal 1 (matrix-ref result 0 0))
                        (assert-equal 2 (matrix-ref result 0 1))
                        (assert-equal 3 (matrix-ref result 1 0))
                        (assert-equal 4 (matrix-ref result 1 1))))))

;;; ============================================================
;;; Matrix Fractional Instance Tests
;;; ============================================================

(test-group matrix-fractional-instance
            (define-test matrix-div-test
              (let ([m1 (matrix-from-lists '((10 20) (30 40)))]
                    [m2 (matrix-from-lists '((2 4) (5 8)))])
                   (let ([result (matrix/ m1 m2)])
                        (assert-equal 5 (matrix-ref result 0 0))
                        (assert-equal 5 (matrix-ref result 0 1))
                        (assert-equal 6 (matrix-ref result 1 0))
                        (assert-equal 5 (matrix-ref result 1 1)))))
            
            (define-test matrix-recip-test
              (let ([m (matrix-from-lists '((2 4) (5 10)))])
                   (let ([result (matrix-recip m)])
                        (assert-equal 1/2 (matrix-ref result 0 0))
                        (assert-equal 1/4 (matrix-ref result 0 1))
                        (assert-equal 1/5 (matrix-ref result 1 0))
                        (assert-equal 1/10 (matrix-ref result 1 1))))))

;;; ============================================================
;;; Matrix Floating Instance Tests
;;; ============================================================

(test-group matrix-floating-instance
            (define-test matrix-exp-test
              (let ([m (matrix-from-lists '((0 1)))])
                   (let ([result (matrix-exp m)])
                        (assert-true (< (abs (- (matrix-ref result 0 0) 1)) 0.001))
                        (assert-true (< (abs (- (matrix-ref result 0 1) 2.718281828)) 0.001)))))
            
            (define-test matrix-sqrt-test
              (let ([m (matrix-from-lists '((4 9) (16 25)))])
                   (let ([result (matrix-sqrt m)])
                        (assert-equal 2 (matrix-ref result 0 0))
                        (assert-equal 3 (matrix-ref result 0 1))
                        (assert-equal 4 (matrix-ref result 1 0))
                        (assert-equal 5 (matrix-ref result 1 1)))))
            
            (define-test matrix-sin-cos-test
              (let ([m (matrix-from-lists '((0)))])
                   (assert-true (< (abs (matrix-ref (matrix-sin m) 0 0)) 0.001))
                   (assert-true (< (abs (- (matrix-ref (matrix-cos m) 0 0) 1)) 0.001)))))

;;; ============================================================
;;; Scalar-Vector Operations Tests
;;; ============================================================

(test-group scalar-vector-ops
            (define-test scalar-vec-add-test
              (let ([v (vec 1 2 3)])
                   (assert-equal '#(11 12 13) (scalar-vec+ 10 v))))
            
            (define-test scalar-vec-mul-test
              (let ([v (vec 1 2 3)])
                   (assert-equal '#(2 4 6) (scalar-vec* 2 v)))))

;;; ============================================================
;;; Scalar-Matrix Operations Tests
;;; ============================================================

(test-group scalar-matrix-ops
            (define-test scalar-matrix-add-test
              (let ([m (matrix-from-lists '((1 2) (3 4)))])
                   (let ([result (scalar-matrix+ 10 m)])
                        (assert-equal 11 (matrix-ref result 0 0))
                        (assert-equal 12 (matrix-ref result 0 1)))))
            
            (define-test scalar-matrix-mul-test
              (let ([m (matrix-from-lists '((1 2) (3 4)))])
                   (let ([result (scalar-matrix* 2 m)])
                        (assert-equal 2 (matrix-ref result 0 0))
                        (assert-equal 4 (matrix-ref result 0 1))
                        (assert-equal 6 (matrix-ref result 1 0))
                        (assert-equal 8 (matrix-ref result 1 1))))))

;;; ============================================================
;;; Applicative Operations Tests
;;; ============================================================

(test-group applicative-ops
            (define-test vec-pure-test
              (assert-equal '#(42) (vec-pure 42)))
            
            (define-test vec-ap-test
              (let ([fs (vec-from-list (list (lambda (x) (+ x 1)) (lambda (x) (* x 2))))]
                    [xs (vec 10 20)])
                   (let ([result (vec-ap fs xs)])
                        (assert-equal 11 (vector-ref result 0))
                        (assert-equal 40 (vector-ref result 1)))))
            
            (define-test vec-lift2-test
              (let ([v1 (vec 1 2 3)]
                    [v2 (vec 4 5 6)])
                   (assert-equal '#(5 7 9) (vec-lift2 + v1 v2)))))

;;; ============================================================
;;; Dimension Mismatch Tests
;;; ============================================================

(test-group dimension-errors
            (define-test vec-add-mismatch-test
              (let ([v1 (vec 1 2)]
                    [v2 (vec 1 2 3)])
                   (let ([result (vec+ v1 v2)])
                        (assert-true (and (pair? result) (eq? (car result) 'error))))))
            
            (define-test matrix-add-mismatch-test
              (let ([m1 (matrix-from-lists '((1 2)))]
                    [m2 (matrix-from-lists '((1) (2)))])
                   (let ([result (matrix+ m1 m2)])
                        (assert-true (and (pair? result) (eq? (car result) 'error)))))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All numeric instances tests passed.\n")
    (display "\n[FAILURE] Some numeric instances tests failed.\n"))
