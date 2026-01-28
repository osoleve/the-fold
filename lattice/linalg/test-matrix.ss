;;; lattice/linalg/test-matrix.ss — Tests for Matrix Operations

(load "core/testing/test-framework.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")

;; Helper for error-return functions
(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

;;; ============================================================================
;;; Matrix Construction Tests
;;; ============================================================================

(test-group construction

  (define-test make-matrix-zeros
    (assert-equal (matrix-from-lists '((0 0 0) (0 0 0)))
                  (make-matrix 2 3 0)))

  (define-test matrix-from-lists-2x3
    (assert-equal '((1 2 3) (4 5 6))
                  (matrix->lists (matrix-from-lists '((1 2 3) (4 5 6))))))

  (define-test matrix-rows-count
    (assert-equal 2
                  (matrix-rows (matrix-from-lists '((1 2 3) (4 5 6))))))

  (define-test matrix-cols-count
    (assert-equal 3
                  (matrix-cols (matrix-from-lists '((1 2 3) (4 5 6))))))

  (define-test matrix-shape-pair
    (assert-equal '(2 . 3)
                  (matrix-shape (matrix-from-lists '((1 2 3) (4 5 6))))))

  (define-test matrix-predicate-true
    (assert-true (matrix? (matrix-from-lists '((1 2) (3 4))))))

  (define-test matrix-predicate-false
    (assert-false (matrix? '((1 2) (3 4)))))

  (define-test matrix-from-lists-ragged-short
    (assert-true (error-result? (matrix-from-lists '((1 2 3) (4 5))))))

  (define-test matrix-from-lists-ragged-long
    (assert-true (error-result? (matrix-from-lists '((1 2) (3 4 5))))))

  (define-test matrix-from-lists-ragged-multiple
    (assert-true (error-result? (matrix-from-lists '((1 2 3) (4 5) (6 7 8 9))))))

  (define-test matrix-from-lists-single-row
    (assert-equal '((1 2 3))
                  (matrix->lists (matrix-from-lists '((1 2 3))))))

  (define-test matrix-from-lists-single-col
    (assert-equal '((1) (2) (3))
                  (matrix->lists (matrix-from-lists '((1) (2) (3)))))))

;;; ============================================================================
;;; Matrix Accessors Tests
;;; ============================================================================

(test-group accessors

  (define-test matrix-ref-0-0
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-equal 1 (matrix-ref m 0 0))))

  (define-test matrix-ref-1-2
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-equal 6 (matrix-ref m 1 2))))

  (define-test matrix-ref-out-of-bounds
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-true (error-result? (matrix-ref m 5 0)))))

  (define-test matrix-row-0
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-equal (vector 1 2 3) (matrix-row m 0))))

  (define-test matrix-row-1
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-equal (vector 4 5 6) (matrix-row m 1))))

  (define-test matrix-col-0
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-equal (vector 1 4) (matrix-col m 0))))

  (define-test matrix-col-2
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-equal (vector 3 6) (matrix-col m 2))))

  (define-test matrix-diagonal-3x3
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))])
      (assert-equal (vector 1 5 9) (matrix-diagonal m)))))

;;; ============================================================================
;;; Matrix Transformations Tests
;;; ============================================================================

(test-group transformations

  (define-test matrix-transpose-2x3
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))])
      (assert-equal '((1 4) (2 5) (3 6))
                    (matrix->lists (matrix-transpose m)))))

  (define-test matrix-map-double
    (assert-equal '((2 4) (6 8))
                  (matrix->lists
                   (matrix-map (lambda (x) (* x 2))
                               (matrix-from-lists '((1 2) (3 4)))))))

  (define-test matrix-fold-sum
    (assert-equal 10
                  (matrix-fold + 0 (matrix-from-lists '((1 2) (3 4)))))))

;;; ============================================================================
;;; Matrix Arithmetic Tests
;;; ============================================================================

(test-group arithmetic

  (define-test matrix-add-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
          [m2 (matrix-from-lists '((5 6) (7 8)))])
      (assert-equal '((6 8) (10 12))
                    (matrix->lists (matrix-add m1 m2)))))

  (define-test matrix-sub-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
          [m2 (matrix-from-lists '((5 6) (7 8)))])
      (assert-equal '((-4 -4) (-4 -4))
                    (matrix->lists (matrix-sub m1 m2)))))

  (define-test matrix-hadamard-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
          [m2 (matrix-from-lists '((5 6) (7 8)))])
      (assert-equal '((5 12) (21 32))
                    (matrix->lists (matrix-hadamard m1 m2)))))

  (define-test matrix-scale-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))])
      (assert-equal '((2 4) (6 8))
                    (matrix->lists (matrix-scale 2 m1)))))

  (define-test matrix-negate-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))])
      (assert-equal '((-1 -2) (-3 -4))
                    (matrix->lists (matrix-negate m1)))))

  (define-test matrix-add-dimension-mismatch
    (assert-true (error-result?
                  (matrix-add (matrix-from-lists '((1 2)))
                              (matrix-from-lists '((1) (2))))))))

;;; ============================================================================
;;; Matrix Multiplication Tests
;;; ============================================================================

(test-group multiplication

  (define-test matrix-mul-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
          [m2 (matrix-from-lists '((5 6) (7 8)))])
      (assert-equal '((19 22) (43 50))
                    (matrix->lists (matrix-mul m1 m2)))))

  (define-test matrix-mul-2x3-3x2
    (let ([m1 (matrix-from-lists '((1 2 3) (4 5 6)))]
          [m2 (matrix-from-lists '((1 2) (3 4) (5 6)))])
      (assert-equal '((22 28) (49 64))
                    (matrix->lists (matrix-mul m1 m2)))))

  (define-test matrix-mul-dimension-mismatch
    (assert-true (error-result?
                  (matrix-mul (matrix-from-lists '((1 2 3)))
                              (matrix-from-lists '((1 2))))))))

;;; ============================================================================
;;; Matrix-Vector Multiplication Tests
;;; ============================================================================

(test-group matrix-vector

  (define-test matrix-vec-mul-2x3
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6)))]
          [v (vector 1 2 3)])
      (assert-equal (vector 14 32) (matrix-vec-mul m v))))

  (define-test vec-matrix-mul-2x2
    (let ([m (matrix-from-lists '((1 2) (3 4)))]
          [v (vector 1 2)])
      (assert-equal (vector 7 10) (vec-matrix-mul v m))))

  (define-test matrix-vec-mul-dimension-mismatch
    (assert-true (error-result?
                  (matrix-vec-mul (matrix-from-lists '((1 2 3)))
                                  (vector 1 2))))))

;;; ============================================================================
;;; Matrix Comparisons Tests
;;; ============================================================================

(test-group comparisons

  (define-test matrix-equal-same
    (assert-true (matrix-equal? (matrix-from-lists '((1 2) (3 4)))
                                (matrix-from-lists '((1 2) (3 4))))))

  (define-test matrix-equal-different
    (assert-false (matrix-equal? (matrix-from-lists '((1 2) (3 4)))
                                 (matrix-from-lists '((1 2) (3 5))))))

  (define-test matrix-equal-different-size
    (assert-false (matrix-equal? (matrix-from-lists '((1 2) (3 4)))
                                 (matrix-from-lists '((1 2 3))))))

  (define-test matrix-approx-equal-within-epsilon
    (assert-true (matrix-approx-equal?
                  (matrix-from-lists '((1.0 2.0) (3.0 4.0)))
                  (matrix-from-lists '((1.00000000001 2.0) (3.0 4.0)))))))

;;; ============================================================================
;;; Matrix Properties Tests
;;; ============================================================================

(test-group properties

  (define-test matrix-square-true
    (assert-true (matrix-square? (matrix-from-lists '((1 2) (3 4))))))

  (define-test matrix-square-false
    (assert-false (matrix-square? (matrix-from-lists '((1 2 3) (4 5 6))))))

  (define-test matrix-symmetric-true
    (assert-true (matrix-symmetric? (matrix-from-lists '((1 2) (2 1))))))

  (define-test matrix-symmetric-false
    (assert-false (matrix-symmetric? (matrix-from-lists '((1 2) (3 1))))))

  (define-test trace-2x2
    (assert-equal 5 (trace (matrix-from-lists '((1 2) (3 4))))))

  (define-test frobenius-norm-identity
    (assert-true (< (abs (- (frobenius-norm (matrix-identity 2)) (sqrt 2))) 1e-10))))

;;; ============================================================================
;;; Special Matrices Tests
;;; ============================================================================

(test-group special-matrices

  (define-test zeros-2x3
    (assert-equal '((0 0 0) (0 0 0))
                  (matrix->lists (zeros 2 3))))

  (define-test ones-3x2
    (assert-equal '((1 1) (1 1) (1 1))
                  (matrix->lists (ones 3 2))))

  (define-test identity-3x3
    (assert-equal '((1 0 0) (0 1 0) (0 0 1))
                  (matrix->lists (matrix-identity 3))))

  (define-test diagonal-3
    (assert-equal '((1 0 0) (0 2 0) (0 0 3))
                  (matrix->lists (diagonal (vector 1 2 3))))))

;;; ============================================================================
;;; Matrix Slicing Tests
;;; ============================================================================

(test-group slicing

  (define-test matrix-submatrix-2x2
    (let ([m (matrix-from-lists '((1 2 3 4) (5 6 7 8) (9 10 11 12)))])
      (assert-equal '((6 7) (10 11))
                    (matrix->lists (matrix-submatrix m 1 1 3 3))))))

;;; ============================================================================
;;; Matrix Concatenation Tests
;;; ============================================================================

(test-group concatenation

  (define-test matrix-hstack-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
          [m2 (matrix-from-lists '((5 6) (7 8)))])
      (assert-equal '((1 2 5 6) (3 4 7 8))
                    (matrix->lists (matrix-hstack m1 m2)))))

  (define-test matrix-vstack-2x2
    (let ([m1 (matrix-from-lists '((1 2) (3 4)))]
          [m2 (matrix-from-lists '((5 6) (7 8)))])
      (assert-equal '((1 2) (3 4) (5 6) (7 8))
                    (matrix->lists (matrix-vstack m1 m2)))))

  (define-test matrix-hstack-dimension-mismatch
    (assert-true (error-result?
                  (matrix-hstack (matrix-from-lists '((1 2)))
                                 (matrix-from-lists '((1) (2))))))))

;;; ============================================================================
;;; Identity Properties Tests
;;; ============================================================================

(test-group identity-properties

  (define-test A-times-I-equals-A
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
          [I (matrix-identity 3)])
      (assert-equal (matrix->lists m)
                    (matrix->lists (matrix-mul m I)))))

  (define-test I-times-A-equals-A
    (let ([m (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
          [I (matrix-identity 3)])
      (assert-equal (matrix->lists m)
                    (matrix->lists (matrix-mul I m))))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
