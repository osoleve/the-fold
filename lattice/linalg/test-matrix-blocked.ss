;;; lattice/linalg/test-matrix-blocked.ss — Tests for Blocked Matrix Algorithms

(load "core/testing/test-framework.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-blocked.ss")

(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

;;; Helper: generate a deterministic test matrix of given size
(define (make-test-matrix rows cols)
  (let ([data (make-vector (* rows cols) 0)])
    (do ([i 0 (+ i 1)])
        ((= i rows))
      (do ([j 0 (+ j 1)])
          ((= j cols))
        (vector-set! data (+ (* i cols) j) (+ (* i cols) j 1.0))))
    (list 'matrix rows cols data)))

;;; ============================================================================
;;; Blocked Multiplication Tests
;;; ============================================================================

(test-group blocked-mul-correctness

  (define-test blocked-mul-2x2
    (let* ([a (matrix-from-lists '((1 2) (3 4)))]
           [b (matrix-from-lists '((5 6) (7 8)))]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b)])
      (assert-true (matrix-approx-equal? naive blocked))))

  (define-test blocked-mul-3x3
    (let* ([a (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
           [b (matrix-from-lists '((9 8 7) (6 5 4) (3 2 1)))]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b)])
      (assert-true (matrix-approx-equal? naive blocked))))

  (define-test blocked-mul-non-square-2x3-times-3x4
    (let* ([a (matrix-from-lists '((1 2 3) (4 5 6)))]
           [b (matrix-from-lists '((1 2 3 4) (5 6 7 8) (9 10 11 12)))]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b)])
      (assert-true (matrix-approx-equal? naive blocked))))

  (define-test blocked-mul-non-multiple-of-bs
    (let* ([a (make-test-matrix 7 5)]
           [b (make-test-matrix 5 9)]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b 4)])
      (assert-true (matrix-approx-equal? naive blocked))))

  (define-test blocked-mul-identity-right
    (let* ([a (make-test-matrix 10 10)]
           [I (matrix-identity 10)]
           [result (matrix-mul-blocked a I)])
      (assert-true (matrix-approx-equal? a result))))

  (define-test blocked-mul-identity-left
    (let* ([a (make-test-matrix 10 10)]
           [I (matrix-identity 10)]
           [result (matrix-mul-blocked I a)])
      (assert-true (matrix-approx-equal? a result))))

  (define-test blocked-mul-dimension-mismatch
    (let ([a (matrix-from-lists '((1 2 3)))]
          [b (matrix-from-lists '((1 2)))])
      (assert-true (error-result? (matrix-mul-blocked a b)))))

  (define-test blocked-mul-50x50
    (let* ([a (make-test-matrix 50 50)]
           [b (make-test-matrix 50 50)]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b)])
      (assert-true (matrix-approx-equal? naive blocked 1e-6))))

  (define-test blocked-mul-100x100
    (let* ([a (make-test-matrix 100 100)]
           [b (make-test-matrix 100 100)]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b)])
      (assert-true (matrix-approx-equal? naive blocked 1e-4))))

  (define-test blocked-mul-small-block-size
    (let* ([a (make-test-matrix 8 8)]
           [b (make-test-matrix 8 8)]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b 2)])
      (assert-true (matrix-approx-equal? naive blocked))))

  (define-test blocked-mul-block-size-larger-than-matrix
    (let* ([a (make-test-matrix 4 4)]
           [b (make-test-matrix 4 4)]
           [naive (matrix-mul a b)]
           [blocked (matrix-mul-blocked a b 64)])
      (assert-true (matrix-approx-equal? naive blocked)))))

;;; ============================================================================
;;; Blocked Multiplication Into Pre-allocated Buffer Tests
;;; ============================================================================

(test-group blocked-mul-into

  (define-test mul-into-full-range
    (let* ([a (make-test-matrix 8 6)]
           [b (make-test-matrix 6 10)]
           [expected (matrix-mul-blocked a b)]
           [result (make-vector (* 8 10) 0)])
      (matrix-mul-blocked-into! a b result 0 8)
      (assert-true (matrix-approx-equal?
                     expected
                     (list 'matrix 8 10 result)))))

  (define-test mul-into-partial-range
    (let* ([a (make-test-matrix 8 6)]
           [b (make-test-matrix 6 10)]
           [expected (matrix-mul-blocked a b)]
           [result (make-vector (* 8 10) 0)])
      ;; Compute rows [0,4) and [4,8) separately
      (matrix-mul-blocked-into! a b result 0 4)
      (matrix-mul-blocked-into! a b result 4 8)
      (assert-true (matrix-approx-equal?
                     expected
                     (list 'matrix 8 10 result))))))

;;; ============================================================================
;;; Blocked Transpose Tests
;;; ============================================================================

(test-group blocked-transpose-correctness

  (define-test blocked-transpose-2x3
    (let* ([m (matrix-from-lists '((1 2 3) (4 5 6)))]
           [naive (matrix-transpose m)]
           [blocked (matrix-transpose-blocked m)])
      (assert-true (matrix-equal? naive blocked))))

  (define-test blocked-transpose-square
    (let* ([m (make-test-matrix 10 10)]
           [naive (matrix-transpose m)]
           [blocked (matrix-transpose-blocked m)])
      (assert-true (matrix-equal? naive blocked))))

  (define-test blocked-transpose-rectangular
    (let* ([m (make-test-matrix 7 13)]
           [naive (matrix-transpose m)]
           [blocked (matrix-transpose-blocked m)])
      (assert-true (matrix-equal? naive blocked))))

  (define-test blocked-transpose-double-is-identity
    (let* ([m (make-test-matrix 15 15)]
           [result (matrix-transpose-blocked (matrix-transpose-blocked m))])
      (assert-true (matrix-equal? m result))))

  (define-test blocked-transpose-non-multiple-of-bs
    (let* ([m (make-test-matrix 11 7)]
           [naive (matrix-transpose m)]
           [blocked (matrix-transpose-blocked m 3)])
      (assert-true (matrix-equal? naive blocked)))))

;;; ============================================================================
;;; Strassen Tests
;;; ============================================================================

(test-group strassen-correctness

  (define-test strassen-below-threshold
    (let* ([a (make-test-matrix 4 4)]
           [b (make-test-matrix 4 4)]
           [naive (matrix-mul a b)]
           [strassen (matrix-mul-strassen a b 8)])
      (assert-true (matrix-approx-equal? naive strassen))))

  (define-test strassen-at-threshold
    (let* ([a (make-test-matrix 8 8)]
           [b (make-test-matrix 8 8)]
           [naive (matrix-mul a b)]
           [strassen (matrix-mul-strassen a b 8)])
      (assert-true (matrix-approx-equal? naive strassen 1e-6))))

  (define-test strassen-above-threshold
    (let* ([a (make-test-matrix 16 16)]
           [b (make-test-matrix 16 16)]
           [naive (matrix-mul a b)]
           [strassen (matrix-mul-strassen a b 8)])
      (assert-true (matrix-approx-equal? naive strassen 1e-4))))

  (define-test strassen-non-square
    (let* ([a (make-test-matrix 5 7)]
           [b (make-test-matrix 7 3)]
           [naive (matrix-mul a b)]
           [strassen (matrix-mul-strassen a b 4)])
      (assert-true (matrix-approx-equal? naive strassen 1e-4))))

  (define-test strassen-non-pow2
    (let* ([a (make-test-matrix 6 6)]
           [b (make-test-matrix 6 6)]
           [naive (matrix-mul a b)]
           [strassen (matrix-mul-strassen a b 4)])
      (assert-true (matrix-approx-equal? naive strassen 1e-4))))

  (define-test strassen-dimension-mismatch
    (let ([a (matrix-from-lists '((1 2)))]
          [b (matrix-from-lists '((1 2)))])
      (assert-true (error-result? (matrix-mul-strassen a b))))))

;;; ============================================================================
;;; Quadrant Assembly Tests
;;; ============================================================================

(test-group quadrant-assembly

  (define-test from-quadrants-2x2
    (let* ([c11 (matrix-from-lists '((1)))]
           [c12 (matrix-from-lists '((2)))]
           [c21 (matrix-from-lists '((3)))]
           [c22 (matrix-from-lists '((4)))]
           [result (matrix-from-quadrants c11 c12 c21 c22)])
      (assert-equal '((1 2) (3 4)) (matrix->lists result))))

  (define-test from-quadrants-4x4
    (let* ([c11 (matrix-from-lists '((1 2) (5 6)))]
           [c12 (matrix-from-lists '((3 4) (7 8)))]
           [c21 (matrix-from-lists '((9 10) (13 14)))]
           [c22 (matrix-from-lists '((11 12) (15 16)))]
           [result (matrix-from-quadrants c11 c12 c21 c22)])
      (assert-equal '((1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16))
                    (matrix->lists result))))

  (define-test quadrant-roundtrip
    (let* ([m (make-test-matrix 8 8)]
           [tl (matrix-quadrant m 'tl)]
           [tr (matrix-quadrant m 'tr)]
           [bl (matrix-quadrant m 'bl)]
           [br (matrix-quadrant m 'br)]
           [reassembled (matrix-from-quadrants tl tr bl br)])
      (assert-true (matrix-equal? m reassembled)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
