;;; lattice/linalg/test-integer-matrix.ss — Tests for integer matrix normal forms
;;; Run: scheme --script lattice/linalg/test-integer-matrix.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/linalg/integer-matrix.ss")

;;; ============================================================================
;;; Test Utilities
;;; ============================================================================

;;; Verify U*A*V = D (for Smith form)
(define (verify-smith A U D V)
  (let* ([UA (matrix-mul U A)]
         [UAV (matrix-mul UA V)])
    (matrix-equal? UAV D)))

;;; Verify U*A = H (for Hermite form)
(define (verify-hermite A U H)
  (let ([UA (matrix-mul U A)])
    (matrix-equal? UA H)))

;;; Check if matrix is diagonal
(define (is-diagonal? mat)
  (let ([m (matrix-rows mat)]
        [n (matrix-cols mat)]
        [data (matrix-data mat)])
    (let loop-i ([i 0])
      (if (>= i m)
          #t
          (let loop-j ([j 0])
            (if (>= j n)
                (loop-i (+ i 1))
                (if (and (not (= i j))
                         (not (= 0 (vector-ref data (+ (* i n) j)))))
                    #f
                    (loop-j (+ j 1)))))))))

;;; Check Smith divisibility: d_i divides d_{i+1}
(define (check-divisibility factors)
  (if (or (null? factors) (null? (cdr factors)))
      #t
      (let ([d1 (car factors)]
            [d2 (cadr factors)])
        (and (= 0 (modulo d2 d1))
             (check-divisibility (cdr factors))))))

;;; Check if matrix is in Hermite form (upper triangular with reduced elements)
(define (is-hermite-form? mat)
  (let ([m (matrix-rows mat)]
        [n (matrix-cols mat)]
        [data (matrix-data mat)])
    ;; Check upper triangular and pivot properties
    (let loop ([col 0] [pivot-row 0])
      (if (or (>= col n) (>= pivot-row m))
          #t
          (let ([pivot (find-pivot-in-col mat col pivot-row)])
            (if (not pivot)
                (loop (+ col 1) pivot-row)
                (and (> (matrix-ref mat pivot col) 0)  ; Pivot positive
                     (all-below-zero? mat pivot col)    ; Zeros below pivot
                     (all-above-reduced? mat pivot col) ; Above elements reduced
                     (loop (+ col 1) (+ pivot 1)))))))))

(define (find-pivot-in-col mat col start-row)
  (let ([m (matrix-rows mat)])
    (let loop ([i start-row])
      (cond
        [(>= i m) #f]
        [(not (= 0 (matrix-ref mat i col))) i]
        [else (loop (+ i 1))]))))

(define (all-below-zero? mat pivot-row col)
  (let ([m (matrix-rows mat)])
    (let loop ([i (+ pivot-row 1)])
      (cond
        [(>= i m) #t]
        [(not (= 0 (matrix-ref mat i col))) #f]
        [else (loop (+ i 1))]))))

(define (all-above-reduced? mat pivot-row col)
  (let ([pivot (matrix-ref mat pivot-row col)])
    (let loop ([i 0])
      (cond
        [(>= i pivot-row) #t]
        [else
         (let ([val (matrix-ref mat i col)])
           (and (>= val 0)
                (< val pivot)
                (loop (+ i 1))))]))))

;;; ============================================================================
;;; Smith Normal Form Tests
;;; ============================================================================

(test-group "smith-normal-form"

  (define-test "2x2 simple matrix"
    (let* ([A (matrix-from-lists '((2 4) (6 8)))]
           [result (smith-normal-form A)]
           [U (car result)]
           [D (cadr result)]
           [V (caddr result)])
      (assert-true (verify-smith A U D V))
      (assert-true (is-diagonal? D))))

  (define-test "3x3 matrix with common factors"
    (let* ([A (matrix-from-lists '((6 4 2) (3 6 9) (12 8 4)))]
           [result (smith-normal-form A)]
           [U (car result)]
           [D (cadr result)]
           [V (caddr result)])
      (assert-true (verify-smith A U D V))
      (assert-true (is-diagonal? D))
      (let ([factors (smith-invariant-factors A)])
        (assert-true (check-divisibility factors)))))

  (define-test "non-square matrix 2x3"
    (let* ([A (matrix-from-lists '((1 2 3) (4 5 6)))]
           [result (smith-normal-form A)]
           [U (car result)]
           [D (cadr result)]
           [V (caddr result)])
      (assert-true (verify-smith A U D V))
      (assert-true (is-diagonal? D))))

  (define-test "non-square matrix 3x2"
    (let* ([A (matrix-from-lists '((1 2) (3 4) (5 6)))]
           [result (smith-normal-form A)]
           [U (car result)]
           [D (cadr result)]
           [V (caddr result)])
      (assert-true (verify-smith A U D V))
      (assert-true (is-diagonal? D))))

  (define-test "identity matrix"
    (let* ([A (matrix-identity 3)]
           [result (smith-normal-form A)]
           [D (cadr result)])
      (assert-true (is-diagonal? D))
      (assert-equal '(1 1 1) (smith-invariant-factors A))))

  (define-test "zero matrix"
    (let* ([A (zeros 2 2)]
           [result (smith-normal-form A)]
           [D (cadr result)])
      (assert-true (is-diagonal? D))
      (assert-equal '() (smith-invariant-factors A))))

  (define-test "matrix with negative entries"
    (let* ([A (matrix-from-lists '((-2 4) (6 -8)))]
           [result (smith-normal-form A)]
           [U (car result)]
           [D (cadr result)]
           [V (caddr result)])
      (assert-true (verify-smith A U D V))
      (assert-true (is-diagonal? D))))

  (define-test "divisibility chain correct"
    (let* ([A (matrix-from-lists '((2 4 6) (4 8 12) (6 12 18)))]
           [factors (smith-invariant-factors A)])
      (assert-true (check-divisibility factors))))

  (define-test "rank computation"
    (let ([A (matrix-from-lists '((1 2 3) (2 4 6) (1 1 1)))])
      (assert-equal 2 (smith-rank A))))

  (define-test "full rank matrix"
    (let ([A (matrix-from-lists '((1 0 0) (0 1 0) (0 0 1)))])
      (assert-equal 3 (smith-rank A)))))

;;; ============================================================================
;;; Hermite Normal Form Tests
;;; ============================================================================

(test-group "hermite-normal-form"

  (define-test "2x2 simple matrix"
    (let* ([A (matrix-from-lists '((3 4) (2 1)))]
           [result (hermite-normal-form A)]
           [U (car result)]
           [H (cadr result)])
      (assert-true (verify-hermite A U H))
      (assert-true (is-hermite-form? H))))

  (define-test "3x3 matrix"
    (let* ([A (matrix-from-lists '((2 3 6) (4 5 14) (2 -5 2)))]
           [result (hermite-normal-form A)]
           [U (car result)]
           [H (cadr result)])
      (assert-true (verify-hermite A U H))
      (assert-true (is-hermite-form? H))))

  (define-test "non-square matrix 2x3"
    (let* ([A (matrix-from-lists '((3 3 6) (2 1 4)))]
           [result (hermite-normal-form A)]
           [U (car result)]
           [H (cadr result)])
      (assert-true (verify-hermite A U H))))

  (define-test "matrix with zero row"
    (let* ([A (matrix-from-lists '((1 2) (0 0) (3 4)))]
           [result (hermite-normal-form A)]
           [U (car result)]
           [H (cadr result)])
      (assert-true (verify-hermite A U H))))

  (define-test "matrix with negative entries"
    (let* ([A (matrix-from-lists '((-3 4) (2 -5)))]
           [result (hermite-normal-form A)]
           [U (car result)]
           [H (cadr result)])
      (assert-true (verify-hermite A U H))))

  (define-test "identity matrix unchanged"
    (let* ([A (matrix-identity 3)]
           [result (hermite-normal-form A)]
           [H (cadr result)])
      (assert-true (matrix-equal? A H)))))

;;; ============================================================================
;;; Diophantine Equation Tests
;;; ============================================================================

(test-group "solve-linear-diophantine"

  (define-test "simple solvable system"
    (let* ([A (matrix-from-lists '((2 3) (1 1)))]
           [b (vector 5 2)]
           [result (solve-linear-diophantine A b)])
      (assert-false (eq? result 'no-solution))
      ;; Verify Ax = b
      (let ([x (car result)])
        (assert-true (vec-equal? b (matrix-vec-mul A x))))))

  (define-test "system with no solution"
    (let* ([A (matrix-from-lists '((2 4) (6 12)))]
           [b (vector 3 7)]  ; Not consistent
           [result (solve-linear-diophantine A b)])
      (assert-equal 'no-solution result)))

  (define-test "underdetermined system"
    (let* ([A (matrix-from-lists '((1 2 3)))]
           [b (vector 6)]
           [result (solve-linear-diophantine A b)])
      (assert-false (eq? result 'no-solution))
      (let ([x (car result)])
        (assert-true (vec-equal? b (matrix-vec-mul A x)))))))

;;; ============================================================================
;;; Integer Kernel/Image Tests
;;; ============================================================================

(test-group "integer-kernel-image"

  (define-test "kernel of rank-deficient matrix"
    (let* ([A (matrix-from-lists '((1 2) (2 4)))]
           [kernel (integer-kernel A)])
      ;; Rank 1 matrix in R^2 should have 1D kernel
      (assert-equal 1 (length kernel))))

  (define-test "kernel of full-rank matrix"
    (let* ([A (matrix-from-lists '((1 0) (0 1)))]
           [kernel (integer-kernel A)])
      (assert-equal 0 (length kernel))))

  (define-test "determinant of 2x2"
    (let ([A (matrix-from-lists '((3 4) (1 2)))])
      (assert-equal 2 (matrix-determinant-int A))))

  (define-test "determinant of 3x3"
    (let ([A (matrix-from-lists '((1 2 3) (4 5 6) (7 8 10)))])
      (assert-equal -3 (matrix-determinant-int A))))

  (define-test "determinant of identity"
    (assert-equal 1 (matrix-determinant-int (matrix-identity 4))))

  (define-test "unimodular inverse"
    (let* ([U (matrix-from-lists '((1 2) (0 1)))]  ; det = 1
           [U-inv (unimodular-inverse U)]
           [product (matrix-mul U U-inv)])
      (assert-true (matrix-equal? product (matrix-identity 2))))))

;;; ============================================================================
;;; Run Tests
;;; ============================================================================

(run-all-tests)
