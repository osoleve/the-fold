;;; svd.sexp — Development tasks for lattice/linalg/svd.ss
;;;
;;; Module: svd (tier 1, depends on prelude, vec, matrix, matrix-decomp, matrix-eigen)
;;; 508 lines, ~20 exported functions
;;; Singular Value Decomposition: full A = USV^T via symmetric eigendecomposition,
;;; thin/economy SVD, Moore-Penrose pseudoinverse, low-rank approximation (Eckart-Young),
;;; matrix rank, Frobenius norm, SVD verification.
;;;
;;; Git history:
;;;   d465c9e0 refactor(linalg): Convert matrix.ss and svd.ss to doc forms
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement eigenvalues->singular-values
  ;; ──────────────────────────────────────────────
  ;; Convert eigenvalues of A^T A to singular values.

  (task
    (id "linalg/svd/eigenvalues-to-sv-impl")
    (module svd)
    (tier 1)
    (type implement)
    (description "Implement eigenvalues->singular-values: convert eigenvalues of A^T A to singular values by taking square roots. Eigenvalues of A^T A are theoretically non-negative (it's positive semidefinite), but numerical error can produce tiny negatives. Clamp: if eigenvalue > tol, set singular value to sqrt(eigenvalue); otherwise set to 0. Allocate a result vector, iterate with a do loop.")
    (context "Available: vector-length, vector-ref, vector-set!, make-vector, sqrt, >, if, do, =. Loop with conditional sqrt.")
    (before "(define (eigenvalues->singular-values eigenvalues tol)
  ;; TODO: sqrt of eigenvalues, clamping negatives to 0
  (make-vector (vector-length eigenvalues) 0))")
    (test-file "lattice/linalg/test-svd.ss")
    (test-forms (";; Standard case: sqrt of positive eigenvalues
(let ([sv (eigenvalues->singular-values '#(9.0 4.0 1.0) 1e-10)])
  (assert-true (< (abs (- (vector-ref sv 0) 3.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref sv 1) 2.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref sv 2) 1.0)) 1e-10)))"
                 ";; Zero eigenvalue maps to zero
(let ([sv (eigenvalues->singular-values '#(4.0 0.0) 1e-10)])
  (assert-true (< (abs (- (vector-ref sv 0) 2.0)) 1e-10))
  (assert-equal 0 (vector-ref sv 1)))"
                 ";; Tiny negative clamped to 0
(let ([sv (eigenvalues->singular-values '#(4.0 -1e-15) 1e-10)])
  (assert-true (< (abs (- (vector-ref sv 0) 2.0)) 1e-10))
  (assert-equal 0 (vector-ref sv 1)))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-eigen))
    (hints ("Allocate: (let* ([n (vector-length eigenvalues)] [result (make-vector n 0)]) ...)"
            "do loop: (do ([i 0 (+ i 1)]) ((= i n) result) ...)"
            "For each: (let ([ev (vector-ref eigenvalues i)]) ...)"
            "Conditional: (vector-set! result i (if (> ev tol) (sqrt ev) 0))"))
    (reference-solution "(define (eigenvalues->singular-values eigenvalues tol)
  (let* ([n (vector-length eigenvalues)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([ev (vector-ref eigenvalues i)])
                 (vector-set! result i (if (> ev tol) (sqrt ev) 0))))))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement build-sigma-matrix
  ;; ──────────────────────────────────────────────
  ;; Build the m x n diagonal matrix of singular values.

  (task
    (id "linalg/svd/build-sigma-matrix-impl")
    (module svd)
    (tier 1)
    (type implement)
    (description "Implement build-sigma-matrix: construct the m x n diagonal matrix Sigma for the SVD A = U Sigma V^T. Create a zero matrix of dimensions m x n, then place singular values on the diagonal. The number of diagonal entries is min(m, n, length-of-singular-values-vector). Use a do loop to set sigma[i][i] = singular_values[i] for i from 0 to k-1.")
    (context "Available: make-matrix, matrix-set!, vector-ref, vector-length, min, do, =, let. Allocation + diagonal fill loop.")
    (before "(define (build-sigma-matrix m n singular-values)
  ;; TODO: m x n diagonal matrix with singular values
  (make-matrix m n 0))")
    (test-file "lattice/linalg/test-svd.ss")
    (test-forms (";; 3x2 sigma with 2 singular values
(let ([sigma (build-sigma-matrix 3 2 '#(5.0 3.0))])
  (assert-true (< (abs (- (matrix-ref sigma 0 0) 5.0)) 1e-10))
  (assert-true (< (abs (- (matrix-ref sigma 1 1) 3.0)) 1e-10))
  (assert-equal 0 (matrix-ref sigma 2 0))
  (assert-equal 0 (matrix-ref sigma 2 1)))"
                 ";; 2x3 sigma (wide matrix)
(let ([sigma (build-sigma-matrix 2 3 '#(7.0 2.0))])
  (assert-true (< (abs (- (matrix-ref sigma 0 0) 7.0)) 1e-10))
  (assert-true (< (abs (- (matrix-ref sigma 1 1) 2.0)) 1e-10))
  (assert-equal 3 (matrix-cols sigma)))"
                 ";; Square case
(let ([sigma (build-sigma-matrix 2 2 '#(4.0 1.0))])
  (assert-equal 2 (matrix-rows sigma))
  (assert-equal 2 (matrix-cols sigma)))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-eigen))
    (hints ("Create zero matrix: (let ([sigma (make-matrix m n 0)]) ...)"
            "How many diagonal entries: (let ([k (min m n (vector-length singular-values))]) ...)"
            "Fill diagonal: (do ([i 0 (+ i 1)]) ((= i k) sigma) (matrix-set! sigma i i (vector-ref singular-values i)))"))
    (reference-solution "(define (build-sigma-matrix m n singular-values)
  (let ([sigma (make-matrix m n 0)]
        [k (min m n (vector-length singular-values))])
       (do ([i 0 (+ i 1)])
           ((= i k) sigma)
           (matrix-set! sigma i i (vector-ref singular-values i)))))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement count-nonzero-singular-values
  ;; ──────────────────────────────────────────────
  ;; Count singular values above tolerance.

  (task
    (id "linalg/svd/count-nonzero-sv-impl")
    (module svd)
    (tier 1)
    (type implement)
    (description "Implement count-nonzero-singular-values: count how many singular values exceed the tolerance threshold. This determines the numerical rank of the matrix. Use a named let loop with an index i and accumulator count. For each element, if the value > tol, increment count; otherwise keep it. This is O(n) where n is the number of singular values.")
    (context "Available: vector-length, vector-ref, >, +, =, if, let. Named let with two accumulators.")
    (before "(define (count-nonzero-singular-values singular-values tol)
  ;; TODO: count values exceeding tolerance
  0)")
    (test-file "lattice/linalg/test-svd.ss")
    (test-forms (";; Two nonzero out of four
(assert-equal 2 (count-nonzero-singular-values '#(5.0 3.0 0.0 0.0) 1e-10))"
                 ";; All zero
(assert-equal 0 (count-nonzero-singular-values '#(0.0 0.0) 1e-10))"
                 ";; All nonzero
(assert-equal 3 (count-nonzero-singular-values '#(1.0 2.0 3.0) 1e-10))"
                 ";; Near-zero below tolerance
(assert-equal 1 (count-nonzero-singular-values '#(5.0 1e-15) 1e-10))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-eigen))
    (hints ("Named let: (let loop ([i 0] [count 0]) ...)"
            "Base: (= i n) -> count"
            "Check: (> (vector-ref singular-values i) tol)"
            "If above tol: (loop (+ i 1) (+ count 1))"
            "If not: (loop (+ i 1) count)"))
    (reference-solution "(define (count-nonzero-singular-values singular-values tol)
  (let ([n (vector-length singular-values)])
       (let loop ([i 0] [count 0])
            (if (= i n)
                count
                (if (> (vector-ref singular-values i) tol)
                    (loop (+ i 1) (+ count 1))
                    (loop (+ i 1) count))))))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement matrix-frobenius-norm
  ;; ──────────────────────────────────────────────
  ;; Frobenius norm of a matrix.

  (task
    (id "linalg/svd/matrix-frobenius-norm-impl")
    (module svd)
    (tier 1)
    (type implement)
    (description "Implement matrix-frobenius-norm: compute the Frobenius norm ||A||_F = sqrt(sum of squares of all elements). This is the matrix analog of the Euclidean vector norm. Access the flat data storage of the matrix with matrix-data, which returns a vector of all elements in row-major order. Iterate over this vector, summing squares, then take the square root. Used in SVD verification to check ||A - U*Sigma*V^T||_F < tolerance.")
    (context "Available: matrix-data, vector-length, vector-ref, +, *, sqrt, =, let. Named let loop over flat data.")
    (before "(define (matrix-frobenius-norm m)
  ;; TODO: sqrt(sum of element squares)
  0)")
    (test-file "lattice/linalg/test-svd.ss")
    (test-forms (";; 2x2 matrix with entries 3 and 4: norm = 5
(let ([m (make-matrix 2 2 0)])
  (matrix-set! m 0 0 3.0)
  (matrix-set! m 0 1 4.0)
  (assert-true (< (abs (- (matrix-frobenius-norm m) 5.0)) 1e-10)))"
                 ";; Identity 2x2: norm = sqrt(2)
(let ([m (matrix-identity 2)])
  (assert-true (< (abs (- (matrix-frobenius-norm m) (sqrt 2))) 1e-10)))"
                 ";; Zero matrix: norm = 0
(assert-true (< (matrix-frobenius-norm (make-matrix 3 3 0)) 1e-10))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-eigen))
    (hints ("Get flat data: (let* ([data (matrix-data m)] [n (vector-length data)]) ...)"
            "Sum of squares: (let loop ([i 0] [sum 0]) ... (+ sum (* x x)) ...)"
            "Final: (sqrt sum-of-squares)"))
    (reference-solution "(define (matrix-frobenius-norm m)
  (let* ([data (matrix-data m)]
         [n (vector-length data)])
        (sqrt (let loop ([i 0] [sum 0])
                   (if (= i n)
                       sum
                       (let ([x (vector-ref data i)])
                            (loop (+ i 1) (+ sum (* x x)))))))))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement matrix-col
  ;; ──────────────────────────────────────────────
  ;; Extract a column from a matrix as a vector.

  (task
    (id "linalg/svd/matrix-col-impl")
    (module svd)
    (tier 1)
    (type implement)
    (description "Implement matrix-col: extract column j from a matrix as a vector. Allocate a vector of length equal to the number of rows, then iterate over rows copying matrix[i][j] into the result vector. This is a fundamental building block for the SVD algorithm: computing u_i = (1/sigma_i) * A * v_i requires extracting columns of V.")
    (context "Available: matrix-rows, matrix-ref, make-vector, vector-set!, do, =, let*. Allocation + loop.")
    (before "(define (matrix-col m j)
  ;; TODO: extract column j as vector
  (make-vector (matrix-rows m) 0))")
    (test-file "lattice/linalg/test-svd.ss")
    (test-forms (";; Extract first column
(let ([m (make-matrix 3 2 0)])
  (matrix-set! m 0 0 1.0) (matrix-set! m 0 1 2.0)
  (matrix-set! m 1 0 3.0) (matrix-set! m 1 1 4.0)
  (matrix-set! m 2 0 5.0) (matrix-set! m 2 1 6.0)
  (assert-equal '#(1.0 3.0 5.0) (matrix-col m 0)))"
                 ";; Extract second column
(let ([m (make-matrix 3 2 0)])
  (matrix-set! m 0 0 1.0) (matrix-set! m 0 1 2.0)
  (matrix-set! m 1 0 3.0) (matrix-set! m 1 1 4.0)
  (matrix-set! m 2 0 5.0) (matrix-set! m 2 1 6.0)
  (assert-equal '#(2.0 4.0 6.0) (matrix-col m 1)))"
                 ";; Single-row matrix
(let ([m (make-matrix 1 3 0)])
  (matrix-set! m 0 0 10.0)
  (matrix-set! m 0 1 20.0)
  (matrix-set! m 0 2 30.0)
  (assert-equal '#(20.0) (matrix-col m 1)))"))
    (available-modules (prelude vec matrix matrix-decomp matrix-eigen))
    (hints ("Get row count: (let* ([rows (matrix-rows m)] [result (make-vector rows 0)]) ...)"
            "do loop: (do ([i 0 (+ i 1)]) ((= i rows) result) ...)"
            "Copy element: (vector-set! result i (matrix-ref m i j))"))
    (reference-solution "(define (matrix-col m j)
  (let* ([rows (matrix-rows m)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (vector-set! result i (matrix-ref m i j)))))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 1))

) ;; end tasks
