;;; matrix.sexp — Development tasks for lattice/linalg/matrix.ss
;;;
;;; Module: matrix (tier 0, depends on prelude, vec)
;;; 468 lines, ~30 exported functions
;;; Row-major flat vector representation, construction, arithmetic,
;;; multiplication (cache-optimized i-k-j), slicing, properties.
;;; Well-tested: test-matrix.ss (327 lines).
;;;
;;; Git history:
;;;   d465c9e0 refactor(linalg): Convert matrix.ss to doc forms
;;;   e4b5bbc2 fix(lattice): Rename identity→matrix-identity (load-order collision)
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement matrix-transpose
  ;; ──────────────────────────────────────────────
  ;; Classic operation. Requires understanding row-major layout.

  (task
    (id "linalg/matrix/transpose-impl")
    (module matrix)
    (tier 0)
    (type implement)
    (description "Implement matrix-transpose: swap rows and columns. Given an m×n matrix, produce an n×m matrix where element (i,j) of the input becomes element (j,i) of the output. The matrix stores data in a flat vector in row-major order: element (i,j) is at index i*cols+j in the data vector. Use make-vector, vector-ref, vector-set! to build the result.")
    (context "Available: prelude, matrix-rows, matrix-cols, matrix-data (returns the flat data vector), make-vector, vector-ref, vector-set!. A matrix is (list 'matrix rows cols data). Row-major order: element at row i, col j is at data[i*cols + j].")
    (before "(define (matrix-transpose m)
  (doc 'type '(-> Matrix Matrix))
  (doc 'description \"Transpose a matrix\")
  ;; TODO: swap rows and columns
  ;; Input (i,j) → Output (j,i)
  ;; Input index: i*cols + j → Output index: j*rows + i
  m)")
    (test-file "lattice/linalg/test-matrix.ss")
    (test-forms ("(assert-equal '((1 4) (2 5) (3 6)) (matrix->lists (matrix-transpose (matrix-from-lists '((1 2 3) (4 5 6))))))"
                 "(assert-equal '((1 3) (2 4)) (matrix->lists (matrix-transpose (matrix-from-lists '((1 2) (3 4))))))"
                 "(let ([m (matrix-transpose (matrix-from-lists '((1 2 3))))]) (assert-equal 3 (matrix-rows m)))"
                 "(let ([m (matrix-transpose (matrix-from-lists '((1 2 3))))]) (assert-equal 1 (matrix-cols m)))"))
    (available-modules (prelude vec))
    (hints ("The output is (list 'matrix cols rows result) — note rows and cols are swapped"
            "Double loop: for each i in [0,rows), for each j in [0,cols)"
            "result[j*rows + i] = data[i*cols + j]"))
    (reference-solution "(define (matrix-transpose m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [result (make-vector (* rows cols) 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows))
            (do ([j 0 (+ j 1)])
                ((= j cols))
                (vector-set! result (+ (* j rows) i)
                             (vector-ref data (+ (* i cols) j)))))
        (list 'matrix cols rows result)))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement matrix-mul
  ;; ──────────────────────────────────────────────
  ;; The classic O(n³) operation. Tests understanding of dimensions.

  (task
    (id "linalg/matrix/mul-impl")
    (module matrix)
    (tier 0)
    (type implement)
    (description "Implement matrix multiplication: A(m×n) * B(n×p) = C(m×p). Check that A's column count equals B's row count; return an error if they mismatch. For each element C[i][j], compute the dot product of row i of A with column j of B. Use the row-major flat vector layout: element (i,j) of matrix M with c columns is at data[i*c + j].")
    (context "Available: prelude, matrix-rows, matrix-cols, matrix-data (returns flat vector), make-vector, vector-ref, vector-set!. Error convention: return (list 'error 'dimension-mismatch (list r1 c1) (list r2 c2)).")
    (before "(define (matrix-mul m1 m2)
  (doc 'type '(-> Matrix Matrix (Or Matrix Error)))
  (doc 'description \"Matrix multiplication\")
  ;; TODO: check dimensions, compute C[i][j] = sum_k A[i][k] * B[k][j]
  m1)")
    (test-file "lattice/linalg/test-matrix.ss")
    (test-forms ("(assert-equal '((19 22) (43 50)) (matrix->lists (matrix-mul (matrix-from-lists '((1 2) (3 4))) (matrix-from-lists '((5 6) (7 8))))))"
                 "(assert-equal '((22 28) (49 64)) (matrix->lists (matrix-mul (matrix-from-lists '((1 2 3) (4 5 6))) (matrix-from-lists '((1 2) (3 4) (5 6))))))"
                 "(assert-true (pair? (matrix-mul (matrix-from-lists '((1 2 3))) (matrix-from-lists '((1 2))))))"))
    (available-modules (prelude vec))
    (hints ("Check: (matrix-cols m1) must equal (matrix-rows m2)"
            "Triple nested loop: i over rows of A, j over cols of B, k over shared dimension"
            "C[i*c2 + j] += A[i*c1 + k] * B[k*c2 + j]"
            "Result shape: (matrix-rows m1) × (matrix-cols m2)"))
    (reference-solution "(define (matrix-mul m1 m2)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
       (if (not (= c1 r2))
           (list 'error 'dimension-mismatch (list r1 c1) (list r2 c2))
           (let* ([data1 (matrix-data m1)]
                  [data2 (matrix-data m2)]
                  [result (make-vector (* r1 c2) 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i r1))
                     (do ([j 0 (+ j 1)])
                         ((= j c2))
                         (do ([k 0 (+ k 1)]
                              [sum 0 (+ sum (* (vector-ref data1 (+ (* i c1) k))
                                               (vector-ref data2 (+ (* k c2) j))))])
                             ((= k c1)
                              (vector-set! result (+ (* i c2) j) sum)))))
                 (list 'matrix r1 c2 result)))))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement matrix-vec-mul
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix/vec-mul-impl")
    (module matrix)
    (tier 0)
    (type implement)
    (description "Implement matrix-vector multiplication: A(m×n) * v(n) = result(m). For each row i of A, compute the dot product with v. Check that A's column count equals the vector's length. Return a vector (not a matrix).")
    (context "Available: prelude, matrix-rows, matrix-cols, matrix-data (flat vector), make-vector, vector-ref, vector-set!, vector-length.")
    (before "(define (matrix-vec-mul m v)
  (doc 'type '(-> Matrix Vec (Or Vec Error)))
  (doc 'description \"Matrix-vector multiplication\")
  ;; TODO: for each row i, result[i] = sum_j A[i,j] * v[j]
  v)")
    (test-file "lattice/linalg/test-matrix.ss")
    (test-forms ("(assert-equal (vector 14 32) (matrix-vec-mul (matrix-from-lists '((1 2 3) (4 5 6))) (vector 1 2 3)))"
                 "(assert-equal (vector 5 11) (matrix-vec-mul (matrix-from-lists '((1 2) (3 4))) (vector 1 2)))"
                 "(assert-true (pair? (matrix-vec-mul (matrix-from-lists '((1 2 3))) (vector 1 2))))"))
    (available-modules (prelude vec))
    (hints ("Check: (matrix-cols m) must equal (vector-length v)"
            "For each row i: sum over j of data[i*cols + j] * v[j]"
            "The result is a vector of length (matrix-rows m)"))
    (reference-solution "(define (matrix-vec-mul m v)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [n (vector-length v)])
       (if (not (= cols n))
           (list 'error 'dimension-mismatch cols n)
           (let ([data (matrix-data m)]
                 [result (make-vector rows 0)])
                (do ([i 0 (+ i 1)])
                    ((= i rows) result)
                    (do ([j 0 (+ j 1)]
                         [sum 0 (+ sum (* (vector-ref data (+ (* i cols) j))
                                          (vector-ref v j)))])
                        ((= j cols)
                         (vector-set! result i sum))))))))")
    (alternatives ())
    (source-commit "d465c9e0")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement matrix-identity
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix/identity-impl")
    (module matrix)
    (tier 0)
    (type implement)
    (description "Implement matrix-identity: create the n×n identity matrix. Start with all zeros, then set each diagonal element data[i*n + i] to 1. Return as (list 'matrix n n data).")
    (context "Available: prelude, make-vector, vector-set!. Row-major layout: diagonal element (i,i) is at index i*n + i in the flat data vector.")
    (before "(define (matrix-identity n)
  (doc 'type '(-> Nat Matrix))
  (doc 'description \"n×n identity matrix\")
  ;; TODO: zeros with 1s on diagonal
  (make-matrix n n 0))")
    (test-file "lattice/linalg/test-matrix.ss")
    (test-forms ("(assert-equal '((1 0 0) (0 1 0) (0 0 1)) (matrix->lists (matrix-identity 3)))"
                 "(assert-equal '((1)) (matrix->lists (matrix-identity 1)))"
                 "(let ([I (matrix-identity 3)] [A (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]) (assert-equal (matrix->lists A) (matrix->lists (matrix-mul A I))))"))
    (available-modules (prelude vec))
    (hints ("Create data vector of size n*n initialized to 0"
            "Loop i from 0 to n-1, set data[i*n + i] = 1"
            "Return (list 'matrix n n data)"))
    (reference-solution "(define (matrix-identity n)
  (let ([data (make-vector (* n n) 0)])
       (do ([i 0 (+ i 1)])
           ((= i n) (list 'matrix n n data))
           (vector-set! data (+ (* i n) i) 1))))")
    (alternatives ())
    (source-commit "e4b5bbc2")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Write tests — matrix algebraic properties
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix/write-tests")
    (module matrix)
    (tier 0)
    (type test)
    (description "Write tests that verify algebraic properties of matrix operations: (1) A*I = I*A = A (identity property), (2) (A*B)*C = A*(B*C) (associativity — use matrix-approx-equal? for floating point), (3) (A+B)^T = A^T + B^T (transpose distributes over addition), (4) (A*B)^T = B^T * A^T (transpose reverses multiplication order), (5) matrix-trace(A+B) = trace(A) + trace(B). Use small 2×2 or 3×3 matrices for concrete examples.")
    (context "Available: full matrix module (loaded via require). Use assert-equal, assert-true, assert-false, and assert-error directly — they are pre-defined in the session. Do NOT use define-test, test-group, test-begin, or run-all-tests. Use matrix-from-lists to construct matrices and matrix->lists for comparison.")
    (before ";; Write tests for algebraic properties of matrix operations.
;; No algebraic-property tests exist yet.")
    (test-file #f)
    (test-forms ())
    (available-modules (prelude vec matrix))
    (hints ("Use (matrix-from-lists '((1 2) (3 4))) for concrete small matrices"
            "For identity: (matrix-equal? (matrix-mul A I) A)"
            "For transpose reversal: (matrix-equal? (matrix-transpose (matrix-mul A B)) (matrix-mul (matrix-transpose B) (matrix-transpose A)))"
            "matrix-approx-equal? handles floating point — use it when intermediate calculations might introduce rounding"))
    (reference-solution ";; Identity property
(let ([A (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))]
      [I (matrix-identity 3)])
  (assert-true (matrix-equal? (matrix-mul A I) A))
  (assert-true (matrix-equal? (matrix-mul I A) A)))

;; Transpose distributes over addition
(let ([A (matrix-from-lists '((1 2) (3 4)))]
      [B (matrix-from-lists '((5 6) (7 8)))])
  (assert-true (matrix-equal? (matrix-transpose (matrix-add A B))
                               (matrix-add (matrix-transpose A) (matrix-transpose B)))))

;; Transpose reverses multiplication
(let ([A (matrix-from-lists '((1 2) (3 4)))]
      [B (matrix-from-lists '((5 6) (7 8)))])
  (assert-true (matrix-equal? (matrix-transpose (matrix-mul A B))
                               (matrix-mul (matrix-transpose B) (matrix-transpose A)))))

;; Trace is additive
(let ([A (matrix-from-lists '((1 2) (3 4)))]
      [B (matrix-from-lists '((5 6) (7 8)))])
  (assert-equal (matrix-trace (matrix-add A B))
                (+ (matrix-trace A) (matrix-trace B))))")
    (alternatives ())
    (source-commit #f)
    (difficulty 3))

) ;; end tasks
