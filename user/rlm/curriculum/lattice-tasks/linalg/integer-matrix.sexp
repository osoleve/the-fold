;;; integer-matrix.sexp — Development tasks for lattice/linalg/integer-matrix.ss
;;;
;;; Module: integer-matrix (tier 0, depends on prelude, matrix, modular)
;;; 666 lines, ~10 exported functions
;;; Smith and Hermite normal forms for integer matrices. Row/column
;;; operations on flat-vector representations, pivot finding, GCD-based
;;; elimination. Used for Diophantine equations and computational topology.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/linalg/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement make-identity-vec
  ;; ──────────────────────────────────────────────
  ;; Flat-vector identity matrix.

  (task
    (id "linalg/integer-matrix/make-identity-vec-impl")
    (module integer-matrix)
    (tier 0)
    (type implement)
    (description "Implement make-identity-vec: create an n×n identity matrix stored as a flat vector of length n². In the flat row-major layout, element (i,j) is at index i*n+j. The identity matrix has 1s on the diagonal (where i=j) and 0s elsewhere. Create a zero vector of size n², then set each diagonal element to 1 using a loop from 0 to n-1.")
    (context "Available: make-vector, vector-set!, do, +, *. One allocation + one loop setting diagonal elements.")
    (before "(define (make-identity-vec n)
  ;; TODO: flat n×n identity matrix
  (make-vector (* n n) 0))")
    (test-file "lattice/linalg/test-integer-matrix.ss")
    (test-forms (";; 3x3 identity as flat vector
(assert-equal '#(1 0 0 0 1 0 0 0 1) (make-identity-vec 3))"
                 ";; 1x1 identity
(assert-equal '#(1) (make-identity-vec 1))"
                 ";; 2x2 identity
(assert-equal '#(1 0 0 1) (make-identity-vec 2))"))
    (available-modules (prelude matrix modular))
    (hints ("Allocate: (let ([data (make-vector (* n n) 0)]) ...)"
            "Diagonal index for element (i,i): (+ (* i n) i)"
            "Loop: (do ([i 0 (+ i 1)]) ((= i n) data) (vector-set! data (+ (* i n) i) 1))"))
    (reference-solution "(define (make-identity-vec n)
  (let ([data (make-vector (* n n) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) data)
      (vector-set! data (+ (* i n) i) 1))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement int-mat-ref
  ;; ──────────────────────────────────────────────
  ;; Row-major flat vector indexing.

  (task
    (id "linalg/integer-matrix/int-mat-ref-impl")
    (module integer-matrix)
    (tier 0)
    (type implement)
    (description "Implement int-mat-ref: access element (i,j) from a flat row-major vector. When an m×n matrix is stored as a flat vector, element at row i, column j is at vector index i*cols+j, where cols is the number of columns. This is the fundamental addressing operation for all integer matrix algorithms.")
    (context "Available: vector-ref, +, *. Single expression.")
    (before "(define (int-mat-ref data cols i j)
  ;; TODO: data[i*cols + j]
  0)")
    (test-file "lattice/linalg/test-integer-matrix.ss")
    (test-forms (";; 2x3 matrix: ((1 2 3) (4 5 6))
(let ([data (vector 1 2 3 4 5 6)])
  (assert-equal 1 (int-mat-ref data 3 0 0))
  (assert-equal 3 (int-mat-ref data 3 0 2))
  (assert-equal 4 (int-mat-ref data 3 1 0))
  (assert-equal 6 (int-mat-ref data 3 1 2)))"
                 ";; 3x3 identity
(let ([data (make-identity-vec 3)])
  (assert-equal 1 (int-mat-ref data 3 0 0))
  (assert-equal 0 (int-mat-ref data 3 0 1))
  (assert-equal 1 (int-mat-ref data 3 2 2)))"))
    (available-modules (prelude matrix modular))
    (hints ("Index formula: i * cols + j"
            "Return: (vector-ref data (+ (* i cols) j))"))
    (reference-solution "(define (int-mat-ref data cols i j)
  (vector-ref data (+ (* i cols) j)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement add-row-multiple!
  ;; ──────────────────────────────────────────────
  ;; Elementary row operation: row i1 += k * row i2.

  (task
    (id "linalg/integer-matrix/add-row-multiple-impl")
    (module integer-matrix)
    (tier 0)
    (type implement)
    (description "Implement add-row-multiple!: perform the elementary row operation row[i1] += k * row[i2]. This is the workhorse of Gaussian elimination and Smith normal form. Loop over all columns j from 0 to cols-1. For each j, read the current value at (i1,j), add k times the value at (i2,j), and write back to (i1,j). Use int-mat-ref and int-mat-set! for access.")
    (context "Available: int-mat-ref, int-mat-set!, +, *, do. One loop over columns with read-modify-write.")
    (before "(define (add-row-multiple! data rows cols i1 i2 k)
  ;; TODO: row i1 += k * row i2
  (void))")
    (test-file "lattice/linalg/test-integer-matrix.ss")
    (test-forms (";; 2x3 matrix: row 0 += 2 * row 1
(let ([data (vector 1 2 3 4 5 6)])
  (add-row-multiple! data 2 3 0 1 2)
  ;; row 0 was (1,2,3), row 1 is (4,5,6), so row 0 = (1+8, 2+10, 3+12) = (9, 12, 15)
  (assert-equal '#(9 12 15 4 5 6) data))"
                 ";; Adding 0 times does nothing
(let ([data (vector 1 2 3 4)])
  (add-row-multiple! data 2 2 0 1 0)
  (assert-equal '#(1 2 3 4) data))"
                 ";; Negative multiplier: row 0 -= row 1
(let ([data (vector 5 10 3 7)])
  (add-row-multiple! data 2 2 0 1 -1)
  (assert-equal '#(2 3 3 7) data))"))
    (available-modules (prelude matrix modular))
    (hints ("Loop: (do ([j 0 (+ j 1)]) ((= j cols)) ...)"
            "Read: (int-mat-ref data cols i1 j)"
            "Add: (+ current (* k (int-mat-ref data cols i2 j)))"
            "Write: (int-mat-set! data cols i1 j new-value)"))
    (reference-solution "(define (add-row-multiple! data rows cols i1 i2 k)
  (do ([j 0 (+ j 1)])
      ((= j cols))
    (int-mat-set! data cols i1 j
                  (+ (int-mat-ref data cols i1 j)
                     (* k (int-mat-ref data cols i2 j))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement negate-row!
  ;; ──────────────────────────────────────────────
  ;; Negate all elements in a row.

  (task
    (id "linalg/integer-matrix/negate-row-impl")
    (module integer-matrix)
    (tier 0)
    (type implement)
    (description "Implement negate-row!: negate all elements in row i of a flat-vector matrix. This operation is used in Smith normal form to ensure the pivot is positive. Loop over all columns j from 0 to cols-1, negating each element at (i,j). Use int-mat-ref and int-mat-set! for access.")
    (context "Available: int-mat-ref, int-mat-set!, -, do. One loop with negate-and-write.")
    (before "(define (negate-row! data rows cols i)
  ;; TODO: row[i] = -row[i]
  (void))")
    (test-file "lattice/linalg/test-integer-matrix.ss")
    (test-forms (";; Negate row 1 of 2x3 matrix
(let ([data (vector 1 2 3 4 5 6)])
  (negate-row! data 2 3 1)
  (assert-equal '#(1 2 3 -4 -5 -6) data))"
                 ";; Negate row 0
(let ([data (vector -3 7 0 1)])
  (negate-row! data 2 2 0)
  (assert-equal '#(3 -7 0 1) data))"))
    (available-modules (prelude matrix modular))
    (hints ("Loop: (do ([j 0 (+ j 1)]) ((= j cols)) ...)"
            "Each step: (int-mat-set! data cols i j (- (int-mat-ref data cols i j)))"))
    (reference-solution "(define (negate-row! data rows cols i)
  (do ([j 0 (+ j 1)])
      ((= j cols))
    (int-mat-set! data cols i j (- (int-mat-ref data cols i j)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement smith-find-pivot
  ;; ──────────────────────────────────────────────
  ;; Find smallest non-zero element in submatrix.

  (task
    (id "linalg/integer-matrix/smith-find-pivot-impl")
    (module integer-matrix)
    (tier 0)
    (type implement)
    (description "Implement smith-find-pivot: find the position of the smallest non-zero element in the submatrix starting at row k, column k. This is used by Smith normal form to select the best pivot for GCD reduction. Search all elements at positions (i,j) where i >= k and j >= k. Track the position (i,j) and absolute value of the best candidate. Return (i . j) as a pair, or #f if the submatrix is all zeros. Use nested named let loops: outer over rows i from k to m-1, inner over columns j from k to n-1.")
    (context "Available: int-mat-ref, abs, not, =, <, or, >=, cons, let. Nested named let loops tracking best position and best absolute value.")
    (before "(define (smith-find-pivot D m n k)
  ;; TODO: find (i . j) of smallest |D[i][j]| > 0 for i,j >= k
  #f)")
    (test-file "lattice/linalg/test-integer-matrix.ss")
    (test-forms (";; 3x3 matrix, starting at k=0: smallest non-zero is 1 at (0,0)
(let ([data (vector 1 0 3 0 2 0 0 0 4)])
  (assert-equal '(0 . 0) (smith-find-pivot data 3 3 0)))"
                 ";; Starting at k=1: submatrix is ((2 0) (0 4)), smallest is 2 at (1,1)
(let ([data (vector 1 0 3 0 2 0 0 0 4)])
  (assert-equal '(1 . 1) (smith-find-pivot data 3 3 1)))"
                 ";; All zeros in submatrix
(let ([data (vector 1 2 0 0 0 0 0 0 0)])
  (assert-false (smith-find-pivot data 3 3 1)))"
                 ";; Finds smallest non-zero: |2| < |5| at k=1
(let ([data (vector 0 0 3 0 0 2 0 5 0)])
  (assert-equal '(1 . 2) (smith-find-pivot data 3 3 1)))"))
    (available-modules (prelude matrix modular))
    (hints ("Outer loop: (let loop-i ([i k] [best #f] [best-val #f]) ...)"
            "Inner loop: (let loop-j ([j k] [best best] [best-val best-val]) ...)"
            "For each element: val = (int-mat-ref D n i j)"
            "Update if: (and (not (= val 0)) (or (not best-val) (< (abs val) best-val)))"
            "Track position as (cons i j) and absolute value"))
    (reference-solution "(define (smith-find-pivot D m n k)
  (let loop-i ([i k] [best #f] [best-val #f])
    (if (>= i m)
        best
        (let loop-j ([j k] [best best] [best-val best-val])
          (if (>= j n)
              (loop-i (+ i 1) best best-val)
              (let ([val (int-mat-ref D n i j)])
                (if (and (not (= val 0))
                         (or (not best-val)
                             (< (abs val) best-val)))
                    (loop-j (+ j 1) (cons i j) (abs val))
                    (loop-j (+ j 1) best best-val))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
