;;; matrix-solvers.sexp — Development tasks for lattice/linalg/matrix-solvers.ss
;;;
;;; Module: matrix-solvers (tier 1, depends on prelude, vec, matrix, matrix-decomp)
;;; 361 lines, ~10 exported functions
;;; Linear equation solvers: Ax=b via LU, matrix inverse, determinant,
;;; least squares via QR, Gaussian elimination, rank, condition number,
;;; Levinson-Durbin for Toeplitz systems.
;;;
;;; Git history:
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require
;;;   9ccd5eda feat(linalg): Add Levinson-Durbin algorithm for Toeplitz systems

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement matrix-solve (Ax = b)
  ;; ──────────────────────────────────────────────
  ;; Compose LU decomposition with LU-solve. Tests error handling pattern.

  (task
    (id "linalg/matrix-solvers/solve-impl")
    (module matrix-solvers)
    (tier 1)
    (type implement)
    (description "Implement matrix-solve: solve the linear system Ax = b for square matrix A. Use matrix-lu to decompose A into (list L U P), then use matrix-lu-solve to solve. If matrix-lu returns an error (a list starting with 'error), propagate it. matrix-lu-solve takes the full LU result and the vector b, and returns the solution vector x.")
    (context "Available: prelude, vec, matrix, matrix-decomp (matrix-lu, matrix-lu-solve). matrix-lu returns (list L U P) on success or (list 'error 'singular-matrix ...) on failure. matrix-lu-solve takes the LU result list and vector b, returns solution vector x.")
    (before "(define (matrix-solve a b)
  (doc 'type '(-> Matrix Vec (Or Vec Error)))
  (doc 'description \"Solve Ax = b for square A using LU decomposition\")
  ;; TODO: decompose A, check for error, solve
  b)")
    (test-file "lattice/linalg/test-matrix-solvers.ss")
    (test-forms ("(let* ([a (matrix-from-lists '((1 1 1) (0 2 5) (2 5 -1)))]
       [b (vector 6 -4 27)]
       [x (matrix-solve a b)])
  (assert-true (< (abs (- (vector-ref x 0) 5)) 1e-10))
  (assert-true (< (abs (- (vector-ref x 1) 3)) 1e-10))
  (assert-true (< (abs (- (vector-ref x 2) -2)) 1e-10)))"
                 "(let* ([a (matrix-from-lists '((2 1) (1 3)))]
       [b (vector 5 7)]
       [x (matrix-solve a b)])
  (assert-true (< (abs (- (vector-ref x 0) 8/5)) 1e-10))
  (assert-true (< (abs (- (vector-ref x 1) 9/5)) 1e-10)))"
                 ";; Singular matrix should return error
(let ([result (matrix-solve (matrix-from-lists '((1 2) (2 4))) (vector 1 2))])
  (assert-true (and (pair? result) (eq? (car result) 'error))))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("Call (matrix-lu a) and store the result"
            "Check if the result is an error: (and (pair? result) (eq? (car result) 'error))"
            "If error, return it directly. If success, call (matrix-lu-solve result b)"))
    (reference-solution "(define (matrix-solve a b)
  (let ([lu (matrix-lu a)])
       (if (and (pair? lu) (eq? (car lu) 'error))
           lu
           (matrix-lu-solve lu b))))")
    (alternatives ())
    (source-commit "9dd7f0b3")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement permutation-parity
  ;; ──────────────────────────────────────────────
  ;; Cycle decomposition — interesting standalone algorithm.

  (task
    (id "linalg/matrix-solvers/perm-parity-impl")
    (module matrix-solvers)
    (tier 1)
    (type implement)
    (description "Implement permutation-parity: compute the sign of a permutation vector. A permutation P of size n maps index i to P[i]. Decompose it into cycles: start at an unvisited index, follow i -> P[i] -> P[P[i]] -> ... until you return to the start. A cycle of length k contributes (k-1) transpositions. Sum all transpositions: even total = parity +1, odd total = parity -1. Use a visited boolean vector to track which indices have been processed.")
    (context "Available: prelude, make-vector, vector-ref, vector-set!, vector-length. The permutation vector P maps index i to P[i]. Example: #(1 0 2) swaps indices 0 and 1 (one transposition, parity = -1).")
    (before "(define (permutation-parity p)
  (doc 'type '(-> Vec Int))
  (doc 'description \"Parity of permutation: +1 for even, -1 for odd\")
  ;; TODO: cycle decomposition, count transpositions
  1)")
    (test-file #f)
    (test-forms ("(assert-equal 1 (permutation-parity (vector 0 1 2)))"
                 "(assert-equal -1 (permutation-parity (vector 1 0 2)))"
                 "(assert-equal 1 (permutation-parity (vector 1 2 0)))"
                 "(assert-equal -1 (permutation-parity (vector 2 1 0)))"))
    (available-modules (prelude vec matrix))
    (hints ("Create a visited vector of booleans, initially all #f"
            "For each unvisited index i, follow the cycle: i -> P[i] -> P[P[i]] -> ..."
            "Count the cycle length; it contributes (length - 1) transpositions"
            "A 3-cycle (1 2 0) has length 3, contributing 2 transpositions (even, parity +1)"))
    (reference-solution "(define (permutation-parity p)
  (let* ([n (vector-length p)]
         [visited (make-vector n #f)])
        (let loop ([i 0] [swaps 0])
             (if (= i n)
                 (if (even? swaps) 1 -1)
                 (if (vector-ref visited i)
                     (loop (+ i 1) swaps)
                     (let cycle-loop ([curr i] [len 0])
                          (if (vector-ref visited curr)
                              (loop (+ i 1) (+ swaps (- len 1)))
                              (begin
                               (vector-set! visited curr #t)
                               (cycle-loop (vector-ref p curr) (+ len 1))))))))))")
    (alternatives ())
    (source-commit "9dd7f0b3")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement matrix-inverse
  ;; ──────────────────────────────────────────────
  ;; Column-by-column solve using LU. Tests loop structure + matrix building.

  (task
    (id "linalg/matrix-solvers/inverse-impl")
    (module matrix-solvers)
    (tier 1)
    (type implement)
    (description "Implement matrix-inverse: compute A^(-1) by solving A * x_j = e_j for each column j of the identity matrix. First compute the LU decomposition once. If singular, propagate the error. Then for each j from 0 to n-1: create e_j (unit vector with 1 at position j), solve using matrix-lu-solve, and copy the solution into column j of the result matrix. The result is stored in a flat vector in row-major order: element (i,j) is at index i*n + j.")
    (context "Available: prelude, vec, matrix (matrix-rows, make-vector, vector-ref, vector-set!), matrix-decomp (matrix-lu, matrix-lu-solve). matrix-lu-solve takes the LU result and a vector b, returns solution vector x.")
    (before "(define (matrix-inverse a)
  (doc 'type '(-> Matrix (Or Matrix Error)))
  (doc 'description \"Compute matrix inverse via LU decomposition\")
  ;; TODO: LU decompose, then solve for each column of identity
  a)")
    (test-file "lattice/linalg/test-matrix-solvers.ss")
    (test-forms (";; 2x2 inverse
(let* ([a (matrix-from-lists '((4 7) (2 6)))]
       [ainv (matrix-inverse a)])
  (assert-true (matrix-approx-equal? (matrix-mul a ainv) (matrix-identity 2))))"
                 ";; 3x3 inverse
(let* ([a (matrix-from-lists '((1 2 1) (2 5 3) (1 3 3)))]
       [ainv (matrix-inverse a)])
  (assert-true (matrix-approx-equal? (matrix-mul a ainv) (matrix-identity 3))))"
                 ";; Singular matrix returns error
(let ([result (matrix-inverse (matrix-from-lists '((1 2) (2 4))))])
  (assert-true (and (pair? result) (eq? (car result) 'error))))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("First compute LU: (matrix-lu a). Check for error."
            "Create result data vector of size n*n"
            "For each column j: make unit vector e with e[j]=1, solve (matrix-lu-solve lu e)"
            "Copy solution x into column j: for each row i, set result[i*n + j] = x[i]"
            "Return (list 'matrix n n result-data)"))
    (reference-solution "(define (matrix-inverse a)
  (let* ([n (matrix-rows a)]
         [lu (matrix-lu a)])
        (if (and (pair? lu) (eq? (car lu) 'error))
            lu
            (let* ([inv-data (make-vector (* n n) 0)]
                   [e (make-vector n 0)])
                  (do ([j 0 (+ j 1)])
                      [(= j n) (list 'matrix n n inv-data)]
                      (do ([i 0 (+ i 1)]) [(= i n)] (vector-set! e i 0))
                      (vector-set! e j 1)
                      (let ([xj (matrix-lu-solve lu e)])
                           (do ([i 0 (+ i 1)])
                               [(= i n)]
                               (vector-set! inv-data (+ (* i n) j) (vector-ref xj i)))))))))")
    (alternatives ())
    (source-commit "9dd7f0b3")
    (difficulty 6))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement matrix-rank
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix-solvers/rank-impl")
    (module matrix-solvers)
    (tier 1)
    (type implement)
    (description "Implement matrix-rank: compute the rank of a matrix (number of linearly independent rows). First compute Row Echelon Form using matrix-gauss-elim (already available). Then count the number of non-zero rows in the REF. A row is considered non-zero if any element exceeds *matrix-tolerance* (1e-10) in absolute value. Scan each row from left to right; if you find any element above the tolerance, that row is non-zero.")
    (context "Available: prelude, vec, matrix (matrix-rows, matrix-cols, matrix-ref), matrix-gauss-elim (returns matrix in REF), *matrix-tolerance* (= 1e-10). A matrix in Row Echelon Form has all-zero rows at the bottom.")
    (before "(define (matrix-rank a)
  (doc 'type '(-> Matrix Nat))
  (doc 'description \"Rank of a matrix (number of linearly independent rows)\")
  ;; TODO: gauss-elim then count non-zero rows
  0)")
    (test-file "lattice/linalg/test-matrix-solvers.ss")
    (test-forms ("(assert-equal 2 (matrix-rank (matrix-from-lists '((1 2 3) (4 5 6) (7 8 9)))))"
                 "(assert-equal 3 (matrix-rank (matrix-identity 3)))"
                 "(assert-equal 1 (matrix-rank (matrix-from-lists '((1 2) (2 4) (3 6)))))"
                 "(assert-equal 2 (matrix-rank (matrix-from-lists '((1 0 1) (0 1 1)))))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("Call (matrix-gauss-elim a) to get REF"
            "For each row i from 0 to m-1, scan columns j from 0 to n-1"
            "If any (abs (matrix-ref ref i j)) > *matrix-tolerance*, increment rank"
            "Stop scanning that row once you find a non-zero element"))
    (reference-solution "(define (matrix-rank a)
  (let* ([ref (matrix-gauss-elim a)]
         [m (matrix-rows ref)]
         [n (matrix-cols ref)])
        (let loop ([i 0] [rank 0])
             (if (= i m)
                 rank
                 (let row-loop ([j 0])
                      (cond
                       [(= j n) (loop (+ i 1) rank)]
                       [(> (abs (matrix-ref ref i j)) *matrix-tolerance*)
                        (loop (+ i 1) (+ rank 1))]
                       [else (row-loop (+ j 1))]))))))")
    (alternatives ())
    (source-commit "9dd7f0b3")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement matrix-least-squares
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix-solvers/least-squares-impl")
    (module matrix-solvers)
    (tier 1)
    (type implement)
    (description "Implement matrix-least-squares: solve the overdetermined system Ax = b in the least-squares sense using QR decomposition. Given A is m×n with m >= n: (1) Compute QR decomposition of A using matrix-qr, which returns (list Q R). (2) Compute Q^T * b using matrix-vec-mul and matrix-transpose. (3) Solve Rx = Q^T*b using matrix-back-substitute. If QR returns an error, propagate it.")
    (context "Available: prelude, vec, matrix (matrix-transpose, matrix-vec-mul), matrix-decomp (matrix-qr, matrix-back-substitute). matrix-qr returns (list Q R) on success or (list 'error ...) on failure. matrix-back-substitute solves Ux = y for upper triangular U.")
    (before "(define (matrix-least-squares a b)
  (doc 'type '(-> Matrix Vec (Or Vec Error)))
  (doc 'description \"Least-squares solution to overdetermined Ax = b via QR\")
  ;; TODO: QR decompose, compute Q^T * b, back-substitute
  b)")
    (test-file "lattice/linalg/test-matrix-solvers.ss")
    (test-forms (";; Overdetermined 3x2 system: x + y = 2, x - y = 0, x + 2y = 3
(let* ([a (matrix-from-lists '((1 1) (1 -1) (1 2)))]
       [b (vector 2 0 3)]
       [x (matrix-least-squares a b)])
  (assert-true (< (abs (- (vector-ref x 0) 1.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref x 1) 1.0)) 1e-10)))"
                 ";; Exactly determined 2x2 should give exact solution
(let* ([a (matrix-from-lists '((1 0) (0 1)))]
       [b (vector 3 4)]
       [x (matrix-least-squares a b)])
  (assert-true (< (abs (- (vector-ref x 0) 3.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref x 1) 4.0)) 1e-10)))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("Decompose: (matrix-qr a) returns (list Q R)"
            "Check for error in the QR result"
            "Compute Q^T * b: (matrix-vec-mul (matrix-transpose Q) b)"
            "Solve: (matrix-back-substitute R qtb)"))
    (reference-solution "(define (matrix-least-squares a b)
  (let ([qr (matrix-qr a)])
       (if (and (pair? qr) (eq? (car qr) 'error))
           qr
           (let* ([q (car qr)]
                  [r (cadr qr)]
                  [qt-b (matrix-vec-mul (matrix-transpose q) b)])
                 (matrix-back-substitute r qt-b)))))")
    (alternatives ())
    (source-commit "9dd7f0b3")
    (difficulty 4))

) ;; end tasks
