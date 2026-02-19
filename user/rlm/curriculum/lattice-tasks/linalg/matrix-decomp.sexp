;;; matrix-decomp.sexp — Development tasks for lattice/linalg/matrix-decomp.ss
;;;
;;; Module: matrix-decomp (tier 1, depends on prelude, vec, matrix)
;;; 292 lines, ~8 exported functions
;;; LU decomposition (partial pivoting), QR (modified Gram-Schmidt),
;;; Cholesky, determinant.
;;;
;;; Git history:
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require
;;;   d1ae3197 fix: Patch 12 P1 algorithmic and mathematical bugs

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement forward substitution
  ;; ──────────────────────────────────────────────
  ;; Solve Ly = b where L is lower triangular. Foundation for
  ;; LU-based solvers.

  (task
    (id "linalg/matrix-decomp/forward-sub-impl")
    (module matrix-solvers)
    (tier 1)
    (type implement)
    (description "Implement forward substitution: solve Ly = b where L is a lower triangular matrix. For each row i from top to bottom: y[i] = (b[i] - sum(L[i,j]*y[j] for j < i)) / L[i,i]. This is O(n²). Use matrix-ref to access matrix elements and vector-ref/vector-set! for vectors.")
    (context "Available: prelude, vec, matrix (matrix-ref, matrix-rows), make-vector, vector-ref, vector-set!. The matrix L is lower triangular: L[i,j] = 0 for j > i.")
    (before "(define (matrix-forward-substitute l b)
  (doc 'type '(-> Matrix Vec Vec))
  (doc 'description \"Solve Ly = b where L is lower triangular\")
  ;; TODO: for each i, y[i] = (b[i] - sum of L[i,j]*y[j]) / L[i,i]
  b)")
    (test-file #f)
    (test-forms ("(let* ([L (matrix-from-lists '((2 0 0) (1 3 0) (4 2 1)))]
       [b (vector 4 7 15)]
       [y (matrix-forward-substitute L b)])
  (assert-equal 2 (vector-ref y 0))
  (assert-true (< (abs (- (vector-ref y 1) 5/3)) 1e-10)))"
                 "(let* ([L (matrix-from-lists '((1 0) (3 1)))]
       [b (vector 1 4)]
       [y (matrix-forward-substitute L b)])
  (assert-equal 1 (vector-ref y 0))
  (assert-equal 1 (vector-ref y 1)))"))
    (available-modules (prelude vec matrix))
    (hints ("Create result vector y of size n"
            "For each i: compute sum of L[i,j]*y[j] for j from 0 to i-1"
            "Then y[i] = (b[i] - sum) / L[i,i]"
            "Use a nested loop: outer over i, inner over j from 0 to i-1"))
    (reference-solution "(define (matrix-forward-substitute l b)
  (let* ([n (vector-length b)]
         [y (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) y)
            (let ([sum (let loop ([j 0] [s 0])
                            (if (= j i)
                                s
                                (loop (+ j 1)
                                      (+ s (* (matrix-ref l i j)
                                              (vector-ref y j))))))])
                 (vector-set! y i (/ (- (vector-ref b i) sum)
                                     (matrix-ref l i i)))))))")
    (alternatives ())
    (source-commit "d1ae3197")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement back substitution
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix-decomp/back-sub-impl")
    (module matrix-solvers)
    (tier 1)
    (type implement)
    (description "Implement back substitution: solve Ux = y where U is an upper triangular matrix. For each row i from bottom to top: x[i] = (y[i] - sum(U[i,j]*x[j] for j > i)) / U[i,i]. This is O(n²). Process rows in reverse order (i from n-1 down to 0).")
    (context "Available: prelude, vec, matrix (matrix-ref, matrix-rows), make-vector, vector-ref, vector-set!. The matrix U is upper triangular: U[i,j] = 0 for j < i.")
    (before "(define (matrix-back-substitute u y)
  (doc 'type '(-> Matrix Vec Vec))
  (doc 'description \"Solve Ux = y where U is upper triangular\")
  ;; TODO: for each i (from bottom), x[i] = (y[i] - sum of U[i,j]*x[j]) / U[i,i]
  y)")
    (test-file #f)
    (test-forms ("(let* ([U (matrix-from-lists '((2 1 3) (0 4 2) (0 0 5)))]
       [y (vector 9 10 5)]
       [x (matrix-back-substitute U y)])
  (assert-equal 1 (vector-ref x 2))
  (assert-equal 2 (vector-ref x 1))
  (assert-equal 1 (vector-ref x 0)))"
                 "(let* ([U (matrix-from-lists '((1 2) (0 3)))]
       [y (vector 5 6)]
       [x (matrix-back-substitute U y)])
  (assert-equal 2 (vector-ref x 1))
  (assert-equal 1 (vector-ref x 0)))"))
    (available-modules (prelude vec matrix))
    (hints ("Create result vector x of size n"
            "Loop i from n-1 down to 0"
            "For each i: sum U[i,j]*x[j] for j from i+1 to n-1"
            "x[i] = (y[i] - sum) / U[i,i]"))
    (reference-solution "(define (matrix-back-substitute u y)
  (let* ([n (vector-length y)]
         [x (make-vector n 0)])
        (do ([i (- n 1) (- i 1)])
            ((< i 0) x)
            (let ([sum (let loop ([j (+ i 1)] [s 0])
                            (if (= j n)
                                s
                                (loop (+ j 1)
                                      (+ s (* (matrix-ref u i j)
                                              (vector-ref x j))))))])
                 (vector-set! x i (/ (- (vector-ref y i) sum)
                                     (matrix-ref u i i)))))))")
    (alternatives ())
    (source-commit "d1ae3197")
    (difficulty 4))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement determinant via LU
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix-decomp/det-impl")
    (module matrix-decomp)
    (tier 1)
    (type implement)
    (description "Implement matrix-det: compute the determinant of a square matrix using its LU decomposition. det(A) = sign(P) * product(U[i,i] for all i). Use matrix-lu which returns (list L U P) where P is a permutation vector. If LU returns an error with tag 'singular-matrix, the determinant is 0. Compute sign(P) by counting inversions in the permutation (pairs where P[i] > P[j] for i < j); even inversions = +1, odd = -1.")
    (context "Available: prelude, vec, matrix, matrix-lu (returns (list L U P) or error). matrix-ref, matrix-rows for accessing U's diagonal. P is a vector of indices representing the row permutation.")
    (before "(define (matrix-det a)
  (doc 'type '(-> Matrix (Or Num Error)))
  (doc 'description \"Determinant via LU decomposition\")
  ;; TODO: LU decompose, handle singular case, compute sign(P) * product(diag(U))
  0)")
    (test-file #f)
    (test-forms ("(assert-equal -2 (matrix-det (matrix-from-lists '((1 2) (3 4)))))"
                 "(assert-equal 0 (matrix-det (matrix-from-lists '((1 2) (2 4)))))"
                 "(assert-equal 1 (matrix-det (matrix-identity 3)))"
                 "(assert-equal 1 (matrix-det (matrix-from-lists '((1 2 3) (0 1 4) (5 6 0)))))"))
    (available-modules (prelude vec matrix))
    (hints ("Call (matrix-lu a) — check if result is an error"
            "If singular-matrix error, return 0"
            "Product of U's diagonal: loop over i, multiply matrix-ref u i i"
            "Permutation sign: count pairs (i,j) with i<j and P[i]>P[j] — even count → +1, odd → -1"
            "Final result: sign * product"))
    (reference-solution "(define (matrix-det a)
  (let ([result (matrix-lu a)])
       (if (and (pair? result) (eq? (car result) 'error))
           (if (eq? (cadr result) 'singular-matrix)
               0
               result)
           (let ([u (cadr result)]
                 [p (caddr result)])
                (let ([n (matrix-rows u)])
                     (let ([prod (do ([i 0 (+ i 1)]
                                      [acc 1 (* acc (matrix-ref u i i))])
                                     ((= i n) acc))]
                           [sign (let outer ([i 0] [swaps 0])
                                      (if (= i n)
                                          (if (even? swaps) 1 -1)
                                          (let inner ([j (+ i 1)] [s swaps])
                                               (if (= j n)
                                                   (outer (+ i 1) s)
                                                   (inner (+ j 1)
                                                          (if (> (vector-ref p i) (vector-ref p j))
                                                              (+ s 1) s))))))])
                          (* sign prod)))))))")
    (alternatives ())
    (source-commit "d1ae3197")
    (difficulty 5))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement Cholesky decomposition
  ;; ──────────────────────────────────────────────

  (task
    (id "linalg/matrix-decomp/cholesky-impl")
    (module matrix-decomp)
    (tier 1)
    (type implement)
    (description "Implement Cholesky decomposition: given a symmetric positive-definite matrix A, find lower triangular L such that A = L * L^T. For each row i and column j <= i: if i=j, L[i,i] = sqrt(A[i,i] - sum(L[i,k]^2 for k<i)). If i!=j, L[i,j] = (A[i,j] - sum(L[i,k]*L[j,k] for k<j)) / L[j,j]. Return error if not positive definite (when the value under sqrt is <= 0). Return error if not square.")
    (context "Available: prelude, vec, matrix (matrix-ref, matrix-rows, matrix-cols, make-matrix), matrix-set! (mutates data: (matrix-set! m i j val)), sqrt. Error convention: return (list 'error 'not-positive-definite i val) or (list 'error 'not-square r c).")
    (before "(define (matrix-cholesky a)
  (doc 'type '(-> Matrix (Or (List Matrix) Error)))
  (doc 'description \"Cholesky decomposition: A = L * L^T\")
  ;; TODO: compute lower triangular L
  ;; Return (list L)
  (list a))")
    (test-file #f)
    (test-forms (";; 2x2 positive definite matrix
(let* ([A (matrix-from-lists '((4 2) (2 3)))]
       [result (matrix-cholesky A)]
       [L (car result)]
       [LT (matrix-transpose L)]
       [reconstructed (matrix-mul L LT)])
  (assert-true (matrix-approx-equal? A reconstructed)))"
                 ";; Non-positive-definite matrix should error
(assert-true (pair? (matrix-cholesky (matrix-from-lists '((1 2) (2 1))))))"
                 ";; Identity matrix
(let* ([result (matrix-cholesky (matrix-identity 3))]
       [L (car result)])
  (assert-true (matrix-approx-equal? L (matrix-identity 3))))"))
    (available-modules (prelude vec matrix))
    (hints ("Check square: (= (matrix-rows a) (matrix-cols a))"
            "Double loop: outer over i (rows), inner over j from 0 to i"
            "Diagonal (i=j): val = A[i,i] - sum(L[i,k]^2 for k<i). If val <= 0, return error. L[i,i] = sqrt(val)"
            "Off-diagonal (j<i): L[i,j] = (A[i,j] - sum(L[i,k]*L[j,k] for k<j)) / L[j,j]"
            "Use matrix-set! to fill L incrementally"))
    (reference-solution "(define (matrix-cholesky a)
  (let ([n (matrix-rows a)])
       (if (not (= n (matrix-cols a)))
           (list 'error 'not-square (matrix-rows a) (matrix-cols a))
           (let ([l (make-matrix n n 0)])
                (let row-loop ([i 0])
                     (if (= i n)
                         (list l)
                         (let col-loop ([j 0])
                              (if (> j i)
                                  (row-loop (+ i 1))
                                  (let ([sum (let k-loop ([k 0] [s 0])
                                                  (if (= k j)
                                                      s
                                                      (k-loop (+ k 1)
                                                              (+ s (* (matrix-ref l i k)
                                                                      (matrix-ref l j k))))))])
                                       (cond
                                        [(= i j)
                                         (let ([val (- (matrix-ref a i i) sum)])
                                              (if (<= val 0)
                                                  (list 'error 'not-positive-definite i val)
                                                  (begin
                                                   (matrix-set! l i i (sqrt val))
                                                   (col-loop (+ j 1)))))]
                                        [else
                                         (matrix-set! l i j (/ (- (matrix-ref a i j) sum)
                                                               (matrix-ref l j j)))
                                         (col-loop (+ j 1))]))))))))))")
    (alternatives ())
    (source-commit "d1ae3197")
    (difficulty 6))

) ;; end tasks
