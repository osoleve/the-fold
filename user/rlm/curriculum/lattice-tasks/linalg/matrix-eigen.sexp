;;; matrix-eigen.sexp — Development tasks for lattice/linalg/matrix-eigen.ss
;;;
;;; Module: matrix-eigen (tier 1, depends on prelude, vec, matrix, matrix-decomp)
;;; 638 lines, ~20 exported functions
;;; Eigenvalue and eigenvector computation: power iteration (dominant eigenvalue),
;;; QR algorithm with Wilkinson shift (all eigenvalues), inverse iteration
;;; (eigenvectors), symmetric eigen (orthogonal eigenvectors), spectral radius.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   9dd7f0b3 feat(module): migrate lattice/linalg/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement qr-converged?
  ;; ──────────────────────────────────────────────
  ;; Check if matrix is sufficiently upper triangular.

  (task
    (id "linalg/matrix-eigen/qr-converged-impl")
    (module matrix-eigen)
    (tier 0)
    (type implement)
    (description "Implement qr-converged?: check if a matrix has converged to upper triangular form (Schur form). The QR algorithm iteratively transforms a matrix toward upper triangular. Convergence is detected when all elements below the diagonal are smaller than a tolerance. Scan the lower triangle: for each row i > 0 and column j < i, if |a[i,j]| > tol, return #f immediately. If all pass, return #t. Use two nested named let loops with early exit.")
    (context "Available: matrix-rows, matrix-ref, abs, >, >=, let. Two nested named let loops.")
    (before "(define (qr-converged? a tol)
  ;; TODO: are all subdiagonal elements < tol?
  #f)")
    (test-file "lattice/linalg/test-matrix-eigen.ss")
    (test-forms (";; Identity matrix is already upper triangular
(assert-true (qr-converged? (matrix-identity 3) 1e-10))"
                 ";; Diagonal matrix is upper triangular
(assert-true (qr-converged? (list 'matrix 3 3 (vector 5 2 3 0 7 1 0 0 9)) 1e-10))"
                 ";; Matrix with large subdiagonal element
(assert-false (qr-converged? (list 'matrix 3 3 (vector 1 2 3 4 5 6 7 8 9)) 1e-10))"
                 ";; Nearly converged: tiny subdiagonal
(assert-true (qr-converged? (list 'matrix 2 2 (vector 3.0 0.5 1e-12 2.0)) 1e-10))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("Get size: (let ([n (matrix-rows a)]) ...)"
            "Outer: (let row-loop ([i 1]) ...) — start at row 1 (row 0 has nothing below diagonal)"
            "Inner: (let col-loop ([j 0]) ...) — columns 0 to i-1"
            "Check: if (> (abs (matrix-ref a i j)) tol) -> #f"
            "If col-loop finishes without #f, advance to next row"))
    (reference-solution "(define (qr-converged? a tol)
  (let ([n (matrix-rows a)])
       (let row-loop ([i 1])
            (if (>= i n)
                #t
                (let col-loop ([j 0])
                     (if (>= j i)
                         (row-loop (+ i 1))
                         (if (> (abs (matrix-ref a i j)) tol)
                             #f
                             (col-loop (+ j 1)))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement symmetric-converged?
  ;; ──────────────────────────────────────────────
  ;; Check if symmetric matrix has converged to diagonal.

  (task
    (id "linalg/matrix-eigen/symmetric-converged-impl")
    (module matrix-eigen)
    (tier 0)
    (type implement)
    (description "Implement symmetric-converged?: check if a symmetric matrix has converged to diagonal form. For symmetric matrices, the QR algorithm converges to a diagonal (not just upper triangular) because symmetric + upper triangular = diagonal. Check all off-diagonal elements: for every (i,j) where i != j, if |a[i,j]| > tol, return #f. This is stricter than qr-converged? which only checks below the diagonal. Use two nested named let loops over all rows and columns.")
    (context "Available: matrix-rows, matrix-ref, abs, >, >=, =, not, and, let. Two nested named let loops checking all off-diagonal.")
    (before "(define (symmetric-converged? a tol)
  ;; TODO: are all off-diagonal elements < tol?
  #f)")
    (test-file "lattice/linalg/test-matrix-eigen.ss")
    (test-forms (";; Diagonal matrix is converged
(assert-true (symmetric-converged? (list 'matrix 3 3 (vector 1 0 0 0 2 0 0 0 3)) 1e-10))"
                 ";; Non-diagonal symmetric matrix is NOT converged
(assert-false (symmetric-converged? (list 'matrix 3 3 (vector 2.0 1.0 0.0 1.0 3.0 1.0 0.0 1.0 2.0)) 1e-10))"
                 ";; 1x1 matrix is trivially converged
(assert-true (symmetric-converged? (list 'matrix 1 1 (vector 5.0)) 1e-10))"
                 ";; Nearly diagonal: tiny off-diagonals
(assert-true (symmetric-converged? (list 'matrix 2 2 (vector 3.0 1e-12 1e-12 2.0)) 1e-10))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("Outer: (let row-loop ([i 0]) ...) — all rows"
            "Inner: (let col-loop ([j 0]) ...) — all columns"
            "Skip diagonal: (and (not (= i j)) (> (abs (matrix-ref a i j)) tol)) -> #f"
            "Otherwise advance to next column/row"))
    (reference-solution "(define (symmetric-converged? a tol)
  (let ([n (matrix-rows a)])
       (let row-loop ([i 0])
            (if (>= i n)
                #t
                (let col-loop ([j 0])
                     (if (>= j n)
                         (row-loop (+ i 1))
                         (if (and (not (= i j))
                                  (> (abs (matrix-ref a i j)) tol))
                             #f
                             (col-loop (+ j 1)))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement verify-eigenvalue
  ;; ──────────────────────────────────────────────
  ;; Check if (lambda, v) is a valid eigenpair.

  (task
    (id "linalg/matrix-eigen/verify-eigenvalue-impl")
    (module matrix-eigen)
    (tier 0)
    (type implement)
    (description "Implement verify-eigenvalue: check whether (lambda, v) is an eigenpair of matrix A. The defining property of an eigenpair is Av = lambda*v. So compute Av using matrix-vec-mul, compute lambda*v using vec-scale, take their difference with vec-sub, and check that the norm of the residual (vec-norm of the difference) is less than the tolerance. This is the standard verification test for eigenvalue computations.")
    (context "Available: matrix-vec-mul, vec-scale, vec-sub, vec-norm, <, abs, let*. Four-step pipeline: mul, scale, sub, norm.")
    (before "(define (verify-eigenvalue a lambda v tol)
  ;; TODO: is ||Av - lambda*v|| < tol?
  #f)")
    (test-file "lattice/linalg/test-matrix-eigen.ss")
    (test-forms (";; Identity matrix: eigenvalue 1, any unit vector
(assert-true (verify-eigenvalue (matrix-identity 3) 1.0 '#(1.0 0.0 0.0) 1e-8))"
                 ";; Wrong eigenvalue should fail
(assert-false (verify-eigenvalue (matrix-identity 3) 2.0 '#(1.0 0.0 0.0) 1e-8))"
                 ";; Diagonal matrix: eigenvalue = diagonal entry
(let ([m (list 'matrix 2 2 (vector 3.0 0.0 0.0 5.0))])
  (assert-true (verify-eigenvalue m 3.0 '#(1.0 0.0) 1e-8)))"
                 ";; Wrong eigenvector should fail
(let ([m (list 'matrix 2 2 (vector 3.0 0.0 0.0 5.0))])
  (assert-false (verify-eigenvalue m 3.0 '#(0.0 1.0) 1e-8)))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("Step 1: (let* ([av (matrix-vec-mul a v)]) ...)"
            "Step 2: (let* ([lambda-v (vec-scale lambda v)]) ...)"
            "Step 3: (let* ([diff (vec-sub av lambda-v)]) ...)"
            "Step 4: (let* ([residual (vec-norm diff)]) (< residual tol))"))
    (reference-solution "(define (verify-eigenvalue a lambda v tol)
  (let* ([av (matrix-vec-mul a v)]
         [lambda-v (vec-scale lambda v)]
         [diff (vec-sub av lambda-v)]
         [residual (vec-norm diff)])
        (< residual tol)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement wilkinson-shift
  ;; ──────────────────────────────────────────────
  ;; Compute Wilkinson shift for QR algorithm acceleration.

  (task
    (id "linalg/matrix-eigen/wilkinson-shift-impl")
    (module matrix-eigen)
    (tier 1)
    (type implement)
    (description "Implement wilkinson-shift: compute the Wilkinson shift from the bottom 2x2 block of the active portion of the matrix. The shift accelerates QR convergence, especially for clustered eigenvalues. Given active-size, extract the 2x2 block at indices (j,j), (j,i), (i,j), (i,i) where i = active-size-1 and j = active-size-2. Compute d = (a[j,j] - a[i,i])/2, then the shift is a[i,i] - sign(d)*a[i,j]*a[j,i] / (|d| + sqrt(d^2 + a[i,j]*a[j,i])). Guard against negative discriminant from numerical errors with a max/abs fallback. Return a[i,i] if the denominator is too small.")
    (context "Available: matrix-ref, -, /, *, +, abs, sqrt, <, >=, if, let*. Arithmetic on 2x2 block entries.")
    (before "(define (wilkinson-shift a active-size)
  ;; TODO: compute Wilkinson shift from bottom 2x2
  0)")
    (test-file "lattice/linalg/test-matrix-eigen.ss")
    (test-forms (";; Single element: shift is 0
(assert-equal 0 (wilkinson-shift (matrix-identity 3) 1))"
                 ";; 2x2 diagonal: shift should be close to one eigenvalue
(let* ([m (list 'matrix 2 2 (vector 3.0 0.0 0.0 2.0))]
       [s (wilkinson-shift m 2)])
  (assert-true (or (< (abs (- s 3.0)) 0.01) (< (abs (- s 2.0)) 0.01))))"
                 ";; Symmetric 2x2: shift is computable
(let ([s (wilkinson-shift (list 'matrix 2 2 (vector 3.0 1.0 1.0 2.0)) 2)])
  (assert-true (number? s)))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("If active-size < 2, return 0"
            "Indices: i = active-size - 1, j = active-size - 2"
            "Extract 2x2: a[j,j], a[i,i], a[j,i], a[i,j]"
            "d = (a[j,j] - a[i,i]) / 2, sign-d = if d >= 0 then 1 else -1"
            "discriminant = d^2 + a[j,i]*a[i,j]; if < 0 use |d| as fallback"
            "denom = |d| + sqrt(discriminant); if too small return a[i,i]"
            "shift = a[i,i] - sign-d * a[j,i] * a[i,j] / denom"))
    (reference-solution "(define (wilkinson-shift a active-size)
  (if (< active-size 2)
      0
      (let* ([i (- active-size 1)]
             [j (- active-size 2)]
             [a-jj (matrix-ref a j j)]
             [a-ii (matrix-ref a i i)]
             [a-ji (matrix-ref a i j)]
             [a-ij (matrix-ref a j i)]
             [d (/ (- a-jj a-ii) 2)]
             [sign-d (if (>= d 0) 1 -1)]
             [discriminant (+ (* d d) (* a-ji a-ij))]
             [denom (if (< discriminant 0)
                        (abs d)
                        (+ (abs d) (sqrt discriminant)))])
            (if (< denom 1e-10)
                a-ii
                (- a-ii (/ (* sign-d a-ji a-ij) denom))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement matrix-diagonal extraction
  ;; ──────────────────────────────────────────────
  ;; Extract the diagonal of a matrix as a vector.

  (task
    (id "linalg/matrix-eigen/matrix-diagonal-impl")
    (module matrix-eigen)
    (tier 0)
    (type implement)
    (description "Implement matrix-diagonal: extract the diagonal elements of a square matrix as a vector. The diagonal of an n x n matrix A is the vector [A[0,0], A[1,1], ..., A[n-1,n-1]]. This is used throughout eigenvalue computation — the diagonal of a converged QR iteration contains the eigenvalues. Allocate a vector of size n, then fill each element with matrix-ref a i i using a do loop.")
    (context "Available: matrix-rows, matrix-ref, make-vector, vector-set!, do, =, let. One allocation + one loop.")
    (before "(define (matrix-diagonal a)
  ;; TODO: extract diagonal as vector
  (make-vector (matrix-rows a) 0))")
    (test-file "lattice/linalg/test-matrix-eigen.ss")
    (test-forms (";; 3x3 matrix diagonal
(assert-equal '#(5 7 9)
  (matrix-diagonal (list 'matrix 3 3 (vector 5 2 3 0 7 1 0 0 9))))"
                 ";; Identity diagonal
(assert-equal '#(1 1 1) (matrix-diagonal (matrix-identity 3)))"
                 ";; 1x1 matrix
(assert-equal '#(42) (matrix-diagonal (list 'matrix 1 1 (vector 42))))"
                 ";; 2x2 matrix
(assert-equal '#(3.0 2.0) (matrix-diagonal (list 'matrix 2 2 (vector 3.0 1.0 0.0 2.0))))"))
    (available-modules (prelude vec matrix matrix-decomp))
    (hints ("Get size: (let ([n (matrix-rows a)]) ...)"
            "Allocate: (let ([result (make-vector n 0)]) ...)"
            "Fill: (do ([i 0 (+ i 1)]) ((= i n) result) (vector-set! result i (matrix-ref a i i)))"))
    (reference-solution "(define (matrix-diagonal a)
  (let* ([n (matrix-rows a)]
         [result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        ((= i n) result)
      (vector-set! result i (matrix-ref a i i)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
