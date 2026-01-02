;;; fabric/stitches/matrix-eigen.ss — Eigenvalue and Eigenvector Computation
;;;
;;; Algorithms for computing eigenvalues and eigenvectors:
;;;   - Power iteration (dominant eigenvalue)
;;;   - QR algorithm (all eigenvalues)
;;;   - Inverse iteration (eigenvector for given eigenvalue)
;;;   - Eigenvalue decomposition
;;;
;;; This is Core code: pure (except where noted), total, assumes reasonable input.
;;;
;;; Dependencies (must be loaded by client in correct order):
;;;   - prelude.ss
;;;   - vec.ss
;;;   - matrix.ss
;;;   - matrix-decomp.ss (for QR decomposition)
;;;
;;; Do NOT load dependencies here to avoid redefinition issues.

;;; ============================================================
;;; Constants
;;; ============================================================

;;; Default tolerance for convergence tests
(define *eigen-tolerance* 1e-10)

;;; Maximum iterations for iterative methods
(define *eigen-max-iterations* 1000)

;;; ============================================================
;;; Power Iteration
;;; ============================================================

;;; power-iteration : Matrix × [Vec] × [Nat] × [Num] → (eigenvalue × eigenvector) | Error
;;;
;;; Find the dominant eigenvalue (largest absolute value) and corresponding
;;; eigenvector using the power method.
;;;
;;; Arguments:
;;;   a       - Square matrix
;;;   v0      - Initial guess vector (default: unit vector)
;;;   max-iter - Maximum iterations (default: *eigen-max-iterations*)
;;;   tol     - Convergence tolerance (default: *eigen-tolerance*)
;;;
;;; Returns: (eigenvalue . eigenvector) or error
;;;
;;; The power method iteratively computes A*v, normalizes, and repeats.
;;; Converges to the eigenvector for the eigenvalue with largest |λ|.
(define (power-iteration a . opts)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            (let* ([v0 (if (and (pair? opts) (vector? (car opts)))
                           (car opts)
                           (vec-unit n 0))]
                   [rest1 (if (and (pair? opts) (vector? (car opts)))
                              (cdr opts)
                              opts)]
                   [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                                 (car rest1)
                                 *eigen-max-iterations*)]
                   [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                              (cdr rest1)
                              rest1)]
                   [tol (if (and (pair? rest2) (number? (car rest2)))
                            (car rest2)
                            *eigen-tolerance*)])
                  (power-iteration-loop a v0 0 max-iter tol 0)))))

;;; power-iteration-loop : Matrix × Vec × Nat × Nat × Num × Num → (Num . Vec) | Error
(define (power-iteration-loop a v iter max-iter tol prev-lambda)
  (if (>= iter max-iter)
      `(error no-convergence ,iter ,prev-lambda)
      (let* ([av (matrix-vec-mul a v)]
             ;; Check for zero result (implies eigenvalue is 0 or no convergence)
             [av-norm (vec-norm av)])
            (if (< av-norm tol)
                ;; Zero eigenvalue or degenerate case
                (cons 0 v)
                (let* ([v-new (vec-scale (/ 1.0 av-norm) av)]
                       ;; Rayleigh quotient for eigenvalue estimate
                       [av-new (matrix-vec-mul a v-new)]
                       [lambda-new (vec-dot v-new av-new)]
                       [change (abs (- lambda-new prev-lambda))])
                      (if (< change tol)
                          (cons lambda-new v-new)
                          (power-iteration-loop a v-new (+ iter 1) max-iter tol lambda-new)))))))

;;; ============================================================
;;; QR Algorithm
;;; ============================================================

;;; qr-algorithm : Matrix × [Nat] × [Num] → Vec | Error
;;;
;;; Compute all eigenvalues using the QR algorithm.
;;;
;;; Arguments:
;;;   a       - Square matrix
;;;   max-iter - Maximum iterations (default: *eigen-max-iterations*)
;;;   tol     - Convergence tolerance (default: *eigen-tolerance*)
;;;
;;; Returns: Vector of eigenvalues (diagonal of converged matrix) or error
;;;
;;; The QR algorithm repeatedly computes A = QR, then A' = RQ.
;;; This preserves eigenvalues and converges to upper triangular (Schur form)
;;; for real eigenvalues, or block upper triangular for complex pairs.
(define (qr-algorithm a . opts)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            (let* ([max-iter (if (and (pair? opts) (integer? (car opts)))
                                 (car opts)
                                 *eigen-max-iterations*)]
                   [rest1 (if (and (pair? opts) (integer? (car opts)))
                              (cdr opts)
                              opts)]
                   [tol (if (and (pair? rest1) (number? (car rest1)))
                            (car rest1)
                            *eigen-tolerance*)])
                  (qr-algorithm-loop a 0 max-iter tol)))))

;;; qr-algorithm-loop : Matrix × Nat × Nat × Num → Vec | Error
(define (qr-algorithm-loop a iter max-iter tol)
  (if (>= iter max-iter)
      ;; Return best estimate even without full convergence
      (matrix-diagonal a)
      (let ([qr-result (matrix-qr a)])
           (if (and (pair? qr-result) (eq? (car qr-result) 'error))
               qr-result
               (let* ([q (car qr-result)]
                      [r (cadr qr-result)]
                      ;; A' = R × Q (note: not Q × R)
                      [a-new (matrix-mul r q)])
                     (if (qr-converged? a-new tol)
                         (matrix-diagonal a-new)
                         (qr-algorithm-loop a-new (+ iter 1) max-iter tol)))))))

;;; qr-converged? : Matrix × Num → Boolean
;;; Check if matrix is sufficiently upper triangular (converged).
(define (qr-converged? a tol)
  (let ([n (matrix-rows a)])
       (let row-loop ([i 1])
            (if (>= i n)
                #t
                (let col-loop ([j 0])
                     (if (>= j i)
                         (row-loop (+ i 1))
                         (if (> (abs (matrix-ref a i j)) tol)
                             #f
                             (col-loop (+ j 1)))))))))

;;; ============================================================
;;; QR Algorithm with Shifts (Wilkinson shift)
;;; ============================================================

;;; qr-algorithm-shifted : Matrix × [Nat] × [Num] → Vec | Error
;;;
;;; QR algorithm with Wilkinson shift for faster convergence.
;;; The shift accelerates convergence especially for clustered eigenvalues.
(define (qr-algorithm-shifted a . opts)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            (if (= n 1)
                ;; 1x1 matrix: eigenvalue is the element itself
                (vector (matrix-ref a 0 0))
                (let* ([max-iter (if (and (pair? opts) (integer? (car opts)))
                                     (car opts)
                                     *eigen-max-iterations*)]
                       [rest1 (if (and (pair? opts) (integer? (car opts)))
                                  (cdr opts)
                                  opts)]
                       [tol (if (and (pair? rest1) (number? (car rest1)))
                                (car rest1)
                                *eigen-tolerance*)])
                      (qr-algorithm-shifted-loop a n 0 max-iter tol))))))

;;; qr-algorithm-shifted-loop : Matrix × Nat × Nat × Nat × Num → Vec | Error
;;; Work on deflating the matrix as eigenvalues converge
(define (qr-algorithm-shifted-loop a active-size iter max-iter tol)
  (cond
   [(>= iter max-iter)
    ;; Return best estimate
    (matrix-diagonal a)]
   [(<= active-size 0)
    ;; All eigenvalues found
    (matrix-diagonal a)]
   [(= active-size 1)
    ;; Single element remaining - it's an eigenvalue
    (matrix-diagonal a)]
   [else
    (let* ([n active-size]
           [i (- n 1)]
           ;; Check if we can deflate (subdiagonal element small)
           [sub (if (>= i 1) (matrix-ref a i (- i 1)) 0)])
          (if (< (abs sub) tol)
              ;; Deflate: eigenvalue found, continue with smaller matrix
              (qr-algorithm-shifted-loop a (- active-size 1) iter max-iter tol)
              ;; Compute Wilkinson shift from bottom 2x2 block
              (let* ([shift (wilkinson-shift a n)]
                     ;; Shift: A - σI
                     [a-shifted (matrix-sub a (matrix-scale shift (identity n)))]
                     [qr-result (matrix-qr a-shifted)])
                    (if (and (pair? qr-result) (eq? (car qr-result) 'error))
                        ;; QR failed - try without shift
                        (qr-algorithm-loop a iter max-iter tol)
                        (let* ([q (car qr-result)]
                               [r (cadr qr-result)]
                               ;; A' = RQ + σI
                               [rq (matrix-mul r q)]
                               [a-new (matrix-add rq (matrix-scale shift (identity n)))])
                              (qr-algorithm-shifted-loop a-new active-size (+ iter 1) max-iter tol))))))]))

;;; wilkinson-shift : Matrix × Nat → Num
;;; Compute Wilkinson shift from bottom 2x2 block for faster convergence.
(define (wilkinson-shift a n)
  (if (< n 2)
      0
      (let* ([i (- n 1)]
             [j (- n 2)]
             [a-jj (matrix-ref a j j)]
             [a-ii (matrix-ref a i i)]
             [a-ji (matrix-ref a i j)]
             [a-ij (matrix-ref a j i)]
             ;; 2x2 block eigenvalue closer to a[n-1,n-1]
             [d (/ (- a-jj a-ii) 2)]
             [sign-d (if (>= d 0) 1 -1)]
             [denom (+ (abs d) (sqrt (+ (* d d) (* a-ji a-ij))))])
            (if (= denom 0)
                a-ii
                (- a-ii (/ (* sign-d a-ji a-ij) denom))))))

;;; ============================================================
;;; Inverse Iteration
;;; ============================================================

;;; inverse-iteration : Matrix × Num × [Vec] × [Nat] × [Num] → Vec | Error
;;;
;;; Find the eigenvector for a given (approximate) eigenvalue using
;;; inverse iteration.
;;;
;;; Arguments:
;;;   a       - Square matrix
;;;   lambda  - Approximate eigenvalue
;;;   v0      - Initial guess vector (default: random-ish)
;;;   max-iter - Maximum iterations (default: 100)
;;;   tol     - Convergence tolerance (default: *eigen-tolerance*)
;;;
;;; Returns: Eigenvector or error
;;;
;;; Inverse iteration solves (A - λI)x = v repeatedly, converging to
;;; the eigenvector for the eigenvalue closest to λ.
(define (inverse-iteration a lambda-approx . opts)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            (let* ([v0 (if (and (pair? opts) (vector? (car opts)))
                           (car opts)
                           ;; Use a vector with varying components to avoid unlucky starts
                           (let ([v (make-vector n 0)])
                                (do ([i 0 (+ i 1)])
                                    ((= i n) (vec-normalize v))
                                    (vector-set! v i (+ 1.0 (/ i (+ n 1.0)))))))]
                   [rest1 (if (and (pair? opts) (vector? (car opts)))
                              (cdr opts)
                              opts)]
                   [max-iter (if (and (pair? rest1) (integer? (car rest1)))
                                 (car rest1)
                                 100)]
                   [rest2 (if (and (pair? rest1) (integer? (car rest1)))
                              (cdr rest1)
                              rest1)]
                   [tol (if (and (pair? rest2) (number? (car rest2)))
                            (car rest2)
                            *eigen-tolerance*)]
                   ;; Shift matrix: A - λI (with small perturbation to avoid singularity)
                   [shifted (matrix-sub a (matrix-scale lambda-approx (identity n)))])
                  (inverse-iteration-loop shifted v0 0 max-iter tol)))))

;;; inverse-iteration-loop : Matrix × Vec × Nat × Nat × Num → Vec | Error
(define (inverse-iteration-loop a-shifted v iter max-iter tol)
  (if (>= iter max-iter)
      `(error no-convergence ,iter)
      ;; Solve (A - λI)x = v using LU decomposition
      (let ([lu-result (matrix-lu a-shifted)])
           (if (and (pair? lu-result) (eq? (car lu-result) 'error))
               ;; Matrix is singular - λ is exact eigenvalue, v is close to eigenvector
               (if (eq? (cadr lu-result) 'singular-matrix)
                   v
                   lu-result)
               (let* ([x (matrix-lu-solve lu-result v)]
                      [x-norm (vec-norm x)])
                     (if (< x-norm tol)
                         v  ;; Degenerate case
                         (let* ([x-new (vec-scale (/ 1.0 x-norm) x)]
                                ;; Check convergence: v and x-new should be parallel
                                [dot (abs (vec-dot v x-new))])
                               (if (> dot (- 1.0 tol))
                                   x-new
                                   (inverse-iteration-loop a-shifted x-new (+ iter 1) max-iter tol)))))))))

;;; ============================================================
;;; Eigenvalue Decomposition
;;; ============================================================

;;; eigen-decomposition : Matrix × [Nat] × [Num] → (eigenvalues . eigenvectors) | Error
;;;
;;; Compute full eigenvalue decomposition: A = V × D × V^(-1)
;;; where D is diagonal with eigenvalues, V has eigenvectors as columns.
;;;
;;; Arguments:
;;;   a       - Square matrix
;;;   max-iter - Maximum iterations for each step
;;;   tol     - Convergence tolerance
;;;
;;; Returns: (eigenvalues-vector . eigenvector-matrix) or error
(define (eigen-decomposition a . opts)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (if (not (= n m))
            `(error not-square ,n ,m)
            (let* ([max-iter (if (and (pair? opts) (integer? (car opts)))
                                 (car opts)
                                 *eigen-max-iterations*)]
                   [rest1 (if (and (pair? opts) (integer? (car opts)))
                              (cdr opts)
                              opts)]
                   [tol (if (and (pair? rest1) (number? (car rest1)))
                            (car rest1)
                            *eigen-tolerance*)]
                   ;; Step 1: Get eigenvalues using shifted QR
                   [eigenvalues (qr-algorithm-shifted a max-iter tol)])
                  (if (and (pair? eigenvalues) (eq? (car eigenvalues) 'error))
                      eigenvalues
                      ;; Step 2: Get eigenvectors for each eigenvalue
                      (let ([eigenvectors (make-matrix n n 0)])
                           (let loop ([i 0])
                                (if (= i n)
                                    (cons eigenvalues eigenvectors)
                                    (let* ([lambda-i (vector-ref eigenvalues i)]
                                           [v-i (inverse-iteration a lambda-i (vec-unit n i) max-iter tol)])
                                          (if (and (pair? v-i) (eq? (car v-i) 'error))
                                              ;; Inverse iteration failed - use fallback
                                              (begin
                                               ;; Store unit vector as fallback
                                               (do ([j 0 (+ j 1)])
                                                   ((= j n))
                                                   (matrix-set! eigenvectors j i (if (= j i) 1 0)))
                                               (loop (+ i 1)))
                                              (begin
                                               ;; Store eigenvector as column i
                                               (do ([j 0 (+ j 1)])
                                                   ((= j n))
                                                   (matrix-set! eigenvectors j i (vector-ref v-i j)))
                                               (loop (+ i 1)))))))))))))

;;; ============================================================
;;; Symmetric Matrix Eigenvalue (more efficient)
;;; ============================================================

;;; symmetric-eigen : Matrix × [Nat] × [Num] → (eigenvalues . eigenvectors) | Error
;;;
;;; Compute eigenvalue decomposition for symmetric matrices.
;;; Uses the fact that symmetric matrices have real eigenvalues and
;;; orthogonal eigenvectors.
;;;
;;; The QR algorithm on symmetric matrices converges to a diagonal matrix,
;;; and the product of Q matrices gives the eigenvector matrix.
(define (symmetric-eigen a . opts)
  (let* ([n (matrix-rows a)]
         [m (matrix-cols a)])
        (cond
         [(not (= n m))
          `(error not-square ,n ,m)]
         [(not (matrix-symmetric? a))
          `(error not-symmetric)]
         [else
          (let* ([max-iter (if (and (pair? opts) (integer? (car opts)))
                               (car opts)
                               *eigen-max-iterations*)]
                 [rest1 (if (and (pair? opts) (integer? (car opts)))
                            (cdr opts)
                            opts)]
                 [tol (if (and (pair? rest1) (number? (car rest1)))
                          (car rest1)
                          *eigen-tolerance*)])
                (symmetric-eigen-loop a (identity n) 0 max-iter tol))])))

;;; symmetric-eigen-loop : Matrix × Matrix × Nat × Nat × Num → (Vec . Matrix) | Error
;;; Accumulate Q matrices while running QR algorithm
(define (symmetric-eigen-loop a q-accum iter max-iter tol)
  (if (>= iter max-iter)
      (cons (matrix-diagonal a) q-accum)
      (if (symmetric-converged? a tol)
          (cons (matrix-diagonal a) q-accum)
          (let ([qr-result (matrix-qr a)])
               (if (and (pair? qr-result) (eq? (car qr-result) 'error))
                   qr-result
                   (let* ([q (car qr-result)]
                          [r (cadr qr-result)]
                          [a-new (matrix-mul r q)]
                          ;; Accumulate: Q_total = Q_1 × Q_2 × ... × Q_k
                          [q-new (matrix-mul q-accum q)])
                         (symmetric-eigen-loop a-new q-new (+ iter 1) max-iter tol)))))))

;;; symmetric-converged? : Matrix × Num → Boolean
;;; Check if symmetric matrix has converged (nearly diagonal)
(define (symmetric-converged? a tol)
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
                             (col-loop (+ j 1)))))))))

;;; ============================================================
;;; Spectral Radius and Condition Number
;;; ============================================================

;;; spectral-radius : Matrix × [Nat] × [Num] → Num | Error
;;;
;;; Compute the spectral radius (largest absolute eigenvalue).
(define (spectral-radius a . opts)
  (let* ([max-iter (if (and (pair? opts) (integer? (car opts)))
                       (car opts)
                       *eigen-max-iterations*)]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [tol (if (and (pair? rest1) (number? (car rest1)))
                  (car rest1)
                  *eigen-tolerance*)]
         [result (power-iteration a (vec-unit (matrix-rows a) 0) max-iter tol)])
        (if (and (pair? result) (eq? (car result) 'error))
            result
            (abs (car result)))))

;;; eigenvalue-condition : Matrix → Num | Error
;;;
;;; Estimate the condition number based on eigenvalues.
;;; For normal matrices: κ = |λ_max| / |λ_min|
(define (eigenvalue-condition a . opts)
  (let* ([max-iter (if (and (pair? opts) (integer? (car opts)))
                       (car opts)
                       *eigen-max-iterations*)]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [tol (if (and (pair? rest1) (number? (car rest1)))
                  (car rest1)
                  *eigen-tolerance*)]
         [eigenvalues (qr-algorithm-shifted a max-iter tol)])
        (if (and (pair? eigenvalues) (eq? (car eigenvalues) 'error))
            eigenvalues
            (let* ([abs-eigenvalues (vec-map abs eigenvalues)]
                   [max-ev (vec-max abs-eigenvalues)]
                   [min-ev (vec-min abs-eigenvalues)])
                  (if (< (abs min-ev) tol)
                      +inf.0  ;; Singular or near-singular
                      (/ (abs max-ev) (abs min-ev)))))))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; eigenvalues : Matrix → Vec | Error
;;; Convenience function to get just eigenvalues
(define (eigenvalues a)
  (qr-algorithm-shifted a))

;;; eigenvectors : Matrix → Matrix | Error
;;; Convenience function to get just eigenvectors
(define (eigenvectors a)
  (let ([result (eigen-decomposition a)])
       (if (and (pair? result) (eq? (car result) 'error))
           result
           (cdr result))))

;;; verify-eigenvalue : Matrix × Num × Vec × [Num] → Boolean
;;; Check if (λ, v) is an eigenpair: ||Av - λv|| < tol
(define (verify-eigenvalue a lambda v . opts)
  (let* ([tol (if (pair? opts) (car opts) *eigen-tolerance*)]
         [av (matrix-vec-mul a v)]
         [lambda-v (vec-scale lambda v)]
         [diff (vec-sub av lambda-v)]
         [residual (vec-norm diff)])
        (< residual tol)))

;;; verify-decomposition : Matrix × Vec × Matrix × [Num] → Boolean
;;; Check if A ≈ V × D × V^(-1) for eigenvalue decomposition
(define (verify-decomposition a eigenvalues eigenvectors . opts)
  (let* ([tol (if (pair? opts) (car opts) (* 100 *eigen-tolerance*))]
         [n (matrix-rows a)])
        ;; Check each eigenpair
        (let loop ([i 0])
             (if (= i n)
                 #t
                 (let* ([lambda-i (vector-ref eigenvalues i)]
                        [v-i (matrix-col eigenvectors i)])
                       (if (verify-eigenvalue a lambda-i v-i tol)
                           (loop (+ i 1))
                           #f))))))
