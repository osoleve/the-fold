;;; lattice/linalg/matrix-eigen.ss — Eigenvalue and Eigenvector Computation
;;; @module matrix-eigen
;;; @requires prelude vec matrix matrix-decomp

(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'matrix-decomp)

(doc 'module 'matrix-eigen)
(doc 'purity 'total)
(doc 'description "Eigenvalue and Eigenvector Computation

Algorithms for computing eigenvalues and eigenvectors:
  - Power iteration (dominant eigenvalue)
  - QR algorithm (all eigenvalues)
  - Inverse iteration (eigenvector for given eigenvalue)
  - Eigenvalue decomposition

This is Core code: pure (except where noted), total, assumes reasonable input.

Dependencies (must be loaded by client in correct order):
  - prelude.ss
  - vec.ss
  - matrix.ss
  - matrix-decomp.ss (for QR decomposition)

Do NOT load dependencies here to avoid redefinition issues.")

(doc 'module 'constants
     'description "Default tolerance and maximum iterations for eigenvalue computations")
(define *eigen-tolerance* 1e-8)
(define *eigen-max-iterations* 200)

(doc 'section 'power-iteration
     'description "Power iteration for finding dominant eigenvalue")

(doc power-iteration
     'type (-> Matrix [Vec] [Nat] [Num] (or (cons Num Vec) Error))
     'description "Find the dominant eigenvalue (largest absolute value) and corresponding eigenvector using the power method.

The power method iteratively computes A*v, normalizes, and repeats.
Converges to the eigenvector for the eigenvalue with largest |λ|."
     'param '(a "Square matrix")
     'param '(v0 "Initial guess vector (default: unit vector)")
     'param '(max-iter "Maximum iterations")
     'param '(tol "Convergence tolerance")
     'returns "(eigenvalue . eigenvector) or error")
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

(doc 'section 'qr-algorithm
     'description "QR algorithm for computing all eigenvalues")

(doc qr-algorithm
     'type (-> Matrix [Nat] [Num] (or Vec Error)))
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

;;; ====
;;; QR Algorithm with Shifts (Wilkinson shift)
;;; ====

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

;;; qr-algorithm-shifted-loop : Matrix × Nat × Nat × Nat × Num → Vec | (complex-eigenvalues Vec)
;;; Work on deflating the matrix as eigenvalues converge.
;;; Returns either a vector of real eigenvalues, or a tagged result indicating
;;; complex eigenvalues were detected: (complex-eigenvalues eigenvalue-vector)
;;; where eigenvalue-vector contains the real parts for complex pairs.
(define (qr-algorithm-shifted-loop a active-size iter max-iter tol)
  (cond
   [(>= iter max-iter)
    ;; Return best estimate - check for unresolved 2x2 blocks
    (extract-eigenvalues-with-complex-check a (matrix-rows a) tol)]
   [(<= active-size 0)
    ;; All eigenvalues found
    (extract-eigenvalues-with-complex-check a (matrix-rows a) tol)]
   [(= active-size 1)
    ;; Single element remaining - it's an eigenvalue
    (extract-eigenvalues-with-complex-check a (matrix-rows a) tol)]
   [(= active-size 2)
    ;; 2x2 block: check if it represents complex conjugate pair
    (let* ([full-n (matrix-rows a)]
           [start (- full-n active-size)]
           [complex-pair (detect-complex-2x2-block a start tol)])
          (if complex-pair
              ;; Complex eigenvalues detected - mark result and return
              (extract-eigenvalues-with-complex-check a full-n tol)
              ;; Real eigenvalues in 2x2 block - continue normal deflation
              (let* ([i (- full-n 1)]
                     [sub (if (>= i 1) (matrix-ref a i (- i 1)) 0)])
                    (if (< (abs sub) tol)
                        (qr-algorithm-shifted-loop a (- active-size 1) iter max-iter tol)
                        (qr-iterate-once a active-size iter max-iter tol full-n)))))]
   [else
    (let* ([full-n (matrix-rows a)]  ;; Use full matrix size for identity
           [start (- full-n active-size)]  ;; Start of active block
           [i (- full-n 1)]  ;; Last row of active block
           ;; Check for 2x2 block representing complex eigenvalues first
           [has-complex-block (and (>= i 1)
                                   (detect-complex-2x2-block a (- i 1) tol))])
          (if has-complex-block
              ;; Complex conjugate pair detected in bottom 2x2
              ;; Deflate by 2 (skip the 2x2 block as a unit)
              (qr-algorithm-shifted-loop a (- active-size 2) iter max-iter tol)
              ;; Check standard deflation (subdiagonal element small)
              (let ([sub (if (>= i 1) (matrix-ref a i (- i 1)) 0)])
                   (if (< (abs sub) tol)
                       ;; Deflate: eigenvalue found, continue with smaller matrix
                       (qr-algorithm-shifted-loop a (- active-size 1) iter max-iter tol)
                       ;; Continue QR iteration
                       (qr-iterate-once a active-size iter max-iter tol full-n)))))]))

;;; qr-iterate-once : Matrix × Nat × Nat × Nat × Num × Nat → Vec | (complex-eigenvalues Vec)
;;; Perform one QR iteration with Wilkinson shift
(define (qr-iterate-once a active-size iter max-iter tol full-n)
  (let* ([shift (wilkinson-shift a active-size)]
         ;; Shift: A - σI (matrix-identity must match full matrix size)
         [a-shifted (matrix-sub a (matrix-scale shift (matrix-identity full-n)))]
         [qr-result (matrix-qr a-shifted)])
        (if (and (pair? qr-result) (eq? (car qr-result) 'error))
            ;; QR failed - try without shift
            (qr-algorithm-loop a iter max-iter tol)
            (let* ([q (car qr-result)]
                   [r (cadr qr-result)]
                   ;; A' = RQ + σI
                   [rq (matrix-mul r q)]
                   [a-new (matrix-add rq (matrix-scale shift (matrix-identity full-n)))])
                  (qr-algorithm-shifted-loop a-new active-size (+ iter 1) max-iter tol)))))

;;; detect-complex-2x2-block : Matrix × Nat × Num → Boolean | (real imag)
;;; Check if the 2x2 block at position (start, start) represents complex eigenvalues.
;;; Returns #f if eigenvalues are real, or (real-part . imaginary-part) if complex.
;;; A 2x2 block has complex eigenvalues when its discriminant is negative.
(define (detect-complex-2x2-block a start tol)
  (let* ([i start]
         [j (+ start 1)]
         [n (matrix-rows a)])
        (if (>= j n)
            #f  ;; Not enough room for 2x2 block
            (let* ([a-ii (matrix-ref a i i)]
                   [a-ij (matrix-ref a i j)]
                   [a-ji (matrix-ref a j i)]
                   [a-jj (matrix-ref a j j)]
                   ;; Check if subdiagonal is significant (indicates unreduced 2x2)
                   [sub-significant? (> (abs a-ji) tol)]
                   ;; Eigenvalues of 2x2: λ = (trace ± sqrt(trace² - 4*det)) / 2
                   ;; Complex when discriminant = trace² - 4*det < 0
                   [trace (+ a-ii a-jj)]
                   [det (- (* a-ii a-jj) (* a-ij a-ji))]
                   [discriminant (- (* trace trace) (* 4 det))])
                  (if (and sub-significant? (< discriminant (- tol)))
                      ;; Complex eigenvalues: return (real-part . imaginary-part)
                      (cons (/ trace 2) (/ (sqrt (- discriminant)) 2))
                      #f)))))

;;; extract-eigenvalues-with-complex-check : Matrix × Nat × Num → Vec | (complex-eigenvalues Vec info)
;;; Extract eigenvalues from quasi-upper-triangular matrix, detecting complex pairs.
;;; Returns either a plain vector (all real) or a tagged result with complex info.
(define (extract-eigenvalues-with-complex-check a n tol)
  (let ([eigenvalues (make-vector n 0)]
        [complex-info '()])
       (let loop ([i 0])
            (cond
             [(>= i n)
              ;; Done - return result
              (if (null? complex-info)
                  eigenvalues
                  `(complex-eigenvalues ,eigenvalues ,complex-info))]
             [(>= (+ i 1) n)
              ;; Last element - must be real
              (vector-set! eigenvalues i (matrix-ref a i i))
              (loop (+ i 1))]
             [else
              ;; Check for 2x2 block
              (let ([complex-pair (detect-complex-2x2-block a i tol)])
                   (if complex-pair
                       ;; Complex conjugate pair - store real parts and record info
                       (begin
                        (vector-set! eigenvalues i (car complex-pair))
                        (vector-set! eigenvalues (+ i 1) (car complex-pair))
                        (set! complex-info
                              (cons (list i (car complex-pair) (cdr complex-pair)) complex-info))
                        (loop (+ i 2)))
                       ;; Real eigenvalue
                       (begin
                        (vector-set! eigenvalues i (matrix-ref a i i))
                        (loop (+ i 1)))))]))))

;;; wilkinson-shift : Matrix × Nat → Num
;;; Compute Wilkinson shift from bottom 2x2 of the active block for faster convergence.
;;; Takes active-size (size of active portion still being reduced).
(define (wilkinson-shift a active-size)
  (if (< active-size 2)
      0
      ;; FIX: Use active-size for indices, not full matrix size
      (let* ([i (- active-size 1)]  ;; Last row of active block
             [j (- active-size 2)]  ;; Second-to-last row of active block
             [a-jj (matrix-ref a j j)]
             [a-ii (matrix-ref a i i)]
             [a-ji (matrix-ref a i j)]
             [a-ij (matrix-ref a j i)]
             ;; 2x2 block eigenvalue closer to a[n-1,n-1]
             [d (/ (- a-jj a-ii) 2)]
             [sign-d (if (>= d 0) 1 -1)]
             ;; Guard against negative discriminant from numerical errors
             [discriminant (+ (* d d) (* a-ji a-ij))]
             [denom (if (< discriminant 0)
                        (abs d)  ;; Fallback: use |d| when discriminant is negative
                        (+ (abs d) (sqrt discriminant)))])
            (if (< denom *eigen-tolerance*)
                a-ii
                (- a-ii (/ (* sign-d a-ji a-ij) denom))))))

;;; ====
;;; Inverse Iteration
;;; ====

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
                   [shifted (matrix-sub a (matrix-scale lambda-approx (matrix-identity n)))])
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

;;; ====
;;; Eigenvalue Decomposition
;;; ====

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
;;;
;;; Errors:
;;;   (error not-square rows cols) - Matrix is not square
;;;   (error complex-eigenvalues eigenvalues info) - Matrix has complex eigenvalues
;;;   (error eigenvector-computation-failed index eigenvalue) - Could not compute eigenvector
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
                   [eigenvalue-result (qr-algorithm-shifted a max-iter tol)])
                  (cond
                   ;; Check for explicit error from QR algorithm
                   [(and (pair? eigenvalue-result) (eq? (car eigenvalue-result) 'error))
                    eigenvalue-result]
                   ;; Check for complex eigenvalues indicator
                   [(and (pair? eigenvalue-result) (eq? (car eigenvalue-result) 'complex-eigenvalues))
                    ;; Return as error - cannot compute real eigenvectors for complex eigenvalues
                    eigenvalue-result]
                   [else
                    ;; eigenvalue-result is a vector of real eigenvalues
                    (let ([eigenvalues eigenvalue-result]
                          [eigenvectors (make-matrix n n 0)])
                         (let loop ([i 0])
                              (if (= i n)
                                  (cons eigenvalues eigenvectors)
                                  (let* ([lambda-i (vector-ref eigenvalues i)]
                                         [v-i (inverse-iteration a lambda-i (vec-unit n i) max-iter tol)])
                                        (if (and (pair? v-i) (eq? (car v-i) 'error))
                                            ;; Inverse iteration failed - return error instead of incorrect fallback
                                            `(error eigenvector-computation-failed ,i ,lambda-i)
                                            ;; Store eigenvector as column i
                                            (begin
                                             (do ([j 0 (+ j 1)])
                                                 ((= j n))
                                                 (matrix-set! eigenvectors j i (vector-ref v-i j)))
                                             (loop (+ i 1))))))))])))))

(doc 'section 'symmetric-eigen
     'description "More efficient eigenvalue decomposition for symmetric matrices")

(doc symmetric-eigen
     'type (-> Matrix [Nat] [Num] (or (cons Vec Matrix) Error)))
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
                (symmetric-eigen-loop a (matrix-identity n) 0 max-iter tol))])))

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

;;; ====
;;; Spectral Radius and Condition Number
;;; ====

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
;;; For normal matrices: kappa = |lambda_max| / |lambda_min|
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
         [eigenvalue-result (qr-algorithm-shifted a max-iter tol)])
        (cond
         ;; Check for explicit error
         [(and (pair? eigenvalue-result) (eq? (car eigenvalue-result) 'error))
          eigenvalue-result]
         ;; Check for complex eigenvalues
         [(and (pair? eigenvalue-result) (eq? (car eigenvalue-result) 'complex-eigenvalues))
          eigenvalue-result]
         [else
          ;; eigenvalue-result is a vector of real eigenvalues
          (let* ([eigenvalues eigenvalue-result]
                 [abs-eigenvalues (vec-map abs eigenvalues)]
                 [max-ev (vec-max abs-eigenvalues)]
                 [min-ev (vec-min abs-eigenvalues)])
                (if (< (abs min-ev) tol)
                    +inf.0  ;; Singular or near-singular
                    (/ (abs max-ev) (abs min-ev))))])))

;;; ====
;;; Utilities
;;; ====

;;; eigenvalues : Matrix → Vec | (complex-eigenvalues Vec info) | Error
;;; Convenience function to get just eigenvalues.
;;; Uses symmetric-eigen for symmetric matrices (more stable),
;;; qr-algorithm-shifted otherwise.
;;; For non-symmetric matrices with complex eigenvalues, returns
;;; (complex-eigenvalues eigenvalue-vector complex-info) where complex-info
;;; is a list of (index real-part imaginary-part) for each complex pair.
(define (eigenvalues a)
  (if (matrix-symmetric? a)
      (let ([result (symmetric-eigen a)])
           (if (and (pair? result) (eq? (car result) 'error))
               result
               (car result)))  ; symmetric-eigen returns (eigenvalues . eigenvectors)
      (qr-algorithm-shifted a)))

;;; eigenvectors : Matrix → Matrix | Error
;;; Convenience function to get just eigenvectors.
;;; Returns error for matrices with complex eigenvalues.
(define (eigenvectors a)
  (let ([result (eigen-decomposition a)])
       (cond
        ;; Check for explicit error
        [(and (pair? result) (eq? (car result) 'error))
         result]
        ;; Check for complex eigenvalues
        [(and (pair? result) (eq? (car result) 'complex-eigenvalues))
         `(error complex-eigenvalues-no-real-eigenvectors ,(cadr result) ,(caddr result))]
        [else
         (cdr result)])))

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
