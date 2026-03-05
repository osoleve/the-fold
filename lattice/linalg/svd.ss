;;; lattice/linalg/svd.ss — Singular Value Decomposition
;;; @module svd
;;; @requires prelude vec matrix matrix-decomp matrix-eigen

(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'matrix-decomp)
(require 'matrix-eigen)

(doc 'module 'svd)
(doc 'description "Singular Value Decomposition and related operations")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Implements: svd (full A = UΣV^T), svd-thin (economy), pseudoinverse (Moore-Penrose), low-rank-approx, matrix-rank")

;;; ====
;;; Constants
;;; ====

;;; Default tolerance for rank determination and zero singular values.
;;; Used as a fallback; most operations now use adaptive relative tolerance
;;; based on the largest singular value (see svd-relative-tolerance).
(define *svd-tolerance* 1e-10)

;;; Machine epsilon for IEEE 754 double precision (2^-52).
(define *svd-machine-epsilon* 2.220446049250313e-16)

;;; Maximum iterations for eigenvalue computation
(define *svd-max-iterations* 1000)

;;; svd-relative-tolerance : Vec × Nat × Nat → Num
;;; Compute adaptive threshold for singular value truncation.
;;;
;;; Formula: tol = sigma_max * sqrt(max(m, n) * eps)
;;;
;;; Rationale: SVD computes singular values as sqrt(eigenvalues of A^T A).
;;; The eigenvalues have error proportional to ||A^T A|| * eps = sigma_max^2 * eps.
;;; Taking the square root, the noise in singular values is proportional to
;;; sigma_max * sqrt(eps). The max(m,n) factor accounts for dimensionality.
;;;
;;; Falls back to *svd-tolerance* if sigma_max is 0 (zero matrix).
(define (svd-relative-tolerance singular-values m n)
  (doc 'export #t)
  (let* ([k (vector-length singular-values)]
         [sigma-max (if (= k 0) 0
                        (let loop ([i 0] [mx 0])
                          (if (= i k) mx
                              (loop (+ i 1) (max mx (abs (vector-ref singular-values i)))))))])
    (if (or (= sigma-max 0) (infinite? sigma-max) (nan? sigma-max))
        *svd-tolerance*
        (max *svd-tolerance*
             (* sigma-max (sqrt (* (max m n) *svd-machine-epsilon*)))))))

;;; ====
;;; Core SVD Algorithm
;;; ====

;;; svd : Matrix × [Nat] × [Num] → (U Σ V) | Error
;;;
;;; Compute the singular value decomposition A = UΣV^T.
;;;
;;; Arguments:
;;;   a       - m×n matrix
;;;   max-iter - Maximum iterations for eigenvalue computation
;;;   tol     - Tolerance for convergence
;;;
;;; Returns: (list U Σ V) where:
;;;   U - m×m orthogonal matrix (left singular vectors)
;;;   Σ - m×n diagonal matrix with singular values
;;;   V - n×n orthogonal matrix (right singular vectors)
;;;
;;; Algorithm:
;;;   1. Compute A^T A (n×n symmetric positive semidefinite)
;;;   2. Find eigenvalues and eigenvectors of A^T A
;;;   3. Singular values σ_i = sqrt(λ_i)
;;;   4. V columns are eigenvectors of A^T A
;;;   5. U columns: u_i = (1/σ_i) × A × v_i for σ_i > 0
;;;
;;; For m < n, we use AA^T instead for efficiency.
(define (svd a . opts)
  (doc 'export #t)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)]
         [max-iter (if (and (pair? opts) (integer? (car opts)))
                       (car opts)
                       *svd-max-iterations*)]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [tol (if (and (pair? rest1) (number? (car rest1)))
                  (car rest1)
                  *svd-tolerance*)])
        ;; Use the smaller dimension for efficiency
        (if (<= n m)
            ;; Standard: compute via A^T A
            (svd-via-ata a m n max-iter tol)
            ;; Transpose approach: compute via AA^T
            (svd-via-aat a m n max-iter tol))))

;;; svd-via-ata : Matrix × Nat × Nat × Nat × Num → (U Σ V) | Error
;;; Compute SVD using A^T A (when n ≤ m)
(define (svd-via-ata a m n max-iter tol)
  (doc 'export #t)
  (let* ([at (matrix-transpose a)]
         [ata (matrix-mul at a)]  ; n×n symmetric
         ;; Get eigendecomposition of A^T A
         [eigen-result (symmetric-eigen ata max-iter tol)])
        (if (and (pair? eigen-result) (eq? (car eigen-result) 'error))
            eigen-result
            (let* ([eigenvalues (car eigen-result)]
                   [v (cdr eigen-result)]
                   ;; Sort eigenvalues (and V columns) in descending order
                   [sorted (sort-singular-data eigenvalues v n)]
                   [sorted-eigenvalues (car sorted)]
                   [sorted-v (cdr sorted)]
                   ;; Compute singular values (sqrt of eigenvalues)
                   ;; First pass: convert with absolute tolerance
                   [raw-sv (eigenvalues->singular-values sorted-eigenvalues tol)]
                   ;; Second pass: apply relative tolerance to clamp noise
                   [singular-values (clamp-noise-singular-values raw-sv m n)]
                   ;; Build Σ matrix (m×n with singular values on diagonal)
                   [sigma (build-sigma-matrix m n singular-values)]
                   ;; Use relative tolerance for rank determination
                   [rel-tol (svd-relative-tolerance singular-values m n)]
                   ;; Compute U columns: u_i = (1/σ_i) × A × v_i
                   [u (compute-u-from-av a sorted-v singular-values m n rel-tol)])
                  (list u sigma sorted-v)))))

;;; svd-via-aat : Matrix × Nat × Nat × Nat × Num → (U Σ V) | Error
;;; Compute SVD using AA^T (when m < n)
(define (svd-via-aat a m n max-iter tol)
  (doc 'export #t)
  (let* ([at (matrix-transpose a)]
         [aat (matrix-mul a at)]  ; m×m symmetric
         ;; Get eigendecomposition of AA^T
         [eigen-result (symmetric-eigen aat max-iter tol)])
        (if (and (pair? eigen-result) (eq? (car eigen-result) 'error))
            eigen-result
            (let* ([eigenvalues (car eigen-result)]
                   [u (cdr eigen-result)]
                   ;; Sort eigenvalues (and U columns) in descending order
                   [sorted (sort-singular-data eigenvalues u m)]
                   [sorted-eigenvalues (car sorted)]
                   [sorted-u (cdr sorted)]
                   ;; Compute singular values with noise clamping
                   [raw-sv (eigenvalues->singular-values sorted-eigenvalues tol)]
                   [singular-values (clamp-noise-singular-values raw-sv m n)]
                   ;; Build Σ matrix
                   [sigma (build-sigma-matrix m n singular-values)]
                   ;; Use relative tolerance for rank determination
                   [rel-tol (svd-relative-tolerance singular-values m n)]
                   ;; Compute V columns: v_i = (1/σ_i) × A^T × u_i
                   [v (compute-v-from-atu at sorted-u singular-values m n rel-tol)])
                  (list sorted-u sigma v)))))

;;; ====
;;; Helper Functions
;;; ====

;;; eigenvalues->singular-values : Vec × Num → Vec
;;; Convert eigenvalues to singular values, clamping noise to 0.
;;; Guards against: negative eigenvalues (numeric noise), infinities
;;; (overflow in A^T A), and NaN.
(define (eigenvalues->singular-values eigenvalues tol)
  (doc 'export #t)
  (let* ([n (vector-length eigenvalues)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (let ([ev (vector-ref eigenvalues i)])
                 ;; Eigenvalues of A^T A should be non-negative.
                 ;; Clamp to 0 if: negative (numeric noise), below tolerance,
                 ;; infinite (overflow in Gram matrix), or NaN.
                 (vector-set! result i
                              (cond
                               [(or (nan? ev) (infinite? ev)) 0]
                               [(> ev tol) (sqrt ev)]
                               [else 0]))))))

;;; clamp-noise-singular-values : Vec × Nat × Nat → Vec
;;; Apply relative tolerance to clamp noise singular values to 0.
;;; After the initial eigenvalue->sqrt conversion, some tiny singular values
;;; may be numerical noise (e.g., sqrt of a spurious eigenvalue in A^T A).
;;; This function computes a relative threshold from the largest singular
;;; value and zeros out anything below it.
(define (clamp-noise-singular-values sv m n)
  (doc 'export #t)
  (let* ([k (vector-length sv)]
         [rel-tol (svd-relative-tolerance sv m n)]
         [result (make-vector k 0)])
    (do ([i 0 (+ i 1)])
        ((= i k) result)
        (let ([s (vector-ref sv i)])
          (vector-set! result i
                       (if (or (nan? s) (infinite? s) (<= s rel-tol))
                           0
                           s))))))

;;; build-sigma-matrix : Nat × Nat × Vec → Matrix
;;; Build the m×n diagonal matrix with singular values.
(define (build-sigma-matrix m n singular-values)
  (doc 'export #t)
  (let ([sigma (make-matrix m n 0)]
        [k (min m n (vector-length singular-values))])
       (do ([i 0 (+ i 1)])
           ((= i k) sigma)
           (matrix-set! sigma i i (vector-ref singular-values i)))))

;;; compute-u-from-av : Matrix × Matrix × Vec × Nat × Nat × Num → Matrix
;;; Compute U matrix where u_i = (1/σ_i) × A × v_i for σ_i > tol.
;;; For zero singular values, use Gram-Schmidt to find orthogonal basis.
(define (compute-u-from-av a v singular-values m n tol)
  (doc 'export #t)
  (let ([u (make-matrix m m 0)]
        [rank (count-nonzero-singular-values singular-values tol)])
       ;; First, compute columns for non-zero singular values
       (do ([i 0 (+ i 1)])
           ((= i (min rank n)))
           (let* ([sigma-i (vector-ref singular-values i)]
                  [v-i (matrix-col v i)]
                  [av-i (matrix-vec-mul a v-i)]
                  [u-i (vec-scale (/ 1.0 sigma-i) av-i)])
                 ;; Store as column i of U
                 (do ([j 0 (+ j 1)])
                     ((= j m))
                     (matrix-set! u j i (vector-ref u-i j)))))
       ;; Complete U to m×m orthogonal matrix if rank < m
       (if (< rank m)
           (complete-orthogonal-basis! u rank m)
           u)
       u))

;;; compute-v-from-atu : Matrix × Matrix × Vec × Nat × Nat × Num → Matrix
;;; Compute V matrix where v_i = (1/σ_i) × A^T × u_i for σ_i > tol.
(define (compute-v-from-atu at u singular-values m n tol)
  (doc 'export #t)
  (let ([v (make-matrix n n 0)]
        [rank (count-nonzero-singular-values singular-values tol)])
       ;; First, compute columns for non-zero singular values
       (do ([i 0 (+ i 1)])
           ((= i (min rank m)))
           (let* ([sigma-i (vector-ref singular-values i)]
                  [u-i (matrix-col u i)]
                  [atu-i (matrix-vec-mul at u-i)]
                  [v-i (vec-scale (/ 1.0 sigma-i) atu-i)])
                 ;; Store as column i of V
                 (do ([j 0 (+ j 1)])
                     ((= j n))
                     (matrix-set! v j i (vector-ref v-i j)))))
       ;; Complete V to n×n orthogonal matrix if rank < n
       (if (< rank n)
           (complete-orthogonal-basis! v rank n)
           v)
       v))

;;; complete-orthogonal-basis! : Matrix × Nat × Nat → Void
;;; Complete an orthogonal matrix by adding orthonormal columns.
;;; M has 'filled' orthonormal columns, need to add columns filled..n-1.
(define (complete-orthogonal-basis! mat filled n)
  (doc 'export #t)
  (let loop ([col filled])
       (when (< col n)
             ;; Find a vector orthogonal to existing columns
             (let ([new-col (find-orthogonal-vector mat col n)])
                  (when new-col
                        (do ([i 0 (+ i 1)])
                            ((= i n))
                            (matrix-set! mat i col (vector-ref new-col i)))))
             (loop (+ col 1)))))

;;; find-orthogonal-vector : Matrix × Nat × Nat → Vec | #f
;;; Find a unit vector orthogonal to columns 0..col-1 of matrix.
(define (find-orthogonal-vector mat col n)
  (doc 'export #t)
  (let try-basis ([k 0])
       (if (= k n)
           #f  ; Failed
           (let* ([e-k (vec-unit n k)]
                  ;; Orthogonalize against existing columns
                  [v (let orth-loop ([c 0] [curr e-k])
                          (if (= c col)
                              curr
                              (let* ([col-c (matrix-col mat c)]
                                     [proj (vec-dot col-c curr)])
                                    (orth-loop (+ c 1)
                                               (vec-sub curr (vec-scale proj col-c))))))]
                  [v-norm (vec-norm v)])
                 (if (> v-norm *svd-tolerance*)
                     (vec-scale (/ 1.0 v-norm) v)
                     (try-basis (+ k 1)))))))

;;; count-nonzero-singular-values : Vec × Num → Nat
(define (count-nonzero-singular-values singular-values tol)
  (doc 'export #t)
  (let ([n (vector-length singular-values)])
       (let loop ([i 0] [count 0])
            (if (= i n)
                count
                (if (> (vector-ref singular-values i) tol)
                    (loop (+ i 1) (+ count 1))
                    (loop (+ i 1) count))))))

;;; sort-singular-data : Vec × Matrix × Nat → (Vec . Matrix)
;;; Sort eigenvalues in descending order, reorder matrix columns accordingly.
(define (sort-singular-data eigenvalues mat n)
  (doc 'export #t)
  (let* ([indices (make-vector n 0)]
         [_ (do ([i 0 (+ i 1)]) ((= i n)) (vector-set! indices i i))]
         ;; Bubble sort indices by eigenvalue (descending)
         [_ (do ([i 0 (+ i 1)])
                ((= i n))
                (do ([j (+ i 1) (+ j 1)])
                    ((= j n))
                    (when (< (vector-ref eigenvalues (vector-ref indices i))
                             (vector-ref eigenvalues (vector-ref indices j)))
                          (let ([temp (vector-ref indices i)])
                               (vector-set! indices i (vector-ref indices j))
                               (vector-set! indices j temp)))))]
         ;; Build sorted results
         [sorted-ev (make-vector n 0)]
         [rows (matrix-rows mat)]
         [sorted-mat (make-matrix rows n 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (let ([idx (vector-ref indices i)])
                 (vector-set! sorted-ev i (vector-ref eigenvalues idx))
                 ;; Copy column idx to column i
                 (do ([r 0 (+ r 1)])
                     ((= r rows))
                     (matrix-set! sorted-mat r i (matrix-ref mat r idx)))))
        (cons sorted-ev sorted-mat)))

;;; ====
;;; Thin/Economy SVD
;;; ====

;;; svd-thin : Matrix × [Nat] × [Num] → (U Σ V) | Error
;;;
;;; Compute the thin SVD for m×n matrix A:
;;;   - If m >= n: U is m×n, Σ is n×n, V is n×n
;;;   - If m < n:  U is m×m, Σ is m×m, V is n×m
;;;
;;; More memory-efficient for rectangular matrices.
(define (svd-thin a . opts)
  (doc 'export #t)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)]
         [max-iter (if (and (pair? opts) (integer? (car opts)))
                       (car opts)
                       *svd-max-iterations*)]
         [rest1 (if (and (pair? opts) (integer? (car opts)))
                    (cdr opts)
                    opts)]
         [tol (if (and (pair? rest1) (number? (car rest1)))
                  (car rest1)
                  *svd-tolerance*)]
         [full-svd (svd a max-iter tol)])
        (if (and (pair? full-svd) (eq? (car full-svd) 'error))
            full-svd
            ;; Extract thin components
            (let* ([u-full (car full-svd)]
                   [sigma-full (cadr full-svd)]
                   [v-full (caddr full-svd)]
                   [k (min m n)])
                  (if (>= m n)
                      ;; m >= n: U is m×n, Σ is n×n, V stays n×n
                      (list (matrix-slice-cols u-full 0 n)
                            (matrix-slice sigma-full 0 n 0 n)
                            v-full)
                      ;; m < n: U stays m×m, Σ is m×m, V is n×m
                      (list u-full
                            (matrix-slice sigma-full 0 m 0 m)
                            (matrix-slice-cols v-full 0 m)))))))

;;; matrix-slice-cols : Matrix × Nat × Nat → Matrix
;;; Extract columns start to end-1.
(define (matrix-slice-cols mat start end)
  (doc 'export #t)
  (let* ([rows (matrix-rows mat)]
         [cols (- end start)]
         [result (make-matrix rows cols 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (do ([j 0 (+ j 1)])
                ((= j cols))
                (matrix-set! result i j (matrix-ref mat i (+ start j)))))))

;;; matrix-slice : Matrix × Nat × Nat × Nat × Nat → Matrix
;;; Extract submatrix [row-start:row-end, col-start:col-end].
(define (matrix-slice mat row-start row-end col-start col-end)
  (doc 'export #t)
  (let* ([rows (- row-end row-start)]
         [cols (- col-end col-start)]
         [result (make-matrix rows cols 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (do ([j 0 (+ j 1)])
                ((= j cols))
                (matrix-set! result i j (matrix-ref mat (+ row-start i) (+ col-start j)))))
        result))

;;; ====
;;; Moore-Penrose Pseudoinverse
;;; ====

;;; pseudoinverse : Matrix × [Num] → Matrix | Error
;;;
;;; Compute the Moore-Penrose pseudoinverse A^+ using SVD.
;;;
;;; For A = UΣV^T, the pseudoinverse is A^+ = VΣ^+U^T
;;; where Σ^+ has 1/σ_i on diagonal for σ_i > tol, else 0.
;;;
;;; Properties:
;;;   - AA^+A = A
;;;   - A^+AA^+ = A^+
;;;   - (AA^+)^T = AA^+
;;;   - (A^+A)^T = A^+A
(define (pseudoinverse a . opts)
  (doc 'export #t)
  (let* ([tol (if (pair? opts) (car opts) *svd-tolerance*)]
         [svd-result (svd a *svd-max-iterations* tol)])
        (if (and (pair? svd-result) (eq? (car svd-result) 'error))
            svd-result
            (let* ([u (car svd-result)]
                   [sigma (cadr svd-result)]
                   [v (caddr svd-result)]
                   [m (matrix-rows a)]
                   [n (matrix-cols a)]
                   ;; Extract singular values for relative tolerance
                   [k (min m n)]
                   [sv (let ([v (make-vector k 0)])
                         (do ([i 0 (+ i 1)]) ((= i k) v)
                           (vector-set! v i (matrix-ref sigma i i))))]
                   ;; Use relative tolerance for pseudoinverse thresholding
                   [rel-tol (svd-relative-tolerance sv m n)]
                   ;; Σ^+ is n×m (transpose of Σ dimensions)
                   [sigma-pinv (build-sigma-pseudoinverse sigma m n rel-tol)]
                   ;; A^+ = V × Σ^+ × U^T
                   [vt (matrix-transpose v)]
                   [ut (matrix-transpose u)]
                   [v-sigma-pinv (matrix-mul v sigma-pinv)])
                  (matrix-mul v-sigma-pinv ut)))))

;;; build-sigma-pseudoinverse : Matrix × Nat × Nat × Num → Matrix
;;; Build n×m matrix with 1/σ_i on diagonal where σ_i > tol.
(define (build-sigma-pseudoinverse sigma m n tol)
  (doc 'export #t)
  (let* ([sigma-pinv (make-matrix n m 0)]
         [k (min m n)])
        (do ([i 0 (+ i 1)])
            ((= i k) sigma-pinv)
            (let ([s (matrix-ref sigma i i)])
                 (when (> (abs s) tol)
                       (matrix-set! sigma-pinv i i (/ 1.0 s)))))))

;;; ====
;;; Low-Rank Approximation
;;; ====

;;; low-rank-approx : Matrix × Nat × [Num] → Matrix | Error
;;;
;;; Compute the best rank-k approximation of A (Eckart-Young theorem).
;;;
;;; A_k = Σ_{i=1}^{k} σ_i × u_i × v_i^T
;;;
;;; This is the optimal approximation in both Frobenius and spectral norms.
(define (low-rank-approx a k . opts)
  (doc 'export #t)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)]
         [max-k (min m n)])
        (cond
         [(< k 0)
          `(error invalid-rank ,k)]
         [(> k max-k)
          `(error rank-exceeds-dimension ,k ,max-k)]
         [else
          (let* ([tol (if (pair? opts) (car opts) *svd-tolerance*)]
                 [svd-result (svd a *svd-max-iterations* tol)])
                (if (and (pair? svd-result) (eq? (car svd-result) 'error))
                    svd-result
                    (let* ([u (car svd-result)]
                           [sigma (cadr svd-result)]
                           [v (caddr svd-result)]
                           ;; Build A_k = U_k × Σ_k × V_k^T
                           [u-k (matrix-slice-cols u 0 k)]
                           [sigma-k (matrix-slice sigma 0 k 0 k)]
                           [v-k (matrix-slice-cols v 0 k)]
                           [v-k-t (matrix-transpose v-k)]
                           [u-sigma (matrix-mul u-k sigma-k)])
                          (matrix-mul u-sigma v-k-t))))])))

;;; ====
;;; Matrix Rank
;;; ====

;;; matrix-rank : Matrix × [Num] → Nat | Error
;;;
;;; Compute the numerical rank of a matrix (number of singular values > tol).
(define (matrix-rank a . opts)
  (doc 'export #t)
  (let* ([tol (if (pair? opts) (car opts) *svd-tolerance*)]
         [svd-result (svd a *svd-max-iterations* tol)])
        (if (and (pair? svd-result) (eq? (car svd-result) 'error))
            svd-result
            (let* ([sigma (cadr svd-result)]
                   [m (matrix-rows sigma)]
                   [n (matrix-cols sigma)]
                   [k (min m n)]
                   ;; Extract singular values for relative tolerance
                   [sv (let ([v (make-vector k 0)])
                         (do ([i 0 (+ i 1)]) ((= i k) v)
                           (vector-set! v i (matrix-ref sigma i i))))]
                   [rel-tol (svd-relative-tolerance sv m n)])
                  (let loop ([i 0] [rank 0])
                       (if (= i k)
                           rank
                           (if (> (abs (matrix-ref sigma i i)) rel-tol)
                               (loop (+ i 1) (+ rank 1))
                               (loop (+ i 1) rank))))))))

;;; ====
;;; Condition Number
;;; ====

;;; matrix-condition-number : Matrix × [Num] → Num | Error
;;;
;;; Compute the 2-norm condition number of a matrix: κ(A) = σ_max / σ_min.
;;;
;;; A large condition number indicates the matrix is nearly singular and
;;; sensitive to perturbation. Returns +inf.0 if σ_min < tolerance (i.e.,
;;; the matrix is rank-deficient).
(define (matrix-condition-number a . opts)
  (doc 'export #t)
  (let* ([tol (if (pair? opts) (car opts) *svd-tolerance*)]
         [sv (singular-values a tol)])
    (if (and (pair? sv) (eq? (car sv) 'error))
        sv
        (let* ([k (vector-length sv)]
               [m (matrix-rows a)]
               [n (matrix-cols a)]
               [sigma-max (vector-ref sv 0)]
               ;; Find smallest non-zero singular value
               [rel-tol (svd-relative-tolerance sv m n)]
               [sigma-min (vector-ref sv (- k 1))])
          (if (<= sigma-min rel-tol)
              +inf.0
              (/ sigma-max sigma-min))))))

;;; ====
;;; Singular Values Only
;;; ====

;;; singular-values : Matrix × [Num] → Vec | Error
;;;
;;; Compute just the singular values (more efficient if U, V not needed).
(define (singular-values a . opts)
  (doc 'export #t)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)]
         [tol (if (pair? opts) (car opts) *svd-tolerance*)]
         ;; Use smaller dimension
         [at (matrix-transpose a)]
         [gram (if (<= n m) (matrix-mul at a) (matrix-mul a at))]
         [eigen-result (symmetric-eigen gram *svd-max-iterations* tol)])
        (if (and (pair? eigen-result) (eq? (car eigen-result) 'error))
            eigen-result
            (let* ([eigenvalues (car eigen-result)]
                   [k (vector-length eigenvalues)]
                   ;; Sort descending
                   [sorted (sort-vector-descending eigenvalues)]
                   ;; Convert to singular values, then apply relative clamping
                   [raw-sv (eigenvalues->singular-values sorted tol)]
                   [singular (clamp-noise-singular-values raw-sv m n)])
                  singular))))

;;; sort-vector-descending : Vec → Vec
(define (sort-vector-descending v)
  (doc 'export #t)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)]
         [_ (do ([i 0 (+ i 1)]) ((= i n)) (vector-set! result i (vector-ref v i)))])
        ;; Bubble sort descending
        (do ([i 0 (+ i 1)])
            ((= i n) result)
            (do ([j (+ i 1) (+ j 1)])
                ((= j n))
                (when (< (vector-ref result i) (vector-ref result j))
                      (let ([temp (vector-ref result i)])
                           (vector-set! result i (vector-ref result j))
                           (vector-set! result j temp)))))))

;;; ====
;;; SVD Verification
;;; ====

;;; verify-svd : Matrix × Matrix × Matrix × Matrix × [Num] → Boolean
;;;
;;; Verify that A ≈ U × Σ × V^T.
(define (verify-svd a u sigma v . opts)
  (doc 'export #t)
  (let* ([tol (if (pair? opts) (car opts) (* 100 *svd-tolerance*))]
         [vt (matrix-transpose v)]
         [u-sigma (matrix-mul u sigma)]
         [reconstructed (matrix-mul u-sigma vt)]
         [diff (matrix-sub a reconstructed)]
         [err (matrix-frobenius-norm diff)])
        (< err tol)))

;;; matrix-frobenius-norm : Matrix → Num
;;; Compute Frobenius norm ||A||_F = sqrt(sum of squares).
(define (matrix-frobenius-norm m)
  (doc 'export #t)
  (let* ([data (matrix-data m)]
         [n (vector-length data)])
        (sqrt (let loop ([i 0] [sum 0])
                   (if (= i n)
                       sum
                       (let ([x (vector-ref data i)])
                            (loop (+ i 1) (+ sum (* x x)))))))))

;;; ====
;;; Matrix Column Extraction (if not defined elsewhere)
;;; ====

;;; matrix-col : Matrix × Nat → Vec
;;; Extract column j as a vector.
(define (matrix-col m j)
  (doc 'export #t)
  (let* ([rows (matrix-rows m)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) result)
            (vector-set! result i (matrix-ref m i j)))))
