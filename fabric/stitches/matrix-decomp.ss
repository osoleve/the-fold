;;; fabric/stitches/matrix-decomp.ss — Matrix Decompositions
;;;
;;; Fundamental matrix decomposition algorithms:
;;;   - LU decomposition with partial pivoting
;;;   - QR decomposition (Gram-Schmidt and Householder)
;;;   - Cholesky decomposition
;;;
;;; These decompositions are essential for:
;;;   - Solving linear systems
;;;   - Computing matrix inverses
;;;   - Least squares problems
;;;   - Eigenvalue computation
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - matrix.ss
;;;   - vec.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/matrix.ss")
(load "fabric/stitches/vec.ss")

;;; ============================================================
;;; LU Decomposition with Partial Pivoting
;;; ============================================================
;;;
;;; Decomposes matrix A into:
;;;   PA = LU
;;; where:
;;;   P is a permutation matrix
;;;   L is lower triangular with 1s on diagonal
;;;   U is upper triangular
;;;
;;; Returns: (L U P) where P is a permutation vector

;;; matrix-lu : Matrix → (Matrix × Matrix × Vec) | Error
;;; LU decomposition with partial pivoting.
(define (matrix-lu a)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)])
        (if (not (= m n))
            `(error not-square ,m ,n)
            (let* ([lu (matrix-copy a)]
                   [p (make-vector n 0)])  ; permutation vector
                  ;; Initialize permutation
                  (do ([i 0 (+ i 1)])
                      [(= i n)]
                      (vector-set! p i i))
                  ;; Gaussian elimination with partial pivoting
                  (do ([k 0 (+ k 1)])
                      [(= k n) (lu-split lu p)]
                      ;; Find pivot
                      (let* ([pivot-row (find-pivot-row lu k n)]
                             [pivot-val (abs (matrix-ref lu pivot-row k))])
                            ;; Check for singularity
                            (if (< pivot-val 1e-10)
                                `(error singular-matrix ,k)
                                (begin
                                 ;; Swap rows if needed
                                 (when (not (= pivot-row k))
                                       (swap-rows! lu k pivot-row)
                                       (vec-swap! p k pivot-row))
                                 ;; Eliminate below pivot
                                 (do ([i (+ k 1) (+ i 1)])
                                     [(= i n)]
                                     (let ([factor (/ (matrix-ref lu i k)
                                                      (matrix-ref lu k k))])
                                          ;; Store multiplier in L part
                                          (matrix-set! lu i k factor)
                                          ;; Update U part
                                          (do ([j (+ k 1) (+ j 1)])
                                              [(= j n)]
                                              (matrix-set! lu i j
                                                           (- (matrix-ref lu i j)
                                                              (* factor (matrix-ref lu k j))))))))))))))
  
  ;;; find-pivot-row : Matrix × Nat × Nat → Nat
  ;;; Find row with largest absolute value in column k starting from row k.
  (define (find-pivot-row m k n)
    (let loop ([i (+ k 1)] [max-row k] [max-val (abs (matrix-ref m k k))])
         (if (= i n)
             max-row
             (let ([val (abs (matrix-ref m i k))])
                  (if (> val max-val)
                      (loop (+ i 1) i val)
                      (loop (+ i 1) max-row max-val))))))
  
  ;;; swap-rows! : Matrix × Nat × Nat → Void
  ;;; Swap rows i and j in matrix (mutates!).
  (define (swap-rows! m i j)
    (let ([cols (matrix-cols m)]
          [data (matrix-data m)])
         (do ([k 0 (+ k 1)])
             [(= k cols)]
             (let* ([idx-i (+ (* i cols) k)]
                    [idx-j (+ (* j cols) k)]
                    [temp (vector-ref data idx-i)])
                   (vector-set! data idx-i (vector-ref data idx-j))
                   (vector-set! data idx-j temp)))))
  
  ;;; vec-swap! : Vec × Nat × Nat → Void
  ;;; Swap elements i and j in vector (mutates!).
  (define (vec-swap! v i j)
    (let ([temp (vector-ref v i)])
         (vector-set! v i (vector-ref v j))
         (vector-set! v j temp)))
  
  ;;; lu-split : Matrix × Vec → (Matrix × Matrix × Vec)
  ;;; Split LU matrix into separate L and U matrices.
  (define (lu-split lu p)
    (let* ([n (matrix-rows lu)]
           [l (make-matrix n n 0)]
           [u (make-matrix n n 0)])
          ;; Extract L (lower) with 1s on diagonal
          (do ([i 0 (+ i 1)])
              [(= i n)]
              (matrix-set! l i i 1)  ; Diagonal is 1
              (do ([j 0 (+ j 1)])
                  [(>= j i)]
                  (when (not (= i j))
                        (matrix-set! l i j (matrix-ref lu i j)))))
          ;; Extract U (upper)
          (do ([i 0 (+ i 1)])
              [(= i n)]
              (do ([j i (+ j 1)])
                  [(= j n)]
                  (matrix-set! u i j (matrix-ref lu i j))))
          (list l u p)))
  
  ;;; ============================================================
  ;;; QR Decomposition (Gram-Schmidt)
  ;;; ============================================================
  ;;;
  ;;; Decomposes matrix A into:
  ;;;   A = QR
  ;;; where:
  ;;;   Q is orthogonal (Q^T Q = I)
  ;;;   R is upper triangular
  ;;;
  ;;; Uses modified Gram-Schmidt for better numerical stability.
  
  ;;; matrix-qr : Matrix → (Matrix × Matrix) | Error
  ;;; QR decomposition using modified Gram-Schmidt.
  (define (matrix-qr a)
    (let* ([m (matrix-rows a)]
           [n (matrix-cols a)])
          (if (< m n)
              `(error underdetermined ,m ,n)
              (let* ([q (matrix-copy a)]  ; Will be orthogonalized in-place
                     [r (make-matrix n n 0)])
                    ;; Modified Gram-Schmidt
                    (do ([j 0 (+ j 1)])
                        [(= j n) (list q r)]
                        (let ([col-j (matrix-column q j)])
                             ;; Compute R[j,j] = ||q_j||
                             (let ([norm (vec-norm col-j)])
                                  (matrix-set! r j j norm)
                                  ;; Normalize column j: q_j = q_j / ||q_j||
                                  (if (< norm 1e-10)
                                      `(error linearly-dependent ,j)
                                      (begin
                                       (matrix-scale-column! q j (/ 1.0 norm))
                                       ;; Orthogonalize remaining columns
                                       (do ([i (+ j 1) (+ i 1)])
                                           [(= i n)]
                                           (let* ([col-i (matrix-column q i)]
                                                  [col-j-norm (matrix-column q j)]
                                                  [proj (vec-dot col-j-norm col-i)])
                                                 ;; R[j,i] = q_j^T q_i
                                                 (matrix-set! r j i proj)
                                                 ;; q_i = q_i - (q_j^T q_i) q_j
                                                 (matrix-sub-scaled-column! q i j proj))))))))))))
  
  ;;; matrix-column : Matrix × Nat → Vec
  ;;; Extract column j as a vector.
  (define (matrix-column m j)
    (let* ([rows (matrix-rows m)]
           [result (make-vector rows 0)])
          (do ([i 0 (+ i 1)])
              [(= i rows) result]
              (vector-set! result i (matrix-ref m i j)))))
  
  ;;; vec-norm : Vec → Num
  ;;; Euclidean (L2) norm of vector.
  (define (vec-norm v)
    (sqrt (vec-dot v v)))
  
  ;;; vec-dot : Vec × Vec → Num
  ;;; Dot product of two vectors.
  (define (vec-dot v1 v2)
    (let ([n (vector-length v1)])
         (let loop ([i 0] [sum 0])
              (if (= i n)
                  sum
                  (loop (+ i 1)
                        (+ sum (* (vector-ref v1 i) (vector-ref v2 i))))))))
  
  ;;; matrix-scale-column! : Matrix × Nat × Num → Void
  ;;; Scale column j by factor k (mutates!).
  (define (matrix-scale-column! m j k)
    (let ([rows (matrix-rows m)])
         (do ([i 0 (+ i 1)])
             [(= i rows)]
             (matrix-set! m i j (* k (matrix-ref m i j))))))
  
  ;;; matrix-sub-scaled-column! : Matrix × Nat × Nat × Num → Void
  ;;; Subtract k times column j from column i: col_i = col_i - k*col_j (mutates!).
  (define (matrix-sub-scaled-column! m i j k)
    (let ([rows (matrix-rows m)])
         (do ([row 0 (+ row 1)])
             [(= row rows)]
             (matrix-set! m row i
                          (- (matrix-ref m row i)
                             (* k (matrix-ref m row j)))))))
  
  ;;; ============================================================
  ;;; Cholesky Decomposition
  ;;; ============================================================
  ;;;
  ;;; For symmetric positive-definite matrix A:
  ;;;   A = LL^T
  ;;; where:
  ;;;   L is lower triangular
  ;;;
  ;;; Only works for symmetric positive-definite matrices.
  
  ;;; matrix-cholesky : Matrix → Matrix | Error
  ;;; Cholesky decomposition (returns L).
  (define (matrix-cholesky a)
    (let* ([n (matrix-rows a)])
          (if (not (= n (matrix-cols a)))
              `(error not-square ,(matrix-rows a) ,(matrix-cols a))
              (let ([l (make-matrix n n 0)])
                   ;; Cholesky-Banachiewicz algorithm
                   (do ([i 0 (+ i 1)])
                       [(= i n) l]
                       (do ([j 0 (+ j 1)])
                           [(> j i)]
                           (let ([sum (let loop ([k 0] [s 0])
                                           (if (= k j)
                                               s
                                               (loop (+ k 1)
                                                     (+ s (* (matrix-ref l i k)
                                                             (matrix-ref l j k))))))])
                                (cond
                                 [(= i j)
                                  (let ([val (- (matrix-ref a i i) sum)])
                                       (if (<= val 0)
                                           `(error not-positive-definite ,i ,val)
                                           (matrix-set! l i i (sqrt val))))]
                                 [else
                                  (matrix-set! l i j
                                               (/ (- (matrix-ref a i j) sum)
                                                  (matrix-ref l j j)))]))))))))
  
  ;;; ============================================================
  ;;; Helper Functions
  ;;; ============================================================
  
  ;;; matrix-copy : Matrix → Matrix
  ;;; Create a deep copy of matrix.
  (define (matrix-copy m)
    (let* ([rows (matrix-rows m)]
           [cols (matrix-cols m)]
           [data (matrix-data m)]
           [new-data (make-vector (* rows cols) 0)])
          (do ([i 0 (+ i 1)])
              [(= i (* rows cols)) (list 'matrix rows cols new-data)]
              (vector-set! new-data i (vector-ref data i)))))
  
  ;;; matrix-set! : Matrix × Nat × Nat × Num → Void
  ;;; Set element at (i,j) (mutates!).
  (define (matrix-set! m i j val)
    (let ([cols (matrix-cols m)]
          [data (matrix-data m)])
         (vector-set! data (+ (* i cols) j) val)))
  
  ;;; ============================================================
  ;;; Export Summary
  ;;; ============================================================
  ;;;
  ;;; LU Decomposition:
  ;;;   matrix-lu : Matrix → (Matrix × Matrix × Vec) | Error
  ;;;   Returns (L, U, P) where PA = LU
  ;;;
  ;;; QR Decomposition:
  ;;;   matrix-qr : Matrix → (Matrix × Matrix) | Error
  ;;;   Returns (Q, R) where A = QR
  ;;;
  ;;; Cholesky Decomposition:
  ;;;   matrix-cholesky : Matrix → Matrix | Error
  ;;;   Returns L where A = LL^T
  ;;;
  ;;; Helper Functions:
  ;;;   matrix-copy, matrix-column, vec-norm, vec-dot
  ;;;   matrix-set!, matrix-scale-column!, matrix-sub-scaled-column!
