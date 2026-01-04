;;; fabric/stitches/matrix-decomp.ss — Matrix Decompositions
;;;
;;; Fundamental matrix decomposition algorithms:
;;;   - LU decomposition with partial pivoting
;;;   - QR decomposition (modified Gram-Schmidt)
;;;   - Cholesky decomposition
;;;
;;; This is Core code: pure (except where noted), total, assumes reasonable input.
;;;
;;; Dependencies (must be loaded by client in correct order):
;;;   - prelude.ss
;;;   - matrix.ss (which loads prelude.ss and vec.ss)
;;;   - matrix-decomp.ss (this file)
;;;
;;; Do NOT load dependencies here to avoid redefinition issues.

;;; ============================================================
;;; Numerical Tolerance
;;; ============================================================

;;; Threshold for numerical stability checks (singularity, linear dependence)
(define *matrix-tolerance* 1e-10)

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; Note: vec-norm and vec-dot are defined in vec.ss
;;; This file assumes vec.ss is loaded (via matrix.ss dependency)

;;; matrix-copy : Matrix -> Matrix
(define (matrix-copy m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [new-data (make-vector (* rows cols) 0)])
        (do ([i 0 (+ i 1)])
            [(= i (* rows cols)) (list 'matrix rows cols new-data)]
            (vector-set! new-data i (vector-ref data i)))))

;;; matrix-set! : Matrix x Nat x Nat x Num -> Void
(define (matrix-set! m i j val)
  (let ([cols (matrix-cols m)]
        [data (matrix-data m)])
       (vector-set! data (+ (* i cols) j) val)))

;;; matrix-column : Matrix x Nat -> Vec
(define (matrix-column m j)
  (let* ([rows (matrix-rows m)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            [(= i rows) result]
            (vector-set! result i (matrix-ref m i j)))))


;;; ============================================================
;;; LU Decomposition
;;; ============================================================

;;; matrix-lu : Matrix -> (Matrix x Matrix x Vec) | Error
(define (matrix-lu a)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)])
        (if (not (= m n))
            `(error not-square ,m ,n)
            (let* ([lu (matrix-copy a)]
                   [p (make-vector n 0)])
                  ;; Initialize permutation
                  (do ([i 0 (+ i 1)])
                      [(= i n)]
                      (vector-set! p i i))
                  ;; Gaussian elimination with partial pivoting
                  (let main-loop ([k 0])
                       (if (= k n)
                           ;; Split into L and U
                           (let ([l (make-matrix n n 0)]
                                 [u (make-matrix n n 0)])
                                (do ([i 0 (+ i 1)])
                                    [(= i n)]
                                    (matrix-set! l i i 1)
                                    (do ([j 0 (+ j 1)])
                                        [(>= j i)]
                                        (when (not (= i j))
                                              (matrix-set! l i j (matrix-ref lu i j))))
                                    (do ([j i (+ j 1)])
                                        [(= j n)]
                                        (matrix-set! u i j (matrix-ref lu i j))))
                                (list l u p))
                           ;; Find pivot row
                           (let find-pivot ([i (+ k 1)] [max-row k] [max-val (abs (matrix-ref lu k k))])
                                (if (= i n)
                                    ;; Check for singularity and continue
                                    (if (< max-val *matrix-tolerance*)
                                        `(error singular-matrix ,k)
                                        (begin
                                         ;; Swap rows if needed
                                         (when (not (= max-row k))
                                               (let ([cols (matrix-cols lu)]
                                                     [data (matrix-data lu)])
                                                    (do ([col 0 (+ col 1)])
                                                        [(= col cols)]
                                                        (let ([idx-k (+ (* k cols) col)]
                                                              [idx-max (+ (* max-row cols) col)])
                                                             (let ([temp (vector-ref data idx-k)])
                                                                  (vector-set! data idx-k (vector-ref data idx-max))
                                                                  (vector-set! data idx-max temp)))))
                                               (let ([temp (vector-ref p k)])
                                                    (vector-set! p k (vector-ref p max-row))
                                                    (vector-set! p max-row temp)))
                                         ;; Eliminate below pivot
                                         (do ([i (+ k 1) (+ i 1)])
                                             [(= i n)]
                                             (let ([factor (/ (matrix-ref lu i k) (matrix-ref lu k k))])
                                                  (matrix-set! lu i k factor)
                                                  (do ([j (+ k 1) (+ j 1)])
                                                      [(= j n)]
                                                      (matrix-set! lu i j
                                                                   (- (matrix-ref lu i j)
                                                                      (* factor (matrix-ref lu k j)))))))
                                         (main-loop (+ k 1))))
                                    ;; Continue finding pivot
                                    (let ([val (abs (matrix-ref lu i k))])
                                         (if (> val max-val)
                                             (find-pivot (+ i 1) i val)
                                             (find-pivot (+ i 1) max-row max-val)))))))))))

;;; matrix-lu-solve : (L x U x P) x Vec -> Vec
;;; Solve Ax = b given LU decomposition with permutation.
;;; Algorithm: 1) Apply P to b, 2) Forward substitute Ly = Pb, 3) Back substitute Ux = y
(define (matrix-lu-solve lu-result b)
  (let* ([l (car lu-result)]
         [u (cadr lu-result)]
         [p (caddr lu-result)]
         [n (vector-length b)]
         ;; Step 1: Apply permutation to b
         [pb (make-vector n 0)]
         [_ (do ([i 0 (+ i 1)])
                [(= i n)]
                (vector-set! pb i (vector-ref b (vector-ref p i))))]
         ;; Step 2: Forward substitution (Ly = Pb)
         [y (make-vector n 0)]
         [_ (do ([i 0 (+ i 1)])
                [(= i n)]
                (let ([sum (let loop ([j 0] [s 0])
                                (if (= j i)
                                    s
                                    (loop (+ j 1)
                                          (+ s (* (matrix-ref l i j)
                                                  (vector-ref y j))))))])
                     (vector-set! y i (- (vector-ref pb i) sum))))]
         ;; Step 3: Back substitution (Ux = y)
         [x (make-vector n 0)])
        (do ([i (- n 1) (- i 1)])
            [(< i 0) x]
            (let ([sum (let loop ([j (+ i 1)] [s 0])
                            (if (= j n)
                                s
                                (loop (+ j 1)
                                      (+ s (* (matrix-ref u i j)
                                              (vector-ref x j))))))])
                 (vector-set! x i (/ (- (vector-ref y i) sum)
                                     (matrix-ref u i i)))))))

;;; ============================================================
;;; QR Decomposition
;;; ============================================================

;;; qr-find-orthogonal-basis : Matrix x Nat x Nat -> Vec | #f
;;; Find a unit vector orthogonal to columns 0..j-1 of Q.
;;; Tries standard basis vectors e_0, e_1, ... until one works.
(define (qr-find-orthogonal-basis q j m)
  (let find-loop ([k 0])
       (if (= k m)
           #f  ; Failed to find orthogonal vector
           (let* ([e-k (vec-unit m k)]
                  ;; Orthogonalize e_k against columns 0..j-1 of Q
                  [v (let orth-loop ([p 0] [curr e-k])
                          (if (= p j)
                              curr
                              (let* ([q-p (matrix-column q p)]
                                     [proj (vec-dot q-p curr)])
                                    (orth-loop (+ p 1)
                                               (vec-sub curr (vec-scale proj q-p))))))]
                  [v-norm (vec-norm v)])
                 (if (> v-norm *matrix-tolerance*)
                     (vec-scale (/ 1.0 v-norm) v)
                     (find-loop (+ k 1)))))))

;;; qr-orthogonalize-remaining! : Matrix x Matrix x Nat x Nat x Nat -> Void
;;; Orthogonalize columns j+1..n-1 against column j.
(define (qr-orthogonalize-remaining! q r j m n)
  (do ([i (+ j 1) (+ i 1)])
      [(= i n)]
      (let* ([col-i (matrix-column q i)]
             [col-j (matrix-column q j)]
             [proj (vec-dot col-j col-i)])
            (matrix-set! r j i proj)
            (do ([row 0 (+ row 1)])
                [(= row m)]
                (matrix-set! q row i
                             (- (matrix-ref q row i)
                                (* proj (matrix-ref q row j))))))))

;;; matrix-qr : Matrix -> (Matrix x Matrix) | Error
;;;
;;; QR decomposition using Modified Gram-Schmidt algorithm.
;;; Handles singular/rank-deficient matrices gracefully by finding
;;; orthogonal basis vectors when linear dependence is detected.
;;; This allows eigenvalue algorithms to converge to 0 eigenvalues
;;; for singular matrices like graph Laplacians.
(define (matrix-qr a)
  (let* ([m (matrix-rows a)]
         [n (matrix-cols a)])
        (if (< m n)
            `(error underdetermined ,m ,n)
            (let ([q (matrix-copy a)]
                  [r (make-matrix n n 0)])
                 (let col-loop ([j 0])
                      (if (= j n)
                          (list q r)
                          (let* ([col-j (matrix-column q j)]
                                 [norm (vec-norm col-j)])
                                (if (< norm *matrix-tolerance*)
                                    ;; Linear dependence - find orthogonal basis vector
                                    (let ([basis-vec (qr-find-orthogonal-basis q j m)])
                                         (if (not basis-vec)
                                             `(error rank-deficient-recovery-failed ,j)
                                             (begin
                                              (matrix-set! r j j 0)
                                              ;; Set column j to basis vector
                                              (do ([row 0 (+ row 1)])
                                                  [(= row m)]
                                                  (matrix-set! q row j (vector-ref basis-vec row)))
                                              ;; Orthogonalize remaining columns
                                              (qr-orthogonalize-remaining! q r j m n)
                                              (col-loop (+ j 1)))))
                                    ;; Standard case - linearly independent
                                    (begin
                                     (matrix-set! r j j norm)
                                     ;; Normalize column j
                                     (do ([i 0 (+ i 1)])
                                         [(= i m)]
                                         (matrix-set! q i j (/ (matrix-ref q i j) norm)))
                                     ;; Orthogonalize remaining columns
                                     (qr-orthogonalize-remaining! q r j m n)
                                     (col-loop (+ j 1)))))))))))

;;; ============================================================
;;; Cholesky Decomposition
;;; ============================================================

;;; matrix-cholesky : Matrix -> (List Matrix) | Error
;;; Returns (list L) where A = L x L^T for positive-definite A.
(define (matrix-cholesky a)
  (let ([n (matrix-rows a)])
       (if (not (= n (matrix-cols a)))
           `(error not-square ,(matrix-rows a) ,(matrix-cols a))
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
                                                  `(error not-positive-definite ,i ,val)
                                                  (begin
                                                   (matrix-set! l i i (sqrt val))
                                                   (col-loop (+ j 1)))))]
                                        [else
                                         (matrix-set! l i j (/ (- (matrix-ref a i j) sum)
                                                               (matrix-ref l j j)))
                                         (col-loop (+ j 1))]))))))))))
