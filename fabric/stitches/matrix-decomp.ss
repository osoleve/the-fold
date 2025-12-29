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
;;; Helper Functions
;;; ============================================================

;;; matrix-copy : Matrix → Matrix
(define (matrix-copy m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [new-data (make-vector (* rows cols) 0)])
        (do ([i 0 (+ i 1)])
            [(= i (* rows cols)) (list 'matrix rows cols new-data)]
            (vector-set! new-data i (vector-ref data i)))))

;;; matrix-set! : Matrix × Nat × Nat × Num → Void
(define (matrix-set! m i j val)
  (let ([cols (matrix-cols m)]
        [data (matrix-data m)])
       (vector-set! data (+ (* i cols) j) val)))

;;; matrix-column : Matrix × Nat → Vec
(define (matrix-column m j)
  (let* ([rows (matrix-rows m)]
         [result (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            [(= i rows) result]
            (vector-set! result i (matrix-ref m i j)))))

;;; vec-norm : Vec → Num
(define (vec-norm v)
  (sqrt (vec-dot-product v v)))

;;; vec-dot-product : Vec × Vec → Num
(define (vec-dot-product v1 v2)
  (let ([n (vector-length v1)])
       (let loop ([i 0] [sum 0])
            (if (= i n)
                sum
                (loop (+ i 1)
                      (+ sum (* (vector-ref v1 i) (vector-ref v2 i))))))))

;;; ============================================================
;;; LU Decomposition
;;; ============================================================

;;; matrix-lu : Matrix → (Matrix × Matrix × Vec) | Error
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
                                    (if (< max-val 1e-10)
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

;;; ============================================================
;;; QR Decomposition
;;; ============================================================

;;; matrix-qr : Matrix → (Matrix × Matrix) | Error
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
                                (matrix-set! r j j norm)
                                (if (< norm 1e-10)
                                    `(error linearly-dependent ,j)
                                    (begin
                                     ;; Normalize column j
                                     (do ([i 0 (+ i 1)])
                                         [(= i m)]
                                         (matrix-set! q i j (/ (matrix-ref q i j) norm)))
                                     ;; Orthogonalize remaining columns
                                     (do ([i (+ j 1) (+ i 1)])
                                         [(= i n)]
                                         (let* ([col-i (matrix-column q i)]
                                                [col-j-norm (matrix-column q j)]
                                                [proj (vec-dot-product col-j-norm col-i)])
                                               (matrix-set! r j i proj)
                                               (do ([row 0 (+ row 1)])
                                                   [(= row m)]
                                                   (matrix-set! q row i
                                                                (- (matrix-ref q row i)
                                                                   (* proj (matrix-ref q row j)))))))
                                     (col-loop (+ j 1)))))))))))

;;; ============================================================
;;; Cholesky Decomposition
;;; ============================================================

;;; matrix-cholesky : Matrix → Matrix | Error
(define (matrix-cholesky a)
  (let ([n (matrix-rows a)])
       (if (not (= n (matrix-cols a)))
           `(error not-square ,(matrix-rows a) ,(matrix-cols a))
           (let ([l (make-matrix n n 0)])
                (let row-loop ([i 0])
                     (if (= i n)
                         l
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
