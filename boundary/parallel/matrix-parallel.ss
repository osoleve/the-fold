;;; boundary/parallel/matrix-parallel.ss — Parallel Matrix Operations
;;; @module matrix-parallel
;;; @requires prelude vec matrix matrix-blocked matrix-blocked-decomp scheduler

(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-blocked.ss")
(load "lattice/linalg/matrix-blocked-decomp.ss")
(load "boundary/parallel/scheduler.ss")

(doc 'module 'matrix-parallel)
(doc 'description "Parallel dispatch for matrix operations using work-stealing thread pool.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ====
;;; Thresholds
;;; ====

(define *parallel-matmul-threshold* 64)
(define *parallel-transpose-threshold* 128)
(define *parallel-map-threshold* 1024)
(define *parallel-decomp-threshold* 128)

;;; ====
;;; Helpers
;;; ====

;;; Build a list of thunks, one per partition of [from, total) with given chunk size.
(define (partition-thunks from total chunk-size make-thunk)
  (let build ([f from] [acc '()])
    (if (>= f total)
        (reverse acc)
        (let ([t (min total (+ f chunk-size))])
          (build t (cons (make-thunk f t) acc))))))

;;; ====
;;; Parallel Matrix Multiplication
;;; ====

;;; parallel-matrix-mul : Matrix × Matrix × [Nat] → Matrix | Error
(define (parallel-matrix-mul m1 m2 . bs-arg)
  (let ([r1 (matrix-rows m1)] [c1 (matrix-cols m1)]
        [r2 (matrix-rows m2)] [c2 (matrix-cols m2)])
    (if (not (= c1 r2))
        `(error dimension-mismatch (,r1 ,c1) (,r2 ,c2))
        (let ([bs (if (null? bs-arg) *default-block-size* (car bs-arg))])
          (if (< r1 *parallel-matmul-threshold*)
              (matrix-mul-blocked m1 m2 bs)
              (let* ([nw (pool-worker-count (ensure-pool!))]
                     [result (make-vector (* r1 c2) 0)]
                     [rpw (max 1 (quotient r1 nw))]
                     [thunks (partition-thunks 0 r1 rpw
                               (lambda (from to)
                                 (lambda ()
                                   (matrix-mul-blocked-into! m1 m2 result from to bs))))])
                (apply parallel-invoke thunks)
                (list 'matrix r1 c2 result)))))))

;;; ====
;;; Parallel Matrix Transpose
;;; ====

;;; parallel-matrix-transpose : Matrix × [Nat] → Matrix
(define (parallel-matrix-transpose m . bs-arg)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [bs (if (null? bs-arg) *default-block-size* (car bs-arg))])
    (if (< (* rows cols) (* *parallel-transpose-threshold* *parallel-transpose-threshold*))
        (matrix-transpose-blocked m bs)
        (let* ([nw (pool-worker-count (ensure-pool!))]
               [result (make-vector (* rows cols) 0)]
               [rpw (max 1 (quotient rows nw))]
               [thunks (partition-thunks 0 rows rpw
                         (lambda (from to)
                           (lambda ()
                             (matrix-transpose-blocked-into! m result from to bs))))])
          (apply parallel-invoke thunks)
          (list 'matrix cols rows result)))))

;;; ====
;;; Parallel Matrix Map
;;; ====

;;; parallel-matrix-map : (a → b) × Matrix a → Matrix b
(define (parallel-matrix-map f m)
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         [n (* rows cols)])
    (if (< n *parallel-map-threshold*)
        (matrix-map f m)
        (let* ([nw (pool-worker-count (ensure-pool!))]
               [result (make-vector n 0)]
               [cpw (max 1 (quotient n nw))]
               [thunks (partition-thunks 0 n cpw
                         (lambda (from to)
                           (lambda ()
                             (do ([i from (+ i 1)])
                                 ((= i to))
                               (vector-set! result i (f (vector-ref data i)))))))])
          (apply parallel-invoke thunks)
          (list 'matrix rows cols result)))))

;;; ====
;;; Parallel LU Decomposition
;;; ====

;;; parallel-lu : Matrix → (L U P) | Error
;;; Sequential pivot + parallel row elimination per column.
(define (parallel-lu a)
  (let ([n (matrix-rows a)])
    (if (not (= n (matrix-cols a)))
        `(error not-square ,n ,(matrix-cols a))
        (let* ([lu-data (vec-copy (matrix-data a))]
               [p (make-vector n 0)]
               [tolerance 1e-10])
          ;; Initialize permutation
          (do ([i 0 (+ i 1)]) ((= i n)) (vector-set! p i i))
          ;; Column-by-column elimination
          (let col-loop ([k 0])
            (cond
              [(= k n)
               ;; Extract L and U
               (let ([l-data (make-vector (* n n) 0)]
                     [u-data (make-vector (* n n) 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i n))
                   (vector-set! l-data (+ (* i n) i) 1)
                   (do ([j 0 (+ j 1)])
                       ((>= j i))
                     (when (not (= i j))
                       (vector-set! l-data (+ (* i n) j)
                         (vector-ref lu-data (+ (* i n) j)))))
                   (do ([j i (+ j 1)])
                       ((= j n))
                     (vector-set! u-data (+ (* i n) j)
                       (vector-ref lu-data (+ (* i n) j)))))
                 (list (list 'matrix n n l-data)
                       (list 'matrix n n u-data)
                       p))]
              [else
               ;; Find pivot for column k
               (let ([pivot-result (lu-find-pivot lu-data n k tolerance)])
                 (cond
                   [(and (pair? pivot-result) (eq? (car pivot-result) 'error))
                    pivot-result]
                   [else
                    (let ([max-row pivot-result])
                      ;; Swap rows if needed
                      (when (not (= max-row k))
                        (lu-swap-rows! lu-data n k max-row)
                        (let ([temp (vector-ref p k)])
                          (vector-set! p k (vector-ref p max-row))
                          (vector-set! p max-row temp)))
                      ;; Row elimination — parallel if large enough
                      (let ([remaining (- n (+ k 1))])
                        (when (> remaining 0)
                          (if (< remaining *parallel-decomp-threshold*)
                              (lu-column-update lu-data n k (+ k 1) n)
                              (let* ([nw (pool-worker-count (ensure-pool!))]
                                     [rpw (max 1 (quotient remaining nw))]
                                     [thunks (partition-thunks (+ k 1) n rpw
                                               (lambda (from to)
                                                 (lambda ()
                                                   (lu-column-update lu-data n k from to))))])
                                (apply parallel-invoke thunks)))))
                      (col-loop (+ k 1)))]))]))))))

;;; lu-find-pivot : Vector × Nat × Nat × Num → Nat | Error
(define (lu-find-pivot data n k tolerance)
  (let loop ([i (+ k 1)] [max-row k]
             [max-val (abs (vector-ref data (+ (* k n) k)))])
    (if (= i n)
        (if (< max-val tolerance)
            `(error singular-matrix ,k)
            max-row)
        (let ([val (abs (vector-ref data (+ (* i n) k)))])
          (if (> val max-val)
              (loop (+ i 1) i val)
              (loop (+ i 1) max-row max-val))))))

;;; lu-swap-rows! : Vector × Nat × Nat × Nat → Void
(define (lu-swap-rows! data n row1 row2)
  (do ([col 0 (+ col 1)])
      ((= col n))
    (let ([idx1 (+ (* row1 n) col)]
          [idx2 (+ (* row2 n) col)])
      (let ([temp (vector-ref data idx1)])
        (vector-set! data idx1 (vector-ref data idx2))
        (vector-set! data idx2 temp)))))

;;; ====
;;; Parallel QR Decomposition
;;; ====

;;; parallel-qr : Matrix → (Q R) | Error
;;; Sequential column normalize + parallel orthogonalize.
(define (parallel-qr a)
  (let ([m (matrix-rows a)]
        [n (matrix-cols a)])
    (if (< m n)
        `(error underdetermined ,m ,n)
        (let ([q-data (vec-copy (matrix-data a))]
              [r-data (make-vector (* n n) 0)]
              [tolerance 1e-10])
          (let col-loop ([j 0])
            (if (= j n)
                (list (list 'matrix m n q-data)
                      (list 'matrix n n r-data))
                ;; Compute column norm
                (let ([norm (qr-col-norm q-data m n j)])
                  (if (< norm tolerance)
                      ;; Rank deficient — handle sequentially (rare path)
                      (qr-handle-dependent-column q-data r-data m n j tolerance col-loop)
                      ;; Normal path: normalize + orthogonalize
                      (begin
                        (vector-set! r-data (+ (* j n) j) norm)
                        ;; Normalize column j
                        (do ([row 0 (+ row 1)])
                            ((= row m))
                          (let ([idx (+ (* row n) j)])
                            (vector-set! q-data idx (/ (vector-ref q-data idx) norm))))
                        ;; Parallel orthogonalization
                        (qr-parallel-orthogonalize! q-data r-data m n j)
                        (col-loop (+ j 1)))))))))))

;;; qr-col-norm : Vector × Nat × Nat × Nat → Num
(define (qr-col-norm q-data m n j)
  (let loop ([row 0] [sum 0])
    (if (= row m)
        (sqrt sum)
        (let ([v (vector-ref q-data (+ (* row n) j))])
          (loop (+ row 1) (+ sum (* v v)))))))

;;; qr-parallel-orthogonalize! : Vector × Vector × Nat × Nat × Nat → Void
(define (qr-parallel-orthogonalize! q-data r-data m n j)
  (let ([remaining (- n (+ j 1))])
    (when (> remaining 0)
      (if (< remaining *parallel-decomp-threshold*)
          (qr-column-orthogonalize q-data r-data m n j (+ j 1) n)
          (let* ([nw (pool-worker-count (ensure-pool!))]
                 [cpw (max 1 (quotient remaining nw))]
                 [thunks (partition-thunks (+ j 1) n cpw
                           (lambda (from to)
                             (lambda ()
                               (qr-column-orthogonalize q-data r-data m n j from to))))])
            (apply parallel-invoke thunks))))))

;;; qr-handle-dependent-column : handles rank-deficient column (sequential fallback)
(define (qr-handle-dependent-column q-data r-data m n j tolerance col-loop)
  (vector-set! r-data (+ (* j n) j) 0)
  (let find-basis ([k 0])
    (if (= k m)
        `(error rank-deficient-recovery-failed ,j)
        ;; Build e_k and orthogonalize against columns 0..j-1
        (let ([v (make-vector m 0)])
          (vector-set! v k 1.0)
          (let orth-loop ([p 0] [curr v])
            (if (= p j)
                ;; Check norm
                (let ([v-norm (let nn ([row 0] [s 0])
                                (if (= row m)
                                    (sqrt s)
                                    (nn (+ row 1)
                                        (+ s (* (vector-ref curr row)
                                                (vector-ref curr row))))))])
                  (if (> v-norm tolerance)
                      ;; Set column j to normalized basis vector
                      (begin
                        (do ([row 0 (+ row 1)])
                            ((= row m))
                          (vector-set! q-data (+ (* row n) j)
                            (/ (vector-ref curr row) v-norm)))
                        (qr-parallel-orthogonalize! q-data r-data m n j)
                        (col-loop (+ j 1)))
                      (find-basis (+ k 1))))
                ;; Orthogonalize against column p
                (let* ([proj (let dot-loop ([row 0] [s 0])
                               (if (= row m) s
                                   (dot-loop (+ row 1)
                                     (+ s (* (vector-ref q-data (+ (* row n) p))
                                             (vector-ref curr row))))))]
                       [new-v (make-vector m 0)])
                  (do ([row 0 (+ row 1)])
                      ((= row m))
                    (vector-set! new-v row
                      (- (vector-ref curr row)
                         (* proj (vector-ref q-data (+ (* row n) p))))))
                  (orth-loop (+ p 1) new-v))))))))
