;;; fabric/stitches/sparse.ss — Sparse Matrix Operations
;;;
;;; Efficient storage and operations for sparse matrices.
;;;
;;; Sparse formats:
;;; - COO (Coordinate): (sparse-coo rows cols row-indices col-indices values)
;;; - CSR (Compressed Sparse Row): (sparse-csr rows cols row-ptrs col-indices values)
;;; - CSC (Compressed Sparse Column): (sparse-csc rows cols col-ptrs row-indices values)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec.ss
;;;   - matrix.ss

;;; ============================================================
;;; COO (Coordinate) Format
;;; ============================================================
;;;
;;; Most flexible format. Good for constructing sparse matrices.
;;; Stores triplets: (row_i, col_j, value)

;;; sparse-coo? : Any → Boolean
(define (sparse-coo? m)
  (and (pair? m)
       (eq? (car m) 'sparse-coo)
       (= (length m) 6)))

;;; make-sparse-coo : Nat × Nat × Vec × Vec × Vec → SparseCOO
;;; Create COO matrix from parallel vectors of (row, col, value).
(define (make-sparse-coo rows cols row-indices col-indices values)
  (list 'sparse-coo rows cols row-indices col-indices values))

;;; sparse-coo-rows : SparseCOO → Nat
(define (sparse-coo-rows m) (list-ref m 1))

;;; sparse-coo-cols : SparseCOO → Nat
(define (sparse-coo-cols m) (list-ref m 2))

;;; sparse-coo-row-indices : SparseCOO → Vec
(define (sparse-coo-row-indices m) (list-ref m 3))

;;; sparse-coo-col-indices : SparseCOO → Vec
(define (sparse-coo-col-indices m) (list-ref m 4))

;;; sparse-coo-values : SparseCOO → Vec
(define (sparse-coo-values m) (list-ref m 5))

;;; sparse-coo-nnz : SparseCOO → Nat
;;; Number of non-zeros.
(define (sparse-coo-nnz m)
  (vector-length (sparse-coo-values m)))

;;; sparse-coo-from-triplets : Nat × Nat × (List (Nat × Nat × Num)) → SparseCOO
;;; Create from list of (row col value) triplets.
(define (sparse-coo-from-triplets rows cols triplets)
  (let* ([n (length triplets)]
         [row-idx (make-vector n 0)]
         [col-idx (make-vector n 0)]
         [vals (make-vector n 0)])
        (do ([i 0 (+ i 1)]
             [ts triplets (cdr ts)])
            ((= i n) (make-sparse-coo rows cols row-idx col-idx vals))
            (let ([t (car ts)])
                 (vector-set! row-idx i (car t))
                 (vector-set! col-idx i (cadr t))
                 (vector-set! vals i (caddr t))))))

;;; sparse-coo-ref : SparseCOO × Nat × Nat → Num
;;; Get element at (i, j). O(nnz) - use CSR/CSC for frequent access.
(define (sparse-coo-ref m i j)
  (let ([row-idx (sparse-coo-row-indices m)]
        [col-idx (sparse-coo-col-indices m)]
        [vals (sparse-coo-values m)]
        [nnz (sparse-coo-nnz m)])
       (let loop ([k 0])
            (cond
             [(= k nnz) 0]
             [(and (= (vector-ref row-idx k) i)
                   (= (vector-ref col-idx k) j))
              (vector-ref vals k)]
             [else (loop (+ k 1))]))))

;;; ============================================================
;;; CSR (Compressed Sparse Row) Format
;;; ============================================================
;;;
;;; Efficient for row slicing and matrix-vector multiplication.
;;; row_ptrs[i] gives index into col_indices/values where row i starts.

;;; sparse-csr? : Any → Boolean
(define (sparse-csr? m)
  (and (pair? m)
       (eq? (car m) 'sparse-csr)
       (= (length m) 6)))

;;; make-sparse-csr : Nat × Nat × Vec × Vec × Vec → SparseCSR
(define (make-sparse-csr rows cols row-ptrs col-indices values)
  (list 'sparse-csr rows cols row-ptrs col-indices values))

;;; sparse-csr-rows : SparseCSR → Nat
(define (sparse-csr-rows m) (list-ref m 1))

;;; sparse-csr-cols : SparseCSR → Nat
(define (sparse-csr-cols m) (list-ref m 2))

;;; sparse-csr-row-ptrs : SparseCSR → Vec
(define (sparse-csr-row-ptrs m) (list-ref m 3))

;;; sparse-csr-col-indices : SparseCSR → Vec
(define (sparse-csr-col-indices m) (list-ref m 4))

;;; sparse-csr-values : SparseCSR → Vec
(define (sparse-csr-values m) (list-ref m 5))

;;; sparse-csr-nnz : SparseCSR → Nat
(define (sparse-csr-nnz m)
  (vector-length (sparse-csr-values m)))

;;; sparse-csr-ref : SparseCSR × Nat × Nat → Num
;;; Get element at (i, j). O(cols_in_row) via binary search potential.
(define (sparse-csr-ref m i j)
  (let* ([row-ptrs (sparse-csr-row-ptrs m)]
         [col-idx (sparse-csr-col-indices m)]
         [vals (sparse-csr-values m)]
         [start (vector-ref row-ptrs i)]
         [end (vector-ref row-ptrs (+ i 1))])
        (let loop ([k start])
             (cond
              [(= k end) 0]
              [(= (vector-ref col-idx k) j) (vector-ref vals k)]
              [else (loop (+ k 1))]))))

;;; sparse-csr-row : SparseCSR × Nat → (List (Nat × Num))
;;; Get row i as list of (col, value) pairs.
(define (sparse-csr-row m i)
  (let* ([row-ptrs (sparse-csr-row-ptrs m)]
         [col-idx (sparse-csr-col-indices m)]
         [vals (sparse-csr-values m)]
         [start (vector-ref row-ptrs i)]
         [end (vector-ref row-ptrs (+ i 1))])
        (do ([k (- end 1) (- k 1)]
             [result '() (cons (list (vector-ref col-idx k)
                                     (vector-ref vals k))
                               result)])
            ((< k start) result))))

;;; ============================================================
;;; CSC (Compressed Sparse Column) Format
;;; ============================================================
;;;
;;; Efficient for column slicing. Transpose of CSR structure.

;;; sparse-csc? : Any → Boolean
(define (sparse-csc? m)
  (and (pair? m)
       (eq? (car m) 'sparse-csc)
       (= (length m) 6)))

;;; make-sparse-csc : Nat × Nat × Vec × Vec × Vec → SparseCSC
(define (make-sparse-csc rows cols col-ptrs row-indices values)
  (list 'sparse-csc rows cols col-ptrs row-indices values))

;;; sparse-csc-rows : SparseCSC → Nat
(define (sparse-csc-rows m) (list-ref m 1))

;;; sparse-csc-cols : SparseCSC → Nat
(define (sparse-csc-cols m) (list-ref m 2))

;;; sparse-csc-col-ptrs : SparseCSC → Vec
(define (sparse-csc-col-ptrs m) (list-ref m 3))

;;; sparse-csc-row-indices : SparseCSC → Vec
(define (sparse-csc-row-indices m) (list-ref m 4))

;;; sparse-csc-values : SparseCSC → Vec
(define (sparse-csc-values m) (list-ref m 5))

;;; sparse-csc-nnz : SparseCSC → Nat
(define (sparse-csc-nnz m)
  (vector-length (sparse-csc-values m)))

;;; sparse-csc-ref : SparseCSC × Nat × Nat → Num
(define (sparse-csc-ref m i j)
  (let* ([col-ptrs (sparse-csc-col-ptrs m)]
         [row-idx (sparse-csc-row-indices m)]
         [vals (sparse-csc-values m)]
         [start (vector-ref col-ptrs j)]
         [end (vector-ref col-ptrs (+ j 1))])
        (let loop ([k start])
             (cond
              [(= k end) 0]
              [(= (vector-ref row-idx k) i) (vector-ref vals k)]
              [else (loop (+ k 1))]))))

;;; sparse-csc-col : SparseCSC × Nat → (List (Nat × Num))
;;; Get column j as list of (row, value) pairs.
(define (sparse-csc-col m j)
  (let* ([col-ptrs (sparse-csc-col-ptrs m)]
         [row-idx (sparse-csc-row-indices m)]
         [vals (sparse-csc-values m)]
         [start (vector-ref col-ptrs j)]
         [end (vector-ref col-ptrs (+ j 1))])
        (do ([k (- end 1) (- k 1)]
             [result '() (cons (list (vector-ref row-idx k)
                                     (vector-ref vals k))
                               result)])
            ((< k start) result))))

;;; ============================================================
;;; Format Conversions
;;; ============================================================

;;; coo->csr : SparseCOO → SparseCSR
;;; Convert COO to CSR format.
(define (coo->csr coo)
  (let* ([rows (sparse-coo-rows coo)]
         [cols (sparse-coo-cols coo)]
         [row-idx (sparse-coo-row-indices coo)]
         [col-idx (sparse-coo-col-indices coo)]
         [vals (sparse-coo-values coo)]
         [nnz (sparse-coo-nnz coo)]
         ;; Count elements per row
         [row-counts (make-vector rows 0)])
        ;; First pass: count entries per row
        (do ([k 0 (+ k 1)])
            ((= k nnz))
            (let ([r (vector-ref row-idx k)])
                 (vector-set! row-counts r (+ 1 (vector-ref row-counts r)))))
        ;; Build row pointers (cumulative sum)
        (let ([row-ptrs (make-vector (+ rows 1) 0)])
             (do ([i 0 (+ i 1)]
                  [cumsum 0 (+ cumsum (vector-ref row-counts i))])
                 ((= i rows) (vector-set! row-ptrs rows cumsum))
                 (vector-set! row-ptrs i cumsum))
             ;; Create output arrays
             (let ([out-cols (make-vector nnz 0)]
                   [out-vals (make-vector nnz 0)]
                   [current-pos (vec-copy row-ptrs)])
                  ;; Second pass: place elements
                  (do ([k 0 (+ k 1)])
                      ((= k nnz))
                      (let* ([r (vector-ref row-idx k)]
                             [pos (vector-ref current-pos r)])
                            (vector-set! out-cols pos (vector-ref col-idx k))
                            (vector-set! out-vals pos (vector-ref vals k))
                            (vector-set! current-pos r (+ pos 1))))
                  (make-sparse-csr rows cols row-ptrs out-cols out-vals)))))

;;; coo->csc : SparseCOO → SparseCSC
;;; Convert COO to CSC format.
(define (coo->csc coo)
  (let* ([rows (sparse-coo-rows coo)]
         [cols (sparse-coo-cols coo)]
         [row-idx (sparse-coo-row-indices coo)]
         [col-idx (sparse-coo-col-indices coo)]
         [vals (sparse-coo-values coo)]
         [nnz (sparse-coo-nnz coo)]
         ;; Count elements per column
         [col-counts (make-vector cols 0)])
        ;; First pass: count entries per column
        (do ([k 0 (+ k 1)])
            ((= k nnz))
            (let ([c (vector-ref col-idx k)])
                 (vector-set! col-counts c (+ 1 (vector-ref col-counts c)))))
        ;; Build column pointers
        (let ([col-ptrs (make-vector (+ cols 1) 0)])
             (do ([j 0 (+ j 1)]
                  [cumsum 0 (+ cumsum (vector-ref col-counts j))])
                 ((= j cols) (vector-set! col-ptrs cols cumsum))
                 (vector-set! col-ptrs j cumsum))
             ;; Create output arrays
             (let ([out-rows (make-vector nnz 0)]
                   [out-vals (make-vector nnz 0)]
                   [current-pos (vec-copy col-ptrs)])
                  ;; Second pass: place elements
                  (do ([k 0 (+ k 1)])
                      ((= k nnz))
                      (let* ([c (vector-ref col-idx k)]
                             [pos (vector-ref current-pos c)])
                            (vector-set! out-rows pos (vector-ref row-idx k))
                            (vector-set! out-vals pos (vector-ref vals k))
                            (vector-set! current-pos c (+ pos 1))))
                  (make-sparse-csc rows cols col-ptrs out-rows out-vals)))))

;;; csr->coo : SparseCSR → SparseCOO
;;; Convert CSR to COO format.
(define (csr->coo csr)
  (let* ([rows (sparse-csr-rows csr)]
         [cols (sparse-csr-cols csr)]
         [row-ptrs (sparse-csr-row-ptrs csr)]
         [col-idx (sparse-csr-col-indices csr)]
         [vals (sparse-csr-values csr)]
         [nnz (sparse-csr-nnz csr)]
         [out-rows (make-vector nnz 0)]
         [out-cols (make-vector nnz 0)]
         [out-vals (make-vector nnz 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows))
            (let ([start (vector-ref row-ptrs i)]
                  [end (vector-ref row-ptrs (+ i 1))])
                 (do ([k start (+ k 1)])
                     ((= k end))
                     (vector-set! out-rows k i)
                     (vector-set! out-cols k (vector-ref col-idx k))
                     (vector-set! out-vals k (vector-ref vals k)))))
        (make-sparse-coo rows cols out-rows out-cols out-vals)))

;;; csc->coo : SparseCSC → SparseCOO
;;; Convert CSC to COO format.
(define (csc->coo csc)
  (let* ([rows (sparse-csc-rows csc)]
         [cols (sparse-csc-cols csc)]
         [col-ptrs (sparse-csc-col-ptrs csc)]
         [row-idx (sparse-csc-row-indices csc)]
         [vals (sparse-csc-values csc)]
         [nnz (sparse-csc-nnz csc)]
         [out-rows (make-vector nnz 0)]
         [out-cols (make-vector nnz 0)]
         [out-vals (make-vector nnz 0)])
        (do ([j 0 (+ j 1)])
            ((= j cols))
            (let ([start (vector-ref col-ptrs j)]
                  [end (vector-ref col-ptrs (+ j 1))])
                 (do ([k start (+ k 1)])
                     ((= k end))
                     (vector-set! out-rows k (vector-ref row-idx k))
                     (vector-set! out-cols k j)
                     (vector-set! out-vals k (vector-ref vals k)))))
        (make-sparse-coo rows cols out-rows out-cols out-vals)))

;;; csr->csc : SparseCSR → SparseCSC
(define (csr->csc csr)
  (coo->csc (csr->coo csr)))

;;; csc->csr : SparseCSC → SparseCSR
(define (csc->csr csc)
  (coo->csr (csc->coo csc)))

;;; ============================================================
;;; Dense ↔ Sparse Conversions
;;; ============================================================

;;; dense->sparse-coo : Matrix × [Num] → SparseCOO
;;; Convert dense matrix to COO, dropping values below tolerance.
(define (dense->sparse-coo m . tol-arg)
  (let* ([tol (if (null? tol-arg) 0 (car tol-arg))]
         [rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [data (matrix-data m)]
         ;; First pass: count non-zeros
         [nnz (let loop ([i 0] [count 0])
                   (if (= i (vector-length data))
                       count
                       (loop (+ i 1)
                             (if (> (abs (vector-ref data i)) tol)
                                 (+ count 1)
                                 count))))]
         [row-idx (make-vector nnz 0)]
         [col-idx (make-vector nnz 0)]
         [vals (make-vector nnz 0)])
        ;; Second pass: fill arrays
        (let loop ([i 0] [k 0])
             (if (= i (vector-length data))
                 (make-sparse-coo rows cols row-idx col-idx vals)
                 (let ([v (vector-ref data i)])
                      (if (> (abs v) tol)
                          (begin
                           (vector-set! row-idx k (quotient i cols))
                           (vector-set! col-idx k (remainder i cols))
                           (vector-set! vals k v)
                           (loop (+ i 1) (+ k 1)))
                          (loop (+ i 1) k)))))))

;;; dense->sparse-csr : Matrix × [Num] → SparseCSR
(define (dense->sparse-csr m . tol-arg)
  (let ([tol (if (null? tol-arg) 0 (car tol-arg))])
       (coo->csr (dense->sparse-coo m tol))))

;;; sparse-coo->dense : SparseCOO → Matrix
(define (sparse-coo->dense coo)
  (let* ([rows (sparse-coo-rows coo)]
         [cols (sparse-coo-cols coo)]
         [row-idx (sparse-coo-row-indices coo)]
         [col-idx (sparse-coo-col-indices coo)]
         [vals (sparse-coo-values coo)]
         [nnz (sparse-coo-nnz coo)]
         [data (make-vector (* rows cols) 0)])
        (do ([k 0 (+ k 1)])
            ((= k nnz) (list 'matrix rows cols data))
            (let ([i (vector-ref row-idx k)]
                  [j (vector-ref col-idx k)]
                  [v (vector-ref vals k)])
                 (vector-set! data (+ (* i cols) j) v)))))

;;; sparse-csr->dense : SparseCSR → Matrix
(define (sparse-csr->dense csr)
  (let* ([rows (sparse-csr-rows csr)]
         [cols (sparse-csr-cols csr)]
         [row-ptrs (sparse-csr-row-ptrs csr)]
         [col-idx (sparse-csr-col-indices csr)]
         [vals (sparse-csr-values csr)]
         [data (make-vector (* rows cols) 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) (list 'matrix rows cols data))
            (let ([start (vector-ref row-ptrs i)]
                  [end (vector-ref row-ptrs (+ i 1))])
                 (do ([k start (+ k 1)])
                     ((= k end))
                     (let ([j (vector-ref col-idx k)]
                           [v (vector-ref vals k)])
                          (vector-set! data (+ (* i cols) j) v)))))))

;;; sparse-csc->dense : SparseCSC → Matrix
(define (sparse-csc->dense csc)
  (sparse-csr->dense (csc->csr csc)))

;;; ============================================================
;;; Sparse Matrix-Vector Multiplication
;;; ============================================================

;;; sparse-csr-vec-mul : SparseCSR × Vec → Vec | Error
;;; y = A * x where A is sparse CSR. O(nnz).
(define (sparse-csr-vec-mul m v)
  (let* ([rows (sparse-csr-rows m)]
         [cols (sparse-csr-cols m)]
         [n (vector-length v)])
        (if (not (= cols n))
            `(error dimension-mismatch ,cols ,n)
            (let ([row-ptrs (sparse-csr-row-ptrs m)]
                  [col-idx (sparse-csr-col-indices m)]
                  [vals (sparse-csr-values m)]
                  [result (make-vector rows 0)])
                 (do ([i 0 (+ i 1)])
                     ((= i rows) result)
                     (let ([start (vector-ref row-ptrs i)]
                           [end (vector-ref row-ptrs (+ i 1))])
                          (do ([k start (+ k 1)]
                               [sum 0 (+ sum (* (vector-ref vals k)
                                                (vector-ref v (vector-ref col-idx k))))])
                              ((= k end)
                               (vector-set! result i sum)))))))))

;;; sparse-csc-vec-mul : SparseCSC × Vec → Vec | Error
;;; y = A * x where A is sparse CSC. O(nnz).
(define (sparse-csc-vec-mul m v)
  (let* ([rows (sparse-csc-rows m)]
         [cols (sparse-csc-cols m)]
         [n (vector-length v)])
        (if (not (= cols n))
            `(error dimension-mismatch ,cols ,n)
            (let ([col-ptrs (sparse-csc-col-ptrs m)]
                  [row-idx (sparse-csc-row-indices m)]
                  [vals (sparse-csc-values m)]
                  [result (make-vector rows 0)])
                 ;; For each column j, add x[j] * column_j to result
                 (do ([j 0 (+ j 1)])
                     ((= j cols) result)
                     (let ([start (vector-ref col-ptrs j)]
                           [end (vector-ref col-ptrs (+ j 1))]
                           [xj (vector-ref v j)])
                          (do ([k start (+ k 1)])
                              ((= k end))
                              (let ([i (vector-ref row-idx k)])
                                   (vector-set! result i
                                                (+ (vector-ref result i)
                                                   (* xj (vector-ref vals k))))))))))))

;;; sparse-coo-vec-mul : SparseCOO × Vec → Vec | Error
;;; y = A * x where A is sparse COO. O(nnz).
(define (sparse-coo-vec-mul m v)
  (let* ([rows (sparse-coo-rows m)]
         [cols (sparse-coo-cols m)]
         [n (vector-length v)])
        (if (not (= cols n))
            `(error dimension-mismatch ,cols ,n)
            (let ([row-idx (sparse-coo-row-indices m)]
                  [col-idx (sparse-coo-col-indices m)]
                  [vals (sparse-coo-values m)]
                  [nnz (sparse-coo-nnz m)]
                  [result (make-vector rows 0)])
                 (do ([k 0 (+ k 1)])
                     ((= k nnz) result)
                     (let ([i (vector-ref row-idx k)]
                           [j (vector-ref col-idx k)]
                           [v-val (vector-ref vals k)])
                          (vector-set! result i
                                       (+ (vector-ref result i)
                                          (* v-val (vector-ref v j))))))))))

;;; ============================================================
;;; Sparse Matrix Addition
;;; ============================================================

;;; sparse-csr-add : SparseCSR × SparseCSR → SparseCSR | Error
;;; C = A + B for CSR matrices.
(define (sparse-csr-add a b)
  (let ([ra (sparse-csr-rows a)] [ca (sparse-csr-cols a)]
        [rb (sparse-csr-rows b)] [cb (sparse-csr-cols b)])
       (if (not (and (= ra rb) (= ca cb)))
           `(error dimension-mismatch (,ra ,ca) (,rb ,cb))
           ;; Convert to COO, merge, convert back (simpler implementation)
           (let* ([coo-a (csr->coo a)]
                  [coo-b (csr->coo b)]
                  [merged (sparse-coo-add-impl coo-a coo-b)])
                 (coo->csr merged)))))

;;; sparse-coo-add : SparseCOO × SparseCOO → SparseCOO | Error
(define (sparse-coo-add a b)
  (let ([ra (sparse-coo-rows a)] [ca (sparse-coo-cols a)]
        [rb (sparse-coo-rows b)] [cb (sparse-coo-cols b)])
       (if (not (and (= ra rb) (= ca cb)))
           `(error dimension-mismatch (,ra ,ca) (,rb ,cb))
           (sparse-coo-add-impl a b))))

;;; sparse-coo-add-impl : SparseCOO × SparseCOO → SparseCOO
;;; Internal: assumes dimensions match. Uses dense accumulator then extracts non-zeros.
(define (sparse-coo-add-impl a b)
  (let* ([rows (sparse-coo-rows a)]
         [cols (sparse-coo-cols a)]
         ;; Use dense accumulator (efficient for reasonable sizes)
         [acc (make-vector (* rows cols) 0)])
        ;; Add entries from A
        (let ([row-idx (sparse-coo-row-indices a)]
              [col-idx (sparse-coo-col-indices a)]
              [vals (sparse-coo-values a)]
              [nnz-a (sparse-coo-nnz a)])
             (do ([k 0 (+ k 1)])
                 ((= k nnz-a))
                 (let* ([i (vector-ref row-idx k)]
                        [j (vector-ref col-idx k)]
                        [idx (+ (* i cols) j)])
                       (vector-set! acc idx (+ (vector-ref acc idx)
                                               (vector-ref vals k))))))
        ;; Add entries from B
        (let ([row-idx (sparse-coo-row-indices b)]
              [col-idx (sparse-coo-col-indices b)]
              [vals (sparse-coo-values b)]
              [nnz-b (sparse-coo-nnz b)])
             (do ([k 0 (+ k 1)])
                 ((= k nnz-b))
                 (let* ([i (vector-ref row-idx k)]
                        [j (vector-ref col-idx k)]
                        [idx (+ (* i cols) j)])
                       (vector-set! acc idx (+ (vector-ref acc idx)
                                               (vector-ref vals k))))))
        ;; Extract non-zeros from accumulator
        (let* ([size (* rows cols)]
               ;; First pass: count non-zeros
               [nnz (let loop ([k 0] [count 0])
                         (if (= k size)
                             count
                             (loop (+ k 1)
                                   (if (not (= (vector-ref acc k) 0))
                                       (+ count 1)
                                       count))))]
               [out-rows (make-vector nnz 0)]
               [out-cols (make-vector nnz 0)]
               [out-vals (make-vector nnz 0)])
              ;; Second pass: extract values
              (let loop ([k 0] [idx 0])
                   (if (= k size)
                       (make-sparse-coo rows cols out-rows out-cols out-vals)
                       (let ([v (vector-ref acc k)])
                            (if (not (= v 0))
                                (begin
                                 (vector-set! out-rows idx (quotient k cols))
                                 (vector-set! out-cols idx (remainder k cols))
                                 (vector-set! out-vals idx v)
                                 (loop (+ k 1) (+ idx 1)))
                                (loop (+ k 1) idx))))))))

;;; ============================================================
;;; Sparse Matrix Transpose
;;; ============================================================

;;; sparse-csr-transpose : SparseCSR → SparseCSR
;;; Transpose A. Result is CSR of A^T (which equals CSC structure of A).
(define (sparse-csr-transpose csr)
  (let* ([rows (sparse-csr-rows csr)]
         [cols (sparse-csr-cols csr)]
         [row-ptrs (sparse-csr-row-ptrs csr)]
         [col-idx (sparse-csr-col-indices csr)]
         [vals (sparse-csr-values csr)]
         [nnz (sparse-csr-nnz csr)]
         ;; Count entries per column (will become row pointers of transpose)
         [col-counts (make-vector cols 0)])
        ;; Count column occurrences
        (do ([k 0 (+ k 1)])
            ((= k nnz))
            (let ([c (vector-ref col-idx k)])
                 (vector-set! col-counts c (+ 1 (vector-ref col-counts c)))))
        ;; Build row pointers for transpose
        (let ([new-row-ptrs (make-vector (+ cols 1) 0)])
             (do ([j 0 (+ j 1)]
                  [cumsum 0 (+ cumsum (vector-ref col-counts j))])
                 ((= j cols) (vector-set! new-row-ptrs cols cumsum))
                 (vector-set! new-row-ptrs j cumsum))
             ;; Fill transpose arrays
             (let ([new-col-idx (make-vector nnz 0)]
                   [new-vals (make-vector nnz 0)]
                   [current-pos (vec-copy new-row-ptrs)])
                  (do ([i 0 (+ i 1)])
                      ((= i rows))
                      (let ([start (vector-ref row-ptrs i)]
                            [end (vector-ref row-ptrs (+ i 1))])
                           (do ([k start (+ k 1)])
                               ((= k end))
                               (let* ([c (vector-ref col-idx k)]
                                      [pos (vector-ref current-pos c)])
                                     (vector-set! new-col-idx pos i)
                                     (vector-set! new-vals pos (vector-ref vals k))
                                     (vector-set! current-pos c (+ pos 1))))))
                  (make-sparse-csr cols rows new-row-ptrs new-col-idx new-vals)))))

;;; sparse-coo-transpose : SparseCOO → SparseCOO
;;; Transpose: swap row and column indices.
(define (sparse-coo-transpose coo)
  (make-sparse-coo (sparse-coo-cols coo)
                   (sparse-coo-rows coo)
                   (vec-copy (sparse-coo-col-indices coo))
                   (vec-copy (sparse-coo-row-indices coo))
                   (vec-copy (sparse-coo-values coo))))

;;; sparse-csc-transpose : SparseCSC → SparseCSC
;;; Transpose CSC matrix.
(define (sparse-csc-transpose csc)
  ;; CSC transpose is essentially reinterpreting CSC as CSR of transpose
  (make-sparse-csc (sparse-csc-cols csc)
                   (sparse-csc-rows csc)
                   (vec-copy (sparse-csc-col-ptrs csc))
                   (vec-copy (sparse-csc-row-indices csc))
                   (vec-copy (sparse-csc-values csc))))

;;; ============================================================
;;; Sparse Matrix-Matrix Multiplication
;;; ============================================================

;;; sparse-csr-mul : SparseCSR × SparseCSR → SparseCSR | Error
;;; C = A * B where A is m×k and B is k×n.
(define (sparse-csr-mul a b)
  (let ([ma (sparse-csr-rows a)] [ka (sparse-csr-cols a)]
        [kb (sparse-csr-rows b)] [nb (sparse-csr-cols b)])
       (if (not (= ka kb))
           `(error dimension-mismatch (,ma ,ka) (,kb ,nb))
           (let* ([a-row-ptrs (sparse-csr-row-ptrs a)]
                  [a-col-idx (sparse-csr-col-indices a)]
                  [a-vals (sparse-csr-values a)]
                  [b-row-ptrs (sparse-csr-row-ptrs b)]
                  [b-col-idx (sparse-csr-col-indices b)]
                  [b-vals (sparse-csr-values b)]
                  ;; Use dense accumulator for result
                  [acc (make-vector (* ma nb) 0)])
                 ;; For each row i in A
                 (do ([i 0 (+ i 1)])
                     ((= i ma))
                     (let ([a-start (vector-ref a-row-ptrs i)]
                           [a-end (vector-ref a-row-ptrs (+ i 1))])
                          ;; For each non-zero A[i,k]
                          (do ([ak a-start (+ ak 1)])
                              ((= ak a-end))
                              (let ([k (vector-ref a-col-idx ak)]
                                    [a-ik (vector-ref a-vals ak)])
                                   ;; For each non-zero B[k,j]
                                   (let ([b-start (vector-ref b-row-ptrs k)]
                                         [b-end (vector-ref b-row-ptrs (+ k 1))])
                                        (do ([bk b-start (+ bk 1)])
                                            ((= bk b-end))
                                            (let* ([j (vector-ref b-col-idx bk)]
                                                   [b-kj (vector-ref b-vals bk)]
                                                   [idx (+ (* i nb) j)])
                                                  (vector-set! acc idx
                                                               (+ (vector-ref acc idx)
                                                                  (* a-ik b-kj))))))))))
                 ;; Extract non-zeros from accumulator
                 (let* ([size (* ma nb)]
                        [nnz (let loop ([k 0] [count 0])
                                  (if (= k size)
                                      count
                                      (loop (+ k 1)
                                            (if (not (= (vector-ref acc k) 0))
                                                (+ count 1)
                                                count))))]
                        [row-idx (make-vector nnz 0)]
                        [col-idx (make-vector nnz 0)]
                        [vals (make-vector nnz 0)])
                       (let loop ([k 0] [idx 0])
                            (if (= k size)
                                (coo->csr (make-sparse-coo ma nb row-idx col-idx vals))
                                (let ([v (vector-ref acc k)])
                                     (if (not (= v 0))
                                         (begin
                                          (vector-set! row-idx idx (quotient k nb))
                                          (vector-set! col-idx idx (remainder k nb))
                                          (vector-set! vals idx v)
                                          (loop (+ k 1) (+ idx 1)))
                                         (loop (+ k 1) idx))))))))))

;;; ============================================================
;;; Sparse Scalar Operations
;;; ============================================================

;;; sparse-csr-scale : Num × SparseCSR → SparseCSR
;;; Scale all values by constant.
(define (sparse-csr-scale k csr)
  (make-sparse-csr (sparse-csr-rows csr)
                   (sparse-csr-cols csr)
                   (vec-copy (sparse-csr-row-ptrs csr))
                   (vec-copy (sparse-csr-col-indices csr))
                   (vec-map (lambda (x) (* k x)) (sparse-csr-values csr))))

;;; sparse-coo-scale : Num × SparseCOO → SparseCOO
(define (sparse-coo-scale k coo)
  (make-sparse-coo (sparse-coo-rows coo)
                   (sparse-coo-cols coo)
                   (vec-copy (sparse-coo-row-indices coo))
                   (vec-copy (sparse-coo-col-indices coo))
                   (vec-map (lambda (x) (* k x)) (sparse-coo-values coo))))

;;; ============================================================
;;; Special Sparse Matrices
;;; ============================================================

;;; sparse-identity : Nat → SparseCSR
;;; n×n sparse identity matrix.
(define (sparse-identity n)
  (let ([row-ptrs (make-vector (+ n 1) 0)]
        [col-idx (make-vector n 0)]
        [vals (make-vector n 1)])
       (do ([i 0 (+ i 1)])
           ((= i n))
           (vector-set! row-ptrs i i)
           (vector-set! col-idx i i))
       (vector-set! row-ptrs n n)
       (make-sparse-csr n n row-ptrs col-idx vals)))

;;; sparse-diagonal : Vec → SparseCSR
;;; Create sparse diagonal matrix.
(define (sparse-diagonal v)
  (let* ([n (vector-length v)]
         [row-ptrs (make-vector (+ n 1) 0)]
         [col-idx (make-vector n 0)]
         [vals (vec-copy v)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! row-ptrs i i)
            (vector-set! col-idx i i))
        (vector-set! row-ptrs n n)
        (make-sparse-csr n n row-ptrs col-idx vals)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; sparse-shape : Sparse → (Nat × Nat)
;;; Get dimensions of any sparse format.
(define (sparse-shape m)
  (cond
   [(sparse-coo? m) (cons (sparse-coo-rows m) (sparse-coo-cols m))]
   [(sparse-csr? m) (cons (sparse-csr-rows m) (sparse-csr-cols m))]
   [(sparse-csc? m) (cons (sparse-csc-rows m) (sparse-csc-cols m))]
   [else '(error not-sparse)]))

;;; sparse-nnz : Sparse → Nat
;;; Get number of non-zeros for any format.
(define (sparse-nnz m)
  (cond
   [(sparse-coo? m) (sparse-coo-nnz m)]
   [(sparse-csr? m) (sparse-csr-nnz m)]
   [(sparse-csc? m) (sparse-csc-nnz m)]
   [else '(error not-sparse)]))

;;; sparse-density : Sparse → Num
;;; Fraction of non-zero elements.
(define (sparse-density m)
  (let ([shape (sparse-shape m)]
        [nnz (sparse-nnz m)])
       (/ nnz (* (car shape) (cdr shape)))))

;;; sparse-memory-ratio : Sparse → Num
;;; Ratio of sparse storage to dense storage.
;;; < 1 means sparse is more efficient.
(define (sparse-memory-ratio m)
  (let* ([shape (sparse-shape m)]
         [rows (car shape)]
         [cols (cdr shape)]
         [nnz (sparse-nnz m)]
         [dense-size (* rows cols)])
        (cond
         [(sparse-coo? m)
          ;; COO: 3*nnz values
          (/ (* 3 nnz) dense-size)]
         [(sparse-csr? m)
          ;; CSR: 2*nnz + rows+1 values
          (/ (+ (* 2 nnz) rows 1) dense-size)]
         [(sparse-csc? m)
          ;; CSC: 2*nnz + cols+1 values
          (/ (+ (* 2 nnz) cols 1) dense-size)]
         [else 1])))
