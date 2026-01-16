;;; fabric/stitches/sparse.ss — Sparse Matrix Operations
;;;
;;; Efficient storage and operations for sparse matrices.
;;;
;;; Sparse formats:
;;; - COO (Coordinate): (sparse-coo rows cols row-indices col-indices values)
;;; - CSR (Compressed Sparse Row): (sparse-csr rows cols row-ptrs col-indices values)
;;; - CSC (Compressed Sparse Column): (sparse-csc rows cols col-ptrs row-indices values)
;;;
;;; Floating-point tolerance:
;;; - *sparse-epsilon* controls threshold for treating values as zero
;;; - Operations drop values |v| < epsilon to prevent fill-in from FP errors
;;; - Use (sparse-drop-below tol m) to clean up after untolerated operations
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec.ss
;;;   - matrix.ss

;;; ====
;;; Floating-Point Tolerance
;;; ====

;;; Default tolerance for sparse operations.
;;; Values with |v| < *sparse-epsilon* are treated as zero.
(define *sparse-epsilon* 1e-15)

;;; ====
;;; COO (Coordinate) Format
;;; ====
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

;;; ====
;;; CSR (Compressed Sparse Row) Format
;;; ====
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

;;; ====
;;; CSC (Compressed Sparse Column) Format
;;; ====
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

;;; ====
;;; Format Conversions
;;; ====

;;; coo->csr : SparseCOO → SparseCSR
;;; Convert COO to CSR format.
;;; Uses sort-based algorithm to avoid O(rows) auxiliary storage.
;;; Complexity: O(nnz log nnz) time, O(nnz) space.
(define (coo->csr coo)
  (let* ([rows (sparse-coo-rows coo)]
         [cols (sparse-coo-cols coo)]
         [row-idx (sparse-coo-row-indices coo)]
         [col-idx (sparse-coo-col-indices coo)]
         [vals (sparse-coo-values coo)]
         [nnz (sparse-coo-nnz coo)])
        (if (= nnz 0)
            ;; Empty matrix: just create empty CSR
            (let ([row-ptrs (make-vector (+ rows 1) 0)])
                 (make-sparse-csr rows cols row-ptrs (make-vector 0 0) (make-vector 0 0)))
            ;; Sort triplets by row index (then by column for stability)
            (let* ([indices (make-vector nnz 0)])
                  ;; Initialize index array
                  (do ([k 0 (+ k 1)])
                      ((= k nnz))
                      (vector-set! indices k k))
                  ;; Sort indices by (row, col) lexicographic order
                  (vector-sort!
                   (lambda (a b)
                           (let ([ra (vector-ref row-idx a)]
                                 [rb (vector-ref row-idx b)])
                                (or (< ra rb)
                                    (and (= ra rb)
                                         (< (vector-ref col-idx a)
                                            (vector-ref col-idx b))))))
                   indices)
                  ;; Build row pointers and sorted output arrays
                  (let ([row-ptrs (make-vector (+ rows 1) 0)]
                        [out-cols (make-vector nnz 0)]
                        [out-vals (make-vector nnz 0)])
                       ;; Fill sorted arrays and build row pointers
                       (do ([k 0 (+ k 1)]
                            [current-row 0 (vector-ref row-idx (vector-ref indices k))])
                           ((= k nnz)
                            ;; Fill remaining row pointers
                            (do ([r (+ current-row 1) (+ r 1)])
                                ((> r rows))
                                (vector-set! row-ptrs r nnz)))
                           (let* ([idx (vector-ref indices k)]
                                  [r (vector-ref row-idx idx)]
                                  [c (vector-ref col-idx idx)]
                                  [v (vector-ref vals idx)])
                                 ;; Update row pointers for any rows we skipped
                                 (when (> r current-row)
                                       (do ([prev-row (+ current-row 1) (+ prev-row 1)])
                                           ((> prev-row r))
                                           (vector-set! row-ptrs prev-row k)))
                                 ;; Store sorted entry
                                 (vector-set! out-cols k c)
                                 (vector-set! out-vals k v)))
                       (make-sparse-csr rows cols row-ptrs out-cols out-vals))))))

;;; coo->csc : SparseCOO → SparseCSC
;;; Convert COO to CSC format.
;;; Uses sort-based algorithm to avoid O(cols) auxiliary storage.
;;; Complexity: O(nnz log nnz) time, O(nnz) space.
(define (coo->csc coo)
  (let* ([rows (sparse-coo-rows coo)]
         [cols (sparse-coo-cols coo)]
         [row-idx (sparse-coo-row-indices coo)]
         [col-idx (sparse-coo-col-indices coo)]
         [vals (sparse-coo-values coo)]
         [nnz (sparse-coo-nnz coo)])
        (if (= nnz 0)
            ;; Empty matrix: just create empty CSC
            (let ([col-ptrs (make-vector (+ cols 1) 0)])
                 (make-sparse-csc rows cols col-ptrs (make-vector 0 0) (make-vector 0 0)))
            ;; Sort triplets by column index (then by row for stability)
            (let* ([indices (make-vector nnz 0)])
                  ;; Initialize index array
                  (do ([k 0 (+ k 1)])
                      ((= k nnz))
                      (vector-set! indices k k))
                  ;; Sort indices by (col, row) lexicographic order
                  (vector-sort!
                   (lambda (a b)
                           (let ([ca (vector-ref col-idx a)]
                                 [cb (vector-ref col-idx b)])
                                (or (< ca cb)
                                    (and (= ca cb)
                                         (< (vector-ref row-idx a)
                                            (vector-ref row-idx b))))))
                   indices)
                  ;; Build column pointers and sorted output arrays
                  (let ([col-ptrs (make-vector (+ cols 1) 0)]
                        [out-rows (make-vector nnz 0)]
                        [out-vals (make-vector nnz 0)])
                       ;; Fill sorted arrays and build column pointers
                       (do ([k 0 (+ k 1)]
                            [current-col 0 (vector-ref col-idx (vector-ref indices k))])
                           ((= k nnz)
                            ;; Fill remaining column pointers
                            (do ([c (+ current-col 1) (+ c 1)])
                                ((> c cols))
                                (vector-set! col-ptrs c nnz)))
                           (let* ([idx (vector-ref indices k)]
                                  [r (vector-ref row-idx idx)]
                                  [c (vector-ref col-idx idx)]
                                  [v (vector-ref vals idx)])
                                 ;; Update column pointers for any columns we skipped
                                 (when (> c current-col)
                                       (do ([prev-col (+ current-col 1) (+ prev-col 1)])
                                           ((> prev-col c))
                                           (vector-set! col-ptrs prev-col k)))
                                 ;; Store sorted entry
                                 (vector-set! out-rows k r)
                                 (vector-set! out-vals k v)))
                       (make-sparse-csc rows cols col-ptrs out-rows out-vals))))))

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

;;; ====
;;; Dense ↔ Sparse Conversions
;;; ====

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

;;; ====
;;; Sparse Matrix-Vector Multiplication
;;; ====

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

;;; ====
;;; Sparse Matrix Addition
;;; ====

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

;;; sparse-coo-add-impl : SparseCOO × SparseCOO × [Num] → SparseCOO
;;; Internal: assumes dimensions match. Uses sparse hash table accumulator.
;;; Drops values with |v| < epsilon to prevent floating-point fill-in.
;;; Complexity: O(nnz_a + nnz_b) space and time.
(define (sparse-coo-add-impl a b . eps-arg)
  (let* ([eps (if (null? eps-arg) *sparse-epsilon* (car eps-arg))]
         [rows (sparse-coo-rows a)]
         [cols (sparse-coo-cols a)]
         ;; Use hash table for sparse accumulation (key = row*cols+col)
         [acc (make-hashtable equal-hash equal?)])
        ;; Add entries from A
        (let ([row-idx (sparse-coo-row-indices a)]
              [col-idx (sparse-coo-col-indices a)]
              [vals (sparse-coo-values a)]
              [nnz-a (sparse-coo-nnz a)])
             (do ([k 0 (+ k 1)])
                 ((= k nnz-a))
                 (let* ([i (vector-ref row-idx k)]
                        [j (vector-ref col-idx k)]
                        [key (cons i j)]
                        [old (hashtable-ref acc key 0)])
                       (hashtable-set! acc key (+ old (vector-ref vals k))))))
        ;; Add entries from B
        (let ([row-idx (sparse-coo-row-indices b)]
              [col-idx (sparse-coo-col-indices b)]
              [vals (sparse-coo-values b)]
              [nnz-b (sparse-coo-nnz b)])
             (do ([k 0 (+ k 1)])
                 ((= k nnz-b))
                 (let* ([i (vector-ref row-idx k)]
                        [j (vector-ref col-idx k)]
                        [key (cons i j)]
                        [old (hashtable-ref acc key 0)])
                       (hashtable-set! acc key (+ old (vector-ref vals k))))))
        ;; Extract non-zeros from hash table (using tolerance)
        (let-values ([(keys values) (hashtable-entries acc)])
                    (let* ([n (vector-length keys)]
                           ;; Filter out near-zeros and collect triplets
                           [triplets (let loop ([k 0] [result '()])
                                          (if (= k n)
                                              result
                                              (let ([v (vector-ref values k)])
                                                   (if (< (abs v) eps)
                                                       (loop (+ k 1) result)
                                                       (let ([key (vector-ref keys k)])
                                                            (loop (+ k 1)
                                                                  (cons (list (car key) (cdr key) v)
                                                                        result)))))))]
                           ;; Sort by (row, col) for consistent output
                           [sorted (list-sort (lambda (a b)
                                                      (or (< (car a) (car b))
                                                          (and (= (car a) (car b))
                                                               (< (cadr a) (cadr b)))))
                                              triplets)]
                           [nnz (length sorted)]
                           [out-rows (make-vector nnz 0)]
                           [out-cols (make-vector nnz 0)]
                           [out-vals (make-vector nnz 0)])
                          ;; Fill output vectors from sorted triplets
                          (do ([k 0 (+ k 1)]
                               [ts sorted (cdr ts)])
                              ((= k nnz) (make-sparse-coo rows cols out-rows out-cols out-vals))
                              (let ([t (car ts)])
                                   (vector-set! out-rows k (car t))
                                   (vector-set! out-cols k (cadr t))
                                   (vector-set! out-vals k (caddr t))))))))

;;; ====
;;; Sparse Matrix Transpose
;;; ====

;;; sparse-csr-transpose : SparseCSR → SparseCSR
;;; Transpose A. Result is CSR of A^T (which equals CSC structure of A).
;;; Uses COO intermediate to avoid O(cols) auxiliary storage.
;;; Complexity: O(nnz log nnz) time, O(nnz) space.
(define (sparse-csr-transpose csr)
  ;; Convert to COO, transpose, convert back to CSR using sort-based algorithm
  (let ([coo (csr->coo csr)])
       (coo->csr (sparse-coo-transpose coo))))

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

;;; ====
;;; Sparse Matrix-Matrix Multiplication
;;; ====

;;; sparse-csr-mul : SparseCSR × SparseCSR × [Num] → SparseCSR | Error
;;; C = A * B where A is m×k and B is k×n.
;;; Uses sparse row accumulator (hash table) for each output row.
;;; Drops values with |v| < epsilon to prevent floating-point fill-in.
;;; Complexity: O(nnz_a * avg_nnz_per_row_b) time, O(max_nnz_per_output_row) space.
(define (sparse-csr-mul a b . eps-arg)
  (let ([eps (if (null? eps-arg) *sparse-epsilon* (car eps-arg))]
        [ma (sparse-csr-rows a)] [ka (sparse-csr-cols a)]
        [kb (sparse-csr-rows b)] [nb (sparse-csr-cols b)])
       (if (not (= ka kb))
           `(error dimension-mismatch (,ma ,ka) (,kb ,nb))
           (let* ([a-row-ptrs (sparse-csr-row-ptrs a)]
                  [a-col-idx (sparse-csr-col-indices a)]
                  [a-vals (sparse-csr-values a)]
                  [b-row-ptrs (sparse-csr-row-ptrs b)]
                  [b-col-idx (sparse-csr-col-indices b)]
                  [b-vals (sparse-csr-values b)]
                  ;; Accumulate all rows, collecting triplets
                  [all-triplets
                   (let row-loop ([i 0] [triplets '()])
                        (if (= i ma)
                            triplets
                            ;; FIX: Use equal-hash/equal? for column indices (fold-n5lm)
                            ;; make-eq-hashtable uses eq? which fails for large integers
                            (let ([row-acc (make-hashtable equal-hash equal?)]
                                  [a-start (vector-ref a-row-ptrs i)]
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
                                                          [old (hashtable-ref row-acc j 0)])
                                                         (hashtable-set! row-acc j (+ old (* a-ik b-kj))))))))
                                 ;; Extract non-zeros from row accumulator (using tolerance)
                                 (let-values ([(cols vals) (hashtable-entries row-acc)])
                                             (let* ([n (vector-length cols)]
                                                    [row-triplets
                                                     (let loop ([k 0] [result '()])
                                                          (if (= k n)
                                                              result
                                                              (let ([v (vector-ref vals k)])
                                                                   (if (< (abs v) eps)
                                                                       (loop (+ k 1) result)
                                                                       (loop (+ k 1)
                                                                             (cons (list i (vector-ref cols k) v)
                                                                                   result))))))])
                                                   (row-loop (+ i 1) (append row-triplets triplets)))))))]
                  ;; Sort triplets by (row, col)
                  [sorted (list-sort (lambda (a b)
                                             (or (< (car a) (car b))
                                                 (and (= (car a) (car b))
                                                      (< (cadr a) (cadr b)))))
                                     all-triplets)]
                  [nnz (length sorted)]
                  [row-idx (make-vector nnz 0)]
                  [col-idx (make-vector nnz 0)]
                  [vals (make-vector nnz 0)])
                 ;; Fill output vectors
                 (do ([k 0 (+ k 1)]
                      [ts sorted (cdr ts)])
                     ((= k nnz) (coo->csr (make-sparse-coo ma nb row-idx col-idx vals)))
                     (let ([t (car ts)])
                          (vector-set! row-idx k (car t))
                          (vector-set! col-idx k (cadr t))
                          (vector-set! vals k (caddr t))))))))

;;; ====
;;; Sparse Scalar Operations
;;; ====

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

;;; ====
;;; Special Sparse Matrices
;;; ====

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

;;; ====
;;; Utility Functions
;;; ====

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

;;; ====
;;; Tolerance-Aware Operations
;;; ====

;;; sparse-drop-below : Num × SparseCOO → SparseCOO
;;; Drop all entries with |value| < tolerance.
;;; Useful for cleaning up after operations that don't use tolerance.
(define (sparse-coo-drop-below tol coo)
  (let* ([rows (sparse-coo-rows coo)]
         [cols (sparse-coo-cols coo)]
         [row-idx (sparse-coo-row-indices coo)]
         [col-idx (sparse-coo-col-indices coo)]
         [vals (sparse-coo-values coo)]
         [nnz (sparse-coo-nnz coo)]
         ;; First pass: count entries to keep
         [keep-count (let loop ([k 0] [count 0])
                          (if (= k nnz)
                              count
                              (loop (+ k 1)
                                    (if (>= (abs (vector-ref vals k)) tol)
                                        (+ count 1)
                                        count))))]
         [new-rows (make-vector keep-count 0)]
         [new-cols (make-vector keep-count 0)]
         [new-vals (make-vector keep-count 0)])
        ;; Second pass: copy entries to keep
        (let loop ([k 0] [j 0])
             (if (= k nnz)
                 (make-sparse-coo rows cols new-rows new-cols new-vals)
                 (let ([v (vector-ref vals k)])
                      (if (>= (abs v) tol)
                          (begin
                           (vector-set! new-rows j (vector-ref row-idx k))
                           (vector-set! new-cols j (vector-ref col-idx k))
                           (vector-set! new-vals j v)
                           (loop (+ k 1) (+ j 1)))
                          (loop (+ k 1) j)))))))

;;; sparse-csr-drop-below : Num × SparseCSR → SparseCSR
;;; Drop all entries with |value| < tolerance from CSR matrix.
(define (sparse-csr-drop-below tol csr)
  (coo->csr (sparse-coo-drop-below tol (csr->coo csr))))

;;; sparse-csc-drop-below : Num × SparseCSC → SparseCSC
;;; Drop all entries with |value| < tolerance from CSC matrix.
(define (sparse-csc-drop-below tol csc)
  (coo->csc (sparse-coo-drop-below tol (csc->coo csc))))

;;; sparse-approx-equal? : Sparse × Sparse × [Num] → Boolean
;;; Check if two sparse matrices are approximately equal within tolerance.
;;; Compares structure and values.
(define (sparse-approx-equal? a b . tol-arg)
  (let ([tol (if (null? tol-arg) *sparse-epsilon* (car tol-arg))])
    (cond
      ;; Different formats: convert both to COO and compare
      [(and (sparse-csr? a) (sparse-csr? b))
       (sparse-coo-approx-equal? (csr->coo a) (csr->coo b) tol)]
      [(and (sparse-csc? a) (sparse-csc? b))
       (sparse-coo-approx-equal? (csc->coo a) (csc->coo b) tol)]
      [(and (sparse-coo? a) (sparse-coo? b))
       (sparse-coo-approx-equal? a b tol)]
      [else
       ;; Mixed formats: convert to COO
       (let ([coo-a (cond [(sparse-csr? a) (csr->coo a)]
                          [(sparse-csc? a) (csc->coo a)]
                          [else a])]
             [coo-b (cond [(sparse-csr? b) (csr->coo b)]
                          [(sparse-csc? b) (csc->coo b)]
                          [else b])])
         (sparse-coo-approx-equal? coo-a coo-b tol))])))

;;; sparse-coo-approx-equal? : SparseCOO × SparseCOO × Num → Boolean
;;; Check if two COO matrices are approximately equal.
(define (sparse-coo-approx-equal? a b tol)
  (and (= (sparse-coo-rows a) (sparse-coo-rows b))
       (= (sparse-coo-cols a) (sparse-coo-cols b))
       ;; Compute A - B and check if all entries are below tolerance
       (let* ([diff (sparse-coo-add-impl a (sparse-coo-scale -1 b) 0)]  ; No filtering
              [diff-vals (sparse-coo-values diff)]
              [nnz (sparse-coo-nnz diff)])
         (let loop ([k 0])
           (or (= k nnz)
               (and (< (abs (vector-ref diff-vals k)) tol)
                    (loop (+ k 1))))))))
