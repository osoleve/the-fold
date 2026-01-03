;;; core/autodiff/sparse-autodiff.ss --- Sparse Automatic Differentiation
;;;
;;; Efficient automatic differentiation for large, sparse systems using
;;; sparse matrix representations (COO, CSR, CSC).
;;;
;;; Key features:
;;;   - Sparse Jacobian computation with automatic sparsity detection
;;;   - Memory-efficient sparse gradient storage
;;;   - Sparse Hessian-vector products
;;;   - Integration with existing traced/dual number systems
;;;   - Pattern-based sparse Jacobian computation
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec.ss
;;;   - linalg/matrix.ss
;;;   - linalg/sparse.ss
;;;   - autodiff/comp-graph.ss
;;;   - autodiff/reverse-diff.ss
;;;   - autodiff/higher-order-diff.ss

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/linalg/matrix.ss")
(load "core/linalg/sparse.ss")
(load "core/autodiff/comp-graph.ss")
(load "core/autodiff/reverse-diff.ss")
(load "core/autodiff/higher-order-diff.ss")

;;; ============================================================
;;; Sparse Gradient Representation
;;; ============================================================

;;; A sparse gradient stores only non-zero partial derivatives.
;;; Represented as: (sparse-grad nnz indices values)
;;; - nnz: number of non-zero entries
;;; - indices: vector of variable indices with non-zero gradients
;;; - values: vector of corresponding gradient values

;;; sparse-grad? : Any -> Boolean
(define (sparse-grad? g)
  (and (pair? g) (eq? (car g) 'sparse-grad)))

;;; make-sparse-grad : Vec x Vec -> SparseGrad
;;; Create a sparse gradient from indices and values.
(define (make-sparse-grad indices values)
  (list 'sparse-grad (vector-length indices) indices values))

;;; sparse-grad-nnz : SparseGrad -> Nat
(define (sparse-grad-nnz g) (list-ref g 1))

;;; sparse-grad-indices : SparseGrad -> Vec
(define (sparse-grad-indices g) (list-ref g 2))

;;; sparse-grad-values : SparseGrad -> Vec
(define (sparse-grad-values g) (list-ref g 3))

;;; sparse-grad-ref : SparseGrad x Nat -> Num
;;; Get gradient value for variable at index i. O(nnz) lookup.
(define (sparse-grad-ref g i)
  (let ([indices (sparse-grad-indices g)]
        [values (sparse-grad-values g)]
        [nnz (sparse-grad-nnz g)])
       (let loop ([k 0])
            (cond
             [(= k nnz) 0]  ; Not found - gradient is 0
             [(= (vector-ref indices k) i) (vector-ref values k)]
             [else (loop (+ k 1))]))))

;;; sparse-grad->dense : SparseGrad x Nat -> Vec
;;; Convert sparse gradient to dense vector of length n.
(define (sparse-grad->dense g n)
  (let ([result (make-vector n 0)]
        [indices (sparse-grad-indices g)]
        [values (sparse-grad-values g)]
        [nnz (sparse-grad-nnz g)])
       (do ([k 0 (+ k 1)])
           ((= k nnz) result)
           (vector-set! result (vector-ref indices k) (vector-ref values k)))))

;;; dense->sparse-grad : Vec x [Num] -> SparseGrad
;;; Convert dense gradient vector to sparse, dropping values below tolerance.
(define (dense->sparse-grad v . tol-arg)
  (let* ([tol (if (null? tol-arg) 0 (car tol-arg))]
         [n (vector-length v)]
         ;; First pass: count non-zeros
         [nnz (let loop ([i 0] [count 0])
                   (if (= i n)
                       count
                       (loop (+ i 1)
                             (if (> (abs (vector-ref v i)) tol)
                                 (+ count 1)
                                 count))))]
         [indices (make-vector nnz 0)]
         [values (make-vector nnz 0)])
        ;; Second pass: fill arrays
        (let loop ([i 0] [k 0])
             (if (= i n)
                 (make-sparse-grad indices values)
                 (let ([val (vector-ref v i)])
                      (if (> (abs val) tol)
                          (begin
                           (vector-set! indices k i)
                           (vector-set! values k val)
                           (loop (+ i 1) (+ k 1)))
                          (loop (+ i 1) k)))))))

;;; sparse-grad-add : SparseGrad x SparseGrad -> SparseGrad
;;; Add two sparse gradients. Result may have more entries if indices differ.
(define (sparse-grad-add g1 g2)
  ;; Use a simple merge approach: collect all entries, accumulate
  (let* ([n1 (sparse-grad-nnz g1)]
         [n2 (sparse-grad-nnz g2)]
         [idx1 (sparse-grad-indices g1)]
         [val1 (sparse-grad-values g1)]
         [idx2 (sparse-grad-indices g2)]
         [val2 (sparse-grad-values g2)]
         ;; Find maximum index to size accumulator
         [max-idx (max (if (> n1 0)
                           (vec-fold max 0 idx1)
                           0)
                       (if (> n2 0)
                           (vec-fold max 0 idx2)
                           0))]
         [acc (make-vector (+ max-idx 1) 0)])
        ;; Accumulate from g1
        (do ([k 0 (+ k 1)])
            ((= k n1))
            (let ([i (vector-ref idx1 k)])
                 (vector-set! acc i (+ (vector-ref acc i) (vector-ref val1 k)))))
        ;; Accumulate from g2
        (do ([k 0 (+ k 1)])
            ((= k n2))
            (let ([i (vector-ref idx2 k)])
                 (vector-set! acc i (+ (vector-ref acc i) (vector-ref val2 k)))))
        ;; Convert back to sparse
        (dense->sparse-grad acc)))

;;; sparse-grad-scale : Num x SparseGrad -> SparseGrad
;;; Scale sparse gradient by a constant.
(define (sparse-grad-scale k g)
  (make-sparse-grad (vec-copy (sparse-grad-indices g))
                    (vec-map (lambda (v) (* k v)) (sparse-grad-values g))))

;;; ============================================================
;;; Sparsity Pattern Detection
;;; ============================================================

;;; A sparsity pattern records which (i,j) entries of a Jacobian are non-zero.
;;; Represented as: (sparsity-pattern m n nnz row-indices col-indices)

;;; sparsity-pattern? : Any -> Boolean
(define (sparsity-pattern? p)
  (and (pair? p) (eq? (car p) 'sparsity-pattern)))

;;; make-sparsity-pattern : Nat x Nat x Vec x Vec -> SparsityPattern
(define (make-sparsity-pattern m n row-indices col-indices)
  (list 'sparsity-pattern m n (vector-length row-indices) row-indices col-indices))

;;; pattern-rows : SparsityPattern -> Nat
(define (pattern-rows p) (list-ref p 1))

;;; pattern-cols : SparsityPattern -> Nat
(define (pattern-cols p) (list-ref p 2))

;;; pattern-nnz : SparsityPattern -> Nat
(define (pattern-nnz p) (list-ref p 3))

;;; pattern-row-indices : SparsityPattern -> Vec
(define (pattern-row-indices p) (list-ref p 4))

;;; pattern-col-indices : SparsityPattern -> Vec
(define (pattern-col-indices p) (list-ref p 5))

;;; detect-sparsity : ((Traced ...) -> (List Traced)) x (List Number) x Num -> SparsityPattern
;;; Detect sparsity pattern of Jacobian using reverse-mode autodiff.
;;; Returns pattern of (i,j) pairs where |J[i,j]| > tolerance.
;;; O(m) backward passes, O(nnz) space - never allocates dense M×N matrix.
(define (detect-sparsity f args tolerance)
  (let* ([n (length args)]
         [sample-result (apply f (map (lambda (x) (make-traced-var x (make-reverse-tape)))
                                      args))]
         [m (if (or (traced? sample-result) (not (list? sample-result)))
                1
                (length sample-result))]
         ;; Collect (row, col) pairs directly from reverse-mode gradients
         ;; One backward pass per output row
         [entries (let loop-i ([i 0] [acc '()])
                       (if (= i m)
                           acc
                           (begin
                            (reset-traced-ids!)
                            (let* ([tape (make-reverse-tape)]
                                   [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
                                   [outputs (apply f traced-args)]
                                   [output-i (if (or (traced? outputs) (not (list? outputs)))
                                                 outputs
                                                 (list-ref outputs i))]
                                   [grads (if (traced? output-i)
                                              (backward tape (traced-id output-i) 1)
                                              (make-hashtable equal-hash equal?))]
                                   [arg-ids (map traced-id traced-args)]
                                   ;; Collect non-zero entries for this row
                                   [row-entries (let loop-j ([j 0] [ids arg-ids] [row-acc '()])
                                                     (if (null? ids)
                                                         row-acc
                                                         (let ([grad (hashtable-ref grads (car ids) 0)])
                                                              (loop-j (+ j 1)
                                                                      (cdr ids)
                                                                      (if (> (abs grad) tolerance)
                                                                          (cons (cons i j) row-acc)
                                                                          row-acc)))))])
                                  (loop-i (+ i 1) (append row-entries acc))))))]
         [nnz (length entries)]
         [row-idx (make-vector nnz 0)]
         [col-idx (make-vector nnz 0)])
        ;; Fill vectors
        (do ([k 0 (+ k 1)]
             [es entries (cdr es)])
            ((= k nnz) (make-sparsity-pattern m n row-idx col-idx))
            (let ([entry (car es)])
                 (vector-set! row-idx k (car entry))
                 (vector-set! col-idx k (cdr entry))))))

;;; pattern-from-explicit : Nat x Nat x (List (Nat x Nat)) -> SparsityPattern
;;; Create sparsity pattern from explicit list of (row, col) pairs.
(define (pattern-from-explicit m n pairs)
  (let* ([nnz (length pairs)]
         [row-idx (make-vector nnz 0)]
         [col-idx (make-vector nnz 0)])
        (do ([k 0 (+ k 1)]
             [ps pairs (cdr ps)])
            ((= k nnz) (make-sparsity-pattern m n row-idx col-idx))
            (let ([p (car ps)])
                 (vector-set! row-idx k (car p))
                 (vector-set! col-idx k (cdr p))))))

;;; diagonal-pattern : Nat -> SparsityPattern
;;; Create diagonal sparsity pattern (n x n, only diagonal non-zero).
(define (diagonal-pattern n)
  (let ([indices (make-vector n 0)])
       (do ([i 0 (+ i 1)])
           ((= i n) (make-sparsity-pattern n n indices (vec-copy indices)))
           (vector-set! indices i i))))

;;; banded-pattern : Nat x Nat -> SparsityPattern
;;; Create banded sparsity pattern with bandwidth b.
;;; Entry (i,j) is non-zero if |i-j| <= b.
(define (banded-pattern n bandwidth)
  (let* ([b bandwidth]
         ;; Count non-zeros
         [nnz (let loop ([i 0] [count 0])
                   (if (= i n)
                       count
                       (loop (+ i 1)
                             (+ count
                                (min (+ b b 1)
                                     (- (min n (+ i b 1))
                                        (max 0 (- i b))))))))]
         [row-idx (make-vector nnz 0)]
         [col-idx (make-vector nnz 0)])
        ;; Fill pattern
        (let loop ([i 0] [k 0])
             (if (= i n)
                 (make-sparsity-pattern n n row-idx col-idx)
                 (let inner ([j (max 0 (- i b))] [kk k])
                      (if (> j (min (- n 1) (+ i b)))
                          (loop (+ i 1) kk)
                          (begin
                           (vector-set! row-idx kk i)
                           (vector-set! col-idx kk j)
                           (inner (+ j 1) (+ kk 1)))))))))

;;; ============================================================
;;; Sparse Jacobian Computation
;;; ============================================================

;;; sparse-jacobian : ((Traced ...) -> (List Traced)) x (List Number) -> SparseCOO
;;; Compute Jacobian in sparse COO format.
;;; Uses reverse mode: one pass per output, but only stores non-zeros.
(define (sparse-jacobian f args)
  (let* ([n (length args)]
         [sample-result (apply f (map (lambda (x) (make-traced-var x (make-reverse-tape)))
                                      args))]
         [m (if (or (traced? sample-result) (not (list? sample-result)))
                1
                (length sample-result))]
         ;; Collect all (row, col, value) triplets
         [triplets (let loop-i ([i 0] [acc '()])
                        (if (= i m)
                            acc
                            (begin
                             (reset-traced-ids!)
                             (let* ([tape (make-reverse-tape)]
                                    [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
                                    [outputs (apply f traced-args)]
                                    [output-i (if (or (traced? outputs) (not (list? outputs)))
                                                  outputs
                                                  (list-ref outputs i))]
                                    [grads (if (traced? output-i)
                                               (backward tape (traced-id output-i) 1)
                                               (make-hashtable equal-hash equal?))]
                                    [arg-ids (map traced-id traced-args)])
                                   (loop-i (+ i 1)
                                           (let loop-j ([j 0] [args-left arg-ids] [acc2 acc])
                                                (if (null? args-left)
                                                    acc2
                                                    (let ([grad (hashtable-ref grads (car args-left) 0)])
                                                         (loop-j (+ j 1)
                                                                 (cdr args-left)
                                                                 (if (= grad 0)
                                                                     acc2
                                                                     (cons (list i j grad) acc2)))))))))))])
        (sparse-coo-from-triplets m n triplets)))

;;; sparse-jacobian-with-pattern : ((Traced ...) -> (List Traced)) x (List Number) x SparsityPattern -> SparseCOO
;;; Compute Jacobian entries only at locations specified by pattern.
;;; More efficient when pattern is known a priori.
(define (sparse-jacobian-with-pattern f args pattern)
  (let* ([m (pattern-rows pattern)]
         [n (pattern-cols pattern)]
         [nnz (pattern-nnz pattern)]
         [row-idx (pattern-row-indices pattern)]
         [col-idx (pattern-col-indices pattern)]
         [values (make-vector nnz 0)]
         ;; Group entries by row for efficient computation
         [rows-to-compute (let loop ([k 0] [acc '()])
                               (if (= k nnz)
                                   (remove-duplicates (reverse acc))
                                   (loop (+ k 1) (cons (vector-ref row-idx k) acc))))])
        ;; Compute gradient for each row
        (for-each
         (lambda (i)
                 (reset-traced-ids!)
                 (let* ([tape (make-reverse-tape)]
                        [traced-args (map (lambda (x) (make-traced-var x tape)) args)]
                        [outputs (apply f traced-args)]
                        [output-i (if (or (traced? outputs) (not (list? outputs)))
                                      outputs
                                      (list-ref outputs i))]
                        [grads (if (traced? output-i)
                                   (backward tape (traced-id output-i) 1)
                                   (make-hashtable equal-hash equal?))]
                        [arg-ids (list->vector (map traced-id traced-args))])
                       ;; Fill in values for this row
                       (do ([k 0 (+ k 1)])
                           ((= k nnz))
                           (when (= (vector-ref row-idx k) i)
                                 (let* ([j (vector-ref col-idx k)]
                                        [arg-id (vector-ref arg-ids j)]
                                        [grad (hashtable-ref grads arg-id 0)])
                                       (vector-set! values k grad))))))
         rows-to-compute)
        (make-sparse-coo m n (vec-copy row-idx) (vec-copy col-idx) values)))

;;; Helper: remove duplicates from list
(define (remove-duplicates lst)
  (if (null? lst)
      '()
      (let ([first (car lst)])
           (cons first
                 (remove-duplicates (filter (lambda (x) (not (equal? x first))) (cdr lst)))))))

;;; ============================================================
;;; Sparse Jacobian-Vector and Vector-Jacobian Products
;;; ============================================================

;;; sparse-jvp : SparseCOO x Vec -> Vec
;;; Compute Jacobian-vector product J*v using sparse Jacobian.
(define (sparse-jvp J v)
  (sparse-coo-vec-mul J v))

;;; sparse-vjp : SparseCOO x Vec -> Vec
;;; Compute vector-Jacobian product v^T*J using sparse Jacobian.
(define (sparse-vjp J v)
  (let* ([m (sparse-coo-rows J)]
         [n (sparse-coo-cols J)]
         [row-idx (sparse-coo-row-indices J)]
         [col-idx (sparse-coo-col-indices J)]
         [vals (sparse-coo-values J)]
         [nnz (sparse-coo-nnz J)]
         [result (make-vector n 0)])
        ;; v^T J: result[j] = sum_i v[i] * J[i,j]
        (do ([k 0 (+ k 1)])
            ((= k nnz) result)
            (let ([i (vector-ref row-idx k)]
                  [j (vector-ref col-idx k)]
                  [Jij (vector-ref vals k)])
                 (vector-set! result j
                              (+ (vector-ref result j)
                                 (* (vector-ref v i) Jij)))))))

;;; ============================================================
;;; Sparse Hessian Computation
;;; ============================================================

;;; sparse-hessian : ((Traced ...) -> Traced) x (List Number) -> SparseCOO
;;; Compute Hessian in sparse COO format.
;;; Uses finite differences on gradient to detect/compute second derivatives.
(define (sparse-hessian f args)
  (let* ([n (length args)]
         [epsilon 1e-6]
         ;; Get gradient at central point
         [grad0 (gradient f args)]
         ;; Collect triplets by perturbing each variable
         [triplets (let loop ([j 0] [acc '()])
                        (if (= j n)
                            acc
                            (let* ([args+ (list-set args j (+ (list-ref args j) epsilon))]
                                   [grad+ (gradient f args+)]
                                   ;; H[i,j] = (grad+[i] - grad0[i]) / epsilon
                                   [new-entries (let loop-i ([i 0] [g0 grad0] [gp grad+] [acc2 '()])
                                                     (if (null? g0)
                                                         acc2
                                                         (let ([hij (/ (- (car gp) (car g0)) epsilon)])
                                                              (loop-i (+ i 1)
                                                                      (cdr g0)
                                                                      (cdr gp)
                                                                      (if (< (abs hij) 1e-10)
                                                                          acc2
                                                                          (cons (list i j hij) acc2))))))])
                                  (loop (+ j 1) (append new-entries acc)))))])
        (sparse-coo-from-triplets n n triplets)))

;;; sparse-hessian-exact : ((List Hyperdual) -> Hyperdual) x (List Number) -> SparseCOO
;;; Compute exact sparse Hessian using hyperdual numbers.
;;; More accurate than finite difference approach.
;;; O(n^2) hyperdual evaluations but O(nnz) space - never allocates dense N×N matrix.
(define (sparse-hessian-exact f args)
  (let* ([n (length args)]
         ;; Compute Hessian entries directly using hyperdual numbers
         ;; Only collect non-zero triplets
         [triplets (let loop-i ([i 0] [acc '()])
                        (if (= i n)
                            acc
                            (loop-i (+ i 1)
                                    ;; Only compute upper triangle (Hessian is symmetric)
                                    (let loop-j ([j i] [acc2 acc])
                                         (if (= j n)
                                             acc2
                                             (let* ([hd-args (let loop ([xs args] [k 0])
                                                                  (if (null? xs)
                                                                      '()
                                                                      (cons (cond
                                                                             [(and (= k i) (= k j)) (hd-var12 (car xs))]  ; diagonal
                                                                             [(= k i) (hd-var1 (car xs))]
                                                                             [(= k j) (hd-var2 (car xs))]
                                                                             [else (hd-lift (car xs))])
                                                                            (loop (cdr xs) (+ k 1)))))]
                                                    [result-hd (apply f hd-args)]
                                                    [h-ij (hd-deriv12 result-hd)])
                                                   (loop-j (+ j 1)
                                                           (if (= h-ij 0)
                                                               acc2
                                                               ;; Add both (i,j) and (j,i) for symmetry if off-diagonal
                                                               (if (= i j)
                                                                   (cons (list i j h-ij) acc2)
                                                                   (cons (list j i h-ij)
                                                                         (cons (list i j h-ij) acc2)))))))))))])
        (sparse-coo-from-triplets n n triplets)))

;;; ============================================================
;;; Sparse Hessian-Vector Product
;;; ============================================================

;;; sparse-hessian-vector-product : ((Traced ...) -> Traced) x (List Number) x Vec -> Vec
;;; Compute H*v without forming the full Hessian.
;;; Uses finite differences on gradient: H*v = (grad(f, x+epsilon*v) - grad(f, x)) / epsilon
(define (sparse-hessian-vector-product f args v)
  (let* ([epsilon 1e-6]
         [n (length args)]
         [v-list (vector->list v)]
         [args+ (map (lambda (x dx) (+ x (* epsilon dx))) args v-list)]
         [grad0 (gradient f args)]
         [grad+ (gradient f args+)]
         [result (make-vector n 0)])
        (do ([i 0 (+ i 1)]
             [g0 grad0 (cdr g0)]
             [gp grad+ (cdr gp)])
            ((= i n) result)
            (vector-set! result i (/ (- (car gp) (car g0)) epsilon)))))

;;; ============================================================
;;; Traced Values with Sparse Gradient Tracking
;;; ============================================================

;;; For very large-scale problems, we can track gradients sparsely
;;; during the backward pass.

;;; sparse-traced : Number x Nat x SparseTape -> SparseTraced
;;; A traced value that uses sparse gradient storage.
(define (sparse-traced val id sparse-tape)
  (list 'sparse-traced val id sparse-tape))

;;; sparse-traced? : Any -> Boolean
(define (sparse-traced? x)
  (and (pair? x) (eq? (car x) 'sparse-traced)))

;;; sparse-traced-value : SparseTraced -> Number
(define (sparse-traced-value t)
  (if (sparse-traced? t) (cadr t) t))

;;; sparse-traced-id : SparseTraced -> Nat
(define (sparse-traced-id t)
  (if (sparse-traced? t) (caddr t) #f))

;;; sparse-traced-tape : SparseTraced -> SparseTape
(define (sparse-traced-tape t)
  (if (sparse-traced? t) (cadddr t) #f))

;;; make-sparse-reverse-tape : -> SparseTape
;;; Create a sparse reverse tape that tracks operations with sparse gradients.
(define (make-sparse-reverse-tape)
  (list 'sparse-reverse-tape (box '())))

;;; sparse-reverse-tape? : Any -> Boolean
(define (sparse-reverse-tape? t)
  (and (pair? t) (eq? (car t) 'sparse-reverse-tape)))

;;; sparse-tape-entries : SparseTape -> (List TapeEntry)
(define (sparse-tape-entries t)
  (unbox (cadr t)))

;;; sparse-tape-push! : SparseTape x TapeEntry -> Void
(define (sparse-tape-push! tape entry)
  (set-box! (cadr tape)
            (cons entry (unbox (cadr tape)))))

;;; sparse-backward : SparseTape x Nat x Number x Nat -> SparseGrad
;;; Perform backward pass returning sparse gradient.
;;; n is the total number of input variables.
(define (sparse-backward tape output-id seed n)
  (let ([grads (make-hashtable equal-hash equal?)])
       ;; Initialize output gradient
       (hashtable-set! grads output-id seed)
       ;; Process tape in reverse
       (for-each
        (lambda (entry)
                (let* ([result-id (car entry)]
                       [input-ids (caddr entry)]
                       [local-grads (cadddr entry)]
                       [result-grad (hashtable-ref grads result-id 0)])
                      (for-each
                       (lambda (input-id local-grad)
                               (when input-id
                                     (let ([current (hashtable-ref grads input-id 0)])
                                          (hashtable-set! grads input-id
                                                          (+ current (* result-grad local-grad))))))
                       input-ids local-grads)))
        (sparse-tape-entries tape))
       ;; Convert to sparse gradient (only non-zero entries)
       (let-values ([(keys vals) (hashtable-entries grads)])
                   (let* ([ks (vector->list keys)]
                          [vs (vector->list vals)]
                          [nz-pairs (filter (lambda (p) (not (= (cdr p) 0)))
                                            (map cons ks vs))]
                          [nnz (length nz-pairs)]
                          [indices (make-vector nnz 0)]
                          [values (make-vector nnz 0)])
                         (do ([k 0 (+ k 1)]
                              [ps nz-pairs (cdr ps)])
                             ((= k nnz) (make-sparse-grad indices values))
                             (vector-set! indices k (caar ps))
                             (vector-set! values k (cdar ps)))))))

;;; ============================================================
;;; Graph Coloring for Efficient Jacobian Computation
;;; ============================================================

;;; When computing a sparse Jacobian, we can often compute multiple
;;; columns simultaneously using graph coloring. If columns i and j
;;; don't share any non-zero rows, they can be computed in one pass.

;;; color-columns : SparsityPattern -> Vec
;;; Assign colors to columns such that columns with the same color
;;; can be computed together. Returns vector of colors for each column.
(define (color-columns pattern)
  (let* ([n (pattern-cols pattern)]
         [nnz (pattern-nnz pattern)]
         [row-idx (pattern-row-indices pattern)]
         [col-idx (pattern-col-indices pattern)]
         [colors (make-vector n -1)]
         ;; Build column adjacency: cols are adjacent if they share a row
         [col-rows (make-vector n '())])
        ;; Collect rows for each column
        (do ([k 0 (+ k 1)])
            ((= k nnz))
            (let ([r (vector-ref row-idx k)]
                  [c (vector-ref col-idx k)])
                 (vector-set! col-rows c (cons r (vector-ref col-rows c)))))
        ;; Greedy coloring
        (do ([j 0 (+ j 1)])
            ((= j n) colors)
            (let* ([rows-j (vector-ref col-rows j)]
                   ;; Find colors used by adjacent columns
                   [forbidden (let loop ([c 0] [acc '()])
                                   (if (= c n)
                                       acc
                                       (let ([col-c (vector-ref colors c)])
                                            (if (and (>= col-c 0)
                                                     (not (= c j))
                                                     (rows-overlap? rows-j (vector-ref col-rows c)))
                                                (loop (+ c 1) (cons col-c acc))
                                                (loop (+ c 1) acc)))))]
                   ;; Find smallest available color
                   [color (let loop ([c 0])
                               (if (member c forbidden)
                                   (loop (+ c 1))
                                   c))])
                  (vector-set! colors j color)))))

;;; rows-overlap? : (List Nat) x (List Nat) -> Boolean
;;; Check if two lists have any common elements.
(define (rows-overlap? rows1 rows2)
  (ormap (lambda (r) (member r rows2)) rows1))

;;; num-colors : Vec -> Nat
;;; Count number of distinct colors used.
(define (num-colors colors)
  (+ 1 (vec-fold max 0 colors)))

;;; ============================================================
;;; Compressed Jacobian Computation via Coloring
;;; ============================================================

;;; sparse-jacobian-colored : ((Traced ...) -> (List Traced)) x (List Number) x SparsityPattern -> SparseCOO
;;; Compute sparse Jacobian efficiently using column coloring.
;;; Groups columns by color and computes multiple columns per forward/reverse pass.
(define (sparse-jacobian-colored f args pattern)
  (let* ([m (pattern-rows pattern)]
         [n (pattern-cols pattern)]
         [nnz (pattern-nnz pattern)]
         [row-idx (pattern-row-indices pattern)]
         [col-idx (pattern-col-indices pattern)]
         [colors (color-columns pattern)]
         [nc (num-colors colors)]
         [values (make-vector nnz 0)])
        ;; For each color, compute compressed Jacobian column
        (do ([c 0 (+ c 1)])
            ((= c nc))
            ;; Create seed vector: 1 for columns with this color, 0 otherwise
            (let ([seed (make-vector n 0)])
                 (do ([j 0 (+ j 1)])
                     ((= j n))
                     (when (= (vector-ref colors j) c)
                           (vector-set! seed j 1)))
                 ;; Compute JVP with this seed using dual numbers
                 ;; This gives us a "compressed" column
                 (let* ([dual-args (let loop ([xs args] [k 0])
                                        (if (null? xs)
                                            '()
                                            (cons (dual (car xs) (vector-ref seed k))
                                                  (loop (cdr xs) (+ k 1)))))]
                        [outputs (apply f dual-args)]
                        [derivs (if (or (dual? outputs) (not (list? outputs)))
                                    (list (dual-deriv outputs))
                                    (map dual-deriv outputs))]
                        [deriv-vec (list->vector derivs)])
                       ;; Extract values for entries with columns of this color
                       (do ([k 0 (+ k 1)])
                           ((= k nnz))
                           (let ([col (vector-ref col-idx k)])
                                (when (= (vector-ref colors col) c)
                                      (let ([row (vector-ref row-idx k)])
                                           (vector-set! values k (vector-ref deriv-vec row)))))))))
        (make-sparse-coo m n (vec-copy row-idx) (vec-copy col-idx) values)))

;;; ============================================================
;;; Conversion Utilities
;;; ============================================================

;;; sparse-jacobian->csr : SparseCOO -> SparseCSR
;;; Convert sparse Jacobian to CSR format for efficient row access.
(define sparse-jacobian->csr coo->csr)

;;; sparse-jacobian->csc : SparseCOO -> SparseCSC
;;; Convert sparse Jacobian to CSC format for efficient column access.
(define sparse-jacobian->csc coo->csc)

;;; sparse-jacobian->dense : SparseCOO -> Matrix
;;; Convert sparse Jacobian to dense matrix (for debugging/testing).
(define sparse-jacobian->dense sparse-coo->dense)

;;; ============================================================
;;; Helper: list-set
;;; ============================================================

(define (list-set lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))
