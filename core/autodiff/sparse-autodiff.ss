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

;;; sparse-grad? : α → Boolean
(define (sparse-grad? g)
  (and (pair? g) (eq? (car g) 'sparse-grad)))

;;; make-sparse-grad : (Vector Nat) × (Vector Number) → SparseGrad
(define (make-sparse-grad indices values)
  (list 'sparse-grad (vector-length indices) indices values))

;;; sparse-grad-nnz : SparseGrad → Nat
(define (sparse-grad-nnz g) (list-ref g 1))

;;; sparse-grad-indices : SparseGrad → (Vector Nat)
(define (sparse-grad-indices g) (list-ref g 2))

;;; sparse-grad-values : SparseGrad → (Vector Number)
(define (sparse-grad-values g) (list-ref g 3))

;;; sparse-grad-ref : SparseGrad × Nat → Number
(define (sparse-grad-ref g i)
  (let ([indices (sparse-grad-indices g)]
        [values (sparse-grad-values g)]
        [nnz (sparse-grad-nnz g)])
       (let loop ([k 0])
            (cond
             [(= k nnz) 0]  ; Not found - gradient is 0
             [(= (vector-ref indices k) i) (vector-ref values k)]
             [else (loop (+ k 1))]))))

;;; sparse-grad->dense : SparseGrad × Nat → (Vector Number)
(define (sparse-grad->dense g n)
  (let ([result (make-vector n 0)]
        [indices (sparse-grad-indices g)]
        [values (sparse-grad-values g)]
        [nnz (sparse-grad-nnz g)])
       (do ([k 0 (+ k 1)])
           ((= k nnz) result)
           (vector-set! result (vector-ref indices k) (vector-ref values k)))))

;;; dense->sparse-grad : (Vector Number) × Number ... → SparseGrad
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

;;; sparse-grad-add : SparseGrad × SparseGrad → SparseGrad
(define (sparse-grad-add g1 g2)
  (let* ([n1 (sparse-grad-nnz g1)]
         [n2 (sparse-grad-nnz g2)]
         [idx1 (sparse-grad-indices g1)]
         [val1 (sparse-grad-values g1)]
         [idx2 (sparse-grad-indices g2)]
         [val2 (sparse-grad-values g2)]
         ;; Use hashtable for O(nnz) memory instead of O(max-idx)
         [acc (make-hashtable equal-hash equal?)])
        ;; Accumulate from g1
        (do ([k 0 (+ k 1)])
            ((= k n1))
            (let ([i (vector-ref idx1 k)]
                  [v (vector-ref val1 k)])
                 (hashtable-set! acc i (+ (hashtable-ref acc i 0) v))))
        ;; Accumulate from g2
        (do ([k 0 (+ k 1)])
            ((= k n2))
            (let ([i (vector-ref idx2 k)]
                  [v (vector-ref val2 k)])
                 (hashtable-set! acc i (+ (hashtable-ref acc i 0) v))))
        ;; Convert hashtable to sparse gradient
        (let-values ([(keys vals) (hashtable-entries acc)])
                    (let* ([ks (vector->list keys)]
                           [vs (vector->list vals)]
                           ;; Filter out zeros
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

;;; sparse-grad-scale : Number × SparseGrad → SparseGrad
(define (sparse-grad-scale k g)
  (make-sparse-grad (vec-copy (sparse-grad-indices g))
                    (vec-map (lambda (v) (* k v)) (sparse-grad-values g))))

;;; ============================================================
;;; Sparsity Pattern Detection
;;; ============================================================

;;; A sparsity pattern records which (i,j) entries of a Jacobian are non-zero.
;;; Represented as: (sparsity-pattern m n nnz row-indices col-indices)

;;; sparsity-pattern? : α → Boolean
(define (sparsity-pattern? p)
  (and (pair? p) (eq? (car p) 'sparsity-pattern)))

;;; make-sparsity-pattern : Nat × Nat × (Vector Nat) × (Vector Nat) → SparsityPattern
(define (make-sparsity-pattern m n row-indices col-indices)
  (list 'sparsity-pattern m n (vector-length row-indices) row-indices col-indices))

;;; pattern-rows : SparsityPattern → Nat
(define (pattern-rows p) (list-ref p 1))

;;; pattern-cols : SparsityPattern → Nat
(define (pattern-cols p) (list-ref p 2))

;;; pattern-nnz : SparsityPattern → Nat
(define (pattern-nnz p) (list-ref p 3))

;;; pattern-row-indices : SparsityPattern → (Vector Nat)
(define (pattern-row-indices p) (list-ref p 4))

;;; pattern-col-indices : SparsityPattern → (Vector Nat)
(define (pattern-col-indices p) (list-ref p 5))

;;; detect-sparsity : ((Traced ...) → (List Traced)) × (List Number) × Number → SparsityPattern
(define (detect-sparsity f args tolerance)
  (let* ([n (length args)]
         [sample-result (apply f (map (lambda (x) (make-traced-var x (make-reverse-tape)))
                                      args))]
         [m (if (or (traced? sample-result) (not (list? sample-result)))
                1
                (length sample-result))]
         ;; Collect (row, col) pairs directly from reverse-mode gradients
         ;; One backward pass per output row
         ;; Use cons instead of append to avoid O(M*NNZ) complexity
         [entries (let loop-i ([i 0] [acc '()])
                       (if (= i m)
                           (reverse acc)  ; Reverse once at end: O(nnz)
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
                                   ;; Collect non-zero entries for this row - prepend to acc
                                   [new-acc (let loop-j ([j 0] [ids arg-ids] [current-acc acc])
                                                 (if (null? ids)
                                                     current-acc
                                                     (let ([grad (hashtable-ref grads (car ids) 0)])
                                                          (loop-j (+ j 1)
                                                                  (cdr ids)
                                                                  (if (> (abs grad) tolerance)
                                                                      (cons (cons i j) current-acc)
                                                                      current-acc)))))])
                                  (loop-i (+ i 1) new-acc)))))]
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

;;; pattern-from-explicit : Nat × Nat × (List (Nat × Nat)) → SparsityPattern
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

;;; diagonal-pattern : Nat → SparsityPattern
(define (diagonal-pattern n)
  (let ([indices (make-vector n 0)])
       (do ([i 0 (+ i 1)])
           ((= i n) (make-sparsity-pattern n n indices (vec-copy indices)))
           (vector-set! indices i i))))

;;; banded-pattern : Nat × Nat → SparsityPattern
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

;;; sparse-jacobian : ((Traced ...) → (List Traced)) × (List Number) → SparseCOO
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

;;; sparse-jacobian-with-pattern : ((Traced ...) → (List Traced)) × (List Number) × SparsityPattern → SparseCOO
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

;;; NOTE: remove-duplicates is provided by core/base/prelude.ss

;;; ============================================================
;;; Sparse Jacobian-Vector and Vector-Jacobian Products
;;; ============================================================

;;; sparse-jvp : SparseCOO × (Vector Number) → (Vector Number)
(define (sparse-jvp J v)
  (sparse-coo-vec-mul J v))

;;; sparse-vjp : SparseCOO × (Vector Number) → (Vector Number)
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

;;; sparse-hessian : ((Traced ...) → Traced) × (List Number) → SparseCOO
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

;;; sparse-hessian-exact : ((List Hyperdual) → Hyperdual) × (List Number) → SparseCOO
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

;;; sparse-hessian-vector-product : ((Traced ...) → Traced) × (List Number) × (Vector Number) → (Vector Number)
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

;;; sparse-traced : Number × Nat × SparseTape → SparseTraced
(define (sparse-traced val id sparse-tape)
  (list 'sparse-traced val id sparse-tape))

;;; sparse-traced? : α → Boolean
(define (sparse-traced? x)
  (and (pair? x) (eq? (car x) 'sparse-traced)))

;;; sparse-traced-value : SparseTraced → Number
(define (sparse-traced-value t)
  (if (sparse-traced? t) (cadr t) t))

;;; sparse-traced-id : SparseTraced → (Option Nat)
(define (sparse-traced-id t)
  (if (sparse-traced? t) (caddr t) #f))

;;; sparse-traced-tape : SparseTraced → (Option SparseTape)
(define (sparse-traced-tape t)
  (if (sparse-traced? t) (cadddr t) #f))

;;; make-sparse-reverse-tape : → SparseTape
(define (make-sparse-reverse-tape)
  (list 'sparse-reverse-tape (box '())))

;;; sparse-reverse-tape? : α → Boolean
(define (sparse-reverse-tape? t)
  (and (pair? t) (eq? (car t) 'sparse-reverse-tape)))

;;; sparse-tape-entries : SparseTape → (List TapeEntry)
(define (sparse-tape-entries t)
  (unbox (cadr t)))

;;; sparse-tape-push! : SparseTape × TapeEntry → Void
(define (sparse-tape-push! tape entry)
  (set-box! (cadr tape)
            (cons entry (unbox (cadr tape)))))

;;; sparse-backward : SparseTape × Nat × Number × Nat → SparseGrad
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

;;; color-columns : SparsityPattern → (Vector Nat)
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

;;; rows-overlap? : (List Nat) × (List Nat) → Boolean
(define (rows-overlap? rows1 rows2)
  (ormap (lambda (r) (member r rows2)) rows1))

;;; num-colors : (Vector Nat) → Nat
(define (num-colors colors)
  (+ 1 (vec-fold max 0 colors)))

;;; ============================================================
;;; Compressed Jacobian Computation via Coloring
;;; ============================================================

;;; sparse-jacobian-colored : ((Traced ...) → (List Traced)) × (List Number) × SparsityPattern → SparseCOO
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

;;; sparse-jacobian->csr : SparseCOO → SparseCSR
(define sparse-jacobian->csr coo->csr)

;;; sparse-jacobian->csc : SparseCOO → SparseCSC
(define sparse-jacobian->csc coo->csc)

;;; sparse-jacobian->dense : SparseCOO → Matrix
(define sparse-jacobian->dense sparse-coo->dense)

;;; ============================================================
;;; Helper: list-set
;;; ============================================================

;;; list-set : (List α) × Nat × α → (List α)
(define (list-set lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))
