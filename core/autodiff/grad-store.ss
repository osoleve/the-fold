;;; core/autodiff/grad-store.ss --- Gradient Storage and Retrieval
;;;
;;; Persistent gradient management using the content-addressed block system.
;;; Enables gradient checkpointing, versioning, and efficient retrieval.
;;;
;;; This is Core code: pure operations plus CAS integration.
;;;
;;; Design:
;;;   - Gradients stored as blocks with structured payloads
;;;   - Supports dense (vector) and sparse (alist) formats
;;;   - Gradient snapshots include metadata and computation graph refs
;;;   - Version chains enable gradient history traversal
;;;
;;; Block Schema:
;;;   Tag: 'grad-store
;;;   Payload: serialized gradient data + metadata
;;;   Refs[0]: previous version (or #f for initial)
;;;   Refs[1]: computation graph block (optional)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - blocks/block.ss
;;;   - blocks/cas.ss

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")

;;; ============================================================
;;; Gradient Data Structures
;;; ============================================================

;;; A gradient-set is either:
;;;   (dense vector-of-numbers)     - for contiguous IDs
;;;   (sparse alist-of-id-gradient) - for sparse gradients

;;; dense-grads : (Vector Number) -> GradientSet
(define (dense-grads vec)
  (list 'dense vec))

;;; sparse-grads : (List (Pair Nat Number)) -> GradientSet
(define (sparse-grads alist)
  (list 'sparse alist))

;;; dense-grads? : GradientSet -> Boolean
(define (dense-grads? gs)
  (and (pair? gs) (eq? (car gs) 'dense)))

;;; sparse-grads? : GradientSet -> Boolean
(define (sparse-grads? gs)
  (and (pair? gs) (eq? (car gs) 'sparse)))

;;; grads-data : GradientSet -> Vector | Alist
(define (grads-data gs)
  (cadr gs))

;;; ============================================================
;;; Gradient Lookup
;;; ============================================================

;;; grads-ref : GradientSet x Nat -> Number
;;; Look up gradient by ID, returning 0 if not found.
(define (grads-ref gs id)
  (cond
   [(dense-grads? gs)
    (let ([vec (grads-data gs)])
         (if (and (>= id 0) (< id (vector-length vec)))
             (vector-ref vec id)
             0))]
   [(sparse-grads? gs)
    (let ([pair (assv id (grads-data gs))])
         (if pair (cdr pair) 0))]
   [else 0]))

;;; grads-size : GradientSet -> Nat
;;; Number of gradients stored.
(define (grads-size gs)
  (cond
   [(dense-grads? gs) (vector-length (grads-data gs))]
   [(sparse-grads? gs) (length (grads-data gs))]
   [else 0]))

;;; grads-ids : GradientSet -> (List Nat)
;;; Get all IDs with stored gradients.
(define (grads-ids gs)
  (cond
   [(dense-grads? gs)
    (let ([vec (grads-data gs)])
         (filter (lambda (i) (not (zero? (vector-ref vec i))))
                 (iota (vector-length vec))))]
   [(sparse-grads? gs)
    (map car (grads-data gs))]
   [else '()]))

;;; ============================================================
;;; Hashtable <-> GradientSet Conversion
;;; ============================================================

;;; hashtable->grads : Hashtable -> GradientSet
;;; Convert a gradient hashtable (id -> grad) to a gradient set.
;;; Uses sparse format if less than 50% density, dense otherwise.
(define (hashtable->grads ht)
  (let-values ([(key-vec val-vec) (hashtable-entries ht)])
              (let* ([keys (vector->list key-vec)]
                     [vals (vector->list val-vec)]
                     [pairs (map cons keys vals)]
                     [max-id (if (null? keys) 0 (+ 1 (apply max keys)))]
                     [density (if (zero? max-id) 0 (/ (length keys) max-id))])
                    (if (and (> max-id 0) (>= density 0.5))
                        ;; Dense storage
                        (let ([vec (make-vector max-id 0)])
                             (for-each (lambda (p) (vector-set! vec (car p) (cdr p))) pairs)
                             (dense-grads vec))
                        ;; Sparse storage
                        (sparse-grads (sort (lambda (a b) (< (car a) (car b))) pairs))))))

;;; grads->hashtable : GradientSet -> Hashtable
;;; Convert a gradient set back to a hashtable.
(define (grads->hashtable gs)
  (let ([ht (make-hashtable equal-hash equal?)])
       (cond
        [(dense-grads? gs)
         (let ([vec (grads-data gs)])
              (do ([i 0 (+ i 1)])
                  ((>= i (vector-length vec)))
                  (let ([v (vector-ref vec i)])
                       (unless (zero? v)
                               (hashtable-set! ht i v)))))]
        [(sparse-grads? gs)
         (for-each (lambda (p) (hashtable-set! ht (car p) (cdr p)))
                   (grads-data gs))])
       ht))

;;; ============================================================
;;; Serialization
;;; ============================================================

;;; Payload format:
;;;   [format: 1 byte] 0=dense, 1=sparse
;;;   [version: 4 bytes u32-le] schema version
;;;   [timestamp: 8 bytes u64-le] epoch seconds (optional, 0 if not set)
;;;   [name-len: 4 bytes u32-le] checkpoint name length
;;;   [name: name-len bytes] UTF-8 checkpoint name
;;;   [data...] format-specific gradient data

(define grad-store-version 1)

;;; number->bytes : Number -> Bytevector
;;; Serialize a Scheme number to 8 bytes (IEEE double).
(define (number->bytes n)
  (let ([bv (make-bytevector 8)])
       (bytevector-ieee-double-set! bv 0 (inexact n) 'little)
       bv))

;;; bytes->number : Bytevector x Nat -> Number
;;; Deserialize an IEEE double from bytes at offset.
(define (bytes->number bv offset)
  (bytevector-ieee-double-ref bv offset 'little))

;;; serialize-grads : GradientSet x String x Nat -> Bytevector
;;; Serialize gradient set with metadata.
(define (serialize-grads gs name timestamp)
  (let* ([format-byte (if (dense-grads? gs) 0 1)]
         [name-bytes (string->utf8 name)]
         [name-len (bytevector-length name-bytes)])
        (if (dense-grads? gs)
            ;; Dense format: [count: 4 bytes][val0: 8 bytes]...[valN: 8 bytes]
            (let* ([vec (grads-data gs)]
                   [count (vector-length vec)]
                   [header-size (+ 1 4 8 4 name-len 4)]
                   [data-size (* count 8)]
                   [result (make-bytevector (+ header-size data-size))])
                  ;; Header
                  (bytevector-u8-set! result 0 format-byte)
                  (bytevector-u32-set! result 1 grad-store-version 'little)
                  (bytevector-u64-set! result 5 timestamp 'little)
                  (bytevector-u32-set! result 13 name-len 'little)
                  (bytevector-copy! name-bytes 0 result 17 name-len)
                  (bytevector-u32-set! result (+ 17 name-len) count 'little)
                  ;; Data
                  (do ([i 0 (+ i 1)])
                      ((>= i count))
                      (bytevector-ieee-double-set! result
                                                   (+ header-size (* i 8))
                                                   (inexact (vector-ref vec i))
                                                   'little))
                  result)
            ;; Sparse format: [count: 4 bytes][id0: 4 bytes][val0: 8 bytes]...
            (let* ([pairs (grads-data gs)]
                   [count (length pairs)]
                   [header-size (+ 1 4 8 4 name-len 4)]
                   [data-size (* count 12)]
                   [result (make-bytevector (+ header-size data-size))])
                  ;; Header
                  (bytevector-u8-set! result 0 format-byte)
                  (bytevector-u32-set! result 1 grad-store-version 'little)
                  (bytevector-u64-set! result 5 timestamp 'little)
                  (bytevector-u32-set! result 13 name-len 'little)
                  (bytevector-copy! name-bytes 0 result 17 name-len)
                  (bytevector-u32-set! result (+ 17 name-len) count 'little)
                  ;; Data
                  (let loop ([pairs pairs] [offset header-size])
                       (unless (null? pairs)
                               (let ([p (car pairs)])
                                    (bytevector-u32-set! result offset (car p) 'little)
                                    (bytevector-ieee-double-set! result (+ offset 4)
                                                                 (inexact (cdr p)) 'little)
                                    (loop (cdr pairs) (+ offset 12)))))
                  result))))

;;; deserialize-grads : Bytevector -> (Values GradientSet String Nat)
;;; Deserialize gradient set, returning (grads, name, timestamp).
(define (deserialize-grads bv)
  (let* ([format-byte (bytevector-u8-ref bv 0)]
         [version (bytevector-u32-ref bv 1 'little)]
         [timestamp (bytevector-u64-ref bv 5 'little)]
         [name-len (bytevector-u32-ref bv 13 'little)]
         [name-bytes (make-bytevector name-len)]
         [_ (bytevector-copy! bv 17 name-bytes 0 name-len)]
         [name (utf8->string name-bytes)]
         [count (bytevector-u32-ref bv (+ 17 name-len) 'little)]
         [data-offset (+ 17 name-len 4)])
        (if (= format-byte 0)
            ;; Dense
            (let ([vec (make-vector count)])
                 (do ([i 0 (+ i 1)])
                     ((>= i count))
                     (vector-set! vec i (bytevector-ieee-double-ref bv
                                                                    (+ data-offset (* i 8))
                                                                    'little)))
                 (values (dense-grads vec) name timestamp))
            ;; Sparse
            (let loop ([i 0] [offset data-offset] [pairs '()])
                 (if (>= i count)
                     (values (sparse-grads (reverse pairs)) name timestamp)
                     (let ([id (bytevector-u32-ref bv offset 'little)]
                           [val (bytevector-ieee-double-ref bv (+ offset 4) 'little)])
                          (loop (+ i 1) (+ offset 12) (cons (cons id val) pairs))))))))

;;; ============================================================
;;; Block Storage
;;; ============================================================

;;; make-null-ref : -> Bytevector
;;; Create a null reference (33 zero bytes).
(define (make-null-ref)
  (make-bytevector address-size 0))

;;; null-ref? : Bytevector -> Boolean
;;; Check if a reference is null.
(define (null-ref? ref)
  (let loop ([i 0])
       (if (>= i (bytevector-length ref))
           #t
           (and (zero? (bytevector-u8-ref ref i))
                (loop (+ i 1))))))

;;; store-grads! : GradientSet x String x Nat x (Option Bytevector) x (Option Bytevector) -> Bytevector
;;; Store gradients as a block, returning the block address.
;;;   grads: the gradient set
;;;   name: checkpoint name
;;;   timestamp: epoch timestamp
;;;   prev-ref: reference to previous version (or #f)
;;;   graph-ref: reference to computation graph (or #f)
(define (store-grads! grads name timestamp prev-ref graph-ref)
  (let* ([payload (serialize-grads grads name timestamp)]
         [refs (vector (or prev-ref (make-null-ref))
                       (or graph-ref (make-null-ref)))]
         [blk (make-block 'grad-store payload refs)])
        (store! blk)))

;;; fetch-grads : Bytevector -> (Values GradientSet String Nat Bytevector Bytevector) | #f
;;; Fetch gradients from storage by address.
;;; Returns (grads, name, timestamp, prev-ref, graph-ref) or #f if not found.
(define (fetch-grads addr)
  (let ([blk (fetch addr)])
       (if (and blk (eq? (block-tag blk) 'grad-store))
           (let-values ([(grads name ts) (deserialize-grads (block-payload blk))])
                       (let ([refs (block-refs blk)])
                            (values grads name ts
                                    (vector-ref refs 0)
                                    (vector-ref refs 1))))
           #f)))

;;; ============================================================
;;; Checkpoint Management
;;; ============================================================

;;; A gradient checkpoint is identified by its block address.
;;; Checkpoints form a linked list via prev-ref.

;;; checkpoint-history : Bytevector -> (List Bytevector)
;;; Get all addresses in the checkpoint chain, newest first.
(define (checkpoint-history addr)
  (let loop ([current addr] [history '()])
       (let ([blk (fetch current)])
            (if (and blk (eq? (block-tag blk) 'grad-store))
                (let* ([refs (block-refs blk)]
                       [prev (vector-ref refs 0)])
                      (if (null-ref? prev)
                          (reverse (cons current history))
                          (loop prev (cons current history))))
                (reverse history)))))

;;; checkpoint-count : Bytevector -> Nat
;;; Count checkpoints in history.
(define (checkpoint-count addr)
  (length (checkpoint-history addr)))

;;; latest-checkpoint : Bytevector -> Bytevector
;;; Return the most recent checkpoint in the chain.
;;; The input address is already the latest.
(define (latest-checkpoint addr)
  addr)

;;; oldest-checkpoint : Bytevector -> Bytevector
;;; Return the oldest checkpoint in the chain.
(define (oldest-checkpoint addr)
  (let ([history (checkpoint-history addr)])
       (if (null? history) addr (car history))))

;;; ============================================================
;;; High-Level API
;;; ============================================================

;;; save-gradients : Hashtable x String -> Bytevector
;;; Save gradients from a hashtable with a checkpoint name.
(define (save-gradients ht name)
  (let ([grads (hashtable->grads ht)]
        [timestamp (if (top-level-bound? 'current-time)
                       (time-second (current-time))
                       0)])
       (store-grads! grads name timestamp #f #f)))

;;; save-gradients-versioned : Hashtable x String x Bytevector -> Bytevector
;;; Save gradients with a reference to the previous version.
(define (save-gradients-versioned ht name prev-addr)
  (let ([grads (hashtable->grads ht)]
        [timestamp (if (top-level-bound? 'current-time)
                       (time-second (current-time))
                       0)])
       (store-grads! grads name timestamp prev-addr #f)))

;;; load-gradients : Bytevector -> Hashtable | #f
;;; Load gradients from storage, returning as hashtable.
(define (load-gradients addr)
  (let ([blk (fetch addr)])
       (if (and blk (eq? (block-tag blk) 'grad-store))
           (let-values ([(grads name ts) (deserialize-grads (block-payload blk))])
                       (grads->hashtable grads))
           #f)))

;;; gradient-info : Bytevector -> (List (Symbol . Any)) | #f
;;; Get metadata about a stored gradient checkpoint.
(define (gradient-info addr)
  (let ([blk (fetch addr)])
       (if (and blk (eq? (block-tag blk) 'grad-store))
           (let-values ([(grads name ts) (deserialize-grads (block-payload blk))])
                       (let ([refs (block-refs blk)])
                            `((name . ,name)
                              (timestamp . ,ts)
                              (size . ,(grads-size grads))
                              (format . ,(if (dense-grads? grads) 'dense 'sparse))
                              (has-previous . ,(not (null-ref? (vector-ref refs 0))))
                              (has-graph . ,(not (null-ref? (vector-ref refs 1)))))))
           #f)))

;;; ============================================================
;;; Gradient Arithmetic
;;; ============================================================

;;; grads-add : GradientSet x GradientSet -> GradientSet
;;; Add two gradient sets element-wise.
(define (grads-add gs1 gs2)
  (let ([ht (make-hashtable equal-hash equal?)])
       ;; Add gs1
       (cond
        [(dense-grads? gs1)
         (let ([vec (grads-data gs1)])
              (do ([i 0 (+ i 1)])
                  ((>= i (vector-length vec)))
                  (hashtable-set! ht i (vector-ref vec i))))]
        [(sparse-grads? gs1)
         (for-each (lambda (p) (hashtable-set! ht (car p) (cdr p)))
                   (grads-data gs1))])
       ;; Add gs2
       (cond
        [(dense-grads? gs2)
         (let ([vec (grads-data gs2)])
              (do ([i 0 (+ i 1)])
                  ((>= i (vector-length vec)))
                  (hashtable-set! ht i (+ (hashtable-ref ht i 0)
                                          (vector-ref vec i)))))]
        [(sparse-grads? gs2)
         (for-each (lambda (p)
                           (hashtable-set! ht (car p)
                                           (+ (hashtable-ref ht (car p) 0) (cdr p))))
                   (grads-data gs2))])
       (hashtable->grads ht)))

;;; grads-scale : GradientSet x Number -> GradientSet
;;; Scale all gradients by a constant.
(define (grads-scale gs k)
  (cond
   [(dense-grads? gs)
    (let* ([vec (grads-data gs)]
           [n (vector-length vec)]
           [result (make-vector n)])
          (do ([i 0 (+ i 1)])
              ((>= i n))
              (vector-set! result i (* k (vector-ref vec i))))
          (dense-grads result))]
   [(sparse-grads? gs)
    (sparse-grads (map (lambda (p) (cons (car p) (* (cdr p) k)))
                       (grads-data gs)))]
   [else gs]))

;;; grads-norm : GradientSet -> Number
;;; Compute L2 norm of gradients.
(define (grads-norm gs)
  (sqrt (cond
         [(dense-grads? gs)
          (let ([vec (grads-data gs)])
               (let loop ([i 0] [acc 0])
                    (if (>= i (vector-length vec))
                        acc
                        (let ([v (vector-ref vec i)])
                             (loop (+ i 1) (+ acc (* v v)))))))]
         [(sparse-grads? gs)
          (fold-left (lambda (acc p) (+ acc (* (cdr p) (cdr p))))
                     0 (grads-data gs))]
         [else 0])))

;;; grads-clip : GradientSet x Number -> GradientSet
;;; Clip gradients to maximum norm (gradient clipping for stability).
(define (grads-clip gs max-norm)
  (let ([norm (grads-norm gs)])
       (if (> norm max-norm)
           (grads-scale gs (/ max-norm norm))
           gs)))
