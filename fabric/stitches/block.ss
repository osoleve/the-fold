;;; fabric/stitches/block.ss — The fundamental unit of The Fold
;;;
;;; Block = {tag, payload, refs[]}
;;;
;;; - tag: Symbol identifying the block type
;;; - payload: Bytevector of raw data
;;; - refs: Vector of hashes (each hash is a 32-byte bytevector)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;
;;; See fabric/stitches/MODULES.md for full dependency graph.

(load "prelude.ss")

;;; ============================================================
;;; Compat: ~s format directive (Chez 9.5 compatibility)
;;; ============================================================

;;; sexpr->string : Any → String
;;; Convert any s-expression to a string using write semantics.
;;; This provides ~s behavior for Chez 9.5 which lacks it.
(define (sexpr->string obj)
  (let ([port (open-output-string)])
    (write obj port)
    (get-output-string port)))

;;; ============================================================
;;; Block Construction and Access
;;; ============================================================

;;; A Block is represented as an immutable record.
;;;
;;; Type: Block
;;; Constructor: make-block : Symbol × Bytevector × (Vector Bytevector) → Block
;;; Accessors:
;;;   block-tag : Block → Symbol
;;;   block-payload : Block → Bytevector
;;;   block-refs : Block → (Vector Bytevector)
(define-record-type block
  (fields tag payload refs))

;;; ============================================================
;;; Canonical Serialization
;;;
;;; Format (all lengths little-endian u32):
;;;   [tag-len : 4 bytes][tag : tag-len bytes (UTF-8, NFC)]
;;;   [payload-len : 4 bytes][payload : payload-len bytes]
;;;   [refs-count : 4 bytes][ref₀ : 32 bytes]...[refₙ : 32 bytes]
;;;
;;; Hash size is fixed at 32 bytes (SHA-256).
;;; ============================================================

(define hash-size 32)

;;; u32->bytes-le : Nat → Bytevector
;;; Encode a 32-bit unsigned integer as 4 bytes, little-endian.
(define (u32->bytes-le n)
  (let ([bv (make-bytevector 4)])
    (bytevector-u32-set! bv 0 n 'little)
    bv))

;;; bytes-le->u32 : Bytevector × Nat → Nat
;;; Decode a 32-bit unsigned integer from bytes at offset, little-endian.
(define (bytes-le->u32 bv offset)
  (bytevector-u32-ref bv offset 'little))

;;; symbol->utf8 : Symbol → Bytevector
;;; Convert symbol name to UTF-8 bytes.
;;; Note: NFC normalization is Shell's responsibility before reaching Core.
(define (symbol->utf8 sym)
  (string->utf8 (symbol->string sym)))

;;; utf8->symbol : Bytevector → Symbol
;;; Convert UTF-8 bytes to symbol.
(define (utf8->symbol bv)
  (string->symbol (utf8->string bv)))

;;; bytevector-concat : (List Bytevector) → Bytevector
;;; Concatenate a list of bytevectors into one.
(define (bytevector-concat bvs)
  (let* ([total (fold-left + 0 (map bytevector-length bvs))]
         [result (make-bytevector total)]
         [pos 0])
    (for-each
      (lambda (bv)
        (bytevector-copy! bv 0 result pos (bytevector-length bv))
        (set! pos (+ pos (bytevector-length bv))))
      bvs)
    result))

;;; block->bytes : Block → Bytevector
;;; Serialize a block to its canonical byte representation.
(define (block->bytes blk)
  (let* ([tag-bytes (symbol->utf8 (block-tag blk))]
         [payload (block-payload blk)]
         [refs (block-refs blk)]
         [refs-count (vector-length refs)])
    (bytevector-concat
      (list
        ;; Tag: length-prefixed
        (u32->bytes-le (bytevector-length tag-bytes))
        tag-bytes
        ;; Payload: length-prefixed
        (u32->bytes-le (bytevector-length payload))
        payload
        ;; Refs: count-prefixed, each ref is hash-size bytes
        (u32->bytes-le refs-count)
        (bytevector-concat (vector->list refs))))))

;;; bytes->block : Bytevector → Block
;;; Deserialize a block from its canonical byte representation.
(define (bytes->block bv)
  (let* ([pos 0]
         ;; Read tag
         [tag-len (bytes-le->u32 bv pos)]
         [_ (set! pos (+ pos 4))]
         [tag-bytes (make-bytevector tag-len)]
         [_ (bytevector-copy! bv pos tag-bytes 0 tag-len)]
         [_ (set! pos (+ pos tag-len))]
         [tag (utf8->symbol tag-bytes)]
         ;; Read payload
         [payload-len (bytes-le->u32 bv pos)]
         [_ (set! pos (+ pos 4))]
         [payload (make-bytevector payload-len)]
         [_ (bytevector-copy! bv pos payload 0 payload-len)]
         [_ (set! pos (+ pos payload-len))]
         ;; Read refs
         [refs-count (bytes-le->u32 bv pos)]
         [_ (set! pos (+ pos 4))]
         [refs (make-vector refs-count)])
    ;; Read each ref
    (do ([i 0 (+ i 1)])
        ((= i refs-count))
      (let ([ref (make-bytevector hash-size)])
        (bytevector-copy! bv pos ref 0 hash-size)
        (vector-set! refs i ref)
        (set! pos (+ pos hash-size))))
    (make-block tag payload refs)))

;;; ============================================================
;;; Block Utilities
;;; ============================================================

;;; empty-payload : Bytevector
;;; The empty bytevector, for blocks with no payload.
(define empty-payload (make-bytevector 0))

;;; empty-refs : (Vector Bytevector)
;;; The empty vector, for blocks with no references.
(define empty-refs (vector))

;;; block-equal? : Block × Block → Boolean
;;; Structural equality of blocks.
(define (block-equal? a b)
  (and (eq? (block-tag a) (block-tag b))
       (bytevector=? (block-payload a) (block-payload b))
       (= (vector-length (block-refs a)) (vector-length (block-refs b)))
       (let ([refs-a (block-refs a)]
             [refs-b (block-refs b)])
         (let loop ([i 0])
           (or (= i (vector-length refs-a))
               (and (bytevector=? (vector-ref refs-a i) (vector-ref refs-b i))
                    (loop (+ i 1))))))))
