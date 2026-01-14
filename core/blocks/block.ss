;;; core/blocks/block.ss — The fundamental unit of The Fold
;;; @module block
;;; @requires prelude
;;;
;;; Block = {tag, payload, refs[]}
;;;
;;; - tag: Symbol identifying the block type
;;; - payload: Bytevector of raw data
;;; - refs: Vector of addresses (each address is a 33-byte bytevector)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;
;;; See core/blocks/MODULES.md for full dependency graph.

(load "core/base/prelude.ss")

;;; ====
;;; Compat: ~s format directive (Chez 9.5 compatibility)
;;; ====

;;; sexpr->string : Any → String
;;; Convert any s-expression to a string using write semantics.
;;; This provides ~s behavior for Chez 9.5 which lacks it.
(define (sexpr->string obj)
  (let ([port (open-output-string)])
       (write obj port)
       (get-output-string port)))

;;; ====
;;; Block Construction and Access
;;; ====

;;; A Block is represented as an immutable record.

;;; make-block : Symbol × Bytevector × (Vector Bytevector) → Block
;;; Construct a new block with tag, payload, and refs.

;;; block? : α → Boolean
;;; Predicate to test if a value is a block.

;;; block-tag : Block → Symbol
;;; Extract the tag from a block.

;;; block-payload : Block → Bytevector
;;; Extract the payload from a block.

;;; block-refs : Block → (Vector Bytevector)
;;; Extract the refs vector from a block.
(define-record-type block
  (fields tag payload refs))

;;; ====
;;; Canonical Serialization
;;;
;;; Format (all lengths little-endian u32):
;;;   [tag-len : 4 bytes][tag : tag-len bytes (UTF-8, NFC)]
;;;   [payload-len : 4 bytes][payload : payload-len bytes]
;;;   [refs-count : 4 bytes][ref₀ : 33 bytes]...[refₙ : 33 bytes]
;;;
;;; Hash size is fixed at 32 bytes (SHA-256).
;;; Address size is 33 bytes (version byte + hash bytes).
;;; ====

;;; hash-size : Nat
;;; The size of a hash in bytes (SHA-256 = 32 bytes).
(define hash-size 32)

;;; address-version : Nat
;;; The version byte for addresses (currently 0).
(define address-version 0)

;;; address-size : Nat
;;; The size of an address in bytes (version byte + hash).
(define address-size (+ 1 hash-size))

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
;;; Note: NFC normalization is required in Shell before reaching Core.
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
          ;; Refs: count-prefixed, each ref is address-size bytes
          (u32->bytes-le refs-count)
          (bytevector-concat (vector->list refs))))))

;;; bytes->block : Bytevector → Block
;;; Deserialize a block from its canonical byte representation.
;;; Validates bounds to prevent malformed input from causing errors.
(define (bytes->block bv)
  (let ([bv-len (bytevector-length bv)])
       ;; Ensure minimum size for tag length field
       (when (< bv-len 4)
             (error 'bytes->block "bytevector too short for tag length" bv-len))
       (let* ([pos 0]
              ;; Read tag
              [tag-len (bytes-le->u32 bv pos)]
              [_ (set! pos (+ pos 4))]
              ;; Validate tag length
              [_ (when (> (+ pos tag-len) bv-len)
                       (error 'bytes->block "tag length exceeds bytevector bounds" tag-len bv-len))]
              [tag-bytes (make-bytevector tag-len)]
              [_ (bytevector-copy! bv pos tag-bytes 0 tag-len)]
              [_ (set! pos (+ pos tag-len))]
              [tag (utf8->symbol tag-bytes)]
              ;; Read payload
              [_ (when (> (+ pos 4) bv-len)
                       (error 'bytes->block "bytevector too short for payload length" pos bv-len))]
              [payload-len (bytes-le->u32 bv pos)]
              [_ (set! pos (+ pos 4))]
              ;; Validate payload length
              [_ (when (> (+ pos payload-len) bv-len)
                       (error 'bytes->block "payload length exceeds bytevector bounds" payload-len bv-len))]
              [payload (make-bytevector payload-len)]
              [_ (bytevector-copy! bv pos payload 0 payload-len)]
              [_ (set! pos (+ pos payload-len))]
              ;; Read refs
              [_ (when (> (+ pos 4) bv-len)
                       (error 'bytes->block "bytevector too short for refs count" pos bv-len))]
              [refs-count (bytes-le->u32 bv pos)]
              [_ (set! pos (+ pos 4))]
              ;; Validate refs total size
              [refs-total-size (* refs-count address-size)]
              [_ (when (> (+ pos refs-total-size) bv-len)
                       (error 'bytes->block "refs exceed bytevector bounds" refs-count bv-len))]
              [refs (make-vector refs-count)])
             ;; Read each ref
             (do ([i 0 (+ i 1)])
                 ((= i refs-count))
                 (let ([ref (make-bytevector address-size)])
                      (bytevector-copy! bv pos ref 0 address-size)
                      (vector-set! refs i ref)
                      (set! pos (+ pos address-size))))
             (make-block tag payload refs))))

;;; ====
;;; Block Utilities
;;; ====

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
