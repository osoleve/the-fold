(load "core/base/prelude.ss")

(doc 'module 'block)
(doc 'description "The fundamental unit of The Fold: Block = {tag, payload, refs[]}")
(doc 'layer 'core)

(doc 'section 'compat)

(define (sexpr->string obj)
  (doc 'type (-> Any String))
  (doc 'description "Convert any s-expression to a string using write semantics. Provides ~s behavior for Chez 9.5.")
  (doc 'export #t)
  (let ([port (open-output-string)])
       (write obj port)
       (get-output-string port)))

(doc 'section 'block-construction)

(doc make-block 'type (-> Symbol Bytevector (Vector Bytevector) Block))
(doc make-block 'description "Construct a new block with tag, payload, and refs.")
(doc make-block 'export #t)

(doc block? 'type (-> Any Boolean))
(doc block? 'description "Predicate to test if a value is a block.")
(doc block? 'export #t)

(doc block-tag 'type (-> Block Symbol))
(doc block-tag 'description "Extract the tag from a block.")
(doc block-tag 'export #t)

(doc block-payload 'type (-> Block Bytevector))
(doc block-payload 'description "Extract the payload from a block.")
(doc block-payload 'export #t)

(doc block-refs 'type (-> Block (Vector Bytevector)))
(doc block-refs 'description "Extract the refs vector from a block.")
(doc block-refs 'export #t)

(define-record-type block
  (nongenerative fold-block-record-v1)
  (fields tag payload refs))

(doc 'section 'serialization)

(doc hash-size 'type Nat)
(doc hash-size 'description "The size of a hash in bytes (SHA-256 = 32 bytes).")
(doc hash-size 'export #t)
(define hash-size 32)

(doc address-version 'type Nat)
(doc address-version 'description "The version byte for addresses (currently 0).")
(doc address-version 'export #t)
(define address-version 0)

(doc address-size 'type Nat)
(doc address-size 'description "The size of an address in bytes (version byte + hash).")
(doc address-size 'export #t)
(define address-size (+ 1 hash-size))

(define (u32->bytes-le n)
  (doc 'type (-> Nat Bytevector))
  (doc 'description "Encode a 32-bit unsigned integer as 4 bytes, little-endian.")
  (doc 'export #t)
  (let ([bv (make-bytevector 4)])
       (bytevector-u32-set! bv 0 n 'little)
       bv))

(define (bytes-le->u32 bv offset)
  (doc 'type (-> Bytevector Nat Nat))
  (doc 'description "Decode a 32-bit unsigned integer from bytes at offset, little-endian.")
  (doc 'export #t)
  (bytevector-u32-ref bv offset 'little))

(define (symbol->utf8 sym)
  (doc 'type (-> Symbol Bytevector))
  (doc 'description "Convert symbol name to UTF-8 bytes. NFC normalization required in Boundary.")
  (doc 'export #t)
  (string->utf8 (symbol->string sym)))

(define (utf8->symbol bv)
  (doc 'type (-> Bytevector Symbol))
  (doc 'description "Convert UTF-8 bytes to symbol.")
  (doc 'export #t)
  (string->symbol (utf8->string bv)))

(define (bytevector-concat bvs)
  (doc 'type (-> (List Bytevector) Bytevector))
  (doc 'description "Concatenate a list of bytevectors into one.")
  (doc 'export #t)
  (let* ([total (fold-left + 0 (map bytevector-length bvs))]
         [result (make-bytevector total)]
         [pos 0])
        (for-each
         (lambda (bv)
                 (bytevector-copy! bv 0 result pos (bytevector-length bv))
                 (set! pos (+ pos (bytevector-length bv))))
         bvs)
        result))

(define (block->bytes blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Serialize a block to its canonical byte representation.")
  (doc 'export #t)
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

(define (bytes->block bv)
  (doc 'type (-> Bytevector Block))
  (doc 'description "Deserialize a block from canonical bytes. Validates bounds. Supports 32-byte and 33-byte refs.")
  (doc 'export #t)
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
              ;; Detect actual ref size from remaining bytes (backwards compat)
              ;; Supports 32-byte (legacy SHA-256) and 33-byte (versioned) refs
              [remaining (- bv-len pos)]
              [actual-ref-size (if (= refs-count 0)
                                   address-size  ; Default if no refs
                                   (quotient remaining refs-count))]
              ;; Validate ref size is either 32 (legacy) or 33 (versioned)
              [_ (unless (or (= actual-ref-size 32) (= actual-ref-size 33) (= refs-count 0))
                         (error 'bytes->block "invalid ref size" actual-ref-size))]
              ;; Validate total refs fit exactly
              [_ (when (and (> refs-count 0)
                            (not (= (* refs-count actual-ref-size) remaining)))
                       (error 'bytes->block "refs don't fill remaining bytes" refs-count remaining))]
              [refs (make-vector refs-count)])
             ;; Read each ref
             (do ([i 0 (+ i 1)])
                 ((= i refs-count))
                 (let ([ref (make-bytevector actual-ref-size)])
                      (bytevector-copy! bv pos ref 0 actual-ref-size)
                      (vector-set! refs i ref)
                      (set! pos (+ pos actual-ref-size))))
             (make-block tag payload refs))))

(doc 'section 'utilities)

(doc empty-payload 'type Bytevector)
(doc empty-payload 'description "The empty bytevector, for blocks with no payload.")
(doc empty-payload 'export #t)
(define empty-payload (make-bytevector 0))

(doc empty-refs 'type (Vector Bytevector))
(doc empty-refs 'description "The empty vector, for blocks with no references.")
(doc empty-refs 'export #t)
(define empty-refs (vector))

(define (block-equal? a b)
  (doc 'type (-> Block Block Boolean))
  (doc 'description "Structural equality of blocks.")
  (doc 'export #t)
  (and (eq? (block-tag a) (block-tag b))
       (bytevector=? (block-payload a) (block-payload b))
       (= (vector-length (block-refs a)) (vector-length (block-refs b)))
       (let ([refs-a (block-refs a)]
             [refs-b (block-refs b)])
            (let loop ([i 0])
                 (or (= i (vector-length refs-a))
                     (and (bytevector=? (vector-ref refs-a i) (vector-ref refs-b i))
                          (loop (+ i 1))))))))
