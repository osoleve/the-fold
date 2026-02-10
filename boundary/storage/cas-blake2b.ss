;;; cas-blake2b.ss — BLAKE2b Content-Addressed Storage Extension
;;;
;;; Provides BLAKE2b-based hashing as an alternative to SHA-256 for CAS.
;;; ~5x faster for large blocks while maintaining cryptographic security.

;;; @module cas-blake2b
;;; @requires prelude block cas normalize blake2b

(require 'prelude)
(require 'block)
(require 'cas)
(require 'normalize)
(require 'blake2b)

(doc 'module 'cas-blake2b)
(doc 'description "BLAKE2b alternative hashing for Content-Addressed Store")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'note "BLAKE2b is ~5x faster than SHA-256 with equivalent security. Use for performance-critical scenarios.")

(doc 'section 'blake2b-hashing-functions)

(define (hash-block-blake2b blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Compute BLAKE2b versioned address of block (v0x10). No normalization applied.")
  (doc 'export #t)
  (let* ([hash (blake2b (block->bytes blk) hash-size)]
         [address (make-bytevector address-size)])
    (bytevector-u8-set! address 0 address-version-blake2b-alpha)
    (bytevector-copy! hash 0 address 1 hash-size)
    address))

(define (hash-sexpr-blake2b tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with BLAKE2b and α-normalization (v0x10).")
  (doc 'export #t)
  (let* ([normalized (normalize sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)])
    (hash-block-blake2b blk)))

(define (hash-sexpr-blake2b-algebraic tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with BLAKE2b and algebraic normalization (v0x11).")
  (doc 'export #t)
  (let* ([normalized (normalize-full sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)]
         [hash (blake2b (block->bytes blk) hash-size)]
         [address (make-bytevector address-size)])
    (bytevector-u8-set! address 0 address-version-blake2b-algebraic)
    (bytevector-copy! hash 0 address 1 hash-size)
    address))

(define (hash-sexpr-blake2b-v2 tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with BLAKE2b and v2 normalization (v0x12).")
  (doc 'export #t)
  (let* ([normalized (normalize-v2 sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)]
         [hash (blake2b (block->bytes blk) hash-size)]
         [address (make-bytevector address-size)])
    (bytevector-u8-set! address 0 address-version-blake2b-v2)
    (bytevector-copy! hash 0 address 1 hash-size)
    address))

(define (hash-sexpr-blake2b-v3 tag sexpr)
  (doc 'type (-> Symbol Any Bytevector))
  (doc 'description "Hash S-expression with BLAKE2b and v3 normalization (v0x13).")
  (doc 'export #t)
  (let* ([normalized (normalize-v3 sexpr)]
         [payload (string->utf8 (format "~s" normalized))]
         [blk (make-block tag payload empty-refs)]
         [hash (blake2b (block->bytes blk) hash-size)]
         [address (make-bytevector address-size)])
    (bytevector-u8-set! address 0 address-version-blake2b-v3)
    (bytevector-copy! hash 0 address 1 hash-size)
    address))

(doc 'section 'hash-algorithm-detection)

(define (address-hash-algorithm addr)
  (doc 'type (-> Bytevector Symbol))
  (doc 'description "Return the hash algorithm used for an address: 'sha256 or 'blake2b.")
  (doc 'export #t)
  (if (address-blake2b? addr)
      'blake2b
      'sha256))

(define (address-normalization-level addr)
  (doc 'type (-> Bytevector Symbol))
  (doc 'description "Return the normalization level: 'alpha, 'algebraic, 'v2, or 'v3.")
  (doc 'export #t)
  (let ([v (address-version-byte addr)])
    (case (bitwise-and v #x0F)  ; Low nibble encodes normalization
      [(0) 'alpha]
      [(1) 'algebraic]
      [(2) 'v2]
      [(3) 'v3]
      [else 'unknown])))
