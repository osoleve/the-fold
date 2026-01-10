;;; core/base/sha256.ss — SHA-256 implementation (FIPS 180-4)
;;; @module sha256
;;; @requires
;;;
;;; sha256 : Bytevector → Bytevector (32 bytes)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;; The implementation follows FIPS 180-4 exactly.
;;; Optimized for Chez Scheme using fixnum operations and fxvectors.
;;;
;;; Dependencies: NONE (self-contained cryptographic primitive)
;;;
;;; This is a BASE module — no internal core dependencies.

;;; ============================================================
;;; Local Utilities (for self-containment)
;;; ============================================================

;;; iota : Nat → (List Nat)
;;; Generate list [0, 1, ..., n-1].
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; 32-bit Arithmetic (mod 2^32) using Fixnums
;;; ============================================================

;;; u32 : Nat → Nat
;;; Mask to 32-bit unsigned integer.
(define-syntax u32
  (syntax-rules ()
    [(_ x) (fxand x #xFFFFFFFF)]))

;;; u32+ : Nat* → Nat
;;; Add multiple values with 32-bit overflow.
(define-syntax u32+
  (syntax-rules ()
    [(_ . args) (fxand (fx+ . args) #xFFFFFFFF)]))

;;; rotr32 : Nat × Nat → Nat
;;; 32-bit right rotation.
;;; Implemented as (x >> n) | ((x & mask) << (32 - n))
;;; to avoid fixnum overflow on the left shift.
(define-syntax rotr32
  (syntax-rules ()
    [(_ x n)
     (fxior (fxarithmetic-shift-right x n)
            (fxarithmetic-shift-left (fxand x (fx- (fxarithmetic-shift-left 1 n) 1))
                                     (fx- 32 n)))]))

;;; shr : Nat × Nat → Nat
;;; Right shift.
(define-syntax shr
  (syntax-rules ()
    [(_ x n) (fxarithmetic-shift-right x n)]))

;;; ============================================================
;;; SHA-256 Functions
;;; ============================================================

;;; Ch : Nat × Nat × Nat → Nat
;;; SHA-256 Ch function: choose bits from y or z based on x.
(define-syntax Ch
  (syntax-rules ()
    [(_ x y z) (fxxor (fxand x y)
                      (fxand (fxnot x) z))]))

;;; Maj : Nat × Nat × Nat → Nat
;;; SHA-256 Maj function: majority of three bits.
(define-syntax Maj
  (syntax-rules ()
    [(_ x y z) (fxxor (fxand x y)
                      (fxxor (fxand x z)
                             (fxand y z)))]))

;;; Sigma0 : Nat → Nat
;;; SHA-256 big sigma 0 function.
(define-syntax Sigma0
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 2)
                  (fxxor (rotr32 x 13)
                         (rotr32 x 22)))]))

;;; Sigma1 : Nat → Nat
;;; SHA-256 big sigma 1 function.
(define-syntax Sigma1
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 6)
                  (fxxor (rotr32 x 11)
                         (rotr32 x 25)))]))

;;; sigma0 : Nat → Nat
;;; SHA-256 small sigma 0 function.
(define-syntax sigma0
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 7)
                  (fxxor (rotr32 x 18)
                         (shr x 3)))]))

;;; sigma1 : Nat → Nat
;;; SHA-256 small sigma 1 function.
(define-syntax sigma1
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 17)
                  (fxxor (rotr32 x 19)
                         (shr x 10)))]))

;;; ============================================================
;;; Constants
;;; ============================================================

;;; Initial hash values (first 32 bits of fractional parts of
;;; square roots of first 8 primes)
(define H-init
  (fxvector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
            #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

;;; Round constants (first 32 bits of fractional parts of
;;; cube roots of first 64 primes)
(define K
  (fxvector #x428a2f98 #x71374491 #xb5c0fbcf #xe9b5dba5
            #x3956c25b #x59f111f1 #x923f82a4 #xab1c5ed5
            #xd807aa98 #x12835b01 #x243185be #x550c7dc3
            #x72be5d74 #x80deb1fe #x9bdc06a7 #xc19bf174
            #xe49b69c1 #xefbe4786 #x0fc19dc6 #x240ca1cc
            #x2de92c6f #x4a7484aa #x5cb0a9dc #x76f988da
            #x983e5152 #xa831c66d #xb00327c8 #xbf597fc7
            #xc6e00bf3 #xd5a79147 #x06ca6351 #x14292967
            #x27b70a85 #x2e1b2138 #x4d2c6dfc #x53380d13
            #x650a7354 #x766a0abb #x81c2c92e #x92722c85
            #xa2bfe8a1 #xa81a664b #xc24b8b70 #xc76c51a3
            #xd192e819 #xd6990624 #xf40e3585 #x106aa070
            #x19a4c116 #x1e376c08 #x2748774c #x34b0bcb5
            #x391c0cb3 #x4ed8aa4a #x5b9cca4f #x682e6ff3
            #x748f82ee #x78a5636f #x84c87814 #x8cc70208
            #x90befffa #xa4506ceb #xbef9a3f7 #xc67178f2))

;;; ============================================================
;;; Message Padding
;;; ============================================================

;;; pad-message : Bytevector → Bytevector
;;; Pad to multiple of 64 bytes (512 bits).
;;; Append 1 bit, zeros, then 64-bit big-endian length.
(define (pad-message msg)
  (let* ([len (bytevector-length msg)]
         [bit-len (* 8 len)]
         ;; Need at least 9 bytes: 1 for 0x80, 8 for length
         ;; Pad to next multiple of 64
         [padded-len (let ([rem (modulo (+ len 9) 64)])
                          (if (= rem 0)
                              (+ len 9)
                              (+ len 9 (- 64 rem))))]
         [result (make-bytevector padded-len 0)])
        ;; Copy message
        (bytevector-copy! msg 0 result 0 len)
        ;; Append 0x80
        (bytevector-u8-set! result len #x80)
        ;; Append length as 64-bit big-endian
        (bytevector-u64-set! result (- padded-len 8) bit-len 'big)
        result))

;;; ============================================================
;;; Message Schedule
;;; ============================================================

;;; make-schedule : Bytevector × Nat → (FxVector Nat)
;;; Create 64-word message schedule from 64-byte block at offset.
(define (make-schedule msg offset)
  (let ([W (make-fxvector 64)])
       ;; W[0..15]: 16 32-bit words from block (big-endian)
       (do ([i 0 (fx+ i 1)])
           ((fx= i 16))
           (fxvector-set! W i (bytevector-u32-ref msg (fx+ offset (fx* i 4)) 'big)))
       ;; W[16..63]: extended schedule
       (do ([i 16 (fx+ i 1)])
           ((fx= i 64))
           (fxvector-set! W i
                        (u32+ (sigma1 (fxvector-ref W (fx- i 2)))
                              (fxvector-ref W (fx- i 7))
                              (sigma0 (fxvector-ref W (fx- i 15)))
                              (fxvector-ref W (fx- i 16)))))
       W))

;;; ============================================================
;;; Compression
;;; ============================================================

;;; compress : (FxVector Nat) × (FxVector Nat) → (FxVector Nat)
;;; One round of compression (H, W) → H'
(define (compress H W)
  (let ([a (fxvector-ref H 0)]
        [b (fxvector-ref H 1)]
        [c (fxvector-ref H 2)]
        [d (fxvector-ref H 3)]
        [e (fxvector-ref H 4)]
        [f (fxvector-ref H 5)]
        [g (fxvector-ref H 6)]
        [h (fxvector-ref H 7)])
       ;; 64 rounds
       (do ([i 0 (fx+ i 1)])
           ((fx= i 64))
           (let* ([T1 (u32+ h (Sigma1 e) (Ch e f g)
                            (fxvector-ref K i) (fxvector-ref W i))]
                  [T2 (u32+ (Sigma0 a) (Maj a b c))])
                 (set! h g)
                 (set! g f)
                 (set! f e)
                 (set! e (u32+ d T1))
                 (set! d c)
                 (set! c b)
                 (set! b a)
                 (set! a (u32+ T1 T2))))
       ;; Add to hash state
       (fxvector (u32+ (fxvector-ref H 0) a)
                 (u32+ (fxvector-ref H 1) b)
                 (u32+ (fxvector-ref H 2) c)
                 (u32+ (fxvector-ref H 3) d)
                 (u32+ (fxvector-ref H 4) e)
                 (u32+ (fxvector-ref H 5) f)
                 (u32+ (fxvector-ref H 6) g)
                 (u32+ (fxvector-ref H 7) h))))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

;;; sha256 : Bytevector → Bytevector
;;; Compute SHA-256 hash (32 bytes).
(define (sha256 msg)
  (let* ([padded (pad-message msg)]
         [num-blocks (quotient (bytevector-length padded) 64)]
         [H (fxvector-copy H-init)])
        ;; Process each 64-byte block
        (do ([i 0 (fx+ i 1)])
            ((fx= i num-blocks))
            (let ([W (make-schedule padded (fx* i 64))])
                 (set! H (compress H W))))
        ;; Convert final hash to bytevector
        (let ([result (make-bytevector 32)])
             (do ([i 0 (fx+ i 1)])
                 ((fx= i 8))
                 (bytevector-u32-set! result (fx* i 4) (fxvector-ref H i) 'big))
             result)))

;;; sha256-hex : Bytevector → String
;;; Convenience: return hash as lowercase hexadecimal string.
(define (sha256-hex msg)
  (let ([hash (sha256 msg)]
        [hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota 32)))))

;;; ============================================================
;;; Hash/Hex Conversion Utilities
;;; ============================================================

;;; hash->hex : Bytevector → String
;;; Convert a hash (bytevector) to lowercase hex string.
(define (hash->hex hash)
  (let ([hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota (bytevector-length hash))))))

;;; hex->hash : String → Bytevector
;;; Convert a hex string to bytevector.
(define (hex->hash hex)
  (let* ([len (string-length hex)]
         [result (make-bytevector (quotient len 2))])
        (do ([i 0 (+ i 2)])
            ((>= i len))
            (bytevector-u8-set! result
                                (quotient i 2)
                                (+ (* 16 (hex-digit (string-ref hex i)))
                                   (hex-digit (string-ref hex (+ i 1))))))
        result))

;;; hex-digit : Char → Nat
;;; Convert hex character to number.
(define (hex-digit c)
  (cond
   [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
   [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
   [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
   [else 0]))

;;; ============================================================
;;; Block Hashing (requires core/block.ss loaded first)
;;; ============================================================

;;; hash-block : Block → Bytevector
;;; Compute the versioned address of a block.
;;; The SHA-256 hash is computed over the canonical serialization,
;;; then prefixed with a version byte.
(define (hash-block blk)
  (let* ([hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
        (bytevector-u8-set! address 0 address-version)
        (bytevector-copy! hash 0 address 1 hash-size)
        address))