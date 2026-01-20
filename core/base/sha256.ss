;;; Local doc macro - sha256.ss is self-contained, no prelude dependency
(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'sha256)
(doc 'description "SHA-256 implementation (FIPS 180-4). The implementation follows FIPS 180-4 exactly. Optimized for Chez Scheme using fixnum operations and fxvectors.")
(doc 'layer 'core)
(doc 'purity 'total)
(doc 'dependencies '())
(doc 'note "BASE module — no internal core dependencies. Self-contained cryptographic primitive.")

(doc 'section 'local-utilities)
(doc 'note "For self-containment.")

(define (iota n)
  (doc 'type (-> Nat (List Nat)))
  (doc 'description "Generate list [0, 1, ..., n-1].")
  (let loop ([i 0] [acc '()])
       (if (= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

(doc 'section '32bit-arithmetic)
(doc 'note "mod 2^32 using Fixnums.")

(doc u32 'type Syntax)
(doc u32 'description "Mask to 32-bit unsigned integer.")
(define-syntax u32
  (syntax-rules ()
    [(_ x) (fxand x #xFFFFFFFF)]))

(doc u32+ 'type Syntax)
(doc u32+ 'description "Add multiple values with 32-bit overflow.")
(define-syntax u32+
  (syntax-rules ()
    [(_ . args) (fxand (fx+ . args) #xFFFFFFFF)]))

(doc rotr32 'type Syntax)
(doc rotr32 'description "32-bit right rotation. Implemented as (x >> n) | ((x & mask) << (32 - n)) to avoid fixnum overflow on the left shift.")
(define-syntax rotr32
  (syntax-rules ()
    [(_ x n)
     (fxior (fxarithmetic-shift-right x n)
            (fxarithmetic-shift-left (fxand x (fx- (fxarithmetic-shift-left 1 n) 1))
                                     (fx- 32 n)))]))

(doc shr 'type Syntax)
(doc shr 'description "Right shift.")
(define-syntax shr
  (syntax-rules ()
    [(_ x n) (fxarithmetic-shift-right x n)]))

(doc 'section 'sha256-functions)

(doc Ch 'type Syntax)
(doc Ch 'description "SHA-256 Ch function: choose bits from y or z based on x.")
(define-syntax Ch
  (syntax-rules ()
    [(_ x y z) (fxxor (fxand x y)
                      (fxand (fxnot x) z))]))

(doc Maj 'type Syntax)
(doc Maj 'description "SHA-256 Maj function: majority of three bits.")
(define-syntax Maj
  (syntax-rules ()
    [(_ x y z) (fxxor (fxand x y)
                      (fxxor (fxand x z)
                             (fxand y z)))]))

(doc Sigma0 'type Syntax)
(doc Sigma0 'description "SHA-256 big sigma 0 function.")
(define-syntax Sigma0
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 2)
                  (fxxor (rotr32 x 13)
                         (rotr32 x 22)))]))

(doc Sigma1 'type Syntax)
(doc Sigma1 'description "SHA-256 big sigma 1 function.")
(define-syntax Sigma1
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 6)
                  (fxxor (rotr32 x 11)
                         (rotr32 x 25)))]))

(doc sigma0 'type Syntax)
(doc sigma0 'description "SHA-256 small sigma 0 function.")
(define-syntax sigma0
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 7)
                  (fxxor (rotr32 x 18)
                         (shr x 3)))]))

(doc sigma1 'type Syntax)
(doc sigma1 'description "SHA-256 small sigma 1 function.")
(define-syntax sigma1
  (syntax-rules ()
    [(_ x) (fxxor (rotr32 x 17)
                  (fxxor (rotr32 x 19)
                         (shr x 10)))]))

(doc 'section 'constants)

(doc H-init 'type FxVector)
(doc H-init 'description "Initial hash values (first 32 bits of fractional parts of square roots of first 8 primes).")
(define H-init
  (fxvector #x6a09e667 #xbb67ae85 #x3c6ef372 #xa54ff53a
            #x510e527f #x9b05688c #x1f83d9ab #x5be0cd19))

(doc K 'type FxVector)
(doc K 'description "Round constants (first 32 bits of fractional parts of cube roots of first 64 primes).")
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

(doc 'section 'message-padding)

(define (pad-message msg)
  (doc 'type (-> Bytevector Bytevector))
  (doc 'description "Pad to multiple of 64 bytes (512 bits). Append 1 bit, zeros, then 64-bit big-endian length.")
  (doc 'export #t)
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

(doc 'section 'message-schedule)

(define (make-schedule msg offset)
  (doc 'type (-> Bytevector Nat FxVector))
  (doc 'description "Create 64-word message schedule from 64-byte block at offset.")
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

(doc 'section 'compression)

(define (compress H W)
  (doc 'type (-> FxVector FxVector FxVector))
  (doc 'description "One round of compression (H, W) → H'.")
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

(doc 'section 'main-entry-point)

(define (sha256 msg)
  (doc 'type (-> Bytevector Bytevector))
  (doc 'description "Compute SHA-256 hash (32 bytes).")
  (doc 'export #t)
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

(define (sha256-hex msg)
  (doc 'type (-> Bytevector String))
  (doc 'description "Convenience: return hash as lowercase hexadecimal string.")
  (doc 'export #t)
  (let ([hash (sha256 msg)]
        [hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota 32)))))

(doc 'section 'hash-hex-conversion)

(define (hash->hex hash)
  (doc 'type (-> Bytevector String))
  (doc 'description "Convert a hash (bytevector) to lowercase hex string.")
  (doc 'export #t)
  (let ([hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota (bytevector-length hash))))))

(define (hex->hash hex)
  (doc 'type (-> String Bytevector))
  (doc 'description "Convert a hex string to bytevector.")
  (doc 'export #t)
  (let* ([len (string-length hex)]
         [result (make-bytevector (quotient len 2))])
        (do ([i 0 (+ i 2)])
            ((>= i len))
            (bytevector-u8-set! result
                                (quotient i 2)
                                (+ (* 16 (hex-digit (string-ref hex i)))
                                   (hex-digit (string-ref hex (+ i 1))))))
        result))

(define (hex-digit c)
  (doc 'type (-> Char Nat))
  (doc 'description "Convert hex character to number.")
  (cond
   [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
   [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
   [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
   [else 0]))

(doc 'section 'block-hashing)
(doc 'note "Requires core/block.ss loaded first.")

(define (hash-block blk)
  (doc 'type (-> Block Bytevector))
  (doc 'description "Compute the versioned address of a block. The SHA-256 hash is computed over the canonical serialization, then prefixed with a version byte.")
  (doc 'export #t)
  (let* ([hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
        (bytevector-u8-set! address 0 address-version)
        (bytevector-copy! hash 0 address 1 hash-size)
        address))