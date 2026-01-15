;;; lattice/crypto/sha512.ss — SHA-512 implementation (FIPS 180-4)
;;;
;;; SHA-512 hash function producing 64-byte (512-bit) digests.
;;;
;;; sha512 : Bytevector → Bytevector (64 bytes)
;;; sha512-hex : Bytevector → String
;;;
;;; Uses 64-bit arithmetic with explicit masking since Chez Scheme
;;; fixnums are 61 bits. Follows FIPS 180-4 specification exactly.
;;;
;;; TIER: 0 (no lattice dependencies)

;;; ============================================================
;;; 64-bit Arithmetic (mod 2^64)
;;; ============================================================

;;; u64-mask : used for masking to 64 bits
(define u64-mask #xFFFFFFFFFFFFFFFF)

;;; u64 : Integer → Integer
;;; Mask to 64-bit unsigned integer.
(define (u64 x)
  (bitwise-and x u64-mask))

;;; u64+ : Integer ... → Integer
;;; Add with 64-bit overflow.
(define (u64+ . args)
  (u64 (apply + args)))

;;; rotr64 : Integer × Integer → Integer
;;; 64-bit right rotation.
(define (rotr64 x n)
  (u64 (bitwise-ior
         (bitwise-arithmetic-shift-right x n)
         (bitwise-arithmetic-shift-left x (- 64 n)))))

;;; shr64 : Integer × Integer → Integer
;;; 64-bit right shift.
(define (shr64 x n)
  (bitwise-arithmetic-shift-right x n))

;;; ============================================================
;;; SHA-512 Functions
;;; ============================================================

;;; Ch : Integer × Integer × Integer → Integer
(define (Ch x y z)
  (bitwise-xor (bitwise-and x y)
               (bitwise-and (bitwise-not x) z)))

;;; Maj : Integer × Integer × Integer → Integer
(define (Maj x y z)
  (bitwise-xor (bitwise-and x y)
               (bitwise-xor (bitwise-and x z)
                            (bitwise-and y z))))

;;; Sigma0-512 : Integer → Integer
(define (Sigma0-512 x)
  (u64 (bitwise-xor (rotr64 x 28)
                    (bitwise-xor (rotr64 x 34)
                                 (rotr64 x 39)))))

;;; Sigma1-512 : Integer → Integer
(define (Sigma1-512 x)
  (u64 (bitwise-xor (rotr64 x 14)
                    (bitwise-xor (rotr64 x 18)
                                 (rotr64 x 41)))))

;;; sigma0-512 : Integer → Integer
(define (sigma0-512 x)
  (u64 (bitwise-xor (rotr64 x 1)
                    (bitwise-xor (rotr64 x 8)
                                 (shr64 x 7)))))

;;; sigma1-512 : Integer → Integer
(define (sigma1-512 x)
  (u64 (bitwise-xor (rotr64 x 19)
                    (bitwise-xor (rotr64 x 61)
                                 (shr64 x 6)))))

;;; ============================================================
;;; Constants
;;; ============================================================

;;; Initial hash values (first 64 bits of fractional parts of
;;; square roots of first 8 primes)
(define H-init-512
  (vector #x6a09e667f3bcc908 #xbb67ae8584caa73b
          #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
          #x510e527fade682d1 #x9b05688c2b3e6c1f
          #x1f83d9abfb41bd6b #x5be0cd19137e2179))

;;; Round constants (first 64 bits of fractional parts of
;;; cube roots of first 80 primes)
(define K-512
  (vector
    #x428a2f98d728ae22 #x7137449123ef65cd #xb5c0fbcfec4d3b2f #xe9b5dba58189dbbc
    #x3956c25bf348b538 #x59f111f1b605d019 #x923f82a4af194f9b #xab1c5ed5da6d8118
    #xd807aa98a3030242 #x12835b0145706fbe #x243185be4ee4b28c #x550c7dc3d5ffb4e2
    #x72be5d74f27b896f #x80deb1fe3b1696b1 #x9bdc06a725c71235 #xc19bf174cf692694
    #xe49b69c19ef14ad2 #xefbe4786384f25e3 #x0fc19dc68b8cd5b5 #x240ca1cc77ac9c65
    #x2de92c6f592b0275 #x4a7484aa6ea6e483 #x5cb0a9dcbd41fbd4 #x76f988da831153b5
    #x983e5152ee66dfab #xa831c66d2db43210 #xb00327c898fb213f #xbf597fc7beef0ee4
    #xc6e00bf33da88fc2 #xd5a79147930aa725 #x06ca6351e003826f #x142929670a0e6e70
    #x27b70a8546d22ffc #x2e1b21385c26c926 #x4d2c6dfc5ac42aed #x53380d139d95b3df
    #x650a73548baf63de #x766a0abb3c77b2a8 #x81c2c92e47edaee6 #x92722c851482353b
    #xa2bfe8a14cf10364 #xa81a664bbc423001 #xc24b8b70d0f89791 #xc76c51a30654be30
    #xd192e819d6ef5218 #xd69906245565a910 #xf40e35855771202a #x106aa07032bbd1b8
    #x19a4c116b8d2d0c8 #x1e376c085141ab53 #x2748774cdf8eeb99 #x34b0bcb5e19b48a8
    #x391c0cb3c5c95a63 #x4ed8aa4ae3418acb #x5b9cca4f7763e373 #x682e6ff3d6b2b8a3
    #x748f82ee5defb2fc #x78a5636f43172f60 #x84c87814a1f0ab72 #x8cc702081a6439ec
    #x90befffa23631e28 #xa4506cebde82bde9 #xbef9a3f7b2c67915 #xc67178f2e372532b
    #xca273eceea26619c #xd186b8c721c0c207 #xeada7dd6cde0eb1e #xf57d4f7fee6ed178
    #x06f067aa72176fba #x0a637dc5a2c898a6 #x113f9804bef90dae #x1b710b35131c471b
    #x28db77f523047d84 #x32caab7b40c72493 #x3c9ebe0a15c9bebc #x431d67c49c100d4c
    #x4cc5d4becb3e42b6 #x597f299cfc657e2a #x5fcb6fab3ad6faec #x6c44198c4a475817))

;;; ============================================================
;;; Message Padding
;;; ============================================================

;;; pad-message-512 : Bytevector → Bytevector
;;; Pad to multiple of 128 bytes (1024 bits).
;;; Append 1 bit, zeros, then 128-bit big-endian length.
(define (pad-message-512 msg)
  (let* ([len (bytevector-length msg)]
         [bit-len (* 8 len)]
         ;; Need at least 17 bytes: 1 for 0x80, 16 for length
         ;; Pad to next multiple of 128
         [padded-len (let ([rem (modulo (+ len 17) 128)])
                          (if (= rem 0)
                              (+ len 17)
                              (+ len 17 (- 128 rem))))]
         [result (make-bytevector padded-len 0)])
        ;; Copy message
        (bytevector-copy! msg 0 result 0 len)
        ;; Append 0x80
        (bytevector-u8-set! result len #x80)
        ;; Append length as 128-bit big-endian (we only use lower 64 bits)
        ;; Upper 64 bits are zero (for messages < 2^64 bits)
        (bytevector-u64-set! result (- padded-len 8) bit-len 'big)
        result))

;;; ============================================================
;;; Message Schedule
;;; ============================================================

;;; make-schedule-512 : Bytevector × Integer → Vector
;;; Create 80-word message schedule from 128-byte block at offset.
(define (make-schedule-512 msg offset)
  (let ([W (make-vector 80)])
       ;; W[0..15]: 16 64-bit words from block (big-endian)
       (do ([i 0 (+ i 1)])
           ((= i 16))
           (vector-set! W i (bytevector-u64-ref msg (+ offset (* i 8)) 'big)))
       ;; W[16..79]: extended schedule
       (do ([i 16 (+ i 1)])
           ((= i 80))
           (vector-set! W i
                        (u64+ (sigma1-512 (vector-ref W (- i 2)))
                              (vector-ref W (- i 7))
                              (sigma0-512 (vector-ref W (- i 15)))
                              (vector-ref W (- i 16)))))
       W))

;;; ============================================================
;;; Compression
;;; ============================================================

;;; compress-512 : Vector × Vector → Vector
;;; One round of compression (H, W) → H'
(define (compress-512 H W)
  (let ([a (vector-ref H 0)]
        [b (vector-ref H 1)]
        [c (vector-ref H 2)]
        [d (vector-ref H 3)]
        [e (vector-ref H 4)]
        [f (vector-ref H 5)]
        [g (vector-ref H 6)]
        [h (vector-ref H 7)])
       ;; 80 rounds
       (do ([i 0 (+ i 1)])
           ((= i 80))
           (let* ([T1 (u64+ h (Sigma1-512 e) (u64 (Ch e f g))
                            (vector-ref K-512 i) (vector-ref W i))]
                  [T2 (u64+ (Sigma0-512 a) (u64 (Maj a b c)))])
                 (set! h g)
                 (set! g f)
                 (set! f e)
                 (set! e (u64+ d T1))
                 (set! d c)
                 (set! c b)
                 (set! b a)
                 (set! a (u64+ T1 T2))))
       ;; Add to hash state
       (vector (u64+ (vector-ref H 0) a)
               (u64+ (vector-ref H 1) b)
               (u64+ (vector-ref H 2) c)
               (u64+ (vector-ref H 3) d)
               (u64+ (vector-ref H 4) e)
               (u64+ (vector-ref H 5) f)
               (u64+ (vector-ref H 6) g)
               (u64+ (vector-ref H 7) h))))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

;;; sha512 : Bytevector → Bytevector
;;; Compute SHA-512 hash (64 bytes).
(define (sha512 msg)
  (let* ([padded (pad-message-512 msg)]
         [num-blocks (quotient (bytevector-length padded) 128)]
         [H (vector-copy H-init-512)])
        ;; Process each 128-byte block
        (do ([i 0 (+ i 1)])
            ((= i num-blocks))
            (let ([W (make-schedule-512 padded (* i 128))])
                 (set! H (compress-512 H W))))
        ;; Convert final hash to bytevector
        (let ([result (make-bytevector 64)])
             (do ([i 0 (+ i 1)])
                 ((= i 8))
                 (bytevector-u64-set! result (* i 8) (vector-ref H i) 'big))
             result)))

;;; sha512-hex : Bytevector → String
;;; Convenience: return hash as lowercase hexadecimal string.
(define (sha512-hex msg)
  (let ([hash (sha512 msg)]
        [hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota 64)))))

;;; iota : Integer → (List Integer)
(define (iota n)
  (let loop ([i 0] [acc '()])
       (if (= i n)
           (reverse acc)
           (loop (+ i 1) (cons i acc)))))

;;; ============================================================
;;; SHA-384 (truncated SHA-512)
;;; ============================================================

;;; SHA-384 uses SHA-512 with different initial values and truncates to 48 bytes
(define H-init-384
  (vector #xcbbb9d5dc1059ed8 #x629a292a367cd507
          #x9159015a3070dd17 #x152fecd8f70e5939
          #x67332667ffc00b31 #x8eb44a8768581511
          #xdb0c2e0d64f98fa7 #x47b5481dbefa4fa4))

;;; sha384 : Bytevector → Bytevector
;;; Compute SHA-384 hash (48 bytes).
(define (sha384 msg)
  (let* ([padded (pad-message-512 msg)]
         [num-blocks (quotient (bytevector-length padded) 128)]
         [H (vector-copy H-init-384)])
        ;; Process each 128-byte block
        (do ([i 0 (+ i 1)])
            ((= i num-blocks))
            (let ([W (make-schedule-512 padded (* i 128))])
                 (set! H (compress-512 H W))))
        ;; Convert final hash to bytevector (truncate to 48 bytes)
        (let ([result (make-bytevector 48)])
             (do ([i 0 (+ i 1)])
                 ((= i 6))
                 (bytevector-u64-set! result (* i 8) (vector-ref H i) 'big))
             result)))

;;; sha384-hex : Bytevector → String
(define (sha384-hex msg)
  (let ([hash (sha384 msg)]
        [hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota 48)))))
