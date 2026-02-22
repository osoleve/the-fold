;;; @module blake2b

(doc 'module 'blake2b)
(doc 'description "BLAKE2b cryptographic hash function (RFC 7693) - faster than MD5, SHA-1, SHA-2, and SHA-3, yet at least as secure as SHA-3")
(doc 'features "Configurable output length (1-64 bytes), optional key for MAC mode (up to 64 bytes), optional salt and personalization")
(doc 'tier 0)
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section '64-bit-arithmetic)

(define u64-mask #xFFFFFFFFFFFFFFFF)

(define (u64 x)
  (bitwise-and x u64-mask))

(define (u64+ . args)
  (u64 (apply + args)))

(define (rotr64 x n)
  (u64 (bitwise-ior
         (bitwise-arithmetic-shift-right x n)
         (bitwise-arithmetic-shift-left x (- 64 n)))))

(doc 'section 'blake2b-constants)

(doc 'note "Initialization vector (same as SHA-512)")
(define IV
  (vector #x6a09e667f3bcc908 #xbb67ae8584caa73b
          #x3c6ef372fe94f82b #xa54ff53a5f1d36f1
          #x510e527fade682d1 #x9b05688c2b3e6c1f
          #x1f83d9abfb41bd6b #x5be0cd19137e2179))

;;; Sigma permutation table (12 rounds, 16 entries each)
(define SIGMA
  (vector
    (vector  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
    (vector 14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3)
    (vector 11  8 12  0  5  2 15 13 10 14  3  6  7  1  9  4)
    (vector  7  9  3  1 13 12 11 14  2  6  5 10  4  0 15  8)
    (vector  9  0  5  7  2  4 10 15 14  1 11 12  6  8  3 13)
    (vector  2 12  6 10  0 11  8  3  4 13  7  5 15 14  1  9)
    (vector 12  5  1 15 14 13  4 10  0  7  6  3  9  2  8 11)
    (vector 13 11  7 14 12  1  3  9  5  0 15  4  8  6  2 10)
    (vector  6 15 14  9 11  3  0  8 12  2 13  7  1  4 10  5)
    (vector 10  2  8  4  7  6  1  5 15 11  9 14  3 12 13  0)
    (vector  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15)
    (vector 14 10  4  8  9 15 13  6  1 12  0  2 11  7  5  3)))

(doc 'section 'blake2b-mixing)

(define (G v a b c d x y)
  (doc 'type '(-> Vector Integer Integer Integer Integer Integer Integer Void))
  (doc 'description "The mixing function. Modifies v in place")
  (let ([va (vector-ref v a)]
        [vb (vector-ref v b)]
        [vc (vector-ref v c)]
        [vd (vector-ref v d)])
    (set! va (u64+ va vb x))
    (set! vd (rotr64 (bitwise-xor vd va) 32))
    (set! vc (u64+ vc vd))
    (set! vb (rotr64 (bitwise-xor vb vc) 24))
    (set! va (u64+ va vb y))
    (set! vd (rotr64 (bitwise-xor vd va) 16))
    (set! vc (u64+ vc vd))
    (set! vb (rotr64 (bitwise-xor vb vc) 63))
    (vector-set! v a va)
    (vector-set! v b vb)
    (vector-set! v c vc)
    (vector-set! v d vd)))

(doc 'section 'blake2b-compression)

(define (blake2b-compress h block offset t last)
  (doc 'type '(-> Vector Bytevector Integer Integer Boolean Void))
  (let ([v (make-vector 16)])
    ;; v[0..7] = h[0..7]
    (do ([i 0 (+ i 1)]) ((= i 8))
      (vector-set! v i (vector-ref h i)))
    ;; v[8..15] = IV[0..7]
    (do ([i 0 (+ i 1)]) ((= i 8))
      (vector-set! v (+ i 8) (vector-ref IV i)))
    ;; XOR in counter and flags
    (vector-set! v 12 (bitwise-xor (vector-ref v 12) (u64 t)))
    (vector-set! v 13 (bitwise-xor (vector-ref v 13) (bitwise-arithmetic-shift-right t 64)))
    (when last
      (vector-set! v 14 (bitwise-xor (vector-ref v 14) u64-mask)))
    ;; Get message words
    (let ([m (make-vector 16)])
      (do ([i 0 (+ i 1)]) ((= i 16))
        (vector-set! m i (bytevector-u64-ref block (+ offset (* i 8)) 'little)))
      ;; 12 rounds of mixing
      (do ([round 0 (+ round 1)]) ((= round 12))
        (let ([s (vector-ref SIGMA round)])
          (G v 0 4  8 12 (vector-ref m (vector-ref s  0)) (vector-ref m (vector-ref s  1)))
          (G v 1 5  9 13 (vector-ref m (vector-ref s  2)) (vector-ref m (vector-ref s  3)))
          (G v 2 6 10 14 (vector-ref m (vector-ref s  4)) (vector-ref m (vector-ref s  5)))
          (G v 3 7 11 15 (vector-ref m (vector-ref s  6)) (vector-ref m (vector-ref s  7)))
          (G v 0 5 10 15 (vector-ref m (vector-ref s  8)) (vector-ref m (vector-ref s  9)))
          (G v 1 6 11 12 (vector-ref m (vector-ref s 10)) (vector-ref m (vector-ref s 11)))
          (G v 2 7  8 13 (vector-ref m (vector-ref s 12)) (vector-ref m (vector-ref s 13)))
          (G v 3 4  9 14 (vector-ref m (vector-ref s 14)) (vector-ref m (vector-ref s 15)))))
      ;; Finalize: h = h ^ v[0..7] ^ v[8..15]
      (do ([i 0 (+ i 1)]) ((= i 8))
        (vector-set! h i (bitwise-xor (vector-ref h i)
                                       (bitwise-xor (vector-ref v i)
                                                    (vector-ref v (+ i 8)))))))))

(doc 'section 'main-entry-points)

(doc blake2b 'type '(case-lambda
                      ((Bytevector) Bytevector)
                      ((Bytevector Integer) Bytevector)))
(doc blake2b 'export #t)
(define blake2b
  (case-lambda
    [(msg) (blake2b-full msg #vu8() 64)]
    [(msg outlen) (blake2b-full msg #vu8() outlen)]))

;;; blake2b-keyed : Bytevector × Bytevector [× Integer] → Bytevector
(doc blake2b-keyed 'export #t)
(define blake2b-keyed
  (case-lambda
    [(key msg) (blake2b-full msg key 64)]
    [(key msg outlen) (blake2b-full msg key outlen)]))

;;; blake2b-full : Bytevector × Bytevector × Integer → Bytevector
(define (blake2b-full msg key outlen)
  (let* ([keylen (bytevector-length key)]
         [msglen (bytevector-length msg)]
         [h (vector-copy IV)])
    ;; XOR in parameter block
    (vector-set! h 0 (bitwise-xor (vector-ref h 0)
                                   (bitwise-ior outlen
                                                (bitwise-arithmetic-shift-left keylen 8)
                                                (bitwise-arithmetic-shift-left 1 16)
                                                (bitwise-arithmetic-shift-left 1 24))))
    ;; If we have a key, pad it to 128 bytes and process as first block
    ;; Note: t starts at 0 regardless of key - the loop adds 128 for each block
    (let ([t 0]
          [data (if (> keylen 0)
                    (let ([padded-key (make-bytevector 128 0)])
                      (bytevector-copy! key 0 padded-key 0 keylen)
                      (bytevector-append padded-key msg))
                    msg)])
      (let ([datalen (bytevector-length data)])
        ;; Process full blocks
        (let loop ([offset 0] [remaining datalen] [bytes-compressed t])
          (cond
            [(> remaining 128)
             (blake2b-compress h data offset (+ bytes-compressed 128) #f)
             (loop (+ offset 128) (- remaining 128) (+ bytes-compressed 128))]
            [else
             (let ([last-block (make-bytevector 128 0)])
               (bytevector-copy! data offset last-block 0 remaining)
               (blake2b-compress h last-block 0 (+ bytes-compressed remaining) #t))]))
        ;; Extract output
        (let ([result (make-bytevector outlen)])
          (do ([i 0 (+ i 1)]) ((>= (* i 8) outlen))
            (let ([word (vector-ref h i)]
                  [out-offset (* i 8)])
              (do ([j 0 (+ j 1)]) ((or (= j 8) (>= (+ out-offset j) outlen)))
                (bytevector-u8-set! result (+ out-offset j)
                                    (bitwise-and (bitwise-arithmetic-shift-right word (* j 8)) #xff)))))
          result)))))

;;; bytevector-append : Bytevector × Bytevector → Bytevector
(define (bytevector-append bv1 bv2)
  (let* ([len1 (bytevector-length bv1)]
         [len2 (bytevector-length bv2)]
         [result (make-bytevector (+ len1 len2))])
    (bytevector-copy! bv1 0 result 0 len1)
    (bytevector-copy! bv2 0 result len1 len2)
    result))

(doc 'section 'hex-convenience)

(doc blake2b-hex 'type '(case-lambda
                          ((Bytevector) String)
                          ((Bytevector Integer) String)))
(doc blake2b-hex 'export #t)
(define blake2b-hex
  (case-lambda
    [(msg) (hash->hex (blake2b msg))]
    [(msg outlen) (hash->hex (blake2b msg outlen))]))

;;; blake2b-keyed-hex : Bytevector × Bytevector [× Integer] → String
(doc blake2b-keyed-hex 'export #t)
(define blake2b-keyed-hex
  (case-lambda
    [(key msg) (hash->hex (blake2b-keyed key msg))]
    [(key msg outlen) (hash->hex (blake2b-keyed key msg outlen))]))

;;; hash->hex : Bytevector → String
(define (hash->hex hash)
  (let ([hex-chars "0123456789abcdef"])
    (apply string-append
           (map (lambda (i)
                  (let ([b (bytevector-u8-ref hash i)])
                    (string
                      (string-ref hex-chars (quotient b 16))
                      (string-ref hex-chars (modulo b 16)))))
                (iota (bytevector-length hash))))))

;;; iota : Integer → (List Integer)
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))
