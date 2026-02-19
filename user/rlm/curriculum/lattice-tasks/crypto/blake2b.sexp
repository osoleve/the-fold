;;; blake2b.sexp — Development tasks for lattice/crypto/blake2b.ss
;;;
;;; Module: blake2b (tier 0, no dependencies)
;;; 211 lines, ~8 exported functions
;;; BLAKE2b cryptographic hash: 64-bit modular arithmetic, quarter-round
;;; mixing function (G), 12-round compression, configurable output length,
;;; optional keyed MAC mode. Faster than SHA-3 with equivalent security.
;;;
;;; Git history:
;;;   f64d84d0 feat(crypto): Implement SHA-512, BLAKE2b, and HMAC
;;;   00b37b24 fix(crypto): Correct BLAKE2b counter initialization for keyed hashes
;;;   fcf2741b feat(module): migrate lattice/crypto and deps from load to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement u64+
  ;; ──────────────────────────────────────────────
  ;; Variadic modular 64-bit addition — used throughout BLAKE2b.

  (task
    (id "crypto/blake2b/u64+-impl")
    (module blake2b)
    (tier 0)
    (type implement)
    (description "Implement u64+: variadic modular addition in 64-bit unsigned arithmetic. This function takes any number of arguments (using Scheme's dotted-pair rest parameter syntax), sums them using the built-in + via apply, then masks the result to 64 bits using bitwise-and with the constant u64-mask = #xFFFFFFFFFFFFFFFF. This is needed because Scheme integers are arbitrary precision, but BLAKE2b operates on 64-bit words where overflow must wrap around. For example: u64+(MAX, 1) = 0, u64+(MAX, MAX) = MAX-1.")
    (context "Available: bitwise-and, apply, +. The constant u64-mask = #xFFFFFFFFFFFFFFFF is defined. The helper (define (u64 x) (bitwise-and x u64-mask)) is also defined.")
    (before "(define (u64+ . args)
  ;; TODO: sum all args, then mask to 64 bits
  0)")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Basic addition
(assert-equal 6 (u64+ 1 2 3))"
                 ";; Overflow wraps to zero
(assert-equal 0 (u64+ #xFFFFFFFFFFFFFFFF 1))"
                 ";; Double overflow
(assert-equal (- #xFFFFFFFFFFFFFFFF 1) (u64+ #xFFFFFFFFFFFFFFFF #xFFFFFFFFFFFFFFFF))"
                 ";; Single argument
(assert-equal 42 (u64+ 42))"
                 ";; Two large values that overflow
(assert-equal 0 (u64+ #x8000000000000000 #x8000000000000000))"))
    (available-modules ())
    (hints ("Use Scheme's rest parameter: (define (u64+ . args) ...)"
            "Sum with (apply + args) — this handles any number of arguments"
            "Mask: (u64 (apply + args)) or equivalently (bitwise-and (apply + args) u64-mask)"
            "That's the entire implementation — one expression"))
    (reference-solution "(define (u64+ . args)
  (u64 (apply + args)))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement hash->hex
  ;; ──────────────────────────────────────────────
  ;; Convert a hash bytevector to a hexadecimal string.

  (task
    (id "crypto/blake2b/hash-to-hex-impl")
    (module blake2b)
    (tier 0)
    (type implement)
    (description "Implement hash->hex: convert a bytevector (hash digest) to its lowercase hexadecimal string representation. For each byte in the bytevector, emit two hex characters: the high nibble (byte divided by 16) and the low nibble (byte modulo 16). Use the lookup string \"0123456789abcdef\" with string-ref to convert nibble values (0-15) to hex characters. Build the result by mapping over indices 0 to n-1, producing a two-character string for each byte, then joining with string-append via apply. For example: #vu8(171 205) becomes \"abcd\" because 171=0xAB and 205=0xCD.")
    (context "Available: bytevector-length, bytevector-u8-ref, string-ref, string, quotient, modulo, map, apply, string-append, iota (generates list 0..n-1).")
    (before "(define (hash->hex hash)
  ;; TODO: convert each byte to two hex chars, concatenate
  \"\")")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Single zero byte
(assert-equal \"00\" (hash->hex #vu8(0)))"
                 ";; Single max byte
(assert-equal \"ff\" (hash->hex #vu8(255)))"
                 ";; Two bytes forming \"abcd\"
(assert-equal \"abcd\" (hash->hex #vu8(171 205)))"
                 ";; Three bytes
(assert-equal \"010203\" (hash->hex #vu8(1 2 3)))"
                 ";; Empty bytevector
(assert-equal \"\" (hash->hex #vu8()))"))
    (available-modules ())
    (hints ("Define hex-chars = \"0123456789abcdef\" as a lookup table"
            "For each index i, get byte b = (bytevector-u8-ref hash i)"
            "High nibble: (string-ref hex-chars (quotient b 16))"
            "Low nibble: (string-ref hex-chars (modulo b 16))"
            "Combine with (string high-char low-char) to make a 2-char string"
            "Use (apply string-append (map ... (iota n))) to join all pairs"))
    (reference-solution "(define (hash->hex hash)
  (let ([hex-chars \"0123456789abcdef\"])
    (apply string-append
           (map (lambda (i)
                  (let ([b (bytevector-u8-ref hash i)])
                    (string
                      (string-ref hex-chars (quotient b 16))
                      (string-ref hex-chars (modulo b 16)))))
                (iota (bytevector-length hash))))))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement bytevector-append
  ;; ──────────────────────────────────────────────
  ;; Concatenate two bytevectors — utility for key padding.

  (task
    (id "crypto/blake2b/bytevector-append-impl")
    (module blake2b)
    (tier 0)
    (type implement)
    (description "Implement bytevector-append: concatenate two bytevectors into a new one. Create a result bytevector of length len1+len2, copy all bytes from bv1 into positions 0..len1-1, then copy all bytes from bv2 into positions len1..len1+len2-1. Use bytevector-copy! which takes (src src-start dst dst-start count). This is used in BLAKE2b for prepending a padded key to the message before compression.")
    (context "Available: bytevector-length, make-bytevector, bytevector-copy! (src src-start dst dst-start count). Pure bytevector manipulation.")
    (before "(define (bytevector-append bv1 bv2)
  ;; TODO: allocate result, copy bv1 then bv2
  #vu8())")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Basic concatenation
(assert-equal #vu8(1 2 3 4) (bytevector-append #vu8(1 2) #vu8(3 4)))"
                 ";; Left empty
(assert-equal #vu8(5 6) (bytevector-append #vu8() #vu8(5 6)))"
                 ";; Right empty
(assert-equal #vu8(7) (bytevector-append #vu8(7) #vu8()))"
                 ";; Result length is sum of input lengths
(assert-equal 6 (bytevector-length (bytevector-append #vu8(1 2 3) #vu8(4 5 6))))"
                 ";; Both empty
(assert-equal #vu8() (bytevector-append #vu8() #vu8()))"))
    (available-modules ())
    (hints ("Get len1 = (bytevector-length bv1), len2 = (bytevector-length bv2)"
            "Allocate: (make-bytevector (+ len1 len2))"
            "Copy first: (bytevector-copy! bv1 0 result 0 len1)"
            "Copy second: (bytevector-copy! bv2 0 result len1 len2)"
            "Return result"))
    (reference-solution "(define (bytevector-append bv1 bv2)
  (let* ([len1 (bytevector-length bv1)]
         [len2 (bytevector-length bv2)]
         [result (make-bytevector (+ len1 len2))])
    (bytevector-copy! bv1 0 result 0 len1)
    (bytevector-copy! bv2 0 result len1 len2)
    result))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement G mixing function
  ;; ──────────────────────────────────────────────
  ;; The quarter-round mixing function — the heart of BLAKE2b.

  (task
    (id "crypto/blake2b/g-mixing-impl")
    (module blake2b)
    (tier 0)
    (type implement)
    (description "Implement G: the BLAKE2b quarter-round mixing function. G takes a working vector v and four indices a,b,c,d plus two message words x,y. It performs four XOR-rotate-add cycles that mix the four state words. The steps are: (1) va = va + vb + x, (2) vd = ROTR(vd XOR va, 32), (3) vc = vc + vd, (4) vb = ROTR(vb XOR vc, 24), (5) va = va + vb + y, (6) vd = ROTR(vd XOR va, 16), (7) vc = vc + vd, (8) vb = ROTR(vb XOR vc, 63). All additions are modular 64-bit (use u64+). All rotations are 64-bit right rotations (use rotr64). The function modifies v in place using vector-set!.")
    (context "Available: u64+ (modular 64-bit addition), rotr64 (64-bit right rotation), bitwise-xor, vector-ref, vector-set!. The rotation amounts 32, 24, 16, 63 are specific to BLAKE2b (different from SHA-512 or ChaCha).")
    (before "(define (G v a b c d x y)
  ;; TODO: four XOR-rotate-add cycles modifying v[a], v[b], v[c], v[d]
  (void))")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; G modifies all four positions
(let ([v (make-vector 16 0)])
  (vector-set! v 0 1) (vector-set! v 4 2)
  (vector-set! v 8 3) (vector-set! v 12 4)
  (G v 0 4 8 12 100 200)
  (assert-true (not (= (vector-ref v 0) 1)))
  (assert-true (not (= (vector-ref v 4) 2)))
  (assert-true (not (= (vector-ref v 8) 3)))
  (assert-true (not (= (vector-ref v 12) 4))))"
                 ";; All results are within 64-bit range
(let ([v (make-vector 16 #xFFFFFFFFFFFFFFFF)])
  (G v 0 1 2 3 #xDEADBEEF #xCAFEBABE)
  (assert-true (<= (vector-ref v 0) #xFFFFFFFFFFFFFFFF))
  (assert-true (<= (vector-ref v 1) #xFFFFFFFFFFFFFFFF))
  (assert-true (<= (vector-ref v 2) #xFFFFFFFFFFFFFFFF))
  (assert-true (<= (vector-ref v 3) #xFFFFFFFFFFFFFFFF)))"
                 ";; Deterministic: same inputs produce same outputs
(let ([v1 (make-vector 16 42)]
      [v2 (make-vector 16 42)])
  (G v1 0 4 8 12 7 11)
  (G v2 0 4 8 12 7 11)
  (assert-equal (vector-ref v1 0) (vector-ref v2 0))
  (assert-equal (vector-ref v1 4) (vector-ref v2 4)))"))
    (available-modules ())
    (hints ("Read all four values into local variables: va, vb, vc, vd"
            "Step 1: (set! va (u64+ va vb x))"
            "Step 2: (set! vd (rotr64 (bitwise-xor vd va) 32))"
            "Continue the pattern: add → XOR+rotate → add → XOR+rotate"
            "Second half uses y instead of x, with rotations 16 and 63"
            "Write all four values back with vector-set! at the end"))
    (reference-solution "(define (G v a b c d x y)
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
    (vector-set! v d vd)))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 3))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement blake2b-compress
  ;; ──────────────────────────────────────────────
  ;; The full compression function — 12 rounds of column/diagonal mixing.

  (task
    (id "crypto/blake2b/compress-impl")
    (module blake2b)
    (tier 0)
    (type implement)
    (description "Implement blake2b-compress: the BLAKE2b compression function. It processes one 128-byte block, updating the 8-word state vector h. Steps: (1) Initialize a 16-word working vector v: v[0..7] = h[0..7], v[8..15] = IV[0..7]. (2) XOR the byte counter t into v[12] (low 64 bits) and v[13] (high 64 bits via shift-right by 64). (3) If this is the last block, invert v[14] by XOR with u64-mask. (4) Extract 16 message words m[0..15] from the block as little-endian 64-bit integers using bytevector-u64-ref with offset. (5) Run 12 rounds of mixing: each round uses SIGMA[round] to permute the message word indices, then calls G four times for columns (0,4,8,12), (1,5,9,13), (2,6,10,14), (3,7,11,15) and four times for diagonals (0,5,10,15), (1,6,11,12), (2,7,8,13), (3,4,9,14). (6) Finalize: h[i] ^= v[i] ^ v[i+8] for i in 0..7.")
    (context "Available: G (mixing function), IV (8-word initialization vector), SIGMA (12x16 permutation table), u64-mask, make-vector, vector-ref, vector-set!, vector-copy, bitwise-xor, bitwise-arithmetic-shift-right, bytevector-u64-ref with 'little endianness. Modifies h in place.")
    (before "(define (blake2b-compress h block offset t last)
  ;; TODO: init working vector, XOR counter/flags, 12 rounds of G, finalize
  (void))")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Compression modifies h
(let ([h (vector-copy IV)]
      [block (make-bytevector 128 0)])
  (let ([h0-before (vector-ref h 0)])
    (blake2b-compress h block 0 0 #t)
    (assert-true (not (= (vector-ref h 0) h0-before)))))"
                 ";; Deterministic: same inputs → same output
(let ([h1 (vector-copy IV)]
      [h2 (vector-copy IV)]
      [block (make-bytevector 128 42)])
  (blake2b-compress h1 block 0 128 #f)
  (blake2b-compress h2 block 0 128 #f)
  (assert-equal (vector-ref h1 0) (vector-ref h2 0))
  (assert-equal (vector-ref h1 7) (vector-ref h2 7)))"))
    (available-modules ())
    (hints ("Initialize v[0..7] from h, v[8..15] from IV using do loops"
            "Counter XOR: (vector-set! v 12 (bitwise-xor (vector-ref v 12) (u64 t)))"
            "High counter: (vector-set! v 13 (bitwise-xor (vector-ref v 13) (bitwise-arithmetic-shift-right t 64)))"
            "Last block flag: (when last (vector-set! v 14 (bitwise-xor (vector-ref v 14) u64-mask)))"
            "Extract message words: m[i] = (bytevector-u64-ref block (+ offset (* i 8)) 'little)"
            "Each round: 4 column G calls then 4 diagonal G calls, with SIGMA-permuted message indices"
            "Finalize: h[i] = h[i] XOR v[i] XOR v[i+8]"))
    (reference-solution "(define (blake2b-compress h block offset t last)
  (let ([v (make-vector 16)])
    (do ([i 0 (+ i 1)]) ((= i 8))
      (vector-set! v i (vector-ref h i)))
    (do ([i 0 (+ i 1)]) ((= i 8))
      (vector-set! v (+ i 8) (vector-ref IV i)))
    (vector-set! v 12 (bitwise-xor (vector-ref v 12) (u64 t)))
    (vector-set! v 13 (bitwise-xor (vector-ref v 13) (bitwise-arithmetic-shift-right t 64)))
    (when last
      (vector-set! v 14 (bitwise-xor (vector-ref v 14) u64-mask)))
    (let ([m (make-vector 16)])
      (do ([i 0 (+ i 1)]) ((= i 16))
        (vector-set! m i (bytevector-u64-ref block (+ offset (* i 8)) 'little)))
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
      (do ([i 0 (+ i 1)]) ((= i 8))
        (vector-set! h i (bitwise-xor (vector-ref h i)
                                       (bitwise-xor (vector-ref v i)
                                                    (vector-ref v (+ i 8)))))))))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 4))

) ;; end tasks
