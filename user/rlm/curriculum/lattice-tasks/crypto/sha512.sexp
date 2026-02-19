;;; sha512.sexp — Development tasks for lattice/crypto/sha512.ss
;;;
;;; Module: sha512 (tier 0, no dependencies)
;;; 273 lines, ~12 exported functions
;;; SHA-512 hash function: 64-bit arithmetic helpers, FIPS 180-4 mixing
;;; functions, Merkle-Damgård padding, message schedule, compression,
;;; entry points (sha512, sha384, hex variants).
;;;
;;; Git history:
;;;   f64d84d0 feat(crypto): Implement SHA-512, BLAKE2b, and HMAC
;;;   372ad8e7 docs(lattice): Convert multiple directories to doc forms
;;;   fcf2741b feat(module): migrate lattice/crypto and deps from load to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement rotr64
  ;; ──────────────────────────────────────────────
  ;; 64-bit right rotation — the fundamental building block of SHA-512.

  (task
    (id "crypto/sha512/rotr64-impl")
    (module sha512)
    (tier 0)
    (type implement)
    (description "Implement rotr64: rotate a 64-bit unsigned integer x right by n positions. A right rotation by n means the low n bits wrap around to the high end. The algorithm: (1) shift x right by n positions, (2) shift x left by (64-n) positions, (3) bitwise-OR the two results, (4) mask to 64 bits. In Scheme, use bitwise-arithmetic-shift-right for right shift, bitwise-arithmetic-shift-left for left shift, bitwise-ior for OR, and bitwise-and with #xFFFFFFFFFFFFFFFF to mask. The mask is needed because Scheme integers are arbitrary precision — without it, the left shift would produce numbers larger than 64 bits.")
    (context "Available: bitwise-arithmetic-shift-right, bitwise-arithmetic-shift-left, bitwise-ior, bitwise-and, -. The constant u64-mask = #xFFFFFFFFFFFFFFFF is defined. This is pure integer arithmetic.")
    (before "(define (rotr64 x n)
  (doc 'type '(-> Integer Integer Integer))
  (doc 'description \"64-bit right rotation\")
  ;; TODO: shift right by n, shift left by (64-n), OR, mask to 64 bits
  0)")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Rotate 1 right by 1: bit 0 wraps to bit 63
(assert-equal #x8000000000000000 (rotr64 1 1))"
                 ";; Rotate #xFF right by 4: low nibble wraps to high end
(assert-equal #xF00000000000000F (rotr64 #xFF 4))"
                 ";; Rotate MSB right by 1: becomes second-highest bit
(assert-equal #x4000000000000000 (rotr64 #x8000000000000000 1))"
                 ";; Rotate by 0 is identity
(assert-equal #xDEADBEEF (rotr64 #xDEADBEEF 0))"
                 ";; Full rotation (64) is identity
(assert-equal #xDEADBEEF (rotr64 #xDEADBEEF 64))"))
    (available-modules ())
    (hints ("The key insight: rotation = (right-shift OR left-shift), masked to 64 bits"
            "Right part: (bitwise-arithmetic-shift-right x n)"
            "Left part: (bitwise-arithmetic-shift-left x (- 64 n))"
            "Combine: (bitwise-ior right-part left-part)"
            "Mask: (bitwise-and result #xFFFFFFFFFFFFFFFF)"))
    (reference-solution "(define (rotr64 x n)
  (bitwise-and
    (bitwise-ior
      (bitwise-arithmetic-shift-right x n)
      (bitwise-arithmetic-shift-left x (- 64 n)))
    #xFFFFFFFFFFFFFFFF))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement Ch (Choose)
  ;; ──────────────────────────────────────────────
  ;; Bitwise conditional: for each bit position, if x=1 select y, else select z.

  (task
    (id "crypto/sha512/ch-impl")
    (module sha512)
    (tier 0)
    (type implement)
    (description "Implement Ch (Choose): a bitwise conditional function used in SHA-512 compression. For each bit position independently: if the bit of x is 1, the result bit comes from y; if the bit of x is 0, the result bit comes from z. The formula is: Ch(x,y,z) = (x AND y) XOR ((NOT x) AND z). Think of x as a selector mask: it 'chooses' between corresponding bits of y and z. This is equivalent to a bitwise multiplexer.")
    (context "Available: bitwise-and, bitwise-xor, bitwise-not. Pure bitwise arithmetic, no masking needed (XOR and AND don't grow beyond input width).")
    (before "(define (Ch x y z)
  (doc 'type '(-> Integer Integer Integer Integer))
  ;; TODO: bitwise choose — if x then y else z, per bit
  0)")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; All-ones selector: choose y entirely
(assert-equal #xAA (Ch #xFFFFFFFFFFFFFFFF #xAA #xBB))"
                 ";; Zero selector: choose z entirely
(assert-equal #xBB (Ch 0 #xAA #xBB))"
                 ";; Mixed selector: #xF0 selects upper nibble from y, lower from z
;; y=#xAA=10101010, z=#x55=01010101, x=#xF0=11110000
;; Upper nibble from y: 1010, lower nibble from z: 0101 → #xA5
(assert-equal #xA5 (Ch #xF0 #xAA #x55))"
                 ";; When y=z, result is always y regardless of x
(assert-equal #x42 (Ch #xDEADBEEF #x42 #x42))"))
    (available-modules ())
    (hints ("The formula is: (x AND y) XOR (NOT(x) AND z)"
            "bitwise-not inverts all bits of x"
            "(bitwise-and x y) keeps only bits where x=1"
            "(bitwise-and (bitwise-not x) z) keeps only bits where x=0"
            "XOR combines the two non-overlapping selections"))
    (reference-solution "(define (Ch x y z)
  (bitwise-xor (bitwise-and x y)
               (bitwise-and (bitwise-not x) z)))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement Maj (Majority)
  ;; ──────────────────────────────────────────────
  ;; Bitwise voting: result bit is 1 if 2+ of {x,y,z} have that bit set.

  (task
    (id "crypto/sha512/maj-impl")
    (module sha512)
    (tier 0)
    (type implement)
    (description "Implement Maj (Majority): a bitwise voting function used in SHA-512 compression. For each bit position, the result is 1 if at least 2 of the 3 inputs have that bit set, and 0 otherwise. The formula is: Maj(x,y,z) = (x AND y) XOR (x AND z) XOR (y AND z). This computes the majority because: each AND gives 1 when a specific pair agrees. For a bit where all three agree (all 1s), all three ANDs give 1, and 1 XOR 1 XOR 1 = 1. For two agreeing on 1: exactly one AND gives 1 → result 1. For only one set: no pair ANDs give 1 → result 0.")
    (context "Available: bitwise-and, bitwise-xor. Pure bitwise arithmetic.")
    (before "(define (Maj x y z)
  (doc 'type '(-> Integer Integer Integer Integer))
  ;; TODO: bitwise majority — 1 if 2+ of {x,y,z} have bit set
  0)")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Two of three have all bits set → all bits set
(assert-equal #xFF (Maj #xFF #xFF #x00))"
                 ";; Only one has bits set → no majority
(assert-equal #x00 (Maj #xFF #x00 #x00))"
                 ";; All three agree → result equals them
(assert-equal #xAB (Maj #xAB #xAB #xAB))"
                 ";; Upper nibble: 2/3 set (#xF0,#xF0,#x0F), lower: 1/3 set
(assert-equal #xF0 (Maj #xF0 #xF0 #x0F))"
                 ";; Commutative: order doesn't matter
(assert-equal (Maj #xAB #xCD #xEF) (Maj #xCD #xEF #xAB))"))
    (available-modules ())
    (hints ("The formula: (x AND y) XOR (x AND z) XOR (y AND z)"
            "Compute all three pairwise ANDs"
            "XOR them together"
            "In Scheme, nest the XORs: (bitwise-xor (bitwise-and x y) (bitwise-xor (bitwise-and x z) (bitwise-and y z)))"))
    (reference-solution "(define (Maj x y z)
  (bitwise-xor (bitwise-and x y)
               (bitwise-xor (bitwise-and x z)
                            (bitwise-and y z))))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement sigma0-512
  ;; ──────────────────────────────────────────────
  ;; Message schedule mixing function — composes rotr64 and shr64.

  (task
    (id "crypto/sha512/sigma0-512-impl")
    (module sha512)
    (tier 0)
    (type implement)
    (description "Implement sigma0-512: the lowercase sigma-0 function from SHA-512, used in the message schedule expansion. It mixes bits of a 64-bit word using two rotations and one shift, then XORs the results. The formula is: sigma0(x) = ROTR(x,1) XOR ROTR(x,8) XOR SHR(x,7), where ROTR is 64-bit right rotation and SHR is logical right shift. The result must be masked to 64 bits. Note the distinction from the uppercase Sigma0 (used in compression) which has different rotation amounts.")
    (context "Available: rotr64 (64-bit right rotation), shr64 (64-bit right shift, equivalent to bitwise-arithmetic-shift-right), bitwise-xor, bitwise-and. The constant u64-mask = #xFFFFFFFFFFFFFFFF is defined for masking.")
    (before "(define (sigma0-512 x)
  (doc 'type '(-> Integer Integer))
  (doc 'description \"SHA-512 message schedule mixing function (lowercase sigma-0)\")
  ;; TODO: ROTR(x,1) XOR ROTR(x,8) XOR SHR(x,7), masked to 64 bits
  0)")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Zero input gives zero
(assert-equal 0 (sigma0-512 0))"
                 ";; Input 1: rotr(1,1)=2^63, rotr(1,8)=2^56, shr(1,7)=0
;; Result = 2^63 XOR 2^56 XOR 0
(assert-equal (bitwise-xor (expt 2 63) (expt 2 56)) (sigma0-512 1))"
                 ";; Verify against known SHA-512 intermediate values
;; sigma0(2) = rotr(2,1) XOR rotr(2,8) XOR shr(2,7)
;; rotr(2,1) = 1, rotr(2,8) = 2^57, shr(2,7) = 0
(assert-equal (bitwise-xor 1 (expt 2 57)) (sigma0-512 2))"))
    (available-modules ())
    (hints ("Compute three terms: (rotr64 x 1), (rotr64 x 8), (shr64 x 7)"
            "XOR all three: (bitwise-xor term1 (bitwise-xor term2 term3))"
            "Mask the result: (bitwise-and result u64-mask)"
            "shr64 is just (bitwise-arithmetic-shift-right x 7) — shift, not rotate"))
    (reference-solution "(define (sigma0-512 x)
  (bitwise-and
    (bitwise-xor (rotr64 x 1)
                 (bitwise-xor (rotr64 x 8)
                              (shr64 x 7)))
    #xFFFFFFFFFFFFFFFF))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement pad-message-512
  ;; ──────────────────────────────────────────────
  ;; Merkle-Damgård padding — the byte-level prep before hashing.

  (task
    (id "crypto/sha512/pad-message-512-impl")
    (module sha512)
    (tier 0)
    (type implement)
    (description "Implement pad-message-512: apply Merkle-Damgård padding to a message bytevector for SHA-512. The padded result must be a multiple of 128 bytes (1024 bits). Steps: (1) Compute the original message length in bytes (len) and bits (bit-len = 8*len). (2) Calculate padded length: need at least 17 extra bytes (1 for the 0x80 marker + 16 for the 128-bit length). Round up (len + 17) to the next multiple of 128. (3) Create a zero-filled bytevector of the padded length. (4) Copy the original message into it. (5) Set byte at position len to #x80 (the '1' bit followed by zeros). (6) Store bit-len as a 64-bit big-endian integer at the last 8 bytes (the upper 64 bits of the 128-bit length field are zero for messages under 2^64 bits). Use bytevector-copy!, bytevector-u8-set!, and bytevector-u64-set! for the byte operations.")
    (context "Available: bytevector-length, bytevector-copy! (src src-start dst dst-start count), bytevector-u8-set! (bv index val), bytevector-u64-set! (bv index val endianness), make-bytevector, modulo, +, -, *, =, if, let*. No dependencies.")
    (before "(define (pad-message-512 msg)
  (doc 'type '(-> Bytevector Bytevector))
  (doc 'description \"Pad to multiple of 128 bytes per FIPS 180-4\")
  ;; TODO: append 0x80, zeros, then 128-bit big-endian length
  msg)")
    (test-file "lattice/crypto/test-crypto.ss")
    (test-forms (";; Empty message pads to exactly 128 bytes (one block)
(assert-equal 128 (bytevector-length (pad-message-512 (string->utf8 \"\"))))"
                 ";; Short message (3 bytes) also pads to 128 bytes
(assert-equal 128 (bytevector-length (pad-message-512 (string->utf8 \"abc\"))))"
                 ";; First byte after message is #x80
(let ([padded (pad-message-512 (string->utf8 \"abc\"))])
  (assert-equal #x80 (bytevector-u8-ref padded 3)))"
                 ";; Last 8 bytes encode bit-length in big-endian
;; \"abc\" = 3 bytes = 24 bits
(let ([padded (pad-message-512 (string->utf8 \"abc\"))])
  (assert-equal 24 (bytevector-u64-ref padded 120 'big)))"
                 ";; Message filling nearly one block (111 bytes) still fits in 128
(assert-equal 128 (bytevector-length (pad-message-512 (make-bytevector 111 0))))"
                 ";; Message of 112 bytes needs two blocks (256 bytes)
(assert-equal 256 (bytevector-length (pad-message-512 (make-bytevector 112 0))))"))
    (available-modules ())
    (hints ("Calculate padded-len: (let ([rem (modulo (+ len 17) 128)]) (if (= rem 0) (+ len 17) (+ len 17 (- 128 rem))))"
            "The 17 comes from: 1 byte for #x80 + 16 bytes for the 128-bit length field"
            "Use (make-bytevector padded-len 0) to start with all zeros"
            "(bytevector-copy! msg 0 result 0 len) copies the message"
            "(bytevector-u8-set! result len #x80) appends the marker"
            "(bytevector-u64-set! result (- padded-len 8) bit-len 'big) writes the length"))
    (reference-solution "(define (pad-message-512 msg)
  (let* ([len (bytevector-length msg)]
         [bit-len (* 8 len)]
         [padded-len (let ([rem (modulo (+ len 17) 128)])
                       (if (= rem 0)
                           (+ len 17)
                           (+ len 17 (- 128 rem))))]
         [result (make-bytevector padded-len 0)])
    (bytevector-copy! msg 0 result 0 len)
    (bytevector-u8-set! result len #x80)
    (bytevector-u64-set! result (- padded-len 8) bit-len 'big)
    result))")
    (alternatives ())
    (source-commit "f64d84d0")
    (difficulty 4))

) ;; end tasks
