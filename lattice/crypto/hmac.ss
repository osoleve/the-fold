;;; lattice/crypto/hmac.ss — HMAC (RFC 2104)
;;;
;;; Keyed-Hash Message Authentication Code.
;;;
;;; HMAC provides message authentication using any cryptographic hash function.
;;; HMAC(K, m) = H((K' ⊕ opad) || H((K' ⊕ ipad) || m))
;;;
;;; Where:
;;;   K' = key padded or hashed to block size
;;;   ipad = 0x36 repeated to block size
;;;   opad = 0x5c repeated to block size
;;;
;;; TIER: 1 (depends on hash functions)

(load "core/base/sha256.ss")
(load "lattice/crypto/sha512.ss")

;;; ============================================================
;;; HMAC Core Implementation
;;; ============================================================

;;; make-hmac : (Bytevector → Bytevector) × Integer × Integer → (Bytevector × Bytevector → Bytevector)
;;; Create an HMAC function for a given hash, block size, and output size.
;;;
;;; hash-fn: The hash function (e.g., sha256, sha512)
;;; block-size: Hash block size in bytes (64 for SHA-256, 128 for SHA-512)
;;; output-size: Hash output size in bytes (32 for SHA-256, 64 for SHA-512)
(define (make-hmac hash-fn block-size output-size)
  (lambda (key message)
    (let* ([key-len (bytevector-length key)]
           ;; If key > block-size, hash it; if shorter, pad with zeros
           [key-prime (cond
                        [(> key-len block-size)
                         (let ([hashed (hash-fn key)]
                               [padded (make-bytevector block-size 0)])
                           (bytevector-copy! hashed 0 padded 0 (bytevector-length hashed))
                           padded)]
                        [(< key-len block-size)
                         (let ([padded (make-bytevector block-size 0)])
                           (bytevector-copy! key 0 padded 0 key-len)
                           padded)]
                        [else key])]
           ;; Create ipad and opad XORed with key
           [ipad-key (make-bytevector block-size)]
           [opad-key (make-bytevector block-size)])
      ;; XOR key with ipad (0x36) and opad (0x5c)
      (do ([i 0 (+ i 1)])
          ((= i block-size))
          (bytevector-u8-set! ipad-key i
                              (bitwise-xor (bytevector-u8-ref key-prime i) #x36))
          (bytevector-u8-set! opad-key i
                              (bitwise-xor (bytevector-u8-ref key-prime i) #x5c)))
      ;; HMAC = H(opad-key || H(ipad-key || message))
      (let* ([inner-input (bytevector-append ipad-key message)]
             [inner-hash (hash-fn inner-input)]
             [outer-input (bytevector-append opad-key inner-hash)])
        (hash-fn outer-input)))))

;;; bytevector-append : Bytevector × Bytevector → Bytevector
;;; Concatenate two bytevectors.
(define (bytevector-append bv1 bv2)
  (let* ([len1 (bytevector-length bv1)]
         [len2 (bytevector-length bv2)]
         [result (make-bytevector (+ len1 len2))])
    (bytevector-copy! bv1 0 result 0 len1)
    (bytevector-copy! bv2 0 result len1 len2)
    result))

;;; ============================================================
;;; Pre-defined HMAC Functions
;;; ============================================================

;;; hmac-sha256 : Bytevector × Bytevector → Bytevector
;;; HMAC using SHA-256.
;;; Block size: 64 bytes, Output: 32 bytes
(define hmac-sha256 (make-hmac sha256 64 32))

;;; hmac-sha512 : Bytevector × Bytevector → Bytevector
;;; HMAC using SHA-512.
;;; Block size: 128 bytes, Output: 64 bytes
(define hmac-sha512 (make-hmac sha512 128 64))

;;; hmac-sha384 : Bytevector × Bytevector → Bytevector
;;; HMAC using SHA-384.
;;; Block size: 128 bytes (same as SHA-512), Output: 48 bytes
(define hmac-sha384 (make-hmac sha384 128 48))

;;; ============================================================
;;; Hex Convenience Functions
;;; ============================================================

;;; hmac-sha256-hex : Bytevector × Bytevector → String
(define (hmac-sha256-hex key message)
  (hash->hex (hmac-sha256 key message)))

;;; hmac-sha512-hex : Bytevector × Bytevector → String
(define (hmac-sha512-hex key message)
  (hash->hex (hmac-sha512 key message)))

;;; hmac-sha384-hex : Bytevector × Bytevector → String
(define (hmac-sha384-hex key message)
  (hash->hex (hmac-sha384 key message)))

;;; hash->hex : Bytevector → String
;;; Convert hash to lowercase hex string.
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
