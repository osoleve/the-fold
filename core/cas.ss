;;; core/cas.ss — Content-Addressed Store
;;;
;;; Every Block has a cryptographic hash that IS its identity.
;;; Same content = same hash, forever.
;;;
;;; Pure operations:
;;;   hash-block : Block → Bytevector (32-byte hash)
;;;
;;; Store operations (in-memory for bootstrap):
;;;   store! : Block → Bytevector (store and return hash)
;;;   fetch : Bytevector → Block | #f
;;;   pin! : Bytevector → void (mark as persistent)
;;;   stored? : Bytevector → Boolean
;;;
;;; Note: The in-memory store uses mutation for the hashtable.
;;; This is acceptable in a bootstrap CAS. Shell provides
;;; capability-gated filesystem persistence (shell/cas-persist.ss).
;;;
;;; This is Core code, but with bootstrap mutation for the store.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - block.ss
;;;   - sha256.ss
;;;
;;; See core/MODULES.md for full dependency graph.

(load "prelude.ss")
(load "block.ss")
(load "sha256.ss")

;;; ============================================================
;;; Hashing (Pure)
;;; ============================================================

;;; hash-block : Block → Bytevector
;;; Compute the cryptographic hash of a block.
;;; The hash is computed over the canonical serialization.
(define (hash-block blk)
  (sha256 (block->bytes blk)))

;;; hash->hex : Bytevector → String
;;; Convert hash to hexadecimal string (for display).
(define (hash->hex hash)
  (let ([hex-chars "0123456789abcdef"])
    (apply string-append
           (map (lambda (i)
                  (let ([b (bytevector-u8-ref hash i)])
                    (string
                      (string-ref hex-chars (quotient b 16))
                      (string-ref hex-chars (modulo b 16)))))
                (iota 32)))))

;;; hex->hash : String → Bytevector
;;; Convert hexadecimal string to hash bytes.
(define (hex->hash str)
  (let ([result (make-bytevector 32)])
    (do ([i 0 (+ i 1)])
        ((= i 32))
      (let* ([j (* i 2)]
             [hi (char->hex-digit (string-ref str j))]
             [lo (char->hex-digit (string-ref str (+ j 1)))])
        (bytevector-u8-set! result i (+ (* hi 16) lo))))
    result))

(define (char->hex-digit c)
  (cond
    [(char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0))]
    [(char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a)))]
    [(char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A)))]
    [else (error 'char->hex-digit "invalid hex character" c)]))

;;; ============================================================
;;; In-Memory Store
;;; ============================================================

;;; The store is a hashtable: hash-bytes → Block
;;; We use bytevector hashes as keys via equal-hash.
(define *store* (make-hashtable equal-hash equal?))

;;; Pinned hashes are preserved during garbage collection.
;;; (Not implemented yet — all stored blocks are retained.)
(define *pinned* (make-hashtable equal-hash equal?))

;;; store! : Block → Bytevector
;;; Store a block and return its hash.
(define (store! blk)
  (let ([hash (hash-block blk)])
    (hashtable-set! *store* hash blk)
    hash))

;;; fetch : Bytevector → Block | #f
;;; Retrieve a block by its hash, or #f if not found.
(define (fetch hash)
  (hashtable-ref *store* hash #f))

;;; stored? : Bytevector → Boolean
;;; Check if a block with this hash exists.
(define (stored? hash)
  (hashtable-contains? *store* hash))

;;; pin! : Bytevector → void
;;; Mark a hash as pinned (should not be garbage collected).
(define (pin! hash)
  (hashtable-set! *pinned* hash #t))

;;; pinned? : Bytevector → Boolean
(define (pinned? hash)
  (hashtable-ref *pinned* hash #f))

;;; ============================================================
;;; Store Statistics
;;; ============================================================

;;; store-count : → Nat
;;; Number of blocks in the store.
(define (store-count)
  (hashtable-size *store*))

;;; store-hashes : → (List Bytevector)
;;; All hashes in the store.
(define (store-hashes)
  (vector->list (hashtable-keys *store*)))

;;; ============================================================
;;; Convenience: Store S-expressions
;;; ============================================================

;;; These wrap S-expressions in blocks for storage.

;;; store-sexpr! : Symbol × S-expr → Bytevector
;;; Store an S-expression as a block with given tag.
(define (store-sexpr! tag sexpr)
  (let* ([payload (string->utf8 (format "~s" sexpr))]
         [blk (make-block tag payload empty-refs)])
    (store! blk)))

;;; fetch-sexpr : Bytevector → S-expr | #f
;;; Retrieve and parse an S-expression block.
(define (fetch-sexpr hash)
  (let ([blk (fetch hash)])
    (if blk
        (read (open-input-string (utf8->string (block-payload blk))))
        #f)))
