;;; core/blocks/cas.ss — Content-Addressed Store
;;; @module cas
;;; @requires prelude block sha256
;;;
;;; Every Block has a cryptographic hash that IS its identity.
;;; Same content = same hash, forever.
;;;
;;; Pure operations:
;;;   hash-block : Block → Bytevector (33-byte address)
;;;
;;; Store operations (in-memory only):
;;;   store! : Block → Bytevector (store and return address)
;;;   fetch : Bytevector → Block | #f
;;;   pin! : Bytevector → void (mark as persistent)
;;;   stored? : Bytevector → Boolean
;;;
;;; Note: The in-memory store uses mutation for the hashtable.
;;; This is acceptable in Core. Shell provides optional filesystem
;;; persistence (thimble/cas-persist.ss or thimble/fs.ss).
;;;
;;; This is Core code, but with bootstrap mutation for the store.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - block.ss
;;;   - sha256.ss
;;;
;;; See fabric/stitches/MODULES.md for full dependency graph.

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")

;;; ============================================================
;;; Hashing (Pure)
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

;;; hash->hex : Bytevector → String
;;; Convert address bytes to hexadecimal string (for display).
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
;;; Convert hexadecimal string to address bytes.
(define (hex->hash str)
  (let* ([len (string-length str)]
         [result (make-bytevector (quotient len 2))])
        (do ([i 0 (+ i 1)])
            ((= i (bytevector-length result)))
            (let* ([j (* i 2)]
                   [hi (char->hex-digit (string-ref str j))]
                   [lo (char->hex-digit (string-ref str (+ j 1)))])
                  (bytevector-u8-set! result i (+ (* hi 16) lo))))
        result))

;;; char->hex-digit : Char → Nat
;;; Convert hexadecimal character to numeric value (0-15).
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
;;;
;;; THREAD SAFETY NOTE:
;;; This implementation is NOT thread-safe. Concurrent access to the
;;; hashtables (*store* and *pinned*) from multiple threads can lead to
;;; race conditions and data corruption. If multi-threaded access is
;;; required, external synchronization (e.g., mutexes) must be used to
;;; serialize all store operations (store!, fetch, pin!, unpin!, gc!).
;;;
;;; For production use, consider:
;;; - Wrapping store operations in a mutex
;;; - Using a thread-safe concurrent data structure
;;; - Ensuring all access goes through a single-threaded coordinator
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
;;; Check if a hash is currently pinned.
(define (pinned? hash)
  (hashtable-ref *pinned* hash #f))

;;; unpin! : Bytevector → void
;;; Remove pin from a hash.
(define (unpin! hash)
  (hashtable-delete! *pinned* hash))

;;; ============================================================
;;; Tree Operations (Transitive Pinning/Unpinning)
;;; ============================================================

;;; collect-refs : Bytevector × (Bytevector → Block) → (List Bytevector)
;;; Collect all transitive references from a block.
;;; Uses iterative traversal. Prepends new refs to avoid O(N²) append.
(define (collect-refs hash fetch)
  (let ([visited (make-hashtable equal-hash equal?)]
        [queue (list hash)]
        [results '()])
       (let loop ()
            (if (null? queue)
                results
                (let ([current (car queue)]
                      [rest-queue (cdr queue)])
                     (if (hashtable-ref visited current #f)
                         ;; Already visited, skip
                         (begin
                          (set! queue rest-queue)
                          (loop))
                         ;; New node: mark visited, add to results
                         (begin
                          (hashtable-set! visited current #t)
                          (set! results (cons current results))
                          (let ([blk (fetch current)])
                               (if blk
                                   ;; Prepend new refs to queue
                                   (let ([new-refs (filter
                                                    (lambda (r) (not (hashtable-ref visited r #f)))
                                                    (vector->list (block-refs blk)))])
                                        (set! queue (append new-refs rest-queue)))
                                   (set! queue rest-queue)))
                          (loop))))))))

;;; pin-tree! : Bytevector → Nat
;;; Pin a hash and all its transitive references.
;;; Returns the number of blocks pinned.
(define (pin-tree! hash)
  (let* ([refs (collect-refs hash fetch)]
         [count 0])
        (for-each
         (lambda (h)
                 (unless (pinned? h)
                         (pin! h)
                         (set! count (+ count 1))))
         refs)
        count))

;;; unpin-tree! : Bytevector → Nat
;;; Unpin a hash and all its transitive references.
;;; Returns the number of blocks unpinned.
(define (unpin-tree! hash)
  (let* ([refs (collect-refs hash fetch)]
         [count 0])
        (for-each
         (lambda (h)
                 (when (pinned? h)
                       (unpin! h)
                       (set! count (+ count 1))))
         refs)
        count))

;;; ============================================================
;;; Garbage Collection
;;; ============================================================

;;; gc! : → (values Nat Nat)
;;; Remove all unpinned blocks from the store.
;;; Returns (collected-count remaining-count).
(define (gc!)
  (let ([to-remove '()]
        [initial-count (store-count)])
       ;; Collect unpinned hashes
       (vector-for-each
        (lambda (hash)
                (unless (pinned? hash)
                        (set! to-remove (cons hash to-remove))))
        (hashtable-keys *store*))
       ;; Remove them
       (for-each
        (lambda (hash)
                (hashtable-delete! *store* hash))
        to-remove)
       (values (length to-remove) (store-count))))

;;; gc-with-roots! : (List Bytevector) → (values Nat Nat)
;;; Collect blocks not reachable from the given root hashes.
;;; First pins all reachable blocks, then collects unpinned.
;;; Returns (collected-count remaining-count).
(define (gc-with-roots! roots)
  ;; Save current pins
  (let ([saved-pins (make-hashtable equal-hash equal?)])
       (vector-for-each
        (lambda (h)
                (hashtable-set! saved-pins h #t))
        (hashtable-keys *pinned*))
       
       ;; Clear all pins
       (hashtable-clear! *pinned*)
       
       ;; Pin from roots
       (for-each
        (lambda (root)
                (when (stored? root)
                      (pin-tree! root)))
        roots)
       
       ;; Run GC
       (let-values ([(collected remaining) (gc!)])
                   ;; Restore original pins for remaining blocks
                   (vector-for-each
                    (lambda (h)
                            (when (hashtable-ref saved-pins h #f)
                                  (pin! h)))
                    (hashtable-keys *store*))
                   (values collected remaining))))

;;; gc-stats : → Alist
;;; Return statistics about pinned vs unpinned blocks.
(define (gc-stats)
  (let ([total 0]
        [pinned-count 0]
        [unpinned-count 0])
       (vector-for-each
        (lambda (hash)
                (set! total (+ total 1))
                (if (pinned? hash)
                    (set! pinned-count (+ pinned-count 1))
                    (set! unpinned-count (+ unpinned-count 1))))
        (hashtable-keys *store*))
       `((total . ,total)
         (pinned . ,pinned-count)
         (unpinned . ,unpinned-count)
         (gc-would-collect . ,unpinned-count))))

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
;;;
;;; SECURITY NOTE: This uses Scheme's standard 'read' which can execute
;;; code via reader macros (e.g., #. in some implementations). Only use
;;; with trusted data stored via store-sexpr!. For untrusted input,
;;; validate at the shell layer before storage.
(define (fetch-sexpr hash)
  (let ([blk (fetch hash)])
       (if blk
           (guard (ex [else #f])  ; Return #f on parse error
                  (let* ([str (utf8->string (block-payload blk))]
                         [port (open-input-string str)]
                         [result (read port)])
                        ;; Verify we consumed all input (no trailing garbage)
                        (if (eof-object? (read port))
                            result
                            #f)))  ; Trailing data = malformed
           #f)))
