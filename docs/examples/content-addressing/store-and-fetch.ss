;;; store-and-fetch.ss --- Content-Addressed Storage Basics
;;;
;;; The Fold stores everything by its cryptographic hash.
;;; Store a value, get back a hash. Use the hash to retrieve the value.
;;;
;;; Run with: scheme --script docs/examples/content-addressing/store-and-fetch.ss

(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")

;;; ============================================================
;;; The Basic Idea
;;; ============================================================

(display "=== Content-Addressed Storage ===\n")

(display "
In traditional storage:
  key -> value       (you choose the key)

In content-addressed storage:
  hash(value) -> value   (the content IS the key)

The hash is computed from the content itself.
Same content = same hash, always.
")

;;; ============================================================
;;; Creating and Storing Blocks
;;; ============================================================

(display "=== Storing Blocks ===\n")

;;; Create a simple block
(define greeting-block (make-block 'message "Hello, Fold!" '()))
(format #t "Created block: ~a~%" greeting-block)

;;; Get its hash (content address)
(define greeting-hash (block-hash greeting-block))
(format #t "Hash: ~a~%" greeting-hash)

;;; Store it in the CAS
(cas-store! greeting-block)
(display "Stored in CAS.\n")

;;; ============================================================
;;; Fetching by Hash
;;; ============================================================

(display "\n=== Fetching by Hash ===\n")

;;; Retrieve using the hash
(define retrieved (cas-fetch greeting-hash))
(format #t "Retrieved: ~a~%" retrieved)

;;; Verify it's the same
(format #t "Same block? ~a~%"
        (and retrieved
             (equal? (block-payload retrieved)
                     (block-payload greeting-block))))

;;; ============================================================
;;; Automatic Deduplication
;;; ============================================================

(display "\n=== Deduplication ===\n")

;;; Create the "same" block again
(define greeting-copy (make-block 'message "Hello, Fold!" '()))
(define copy-hash (block-hash greeting-copy))

(format #t "Original hash: ~a~%" greeting-hash)
(format #t "Copy hash:     ~a~%" copy-hash)
(format #t "Same hash? ~a~%" (equal? greeting-hash copy-hash))

;;; Storing the copy doesn't create a duplicate
(cas-store! greeting-copy)
(display "\nStoring 'copy' is a no-op - content already exists.\n")

;;; ============================================================
;;; Storing Different Content
;;; ============================================================

(display "\n=== Different Content = Different Hash ===\n")

(define block-a (make-block 'data "Version A" '()))
(define block-b (make-block 'data "Version B" '()))

(format #t "Block A hash: ~a...~%" (substring (block-hash block-a) 0 16))
(format #t "Block B hash: ~a...~%" (substring (block-hash block-b) 0 16))
(format #t "Same? ~a~%" (equal? (block-hash block-a) (block-hash block-b)))

;;; ============================================================
;;; Blocks with References
;;; ============================================================

(display "\n=== Blocks with References ===\n")

;;; Create blocks that reference other blocks
(define part1 (make-block 'part "First part" '()))
(define part2 (make-block 'part "Second part" '()))

(cas-store! part1)
(cas-store! part2)

;;; Parent block references children by hash
(define parent (make-block 'composite "Two parts"
                           (list (block-hash part1)
                                 (block-hash part2))))
(cas-store! parent)

(format #t "Parent hash: ~a...~%" (substring (block-hash parent) 0 16))
(format #t "Parent refs: ~a~%" (block-refs parent))

;;; Following references
(display "\nFollowing references:\n")
(for-each
 (lambda (ref-hash)
   (let ([child (cas-fetch ref-hash)])
     (format #t "  ~a... -> ~a~%"
             (substring ref-hash 0 12)
             (block-payload child))))
 (block-refs parent))

;;; ============================================================
;;; The Power of Content Addressing
;;; ============================================================

(display "\n=== Why This Matters ===\n")

(display "
1. IMMUTABILITY
   A hash always refers to the same content.
   You can trust that what you fetch is what was stored.

2. INTEGRITY
   If content is corrupted, the hash won't match.
   Verification is automatic.

3. SHARING
   Same content is only stored once.
   Share hashes, not copies.

4. CACHING
   If you have hash H, you know the content forever.
   Cache aggressively without invalidation concerns.

5. COLLABORATION
   Two systems that independently create the same content
   will produce the same hash. Natural deduplication.
")

(display "\nDone!\n")
