;;; blocks.ss --- The Universal Primitive
;;;
;;; In The Fold, everything is a Block: { tag, payload, refs[] }
;;; Blocks are content-addressed: their identity IS their cryptographic hash.
;;;
;;; Run with: scheme --script docs/examples/data-structures/blocks.ss

(load "core/blocks/block.ss")

;;; ====
;;; Block Structure
;;; ====

(display "=== Block Structure ===\n")

(display "
A Block has three parts:

  Block = { tag: Symbol, payload: Bytes, refs: [Hash] }

  tag     - A symbol identifying the block type (lambda, if, cons, etc.)
  payload - Raw bytes: literals, encoded data
  refs    - Ordered list of hashes pointing to other blocks

This simple structure can represent ANY data or code.
")

;;; ====
;;; Creating Blocks
;;; ====

(display "=== Creating Blocks ===\n")

;;; make-block creates a block from tag, payload, and refs
(define b1 (make-block 'greeting "Hello, World!" '()))
(format #t "Simple block: ~a~%" b1)

;;; Blocks with references to other blocks
(define b2 (make-block 'number 42 '()))
(define b3 (make-block 'number 100 '()))
(define b-sum (make-block 'add #f (list (block-hash b2) (block-hash b3))))

(format #t "Number block: ~a~%" b2)
(format #t "Sum block (refs two numbers): ~a~%" b-sum)

;;; ====
;;; Block Components
;;; ====

(display "\n=== Accessing Block Components ===\n")

(format #t "tag of b1: ~a~%" (block-tag b1))
(format #t "payload of b1: ~a~%" (block-payload b1))
(format #t "refs of b1: ~a~%" (block-refs b1))
(format #t "hash of b1: ~a~%" (block-hash b1))

(format #t "\nrefs of b-sum: ~a~%" (block-refs b-sum))

;;; ====
;;; Content Addressing
;;; ====

(display "\n=== Content Addressing ===\n")

;;; Two blocks with identical content have the same hash
(define b1-copy (make-block 'greeting "Hello, World!" '()))

(format #t "b1 hash:      ~a~%" (block-hash b1))
(format #t "b1-copy hash: ~a~%" (block-hash b1-copy))
(format #t "Same hash? ~a~%" (equal? (block-hash b1) (block-hash b1-copy)))

;;; This is the core insight: identity = content
(display "\nIdentity IS content. Same content = same identity.\n")

;;; ====
;;; Blocks as Code
;;; ====

(display "\n=== Blocks Represent Code ===\n")

;;; The expression (+ 1 2) as blocks:
;;;
;;;   [add]
;;;    |  \
;;;   [1]  [2]

(define one-block (make-block 'literal 1 '()))
(define two-block (make-block 'literal 2 '()))
(define add-block (make-block 'add #f
                              (list (block-hash one-block)
                                    (block-hash two-block))))

(format #t "Expression (+ 1 2) as block tree:~%")
(format #t "  [add] hash: ~a...~%" (substring (block-hash add-block) 0 16))
(format #t "    -> [1] hash: ~a...~%" (substring (block-hash one-block) 0 16))
(format #t "    -> [2] hash: ~a...~%" (substring (block-hash two-block) 0 16))

;;; ====
;;; Why Blocks Matter
;;; ====

(display "\n=== Why Blocks Matter ===\n")

(display "
1. DEDUPLICATION
   Same computation stored twice = stored once.
   Your codebase automatically deduplicates.

2. VERIFICATION
   If you have a hash, you can verify the content.
   Tampering is detectable.

3. REFERENTIAL TRANSPARENCY
   A hash always refers to the same thing.
   No 'version drift' or 'dependency hell'.

4. HOMOICONICITY
   Code and data are both blocks.
   The system can introspect everything.

5. SEMANTIC IDENTITY
   (lambda (x) (+ x 1)) and (lambda (y) (+ y 1))
   hash identically because variable names are normalized away.
")

;;; ====
;;; Alpha Normalization Preview
;;; ====

(display "=== Alpha Normalization ===\n")

(display "
Before hashing, The Fold normalizes variable names to de Bruijn indices.

  (lambda (x) (+ x 1))  -->  (lambda (+ #0 1))
  (lambda (y) (+ y 1))  -->  (lambda (+ #0 1))

These produce the SAME hash because they ARE the same function.
Variable names are presentation, not semantics.
")

(display "\nDone!\n")
