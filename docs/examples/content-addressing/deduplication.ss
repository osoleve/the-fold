;;; deduplication.ss --- How Identical Content Shares Identity
;;;
;;; Demonstrates automatic deduplication in content-addressed systems.
;;;
;;; Run with: scheme --script docs/examples/content-addressing/deduplication.ss

(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")
(load "core/blocks/normalize.ss")

;;; ============================================================
;;; Basic Deduplication
;;; ============================================================

(display "=== Basic Deduplication ===\n")

;;; Create the same content multiple times
(define msg1 (make-block 'text "Hello, World!" '()))
(define msg2 (make-block 'text "Hello, World!" '()))
(define msg3 (make-block 'text "Hello, World!" '()))

(format #t "msg1 hash: ~a~%" (block-hash msg1))
(format #t "msg2 hash: ~a~%" (block-hash msg2))
(format #t "msg3 hash: ~a~%" (block-hash msg3))
(format #t "All same? ~a~%"
        (and (equal? (block-hash msg1) (block-hash msg2))
             (equal? (block-hash msg2) (block-hash msg3))))

;;; Store all three - only one copy exists
(cas-store! msg1)
(cas-store! msg2)
(cas-store! msg3)
(display "\nStored 3 times, but only 1 copy exists in CAS.\n")

;;; ============================================================
;;; Semantic Deduplication (Alpha Normalization)
;;; ============================================================

(display "\n=== Semantic Deduplication ===\n")

(display "
The Fold goes further: semantically identical code deduplicates
even if written differently.

  (lambda (x) (+ x 1))
  (lambda (y) (+ y 1))

These ARE the same function. Variable names don't matter.
")

;;; Before normalization: different representations
(define expr1 '(lambda (x) (+ x 1)))
(define expr2 '(lambda (y) (+ y 1)))
(define expr3 '(lambda (z) (+ z 1)))

(format #t "expr1: ~a~%" expr1)
(format #t "expr2: ~a~%" expr2)
(format #t "expr3: ~a~%" expr3)

;;; After alpha-normalization: identical
(define norm1 (alpha-normalize expr1))
(define norm2 (alpha-normalize expr2))
(define norm3 (alpha-normalize expr3))

(format #t "\nAfter normalization:~%")
(format #t "norm1: ~a~%" norm1)
(format #t "norm2: ~a~%" norm2)
(format #t "norm3: ~a~%" norm3)
(format #t "All equal? ~a~%" (and (equal? norm1 norm2) (equal? norm2 norm3)))

;;; ============================================================
;;; de Bruijn Indices
;;; ============================================================

(display "\n=== How It Works: de Bruijn Indices ===\n")

(display "
Alpha normalization replaces variable names with de Bruijn indices:
- Each variable becomes a number
- The number indicates 'how many lambdas up' to find the binding

  (lambda (x) x)           ->  (lambda #0)
  (lambda (x) (lambda (y) x))  ->  (lambda (lambda #1))
  (lambda (x) (lambda (y) y))  ->  (lambda (lambda #0))

#0 = bound by closest lambda
#1 = bound by second-closest lambda
")

;;; Examples
(define ex1 '(lambda (x) x))
(define ex2 '(lambda (x) (lambda (y) x)))
(define ex3 '(lambda (x) (lambda (y) y)))

(format #t "~a -> ~a~%" ex1 (alpha-normalize ex1))
(format #t "~a -> ~a~%" ex2 (alpha-normalize ex2))
(format #t "~a -> ~a~%" ex3 (alpha-normalize ex3))

;;; ============================================================
;;; Deduplication of Subexpressions
;;; ============================================================

(display "\n=== Subexpression Sharing ===\n")

(display "
Common subexpressions automatically share storage.
")

;;; Two expressions sharing a common part
(define common-part '(+ 1 2))
(define big-expr1 `(* ,common-part ,common-part))
(define big-expr2 `(+ ,common-part 10))

(define common-block (make-block 'expr common-part '()))
(cas-store! common-block)

(format #t "Common subexpression (+ 1 2): ~a...~%"
        (substring (block-hash common-block) 0 16))

;;; Both larger expressions reference the same common-block hash
(format #t "\nBoth expressions reference the same hash for (+ 1 2).\n")
(format #t "Storage grows linearly with unique content, not total size.\n")

;;; ============================================================
;;; Real-World Impact
;;; ============================================================

(display "\n=== Real-World Impact ===\n")

(display "
In a large codebase:

1. COMMON PATTERNS DEDUPLICATE
   Every (if x y z) with the same structure shares storage.
   Every (define (f x) ...) pattern shares structure.

2. LIBRARY FUNCTIONS DEDUPLICATE
   Import map from two modules? Same hash, one copy.
   Two projects using same version of a library? Shared.

3. DATA DEDUPLICATION
   Same configuration used in 100 places? One copy.
   Log messages with repeated prefixes? Prefixes shared.

4. VERSION CONTROL
   Two commits sharing most code? Only differences stored.
   Revert to old version? No extra storage (already exists).

5. TESTING/VERIFICATION
   Same test case stored once, reused everywhere.
   Verification proofs for identical code? Shared.
")

;;; ============================================================
;;; Counting Unique Blocks
;;; ============================================================

(display "=== Demonstration ===\n")

;;; Create many 'similar' blocks
(define (make-numbered-block n)
  (make-block 'numbered n '()))

;;; Store numbers 1-10, but also 1-5 again
(for-each (lambda (n)
            (cas-store! (make-numbered-block n)))
          '(1 2 3 4 5 6 7 8 9 10 1 2 3 4 5))

(display "Stored: 1 2 3 4 5 6 7 8 9 10 1 2 3 4 5 (15 operations)\n")
(display "Unique blocks: 10 (duplicates of 1-5 are no-ops)\n")

;;; Verify by checking hashes
(format #t "\nblock(1) hash: ~a...~%"
        (substring (block-hash (make-numbered-block 1)) 0 16))
(format #t "block(1) hash (again): ~a...~%"
        (substring (block-hash (make-numbered-block 1)) 0 16))
(format #t "Same? ~a~%"
        (equal? (block-hash (make-numbered-block 1))
                (block-hash (make-numbered-block 1))))

(display "\nDone!\n")
