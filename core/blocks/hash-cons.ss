;;; core/blocks/hash-cons.ss — Hash-consing for canonical S-expression construction
;;; @module hash-cons
;;; @requires prelude
;;;
;;; Provides a global canonicalization table that ensures structurally
;;; identical S-expressions share the same physical memory representation.
;;; This enables:
;;;   - Fast equality via pointer comparison (eq? instead of equal?)
;;;   - Memory deduplication for repeated subexpressions
;;;   - Memoized normalization (same input → cached output)
;;;
;;; Based on arXiv:2509.20534 "Efficient Symbolic Computation via Hash Consing"
;;; which demonstrates 3.2x speedup and 2x memory reduction.
;;;
;;; This is Core code: pure (except for the global table), assumes valid input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Global Canonicalization Table
;;; ====

;;; *cons-table* : Hashtable[S-expr, S-expr]
;;; Maps expressions to their unique canonical representatives.
;;; Uses equal? hashing for structural comparison.
(define *cons-table* (make-hashtable equal-hash equal?))

;;; *cons-table-hits* : Nat
;;; Counter for cache hits (for diagnostics).
(define *cons-table-hits* 0)

;;; *cons-table-misses* : Nat
;;; Counter for cache misses (for diagnostics).
(define *cons-table-misses* 0)

;;; ====
;;; Core Hash-Consing Operations
;;; ====

;;; hash-cons : S-expr → S-expr
;;; Return the unique canonical representative for an S-expression.
;;; Atoms (symbols, numbers, etc.) pass through unchanged.
;;; Pairs are recursively canonicalized and deduplicated.
(define (hash-cons x)
  (cond
    ;; Atoms are already effectively interned or immediate values
    [(not (pair? x)) x]

    ;; Pairs: recursively canonicalize children, then deduplicate
    [else
     (let* ([car-v (hash-cons (car x))]
            [cdr-v (hash-cons (cdr x))]
            [probe (cons car-v cdr-v)]
            [cached (hashtable-ref *cons-table* probe #f)])
       (if cached
           (begin
             (set! *cons-table-hits* (+ *cons-table-hits* 1))
             cached)
           (begin
             (set! *cons-table-misses* (+ *cons-table-misses* 1))
             (hashtable-set! *cons-table* probe probe)
             probe)))]))

;;; hash-cons-list : (List S-expr) → (List S-expr)
;;; Canonicalize a list of expressions.
(define (hash-cons-list xs)
  (hash-cons (map hash-cons xs)))

;;; ====
;;; Table Management
;;; ====

;;; hash-cons-reset! : → Void
;;; Clear the canonicalization table and reset counters.
;;; Call this periodically to prevent unbounded memory growth,
;;; or after major GC cycles / batch operations.
(define (hash-cons-reset!)
  (set! *cons-table* (make-hashtable equal-hash equal?))
  (set! *cons-table-hits* 0)
  (set! *cons-table-misses* 0))

;;; hash-cons-stats : → (hits . misses)
;;; Return cache hit/miss statistics.
(define (hash-cons-stats)
  (cons *cons-table-hits* *cons-table-misses*))

;;; hash-cons-size : → Nat
;;; Return number of entries in the canonicalization table.
(define (hash-cons-size)
  (hashtable-size *cons-table*))

;;; hash-cons-hit-rate : → Number
;;; Return cache hit rate as a fraction [0, 1].
;;; Returns 0 if no lookups have been performed.
(define (hash-cons-hit-rate)
  (let ([total (+ *cons-table-hits* *cons-table-misses*)])
    (if (= total 0)
        0
        (/ *cons-table-hits* total))))

;;; ====
;;; Memoized Function Wrapper
;;; ====

;;; make-memoized : (S-expr → S-expr) → (S-expr → S-expr)
;;; Wrap a function with hash-consing on both input and output.
;;; The function will:
;;;   1. Canonicalize input
;;;   2. Check if result is cached
;;;   3. If not, compute and canonicalize output
;;;   4. Cache and return result
(define (make-memoized f)
  (let ([cache (make-hashtable eq-hash eq?)])  ; Use eq? since inputs are hash-consed
    (lambda (x)
      (let* ([canon-x (hash-cons x)]
             [cached (hashtable-ref cache canon-x #f)])
        (if cached
            cached
            (let ([result (hash-cons (f canon-x))])
              (hashtable-set! cache canon-x result)
              result))))))

;;; ====
;;; Diagnostic Utilities
;;; ====

;;; hash-cons-report : → String
;;; Generate a human-readable report of hash-consing statistics.
(define (hash-cons-report)
  (let ([stats (hash-cons-stats)])
    (format "Hash-cons stats: ~a hits, ~a misses, ~a entries, ~a% hit rate"
            (car stats)
            (cdr stats)
            (hash-cons-size)
            (exact->inexact (* 100 (hash-cons-hit-rate))))))
