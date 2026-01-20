(load "core/base/prelude.ss")

(doc 'module 'hash-cons)
(doc 'description "Hash-consing for canonical S-expression construction. Enables fast equality and memory deduplication.")
(doc 'layer 'core)

(doc 'section 'global-table)

(doc *cons-table* 'type (Hashtable Any Any))
(doc *cons-table* 'description "Maps expressions to their unique canonical representatives.")
(define *cons-table* (make-hashtable equal-hash equal?))

(doc *cons-table-hits* 'type Nat)
(doc *cons-table-hits* 'description "Counter for cache hits (diagnostics).")
(define *cons-table-hits* 0)

(doc *cons-table-misses* 'type Nat)
(doc *cons-table-misses* 'description "Counter for cache misses (diagnostics).")
(define *cons-table-misses* 0)

(doc 'section 'core-operations)

(define (hash-cons x)
  (doc 'type (-> Any Any))
  (doc 'description "Return unique canonical representative for S-expression. Atoms pass through, pairs deduplicated.")
  (doc 'export #t)
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

(define (hash-cons-list xs)
  (doc 'type (-> (List Any) (List Any)))
  (doc 'description "Canonicalize a list of expressions.")
  (doc 'export #t)
  (hash-cons (map hash-cons xs)))

(doc 'section 'table-management)

(define (hash-cons-reset!)
  (doc 'type (-> Void))
  (doc 'description "Clear canonicalization table and reset counters.")
  (doc 'export #t)
  (set! *cons-table* (make-hashtable equal-hash equal?))
  (set! *cons-table-hits* 0)
  (set! *cons-table-misses* 0))

(define (hash-cons-stats)
  (doc 'type (-> (Pair Nat Nat)))
  (doc 'description "Return cache hit/miss statistics.")
  (doc 'export #t)
  (cons *cons-table-hits* *cons-table-misses*))

(define (hash-cons-size)
  (doc 'type (-> Nat))
  (doc 'description "Return number of entries in canonicalization table.")
  (doc 'export #t)
  (hashtable-size *cons-table*))

(define (hash-cons-hit-rate)
  (doc 'type (-> Number))
  (doc 'description "Return cache hit rate as a fraction [0, 1].")
  (doc 'export #t)
  (let ([total (+ *cons-table-hits* *cons-table-misses*)])
    (if (= total 0)
        0
        (/ *cons-table-hits* total))))

(doc 'section 'memoization)

(define (make-memoized f)
  (doc 'type (-> (-> Any Any) (-> Any Any)))
  (doc 'description "Wrap function with hash-consing on input/output and result caching.")
  (doc 'export #t)
  (let ([cache (make-hashtable eq-hash eq?)])  ; Use eq? since inputs are hash-consed
    (lambda (x)
      (let* ([canon-x (hash-cons x)]
             [cached (hashtable-ref cache canon-x #f)])
        (if cached
            cached
            (let ([result (hash-cons (f canon-x))])
              (hashtable-set! cache canon-x result)
              result))))))

(doc 'section 'diagnostics)

(define (hash-cons-report)
  (doc 'type (-> String))
  (doc 'description "Generate human-readable report of hash-consing statistics.")
  (doc 'export #t)
  (let ([stats (hash-cons-stats)])
    (format "Hash-cons stats: ~a hits, ~a misses, ~a entries, ~a% hit rate"
            (car stats)
            (cdr stats)
            (hash-cons-size)
            (exact->inexact (* 100 (hash-cons-hit-rate))))))
