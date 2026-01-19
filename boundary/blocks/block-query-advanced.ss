;;; boundary/blocks/block-query-advanced.ss — Advanced query capabilities
;;; Created by claude-sonnet-4
;;;
;;; Extended query features for content-addressed blocks:
;;;   - Time-based queries (with external metadata)
;;;   - Graph depth analysis
;;;   - Size-based filtering
;;;   - Higher-order query composition

(library (shell block-query-advanced)
         (export
          ;; Size-based queries
          query-size
          payload-size-bytes
          payload-size-between
          
          ;; Graph depth queries
          query-depth
          depth-exactly
          depth-at-least
          depth-at-most
          depth-between
          
          ;; Time-based queries (requires metadata)
          query-timestamp
          created-after
          created-before
          created-between
          
          ;; Higher-order query composition
          query-compose
          query-pipe
          query-all-of
          query-any-of
          query-none-of
          query-count
          query-limit
          query-map)
         
         (import (chezscheme))
         
         ;; Load dependencies
         ;; In actual use, this would import from (core block), (core cas), and (shell block-query)
         
         ;;; ====
         ;;; Size-based Queries
         ;;; ====
         
         ;;; query-size : (Nat → Boolean) → QueryPattern
         ;;; Match blocks based on payload size in bytes
         ;;; Examples: (query-size (lambda (n) (< n 1000)))
         ;;;           (query-size (lambda (n) (= n 0)))
         (define (query-size predicate)
           (lambda (blk)
                   (let ([size (bytevector-length (block-payload blk))])
                        (predicate size))))
         
         ;;; payload-size-bytes : Nat → QueryPattern
         ;;; Match blocks with exact payload size
         (define (payload-size-bytes n)
           (query-size (lambda (size) (= size n))))
         
         ;;; payload-size-between : Nat × Nat → QueryPattern
         ;;; Match blocks with payload size in range [min, max]
         (define (payload-size-between min-size max-size)
           (query-size (lambda (size)
                               (and (>= size min-size)
                                    (<= size max-size)))))
         
         ;;; ====
         ;;; Graph Depth Queries
         ;;; ====
         
         ;;; compute-depth : Block × (Hash → Block) → Nat
         ;;; Compute the maximum depth of the reference graph
         ;;; Depth is the longest path from this block to a leaf (block with no refs)
         ;;; Uses memoization to avoid recomputation
         (define (compute-depth blk fetch-fn)
           (let ([memo (make-eq-hashtable)])
                (let depth ([b blk] [visited '()])
                     (let* ([refs (block-refs b)]
                            [block-key (if (block? b)
                                           (bytevector->integer (block-hash b))
                                           0)])
                           ;; Check memo first (BUGFIX: actually use memoization)
                           (cond
                            [(hashtable-ref memo block-key #f)
                             => (lambda (cached) cached)]
                            [(= (vector-length refs) 0)
                             (hashtable-set! memo block-key 0)
                             0]
                            [else
                             (let loop ([i 0] [max-depth 0])
                                  (if (= i (vector-length refs))
                                      (let ([result (+ 1 max-depth)])
                                           (hashtable-set! memo block-key result)
                                           result)
                                      (let* ([ref-hash (vector-ref refs i)]
                                             [ref-hash-key (bytevector->integer ref-hash)])
                                            ;; Check for cycles
                                            (if (member ref-hash-key visited)
                                                (loop (+ i 1) max-depth)
                                                (let* ([ref-block (fetch-fn ref-hash)]
                                                       [ref-depth (depth ref-block (cons ref-hash-key visited))])
                                                      (loop (+ i 1) (max max-depth ref-depth)))))))])))))
         
         ;;; Helper: bytevector->integer
         ;;; Convert first 8 bytes of bytevector to integer for hashing
         (define (bytevector->integer bv)
           (if (< (bytevector-length bv) 8)
               0
               (bytevector-u64-ref bv 0 'little)))
         
         ;;; query-depth : (Nat → Boolean) × (Hash → Block) → QueryPattern
         ;;; Match blocks based on reference graph depth
         ;;; Requires a fetch function to resolve references
         ;;; Examples: (query-depth (lambda (d) (> d 3)) fetch)
         (define (query-depth predicate fetch-fn)
           (lambda (blk)
                   (let ([depth (compute-depth blk fetch-fn)])
                        (predicate depth))))
         
         ;;; depth-exactly : Nat × (Hash → Block) → QueryPattern
         ;;; Match blocks with exact reference depth
         (define (depth-exactly n fetch-fn)
           (query-depth (lambda (d) (= d n)) fetch-fn))
         
         ;;; depth-at-least : Nat × (Hash → Block) → QueryPattern
         ;;; Match blocks with minimum reference depth
         (define (depth-at-least n fetch-fn)
           (query-depth (lambda (d) (>= d n)) fetch-fn))
         
         ;;; depth-at-most : Nat × (Hash → Block) → QueryPattern
         ;;; Match blocks with maximum reference depth
         (define (depth-at-most n fetch-fn)
           (query-depth (lambda (d) (<= d n)) fetch-fn))
         
         ;;; depth-between : Nat × Nat × (Hash → Block) → QueryPattern
         ;;; Match blocks with depth in range [min, max]
         (define (depth-between min-depth max-depth fetch-fn)
           (query-depth (lambda (d)
                                (and (>= d min-depth)
                                     (<= d max-depth)))
                        fetch-fn))
         
         ;;; ====
         ;;; Time-based Queries (Metadata-dependent)
         ;;; ====
         
         ;;; These queries require external metadata that maps hashes to timestamps.
         ;;; The metadata is provided as a function: Hash → Timestamp (or #f if unknown)
         ;;; Timestamps are represented as integers (e.g., Unix epoch seconds)
         
         ;;; query-timestamp : (Timestamp → Boolean) × (Hash → Timestamp) → QueryPattern
         ;;; Match blocks based on creation timestamp from metadata
         ;;; The metadata-fn should return #f if timestamp is unknown
         (define (query-timestamp predicate metadata-fn)
           (lambda (blk)
                   ;; Need the hash to look up metadata
                   ;; This is a limitation: we need hash context
                   ;; For now, we'll assume the metadata-fn can work with the block
                   ;; In practice, query-blocks would need to pass hash information
                   (let ([timestamp (metadata-fn blk)])
                        (and timestamp (predicate timestamp)))))
         
         ;;; created-after : Timestamp × (Hash → Timestamp) → QueryPattern
         ;;; Match blocks created after given timestamp
         (define (created-after timestamp metadata-fn)
           (query-timestamp (lambda (t) (> t timestamp)) metadata-fn))
         
         ;;; created-before : Timestamp × (Hash → Timestamp) → QueryPattern
         ;;; Match blocks created before given timestamp
         (define (created-before timestamp metadata-fn)
           (query-timestamp (lambda (t) (< t timestamp)) metadata-fn))
         
         ;;; created-between : Timestamp × Timestamp × (Hash → Timestamp) → QueryPattern
         ;;; Match blocks created in time range [start, end]
         (define (created-between start-time end-time metadata-fn)
           (query-timestamp (lambda (t)
                                    (and (>= t start-time)
                                         (<= t end-time)))
                            metadata-fn))
         
         ;;; ====
         ;;; Higher-Order Query Composition
         ;;; ====
         
         ;;; query-compose : (QueryPattern → QueryPattern) ... → QueryPattern → QueryPattern
         ;;; Compose query transformers left-to-right
         ;;; Example: (query-compose (query-map extract-field) base-query)
         (define (query-compose . transformers)
           (lambda (base-pattern)
                   (fold-left (lambda (pattern transformer)
                                      (transformer pattern))
                              base-pattern
                              transformers)))
         
         ;;; query-pipe : QueryPattern × (QueryPattern → QueryPattern) ... → QueryPattern
         ;;; Apply a sequence of transformers to a base pattern
         ;;; Similar to compose but takes pattern first (more convenient for chaining)
         (define (query-pipe base-pattern . transformers)
           (fold-left (lambda (pattern transformer)
                              (transformer pattern))
                      base-pattern
                      transformers))
         
         ;;; query-all-of : (List QueryPattern) → QueryPattern
         ;;; Match blocks that satisfy ALL patterns (list version of query-and)
         (define (query-all-of patterns)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #t]
                         [((car ps) blk) (loop (cdr ps))]
                         [else #f]))))
         
         ;;; query-any-of : (List QueryPattern) → QueryPattern
         ;;; Match blocks that satisfy ANY pattern (list version of query-or)
         (define (query-any-of patterns)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #f]
                         [((car ps) blk) #t]
                         [else (loop (cdr ps))]))))
         
         ;;; query-none-of : (List QueryPattern) → QueryPattern
         ;;; Match blocks that satisfy NONE of the patterns
         (define (query-none-of patterns)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #t]
                         [((car ps) blk) #f]
                         [else (loop (cdr ps))]))))
         
         ;;; query-count : QueryPattern → (List Block) → Nat
         ;;; Count how many blocks satisfy the pattern
         (define (query-count pattern)
           (lambda (blocks)
                   (length (filter pattern blocks))))
         
         ;;; query-limit : Nat → (List Block) → (List Block)
         ;;; Limit results to first N blocks
         ;;; This is a result transformer, not a pattern
         (define (query-limit n)
           (lambda (blocks)
                   (let loop ([bs blocks] [count 0] [acc '()])
                        (cond
                         [(or (null? bs) (= count n)) (reverse acc)]
                         [else (loop (cdr bs) (+ count 1) (cons (car bs) acc))]))))
         
         ;;; query-map : (Block → Any) → (List Block) → (List Any)
         ;;; Transform query results by applying function to each block
         ;;; This is a result transformer, not a pattern
         (define (query-map transform-fn)
           (lambda (blocks)
                   (map transform-fn blocks))))
