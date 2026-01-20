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

         (doc 'module 'block-query-advanced)
         (doc 'description "Advanced query capabilities for content-addressed blocks")
         (doc 'layer 'boundary)
         (doc 'purity 'partial)
         (doc 'note "Extended query features: time-based queries (with external metadata), graph depth analysis, size-based filtering, higher-order query composition")

         (doc 'section 'size-queries)

         (define (query-size predicate)
           (doc 'type (-> (-> Nat Boolean) QueryPattern))
           (doc 'description "Match blocks based on payload size in bytes")
           (doc 'example "(query-size (lambda (n) (< n 1000)))")
           (doc 'example "(query-size (lambda (n) (= n 0)))")
           (doc 'export #t)
           (lambda (blk)
                   (let ([size (bytevector-length (block-payload blk))])
                        (predicate size))))

         (define (payload-size-bytes n)
           (doc 'type (-> Nat QueryPattern))
           (doc 'description "Match blocks with exact payload size")
           (doc 'export #t)
           (query-size (lambda (size) (= size n))))

         (define (payload-size-between min-size max-size)
           (doc 'type (-> Nat Nat QueryPattern))
           (doc 'description "Match blocks with payload size in range [min, max]")
           (doc 'export #t)
           (query-size (lambda (size)
                               (and (>= size min-size)
                                    (<= size max-size)))))

         (doc 'section 'graph-depth)

         (define (compute-depth blk fetch-fn)
           (doc 'type (-> Block (-> Hash Block) Nat))
           (doc 'description "Compute the maximum depth of the reference graph")
           (doc 'note "Depth is the longest path from this block to a leaf (block with no refs)")
           (doc 'note "Uses memoization to avoid recomputation")
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

         (define (bytevector->integer bv)
           (doc 'type (-> Bytevector Integer))
           (doc 'description "Convert first 8 bytes of bytevector to integer for hashing")
           (if (< (bytevector-length bv) 8)
               0
               (bytevector-u64-ref bv 0 'little)))

         (define (query-depth predicate fetch-fn)
           (doc 'type (-> (-> Nat Boolean) (-> Hash Block) QueryPattern))
           (doc 'description "Match blocks based on reference graph depth")
           (doc 'note "Requires a fetch function to resolve references")
           (doc 'example "(query-depth (lambda (d) (> d 3)) fetch)")
           (doc 'export #t)
           (lambda (blk)
                   (let ([depth (compute-depth blk fetch-fn)])
                        (predicate depth))))

         (define (depth-exactly n fetch-fn)
           (doc 'type (-> Nat (-> Hash Block) QueryPattern))
           (doc 'description "Match blocks with exact reference depth")
           (doc 'export #t)
           (query-depth (lambda (d) (= d n)) fetch-fn))

         (define (depth-at-least n fetch-fn)
           (doc 'type (-> Nat (-> Hash Block) QueryPattern))
           (doc 'description "Match blocks with minimum reference depth")
           (doc 'export #t)
           (query-depth (lambda (d) (>= d n)) fetch-fn))

         (define (depth-at-most n fetch-fn)
           (doc 'type (-> Nat (-> Hash Block) QueryPattern))
           (doc 'description "Match blocks with maximum reference depth")
           (doc 'export #t)
           (query-depth (lambda (d) (<= d n)) fetch-fn))

         (define (depth-between min-depth max-depth fetch-fn)
           (doc 'type (-> Nat Nat (-> Hash Block) QueryPattern))
           (doc 'description "Match blocks with depth in range [min, max]")
           (doc 'export #t)
           (query-depth (lambda (d)
                                (and (>= d min-depth)
                                     (<= d max-depth)))
                        fetch-fn))

         (doc 'section 'time-queries)
         (doc 'note "These queries require external metadata that maps hashes to timestamps")
         (doc 'note "The metadata is provided as a function: Hash → Timestamp (or #f if unknown)")
         (doc 'note "Timestamps are represented as integers (e.g., Unix epoch seconds)")

         (define (query-timestamp predicate metadata-fn)
           (doc 'type (-> (-> Timestamp Boolean) (-> Hash Timestamp) QueryPattern))
           (doc 'description "Match blocks based on creation timestamp from metadata")
           (doc 'note "The metadata-fn should return #f if timestamp is unknown")
           (doc 'export #t)
           (lambda (blk)
                   ;; Need the hash to look up metadata
                   ;; This is a limitation: we need hash context
                   ;; For now, we'll assume the metadata-fn can work with the block
                   ;; In practice, query-blocks would need to pass hash information
                   (let ([timestamp (metadata-fn blk)])
                        (and timestamp (predicate timestamp)))))

         (define (created-after timestamp metadata-fn)
           (doc 'type (-> Timestamp (-> Hash Timestamp) QueryPattern))
           (doc 'description "Match blocks created after given timestamp")
           (doc 'export #t)
           (query-timestamp (lambda (t) (> t timestamp)) metadata-fn))

         (define (created-before timestamp metadata-fn)
           (doc 'type (-> Timestamp (-> Hash Timestamp) QueryPattern))
           (doc 'description "Match blocks created before given timestamp")
           (doc 'export #t)
           (query-timestamp (lambda (t) (< t timestamp)) metadata-fn))

         (define (created-between start-time end-time metadata-fn)
           (doc 'type (-> Timestamp Timestamp (-> Hash Timestamp) QueryPattern))
           (doc 'description "Match blocks created in time range [start, end]")
           (doc 'export #t)
           (query-timestamp (lambda (t)
                                    (and (>= t start-time)
                                         (<= t end-time)))
                            metadata-fn))

         (doc 'section 'composition)

         (define (query-compose . transformers)
           (doc 'type (-> (-> QueryPattern QueryPattern) ... (-> QueryPattern QueryPattern)))
           (doc 'description "Compose query transformers left-to-right")
           (doc 'example "(query-compose (query-map extract-field) base-query)")
           (doc 'export #t)
           (lambda (base-pattern)
                   (fold-left (lambda (pattern transformer)
                                      (transformer pattern))
                              base-pattern
                              transformers)))

         (define (query-pipe base-pattern . transformers)
           (doc 'type (-> QueryPattern (-> QueryPattern QueryPattern) ... QueryPattern))
           (doc 'description "Apply a sequence of transformers to a base pattern")
           (doc 'note "Similar to compose but takes pattern first (more convenient for chaining)")
           (doc 'export #t)
           (fold-left (lambda (pattern transformer)
                              (transformer pattern))
                      base-pattern
                      transformers))

         (define (query-all-of patterns)
           (doc 'type (-> (List QueryPattern) QueryPattern))
           (doc 'description "Match blocks that satisfy ALL patterns (list version of query-and)")
           (doc 'export #t)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #t]
                         [((car ps) blk) (loop (cdr ps))]
                         [else #f]))))

         (define (query-any-of patterns)
           (doc 'type (-> (List QueryPattern) QueryPattern))
           (doc 'description "Match blocks that satisfy ANY pattern (list version of query-or)")
           (doc 'export #t)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #f]
                         [((car ps) blk) #t]
                         [else (loop (cdr ps))]))))

         (define (query-none-of patterns)
           (doc 'type (-> (List QueryPattern) QueryPattern))
           (doc 'description "Match blocks that satisfy NONE of the patterns")
           (doc 'export #t)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #t]
                         [((car ps) blk) #f]
                         [else (loop (cdr ps))]))))

         (define (query-count pattern)
           (doc 'type (-> QueryPattern (-> (List Block) Nat)))
           (doc 'description "Count how many blocks satisfy the pattern")
           (doc 'export #t)
           (lambda (blocks)
                   (length (filter pattern blocks))))

         (define (query-limit n)
           (doc 'type (-> Nat (-> (List Block) (List Block))))
           (doc 'description "Limit results to first N blocks")
           (doc 'note "This is a result transformer, not a pattern")
           (doc 'export #t)
           (lambda (blocks)
                   (let loop ([bs blocks] [count 0] [acc '()])
                        (cond
                         [(or (null? bs) (= count n)) (reverse acc)]
                         [else (loop (cdr bs) (+ count 1) (cons (car bs) acc))]))))

         (define (query-map transform-fn)
           (doc 'type (-> (-> Block Any) (-> (List Block) (List Any))))
           (doc 'description "Transform query results by applying function to each block")
           (doc 'note "This is a result transformer, not a pattern")
           (doc 'export #t)
           (lambda (blocks)
                   (map transform-fn blocks))))
