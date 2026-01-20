(library (shell block-query)
         (export
          ;; Query execution
          query-blocks

          ;; Pattern constructors
          tag-is
          tag-matches
          payload-contains
          payload-matches
          refs-count
          has-ref

          ;; Combinators
          query-and
          query-or
          query-not)

         (import (chezscheme))

         (doc 'module 'block-query)
         (doc 'description "Query language for content-addressed blocks")
         (doc 'layer 'boundary)
         (doc 'purity 'partial)
         (doc 'note "string-contains? is provided by core/prelude.ss")

         (doc 'section 'patterns)
         (doc 'note "A query pattern is a predicate: Block → Boolean")
         (doc 'note "We represent queries as procedures for maximum flexibility")

         (define (tag-is expected-tag)
           (doc 'type (-> Symbol QueryPattern))
           (doc 'description "Match blocks with exact tag")
           (doc 'export #t)
           (lambda (blk)
                   (eq? (block-tag blk) expected-tag)))

         (define (tag-matches pattern)
           (doc 'type (-> String QueryPattern))
           (doc 'description "Match blocks whose tag matches regex pattern")
           (doc 'export #t)
           (let ([rx (pregexp pattern)])
                (lambda (blk)
                        (pregexp-match rx (symbol->string (block-tag blk))))))

         (define (payload-contains substring)
           (doc 'type (-> String QueryPattern))
           (doc 'description "Match blocks whose payload (as UTF-8) contains substring")
           (doc 'export #t)
           (lambda (blk)
                   (let ([payload-str (utf8->string (block-payload blk))])
                        (string-contains? payload-str substring))))

         (define (payload-matches pattern)
           (doc 'type (-> String QueryPattern))
           (doc 'description "Match blocks whose payload matches regex")
           (doc 'export #t)
           (let ([rx (pregexp pattern)])
                (lambda (blk)
                        (let ([payload-str (utf8->string (block-payload blk))])
                             (pregexp-match rx payload-str)))))

         (define (refs-count predicate)
           (doc 'type (-> (-> Nat Boolean) QueryPattern))
           (doc 'description "Match blocks based on reference count")
           (doc 'example "(refs-count (lambda (n) (> n 2)))")
           (doc 'example "(refs-count zero?)")
           (doc 'export #t)
           (lambda (blk)
                   (predicate (vector-length (block-refs blk)))))

         (define (has-ref target-hash)
           (doc 'type (-> Bytevector QueryPattern))
           (doc 'description "Match blocks that reference the given hash")
           (doc 'export #t)
           (lambda (blk)
                   (let ([refs (block-refs blk)])
                        (let loop ([i 0])
                             (cond
                              [(= i (vector-length refs)) #f]
                              [(bytevector=? (vector-ref refs i) target-hash) #t]
                              [else (loop (+ i 1))])))))

         (doc 'section 'combinators)

         (define (query-and . patterns)
           (doc 'type (-> QueryPattern ... QueryPattern))
           (doc 'description "Match blocks that satisfy ALL patterns")
           (doc 'export #t)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #t]
                         [((car ps) blk) (loop (cdr ps))]
                         [else #f]))))

         (define (query-or . patterns)
           (doc 'type (-> QueryPattern ... QueryPattern))
           (doc 'description "Match blocks that satisfy ANY pattern")
           (doc 'export #t)
           (lambda (blk)
                   (let loop ([ps patterns])
                        (cond
                         [(null? ps) #f]
                         [((car ps) blk) #t]
                         [else (loop (cdr ps))]))))

         (define (query-not pattern)
           (doc 'type (-> QueryPattern QueryPattern))
           (doc 'description "Match blocks that do NOT satisfy pattern")
           (doc 'export #t)
           (lambda (blk)
                   (not (pattern blk))))

         (doc 'section 'execution)

         (define (query-blocks pattern)
           (doc 'type (-> QueryPattern (List (Pair Bytevector Block))))
           (doc 'description "Execute query against all blocks in the store")
           (doc 'returns "List of (hash . block) pairs")
           (doc 'note "This requires access to the store, which is in core/cas.ss")
           (doc 'note "In practice, this would need to be called with store access")
           (doc 'note "FIX: Filters out #f results from fetch to prevent crashes")
           (doc 'export #t)
           ;; Assuming we have access to (store-hashes) and (fetch)
           (let ([all-hashes (store-hashes)])
                (filter
                 (lambda (hash-block-pair)
                         ;; FIX: Check if block is not #f before applying pattern
                         (let ([blk (cdr hash-block-pair)])
                              (and blk (pattern blk))))
                 (map (lambda (hash)
                              (cons hash (fetch hash)))
                      all-hashes))))

         (doc 'note "string-contains? provided by core/prelude.ss")
         (doc 'note "If this module is used standalone, ensure core/prelude.ss is loaded")
         )
