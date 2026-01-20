(source-directories (cons "core" (source-directories)))
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")

(doc 'module 'collection-utils)
(doc 'description "Collection Utilities for Block Collections — Higher-order functions for working with block collections. Collections are blocks with multiple refs - this library provides functional programming primitives for transforming and querying them")
(doc 'layer 'lattice)
(doc 'tier 5)
(doc 'purity 'partial)
(doc 'note "Most operations load blocks and apply functions (tier 5-6)")

(doc 'section 'general-list-utilities)

(define (foldr f init lst)
  (doc 'type '(-> (-> A B B) B (List A) B))
  (doc 'description "Right-associative fold over a list. Processes elements from right to left: (f x1 (f x2 (f x3 ... init)))")
  (if (null? lst)
      init
      (f (car lst) (foldr f init (cdr lst)))))

(doc 'section 'core-collection-operations)

(define (collection-hashes collection)
  (doc 'type '(-> Block (List Hash)))
  (doc 'description "Extract all hashes from a collection block")
  (let ([refs (block-refs collection)])
       (let loop ([i 0]
                  [result '()])
            (if (>= i (vector-length refs))
                (reverse result)
                (loop (+ i 1)
                      (cons (vector-ref refs i) result))))))

(define (collection-size collection)
  (doc 'type '(-> Block Integer))
  (doc 'description "Get the number of members in a collection")
  (vector-length (block-refs collection)))

(define (collection-empty? collection)
  (doc 'type '(-> Block Boolean))
  (doc 'description "Check if collection has no members")
  (= (vector-length (block-refs collection)) 0))

(doc 'section 'higher-order-collection-functions)

(define (map-collection fs f collection)
  (doc 'type '(-> FSCap (-> Block A) Block (List A)))
  (doc 'description "Map function over all members of a collection. Loads each member block and applies function")
  (let ([hashes (collection-hashes collection)])
       (map (lambda (h)
                    (let ([block (fs-fetch fs h)])
                         (if block
                             (f block)
                             (error 'map-collection "Block not found" h))))
            hashes)))

(define (filter-collection fs predicate collection)
  (doc 'type '(-> FSCap (-> Block Boolean) Block (List Block)))
  (doc 'description "Filter collection members by predicate. Returns list of blocks that match predicate")
  (let ([hashes (collection-hashes collection)])
       (filter (lambda (b) b)
               (map (lambda (h)
                            (let ([block (fs-fetch fs h)])
                                 (if (and block (predicate block))
                                     block
                                     #f)))
                    hashes))))

(define (fold-collection fs f init collection)
  (doc 'type '(-> FSCap (-> A Block A) A Block A))
  (doc 'description "Fold over collection members (left-to-right)")
  (let ([hashes (collection-hashes collection)])
       (fold-left (lambda (acc h)
                          (let ([block (fs-fetch fs h)])
                               (if block
                                   (f acc block)
                                   acc)))
                  init
                  hashes)))

(define (foldr-collection fs f init collection)
  (doc 'type '(-> FSCap (-> Block A A) A Block A))
  (doc 'description "Fold over collection members (right-to-left). Function signature is (Block A → A) to match foldr convention")
  (let ([hashes (collection-hashes collection)])
       (foldr (lambda (h acc)
                      (let ([block (fs-fetch fs h)])
                           (if block
                               (f block acc)
                               acc)))
              init
              hashes)))

(define (for-each-collection fs f collection)
  (doc 'type '(-> FSCap (-> Block Void) Block Void))
  (doc 'description "Execute function for each member (for side effects)")
  (let ([hashes (collection-hashes collection)])
       (for-each (lambda (h)
                         (let ([block (fs-fetch fs h)])
                              (when block (f block))))
                 hashes)))

(doc 'section 'collection-queries)

(define (collection-find fs predicate collection)
  (doc 'type '(-> FSCap (-> Block Boolean) Block (Maybe Block)))
  (doc 'description "Find first block in collection matching predicate")
  (let ([hashes (collection-hashes collection)])
       (let loop ([hashes hashes])
            (if (null? hashes)
                #f
                (let ([block (fs-fetch fs (car hashes))])
                     (if (and block (predicate block))
                         block
                         (loop (cdr hashes))))))))

(define (collection-any? fs predicate collection)
  (doc 'type '(-> FSCap (-> Block Boolean) Block Boolean))
  (doc 'description "Check if any member satisfies predicate")
  (not (eq? (collection-find fs predicate collection) #f)))

(define (collection-all? fs predicate collection)
  (doc 'type '(-> FSCap (-> Block Boolean) Block Boolean))
  (doc 'description "Check if all members satisfy predicate. Missing blocks are skipped for consistency with other collection functions")
  (let ([hashes (collection-hashes collection)])
       (let loop ([hashes hashes])
            (if (null? hashes)
                #t
                (let ([block (fs-fetch fs (car hashes))])
                     (if block
                         (if (predicate block)
                             (loop (cdr hashes))
                             #f)
                         (loop (cdr hashes))))))))

(define (collection-count-matching fs predicate collection)
  (doc 'type '(-> FSCap (-> Block Boolean) Block Integer))
  (doc 'description "Count how many members satisfy predicate")
  (length (filter-collection fs predicate collection)))

(doc 'section 'collection-transformation)

(define (collection-partition fs predicate collection)
  (doc 'type '(-> FSCap (-> Block Boolean) Block (Values (List Block) (List Block))))
  (doc 'description "Partition collection into (matching, non-matching)")
  (let ([hashes (collection-hashes collection)])
       (let loop ([hashes hashes]
                  [matching '()]
                  [non-matching '()])
            (if (null? hashes)
                (values (reverse matching) (reverse non-matching))
                (let ([block (fs-fetch fs (car hashes))])
                     (if block
                         (if (predicate block)
                             (loop (cdr hashes)
                                   (cons block matching)
                                   non-matching)
                             (loop (cdr hashes)
                                   matching
                                   (cons block non-matching)))
                         (loop (cdr hashes) matching non-matching)))))))

(define (collection-group-by fs key-fn collection)
  (doc 'type '(-> FSCap (-> Block A) Block (List (Pair A (List Block)))))
  (doc 'description "Group collection members by key function. Returns association list: ((key1 . [blocks...]) (key2 . [blocks...]) ...). Uses hash table internally for O(N) complexity instead of O(N*G)")
  (let ([groups (make-hashtable equal-hash equal?)])
       (fold-collection
        fs
        (lambda (acc block)
                (let* ([key (key-fn block)]
                       [existing (hashtable-ref groups key '())])
                      (hashtable-set! groups key (cons block existing)))
                acc)
        '() collection)
       (let-values ([(keys vals) (hashtable-entries groups)])
                   (map cons (vector->list keys) (vector->list vals)))))

(doc 'section 'collection-construction)

(define (make-collection-from-blocks tag name blocks)
  (doc 'type '(-> Symbol String (List Block) Block))
  (doc 'description "Create a collection from a list of blocks")
  (let ([hashes (map hash-block blocks)])
       (make-block tag
                   (string->utf8 (format "~a (~a members)" name (length blocks)))
                   (list->vector hashes))))

(define (collection-add collection new-member)
  (doc 'type '(-> Block Block Block))
  (doc 'description "Create new collection with additional member. Returns new collection block (original is immutable)")
  (let* ([old-refs (block-refs collection)]
         [new-hash (hash-block new-member)]
         [new-refs (list->vector (cons new-hash (vector->list old-refs)))])
        (make-block (block-tag collection)
                    (block-payload collection)
                    new-refs)))

(define (collection-remove collection member-hash)
  (doc 'type '(-> Block Hash Block))
  (doc 'description "Create new collection with member removed. Returns new collection block (original is immutable)")
  (let* ([old-refs (block-refs collection)]
         [new-refs (list->vector
                    (filter (lambda (h) (not (equal? h member-hash)))
                            (vector->list old-refs)))])
        (make-block (block-tag collection)
                    (block-payload collection)
                    new-refs)))

(define (collection-merge coll1 coll2)
  (doc 'type '(-> Block Block Block))
  (doc 'description "Merge two collections into one. Combines refs from both (may have duplicates)")
  (let* ([refs1 (block-refs coll1)]
         [refs2 (block-refs coll2)]
         [combined (append (vector->list refs1) (vector->list refs2))]
         [new-refs (list->vector combined)])
        (make-block (block-tag coll1)
                    (string->utf8 (format "merged (~a members)" (length combined)))
                    new-refs)))

(printf "✓ Collection utilities loaded\n")
