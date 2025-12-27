;;; collection-utils.ss — Collection Utilities for Block Collections
;;;
;;; Higher-order functions for working with block collections.
;;; Collections are blocks with multiple refs - this library provides
;;; functional programming primitives for transforming and querying them.
;;;
;;; TIER ASSIGNMENT:
;;;   Tier 5-6: Most operations (load blocks, apply functions)

(source-directories (cons "fabric/stitches" (source-directories)))
(load "fabric/stitches/block.ss")
(load "fabric/stitches/sha256.ss")

;;; ============================================================
;;; Core Collection Operations (Tier 5-6)
;;; ============================================================

;;; collection-hashes : Block → (List Hash)
;;; Extract all hashes from a collection block.
(define (collection-hashes collection)
  (let ([refs (block-refs collection)])
    (let loop ([i 0]
               [result '()])
      (if (>= i (vector-length refs))
          (reverse result)
          (loop (+ i 1)
                (cons (vector-ref refs i) result))))))

;;; collection-size : Block → Integer
;;; Get the number of members in a collection.
(define (collection-size collection)
  (vector-length (block-refs collection)))

;;; collection-empty? : Block → Boolean
;;; Check if collection has no members.
(define (collection-empty? collection)
  (= (vector-length (block-refs collection)) 0))

;;; ============================================================
;;; Higher-Order Collection Functions (Tier 6)
;;; ============================================================

;;; map-collection : FSCap (Block → A) Block → (List A)
;;; Map function over all members of a collection.
;;; Loads each member block and applies function.
(define (map-collection fs f collection)
  (let ([hashes (collection-hashes collection)])
    (map (lambda (h)
           (let ([block (fs-fetch fs h)])
             (if block
                 (f block)
                 (error 'map-collection "Block not found" h))))
         hashes)))

;;; filter-collection : FSCap (Block → Boolean) Block → (List Block)
;;; Filter collection members by predicate.
;;; Returns list of blocks that match predicate.
(define (filter-collection fs predicate collection)
  (let ([hashes (collection-hashes collection)])
    (filter (lambda (b) b)  ; Remove #f values
            (map (lambda (h)
                   (let ([block (fs-fetch fs h)])
                     (if (and block (predicate block))
                         block
                         #f)))
                 hashes))))

;;; fold-collection : FSCap (A Block → A) A Block → A
;;; Fold over collection members.
(define (fold-collection fs f init collection)
  (let ([hashes (collection-hashes collection)])
    (fold-left (lambda (acc h)
                 (let ([block (fs-fetch fs h)])
                   (if block
                       (f acc block)
                       acc)))
               init
               hashes)))

;;; for-each-collection : FSCap (Block → Void) Block → Void
;;; Execute function for each member (for side effects).
(define (for-each-collection fs f collection)
  (let ([hashes (collection-hashes collection)])
    (for-each (lambda (h)
                (let ([block (fs-fetch fs h)])
                  (when block (f block))))
              hashes)))

;;; ============================================================
;;; Collection Queries (Tier 6)
;;; ============================================================

;;; collection-find : FSCap (Block → Boolean) Block → (Maybe Block)
;;; Find first block in collection matching predicate.
(define (collection-find fs predicate collection)
  (let ([hashes (collection-hashes collection)])
    (let loop ([hashes hashes])
      (if (null? hashes)
          #f
          (let ([block (fs-fetch fs (car hashes))])
            (if (and block (predicate block))
                block
                (loop (cdr hashes))))))))

;;; collection-any? : FSCap (Block → Boolean) Block → Boolean
;;; Check if any member satisfies predicate.
(define (collection-any? fs predicate collection)
  (not (eq? (collection-find fs predicate collection) #f)))

;;; collection-all? : FSCap (Block → Boolean) Block → Boolean
;;; Check if all members satisfy predicate.
(define (collection-all? fs predicate collection)
  (let ([hashes (collection-hashes collection)])
    (let loop ([hashes hashes])
      (if (null? hashes)
          #t
          (let ([block (fs-fetch fs (car hashes))])
            (if (and block (predicate block))
                (loop (cdr hashes))
                #f))))))

;;; collection-count-matching : FSCap (Block → Boolean) Block → Integer
;;; Count how many members satisfy predicate.
(define (collection-count-matching fs predicate collection)
  (length (filter-collection fs predicate collection)))

;;; ============================================================
;;; Collection Transformation (Tier 6)
;;; ============================================================

;;; collection-partition : FSCap (Block → Boolean) Block → (Values (List Block) (List Block))
;;; Partition collection into (matching, non-matching).
(define (collection-partition fs predicate collection)
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

;;; collection-group-by : FSCap (Block → A) Block → (List (Pair A (List Block)))
;;; Group collection members by key function.
;;; Returns association list: ((key1 . [blocks...]) (key2 . [blocks...]) ...)
(define (collection-group-by fs key-fn collection)
  (let ([hashes (collection-hashes collection)])
    (let loop ([hashes hashes]
               [groups '()])
      (if (null? hashes)
          groups
          (let ([block (fs-fetch fs (car hashes))])
            (if block
                (let* ([key (key-fn block)]
                       [existing (assoc key groups)])
                  (if existing
                      (loop (cdr hashes)
                            (map (lambda (pair)
                                   (if (equal? (car pair) key)
                                       (cons key (cons block (cdr pair)))
                                       pair))
                                 groups))
                      (loop (cdr hashes)
                            (cons (cons key (list block)) groups))))
                (loop (cdr hashes) groups)))))))

;;; ============================================================
;;; Collection Construction (Tier 5)
;;; ============================================================

;;; make-collection-from-blocks : Symbol String (List Block) → Block
;;; Create a collection from a list of blocks.
(define (make-collection-from-blocks tag name blocks)
  (let ([hashes (map hash-block blocks)])
    (make-block tag
                (string->utf8 (format "~a (~a members)" name (length blocks)))
                (list->vector hashes))))

;;; collection-add : Block Block → Block
;;; Create new collection with additional member.
;;; Returns new collection block (original is immutable).
(define (collection-add collection new-member)
  (let* ([old-refs (block-refs collection)]
         [new-hash (hash-block new-member)]
         [new-refs (list->vector (cons new-hash (vector->list old-refs)))])
    (make-block (block-tag collection)
                (block-payload collection)
                new-refs)))

;;; collection-remove : Block Hash → Block
;;; Create new collection with member removed.
;;; Returns new collection block (original is immutable).
(define (collection-remove collection member-hash)
  (let* ([old-refs (block-refs collection)]
         [new-refs (list->vector
                    (filter (lambda (h) (not (equal? h member-hash)))
                            (vector->list old-refs)))])
    (make-block (block-tag collection)
                (block-payload collection)
                new-refs)))

;;; collection-merge : Block Block → Block
;;; Merge two collections into one.
;;; Combines refs from both (may have duplicates).
(define (collection-merge coll1 coll2)
  (let* ([refs1 (block-refs coll1)]
         [refs2 (block-refs coll2)]
         [combined (append (vector->list refs1) (vector->list refs2))]
         [new-refs (list->vector combined)])
    (make-block (block-tag coll1)
                (string->utf8 (format "merged (~a members)" (length combined)))
                new-refs)))

(printf "✓ Collection utilities loaded\n")
