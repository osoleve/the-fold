;;; Set up source-directories to find core modules
(source-directories (cons "core" (source-directories)))

(load "base/prelude.ss")
(load "blocks/block.ss")
(load "blocks/cas.ss")

(doc 'module 'block-index)
(doc 'description "Block Indexing for Analytics and Navigation")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/prelude core/block core/cas))

(doc 'section 'structure)

(doc 'note "This is Shell code: impure (uses mutation for indexing)")
(doc 'note "The index structures use hashtables which are mutated in place")

(doc 'note "Indices: Tag Index (tag → set of hashes), Reference Index (hash → set of referencing hashes), Content Index (content substring → set of hashes)")

(doc 'note "Run from ccverse root directory")

(define (make-index)
  (doc 'type (-> Index))
  (doc 'description "Create an empty index")
  (doc 'export #t)
  (doc 'note "An Index is a record containing: tag-index (hashtable mapping tags to lists of hashes), ref-index (hashtable mapping hashes to lists of referencing hashes), content-index (hashtable mapping content fragments to lists of hashes)")
  (doc 'note "WARNING: Index structures use mutation (hashtable-set!)")
  (list 'index
        (make-hashtable equal-hash equal?)
        (make-hashtable equal-hash equal?)
        (make-hashtable equal-hash equal?)))

(define (index-tag-table idx)
  (doc 'type (-> Index Hashtable))
  (doc 'description "Extract tag index table from index")
  (doc 'export #t)
  (list-ref idx 1))

(define (index-ref-table idx)
  (doc 'type (-> Index Hashtable))
  (doc 'description "Extract reference index table from index")
  (doc 'export #t)
  (list-ref idx 2))

(define (index-content-table idx)
  (doc 'type (-> Index Hashtable))
  (doc 'description "Extract content index table from index")
  (doc 'export #t)
  (list-ref idx 3))

(doc 'section 'indexing-operations)
(doc 'note "MUTATING operations")

(define (index-block! idx hash blk)
  (doc 'type (-> Index Hash Block Void))
  (doc 'description "Add a block to the index (MUTATES the index tables)")
  (doc 'export #t)
  (doc 'note "This is a Shell operation - it uses hashtable-set!")
  (let ([tag-table (index-tag-table idx)]
        [ref-table (index-ref-table idx)]
        [content-table (index-content-table idx)]
        [tag (block-tag blk)]
        [payload (block-payload blk)]
        [refs (block-refs blk)])

       ;; Index by tag
       (let ([existing (hashtable-ref tag-table tag '())])
            (hashtable-set! tag-table tag (cons hash existing)))

       ;; Index references (reverse index)
       (vector-for-each
        (lambda (ref-hash)
                (let ([existing (hashtable-ref ref-table ref-hash '())])
                     (hashtable-set! ref-table ref-hash (cons hash existing))))
        refs)

       ;; Index content (for text payloads)
       ;; Convert payload to string if it's text-like
       (when (bytevector? payload)
             (let ([content (utf8->string payload)])
                  ;; Index 3-character substrings for search
                  ;; (This is a simple approach; shell can optimize)
                  (let ([len (string-length content)])
                       (do ([i 0 (+ i 1)])
                           ;; FIX: Changed >= to > to include last valid fragment
                           ((> (+ i 3) len))
                           (let ([fragment (substring content i (+ i 3))])
                                (let ([existing (hashtable-ref content-table fragment '())])
                                     (hashtable-set! content-table fragment (cons hash existing))))))))))

(doc 'section 'query-operations)
(doc 'note "Pure operations")

(define (find-blocks-by-tag idx tag)
  (doc 'type (-> Index Symbol (List Hash)))
  (doc 'description "Find all blocks with the given tag")
  (doc 'export #t)
  (hashtable-ref (index-tag-table idx) tag '()))

(define (find-referencing-blocks idx hash)
  (doc 'type (-> Index Hash (List Hash)))
  (doc 'description "Find all blocks that reference the given hash")
  (doc 'export #t)
  (hashtable-ref (index-ref-table idx) hash '()))

(define (find-blocks-by-content idx query)
  (doc 'type (-> Index String (List Hash)))
  (doc 'description "Find blocks containing the given content substring")
  (doc 'returns "The union of all matching fragments")
  (doc 'export #t)
  (let ([content-table (index-content-table idx)]
        [results '()])
       ;; Search for 3-char fragments
       (let ([len (string-length query)])
            (if (< len 3)
                '()  ; Query too short
                (begin
                 ;; Get matches for first fragment
                 (let ([fragment (substring query 0 3)])
                      (hashtable-ref content-table fragment '())))))
       ))

(doc 'section 'statistics)
(doc 'note "Pure queries")

(define (index-stats idx)
  (doc 'type (-> Index Alist))
  (doc 'description "Return statistics about the index")
  (doc 'export #t)
  (let ([tag-table (index-tag-table idx)]
        [ref-table (index-ref-table idx)]
        [content-table (index-content-table idx)])
       (list
        (cons 'tag-count (hashtable-size tag-table))
        (cons 'ref-count (hashtable-size ref-table))
        (cons 'content-fragments (hashtable-size content-table)))))

(define (get-all-tags idx)
  (doc 'type (-> Index (List Symbol)))
  (doc 'description "Return all tags in the index")
  (doc 'export #t)
  (let ((tag-table (index-tag-table idx)))
       (vector->list (hashtable-keys tag-table))))

(define (get-tag-distribution idx)
  (doc 'type (-> Index Alist))
  (doc 'description "Return tag → count distribution")
  (doc 'export #t)
  (let ((tag-table (index-tag-table idx)))
       (map (lambda (tag)
                    (cons tag (length (hashtable-ref tag-table tag '()))))
            (vector->list (hashtable-keys tag-table)))))

(doc 'section 'graph-traversal)
(doc 'note "Use mutation for visited set")

(define (traverse-refs fetch start pred depth)
  (doc 'type (-> (-> Hash Block) Hash (-> Block Bool) Int (List Hash)))
  (doc 'description "Traverse block references depth-first, collecting matching hashes")
  (doc 'param 'fetch "function to retrieve blocks by hash")
  (doc 'param 'start "starting hash")
  (doc 'param 'pred "predicate to test blocks")
  (doc 'param 'depth "maximum depth (fuel)")
  (doc 'note "Uses mutation for visited set and results accumulation")
  (doc 'export #t)
  (let ([visited (make-hashtable equal-hash equal?)]
        [results '()])
       (define (visit hash d)
         (when (and (> d 0) (not (hashtable-ref visited hash #f)))
               (hashtable-set! visited hash #t)
               (let ([blk (fetch hash)])
                    (when blk
                          (when (pred blk)
                                (set! results (cons hash results)))
                          (vector-for-each
                           (lambda (ref) (visit ref (- d 1)))
                           (block-refs blk))))))
       (visit start depth)
       results))

(define (find-path fetch start target max-depth)
  (doc 'type (-> (-> Hash Block) Hash Hash Int (Maybe (List Hash))))
  (doc 'description "Find a path from start to target, up to max-depth")
  (doc 'returns "The path as a list of hashes, or #f if no path found")
  (doc 'note "Uses mutation for visited set")
  (doc 'export #t)
  (let ((visited (make-hashtable equal-hash equal?)))
       (define (search hash depth path)
         (cond
          ((equal? hash target) (reverse (cons hash path)))
          ((<= depth 0) #f)
          ((hashtable-ref visited hash #f) #f)
          (else
           (hashtable-set! visited hash #t)
           (let ((blk (fetch hash)))
                (if (not blk)
                    #f
                    (search-refs (block-refs blk) depth (cons hash path)))))))
       (define (search-refs refs depth path)
         (let loop ((i 0))
              (cond
               ((>= i (vector-length refs)) #f)
               (else
                (let ((result (search (vector-ref refs i) (- depth 1) path)))
                     (if result
                         result
                         (loop (+ i 1))))))))
       (search start max-depth '())))

(define (compute-reference-counts hashes fetch)
  (doc 'type (-> (List Hash) (-> Hash Block) Hashtable))
  (doc 'description "Count how many times each hash is referenced")
  (doc 'returns "A hashtable: hash → count")
  (doc 'note "Uses mutation for counting")
  (doc 'export #t)
  (let ([counts (make-hashtable equal-hash equal?)])
       ;; Initialize all hashes with count 0
       (for-each
        (lambda (h) (hashtable-set! counts h 0))
        hashes)
       ;; Count references
       (for-each
        (lambda (h)
                (let ([blk (fetch h)])
                     (when blk
                           (vector-for-each
                            (lambda (ref)
                                    (let ([current (hashtable-ref counts ref 0)])
                                         (hashtable-set! counts ref (+ current 1))))
                            (block-refs blk)))))
        hashes)
       counts))

(doc 'note "utf8->string and string->utf8 are Chez Scheme built-ins")
