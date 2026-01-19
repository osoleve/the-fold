;;; boundary/blocks/block-index.ss — Block Indexing for Analytics and Navigation
;;;
;;; Mutable indexing structures for efficient block queries.
;;; Supports analytics and navigation tools in boundary/.
;;;
;;; This is Shell code: impure (uses mutation for indexing).
;;; The index structures use hashtables which are mutated in place.
;;;
;;; Indices:
;;;   - Tag Index: tag → set of hashes
;;;   - Reference Index: hash → set of referencing hashes (reverse index)
;;;   - Content Index: content substring → set of hashes
;;;
;;; Dependencies:
;;;   - core/prelude.ss
;;;   - core/block.ss
;;;   - core/cas.ss
;;;
;;; NOTE: Run from ccverse root directory.

;;; Set up source-directories to find core modules
(source-directories (cons "core" (source-directories)))

(load "base/prelude.ss")
(load "blocks/block.ss")
(load "blocks/cas.ss")

;;; ====
;;; Index Structure
;;; ====

;;; An Index is a record containing:
;;;   - tag-index: hashtable mapping tags to lists of hashes
;;;   - ref-index: hashtable mapping hashes to lists of referencing hashes
;;;   - content-index: hashtable mapping content fragments to lists of hashes
;;;
;;; WARNING: Index structures use mutation (hashtable-set!)

;;; make-index : -> Index
;;; Create an empty index.
(define (make-index)
  (list 'index
        (make-hashtable equal-hash equal?)
        (make-hashtable equal-hash equal?)
        (make-hashtable equal-hash equal?)))

;;; index-tag-table : Index -> Hashtable
(define (index-tag-table idx)
  (list-ref idx 1))

;;; index-ref-table : Index -> Hashtable
(define (index-ref-table idx)
  (list-ref idx 2))

;;; index-content-table : Index -> Hashtable
(define (index-content-table idx)
  (list-ref idx 3))

;;; ====
;;; Indexing Operations (MUTATING)
;;; ====

;;; index-block! : Index x Hash x Block -> void
;;; Add a block to the index (MUTATES the index tables).
;;; This is a Shell operation - it uses hashtable-set!
(define (index-block! idx hash blk)
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

;;; ====
;;; Query Operations (Pure)
;;; ====

;;; find-blocks-by-tag : Index x Symbol -> List[Hash]
;;; Find all blocks with the given tag.
(define (find-blocks-by-tag idx tag)
  (hashtable-ref (index-tag-table idx) tag '()))

;;; find-referencing-blocks : Index x Hash -> List[Hash]
;;; Find all blocks that reference the given hash.
(define (find-referencing-blocks idx hash)
  (hashtable-ref (index-ref-table idx) hash '()))

;;; find-blocks-by-content : Index x String -> List[Hash]
;;; Find blocks containing the given content substring.
;;; Returns the union of all matching fragments.
(define (find-blocks-by-content idx query)
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

;;; ====
;;; Index Statistics (Pure queries)
;;; ====

;;; index-stats : Index -> Alist
;;; Return statistics about the index.
(define (index-stats idx)
  (let ([tag-table (index-tag-table idx)]
        [ref-table (index-ref-table idx)]
        [content-table (index-content-table idx)])
       (list
        (cons 'tag-count (hashtable-size tag-table))
        (cons 'ref-count (hashtable-size ref-table))
        (cons 'content-fragments (hashtable-size content-table)))))

;;; get-all-tags : Index -> List[Symbol]
;;; Return all tags in the index.
(define (get-all-tags idx)
  (let ((tag-table (index-tag-table idx)))
       (vector->list (hashtable-keys tag-table))))

;;; get-tag-distribution : Index -> Alist
;;; Return tag -> count distribution.
(define (get-tag-distribution idx)
  (let ((tag-table (index-tag-table idx)))
       (map (lambda (tag)
                    (cons tag (length (hashtable-ref tag-table tag '()))))
            (vector->list (hashtable-keys tag-table)))))

;;; ====
;;; Graph Traversal Primitives (Use mutation for visited set)
;;; ====

;;; traverse-refs : (Hash -> Block) x Hash x (Block -> Bool) x Int -> List[Hash]
;;; Traverse block references depth-first, collecting matching hashes.
;;; - fetch: function to retrieve blocks by hash
;;; - start: starting hash
;;; - pred: predicate to test blocks
;;; - depth: maximum depth (fuel)
;;; NOTE: Uses mutation for visited set and results accumulation.
(define (traverse-refs fetch start pred depth)
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

;;; find-path : (Hash -> Block) x Hash x Hash x Int -> List[Hash] | #f
;;; Find a path from start to target, up to max-depth.
;;; Returns the path as a list of hashes, or #f if no path found.
;;; NOTE: Uses mutation for visited set.
(define (find-path fetch start target max-depth)
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

;;; compute-reference-counts : List[Hash] x (Hash -> Block) -> Hashtable
;;; Count how many times each hash is referenced.
;;; Returns a hashtable: hash -> count
;;; NOTE: Uses mutation for counting.
(define (compute-reference-counts hashes fetch)
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

;;; Note: utf8->string and string->utf8 are Chez Scheme built-ins.
