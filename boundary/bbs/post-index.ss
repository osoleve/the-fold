(load "boundary/bbs/store.ss")
(load "boundary/io/atomic.ss")
(load "boundary/io/file-lock.ss")

(doc 'module 'bbs/post-index)
(doc 'description "BBS In-Memory Post Indices - Fast post lookups with disk-based cache")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Maintains in-memory indices:
  *bbs-posts*         - hashtable: id-string -> hash-bytevector (O(1) lookup)
  *bbs-posts-by-type* - hashtable: type -> (id ...)")
(doc 'note "Cache Strategy:
  - Save index with head-file count as version marker
  - On load: if count matches disk, use cache; else rebuild
  - Individual posts auto-refresh via post-index-hash on cache miss")
(doc 'note "Unlike issues, posts do NOT have dependencies (blockers/blocking).
This simplifies the index - no dependency tracking or dep persistence needed.
If post relationships are added in the future, add deps infrastructure here.")

(doc 'section 'counter-functions)
(doc 'note "Local copies to avoid circular dependency with posts.ss")

(define *post-counter-file* ".bbs/post-counter")

(define (post-read-counter)
  (doc 'type (-> Int))
  (doc 'description "Read current counter value (or 0 if not exists)")
  (guard (e [else 0])
    (if (file-exists? *post-counter-file*)
        (call-with-input-file *post-counter-file*
          (lambda (port)
            (let ([line (get-line port)])
              (string->number line))))
        0)))

(define (%post-write-counter! n)
  (doc 'type (-> Int Void))
  (doc 'description "INTERNAL: Write counter value to file (caller must hold lock)")
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (call-with-atomic-output-file *post-counter-file*
    (lambda (port)
      (put-string port (number->string n))
      (newline port))
    '(replace)))

(doc 'section 'index-cache)

(define *post-index-cache-file* ".bbs/post-index.cache")
(define *post-index-cache-version* 1)

(doc 'section 'id-normalization)

(define (post-normalize-id id)
  (doc 'type (-> (Or String Symbol) String))
  (doc 'description "Convert an ID to string form. Accepts both symbols and strings")
  (if (symbol? id) (symbol->string id) id))

(doc 'section 'global-state)

(doc *bbs-posts* 'type 'Hashtable)
(doc *bbs-posts* 'description "All posts: hashtable id-string -> hash-bytevector (O(1) lookup)")
(define *bbs-posts* (make-hashtable string-hash string=?))

(doc *bbs-posts-by-type* 'type 'Hashtable)
(doc *bbs-posts-by-type* 'description "Posts by type: hashtable type -> (id ...)")
(define *bbs-posts-by-type* (make-eq-hashtable))

(doc 'section 'index-cache-persistence)

(define (post-save-index-cache!)
  (doc 'type (-> Void))
  (doc 'description "Save the current index to disk cache")
  (doc 'note "Called after rebuild to speed up future startups; uses atomic write-then-rename to prevent corruption")
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (guard (e [else #f])  ; Silently fail - cache is optional
    (call-with-atomic-output-file *post-index-cache-file*
      (lambda (port)
        ;; Convert hashtable to alist with hex hashes for serialization
        (let ([posts-hex (post-hashtable-map *bbs-posts*
                           (lambda (id hash) (cons id (hash->hex hash))))]
              [by-type-alist (post-hashtable->alist *bbs-posts-by-type*)])
          (write
           `(post-index-cache
             (version ,*post-index-cache-version*)
             (head-count ,(hashtable-size *bbs-posts*))
             (posts ,posts-hex)
             (by-type ,by-type-alist))
           port)
          (newline port)))
      '(replace))))

(define (post-hashtable->alist ht)
  (doc 'type (-> Hashtable Alist))
  (doc 'description "Convert hashtable to association list for serialization")
  (let-values ([(keys vals) (hashtable-entries ht)])
    (let loop ([i 0] [acc '()])
      (if (>= i (vector-length keys))
          acc
          (loop (+ i 1)
                (cons (cons (vector-ref keys i) (vector-ref vals i)) acc))))))

(define (post-hashtable-map ht fn)
  (doc 'type (-> Hashtable (-> K V R) (List R)))
  (doc 'description "Map a function over hashtable entries, returning a list")
  (let-values ([(keys vals) (hashtable-entries ht)])
    (let loop ([i 0] [acc '()])
      (if (>= i (vector-length keys))
          (reverse acc)
          (loop (+ i 1)
                (cons (fn (vector-ref keys i) (vector-ref vals i)) acc))))))

(define (post-load-index-cache!)
  (doc 'type (-> Boolean))
  (doc 'description "Try to load index from disk cache")
  (doc 'returns "#t if cache was valid and loaded, #f otherwise")
  (guard (e [else #f])
    (and (file-exists? *post-index-cache-file*)
         (let ([data (call-with-input-file *post-index-cache-file* read)])
           (and (pair? data)
                (eq? (car data) 'post-index-cache)
                ;; Use cadr because structure is (key value) not (key . value)
                (let ([version (cadr (assq 'version (cdr data)))]
                      [head-count (cadr (assq 'head-count (cdr data)))]
                      [posts-hex (cadr (assq 'posts (cdr data)))]
                      [by-type-alist (cadr (assq 'by-type (cdr data)))])
                  ;; Validate cache version
                  (and (= version *post-index-cache-version*)
                       ;; Get actual disk heads for validation
                       (let ([disk-heads (post-list-disk-heads)])
                         (and (= head-count (length disk-heads))
                              ;; Cache is valid - restore state
                              (begin
                                ;; Restore posts hashtable (convert hex back to bytevector)
                                (set! *bbs-posts* (make-hashtable string-hash string=?))
                                (for-each
                                 (lambda (entry)
                                   (hashtable-set! *bbs-posts* (car entry) (hex->hash (cdr entry))))
                                 posts-hex)
                                ;; Restore by-type hashtable
                                (set! *bbs-posts-by-type* (make-eq-hashtable))
                                (for-each
                                 (lambda (entry)
                                   (hashtable-set! *bbs-posts-by-type* (car entry) (cdr entry)))
                                 by-type-alist)
                                ;; Sync counter from disk heads
                                (post-sync-counter-from-heads! disk-heads)
                                #t))))))))))

(define (post-list-disk-heads)
  (doc 'type (-> (List String)))
  (doc 'description "List all post IDs that have head files on disk")
  (doc 'note "Duplicate of post-list-heads but clearer naming for index context")
  (filter (lambda (id) (string-starts-with? id "post-"))
          (bbs-list-heads)))

(define (post-sync-counter-from-heads! ids)
  (doc 'type (-> (List String) Void))
  (doc 'description "Sync the post counter to be >= max ID found in heads")
  (doc 'note "Prevents ID collisions when cache is stale; uses file lock to prevent race condition")
  (let ([max-n 0])
    (for-each
     (lambda (id)
       ;; Parse "post-<base36>" to get the number
       (let* ([num-str (substring id 5 (string-length id))]  ; Skip "post-"
              [n (string->number num-str 36)])
         (when (and n (> n max-n))
           (set! max-n n))))
     ids)
    ;; Only update if we found a higher counter
    ;; Lock must be held for atomic read-compare-write
    (when (> max-n 0)
      (with-file-lock *post-counter-file*
        (lambda ()
          (when (> max-n (post-read-counter))
            (%post-write-counter! max-n)))))))

(doc 'section 'index-building)

(define (post-rebuild-indices!)
  (doc 'type (-> Int))
  (doc 'description "Rebuild all indices from head files")
  (doc 'returns "The number of posts indexed")
  (set! *bbs-posts* (make-hashtable string-hash string=?))
  (set! *bbs-posts-by-type* (make-eq-hashtable))

  (let* ([ids (post-list-disk-heads)]
         [count 0])
    ;; Sync counter to avoid ID collisions
    (post-sync-counter-from-heads! ids)

    ;; Index each post
    (for-each
     (lambda (id)
       (let ([hash (bbs-read-head id)])
         (when hash
           (let ([blk (bbs-fetch hash)])
             (when blk
               (let ([data (post-block-data blk)])
                 (when data
                   ;; Add to main index (O(1) hashtable insert)
                   (hashtable-set! *bbs-posts* id hash)
                   (set! count (+ count 1))

                   ;; Index by type
                   (let* ([post-type (cdr (assq 'post-type data))]
                          [existing (hashtable-ref *bbs-posts-by-type* post-type '())])
                     (hashtable-set! *bbs-posts-by-type* post-type (cons id existing))))))))))
     ids)

    ;; Save cache for future fast loads
    (post-save-index-cache!)

    count))

(doc 'section 'index-updates)

(define (post-index-add! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Add a new post to the index")
  (let ([blk (bbs-fetch hash)])
    (when blk
      (let ([data (post-block-data blk)])
        (when data
          ;; Add to main index (O(1) hashtable insert)
          (hashtable-set! *bbs-posts* id hash)

          ;; Index by type
          (let* ([post-type (cdr (assq 'post-type data))]
                 [existing (hashtable-ref *bbs-posts-by-type* post-type '())])
            (hashtable-set! *bbs-posts-by-type* post-type (cons id existing))))))))

(define (post-index-update! id new-hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Update a post in the index (type cannot change, so just update hash)")
  ;; Update main index (O(1) hashtable update)
  (hashtable-set! *bbs-posts* id new-hash))

(doc 'section 'index-queries)

(define (post-all-ids)
  (doc 'type (-> (List String)))
  (doc 'description "Get all post IDs as a list. O(n) to build list from hashtable keys")
  (vector->list (hashtable-keys *bbs-posts*)))

(define (post-all-posts)
  (doc 'type (-> (List (Pair String Bytevector))))
  (doc 'description "Get all posts as (id . hash) pairs (alist form for compatibility)")
  (post-hashtable-map *bbs-posts* cons))

(define (post-ids-by-type post-type)
  (doc 'type (-> Symbol (List String)))
  (doc 'description "Get post IDs with a given type")
  (hashtable-ref *bbs-posts-by-type* post-type '()))

(define (post-index-count)
  (doc 'type (-> Int))
  (doc 'description "Get total number of posts. O(1) with hashtable")
  (hashtable-size *bbs-posts*))

(define (post-index-exists? id)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if a post exists in the index. O(1) lookup")
  (let ([id-str (post-normalize-id id)])
    (hashtable-contains? *bbs-posts* id-str)))

(define (post-index-hash id)
  (doc 'type (-> (Or String Symbol) (Or Bytevector Boolean)))
  (doc 'description "Get the current hash for a post ID. O(1) lookup")
  (doc 'note "Auto-refreshes from disk if post not in index (handles cross-session creation)")
  (let* ([id-str (post-normalize-id id)]
         [hash (hashtable-ref *bbs-posts* id-str #f)])
    (if hash
        hash
        ;; Not in index - try loading from disk (auto-refresh on cache miss)
        (let ([disk-hash (bbs-read-head id-str)])
          (when disk-hash
            (post-index-from-disk! id-str disk-hash))
          disk-hash))))

(define (post-index-from-disk! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Load a single post into the index from its hash")
  (doc 'note "Used for auto-refresh when a post exists on disk but not in memory; guards against duplicate indexing")
  ;; Guard: skip if already indexed (prevents duplicates) - O(1) check
  (unless (hashtable-contains? *bbs-posts* id)
    (let ([blk (bbs-fetch hash)])
      (when blk
        (let ([data (post-block-data blk)])
          (when data
            ;; Add to main index (O(1) insert)
            (hashtable-set! *bbs-posts* id hash)
            ;; Index by type
            (let* ([post-type (cdr (assq 'post-type data))]
                   [existing (hashtable-ref *bbs-posts-by-type* post-type '())])
              (hashtable-set! *bbs-posts-by-type* post-type (cons id existing)))))))))

(doc 'section 'statistics)

(define (post-index-stats)
  (doc 'type (-> Alist))
  (doc 'description "Get statistics about the post database")
  `((total . ,(hashtable-size *bbs-posts*))
    (changelog . ,(length (post-ids-by-type 'changelog)))
    (note . ,(length (post-ids-by-type 'note)))
    (announcement . ,(length (post-ids-by-type 'announcement)))
    (session-summary . ,(length (post-ids-by-type 'session-summary)))))
