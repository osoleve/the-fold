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
(doc 'note "Board-aware: mutation functions use board context when active.")

(doc 'section 'counter-functions)
(doc 'note "Local copies to avoid circular dependency with posts.ss")

(define *post-counter-file* ".bbs/post-counter")

(define (post-current-counter-file)
  (doc 'type (-> String))
  (doc 'description "Get the post counter file for current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-counter-file (current-board))
      *post-counter-file*))

(define (post-read-counter)
  (doc 'type (-> Int))
  (doc 'description "Read current counter value (or 0 if not exists). Board-aware.")
  (let ([cf (post-current-counter-file)])
    (guard (e [else 0])
      (if (file-exists? cf)
          (call-with-input-file cf
            (lambda (port)
              (let ([line (get-line port)])
                (string->number line))))
          0))))

(define (%post-write-counter! n)
  (doc 'type (-> Int Void))
  (doc 'description "INTERNAL: Write counter value to file (caller must hold lock). Board-aware.")
  (let ([cf (post-current-counter-file)])
    (unless (file-exists? ".bbs")
      (mkdir ".bbs"))
    (when (and (top-level-bound? 'board-context?)
               (board-context?))
      (board-ensure-dirs! (current-board)))
    (call-with-atomic-output-file cf
      (lambda (port)
        (put-string port (number->string n))
        (newline port))
      '(replace))))

(doc 'section 'index-cache)

(define *post-index-cache-file* ".bbs/post-index.cache")
(define *post-index-cache-version* 1)

(define (post-current-index-cache-file)
  (doc 'type (-> String))
  (doc 'description "Get the post index cache file for current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-index-cache-file (current-board))
      *post-index-cache-file*))

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

(doc 'section 'board-aware-helpers)

(define (post-current-items-ht)
  (doc 'type (-> Hashtable))
  (doc 'description "Get the items hashtable for current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-items-ht (current-board))
      *bbs-posts*))

(define (post-current-type-ht)
  (doc 'type (-> (Option Hashtable)))
  (doc 'description "Get the by-type hashtable for current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-by-type-ht (current-board))
      *bbs-posts-by-type*))

(doc 'section 'index-cache-persistence)

(define (post-save-index-cache!)
  (doc 'type (-> Void))
  (doc 'description "Save the current index to disk cache. Board-aware.")
  (let ([cache-file (post-current-index-cache-file)]
        [items (post-current-items-ht)]
        [types (post-current-type-ht)])
    (unless (file-exists? ".bbs")
      (mkdir ".bbs"))
    (when (and (top-level-bound? 'board-context?)
               (board-context?))
      (board-ensure-dirs! (current-board)))
    (guard (e [else #f])
      (call-with-atomic-output-file cache-file
        (lambda (port)
          (let ([posts-hex (post-hashtable-map items
                             (lambda (id hash) (cons id (hash->hex hash))))]
                [by-type-alist (if types (post-hashtable->alist types) '())])
            (write
             `(post-index-cache
               (version ,*post-index-cache-version*)
               (head-count ,(hashtable-size items))
               (posts ,posts-hex)
               (by-type ,by-type-alist))
             port)
            (newline port)))
        '(replace)))))

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
  (doc 'description "Try to load index from disk cache. Board-aware.")
  (doc 'returns "#t if cache was valid and loaded, #f otherwise")
  (let ([cache-file (post-current-index-cache-file)]
        [items (post-current-items-ht)]
        [types (post-current-type-ht)])
    (guard (e [else #f])
      (and (file-exists? cache-file)
           (let ([data (call-with-input-file cache-file read)])
             (and (pair? data)
                  (eq? (car data) 'post-index-cache)
                  (let ([version (cadr (assq 'version (cdr data)))]
                        [head-count (cadr (assq 'head-count (cdr data)))]
                        [posts-hex (cadr (assq 'posts (cdr data)))]
                        [by-type-alist (cadr (assq 'by-type (cdr data)))])
                    (and (= version *post-index-cache-version*)
                         (let ([disk-heads (post-list-disk-heads)])
                           (and (= head-count (length disk-heads))
                                (begin
                                  ;; Restore items hashtable
                                  (hashtable-clear! items)
                                  (for-each
                                   (lambda (entry)
                                     (hashtable-set! items (car entry) (hex->hash (cdr entry))))
                                   posts-hex)
                                  ;; Restore by-type hashtable
                                  (when types
                                    (hashtable-clear! types)
                                    (for-each
                                     (lambda (entry)
                                       (hashtable-set! types (car entry) (cdr entry)))
                                     by-type-alist))
                                  ;; Sync counter from disk heads
                                  (post-sync-counter-from-heads! disk-heads)
                                  #t)))))))))))

(define (post-list-disk-heads)
  (doc 'type (-> (List String)))
  (doc 'description "List all post IDs that have head files on disk. Board-aware.")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      ;; In board context, all heads belong to this board
      (bbs-list-heads)
      ;; Legacy: filter by prefix
      (filter (lambda (id) (string-starts-with? id "post-"))
              (bbs-list-heads))))

(define (post-sync-counter-from-heads! ids)
  (doc 'type (-> (List String) Void))
  (doc 'description "Sync the post counter to be >= max ID found in heads. Board-aware.")
  (let ([max-n 0]
        [cf (post-current-counter-file)])
    (for-each
     (lambda (id)
       ;; Parse "post-<base36>" to get the number
       (when (and (> (string-length id) 5)
                  (string=? (substring id 0 5) "post-"))
         (let* ([num-str (substring id 5 (string-length id))]
                [n (string->number num-str 36)])
           (when (and n (> n max-n))
             (set! max-n n)))))
     ids)
    (when (> max-n 0)
      (with-file-lock cf
        (lambda ()
          (when (> max-n (post-read-counter))
            (%post-write-counter! max-n)))))))

(doc 'section 'index-building)

(define (post-rebuild-indices!)
  (doc 'type (-> Int))
  (doc 'description "Rebuild all indices from head files. Board-aware.")
  (doc 'returns "The number of posts indexed")
  (let ([items (post-current-items-ht)]
        [types (post-current-type-ht)])
    (hashtable-clear! items)
    (when types (hashtable-clear! types))

    (let* ([ids (post-list-disk-heads)]
           [count 0])
      (post-sync-counter-from-heads! ids)

      (for-each
       (lambda (id)
         (let ([hash (bbs-read-head id)])
           (when hash
             (let ([blk (bbs-fetch hash)])
               (when blk
                 (let ([data (post-block-data blk)])
                   (when data
                     (hashtable-set! items id hash)
                     (set! count (+ count 1))
                     (when types
                       (let* ([post-type (cdr (assq 'post-type data))]
                              [existing (hashtable-ref types post-type '())])
                         (hashtable-set! types post-type (cons id existing)))))))))))
       ids)

      (post-save-index-cache!)
      count)))

(doc 'section 'index-updates)

(define (post-index-add! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Add a new post to the index. Board-aware.")
  (let ([items (post-current-items-ht)]
        [types (post-current-type-ht)])
    (let ([blk (bbs-fetch hash)])
      (when blk
        (let ([data (post-block-data blk)])
          (when data
            (hashtable-set! items id hash)
            (when types
              (let* ([post-type (cdr (assq 'post-type data))]
                     [existing (hashtable-ref types post-type '())])
                (hashtable-set! types post-type (cons id existing))))))))))

(define (post-index-update! id new-hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Update a post in the index. Board-aware.")
  (hashtable-set! (post-current-items-ht) id new-hash))

(doc 'section 'index-queries)

(define (post-all-ids)
  (doc 'type (-> (List String)))
  (doc 'description "Get all post IDs as a list")
  (vector->list (hashtable-keys (post-current-items-ht))))

(define (post-all-posts)
  (doc 'type (-> (List (Pair String Bytevector))))
  (doc 'description "Get all posts as (id . hash) pairs")
  (post-hashtable-map (post-current-items-ht) cons))

(define (post-ids-by-type post-type)
  (doc 'type (-> Symbol (List String)))
  (doc 'description "Get post IDs with a given type")
  (let ([ht (post-current-type-ht)])
    (if ht (hashtable-ref ht post-type '()) '())))

(define (post-index-count)
  (doc 'type (-> Int))
  (doc 'description "Get total number of posts. O(1) with hashtable")
  (hashtable-size (post-current-items-ht)))

(define (post-index-exists? id)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if a post exists in the index. O(1) lookup")
  (let ([id-str (post-normalize-id id)])
    (hashtable-contains? (post-current-items-ht) id-str)))

(define (post-index-hash id)
  (doc 'type (-> (Or String Symbol) (Or Bytevector Boolean)))
  (doc 'description "Get the current hash for a post ID. O(1) lookup")
  (let* ([id-str (post-normalize-id id)]
         [hash (hashtable-ref (post-current-items-ht) id-str #f)])
    (if hash
        hash
        (let ([disk-hash (bbs-read-head id-str)])
          (when disk-hash
            (post-index-from-disk! id-str disk-hash))
          disk-hash))))

(define (post-index-from-disk! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Load a single post into the index from its hash. Board-aware.")
  (let ([items (post-current-items-ht)]
        [types (post-current-type-ht)])
    (unless (hashtable-contains? items id)
      (let ([blk (bbs-fetch hash)])
        (when blk
          (let ([data (post-block-data blk)])
            (when data
              (hashtable-set! items id hash)
              (when types
                (let* ([post-type (cdr (assq 'post-type data))]
                       [existing (hashtable-ref types post-type '())])
                  (hashtable-set! types post-type (cons id existing)))))))))))

(doc 'section 'statistics)

(define (post-index-stats)
  (doc 'type (-> Alist))
  (doc 'description "Get statistics about the post database")
  `((total . ,(hashtable-size (post-current-items-ht)))
    (changelog . ,(length (post-ids-by-type 'changelog)))
    (note . ,(length (post-ids-by-type 'note)))
    (announcement . ,(length (post-ids-by-type 'announcement)))
    (session-summary . ,(length (post-ids-by-type 'session-summary)))))
