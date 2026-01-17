;;; shell/bbs/post-index.ss — BBS In-Memory Post Indices
;;;
;;; Maintains in-memory indices for fast post lookups.
;;; Uses a disk-based cache (.bbs/post-index.cache) to avoid expensive
;;; rebuilds on every session startup.
;;;
;;; Indices:
;;;   *bbs-posts*         - hashtable: id-string -> hash-bytevector (O(1) lookup)
;;;   *bbs-posts-by-type* - hashtable: type -> (id ...)
;;;
;;; Cache Strategy:
;;;   - Save index with head-file count as version marker
;;;   - On load: if count matches disk, use cache; else rebuild
;;;   - Individual posts auto-refresh via post-index-hash on cache miss
;;;
;;; This is Shell code: impure (maintains mutable state).

(load "shell/bbs/store.ss")
(load "shell/io/atomic.ss")

;;; ====
;;; Index Cache
;;; ====

(define *post-index-cache-file* ".bbs/post-index.cache")
(define *post-index-cache-version* 1)

;;; ====
;;; ID Normalization
;;; ====

;;; post-normalize-id : String|Symbol -> String
;;; Convert an ID to string form. Accepts both symbols and strings.
(define (post-normalize-id id)
  (if (symbol? id) (symbol->string id) id))

;;; ====
;;; Global State
;;; ====

;;; All posts: hashtable id-string -> hash-bytevector (O(1) lookup)
(define *bbs-posts* (make-hashtable string-hash string=?))

;;; Posts by type: hashtable type -> (id ...)
(define *bbs-posts-by-type* (make-eq-hashtable))

;;; ====
;;; Index Cache Persistence
;;; ====

;;; post-save-index-cache! : -> Void
;;; Save the current index to disk cache.
;;; Called after rebuild to speed up future startups.
;;; Uses atomic write-then-rename to prevent corruption.
(define (post-save-index-cache!)
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

;;; post-hashtable->alist : Hashtable -> Alist
;;; Convert hashtable to association list for serialization.
(define (post-hashtable->alist ht)
  (let-values ([(keys vals) (hashtable-entries ht)])
    (let loop ([i 0] [acc '()])
      (if (>= i (vector-length keys))
          acc
          (loop (+ i 1)
                (cons (cons (vector-ref keys i) (vector-ref vals i)) acc))))))

;;; post-hashtable-map : Hashtable (K V -> R) -> (List R)
;;; Map a function over hashtable entries, returning a list.
(define (post-hashtable-map ht fn)
  (let-values ([(keys vals) (hashtable-entries ht)])
    (let loop ([i 0] [acc '()])
      (if (>= i (vector-length keys))
          (reverse acc)
          (loop (+ i 1)
                (cons (fn (vector-ref keys i) (vector-ref vals i)) acc))))))

;;; post-load-index-cache! : -> Boolean
;;; Try to load index from disk cache.
;;; Returns #t if cache was valid and loaded, #f otherwise.
(define (post-load-index-cache!)
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

;;; post-list-disk-heads : -> (List String)
;;; List all post IDs that have head files on disk.
;;; (Duplicate of post-list-heads but clearer naming for index context)
(define (post-list-disk-heads)
  (filter (lambda (id) (string-starts-with? id "post-"))
          (bbs-list-heads)))

;;; string-starts-with? : String String -> Boolean
(define (string-starts-with? str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; post-sync-counter-from-heads! : (List String) -> Void
;;; Sync the post counter to be >= max ID found in heads.
;;; Prevents ID collisions when cache is stale.
(define (post-sync-counter-from-heads! ids)
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
    (when (> max-n (post-read-counter))
      (post-write-counter! max-n))))

;;; ====
;;; Index Building
;;; ====

;;; post-rebuild-indices! : -> Int
;;; Rebuild all indices from head files.
;;; Returns the number of posts indexed.
(define (post-rebuild-indices!)
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

;;; ====
;;; Index Updates
;;; ====

;;; post-index-add! : String Bytevector -> Void
;;; Add a new post to the index.
(define (post-index-add! id hash)
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

;;; post-index-update! : String Bytevector -> Void
;;; Update a post in the index (type cannot change, so just update hash).
(define (post-index-update! id new-hash)
  ;; Update main index (O(1) hashtable update)
  (hashtable-set! *bbs-posts* id new-hash))

;;; ====
;;; Index Queries
;;; ====

;;; post-all-ids : -> (List String)
;;; Get all post IDs as a list. O(n) to build list from hashtable keys.
(define (post-all-ids)
  (vector->list (hashtable-keys *bbs-posts*)))

;;; post-all-posts : -> (List (String . Bytevector))
;;; Get all posts as (id . hash) pairs (alist form for compatibility).
(define (post-all-posts)
  (post-hashtable-map *bbs-posts* cons))

;;; post-ids-by-type : Symbol -> (List String)
;;; Get post IDs with a given type.
(define (post-ids-by-type post-type)
  (hashtable-ref *bbs-posts-by-type* post-type '()))

;;; post-index-count : -> Int
;;; Get total number of posts. O(1) with hashtable.
(define (post-index-count)
  (hashtable-size *bbs-posts*))

;;; post-index-exists? : String -> Boolean
;;; Check if a post exists in the index. O(1) lookup.
(define (post-index-exists? id)
  (let ([id-str (post-normalize-id id)])
    (hashtable-contains? *bbs-posts* id-str)))

;;; post-index-hash : String|Symbol -> Bytevector | #f
;;; Get the current hash for a post ID. O(1) lookup.
;;; Auto-refreshes from disk if post not in index (handles cross-session creation).
(define (post-index-hash id)
  (let* ([id-str (post-normalize-id id)]
         [hash (hashtable-ref *bbs-posts* id-str #f)])
    (if hash
        hash
        ;; Not in index - try loading from disk (auto-refresh on cache miss)
        (let ([disk-hash (bbs-read-head id-str)])
          (when disk-hash
            (post-index-from-disk! id-str disk-hash))
          disk-hash))))

;;; post-index-from-disk! : String Bytevector -> Void
;;; Load a single post into the index from its hash.
;;; Used for auto-refresh when a post exists on disk but not in memory.
;;; Guards against duplicate indexing (e.g., from concurrent calls).
(define (post-index-from-disk! id hash)
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

;;; ====
;;; Statistics
;;; ====

;;; post-index-stats : -> Alist
;;; Get statistics about the post database.
(define (post-index-stats)
  `((total . ,(hashtable-size *bbs-posts*))
    (changelog . ,(length (post-ids-by-type 'changelog)))
    (note . ,(length (post-ids-by-type 'note)))
    (announcement . ,(length (post-ids-by-type 'announcement)))
    (session-summary . ,(length (post-ids-by-type 'session-summary)))))
