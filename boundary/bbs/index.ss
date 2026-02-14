(load "boundary/bbs/store.ss")
(load "boundary/bbs/counter.ss")
(load "boundary/io/atomic.ss")

(doc 'module 'bbs/index)
(doc 'description "BBS In-Memory Indices - Fast issue lookups with disk-based cache")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Maintains in-memory indices:
  *bbs-issues*     - hashtable: id-string -> hash-bytevector (O(1) lookup)
  *bbs-by-status*  - hashtable: status -> (id ...)
  *bbs-by-priority* - hashtable: priority -> (id ...)
  *bbs-deps*       - ((blocker-id . blocked-id) ...)")
(doc 'note "Cache Strategy:
  - Save index with head-file count as version marker
  - On load: if count matches disk, use cache; else rebuild
  - Individual issues auto-refresh via bbs-issue-hash on cache miss")

(doc 'section 'index-cache)

(define *bbs-index-cache-file* ".bbs/index.cache")
(define *bbs-index-cache-version* 1)

(doc 'section 'id-normalization)

(define (normalize-id id)
  (doc 'type (-> (Or String Symbol) String))
  (doc 'description "Convert an ID to string form. Accepts both symbols and strings")
  (if (symbol? id) (symbol->string id) id))

(doc 'section 'global-state)

(doc *bbs-issues* 'type 'Hashtable)
(doc *bbs-issues* 'description "All issues: hashtable id-string -> hash-bytevector (O(1) lookup)")
(define *bbs-issues* (make-hashtable string-hash string=?))

(doc *bbs-by-status* 'type 'Hashtable)
(doc *bbs-by-status* 'description "Issues by status: hashtable status -> (id ...)")
(define *bbs-by-status* (make-eq-hashtable))

(doc *bbs-by-priority* 'type 'Hashtable)
(doc *bbs-by-priority* 'description "Issues by priority: hashtable priority -> (id ...)")
(define *bbs-by-priority* (make-eqv-hashtable))

(doc *bbs-deps* 'type '(List (Pair String String)))
(doc *bbs-deps* 'description "Dependencies: ((blocker-id . blocked-id) ...)")
(define *bbs-deps* '())

(doc 'section 'index-cache-persistence)

(define (bbs-save-index-cache!)
  (doc 'type (-> Void))
  (doc 'description "Save the current index to disk cache")
  (doc 'note "Called after rebuild to speed up future startups")
  (doc 'note "Uses atomic write-then-rename to prevent corruption")
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (guard (e [else #f])  ; Silently fail - cache is optional
    (call-with-atomic-output-file *bbs-index-cache-file*
      (lambda (port)
        ;; Convert hashtable to alist with hex hashes for serialization
        (let ([issues-hex (hashtable-map *bbs-issues*
                            (lambda (id hash) (cons id (hash->hex hash))))]
              [by-status-alist (hashtable->alist *bbs-by-status*)]
              [by-priority-alist (hashtable->alist *bbs-by-priority*)])
          (write
           `(bbs-index-cache
             (version ,*bbs-index-cache-version*)
             (head-count ,(hashtable-size *bbs-issues*))
             (issues ,issues-hex)
             (by-status ,by-status-alist)
             (by-priority ,by-priority-alist))
           port)
          (newline port)))
      '(replace))))

(define (hashtable->alist ht)
  (doc 'type (-> Hashtable Alist))
  (doc 'description "Convert hashtable to association list for serialization")
  (let-values ([(keys vals) (hashtable-entries ht)])
    (let loop ([i 0] [acc '()])
      (if (>= i (vector-length keys))
          acc
          (loop (+ i 1)
                (cons (cons (vector-ref keys i) (vector-ref vals i)) acc))))))

(define (hashtable-map ht fn)
  (doc 'type (-> Hashtable (-> K V R) (List R)))
  (doc 'description "Map a function over hashtable entries, returning a list")
  (let-values ([(keys vals) (hashtable-entries ht)])
    (let loop ([i 0] [acc '()])
      (if (>= i (vector-length keys))
          (reverse acc)
          (loop (+ i 1)
                (cons (fn (vector-ref keys i) (vector-ref vals i)) acc))))))

(define (string-set-equal? lst1 lst2)
  (doc 'type (-> (List String) (List String) Boolean))
  (doc 'description "Check if two lists contain the same strings (set equality)")
  (let ([sorted1 (list-sort string<? lst1)]
        [sorted2 (list-sort string<? lst2)])
    (equal? sorted1 sorted2)))

(define (bbs-load-index-cache!)
  (doc 'type (-> Boolean))
  (doc 'description "Try to load index from disk cache")
  (doc 'returns "#t if cache was valid and loaded, #f otherwise")
  (guard (e [else #f])
    (and (file-exists? *bbs-index-cache-file*)
         (let ([data (call-with-input-file *bbs-index-cache-file* read)])
           (and (pair? data)
                (eq? (car data) 'bbs-index-cache)
                ;; Use cadr because structure is (key value) not (key . value)
                (let ([version (cadr (assq 'version (cdr data)))]
                      [head-count (cadr (assq 'head-count (cdr data)))]
                      [issues-hex (cadr (assq 'issues (cdr data)))]
                      [by-status-alist (cadr (assq 'by-status (cdr data)))]
                      [by-priority-alist (cadr (assq 'by-priority (cdr data)))])
                  ;; Validate cache version
                  (and (= version *bbs-index-cache-version*)
                       ;; Get actual disk heads for validation and counter sync
                       ;; (Gemini QA: use disk heads, not cached IDs, for counter sync)
                       ;; Use issue-only heads to avoid counting posts in validation
                       (let* ([disk-heads (bbs-list-issue-heads)]
                              [cached-ids (map car issues-hex)])
                         ;; Validate by ID set comparison, not just count
                         ;; Catches: deletions, additions, renames, corruption
                         (and (string-set-equal? cached-ids disk-heads)
                              ;; Cache is valid - restore state
                              (begin
                                ;; Restore issues hashtable (convert hex back to bytevector)
                                (set! *bbs-issues* (make-hashtable string-hash string=?))
                                (for-each
                                 (lambda (entry)
                                   (hashtable-set! *bbs-issues* (car entry) (hex->hash (cdr entry))))
                                 issues-hex)
                                ;; Restore by-status hashtable
                                (set! *bbs-by-status* (make-eq-hashtable))
                                (for-each
                                 (lambda (entry)
                                   (hashtable-set! *bbs-by-status* (car entry) (cdr entry)))
                                 by-status-alist)
                                ;; Restore by-priority hashtable
                                (set! *bbs-by-priority* (make-eqv-hashtable))
                                (for-each
                                 (lambda (entry)
                                   (hashtable-set! *bbs-by-priority* (car entry) (cdr entry)))
                                 by-priority-alist)
                                ;; CRITICAL: Sync counter from DISK heads, not cache
                                ;; This prevents ID collisions if cache is stale
                                (bbs-sync-counter-from-heads! disk-heads)
                                ;; Load dependencies (always from disk, not cached)
                                (bbs-load-deps!)
                                #t))))))))))

(doc 'section 'index-building)

(define (bbs-rebuild-indices!)
  (doc 'type (-> Int))
  (doc 'description "Rebuild all indices from head files")
  (doc 'returns "The number of issues indexed")
  (set! *bbs-issues* (make-hashtable string-hash string=?))
  (set! *bbs-by-status* (make-eq-hashtable))
  (set! *bbs-by-priority* (make-eqv-hashtable))
  (set! *bbs-deps* '())

  (let* ([ids (bbs-list-issue-heads)]
         [count 0])
    ;; Sync counter to avoid ID collisions (issue heads only)
    (bbs-sync-counter-from-heads! ids)

    ;; Index each issue
    (for-each
     (lambda (id)
       (let ([hash (bbs-read-head id)])
         (when hash
           (let ([blk (bbs-fetch hash)])
             (when blk
               (let ([data (issue-block-data blk)])
                 (when data
                   ;; Add to main index (O(1) hashtable insert)
                   (hashtable-set! *bbs-issues* id hash)
                   (set! count (+ count 1))

                   ;; Index by status
                   (let* ([status (cdr (assq 'status data))]
                          [existing (hashtable-ref *bbs-by-status* status '())])
                     (hashtable-set! *bbs-by-status* status (cons id existing)))

                   ;; Index by priority
                   (let* ([priority (cdr (assq 'priority data))]
                          [existing (hashtable-ref *bbs-by-priority* priority '())])
                     (hashtable-set! *bbs-by-priority* priority (cons id existing))))))))))
     ids)

    ;; Load dependencies from disk
    (bbs-load-deps!)

    ;; Save cache for future fast loads
    (bbs-save-index-cache!)

    count))

(doc 'section 'index-updates)

(define (bbs-index-add! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Add a new issue to the index")
  (let ([blk (bbs-fetch hash)])
    (when blk
      (let ([data (issue-block-data blk)])
        (when data
          ;; Add to main index (O(1) hashtable insert)
          (hashtable-set! *bbs-issues* id hash)

          ;; Index by status
          (let* ([status (cdr (assq 'status data))]
                 [existing (hashtable-ref *bbs-by-status* status '())])
            (hashtable-set! *bbs-by-status* status (cons id existing)))

          ;; Index by priority
          (let* ([priority (cdr (assq 'priority data))]
                 [existing (hashtable-ref *bbs-by-priority* priority '())])
            (hashtable-set! *bbs-by-priority* priority (cons id existing)))

          ;; Persist cache so other processes see the change
          (bbs-save-index-cache!))))))

(define (bbs-index-update! id new-hash old-status new-status old-priority new-priority)
  (doc 'type (-> String Bytevector Symbol Symbol Symbol Symbol Void))
  (doc 'description "Update an issue in the index (handles status/priority changes)")
  ;; Update main index (O(1) hashtable update - replaces filter+cons)
  (hashtable-set! *bbs-issues* id new-hash)

  ;; Update status index if changed
  (unless (eq? old-status new-status)
    ;; Remove from old status
    (let ([old-list (hashtable-ref *bbs-by-status* old-status '())])
      (hashtable-set! *bbs-by-status* old-status
                      (filter (lambda (x) (not (string=? x id))) old-list)))
    ;; Add to new status
    (let ([new-list (hashtable-ref *bbs-by-status* new-status '())])
      (hashtable-set! *bbs-by-status* new-status (cons id new-list))))

  ;; Update priority index if changed
  (unless (equal? old-priority new-priority)
    ;; Remove from old priority
    (let ([old-list (hashtable-ref *bbs-by-priority* old-priority '())])
      (hashtable-set! *bbs-by-priority* old-priority
                      (filter (lambda (x) (not (string=? x id))) old-list)))
    ;; Add to new priority
    (let ([new-list (hashtable-ref *bbs-by-priority* new-priority '())])
      (hashtable-set! *bbs-by-priority* new-priority (cons id new-list))))

  ;; Persist cache so other processes see the change
  (bbs-save-index-cache!))

(doc 'section 'index-queries)

(define (bbs-all-ids)
  (doc 'type (-> (List String)))
  (doc 'description "Get all issue IDs as a list. O(n) to build list from hashtable keys")
  (vector->list (hashtable-keys *bbs-issues*)))

(define (bbs-all-issues)
  (doc 'type (-> (List (Pair String Bytevector))))
  (doc 'description "Get all issues as (id . hash) pairs (alist form for compatibility)")
  (hashtable-map *bbs-issues* cons))

(define (bbs-issues-by-status status)
  (doc 'type (-> Symbol (List String)))
  (doc 'description "Get issue IDs with a given status")
  (hashtable-ref *bbs-by-status* status '()))

(define (bbs-issues-by-priority priority)
  (doc 'type (-> Int (List String)))
  (doc 'description "Get issue IDs with a given priority")
  (hashtable-ref *bbs-by-priority* priority '()))

(define (bbs-issue-count)
  (doc 'type (-> Int))
  (doc 'description "Get total number of issues. O(1) with hashtable")
  (hashtable-size *bbs-issues*))

(define (bbs-issue-exists? id)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if an issue exists in the index. O(1) lookup")
  (let ([id-str (normalize-id id)])
    (hashtable-contains? *bbs-issues* id-str)))

(define (bbs-issue-hash id)
  (doc 'type (-> (Or String Symbol) (Or Bytevector Boolean)))
  (doc 'description "Get the current hash for an issue ID. O(1) lookup")
  (doc 'note "Auto-refreshes from disk if issue not in index (handles cross-session creation)")
  (let* ([id-str (normalize-id id)]
         [hash (hashtable-ref *bbs-issues* id-str #f)])
    (if hash
        hash
        ;; Not in index - try loading from disk (auto-refresh on cache miss)
        (let ([disk-hash (bbs-read-head id-str)])
          (when disk-hash
            (bbs-index-issue-from-disk! id-str disk-hash))
          disk-hash))))

(define (bbs-index-issue-from-disk! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Load a single issue into the index from its hash")
  (doc 'note "Used for auto-refresh when an issue exists on disk but not in memory")
  (doc 'note "Guards against duplicate indexing (e.g., from concurrent calls)")
  ;; Guard: skip if already indexed (prevents duplicates) - O(1) check
  (unless (hashtable-contains? *bbs-issues* id)
    (let ([blk (bbs-fetch hash)])
      (when blk
        (let ([data (issue-block-data blk)])
          (when data
            ;; Add to main index (O(1) insert)
            (hashtable-set! *bbs-issues* id hash)
            ;; Index by status
            (let* ([status (cdr (assq 'status data))]
                   [existing (hashtable-ref *bbs-by-status* status '())])
              (hashtable-set! *bbs-by-status* status (cons id existing)))
            ;; Index by priority
            (let* ([priority (cdr (assq 'priority data))]
                   [existing (hashtable-ref *bbs-by-priority* priority '())])
              (hashtable-set! *bbs-by-priority* priority (cons id existing)))))))))

(doc 'section 'dependency-management)

(define *bbs-deps-file* ".bbs/deps")

(define (bbs-save-deps!)
  (doc 'type (-> Void))
  (doc 'description "Persist dependencies to disk")
  (doc 'note "Uses atomic write-then-rename to prevent corruption")
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (call-with-atomic-output-file *bbs-deps-file*
    (lambda (port)
      (write *bbs-deps* port)
      (newline port))
    '(replace)))

(define (bbs-load-deps!)
  (doc 'type (-> Void))
  (doc 'description "Load dependencies from disk")
  (guard (e [else (set! *bbs-deps* '())])
    (if (file-exists? *bbs-deps-file*)
        (set! *bbs-deps*
              (call-with-input-file *bbs-deps-file*
                (lambda (port)
                  (let ([data (read port)])
                    (if (eof-object? data) '() data)))))
        (set! *bbs-deps* '()))))

(define (bbs-add-dep! blocker-id blocked-id)
  (doc 'type (-> String String Void))
  (doc 'description "Add a dependency: blocker-id blocks blocked-id")
  (unless (assoc blocker-id
                 (filter (lambda (d) (string=? (cdr d) blocked-id)) *bbs-deps*))
    (set! *bbs-deps* (cons (cons blocker-id blocked-id) *bbs-deps*))
    (bbs-save-deps!)))

(define (bbs-remove-dep! blocker-id blocked-id)
  (doc 'type (-> String String Void))
  (doc 'description "Remove a dependency")
  (set! *bbs-deps*
        (filter (lambda (d)
                  (not (and (string=? (car d) blocker-id)
                            (string=? (cdr d) blocked-id))))
                *bbs-deps*))
  (bbs-save-deps!))

(define (bbs-gc-deps!)
  (doc 'type (-> (List (Pair String String))))
  (doc 'description "Remove deps where either issue no longer exists")
  (doc 'returns "Removed dependencies")
  (let* ([stale (filter (lambda (d)
                          (or (not (bbs-fetch-issue-data (car d)))
                              (not (bbs-fetch-issue-data (cdr d)))))
                        *bbs-deps*)]
         [kept (filter (lambda (d)
                         (and (bbs-fetch-issue-data (car d))
                              (bbs-fetch-issue-data (cdr d))))
                       *bbs-deps*)])
    (when (not (null? stale))
      (set! *bbs-deps* kept)
      (bbs-save-deps!))
    stale))

(define (bbs-blockers id)
  (doc 'type (-> String (List String)))
  (doc 'description "Get IDs of issues that block the given issue")
  (map car
       (filter (lambda (d) (string=? (cdr d) id)) *bbs-deps*)))

(define (bbs-blocker-status blocker-id)
  (doc 'type (-> String Symbol))
  (doc 'description "Get status of a blocker: 'open, 'closed, 'in_progress, or 'missing")
  (let ([data (bbs-fetch-issue-data blocker-id)])
    (if data
        (cdr (assq 'status data))
        'missing)))

(define (bbs-blockers-with-status id)
  (doc 'type (-> String (List (Pair String Symbol))))
  (doc 'description "Get blockers annotated with their current status")
  (map (lambda (blocker-id)
         (cons blocker-id (bbs-blocker-status blocker-id)))
       (bbs-blockers id)))

(define (bbs-blocking id)
  (doc 'type (-> String (List String)))
  (doc 'description "Get IDs of issues that the given issue blocks")
  (map cdr
       (filter (lambda (d) (string=? (car d) id)) *bbs-deps*)))

(define (bbs-is-blocked? id)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if an issue is blocked by any open issues")
  (let ([blockers (bbs-blockers id)])
    (any (lambda (blocker-id)
           (let ([data (bbs-fetch-issue-data blocker-id)])
             (and data
                  (not (eq? (cdr (assq 'status data)) 'closed)))))
         blockers)))

(define (bbs-blocked-issues)
  (doc 'type (-> (List String)))
  (doc 'description "Get all issues that are blocked")
  (filter bbs-is-blocked?
          (bbs-issues-by-status 'open)))

(define (bbs-ready-issues)
  (doc 'type (-> (List String)))
  (doc 'description "Get all open issues that are not blocked")
  (filter (lambda (id) (not (bbs-is-blocked? id)))
          (bbs-issues-by-status 'open)))

(define (any pred lst)
  (doc 'type (-> (-> a Bool) (List a) Bool))
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

(doc 'section 'statistics)

(define (bbs-stats)
  (doc 'type (-> Alist))
  (doc 'description "Get statistics about the issue database")
  `((total . ,(hashtable-size *bbs-issues*))
    (open . ,(length (bbs-issues-by-status 'open)))
    (in_progress . ,(length (bbs-issues-by-status 'in_progress)))
    (closed . ,(length (bbs-issues-by-status 'closed)))
    (blocked . ,(length (bbs-blocked-issues)))
    (ready . ,(length (bbs-ready-issues)))
    (deps . ,(length *bbs-deps*))))
