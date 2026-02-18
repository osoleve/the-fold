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
(doc 'note "Board-aware: all mutation functions use bbs-current-* helpers
  which route to board hashtables when board context is active,
  falling back to globals for legacy compatibility.")

(doc 'section 'index-cache)

(define *bbs-index-cache-file* ".bbs/index.cache")
(define *bbs-index-cache-version* 1)

(doc 'section 'board-aware-index-helpers)

(define (bbs-current-items-ht)
  (doc 'type '(-> Hashtable))
  (doc 'description "Get the items hashtable for the current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-items-ht (current-board))
      *bbs-issues*))

(define (bbs-current-status-ht)
  (doc 'type '(-> Hashtable))
  (doc 'description "Get the by-status hashtable for the current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-by-status-ht (current-board))
      *bbs-by-status*))

(define (bbs-current-priority-ht)
  (doc 'type '(-> Hashtable))
  (doc 'description "Get the by-priority hashtable for the current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-by-priority-ht (current-board))
      *bbs-by-priority*))

(define (bbs-current-deps)
  (doc 'type '(-> (List (Pair String String))))
  (doc 'description "Get the deps list for the current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-deps (current-board))
      *bbs-deps*))

(define (bbs-current-deps-set! deps)
  (doc 'type '(-> (List (Pair String String)) Void))
  (doc 'description "Set the deps list for the current context.
When no board context is active, updates both the global and the default
issues board's deps box to keep them in sync.")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-deps-set! (current-board) deps)
      (begin
        (set! *bbs-deps* deps)
        ;; Also sync to issues board record if registry loaded
        (when (and (top-level-bound? 'board-ref)
                   (board-ref "issues"))
          (board-deps-set! (board-ref "issues") deps)))))

(define (bbs-current-index-cache-file)
  (doc 'type '(-> String))
  (doc 'description "Get the index cache file path for current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-index-cache-file (current-board))
      *bbs-index-cache-file*))

(define (bbs-current-deps-file)
  (doc 'type '(-> String))
  (doc 'description "Get the deps file path for current context")
  (if (and (top-level-bound? 'board-context?)
           (board-context?))
      (board-deps-file (current-board))
      *bbs-deps-file*))

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
  (doc 'description "Save the current index to disk cache. Board-aware.")
  (doc 'note "Called after rebuild to speed up future startups")
  (let ([cache-file (bbs-current-index-cache-file)]
        [items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)]
        [priorities (bbs-current-priority-ht)])
    ;; Ensure parent dir exists
    (unless (file-exists? ".bbs")
      (mkdir ".bbs"))
    (when (and (top-level-bound? 'board-context?)
               (board-context?))
      (board-ensure-dirs! (current-board)))
    (guard (e [else #f])  ; Silently fail - cache is optional
      (call-with-atomic-output-file cache-file
        (lambda (port)
          (let ([issues-hex (hashtable-map items
                              (lambda (id hash) (cons id (hash->hex hash))))]
                [by-status-alist (if statuses (hashtable->alist statuses) '())]
                [by-priority-alist (if priorities (hashtable->alist priorities) '())])
            (write
             `(bbs-index-cache
               (version ,*bbs-index-cache-version*)
               (head-count ,(hashtable-size items))
               (issues ,issues-hex)
               (by-status ,by-status-alist)
               (by-priority ,by-priority-alist))
             port)
            (newline port)))
        '(replace)))))

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
  (doc 'description "Try to load index from disk cache. Board-aware.")
  (doc 'returns "#t if cache was valid and loaded, #f otherwise")
  (let ([cache-file (bbs-current-index-cache-file)]
        [items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)]
        [priorities (bbs-current-priority-ht)])
    (guard (e [else #f])
      (and (file-exists? cache-file)
           (let ([data (call-with-input-file cache-file read)])
             (and (pair? data)
                  (eq? (car data) 'bbs-index-cache)
                  (let ([version (cadr (assq 'version (cdr data)))]
                        [head-count (cadr (assq 'head-count (cdr data)))]
                        [issues-hex (cadr (assq 'issues (cdr data)))]
                        [by-status-alist (cadr (assq 'by-status (cdr data)))]
                        [by-priority-alist (cadr (assq 'by-priority (cdr data)))])
                    (and (= version *bbs-index-cache-version*)
                         (let* ([disk-heads (bbs-list-issue-heads)]
                                [cached-ids (map car issues-hex)])
                           (and (string-set-equal? cached-ids disk-heads)
                                (begin
                                  ;; Restore items hashtable
                                  (hashtable-clear! items)
                                  (for-each
                                   (lambda (entry)
                                     (hashtable-set! items (car entry) (hex->hash (cdr entry))))
                                   issues-hex)
                                  ;; Restore by-status hashtable
                                  (when statuses
                                    (hashtable-clear! statuses)
                                    (for-each
                                     (lambda (entry)
                                       (hashtable-set! statuses (car entry) (cdr entry)))
                                     by-status-alist))
                                  ;; Restore by-priority hashtable
                                  (when priorities
                                    (hashtable-clear! priorities)
                                    (for-each
                                     (lambda (entry)
                                       (hashtable-set! priorities (car entry) (cdr entry)))
                                     by-priority-alist))
                                  ;; CRITICAL: Sync counter from DISK heads, not cache
                                  (bbs-sync-counter-from-heads! disk-heads)
                                  ;; Load dependencies (always from disk, not cached)
                                  (bbs-load-deps!)
                                  #t)))))))))))

(doc 'section 'index-building)

(define (bbs-rebuild-indices!)
  (doc 'type (-> Int))
  (doc 'description "Rebuild all indices from head files. Board-aware.")
  (doc 'returns "The number of issues indexed")
  (let ([items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)]
        [priorities (bbs-current-priority-ht)])
    (hashtable-clear! items)
    (when statuses (hashtable-clear! statuses))
    (when priorities (hashtable-clear! priorities))
    (bbs-current-deps-set! '())

    (let* ([ids (bbs-list-issue-heads)]
           [count 0])
      ;; Sync counter to avoid ID collisions
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
                     (hashtable-set! items id hash)
                     (set! count (+ count 1))
                     ;; Index by status
                     (when statuses
                       (let* ([status (cdr (assq 'status data))]
                              [existing (hashtable-ref statuses status '())])
                         (hashtable-set! statuses status (cons id existing))))
                     ;; Index by priority
                     (when priorities
                       (let* ([priority (cdr (assq 'priority data))]
                              [existing (hashtable-ref priorities priority '())])
                         (hashtable-set! priorities priority (cons id existing)))))))))))
       ids)

      ;; Load dependencies from disk
      (bbs-load-deps!)

      ;; Save cache for future fast loads
      (bbs-save-index-cache!)

      count)))

(doc 'section 'index-updates)

(define (bbs-index-add! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Add a new issue to the index. Board-aware.")
  (let ([items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)]
        [priorities (bbs-current-priority-ht)])
    (let ([blk (bbs-fetch hash)])
      (when blk
        (let ([data (issue-block-data blk)])
          (when data
            (hashtable-set! items id hash)
            ;; Index by status
            (when statuses
              (let* ([status (cdr (assq 'status data))]
                     [existing (hashtable-ref statuses status '())])
                (hashtable-set! statuses status (cons id existing))))
            ;; Index by priority
            (when priorities
              (let* ([priority (cdr (assq 'priority data))]
                     [existing (hashtable-ref priorities priority '())])
                (hashtable-set! priorities priority (cons id existing))))
            ;; Persist cache
            (bbs-save-index-cache!)))))))

(define (bbs-index-update! id new-hash old-status new-status old-priority new-priority)
  (doc 'type (-> String Bytevector Symbol Symbol Symbol Symbol Void))
  (doc 'description "Update an issue in the index (handles status/priority changes). Board-aware.")
  (let ([items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)]
        [priorities (bbs-current-priority-ht)])
    ;; Update main index
    (hashtable-set! items id new-hash)
    ;; Update status index if changed
    (when (and statuses (not (eq? old-status new-status)))
      (let ([old-list (hashtable-ref statuses old-status '())])
        (hashtable-set! statuses old-status
                        (filter (lambda (x) (not (string=? x id))) old-list)))
      (let ([new-list (hashtable-ref statuses new-status '())])
        (hashtable-set! statuses new-status (cons id new-list))))
    ;; Update priority index if changed
    (when (and priorities (not (equal? old-priority new-priority)))
      (let ([old-list (hashtable-ref priorities old-priority '())])
        (hashtable-set! priorities old-priority
                        (filter (lambda (x) (not (string=? x id))) old-list)))
      (let ([new-list (hashtable-ref priorities new-priority '())])
        (hashtable-set! priorities new-priority (cons id new-list))))
    ;; Persist cache
    (bbs-save-index-cache!)))

(doc 'section 'index-queries)
;;; Query functions use globals for backward compatibility.
;;; In multi-board mode, globals are aliased to the default board's hashtables.

(define (bbs-all-ids)
  (doc 'type (-> (List String)))
  (doc 'description "Get all issue IDs as a list. O(n) to build list from hashtable keys")
  (vector->list (hashtable-keys (bbs-current-items-ht))))

(define (bbs-all-issues)
  (doc 'type (-> (List (Pair String Bytevector))))
  (doc 'description "Get all issues as (id . hash) pairs (alist form for compatibility)")
  (hashtable-map (bbs-current-items-ht) cons))

(define (bbs-issues-by-status status)
  (doc 'type (-> Symbol (List String)))
  (doc 'description "Get issue IDs with a given status")
  (let ([ht (bbs-current-status-ht)])
    (if ht (hashtable-ref ht status '()) '())))

(define (bbs-issues-by-priority priority)
  (doc 'type (-> Int (List String)))
  (doc 'description "Get issue IDs with a given priority")
  (let ([ht (bbs-current-priority-ht)])
    (if ht (hashtable-ref ht priority '()) '())))

(define (bbs-issue-count)
  (doc 'type (-> Int))
  (doc 'description "Get total number of issues. O(1) with hashtable")
  (hashtable-size (bbs-current-items-ht)))

(define (bbs-issue-exists? id)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if an issue exists in the index. O(1) lookup")
  (let ([id-str (normalize-id id)])
    (hashtable-contains? (bbs-current-items-ht) id-str)))

(define (bbs-issue-hash id)
  (doc 'type (-> (Or String Symbol) (Or Bytevector Boolean)))
  (doc 'description "Get the current hash for an issue ID. O(1) lookup")
  (doc 'note "Auto-refreshes from disk if issue not in index (handles cross-session creation)")
  (let* ([id-str (normalize-id id)]
         [hash (hashtable-ref (bbs-current-items-ht) id-str #f)])
    (if hash
        hash
        ;; Not in index - try loading from disk (auto-refresh on cache miss)
        (let ([disk-hash (bbs-read-head id-str)])
          (when disk-hash
            (bbs-index-issue-from-disk! id-str disk-hash))
          disk-hash))))

(define (bbs-index-issue-from-disk! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Load a single issue into the index from its hash. Board-aware.")
  (let ([items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)]
        [priorities (bbs-current-priority-ht)])
    ;; Guard: skip if already indexed
    (unless (hashtable-contains? items id)
      (let ([blk (bbs-fetch hash)])
        (when blk
          (let ([data (issue-block-data blk)])
            (when data
              (hashtable-set! items id hash)
              (when statuses
                (let* ([status (cdr (assq 'status data))]
                       [existing (hashtable-ref statuses status '())])
                  (hashtable-set! statuses status (cons id existing))))
              (when priorities
                (let* ([priority (cdr (assq 'priority data))]
                       [existing (hashtable-ref priorities priority '())])
                  (hashtable-set! priorities priority (cons id existing)))))))))))

(doc 'section 'dependency-management)

(define *bbs-deps-file* ".bbs/deps")

(define (bbs-save-deps!)
  (doc 'type (-> Void))
  (doc 'description "Persist dependencies to disk. Board-aware.")
  (let ([deps-file (bbs-current-deps-file)]
        [deps (bbs-current-deps)])
    (unless (file-exists? ".bbs")
      (mkdir ".bbs"))
    (when (and (top-level-bound? 'board-context?)
               (board-context?))
      (board-ensure-dirs! (current-board)))
    (call-with-atomic-output-file deps-file
      (lambda (port)
        (write deps port)
        (newline port))
      '(replace))))

(define (bbs-load-deps!)
  (doc 'type (-> Void))
  (doc 'description "Load dependencies from disk. Board-aware.")
  (let ([deps-file (bbs-current-deps-file)])
    (guard (e [else (bbs-current-deps-set! '())])
      (if (file-exists? deps-file)
          (bbs-current-deps-set!
           (call-with-input-file deps-file
             (lambda (port)
               (let ([data (read port)])
                 (if (eof-object? data) '() data)))))
          (bbs-current-deps-set! '())))))

(define (bbs-add-dep! blocker-id blocked-id)
  (doc 'type (-> String String Void))
  (doc 'description "Add a dependency: blocker-id blocks blocked-id. Board-aware.")
  (let ([deps (bbs-current-deps)])
    (unless (assoc blocker-id
                   (filter (lambda (d) (string=? (cdr d) blocked-id)) deps))
      (bbs-current-deps-set! (cons (cons blocker-id blocked-id) deps))
      (bbs-save-deps!))))

(define (bbs-remove-dep! blocker-id blocked-id)
  (doc 'type (-> String String Void))
  (doc 'description "Remove a dependency. Board-aware.")
  (bbs-current-deps-set!
   (filter (lambda (d)
             (not (and (string=? (car d) blocker-id)
                       (string=? (cdr d) blocked-id))))
           (bbs-current-deps)))
  (bbs-save-deps!))

(define (bbs-gc-deps!)
  (doc 'type (-> (List (Pair String String))))
  (doc 'description "Remove deps where either issue no longer exists. Board-aware.")
  (doc 'returns "Removed dependencies")
  (let* ([deps (bbs-current-deps)]
         [stale (filter (lambda (d)
                          (or (not (bbs-fetch-issue-data (car d)))
                              (not (bbs-fetch-issue-data (cdr d)))))
                        deps)]
         [kept (filter (lambda (d)
                         (and (bbs-fetch-issue-data (car d))
                              (bbs-fetch-issue-data (cdr d))))
                       deps)])
    (when (not (null? stale))
      (bbs-current-deps-set! kept)
      (bbs-save-deps!))
    stale))

(define (bbs-blockers id)
  (doc 'type (-> String (List String)))
  (doc 'description "Get IDs of issues that block the given issue. Board-aware.")
  (map car
       (filter (lambda (d) (string=? (cdr d) id)) (bbs-current-deps))))

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
  (doc 'description "Get IDs of issues that the given issue blocks. Board-aware.")
  (map cdr
       (filter (lambda (d) (string=? (car d) id)) (bbs-current-deps))))

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
  `((total . ,(hashtable-size (bbs-current-items-ht)))
    (open . ,(length (bbs-issues-by-status 'open)))
    (in_progress . ,(length (bbs-issues-by-status 'in_progress)))
    (closed . ,(length (bbs-issues-by-status 'closed)))
    (blocked . ,(length (bbs-blocked-issues)))
    (ready . ,(length (bbs-ready-issues)))
    (deps . ,(length (bbs-current-deps)))))
