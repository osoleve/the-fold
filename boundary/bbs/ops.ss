(load "boundary/bbs/index.ss")

(doc 'module 'boundary/bbs/ops)
(doc 'description "BBS Issue Operations - Create, update, close, and manage issues")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(boundary/bbs/index))
(doc 'note "Lock-aware design: Operations call public functions (bbs-write-head!, bbs-next-id!) which acquire their own locks. Each operation accesses different resources (counter, CAS store, head files) with independent locks")

(doc 'section 'timestamp-generation)

(define (bbs-timestamp)
  (doc 'type '(-> String))
  (doc 'description "Generate an ISO 8601 timestamp for now")
  (doc 'export #t)
  (let ([t (current-date)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year t)
            (date-month t)
            (date-day t)
            (date-hour t)
            (date-minute t)
            (date-second t))))

(doc 'section 'issue-creation)

(define (bbs-create title . args)
  (doc 'type '(-> String String))
  (doc 'description "Create a new issue with just a title. Returns the new issue ID")
  (doc 'export #t)
  (let* ([description (get-keyword-arg args 'description "")]
         [priority (get-keyword-arg args 'priority 2)]
         [type (get-keyword-arg args 'type 'task)]
         [labels (get-keyword-arg args 'labels '())]
         [created-by (get-keyword-arg args 'created-by "system")]
         [id (bbs-next-id!)]
         [timestamp (bbs-timestamp)]
         [blk (make-issue-block id title description 'open priority type labels
                                timestamp created-by 1 #f)]
         [hash (bbs-store! blk)])
    (bbs-write-head! id hash)
    (bbs-index-add! id hash)
    id))

(define (get-keyword-arg args key default)
  (doc 'type '(-> (List Any) Symbol Any Any))
  (doc 'description "Extract a keyword argument from an argument list")
  (let loop ([lst args])
    (cond
     [(null? lst) default]
     [(null? (cdr lst)) default]
     [(eq? (car lst) key) (cadr lst)]
     [else (loop (cdr lst))])))

(doc 'section 'issue-updates)

(define (bbs-update id . args)
  (doc 'type '(-> (U String Symbol) Bytevector))
  (doc 'description "Update an issue. Returns the new hash. Keyword arguments: 'status, 'priority, 'title, 'description, 'labels, 'expect-hash (for OCC)")
  (doc 'export #t)
  (let* ([id-str (normalize-id id)]
         [current-hash (bbs-issue-hash id-str)])

    (unless current-hash
      (error 'bbs-update "Issue not found" id-str))

    (let* ([expect-hash (get-keyword-arg args 'expect-hash #f)]
           [blk (bbs-fetch current-hash)]
           [data (issue-block-data blk)])

      (when (and expect-hash
                 (not (bytevector=? current-hash expect-hash)))
        (error 'bbs-update "Concurrent modification detected" id-str))

    (let* ([old-status (cdr (assq 'status data))]
           [old-priority (cdr (assq 'priority data))]
           [new-status (get-keyword-arg args 'status old-status)]
           [new-priority (get-keyword-arg args 'priority old-priority)]
           [new-title (get-keyword-arg args 'title (cdr (assq 'title data)))]
           [new-description (get-keyword-arg args 'description (cdr (assq 'description data)))]
           [new-labels (get-keyword-arg args 'labels (cdr (assq 'labels data)))]
           [version (+ (cdr (assq 'version data)) 1)]
           [new-blk (make-issue-block id-str new-title new-description new-status
                                      new-priority (cdr (assq 'type data)) new-labels
                                      (cdr (assq 'created-at data))
                                      (cdr (assq 'created-by data))
                                      version current-hash)]
           [new-hash (bbs-store! new-blk)])

      (bbs-write-head! id-str new-hash)
      (bbs-index-update! id-str new-hash old-status new-status old-priority new-priority)

      new-hash))))

(doc 'section 'issue-close-reopen)

(define (bbs-close id . args)
  (doc 'type '(-> (U String Symbol) (Option Bytevector)))
  (doc 'description "Close an issue. Returns #f if already closed (idempotent). Keyword arguments: 'reason - Reason for closing")
  (doc 'export #t)
  (let* ([id-str (normalize-id id)]
         [data (bbs-fetch-issue-data id-str)])
    (if (and data (eq? (cdr (assq 'status data)) 'closed))
        #f
        (bbs-update id 'status 'closed))))

(define (bbs-reopen id)
  (doc 'type '(-> (U String Symbol) (Option Bytevector)))
  (doc 'description "Reopen a closed issue. Returns #f if already open (idempotent)")
  (doc 'export #t)
  (let* ([id-str (normalize-id id)]
         [data (bbs-fetch-issue-data id-str)])
    (if (and data (eq? (cdr (assq 'status data)) 'open))
        #f
        (bbs-update id 'status 'open))))

(doc 'section 'dependencies)

(define (bbs-dep blocker blocked)
  (doc 'type '(-> (U String Symbol) (U String Symbol) Void))
  (doc 'description "Add a dependency: blocker blocks blocked")
  (doc 'export #t)
  (let* ([blocker-str (normalize-id blocker)]
         [blocked-str (normalize-id blocked)]
         [blocker-hash (bbs-issue-hash blocker-str)]
         [blocked-hash (bbs-issue-hash blocked-str)])
    (when (and blocker-hash blocked-hash)
      (let* ([dep-blk (make-dep-block blocker-hash blocked-hash)]
             [dep-hash (bbs-store! dep-blk)])
        (bbs-add-dep! blocker-str blocked-str)))))

(define (bbs-undep blocker blocked)
  (doc 'type '(-> (U String Symbol) (U String Symbol) Void))
  (doc 'description "Remove a dependency")
  (doc 'export #t)
  (let ([blocker-str (normalize-id blocker)]
        [blocked-str (normalize-id blocked)])
    (bbs-remove-dep! blocker-str blocked-str)))

(doc 'section 'comments)

(doc *bbs-comment-counter* 'type '(Hashtable String Int))
(doc *bbs-comment-counter* 'description "Track next comment ID per issue")
(define *bbs-comment-counter* (make-hashtable string-hash string=?))

(define (bbs-next-comment-id! issue-id)
  (doc 'type '(-> String Int))
  (doc 'description "Get next comment ID for an issue")
  (let ([current (hashtable-ref *bbs-comment-counter* issue-id 0)])
    (hashtable-set! *bbs-comment-counter* issue-id (+ current 1))
    (+ current 1)))

(define (bbs-comment issue-id text . args)
  (doc 'type '(-> (U String Symbol) String Bytevector))
  (doc 'description "Add a comment to an issue. Keyword arguments: 'content-type ('text | 'code | 'tool-result | 'thought), 'author - Comment author")
  (doc 'export #t)
  (let* ([id-str (normalize-id issue-id)]
         [content-type (get-keyword-arg args 'content-type 'text)]
         [author (get-keyword-arg args 'author "system")]
         [issue-hash (bbs-issue-hash id-str)]
         [comment-id (bbs-next-comment-id! id-str)]
         [timestamp (bbs-timestamp)]
         [blk (make-comment-block issue-hash comment-id author text
                                  content-type timestamp)]
         [hash (bbs-store! blk)])
    hash))

(doc 'section 'history-compaction)

(define (bbs-compact-history! id)
  (doc 'type '(-> (U String Symbol) (Option Bytevector)))
  (doc 'description "Compact redundant history by removing consecutive versions with same status. Creates a new version that skips redundant intermediate versions. Returns new hash, or #f if no compaction needed")
  (doc 'export #t)
  (let* ([id-str (normalize-id id)]
         [history (bbs-issue-history-data id-str)])
    (if (< (length history) 3)
        #f
        (let* ([current (car history)]
               [current-status (cdr (assq 'status current))]
               [meaningful-prev
                (let loop ([versions (cdr history)])
                  (if (null? versions)
                      #f
                      (let ([v (car versions)])
                        (if (not (eq? (cdr (assq 'status v)) current-status))
                            v
                            (loop (cdr versions))))))])
          (if (not meaningful-prev)
              #f
              (let ([skipped (- (cdr (assq 'version current))
                                (cdr (assq 'version meaningful-prev))
                                1)])
                (if (< skipped 1)
                    #f
                    (let* ([prev-hash (bbs-find-version-hash id-str
                                        (cdr (assq 'version meaningful-prev)))]
                           [new-blk (make-issue-block
                                     id-str
                                     (cdr (assq 'title current))
                                     (cdr (assq 'description current))
                                     (cdr (assq 'status current))
                                     (cdr (assq 'priority current))
                                     (cdr (assq 'type current))
                                     (cdr (assq 'labels current))
                                     (cdr (assq 'created-at current))
                                     (cdr (assq 'created-by current))
                                     (+ (cdr (assq 'version meaningful-prev)) 1)
                                     prev-hash)]
                           [new-hash (bbs-store! new-blk)])
                      (bbs-write-head! id-str new-hash)
                      (bbs-index-update! id-str new-hash
                                         current-status current-status
                                         (cdr (assq 'priority current))
                                         (cdr (assq 'priority current)))
                      (display (format "Compacted ~a versions from ~a~n"
                                       skipped id-str))
                      new-hash))))))))

(define (bbs-find-version-hash id version-num)
  (doc 'type '(-> String Int (Option Bytevector)))
  (doc 'description "Find the hash of a specific version number")
  (let loop ([hash (bbs-read-head id)])
    (if (not hash)
        #f
        (let ([blk (bbs-fetch hash)])
          (if (not blk)
              #f
              (let ([data (issue-block-data blk)])
                (if (= (cdr (assq 'version data)) version-num)
                    hash
                    (let ([prev (issue-block-prev blk)])
                      (if prev
                          (loop prev)
                          #f)))))))))
