(load "core/base/prelude.ss")
(load "boundary/io/atomic.ss")
(load "boundary/bbs/context.ss")

(doc 'module 'boundary/bbs/board)
(doc 'description "BBS Board Record + Registry - Named typed namespaces for the multi-board BBS")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude boundary/io/atomic boundary/bbs/context))
(doc 'note "A board is a named, typed namespace that scopes IDs, heads, counters,
and indices — but shares the CAS with all other boards.

Board types:
  'tracker  - Issue tracker (status, priority, dependencies)
  'forum    - Threaded discussion (topics + replies)
  'feed     - Flat posts (changelog, notes, announcements)")

(doc 'section 'board-record)

;;; Board record type
;;; Mutable fields use -box suffix to distinguish raw box accessors
;;; from the convenience wrappers (board-deps, board-counter, etc.)

(define-record-type board
  (fields slug board-type id-prefix description config
          items-ht by-status-ht by-priority-ht by-type-ht reply-map-ht
          deps-box counter-box init-box))

(doc 'section 'board-constructors)

(define (make-tracker-board slug id-prefix description . config)
  (doc 'type '(-> String String String Board))
  (doc 'description "Create a tracker board (issues with status, priority, deps)")
  (doc 'export #t)
  (make-board slug 'tracker id-prefix description
              (if (null? config) '() (car config))
              (make-hashtable string-hash string=?)   ; items-ht
              (make-eq-hashtable)                      ; by-status-ht
              (make-eqv-hashtable)                     ; by-priority-ht
              #f                                       ; by-type-ht (not used)
              #f                                       ; reply-map-ht (not used)
              (box '())                                ; deps-box
              (box 0)                                  ; counter-box
              (box #f)))                               ; init-box

(define (make-feed-board slug id-prefix description . config)
  (doc 'type '(-> String String String Board))
  (doc 'description "Create a feed board (flat posts with types)")
  (doc 'export #t)
  (make-board slug 'feed id-prefix description
              (if (null? config) '() (car config))
              (make-hashtable string-hash string=?)   ; items-ht
              #f                                       ; by-status-ht (not used)
              #f                                       ; by-priority-ht (not used)
              (make-eq-hashtable)                      ; by-type-ht
              #f                                       ; reply-map-ht (not used)
              (box '())                                ; deps-box (not used)
              (box 0)                                  ; counter-box
              (box #f)))                               ; init-box

(define (make-forum-board slug id-prefix description . config)
  (doc 'type '(-> String String String Board))
  (doc 'description "Create a forum board (topics with threaded replies)")
  (doc 'export #t)
  (make-board slug 'forum id-prefix description
              (if (null? config) '() (car config))
              (make-hashtable string-hash string=?)   ; items-ht (topics)
              (make-eq-hashtable)                      ; by-status-ht (open/locked/archived)
              #f                                       ; by-priority-ht (not used)
              #f                                       ; by-type-ht (not used)
              (make-hashtable string-hash string=?)   ; reply-map-ht
              (box '())                                ; deps-box (not used)
              (box 0)                                  ; counter-box
              (box #f)))                               ; init-box

(doc 'section 'board-mutable-accessors)

(define (board-deps b)
  (doc 'type '(-> Board (List (Pair String String))))
  (doc 'description "Get the dependency list for a board")
  (doc 'export #t)
  (unbox (board-deps-box b)))

(define (board-deps-set! b deps)
  (doc 'type '(-> Board (List (Pair String String)) Void))
  (doc 'description "Set the dependency list for a board")
  (doc 'export #t)
  (set-box! (board-deps-box b) deps))

(define (board-counter b)
  (doc 'type '(-> Board Int))
  (doc 'description "Get the current counter value for a board")
  (doc 'export #t)
  (unbox (board-counter-box b)))

(define (board-counter-set! b n)
  (doc 'type '(-> Board Int Void))
  (doc 'description "Set the counter value for a board")
  (doc 'export #t)
  (set-box! (board-counter-box b) n))

(define (board-initialized? b)
  (doc 'type '(-> Board Boolean))
  (doc 'description "Check if a board has been initialized")
  (doc 'export #t)
  (unbox (board-init-box b)))

(define (board-set-initialized! b)
  (doc 'type '(-> Board Void))
  (doc 'description "Mark a board as initialized")
  (doc 'export #t)
  (set-box! (board-init-box b) #t))

(doc 'section 'board-paths)

(define (board-dir b)
  (doc 'type '(-> Board String))
  (doc 'description "Get the filesystem directory for a board's metadata")
  (doc 'export #t)
  (string-append ".bbs/boards/" (board-slug b)))

(define (board-heads-dir b)
  (doc 'type '(-> Board String))
  (doc 'description "Get the filesystem directory for a board's head files")
  (doc 'export #t)
  (string-append ".store/heads/bbs/" (board-slug b)))

(define (board-counter-file b)
  (doc 'type '(-> Board String))
  (doc 'description "Get the path to a board's counter file")
  (doc 'export #t)
  (string-append (board-dir b) "/counter"))

(define (board-deps-file b)
  (doc 'type '(-> Board String))
  (doc 'description "Get the path to a board's deps file (tracker only)")
  (doc 'export #t)
  (string-append (board-dir b) "/deps"))

(define (board-index-cache-file b)
  (doc 'type '(-> Board String))
  (doc 'description "Get the path to a board's index cache file")
  (doc 'export #t)
  (string-append (board-dir b) "/index.cache"))

(define (board-descriptor-file b)
  (doc 'type '(-> Board String))
  (doc 'description "Get the path to a board's descriptor file")
  (doc 'export #t)
  (string-append (board-dir b) "/board.sexp"))

(doc 'section 'board-directory-creation)

(define (board-ensure-dirs! b)
  (doc 'type '(-> Board Void))
  (doc 'description "Ensure all directories for a board exist")
  (doc 'export #t)
  ;; Metadata dirs
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (unless (file-exists? ".bbs/boards")
    (mkdir ".bbs/boards"))
  (let ([dir (board-dir b)])
    (unless (file-exists? dir)
      (mkdir dir)))
  ;; Head dirs
  (unless (file-exists? ".store")
    (mkdir ".store"))
  (unless (file-exists? ".store/heads")
    (mkdir ".store/heads"))
  (unless (file-exists? ".store/heads/bbs")
    (mkdir ".store/heads/bbs"))
  (let ([hdir (board-heads-dir b)])
    (unless (file-exists? hdir)
      (mkdir hdir))))

(doc 'section 'board-registry)

(define *board-registry* (make-hashtable string-hash string=?))
(define *board-registry-file* ".bbs/boards.sexp")

(define (board-register! b)
  (doc 'type '(-> Board Void))
  (doc 'description "Register a board in the in-memory registry")
  (doc 'export #t)
  (hashtable-set! *board-registry* (board-slug b) b))

(define (board-ref slug)
  (doc 'type '(-> String (Option Board)))
  (doc 'description "Look up a board by slug. Returns #f if not found")
  (doc 'export #t)
  (hashtable-ref *board-registry* slug #f))

(define (board-exists? slug)
  (doc 'type '(-> String Boolean))
  (doc 'description "Check if a board is registered")
  (doc 'export #t)
  (hashtable-contains? *board-registry* slug))

(define (board-list)
  (doc 'type '(-> (List Board)))
  (doc 'description "List all registered boards")
  (doc 'export #t)
  (let-values ([(keys vals) (hashtable-entries *board-registry*)])
    (vector->list vals)))

(define (board-slugs)
  (doc 'type '(-> (List String)))
  (doc 'description "List all registered board slugs")
  (doc 'export #t)
  (vector->list (hashtable-keys *board-registry*)))

(define (board-save-registry!)
  (doc 'type '(-> Void))
  (doc 'description "Save the board registry to disk")
  (doc 'export #t)
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (call-with-atomic-output-file *board-registry-file*
    (lambda (port)
      (let ([entries (map (lambda (b)
                            `(,(board-slug b) ,(board-board-type b)
                              ,(board-id-prefix b) ,(board-description b)))
                          (board-list))])
        (write entries port)
        (newline port)))
    '(replace)))

(define (board-load-registry!)
  (doc 'type '(-> Boolean))
  (doc 'description "Load the board registry from disk. Returns #t if loaded successfully")
  (doc 'export #t)
  (guard (e [else #f])
    (and (file-exists? *board-registry-file*)
         (let ([data (call-with-input-file *board-registry-file* read)])
           (and (list? data)
                (begin
                  (set! *board-registry* (make-hashtable string-hash string=?))
                  (for-each
                   (lambda (entry)
                     (let ([slug (car entry)]
                           [type (cadr entry)]
                           [prefix (caddr entry)]
                           [desc (if (> (length entry) 3) (cadddr entry) "")])
                       (let ([b (case type
                                  [(tracker) (make-tracker-board slug prefix desc)]
                                  [(forum)   (make-forum-board slug prefix desc)]
                                  [(feed)    (make-feed-board slug prefix desc)]
                                  [else (error 'board-load-registry!
                                               "Unknown board type" type)])])
                         (board-register! b))))
                   data)
                  #t))))))

(define (board-save-descriptor! b)
  (doc 'type '(-> Board Void))
  (doc 'description "Save a board descriptor to its board.sexp file")
  (doc 'export #t)
  (board-ensure-dirs! b)
  (let ([path (board-descriptor-file b)])
    (call-with-atomic-output-file path
      (lambda (port)
        (write `((name . ,(board-slug b))
                 (slug . ,(board-slug b))
                 (board-type . ,(board-board-type b))
                 (id-prefix . ,(board-id-prefix b))
                 (description . ,(board-description b)))
               port)
        (newline port))
      '(replace))))

(doc 'section 'board-creation)

(define (board-create! slug type id-prefix description)
  (doc 'type '(-> String Symbol String String Board))
  (doc 'description "Create a new board, register it, create directories, and save registry")
  (doc 'export #t)
  (when (board-exists? slug)
    (error 'board-create! "Board already exists" slug))
  (let ([b (case type
             [(tracker) (make-tracker-board slug id-prefix description)]
             [(forum)   (make-forum-board slug id-prefix description)]
             [(feed)    (make-feed-board slug id-prefix description)]
             [else (error 'board-create! "Unknown board type" type)])])
    (board-ensure-dirs! b)
    (board-save-descriptor! b)
    (board-register! b)
    (board-save-registry!)
    b))

(define (board-info slug)
  (doc 'type '(-> String Void))
  (doc 'description "Display information about a board")
  (doc 'export #t)
  (let ([b (board-ref slug)])
    (if b
        (begin
          (printf "Board: ~a~n" (board-slug b))
          (printf "Type:  ~a~n" (board-board-type b))
          (printf "Prefix: ~a~n" (board-id-prefix b))
          (printf "Desc:  ~a~n" (board-description b))
          (printf "Dir:   ~a~n" (board-dir b))
          (printf "Heads: ~a~n" (board-heads-dir b))
          (printf "Init:  ~a~n" (board-initialized? b)))
        (printf "Board not found: ~a~n" slug))))
