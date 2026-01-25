(load "boundary/bbs/store.ss")
(load "boundary/bbs/post-index.ss")
(load "boundary/io/file-lock.ss")

(doc 'module 'bbs/posts)
(doc 'description "BBS Posts (Changelogs, Notes, Announcements)")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "General-purpose posts for the bulletin board system.
Unlike issues, posts don't have status/priority/dependencies.

Post types:
  'changelog         - Release notes, what changed
  'note              - General notes, documentation
  'announcement      - Important announcements
  'session-summary   - Summary of a work session

Lock-aware design:
  - Public functions (post-write-counter!, post-next-id!) acquire locks
  - Internal functions (%post-write-counter!) for use when lock already held")
(doc 'example "(post-create \"Title\" \"Body...\" 'changelog)
(post-list)
(post-show 'post-001)")

(doc 'section 'timestamp-generation)

(define (post-timestamp)
  (doc 'type (-> String))
  (doc 'description "Generate an ISO 8601 timestamp for now")
  (let ([t (current-date)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year t)
            (date-month t)
            (date-day t)
            (date-hour t)
            (date-minute t)
            (date-second t))))

(doc 'section 'configuration)

(define *post-counter-file* ".bbs/post-counter")

(doc 'section 'id-generation)

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

(define (post-write-counter! n)
  (doc 'type (-> Int Void))
  (doc 'description "PUBLIC: Write counter value to file with locking")
  (with-file-lock *post-counter-file*
    (lambda ()
      (%post-write-counter! n))))

(define (post-next-id!)
  (doc 'type (-> String))
  (doc 'description "Generate next post ID")
  (with-file-lock *post-counter-file*
    (lambda ()
      (let* ([n (+ (post-read-counter) 1)]
             [id (string-append "post-" (number->string n 36))])
        (%post-write-counter! n)  ; Use internal version - already holding lock
        id))))

(doc 'section 'head-management)
(doc 'note "Reuses bbs heads infrastructure")

(define (post-head-path id)
  (doc 'type (-> String String))
  (doc 'description "Get filesystem path for a post's head file")
  (string-append ".store/heads/bbs/" id ".head"))

(define (post-read-head id)
  (doc 'type (-> String (Or Bytevector Boolean)))
  (doc 'description "Read the current hash for a post ID")
  (bbs-read-head id))

(define (post-write-head! id hash)
  (doc 'type (-> String Bytevector Void))
  (doc 'description "Write the current hash for a post ID")
  (bbs-write-head! id hash))

(define (post-list-heads)
  (doc 'type (-> (List String)))
  (doc 'description "List all post IDs that have head files")
  (filter (lambda (id) (string-starts-with? id "post-"))
          (bbs-list-heads)))

(doc 'section 'post-operations)

(define (post-create title body post-type . args)
  (doc 'type (-> String String Symbol String))
  (doc 'description "Create a new post and return its ID")
  (doc 'param 'title "Post title")
  (doc 'param 'body "Post body (markdown)")
  (doc 'param 'post-type "'changelog | 'note | 'announcement | 'session-summary")
  (doc 'param 'tags "List of tag symbols (default: '())")
  (doc 'param 'author "Author name (default: \"system\")")
  (let* ([tags (get-keyword-arg args 'tags '())]
         [author (get-keyword-arg args 'author "system")]
         [id (post-next-id!)]
         [timestamp (post-timestamp)]
         [blk (make-post-block id title body post-type tags timestamp author 1 #f)]
         [hash (bbs-store! blk)])
    ;; Write head file
    (post-write-head! id hash)
    ;; Update in-memory index
    (post-index-add! id hash)
    id))

(define (post-fetch id)
  (doc 'type (-> String (Or Block Boolean)))
  (doc 'description "Fetch a post by ID")
  (doc 'note "Uses index for O(1) hash lookup with auto-refresh from disk")
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
         [hash (post-index-hash id-str)])
    (if hash
        (bbs-fetch hash)
        #f)))

(define (post-fetch-data id)
  (doc 'type (-> String (Or Alist Boolean)))
  (doc 'description "Fetch and parse post data by ID")
  (let ([blk (post-fetch id)])
    (if blk
        (post-block-data blk)
        #f)))

(define (post-update id . args)
  (doc 'type (-> String Bytevector))
  (doc 'description "Update a post's content")
  (doc 'param 'title "New title")
  (doc 'param 'body "New body")
  (doc 'param 'tags "New tags")
  (doc 'param 'expect-hash "Expected current hash (for OCC)")
  (let* ([current-hash (post-read-head id)])
    (unless current-hash
      (error 'post-update "Post not found" id))
    (let ([expect-hash (get-keyword-arg args 'expect-hash #f)])
      ;; OCC check if expect-hash provided
      (when (and expect-hash
                 (not (bytevector=? current-hash expect-hash)))
        (error 'post-update "Concurrent modification detected" id))
      (let* ([blk (bbs-fetch current-hash)]
             [data (post-block-data blk)]
             [new-title (get-keyword-arg args 'title (cdr (assq 'title data)))]
             [new-body (get-keyword-arg args 'body (cdr (assq 'body data)))]
             [new-tags (get-keyword-arg args 'tags (cdr (assq 'tags data)))]
             [version (+ (cdr (assq 'version data)) 1)]
             [new-blk (make-post-block id new-title new-body
                                       (cdr (assq 'post-type data))
                                       new-tags
                                       (cdr (assq 'created-at data))
                                       (cdr (assq 'author data))
                                       version
                                       current-hash)]
             [new-hash (bbs-store! new-blk)])
        (post-write-head! id new-hash)
        ;; Update in-memory index
        (post-index-update! id new-hash)
        new-hash))))

(doc 'section 'display-functions)

(define (post-show id)
  (doc 'type (-> (Or String Symbol) Void))
  (doc 'description "Display a post")
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
         [data (post-fetch-data id-str)])
    (if data
        (begin
          (printf "~a~n" (make-string 60 #\=))
          (printf "~a~n" (cdr (assq 'title data)))
          (printf "~a~n" (make-string 60 #\=))
          (printf "ID:      ~a~n" (cdr (assq 'id data)))
          (printf "Type:    ~a~n" (cdr (assq 'post-type data)))
          (printf "Author:  ~a~n" (cdr (assq 'author data)))
          (printf "Date:    ~a~n" (cdr (assq 'created-at data)))
          (let ([tags (cdr (assq 'tags data))])
            (unless (null? tags)
              (printf "Tags:    ~a~n" tags)))
          (printf "Version: ~a~n" (cdr (assq 'version data)))
          (printf "~a~n~n" (make-string 60 #\-))
          (printf "~a~n" (cdr (assq 'body data)))
          (printf "~a~n" (make-string 60 #\=)))
        (printf "Post not found: ~a~n" id-str))))

(define (post-list . args)
  (doc 'type (-> Void))
  (doc 'description "List all posts")
  (doc 'param 'type "Filter by post type (default: show all)")
  (doc 'param 'limit "Maximum number to show (default: 20)")
  (doc 'note "Uses in-memory index for O(1) ID retrieval by type")
  (let* ([type-filter (get-keyword-arg args 'type #f)]
         [limit (get-keyword-arg args 'limit 20)]
         ;; Use index for O(1) type filtering (vs O(n) disk reads)
         [ids (if type-filter
                  (post-ids-by-type type-filter)
                  (post-all-ids))]
         ;; Still need to fetch data for sorting - but only for visible posts
         [posts (filter-map
                 (lambda (id)
                   (let ([data (post-fetch-data id)])
                     (if data (cons id data) #f)))
                 ids)]
         ;; Sort by date descending
         [sorted (sort (lambda (a b)
                        (string>? (cdr (assq 'created-at (cdr a)))
                                  (cdr (assq 'created-at (cdr b)))))
                       posts)]
         [to-show (take-n limit sorted)])
    (printf "~a posts~a~n"
            (length posts)
            (if type-filter (format " (type: ~a)" type-filter) ""))
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (entry)
       (let ([id (car entry)]
             [data (cdr entry)])
         (printf "~a  [~a]  ~a~n"
                 (pad-right id 10)
                 (pad-right (symbol->string (cdr (assq 'post-type data))) 15)
                 (truncate-str (cdr (assq 'title data)) 30))))
     to-show)
    (when (> (length posts) limit)
      (printf "... and ~a more~n" (- (length posts) limit)))))

(define (take-n n lst)
  (doc 'type (-> Int (List a) (List a)))
  (doc 'description "Take at most n elements from a list")
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

(define (pad-right str width)
  (doc 'type (-> String Int String))
  (doc 'description "Pad string with spaces on the right")
  (let ([len (string-length str)])
    (if (>= len width)
        (substring str 0 width)
        (string-append str (make-string (- width len) #\space)))))

(define (truncate-str str max-len)
  (doc 'type (-> String Int String))
  (doc 'description "Truncate string with ellipsis if too long")
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

(doc 'section 'repl-interface)

(printf "posts.ss loaded.~n")
(printf "  (post-create title body type)  - Create post~n")
(printf "  (post-show id)                 - Show post~n")
(printf "  (post-list)                    - List posts~n")
(printf "  (post-update id ...)           - Update post~n")
