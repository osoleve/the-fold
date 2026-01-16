;;; shell/bbs/posts.ss — BBS Posts (Changelogs, Notes, Announcements)
;;;
;;; General-purpose posts for the bulletin board system.
;;; Unlike issues, posts don't have status/priority/dependencies.
;;;
;;; Post types:
;;;   'changelog         - Release notes, what changed
;;;   'note              - General notes, documentation
;;;   'announcement      - Important announcements
;;;   'session-summary   - Summary of a work session
;;;
;;; Usage:
;;;   (post-create "Title" "Body..." 'changelog)
;;;   (post-list)
;;;   (post-show 'post-001)
;;;
;;; This is Shell code: impure (modifies state and filesystem).

(load "shell/bbs/store.ss")

;;; ====
;;; Timestamp Generation
;;; ====

;;; post-timestamp : -> String
;;; Generate an ISO 8601 timestamp for now.
(define (post-timestamp)
  (let ([t (current-date)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year t)
            (date-month t)
            (date-day t)
            (date-hour t)
            (date-minute t)
            (date-second t))))

;;; ====
;;; Configuration
;;; ====

(define *post-counter-file* ".bbs/post-counter")

;;; ====
;;; ID Generation
;;; ====

;;; post-read-counter : -> Int
;;; Read current counter value (or 0 if not exists).
(define (post-read-counter)
  (guard (e [else 0])
    (if (file-exists? *post-counter-file*)
        (call-with-input-file *post-counter-file*
          (lambda (port)
            (let ([line (get-line port)])
              (string->number line))))
        0)))

;;; post-write-counter! : Int -> Void
;;; Write counter value to file.
(define (post-write-counter! n)
  (unless (file-exists? ".bbs")
    (mkdir ".bbs"))
  (call-with-output-file *post-counter-file*
    (lambda (port)
      (put-string port (number->string n))
      (newline port))
    '(replace)))

;;; post-next-id! : -> String
;;; Generate next post ID.
(define (post-next-id!)
  (let* ([n (+ (post-read-counter) 1)]
         [id (string-append "post-" (number->string n 36))])
    (post-write-counter! n)
    id))

;;; ====
;;; Head Management (reuses bbs heads infrastructure)
;;; ====

;;; post-head-path : String -> String
;;; Get filesystem path for a post's head file.
(define (post-head-path id)
  (string-append ".store/heads/bbs/" id ".head"))

;;; post-read-head : String -> Bytevector | #f
;;; Read the current hash for a post ID.
(define (post-read-head id)
  (bbs-read-head id))

;;; post-write-head! : String Bytevector -> Void
;;; Write the current hash for a post ID.
(define (post-write-head! id hash)
  (bbs-write-head! id hash))

;;; post-list-heads : -> (List String)
;;; List all post IDs that have head files.
(define (post-list-heads)
  (filter (lambda (id) (string-starts-with? id "post-"))
          (bbs-list-heads)))

;;; string-starts-with? : String String -> Boolean
(define (string-starts-with? str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; ====
;;; Post Operations
;;; ====

;;; post-create : String String Symbol -> String
;;; Create a new post and return its ID.
;;;
;;; Arguments:
;;;   title     - Post title
;;;   body      - Post body (markdown)
;;;   post-type - 'changelog | 'note | 'announcement | 'session-summary
;;;
;;; Keyword arguments:
;;;   'tags     - List of tag symbols (default: '())
;;;   'author   - Author name (default: "system")
(define (post-create title body post-type . args)
  (let* ([tags (get-keyword-arg args 'tags '())]
         [author (get-keyword-arg args 'author "system")]
         [id (post-next-id!)]
         [timestamp (post-timestamp)]
         [blk (make-post-block id title body post-type tags timestamp author 1 #f)]
         [hash (bbs-store! blk)])
    ;; Write head file
    (post-write-head! id hash)
    id))

;;; post-fetch : String -> Block | #f
;;; Fetch a post by ID.
(define (post-fetch id)
  (let ([hash (post-read-head id)])
    (if hash
        (bbs-fetch hash)
        #f)))

;;; post-fetch-data : String -> Alist | #f
;;; Fetch and parse post data by ID.
(define (post-fetch-data id)
  (let ([blk (post-fetch id)])
    (if blk
        (post-block-data blk)
        #f)))

;;; post-update : String -> Bytevector
;;; Update a post's content.
;;;
;;; Keyword arguments:
;;;   'title       - New title
;;;   'body        - New body
;;;   'tags        - New tags
;;;   'expect-hash - Expected current hash (for OCC)
(define (post-update id . args)
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
        new-hash))))

;;; ====
;;; Display Functions
;;; ====

;;; post-show : String | Symbol -> Void
;;; Display a post.
(define (post-show id)
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

;;; post-list : -> Void
;;; List all posts.
;;;
;;; Keyword arguments:
;;;   'type  - Filter by post type (default: show all)
;;;   'limit - Maximum number to show (default: 20)
(define (post-list . args)
  (let* ([type-filter (get-keyword-arg args 'type #f)]
         [limit (get-keyword-arg args 'limit 20)]
         [ids (post-list-heads)]
         [posts (filter-map
                 (lambda (id)
                   (let ([data (post-fetch-data id)])
                     (if (and data
                              (or (not type-filter)
                                  (eq? (cdr (assq 'post-type data)) type-filter)))
                         (cons id data)
                         #f)))
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

;;; take-n : Int (List a) -> (List a)
;;; Take at most n elements from a list.
(define (take-n n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

;;; pad-right : String Int -> String
;;; Pad string with spaces on the right.
(define (pad-right str width)
  (let ([len (string-length str)])
    (if (>= len width)
        (substring str 0 width)
        (string-append str (make-string (- width len) #\space)))))

;;; truncate-str : String Int -> String
;;; Truncate string with ellipsis if too long.
(define (truncate-str str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; ====
;;; REPL Interface
;;; ====

(printf "posts.ss loaded.~n")
(printf "  (post-create title body type)  - Create post~n")
(printf "  (post-show id)                 - Show post~n")
(printf "  (post-list)                    - List posts~n")
(printf "  (post-update id ...)           - Update post~n")
