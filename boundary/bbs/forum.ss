(doc 'module 'boundary/bbs/forum)
(doc 'description "BBS Forum Board Type - Threaded discussion with topics and replies")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Forum boards support:
  - Topics with title, body, author, status (open/locked/archived)
  - Nested replies via CAS refs (reply -> parent hash)
  - Reply head files: reply-<topic-id>-<n>.head
  - Topic status transitions: open -> locked, open -> archived")

(doc 'section 'id-generation)

(define (board-next-topic-id!)
  (doc 'type '(-> String))
  (doc 'description "Generate next topic ID for the current forum board")
  (doc 'export #t)
  (bbs-next-id!))

(define (board-next-reply-id! topic-id)
  (doc 'type '(-> String String))
  (doc 'description "Generate next reply ID for a topic. Format: reply-<topic-id>-<n>")
  (doc 'export #t)
  (let* ([b (current-board)]
         [reply-map (board-reply-map-ht b)]
         [existing (if reply-map
                       (hashtable-ref reply-map topic-id '())
                       '())]
         [n (+ (length existing) 1)])
    (format "reply-~a-~3,'0d" topic-id n)))

(doc 'section 'topic-operations)

(define (topic-create! title body . args)
  (doc 'type '(-> String String String))
  (doc 'description "Create a new topic on the current forum board. Returns topic ID.")
  (doc 'export #t)
  (let* ([author (get-keyword-arg args 'author "system")]
         [id (board-next-topic-id!)]
         [timestamp (bbs-timestamp)]
         [blk (make-topic-block id title body author timestamp 'open 1 #f)]
         [hash (bbs-store! blk)])
    (bbs-write-head! id hash)
    (forum-index-add-topic! id hash)
    id))

(define (topic-reply! topic-id body . args)
  (doc 'type '(-> String String String))
  (doc 'description "Add a reply to a topic. Returns reply ID.
Errors if the topic is locked or archived.")
  (doc 'export #t)
  (let* ([author (get-keyword-arg args 'author "system")]
         [parent-hash (get-keyword-arg args 'parent-hash #f)]
         [topic-hash (bbs-issue-hash topic-id)])
    (unless topic-hash
      (error 'topic-reply! "Topic not found" topic-id))
    ;; Check topic status — reject replies to locked/archived topics
    (let ([blk (bbs-fetch topic-hash)])
      (when blk
        (let ([data (topic-block-data blk)])
          (when data
            (let ([status (cdr (assq 'status data))])
              (when (eq? status 'locked)
                (error 'topic-reply! "Topic is locked" topic-id))
              (when (eq? status 'archived)
                (error 'topic-reply! "Topic is archived" topic-id)))))))
    (let* ([reply-id (board-next-reply-id! topic-id)]
           [timestamp (bbs-timestamp)]
           [blk (make-reply-block topic-hash reply-id body author timestamp parent-hash)]
           [hash (bbs-store! blk)])
      (bbs-write-head! reply-id hash)
      (forum-index-add-reply! topic-id reply-id hash)
      reply-id)))

(define (topic-lock! topic-id)
  (doc 'type '(-> String (Option Bytevector)))
  (doc 'description "Lock a topic (no new replies). Returns new hash or #f if already locked.")
  (doc 'export #t)
  (let ([data (item-fetch-data topic-id)])
    (if (and data (eq? (cdr (assq 'status data)) 'locked))
        #f
        (topic-update-status! topic-id 'locked))))

(define (topic-archive! topic-id)
  (doc 'type '(-> String (Option Bytevector)))
  (doc 'description "Archive a topic. Returns new hash or #f if already archived.")
  (doc 'export #t)
  (let ([data (item-fetch-data topic-id)])
    (if (and data (eq? (cdr (assq 'status data)) 'archived))
        #f
        (topic-update-status! topic-id 'archived))))

(define (topic-update-status! topic-id new-status)
  (doc 'type '(-> String Symbol Bytevector))
  (doc 'description "Update a topic's status")
  (let* ([current-hash (bbs-issue-hash topic-id)]
         [blk (bbs-fetch current-hash)]
         [data (topic-block-data blk)]
         [old-status (cdr (assq 'status data))]
         [version (+ (cdr (assq 'version data)) 1)]
         [new-blk (make-topic-block
                    topic-id
                    (cdr (assq 'title data))
                    (cdr (assq 'body data))
                    (cdr (assq 'author data))
                    (cdr (assq 'created-at data))
                    new-status
                    version
                    current-hash)]
         [new-hash (bbs-store! new-blk)])
    (bbs-write-head! topic-id new-hash)
    ;; Update index
    (let ([items (bbs-current-items-ht)]
          [statuses (bbs-current-status-ht)])
      (hashtable-set! items topic-id new-hash)
      (when (and statuses (not (eq? old-status new-status)))
        (let ([old-list (hashtable-ref statuses old-status '())])
          (hashtable-set! statuses old-status
                          (filter (lambda (x) (not (string=? x topic-id))) old-list)))
        (let ([new-list (hashtable-ref statuses new-status '())])
          (hashtable-set! statuses new-status (cons topic-id new-list)))))
    new-hash))

(doc 'section 'topic-queries)

(define (topic-replies topic-id)
  (doc 'type '(-> String (List Alist)))
  (doc 'description "Get all replies for a topic, in chronological order")
  (doc 'export #t)
  (let* ([b (current-board)]
         [reply-map (board-reply-map-ht b)]
         [reply-ids (if reply-map
                        (hashtable-ref reply-map topic-id '())
                        '())])
    (filter-map
     (lambda (rid)
       (let ([hash (bbs-read-head rid)])
         (and hash
              (let ([blk (bbs-fetch hash)])
                (and blk (reply-block-data blk))))))
     (reverse reply-ids))))  ; reverse because ids are consed newest-first

(define (topic-reply-count topic-id)
  (doc 'type '(-> String Int))
  (doc 'description "Get the number of replies for a topic")
  (doc 'export #t)
  (let* ([b (current-board)]
         [reply-map (board-reply-map-ht b)])
    (if reply-map
        (length (hashtable-ref reply-map topic-id '()))
        0)))

(doc 'section 'topic-display)

(define (topic-show topic-id)
  (doc 'type '(-> String Void))
  (doc 'description "Display a topic with its replies")
  (doc 'export #t)
  (let ([data (item-fetch-data topic-id)])
    (if (not data)
        (printf "Topic not found: ~a~n" topic-id)
        (begin
          (printf "~a~n" (make-string 60 #\=))
          (printf "~a~n" (cdr (assq 'title data)))
          (printf "~a~n" (make-string 60 #\=))
          (printf "ID:      ~a~n" (cdr (assq 'id data)))
          (printf "Author:  ~a~n" (cdr (assq 'author data)))
          (printf "Status:  ~a~n" (cdr (assq 'status data)))
          (printf "Date:    ~a~n" (cdr (assq 'created-at data)))
          (printf "~a~n~n" (make-string 60 #\-))
          (printf "~a~n" (cdr (assq 'body data)))
          (printf "~a~n" (make-string 60 #\-))
          ;; Show replies
          (let ([replies (topic-replies topic-id)])
            (unless (null? replies)
              (printf "~n~a replies:~n" (length replies))
              (for-each
               (lambda (reply)
                 (printf "~n  ~a (~a):~n"
                         (cdr (assq 'author reply))
                         (cdr (assq 'created-at reply)))
                 (printf "  ~a~n" (cdr (assq 'body reply))))
               replies)))
          (printf "~a~n" (make-string 60 #\=))))))
