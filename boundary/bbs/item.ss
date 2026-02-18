(doc 'module 'boundary/bbs/item)
(doc 'description "BBS Polymorphic Item API - Board-type-dispatched operations")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Provides a uniform item API that dispatches by current board type.
  tracker -> issue operations
  feed    -> post operations
  forum   -> topic/reply operations")

(doc 'section 'item-creation)

(define (item-create! title . args)
  (doc 'type '(-> String String))
  (doc 'description "Create a new item on the current board. Dispatches by board type.")
  (doc 'export #t)
  (case (current-board-type)
    [(tracker)
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
       id)]
    [(feed)
     (let* ([body (get-keyword-arg args 'body "")]
            [post-type (get-keyword-arg args 'post-type 'note)]
            [tags (get-keyword-arg args 'tags '())]
            [author (get-keyword-arg args 'author "system")]
            [id (bbs-next-id!)]  ; board-scoped counter + prefix
            [timestamp (bbs-timestamp)]
            [blk (make-post-block id title body post-type tags timestamp author 1 #f)]
            [hash (bbs-store! blk)])
       (bbs-write-head! id hash)
       (post-index-add! id hash)
       id)]
    [(forum)
     (let* ([body (get-keyword-arg args 'body "")]
            [author (get-keyword-arg args 'author "system")]
            [id (board-next-topic-id!)]
            [timestamp (bbs-timestamp)]
            [blk (make-topic-block id title body author timestamp 'open 1 #f)]
            [hash (bbs-store! blk)])
       (bbs-write-head! id hash)
       (forum-index-add-topic! id hash)
       id)]
    [else (error 'item-create! "Unknown board type" (current-board-type))]))

(doc 'section 'item-fetch)

(define (item-fetch id)
  (doc 'type '(-> (Or String Symbol) (Option Block)))
  (doc 'description "Fetch an item by ID from the current board")
  (doc 'export #t)
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
         [hash (hashtable-ref (bbs-current-items-ht) id-str #f)])
    (if hash
        (bbs-fetch hash)
        ;; Try disk refresh
        (let ([disk-hash (bbs-read-head id-str)])
          (and disk-hash (bbs-fetch disk-hash))))))

(define (item-fetch-data id)
  (doc 'type '(-> (Or String Symbol) (Option Alist)))
  (doc 'description "Fetch and parse item data by ID from the current board")
  (doc 'export #t)
  (let ([blk (item-fetch id)])
    (and blk
         (case (current-board-type)
           [(tracker) (issue-block-data blk)]
           [(feed) (post-block-data blk)]
           [(forum) (topic-block-data blk)]
           [else #f]))))

(doc 'section 'item-listing)

(define (item-count)
  (doc 'type '(-> Int))
  (doc 'description "Get the number of items on the current board")
  (doc 'export #t)
  (hashtable-size (bbs-current-items-ht)))

(define (item-all-ids)
  (doc 'type '(-> (List String)))
  (doc 'description "Get all item IDs on the current board")
  (doc 'export #t)
  (vector->list (hashtable-keys (bbs-current-items-ht))))

(doc 'section 'item-display)

(define (item-show id)
  (doc 'type '(-> (Or String Symbol) Void))
  (doc 'description "Display an item from the current board")
  (doc 'export #t)
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
         [data (item-fetch-data id-str)])
    (if (not data)
        (printf "Item not found: ~a~n" id-str)
        (case (current-board-type)
          [(tracker)
           (printf "~a~n" (make-string 60 #\-))
           (printf "ID:          ~a~n" (cdr (assq 'id data)))
           (printf "Title:       ~a~n" (cdr (assq 'title data)))
           (printf "Status:      ~a~n" (cdr (assq 'status data)))
           (printf "Priority:    P~a~n" (cdr (assq 'priority data)))
           (printf "Type:        ~a~n" (cdr (assq 'type data)))
           (printf "~a~n" (make-string 60 #\-))]
          [(feed)
           (printf "~a~n" (make-string 60 #\=))
           (printf "~a~n" (cdr (assq 'title data)))
           (printf "~a~n" (make-string 60 #\=))
           (printf "ID:      ~a~n" (cdr (assq 'id data)))
           (printf "Type:    ~a~n" (cdr (assq 'post-type data)))
           (printf "Author:  ~a~n" (cdr (assq 'author data)))
           (printf "~a~n~n" (make-string 60 #\-))
           (printf "~a~n" (cdr (assq 'body data)))
           (printf "~a~n" (make-string 60 #\=))]
          [(forum)
           (printf "~a~n" (make-string 60 #\-))
           (printf "Topic:   ~a~n" (cdr (assq 'title data)))
           (printf "ID:      ~a~n" (cdr (assq 'id data)))
           (printf "Author:  ~a~n" (cdr (assq 'author data)))
           (printf "Status:  ~a~n" (cdr (assq 'status data)))
           (printf "~a~n~n" (make-string 60 #\-))
           (printf "~a~n" (cdr (assq 'body data)))
           (printf "~a~n" (make-string 60 #\-))]))))
