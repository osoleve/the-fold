(doc 'module 'boundary/bbs/forum-index)
(doc 'description "BBS Forum Index - Per-board forum index for topics and replies")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Forum index maintains:
  items-ht   (from board record) - topic-id -> hash
  by-status  (from board record) - status -> (topic-id ...)
  reply-map  (from board record) - topic-id -> (reply-id ...)

Reply classification: head files prefixed with 'reply-' are replies,
others are topics.")

(doc 'section 'index-operations)

(define (forum-index-add-topic! topic-id hash)
  (doc 'type '(-> String Bytevector Void))
  (doc 'description "Add a topic to the forum index")
  (doc 'export #t)
  (let ([items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)])
    (hashtable-set! items topic-id hash)
    (when statuses
      (let ([existing (hashtable-ref statuses 'open '())])
        (hashtable-set! statuses 'open (cons topic-id existing))))))

(define (forum-index-add-reply! topic-id reply-id hash)
  (doc 'type '(-> String String Bytevector Void))
  (doc 'description "Add a reply to the forum index reply-map")
  (doc 'export #t)
  (let* ([b (current-board)]
         [reply-map (board-reply-map-ht b)])
    (when reply-map
      (let ([existing (hashtable-ref reply-map topic-id '())])
        (hashtable-set! reply-map topic-id (cons reply-id existing))))))

(doc 'section 'index-rebuild)

(define (forum-rebuild-index!)
  (doc 'type '(-> Int))
  (doc 'description "Rebuild forum index from head files. Must be in forum board context.")
  (doc 'returns "Number of topics indexed")
  (doc 'export #t)
  (let ([items (bbs-current-items-ht)]
        [statuses (bbs-current-status-ht)]
        [b (current-board)]
        [reply-map (board-reply-map-ht b)])
    ;; Clear
    (hashtable-clear! items)
    (when statuses (hashtable-clear! statuses))
    (when reply-map (hashtable-clear! reply-map))

    (let* ([all-heads (bbs-list-heads)]
           [topic-count 0])
      ;; Classify heads into topics and replies
      (for-each
       (lambda (id)
         (let ([hash (bbs-read-head id)])
           (when hash
             (if (string-starts-with? id "reply-")
                 ;; Reply: extract topic-id from "reply-<topic-id>-<n>" format
                 (let ([topic-id (reply-id->topic-id id)])
                   (when (and topic-id reply-map)
                     (let ([existing (hashtable-ref reply-map topic-id '())])
                       (hashtable-set! reply-map topic-id (cons id existing)))))
                 ;; Topic
                 (let ([blk (bbs-fetch hash)])
                   (when blk
                     (let ([data (topic-block-data blk)])
                       (when data
                         (hashtable-set! items id hash)
                         (set! topic-count (+ topic-count 1))
                         (when statuses
                           (let* ([status (cdr (assq 'status data))]
                                  [existing (hashtable-ref statuses status '())])
                             (hashtable-set! statuses status (cons id existing))))))))))))
       all-heads)

      ;; Sync counter
      (let ([topic-ids (filter (lambda (id) (not (string-starts-with? id "reply-"))) all-heads)])
        (bbs-sync-counter-from-heads! topic-ids))

      topic-count)))

(doc 'section 'reply-id-parsing)

(define (reply-id->topic-id reply-id)
  (doc 'type '(-> String (Option String)))
  (doc 'description "Extract topic-id from reply-id format: reply-<topic-id>-<n>")
  (doc 'export #t)
  ;; reply-<topic-id>-<nnn>
  ;; Find last dash, everything between "reply-" and last dash is topic-id
  (let ([len (string-length reply-id)])
    (if (< len 7)  ; minimum: "reply-x"
        #f
        (let ([prefix "reply-"])
          (if (not (string=? (substring reply-id 0 6) prefix))
              #f
              (let loop ([i (- len 1)])
                (cond
                 [(< i 6) #f]  ; no dash found after prefix
                 [(char=? (string-ref reply-id i) #\-)
                  (substring reply-id 6 i)]
                 [else (loop (- i 1))])))))))

(define (string-starts-with? str prefix)
  (doc 'type '(-> String String Boolean))
  (doc 'description "Check if string starts with prefix")
  (let ([plen (string-length prefix)])
    (and (>= (string-length str) plen)
         (string=? (substring str 0 plen) prefix))))

(doc 'section 'forum-save-load-cache)

(define (forum-save-index-cache!)
  (doc 'type '(-> Void))
  (doc 'description "Save forum index cache to disk")
  (doc 'export #t)
  ;; Reuse the tracker cache format for topics; replies are cheap to rebuild
  (bbs-save-index-cache!))

(define (forum-load-index-cache!)
  (doc 'type '(-> Boolean))
  (doc 'description "Load forum index cache from disk")
  (doc 'export #t)
  ;; For now, always rebuild (reply-map isn't cached)
  #f)
