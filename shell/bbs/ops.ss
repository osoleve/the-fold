;;; shell/bbs/ops.ss — BBS Issue Operations
;;;
;;; Create, update, close, and manage issues.
;;;
;;; This is Shell code: impure (modifies state and filesystem).

(load "shell/bbs/index.ss")

;;; ====
;;; Timestamp Generation
;;; ====

;;; bbs-timestamp : -> String
;;; Generate an ISO 8601 timestamp for now.
(define (bbs-timestamp)
  (let ([t (current-date)])
    (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0dZ"
            (date-year t)
            (date-month t)
            (date-day t)
            (date-hour t)
            (date-minute t)
            (date-second t))))

;;; ====
;;; Issue Creation
;;; ====

;;; bbs-create : String -> String
;;; Create a new issue with just a title.
;;; Returns the new issue ID.
(define (bbs-create title . args)
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
    ;; Write head file
    (bbs-write-head! id hash)
    ;; Add to index
    (bbs-index-add! id hash)
    id))

;;; get-keyword-arg : (List Any) Symbol Any -> Any
;;; Extract a keyword argument from an argument list.
(define (get-keyword-arg args key default)
  (let loop ([lst args])
    (cond
     [(null? lst) default]
     [(null? (cdr lst)) default]
     [(eq? (car lst) key) (cadr lst)]
     [else (loop (cdr lst))])))

;;; ====
;;; Issue Updates
;;; ====

;;; bbs-update : String -> Bytevector
;;; Update an issue. Returns the new hash.
;;;
;;; Keyword arguments:
;;;   'status - New status
;;;   'priority - New priority
;;;   'title - New title
;;;   'description - New description
;;;   'labels - New labels
;;;   'expect-hash - Expected current hash (for OCC)
(define (bbs-update id . args)
  (let* ([id-str (if (symbol? id) (symbol->string id) id)]
         [current-hash (bbs-issue-hash id-str)])

    ;; Existence check
    (unless current-hash
      (error 'bbs-update "Issue not found" id-str))

    (let* ([expect-hash (get-keyword-arg args 'expect-hash #f)]
           [blk (bbs-fetch current-hash)]
           [data (issue-block-data blk)])

      ;; OCC check if expect-hash provided
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

      ;; Update head
      (bbs-write-head! id-str new-hash)

      ;; Update index
      (bbs-index-update! id-str new-hash old-status new-status old-priority new-priority)

      new-hash))))

;;; ====
;;; Issue Close/Reopen
;;; ====

;;; bbs-close : String -> Bytevector
;;; Close an issue.
;;;
;;; Keyword arguments:
;;;   'reason - Reason for closing
(define (bbs-close id . args)
  (let ([reason (get-keyword-arg args 'reason "")])
    (bbs-update id 'status 'closed)))

;;; bbs-reopen : String -> Bytevector
;;; Reopen a closed issue.
(define (bbs-reopen id)
  (bbs-update id 'status 'open))

;;; ====
;;; Dependencies
;;; ====

;;; bbs-dep : String|Symbol String|Symbol -> Void
;;; Add a dependency: blocker blocks blocked.
(define (bbs-dep blocker blocked)
  (let* ([blocker-str (if (symbol? blocker) (symbol->string blocker) blocker)]
         [blocked-str (if (symbol? blocked) (symbol->string blocked) blocked)]
         [blocker-hash (bbs-issue-hash blocker-str)]
         [blocked-hash (bbs-issue-hash blocked-str)])
    (when (and blocker-hash blocked-hash)
      ;; Create and store dep block
      (let* ([dep-blk (make-dep-block blocker-hash blocked-hash)]
             [dep-hash (bbs-store! dep-blk)])
        ;; Add to index
        (bbs-add-dep! blocker-str blocked-str)))))

;;; bbs-undep : String|Symbol String|Symbol -> Void
;;; Remove a dependency.
(define (bbs-undep blocker blocked)
  (let ([blocker-str (if (symbol? blocker) (symbol->string blocker) blocker)]
        [blocked-str (if (symbol? blocked) (symbol->string blocked) blocked)])
    (bbs-remove-dep! blocker-str blocked-str)))

;;; ====
;;; Comments
;;; ====

;;; *bbs-comment-counter* : Hashtable String -> Int
;;; Track next comment ID per issue.
(define *bbs-comment-counter* (make-hashtable string-hash string=?))

;;; bbs-next-comment-id! : String -> Int
(define (bbs-next-comment-id! issue-id)
  (let ([current (hashtable-ref *bbs-comment-counter* issue-id 0)])
    (hashtable-set! *bbs-comment-counter* issue-id (+ current 1))
    (+ current 1)))

;;; bbs-comment : String|Symbol String -> Bytevector
;;; Add a comment to an issue.
;;;
;;; Keyword arguments:
;;;   'content-type - 'text | 'code | 'tool-result | 'thought
;;;   'author - Comment author
(define (bbs-comment issue-id text . args)
  (let* ([id-str (if (symbol? issue-id) (symbol->string issue-id) issue-id)]
         [content-type (get-keyword-arg args 'content-type 'text)]
         [author (get-keyword-arg args 'author "system")]
         [issue-hash (bbs-issue-hash id-str)]
         [comment-id (bbs-next-comment-id! id-str)]
         [timestamp (bbs-timestamp)]
         [blk (make-comment-block issue-hash comment-id author text
                                  content-type timestamp)]
         [hash (bbs-store! blk)])
    hash))
