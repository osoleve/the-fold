;;; boundary/bbs/blocks.ss — BBS Block Creation Helpers
;;;
;;; Creates CAS blocks for the bulletin board system.
;;;
;;; Block Tags:
;;;   bbs/issue    - Issue entity with fields
;;;   bbs/event    - State change event (audit trail)
;;;   bbs/comment  - Comment on issue
;;;   bbs/dep      - Dependency relation (A blocks B)
;;;   bbs/index    - Root index of all issues
;;;   bbs/post     - General post (changelog, note, announcement)
;;;
;;; This is Shell code: uses Core block primitives.

(load "core/base/prelude.ss")
(load "core/blocks/block.ss")
(load "core/blocks/cas.ss")

;;; ====
;;; Block Tags
;;; ====

(define BBS-ISSUE 'bbs/issue)
(define BBS-EVENT 'bbs/event)
(define BBS-COMMENT 'bbs/comment)
(define BBS-DEP 'bbs/dep)
(define BBS-INDEX 'bbs/index)
(define BBS-POST 'bbs/post)

;;; ====
;;; Issue Block
;;; ====

;;; make-issue-block : String String String Symbol Int Symbol (List Symbol) String String Int (Option Bytevector) -> Block
;;; Create an issue block.
;;;
;;; Arguments:
;;;   id          - Human-readable ID (e.g., "fold-0001")
;;;   title       - Issue title
;;;   description - Issue description
;;;   status      - 'open | 'in_progress | 'closed
;;;   priority    - 0-4 (0=critical, 4=backlog)
;;;   type        - 'task | 'bug | 'feature | 'epic
;;;   labels      - List of label symbols
;;;   created-at  - ISO 8601 timestamp
;;;   created-by  - Username
;;;   version     - Version number (increments on update)
;;;   prev-hash   - Hash of previous version (or #f for first version)
(define (make-issue-block id title description status priority type labels
                          created-at created-by version prev-hash)
  (let* ([payload-data `((id . ,id)
                         (title . ,title)
                         (description . ,description)
                         (status . ,status)
                         (priority . ,priority)
                         (type . ,type)
                         (labels . ,labels)
                         (created-at . ,created-at)
                         (created-by . ,created-by)
                         (version . ,version))]
         [payload (string->utf8 (format "~s" payload-data))]
         [refs (if prev-hash
                   (vector prev-hash)
                   (vector))])
    (make-block BBS-ISSUE payload refs)))

;;; issue-block-data : Block -> Alist | #f
;;; Extract issue data from a block.
(define (issue-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) BBS-ISSUE))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; issue-block-prev : Block -> Bytevector | #f
;;; Get the previous version hash from an issue block.
(define (issue-block-prev blk)
  (let ([refs (block-refs blk)])
    (if (> (vector-length refs) 0)
        (vector-ref refs 0)
        #f)))

;;; ====
;;; Event Block
;;; ====

;;; make-event-block : Bytevector Symbol Any Any String String -> Block
;;; Create an event block recording a state change.
;;;
;;; Arguments:
;;;   issue-hash - Hash of the issue this event relates to
;;;   event-type - 'status-change | 'priority-change | 'title-change | etc.
;;;   old-value  - Previous value
;;;   new-value  - New value
;;;   timestamp  - ISO 8601 timestamp
;;;   actor      - Username who made the change
(define (make-event-block issue-hash event-type old-value new-value timestamp actor)
  (let* ([payload-data `((event-type . ,event-type)
                         (old-value . ,old-value)
                         (new-value . ,new-value)
                         (timestamp . ,timestamp)
                         (actor . ,actor))]
         [payload (string->utf8 (format "~s" payload-data))])
    (make-block BBS-EVENT payload (vector issue-hash))))

;;; event-block-data : Block -> Alist | #f
(define (event-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) BBS-EVENT))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; ====
;;; Comment Block
;;; ====

;;; make-comment-block : Bytevector Int String String Symbol String -> Block
;;; Create a comment block.
;;;
;;; Arguments:
;;;   issue-hash   - Hash of the issue
;;;   comment-id   - Sequential comment ID within issue
;;;   author       - Username
;;;   text         - Comment text
;;;   content-type - 'text | 'code | 'tool-result | 'thought
;;;   created-at   - ISO 8601 timestamp
(define (make-comment-block issue-hash comment-id author text content-type created-at)
  (let* ([payload-data `((id . ,comment-id)
                         (author . ,author)
                         (text . ,text)
                         (content-type . ,content-type)
                         (created-at . ,created-at))]
         [payload (string->utf8 (format "~s" payload-data))])
    (make-block BBS-COMMENT payload (vector issue-hash))))

;;; comment-block-data : Block -> Alist | #f
(define (comment-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) BBS-COMMENT))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; ====
;;; Dependency Block
;;; ====

;;; make-dep-block : Bytevector Bytevector -> Block
;;; Create a dependency block: blocker-hash blocks blocked-hash.
;;; refs[0] = blocker, refs[1] = blocked
(define (make-dep-block blocker-hash blocked-hash)
  (make-block BBS-DEP #vu8() (vector blocker-hash blocked-hash)))

;;; dep-block-blocker : Block -> Bytevector
;;; Get the blocker hash from a dep block.
(define (dep-block-blocker blk)
  (vector-ref (block-refs blk) 0))

;;; dep-block-blocked : Block -> Bytevector
;;; Get the blocked hash from a dep block.
(define (dep-block-blocked blk)
  (vector-ref (block-refs blk) 1))

;;; ====
;;; Index Block
;;; ====

;;; make-index-block : (List Bytevector) String -> Block
;;; Create an index block aggregating all issue hashes.
;;;
;;; Arguments:
;;;   issue-hashes - List of all current issue hashes
;;;   timestamp    - When index was created
(define (make-index-block issue-hashes timestamp)
  (let* ([payload-data `((count . ,(length issue-hashes))
                         (timestamp . ,timestamp))]
         [payload (string->utf8 (format "~s" payload-data))])
    (make-block BBS-INDEX payload (list->vector issue-hashes))))

;;; index-block-data : Block -> Alist | #f
(define (index-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) BBS-INDEX))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; index-block-issues : Block -> (Vector Bytevector)
;;; Get the issue hashes from an index block.
(define (index-block-issues blk)
  (block-refs blk))

;;; ====
;;; Post Block
;;; ====

;;; make-post-block : String String String Symbol (List Symbol) String String Int (Option Bytevector) -> Block
;;; Create a post block for changelogs, notes, announcements.
;;;
;;; Arguments:
;;;   id          - Human-readable ID (e.g., "post-001")
;;;   title       - Post title
;;;   body        - Post body (markdown)
;;;   post-type   - 'changelog | 'note | 'announcement | 'session-summary
;;;   tags        - List of tag symbols
;;;   created-at  - ISO 8601 timestamp
;;;   author      - Username
;;;   version     - Version number (increments on edit)
;;;   prev-hash   - Hash of previous version (or #f for first version)
(define (make-post-block id title body post-type tags created-at author version prev-hash)
  (let* ([payload-data `((id . ,id)
                         (title . ,title)
                         (body . ,body)
                         (post-type . ,post-type)
                         (tags . ,tags)
                         (created-at . ,created-at)
                         (author . ,author)
                         (version . ,version))]
         [payload (string->utf8 (format "~s" payload-data))]
         [refs (if prev-hash
                   (vector prev-hash)
                   (vector))])
    (make-block BBS-POST payload refs)))

;;; post-block-data : Block -> Alist | #f
;;; Extract post data from a block.
(define (post-block-data blk)
  (if (and (block? blk) (eq? (block-tag blk) BBS-POST))
      (read (open-input-string (utf8->string (block-payload blk))))
      #f))

;;; post-block-prev : Block -> Bytevector | #f
;;; Get the previous version hash from a post block.
(define (post-block-prev blk)
  (let ([refs (block-refs blk)])
    (if (> (vector-length refs) 0)
        (vector-ref refs 0)
        #f)))
