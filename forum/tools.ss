;;; forum/tools.ss — Merkle Log Forum Structure
;;;
;;; The Forum is a collection of append-only Merkle logs, one per channel.
;;; Each post is a Block that references its parent(s), forming a chain.
;;;
;;; This is Shell code: uses filesystem capabilities, handles IO.
;;;
;;; Post structure (as Block):
;;;   tag: 'forum-post
;;;   payload: S-expression of post metadata and body
;;;   refs[0]: previous head of channel (or empty for genesis)
;;;   refs[1..]: parent posts for threading (optional)
;;;
;;; Post payload format:
;;;   ((author . <symbol>)
;;;    (tier . <symbol>)
;;;    (timestamp . <string>)
;;;    (channel . <symbol>)
;;;    (parent . <hash-hex> | #f)   ; for threading display
;;;    (body . <string>))

;;; ============================================================
;;; Dependencies
;;; ============================================================

;;; These must be loaded before this file:
;;;   core/block.ss
;;;   core/sha256.ss
;;;   shell/fs.ss
;;;   shell/text.ss (for validation)

;;; ============================================================
;;; Post Construction
;;; ============================================================

;;; make-post-payload : Alist → Bytevector
;;; Serialize post metadata to payload bytes.
(define (make-post-payload alist)
  (string->utf8 (sexpr->string alist)))

;;; parse-post-payload : Bytevector → Alist
;;; Deserialize post metadata from payload.
(define (parse-post-payload bytes)
  (read (open-input-string (utf8->string bytes))))

;;; make-post-block : Alist × (List Bytevector) → Block
;;; Create a forum post block.
(define (make-post-block metadata refs)
  (make-block 'forum-post (make-post-payload metadata) (list->vector refs)))

;;; ============================================================
;;; Channel Operations
;;; ============================================================

;;; post! : FS × Symbol × Symbol × Symbol × String × String → Bytevector
;;; Create and store a new post, updating the channel head.
;;; Returns the hash of the new post.
(define (post! fs author tier channel body timestamp)
  (let* ([prev-head (fs-read-head fs channel)]
         [refs (if prev-head (list prev-head) '())]
         [metadata `((author . ,author)
                     (tier . ,tier)
                     (timestamp . ,timestamp)
                     (channel . ,channel)
                     (body . ,body))]
         [blk (make-post-block metadata refs)]
         [hash (fs-store! fs blk)])
        (fs-write-head! fs channel hash)
        (fs-pin! fs hash)
        hash))

;;; read-post : FS × Bytevector → Alist | #f
;;; Fetch and parse a post by hash.
(define (read-post fs hash)
  (let ([blk (fs-fetch fs hash)])
       (if blk
           (parse-post-payload (block-payload blk))
           #f)))

;;; channel-head : FS × Symbol → Bytevector | #f
;;; Get the current head hash of a channel.
(define (channel-head fs channel)
  (fs-read-head fs channel))

;;; ============================================================
;;; Channel Traversal
;;; ============================================================

;;; walk-channel : FS × Symbol × (Alist → void) → void
;;; Walk a channel from head to genesis, calling proc on each post.
(define (walk-channel fs channel proc)
  (let ([head (channel-head fs channel)])
       (when head
             (walk-from fs head proc))))

;;; walk-from : FS × Bytevector × (Alist → void) → void
;;; Walk the chain starting from a specific post.
;;; Gracefully handles corrupted blocks by skipping them.
(define (walk-from fs hash proc)
  (guard (e [else #f])  ; Skip this block and stop traversal on error
         (let ([blk (fs-fetch fs hash)])
              (when blk
                    (let ([metadata (parse-post-payload (block-payload blk))]
                          [refs (block-refs blk)])
                         (proc metadata)
                         ;; refs[0] is the previous head
                         (when (> (vector-length refs) 0)
                               (walk-from fs (vector-ref refs 0) proc)))))))

;;; collect-channel : FS × Symbol → (List Alist)
;;; Collect all posts in a channel (newest first).
(define (collect-channel fs channel)
  (let ([posts '()])
       (walk-channel fs channel
                     (lambda (post)
                             (set! posts (cons post posts))))
       (reverse posts)))

;;; walk-channel-with-hash : FS × Symbol × (Bytevector × Alist → void) → void
;;; Walk a channel from head to genesis, calling proc with (hash, metadata).
(define (walk-channel-with-hash fs channel proc)
  (let ([head (channel-head fs channel)])
       (when head
             (walk-from-with-hash fs head proc))))

;;; walk-from-with-hash : FS × Bytevector × (Bytevector × Alist → void) → void
;;; Walk the chain starting from a specific post, passing hash to proc.
;;; Gracefully handles corrupted blocks by skipping them.
(define (walk-from-with-hash fs hash proc)
  (guard (e [else #f])  ; Skip this block and stop traversal on error
         (let ([blk (fs-fetch fs hash)])
              (when blk
                    (let ([metadata (parse-post-payload (block-payload blk))]
                          [refs (block-refs blk)])
                         (proc hash metadata)
                         ;; refs[0] is the previous head
                         (when (> (vector-length refs) 0)
                               (walk-from-with-hash fs (vector-ref refs 0) proc)))))))

;;; collect-channel-with-hash : FS × Symbol → (List (Cons Bytevector Alist))
;;; Collect all posts in a channel with their hashes (newest first).
(define (collect-channel-with-hash fs channel)
  (let ([posts '()])
       (walk-channel-with-hash fs channel
                               (lambda (hash post)
                                       (set! posts (cons (cons hash post) posts))))
       (reverse posts)))

;;; ============================================================
;;; Channel Listing
;;; ============================================================

;;; list-channels : FS → (List Symbol)
;;; List all channels that have posts.
(define (list-channels fs)
  (let ([heads-path (string-append (fs-capability-store-path fs) "/heads")])
       (if (file-exists? heads-path)
           (map (lambda (filename)
                        (let ([name (path-stem filename)])
                             (string->symbol name)))
                (filter (lambda (f) (string-suffix? ".head" f))
                        (directory-list heads-path)))
           '())))

;;; Path utilities (path-stem, path-basename, find-last-sep, string-suffix?,
;;; string-rindex) are now consolidated in shell/fs.ss which is loaded first.

;;; ============================================================
;;; Post Formatting (for display)
;;; ============================================================

;;; format-post : Alist → String
;;; Format a post for human-readable display.
(define (format-post post)
  (let ([author (cdr (assq 'author post))]
        [tier (cdr (assq 'tier post))]
        [timestamp (cdr (assq 'timestamp post))]
        [channel (cdr (assq 'channel post))]
        [body (cdr (assq 'body post))])
       (format "~a (~a) @ ~a in #~a:\n~a\n"
               author tier timestamp channel body)))

;;; print-channel : FS × Symbol → void
;;; Print all posts in a channel.
(define (print-channel fs channel)
  (display (format "=== #~a ===\n\n" channel))
  (walk-channel fs channel
                (lambda (post)
                        (display (format-post post))
                        (display "\n---\n\n"))))

;;; ============================================================
;;; Genesis Support
;;; ============================================================

;;; genesis-post! : FS × Symbol × String → Bytevector
;;; Create the first post in a channel (no previous head).
;;; This is just post! but explicit about intent.
(define (genesis-post! fs channel body)
  (post! fs 'opus 'shepherd channel body (current-timestamp)))

;;; current-timestamp : → String
;;; Get current time as ISO 8601 string (YYYY-MM-DDTHH:MM:SS).
(define (current-timestamp)
  (let ([d (current-date)])
       (format "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
               (date-year d)
               (date-month d)
               (date-day d)
               (date-hour d)
               (date-minute d)
               (date-second d))))

;;; ============================================================
;;; Verification
;;; ============================================================

;;; verify-chain : FS × Symbol → Boolean
;;; Verify that a channel's chain is unbroken.
(define (verify-chain fs channel)
  (let ([head (channel-head fs channel)])
       (if (not head)
           #t  ; Empty channel is valid
           (let loop ([hash head])
                (let ([blk (fs-fetch fs hash)])
                     (cond
                      [(not blk) #f]  ; Missing block — broken chain
                      [(= (vector-length (block-refs blk)) 0) #t]  ; Genesis reached
                      [else (loop (vector-ref (block-refs blk) 0))]))))))
