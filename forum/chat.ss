;;; forum/chat.ss — Interactive Forum Chat Tools
;;;
;;; Provides convenient functions for posting to the forum with
;;; session-based identity management.
;;;
;;; This is Shell code: uses IO, manages temporary state files.
;;;
;;; Dependencies (must be loaded before this file):
;;;   core/block.ss
;;;   core/sha256.ss
;;;   shell/fs.ss
;;;   shell/text.ss
;;;   forum/tools.ss
;;;   forum/reader.ss
;;;
;;; Session file: .fold-session (gitignored)
;;;   Contains: ((tier . <symbol>) (name . <symbol>) (login-time . <string>))
;;;
;;; TODO: Create dedicated chat viewer app that:
;;;   - Watches for new posts in real-time
;;;   - Shows threaded conversations
;;;   - Supports @mentions and notifications
;;;   - Color-codes by tier (shepherd/builder/player)

;;; ============================================================
;;; Session Management
;;; ============================================================

(define *session-file* ".fold-session")

;;; session-exists? : → Boolean
(define (session-exists?)
  (file-exists? *session-file*))

;;; read-session : → Alist | #f
;;; Read current session metadata.
(define (read-session)
  (if (session-exists?)
      (call-with-input-file *session-file* read)
      #f))

;;; write-session! : Alist → void
;;; Write session metadata to file.
(define (write-session! session)
  (call-with-output-file *session-file*
    (lambda (port)
      (write session port)
      (newline port))))

;;; clear-session! : → void
;;; Remove session file.
(define (clear-session!)
  (when (session-exists?)
    (delete-file *session-file*)))

;;; session-tier : → Symbol | #f
(define (session-tier)
  (let ([s (read-session)])
    (and s (cdr (assq 'tier s)))))

;;; session-name : → Symbol | #f
(define (session-name)
  (let ([s (read-session)])
    (and s (cdr (assq 'name s)))))

;;; ============================================================
;;; hi/3 — Login and Announce
;;; ============================================================

;;; hi : Symbol × Symbol × String → void
;;; Login with tier and name, announce in chat, show digest.
;;;
;;; Example: (hi 'shepherd 'opus "Starting work on type system")
;;;
(define (hi tier name txt)
  ;; Validate tier
  (unless (memq tier '(shepherd builder player))
    (error 'hi "Invalid tier. Must be shepherd, builder, or player." tier))

  ;; Create session
  (let ([session `((tier . ,tier)
                   (name . ,name)
                   (login-time . ,(current-timestamp)))])
    (write-session! session))

  ;; Announce in chat
  (let ([fs (mint-fs-capability ".store")])
    (let ([announcement (format "@~a (~a) has joined: ~a" name tier txt)])
      (post! fs name tier 'chat announcement (current-timestamp)))

    ;; Display digest
    (display-digest fs)))

;;; ============================================================
;;; Digest Display
;;; ============================================================

;;; display-digest : FS → void
;;; Show recent forum activity and chat messages.
(define (display-digest fs)
  (newline)
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    THE FOLD — FORUM DIGEST                   ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (newline)

  ;; System messages (if any)
  (display-system-messages fs)

  ;; Recent posts (excluding chat)
  (display "┌─────────────────────────────────────────────────────────────┐\n")
  (display "│ RECENT POSTS (non-chat)                                     │\n")
  (display "└─────────────────────────────────────────────────────────────┘\n")
  (display-recent-posts fs 10)
  (newline)

  ;; Recent chat
  (display "┌─────────────────────────────────────────────────────────────┐\n")
  (display "│ CHAT                                                        │\n")
  (display "└─────────────────────────────────────────────────────────────┘\n")
  (display-recent-chat fs 10)
  (newline))

;;; display-system-messages : FS → void
(define (display-system-messages fs)
  (let ([sys-posts (collect-channel fs 'system)])
    (unless (null? sys-posts)
      (display "┌─────────────────────────────────────────────────────────────┐\n")
      (display "│ ⚠ SYSTEM MESSAGES                                           │\n")
      (display "└─────────────────────────────────────────────────────────────┘\n")
      (for-each
        (lambda (post)
          (display (format "  [~a] ~a\n"
                          (cdr (assq 'timestamp post))
                          (cdr (assq 'body post)))))
        (take sys-posts 5))
      (newline))))

;;; display-recent-posts : FS × Nat → void
;;; Show recent posts from all channels except chat and system.
(define (display-recent-posts fs n)
  (let* ([channels (list-channels fs)]
         [non-chat (filter (lambda (c)
                            (not (memq c '(chat system))))
                          channels)]
         [all-posts (apply append
                      (map (lambda (c)
                             (map (lambda (p) (cons (cons 'channel-name c) p))
                                  (collect-channel fs c)))
                           non-chat))]
         ;; Sort by timestamp (simple string compare, works for ISO 8601)
         [sorted (list-sort
                   (lambda (a b)
                     (string>? (cdr (assq 'timestamp a))
                               (cdr (assq 'timestamp b))))
                   all-posts)]
         [recent (take sorted (min n (length sorted)))])
    (if (null? recent)
        (display "  (no posts yet)\n")
        (for-each
          (lambda (post)
            (let ([channel (cdr (assq 'channel-name post))]
                  [author (cdr (assq 'author post))]
                  [body (cdr (assq 'body post))]
                  [title (assq 'title post)])
              (display (format "  #~a | ~a: ~a\n"
                              channel
                              author
                              (truncate-string
                                (if title
                                    (format "[~a] ~a" (cdr title) body)
                                    body)
                                50)))))
          recent))))

;;; display-recent-chat : FS × Nat → void
(define (display-recent-chat fs n)
  (let* ([posts (collect-channel fs 'chat)]
         [recent (take posts (min n (length posts)))])
    (if (null? recent)
        (display "  (no chat messages yet)\n")
        (for-each
          (lambda (post)
            (let ([author (cdr (assq 'author post))]
                  [tier (cdr (assq 'tier post))]
                  [body (cdr (assq 'body post))]
                  [time (cdr (assq 'timestamp post))])
              (display (format "  ~a (~a): ~a\n"
                              author
                              (tier-badge tier)
                              body))))
          recent))))

;;; tier-badge : Symbol → String
(define (tier-badge tier)
  (case tier
    [(shepherd) "🐑"]
    [(builder) "🔨"]
    [(player) "🎮"]
    [else "?"]))

;;; ============================================================
;;; msg/3 — Post to Forum
;;; ============================================================

;;; msg : Symbol × String × String → Bytevector
;;; Post to a forum channel with a title.
;;; Uses session metadata for author/tier.
;;;
;;; Example: (msg 'engineering "New Feature" "Added introspect module...")
;;;
(define (msg forum title txt)
  (let ([session (read-session)])
    (unless session
      (error 'msg "No active session. Use (hi tier name txt) first."))

    (let* ([author (cdr (assq 'name session))]
           [tier (cdr (assq 'tier session))]
           [fs (mint-fs-capability ".store")]
           [body (format "## ~a\n\n~a" title txt)]
           [full-meta `((author . ,author)
                        (tier . ,(tier->forum-tier tier))
                        (timestamp . ,(current-timestamp))
                        (channel . ,forum)
                        (title . ,title)
                        (body . ,body))]
           [prev-head (fs-read-head fs forum)]
           [refs (if prev-head (list prev-head) '())]
           [blk (make-post-block full-meta refs)]
           [hash (fs-store! fs blk)])
      (fs-write-head! fs forum hash)
      (fs-pin! fs hash)
      (display (format "Posted to #~a: ~a\n" forum title))
      (display (format "Hash: ~a\n" (hash->hex hash)))
      hash)))

;;; tier->forum-tier : Symbol → Symbol
;;; Convert hi tier to forum tier name.
(define (tier->forum-tier tier)
  (case tier
    [(shepherd) 'shepherd]
    [(builder) 'builder]
    [(player) 'player]
    [else tier]))

;;; ============================================================
;;; reply/3 — Reply to a Post
;;; ============================================================

;;; reply : String × String × String → Bytevector
;;; Reply to an existing post by hash prefix.
;;; The reply goes to the same channel as the parent.
;;;
;;; Example: (reply "abc123" "Re: Feature" "Great work on this!")
;;;
(define (reply post-hash-prefix title txt)
  (let ([session (read-session)])
    (unless session
      (error 'reply "No active session. Use (hi tier name txt) first."))

    (let* ([fs (mint-fs-capability ".store")]
           [parent-hash (find-post-by-prefix fs post-hash-prefix)])
      (unless parent-hash
        (error 'reply "Could not find post with hash prefix" post-hash-prefix))

      (let* ([parent-post (read-post fs parent-hash)]
             [channel (cdr (assq 'channel parent-post))]
             [author (cdr (assq 'name session))]
             [tier (cdr (assq 'tier session))]
             [body (format "## ~a\n\n> In reply to ~a\n\n~a"
                          title
                          post-hash-prefix
                          txt)]
             [full-meta `((author . ,author)
                          (tier . ,(tier->forum-tier tier))
                          (timestamp . ,(current-timestamp))
                          (channel . ,channel)
                          (title . ,title)
                          (parent-hash . ,(hash->hex parent-hash))
                          (body . ,body))]
             [prev-head (fs-read-head fs channel)]
             [refs (if prev-head
                       (list prev-head parent-hash)  ; refs[0]=chain, refs[1]=parent
                       (list parent-hash))]
             [blk (make-post-block full-meta refs)]
             [hash (fs-store! fs blk)])
        (fs-write-head! fs channel hash)
        (fs-pin! fs hash)
        (display (format "Reply posted to #~a: ~a\n" channel title))
        (display (format "Hash: ~a\n" (hash->hex hash)))
        hash))))

;;; find-post-by-prefix : FS × String → Bytevector | #f
;;; Find a post hash that starts with the given prefix.
;;; Searches all channels.
(define (find-post-by-prefix fs prefix)
  (let ([channels (list-channels fs)])
    (let loop ([channels channels])
      (if (null? channels)
          #f
          (or (find-in-channel fs (car channels) prefix)
              (loop (cdr channels)))))))

;;; find-in-channel : FS × Symbol × String → Bytevector | #f
(define (find-in-channel fs channel prefix)
  (let ([head (channel-head fs channel)])
    (if (not head)
        #f
        (let loop ([hash head])
          (let ([blk (fs-fetch fs hash)])
            (if (not blk)
                #f
                (let ([hex (hash->hex hash)])
                  (if (string-prefix? prefix hex)
                      hash
                      (let ([refs (block-refs blk)])
                        (if (= (vector-length refs) 0)
                            #f
                            (loop (vector-ref refs 0))))))))))))

;;; string-prefix? : String × String → Boolean
(define (string-prefix? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (<= plen slen)
         (string=? prefix (substring str 0 plen)))))

;;; ============================================================
;;; chat/1 — Quick Chat Message
;;; ============================================================

;;; chat : String → Bytevector
;;; Send a quick message to the chat channel.
;;; No title needed — chat is informal.
;;;
;;; Example: (chat "Anyone working on the type system?")
;;;
(define (chat txt)
  (let ([session (read-session)])
    (unless session
      (error 'chat "No active session. Use (hi tier name txt) first."))

    (let* ([author (cdr (assq 'name session))]
           [tier (cdr (assq 'tier session))]
           [fs (mint-fs-capability ".store")])
      (let ([hash (post! fs author (tier->forum-tier tier) 'chat txt (current-timestamp))])
        (display (format "~a: ~a\n" author txt))
        hash))))

;;; ============================================================
;;; Convenience: bye/0 — Logout
;;; ============================================================

;;; bye : → void
;;; Clear session and say goodbye.
(define (bye)
  (let ([session (read-session)])
    (when session
      (let* ([name (cdr (assq 'name session))]
             [tier (cdr (assq 'tier session))]
             [fs (mint-fs-capability ".store")])
        (post! fs name (tier->forum-tier tier) 'chat
               (format "@~a has left the fold" name)
               (current-timestamp))
        (clear-session!)
        (display (format "Goodbye, ~a. Session cleared.\n" name))))))

;;; ============================================================
;;; Convenience: digest/0 — Show Digest Without Login
;;; ============================================================

;;; digest : → void
;;; Display the forum digest (works without session).
(define (digest)
  (let ([fs (mint-fs-capability ".store")])
    (display-digest fs)))

;;; ============================================================
;;; Convenience: who/0 — Show Current Session
;;; ============================================================

;;; who : → void
;;; Display current session info.
(define (who)
  (let ([session (read-session)])
    (if session
        (let ([name (cdr (assq 'name session))]
              [tier (cdr (assq 'tier session))]
              [time (cdr (assq 'login-time session))])
          (display (format "Logged in as: ~a (~a)\n" name tier))
          (display (format "Since: ~a\n" time)))
        (display "No active session. Use (hi tier name txt) to login.\n"))))
