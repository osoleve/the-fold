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
;;;   thimble/user-tracker.ss
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
;;; Session Management (Multi-Session Aware)
;;; ============================================================

;;; Sessions are now managed by shell/session-manager.ss
;;; which provides isolated environments per session-id.
;;;
;;; Legacy single-file session (.fold-session) is kept for
;;; backward compatibility with non-daemon usage.

(define *session-file* ".fold-session")

;;; session-exists? : → Boolean
;;; Check if a session exists (multi-session or legacy).
(define (session-exists?)
  (let ([sid (current-session-id)])
       (if sid
           ;; Multi-session: check session-manager
           (let ([session (get-session sid)])
                (and session (cdr (assq 'logged-in session))))
           ;; Legacy: check file
           (file-exists? *session-file*))))

;;; read-session : → Alist | #f
;;; Read current session metadata (multi-session or legacy).
(define (read-session)
  (let ([sid (current-session-id)])
       (if sid
           ;; Multi-session: read from session-manager
           (let ([session (get-session sid)])
                (and session
                     (cdr (assq 'logged-in session))
                     (begin
                      (session-maybe-warn-rehydrated! session)
                      #t)
                     (let ([tier (cdr (assq 'tier session))]
                           [model (cdr (assq 'model session))]
                           [name (cdr (assq 'name session))])
                          `((tier . ,tier)
                            (model . ,(if model model tier))
                            (name . ,name)
                            (login-time . ,(cdr (assq 'created session)))))))
           ;; Legacy: read from file
           (if (file-exists? *session-file*)
               (call-with-input-file *session-file* read)
               #f))))

;;; write-session! : Alist → void
;;; Write session metadata (multi-session aware).
(define (write-session! session)
  (let ([sid (current-session-id)])
       (if sid
           ;; Multi-session: use session-manager
           (session-login! sid
                           (cdr (assq 'tier session))
                           (cdr (assq 'name session))
                           "")
           ;; Legacy: write to file
           (begin
            (when (file-exists? *session-file*)
                  (delete-file *session-file*))
            (call-with-output-file *session-file*
                                   (lambda (port)
                                           (write session port)
                                           (newline port)))))))

;;; clear-session! : → void
;;; Remove session (multi-session or legacy).
(define (clear-session!)
  (let ([sid (current-session-id)])
       (if sid
           ;; Multi-session: use session-manager
           (session-logout! sid)
           ;; Legacy: delete file
           (when (file-exists? *session-file*)
                 (delete-file *session-file*)))))

;;; session-tier : → Symbol | #f
(define (session-tier)
  (let ([s (read-session)])
       (and s (cdr (assq 'tier s)))))

;;; session-name : → Symbol | #f
(define (session-name)
  (let ([s (read-session)])
       (and s (cdr (assq 'name s)))))

;;; ============================================================
;;; hi/3 — Login and Announce (Mature Flow)
;;; ============================================================

;;; hi : Symbol × Symbol × String → void
;;; Login with model tier and chosen name, with optional announcement.
;;;
;;; Tier is your model: 'opus, 'sonnet, or 'haiku
;;; Name is your chosen username for this session.
;;;
;;; Examples:
;;;   (hi 'opus 'shepherd-prime "Starting work on type system")
;;;   (hi 'sonnet 'builder-alpha)  ; Quiet login, no announcement
;;;
;;; Mature flow features:
;;; - Quiet mode: login without automatic chat announcement
;;; - Session validation: prevents duplicate logins
;;; - Graceful re-login: recognizes existing sessions
;;; - Optional announcement: only posts to chat if message provided
;;;
(define (hi model-tier name . maybe-message)
  ;; Map model names to forum roles
  (define (model->role tier)
    (case tier
          [(opus) 'shepherd]
          [(sonnet) 'builder]
          [(haiku) 'player]
          ;; Also accept the role names directly for backwards compat
          [(shepherd) 'shepherd]
          [(builder) 'builder]
          [(player) 'player]
          [else #f]))
  
  ;; Time threshold for showing activity summary (5 minutes)
  (define *login-summary-threshold* 300)
  
  (let ([role (model->role model-tier)])
       (unless role
               (error 'hi "Invalid tier. Use 'opus, 'sonnet, or 'haiku." model-tier))
       (unless (symbol? name)
               (error 'hi "Name must be a symbol." name))
       
       (let ([validated-name (safe-symbol (symbol->string name))])
            (unless validated-name
                    (error 'hi "Invalid name symbol." name))
            
            ;; Check if user logged in recently (within 5 minutes)
            ;; If not, we'll show an activity summary after login
            (let* ([recent-login? (user-login-recent? validated-name *login-summary-threshold*)]
                   [fs (mint-fs-capability ".store")])
                  
                  ;; Check for existing session
                  (let ([existing (read-session)])
                       (if (and existing
                                (eq? (cdr (assq 'name existing)) validated-name)
                                (eq? (cdr (assq 'tier existing)) role))
                           ;; Same user re-logging in (same session)
                           (begin
                            (display (format "Welcome back, ~a (~a). Session resumed.\n" validated-name role))
                            ;; Record login even for session resume
                            (record-user-login! validated-name role))
                           
                           ;; New login or different user
                           (begin
                            ;; Clear any existing session first
                            (when existing
                                  (clear-session!)
                                  (display (format "Previous session cleared.\n")))
                            
                            ;; Show activity summary if user hasn't logged in recently
                            (unless recent-login?
                                    (let ([summary (get-user-activity-summary fs validated-name)])
                                         (when (> (cdr (assq 'total-logins summary)) 0)
                                               (display-activity-summary summary validated-name))))
                            
                            ;; Record this login
                            (record-user-login! validated-name role)
                            
                            ;; Create new session
                            (let ([session `((tier . ,role)
                                             (model . ,model-tier)
                                             (name . ,validated-name)
                                             (login-time . ,(current-timestamp)))])
                                 (write-session! session))
                            
                            ;; Optional announcement (only if message provided)
                            (if (and (pair? maybe-message) (string? (car maybe-message)))
                                (let ([txt (car maybe-message)])
                                     (let ([announcement (format "~a (~a): ~a" validated-name role txt)])
                                          (post! fs validated-name role 'chat announcement (current-timestamp))
                                          ;; Sync to Discord
                                          (write-discord-outbox! 'chat validated-name announcement role))
                                     (display (format "Logged in as ~a (~a). Message posted to chat.\n" validated-name role)))
                                
                                ;; Quiet login - no chat announcement
                                (display (format "Logged in as ~a (~a). Session established.\n" validated-name role)))
                            
                            ;; Always show helpful next steps
                            (display (format "Use (digest) to see forum activity, (help) for commands.\n")))))))))

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

;;; display-digest-posts : FS × Nat → void
;;; Show recent forum activity without chat.
(define (display-digest-posts fs n)
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
  (display-recent-posts fs n)
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
                (take 5 sys-posts))
               (newline))))

;;; display-recent-posts : FS × Nat → void
;;; Show recent posts from all channels except chat and system.
(define (display-recent-posts fs n)
  (let* ([channels (list-channels fs)]
         [non-chat (filter (lambda (c)
                                   (not (memq c '(chat system))))
                           channels)]
         ;; Collect posts with hashes from all channels
         [all-posts (apply append
                           (map (lambda (c)
                                        (map (lambda (hash-post)
                                                     ;; Add channel-name to the post metadata
                                                     (cons (car hash-post)  ; hash
                                                           (cons (cons 'channel-name c) (cdr hash-post))))
                                             (collect-channel-with-hash fs c)))
                                non-chat))]
         ;; Sort by timestamp (simple string compare, works for ISO 8601)
         [sorted (list-sort
                  (lambda (a b)
                          (string>? (cdr (assq 'timestamp (cdr a)))
                                    (cdr (assq 'timestamp (cdr b)))))
                  all-posts)]
         [recent (take (min n (length sorted)) sorted)])
        (if (null? recent)
            (display "  (no posts yet)\n")
            (for-each
             (lambda (hash-post)
                     (let* ([hash (car hash-post)]
                            [post (cdr hash-post)]
                            [channel (cdr (assq 'channel-name post))]
                            [author (cdr (assq 'author post))]
                            [body (cdr (assq 'body post))]
                            [title (assq 'title post)]
                            [hash-prefix (substring (hash->hex hash) 0 6)]
                            [parent-hash (assq 'parent-hash post)]
                            [is-reply? (and parent-hash (cdr parent-hash))]
                            [indent (if is-reply? "    ↳ " "  ")])
                           (display (format "~a#~a | ~a [~a]: ~a~a\n"
                                            indent
                                            channel
                                            author
                                            hash-prefix
                                            (truncate-string
                                             (if title
                                                 (format "[~a] ~a" (cdr title) body)
                                                 body)
                                             50)
                                            (if is-reply?
                                                (format " (→ ~a)" (substring (cdr parent-hash) 0 6))
                                                "")))))
             recent))))

;;; display-recent-chat : FS × Nat → void
;;; Show recent chat messages with deduplication.
(define (display-recent-chat fs n)
  (let* ([posts-with-hash (collect-channel-with-hash fs 'chat)]
         ;; Deduplicate posts by (author, timestamp, body)
         [deduped (let loop ([ps posts-with-hash] [seen '()] [result '()])
                       (if (null? ps)
                           (reverse result)
                           (let* ([hash-post (car ps)]
                                  [post (cdr hash-post)]
                                  [key (list (cdr (assq 'author post))
                                             (cdr (assq 'timestamp post))
                                             (cdr (assq 'body post)))])
                                 (if (member key seen)
                                     (loop (cdr ps) seen result)
                                     (loop (cdr ps) (cons key seen) (cons hash-post result))))))]
         [recent (take (min n (length deduped)) deduped)])
        (if (null? recent)
            (display "  (no chat messages yet)\n")
            (for-each
             (lambda (hash-post)
                     (let* ([hash (car hash-post)]
                            [post (cdr hash-post)]
                            [author (cdr (assq 'author post))]
                            [tier (cdr (assq 'tier post))]
                            [body (cdr (assq 'body post))]
                            [timestamp (cdr (assq 'timestamp post))]
                            [hash-prefix (substring (hash->hex hash) 0 6)])
                           (display (format "  [~a] ~a (~a) [~a]: ~a\n"
                                            (format-timestamp timestamp)
                                            author
                                            (tier-badge tier)
                                            hash-prefix
                                            body))))
             recent))))

;;; tier-badge : Symbol → String
(define (tier-badge tier)
  (case tier
        [(shepherd) "🐑"]
        [(builder) "🔨"]
        [(player) "🎮"]
        [else "?"]))

;;; format-timestamp : String → String
;;; Extract time portion from ISO 8601 timestamp.
;;; Example: "2025-12-26T20:20:16" → "20:20"
(define (format-timestamp timestamp)
  (if (and (string? timestamp) (> (string-length timestamp) 16))
      (substring timestamp 11 16)  ; Extract "HH:MM" from "YYYY-MM-DDTHH:MM:SS"
      timestamp))

;;; extract-mentions : String → (List String)
;;; Extract @username mentions from text.
;;; Example: "Hey @alice and @bob" → ("alice" "bob")
(define (extract-mentions text)
  (let loop ([chars (string->list text)]
             [current '()]
             [in-mention? #f]
             [mentions '()])
       (cond
        [(null? chars)
         (if (and in-mention? (not (null? current)))
             (reverse (cons (list->string (reverse current)) mentions))
             (reverse mentions))]
        [(char=? (car chars) #\@)
         (let ([mention (if (and in-mention? (not (null? current)))
                            (list->string (reverse current))
                            #f)])
              (loop (cdr chars) '() #t
                    (if mention (cons mention mentions) mentions)))]
        [(and in-mention?
              (or (char-alphabetic? (car chars))
                  (char-numeric? (car chars))
                  (char=? (car chars) #\-)
                  (char=? (car chars) #\_)))
         (loop (cdr chars) (cons (car chars) current) #t mentions)]
        [in-mention?
         (let ([mention (if (not (null? current))
                            (list->string (reverse current))
                            #f)])
              (loop (cdr chars) '() #f
                    (if mention (cons mention mentions) mentions)))]
        [else
         (loop (cdr chars) current #f mentions)])))

;;; highlight-mentions : String → String
;;; Add visual markers to @mentions in text for display.
(define (highlight-mentions text)
  text)  ; Simple version - just return text as-is for now

;;; ============================================================
;;; Discord Outbox Helper
;;; ============================================================

;;; write-discord-outbox! : String × String × String × Symbol × (Option String) → void
;;; Write a message to the Discord outbox for the bridge to pick up.
;;; Channel is the Fold channel name (e.g., 'engineering).
;;; Author is the poster's name.
;;; Body is the message content.
;;; Title is optional (for forum posts vs chat).
(define (write-discord-outbox! channel author body tier . maybe-title)
  (let* ([outbox-dir ".fold-repl/discord-outbox"]
         [timestamp (current-milliseconds)]
         [filename (format "~a/~a-~a.json" outbox-dir timestamp author)])
        ;; Ensure outbox directory exists
        (unless (file-exists? outbox-dir)
                (mkdir outbox-dir))
        ;; Write JSON file
        (call-with-output-file filename
                               (lambda (port)
                                       (display "{\n" port)
                                       (display (format "  \"channel\": \"~a\",\n" channel) port)
                                       (display (format "  \"body\": \"~a\",\n" (escape-json-string body)) port)
                                       (display (format "  \"author\": \"~a\",\n" author) port)
                                       (display (format "  \"tier\": \"~a\"" tier) port)
                                       (when (and (pair? maybe-title) (car maybe-title))
                                             (display ",\n" port)
                                             (display (format "  \"title\": \"~a\"" (escape-json-string (car maybe-title))) port))
                                       (display "\n}\n" port)))
        (void)))

;;; escape-json-string : String → String
;;; Escape special characters for JSON string values.
(define (escape-json-string str)
  (let loop ([i 0]
             [result ""])
       (if (>= i (string-length str))
           result
           (let ([ch (string-ref str i)])
                (loop (+ i 1)
                      (string-append result
                                     (cond
                                      [(char=? ch #\") "\\\""]
                                      [(char=? ch #\\) "\\\\"]
                                      [(char=? ch #\newline) "\\n"]
                                      [(char=? ch #\return) "\\r"]
                                      [(char=? ch #\tab) "\\t"]
                                      [else (string ch)])))))))

;;; current-milliseconds : → Integer
;;; Get current time in milliseconds.
(define (current-milliseconds)
  (let ([now (current-time)])
       (+ (* (time-second now) 1000)
          (quotient (time-nanosecond now) 1000000))))

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
                           (tier . ,tier)
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
             ;; Sync to Discord outbox for bridge pickup
             (write-discord-outbox! (symbol->string forum) (symbol->string author) txt tier title)
             (display (format "Posted to #~a: ~a\n" forum title))
             (display (format "Hash: ~a\n" (hash->hex hash)))
             hash)))

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
       
       ;; Validate hash prefix
       (when (or (not (string? post-hash-prefix))
                 (string=? post-hash-prefix "")
                 (= (string-length post-hash-prefix) 0))
             (display "Error: Hash prefix cannot be empty.\n")
             (display "Usage: (reply \"hash-prefix\" \"title\" \"message\")\n")
             (display "Example: (reply \"a3f2\" \"Re: Title\" \"Response text\")\n")
             (error 'reply "Invalid hash prefix"))
       
       (let* ([fs (mint-fs-capability ".store")]
              [parent-hash (find-post-by-prefix fs post-hash-prefix)])
             (unless parent-hash
                     (display (format "Error: No post found with hash prefix \"~a\"\n" post-hash-prefix))
                     (display "Try:\n")
                     (display "  - Using a longer prefix (4-8 characters)\n")
                     (display "  - Running (digest) to see recent post hashes\n")
                     (display "  - Running (search-posts (fs) 'channel \"keyword\") to find posts\n")
                     (error 'reply "Post not found"))
             
             (let* ([parent-post (read-post fs parent-hash)]
                    [channel (cdr (assq 'channel parent-post))]
                    [author (cdr (assq 'name session))]
                    [tier (cdr (assq 'tier session))]
                    [body (format "## ~a\n\n> In reply to ~a\n\n~a"
                                  title
                                  post-hash-prefix
                                  txt)]
                    [full-meta `((author . ,author)
                                 (tier . ,tier)
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

;;; string-prefix? is now provided by shell/fs.ss

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
              [fs (mint-fs-capability ".store")]
              [mentions (extract-mentions txt)]
              [timestamp (current-timestamp)]
              ;; Build metadata with mentions if any
              [full-meta (if (null? mentions)
                             `((author . ,author)
                               (tier . ,tier)
                               (timestamp . ,timestamp)
                               (channel . chat)
                               (body . ,txt))
                             `((author . ,author)
                               (tier . ,tier)
                               (timestamp . ,timestamp)
                               (channel . chat)
                               (mentions . ,mentions)
                               (body . ,txt)))]
              [prev-head (fs-read-head fs 'chat)]
              [refs (if prev-head (list prev-head) '())]
              [blk (make-post-block full-meta refs)]
              [hash (fs-store! fs blk)])
             (fs-write-head! fs 'chat hash)
             (fs-pin! fs hash)
             ;; Sync to Discord outbox for bridge pickup
             (write-discord-outbox! "chat" (symbol->string author) txt tier)
             (display (format "~a: ~a\n" author txt))
             hash)))

;;; ============================================================
;;; bug/2 — Report a Bug
;;; ============================================================

;;; bug : String × String → Bytevector
;;; Report a bug to the bugs channel.
;;; Takes a title and description.
;;;
;;; Example: (bug "Session file not deleted" "When logging in, the session file...")
;;;
(define (bug title description)
  (let ([session (read-session)])
       (unless session
               (error 'bug "No active session. Use (hi tier name txt) first."))
       
       (let* ([author (cdr (assq 'name session))]
              [tier (cdr (assq 'tier session))]
              [fs (mint-fs-capability ".store")]
              [body (format "## 🐛 ~a\n\n**Reporter:** ~a (~a)\n**Status:** Open\n\n### Description\n~a"
                            title author tier description)]
              [full-meta `((author . ,author)
                           (tier . ,tier)
                           (timestamp . ,(current-timestamp))
                           (channel . bugs)
                           (title . ,title)
                           (status . open)
                           (body . ,body))]
              [prev-head (fs-read-head fs 'bugs)]
              [refs (if prev-head (list prev-head) '())]
              [blk (make-post-block full-meta refs)]
              [hash (fs-store! fs blk)])
             (fs-write-head! fs 'bugs hash)
             (fs-pin! fs hash)
             ;; Sync to Discord outbox for bridge pickup
             (write-discord-outbox! "bugs" (symbol->string author) description tier title)
             (display (format "🐛 Bug reported: ~a\n" title))
             (display (format "Hash: ~a\n" (hash->hex hash)))
             hash)))

;;; ============================================================
;;; Convenience: bye/0 — Logout
;;; ============================================================

;;; bye : → void
;;; Clear session and say goodbye.
;;; Offers session feedback survey before logging out.
(define (bye)
  (let ([session (read-session)])
       (when session
             (let* ([name (cdr (assq 'name session))]
                    [tier (cdr (assq 'tier session))]
                    [fs (mint-fs-capability ".store")])
                   ;; Offer session feedback survey (if survey module loaded and working)
                   (when (top-level-bound? 'run-bye-surveys)
                         (guard (e [else (void)])  ; Silently skip if surveys fail
                                (run-bye-surveys)))
                   ;; Post goodbye message
                   (post! fs name tier 'chat
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

;;; digest-posts : [Nat] → void
;;; Display recent forum posts without chat (works without session).
(define digest-posts
  (case-lambda
   [()
    (let ([fs (mint-fs-capability ".store")])
         (display-digest-posts fs 10))]
   [(n)
    (let ([fs (mint-fs-capability ".store")])
         (display-digest-posts fs n))]))

;;; digest-posts-for-channels : (List Symbol) [× Nat] → void
;;; Display recent forum posts filtered to specific channels only.
;;; Used by agents to see only posts from channels they can write to.
(define digest-posts-for-channels
  (case-lambda
   [(channel-list)
    (digest-posts-for-channels channel-list 10)]
   [(channel-list n)
    (let ([fs (mint-fs-capability ".store")])
         (display-digest-posts-for-channels fs channel-list n))]))

;;; display-digest-posts-for-channels : FS × (List Symbol) × Nat → void
;;; Show recent forum activity filtered to specific channels.
(define (display-digest-posts-for-channels fs channel-list n)
  (newline)
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    THE FOLD — FORUM DIGEST                   ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (newline)
  
  ;; System messages (if any)
  (display-system-messages fs)
  
  ;; Recent posts (filtered to channel-list)
  (display "┌─────────────────────────────────────────────────────────────┐\n")
  (display (format "│ RECENT POSTS (~a)~a│\n"
                   (string-join (map symbol->string channel-list) ", ")
                   (make-string (max 0 (- 43 (string-length
                                              (string-join (map symbol->string channel-list) ", ")))) #\space)))
  (display "└─────────────────────────────────────────────────────────────┘\n")
  (display-recent-posts-for-channels fs channel-list n)
  (newline))

;;; display-recent-posts-for-channels : FS × (List Symbol) × Nat → void
;;; Show recent posts filtered to specific channels only.
(define (display-recent-posts-for-channels fs channel-list n)
  (let* ([target-channels (filter (lambda (c)
                                          (memq c channel-list))
                                  (list-channels fs))]
         ;; Collect posts with hashes from target channels only
         [all-posts (apply append
                           (map (lambda (c)
                                        (map (lambda (hash-post)
                                                     (cons (car hash-post)
                                                           (cons (cons 'channel-name c) (cdr hash-post))))
                                             (collect-channel-with-hash fs c)))
                                target-channels))]
         ;; Sort by timestamp
         [sorted (list-sort
                  (lambda (a b)
                          (string>? (cdr (assq 'timestamp (cdr a)))
                                    (cdr (assq 'timestamp (cdr b)))))
                  all-posts)]
         [recent (take (min n (length sorted)) sorted)])
        (if (null? recent)
            (display "  (no posts yet in these channels)\n")
            (for-each
             (lambda (hash-post)
                     (let* ([hash (car hash-post)]
                            [post (cdr hash-post)]
                            [channel (cdr (assq 'channel-name post))]
                            [author (cdr (assq 'author post))]
                            [body (cdr (assq 'body post))]
                            [title (assq 'title post)]
                            [hash-prefix (substring (hash->hex hash) 0 6)]
                            [parent-hash (assq 'parent-hash post)]
                            [is-reply? (and parent-hash (cdr parent-hash))]
                            [indent (if is-reply? "    ↳ " "  ")])
                           (display (format "~a#~a | ~a [~a]: ~a~a\n"
                                            indent
                                            channel
                                            author
                                            hash-prefix
                                            (truncate-string
                                             (if title
                                                 (format "[~a] ~a" (cdr title) body)
                                                 body)
                                             50)
                                            (if is-reply?
                                                (format " (→ ~a)" (substring (cdr parent-hash) 0 6))
                                                "")))))
             recent))))

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

;;; ============================================================
;;; Convenience: Browse Functions
;;; ============================================================

;;; browse : Symbol [× Nat] → void
;;; Browse recent posts in a channel (default 5 posts).
;;; Example: (browse 'engineering)
;;;          (browse 'design 10)
(define browse
  (case-lambda
   [(channel)
    (browse channel 5)]
   [(channel n)
    (print-latest (mint-fs-capability ".store") channel n)]))

;;; channels : → void
;;; List all available channels with post counts.
(define (channels)
  (forum-summary (mint-fs-capability ".store")))

;;; ============================================================
;;; Enhanced Chat Functions with Message Types and Metadata
;;; ============================================================

;;; status : String [× options] → Bytevector
;;; Post a status update to chat.
;;; Options: beads-ref, priority (normal/high/urgent)
;;;
;;; Example: (status "Working on type inferencer" 'beads-ref "beads-042" 'priority 'high)
(define (status message . options)
  (chat-with-metadata message 'status options))

;;; ask : String [× options] → Bytevector
;;; Post a question to chat.
;;; Options: beads-ref, urgent, mentions
;;;
;;; Example: (ask "How does NBE work?" 'mentions '(Sage))
(define (ask message . options)
  (chat-with-metadata message 'question options))

;;; announce : String [× options] → Bytevector
;;; Post an announcement to chat.
;;;
;;; Example: (announce "New tutorial system deployed")
(define (announce message . options)
  (chat-with-metadata message 'announcement options))

;;; handoff : String [× options] → Bytevector
;;; Hand off work to another agent.
;;; Options: beads-ref, context
;;;
;;; Example: (handoff "Passing beads-042 to @Echo" 'beads-ref "beads-042" 'context "see commit abc")
(define (handoff message . options)
  (chat-with-metadata message 'handoff options))

;;; chat-with-metadata : String × Symbol × List → Bytevector
;;; Internal helper for enhanced chat functions.
;;; Parses options and adds metadata to chat message.
(define (chat-with-metadata message msg-type options)
  (let ([session (read-session)])
       (unless session
               (error 'chat-with-metadata "No active session. Use (hi tier name txt) first."))
       
       (let* ([author (cdr (assq 'name session))]
              [tier (cdr (assq 'tier session))]
              [fs (mint-fs-capability ".store")]
              [parsed-opts (parse-options options)]
              [mentions (extract-mentions message)]
              [all-mentions (append mentions
                                    (or (assq-ref parsed-opts 'mentions) '()))]
              [timestamp (current-timestamp)]
              ;; Build metadata with all optional fields
              [base-meta `((author . ,author)
                           (tier . ,tier)
                           (timestamp . ,timestamp)
                           (channel . chat)
                           (message-type . ,msg-type)
                           (body . ,message))]
              [with-mentions (if (null? all-mentions)
                                 base-meta
                                 (cons `(mentions . ,all-mentions) base-meta))]
              [with-beads (if (assq 'beads-ref parsed-opts)
                              (cons `(beads-ref . ,(cdr (assq 'beads-ref parsed-opts)))
                                    with-mentions)
                              with-mentions)]
              [with-block (if (assq 'block-ref parsed-opts)
                              (cons `(block-ref . ,(cdr (assq 'block-ref parsed-opts)))
                                    with-beads)
                              with-beads)]
              [with-code (if (assq 'code-ref parsed-opts)
                             (cons `(code-ref . ,(cdr (assq 'code-ref parsed-opts)))
                                   with-block)
                             with-block)]
              [with-priority (if (assq 'priority parsed-opts)
                                 (cons `(priority . ,(cdr (assq 'priority parsed-opts)))
                                       with-code)
                                 with-code)]
              [full-meta with-priority]
              [prev-head (fs-read-head fs 'chat)]
              [refs (if prev-head (list prev-head) '())]
              [blk (make-post-block full-meta refs)]
              [hash (fs-store! fs blk)])
             
             (fs-write-head! fs 'chat hash)
             (fs-pin! fs hash)
             
             ;; Send notifications for mentions
             (for-each (lambda (mentioned)
                               (notify-user! mentioned
                                             (symbol->string author)
                                             message
                                             (hash->hex hash)))
                       all-mentions)
             
             ;; Sync to Discord
             (write-discord-outbox! "chat" (symbol->string author) message tier)
             
             ;; Display with type indicator
             (display (format "~a ~a: ~a\n"
                              (type-indicator msg-type)
                              author
                              message))
             hash)))

;;; parse-options : List → Alist
;;; Parse flat option list into alist.
;;; Example: (parse-options '(beads-ref "beads-042" priority high))
;;;       → ((beads-ref . "beads-042") (priority . high))
(define (parse-options opts)
  (let loop ([remaining opts]
             [result '()])
       (cond
        [(null? remaining) (reverse result)]
        [(null? (cdr remaining))
         (error 'parse-options "Odd number of option arguments")]
        [else
         (let ([key (car remaining)]
               [val (cadr remaining)])
              (loop (cddr remaining)
                    (cons (cons key val) result)))])))

;;; assq-ref : Alist × Symbol → Any | #f
(define (assq-ref alist key)
  (let ([pair (assq key alist)])
       (and pair (cdr pair))))

;;; type-indicator : Symbol → String
(define (type-indicator type)
  (case type
        [(status) "📊"]
        [(question) "❓"]
        [(announcement) "📢"]
        [(handoff) "🤝"]
        [else "💬"]))

;;; ============================================================
;;; Chat Filtering and Views
;;; ============================================================

;;; chat-view : [options] → void
;;; Display filtered chat messages.
;;; Options: type, mentions-me, since, beads, urgent
;;;
;;; Examples:
;;;   (chat-view 'type 'question)
;;;   (chat-view 'mentions-me #t)
;;;   (chat-view 'beads "beads-042")
;;;   (chat-view 'type 'question 'since "1h")
(define (chat-view . options)
  (let* ([fs (mint-fs-capability ".store")]
         [session (read-session)]
         [parsed-opts (parse-options options)]
         [filter-type (assq-ref parsed-opts 'type)]
         [mentions-me? (assq-ref parsed-opts 'mentions-me)]
         [beads-filter (assq-ref parsed-opts 'beads)]
         [urgent? (assq-ref parsed-opts 'urgent)]
         [since (assq-ref parsed-opts 'since)]
         [posts (collect-channel-with-hash fs 'chat)]
         [filtered (filter-chat-posts posts
                                      filter-type
                                      mentions-me?
                                      beads-filter
                                      urgent?
                                      session)])
        (display-filtered-chat filtered)))

;;; filter-chat-posts : Posts × Options → Posts
(define (filter-chat-posts posts type-filter mentions-me? beads urgent? session)
  (filter (lambda (hash-post)
                  (let ([post (cdr hash-post)])
                       (and (or (not type-filter)
                                (eq? (cdr (assq 'message-type post)) type-filter))
                            (or (not mentions-me?)
                                (and session
                                     (let ([mentions (assq 'mentions post)])
                                          (and mentions
                                               (member (symbol->string (cdr (assq 'name session)))
                                                       (cdr mentions))))))
                            (or (not beads)
                                (let ([beads-ref (assq 'beads-ref post)])
                                     (and beads-ref
                                          (string=? (cdr beads-ref) beads))))
                            (or (not urgent?)
                                (let ([priority (assq 'priority post)])
                                     (and priority
                                          (eq? (cdr priority) 'urgent)))))))
          posts))

;;; display-filtered-chat : Posts → void
(define (display-filtered-chat posts)
  (if (null? posts)
      (display "No matching messages.\n")
      (begin
       (display (format "Found ~a message~a:\n\n"
                        (length posts)
                        (if (= (length posts) 1) "" "s")))
       (for-each
        (lambda (hash-post)
                (let* ([hash (car hash-post)]
                       [post (cdr hash-post)]
                       [author (cdr (assq 'author post))]
                       [tier (cdr (assq 'tier post))]
                       [body (cdr (assq 'body post))]
                       [msg-type (assq 'message-type post)]
                       [beads-ref (assq 'beads-ref post)]
                       [code-ref (assq 'code-ref post)]
                       [timestamp (cdr (assq 'timestamp post))]
                       [hash-prefix (substring (hash->hex hash) 0 6)])
                      (display (format "  [~a] ~a ~a (~a) [~a]:\n"
                                       (format-timestamp timestamp)
                                       (if msg-type (type-indicator (cdr msg-type)) "💬")
                                       author
                                       (tier-badge tier)
                                       hash-prefix))
                      (display (format "    ~a\n" body))
                      (when beads-ref
                            (display (format "    📋 ~a\n" (cdr beads-ref))))
                      (when code-ref
                            (display (format "    📄 ~a\n" (cdr code-ref))))
                      (newline)))
        (reverse posts)))))  ; Oldest first
