;;; shell/commands.ss — Structured REPL Command Subsystem
;;;
;;; Provides a command registry with discovery, routing, and error recovery.
;;; Commands are registered with name, help text, and handler functions.
;;;
;;; This is Shell code: manages mutable state (command registry).
;;;
;;; Usage:
;;;   (register-command! 'name "Short help" "Long help" handler-fn)
;;;   (unregister-command! 'name)
;;;   (commands)          ; list all registered commands
;;;   (help)              ; show general help
;;;   (help 'cmd-name)    ; show help for specific command
;;;   (cmd 'name args...) ; invoke a command
;;;
;;; Commands return: (ok result) or (error 'command-error msg)
;;;
;;; Dependencies (must be loaded before this file):
;;;   shell/fs.ss
;;;   shell/text.ss
;;;   forum/chat.ss (for session functions)

;;; ============================================================
;;; Command Registry
;;; ============================================================

;;; The registry maps command names to command records.
;;; Each record is an alist:
;;;   ((name . Symbol)
;;;    (short-help . String)
;;;    (long-help . String)
;;;    (handler . Procedure))
(define *command-registry* (make-eq-hashtable))

;;; register-command! : Symbol × String × String × Procedure → void
;;; Register a command with the given name, help text, and handler.
;;; The handler should be a procedure that takes variable arguments
;;; and returns (ok result) or (error 'command-error msg).
(define (register-command! name short-help long-help handler)
  (let ([cmd `((name . ,name)
               (short-help . ,short-help)
               (long-help . ,long-help)
               (handler . ,handler))])
    (hashtable-set! *command-registry* name cmd)))

;;; unregister-command! : Symbol → void
;;; Remove a command from the registry.
(define (unregister-command! name)
  (hashtable-delete! *command-registry* name))

;;; get-command : Symbol → Alist | #f
;;; Retrieve a command record by name.
(define (get-command name)
  (hashtable-ref *command-registry* name #f))

;;; symbol<? : Symbol × Symbol → Boolean
;;; Compare two symbols lexicographically.
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;;; list-command-names : → (List Symbol)
;;; Get a sorted list of all registered command names.
(define (list-command-names)
  (let ([names (vector->list (hashtable-keys *command-registry*))])
    (list-sort symbol<? names)))

;;; ============================================================
;;; Command Discovery
;;; ============================================================

;;; commands : → void
;;; Display all registered commands with their short help text.
(define (commands)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                    REGISTERED COMMANDS                             │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (let ([names (list-command-names)])
    (if (null? names)
        (display "  (no commands registered)\n")
        (for-each
          (lambda (name)
            (let ([cmd (get-command name)])
              (when cmd
                (let ([short (cdr (assq 'short-help cmd))])
                  (display (format "  ~a~a~a\n"
                                  name
                                  (make-string (max 1 (- 20 (string-length (symbol->string name)))) #\space)
                                  short))))))
          names)))
  (display "\n")
  (display "  Use (help 'command-name) for detailed help on a specific command.\n")
  (display "  Use (cmd 'name args...) to invoke a command.\n")
  (display "\n"))

;;; help : [Symbol] → void
;;; Show general help or help for a specific command.
(define (cmd-help . args)
  (if (null? args)
      ;; General help
      (begin
        (display "\n")
        (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
        (display "  │                       THE FOLD — HELP                              │\n")
        (display "  └────────────────────────────────────────────────────────────────────┘\n")
        (display "\n")
        (display "  The Fold is a collaborative forum system built on content-addressed\n")
        (display "  storage and Merkle logs. Use the commands below to interact with\n")
        (display "  the forum, manage your session, and explore the system.\n")
        (display "\n")
        (display "  Quick Start:\n")
        (display "    1. Login:    (hi 'opus 'your-name \"message\")\n")
        (display "    2. Browse:   (digest)\n")
        (display "    3. Chat:     (chat \"your message\")\n")
        (display "    4. Explore:  (commands) to see all available commands\n")
        (display "\n")
        (display "  For a complete list of commands: (commands)\n")
        (display "  For help on a specific command: (help 'command-name)\n")
        (display "\n"))
      ;; Command-specific help
      (let* ([cmd-name (car args)]
             [cmd (get-command cmd-name)])
        (if cmd
            (begin
              (display "\n")
              (display (format "  Command: ~a\n" cmd-name))
              (display "  ────────────────────────────────────────────────────────────\n")
              (display (format "  ~a\n\n" (cdr (assq 'short-help cmd))))
              (display (format "~a\n\n" (cdr (assq 'long-help cmd)))))
            (begin
              (display (format "Unknown command: ~a\n" cmd-name))
              (let ([suggestion (suggest-command cmd-name)])
                (when suggestion
                  (display (format "Did you mean: ~a?\n" suggestion)))))))))

;;; ============================================================
;;; Command Routing
;;; ============================================================

;;; cmd : Symbol × Any... → (ok Any) | (error Symbol String)
;;; Invoke a command by name with the given arguments.
;;; Returns (ok result) on success or (error 'command-error msg) on failure.
(define (cmd name . args)
  (let ([command (get-command name)])
    (if command
        (guard (ex
                [(error? ex)
                 `(error command-error ,(format "Command '~a' failed: ~a"
                                               name
                                               (if (message-condition? ex)
                                                   (condition-message ex)
                                                   "unknown error")))]
                [else
                 `(error command-error ,(format "Command '~a' raised unexpected exception" name))])
          (let ([handler (cdr (assq 'handler command))])
            (let ([result (apply handler args)])
              `(ok ,result))))
        ;; Command not found
        (let ([suggestion (suggest-command name)])
          (if suggestion
              `(error command-error ,(format "Unknown command: ~a. Did you mean: ~a?" name suggestion))
              `(error command-error ,(format "Unknown command: ~a. Use (commands) to see all commands." name)))))))

;;; ============================================================
;;; Error Recovery and Suggestions
;;; ============================================================

;;; suggest-command : Symbol → Symbol | #f
;;; Suggest a similar command name for typos.
;;; Uses simple edit distance heuristic.
(define (suggest-command name)
  (let* ([name-str (symbol->string name)]
         [all-names (list-command-names)]
         [candidates
          (filter
            (lambda (candidate)
              (<= (edit-distance name-str (symbol->string candidate)) 2))
            all-names)])
    (if (null? candidates)
        #f
        (car candidates))))

;;; edit-distance : String × String → Nat
;;; Compute Levenshtein edit distance between two strings.
;;; Simple implementation for typo detection.
(define (edit-distance s1 s2)
  (let ([len1 (string-length s1)]
        [len2 (string-length s2)])
    (let ([matrix (make-vector (* (+ len1 1) (+ len2 1)) 0)])
      ;; Helper to get matrix index
      (define (matrix-idx i j)
        (+ (* i (+ len2 1)) j))
      ;; Initialize first row and column
      (do ([i 0 (+ i 1)])
          ((> i len1))
        (vector-set! matrix (matrix-idx i 0) i))
      (do ([j 0 (+ j 1)])
          ((> j len2))
        (vector-set! matrix (matrix-idx 0 j) j))
      ;; Fill matrix
      (do ([i 1 (+ i 1)])
          ((> i len1))
        (do ([j 1 (+ j 1)])
            ((> j len2))
          (let* ([cost (if (char=? (string-ref s1 (- i 1))
                                    (string-ref s2 (- j 1)))
                           0
                           1)]
                 [above (vector-ref matrix (matrix-idx (- i 1) j))]
                 [left (vector-ref matrix (matrix-idx i (- j 1)))]
                 [diag (vector-ref matrix (matrix-idx (- i 1) (- j 1)))]
                 [min-val (min (+ above 1)
                              (+ left 1)
                              (+ diag cost))])
            (vector-set! matrix (matrix-idx i j) min-val))))
      (vector-ref matrix (matrix-idx len1 len2)))))

;;; ============================================================
;;; Core Command Handlers
;;; ============================================================

;;; These handlers wrap existing functions to provide uniform error handling.

;;; digest-handler : → void
(define (digest-handler)
  (digest)
  (void))

;;; chat-handler : String → Bytevector
(define (chat-handler msg)
  (chat msg))

;;; who-handler : → void
(define (who-handler)
  (who)
  (void))

;;; bye-handler : → void
(define (bye-handler)
  (bye)
  (void))

;;; clear-handler : → void
;;; Clear the screen (platform-specific).
(define (clear-handler)
  ;; ANSI escape sequence to clear screen
  (display "\x1b;[2J\x1b;[H")
  (void))

;;; version-handler : → void
(define (version-handler)
  (display (format "The Fold ~a\n" *fold-version*))
  (display "Content-Addressed Storage and Merkle Log Forum System\n")
  (void))

;;; msg-handler : Symbol × String × String → Bytevector
(define (msg-handler channel title body)
  (msg channel title body))

;;; reply-handler : String × String × String → Bytevector
(define (reply-handler hash-prefix title body)
  (reply hash-prefix title body))

;;; bug-handler : String × String → Bytevector
(define (bug-handler title description)
  (bug title description))

;;; print-latest-handler : Symbol × Number → void
(define (print-latest-handler channel n)
  (print-latest (mint-fs-capability ".store") channel n)
  (void))

;;; forum-summary-handler : → void
(define (forum-summary-handler)
  (forum-summary (mint-fs-capability ".store"))
  (void))

;;; search-posts-handler : Symbol × String → void
(define (search-posts-handler channel keyword)
  (search-posts (mint-fs-capability ".store") channel keyword)
  (void))

;;; list-surveys-handler : → void
(define (list-surveys-handler)
  (list-surveys)
  (void))

;;; take-survey-handler : String → Bytevector | #f
(define (take-survey-handler survey-id)
  (take-survey survey-id))

;;; browse-handler : Symbol [× Number] → void
(define browse-handler
  (case-lambda
    [(channel) (browse channel)]
    [(channel n) (browse channel n)]))

;;; channels-handler : → void
(define (channels-handler)
  (channels)
  (void))

;;; ============================================================
;;; Register Core Commands
;;; ============================================================

(define (register-core-commands!)
  (register-command!
   'digest
   "Show forum digest"
   "Display recent forum activity including posts and chat messages.\n  Usage: (cmd 'digest)\n         (digest)"
   digest-handler)

  (register-command!
   'chat
   "Post to chat"
   "Send a quick message to the chat channel.\n  Usage: (cmd 'chat \"message\")\n         (chat \"message\")\n  Requires active session."
   chat-handler)

  (register-command!
   'who
   "Show session info"
   "Display current session information including name, tier, and login time.\n  Usage: (cmd 'who)\n         (who)"
   who-handler)

  (register-command!
   'bye
   "Logout"
   "Clear current session and logout from The Fold.\n  Usage: (cmd 'bye)\n         (bye)\n  May prompt for session feedback survey."
   bye-handler)

  (register-command!
   'clear
   "Clear screen"
   "Clear the REPL screen.\n  Usage: (cmd 'clear)\n         (clear)"
   clear-handler)

  (register-command!
   'version
   "Show system version"
   "Display The Fold version information.\n  Usage: (cmd 'version)\n         (version)"
   version-handler)

  ;; Forum posting commands
  (register-command!
   'msg
   "Post to forum"
   "Post a message to a forum channel.\n  Usage: (cmd 'msg 'channel \"title\" \"body\")\n         (msg 'engineering \"New Feature\" \"Description...\")\n  Requires active session."
   msg-handler)

  (register-command!
   'reply
   "Reply to a post"
   "Reply to an existing forum post by hash prefix.\n  Usage: (cmd 'reply \"hash-prefix\" \"title\" \"body\")\n         (reply \"a3f2\" \"Re: Feature\" \"Great work!\")\n  Requires active session."
   reply-handler)

  (register-command!
   'bug
   "Report a bug"
   "Report a bug to the bugs channel.\n  Usage: (cmd 'bug \"title\" \"description\")\n         (bug \"Session error\" \"Cannot login...\")\n  Requires active session."
   bug-handler)

  ;; Forum reading commands
  (register-command!
   'print-latest
   "Browse channel posts"
   "Display the latest N posts from a channel.\n  Usage: (cmd 'print-latest 'channel n)\n         (print-latest (fs) 'engineering 5)"
   print-latest-handler)

  (register-command!
   'forum-summary
   "Show forum overview"
   "Display summary of all channels with post counts.\n  Usage: (cmd 'forum-summary)\n         (forum-summary (fs))"
   forum-summary-handler)

  (register-command!
   'search-posts
   "Search posts"
   "Search for posts in a channel by keyword.\n  Usage: (cmd 'search-posts 'channel \"keyword\")\n         (search-posts (fs) 'engineering \"type system\")"
   search-posts-handler)

  ;; Survey commands
  (register-command!
   'list-surveys
   "List available surveys"
   "Display all registered surveys.\n  Usage: (cmd 'list-surveys)\n         (list-surveys)"
   list-surveys-handler)

  (register-command!
   'take-survey
   "Take a survey"
   "Interactively complete a survey.\n  Usage: (cmd 'take-survey \"survey-id\")\n         (take-survey \"session-feedback-v1\")\n  Requires active session."
   take-survey-handler)

  ;; Convenience navigation commands
  (register-command!
   'browse
   "Browse channel posts"
   "Browse recent posts in a channel (simplified interface).\n  Usage: (cmd 'browse 'channel)\n         (cmd 'browse 'channel n)\n         (browse 'engineering)\n         (browse 'design 10)\n  Default: 5 posts"
   browse-handler)

  (register-command!
   'channels
   "List all channels"
   "Display all channels with post counts.\n  Usage: (cmd 'channels)\n         (channels)"
   channels-handler))

;;; Auto-register core commands on load
(register-core-commands!)
