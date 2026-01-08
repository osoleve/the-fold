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
;;; Version Information
;;; ============================================================

;;; The Fold version string (should match repl.ss)
(define *fold-version* "GENESIS")

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

;;; cmd-help-string : [Symbol] → String
;;; Build help string (general help or for a specific command).
(define (cmd-help-string . args)
  (if (null? args)
      ;; General help
      (string-append
       "\n"
       "  ┌────────────────────────────────────────────────────────────────────┐\n"
       "  │                       THE FOLD — HELP                              │\n"
       "  └────────────────────────────────────────────────────────────────────┘\n"
       "\n"
       "  The Fold is a collaborative forum system built on content-addressed\n"
       "  storage and Merkle logs. Use the commands below to interact with\n"
       "  the forum, manage your session, and explore the system.\n"
       "\n"
       "  Quick Start:\n"
       "    1. Login:    (hi 'opus 'your-name \"message\")\n"
       "    2. Browse:   (digest)\n"
       "    3. Chat:     (chat \"your message\")\n"
       "    4. Explore:  (commands) to see all available commands\n"
       "\n"
       "  For a complete list of commands: (commands)\n"
       "  For help on a specific command: (help 'command-name)\n"
       "\n")
      ;; Command-specific help
      (let* ([cmd-name (car args)]
             [cmd (get-command cmd-name)])
            (if cmd
                (string-append
                 "\n"
                 (format "  Command: ~a\n" cmd-name)
                 "  ────────────────────────────────────────────────────────────\n"
                 (format "  ~a\n\n" (cdr (assq 'short-help cmd)))
                 (format "~a\n\n" (cdr (assq 'long-help cmd))))
                (let ([suggestion (suggest-command cmd-name)])
                     (string-append
                      (format "Unknown command: ~a\n" cmd-name)
                      (if suggestion
                          (format "Did you mean: ~a?\n" suggestion)
                          "\n")))))))

;;; help : [Symbol] → void
;;; Show general help or help for a specific command.
(define (cmd-help . args)
  (display (apply cmd-help-string args)))

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

;;; ============================================================
;;; Core Command Handlers
;;; ============================================================

;;; These handlers wrap existing functions to provide uniform error handling.

;;; digest-handler : → void
(define (digest-handler)
  (digest)
  (void))

;;; digest-posts-handler : [Nat] → void
(define digest-posts-handler
  (case-lambda
   [()
    (digest-posts)
    (void)]
   [(n)
    (digest-posts n)
    (void)]))

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
   'digest-posts
   "Show posts-only digest"
   "Display recent forum posts without chat.\n  Usage: (cmd 'digest-posts)\n         (cmd 'digest-posts 20)\n         (digest-posts)\n         (digest-posts 20)"
   digest-posts-handler)
  
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

;;; ============================================================
;;; Debugger Commands (loaded dynamically)
;;; ============================================================

;;; register-debugger-commands! : → void
;;; Register debugger commands (call after loading debug-repl.ss)
(define (register-debugger-commands!)
  (register-command!
   'debug
   "Start debugging expression"
   "Begin interactive debugging with fuel tracking.\n  Usage: (debug '(+ 1 2))\n         (debug '(fib 10) 5000)  ; with fuel budget\n  Starts a new debug session for the expression."
   cmd-debug)
  
  (register-command!
   'step
   "Execute one debug step"
   "Execute one reduction step in the debugger.\n  Usage: (step)\n  Shows current expr, fuel consumed, environment."
   cmd-step)
  
  (register-command!
   'next
   "Step over (don't descend)"
   "Execute current expression without descending into calls.\n  Usage: (next)"
   cmd-next)
  
  (register-command!
   'continue
   "Run to breakpoint/completion"
   "Continue execution until breakpoint or completion.\n  Usage: (continue)"
   cmd-continue)
  
  (register-command!
   'break
   "Set breakpoint"
   "Set breakpoint on function name.\n  Usage: (break 'fn-name)\n  Example: (break 'foldl)"
   cmd-break)
  
  (register-command!
   'inspect
   "Show environment bindings"
   "Display current environment with values.\n  Usage: (inspect)"
   cmd-inspect)
  
  (register-command!
   'dbg-fuel
   "Show fuel status"
   "Display detailed fuel consumption.\n  Usage: (dbg-fuel)"
   cmd-fuel)
  
  (register-command!
   'trace
   "Show execution trace"
   "Display execution trace with fuel costs.\n  Usage: (trace)"
   cmd-trace)
  
  (register-command!
   'dbg-undo
   "Undo last step"
   "Undo the last debug step.\n  Usage: (dbg-undo)"
   cmd-undo)
  
  (register-command!
   'dbg-redo
   "Redo step (time travel forward)"
   "Redo a previously undone step.\n  Usage: (dbg-redo)\n         (dbg-redo 3)  ; redo 3 steps"
   cmd-redo)
  
  (register-command!
   'dbg-reset
   "Reset to initial state"
   "Reset debugger to initial state.\n  Usage: (dbg-reset)"
   cmd-reset)
  
  (register-command!
   'dbg-quit
   "Quit debug session"
   "End the current debug session.\n  Usage: (dbg-quit)"
   cmd-quit-debug)
  
  ;; Watch commands
  (register-command!
   'watch
   "Watch a variable"
   "Add a variable to the watch list. Changes will be reported during stepping.\n  Usage: (watch 'x)"
   cmd-watch)
  
  (register-command!
   'unwatch
   "Stop watching a variable"
   "Remove a variable from the watch list.\n  Usage: (unwatch 'x)"
   cmd-unwatch)
  
  (register-command!
   'watches
   "List watched variables"
   "Display all watched variables with their current values and recent events.\n  Usage: (watches)"
   cmd-watches)
  
  ;; Explain commands
  (register-command!
   'explain
   "Explain evaluation result"
   "Show why the current expression evaluated to its result.\n  Usage: (explain)       ; explain current result\n         (explain 'x)   ; explain how variable x got its value"
   cmd-explain)
  
  (register-command!
   'why
   "Explain (shorthand)"
   "Shorthand for (explain). Shows causal trace of evaluation.\n  Usage: (why)"
   cmd-why)
  
  ;; Export commands
  (register-command!
   'export-trace
   "Export structured trace"
   "Export the debug trace as a structured S-expression.\n  Usage: (export-trace)            ; display to console\n         (export-trace \"file.ss\")  ; save to file"
   cmd-export-trace))

;;; ============================================================
;;; Profiler Commands (loaded dynamically)
;;; ============================================================

;;; register-profiler-commands! : → void
;;; Register profiler commands (call after loading profile-repl.ss)
(define (register-profiler-commands!)
  (register-command!
   'profile
   "Profile an expression"
   "Profile expression execution and track fuel consumption.\n  Usage: (profile '(fib 10))\n         (profile '(fib 10) 5000)  ; with fuel budget\n  Returns the result and stores profile for further analysis."
   repl-profile)
  
  (register-command!
   'profile-report
   "Show profile report"
   "Display detailed profile report for last run.\n  Usage: (profile-report)\n  Shows fuel usage, hotspots, and call tree."
   repl-profile-report)
  
  (register-command!
   'profile-viz
   "Visualize profile"
   "Display visual profile with flame graph and bars.\n  Usage: (profile-viz)\n  Shows ASCII flame graph and fuel consumption bars."
   repl-profile-viz)
  
  (register-command!
   'profile-hints
   "Show optimization hints"
   "Analyze profile and suggest optimizations.\n  Usage: (profile-hints)\n  Detects tail-call opportunities, fusion candidates, etc."
   repl-profile-hints)
  
  (register-command!
   'profile-tree
   "Show call tree"
   "Display call tree with fuel consumption.\n  Usage: (profile-tree)"
   repl-profile-tree)
  
  (register-command!
   'profile-flame
   "Show flame graph"
   "Display ASCII flame graph.\n  Usage: (profile-flame)"
   repl-profile-flame)
  
  (register-command!
   'profile-history
   "Show profile history"
   "Display recent profile runs.\n  Usage: (profile-history)"
   repl-profile-history)
  
  (register-command!
   'set-baseline
   "Set profile baseline"
   "Set current profile as baseline for regression detection.\n  Usage: (set-baseline)"
   repl-set-baseline)
  
  (register-command!
   'qprofile
   "Quick profile"
   "Profile with minimal output, returns (fuel . result).\n  Usage: (qprofile '(+ 1 2))"
   qprofile))

;;; Auto-register core commands on load
(register-core-commands!)
