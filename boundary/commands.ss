;;; boundary/commands.ss — Structured REPL Command Subsystem
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
;;;   boundary/io/fs.ss
;;;   boundary/ui/text.ss

;;; ====
;;; Version Information
;;; ====

;;; The Fold version string (should match repl.ss)
(define *fold-version* "GENESIS")

;;; ====
;;; Command Registry
;;; ====

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

;;; ====
;;; Command Discovery
;;; ====

;;; commands : → void
;;; Display all registered commands with their short help text.
(define (commands)
  (display "\n")
  (display "  --- REGISTERED COMMANDS -----------------------------------------------\n")
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
       "  --- THE FOLD -- HELP -------------------------------------------------\n"
       "\n"
       "  The Fold is a content-addressed storage system. Use the commands\n"
       "  below to explore blocks and interact with the system.\n"
       "\n"
       "  Quick Start:\n"
       "    1. Explore:  (blocks) to see CAS statistics\n"
       "    2. Search:   (search \"query\") to find blocks\n"
       "    3. Navigate: (explore-block hash) to dive into a block\n"
       "    4. Browse:   (commands) to see all available commands\n"
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
                 "  ------------------------------------------------------------\n"
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

;;; ====
;;; Command Routing
;;; ====

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

;;; ====
;;; Error Recovery and Suggestions
;;; ====

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

;;; ====
;;; Core Command Handlers
;;; ====

;;; These handlers wrap existing functions to provide uniform error handling.

;;; clear-handler : → void
;;; Clear the screen (platform-specific).
(define (clear-handler)
  ;; ANSI escape sequence to clear screen
  (display "\x1b;[2J\x1b;[H")
  (void))

;;; version-handler : → void
(define (version-handler)
  (display (format "The Fold ~a\n" *fold-version*))
  (display "Content-Addressed Storage System\n")
  (void))

;;; ====
;;; Register Core Commands
;;; ====

(define (register-core-commands!)
  (register-command!
   'clear
   "Clear screen"
   "Clear the REPL screen.\n  Usage: (cmd 'clear)\n         (clear)"
   clear-handler)
  
  (register-command!
   'version
   "Show system version"
   "Display The Fold version information.\n  Usage: (cmd 'version)\n         (version)"
   version-handler))

;;; ====
;;; Debugger Commands (loaded dynamically)
;;; ====

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

;;; ====
;;; Profiler Commands (loaded dynamically)
;;; ====

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

;;; ====
;;; Refactoring Commands (loaded dynamically)
;;; ====

;;; register-refactoring-commands! : -> void
;;; Register refactoring commands (call after loading refactor-toolkit.ss)
(define (register-refactoring-commands!)
  (register-command!
   'refactor
   "Unified refactoring toolkit"
   "Access all refactoring operations through a single interface.\n  Usage: (refactor 'operation args...)\n\n  Operations:\n    help      - Show all available operations\n    rename    - Rename a symbol globally\n    move      - Move a symbol to another module\n    extract   - Extract code to new function\n    inline    - Inline a function at call sites\n    dead-code - Find unused definitions\n    deps      - Show dependencies for a symbol\n    undo      - Undo last refactoring\n    status    - Show pending changes\n    apply     - Apply pending changes\n    clear     - Discard pending changes\n\n  Example:\n    (refactor 'help)\n    (refactor 'rename 'old-name 'new-name)\n    (refactor 'move 'my-func \"new-file.ss\")"
   (lambda args (apply refactor args)))

  (register-command!
   'rr
   "Quick rename (alias)"
   "Quick alias for rename refactoring.\n  Usage: (rr 'old-name 'new-name)\n  Same as: (refactor 'rename 'old-name 'new-name)"
   rr)

  (register-command!
   'rm
   "Quick move (alias)"
   "Quick alias for move refactoring.\n  Usage: (rm 'symbol \"target.ss\")\n  Same as: (refactor 'move 'symbol \"target.ss\")"
   rm)

  (register-command!
   'rd
   "Quick dependency check (alias)"
   "Quick alias for dependency analysis.\n  Usage: (rd 'symbol)\n  Same as: (refactor 'deps 'symbol)"
   rd)

  (register-command!
   'rdc
   "Quick dead code scan (alias)"
   "Quick alias for dead code scanning.\n  Usage: (rdc)           - Scan entire codebase\n         (rdc \"path\")    - Scan specific path\n  Same as: (refactor 'dead-code)"
   (lambda args (apply rdc args))))

;;; ====
;;; Doc Commands (loaded dynamically)
;;; ====

;;; register-doc-commands! : -> void
;;; Register doc search commands (call after loading lattice/meta/docs.ss)
(define (register-doc-commands!)
  (register-command!
   'lf-todo
   "Find all TODO docs"
   "Search for (doc 'todo ...) forms across the codebase.\n  Usage: (lf-todo)\n  Displays file:line [todo] content for each match."
   lf-todo)

  (register-command!
   'lf-fixme
   "Find all FIXME docs"
   "Search for (doc 'fixme ...) forms across the codebase.\n  Usage: (lf-fixme)\n  Displays file:line [fixme] content for each match."
   lf-fixme)

  (register-command!
   'lf-types
   "Find all type docs"
   "Search for (doc 'type ...) forms across the codebase.\n  Usage: (lf-types)\n  Displays file:line [type] content for each match."
   lf-types)

  (register-command!
   'lf-deprecated
   "Find deprecated items"
   "Search for (doc 'deprecated ...) forms across the codebase.\n  Usage: (lf-deprecated)\n  Displays file:line [deprecated] content for each match."
   lf-deprecated)

  (register-command!
   'doc-stats
   "Doc annotation statistics"
   "Show count of doc annotations by tag type.\n  Usage: (doc-stats)\n  Returns: ((todo . N) (type . M) ...)"
   doc-stats)

  (register-command!
   'doc-reindex!
   "Rebuild doc index"
   "Force rebuild of the doc annotation index.\n  Usage: (doc-reindex!)\n  Use after adding new (doc ...) forms to update the index."
   doc-reindex!))

;;; Auto-register core commands on load
(register-core-commands!)
