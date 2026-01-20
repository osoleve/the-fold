(load "core/util/debug.ss")

(doc 'module 'session-debugger)
(doc 'description "Per-Session Debugger State Management")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/util/debug))

(doc 'note "Manages debugger instances per REPL session - each session has independent debugger state")

(doc 'section 'registry)

(define *session-debuggers* (make-hashtable string-hash string=?))
(doc *session-debuggers* 'type '(Hashtable String Debugger))
(doc *session-debuggers* 'description "Global registry: session-id → debugger")

(define *current-session-id* "default")
(doc *current-session-id* 'type 'String)
(doc *current-session-id* 'description "Current session ID (set by REPL daemon)")

(doc 'section 'session-id-management)

(doc get-current-session-id 'export #t)
(define (get-current-session-id)
  (doc 'type '(-> String))
  (doc 'description "Get current session ID")
  *current-session-id*)

(doc set-current-session-id! 'export #t)
(define (set-current-session-id! id)
  (doc 'type '(-> String void))
  (doc 'description "Set current session ID")
  (set! *current-session-id* id))

(doc 'section 'debugger-state)

(doc get-session-debugger 'export #t)
(define (get-session-debugger)
  (doc 'type '(-> (Maybe Debugger)))
  (doc 'description "Get the debugger for the current session")
  (hashtable-ref *session-debuggers* (get-current-session-id) #f))

(doc set-session-debugger! 'export #t)
(define (set-session-debugger! dbg)
  (doc 'type '(-> Debugger void))
  (doc 'description "Set the debugger for the current session")
  (hashtable-set! *session-debuggers* (get-current-session-id) dbg))

(doc clear-session-debugger! 'export #t)
(define (clear-session-debugger!)
  (doc 'type '(-> void))
  (doc 'description "Remove the debugger for the current session")
  (hashtable-delete! *session-debuggers* (get-current-session-id)))

(doc has-active-debugger? 'export #t)
(define (has-active-debugger?)
  (doc 'type '(-> Bool))
  (doc 'description "Check if the current session has an active debugger")
  (let ([dbg (get-session-debugger)])
       (and dbg
            (not (eq? (debugger-status dbg) 'complete))
            (not (eq? (debugger-status dbg) 'error)))))

(doc 'section 'lifecycle)

(doc start-debug-session! 'export #t)
(define (start-debug-session! expr env fuel)
  (doc 'type '(-> Expr Env Fuel Debugger))
  (doc 'description "Start a new debug session for the current REPL session")
  (let ([dbg (make-fuel-debugger expr env fuel)])
       (set-session-debugger! dbg)
       dbg))

(doc end-debug-session! 'export #t)
(define (end-debug-session!)
  (doc 'type '(-> void))
  (doc 'description "End the current debug session")
  (clear-session-debugger!))

(doc require-debugger! 'export #t)
(define (require-debugger!)
  (doc 'type '(-> Debugger))
  (doc 'description "Get debugger or error if none active")
  (let ([dbg (get-session-debugger)])
       (unless dbg
               (error 'debugger "No active debugging session. Use (debug expr) to start."))
       dbg))

(doc 'section 'cleanup)

(doc cleanup-session-debugger! 'export #t)
(define (cleanup-session-debugger! session-id)
  (doc 'type '(-> String void))
  (doc 'description "Clean up debugger for a specific session (called on session end)")
  (hashtable-delete! *session-debuggers* session-id))

(doc list-debug-sessions 'export #t)
(define (list-debug-sessions)
  (doc 'type '(-> (List (List String Symbol Nat))))
  (doc 'description "List all active debug sessions (for admin)")
  (let ([keys (vector->list (hashtable-keys *session-debuggers*))])
       (map (lambda (id)
                    (let ([dbg (hashtable-ref *session-debuggers* id #f)])
                         (list id
                               (if dbg (debugger-status dbg) 'none)
                               (if dbg (debugger-fuel dbg) 0))))
            keys)))
