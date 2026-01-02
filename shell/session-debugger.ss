;;; shell/session-debugger.ss — Per-Session Debugger State Management
;;;
;;; Manages debugger instances per REPL session:
;;;   - Each session has independent debugger state
;;;   - Debugger persists across commands within session
;;;   - Cleanup on session end
;;;
;;; This is Shell code: manages mutable state for debugger sessions.
;;;
;;; Dependencies:
;;;   - core/util/debug.ss

(load "core/util/debug.ss")

;;; ============================================================
;;; Session Debugger Registry
;;; ============================================================

;;; Global registry: session-id → debugger
(define *session-debuggers* (make-hashtable string-hash string=?))

;;; Current session ID (set by REPL daemon)
(define *current-session-id* "default")

;;; ============================================================
;;; Session ID Management
;;; ============================================================

;;; get-current-session-id : → String
(define (get-current-session-id)
  *current-session-id*)

;;; set-current-session-id! : String → void
(define (set-current-session-id! id)
  (set! *current-session-id* id))

;;; ============================================================
;;; Debugger State Management
;;; ============================================================

;;; get-session-debugger : → Debugger | #f
;;; Get the debugger for the current session.
(define (get-session-debugger)
  (hashtable-ref *session-debuggers* (get-current-session-id) #f))

;;; set-session-debugger! : Debugger → void
;;; Set the debugger for the current session.
(define (set-session-debugger! dbg)
  (hashtable-set! *session-debuggers* (get-current-session-id) dbg))

;;; clear-session-debugger! : → void
;;; Remove the debugger for the current session.
(define (clear-session-debugger!)
  (hashtable-delete! *session-debuggers* (get-current-session-id)))

;;; has-active-debugger? : → Boolean
;;; Check if the current session has an active debugger.
(define (has-active-debugger?)
  (let ([dbg (get-session-debugger)])
       (and dbg
            (not (eq? (debugger-status dbg) 'complete))
            (not (eq? (debugger-status dbg) 'error)))))

;;; ============================================================
;;; Debugger Lifecycle
;;; ============================================================

;;; start-debug-session! : Expr × Env × Fuel → Debugger
;;; Start a new debug session for the current REPL session.
(define (start-debug-session! expr env fuel)
  (let ([dbg (make-fuel-debugger expr env fuel)])
       (set-session-debugger! dbg)
       dbg))

;;; end-debug-session! : → void
;;; End the current debug session.
(define (end-debug-session!)
  (clear-session-debugger!))

;;; require-debugger! : → Debugger
;;; Get debugger or error if none active.
(define (require-debugger!)
  (let ([dbg (get-session-debugger)])
       (unless dbg
               (error 'debugger "No active debugging session. Use (debug expr) to start."))
       dbg))

;;; ============================================================
;;; Session Cleanup
;;; ============================================================

;;; cleanup-session-debugger! : String → void
;;; Clean up debugger for a specific session (called on session end).
(define (cleanup-session-debugger! session-id)
  (hashtable-delete! *session-debuggers* session-id))

;;; list-debug-sessions : → List
;;; List all active debug sessions (for admin).
(define (list-debug-sessions)
  (let ([keys (vector->list (hashtable-keys *session-debuggers*))])
       (map (lambda (id)
                    (let ([dbg (hashtable-ref *session-debuggers* id #f)])
                         (list id
                               (if dbg (debugger-status dbg) 'none)
                               (if dbg (debugger-fuel dbg) 0))))
            keys)))
