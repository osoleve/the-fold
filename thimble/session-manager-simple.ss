;;; thimble/session-manager-simple.ss — Simple Enhanced Session Management

;; Load the original session manager
(load "thimble/session-manager.ss")

;;; ============================================================
;;; Enhanced Session Display
;;; ============================================================

(define (show-sessions-enhanced)
  "Display all sessions in a user-friendly format"
  (let ([sessions (list-sessions)]
        [count (session-count)])
       
       (display "═══════════════════════════════════════════════════════════════\n")
       (display "  SESSION MANAGER\n")
       (display "═══════════════════════════════════════════════════════════════\n\n")
       
       (display "Active sessions: ")
       (display count)
       (newline)
       (newline)
       
       (if (null? sessions)
           (display "No active sessions.\n")
           (begin
            (for-each display-session-enhanced sessions)
            (newline)
            (display "Commands:\n")
            (display "  (show-sessions-enhanced) - Show this overview\n")
            (display "  (session-info 'id)      - Get session details\n")
            (display "  (list-sessions)         - List all sessions\n")))))

(define (display-session-enhanced session)
  "Display a single session's information"
  (let ([id (cdr (assq 'id session))]
        [tier (cdr (assq 'tier session))]
        [name (cdr (assq 'name session))]
        [logged-in (cdr (assq 'logged-in session))]
        [created (cdr (assq 'created session))]
        [last-active (cdr (assq 'last-active session))])
       
       (display "┌───────────────────────────────────────────────────────────────┐\n")
       (display "│ Session: ")
       (display id)
       (newline)
       (display "│ Tier: ")
       (display tier)
       (display " | Name: ")
       (display name)
       (newline)
       (display "│ Status: ")
       (if logged-in (display "✅ Logged in") (display "❌ Not logged in"))
       (newline)
       (display "│ Created: ")
       (display created)
       (newline)
       (display "│ Last Active: ")
       (display last-active)
       (newline)
       (display "└───────────────────────────────────────────────────────────────┘\n")
       (newline)))

;;; ============================================================
;;; Session Commands
;;; ============================================================

(define (sessions)
  "Alias for show-sessions-enhanced"
  (show-sessions-enhanced))

(define (my-session)
  "Get current session info"
  (let ([current-id (current-session-id)])
       (if current-id
           (session-info current-id)
           #f)))

;;; ============================================================
;;; Demo
;;; ============================================================

(define (demo-session-enhanced)
  "Demonstrate enhanced session management"
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "  ENHANCED SESSION MANAGEMENT DEMO\n")
  (display "═══════════════════════════════════════════════════════════════\n\n")
  
  (display "Current sessions:\n")
  (show-sessions-enhanced)
  
  (display "\n✅ Enhanced session management provides:\n")
  (display "✅ Clear session overview with visual formatting\n")
  (display "✅ Easy-to-read session information\n")
  (display "✅ Quick access to session details\n")
  (display "✅ User-friendly commands\n"))

(display "Enhanced session management loaded! Run (demo-session-enhanced) to see it.\n")

;; Run demo
(demo-session-enhanced)
