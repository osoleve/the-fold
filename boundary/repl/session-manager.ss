;;; boundary/repl/session-manager.ss — Multi-Session Management
;;;
;;; Manages multiple concurrent sessions for The Fold REPL.
;;; Each session has its own tier and name, stored in session data.
;;;
;;; Session Structure:
;;;   {id: String
;;;    tier: Symbol (shepherd | builder | player)
;;;    model: Symbol (opus | sonnet | haiku) | #f
;;;    name: Symbol
;;;    created: Timestamp
;;;    last-active: Timestamp
;;;    logged-in: Boolean}
;;;
;;; ARCHITECTURE NOTE:
;;; Each worker process handles one session. Session DATA isolation (tier, name)
;;; is still achieved via the *current-session-id* parameter, which is set
;;; before each evaluation. Functions like hi/who/bye look up their session
;;; data using (current-session-id).
;;;
;;; This is Shell code: manages mutable session state.

;;; ====
;;; Session Storage
;;; ====

(define *sessions* (make-hashtable string-hash string=?))
(define *session-timeout* 3600) ; 1 hour in seconds

;;; *current-session-id* : Parameter holding the current session ID
;;; Set by the daemon before evaluating each request.
;;; Used by hi/bye/who to know which session they're operating on.
(define *current-session-id* (make-parameter #f))

;;; with-session : String Thunk → Any
;;; Execute thunk with *current-session-id* bound to the given session.
(define (with-session session-id thunk)
  (parameterize ([*current-session-id* session-id])
                (thunk)))

;;; current-session-id : → String | #f
;;; Get the current session ID (if any).
(define (current-session-id)
  (*current-session-id*))

;;; ====
;;; Session Operations
;;; ====

;;; create-session! : String → Session
;;; Create a new session with the given ID.
(define (create-session! session-id)
  (let ([session (make-session session-id)])
       (hashtable-set! *sessions* session-id session)
       session))

;;; make-session : String → Session
;;; Construct a new session record.
;;; Isolation is handled by process boundaries; *current-session-id* tags data.
;;; Timestamps are stored as seconds (numbers) for easy arithmetic.
;;;
;;; IMPORTANT: We use explicit cons/list to ensure each session gets FRESH cons cells.
;;; Using quasiquote with literal parts like `(name . #f) would share structure,
;;; causing set-cdr! on one session to affect all sessions.
(define (make-session id)
  (let ([now (time-second (current-time))])
       (list (cons 'id id)
             (cons 'tier #f)
             (cons 'model #f)
             (cons 'name #f)
             (cons 'created now)
             (cons 'last-active now)
             (cons 'logged-in #f)
             (cons 'rehydrated-at #f)
             (cons 'rehydrated-warned-at #f))))

;;; load-session-file : String → Session | #f
;;; Load session metadata from disk and register it.
(define (load-session-file session-id)
  (let ([path (session-file-path session-id)])
       (guard (e [else #f])
              (and (file-exists? path)
                   (let ([data (call-with-input-file path read)])
                        (and (list? data)
                             (let* ([tier (cdr (assq 'tier data))]
                                    [name (cdr (assq 'name data))]
                                    [model (cdr (assq 'model data))]
                                    [session (make-session session-id)])
                                   (when tier
                                         (set-cdr! (assq 'tier session) tier))
                                   (when model
                                         (set-cdr! (assq 'model session) model))
                                   (when name
                                         (set-cdr! (assq 'name session) name))
                                   (set-cdr! (assq 'rehydrated-at session) (time-second (current-time)))
                                   (set-cdr! (assq 'rehydrated-warned-at session) #f)
                                   (when (and tier name)
                                         (set-cdr! (assq 'logged-in session) #t))
                                   (hashtable-set! *sessions* session-id session)
                                   session)))))))

;;; session-maybe-warn-rehydrated! : Session → void
;;; Warn once per 5 minutes if this session was restored from disk.
(define (session-maybe-warn-rehydrated! session)
  (let* ([rehydrated-pair (assq 'rehydrated-at session)]
         [warned-pair (assq 'rehydrated-warned-at session)]
         [rehydrated (and rehydrated-pair (cdr rehydrated-pair))]
         [warned (and warned-pair (cdr warned-pair))])
        (when rehydrated
              (let ([now (time-second (current-time))])
                   (when (or (not warned) (>= (- now warned) 300))
                         (display "Session restored from disk; run (hi ...) if you want to announce.\n")
                         (when warned-pair
                               (set-cdr! warned-pair now)))))))

;;; get-session : String → Session | #f
;;; Get a session by ID, updating last-active.
(define (get-session session-id)
  (let ([session (hashtable-ref *sessions* session-id #f)])
       (when session
             (set-cdr! (assq 'last-active session) (time-second (current-time))))
       (or session
           (let ([loaded (load-session-file session-id)])
                (when loaded
                      (set-cdr! (assq 'last-active loaded) (time-second (current-time))))
                loaded))))

;;; get-or-create-session! : String → Session
;;; Get existing session or create new one.
(define (get-or-create-session! session-id)
  (or (get-session session-id)
      (create-session! session-id)))

;;; delete-session! : String → void
;;; Delete a session by ID.
(define (delete-session! session-id)
  (hashtable-delete! *sessions* session-id))

;;; ====
;;; Session Login/Logout
;;; ====

;;; session-login! : String Symbol Symbol [Symbol] → void
;;; Login a session with tier, name, and optional model.
(define (session-login! session-id tier name . rest)
  (let* ([model (if (and (pair? rest) (symbol? (car rest)))
                    (car rest)
                    tier)]
         [session (get-or-create-session! session-id)])
        (set-cdr! (assq 'tier session) tier)
        (set-cdr! (assq 'model session) model)
        (set-cdr! (assq 'name session) name)
        (set-cdr! (assq 'logged-in session) #t)
        (set-cdr! (assq 'rehydrated-at session) #f)
        (set-cdr! (assq 'rehydrated-warned-at session) #f)
        (set-cdr! (assq 'last-active session) (time-second (current-time)))
        
        ;; Store session file for this session
        (save-session-file! session-id tier name model)))

;;; session-logout! : String → void
;;; Logout a session.
(define (session-logout! session-id)
  (let ([session (get-session session-id)])
       (when session
             (set-cdr! (assq 'tier session) #f)
             (set-cdr! (assq 'name session) #f)
             (set-cdr! (assq 'logged-in session) #f)
             (delete-session-file! session-id))))

;;; ====
;;; Session File Storage (for compatibility with existing REPL)
;;; ====

(define *session-dir* ".fold-sessions")

;;; ensure-session-dir! : → void
(define (ensure-session-dir!)
  (unless (file-exists? *session-dir*)
          (mkdir *session-dir*)))

;;; session-file-path : String → String
(define (session-file-path session-id)
  (string-append *session-dir* "/" session-id ".session"))

;;; save-session-file! : String Symbol Symbol Symbol → void
;;; Save session to file (for compatibility with existing tools).
(define (save-session-file! session-id tier name model)
  (ensure-session-dir!)
  (call-with-output-file (session-file-path session-id)
                         (lambda (p)
                                 (write `((tier . ,tier)
                                          (model . ,model)
                                          (name . ,name)
                                          (session-id . ,session-id)) p))
                         'replace))

;;; delete-session-file! : String → void
(define (delete-session-file! session-id)
  (let ([path (session-file-path session-id)])
       (when (file-exists? path)
             (delete-file path))))

;;; ====
;;; Session Cleanup
;;; ====

;;; cleanup-expired-sessions! : → Nat
;;; Remove sessions that haven't been active recently.
;;; Returns the number of sessions cleaned up.
(define (cleanup-expired-sessions!)
  (let ([now (time-second (current-time))]
        [cleaned 0])
       (let ([session-ids (hashtable-keys *sessions*)])
            (vector-for-each
             (lambda (session-id)
                     (let ([session (hashtable-ref *sessions* session-id #f)])
                          (when session
                                (let ([last-active (cdr (assq 'last-active session))])
                                     (when (> (- now last-active) *session-timeout*)
                                           (delete-session! session-id)
                                           (delete-session-file! session-id)
                                           (set! cleaned (+ cleaned 1)))))))
             session-ids))
       cleaned))

;;; ====
;;; Session Information
;;; ====

;;; list-sessions : → List
;;; List all active sessions.
(define (list-sessions)
  (let ([sessions '()])
       (vector-for-each
        (lambda (session-id)
                (let ([session (hashtable-ref *sessions* session-id #f)])
                     (when session
                           (set! sessions (cons session sessions)))))
        (hashtable-keys *sessions*))
       sessions))

;;; session-count : → Nat
(define (session-count)
  (hashtable-size *sessions*))

;;; session-info : String → Alist | #f
;;; Get basic info about a session.
(define (session-info session-id)
  (let ([session (get-session session-id)])
       (and session
            `((id . ,session-id)
              (tier . ,(cdr (assq 'tier session)))
              (name . ,(cdr (assq 'name session)))
              (logged-in . ,(cdr (assq 'logged-in session)))))))

;;; ====
;;; User-Facing Session Commands
;;; ====

;;; session-field : Session Symbol Any → Any
;;; Safe alist lookup with default value.
(define (session-field session key default)
  (let ([pair (assq key session)])
    (if pair (cdr pair) default)))

;;; who : → void
;;; Display current session info.
(define (who)
  (let ([session-id (current-session-id)])
    (cond
     [(not session-id)
      (display "No active session.\n")]
     [else
      ;; Use get-or-create to ensure session exists
      (let ([session (get-or-create-session! session-id)])
        (cond
         [(session-field session 'logged-in #f)
          (display (format "~a (~a) - session: ~a\n"
                           (session-field session 'name 'anonymous)
                           (session-field session 'tier 'unknown)
                           session-id))]
         [else
          (display (format "Anonymous session: ~a\n" session-id))]))])))

;;; bye : → void
;;; Cleanup and logout current session.
;;; - Logs out from session (clears tier/name)
;;; - Deletes .fold-session file if present
;;; - Deletes session file from .fold-sessions/
(define (bye)
  (let ([session-id (current-session-id)])
    (when session-id
      (session-logout! session-id))
    ;; Clean up .fold-session file (persisted session from fold-agent.py)
    (when (file-exists? ".fold-session")
      (delete-file ".fold-session"))
    (display "Goodbye.\n")))
