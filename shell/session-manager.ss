;;; shell/session-manager.ss — Multi-Session Management
;;;
;;; Manages multiple concurrent sessions for The Fold REPL.
;;; Each session has its own tier, name, and context.
;;;
;;; Session Structure:
;;;   {id: String
;;;    tier: Symbol (opus | sonnet | haiku)
;;;    name: Symbol
;;;    created: Timestamp
;;;    last-active: Timestamp
;;;    context: Environment}
;;;
;;; This is Shell code: manages mutable session state.

;;; ============================================================
;;; Session Storage
;;; ============================================================

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

;;; ============================================================
;;; Session Operations
;;; ============================================================

;;; create-session! : String → Session
;;; Create a new session with the given ID.
(define (create-session! session-id)
  (let ([session (make-session session-id)])
    (hashtable-set! *sessions* session-id session)
    session))

;;; make-session : String → Session
;;; Construct a new session record.
;;; Each session gets an ISOLATED environment to prevent cross-session pollution.
;;; Timestamps are stored as seconds (numbers) for easy arithmetic.
(define (make-session id)
  `((id . ,id)
    (tier . #f)
    (name . #f)
    (created . ,(time-second (current-time)))
    (last-active . ,(time-second (current-time)))
    (logged-in . #f)
    (context . ,(make-isolated-environment))))

;;; make-isolated-environment : → Environment
;;; Create a fresh isolated environment for a session.
;;; Inherits from interaction-environment but is a separate copy.
(define (make-isolated-environment)
  ;; Create a new environment that inherits from the current one
  ;; but has its own bindings. Using (environment '(chezscheme))
  ;; gives us a clean Chez Scheme environment.
  ;; We then copy key bindings from interaction-environment.
  (let ([env (copy-environment (interaction-environment) #t)])
    env))

;;; get-session : String → Session | #f
;;; Get a session by ID, updating last-active.
(define (get-session session-id)
  (let ([session (hashtable-ref *sessions* session-id #f)])
    (when session
      (set-cdr! (assq 'last-active session) (time-second (current-time))))
    session))

;;; get-or-create-session! : String → Session
;;; Get existing session or create new one.
(define (get-or-create-session! session-id)
  (or (get-session session-id)
      (create-session! session-id)))

;;; delete-session! : String → void
;;; Delete a session by ID.
(define (delete-session! session-id)
  (hashtable-delete! *sessions* session-id))

;;; ============================================================
;;; Session Login/Logout
;;; ============================================================

;;; session-login! : String Symbol Symbol String → void
;;; Login a session with tier and name.
(define (session-login! session-id tier name message)
  (let ([session (get-or-create-session! session-id)])
    (set-cdr! (assq 'tier session) tier)
    (set-cdr! (assq 'name session) name)
    (set-cdr! (assq 'logged-in session) #t)
    (set-cdr! (assq 'last-active session) (time-second (current-time)))

    ;; Environment is already isolated from make-session
    ;; No need to reset it here - that would lose any definitions

    ;; Store session file for this session
    (save-session-file! session-id tier name)))

;;; session-logout! : String → void
;;; Logout a session.
(define (session-logout! session-id)
  (let ([session (get-session session-id)])
    (when session
      (set-cdr! (assq 'tier session) #f)
      (set-cdr! (assq 'name session) #f)
      (set-cdr! (assq 'logged-in session) #f)
      (delete-session-file! session-id))))

;;; ============================================================
;;; Session Context Evaluation
;;; ============================================================

;;; eval-in-session : String String → Any
;;; Evaluate an expression string in a session's context.
;;; Auto-creates the session if it doesn't exist.
(define (eval-in-session session-id expr-str)
  (let ([session (get-or-create-session! session-id)])
    (let ([context (cdr (assq 'context session))])
      (let ([port (open-input-string expr-str)])
        (let loop ([last-result (void)])
          (let ([expr (read port)])
            (if (eof-object? expr)
                last-result
                (loop (eval expr context)))))))))

;;; ============================================================
;;; Session File Storage (for compatibility with existing REPL)
;;; ============================================================

(define *session-dir* ".fold-sessions")

;;; ensure-session-dir! : → void
(define (ensure-session-dir!)
  (unless (file-exists? *session-dir*)
    (mkdir *session-dir*)))

;;; session-file-path : String → String
(define (session-file-path session-id)
  (string-append *session-dir* "/" session-id ".session"))

;;; save-session-file! : String Symbol Symbol → void
;;; Save session to file (for compatibility with existing tools).
(define (save-session-file! session-id tier name)
  (ensure-session-dir!)
  (call-with-output-file (session-file-path session-id)
    (lambda (p)
      (write `((tier . ,tier)
               (name . ,name)
               (session-id . ,session-id)) p))
    'replace))

;;; delete-session-file! : String → void
(define (delete-session-file! session-id)
  (let ([path (session-file-path session-id)])
    (when (file-exists? path)
      (delete-file path))))

;;; ============================================================
;;; Session Cleanup
;;; ============================================================

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

;;; ============================================================
;;; Session Information
;;; ============================================================

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
