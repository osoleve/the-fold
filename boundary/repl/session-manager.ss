(doc 'module 'session-manager)
(doc 'description "Multi-Session Management — Manages multiple concurrent sessions for The Fold REPL")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'architecture)
(doc 'note "Session Structure: {id, tier, model, name, created, last-active, logged-in}")
(doc 'note "Each worker process handles one session. Session DATA isolation (tier, name) is achieved via the *current-session-id* parameter, which is set before each evaluation. Functions like hi/who/bye look up their session data using (current-session-id).")

(doc 'section 'session-storage)

(define *sessions* (make-hashtable string-hash string=?))
(define *session-timeout* 3600) ; 1 hour in seconds

(doc *current-session-id* 'type 'Parameter)
(doc *current-session-id* 'description "Parameter holding the current session ID. Set by the daemon before evaluating each request. Used by hi/bye/who to know which session they're operating on.")
(define *current-session-id* (make-parameter #f))

(doc with-session 'type '(-> String Thunk Any))
(doc with-session 'description "Execute thunk with *current-session-id* bound to the given session")
(define (with-session session-id thunk)
  (parameterize ([*current-session-id* session-id])
                (thunk)))

(doc current-session-id 'type '(-> (Option String)))
(doc current-session-id 'description "Get the current session ID (if any)")
(define (current-session-id)
  (*current-session-id*))

(doc 'section 'session-operations)

(doc create-session! 'type '(-> String Session))
(doc create-session! 'description "Create a new session with the given ID")
(define (create-session! session-id)
  (let ([session (make-session session-id)])
       (hashtable-set! *sessions* session-id session)
       session))

(doc make-session 'type '(-> String Session))
(doc make-session 'description "Construct a new session record. Isolation is handled by process boundaries; *current-session-id* tags data. Timestamps are stored as seconds (numbers) for easy arithmetic.")
(doc make-session 'note "We use explicit cons/list to ensure each session gets FRESH cons cells. Using quasiquote with literal parts would share structure, causing set-cdr! on one session to affect all sessions.")
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

(doc load-session-file 'type '(-> String (Option Session)))
(doc load-session-file 'description "Load session metadata from disk and register it")
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

(doc session-maybe-warn-rehydrated! 'type '(-> Session Void))
(doc session-maybe-warn-rehydrated! 'description "Warn once per 5 minutes if this session was restored from disk")
(define (session-maybe-warn-rehydrated! session)
  (let* ([rehydrated-pair (assq 'rehydrated-at session)]
         [warned-pair (assq 'rehydrated-warned-at session)]
         [rehydrated (and rehydrated-pair (cdr rehydrated-pair))]
         [warned (and warned-pair (cdr warned-pair))])
        (when rehydrated
              (let ([now (time-second (current-time))])
                   (when (or (not warned) (>= (- now warned) 300))
                         (display "Session restored from disk.\n")
                         (when warned-pair
                               (set-cdr! warned-pair now)))))))

(doc get-session 'type '(-> String (Option Session)))
(doc get-session 'description "Get a session by ID, updating last-active")
(define (get-session session-id)
  (let ([session (hashtable-ref *sessions* session-id #f)])
       (when session
             (set-cdr! (assq 'last-active session) (time-second (current-time))))
       (or session
           (let ([loaded (load-session-file session-id)])
                (when loaded
                      (set-cdr! (assq 'last-active loaded) (time-second (current-time))))
                loaded))))

(doc get-or-create-session! 'type '(-> String Session))
(doc get-or-create-session! 'description "Get existing session or create new one")
(define (get-or-create-session! session-id)
  (or (get-session session-id)
      (create-session! session-id)))

(doc delete-session! 'type '(-> String Void))
(doc delete-session! 'description "Delete a session by ID")
(define (delete-session! session-id)
  (hashtable-delete! *sessions* session-id))

(doc 'section 'session-login-logout)

(doc session-login! 'type '(-> String Symbol Symbol (Option Symbol) Void))
(doc session-login! 'description "Login a session with tier, name, and optional model")
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

(doc session-logout! 'type '(-> String Void))
(doc session-logout! 'description "Logout a session")
(define (session-logout! session-id)
  (let ([session (get-session session-id)])
       (when session
             (set-cdr! (assq 'tier session) #f)
             (set-cdr! (assq 'name session) #f)
             (set-cdr! (assq 'logged-in session) #f)
             (delete-session-file! session-id))))

(doc 'section 'session-file-storage)
(doc 'note "Session file storage for compatibility with existing REPL")

(define *session-dir* ".fold-sessions")

(doc ensure-session-dir! 'type '(-> Void))
(doc ensure-session-dir! 'description "Ensure session directory exists")
(define (ensure-session-dir!)
  (unless (file-exists? *session-dir*)
          (mkdir *session-dir*)))

(doc session-file-path 'type '(-> String String))
(doc session-file-path 'description "Get path to session file")
(define (session-file-path session-id)
  (string-append *session-dir* "/" session-id ".session"))

(doc save-session-file! 'type '(-> String Symbol Symbol Symbol Void))
(doc save-session-file! 'description "Save session to file (for compatibility with existing tools)")
(define (save-session-file! session-id tier name model)
  (ensure-session-dir!)
  (call-with-output-file (session-file-path session-id)
                         (lambda (p)
                                 (write `((tier . ,tier)
                                          (model . ,model)
                                          (name . ,name)
                                          (session-id . ,session-id)) p))
                         'replace))

(doc delete-session-file! 'type '(-> String Void))
(doc delete-session-file! 'description "Delete session file from disk")
(define (delete-session-file! session-id)
  (let ([path (session-file-path session-id)])
       (when (file-exists? path)
             (delete-file path))))

(doc 'section 'session-cleanup)

(doc cleanup-expired-sessions! 'type '(-> Nat))
(doc cleanup-expired-sessions! 'description "Remove sessions that haven't been active recently. Returns the number of sessions cleaned up.")
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

(doc 'section 'session-information)

(doc list-sessions 'type '(-> (List Session)))
(doc list-sessions 'description "List all active sessions")
(define (list-sessions)
  (let ([sessions '()])
       (vector-for-each
        (lambda (session-id)
                (let ([session (hashtable-ref *sessions* session-id #f)])
                     (when session
                           (set! sessions (cons session sessions)))))
        (hashtable-keys *sessions*))
       sessions))

(doc session-count 'type '(-> Nat))
(doc session-count 'description "Get count of active sessions")
(define (session-count)
  (hashtable-size *sessions*))

(doc session-info 'type '(-> String (Option Alist)))
(doc session-info 'description "Get basic info about a session")
(define (session-info session-id)
  (let ([session (get-session session-id)])
       (and session
            `((id . ,session-id)
              (tier . ,(cdr (assq 'tier session)))
              (name . ,(cdr (assq 'name session)))
              (logged-in . ,(cdr (assq 'logged-in session)))))))

(doc 'section 'user-facing-commands)

(doc session-field 'type '(-> Session Symbol Any Any))
(doc session-field 'description "Safe alist lookup with default value")
(define (session-field session key default)
  (let ([pair (assq key session)])
    (if pair (cdr pair) default)))

(doc who 'type '(-> Void))
(doc who 'description "Display current session info")
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

(doc bye 'type '(-> Void))
(doc bye 'description "Cleanup and logout current session. Logs out, deletes session files, and signals the worker process to exit after sending its response.")
(define (bye)
  (let ([session-id (current-session-id)])
    (when session-id
      (session-logout! session-id))
    ;; Clean up .fold-session file (persisted session from fold-agent.py)
    (when (file-exists? ".fold-session")
      (delete-file ".fold-session"))
    ;; Signal worker to exit after sending response
    (when (top-level-bound? '*bye-requested*)
      (set! *bye-requested* #t))
    (display "Goodbye.\n")))
