(load "core/base/prelude.ss")

(doc 'module 'repl-daemon-mcp)
(doc 'description "Session Broker Daemon — Broker daemon that spawns one REPL worker process per session-id")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'note "The broker does not eval user code; it only watches request files and ensures a worker is running for each active session")
(doc 'note "string-contains? provided by core/prelude.ss")

(doc 'protocol "
.fold-repl/
  ready                        — Sentinel (daemon running)
  requests/<session-id>.ss     — Session requests (raw expressions)
  responses/<session-id>.txt   — Session responses
  responses/<session-id>.error.txt — Session errors
  workers/<session-id>.*       — Worker metadata
")

(doc 'section 'configuration)

(define *repl-dir* ".fold-repl")
(define *requests-dir* ".fold-repl/requests")
(define *responses-dir* ".fold-repl/responses")
(define *workers-dir* ".fold-repl/workers")
(define *ready-file* ".fold-repl/ready")
(define *worker-script* "boundary/repl/repl-worker.ss")
(define *poll-interval-ns* 100000000)  ; 100ms in nanoseconds
(define *worker-timeout* 600)          ; seconds without heartbeat (10 min - allows long jobs)
(define *starting-timeout* 30)         ; seconds to wait for worker startup
(define *cleanup-interval* 300)        ; 5 minutes in seconds
(define *idle-timeout* 600)            ; 10 minutes without request - kill idle workers
(define *max-workers* 50)              ; Maximum concurrent workers (DoS prevention)
(define *last-cleanup* (time-second (current-time)))

;;; ====
;;; Input Validation
;;; ====

;;; valid-session-id? : String → Boolean
;;; Session IDs must contain only alphanumeric characters and hyphens.
;;; This prevents command injection when session-id is used in shell commands.
(define (valid-session-id? s)
  (let ([len (string-length s)])
       (and (> len 0)
            (<= len 128)  ; Reasonable max length
            (let loop ([i 0])
                 (if (>= i len)
                     #t
                     (let ([c (string-ref s i)])
                          (if (or (char-alphabetic? c)
                                  (char-numeric? c)
                                  (char=? c #\-)
                                  (char=? c #\_))
                              (loop (+ i 1))
                              #f)))))))

;;; sanitize-session-id : String → String | #f
;;; Return session-id if valid, #f otherwise.
(define (sanitize-session-id s)
  (if (valid-session-id? s)
      s
      #f))

;;; valid-scheme-command? : String → Boolean
;;; Scheme command must be a safe path/name without shell metacharacters.
;;; Allows: alphanumeric, hyphen, underscore, period, forward slash
;;; This prevents command injection via FOLD_SCHEME_CMD environment variable.
(define (valid-scheme-command? s)
  (and (string? s)
       (let ([len (string-length s)])
            (and (> len 0)
                 (<= len 256)  ; Reasonable max path length
                 (let loop ([i 0])
                      (if (>= i len)
                          #t
                          (let ([c (string-ref s i)])
                               (if (or (char-alphabetic? c)
                                       (char-numeric? c)
                                       (char=? c #\-)
                                       (char=? c #\_)
                                       (char=? c #\.)
                                       (char=? c #\/))
                                   (loop (+ i 1))
                                   #f))))))))

;;; ====
;;; Utilities
;;; ====

;;; NOTE: string-contains? provided by core/prelude.ss

(define (windows?)
  (let ([os (getenv "OS")])
       (and os (string-contains? os "Windows"))))

(define (ensure-dirs!)
  (unless (file-exists? *repl-dir*)
          (mkdir *repl-dir*))
  (unless (file-exists? *requests-dir*)
          (mkdir *requests-dir*))
  (unless (file-exists? *responses-dir*)
          (mkdir *responses-dir*))
  (unless (file-exists? *workers-dir*)
          (mkdir *workers-dir*)))

(define (write-ready!)
  (when (file-exists? *ready-file*)
        (delete-file *ready-file*))
  (call-with-output-file *ready-file*
                         (lambda (p)
                                 (display (format "~a" (current-time)) p))))

(define (clear-ready!)
  (when (file-exists? *ready-file*)
        (delete-file *ready-file*)))

(define (daemon-running?)
  (file-exists? *ready-file*))

(define (extract-session-id-from-filename filename)
  (let ([len (string-length filename)])
       (if (and (>= len 3) (string=? (substring filename (- len 3) len) ".ss"))
           (substring filename 0 (- len 3))
           filename)))

(define (pid-path session-id)
  (string-append *workers-dir* "/" session-id ".pid"))

(define (ready-path session-id)
  (string-append *workers-dir* "/" session-id ".ready"))

(define (heartbeat-path session-id)
  (string-append *workers-dir* "/" session-id ".heartbeat"))

(define (starting-path session-id)
  (string-append *workers-dir* "/" session-id ".starting"))

(define (log-path session-id)
  (string-append *workers-dir* "/" session-id ".log"))

(define (lastreq-path session-id)
  (string-append *workers-dir* "/" session-id ".lastreq"))

(define (expires-path session-id)
  (string-append *workers-dir* "/" session-id ".expires"))

(define (read-number-file path)
  (guard (e [else #f])
         (and (file-exists? path)
              (call-with-input-file path
                                    (lambda (p)
                                            (string->number (get-line p)))))))

(define (process-alive? pid)
  ;; SECURITY: pid must be a positive integer
  (if (or (not (integer? pid)) (<= pid 0))
      #f
      (if (windows?)
          ;; On Windows, use tasklist + find to check if process exists
          ;; tasklist always exits 0, but piping to find fails if PID not found
          (zero? (system (format "tasklist /FI \"PID eq ~a\" 2>nul | find \"~a\" >nul" pid pid)))
          (zero? (system (format "kill -0 ~a 2>/dev/null" pid))))))

(define (request-pending? session-id)
  ;; Check if there's a pending request file for this session.
  (file-exists? (string-append *requests-dir* "/" session-id ".ss")))

(define (worker-alive? session-id)
  ;; A worker is alive if:
  ;; 1. It has a recent heartbeat (within timeout) AND process is running, OR
  ;; 2. Process is running AND has a pending request (long-running job)
  ;; The second condition prevents killing workers during long evaluations.
  (let* ([now (time-second (current-time))]
         [hb (read-number-file (heartbeat-path session-id))]
         [pid (read-number-file (pid-path session-id))])
        (and pid
             (process-alive? pid)
             (or (and hb (< (- now hb) *worker-timeout*))
                 (request-pending? session-id)))))

(define (terminate-worker! session-id)
  ;; SECURITY: Validate session-id (path construction) and pid (shell command)
  (unless (valid-session-id? session-id)
          (error 'terminate-worker! "Invalid session-id" session-id))
  (let ([pid (read-number-file (pid-path session-id))])
       (when (and pid (integer? pid) (> pid 0))
             (if (windows?)
                 (system (format "taskkill /PID ~a /F >nul 2>nul" pid))
                 (begin
                  (system (format "kill ~a 2>/dev/null" pid))
                  (when (process-alive? pid)
                        (system (format "kill -9 ~a 2>/dev/null" pid))))))))

(define (worker-starting? session-id)
  (let* ([now (time-second (current-time))]
         [started (read-number-file (starting-path session-id))])
        (cond
         [(and started (< (- now started) *starting-timeout*)) #t]
         [started
          (when (file-exists? (starting-path session-id))
                (delete-file (starting-path session-id)))
          #f]
         [else #f])))

;;; ====
;;; Worker Spawning
;;; ====

(define (scheme-command)
  (let ([env-cmd (getenv "FOLD_SCHEME_CMD")])
       (cond
        [(not env-cmd) "scheme"]  ; Default
        [(valid-scheme-command? env-cmd) env-cmd]
        [else
         ;; Log the rejection and use default
         (display (format "WARNING: Invalid FOLD_SCHEME_CMD rejected: ~s (using 'scheme')\n"
                          env-cmd))
         "scheme"])))

(define (spawn-worker! session-id)
  ;; SECURITY: Validate session-id to prevent command injection
  (unless (valid-session-id? session-id)
          (display (format "WARNING: Invalid session-id rejected: ~s\n" session-id))
          (error 'spawn-worker! "Invalid session-id" session-id))
  ;; Claim the "starting" slot first
  (call-with-output-file (starting-path session-id)
                         (lambda (p)
                                 (display (time-second (current-time)) p))
                         'replace)
  ;; Double-check: if worker already has a PID and is alive, don't spawn duplicate
  (let ([existing-pid (read-number-file (pid-path session-id))])
       (unless (and existing-pid (process-alive? existing-pid))
               ;; No existing worker, proceed with spawn
               (let* ([scheme (scheme-command)]
                      [script *worker-script*]
                      [log (log-path session-id)]
                      [cmd (if (windows?)
                               (format "cmd.exe /c start /b \"\" \"~a\" --script ~a ~a"
                                       scheme script session-id)
                               (format "~a --script ~a ~a > ~a 2>&1 &"
                                       scheme script session-id log))])
                     (system cmd)))))

(define (count-active-workers)
  "Count number of active workers (alive or starting)."
  (if (not (file-exists? *workers-dir*))
      0
      (let ([files (directory-list *workers-dir*)])
           (let loop ([fs files] [count 0])
                (if (null? fs)
                    count
                    (let ([f (car fs)])
                         ;; Count .pid files with alive processes
                         (if (and (string? f)
                                  (> (string-length f) 4)
                                  (string=? (substring f (- (string-length f) 4) (string-length f)) ".pid"))
                             (let* ([session-id (substring f 0 (- (string-length f) 4))]
                                    [pid (read-number-file (pid-path session-id))])
                                   (if (and pid (or (process-alive? pid)
                                                    (worker-starting? session-id)))
                                       (loop (cdr fs) (+ count 1))
                                       (loop (cdr fs) count)))
                             (loop (cdr fs) count))))))))

(define (ensure-worker! session-id)
  ;; Check worker limit before spawning (DoS prevention)
  (when (< (count-active-workers) *max-workers*)
        (unless (or (worker-alive? session-id) (worker-starting? session-id))
                (spawn-worker! session-id))))

;;; ====
;;; Cleanup
;;; ====

(define (cleanup-worker-files! session-id)
  "Remove all worker metadata files for a session."
  (for-each
   (lambda (path)
           (when (file-exists? path)
                 (delete-file path)))
   (list (pid-path session-id)
         (ready-path session-id)
         (heartbeat-path session-id)
         (lastreq-path session-id)
         (expires-path session-id)
         (starting-path session-id))))

(define (cleanup-stale-workers!)
  "Kill workers with stale heartbeats (process died without cleanup)."
  (when (file-exists? *workers-dir*)
        (let ([files (directory-list *workers-dir*)])
             (for-each
              (lambda (filename)
                      (when (and (string? filename)
                                 (> (string-length filename) 10)
                                 (string=? (substring filename
                                                      (- (string-length filename) 10)
                                                      (string-length filename))
                                           ".heartbeat"))
                            (let* ([session-id (substring filename 0 (- (string-length filename) 10))]
                                   [hb (read-number-file (heartbeat-path session-id))]
                                   [now (time-second (current-time))])
                                  (when (and hb (>= (- now hb) *worker-timeout*))
                                        (terminate-worker! session-id)
                                        (cleanup-worker-files! session-id)))))
              files))))

(define (cleanup-expired-workers!)
  "Kill workers whose expiration time has passed.
   Uses decay-based expiration: active sessions extend their lifetime,
   but with diminishing returns as expiration gets further out."
  (when (file-exists? *workers-dir*)
        (let ([files (directory-list *workers-dir*)])
             (for-each
              (lambda (filename)
                      (when (and (string? filename)
                                 (> (string-length filename) 8)
                                 (string=? (substring filename
                                                      (- (string-length filename) 8)
                                                      (string-length filename))
                                           ".expires"))
                            (let* ([session-id (substring filename 0 (- (string-length filename) 8))]
                                   [expires (read-number-file (expires-path session-id))]
                                   [now (time-second (current-time))])
                                  (when (and expires (>= now expires))
                                        (terminate-worker! session-id)
                                        (cleanup-worker-files! session-id)))))
              files))))

(define (cleanup-idle-workers!)
  "Kill workers that haven't processed a request in *idle-timeout* seconds.
   Fallback for workers without expiration files (legacy compatibility)."
  (when (file-exists? *workers-dir*)
        (let ([files (directory-list *workers-dir*)])
             (for-each
              (lambda (filename)
                      (when (and (string? filename)
                                 (> (string-length filename) 8)
                                 (string=? (substring filename
                                                      (- (string-length filename) 8)
                                                      (string-length filename))
                                           ".lastreq"))
                            (let* ([session-id (substring filename 0 (- (string-length filename) 8))]
                                   [has-expires (file-exists? (expires-path session-id))])
                                  ;; Only use idle-timeout for sessions without expiration
                                  (unless has-expires
                                    (let ([lastreq (read-number-file (lastreq-path session-id))]
                                          [now (time-second (current-time))])
                                         (when (and lastreq (>= (- now lastreq) *idle-timeout*))
                                               (terminate-worker! session-id)
                                               (cleanup-worker-files! session-id)))))))
              files))))

;;; ====
;;; Main Daemon Loop
;;; ====

(define *daemon-running* #t)

(define (daemon-stop!)
  (set! *daemon-running* #f)
  (clear-ready!)
  "Daemon stopping...")

(define (scan-for-requests!)
  (when (file-exists? *requests-dir*)
        (let ([files (directory-list *requests-dir*)])
             (for-each
              (lambda (filename)
                      (when (and (string? filename)
                                 (> (string-length filename) 3)
                                 (string=? (substring filename
                                                      (- (string-length filename) 3)
                                                      (string-length filename))
                                           ".ss"))
                            (let ([session-id (extract-session-id-from-filename filename)])
                                 ;; SECURITY: Skip invalid session IDs
                                 (when (valid-session-id? session-id)
                                       (ensure-worker! session-id)))))
              files))))

(define (periodic-cleanup!)
  (let ([now (time-second (current-time))])
       (when (> (- now *last-cleanup*) *cleanup-interval*)
             (cleanup-stale-workers!)    ; Dead processes (no heartbeat)
             (cleanup-expired-workers!)  ; Expired workers (decay-based)
             (cleanup-idle-workers!)     ; Legacy: idle workers without expiration
             (set! *last-cleanup* now))))

(define (daemon-loop)
  (when *daemon-running*
        (scan-for-requests!)
        (periodic-cleanup!)
        (when (and *daemon-running* (daemon-running?))
              (sleep (make-time 'time-duration *poll-interval-ns* 0))
              (daemon-loop))))

;;; ====
;;; Startup
;;; ====

(define (start-daemon!)
  "Start the session broker daemon."
  (ensure-dirs!)
  (write-ready!)
  
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║          THE FOLD — SESSION BROKER STARTED                  ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display (format "Sessions: ~a\n" *requests-dir*))
  (display (format "Output:   ~a\n" *responses-dir*))
  (display (format "Workers:  ~a\n" *workers-dir*))
  (display "Multitenancy enabled. Waiting for requests...\n\n")
  
  (daemon-loop)
  
  (clear-ready!)
  (display "Daemon stopped.\n"))
