;;; thimble/repl-daemon-mcp.ss — Session Broker Daemon
;;;
;;; Broker daemon that spawns one REPL worker process per session-id.
;;; The broker does not eval user code; it only watches request files
;;; and ensures a worker is running for each active session.
;;;
;;; Protocol:
;;;   .fold-repl/
;;;     ready                        — Sentinel (daemon running)
;;;     requests/<session-id>.ss     — Session requests (raw expressions)
;;;     responses/<session-id>.txt   — Session responses
;;;     responses/<session-id>.error.txt — Session errors
;;;     workers/<session-id>.*       — Worker metadata
;;;
;;; This is Shell code: uses IO and process spawning.
;;;
;;; NOTE: string-contains? provided by core/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *repl-dir* ".fold-repl")
(define *requests-dir* ".fold-repl/requests")
(define *responses-dir* ".fold-repl/responses")
(define *workers-dir* ".fold-repl/workers")
(define *ready-file* ".fold-repl/ready")
(define *worker-script* "shell/repl-worker.ss")
(define *poll-interval-ns* 100000000)  ; 100ms in nanoseconds
(define *worker-timeout* 60)           ; seconds without heartbeat
(define *starting-timeout* 30)         ; seconds to wait for worker startup
(define *cleanup-interval* 300)        ; 5 minutes in seconds
(define *last-cleanup* (time-second (current-time)))

;;; ============================================================
;;; Utilities
;;; ============================================================

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

(define (read-number-file path)
  (guard (e [else #f])
         (and (file-exists? path)
              (call-with-input-file path
                                    (lambda (p)
                                            (string->number (get-line p)))))))

(define (process-alive? pid)
  (if (windows?)
      #f
      (zero? (system (format "kill -0 ~a 2>/dev/null" pid)))))

(define (worker-alive? session-id)
  (let* ([now (time-second (current-time))]
         [hb (read-number-file (heartbeat-path session-id))]
         [pid (read-number-file (pid-path session-id))])
        (cond
         [(and hb (< (- now hb) *worker-timeout*)) #t]
         [(and pid (process-alive? pid)) #t]
         [else #f])))

(define (terminate-worker! session-id)
  (let ([pid (read-number-file (pid-path session-id))])
       (when pid
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

;;; ============================================================
;;; Worker Spawning
;;; ============================================================

(define (scheme-command)
  (or (getenv "FOLD_SCHEME_CMD") "scheme"))

(define (spawn-worker! session-id)
  (let* ([scheme (scheme-command)]
         [script *worker-script*]
         [log (log-path session-id)]
         [cmd (if (windows?)
                  (format "cmd.exe /c start /b \"\" \"~a\" --script ~a ~a"
                          scheme script session-id)
                  (format "~a --script ~a ~a > ~a 2>&1 &"
                          scheme script session-id log))])
        (call-with-output-file (starting-path session-id)
                               (lambda (p)
                                       (display (time-second (current-time)) p))
                               'replace)
        (system cmd)))

(define (ensure-worker! session-id)
  (unless (or (worker-alive? session-id) (worker-starting? session-id))
          (spawn-worker! session-id)))

;;; ============================================================
;;; Cleanup
;;; ============================================================

(define (cleanup-stale-workers!)
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
                                        (for-each
                                         (lambda (path)
                                                 (when (file-exists? path)
                                                       (delete-file path)))
                                         (list (pid-path session-id)
                                               (ready-path session-id)
                                               (heartbeat-path session-id)
                                               (starting-path session-id)))))))
              files))))

;;; ============================================================
;;; Main Daemon Loop
;;; ============================================================

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
                            (ensure-worker! (extract-session-id-from-filename filename))))
              files))))

(define (periodic-cleanup!)
  (let ([now (time-second (current-time))])
       (when (> (- now *last-cleanup*) *cleanup-interval*)
             (cleanup-stale-workers!)
             (set! *last-cleanup* now))))

(define (daemon-loop)
  (when *daemon-running*
        (scan-for-requests!)
        (periodic-cleanup!)
        (when (and *daemon-running* (daemon-running?))
              (sleep (make-time 'time-duration *poll-interval-ns* 0))
              (daemon-loop))))

;;; ============================================================
;;; Startup
;;; ============================================================

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
