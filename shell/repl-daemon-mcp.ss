;;; shell/repl-daemon-mcp.ss — Multi-Session REPL Daemon
;;;
;;; Enhanced version of repl-daemon.ss with session support.
;;; Supports both:
;;;   1. Legacy single-file IPC (.fold-repl/request.ss)
;;;   2. Multi-session IPC (.fold-repl/requests/<session-id>.ss)
;;;
;;; Protocol:
;;;   .fold-repl/
;;;     ready                        — Sentinel (daemon running)
;;;     request.ss                   — Legacy single-session request
;;;     response.txt                 — Legacy single-session response
;;;     requests/<session-id>.ss     — Multi-session requests
;;;     responses/<session-id>.txt   — Multi-session responses
;;;
;;; Request Format (multi-session):
;;;   ((session-id . "uuid")
;;;    (expression . <scheme-expr>)
;;;    (timestamp . 1234567890))
;;;
;;; This is Shell code: uses IO, manages daemon state.

;;; ============================================================
;;; Load Dependencies
;;; ============================================================

;; Load session manager
(load "shell/session-manager.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *repl-dir* ".fold-repl")
(define *requests-dir* ".fold-repl/requests")
(define *responses-dir* ".fold-repl/responses")
(define *ready-file* ".fold-repl/ready")

;; Legacy paths (for backward compatibility)
(define *legacy-request-file* ".fold-repl/request.ss")
(define *legacy-response-file* ".fold-repl/response.txt")
(define *legacy-error-file* ".fold-repl/error.txt")

(define *poll-interval-ns* 100000000)  ; 100ms in nanoseconds
(define *cleanup-interval* 300)        ; 5 minutes in seconds
(define *last-cleanup* (time-second (current-time)))

;;; ============================================================
;;; Directory Setup
;;; ============================================================

(define (ensure-dirs!)
  (unless (file-exists? *repl-dir*)
    (mkdir *repl-dir*))
  (unless (file-exists? *requests-dir*)
    (mkdir *requests-dir*))
  (unless (file-exists? *responses-dir*)
    (mkdir *responses-dir*)))

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

;;; ============================================================
;;; Session Request Processing
;;; ============================================================

;;; parse-session-request : String → Alist | #f
;;; Parse a session request from file content.
(define (parse-session-request content)
  (guard (e [else #f])
    (let ([data (read (open-input-string content))])
      (if (and (list? data) (assq 'session-id data) (assq 'expression data))
          data
          #f))))

;;; extract-session-id : Alist → String
(define (extract-session-id request)
  (cdr (assq 'session-id request)))

;;; extract-expression : Alist → Scheme
(define (extract-expression request)
  (cdr (assq 'expression request)))

;;; process-session-request! : String → void
;;; Process a single session request file.
(define (process-session-request! filename)
  (let ([request-path (string-append *requests-dir* "/" filename)])
    (guard (e [else
               (let ([msg (if (condition? e)
                             (condition-message e)
                             (format "~a" e))])
                 ;; Try to extract session ID for error response
                 (let ([session-id (extract-session-id-from-filename filename)])
                   (write-session-error! session-id msg)
                   (delete-file request-path)))])

      (let* ([content (call-with-input-file request-path get-string-all)]
             [request (parse-session-request content)])

        (if request
            ;; Valid session request
            (let ([session-id (extract-session-id request)]
                  [expression (extract-expression request)])
              (let ([result (eval-in-session-and-capture session-id expression)])
                (write-session-response! session-id result))
              (delete-file request-path))

            ;; Invalid format - treat as legacy request
            (let ([result (eval-and-capture content)])
              (let ([session-id (extract-session-id-from-filename filename)])
                (write-session-response! session-id result))
              (delete-file request-path)))))))

;;; extract-session-id-from-filename : String → String
;;; Extract session ID from filename (e.g., "abc-123.ss" → "abc-123").
(define (extract-session-id-from-filename filename)
  (let ([len (string-length filename)])
    (if (and (>= len 3) (string=? (substring filename (- len 3) len) ".ss"))
        (substring filename 0 (- len 3))
        filename)))

;;; eval-in-session-and-capture : String Scheme → String
;;; Evaluate expression in session context and capture output.
;;; Sets *current-session-id* so hi/bye/who know which session they're in.
(define (eval-in-session-and-capture session-id expr)
  (let ([output-port (open-output-string)])
    (let ([result
           (parameterize ([current-output-port output-port]
                          [*current-session-id* session-id])
             (if (string? expr)
                 (eval-in-session session-id expr)
                 (eval-in-session session-id (format "~s" expr))))])
      (let ([output (get-output-string output-port)])
        (cond
          ;; Only output, no meaningful return value
          [(and (eq? result (void)) (> (string-length output) 0))
           output]
          ;; Both output and result
          [(> (string-length output) 0)
           (string-append output
                         (if (eq? result (void))
                             ""
                             (string-append "\n=> " (format "~a" result))))]
          ;; Only result, no output
          [(not (eq? result (void)))
           (format "~a" result)]
          ;; Nothing
          [else ""])))))

;;; write-session-response! : String String → void
(define (write-session-response! session-id result)
  (let ([response-path (string-append *responses-dir* "/" session-id ".txt")])
    (when (file-exists? response-path)
      (delete-file response-path))
    (call-with-output-file response-path
      (lambda (p)
        (display result p)))))

;;; write-session-error! : String String → void
(define (write-session-error! session-id msg)
  (let ([error-path (string-append *responses-dir* "/" session-id ".error.txt")])
    (when (file-exists? error-path)
      (delete-file error-path))
    (call-with-output-file error-path
      (lambda (p)
        (display msg p)))))

;;; ============================================================
;;; Legacy Request Processing (Backward Compatibility)
;;; ============================================================

(define (eval-and-capture str)
  "Evaluate expressions and capture both stdout and return value.
   Returns a string with output followed by the result."
  (let ([output-port (open-output-string)])
    (let ([result
           (parameterize ([current-output-port output-port])
             (eval-string str))])
      (let ([output (get-output-string output-port)])
        (cond
          [(and (eq? result (void)) (> (string-length output) 0))
           output]
          [(> (string-length output) 0)
           (string-append output
                         (if (eq? result (void))
                             ""
                             (string-append "\n=> " (format "~a" result))))]
          [(not (eq? result (void)))
           (format "~a" result)]
          [else ""])))))

(define (eval-string str)
  "Evaluate a string containing Scheme expressions."
  (let ([port (open-input-string str)])
    (let loop ([last-result (void)])
      (let ([expr (read port)])
        (if (eof-object? expr)
            last-result
            (loop (eval expr)))))))

(define (process-legacy-request!)
  (when (file-exists? *legacy-request-file*)
    (guard (e [else
               (let ([msg (if (condition? e)
                             (condition-message e)
                             (format "~a" e))])
                 (call-with-output-file *legacy-error-file*
                   (lambda (p) (display msg p))
                   'replace)
                 (call-with-output-file *legacy-response-file*
                   (lambda (p) (display (format "ERROR: ~a" msg) p))
                   'replace))])

      (let* ([content (call-with-input-file *legacy-request-file* get-string-all)]
             [result (eval-and-capture content)])
        (when (file-exists? *legacy-response-file*)
          (delete-file *legacy-response-file*))
        (call-with-output-file *legacy-response-file*
          (lambda (p) (display result p))))
      (delete-file *legacy-request-file*))))

;;; ============================================================
;;; Main Daemon Loop
;;; ============================================================

(define *daemon-running* #t)

(define (daemon-stop!)
  (set! *daemon-running* #f)
  (clear-ready!)
  "Daemon stopping...")

;;; scan-for-requests! : → void
;;; Scan for session requests only.
;;; NOTE: Legacy single-file IPC removed to prevent multitenancy race conditions.
;;;       All clients must use session-id based requests.
(define (scan-for-requests!)
  ;; Process session requests only
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
            (process-session-request! filename)))
        files))))

;;; periodic-cleanup! : → void
;;; Periodically cleanup expired sessions.
(define (periodic-cleanup!)
  (let ([now (time-second (current-time))])
    (when (> (- now *last-cleanup*) *cleanup-interval*)
      (let ([cleaned (cleanup-expired-sessions!)])
        (when (> cleaned 0)
          (display (format "Cleaned up ~a expired sessions\n" cleaned))))
      (set! *last-cleanup* now))))

(define (daemon-loop)
  (when *daemon-running*
    (scan-for-requests!)
    (periodic-cleanup!)

    ;; Check if we should keep running
    (when (and *daemon-running* (daemon-running?))
      (sleep (make-time 'time-duration *poll-interval-ns* 0))
      (daemon-loop))))

;;; ============================================================
;;; Startup
;;; ============================================================

(define (start-daemon!)
  "Load the REPL environment and start the MCP-enabled daemon."
  (load "shell/repl.ss")
  (ensure-dirs!)

  ;; Clear any stale files
  (when (file-exists? *legacy-request-file*)
    (delete-file *legacy-request-file*))
  (when (file-exists? *legacy-response-file*)
    (delete-file *legacy-response-file*))
  (when (file-exists? *legacy-error-file*)
    (delete-file *legacy-error-file*))

  ;; Mark as ready
  (write-ready!)

  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║          THE FOLD — MCP REPL DAEMON STARTED                  ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display (format "Legacy:   ~a\n" *legacy-request-file*))
  (display (format "Sessions: ~a\n" *requests-dir*))
  (display (format "Output:   ~a\n" *responses-dir*))
  (display "Multitenancy enabled. Waiting for requests...\n\n")

  ;; Enter the loop
  (daemon-loop)

  ;; Cleanup on exit
  (clear-ready!)
  (display "Daemon stopped.\n"))
