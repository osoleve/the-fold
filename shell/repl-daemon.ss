;;; shell/repl-daemon.ss — Persistent REPL Daemon
;;;
;;; A file-based IPC system for Claude Code integration.
;;; Polls for request files, evaluates them, writes responses.
;;;
;;; Protocol:
;;;   .fold-repl/
;;;     request.ss    — Claude writes expressions here
;;;     response.txt  — Daemon writes results here
;;;     ready         — Sentinel file (exists = daemon running)
;;;     error.txt     — Error output if evaluation fails
;;;
;;; Usage:
;;;   Start:  scheme --script shell/start-daemon.ss
;;;   Stop:   Delete .fold-repl/ready or send (daemon-stop!)
;;;
;;; This is Shell code: uses IO, manages daemon state.

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *repl-dir* ".fold-repl")
(define *request-file* ".fold-repl/request.ss")
(define *response-file* ".fold-repl/response.txt")
(define *error-file* ".fold-repl/error.txt")
(define *ready-file* ".fold-repl/ready")
(define *poll-interval-ns* 100000000)  ; 100ms in nanoseconds

;;; ============================================================
;;; Directory Setup
;;; ============================================================

(define (ensure-repl-dir!)
  (unless (file-exists? *repl-dir*)
    (mkdir *repl-dir*)))

(define (write-ready!)
  (call-with-output-file *ready-file*
    (lambda (p)
      (display (format "~a" (current-time)) p))))

(define (clear-ready!)
  (when (file-exists? *ready-file*)
    (delete-file *ready-file*)))

(define (daemon-running?)
  (file-exists? *ready-file*))

;;; ============================================================
;;; Request Processing
;;; ============================================================

(define (read-request)
  (when (file-exists? *request-file*)
    (call-with-input-file *request-file*
      (lambda (p)
        (get-string-all p)))))

(define (clear-request!)
  (when (file-exists? *request-file*)
    (delete-file *request-file*)))

(define (write-response result)
  (when (file-exists? *response-file*)
    (delete-file *response-file*))
  (call-with-output-file *response-file*
    (lambda (p)
      (if (string? result)
          (display result p)
          (pretty-print result p)))))

(define (write-error msg)
  (when (file-exists? *error-file*)
    (delete-file *error-file*))
  (call-with-output-file *error-file*
    (lambda (p)
      (display msg p))))

(define (clear-error!)
  (when (file-exists? *error-file*)
    (delete-file *error-file*)))

;;; ============================================================
;;; Expression Evaluation
;;; ============================================================

;;; NOTE: These functions use unique names to avoid shadowing by
;;; core/compile.ss which defines its own eval-string for the Fold
;;; type system. We need Chez Scheme's eval, not Fold's type checker.

(define (scheme-eval-string str)
  "Evaluate a string containing Scheme expressions.
   Returns the result of the last expression."
  (let ([port (open-input-string str)])
    (let loop ([last-result (void)])
      (let ([expr (read port)])
        (if (eof-object? expr)
            last-result
            (loop (eval expr)))))))

(define (scheme-eval-and-capture str)
  "Evaluate expressions and capture both stdout and return value.
   Returns a string with output followed by the result."
  (let ([output-port (open-output-string)])
    (let ([result
           (parameterize ([current-output-port output-port])
             (scheme-eval-string str))])
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

(define (process-request!)
  (let ([request (read-request)])
    (when request
      (clear-error!)
      (guard (e [else
                 (let ([msg (if (condition? e)
                               (format "~a" (condition-message e))
                               (format "~a" e))])
                   (write-error msg)
                   (write-response (format "ERROR: ~a" msg)))])
        (let ([result (scheme-eval-and-capture request)])
          (write-response result)))
      (clear-request!))))

;;; ============================================================
;;; Main Daemon Loop
;;; ============================================================

(define *daemon-running* #t)

(define (daemon-stop!)
  (set! *daemon-running* #f)
  (clear-ready!)
  "Daemon stopping...")

(define (daemon-loop)
  (when *daemon-running*
    (when (file-exists? *request-file*)
      (process-request!))
    ;; Check if we should keep running
    (when (and *daemon-running* (daemon-running?))
      (sleep (make-time 'time-duration *poll-interval-ns* 0))
      (daemon-loop))))

;;; ============================================================
;;; Startup
;;; ============================================================

(define (start-daemon!)
  "Load the REPL environment and start the daemon loop."
  (load "shell/repl.ss")
  (ensure-repl-dir!)

  ;; Clear any stale files
  (clear-request!)
  (when (file-exists? *response-file*)
    (delete-file *response-file*))
  (clear-error!)

  ;; Mark as ready
  (write-ready!)

  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              THE FOLD — REPL DAEMON STARTED                  ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display (format "Watching: ~a\n" *request-file*))
  (display (format "Output:   ~a\n" *response-file*))
  (display "Waiting for requests...\n\n")

  ;; Enter the loop
  (daemon-loop)

  ;; Cleanup on exit
  (clear-ready!)
  (display "Daemon stopped.\n"))
