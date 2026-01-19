;;; boundary/repl/fold-client.ss — Multi-Session Client for The Fold REPL
;;;
;;; Provides functions for Claude agents to interact with the REPL daemon
;;; in a multi-tenant safe way. Each agent uses a unique session-id.
;;;
;;; Usage (from Claude Code):
;;;   1. Generate a unique session-id (use UUID or agent ID)
;;;   2. Use Write tool to write request to .fold-repl/requests/<session-id>.ss
;;;   3. Use Read tool to read response from .fold-repl/responses/<session-id>.txt
;;;
;;; Request Format:
;;;   ((session-id . "your-unique-id")
;;;    (expression . <scheme-expression>)
;;;    (timestamp . <unix-timestamp>))
;;;
;;; This file is for documentation and can be loaded for testing.

;;; ====
;;; Client Configuration
;;; ====

(define *repl-dir* ".fold-repl")
(define *requests-dir* ".fold-repl/requests")
(define *responses-dir* ".fold-repl/responses")

;;; ====
;;; Request/Response Helpers
;;; ====

;;; make-request : String Scheme → String
;;; Create a request S-expression string for the given session and expression.
(define (make-request session-id expr)
  (format "~s"
          `((session-id . ,session-id)
            (expression . ,expr)
            (timestamp . ,(time-second (current-time))))))

;;; request-path : String → String
;;; Get the file path for a session's request.
(define (request-path session-id)
  (string-append *requests-dir* "/" session-id ".ss"))

;;; response-path : String → String
;;; Get the file path for a session's response.
(define (response-path session-id)
  (string-append *responses-dir* "/" session-id ".txt"))

;;; ====
;;; Synchronous Client (for testing)
;;; ====

;;; fold-eval : String Scheme → String
;;; Send a request to the daemon and wait for response.
;;; NOTE: This is for testing only. Claude agents should use
;;;       Write/Read tools directly for better control.
(define (fold-eval session-id expr)
  (let ([req-path (request-path session-id)]
        [resp-path (response-path session-id)])
       
       ;; Clear any stale response
       (when (file-exists? resp-path)
             (delete-file resp-path))
       
       ;; Write request
       (call-with-output-file req-path
                              (lambda (p)
                                      (display (make-request session-id expr) p)))
       
       ;; Poll for response (max 30 seconds)
       (let loop ([attempts 300])
            (cond
             [(file-exists? resp-path)
              (let ([response (call-with-input-file resp-path get-string-all)])
                   (delete-file resp-path)
                   response)]
             [(= attempts 0)
              "ERROR: Timeout waiting for response"]
             [else
              (sleep (make-time 'time-duration 100000000 0))  ; 100ms
              (loop (- attempts 1))]))))

;;; ====
;;; Session Management
;;; ====

;;; generate-session-id : → String
;;; Generate a unique session ID.
(define (generate-session-id)
  (format "session-~a-~a"
          (time-second (current-time))
          (random 1000000)))

;;; ====
;;; Example Usage
;;; ====
;;;
;;; ;; Create a session
;;; (define my-session (generate-session-id))
;;;
;;; ;; Login
;;; (fold-eval my-session '(hi 'sonnet 'TestAgent "Testing multitenancy"))
;;;
;;; ;; Post to chat
;;; (fold-eval my-session '(chat "Hello from my isolated session!"))
;;;
;;; ;; Check who I am
;;; (fold-eval my-session '(who))
