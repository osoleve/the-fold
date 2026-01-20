(doc 'module 'fold-client)
(doc 'description "Multi-Session Client for The Fold REPL - provides functions for Claude agents to interact with the REPL daemon")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "For documentation and testing. Claude agents should use Write/Read tools directly")

(doc 'section 'client-configuration)

(define *repl-dir* ".fold-repl")
(define *requests-dir* ".fold-repl/requests")
(define *responses-dir* ".fold-repl/responses")

(doc 'section 'request-response-helpers)

(doc make-request 'type "String Scheme → String")
(doc make-request 'description "Create a request S-expression string for the given session and expression")
(define (make-request session-id expr)
  (format "~s"
          `((session-id . ,session-id)
            (expression . ,expr)
            (timestamp . ,(time-second (current-time))))))

(doc request-path 'type "String → String")
(doc request-path 'description "Get the file path for a session's request")
(define (request-path session-id)
  (string-append *requests-dir* "/" session-id ".ss"))

(doc response-path 'type "String → String")
(doc response-path 'description "Get the file path for a session's response")
(define (response-path session-id)
  (string-append *responses-dir* "/" session-id ".txt"))

(doc 'section 'synchronous-client-for-testing)

(doc fold-eval 'type "String Scheme → String")
(doc fold-eval 'description "Send a request to the daemon and wait for response")
(doc fold-eval 'note "This is for testing only. Claude agents should use Write/Read tools directly for better control")
(define (fold-eval session-id expr)
  (let ([req-path (request-path session-id)]
        [resp-path (response-path session-id)])

       (when (file-exists? resp-path)
             (delete-file resp-path))

       (call-with-output-file req-path
                              (lambda (p)
                                      (display (make-request session-id expr) p)))

       (let loop ([attempts 300])
            (cond
             [(file-exists? resp-path)
              (let ([response (call-with-input-file resp-path get-string-all)])
                   (delete-file resp-path)
                   response)]
             [(= attempts 0)
              "ERROR: Timeout waiting for response"]
             [else
              (sleep (make-time 'time-duration 100000000 0))
              (loop (- attempts 1))]))))

(doc 'section 'session-management)

(doc generate-session-id 'type "→ String")
(doc generate-session-id 'description "Generate a unique session ID")
(define (generate-session-id)
  (format "session-~a-~a"
          (time-second (current-time))
          (random 1000000)))

(doc 'section 'example-usage)
(doc 'example "
;; Create a session
(define my-session (generate-session-id))

;; Login (use MCP fold_login tool, or session-login! directly)
(fold-eval my-session '(session-login! my-session 'sonnet 'TestAgent))

;; Post to chat
(fold-eval my-session '(chat \"Hello from my isolated session!\"))

;; Check who I am
(fold-eval my-session '(who))
")
