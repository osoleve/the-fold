#!/usr/bin/env scheme --script
;;; agents/llm-agent-poll.ss — Agent Response with Real LLM Integration
;;;
;;; Processes triggers and calls actual LLM APIs for responses.
;;; Includes depth-based loop prevention.

(define *trigger-dir* ".fold-repl/triggers")
(define *agents* '(opus pedagogue archivist sonnet haiku))
;;; Per-agent state files to avoid race conditions between concurrent agents
(define (state-file-for agent)
  (format ".fold-repl/bot-message-count-~a.txt" agent))
(define *max-depth* 3)

;;; Load bot message count for specific agent
(define (load-count agent)
  (let ([state-file (state-file-for agent)])
       (if (file-exists? state-file)
           (guard (e [else 0])  ; Handle read errors gracefully
                  (call-with-input-file state-file read))
           0)))

;;; Save bot message count atomically (write to temp, rename)
(define (save-count! agent n)
  (let* ([state-file (state-file-for agent)]
         [tmp-file (format "~a.tmp.~a" state-file (current-milliseconds))])
        ;; Write to temp file first
        (call-with-output-file tmp-file
                               (lambda (port)
                                       (write n port)))
        ;; Atomic rename
        (when (file-exists? state-file)
              (delete-file state-file))
        (rename-file tmp-file state-file)))

;;; Check for and process triggers
(define (poll-triggers)
  (for-each check-agent *agents*))

(define (check-agent agent)
  ;; Check both fold-trigger and discord-trigger files
  (let ([fold-trigger (format "~a/~a-fold-trigger.ss" *trigger-dir* agent)]
        [discord-trigger (format "~a/~a-discord-trigger.ss" *trigger-dir* agent)])
       (when (file-exists? fold-trigger)
             (display (format "📬 Fold trigger for ~a\n" agent))
             (process-trigger agent fold-trigger))
       (when (file-exists? discord-trigger)
             (display (format "📬 Discord trigger for ~a\n" agent))
             (process-trigger agent discord-trigger))))

(define (process-trigger agent trigger-file)
  (let ([trigger-data (call-with-input-file trigger-file read)])
       (display (format "   From: ~a\n" (cdr (assq 'author trigger-data))))
       (display (format "   Body: ~a\n" (cdr (assq 'body trigger-data))))
       
       ;; Check if author is a bot
       (let* ([author-val (cdr (assq 'author trigger-data))]
              [author (if (symbol? author-val) (symbol->string author-val) author-val)]
              [author-is-agent (member (string->symbol author) *agents*)]
              [count (load-count agent)])  ; Per-agent count
             
             (cond
              ;; From human - reset counter and respond
              [(not author-is-agent)
               (display "   ✓ From human, responding\n")
               (create-llm-response agent trigger-data trigger-file)
               (save-count! agent 1)]
              
              ;; From bot but under limit - increment and respond
              [(< count *max-depth*)
               (display (format "   ✓ From bot, depth ~a/~a, responding\n" count *max-depth*))
               (create-llm-response agent trigger-data trigger-file)
               (save-count! agent (+ count 1))]
              
              ;; Over limit - skip
              [else
               (display (format "   ✗ Max depth reached (~a/~a), skipping\n" count *max-depth*))
               (delete-file trigger-file)
               (save-count! agent 0)]))))  ; Reset for next conversation

(define (create-llm-response agent trigger-data trigger-file)
  (display "   🤖 Calling LLM API...\n")
  
  ;; Call the Node.js LLM invoker (stderr goes to /dev/null to avoid polluting response)
  (let* ([invoke-script (format "agents/invoke-~a.js" agent)]
         [cmd (format "node ~a ~a 2>/dev/null" invoke-script trigger-file)]
         [response (shell-command-to-string cmd)])
        
        (if (string-prefix? "❌" response)
            ;; API call failed
            (begin
             (display (format "   ✗ LLM call failed: ~a\n" response))
             (delete-file trigger-file))
            
            ;; API call succeeded
            (let* ([timestamp (current-milliseconds)]
                   [chat-file (format "forum/chat/~a-~a.sexp" timestamp agent)])
                  
                  ;; Write chat post
                  (call-with-output-file chat-file
                                         (lambda (port)
                                                 (write `((author . ,agent)
                                                          (tier . shepherd)
                                                          (channel . chat)
                                                          (body . ,response)
                                                          (timestamp . ,(get-timestamp))
                                                          (in-reply-to . ,(cdr (assq 'session-id trigger-data))))
                                                        port)))
                  
                  (display (format "   ✅ Posted to chat: ~a\n" chat-file))
                  
                  ;; Create Discord outbox
                  (let ([discord-outbox-file (format ".fold-repl/discord-outbox/~a-~a.json" timestamp agent)])
                       (call-with-output-file discord-outbox-file
                                              (lambda (port)
                                                      (display "{\n" port)
                                                      (display (format "  \"channel\": \"chat\",\n") port)
                                                      ;; Escape quotes in response for JSON
                                                      (display (format "  \"body\": \"~a\",\n" (escape-json response)) port)
                                                      (display (format "  \"author\": \"~a\",\n" agent) port)
                                                      (display (format "  \"tier\": \"shepherd\"\n") port)
                                                      (display "}\n" port)))
                       (display (format "   ✅ Discord outbox: ~a\n" discord-outbox-file)))
                  
                  ;; Delete trigger
                  (delete-file trigger-file)
                  (display "   🗑️  Trigger processed\n")))))

;;; Shell command helper (unique temp files to avoid collisions)
(define (shell-command-to-string cmd)
  (let* ([tmp-file (format "/tmp/fold-llm-~a-~a.txt"
                           (current-milliseconds)
                           (random 100000))]  ; Add randomness for concurrency
         [full-cmd (format "~a > ~a" cmd tmp-file)])
        (system full-cmd)
        (let ([result (call-with-input-file tmp-file
                                            (lambda (port)
                                                    (let loop ([lines '()])
                                                         (let ([line (get-line port)])
                                                              (if (eof-object? line)
                                                                  (string-join (reverse lines) "\n")
                                                                  (loop (cons line lines)))))))])
             (delete-file tmp-file)
             result)))

;;; Helper to join strings
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([result (car strs)]
                 [rest (cdr strs)])
           (if (null? rest)
               result
               (loop (string-append result sep (car rest))
                     (cdr rest))))))

;;; Check if string starts with prefix
(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; Escape special characters for JSON (RFC 8259 compliant)
(define (escape-json str)
  (let loop ([i 0]
             [result ""])
       (if (>= i (string-length str))
           result
           (let ([ch (string-ref str i)])
                (loop (+ i 1)
                      (string-append result
                                     (cond
                                      [(char=? ch #\") "\\\""]
                                      [(char=? ch #\\) "\\\\"]
                                      [(char=? ch #\newline) "\\n"]
                                      [(char=? ch #\return) "\\r"]
                                      [(char=? ch #\tab) "\\t"]
                                      [(char=? ch #\backspace) "\\b"]
                                      [(char=? ch #\page) "\\f"]
                                      ;; Control chars (U+0000 to U+001F) as \uXXXX
                                      [(< (char->integer ch) 32)
                                       (format "\\u~4,'0x" (char->integer ch))]
                                      [else (string ch)])))))))

(define (current-milliseconds)
  (let ([now (current-time)])
       (+ (* (time-second now) 1000)
          (quotient (time-nanosecond now) 1000000))))

(define (get-timestamp)
  (format "~a" (time-second (current-time))))

;;; Run the poll
(poll-triggers)
