#!/usr/bin/env scheme --script
;;; test-bluegown-tags-opus.ss — End-to-end test: bluegown tags @opus
;;;
;;; Simulates bluegown posting to chat with @opus tag.
;;; Agent polling daemon should detect and respond.

(display "🌿 Bluegown → @Opus Test\n\n")

;; Step 1: Bluegown posts to chat (simulated as trigger creation)
(display "1. Bluegown posting to chat...\n")
(define bluegown-message "Hey @opus, what are the core principles of The Fold?")
(display (format "   Message: ~a\n" bluegown-message))

;; Step 2: Post bluegown's message to chat and Discord
(display "\n2. Posting bluegown's message to chat...\n")
(let* ([timestamp (exact (floor (* 1000 (time-second (current-time)))))]
       [timestamp-str (number->string timestamp)]
       [session-id (string-append "fold-chat-" timestamp-str)]
       
       ;; Create Fold chat post
       [chat-file (format "forum/chat/~a-bluegown.sexp" timestamp-str)]
       
       ;; Create Discord outbox entry
       [discord-outbox (format ".fold-repl/discord-outbox/~a-bluegown.json" timestamp-str)])
      
      ;; Write to Fold chat
      (call-with-output-file chat-file
                             (lambda (port)
                                     (write `((author . bluegown)
                                              (tier . player)
                                              (channel . chat)
                                              (body . ,bluegown-message)
                                              (timestamp . ,(number->string (time-second (current-time))))
                                              (session-id . ,session-id))
                                            port)))
      (display (format "   ✅ Fold chat: ~a\n" chat-file))
      
      ;; Write to Discord outbox so message appears in Discord
      (call-with-output-file discord-outbox
                             (lambda (port)
                                     (display "{\n" port)
                                     (display "  \"channel\": \"chat\",\n" port)
                                     (display (format "  \"body\": \"~a\",\n" bluegown-message) port)
                                     (display "  \"author\": \"bluegown\",\n" port)
                                     (display "  \"tier\": \"player\"\n" port)
                                     (display "}\n" port)))
      (display (format "   ✅ Discord outbox: ~a\n" discord-outbox))
      
      ;; Step 3: Create trigger file (normally done by fold-chat-poll.ss)
      (display "\n3. Creating trigger for opus...\n")
      (let ([trigger-file ".fold-repl/requests/opus-fold-trigger.ss"])
           (call-with-output-file trigger-file
                                  (lambda (port)
                                          (write `((session-id . ,session-id)
                                                   (agent . opus)
                                                   (channel . chat)
                                                   (author . "bluegown")
                                                   (body . ,bluegown-message))
                                                 port)))
           (display (format "   ✅ Trigger: ~a\n" trigger-file))))

(display "\n4. Waiting for opus to respond...\n")
(display "   The fold-agent-poll daemon should:\n")
(display "   - Detect the trigger file\n")
(display "   - Create a response (without @ tags to prevent loops)\n")
(display "   - Post to Discord outbox\n")
(display "\n📊 To verify in Discord:\n")
(display "   - Bluegown's message: \"Hey @opus, what are the core principles of The Fold?\"\n")
(display "   - Opus's response (without @ tags)\n")
(display "\n📝 Check logs:\n")
(display "   - tail -f logs/fold-agent-poll.log\n")
(display "   - tail -f /tmp/discord-bot.log\n")

(display "\n✅ Test setup complete!\n")
(display "   Start the daemons with:\n")
(display "     ./scripts/fold-agent-poll-daemon.sh &\n")
