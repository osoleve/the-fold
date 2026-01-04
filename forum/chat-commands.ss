;;; forum/chat-commands.ss — Chat Command Parser and Dispatcher
;;;
;;; Provides /command support in chat messages.
;;; Commands trigger actions directly from chat.
;;;
;;; This is Shell code: uses IO, dispatches to other modules.
;;;
;;; Dependencies (must be loaded before this file):
;;;   forum/chat.ss
;;;   forum/coordination.ss

;;; ============================================================
;;; Command Parser
;;; ============================================================

;;; is-command? : String → Boolean
;;; Check if a chat message is a command (starts with /).
(define (is-command? text)
  (and (string? text)
       (> (string-length text) 0)
       (char=? (string-ref text 0) #\/)))

;;; parse-command : String → (Symbol . List)
;;; Parse command string into (command-name . args).
;;; Example: "/claim beads-042" → ('claim . ("beads-042"))
(define (parse-command text)
  (let ([parts (string-split text #\space)])
       (if (null? parts)
           (cons 'unknown '())
           (let ([cmd-str (car parts)])
                (if (and (> (string-length cmd-str) 1)
                         (char=? (string-ref cmd-str 0) #\/))
                    (cons (string->symbol (substring cmd-str 1 (string-length cmd-str)))
                          (cdr parts))
                    (cons 'unknown parts))))))

;;; ============================================================
;;; Command Dispatcher
;;; ============================================================

;;; execute-chat-command : String → Any
;;; Execute a chat command and return result.
;;; If not a command, returns #f.
(define (execute-chat-command text)
  (if (not (is-command? text))
      #f
      (let* ([parsed (parse-command text)]
             [cmd (car parsed)]
             [args (cdr parsed)])
            (case cmd
                  [(claim)
                   (if (null? args)
                       (display "Usage: /claim <item-id>\nExample: /claim beads-042\n")
                       (claim-work (car args)))]
                  [(release)
                   (if (null? args)
                       (display "Usage: /release <item-id>\nExample: /release beads-042\n")
                       (release-work (car args)))]
                  [(ready)
                   (system "bd ready")]
                  [(list)
                   (if (null? args)
                       (system "bd list --status=open")
                       (system (string-append "bd list --status=" (car args))))]
                  [(show)
                   (if (null? args)
                       (display "Usage: /show <item-id>\nExample: /show beads-042\n")
                       (system (string-append "bd show " (car args))))]
                  [(assign)
                   (if (< (length args) 2)
                       (display "Usage: /assign <item-id> <agent>\nExample: /assign beads-042 Echo\n")
                       (system (string-append "bd update " (car args) " --assignee=" (cadr args))))]
                  [(close)
                   (if (null? args)
                       (display "Usage: /close <item-id>\nExample: /close beads-042\n")
                       (system (string-append "bd close " (car args))))]
                  [(claims)
                   (show-all-claims)]
                  [(notifications notifs)
                   (show-notifications)]
                  [(clear-notifs)
                   (clear-notifications)]
                  [(help commands)
                   (display-chat-commands)]
                  [else
                   (display (format "Unknown command: /~a\nUse /help for available commands.\n" cmd))]))))

;;; display-chat-commands : → void
;;; Display available chat commands.
(define (display-chat-commands)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                     CHAT COMMANDS                            ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n")
  (display "\n")
  (display "Work Management:\n")
  (display "  /claim <id>           Claim work on an item\n")
  (display "  /release <id>         Release a work claim\n")
  (display "  /claims               Show all active claims\n")
  (display "\n")
  (display "Issue Tracking:\n")
  (display "  /ready                Show ready-to-work issues\n")
  (display "  /list [status]        List issues (default: open)\n")
  (display "  /show <id>            Show issue details\n")
  (display "  /assign <id> <agent>  Assign issue to agent\n")
  (display "  /close <id>           Close an issue\n")
  (display "\n")
  (display "Notifications:\n")
  (display "  /notifications        Show your notifications\n")
  (display "  /clear-notifs         Clear all notifications\n")
  (display "\n")
  (display "  /help                 Show this help\n")
  (display "\n"))

;;; ============================================================
;;; Integration with Chat
;;; ============================================================

;;; chat-or-command : String → Any
;;; Send a chat message or execute a command.
;;; Automatically detects commands (starting with /).
;;;
;;; Example: (chat-or-command "Hello!")           ; sends chat
;;;          (chat-or-command "/claim beads-042") ; executes command
(define (chat-or-command text)
  (if (is-command? text)
      (execute-chat-command text)
      (chat text)))

;;; Enhanced chat wrapper that checks for commands
(define chat-original chat)

;;; Override chat to support commands
(set! chat
      (lambda (text)
              (if (is-command? text)
                  (execute-chat-command text)
                  (chat-original text))))
