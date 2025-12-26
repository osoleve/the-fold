;;; shell/commands-example.ss — Examples of Custom Commands
;;;
;;; This file shows how to extend the command system with custom commands.
;;; Load this file after repl.ss to add example commands.
;;;
;;; Usage:
;;;   (load "shell/repl.ss")
;;;   (load "shell/commands-example.ss")

;;; ============================================================
;;; Example 1: Simple Info Command
;;; ============================================================

(register-command!
 'system-info
 "Show system information"
 "Display basic system information including Scheme version and platform.\n  Usage: (cmd 'system-info)\n         (system-info)"
 (lambda ()
   (display "System Information:\n")
   (display (format "  Scheme: ~a\n" (scheme-version)))
   (display (format "  The Fold: ~a\n" *fold-version*))
   (display (format "  Platform: ~a\n" (machine-type)))
   (void)))

;; Convenience wrapper
(define (system-info)
  (let ([result (cmd 'system-info)])
    (if (eq? (car result) 'error)
        (display (caddr result))
        (cadr result))))

;;; ============================================================
;;; Example 2: Command with Arguments
;;; ============================================================

(register-command!
 'count-words
 "Count words in text"
 "Count the number of words in the given text.\n  Usage: (cmd 'count-words \"some text\")\n         (count-words \"some text\")"
 (lambda (text)
   (let* ([words (string-split text #\space)]
          [count (length (filter (lambda (w) (> (string-length w) 0)) words))])
     (display (format "Word count: ~a\n" count))
     count)))

;; Convenience wrapper
(define (count-words text)
  (let ([result (cmd 'count-words text)])
    (if (eq? (car result) 'ok)
        (cadr result)
        (error 'count-words (caddr result)))))

;;; ============================================================
;;; Example 3: Command that Uses The Fold Features
;;; ============================================================

(register-command!
 'quick-post
 "Quick post to a channel"
 "Post to a forum channel with automatic timestamp.\n  Usage: (cmd 'quick-post 'channel \"message\")\n         (quick-post 'channel \"message\")\n  Requires active session."
 (lambda (channel message)
   (let ([session (read-session)])
     (unless session
       (error 'quick-post "No active session. Use (hi tier name txt) first."))
     (let ([result (msg channel "Quick Post" message)])
       (display "Posted!\n")
       result))))

;; Convenience wrapper
(define (quick-post channel message)
  (let ([result (cmd 'quick-post channel message)])
    (if (eq? (car result) 'ok)
        (cadr result)
        (error 'quick-post (caddr result)))))

;;; ============================================================
;;; Example 4: Command with Validation
;;; ============================================================

(register-command!
 'validate-hash
 "Validate a hash string"
 "Check if a string is a valid SHA-256 hash (64 hex characters).\n  Usage: (cmd 'validate-hash \"abc123...\")\n         (validate-hash \"abc123...\")"
 (lambda (hash-str)
   (define (hex-char? c)
     (or (char<=? #\0 c #\9)
         (char<=? #\a c #\f)
         (char<=? #\A c #\F)))

   (let ([len (string-length hash-str)]
         [valid? (and (= (string-length hash-str) 64)
                     (string-every hex-char? hash-str))])
     (if valid?
         (begin
           (display "✓ Valid SHA-256 hash\n")
           #t)
         (begin
           (display "✗ Invalid hash (must be 64 hex characters)\n")
           #f)))))

;; Convenience wrapper
(define (validate-hash hash-str)
  (let ([result (cmd 'validate-hash hash-str)])
    (if (eq? (car result) 'ok)
        (cadr result)
        (error 'validate-hash (caddr result)))))

;;; ============================================================
;;; Example 5: Command with Error Handling
;;; ============================================================

(register-command!
 'safe-divide
 "Safely divide two numbers"
 "Divide two numbers with error handling for division by zero.\n  Usage: (cmd 'safe-divide a b)\n         (safe-divide a b)"
 (lambda (a b)
   (if (= b 0)
       (error 'safe-divide "Division by zero")
       (let ([result (/ a b)])
         (display (format "~a / ~a = ~a\n" a b result))
         result))))

;; Convenience wrapper
(define (safe-divide a b)
  (let ([result (cmd 'safe-divide a b)])
    (if (eq? (car result) 'ok)
        (cadr result)
        (begin
          (display (format "Error: ~a\n" (caddr result)))
          #f))))

;;; ============================================================
;;; Notify User
;;; ============================================================

(display "Example commands loaded. New commands available:\n")
(display "  - system-info     : Show system information\n")
(display "  - count-words     : Count words in text\n")
(display "  - quick-post      : Quick post to a channel\n")
(display "  - validate-hash   : Validate a hash string\n")
(display "  - safe-divide     : Safely divide two numbers\n")
(display "\nUse (commands) to see all registered commands.\n")
