;;; session-debug.ss — Debug and fix session persistence issues

(display "🔍 Debugging Session Management...\n\n")

;; Test session state across multiple calls
(display "1. Testing session state persistence...\n")

;; First, check if we have a session
(define (test-session-state)
  (display "Current session check:\n")
  (if (session-exists?)
      (let ((session (read-session)))
        (display (format "  Session exists: ✅\n"))
        (display (format "  Name: ~a\n" (cdr (assq 'name session))))
        (display (format "  Tier: ~a\n" (cdr (assq 'tier session))))
        (display (format "  Login time: ~a\n" (cdr (assq 'login-time session)))))
      (display "  Session exists: ❌\n")))

(test-session-state)

;; Test who command specifically
(display "\n2. Testing (who) command output...\n")
(display "Executing (who):\n")
(who)

;; Test multiple who calls
(display "\nExecuting (who) again to test persistence:\n")
(who)

;; Check session after who calls
(display "\n3. Session state after (who) calls...\n")
(test-session-state)

;; Test the actual session data structure
(display "\n4. Raw session data inspection...\n")
(when (session-exists?)
  (let ((session (read-session)))
    (display "Raw session alist:\n")
    (for-each 
     (lambda (pair)
       (display (format "  ~a: ~a\n" (car pair) (cdr pair))))
     session)))

;; Test session clearing and re-establishment
(display "\n5. Testing session robustness...\n")
(display "Session should remain stable across multiple operations.\n")

;; Test with a simple command that should preserve session
(display "\n6. Testing session with simple command...\n")
(display "Executing (digest-posts 1)...\n")
(digest-posts 1)

(display "\nSession state after digest-posts:\n")
(test-session-state)

(display "\n🔍 Session debug complete!\n")