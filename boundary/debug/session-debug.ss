(doc 'module 'session-debug)
(doc 'description "Debug and fix session persistence issues")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Diagnostic script for session state testing")

(display "🔍 Debugging Session Management...\n\n")

(display "1. Testing session state persistence...\n")

(define (test-session-state)
  (doc 'type '(-> void))
  (doc 'description "Check current session state")
  (display "Current session check:\n")
  (if (session-exists?)
      (let ((session (read-session)))
           (display (format "  Session exists: ✅\n"))
           (display (format "  Name: ~a\n" (cdr (assq 'name session))))
           (display (format "  Tier: ~a\n" (cdr (assq 'tier session))))
           (display (format "  Login time: ~a\n" (cdr (assq 'login-time session)))))
      (display "  Session exists: ❌\n")))

(test-session-state)

(display "\n2. Testing (who) command output...\n")
(display "Executing (who):\n")
(who)

(display "\nExecuting (who) again to test persistence:\n")
(who)

(display "\n3. Session state after (who) calls...\n")
(test-session-state)

(display "\n4. Raw session data inspection...\n")
(when (session-exists?)
      (let ((session (read-session)))
           (display "Raw session alist:\n")
           (for-each
            (lambda (pair)
                    (display (format "  ~a: ~a\n" (car pair) (cdr pair))))
            session)))

(display "\n5. Testing session robustness...\n")
(display "Session should remain stable across multiple operations.\n")

(display "\n6. Testing session with simple command...\n")
(display "Executing (digest-posts 1)...\n")
(digest-posts 1)

(display "\nSession state after digest-posts:\n")
(test-session-state)

(display "\n🔍 Session debug complete!\n")
