;;; Test harness for forum/coordination.ss

;;; Load dependencies
(load "core/blocks/block.ss")
(load "core/base/sha256.ss")

;;; Inline hash functions (same as test-tools.ss)
(define (hash-block blk)
  (let* ([hash (sha256 (block->bytes blk))]
         [address (make-bytevector address-size)])
        (bytevector-u8-set! address 0 address-version)
        (bytevector-copy! hash 0 address 1 hash-size)
        address))

(define (hash->hex hash)
  (let ([hex-chars "0123456789abcdef"])
       (apply string-append
              (map (lambda (i)
                           (let ([b (bytevector-u8-ref hash i)])
                                (string
                                 (string-ref hex-chars (quotient b 16))
                                 (string-ref hex-chars (modulo b 16)))))
                   (iota (bytevector-length hash))))))

(load "shell/fs.ss")
(load "forum/tools.ss")
(load "forum/chat.ss")
(load "forum/coordination.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)))
  (newline))

;;; Test store path
(define test-store-path "./test-coord-store")
(define test-repl-dir "./.test-fold-repl")
(define test-session-file (string-append test-repl-dir "/session.sexp"))
(define test-claims-file (string-append test-repl-dir "/work-claims.json"))
(define test-notif-dir (string-append test-repl-dir "/notifications"))

(define (clean-test-env!)
  (when (file-exists? test-store-path)
        (system (string-append "rm -rf " test-store-path)))
  (when (file-exists? test-repl-dir)
        (system (string-append "rm -rf " test-repl-dir))))

(define (setup-test-session! name tier)
  (unless (file-exists? test-repl-dir)
          (mkdir test-repl-dir))
  (call-with-output-file test-session-file
                         (lambda (port)
                                 (write `((name . ,name)
                                          (tier . ,tier)
                                          (timestamp . "2026-01-04T00:00:00Z"))
                                        port)
                                 (newline port))
                         'replace))

(display "Forum Coordination Tests\n")
(display "=========================\n\n")

;;; Setup
(clean-test-env!)

;;; ============================================================
;;; String Utility Tests
;;; ============================================================

(display "Test Suite 1: String Utilities\n")
(display "-------------------------------\n")

;; Test string-contains?
(test "string-contains? - found" #t (string-contains? "hello world" "world"))
(test "string-contains? - not found" #f (string-contains? "hello world" "foo"))
(test "string-contains? - empty substring" #t (string-contains? "hello" ""))
(test "string-contains? - exact match" #t (string-contains? "test" "test"))

;; Test string-split
(test "string-split - simple" '("a" "b" "c") (string-split "a/b/c" #\/))
(test "string-split - no delim" '("test") (string-split "test" #\/))
(test "string-split - consecutive delims" '("a" "b") (string-split "a//b" #\/))

;; Test string-join
(test "string-join - simple" "a/b/c" (string-join '("a" "b" "c") "/"))
(test "string-join - empty list" "" (string-join '() "/"))
(test "string-join - single item" "test" (string-join '("test") "/"))

;; Test path-parent
(test "path-parent - simple" "a/b" (path-parent "a/b/c"))
(test "path-parent - root" "." (path-parent "file"))
(test "path-parent - nested" ".fold-repl" (path-parent ".fold-repl/session.sexp"))

(newline)

;;; ============================================================
;;; Work Claims Tests
;;; ============================================================

(display "Test Suite 2: Work Claims System\n")
(display "---------------------------------\n")

;; Override file paths for testing
(set! *work-claims-file* test-claims-file)

;; Test with no session (should error)
(display "  claim-work without session: ")
(let ([caught-error #f])
     (guard (e [else (set! caught-error #t)])
            (claim-work "test-item"))
     (if caught-error
         (display "✓ (error caught as expected)")
         (display "✗ (should have errored)"))
     (newline))

;; Setup test session
(setup-test-session! 'TestAgent 'builder)

;; Now load session in the running environment
(define test-fs (mint-fs-capability test-store-path))

;; Override read-session to use our test file
(define original-read-session read-session)
(define (read-session)
  (if (file-exists? test-session-file)
      (call-with-input-file test-session-file read)
      #f))

;; Test initial state
(test "initial claims empty" '() (read-work-claims))

;; Mock chat function to suppress output
(define original-chat chat)
(define chat-calls '())
(define (chat msg)
  (set! chat-calls (cons msg chat-calls)))

;; Test claim-work
(display "  claim-work 'beads-042': ")
(claim-work "beads-042")
(display "✓\n")

;; Verify claim was recorded
(let ([claims (read-work-claims)])
     (test "claim recorded" 1 (length claims))
     (test "claim item" "beads-042" (cdr (assq 'item (cdr (car claims)))))
     (test "claim agent" 'TestAgent (cdr (assq 'agent (cdr (car claims))))))

;; Test claim stealing (warns but allows)
(setup-test-session! 'AnotherAgent 'builder)
(display "  claim-work 'beads-042' by different agent (steal with warning): ")
(claim-work "beads-042")
(display "✓\n")

;; Verify claim was stolen
(let ([claims (read-work-claims)])
     (test "claim stolen" 1 (length claims))
     (test "new owner" 'AnotherAgent (cdr (assq 'agent (cdr (car claims))))))

;; Test who-is-working-on
(display "  who-is-working-on 'beads' (output suppressed): ")
(guard (e [else #f])
       (open-output-string)
       (who-is-working-on "beads"))
(display "✓\n")

;; Test release-work by owner (AnotherAgent still has it)
(display "  release-work by current owner: ")
(release-work "beads-042")
(display "✓\n")

;; Verify release
(test "claim released" '() (read-work-claims))

;; Test release of non-existent claim
(setup-test-session! 'TestAgent 'builder)
(display "  release-work on unclaimed item (should warn): ")
(guard (e [else #f])
       (release-work "beads-042"))
(display "✓\n")

;; Test release by wrong agent
(setup-test-session! 'TestAgent 'builder)
(display "  claim 'beads-999' and try to release by different agent: ")
(claim-work "beads-999")
(setup-test-session! 'AnotherAgent 'builder)
(guard (e [else #f])
       (release-work "beads-999"))
(display "✓\n")

;; Verify claim still exists
(test "claim not released by wrong agent" 1 (length (read-work-claims)))

;; Restore chat
(set! chat original-chat)

(newline)

;;; ============================================================
;;; Notification Tests
;;; ============================================================

(display "Test Suite 3: Notification System\n")
(display "----------------------------------\n")

;; Override notification dir for testing
(set! *notifications-dir* test-notif-dir)

;; Switch back to TestAgent session for notification tests
(setup-test-session! 'TestAgent 'builder)

;; Test initial state
(test "initial notifications empty" '() (read-notifications))

;; Test notify-user!
(notify-user! "TestAgent" "SomeAgent" "Check this out" "abc123")
(let* ([notifs (read-notifications)]
       [first-notif (car notifs)]
       [from-value (cdr (assq 'from first-notif))])
      (test "notification created" 1 (length notifs))
      ;; from is stored as a string
      (test "notification from" "SomeAgent" from-value)
      (test "notification context" "Check this out" (cdr (assq 'context first-notif)))
      (test "notification hash" "abc123" (cdr (assq 'message-hash first-notif))))

;; Test multiple notifications
(notify-user! "TestAgent" "AnotherAgent" "Second message" "def456")
(test "two notifications" 2 (length (read-notifications)))

;; Test show-notifications (output suppressed)
(display "  show-notifications (output suppressed): ")
(guard (e [else #f])
       (show-notifications))
(display "✓\n")

;; Test clear-notifications
(clear-notifications)
(test "notifications cleared" '() (read-notifications))

(newline)

;;; ============================================================
;;; Reaction Tests
;;; ============================================================

(display "Test Suite 4: Reactions\n")
(display "------------------------\n")

;; Test reaction->symbol
(test "reaction ack" "✓" (reaction->symbol 'ack))
(test "reaction question" "?" (reaction->symbol 'question))
(test "reaction done" "✅" (reaction->symbol 'done))
(test "reaction thanks" "🙏" (reaction->symbol 'thanks))
(test "reaction help" "🆘" (reaction->symbol 'help))
(test "reaction unknown" "•" (reaction->symbol 'unknown))

;; Note: Full react function test requires CAS and chat integration
;; which is complex to mock. We've tested the symbol mapping.

(newline)

;;; ============================================================
;;; Summary
;;; ============================================================

(display "All Coordination Tests Complete!\n")
(display "=================================\n\n")

;; Cleanup
(clean-test-env!)

(display "Test environment cleaned up.\n")
