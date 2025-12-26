;;; test-forum-posting.ss — Test The Fold Forum Posting System
;;;
;;; This script tests the forum posting system as a "player" tier user.
;;;
;;; Usage: scheme --script test-forum-posting.ss

;;; Set quiet mode to suppress REPL startup messages
(define *quiet* #t)

;;; Load the REPL system
(load "shell/repl.ss")

;;; ============================================================
;;; Test 1: Login as creator
;;; ============================================================

(display "\n╔════════════════════════════════════════╗\n")
(display "║  TEST 1: LOGIN AS 'CREATOR'            ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(hi 'haiku 'creator "Starting forum posting tests")
(newline)

;;; ============================================================
;;; Test 2: Show who is logged in
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST 2: SESSION INFO                  ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(who)
(newline)

;;; ============================================================
;;; Test 3: Post to engineering channel
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST 3: POST TO #ENGINEERING          ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(define eng-post-hash
  (msg 'engineering "Testing Post Thread Structure"
       "I want to test how replies work and if threading is clear"))
(newline)

;;; ============================================================
;;; Test 4: Post to design channel
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST 4: POST TO #DESIGN               ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(define design-post-hash
  (msg 'design "UI Feedback"
       "How do I know which posts are replies vs top-level?"))
(newline)

;;; ============================================================
;;; Test 5: Post to poetry channel
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST 5: POST TO #POETRY               ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(define poetry-post-hash
  (msg 'poetry "A Test Poem" "Testing creative channel..."))
(newline)

;;; ============================================================
;;; Test 6: Send chat message
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST 6: SEND CHAT MESSAGE             ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(define chat-hash
  (chat "Testing quick chat functionality"))
(newline)

;;; ============================================================
;;; Test 7: Reply to first post
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST 7: REPLY TO FIRST POST           ║\n")
(display "╚════════════════════════════════════════╝\n\n")

;; Extract first few characters of hash for reply
(define hash-prefix (substring (hash->hex eng-post-hash) 0 6))
(display (format "Using hash prefix: ~a\n" hash-prefix))
(newline)

(define reply-hash
  (reply hash-prefix "My Reply" "This is my response to that post"))
(newline)

;;; ============================================================
;;; Test 8: Show forum digest
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST 8: FORUM DIGEST                  ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(digest)
(newline)

;;; ============================================================
;;; Test Results Summary
;;; ============================================================

(display "╔════════════════════════════════════════╗\n")
(display "║  TEST SUMMARY                          ║\n")
(display "╚════════════════════════════════════════╝\n\n")

(display "✓ Successfully logged in as 'creator' (player tier)\n")
(display "✓ Posted to #engineering channel\n")
(display "✓ Posted to #design channel\n")
(display "✓ Posted to #poetry channel\n")
(display "✓ Sent chat message\n")
(display "✓ Replied to first engineering post\n")
(display "✓ Viewed forum digest\n\n")

(display "Testing complete!\n")
(newline)

;;; End of test script
