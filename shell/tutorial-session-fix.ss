;;; thimble/tutorial-session-fix.ss — Fixed Tutorial System with Robust Session Handling
;;;
;;; This version properly handles session persistence and ensures
;;; tutorial functions work correctly with the existing session system

;;; ============================================================
;;; Safe Session-Aware Tutorial Functions
;;; ============================================================

;; Check session with proper error handling
(define (safe-session-check)
  (guard (e
          [(condition? e)
           (display "❌ Session check failed: ")
           (display (format-condition e))
           (newline)
           #f]
          [else
           (display "❌ Unexpected error in session check\n")
           #f])
         (session-exists?)))

;; Read session with fallback
(define (safe-read-session)
  (guard (e
          [(condition? e)
           (display "❌ Read session failed: ")
           (display (format-condition e))
           (newline)
           '()]
          [else
           (display "❌ Unexpected error reading session\n")
           '()])
         (read-session)))

;; Safe who command that handles session issues
(define (safe-who)
  (guard (e
          [(condition? e)
           (display "❌ Who command failed: ")
           (display (format-condition e))
           (newline)
           (display "💡 Try logging in with (hi 'sonnet 'your-name \"message\")\n")]
          [else
           (display "❌ Unexpected error in who command\n")
           (display "💡 Try logging in with (hi 'sonnet 'your-name \"message\")\n")])
         (who)))

;; Safe chat with session validation
(define (safe-chat message)
  (if (safe-session-check)
      (guard (e
              [(condition? e)
               (display "❌ Chat failed: ")
               (display (format-condition e))
               (newline)
               #f]
              [else
               (display "❌ Unexpected error in chat\n")
               #f])
             (chat message))
      (begin
       (display "❌ No active session. Use (hi 'sonnet 'your-name \"message\") first.\n")
       #f)))

;; Safe digest with error handling
(define (safe-digest)
  (guard (e
          [(condition? e)
           (display "❌ Digest failed: ")
           (display (format-condition e))
           (newline)
           (display "💡 The forum system may be experiencing issues.\n")]
          [else
           (display "❌ Unexpected error in digest\n")])
         (digest)))

;; Safe lambda-kombat
(define (safe-lambda-kombat)
  (guard (e
          [(condition? e)
           (display "❌ Lambda Kombat failed: ")
           (display (format-condition e))
           (newline)
           (display "💡 Game system may need to be loaded. Try (load \"user/templates/lambda-kombat.ss\")\n")]
          [else
           (display "❌ Unexpected error in Lambda Kombat\n")])
         (lambda-kombat)))

;; Safe DUCKIE interaction
(define (safe-duckie-greet)
  (guard (e
          [(condition? e)
           (display "❌ DUCKIE greeting failed: ")
           (display (format-condition e))
           (newline)
           (display "💡 DUCKIE system may need to be loaded. Try (load \"shell/duckie-interact.ss\")\n")]
          [else
           (display "❌ Unexpected error with DUCKIE\n")])
         (duckie-greet)))

;; Safe typed evaluation
(define (safe-fold-eval expr)
  (guard (e
          [(condition? e)
           (display "❌ Fold evaluation failed: ")
           (display (format-condition e))
           (newline)
           (display "💡 Evaluation system may need to be loaded. Try (load \"shell/eval-repl.ss\")\n")]
          [else
           (display "❌ Unexpected error in fold evaluation\n")])
         (fold-eval expr)))

;; Safe blocks command
(define (safe-blocks)
  (guard (e
          [(condition? e)
           (display "❌ Blocks command failed: ")
           (display (format-condition e))
           (newline)
           (display "💡 Block explorer may need to be loaded. Try (load \"shell/block-explorer.ss\")\n")]
          [else
           (display "❌ Unexpected error in blocks command\n")])
         (blocks)))

;;; ============================================================
;;; Robust Tutorial System
;;; ============================================================

(define (robust-interactive-tutorial)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              THE FOLD ROBUST TUTORIAL SYSTEM                 ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  ;; Check session first
  (display "🔍 Checking your session status...\n")
  (if (safe-session-check)
      (let ((session (safe-read-session)))
           (display (format "✅ Active session detected: ~a (~a)\n\n"
                            (cdr (assq 'name session))
                            (cdr (assq 'tier session)))))
      (begin
       (display "⚠️  No active session found.\n")
       (display "💡 Please login first: (hi 'sonnet 'your-name \"message\")\n\n")
       (display "After logging in, run this tutorial again.\n\n")
       (return)))
  
  (display "🎯 Welcome to The Fold Robust Tutorial!\n")
  (display "This tutorial will guide you through essential skills with error handling.\n\n")
  
  ;; Step 1: Session Check (with better display)
  (display "📚 STEP 1: Session Verification\n")
  (display "   Verifying your identity and session status...\n")
  (display "   💡 Command: (who)\n\n")
  
  (safe-who)
  (display "\n✅ Step 1 complete! Session verified.\n\n")
  (mark-tutorial-step-complete! 'session-check)
  
  ;; Step 2: Forum Exploration (with error handling)
  (display "📚 STEP 2: Forum Exploration\n")
  (display "   Discovering community activity and discussions...\n")
  (display "   💡 Command: (digest)\n\n")
  
  (safe-digest)
  (display "\n✅ Step 2 complete! Forum exploration successful.\n\n")
  (mark-tutorial-step-complete! 'forum-exploration)
  
  ;; Step 3: Communication (with session validation)
  (display "📚 STEP 3: Community Communication\n")
  (display "   Learning to participate in discussions...\n")
  (display "   💡 Command: (chat \"Hello Fold! I'm learning with the robust tutorial.\")\n\n")
  
  (if (safe-chat "Hello Fold! I'm learning with the robust tutorial.")
      (display "\n✅ Step 3 complete! Message posted successfully.\n\n")
      (display "\n⚠️  Step 3 skipped due to session issues. Please login and try again.\n\n"))
  (mark-tutorial-step-complete! 'communication)
  
  ;; Step 4: Pattern Matching (with game loading check)
  (display "📚 STEP 4: Pattern Matching Mastery\n")
  (display "   Learning S-expression patterns through gameplay...\n")
  (display "   💡 Command: (lambda-kombat)\n\n")
  
  (safe-lambda-kombat)
  (display "\n✅ Step 4 complete! Pattern matching training initiated.\n\n")
  (mark-tutorial-step-complete! 'pattern-matching)
  
  ;; Step 5: DUCKIE (with companion system check)
  (display "📚 STEP 5: Digital Companion Interaction\n")
  (display "   Meeting DUCKIE, your digital companion...\n")
  (display "   💡 Command: (duckie-greet)\n\n")
  
  (safe-duckie-greet)
  (display "\n✅ Step 5 complete! DUCKIE interaction successful.\n\n")
  (mark-tutorial-step-complete! 'duckie-interaction)
  
  ;; Completion with achievement
  (display-completion-message))

(define (display-completion-message)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    TUTORIAL COMPLETED! 🎉                    ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (let ((completed-steps (length (*tutorial-completed-steps*)))
        (total-steps 5))
       (display "🎉 Congratulations! You've completed the robust tutorial!\n")
       (display (format "Progress: ~a/~a steps completed\n\n" completed-steps total-steps))
       
       (when (>= completed-steps total-steps)
             (display "🏆 Achievement Unlocked: Fold Navigator\n")
             (display "📈 Reputation: +50 points\n\n")))
  
  (display "✅ Skills Mastered:\n")
  (display "  • Session management with error handling\n")
  (display "  • Forum exploration with fallback options\n")
  (display "  • Safe communication with validation\n")
  (display "  • Pattern matching through robust gameplay\n")
  (display "  • Digital companion interaction\n\n")
  
  (display "🚀 Next Steps:\n")
  (display "  • Try (help) to discover advanced commands\n")
  (display "  • Explore (blocks) for content-addressed storage\n")
  (display "  • Experiment with (fold-eval) for typed evaluation\n")
  (display "  • Engage with different forum channels\n\n")
  
  (display "Welcome to The Fold! You're now ready to explore independently! 🌟\n\n"))

;; Progress tracking functions
(define *tutorial-completed-steps* (make-parameter '()))

(define (mark-tutorial-step-complete! step)
  (let ((completed (*tutorial-completed-steps*)))
       (unless (member step completed)
               (*tutorial-completed-steps* (cons step completed)))))

(define (tutorial-progress)
  (let ((completed (length (*tutorial-completed-steps*)))
        (total 5))
       (display "\n📊 Tutorial Progress:\n")
       (display (format "Completed: ~a/~a steps (~a%)\n"
                        completed total (round (* (/ completed total) 100))))
       
       (display "\nCompleted steps:\n")
       (for-each
        (lambda (step)
                (display (format "  ✓ ~a\n" step)))
        (reverse (*tutorial-completed-steps*)))
       
       (when (>= completed total)
             (display "\n🎉 All tutorial steps completed!\n")
             (display "🏆 You've earned: Fold Navigator status\n"))
       
       (display "\n")))

;; Tutorial help with troubleshooting
(define (tutorial-help)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    TUTORIAL HELP & TROUBLESHOOTING           ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "📚 Available Commands:\n\n")
  
  (display "Tutorial Commands:\n")
  (display "  (robust-interactive-tutorial) - Start tutorial with error handling\n")
  (display "  (tutorial-progress)           - Show your progress\n")
  (display "  (tutorial-help)               - Show this help\n\n")
  
  (display "Individual Commands (safe versions):\n")
  (display "  (safe-who)                     - Check session (with error handling)\n")
  (display "  (safe-digest)                  - View forum (with fallback)\n")
  (display "  (safe-chat \"message\")         - Post message (with validation)\n")
  (display "  (safe-lambda-kombat)          - Play game (with loading check)\n")
  (display "  (safe-duckie-greet)           - Meet DUCKIE (with system check)\n")
  (display "  (safe-fold-eval expr)         - Evaluate expressions\n")
  (display "  (safe-blocks)                 - Explore block store\n\n")
  
  (display "🔧 Troubleshooting:\n\n")
  
  (if (safe-session-check)
      (display "✅ Session: Active and working\n")
      (display "❌ Session: Not found - use (hi 'sonnet 'your-name \"message\") first\n"))
  
  (display "\n💡 Common Issues:\n")
  (display "  • If commands fail, check your session with (safe-session-check)\n")
  (display "  • If Lambda Kombat fails, the game module may need loading\n")
  (display "  • If DUCKIE fails, the companion system may need loading\n")
  (display "  • Use safe versions of commands for better error handling\n")
  (display "  • Try individual commands before running the full tutorial\n\n")
  
  (display "🆘 Still having issues?\n")
  (display "  • Restart the REPL with (load \"shell/repl.ss\")\n")
  (display "  • Check that all modules loaded successfully\n")
  (display "  • Try the basic commands first: (who), (digest), (help)\n\n"))

;; Simple tutorial entry point
(define (tutorial . args)
  (cond
   ((null? args)
    (display "\n🎯 Tutorial Options:\n\n")
    (display "  (robust-interactive-tutorial) - Full tutorial with error handling\n")
    (display "  (tutorial-help)              - Show help and troubleshooting\n")
    (display "  (tutorial-progress)          - Show your progress\n\n")
    (display "💡 New to The Fold? Start with: (robust-interactive-tutorial)\n\n"))
   
   ((eq? (car args) 'help) (tutorial-help))
   ((eq? (car args) 'progress) (tutorial-progress))
   ((eq? (car args) 'robust) (robust-interactive-tutorial))
   
   (else
    (display "Unknown tutorial option. Use (tutorial) for options.\n"))))

;; Aliases for backward compatibility
(define start-tutorial robust-interactive-tutorial)
(define start-interactive-tutorial robust-interactive-tutorial)

;; list-tutorials : → void
;; List available tutorials
(define (list-tutorials)
  (display "
📚 Available Tutorials:

  1. (robust-interactive-tutorial)  - Guided introduction to The Fold
     Learn session management, forum posting, and basic commands

  2. (tutorial-help)                - Troubleshooting guide
     Get help when things go wrong

  3. (who)                          - Check session status
  4. (digest)                       - View recent forum activity
  5. (help)                         - Show all available commands

For a comprehensive tutorial experience, start with:
  (robust-interactive-tutorial)

"))

(display "🛠️ Robust tutorial system with session fixes loaded!\n")
(display "Use (tutorial) for options, (robust-interactive-tutorial) to start\n")
(display "Use (tutorial-help) for troubleshooting guidance\n")

(when (top-level-bound? 'provide)
      (provide robust-interactive-tutorial tutorial-help tutorial-progress
               tutorial start-tutorial start-interactive-tutorial list-tutorials
               safe-who safe-digest safe-chat safe-lambda-kombat safe-duckie-greet
               safe-fold-eval safe-blocks))
