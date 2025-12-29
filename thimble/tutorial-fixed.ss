;;; thimble/tutorial-fixed.ss — Fixed Tutorial System with Proper Loading
;;;
;;; This version fixes the binding issues and ensures tutorial functions
;;; are properly available in the main REPL namespace

;;; ============================================================
;;; Core Tutorial Functions - Defined in Global Namespace
;;; ============================================================

;; Simple interactive tutorial - most reliable entry point
(define (interactive-tutorial)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              THE FOLD INTERACTIVE TUTORIAL                   ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "🎯 Welcome to The Fold Interactive Tutorial!\n\n")
  (display "This tutorial will guide you through essential skills:\n\n")
  
  (display "📚 STEP 1: Check Your Identity\n")
  (display "   Use (who) to see your current session information\n\n")
  
  (display "📚 STEP 2: Explore the Forum\n")
  (display "   Use (digest) to view recent forum activity\n\n")
  
  (display "📚 STEP 3: Post Your First Message\n")
  (display "   Use (chat \"Hello! I'm learning The Fold.\") to introduce yourself\n\n")
  
  (display "📚 STEP 4: Master Pattern Matching\n")
  (display "   Use (lambda-kombat) to play the pattern matching game\n\n")
  
  (display "📚 STEP 5: Meet DUCKIE\n")
  (display "   Use (duckie-greet) to meet your digital companion\n\n")
  
  (display "💡 TIP: Try each command and see what happens!\n")
  (display "💡 TIP: Use (help) to see all available commands\n")
  (display "💡 TIP: Explore at your own pace - there's no rush!\n\n")
  
  (display "Ready to begin your journey? Start with (who) and explore from there!\n\n"))

;; Guided step-by-step tutorial
(define (guided-tutorial)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║              THE FOLD GUIDED TUTORIAL                        ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "🎯 Welcome to the guided tutorial experience!\n\n")
  (display "I'll walk you through each step with detailed instructions.\n\n")
  
  ;; Step 1: Session Check
  (display "📚 STEP 1: Check Your Session Status\n")
  (display "   Let's verify your identity in The Fold...\n")
  (display "   💡 Type exactly: (who)\n")
  (display "   💡 This will show your name, tier, and login time\n\n")
  
  (display "Press Enter when you've completed this step...\n")
  ;; Note: In a real interactive system, we'd wait for user input
  ;; For now, we'll provide the command to execute
  
  ;; Auto-execute to demonstrate
  (display "🚀 Executing: (who)\n")
  (who)
  (display "\n✅ Step 1 complete! You now know how to check your session.\n\n")
  
  ;; Step 2: Forum Exploration
  (display "📚 STEP 2: Explore Forum Activity\n")
  (display "   Let's see what's happening in the community...\n")
  (display "   💡 Type exactly: (digest)\n")
  (display "   💡 This shows recent posts and chat messages\n\n")
  
  (display "🚀 Executing: (digest)\n")
  (digest)
  (display "\n✅ Step 2 complete! You can now explore forum activity.\n\n")
  
  ;; Step 3: First Message
  (display "📚 STEP 3: Post Your First Message\n")
  (display "   Time to introduce yourself to the community...\n")
  (display "   💡 Type exactly: (chat \"Hello Fold! I'm learning the tutorial.\")\n")
  (display "   💡 This posts to the chat channel\n\n")
  
  (display "🚀 Executing: (chat \"Hello Fold! I'm learning the tutorial.\")\n")
  (chat "Hello Fold! I'm learning the tutorial.")
  (display "\n✅ Step 3 complete! You can now participate in discussions.\n\n")
  
  ;; Step 4: Lambda Kombat
  (display "📚 STEP 4: Play Lambda Kombat\n")
  (display "   Let's learn pattern matching through gameplay...\n")
  (display "   💡 Type exactly: (lambda-kombat)\n")
  (display "   💡 Match S-expression patterns to score points\n\n")
  
  (display "🚀 Executing: (lambda-kombat)\n")
  (lambda-kombat)
  (display "\n✅ Step 4 complete! You've started learning pattern matching.\n\n")
  
  ;; Step 5: DUCKIE
  (display "📚 STEP 5: Meet DUCKIE\n")
  (display "   Let's meet your digital companion...\n")
  (display "   💡 Type exactly: (duckie-greet)\n")
  (display "   💡 DUCKIE has moods and personality\n\n")
  
  (display "🚀 Executing: (duckie-greet)\n")
  (duckie-greet)
  (display "\n✅ Step 5 complete! You've met DUCKIE.\n\n")
  
  ;; Completion
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    TUTORIAL COMPLETED! 🎉                    ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "🎉 Congratulations! You've completed the guided tutorial!\n\n")
  
  (display "✅ Skills Mastered:\n")
  (display "  • Session management with (who)\n")
  (display "  • Forum exploration with (digest)\n")
  (display "  • Chat posting with (chat)\n")
  (display "  • Pattern matching with Lambda Kombat\n")
  (display "  • DUCKIE interaction\n\n")
  
  (display "🚀 Next Steps:\n")
  (display "  • Try (help) to discover more commands\n")
  (display "  • Explore different channels with (browse 'engineering)\n")
  (display "  • Use (blocks) to explore the content store\n")
  (display "  • Experiment with (fold-eval (prim 'add 1 2))\n\n")
  
  (display "Welcome to The Fold! Explore and enjoy! 🌟\n\n"))

;; Quick tutorial command that adapts to user preference
(define (tutorial . args)
  (cond
   ((null? args)
    (display "\n🎯 Tutorial Options:\n\n")
    (display "  (tutorial 'interactive)  - Quick overview tutorial\n")
    (display "  (tutorial 'guided)      - Step-by-step guided experience\n")
    (display "  (tutorial 'help)        - Show this help\n\n")
    (display "Or just try individual commands:\n")
    (display "  (who), (digest), (chat \"hello\"), (lambda-kombat), (duckie-greet)\n\n"))
   
   ((eq? (car args) 'interactive) (interactive-tutorial))
   ((eq? (car args) 'guided) (guided-tutorial))
   ((eq? (car args) 'help) (tutorial))
   
   (else
    (display "Unknown tutorial option. Use (tutorial 'help) for options.\n"))))

;; Alias for common usage
(define start-tutorial interactive-tutorial)
(define start-interactive-tutorial guided-tutorial)

;; Tutorial help system
(define (tutorial-help)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    TUTORIAL HELP SYSTEM                      ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (display "📚 Available Tutorial Commands:\n\n")
  
  (display "Basic Tutorials:\n")
  (display "  (tutorial)                    - Show tutorial options\n")
  (display "  (tutorial 'interactive)       - Quick interactive overview\n")
  (display "  (tutorial 'guided)            - Step-by-step guided experience\n")
  (display "  (tutorial 'help)              - Show this help\n\n")
  
  (display "Individual Learning:\n")
  (display "  (who)                         - Check your session\n")
  (display "  (digest)                      - View forum activity\n")
  (display "  (chat \"message\")             - Post to chat\n")
  (display "  (lambda-kombat)              - Play pattern matching game\n")
  (display "  (duckie-greet)               - Meet DUCKIE companion\n")
  (display "  (help)                       - See all commands\n\n")
  
  (display "💡 Tips for Success:\n")
  (display "  • Take your time - there's no rush!\n")
  (display "  • Try commands multiple times to get comfortable\n")
  (display "  • Use (help) to discover more features\n")
  (display "  • Explore different forum channels\n")
  (display "  • Don't hesitate to experiment!\n\n"))

;; Simple progress tracking
(define *tutorial-completed-steps* (make-parameter '()))

(define (mark-tutorial-step-complete! step)
  (let ((completed (*tutorial-completed-steps*)))
       (unless (member step completed)
               (*tutorial-completed-steps* (cons step completed)))))

(define (tutorial-progress)
  (let ((completed (length (*tutorial-completed-steps*)))
        (total 5))  ; Total number of basic tutorial steps
       (display "\n📊 Tutorial Progress:\n")
       (display (format "Completed: ~a/~a steps (~a%)\n"
                        completed total (round (* (/ completed total) 100))))
       
       (when (>= completed total)
             (display "\n🎉 All basic tutorial steps completed!\n")
             (display "🏆 You've earned the title: Fold Explorer\n"))
       
       (display "\n")))

;; Ensure these functions are available in the main namespace
(display "📚 Fixed tutorial system loaded!\n")
(display "Use (tutorial) to see options, (interactive-tutorial) for quick start\n")

;; Functions are available globally (Chez Scheme doesn't need provide)
