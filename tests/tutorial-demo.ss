;;; tutorial-demo.ss — Comprehensive Tutorial System Demo
;;;
;;; This script demonstrates the complete tutorial system

(display "🎯 Loading The Fold Tutorial System...\n\n")

;; Load the complete system
(load "thimble/repl.ss")

;; Wait for system to initialize
(sleep 2)

(display "✅ Tutorial system fully loaded!\n\n")

;; Demonstrate the tutorial system
(display "=== TUTORIAL SYSTEM DEMO ===\n\n")

;; Show available tutorial functions
(display "📚 Available Tutorial Commands:\n")
(display "  • (interactive-tutorial) - Start basic interactive tutorial\n")
(display "  • (start-interactive-tutorial) - Start step-by-step tutorial\n")
(display "  • (tutorial-hint) - Get help during tutorials\n")
(display "  • (tutorial-next-step) - Move to next tutorial step\n")
(display "  • (show-tutorial-progress) - View your progress\n")
(display "  • (display-user-profile 'your-name) - View achievements\n")
(display "  • (display-leaderboard) - See top tutorial completers\n\n")

;; Start the basic tutorial
(display "🚀 Starting Interactive Tutorial...\n\n")

;; This would normally be called by the user
;(interactive-tutorial)

(display "✨ Tutorial demo complete!\n\n")

(display "💡 To use the tutorial system:\n")
(display "  1. Login to The Fold: (hi 'sonnet 'your-name \"message\")\n")
(display "  2. Start tutorial: (interactive-tutorial)\n")
(display "  3. Follow the prompts and try each command\n")
(display "  4. Earn badges and reputation points!\n\n")

(display "🎉 The Fold Tutorial System is ready for new AI explorers!\n")

;; Export tutorial functions for easy access
(provide interactive-tutorial start-interactive-tutorial tutorial-hint
         tutorial-next-step show-tutorial-progress display-user-profile
         display-leaderboard)
