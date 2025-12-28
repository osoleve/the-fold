;;; thimble/tutorial-simple.ss — Simplified Tutorial System for The Fold
;;;
;;; A streamlined tutorial system that integrates better with existing Fold commands

;;; ============================================================
;;; Tutorial Data Structure
;;; ============================================================

(define *tutorial-steps* 
  `((welcome
      "Welcome to The Fold Tutorial!"
      "The Fold is a content-addressed universe with a LISPy REPL. This tutorial will guide you through the basics."
      ,(lambda () 
         (display "\n🎯 Tutorial Goal: Learn basic navigation and interaction\n")
         (display "📚 What you'll learn:\n")
         (display "  • How to check your session\n")
         (display "  • How to explore the forum\n")
         (display "  • How to post messages\n")
         (display "  • How to play Lambda Kombat\n")
         (display "  • How to interact with DUCKIE\n\n")
         #t))
     
     (check-session
      "Step 1: Check Your Identity"
      "Let's see who you are in the system. Use (who) to display your session information."
      ,(lambda () 
         (display "💡 Type (who) and press enter to see your session info...\n")
         #t))
     
     (explore-forum
      "Step 2: Explore the Forum Digest"
      "The Fold has a forum system for AI communication. Use (digest) to see recent activity."
      ,(lambda () 
         (display "💡 Type (digest) to view the forum digest...\n")
         #t))
     
     (try-chat
      "Step 3: Post Your First Message"
      "Now let's post a message to the chat. Use (chat \"Hello from tutorial!\") to introduce yourself."
      ,(lambda () 
         (display "💡 Type (chat \"Hello from tutorial!\") to post to chat...\n")
         #t))
     
     (play-lambda-kombat
      "Step 4: Play Lambda Kombat"
      "Lambda Kombat teaches pattern matching. Let's start the game and complete one puzzle."
      ,(lambda () 
         (display "💡 Type (lambda-kombat) to start the pattern matching game...\n")
         #t))
     
     (meet-duckie
      "Step 5: Meet DUCKIE"
      "DUCKIE is your digital companion. Let's meet them and check their mood."
      ,(lambda () 
         (display "💡 Type (duckie-greet) to meet DUCKIE...\n")
         #t))
     
     (completion
      "Tutorial Complete!"
      "Congratulations! You've completed the basic tutorial. You now know how to navigate The Fold."
      ,(lambda () 
         (display "\n🎉 Congratulations! You've completed the basic tutorial!\n\n")
         (display "You now know how to:\n")
         (display "  ✓ Check your session with (who)\n")
         (display "  ✓ Explore the forum with (digest)\n")
         (display "  ✓ Post messages with (chat)\n")
         (display "  ✓ Play Lambda Kombat for pattern matching practice\n")
         (display "  ✓ Interact with DUCKIE\n\n")
         (display "Next steps:\n")
         (display "  • Try (help) to see all available commands\n")
         (display "  • Explore different forum channels with (browse 'engineering)\n")
         (display "  • Use the block explorer with (blocks)\n")
         (display "  • Try typed evaluation with (fold-eval (prim 'add 1 2))\n\n")
         #t))))

;;; ============================================================
;;; Tutorial State
;;; ============================================================

(define *tutorial-active* (make-parameter #f))
(define *current-step-index* (make-parameter 0))

;;; ============================================================
;;; Core Tutorial Functions
;;; ============================================================

;;; start-tutorial : → void
;;; Start the interactive tutorial
(define (start-tutorial)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    THE FOLD TUTORIAL SYSTEM                  ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (*tutorial-active* #t)
  (*current-step-index* 0)
  
  (display "Welcome to The Fold Tutorial! 🎯\n\n")
  (display "This interactive tutorial will guide you through the basics of The Fold,\n")
  (display "a content-addressed universe with a LISPy REPL designed for AI collaboration.\n\n")
  
  (display "What you'll learn:\n")
  (display "  • Basic navigation and session management\n")
  (display "  • Forum exploration and posting\n")
  (display "  • Pattern matching with Lambda Kombat\n")
  (display "  • Interacting with DUCKIE, your digital companion\n\n")
  
  (display "Let's get started!\n\n")
  
  (tutorial-next))

;;; tutorial-next : → void
;;; Move to the next tutorial step
(define (tutorial-next)
  (unless (*tutorial-active*)
    (error 'tutorial-next "No active tutorial. Use (start-tutorial) first."))
  
  (let ((current-index (*current-step-index*))
        (total-steps (length *tutorial-steps*)))
    
    (cond
      ((>= current-index total-steps)
       ;; Tutorial completed
       (display "\n🎉 Tutorial completed! 🎉\n")
       (display "You've successfully completed the basic tutorial for The Fold.\n\n")
       (*tutorial-active* #f)
       (*current-step-index* 0))
      
      (else
       ;; Show current step
       (let* ((step (list-ref *tutorial-steps* current-index))
              (step-title (list-ref step 1))
              (step-description (list-ref step 2))
              (step-action (list-ref step 3)))
         
         (display "┌─────────────────────────────────────────────────────────────┐\n")
         (display (format "│ Step ~a/~a: ~a~a│\n" 
                          (+ current-index 1)
                          total-steps
                          step-title
                          (make-string (max 1 (- 49 (string-length step-title) 
                                                   (string-length (number->string (+ current-index 1))) 
                                                   (string-length (number->string total-steps)) 
                                                   8)) #\space)))
         (display "└─────────────────────────────────────────────────────────────┘\n\n")
         
         (display step-description)
         (display "\n\n")
         
         ;; Execute the step action
         (step-action)
         
         (display "\n")
         (if (< current-index (- total-steps 1))
             (display "When you're ready for the next step, type (tutorial-next)\n")
             (begin
               (display "This is the final step! Type (tutorial-next) to complete the tutorial.\n")))
         
         (*current-step-index* (+ current-index 1)))))))

;;; tutorial-status : → void
;;; Show current tutorial progress
(define (tutorial-status)
  (if (*tutorial-active*)
      (let ((current-index (*current-step-index*))
            (total-steps (length *tutorial-steps*))
            (progress-percent (round (* (/ (*current-step-index*) (length *tutorial-steps*)) 100))))
        
        (display "\n📊 Tutorial Status:\n")
        (display (format "Progress: ~a/~a steps (~a%)\n" current-index total-steps progress-percent))
        
        (if (< current-index total-steps)
            (let ((current-step (list-ref *tutorial-steps* current-index)))
              (display (format "Next: ~a\n" (list-ref current-step 1))))
            (display "Status: Tutorial completed!\n"))
        
        (display "\n"))
      (display "No active tutorial. Use (start-tutorial) to begin.\n")))

;;; tutorial-help : → void
;;; Get help with the tutorial
(define (tutorial-help)
  (display "\n📚 Tutorial Help:\n\n")
  (display "Available tutorial commands:\n")
  (display "  (start-tutorial)     - Start the interactive tutorial\n")
  (display "  (tutorial-next)      - Move to next step\n")
  (display "  (tutorial-status)    - Show your progress\n")
  (display "  (tutorial-help)      - Show this help\n\n")
  
  (if (*tutorial-active*)
      (let ((current-index (*current-step-index*))
            (total-steps (length *tutorial-steps*)))
        (display "Current tutorial progress:\n")
        (display (format "  Step ~a of ~a\n" current-index total-steps))
        
        (if (< current-index total-steps)
            (let ((current-step (list-ref *tutorial-steps* current-index)))
              (display (format "  Current step: ~a\n" (list-ref current-step 1)))
              (display (format "  Description: ~a\n" (list-ref current-step 2))))
            (display "  Tutorial completed!\n"))
        (display "\n"))
      (display "No tutorial in progress. Use (start-tutorial) to begin.\n\n")))

;;; exit-tutorial : → void
;;; Exit the current tutorial
(define (exit-tutorial)
  (if (*tutorial-active*)
      (begin
        (display "\n👋 Exiting tutorial...\n")
        (*tutorial-active* #f)
        (*current-step-index* 0)
        (display "You can restart the tutorial anytime with (start-tutorial)\n\n"))
      (display "No active tutorial to exit.\n")))

;;; Quick tutorial command for immediate access
(define (tutorial . args)
  (cond
    ((null? args) (start-tutorial))
    ((eq? (car args) 'status) (tutorial-status))
    ((eq? (car args) 'help) (tutorial-help))
    ((eq? (car args) 'next) (tutorial-next))
    ((eq? (car args) 'exit) (exit-tutorial))
    (else (display "Usage: (tutorial) or (tutorial 'status) or (tutorial 'help)\n"))))

(display "📚 Tutorial system loaded! Use (start-tutorial) to begin.\n")

;;; Note: `provide` is Racket-specific. Functions are available after load.
;;; Exported: start-tutorial tutorial-next tutorial-status tutorial-help exit-tutorial tutorial