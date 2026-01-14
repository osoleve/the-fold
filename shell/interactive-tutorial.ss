;;; shell/interactive-tutorial.ss — Interactive Step-by-Step Tutorial
;;;
;;; A comprehensive tutorial system with interactive guidance and progress tracking

;;; Tutorial curriculum structure
(define *tutorial-curriculum*
  `((basics
     ((session-check
       "Check Your Session"
       "Learn to verify your identity and session status"
       "Use (who) to display your current session information."
       ,(lambda () (who))
       completed)
      
      (forum-exploration
       "Explore the Forum"
       "Discover how to browse forum activity and channels"
       "Use (digest) to view recent forum activity."
       ,(lambda () (digest))
       completed)
      
      (first-message
       "Post Your First Message"
       "Learn to communicate in the chat system"
       "Use (chat \"Hello! I'm learning The Fold.\") to post to chat."
       ,(lambda () (chat "Hello! I'm learning The Fold."))
       completed)
      
      (pattern-matching
       "Master Pattern Matching"
       "Play Lambda Kombat to learn S-expression patterns"
       "Use (lambda-kombat) to start the pattern matching game."
       ,(lambda () (lambda-kombat))
       pending)
      
      (duckie-meeting
       "Meet Your Digital Companion"
       "Interact with DUCKIE, the Fold's digital entity"
       "Use (duckie-greet) to meet DUCKIE and (duckie-mood) to check their mood."
       ,(lambda () (duckie-greet))
       pending)))
    
    (advanced
     ((typed-evaluation
       "Typed Evaluation System"
       "Learn to use the type system and evaluator"
       "Use (fold-eval (prim 'add 1 2)) to evaluate expressions."
       ,(lambda () (fold-eval (prim 'add 1 2)))
       pending)
      
      (block-exploration
       "Explore the Block Store"
       "Navigate the content-addressed storage system"
       "Use (blocks) to see statistics and (popular 5) for popular blocks."
       ,(lambda () (blocks))
       pending)
      
      (forum-posting
       "Advanced Forum Usage"
       "Create posts, replies, and navigate channels"
       "Use (browse 'engineering 3) to explore the engineering channel."
       ,(lambda () (browse 'engineering 3))
       pending)))))

;;; Tutorial state management
(define *current-tutorial* (make-parameter #f))
(define *current-step* (make-parameter 0))
(define *tutorial-progress* (make-hash-table))

;;; ====
;;; Core Tutorial Functions
;;; ====

(define (start-interactive-tutorial . tutorial-name)
  (let ((tutorial (or (and (not (null? tutorial-name)) (car tutorial-name))
                      'basics)))
       
       (display "\n")
       (display "╔══════════════════════════════════════════════════════════════╗\n")
       (display "║              THE FOLD INTERACTIVE TUTORIAL                   ║\n")
       (display "╚══════════════════════════════════════════════════════════════╝\n\n")
       
       (*current-tutorial* tutorial)
       (*current-step* 0)
       
       (display (format "Starting ~a tutorial...\n\n" (symbol->string tutorial)))
       
       (case tutorial
             ((basics)
              (display "Welcome to The Fold! This tutorial will teach you essential skills:\n\n")
              (display "📚 Learning Objectives:\n")
              (display "  • Navigate the forum and chat systems\n")
              (display "  • Understand your session and identity\n")
              (display "  • Master pattern matching with Lambda Kombat\n")
              (display "  • Interact with DUCKIE, your digital companion\n\n"))
             
             ((advanced)
              (display "Advanced tutorial for experienced users:\n\n")
              (display "📚 Learning Objectives:\n")
              (display "  • Master the typed evaluation system\n")
              (display "  • Explore the content-addressed block store\n")
              (display "  • Create sophisticated forum posts and replies\n\n"))
             
             (else
              (display "Custom tutorial selected. Let's begin!\n\n")))
       
       (display "💡 Tip: You can exit anytime with (exit-tutorial)\n")
       (display "💡 Tip: Use (tutorial-hint) if you need help with the current step\n\n")
       
       (tutorial-next-step)))

(define (tutorial-next-step)
  (unless (*current-tutorial*)
          (error 'tutorial-next-step "No active tutorial. Use (start-interactive-tutorial) first."))
  
  (let* ((tutorial-name (*current-tutorial*))
         (tutorial-data (assoc tutorial-name *tutorial-curriculum*))
         (steps (cadr tutorial-data))
         (current-index (*current-step*))
         (total-steps (length steps)))
        
        (cond
         ((>= current-index total-steps)
          (complete-tutorial tutorial-name))
         
         (else
          (let* ((current-step-data (list-ref steps current-index))
                 (step-id (car current-step-data))
                 (step-title (cadr current-step-data))
                 (step-description (caddr current-step-data))
                 (step-instruction (cadddr current-step-data))
                 (step-action (car (cddddr current-step-data)))
                 (step-status (cadr (cddddr current-step-data))))
                
                (display-step-header step-title step-description current-index total-steps)
                (display-instruction step-instruction step-status)
                
                (when (procedure? step-action)
                      (display "\n🎯 Ready to try? The command is prepared above.\n")
                      (display "💡 Type the command and press Enter to execute it.\n")
                      (display "💡 Then use (tutorial-next-step) to continue.\n")))))))

(define (display-step-header title description current-index total-steps)
  (display "┌─────────────────────────────────────────────────────────────┐\n")
  (display (format "│ Step ~a/~a: ~a~a│\n"
                   (+ current-index 1) total-steps title
                   (make-string (max 1 (- 49 (string-length title)
                                          (string-length (number->string (+ current-index 1)))
                                          (string-length (number->string total-steps))
                                          8)) #\space)))
  (display "└─────────────────────────────────────────────────────────────┘\n\n")
  
  (display description)
  (display "\n\n"))

(define (display-instruction instruction status)
  (display "📖 Instructions:\n")
  (display instruction)
  (display "\n\n")
  
  (case status
        ((completed) (display "✅ You've already completed this step!\n"))
        ((pending) (display "⏳ This step is waiting to be completed.\n"))
        ((active) (display "🎯 This is your current active step.\n"))
        (else (display "🤔 Step status unknown.\n"))))

(define (complete-tutorial tutorial-name)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    TUTORIAL COMPLETED! 🎉                    ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")
  
  (case tutorial-name
        ((basics)
         (display "Congratulations! You've mastered the basics of The Fold!\n\n")
         (display "✅ What you've learned:\n")
         (display "  • Session management and identity\n")
         (display "  • Forum navigation and posting\n")
         (display "  • Pattern matching with Lambda Kombat\n")
         (display "  • Interacting with DUCKIE\n\n")
         (display "🚀 Next steps:\n")
         (display "  • Try (start-interactive-tutorial 'advanced) for advanced topics\n")
         (display "  • Explore all commands with (help)\n")
         (display "  • Join discussions in various forum channels\n")
         (display "  • Experiment with the typed evaluation system\n\n"))
        
        ((advanced)
         (display "Excellent! You've mastered advanced Fold techniques!\n\n")
         (display "✅ Advanced skills acquired:\n")
         (display "  • Typed evaluation and type checking\n")
         (display "  • Block store exploration\n")
         (display "  • Sophisticated forum usage\n\n")
         (display "🎆 You're now ready to contribute to The Fold ecosystem!\n\n")))
  
  ;; Mark tutorial as completed in progress tracking
  (hash-table-set! *tutorial-progress* tutorial-name 'completed)
  
  (*current-tutorial* #f)
  (*current-step* 0)
  
  (display "🏆 Tutorial badge earned! You can review your progress anytime.\n\n"))

(define (tutorial-hint)
  (unless (*current-tutorial*)
          (display "No active tutorial. Use (start-interactive-tutorial) to begin.\n")
          (return))
  
  (let* ((tutorial-name (*current-tutorial*))
         (tutorial-data (assoc tutorial-name *tutorial-curriculum*))
         (steps (cadr tutorial-data))
         (current-index (*current-step*))
         (current-step-data (and (< current-index (length steps))
                                 (list-ref steps current-index))))
        
        (if current-step-data
            (let ((step-title (cadr current-step-data))
                  (step-instruction (cadddr current-step-data)))
                 (display "\n💡 Tutorial Hint:\n\n")
                 (display (format "Current step: ~a\n" step-title))
                 (display "Need help? Here's the exact command to use:\n")
                 (display step-instruction)
                 (display "\n\n💡 After executing the command, use (tutorial-next-step) to continue.\n\n"))
            (display "Tutorial completed or no current step.\n"))))

(define (exit-tutorial)
  (if (*current-tutorial*)
      (begin
       (display "\n👋 Exiting tutorial...\n")
       (display "Your progress has been saved. Use (start-interactive-tutorial) to continue later.\n")
       (*current-tutorial* #f)
       (*current-step* 0)
       (display "\n"))
      (display "No active tutorial to exit.\n")))

(define (show-tutorial-progress)
  (display "\n📊 Tutorial Progress Report:\n")
  (display "====\n\n")
  
  (hash-table-walk
   *tutorial-progress*
   (lambda (tutorial status)
           (display (format "~a: ~a\n" tutorial status))))
  
  (if (*current-tutorial*)
      (let ((current-index (*current-step*))
            (tutorial-name (*current-tutorial*))
            (tutorial-data (assoc (*current-tutorial*) *tutorial-curriculum*))
            (steps (cadr (assoc (*current-tutorial*) *tutorial-curriculum*))))
           (display (format "\nCurrently working on: ~a (step ~a/~a)\n"
                            tutorial-name current-index (length steps))))
      (display "\nNo tutorial currently in progress.\n"))
  
  (display "\n"))

;;; Convenience aliases
(define tutorial-next tutorial-next-step)
(define tutorial-start start-interactive-tutorial)

(display "🎯 Interactive tutorial system loaded!\n")
(display "Use (tutorial-start) or (start-interactive-tutorial) to begin.\n")
(display "Use (tutorial-help) for guidance during tutorials.\n")

;;; Note: `provide` is Racket-specific. Functions are available after load.
;;; Exported: start-interactive-tutorial tutorial-next-step tutorial-hint
;;;           exit-tutorial show-tutorial-progress tutorial-next tutorial-start
