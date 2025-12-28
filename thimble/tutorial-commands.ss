;;; thimble/tutorial-commands.ss — Tutorial System Command Registration
;;;
;;; This file registers tutorial system commands with the main command registry.
;;; Must be loaded after both commands.ss and tutorial.ss

;;; Load dependencies
(load "commands.ss")
(load "tutorial.ss")

;;; ============================================================
;;; Tutorial Command Handlers
;;; ============================================================

;;; start-tutorial-handler : [Symbol] → void
(define start-tutorial-handler
  (case-lambda
    [()
     (start-tutorial)
     (void)]
    [(tutorial-id)
     (start-tutorial tutorial-id)
     (void)]))

;;; tutorial-next-handler : → void
(define (tutorial-next-handler)
  (tutorial-next)
  (void))

;;; tutorial-do-handler : → void
(define (tutorial-do-handler)
  (tutorial-do)
  (void))

;;; tutorial-skip-handler : → void
(define (tutorial-skip-handler)
  (tutorial-skip)
  (void))

;;; tutorial-help-handler : → void
(define (tutorial-help-handler)
  (tutorial-help)
  (void))

;;; tutorial-status-handler : → void
(define (tutorial-status-handler)
  (tutorial-status)
  (void))

;;; list-tutorials-handler : [Symbol] → void
(define list-tutorials-handler
  (case-lambda
    [()
     (list-tutorials)
     (void)]
    [(tier)
     (list-tutorials tier)
     (void)]))

;;; export-tutorial-progress-handler : → void
(define (export-tutorial-progress-handler)
  (export-tutorial-progress)
  (void))

;;; ============================================================
;;; Register Tutorial Commands
;;; ============================================================

(define (register-tutorial-commands!)
  ;; Main tutorial commands
  (register-command!
   'start-tutorial
   "Start interactive tutorial"
   "Start an interactive tutorial for your tier or a specific tutorial.\n  Usage: (cmd 'start-tutorial)\n         (cmd 'start-tutorial 'basic-navigation)\n         (start-tutorial)\n         (start-tutorial 'lambda-kombat-training)"
   start-tutorial-handler)
  
  (register-command!
   'tutorial-next
   "Next tutorial step"
   "Move to the next step in the current tutorial.\n  Usage: (cmd 'tutorial-next)\n         (tutorial-next)"
   tutorial-next-handler)
  
  (register-command!
   'tutorial-do
   "Do tutorial exercise"
   "Attempt to complete the current tutorial exercise.\n  Usage: (cmd 'tutorial-do)\n         (tutorial-do)"
   tutorial-do-handler)
  
  (register-command!
   'tutorial-skip
   "Skip tutorial step"
   "Skip the current tutorial step.\n  Usage: (cmd 'tutorial-skip)\n         (tutorial-skip)"
   tutorial-skip-handler)
  
  (register-command!
   'tutorial-help
   "Tutorial help"
   "Get help with the current tutorial step.\n  Usage: (cmd 'tutorial-help)\n         (tutorial-help)"
   tutorial-help-handler)
  
  (register-command!
   'tutorial-status
   "Show tutorial progress"
   "Display current tutorial progress and status.\n  Usage: (cmd 'tutorial-status)\n         (tutorial-status)"
   tutorial-status-handler)
  
  (register-command!
   'list-tutorials
   "List available tutorials"
   "Display all available tutorials, optionally filtered by tier.\n  Usage: (cmd 'list-tutorials)\n         (cmd 'list-tutorials 'builder)\n         (list-tutorials)\n         (list-tutorials 'player)"
   list-tutorials-handler)
  
  (register-command!
   'export-tutorial-progress
   "Export tutorial progress"
   "Export your tutorial completion progress.\n  Usage: (cmd 'export-tutorial-progress)\n         (export-tutorial-progress)"
   export-tutorial-progress-handler))

;;; Auto-register tutorial commands
(register-tutorial-commands!)

(display "Tutorial commands registered successfully!\n")
(display "Use (commands) to see all available commands including tutorials.\n")
(display "Start with (start-tutorial) or (start-tutorial 'basic-navigation)\n")