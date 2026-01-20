;;; Load dependencies
(load "commands.ss")
(load "tutorial.ss")

(doc 'module 'boundary/tutorial/tutorial-commands)
(doc 'description "Tutorial System Command Registration - registers tutorial commands with main command registry")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Must be loaded after both commands.ss and tutorial.ss")

(doc 'section 'tutorial-command-handlers)
(define start-tutorial-handler
  (doc 'type (-> (Maybe Symbol) Void))
  (doc 'description "Command handler for starting tutorials")
  (case-lambda
   [()
    (start-tutorial)
    (void)]
   [(tutorial-id)
    (start-tutorial tutorial-id)
    (void)]))

(define (tutorial-next-handler)
  (doc 'type (-> Void))
  (doc 'description "Command handler for advancing to next tutorial step")
  (tutorial-next)
  (void))

(define (tutorial-do-handler)
  (doc 'type (-> Void))
  (doc 'description "Command handler for executing tutorial exercise")
  (tutorial-do)
  (void))

(define (tutorial-skip-handler)
  (doc 'type (-> Void))
  (doc 'description "Command handler for skipping tutorial step")
  (tutorial-skip)
  (void))

(define (tutorial-help-handler)
  (doc 'type (-> Void))
  (doc 'description "Command handler for displaying tutorial help")
  (tutorial-help)
  (void))

(define (tutorial-status-handler)
  (doc 'type (-> Void))
  (doc 'description "Command handler for showing tutorial status")
  (tutorial-status)
  (void))

(define list-tutorials-handler
  (doc 'type (-> (Maybe Symbol) Void))
  (doc 'description "Command handler for listing tutorials")
  (case-lambda
   [()
    (list-tutorials)
    (void)]
   [(tier)
    (list-tutorials tier)
    (void)]))

(define (export-tutorial-progress-handler)
  (doc 'type (-> Void))
  (doc 'description "Command handler for exporting tutorial progress")
  (export-tutorial-progress)
  (void))

(doc 'section 'register-tutorial-commands)

(define (register-tutorial-commands!)
  (doc 'type (-> Void))
  (doc 'description "Register all tutorial commands with the command registry")
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
