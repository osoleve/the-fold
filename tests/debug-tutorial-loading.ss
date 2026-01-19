;;; debug-tutorial-loading.ss — Debug tutorial system loading issues

(display "🔍 Debugging Tutorial System Loading...\n\n")

;; Test basic loading
(display "1. Testing basic REPL load...\n")
(load "boundary/repl/repl.ss")
(sleep 1)

;; Check if tutorial functions are available
(display "\n2. Checking tutorial function availability...\n")

(define (test-function name fn)
  (display (format "Testing ~a... " name))
  (if (top-level-bound? fn)
      (display "✅ BOUND\n")
      (display "❌ NOT BOUND\n")))

(test-function "interactive-tutorial" 'interactive-tutorial)
(test-function "start-interactive-tutorial" 'start-interactive-tutorial)
(test-function "start-tutorial" 'start-tutorial)
(test-function "tutorial-next" 'tutorial-next)
(test-function "tutorial-help" 'tutorial-help)

;; Check session management
(display "\n3. Testing session management...\n")
(test-function "session-exists?" 'session-exists?)
(test-function "read-session" 'read-session)
(test-function "who" 'who)

;; Check if we can define a simple tutorial function
(display "\n4. Testing manual function definition...\n")
(define (test-tutorial-func)
  (display "✅ Manual tutorial function works!\n"))

(test-tutorial-func)

;; Check command system
(display "\n5. Testing command system...\n")
(test-function "commands" 'commands)
(test-function "help" 'help)
(test-function "cmd-help" 'cmd-help)

;; Check what files are actually loaded
(display "\n6. Checking loaded modules...\n")
(display "Tutorial simple: ")
(if (top-level-bound? 'start-tutorial)
    (display "✅ Loaded\n")
    (begin
     (display "❌ Not loaded - attempting manual load...\n")
     (load "boundary/tutorial-simple.ss")
     (test-function "start-tutorial (after manual load)" 'start-tutorial)))

(display "Interactive tutorial: ")
(if (top-level-bound? 'interactive-tutorial)
    (display "✅ Loaded\n")
    (begin
     (display "❌ Not loaded - attempting manual load...\n")
     (load "boundary/tutorial/interactive-tutorial.ss")
     (test-function "interactive-tutorial (after manual load)" 'interactive-tutorial)))

;; Test if we can call the functions after manual loading
(display "\n7. Testing function execution after manual loading...\n")
(when (top-level-bound? 'interactive-tutorial)
      (display "Calling interactive-tutorial...\n")
      (interactive-tutorial))

(display "\n🔍 Debug complete! Check results above.\n")
