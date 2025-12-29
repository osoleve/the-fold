;;; test-tutorial.ss — Test the tutorial system
;;;
;;; This script tests the tutorial system functionality

(display "Testing Tutorial System...\n\n")

;; Load the tutorial system
(load "thimble/repl.ss")

;; Wait a moment for everything to load
(sleep 1)

(display "✓ REPL and tutorial system loaded\n")

;; Test basic tutorial functions
(display "\n=== Testing Tutorial Commands ===\n")

;; Test list-tutorials
(display "Testing (list-tutorials)...\n")
(list-tutorials)

(display "\n=== Tutorial System Test Complete ===\n")
(display "The tutorial system is ready for use!\n")
(display "Try: (start-tutorial) or (start-tutorial 'basic-navigation)\n")
