;;; Test the command system

;; Load in quiet mode
(define *quiet* #t)
(load "boundary/repl/repl.ss")

;; Test 1: List all commands
(display "\n=== Test 1: List all commands ===\n")
(commands)

;; Test 2: Get help on specific command
(display "\n=== Test 2: Help on 'digest' command ===\n")
(help 'digest)

;; Test 3: Test cmd function with valid command
(display "\n=== Test 3: Invoke version via cmd ===\n")
(let ([result (cmd 'version)])
     (display (format "Result: ~a\n" result)))

;; Test 4: Test unknown command with suggestion
(display "\n=== Test 4: Unknown command (typo) ===\n")
(let ([result (cmd 'chatt "test")])
     (display (format "Result: ~a\n" result)))

;; Test 5: Test unknown command without good suggestion
(display "\n=== Test 5: Unknown command (no suggestion) ===\n")
(let ([result (cmd 'foobar)])
     (display (format "Result: ~a\n" result)))

;; Test 6: Direct function call
(display "\n=== Test 6: Direct version call ===\n")
(version)

(display "\n=== All tests complete ===\n")
