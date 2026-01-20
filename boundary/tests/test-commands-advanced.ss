(define *quiet* #t)
(load "boundary/repl/repl.ss")

(doc 'module 'test-commands-advanced)
(doc 'description "Advanced command system tests - registration, unregistration, error handling")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "\n=== Advanced Command System Tests ===\n\n")

(doc 'section 'register-custom-command)
(display "Test 1: Register custom command 'greet'\n")
(register-command!
 'greet
 "Greet the user"
 "Greet the user with a friendly message.\n  Usage: (cmd 'greet)\n         (cmd 'greet name)"
 (lambda args
         (if (null? args)
             (display "Hello, friend!\n")
             (display (format "Hello, ~a!\n" (car args))))
         (void)))

(display "Registered. Command list:\n")
(commands)

(doc 'section 'invoke-custom-command)
(display "\nTest 2: Invoke custom command\n")
(let ([result1 (cmd 'greet)]
      [result2 (cmd 'greet "Alice")])
     (display (format "Result 1: ~a\n" result1))
     (display (format "Result 2: ~a\n" result2)))

(doc 'section 'help-custom-command)
(display "\nTest 3: Help on custom command\n")
(help 'greet)

(doc 'section 'unregister-command)
(display "\nTest 4: Unregister custom command\n")
(unregister-command! 'greet)
(display "Unregistered. Command list:\n")
(commands)

(doc 'section 'use-unregistered-command)
(display "\nTest 5: Try to use unregistered command\n")
(let ([result (cmd 'greet)])
     (display (format "Result: ~a\n" result)))

(doc 'section 'error-handling)
(display "\nTest 6: Command with error\n")
(register-command!
 'errortest
 "Test error handling"
 "A command that always fails for testing."
 (lambda args
         (error 'errortest "This is a test error")))

(let ([result (cmd 'errortest)])
     (display (format "Result: ~a\n" result)))

(unregister-command! 'errortest)

(display "\n=== All advanced tests complete ===\n")
