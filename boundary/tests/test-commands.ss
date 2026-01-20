(define *quiet* #t)
(load "boundary/repl/repl.ss")

(doc 'module 'test-commands)
(doc 'description "Basic command system tests - listing, help, invocation, error handling")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'list-commands)
(display "\n=== Test 1: List all commands ===\n")
(commands)

(doc 'section 'command-help)
(display "\n=== Test 2: Help on 'digest' command ===\n")
(help 'digest)

(doc 'section 'invoke-command)
(display "\n=== Test 3: Invoke version via cmd ===\n")
(let ([result (cmd 'version)])
     (display (format "Result: ~a\n" result)))

(doc 'section 'unknown-command-with-typo)
(display "\n=== Test 4: Unknown command (typo) ===\n")
(let ([result (cmd 'chatt "test")])
     (display (format "Result: ~a\n" result)))

(doc 'section 'unknown-command-no-suggestion)
(display "\n=== Test 5: Unknown command (no suggestion) ===\n")
(let ([result (cmd 'foobar)])
     (display (format "Result: ~a\n" result)))

(doc 'section 'direct-call)
(display "\n=== Test 6: Direct version call ===\n")
(version)

(display "\n=== All tests complete ===\n")
