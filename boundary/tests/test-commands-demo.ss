(define *quiet* #t)
(load "boundary/repl/repl.ss")

(doc 'module 'test-commands-demo)
(doc 'description "Comprehensive demo of command system - discovery, help, routing, error recovery, registration")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(display "\n")
(display "╔═══════════════════════════════════════════════════════════════════╗\n")
(display "║       THE FOLD — STRUCTURED REPL COMMAND SUBSYSTEM DEMO          ║\n")
(display "╚═══════════════════════════════════════════════════════════════════╝\n")
(display "\n")

(doc 'section 'command-discovery)
(display "FEATURE 1: Command Discovery\n")
(display "────────────────────────────────────────────────────────────────────\n")
(display "The (commands) function lists all registered commands:\n\n")
(commands)

(doc 'section 'help-system)
(display "\nFEATURE 2: Help System\n")
(display "────────────────────────────────────────────────────────────────────\n")
(display "General help with (help):\n")
(help)

(display "\nCommand-specific help with (help 'command-name):\n")
(help 'version)

(doc 'section 'command-routing)
(display "\nFEATURE 3: Command Routing\n")
(display "────────────────────────────────────────────────────────────────────\n")
(display "Invoke commands via (cmd 'name args...):\n\n")
(display "  (cmd 'version) => ")
(let ([result (cmd 'version)])
     (display (format "~a\n" (car result))))

(doc 'section 'error-recovery)
(display "\nFEATURE 4: Error Recovery\n")
(display "────────────────────────────────────────────────────────────────────\n")
(display "Unknown command with typo detection:\n")
(display "  (cmd 'whoo) => ")
(let ([result (cmd 'whoo)])
     (display (format "~a\n" (cadr result)))
     (display (format "  Full result: ~a\n" result)))

(display "\n  (cmd 'unknown-cmd) => ")
(let ([result (cmd 'unknown-cmd)])
     (display (format "~a\n" (cadr result))))

(doc 'section 'dynamic-registration)
(display "\nFEATURE 5: Dynamic Command Registration\n")
(display "────────────────────────────────────────────────────────────────────\n")
(display "Register new command at runtime:\n\n")

(register-command!
 'hello
 "Say hello"
 "A simple greeting command.\n  Usage: (cmd 'hello [name])"
 (lambda args
         (if (null? args)
             (display "Hello, world!\n")
             (display (format "Hello, ~a!\n" (car args))))
         "greeting-sent"))

(display "  Registered 'hello' command.\n")
(display "  (cmd 'hello) => ")
(cmd 'hello)
(display "  (cmd 'hello \"Scheme\") => ")
(cmd 'hello "Scheme")

(display "\n  Unregister command:\n")
(unregister-command! 'hello)
(display "  'hello' unregistered.\n")
(display "  (cmd 'hello) => ")
(let ([result (cmd 'hello)])
     (display (format "~a\n" result)))

(doc 'section 'direct-functions)
(display "\nFEATURE 6: Direct Convenience Functions\n")
(display "────────────────────────────────────────────────────────────────────\n")
(display "All registered commands also available as direct functions:\n\n")
(display "  (version) =>\n    ")
(version)

(display "\n")
(display "╔═══════════════════════════════════════════════════════════════════╗\n")
(display "║                    DEMO COMPLETE                                  ║\n")
(display "╚═══════════════════════════════════════════════════════════════════╝\n")
(display "\n")
(display "Summary of features:\n")
(display "  ✓ Command registry with register/unregister\n")
(display "  ✓ Command discovery via (commands)\n")
(display "  ✓ Help system: (help) and (help 'cmd)\n")
(display "  ✓ Command routing via (cmd 'name args...)\n")
(display "  ✓ Error recovery with 'did you mean?' suggestions\n")
(display "  ✓ Return values: (ok result) or (error 'command-error msg)\n")
(display "  ✓ Core commands: digest, chat, who, bye, clear, version\n")
(display "\n")
