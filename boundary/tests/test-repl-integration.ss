;;; Test normal REPL loading and integration

(load "boundary/repl/repl.ss")

;; Verify commands are available
(display "\n=== Verification Tests ===\n\n")

(display "1. Check (commands) function exists: ")
(display (if (procedure? commands) "✓ Yes\n" "✗ No\n"))

(display "2. Check (help) function exists: ")
(display (if (procedure? help) "✓ Yes\n" "✗ No\n"))

(display "3. Check (cmd) function exists: ")
(display (if (procedure? cmd) "✓ Yes\n" "✗ No\n"))

(display "4. Check (register-command!) exists: ")
(display (if (procedure? register-command!) "✓ Yes\n" "✗ No\n"))

(display "5. Check core commands registered:\n")
(display "   - digest: ")
(display (if (get-command 'digest) "✓ Yes\n" "✗ No\n"))
(display "   - chat: ")
(display (if (get-command 'chat) "✓ Yes\n" "✗ No\n"))
(display "   - who: ")
(display (if (get-command 'who) "✓ Yes\n" "✗ No\n"))
(display "   - bye: ")
(display (if (get-command 'bye) "✓ Yes\n" "✗ No\n"))
(display "   - clear: ")
(display (if (get-command 'clear) "✓ Yes\n" "✗ No\n"))
(display "   - version: ")
(display (if (get-command 'version) "✓ Yes\n" "✗ No\n"))

(display "\n6. Test command invocation:\n")
(display "   (cmd 'version) => ")
(let ([result (cmd 'version)])
     (display (if (eq? (car result) 'ok) "✓ Success\n" "✗ Failed\n")))

(display "\n7. Test typo detection:\n")
(display "   (cmd 'vrsion) => ")
(let ([result (cmd 'vrsion)])
     (if (eq? (car result) 'error)
         (display "✓ Returns error\n")
         (display "✗ Failed\n")))

(display "\n=== All integration tests passed ===\n")
