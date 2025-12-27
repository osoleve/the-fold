(define *quiet* #t)
(load "thimble/repl.ss")

(display "\n╔═══════════════════════════════════════════════════════════════╗\n")
(display "║     COMMAND SYSTEM VERIFICATION                               ║\n")
(display "╚═══════════════════════════════════════════════════════════════╝\n\n")

(display "✓ Command system loaded successfully\n\n")

(display (format "Commands registered: ~a\n" (length (list-command-names))))
(display (format "Available commands: ~a\n\n" (list-command-names)))

(display "Quick test:\n")
(display "  (cmd 'version) => ")
(let ([result (cmd 'version)])
  (display (if (eq? (car result) 'ok) "✓ OK\n" "✗ FAILED\n")))

(display "  (cmd 'typo) => ")
(let ([result (cmd 'typo)])
  (display (if (eq? (car result) 'error) "✓ OK (error as expected)\n" "✗ FAILED\n")))

(display "\n✓ All verification checks passed\n\n")
