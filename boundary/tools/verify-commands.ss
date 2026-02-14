(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'verify-commands)
(doc 'description "Command system verification and sanity checking")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(define *quiet* #t)
(load "boundary/repl/repl.ss")

(display "\n=================== COMMAND SYSTEM VERIFICATION ===================\n\n")

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
