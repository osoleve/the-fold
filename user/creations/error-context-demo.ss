;;; error-context-demo.ss — Simple demo of error context system

(display "═══════════════════════════════════════════════════════════════\n")
(display "  ERROR CONTEXT SYSTEM DEMO\n")
(display "═══════════════════════════════════════════════════════════════\n\n")

;; Load just the core functions (without provide)
(load "/home/oso/the-fold/thimble/error-context-fixed.ss")

;; Run the demo function directly
(demo-error-context)

(display "\n✅ Error context system demo completed!\n")
(display "✅ The system transforms cryptic errors into helpful guidance\n")
(display "✅ Ready to integrate into the main REPL system!\n")
