(display "Loading TDD system...
")

;; Load dependencies
(load "./fabric/stitches/prelude.ss")
(load "./fabric/stitches/test-framework.ss")

(display "Basic dependencies loaded.
")

;; Simple TDD help function
(define (tdd-help)
  (display "TDD System Commands:
")
  (display "  (test:watch) - Start watching tests
")
  (display "  (test:stop)  - Stop watching tests
"))

(display "TDD system ready!
")
(display "Use (tdd-help) for help.
")
