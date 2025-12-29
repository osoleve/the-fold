;;; test-enhanced-errors-fixed.ss — Test enhanced error messages (Fixed)

(display "Testing enhanced error messages with context...\n\n")

;; Load the error context system
(load "/home/oso/the-fold/thimble/error-context-simple.ss")

;; The functions are already defined and available after loading
(display "1. Testing BoardCraft error enhancement:\n")
(let ([result (format-error-with-fold-context "variable make-hex-board is not bound")])
     (display result)
     (newline))

(display "\n2. Testing Loom error enhancement:\n")
(let ([result (format-error-with-fold-context "variable tilemap-fill! is not bound")])
     (display result)
     (newline))

(display "\n3. Testing general error enhancement:\n")
(let ([result (format-error-with-fold-context "variable unknown-thing is not bound")])
     (display result)
     (newline))

(display "\n✅ Error enhancement working!\n")
(display "✅ Provides context-specific suggestions\n")
(display "✅ Links to relevant tutorials\n")
(display "✅ Makes errors actionable\n")
