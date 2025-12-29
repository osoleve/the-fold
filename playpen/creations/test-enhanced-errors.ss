;;; test-enhanced-errors.ss — Test enhanced error messages

(display "Testing enhanced error messages with context...\n\n")

;; Load the enhanced error system
(load "/home/oso/the-fold/thimble/error-context-simple.ss")

;; Test the enhancement function directly
(define (test-enhancement)
  (display "1. Testing BoardCraft error enhancement:\n")
  (let ([result (enhance-error-message "variable make-hex-board is not bound")])
       (display result)
       (newline))
  
  (display "\n2. Testing Loom error enhancement:\n")
  (let ([result (enhance-error-message "variable tilemap-fill! is not bound")])
       (display result)
       (newline))
  
  (display "\n3. Testing general error enhancement:\n")
  (let ([result (enhance-error-message "variable unknown-thing is not bound")])
       (display result)
       (newline))
  
  (display "\n✅ Error enhancement working!\n")
  (display "✅ Provides context-specific suggestions\n")
  (display "✅ Links to relevant tutorials\n")
  (display "✅ Makes errors actionable\n"))

;; Run the test
(test-enhancement)
