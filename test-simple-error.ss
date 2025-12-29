(load "thimble/error-improvements.ss")
(display "Testing enhanced error system...
")

;; Test a simple error
(let ([err (make-condition 'test-func "not bound" '())])
     (display (format-enhanced-error err)))
