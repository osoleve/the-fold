(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/help.ss")

;; Simple test
(display "Testing basic error formatting...
")
(let ([err (make-condition 'test "not bound" '())])
     (display "Error: ")
     (display (condition-message err))
     (newline))
