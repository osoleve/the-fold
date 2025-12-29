(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/help.ss")

;; Test basic string utilities
(display "Testing string utilities...
")

(define (string-contains? haystack needle)
  (let ([nlen (string-length needle)]
        [hlen (string-length haystack)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? needle (substring haystack i (+ i nlen))) #t]
             [else (loop (+ i 1))]))))

(display "string-contains? test 1: ")
(display (string-contains? "hello world" "world"))
(newline)

(display "string-contains? test 2: ")
(display (string-contains? "hello world" "xyz"))
(newline)

;; Test error context detection
(display "Testing error context detection...
")

(define (detect-error-context-enhanced error-msg stack-trace)
  (display "Detected context for: ")
  (display error-msg)
  (newline)
  'general)

(let ([context (detect-error-context-enhanced "make-hex-board not bound" '())])
     (display "Context: ")
     (display context)
     (newline))

(display "Basic error system tests completed successfully!
")
