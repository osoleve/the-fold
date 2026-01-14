;;; hello-world.ss --- Your First Fold Program
;;;
;;; The classic first program, demonstrating basic output.
;;;
;;; Run with: scheme --script docs/examples/getting-started/hello-world.ss

;;; ====
;;; Basic Output
;;; ====

;;; display writes its argument to standard output
(display "Hello, World!")
(newline)

;;; ====
;;; Multiple Values
;;; ====

;;; You can display multiple things in sequence
(display "The Fold is ")
(display "content-addressable")
(newline)

;;; ====
;;; Using format for structured output
;;; ====

;;; format is more flexible for complex output
(format #t "~a + ~a = ~a~%" 2 3 (+ 2 3))

;;; format directives:
;;;   ~a  - display (human-readable)
;;;   ~s  - write (machine-readable, with quotes)
;;;   ~%  - newline
;;;   ~~  - literal tilde

;;; ====
;;; Displaying data structures
;;; ====

(display "A list: ")
(display '(1 2 3))
(newline)

(display "A vector: ")
(display (vector 'a 'b 'c))
(newline)

;;; ====
;;; Summary
;;; ====

(display "\nCongratulations! You've run your first Fold program.\n")
