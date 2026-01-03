;;; user/creations/sierpinski.ss — ASCII Sierpinski Triangle Generator
;;;
;;; Generates Sierpinski triangle fractals using ASCII art.
;;; Demonstrates recursion and pattern generation in The Fold.
;;;
;;; Author: ClaudeExplorer (player)
;;; Created: 2026-01-03
;;;
;;; Usage:
;;;   (load "user/creations/sierpinski.ss")
;;;   (sierpinski 4)  ; Generate depth-4 triangle

(define (repeat-string str n)
  "Repeat a string n times"
  (if (<= n 0)
      ""
      (string-append str (repeat-string str (- n 1)))))

(define (make-spaces n)
  "Create n spaces"
  (repeat-string " " n))

(define (sierpinski-line depth level)
  "Generate one line of the Sierpinski triangle"
  (let* ([total-width (expt 2 depth)]
         [triangles (expt 2 level)]
         [triangle-width (/ total-width triangles)]
         [gap-width triangle-width])
        (let loop ([i 0] [result ""])
             (if (>= i triangles)
                 result
                 (loop (+ i 1)
                       (string-append result
                                      (if (even? i) "/\\" (make-spaces 2))
                                      (make-spaces (- gap-width 2))))))))

(define (sierpinski-recursive depth level row)
  "Recursive helper for generating triangle rows"
  (if (< row (expt 2 level))
      (begin
       (let* ([total-width (expt 2 depth)]
              [indent (/ (- total-width (expt 2 (+ level 1))) 2)])
             (display (make-spaces indent))
             (display (sierpinski-line depth level))
             (newline))
       (sierpinski-recursive depth level (+ row 1)))
      (if (< level depth)
          (sierpinski-recursive depth (+ level 1) 0))))

(define (sierpinski depth)
  "Generate and display a Sierpinski triangle of given depth (0-5 recommended)"
  (display "\n")
  (display "Sierpinski Triangle (depth ")
  (display depth)
  (display ")")
  (newline)
  (display (repeat-string "=" 60))
  (newline)
  (if (or (< depth 0) (> depth 6))
      (display "Error: Depth must be between 0 and 6\n")
      (begin
       (let ([total-width (expt 2 depth)])
            (display (make-spaces (- total-width 1)))
            (display "/\\")
            (newline))
       (sierpinski-recursive depth 0 0)
       (display (repeat-string "=" 60))
       (newline)
       (display "\n"))))

(define (demo)
  "Show a few examples"
  (sierpinski 2)
  (sierpinski 3)
  (sierpinski 4))

;; Quick helper
(define (s n) (sierpinski n))
