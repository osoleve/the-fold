;;; user/creations/mandala.ss — ASCII Mandala Generator
;;;
;;; Generates symmetric ASCII mandalas with various patterns.
;;; Demonstrates pattern generation and symmetry in The Fold.
;;;
;;; Author: ClaudeExplorer (player)
;;; Created: 2026-01-03
;;;
;;; Usage:
;;;   (load "user/creations/mandala.ss")
;;;   (mandala 'circles 7)
;;;   (mandala 'stars 9)
;;;   (mandala 'waves 11)

(define (char-at x y pattern size)
  "Determine character at position (x,y) for given pattern"
  (let* ([cx (/ size 2)]
         [cy (/ size 2)]
         [dx (- x cx)]
         [dy (- y cy)]
         [dist (sqrt (+ (* dx dx) (* dy dy)))]
         [angle (atan dy dx)]
         [r (inexact->exact (floor dist))])
        (case pattern
              [(circles) (if (even? r) #\* #\space)]
              [(stars) (if (< (abs (sin (* 6 angle))) 0.3) #\* #\space)]
              [(waves) (if (< (abs (sin (+ (* 2 angle) (* 0.5 dist)))) 0.4) #\+ #\space)]
              [(diamond) (if (< (+ (abs dx) (abs dy)) (/ size 2)) #\# #\space)]
              [(cross) (if (or (< (abs dx) 2) (< (abs dy) 2)) #\| #\space)]
              [(spiral)
               (let ([theta (+ angle (* 0.3 dist))])
                    (if (< (abs (sin (* 4 theta))) 0.3) #\@ #\space))]
              [else #\?])))

(define (draw-mandala pattern size)
  "Draw a mandala of given pattern and size"
  (let loop-y ([y 0])
       (when (< y size)
             (let loop-x ([x 0])
                  (when (< x size)
                        (display (char-at x y pattern size))
                        (loop-x (+ x 1))))
             (newline)
             (loop-y (+ y 1)))))

(define (mandala pattern size)
  "Generate and display a mandala"
  (display "\n")
  (display "Mandala: ")
  (display pattern)
  (display " (size ")
  (display size)
  (display ")")
  (newline)
  (display (make-string 60 #\=))
  (newline)
  (if (or (< size 5) (> size 50))
      (display "Error: Size must be between 5 and 50 (odd numbers work best)\n")
      (begin
       (draw-mandala pattern size)
       (display (make-string 60 #\=))
       (newline)
       (newline))))

(define (gallery)
  "Show all available patterns"
  (mandala 'circles 15)
  (mandala 'stars 15)
  (mandala 'waves 15)
  (mandala 'diamond 15)
  (mandala 'cross 15)
  (mandala 'spiral 15))

(define (random-mandala)
  "Generate a random mandala"
  (let ([patterns '(circles stars waves diamond cross spiral)]
        [sizes '(11 13 15 17 19)])
       (mandala (list-ref patterns (random (length patterns)))
                (list-ref sizes (random (length sizes))))))

;; Quick helpers
(define (m pat size) (mandala pat size))
(define (rm) (random-mandala))
