;;; user/demos/lorenz-butterfly-demo.ss --- Export spinning Lorenz butterfly
;;;
;;; The signature demo for the chaos module.
;;; Creates a beautiful spinning animation of the Lorenz attractor.

(load "lattice/sim/dynamics/attractor-render.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

(display "=======================================================\n")
(display "  LORENZ BUTTERFLY - Spinning Strange Attractor Demo\n")
(display "=======================================================\n\n")

;;; Configuration
(define width 100)    ; 100 chars wide (matches default terminal)
(define height 42)    ; 42 rows (800x600 aesthetic)
(define n-points 8000)
(define n-frames 48)  ; 48 frames = smooth rotation at 12fps

(display "Generating Lorenz attractor trajectory...\n")
(flush-output-port (current-output-port))

(define trajectory
  (generate-attractor lorenz-classic
                      (vector 1.0 1.0 1.0)
                      0.005 n-points 1000))

(display "Trajectory generated: ")
(display (length trajectory))
(display " points\n\n")

(display "Rendering ")
(display n-frames)
(display " frames of spinning animation...\n")
(flush-output-port (current-output-port))

;;; Strip ANSI codes from string
(define (strip-ansi str)
  (let loop ([chars (string->list str)]
             [result '()]
             [in-escape #f])
    (cond
     [(null? chars)
      (list->string (reverse result))]
     [(char=? (car chars) #\x1b)
      (loop (cdr chars) result #t)]
     [in-escape
      (if (char-alphabetic? (car chars))
          (loop (cdr chars) result #f)  ; End of ANSI sequence
          (loop (cdr chars) result #t))]
     [else
      (loop (cdr chars) (cons (car chars) result) #f)])))

;;; Convert ANSI-colored string to ASCII video frame
(define (ansi-string->frame str width height)
  (let* ([lines (let split ([chars (string->list str)]
                             [current '()]
                             [result '()])
                  (cond
                   [(null? chars)
                    (if (null? current)
                        (reverse result)
                        (reverse (cons (list->string (reverse current)) result)))]
                   [(char=? (car chars) #\newline)
                    (split (cdr chars)
                           '()
                           (cons (list->string (reverse current)) result))]
                   [else
                    (split (cdr chars) (cons (car chars) current) result)]))]
         [frame (make-frame width height #\space)])
    ;; Strip ANSI codes and copy to frame
    (let row-loop ([y 0] [lines lines])
      (when (and (< y height) (pair? lines))
        (let* ([line (strip-ansi (car lines))]
               [len (min width (string-length line))])
          (do ([x 0 (+ x 1)])
              ((>= x len))
              (frame-set! frame x y (string-ref line x))))
        (row-loop (+ y 1) (cdr lines))))
    frame))

;;; Generate all frames
(define ansi-frames
  (render-spinning-attractor-colored trajectory width height 13 n-frames))

(display "Converting to video format...\n")
(flush-output-port (current-output-port))

;;; Convert to ascii-video format
(define video (make-video))

(let loop ([frames ansi-frames] [i 0])
  (when (pair? frames)
    (when (zero? (modulo i 8))
      (display "  Frame ")
      (display i)
      (display "/")
      (display n-frames)
      (display "\n")
      (flush-output-port (current-output-port)))
    (let ([frame (ansi-string->frame (car frames) width height)])
      (video-add-frame! video frame))
    (loop (cdr frames) (+ i 1))))

(display "\nExporting to GIF...\n")
(flush-output-port (current-output-port))

;;; Export to GIF (12fps = ~83ms per frame)
(define output-path "user/demos/lorenz-butterfly.gif")
(video->gif video output-path 83)

(display "\n=======================================================\n")
(display "  Demo complete!\n")
(display "  Output: ")
(display output-path)
(display "\n")
(display "=======================================================\n")
(display "\nThe Lorenz butterfly spins.\n")
(display "Chaos rendered in ASCII.\n")
(display "The Fold remembers.\n\n")
