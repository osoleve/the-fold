;;; user/demos/lorenz-signature.ss --- The perfect loop
;;;
;;; The canonical Lorenz butterfly demo.
;;; 100x42 chars (800x600), 60 frames, optimized for web display.
;;; This is THE demo to show when someone asks "what can The Fold do?"

(load "lattice/sim/dynamics/attractor-render.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

(display "\n")
(display "╔═══════════════════════════════════════════════════════════╗\n")
(display "║                                                           ║\n")
(display "║         LORENZ BUTTERFLY - Signature Demo                ║\n")
(display "║                                                           ║\n")
(display "║   The strange attractor that launched a thousand papers  ║\n")
(display "║   Now spinning in your terminal                          ║\n")
(display "║                                                           ║\n")
(display "╚═══════════════════════════════════════════════════════════╝\n\n")

;;; The perfect configuration
(define width 100)
(define height 42)
(define n-points 10000)
(define n-frames 60)  ; Divisible by 12, 15, 20, 30, 60 fps

(display "Generating trajectory (10,000 points)...\n")
(flush-output-port (current-output-port))

(define trajectory
  (generate-attractor lorenz-classic
                      (vector 1.0 1.0 1.0)
                      0.005 n-points 1000))

(display "Rendering 60-frame animation...\n")
(flush-output-port (current-output-port))

(define (strip-ansi str)
  (let loop ([chars (string->list str)]
             [result '()]
             [in-escape #f])
    (cond
     [(null? chars) (list->string (reverse result))]
     [(char=? (car chars) #\x1b) (loop (cdr chars) result #t)]
     [in-escape
      (if (char-alphabetic? (car chars))
          (loop (cdr chars) result #f)
          (loop (cdr chars) result #t))]
     [else (loop (cdr chars) (cons (car chars) result) #f)])))

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
                    (split (cdr chars) '() (cons (list->string (reverse current)) result))]
                   [else
                    (split (cdr chars) (cons (car chars) current) result)]))]
         [frame (make-frame width height #\space)])
    (let row-loop ([y 0] [lines lines])
      (when (and (< y height) (pair? lines))
        (let* ([line (strip-ansi (car lines))]
               [len (min width (string-length line))])
          (do ([x 0 (+ x 1)])
              ((>= x len))
              (frame-set! frame x y (string-ref line x))))
        (row-loop (+ y 1) (cdr lines))))
    frame))

(define ansi-frames
  (render-spinning-attractor-colored trajectory width height 13 n-frames))

(display "Converting to video...\n")
(flush-output-port (current-output-port))

(define video (make-video))

(let loop ([frames ansi-frames] [i 0])
  (when (pair? frames)
    (when (zero? (modulo i 10))
      (display ".")
      (flush-output-port (current-output-port)))
    (video-add-frame! video (ansi-string->frame (car frames) width height))
    (loop (cdr frames) (+ i 1))))

(display "\n\nExporting GIF (optimized for web)...\n")
(flush-output-port (current-output-port))

;;; 12fps = 83ms per frame, smooth and efficient
(video->gif video "user/demos/lorenz-signature.gif" 83)

(display "\n")
(display "═══════════════════════════════════════════════════════════\n")
(display " ✓ Signature demo complete\n")
(display " → user/demos/lorenz-signature.gif\n")
(display " → 800x588 pixels, 60 frames, 5 second loop\n")
(display "═══════════════════════════════════════════════════════════\n\n")
(display "Share this when someone asks:\n")
(display "\"What can The Fold do?\"\n\n")
