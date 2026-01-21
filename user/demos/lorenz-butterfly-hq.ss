;;; user/demos/lorenz-butterfly-hq.ss --- High quality Lorenz butterfly
;;;
;;; Larger resolution, more frames, more points - the cinema version.
;;; This is for sharing and showcasing.

(load "lattice/sim/dynamics/attractor-render.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

(display "=======================================================\n")
(display "  LORENZ BUTTERFLY - High Quality Export\n")
(display "=======================================================\n\n")

;;; High-quality configuration
(define width 160)     ; 1280x720 when rendered (HD)
(define height 51)     ; Matches 720p aspect
(define n-points 12000) ; More trajectory detail
(define n-frames 72)    ; Extra smooth at 12fps (6 second loop)

(display "Configuration:\n")
(display "  Resolution: ")
(display width)
(display "x")
(display height)
(display " chars (")
(display (* width 8))
(display "x")
(display (* height 14))
(display " pixels)\n")
(display "  Trajectory points: ")
(display n-points)
(display "\n")
(display "  Animation frames: ")
(display n-frames)
(display " (6 seconds at 12fps)\n\n")

(display "Generating high-detail trajectory...\n")
(flush-output-port (current-output-port))

(define trajectory
  (generate-attractor lorenz-classic
                      (vector 1.0 1.0 1.0)
                      0.005 n-points 1000))

(display "Rendering ")
(display n-frames)
(display " high-resolution frames...\n")
(display "(This will take a moment...)\n")
(flush-output-port (current-output-port))

;;; Strip ANSI codes
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
          (loop (cdr chars) result #f)
          (loop (cdr chars) result #t))]
     [else
      (loop (cdr chars) (cons (car chars) result) #f)])))

;;; Convert ANSI string to frame
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

;;; Generate spinning frames with larger scale
(define ansi-frames
  (render-spinning-attractor-colored trajectory width height 18 n-frames))

(display "\nConverting to video format...\n")
(flush-output-port (current-output-port))

(define video (make-video))

(let loop ([frames ansi-frames] [i 0])
  (when (pair? frames)
    (when (zero? (modulo i 12))
      (display "  Frame ")
      (display i)
      (display "/")
      (display n-frames)
      (display "\n")
      (flush-output-port (current-output-port)))
    (let ([frame (ansi-string->frame (car frames) width height)])
      (video-add-frame! video frame))
    (loop (cdr frames) (+ i 1))))

(display "\nExporting to MP4 (H.264, high quality)...\n")
(flush-output-port (current-output-port))

;;; Export as MP4 for better quality at this resolution
(define output-path "user/demos/lorenz-butterfly-hq.mp4")
(video->mp4 video output-path 12)

(display "\n=======================================================\n")
(display "  High-quality export complete!\n")
(display "  Output: ")
(display output-path)
(display "\n")
(display "  Resolution: 1280x714 pixels\n")
(display "  Duration: 6 seconds (72 frames @ 12fps)\n")
(display "=======================================================\n\n")
