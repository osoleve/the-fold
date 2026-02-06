;;; Mountain Dreams Animation - Using ascii-video system
;;; Twinkling stars over static mountains

(load "core/base/prelude.ss")
(load "lattice/ui/layout.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

(define *width* 60)
(define *height* 18)

;;; Generate a single frame at time t (0.0 to 1.0)
(define (render-mountain-frame t)
  (let ([canvas (make-canvas *width* *height*)])
       
       ;; Twinkling stars - different phases
       (let loop ([i 0])
            (when (< i 50)
                  (let* ([base-x (* i 17)]
                         [base-y (* i 7)]
                         [phase (+ (* i 0.3) (* t 6.28))]
                         [twinkle (abs (sin phase))]
                         [x (modulo (+ base-x (inexact->exact (floor (* t 5)))) *width*)]
                         [y (modulo (+ base-y 1) 8)]
                         [char (cond
                                [(< twinkle 0.3) #\.]
                                [(< twinkle 0.7) #\*]
                                [else #\+])])
                        (when (and (>= x 0) (< x *width*) (>= y 0) (< y 8))
                              (canvas-set! canvas x y char))
                        (loop (+ i 1)))))
       
       ;; Distant mountains (light)
       (let loop ([x 0])
            (when (< x *width*)
                  (let* ([wave (+ 9 (* 2 (sin (/ x 10.0))))]
                         [peak (inexact->exact (floor wave))])
                        (let fill ([y peak])
                             (when (< y 14)
                                   (canvas-set! canvas x y #\░)
                                   (fill (+ y 1)))))
                  (loop (+ x 1))))
       
       ;; Near mountains (darker)
       (let loop ([x 0])
            (when (< x *width*)
                  (let* ([wave1 (* 3 (sin (/ x 7.0)))]
                         [wave2 (* 2 (cos (/ x 4.0)))]
                         [h (+ 11 wave1 wave2)]
                         [peak (inexact->exact (floor h))])
                        (let fill ([y peak])
                             (when (< y 15)
                                   (canvas-set! canvas x y (if (< y (+ peak 1)) #\▒ #\▓))
                                   (fill (+ y 1)))))
                  (loop (+ x 1))))
       
       ;; Foreground ridge
       (let loop ([x 0])
            (when (< x *width*)
                  (let* ([wave (* 1.5 (sin (/ x 5.0)))]
                         [h (+ 14 wave)]
                         [peak (inexact->exact (floor h))])
                        (let fill ([y peak])
                             (when (< y 16)
                                   (canvas-set! canvas x y #\█)
                                   (fill (+ y 1)))))
                  (loop (+ x 1))))
       
       ;; Water
       (let loop ([x 0])
            (when (< x *width*)
                  (canvas-set! canvas x 16 #\~)
                  (canvas-set! canvas x 17 #\≈)
                  (loop (+ x 1))))
       
       ;; Pulsing moon
       (let* ([moon-phase (abs (sin (* t 6.28)))]
              [moon-char (if (< moon-phase 0.5) #\◐ #\○)])
             (canvas-set! canvas 48 2 moon-char))
       
       ;; Border
       (canvas-set! canvas 0 0 #\┌)
       (canvas-set! canvas 59 0 #\┐)
       (canvas-set! canvas 0 17 #\└)
       (canvas-set! canvas 59 17 #\┘)
       
       ;; Top border
       (let loop ([x 1])
            (when (< x 17)
                  (canvas-set! canvas x 0 #\─)
                  (loop (+ x 1))))
       (let loop ([x 43])
            (when (< x 59)
                  (canvas-set! canvas x 0 #\─)
                  (loop (+ x 1))))
       
       ;; Title
       (draw-string canvas (point 17 0) " MOUNTAIN DREAMS ")
       
       (canvas->string canvas)))

;;; Convert string to frame (vector of strings, one per line)
(define (string->frame-vec str)
  (let* ([lines (let loop ([s str] [acc '()])
                     (let ([newline-pos (string-contains s "\n")])
                          (if newline-pos
                              (loop (substring s (+ newline-pos 1) (string-length s))
                                    (cons (substring s 0 newline-pos) acc))
                              (reverse (cons s acc)))))]
         [vec (make-vector (length lines))])
        (let fill ([i 0] [lst lines])
             (when (not (null? lst))
                   (vector-set! vec i (car lst))
                   (fill (+ i 1) (cdr lst))))
        vec))

;;; Helper: string-contains
(define (string-contains str substr)
  (let ([len (string-length str)]
        [sublen (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sublen) len) #f]
             [(string=? (substring str i (+ i sublen)) substr) i]
             [else (loop (+ i 1))]))))

;;; Create the video
(define (create-mountain-animation)
  (display "Generating mountain animation frames...\n")
  (let* ([num-frames 24]
         [video (make-video)])
        ;; Add frames one by one
        (let loop ([i 0])
             (when (< i num-frames)
                   (let* ([t (/ i (exact->inexact num-frames))]
                          [frame-string (render-mountain-frame t)]
                          [frame (string->frame-vec frame-string)])
                         (display (string-append "  Frame " (number->string i) "/" (number->string num-frames) "\r"))
                         (video-add-frame! video frame)
                         (loop (+ i 1)))))
        (display "\nExporting to GIF...\n")
        (video->gif video "user/creations/mountain-dreams.gif" 100)
        (display "Done! Created user/creations/mountain-dreams.gif\n")
        video))

;; Run it
(create-mountain-animation)
