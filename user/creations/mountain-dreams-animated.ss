;;; Mountain Dreams Animated - Twinkling stars over mountains
;;; Created by ClaudeOpus

(load "shell/ui/layout.ss")

(define width 60)
(define height 18)

;;; Generate a frame with animated stars
(define (create-frame frame-num total-frames)
  (define scene (make-canvas width height))
  
  ;; Animate stars - they drift and twinkle
  (let loop ([i 0])
       (when (< i 60)
             (let* ([base-x1 (* i 17)]
                    [base-y1 (* i 7)]
                    [base-x2 (* i 23)]
                    [base-y2 (* i 11)]
                    ;; Add movement based on frame
                    [offset (/ frame-num 4.0)]
                    [x1 (modulo (+ base-x1 (inexact->exact (floor offset))) width)]
                    [y1 (modulo (+ base-y1 1) 8)]
                    [x2 (modulo (+ base-x2 (inexact->exact (floor (* offset 1.5)))) width)]
                    [y2 (modulo base-y2 6)]
                    ;; Twinkle effect - vary character based on frame
                    [twinkle-phase (modulo (+ i frame-num) 6)]
                    [char1 (cond
                            [(< twinkle-phase 2) #\.]
                            [(< twinkle-phase 4) #\*]
                            [else #\+])]
                    [char2 (if (< twinkle-phase 3) #\. #\*)])
                   (when (and (>= x1 0) (< x1 width) (>= y1 0) (< y1 8))
                         (canvas-set! scene x1 y1 char1))
                   (when (and (>= x2 0) (< x2 width) (>= y2 0) (< y2 6))
                         (canvas-set! scene x2 y2 char2)))
             (loop (+ i 1))))
  
  ;; Distant mountains (static)
  (let loop ([x 0])
       (when (< x width)
             (let* ([wave (+ 9 (* 2 (sin (/ x 10.0))))]
                    [peak (inexact->exact (floor wave))])
                   (let fill ([y peak])
                        (when (< y 14)
                              (canvas-set! scene x y #\░)
                              (fill (+ y 1)))))
             (loop (+ x 1))))
  
  ;; Near mountains (static)
  (let loop ([x 0])
       (when (< x width)
             (let* ([wave1 (* 3 (sin (/ x 7.0)))]
                    [wave2 (* 2 (cos (/ x 4.0)))]
                    [h (+ 11 wave1 wave2)]
                    [peak (inexact->exact (floor h))])
                   (let fill ([y peak])
                        (when (< y 15)
                              (canvas-set! scene x y (if (< y (+ peak 1)) #\▒ #\▓))
                              (fill (+ y 1)))))
             (loop (+ x 1))))
  
  ;; Foreground ridge (static)
  (let loop ([x 0])
       (when (< x width)
             (let* ([wave (* 1.5 (sin (/ x 5.0)))]
                    [h (+ 14 wave)]
                    [peak (inexact->exact (floor h))])
                   (let fill ([y peak])
                        (when (< y 16)
                              (canvas-set! scene x y #\█)
                              (fill (+ y 1)))))
             (loop (+ x 1))))
  
  ;; Water (static)
  (let loop ([x 0])
       (when (< x width)
             (canvas-set! scene x 16 #\~)
             (canvas-set! scene x 17 #\≈)
             (loop (+ x 1))))
  
  ;; Moon with subtle breathing effect
  (let* ([moon-brightness (modulo frame-num 8)]
         [moon-char (if (< moon-brightness 4) #\◐ #\○)])
        (canvas-set! scene 48 2 moon-char))
  
  ;; Frame corners
  (canvas-set! scene 0 0 #\┌)
  (canvas-set! scene 59 0 #\┐)
  (canvas-set! scene 0 17 #\└)
  (canvas-set! scene 59 17 #\┘)
  
  ;; Top border
  (let loop ([x 1])
       (when (< x 17)
             (canvas-set! scene x 0 #\─)
             (loop (+ x 1))))
  (let loop ([x 43])
       (when (< x 59)
             (canvas-set! scene x 0 #\─)
             (loop (+ x 1))))
  
  ;; Title
  (draw-string scene (point 17 0) " MOUNTAIN DREAMS ")
  
  ;; Frame counter (for debugging)
  ;; (draw-string scene (point 2 17) (string-append "F" (number->string frame-num)))
  
  scene)

;;; Generate all frames
(define num-frames 24)  ; 24 frames for smooth animation

(let loop ([frame 0])
     (when (< frame num-frames)
           ;; Create output directory if needed
           (when (= frame 0)
                 (system "mkdir -p user/creations/frames"))
           
           ;; Generate frame
           (let* ([canvas (create-frame frame num-frames)]
                  [filename (string-append "user/creations/frames/frame-"
                                           (if (< frame 10) "0" "")
                                           (number->string frame)
                                           ".txt")])
                 ;; Write frame to file
                 (with-output-to-file filename
                                      (lambda ()
                                              (display (canvas->string canvas)))
                                      'replace))
           
           (loop (+ frame 1))))

(display "Generated ")
(display num-frames)
(display " frames in user/creations/frames/\n")
(display "Run: ./create-gif.sh to create animated GIF\n")
