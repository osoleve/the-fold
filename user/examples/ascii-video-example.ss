;;; user/examples/ascii-video-example.ss --- ASCII Video Buffer Example
;;;
;;; A complete end-to-end example showing how to use the ASCII video system.
;;; Creates a simple bouncing ball animation.
;;;
;;; Usage:
;;;   scheme --script user/examples/ascii-video-example.ss
;;;
;;; In interactive mode:
;;;   (load "user/examples/ascii-video-example.ss")
;;;   (video-play *ball-video* 50)  ; Play at 50ms/frame

(load "user/creations/ascii-video.ss")

;;; ============================================================
;;; Example 1: Bouncing Ball Animation
;;; ============================================================

(define (make-bouncing-ball-video width height frames)
  (let ([video (make-video)]
        [frame (make-frame width height #\space)]
        ;; Ball physics
        [x 5.0]
        [y 3.0]
        [vx 1.2]
        [vy 0.8])
       
       (do ([i 0 (+ i 1)])
           ((>= i frames))
           
           ;; Clear and draw border
           (frame-clear! frame #\space)
           (frame-draw-box! frame 0 0 width height)
           
           ;; Draw the ball
           (let ([bx (inexact->exact (round x))]
                 [by (inexact->exact (round y))])
                (when (and (> bx 0) (< bx (- width 1))
                           (> by 0) (< by (- height 1)))
                      (frame-set! frame bx by #\O)
                      ;; Add a little trail/shadow
                      (frame-set! frame (- bx 1) by #\.)
                      (frame-set! frame bx (- by 1) #\.)))
           
           ;; Status line
           (frame-put-string! frame 2 (- height 2)
                              (string-append "Frame " (number->string i)
                                             " | Pos: (" (number->string (inexact->exact (round x)))
                                             "," (number->string (inexact->exact (round y))) ")"))
           
           ;; Record frame
           (video-add-frame! video frame)
           
           ;; Update physics
           (set! x (+ x vx))
           (set! y (+ y vy))
           
           ;; Bounce off walls
           (when (or (<= x 1) (>= x (- width 2)))
                 (set! vx (- vx))
                 (set! x (max 1 (min (- width 2) x))))
           (when (or (<= y 1) (>= y (- height 2)))
                 (set! vy (- vy))
                 (set! y (max 1 (min (- height 2) y)))))
       
       video))

;;; ============================================================
;;; Example 2: Text Scroller
;;; ============================================================

(define (make-text-scroller-video width height text frames)
  (let ([video (make-video)]
        [frame (make-frame width height #\space)]
        [text-len (string-length text)]
        [start-x width])  ; Start off-screen right
       
       (do ([i 0 (+ i 1)])
           ((>= i frames))
           
           ;; Clear frame
           (frame-clear! frame #\space)
           
           ;; Draw decorative borders
           (do ([col 0 (+ col 1)])
               ((>= col width))
               (frame-set! frame col 0 #\~)
               (frame-set! frame col (- height 1) #\~))
           
           ;; Draw scrolling text at middle height
           (let ([text-x (- start-x i)]
                 [text-y (quotient height 2)])
                (do ([j 0 (+ j 1)])
                    ((>= j text-len))
                    (let ([x (+ text-x j)])
                         (when (and (>= x 0) (< x width))
                               (frame-set! frame x text-y
                                           (string-ref text j))))))
           
           ;; Record frame
           (video-add-frame! video frame))
       
       video))

;;; ============================================================
;;; Example 3: Spinner Animation
;;; ============================================================

(define (make-spinner-video width height frames)
  (let ([video (make-video)]
        [frame (make-frame width height #\space)]
        [spinner-chars "|/-\\"]
        [cx (quotient width 2)]
        [cy (quotient height 2)])
       
       (do ([i 0 (+ i 1)])
           ((>= i frames))
           
           ;; Clear frame
           (frame-clear! frame #\space)
           
           ;; Draw box around spinner
           (frame-draw-box! frame (- cx 5) (- cy 2) 11 5)
           
           ;; Draw spinner character
           (let ([char (string-ref spinner-chars (modulo i 4))])
                (frame-set! frame cx cy char))
           
           ;; Label
           (frame-put-string! frame (- cx 4) (+ cy 1) "Loading...")
           
           ;; Record frame
           (video-add-frame! video frame))
       
       video))

;;; ============================================================
;;; Run Examples
;;; ============================================================

(display "\n")
(display "========================================\n")
(display "   ASCII Video Buffer Examples\n")
(display "========================================\n\n")

;; Example 1: Bouncing Ball
(display "Creating bouncing ball animation (40 frames)...\n")
(define *ball-video* (make-bouncing-ball-video 40 15 40))
(display "Done! Showing flipbook:\n\n")
(video-flipbook *ball-video* 10)

;; Example 2: Text Scroller
(display "\n----------------------------------------\n")
(display "Creating text scroller (30 frames)...\n")
(define *scroller-video* (make-text-scroller-video 30 5 "Hello from The Fold!" 30))
(display "Done! Showing flipbook:\n\n")
(video-flipbook *scroller-video* 6)

;; Example 3: Spinner
(display "\n----------------------------------------\n")
(display "Creating spinner animation (16 frames)...\n")
(define *spinner-video* (make-spinner-video 20 7 16))
(display "Done! Showing all frames:\n\n")
(video-print *spinner-video*)

;; Instructions
(display "\n========================================\n")
(display "   Interactive Playback\n")
(display "========================================\n\n")
(display "Videos are stored in:\n")
(display "  *ball-video*     - Bouncing ball (40 frames)\n")
(display "  *scroller-video* - Text scroller (30 frames)\n")
(display "  *spinner-video*  - Loading spinner (16 frames)\n")
(display "\n")
(display "In a terminal with ANSI support, try:\n")
(display "  (video-play *ball-video* 100)        ; 100ms per frame\n")
(display "  (video-play *spinner-video* 150)     ; Slower spinner\n")
(display "  (video-play-loop *spinner-video* 100 5) ; Loop 5 times\n")
(display "\n")
