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
;;; Quick Demo (just bouncing ball)
;;; ============================================================

(display "\n=== ASCII Video Example ===\n\n")

;; Create a small bouncing ball demo
(define *ball-video* (make-bouncing-ball-video 40 12 20))

;; Show just 3 key frames (fast, won't timeout)
(display "Bouncing ball - frames 0, 10, 19:\n\n")
(video-show-frame *ball-video* 0)
(display "\n  ... ball moves ...\n\n")
(video-show-frame *ball-video* 10)
(display "\n  ... bounces off wall ...\n\n")
(video-show-frame *ball-video* 19)

(display "\n=== Available Functions ===\n")
(display "(make-bouncing-ball-video w h frames)\n")
(display "(make-text-scroller-video w h text frames)\n")
(display "(make-spinner-video w h frames)\n")
(display "\n=== Playback ===\n")
(display "(video-show-frame video n)   ; Show one frame (safe)\n")
(display "(video-flipbook video step)  ; Show key frames\n")
(display "(video-play video ms)        ; Animate (terminal only)\n")
