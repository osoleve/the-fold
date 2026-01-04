;;; user/creations/gradient-descent-animated.ss --- Animated Gradient Descent
;;;
;;; Watch a particle roll down a bumpy bowl in real-time ASCII animation!

(load "user/creations/ascii-video.ss")
(load "core/autodiff/grad-store.ss")

;;; ============================================================
;;; The Landscape
;;; ============================================================

(define (landscape x y)
  (+ (* x x)
     (* y y)
     (* 0.5 (sin (* 3 x)) (cos (* 3 y)))))

(define (landscape-grad x y)
  (let ([dx (+ (* 2 x) (* 1.5 (cos (* 3 x)) (cos (* 3 y))))]
        [dy (- (* 2 y) (* 1.5 (sin (* 3 x)) (sin (* 3 y))))])
       (cons dx dy)))

;;; ============================================================
;;; Rendering the Landscape to a Frame
;;; ============================================================

(define *landscape-chars* " .'`^\",:;Il!i><~+_-?][}{1)(|\\/tfjrxnuvczXYUJCLQ0OZmwqpdbkhao*#MW&8%B@$")

(define (render-landscape frame x-min x-max y-min y-max particle-x particle-y)
  (let* ([w (frame-width frame)]
         [h (frame-height frame)]
         [x-step (/ (- x-max x-min) w)]
         [y-step (/ (- y-max y-min) h)]
         [char-count (string-length *landscape-chars*)])
        
        (do ([row 0 (+ row 1)])
            ((>= row h))
            (let ([y (+ y-min (* row y-step))])
                 (do ([col 0 (+ col 1)])
                     ((>= col w))
                     (let* ([x (+ x-min (* col x-step))]
                            [z (landscape x y)]
                            [idx (min (- char-count 1)
                                      (max 0 (inexact->exact (round (* 10 z)))))]
                            [is-particle (and (< (abs (- x particle-x)) (* 1.5 x-step))
                                              (< (abs (- y particle-y)) (* 1.5 y-step)))])
                           (frame-set! frame col row
                                       (if is-particle #\O
                                           (string-ref *landscape-chars* idx)))))))))

;;; ============================================================
;;; Status Bar
;;; ============================================================

(define (render-status frame y epoch loss grad-norm px py)
  (let ([status (string-append
                 "Epoch: " (number->string epoch)
                 " | Loss: " (number->string (round-to loss 4))
                 " | |grad|: " (number->string (round-to grad-norm 4))
                 " | Pos: (" (number->string (round-to px 2))
                 ", " (number->string (round-to py 2)) ")")])
       (frame-put-string! frame 0 y (make-string (frame-width frame) #\space))
       (frame-put-string! frame 1 y status)))

(define (render-progress-bar frame y value max-value width)
  (let* ([ratio (min 1.0 (max 0.0 (/ value max-value)))]
         [filled (inexact->exact (round (* ratio width)))]
         [empty (- width filled)]
         [bar (string-append "[" (make-string filled #\#) (make-string empty #\-) "]")])
        (frame-put-string! frame 1 y bar)))

(define (round-to n places)
  (let ([factor (expt 10 places)])
       (/ (round (* n factor)) factor)))

;;; ============================================================
;;; Gradient Descent with Recording
;;; ============================================================

(define (record-gradient-descent start-x start-y epochs learning-rate
                                 frame-width frame-height
                                 x-min x-max y-min y-max)
  (let ([video (make-video)]
        [frame (make-frame frame-width frame-height #\space)]
        [landscape-height (- frame-height 4)])  ; Reserve 4 rows for status
       
       ;; Initial frame
       (render-landscape frame x-min x-max y-min y-max start-x start-y)
       (frame-put-string! frame 0 landscape-height (make-string frame-width #\=))
       (render-status frame (+ landscape-height 1) 0
                      (landscape start-x start-y) 0 start-x start-y)
       (frame-put-string! frame 0 (+ landscape-height 2) "Starting gradient descent...")
       (video-add-frame! video frame)
       
       ;; Training loop
       (let loop ([px start-x]
                  [py start-y]
                  [epoch 0])
            (if (>= epoch epochs)
                video
                (let* ([grad (landscape-grad px py)]
                       [dx (car grad)]
                       [dy (cdr grad)]
                       [grad-norm (sqrt (+ (* dx dx) (* dy dy)))]
                       [new-px (- px (* learning-rate dx))]
                       [new-py (- py (* learning-rate dy))]
                       [loss (landscape new-px new-py)])
                      
                      ;; Render frame
                      (frame-clear! frame #\space)
                      (render-landscape frame x-min x-max y-min y-max new-px new-py)
                      (frame-put-string! frame 0 landscape-height (make-string frame-width #\=))
                      (render-status frame (+ landscape-height 1) epoch loss grad-norm new-px new-py)
                      (render-progress-bar frame (+ landscape-height 2) loss 6.0 (- frame-width 4))
                      (frame-put-string! frame 0 (+ landscape-height 3)
                                         (if (< grad-norm 0.01)
                                             "Converged! Particle found minimum."
                                             "Rolling down the gradient..."))
                      
                      (video-add-frame! video frame)
                      (loop new-px new-py (+ epoch 1)))))))

;;; ============================================================
;;; Main Demo
;;; ============================================================

(define (run-animated-demo)
  (display "\n")
  (display "Recording gradient descent animation...\n")
  (display "This may take a moment.\n\n")
  
  (let ([video (record-gradient-descent
                2.0 1.5      ; start position
                60           ; epochs
                0.1          ; learning rate
                70 28        ; frame size
                -3.0 3.0     ; x range
                -3.0 3.0)])  ; y range
       
       (display "Recorded ")
       (display (video-frame-count video))
       (display " frames.\n\n")
       
       ;; Show flipbook
       (display "=== FLIPBOOK (key frames) ===\n\n")
       (video-flipbook video 10)
       
       (display "\n=== ANIMATION INFO ===\n")
       (display "To play with animation, run:\n")
       (display "  (video-play video 100)  ; 100ms per frame\n")
       (display "\nOr loop it:\n")
       (display "  (video-play-loop video 50 3)  ; 3 loops, 50ms/frame\n")
       
       ;; Return video for interactive use
       video))

;; Run and save result
(define *demo-video* (run-animated-demo))

(display "\nVideo stored in *demo-video*\n")
(display "In a terminal with ANSI support, try:\n")
(display "  (video-play *demo-video* 80)\n")
