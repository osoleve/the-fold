;;; user/creations/graphics-showcase.ss — Graphics Library Animation Showcase
;;;
;;; Creates an animated GIF demonstrating the graphics library features:
;;; - Pattern fills (checker, dots, diagonal, hatch)
;;; - Shape primitives (circles, boxes, lines)
;;; - Connection combinators (arrows, edges)
;;; - Layout combinators (composition)
;;;
;;; Usage:
;;;   scheme --script user/creations/graphics-showcase.ss

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

;;; ====
;;; Animation Parameters
;;; ====

(define *width* 50)
(define *height* 20)
(define *frame-count* 60)

;;; ====
;;; Pattern Definitions (for animation)
;;; ====

(define (pattern-char pattern x y)
  (let* ([h (length pattern)]
         [row (list-ref pattern (modulo y h))]
         [w (length row)])
        (list-ref row (modulo x w))))

(define pat-checker '((#\# #\space) (#\space #\#)))
(define pat-dots '((#\. #\space) (#\space #\.)))
(define pat-diagonal '((#\/ #\space) (#\space #\/)))
(define pat-hatch '((#\# #\-) (#\| #\#)))
(define pat-waves '((#\~ #\space #\~ #\space) (#\space #\~ #\space #\~)))

;;; ====
;;; Drawing Helpers
;;; ====

(define (draw-pattern-rect! frame x y w h pattern)
  (do ([row 0 (+ row 1)])
      ((>= row h))
      (do ([col 0 (+ col 1)])
          ((>= col w))
          (frame-set! frame (+ x col) (+ y row)
                      (pattern-char pattern col row)))))

(define (draw-circle! frame cx cy r char)
  (do ([angle 0 (+ angle 0.15)])
      ((>= angle 6.3))
      (let ([x (inexact->exact (round (+ cx (* r (cos angle)))))]
            [y (inexact->exact (round (+ cy (* r 0.5 (sin angle)))))])
           (frame-set! frame x y char))))

(define (draw-filled-circle! frame cx cy r pattern)
  (do ([dy (- r) (+ dy 1)])
      ((> dy r))
      (let ([dx-max (sqrt (- (* r r) (* dy dy)))])
           (do ([dx (- (inexact->exact (round dx-max)))
                    (+ dx 1)])
               ((> dx (inexact->exact (round dx-max))))
               (let ([x (+ cx dx)]
                     [y (+ cy (quotient dy 2))])
                    (when (and (>= x 0) (< x *width*)
                               (>= y 0) (< y *height*))
                          (frame-set! frame x y
                                      (pattern-char pattern (+ cx dx) (+ cy dy)))))))))

(define (draw-arrow! frame x1 y1 x2 y2 char)
  ;; Simple line with arrow
  (let* ([dx (- x2 x1)]
         [dy (- y2 y1)]
         [steps (max (abs dx) (abs dy))])
        (when (> steps 0)
              (do ([i 0 (+ i 1)])
                  ((> i steps))
                  (let ([x (+ x1 (quotient (* i dx) steps))]
                        [y (+ y1 (quotient (* i dy) steps))])
                       (frame-set! frame x y char)))))
  ;; Arrowhead
  (let ([arrow-ch (cond
                   [(and (> (abs (- x2 x1)) (abs (- y2 y1))) (> x2 x1)) #\>]
                   [(and (> (abs (- x2 x1)) (abs (- y2 y1))) (< x2 x1)) #\<]
                   [(> y2 y1) #\v]
                   [else #\^])])
       (frame-set! frame x2 y2 arrow-ch)))

(define (draw-box-styled! frame x y w h style)
  (let ([tl (case style [(light) #\+] [(heavy) #\#] [(double) #\*] [else #\+])]
        [h-char (case style [(light) #\-] [(heavy) #\=] [(double) #\=] [else #\-])]
        [v-char (case style [(light) #\|] [(heavy) #\|] [(double) #\|] [else #\|])])
       ;; Top
       (frame-set! frame x y tl)
       (do ([i 1 (+ i 1)]) ((>= i (- w 1))) (frame-set! frame (+ x i) y h-char))
       (frame-set! frame (+ x w -1) y tl)
       ;; Sides
       (do ([j 1 (+ j 1)]) ((>= j (- h 1)))
           (frame-set! frame x (+ y j) v-char)
           (frame-set! frame (+ x w -1) (+ y j) v-char))
       ;; Bottom
       (frame-set! frame x (+ y h -1) tl)
       (do ([i 1 (+ i 1)]) ((>= i (- w 1))) (frame-set! frame (+ x i) (+ y h -1) h-char))
       (frame-set! frame (+ x w -1) (+ y h -1) tl)))

;;; ====
;;; Animation Scenes
;;; ====

(define (scene-title frame t)
  (frame-clear! frame #\space)
  (frame-put-string! frame 5 4  "  THE FOLD GRAPHICS LIBRARY")
  (frame-put-string! frame 5 5  "  ====")
  (frame-put-string! frame 5 8  "     Shapes | Patterns")
  (frame-put-string! frame 5 9  "     Connections | Transforms")
  (when (> t 0.5)
        (frame-put-string! frame 15 14 "[Press Play]")))

(define (scene-patterns frame t)
  (frame-clear! frame #\space)
  (frame-put-string! frame 2 1 "PATTERN FILLS")
  
  ;; Animate patterns appearing
  (let ([phase (min 1.0 (* t 4))])
       (when (> phase 0.0)
             (let ([w (inexact->exact (round (* 8 phase)))])
                  (draw-pattern-rect! frame 2 3 w 6 pat-checker)))
       (when (> phase 0.25)
             (let ([w (inexact->exact (round (* 8 (- phase 0.25) 4)))])
                  (draw-pattern-rect! frame 13 3 (min 8 w) 6 pat-dots)))
       (when (> phase 0.5)
             (let ([w (inexact->exact (round (* 8 (- phase 0.5) 4)))])
                  (draw-pattern-rect! frame 24 3 (min 8 w) 6 pat-diagonal)))
       (when (> phase 0.75)
             (let ([w (inexact->exact (round (* 8 (- phase 0.75) 4)))])
                  (draw-pattern-rect! frame 35 3 (min 8 w) 6 pat-hatch))))
  
  ;; Labels
  (frame-put-string! frame 2 10 "checker")
  (frame-put-string! frame 13 10 "dots")
  (frame-put-string! frame 24 10 "diagonal")
  (frame-put-string! frame 35 10 "hatch")
  
  ;; Demo circle with pattern
  (when (> t 0.6)
        (frame-put-string! frame 2 13 "Circle fill:")
        (draw-filled-circle! frame 35 15 8 pat-waves)))

(define (scene-connections frame t)
  (frame-clear! frame #\space)
  (frame-put-string! frame 2 1 "CONNECTION COMBINATORS")
  
  ;; Draw boxes
  (draw-box-styled! frame 3 5 10 5 'double)
  (frame-put-string! frame 5 7 "START")
  
  (draw-box-styled! frame 20 5 10 5 'double)
  (frame-put-string! frame 23 7 "MID")
  
  (draw-box-styled! frame 37 5 10 5 'double)
  (frame-put-string! frame 40 7 "END")
  
  ;; Animate arrows appearing
  (when (> t 0.2)
        (draw-arrow! frame 13 7 19 7 #\-))
  (when (> t 0.4)
        (draw-arrow! frame 30 7 36 7 #\-))
  
  ;; Lower diagram with different edge styles
  (when (> t 0.6)
        (draw-box-styled! frame 5 13 8 4 'light)
        (frame-put-string! frame 6 15 "A")
        
        (draw-box-styled! frame 30 13 8 4 'light)
        (frame-put-string! frame 33 15 "B")
        
        ;; Orthogonal edge (animated)
        (let ([progress (min 1.0 (* (- t 0.6) 2.5))])
             (let ([mid-x 22])
                  ;; First horizontal segment
                  (when (> progress 0)
                        (let ([end-x (+ 13 (inexact->exact (round (* (- mid-x 13) (min 1.0 (* progress 3))))))])
                             (do ([x 13 (+ x 1)])
                                 ((> x end-x))
                                 (frame-set! frame x 15 #\-))))
                  ;; Vertical segment
                  (when (> progress 0.33)
                        (frame-set! frame mid-x 15 #\|))
                  ;; Second horizontal
                  (when (> progress 0.66)
                        (let ([end-x (+ mid-x (inexact->exact (round (* (- 29 mid-x) (min 1.0 (* (- progress 0.66) 3))))))])
                             (do ([x mid-x (+ x 1)])
                                 ((> x end-x))
                                 (frame-set! frame x 15 #\-))))
                  (when (>= progress 1.0)
                        (frame-set! frame 29 15 #\>)))))
  
  (frame-put-string! frame 14 17 "'orthogonal edge style"))

(define (scene-shapes frame t)
  (frame-clear! frame #\space)
  (frame-put-string! frame 2 1 "SHAPE PRIMITIVES")
  
  ;; Animated circle
  (let ([r (+ 3 (* 2 (sin (* t 6.28))))])
       (draw-circle! frame 12 10 (inexact->exact (round r)) #\*))
  
  ;; Animated box
  (let ([w (+ 8 (inexact->exact (round (* 4 (sin (* t 3.14))))))])
       (draw-box-styled! frame 25 6 w 8 'heavy))
  
  ;; Pulsing pattern square
  (let ([size (+ 4 (inexact->exact (round (* 2 (cos (* t 4.7))))))])
       (draw-pattern-rect! frame 40 8 size size
                           (if (> (sin (* t 10)) 0) pat-checker pat-dots)))
  
  ;; Labels
  (frame-put-string! frame 8 17 "circle")
  (frame-put-string! frame 26 17 "box")
  (frame-put-string! frame 40 17 "pattern"))

(define (scene-finale frame t)
  (frame-clear! frame #\space)
  
  ;; Spinning pattern background
  (let ([pat (cond
              [(< t 0.25) pat-dots]
              [(< t 0.5) pat-diagonal]
              [(< t 0.75) pat-waves]
              [else pat-checker])])
       (draw-pattern-rect! frame 1 1 48 18 pat))
  
  ;; Central message box
  (draw-box-styled! frame 10 6 30 8 'double)
  (do ([y 7 (+ y 1)]) ((>= y 13))
      (do ([x 11 (+ x 1)]) ((>= x 39))
          (frame-set! frame x y #\space)))
  
  (frame-put-string! frame 12 8  "THE FOLD GRAPHICS")
  (frame-put-string! frame 12 9  "====")
  (frame-put-string! frame 14 11 "Complete!")
  
  ;; Corner decorations
  (frame-put-string! frame 2 2 "***")
  (frame-put-string! frame 45 2 "***")
  (frame-put-string! frame 2 17 "***")
  (frame-put-string! frame 45 17 "***"))

;;; ====
;;; Main Animation Loop
;;; ====

(define (generate-showcase-video)
  (display "Generating graphics showcase animation...\n")
  (let ([video (make-video)]
        [frame (make-frame *width* *height* #\space)])
       
       ;; Scene 1: Title (frames 0-14)
       (do ([i 0 (+ i 1)])
           ((>= i 15))
           (let ([t (/ i 14.0)])
                (scene-title frame t)
                (video-add-frame! video frame)))
       
       ;; Scene 2: Patterns (frames 15-29)
       (do ([i 0 (+ i 1)])
           ((>= i 15))
           (let ([t (/ i 14.0)])
                (scene-patterns frame t)
                (video-add-frame! video frame)))
       
       ;; Scene 3: Connections (frames 30-44)
       (do ([i 0 (+ i 1)])
           ((>= i 15))
           (let ([t (/ i 14.0)])
                (scene-connections frame t)
                (video-add-frame! video frame)))
       
       ;; Scene 4: Shapes (frames 45-54)
       (do ([i 0 (+ i 1)])
           ((>= i 10))
           (let ([t (/ i 9.0)])
                (scene-shapes frame t)
                (video-add-frame! video frame)))
       
       ;; Scene 5: Finale (frames 55-59)
       (do ([i 0 (+ i 1)])
           ((>= i 5))
           (let ([t (/ i 4.0)])
                (scene-finale frame t)
                (video-add-frame! video frame)))
       
       (display "Generated ")
       (display (video-frame-count video))
       (display " frames.\n")
       video))

;;; ====
;;; Export
;;; ====

(define showcase-video (generate-showcase-video))

(display "\nExporting to GIF...\n")
(video->gif showcase-video "user/creations/graphics-showcase.gif" 150)

(display "\nDone! GIF saved to: user/creations/graphics-showcase.gif\n")
