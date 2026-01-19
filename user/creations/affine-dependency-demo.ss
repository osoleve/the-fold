;;; user/creations/affine-dependency-demo.ss --- The Dependency Problem
;;;
;;; A demoscene-style visualization of why affine arithmetic is necessary.
;;; Shows the critical difference: x - x = 0 (with correlation) vs [-1, 1] (without).

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "boundary/ui/animation.ss")

;;; ====
;;; Configuration
;;; ====

(define *width* 80)
(define *height* 34)
(define *fps* 60)
(define *total-frames* 360)  ; 6 seconds at 60fps

;;; ====
;;; Easing Helpers
;;; ====

(define (frame->t frame total)
  (/ frame (- total 1.0)))

(define (clamp x lo hi)
  (max lo (min hi x)))

(define (lerp a b t)
  (+ a (* (- b a) t)))

;;; ====
;;; Text Drawing
;;; ====

(define (centered-text frame y text)
  (let ([x (quotient (- *width* (string-length text)) 2)])
       (when (>= x 0)
             (frame-put-string! frame x y text))))

(define (draw-char-by-char frame x y text t)
  "Draw text character by character based on t (0-1)"
  (let* ([len (string-length text)]
         [visible (inexact->exact (floor (* len (clamp t 0.0 1.0))))])
        (when (> visible 0)
              (frame-put-string! frame x y (substring text 0 visible)))))

;;; ====
;;; Sparkle Effect
;;; ====

(define *sparkle-chars* " .':*.")

(define (sparkle frame x y seed phase)
  "Random sparkle animation"
  (let* ([idx (modulo (+ seed (inexact->exact (floor (* phase 13)))) 6)]
         [c (string-ref *sparkle-chars* idx)])
        (when (and (>= x 0) (< x *width*)
                   (>= y 0) (< y *height*))
              (frame-set! frame x y c))))

;;; ====
;;; Visual Interval
;;; ====

(define (draw-interval frame x y lo hi label)
  "Draw a visual interval representation: [lo, hi]"
  (let* ([center-val (/ (+ lo hi) 2.0)]
         [radius (/ (- hi lo) 2.0)]
         [bar-width 20]
         [bar-start x]
         [bar-end (+ x bar-width)]
         [mid (+ x (quotient bar-width 2))]
         [lo-pos (+ x (inexact->exact (floor (* radius 5))))]
         [hi-pos (+ x (- bar-width (inexact->exact (floor (* radius 5)))))])
        ;; Draw bracket
        (frame-put-string! frame bar-start y "[")
        (frame-put-string! frame bar-end y "]")
        ;; Fill the bar
        (do ([i (+ bar-start 1) (+ i 1)])
            ((>= i bar-end))
            (frame-set! frame i y #\-))
        ;; Mark center
        (frame-set! frame mid y #\+)
        ;; Label
        (frame-put-string! frame (+ bar-end 2) y label)))

;;; ====
;;; Phase Definitions
;;; ====

;;; Phase 1: Title (0-89)
(define (render-phase-title frame t global-t)
  (frame-clear! frame #\space)
  (let ([title-t (ease-out-cubic t)])
       ;; Main title
       (centered-text frame 8 "THE DEPENDENCY PROBLEM")
       ;; Subtitle with fade-in
       (when (> t 0.3)
             (let ([sub-t (ease-out-cubic (/ (- t 0.3) 0.7))])
                  (centered-text frame 10 "Why Interval Arithmetic Fails")
                  ;; Sparkles
                  (when (> t 0.6)
                        (sparkle frame 20 8 1 global-t)
                        (sparkle frame 59 8 2 global-t)
                        (sparkle frame 25 10 3 global-t)
                        (sparkle frame 54 10 4 global-t))))))

;;; Phase 2: Setup - Introduce x = [1, 2] (90-149)
(define (render-phase-setup frame t global-t)
  (frame-clear! frame #\space)
  (centered-text frame 6 "Given: x = [1, 2]")
  (when (> t 0.4)
        (centered-text frame 8 "(an uncertain value between 1 and 2)")
        ;; Draw visual interval
        (when (> t 0.6)
              (let ([int-t (ease-out-cubic (/ (- t 0.6) 0.4))])
                   (draw-interval frame 30 12 1.0 2.0 "x")))))

;;; Phase 3: The Failure - Show x - x with intervals (150-209)
(define (render-phase-failure frame t global-t)
  (frame-clear! frame #\space)
  (centered-text frame 4 "INTERVAL ARITHMETIC:")
  (centered-text frame 6 "x - x = ?")

  (when (> t 0.3)
        (let ([reveal-t (ease-out-cubic (/ (- t 0.3) 0.7))])
             ;; Show the wrong calculation
             (centered-text frame 10 "x - x = [1,2] - [1,2]")
             (when (> t 0.5)
                   (centered-text frame 12 "     = [1-2, 2-1]")
                   (when (> t 0.7)
                         (centered-text frame 14 "     = [-1, 1]  <-- WRONG!")
                         ;; Visual emphasis
                         (when (> t 0.85)
                               (let ([x-pos 25])
                                    (draw-interval frame x-pos 18 -1.0 1.0 "result")
                                    ;; Error indicators
                                    (frame-put-string! frame 15 20 "Lost correlation!")
                                    (sparkle frame 50 14 10 (* global-t 2))
                                    (sparkle frame 51 14 11 (* global-t 2)))))))))

;;; Phase 4: The Solution - Introduce affine (210-269)
(define (render-phase-solution frame t global-t)
  (frame-clear! frame #\space)
  (centered-text frame 4 "AFFINE ARITHMETIC:")

  (let ([line1-t (clamp (/ t 0.25) 0.0 1.0)]
        [line2-t (clamp (/ (- t 0.25) 0.25) 0.0 1.0)]
        [line3-t (clamp (/ (- t 0.5) 0.25) 0.0 1.0)]
        [line4-t (clamp (/ (- t 0.75) 0.25) 0.0 1.0)])

       ;; Show affine form
       (draw-char-by-char frame 20 7 "x = 1.5 + 0.5*epsilon[0]" line1-t)

       (when (> t 0.25)
             (draw-char-by-char frame 20 9 "   ^      ^     ^" line2-t)
             (draw-char-by-char frame 20 10 " center radius noise" line2-t))

       (when (> t 0.5)
             (centered-text frame 13 "Now compute: x - x")
             (draw-char-by-char frame 15 15 "= (1.5 + 0.5*eps) - (1.5 + 0.5*eps)" line3-t))

       (when (> t 0.75)
             (centered-text frame 17 "= 0")
             (centered-text frame 19 "CORRECT!")
             ;; Celebration sparkles
             (sparkle frame 25 19 20 (* global-t 3))
             (sparkle frame 54 19 21 (* global-t 3))
             (sparkle frame 30 17 22 (* global-t 3))
             (sparkle frame 49 17 23 (* global-t 3)))))

;;; Phase 5: Comparison (270-329)
(define (render-phase-comparison frame t global-t)
  (frame-clear! frame #\space)
  (centered-text frame 3 "SIDE BY SIDE")

  ;; Left: Interval
  (frame-put-string! frame 8 7 "INTERVAL")
  (frame-put-string! frame 5 9 "x - x = [-1, 1]")
  (draw-interval frame 5 11 -1.0 1.0 "")
  (frame-put-string! frame 5 13 "Width: 2.0")
  (frame-put-string! frame 5 15 "WRONG")

  ;; Divider
  (do ([y 7 (+ y 1)])
      ((> y 16))
      (frame-set! frame 39 y #\|))

  ;; Right: Affine
  (frame-put-string! frame 50 7 "AFFINE")
  (frame-put-string! frame 47 9 "x - x = 0")
  (frame-put-string! frame 47 11 "    +")
  (frame-put-string! frame 47 13 "Width: 0.0")
  (frame-put-string! frame 47 15 "CORRECT")

  ;; Bottom explanation
  (when (> t 0.5)
        (centered-text frame 20 "Noise symbols track correlation")
        (when (> t 0.7)
              (centered-text frame 22 "Same epsilon cancels when subtracted"))))

;;; Phase 6: Outro (330-359)
(define (render-phase-outro frame t global-t)
  (frame-clear! frame #\space)
  (let ([fade-t (ease-in-out-cubic t)])
       (centered-text frame 14 "Affine Arithmetic")
       (centered-text frame 16 "Solving the dependency problem since 1993")
       (when (> t 0.5)
             (centered-text frame 20 "lattice/numeric/affine.ss"))
       ;; Final sparkles
       (when (> t 0.6)
             (sparkle frame 20 14 30 global-t)
             (sparkle frame 59 14 31 global-t)
             (sparkle frame 25 16 32 global-t)
             (sparkle frame 54 16 33 global-t))))

;;; ====
;;; Main Render Loop
;;; ====

(define (render-frame frame-num)
  (let* ([global-t (frame->t frame-num *total-frames*)]
         [f (make-frame *width* *height* #\space)])

        (cond
         ;; Phase 1: Title (0-89)
         [(< frame-num 90)
          (let ([t (/ frame-num 89.0)])
               (render-phase-title f t global-t))]

         ;; Phase 2: Setup (90-149)
         [(< frame-num 150)
          (let ([t (/ (- frame-num 90) 59.0)])
               (render-phase-setup f t global-t))]

         ;; Phase 3: Failure (150-209)
         [(< frame-num 210)
          (let ([t (/ (- frame-num 150) 59.0)])
               (render-phase-failure f t global-t))]

         ;; Phase 4: Solution (210-269)
         [(< frame-num 270)
          (let ([t (/ (- frame-num 210) 59.0)])
               (render-phase-solution f t global-t))]

         ;; Phase 5: Comparison (270-329)
         [(< frame-num 330)
          (let ([t (/ (- frame-num 270) 59.0)])
               (render-phase-comparison f t global-t))]

         ;; Phase 6: Outro (330-359)
         [else
          (let ([t (/ (- frame-num 330) 29.0)])
               (render-phase-outro f t global-t))])

        f))

;;; ====
;;; Export
;;; ====

(define (create-demo)
  (display "Generating affine dependency demo...\n")
  (display "Frames: ")
  (display *total-frames*)
  (display " @ ")
  (display *fps*)
  (display "fps (6.0 seconds)\n")

  (let ([video (make-video)])
       ;; Generate all frames
       (do ([i 0 (+ i 1)])
           ((>= i *total-frames*))
           (when (= (modulo i 30) 0)
                 (display "Frame ")
                 (display i)
                 (display "/")
                 (display *total-frames*)
                 (newline))
           (video-add-frame! video (render-frame i)))

       (display "\nExporting GIF...\n")
       (video->gif video "user/creations/affine-dependency.gif"
                   (inexact->exact (floor (/ 1000.0 *fps*))))

       (display "Done! The dependency problem has been visualized.\n")
       video))

;;; Run it
(define *demo-video* (create-demo))
