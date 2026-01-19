;;; user/creations/vi-demo.ss --- Variational Inference ASCII Demo
;;;
;;; A demoscene-style visualization of variational inference concepts:
;;; - ELBO optimization convergence
;;; - Reparameterization trick (z = μ + σε)
;;; - Posterior approximation narrowing
;;; - The critical entropy gradient fix (variance collapse prevention)
;;;
;;; This demo showcases The Fold's probabilistic programming capability
;;; through the lattice/random/variational-inference.ss module.

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "boundary/ui/animation.ss")

;;; ====
;;; Configuration
;;; ====

(define WIDTH 80)
(define HEIGHT 34)
(define TOTAL-FRAMES 160)  ; 6 seconds at 12fps (terminal-appropriate)

;;; Phase timing (frame ranges) - 12fps
;;; Classic ASCII animation doesn't need 60fps. 12fps has character.
(define PHASE-TITLE-START 0)
(define PHASE-TITLE-END 11)       ; 1s

(define PHASE-ELBO-START 12)
(define PHASE-ELBO-END 29)        ; 1.5s

(define PHASE-REPARAM-START 30)
(define PHASE-REPARAM-END 41)     ; 1s

(define PHASE-POSTERIOR-START 42)
(define PHASE-POSTERIOR-END 53)   ; 1s

(define PHASE-FIX-START 54)
(define PHASE-FIX-END 65)         ; 1s

(define PHASE-OUTRO-START 66)
(define PHASE-OUTRO-END 71)       ; 0.5s

;;; ====
;;; Utilities
;;; ====

(define (clamp x min-val max-val)
  (max min-val (min max-val x)))

(define (lerp a b t)
  (+ a (* (- b a) t)))

;;; Gaussian density (unnormalized, for visualization)
(define (gaussian-density x mu sigma)
  (let* ([z (/ (- x mu) sigma)]
         [exponent (* -0.5 z z)])
        (exp exponent)))

;;; Map value to ASCII density characters
(define (density->char density)
  (let ([d (clamp density 0.0 1.0)])
       (cond
        [(< d 0.05) #\space]
        [(< d 0.15) #\.]
        [(< d 0.25) #\:]
        [(< d 0.35) #\-]
        [(< d 0.45) #\=]
        [(< d 0.55) #\+]
        [(< d 0.70) #\*]
        [(< d 0.85) #\#]
        [else #\@])))

;;; ====
;;; ASCII Gaussian Rendering
;;; ====

;;; Draw a Gaussian curve using character density
(define (draw-gaussian! frame center-x baseline-y mu sigma scale)
  (let ([sigma-safe (max 0.1 sigma)])  ; Prevent division by zero
       (do ([x 0 (+ x 1)])
           ((>= x WIDTH))
           ;; Map x to coordinate space centered at mu
           (let* ([coord (+ mu (* (- x center-x) 0.3))]
                  [density (* scale (gaussian-density coord mu sigma-safe))]
                  [char (density->char density)])
                 (when (not (char=? char #\space))
                       (frame-set! frame x baseline-y char))))))

;;; Draw a vertical Gaussian (for posterior visualization)
(define (draw-vertical-gaussian! frame center-x baseline-y mu sigma scale height)
  (let ([sigma-safe (max 0.1 sigma)])
       (do ([dy 0 (+ dy 1)])
           ((>= dy height))
           (let* ([y (- baseline-y dy)]
                  [coord (* (- dy (/ height 2)) 0.2)]
                  [density (* scale (gaussian-density coord mu sigma-safe))]
                  [char (density->char density)])
                 (when (and (>= y 0) (< y HEIGHT) (not (char=? char #\space)))
                       (frame-set! frame center-x y char))))))

;;; ====
;;; Phase 1: Title Card
;;; ====

(define (render-title! frame frame-num)
  (let* ([local-frame (- frame-num PHASE-TITLE-START)]
         [duration (- PHASE-TITLE-END PHASE-TITLE-START)]
         [t (frame->t local-frame duration)]
         [fade-t (ease-in-out-cubic t)])

        (frame-clear! frame #\space)

        ;; Title appears with fade
        (when (> fade-t 0.2)
              (let* ([title "VARIATIONAL INFERENCE"]
                     [y 14]
                     [x (- (quotient WIDTH 2) (quotient (string-length title) 2))])
                   (frame-put-string! frame x y title)))

        ;; Subtitle
        (when (> fade-t 0.5)
              (let* ([subtitle "Gradient-Based Approximate Bayesian Inference"]
                     [y 17]
                     [x (- (quotient WIDTH 2) (quotient (string-length subtitle) 2))])
                   (frame-put-string! frame x y subtitle)))

        ;; Sparkle effect
        (when (> fade-t 0.7)
              (let* ([sparkles '((20 . 12) (60 . 12) (25 . 18) (55 . 18))]
                     [phase (* 10 t)]
                     [visible (> (sin phase) 0)])
                   (when visible
                         (for-each (lambda (pos)
                                           (frame-set! frame (car pos) (cdr pos) #\*))
                                   sparkles))))))

;;; ====
;;; Phase 2: ELBO Convergence
;;; ====

(define (render-elbo! frame frame-num)
  (let* ([local-frame (- frame-num PHASE-ELBO-START)]
         [duration (- PHASE-ELBO-END PHASE-ELBO-START)]
         [t (frame->t local-frame duration)]
         [curve-t (ease-out-cubic t)])

        (frame-clear! frame #\space)

        ;; Title
        (frame-put-string! frame 2 2 "ELBO OPTIMIZATION")
        (frame-put-string! frame 2 3 "Evidence Lower Bound Converges")

        ;; Draw axes
        (do ([y 10 (+ y 1)])
            ((> y 28))
            (frame-set! frame 10 y #\|))
        (do ([x 10 (+ x 1)])
            ((> x 75))
            (frame-set! frame x 28 #\-))

        ;; Y-axis label
        (frame-put-string! frame 2 10 "ELBO")

        ;; X-axis label
        (frame-put-string! frame 68 30 "Iteration")

        ;; Draw ELBO curve (ascending from low to high)
        (do ([x 0 (+ x 1)])
            ((> x (* 65 curve-t)))
            (let* ([progress (/ x 65.0)]
                   [elbo-growth (ease-out-expo progress)]
                   ;; Map ELBO from bottom (y=28) to top (y=12)
                   [y (inexact->exact (round (lerp 28 12 elbo-growth)))]
                   [plot-x (+ 10 x)])
                  (when (and (>= plot-x 10) (< plot-x 75) (>= y 10) (<= y 28))
                        (frame-set! frame plot-x y #\#))))

        ;; Gradient arrows flowing upward
        (when (> t 0.5)
              (let ([arrow-phase (- (* t 10) (floor (* t 10)))])
                   (do ([i 0 (+ i 1)])
                       ((>= i 4))
                       (let* ([x (+ 15 (* i 15))]
                              [y (+ 12 (inexact->exact (round (* 8 arrow-phase))))])
                             (when (and (< y 28) (> y 10))
                                   (frame-set! frame x y #\^))))))))

;;; ====
;;; Phase 3: Reparameterization Trick
;;; ====

(define (render-reparam! frame frame-num)
  (let* ([local-frame (- frame-num PHASE-REPARAM-START)]
         [duration (- PHASE-REPARAM-END PHASE-REPARAM-START)]
         [t (frame->t local-frame duration)])

        (frame-clear! frame #\space)

        ;; Title
        (frame-put-string! frame 2 2 "REPARAMETERIZATION TRICK")
        (frame-put-string! frame 2 3 "z = mu + sigma * epsilon")

        ;; Draw the equation with animation
        (let ([eq-y 12])
             ;; z =
             (frame-put-string! frame 15 eq-y "z =")

             ;; mu appears
             (when (> t 0.2)
                   (frame-put-string! frame 22 eq-y "mu"))

             ;; + appears
             (when (> t 0.4)
                   (frame-put-string! frame 27 eq-y "+"))

             ;; sigma appears
             (when (> t 0.5)
                   (frame-put-string! frame 31 eq-y "sigma"))

             ;; * appears
             (when (> t 0.6)
                   (frame-put-string! frame 39 eq-y "*"))

             ;; epsilon appears
             (when (> t 0.7)
                   (frame-put-string! frame 43 eq-y "epsilon")))

        ;; Particle flow visualization
        (when (> t 0.3)
              (let* ([flow-t (/ (- t 0.3) 0.7)]
                     [num-particles 8])
                   (do ([i 0 (+ i 1)])
                       ((>= i num-particles))
                       (let* ([offset (* i (/ 1.0 num-particles))]
                              [particle-t (- (+ flow-t offset) (floor (+ flow-t offset)))]
                              [x (inexact->exact (round (lerp 43 65 particle-t)))]
                              [y-offset (* 3 (sin (* 6.28 particle-t)))]
                              [y (+ 16 (inexact->exact (round y-offset)))])
                             (when (and (>= x 0) (< x WIDTH) (>= y 0) (< y HEIGHT))
                                   (frame-set! frame x y #\o))))))

        ;; Note about gradient flow
        (when (> t 0.8)
              (frame-put-string! frame 10 25 "Allows gradients to flow through random samples"))))

;;; ====
;;; Phase 4: Posterior Narrowing
;;; ====

(define (render-posterior! frame frame-num)
  (let* ([local-frame (- frame-num PHASE-POSTERIOR-START)]
         [duration (- PHASE-POSTERIOR-END PHASE-POSTERIOR-START)]
         [t (frame->t local-frame duration)]
         [narrow-t (ease-in-out-cubic t)])

        (frame-clear! frame #\space)

        ;; Title
        (frame-put-string! frame 2 2 "POSTERIOR APPROXIMATION")
        (frame-put-string! frame 2 3 "Prior -> Posterior")

        ;; Draw wide prior (fading out)
        (let* ([prior-sigma (lerp 5.0 8.0 narrow-t)]
               [prior-alpha (- 1.0 narrow-t)])
              (when (> prior-alpha 0.1)
                    (draw-gaussian! frame 25 20 0.0 prior-sigma prior-alpha)))

        ;; Draw narrowing posterior (fading in)
        (let* ([posterior-sigma (lerp 5.0 1.5 narrow-t)]
               [posterior-alpha narrow-t])
              (when (> posterior-alpha 0.1)
                    (draw-gaussian! frame 25 20 0.0 posterior-sigma posterior-alpha)))

        ;; Labels
        (when (< t 0.5)
              (frame-put-string! frame 5 22 "Wide Prior"))
        (when (> t 0.5)
              (frame-put-string! frame 5 22 "Narrow Posterior"))

        ;; Show sigma value
        (let ([sigma-val (lerp 5.0 1.5 narrow-t)])
             (frame-put-string! frame 55 20
                                (string-append "sigma: "
                                               (number->string (inexact->exact (round (* 10 sigma-val))))
                                               "/10")))))

;;; ====
;;; Phase 5: The Critical Fix (Side-by-side comparison)
;;; ====

(define (render-fix! frame frame-num)
  (let* ([local-frame (- frame-num PHASE-FIX-START)]
         [duration (- PHASE-FIX-END PHASE-FIX-START)]
         [t (frame->t local-frame duration)]
         [reveal-t (ease-in-out-cubic t)])

        (frame-clear! frame #\space)

        ;; Title
        (frame-put-string! frame 2 2 "THE CRITICAL FIX")
        (frame-put-string! frame 2 3 "Analytical Entropy Gradient")

        ;; Left side: Variance Collapse (bad)
        (frame-put-string! frame 5 8 "WRONG:")
        (frame-put-string! frame 5 9 "MC Entropy Est.")

        ;; Draw collapsed posterior (very narrow)
        (when (> t 0.3)
              (draw-gaussian! frame 20 18 0.0 0.3 0.8))

        (when (> t 0.5)
              (frame-put-string! frame 5 22 "Variance")
              (frame-put-string! frame 5 23 "Collapse!"))

        ;; Divider
        (do ([y 6 (+ y 1)])
            ((>= y 28))
            (frame-set! frame 39 y #\|))

        ;; Right side: Proper Posterior (good)
        (frame-put-string! frame 45 8 "RIGHT:")
        (frame-put-string! frame 45 9 "Analytical dH/dlog_sigma")

        ;; Draw proper posterior
        (when (> t 0.3)
              (draw-gaussian! frame 60 18 0.0 1.8 0.8))

        (when (> t 0.5)
              (frame-put-string! frame 45 22 "Proper")
              (frame-put-string! frame 45 23 "Posterior"))

        ;; The fix explanation
        (when (> t 0.7)
              (frame-put-string! frame 8 30 "Fix: Use H[q] = 0.5*d*(1+log(2*pi)) + sum(log_sigma)")
              (frame-put-string! frame 8 31 "Gradient: dH/dlog_sigma = +1  (prevents collapse)"))))

;;; ====
;;; Phase 6: Outro
;;; ====

(define (render-outro! frame frame-num)
  (let* ([local-frame (- frame-num PHASE-OUTRO-START)]
         [duration (- PHASE-OUTRO-END PHASE-OUTRO-START)]
         [t (frame->t local-frame duration)])

        (frame-clear! frame #\space)

        ;; Main text
        (let* ([line1 "THE FOLD"]
               [line2 "PROBABILISTIC PROGRAMMING"]
               [x1 (- (quotient WIDTH 2) (quotient (string-length line1) 2))]
               [x2 (- (quotient WIDTH 2) (quotient (string-length line2) 2))])
             (frame-put-string! frame x1 14 line1)
             (frame-put-string! frame x2 16 line2))

        ;; Decorative Gaussians
        (draw-gaussian! frame 15 25 0.0 2.0 0.5)
        (draw-gaussian! frame 65 25 0.0 2.0 0.5)))

;;; ====
;;; Main Rendering Loop
;;; ====

(define (render-frame frame-num)
  (let ([frame (make-frame WIDTH HEIGHT #\space)])
       (cond
        [(and (>= frame-num PHASE-TITLE-START) (<= frame-num PHASE-TITLE-END))
         (render-title! frame frame-num)]
        [(and (>= frame-num PHASE-ELBO-START) (<= frame-num PHASE-ELBO-END))
         (render-elbo! frame frame-num)]
        [(and (>= frame-num PHASE-REPARAM-START) (<= frame-num PHASE-REPARAM-END))
         (render-reparam! frame frame-num)]
        [(and (>= frame-num PHASE-POSTERIOR-START) (<= frame-num PHASE-POSTERIOR-END))
         (render-posterior! frame frame-num)]
        [(and (>= frame-num PHASE-FIX-START) (<= frame-num PHASE-FIX-END))
         (render-fix! frame frame-num)]
        [(and (>= frame-num PHASE-OUTRO-START) (<= frame-num PHASE-OUTRO-END))
         (render-outro! frame frame-num)]
        [else
         (frame-clear! frame #\space)])
       frame))

;;; ====
;;; Video Generation
;;; ====

(define (make-vi-demo)
  (display "Generating VI demo...\n")
  (let ([video (make-video)])
       (do ([i 0 (+ i 1)])
           ((>= i TOTAL-FRAMES))
           (when (= 0 (modulo i 30))
                 (display (string-append "Frame " (number->string i) "/" (number->string TOTAL-FRAMES) "\n")))
           (let ([frame (render-frame i)])
                (video-add-frame! video frame)))
       (display "Done generating frames.\n")
       video))

;;; ====
;;; Export
;;; ====

(define (export-vi-demo)
  (let ([video (make-vi-demo)])
       (display "\nExporting to GIF...\n")
       (video->gif video "/home/oso/the-fold/user/creations/vi-demo.gif" 125)  ; 12fps (83ms per frame)
       (display "\nVI demo complete!\n")))

;; Run the demo
(export-vi-demo)
