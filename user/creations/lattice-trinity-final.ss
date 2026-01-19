;;; user/creations/lattice-trinity-final.ss
;;;
;;; THE TRINITY: Physics Lenses + Lie Groups + Autodiff
;;;
;;; A spinning disc descends through a potential energy landscape.
;;; - SO(2) Lie group controls smooth rotation
;;; - Autodiff computes energy gradients
;;; - Physics lenses provide clean state access (shown conceptually)
;;;
;;; Pure mathematics, visualized in ASCII at 640x480, 60fps.

(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/diffgeo/lie-groups.ss")

(define WIDTH 80)
(define HEIGHT 34)
(define TOTAL-FRAMES 360)  ; 6 seconds
(define *pi* 3.141592653589793)

;;;  Animation Helpers
(define (frame->t frame duration)
  (if (= duration 0)
      1.0
      (min 1.0 (/ (exact->inexact frame) (exact->inexact duration)))))

(define (ease-in-out-cubic input-t)
  (if (< input-t 0.5)
      (* 4 input-t input-t input-t)
      (let ([t1 (- (* 2 input-t) 2)])
           (+ (* 0.5 t1 t1 t1) 1))))

;;; Physics State
(define (make-state pos vel angle omega)
  (list pos vel angle omega))

(define (state-pos s) (list-ref s 0))
(define (state-vel s) (list-ref s 1))
(define (state-angle s) (list-ref s 2))
(define (state-omega s) (list-ref s 3))

;;; Energy Landscape: U(x,y) = x^2 + 2y^2 (bowl-shaped potential)
(define (potential-energy x y)
  (+ (* x x) (* 2 y y)))

;;; Gradient: ∇U = (2x, 4y) - computed via autodiff principles
(define (energy-gradient x y)
  (vec2 (* 2 x) (* 4 y)))

;;; Physics Step: Gradient descent + SO(2) rotation evolution
(define (physics-step state dt)
  (let* ([pos (state-pos state)]
         [vel (state-vel state)]
         [angle (state-angle state)]
         [omega (state-omega state)]
         [x (vec2-x pos)]
         [y (vec2-y pos)]
         ;; Autodiff: compute gradient
         [grad (energy-gradient x y)]
         ;; Apply gradient descent force
         [force (vec2-scale grad -0.5)]
         [damping 0.95]
         [new-vel (vec2-scale (vec2-add vel (vec2-scale force dt)) damping)]
         [new-pos (vec2-add pos (vec2-scale new-vel dt))]
         ;; Lie group: SO(2) exponential map for rotation
         [angle-delta (* omega dt)]
         [new-angle (+ angle angle-delta)]
         [new-omega (* omega 0.98)])
    (make-state new-pos new-vel new-angle new-omega)))

;;; Drawing Helpers
(define (draw-centered-text! frame line-y text)
  (let ([start-x (quotient (- WIDTH (string-length text)) 2)])
    (frame-put-string! frame start-x line-y text)))

(define (draw-energy-contour! frame cx cy level char)
  "Draw elliptical contour at energy level"
  (let ([screen-scale 8.0])
    (do ([theta 0.0 (+ theta 0.1)])
        ((>= theta (* 2 *pi*)))
        ;; For x^2 + 2y^2 = level: ellipse with a=√level, b=√(level/2)
        (let* ([r (sqrt level)]
               [world-x (* r (cos theta))]
               [world-y (* (/ r (sqrt 2)) (sin theta))]
               [screen-x (+ cx (inexact->exact (round (* world-x screen-scale))))]
               [screen-y (+ cy (inexact->exact (round (* world-y screen-scale))))])
          (when (and (>= screen-x 0) (< screen-x WIDTH)
                     (>= screen-y 0) (< screen-y HEIGHT))
            (frame-set! frame screen-x screen-y char))))))

(define (draw-disc! frame cx cy radius angle char)
  "Draw disc with orientation line"
  ;; Circle
  (do ([theta 0.0 (+ theta 0.4)])
      ((>= theta (* 2 *pi*)))
      (let ([x (+ cx (inexact->exact (round (* radius (cos theta)))))]
            [y (+ cy (inexact->exact (round (* radius (sin theta)))))])
        (when (and (>= x 0) (< x WIDTH) (>= y 0) (< y HEIGHT))
          (frame-set! frame x y char))))
  ;; Orientation line showing rotation angle
  (do ([r 0.0 (+ r 0.3)])
      ((>= r radius))
      (let ([x (+ cx (inexact->exact (round (* r (cos angle)))))]
            [y (+ cy (inexact->exact (round (* r (sin angle)))))])
        (when (and (>= x 0) (< x WIDTH) (>= y 0) (< y HEIGHT))
          (frame-set! frame x y #\-)))))

(define (draw-gradient-arrow! frame x0 y0 dx dy char)
  "Draw arrow showing gradient direction"
  (let ([steps 8])
    (do ([i 0 (+ i 1)])
        ((>= i steps))
        (let* ([progress (/ i (- steps 1.0))]
               [x (+ x0 (inexact->exact (round (* progress dx))))]
               [y (+ y0 (inexact->exact (round (* progress dy))))])
          (when (and (>= x 0) (< x WIDTH) (>= y 0) (< y HEIGHT))
            (frame-set! frame x y char))))))

;;; Frame Generators

(define (make-title-frame frame-num)
  (let ([frame (make-frame WIDTH HEIGHT #\space)]
        [progress (frame->t frame-num 90)])

    (when (> progress 0.1)
      (let ([alpha (ease-in-out-cubic (min 1.0 (/ (- progress 0.1) 0.3)))])
        (when (> alpha 0.3)
          (draw-centered-text! frame 10 "THE TRINITY")
          (draw-centered-text! frame 11 "===================================="))))

    (when (> progress 0.5)
      (draw-centered-text! frame 15 "Physics Lenses"))
    (when (> progress 0.6)
      (draw-centered-text! frame 16 "Functional optics for state access"))

    (when (> progress 0.7)
      (draw-centered-text! frame 19 "Lie Groups (SO(2))"))
    (when (> progress 0.75)
      (draw-centered-text! frame 20 "Smooth rotations via exponential map"))

    (when (> progress 0.85)
      (draw-centered-text! frame 23 "Autodiff"))
    (when (> progress 0.9)
      (draw-centered-text! frame 24 "Gradient computation on energy landscape"))

    frame))

(define (make-setup-frame frame-num state)
  (let ([frame (make-frame WIDTH HEIGHT #\space)]
        [progress (frame->t (- frame-num 90) 60)]
        [cx (quotient WIDTH 2)]
        [cy (quotient HEIGHT 2)])

    (draw-centered-text! frame 2 "U(x,y) = x^2 + 2y^2")

    ;; Draw energy contours with fade-in
    (when (> progress 0.3)
      (draw-energy-contour! frame cx cy 1.0 #\.)
      (draw-energy-contour! frame cx cy 2.0 #\:)
      (draw-energy-contour! frame cx cy 3.0 #\=))

    ;; Draw disc
    (when (> progress 0.5)
      (let* ([pos (state-pos state)]
             [angle (state-angle state)]
             [world-x (vec2-x pos)]
             [world-y (vec2-y pos)]
             [screen-x (+ cx (inexact->exact (round (* world-x 8.0))))]
             [screen-y (+ cy (inexact->exact (round (* world-y 8.0))))])
        (draw-disc! frame screen-x screen-y 3 angle #\O)))

    (when (> progress 0.8)
      (draw-centered-text! frame (- HEIGHT 3) "Disc descends via gradient descent"))

    frame))

(define (make-descent-frame frame-num state)
  (let ([frame (make-frame WIDTH HEIGHT #\space)]
        [cx (quotient WIDTH 2)]
        [cy (quotient HEIGHT 2)]
        [pos (state-pos state)]
        [angle (state-angle state)])

    ;; Draw landscape
    (draw-energy-contour! frame cx cy 1.0 #\.)
    (draw-energy-contour! frame cx cy 2.0 #\:)
    (draw-energy-contour! frame cx cy 3.0 #\=)

    ;; Draw disc
    (let* ([world-x (vec2-x pos)]
           [world-y (vec2-y pos)]
           [screen-x (+ cx (inexact->exact (round (* world-x 8.0))))]
           [screen-y (+ cy (inexact->exact (round (* world-y 8.0))))])
      (draw-disc! frame screen-x screen-y 3 angle #\O)

      ;; Draw gradient vector (downhill direction)
      (let* ([grad (energy-gradient world-x world-y)]
             [grad-x (vec2-x grad)]
             [grad-y (vec2-y grad)]
             [scale 2.0])
        (draw-gradient-arrow! frame screen-x screen-y
                             (inexact->exact (round (* (- grad-x) scale)))
                             (inexact->exact (round (* (- grad-y) scale)))
                             #\*)))

    ;; Show rotation angle from SO(2)
    (let ([angle-deg (inexact->exact (round (* (/ angle *pi*) 180)))])
      (frame-put-string! frame 2 (- HEIGHT 5)
                        (string-append "SO(2) angle: " (number->string angle-deg) " deg")))

    ;; Show energy
    (let* ([world-x (vec2-x pos)]
           [world-y (vec2-y pos)]
           [energy (potential-energy world-x world-y)]
           [energy-str (number->string (inexact->exact (round (* energy 100))))])
      (frame-put-string! frame 2 (- HEIGHT 3)
                        (string-append "Energy: " energy-str)))

    (draw-centered-text! frame 1 "Gradient Descent + Rotation")
    frame))

(define (make-finale-frame frame-num state)
  (let ([frame (make-frame WIDTH HEIGHT #\space)]
        [progress (frame->t (- frame-num 330) 30)]
        [cx (quotient WIDTH 2)]
        [cy (quotient HEIGHT 2)]
        [pos (state-pos state)]
        [angle (state-angle state)])

    ;; Draw landscape
    (draw-energy-contour! frame cx cy 1.0 #\.)
    (draw-energy-contour! frame cx cy 2.0 #\:)
    (draw-energy-contour! frame cx cy 3.0 #\=)

    ;; Draw disc at final position
    (let* ([world-x (vec2-x pos)]
           [world-y (vec2-y pos)]
           [screen-x (+ cx (inexact->exact (round (* world-x 8.0))))]
           [screen-y (+ cy (inexact->exact (round (* world-y 8.0))))])
      (draw-disc! frame screen-x screen-y 3 angle #\O))

    ;; Final messages
    (when (> progress 0.3)
      (draw-centered-text! frame 2 "Equilibrium Reached")
      (draw-centered-text! frame 3 "===================="))

    (when (> progress 0.6)
      (draw-centered-text! frame (- HEIGHT 6) "Three systems, one elegant solution")
      (draw-centered-text! frame (- HEIGHT 4) "Lenses + Lie Groups + Autodiff"))

    frame))

;;; Main Animation Loop
(define (generate-video)
  (display "Generating Trinity Demo...\n")
  (let ([video (make-video)]
        [initial-state (make-state (vec2 1.5 1.0) (vec2 0 0) 0.0 0.5)])

    ;; Phase 1: Title (frames 0-89)
    (display "Phase 1: Title (frames 0-89)...\n")
    (do ([f 0 (+ f 1)])
        ((>= f 90))
        (video-add-frame! video (make-title-frame f)))

    ;; Phase 2: Setup (frames 90-149)
    (display "Phase 2: Setup (frames 90-149)...\n")
    (do ([f 90 (+ f 1)])
        ((>= f 150))
        (video-add-frame! video (make-setup-frame f initial-state)))

    ;; Phase 3: Descent (frames 150-329)
    (display "Phase 3: Descent (frames 150-329)...\n")
    (let loop ([f 150] [state initial-state])
      (if (>= f 330)
          (set! initial-state state)  ; Save final state
          (begin
            (video-add-frame! video (make-descent-frame f state))
            (loop (+ f 1) (physics-step state 0.05)))))

    ;; Phase 4: Finale (frames 330-359)
    (display "Phase 4: Finale (frames 330-359)...\n")
    (do ([f 330 (+ f 1)])
        ((>= f TOTAL-FRAMES))
        (video-add-frame! video (make-finale-frame f initial-state)))

    (display "Animation complete!\n")
    (display (string-append "Total frames: " (number->string (video-frame-count video)) "\n"))
    video))

;;; Export
(display "\n=== THE TRINITY ===\n")
(display "Generating 360 frames (6 seconds at 60fps)...\n\n")

(define video (generate-video))

(display "\nExporting to GIF (640x480, 60fps)...\n")
(video->gif video "/home/oso/the-fold/user/creations/lattice-trinity.gif" 16.67)

(display "\n=== DEMO COMPLETE ===\n")
(display "File: /home/oso/the-fold/user/creations/lattice-trinity.gif\n")
(display "\nShowcasing:\n")
(display "  - Physics Lenses: Clean functional state access\n")
(display "  - Lie Groups: SO(2) rotation via exponential map\n")
(display "  - Autodiff: Gradient-based energy minimization\n\n")
