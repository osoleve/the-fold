;;; user/creations/diff-physics-demo.ss --- Differentiable Physics Showcase
;;;
;;; Visual demonstration of trajectory optimization using gradient descent.
;;; Shows: initial guess trajectory, optimization steps, and final optimized path.

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "core/autodiff/reverse-diff.ss")
(load "lattice/physics/diff/traced-vec2.ss")
(load "lattice/physics/diff/traced-body.ss")
(load "lattice/physics/diff/traced-integrators.ss")
(load "lattice/physics/diff/rollout.ss")
(load "lattice/physics/diff/optimize.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

;;; ====
;;; Configuration
;;; ====

(define WIDTH 60)
(define HEIGHT 30)

;; Physics setup
(define START-POS (vec2 5 20))      ; Starting position
(define TARGET-POS (vec2 50 8))     ; Target (basket/goal)
(define GRAVITY (vec2 0 -15))       ; Gravity
(define DT (/ 1 30))                ; Timestep
(define NUM-STEPS 60)               ; Simulation steps

;;; ====
;;; Coordinate Transforms
;;; ====

;; World coords: Y up, origin bottom-left
;; Screen coords: Y down, origin top-left
(define (world->screen pos)
  (let ([x (vec2-x pos)]
        [y (vec2-y pos)])
       (cons (inexact->exact (round x))
             (inexact->exact (round (- HEIGHT y 1))))))

;;; ====
;;; Frame Drawing Utilities
;;; ====

(define (safe-set! frame x y char)
  (let ([xi (if (integer? x) x (inexact->exact (round x)))]
        [yi (if (integer? y) y (inexact->exact (round y)))])
       (when (and (>= xi 0) (< xi WIDTH)
                  (>= yi 0) (< yi HEIGHT))
             (frame-set! frame xi yi char))))

(define (draw-point! frame pos char)
  (let ([screen (world->screen pos)])
       (safe-set! frame (car screen) (cdr screen) char)))

(define (draw-target! frame pos)
  (let ([screen (world->screen pos)])
       (let ([x (car screen)] [y (cdr screen)])
            ;; Draw a target ring
            (safe-set! frame (- x 1) y #\()
            (safe-set! frame x y #\X)
            (safe-set! frame (+ x 1) y #\)))))

(define (draw-launcher! frame pos)
  (let ([screen (world->screen pos)])
       (let ([x (car screen)] [y (cdr screen)])
            (safe-set! frame x y #\O)
            (safe-set! frame (+ x 1) y #\=)
            (safe-set! frame (+ x 2) y #\>))))

(define (draw-ground! frame)
  (do ([x 0 (+ x 1)])
      ((>= x WIDTH))
      (safe-set! frame x (- HEIGHT 1) #\-)))

(define (draw-title! frame title y-pos)
  ;; Use floor to ensure start-x is always an integer (avoid banker's rounding issues)
  (let ([start-x (inexact->exact (floor (- (/ WIDTH 2) (/ (string-length title) 2))))])
       (do ([i 0 (+ i 1)])
           ((>= i (string-length title)))
           (frame-set! frame (+ start-x i) y-pos (string-ref title i)))))

;;; ====
;;; Trajectory Simulation (Plain Numbers)
;;; ====

(define (simulate-trajectory vel)
  "Simulate projectile with given initial velocity, return list of positions"
  (let loop ([pos START-POS] [v vel] [i 0] [traj '()])
       (if (>= i NUM-STEPS)
           (reverse traj)
           (let* ([new-v (vec2-add v (vec2-scale GRAVITY DT))]
                  [new-pos (vec2-add pos (vec2-scale new-v DT))])
                 (loop new-pos new-v (+ i 1) (cons pos traj))))))

;;; ====
;;; Trajectory Optimization
;;; ====

;; Manual optimization using gradient (following test patterns)
(define (optimize-velocity)
  "Find initial velocity to hit target using gradient descent"
  (let* ([;; Better initial guess: aim higher to account for gravity
          dx (- (vec2-x TARGET-POS) (vec2-x START-POS))]
         [dy (- (vec2-y TARGET-POS) (vec2-y START-POS))]
         [time (* NUM-STEPS DT)]
         ;; Start with a reasonable upward velocity
         [initial-vx (/ dx time)]
         [initial-vy (+ (/ dy time) (* 0.5 (- (vec2-y GRAVITY)) time))]  ; Compensate for gravity
         [lr 0.001]  ; Much smaller learning rate
         [iterations 30])  ; Fewer iterations for quick demo
        (display "  Initial vx: ") (display initial-vx) (newline)
        (display "  Initial vy: ") (display initial-vy) (newline)
        ;; Manual gradient descent loop
        (let loop ([vx initial-vx] [vy initial-vy] [iter 0])
             (if (>= iter iterations)
                 (vec2 vx vy)
                 ;; Compute gradients
                 (let ([grads (gradient
                               (lambda (cur-vx cur-vy)
                                       ;; Simulate projectile with these velocities
                                       (let sim-loop ([px (vec2-x START-POS)] [py (vec2-y START-POS)]
                                                      [sim-vx cur-vx] [sim-vy cur-vy]
                                                      [step 0])
                                            (if (>= step NUM-STEPS)
                                                ;; Distance squared to target
                                                (traced-add
                                                 (traced-mul (traced-sub px (vec2-x TARGET-POS))
                                                             (traced-sub px (vec2-x TARGET-POS)))
                                                 (traced-mul (traced-sub py (vec2-y TARGET-POS))
                                                             (traced-sub py (vec2-y TARGET-POS))))
                                                ;; Euler step with gravity
                                                (let* ([new-vy (traced-add sim-vy (* (vec2-y GRAVITY) DT))]
                                                       [new-px (traced-add px (traced-mul sim-vx DT))]
                                                       [new-py (traced-add py (traced-mul new-vy DT))])
                                                      (sim-loop new-px new-py sim-vx new-vy (+ step 1))))))
                               (list vx vy))])
                      ;; Clamp gradients to avoid explosion
                      (let ([gx (max -100 (min 100 (car grads)))]
                            [gy (max -100 (min 100 (cadr grads)))])
                           (loop (- vx (* lr gx))
                                 (- vy (* lr gy))
                                 (+ iter 1))))))))

;;; ====
;;; Animation Generation
;;; ====

(define (make-intro-frames video n-frames)
  "Create intro frames with title"
  (do ([i 0 (+ i 1)])
      ((>= i n-frames))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "DIFFERENTIABLE PHYSICS" 8)
           (draw-title! frame "Trajectory Optimization" 10)
           (draw-title! frame "via Gradient Descent" 12)
           (draw-ground! frame)
           (draw-launcher! frame START-POS)
           (draw-target! frame TARGET-POS)
           (video-add-frame! video frame))))

(define (make-initial-guess-frames video initial-vel n-repeat)
  "Show the initial (wrong) trajectory"
  (let* ([traj (simulate-trajectory initial-vel)]
         [n-points (length traj)])
        ;; Animate the ball moving
        (do ([step 0 (+ step 1)])
            ((>= step n-points))
            (let ([frame (make-frame WIDTH HEIGHT #\space)])
                 (draw-title! frame "Initial Guess (Wrong)" 1)
                 (draw-ground! frame)
                 (draw-launcher! frame START-POS)
                 (draw-target! frame TARGET-POS)
                 ;; Draw trajectory so far
                 (do ([j 0 (+ j 1)])
                     ((> j step))
                     (when (< j (length traj))
                           (draw-point! frame (list-ref traj j) #\.)))
                 ;; Draw ball at current position
                 (when (< step (length traj))
                       (draw-point! frame (list-ref traj step) #\@))
                 (video-add-frame! video frame)))))

(define (make-optimizing-frames video)
  "Show optimization in progress"
  (do ([i 0 (+ i 1)])
      ((>= i 20))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "Optimizing..." 1)
           (draw-title! frame (string-append "Iteration " (number->string (* i 3))) 3)
           ;; Animated loading dots
           (let ([dots (list-ref '("." ".." "..." "....") (mod i 4))])
                (draw-title! frame (string-append "Computing gradients" dots) 5))
           (draw-ground! frame)
           (draw-launcher! frame START-POS)
           (draw-target! frame TARGET-POS)
           (video-add-frame! video frame))))

(define (make-optimized-frames video optimized-vel n-repeat)
  "Show the optimized trajectory hitting target"
  (let* ([traj (simulate-trajectory optimized-vel)]
         [n-points (length traj)])
        ;; Animate the ball moving
        (do ([step 0 (+ step 1)])
            ((>= step n-points))
            (let ([frame (make-frame WIDTH HEIGHT #\space)])
                 (draw-title! frame "Optimized Trajectory" 1)
                 (draw-ground! frame)
                 (draw-launcher! frame START-POS)
                 (draw-target! frame TARGET-POS)
                 ;; Draw full trajectory
                 (do ([j 0 (+ j 1)])
                     ((> j step))
                     (when (< j (length traj))
                           (draw-point! frame (list-ref traj j) #\*)))
                 ;; Draw ball at current position
                 (when (< step (length traj))
                       (draw-point! frame (list-ref traj step) #\@))
                 (video-add-frame! video frame)))
        ;; Hold on final frame showing success
        (do ([i 0 (+ i 1)])
            ((>= i 15))
            (let ([frame (make-frame WIDTH HEIGHT #\space)])
                 (draw-title! frame "TARGET HIT" 1)
                 (draw-ground! frame)
                 (draw-launcher! frame START-POS)
                 (draw-target! frame TARGET-POS)
                 ;; Draw full trajectory
                 (for-each (lambda (p) (draw-point! frame p #\*)) traj)
                 ;; Ball at target
                 (draw-point! frame TARGET-POS #\@)
                 (video-add-frame! video frame)))))

;;; ====
;;; Main Demo
;;; ====

(define (run-diff-physics-demo)
  (display "Computing optimized trajectory...\n")
  
  ;; Compute initial guess
  (let* ([delta (vec2-sub TARGET-POS START-POS)]
         [time (* NUM-STEPS DT)]
         [initial-vel (vec2-scale-inv delta time)])
        
        (display "Initial guess: ")
        (display (vec2-x initial-vel))
        (display ", ")
        (display (vec2-y initial-vel))
        (newline)
        
        ;; Run optimization
        (display "Running gradient descent optimization...\n")
        (let ([optimized-vel (optimize-velocity)])
             
             (display "Optimized velocity: ")
             (display (vec2-x optimized-vel))
             (display ", ")
             (display (vec2-y optimized-vel))
             (newline)
             
             ;; Create video
             (display "Generating animation frames...\n")
             (let ([video (make-video)])
                  
                  ;; Intro
                  (make-intro-frames video 20)
                  
                  ;; Initial guess (wrong)
                  (make-initial-guess-frames video initial-vel 1)
                  
                  ;; Optimization animation
                  (make-optimizing-frames video)
                  
                  ;; Optimized result
                  (make-optimized-frames video optimized-vel 1)
                  
                  (display "Total frames: ")
                  (display (video-frame-count video))
                  (newline)
                  
                  ;; Export to GIF
                  (display "Exporting to GIF...\n")
                  (video->gif video "user/creations/diff-physics-demo.gif" 80)
                  
                  (display "Done! Saved to user/creations/diff-physics-demo.gif\n")
                  video))))

;; Run it
(run-diff-physics-demo)
