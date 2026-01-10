;;; user/creations/symbolic-physics-demo.ss --- Symbolic + Numeric Autodiff Physics Demo
;;;
;;; Shows both worlds:
;;;   - Symbolic: derive physics equations, see actual formulas
;;;   - Numeric: optimize trajectories via gradient descent
;;;
;;; "Know the equation AND crunch the numbers!"

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/fp/symbolic/integrate-autodiff.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define WIDTH 70)
(define HEIGHT 35)
(define DT (/ 1 30))
(define GRAVITY -15)
(define NUM-STEPS 50)

;; World positions
(define START-X 8)
(define START-Y 25)
(define TARGET-X 58)
(define TARGET-Y 12)

;;; ============================================================
;;; Symbolic Physics Equations
;;; ============================================================

;; Build symbolic projectile equations:
;; y(t) = y0 + vy*t + 0.5*g*t^2
;; x(t) = x0 + vx*t

(define sym-y0 (var 'y0))
(define sym-vy (var 'vy))
(define sym-g (var 'g))
(define sym-t (var 't))
(define sym-x0 (var 'x0))
(define sym-vx (var 'vx))

;; y = y0 + vy*t + 0.5*g*t^2
(define sym-y-equation
  (sum sym-y0
       (sum (product sym-vy sym-t)
            (product (num 0.5) (product sym-g (power sym-t (num 2)))))))

;; x = x0 + vx*t
(define sym-x-equation
  (sum sym-x0 (product sym-vx sym-t)))

;; Compute symbolic derivatives
(define dy/dvy (simplify (deriv sym-y-equation 'vy)))  ; Should be t
(define dy/dt (simplify (deriv sym-y-equation 't)))    ; Should be vy + g*t
(define dx/dvx (simplify (deriv sym-x-equation 'vx)))  ; Should be t

;;; ============================================================
;;; Frame Drawing Utilities
;;; ============================================================

(define (draw-title! frame title y-pos)
  (let ([start-x (inexact->exact (floor (- (/ WIDTH 2) (/ (string-length title) 2))))])
       (do ([i 0 (+ i 1)])
           ((>= i (string-length title)))
           (frame-set! frame (+ start-x i) y-pos (string-ref title i)))))

(define (draw-text! frame text x y)
  (do ([i 0 (+ i 1)])
      ((>= i (string-length text)))
      (when (< (+ x i) WIDTH)
            (frame-set! frame (+ x i) y (string-ref text i)))))

(define (draw-ground! frame)
  (do ([x 0 (+ x 1)])
      ((>= x WIDTH))
      (frame-set! frame x (- HEIGHT 1) #\-)))

(define (world->screen x y)
  (cons (inexact->exact (round x))
        (inexact->exact (round (- HEIGHT y 1)))))

(define (draw-point! frame x y char)
  (let ([screen (world->screen x y)])
       (let ([sx (car screen)] [sy (cdr screen)])
            (when (and (>= sx 0) (< sx WIDTH) (>= sy 0) (< sy HEIGHT))
                  (frame-set! frame sx sy char)))))

(define (draw-target! frame x y)
  (let ([screen (world->screen x y)])
       (let ([sx (car screen)] [sy (cdr screen)])
            (when (and (>= (- sx 1) 0) (< (+ sx 1) WIDTH) (>= sy 0) (< sy HEIGHT))
                  (frame-set! frame (- sx 1) sy #\()
                  (frame-set! frame sx sy #\X)
                  (frame-set! frame (+ sx 1) sy #\))))))

(define (draw-launcher! frame x y)
  (let ([screen (world->screen x y)])
       (let ([sx (car screen)] [sy (cdr screen)])
            (when (and (>= sx 0) (< (+ sx 2) WIDTH) (>= sy 0) (< sy HEIGHT))
                  (frame-set! frame sx sy #\O)
                  (frame-set! frame (+ sx 1) sy #\=)
                  (frame-set! frame (+ sx 2) sy #\>)))))

(define (draw-box! frame x y w h)
  ;; Top and bottom borders
  (do ([i 0 (+ i 1)])
      ((>= i w))
      (frame-set! frame (+ x i) y #\-)
      (frame-set! frame (+ x i) (+ y h -1) #\-))
  ;; Left and right borders
  (do ([j 1 (+ j 1)])
      ((>= j (- h 1)))
      (frame-set! frame x (+ y j) #\|)
      (frame-set! frame (+ x w -1) (+ y j) #\|))
  ;; Corners
  (frame-set! frame x y #\+)
  (frame-set! frame (+ x w -1) y #\+)
  (frame-set! frame x (+ y h -1) #\+)
  (frame-set! frame (+ x w -1) (+ y h -1) #\+))

;;; ============================================================
;;; Trajectory Simulation (Plain Numbers)
;;; ============================================================

(define (simulate-trajectory vx vy)
  "Simulate projectile, return list of (x . y) pairs"
  (let loop ([x START-X] [y START-Y]
             [vel-x vx] [vel-y vy]
             [i 0] [traj (list)])
       (if (>= i NUM-STEPS)
           (reverse traj)
           (let* ([new-vy (+ vel-y (* GRAVITY DT))]
                  [new-x (+ x (* vel-x DT))]
                  [new-y (+ y (* new-vy DT))])
                 (loop new-x new-y vel-x new-vy (+ i 1) (cons (cons x y) traj))))))

;;; ============================================================
;;; Numeric Optimization via Autodiff
;;; ============================================================

(define (optimize-velocity)
  "Find initial velocity to hit target using gradient descent"
  (let* ([dx (- TARGET-X START-X)]
         [dy (- TARGET-Y START-Y)]
         [time (* NUM-STEPS DT)]
         ;; Better initial guess compensating for gravity
         [initial-vx (/ dx time)]
         [initial-vy (+ (/ dy time) (* 0.5 (- GRAVITY) time))]
         [lr 0.002]
         [iterations 40])
        ;; Manual gradient descent loop
        (let loop ([vx initial-vx] [vy initial-vy] [iter 0])
             (if (>= iter iterations)
                 (cons vx vy)
                 ;; Compute gradients using autodiff
                 (let ([grads (autodiff-gradient
                               (lambda (cur-vx cur-vy)
                                       ;; Simulate projectile
                                       (let sim-loop ([x START-X] [y START-Y]
                                                      [sim-vx cur-vx] [sim-vy cur-vy]
                                                      [step 0])
                                            (if (>= step NUM-STEPS)
                                                ;; Distance squared to target
                                                (traced-add
                                                 (traced-mul (traced-sub x TARGET-X)
                                                             (traced-sub x TARGET-X))
                                                 (traced-mul (traced-sub y TARGET-Y)
                                                             (traced-sub y TARGET-Y)))
                                                ;; Euler step
                                                (let* ([new-vy (traced-add sim-vy (* GRAVITY DT))]
                                                       [new-x (traced-add x (traced-mul sim-vx DT))]
                                                       [new-y (traced-add y (traced-mul new-vy DT))])
                                                      (sim-loop new-x new-y sim-vx new-vy (+ step 1))))))
                               (list vx vy))])
                      ;; Clamp and update
                      (let ([gx (max -100 (min 100 (car grads)))]
                            [gy (max -100 (min 100 (cadr grads)))])
                           (loop (- vx (* lr gx))
                                 (- vy (* lr gy))
                                 (+ iter 1))))))))

;;; ============================================================
;;; Animation Frames
;;; ============================================================

(define (make-title-frames video n)
  (do ([i 0 (+ i 1)])
      ((>= i n))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "SYMBOLIC + NUMERIC AUTODIFF" 8)
           (draw-title! frame "Physics Simulation" 10)
           (draw-title! frame "Two Ways to Differentiate" 14)
           (draw-ground! frame)
           (draw-launcher! frame START-X START-Y)
           (draw-target! frame TARGET-X TARGET-Y)
           (video-add-frame! video frame))))

(define (make-symbolic-frames video n)
  "Show symbolic physics equations"
  (do ([i 0 (+ i 1)])
      ((>= i n))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "SYMBOLIC DIFFERENTIATION" 1)
           
           ;; Equation box
           (draw-box! frame 5 4 60 12)
           (draw-text! frame "Projectile Motion Equations:" 8 6)
           (draw-text! frame "y(t) = y0 + vy*t + 0.5*g*t^2" 10 8)
           (draw-text! frame "x(t) = x0 + vx*t" 10 10)
           
           (draw-text! frame "Symbolic Derivatives:" 8 12)
           (draw-text! frame (string-append "dy/dvy = " (expr->infix dy/dvy)) 10 14)
           
           (draw-ground! frame)
           (draw-launcher! frame START-X START-Y)
           (draw-target! frame TARGET-X TARGET-Y)
           (video-add-frame! video frame))))

(define (make-derivative-detail-frames video n)
  "Show derivative computation details"
  (do ([i 0 (+ i 1)])
      ((>= i n))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "SYMBOLIC DERIVATIVES" 1)
           
           (draw-box! frame 5 4 60 14)
           (draw-text! frame "Chain Rule Applied:" 8 6)
           (draw-text! frame "d/dvy[y0 + vy*t + 0.5*g*t^2]" 10 8)
           (draw-text! frame "= 0 + t + 0" 10 10)
           (draw-text! frame "= t" 10 12)
           
           (draw-text! frame "Interpretation:" 8 14)
           (draw-text! frame "Final position changes by t" 10 16)
           (draw-text! frame "for each unit of initial vy" 10 17)
           
           (draw-ground! frame)
           (video-add-frame! video frame))))

(define (make-numeric-intro-frames video n)
  "Introduce numeric optimization"
  (do ([i 0 (+ i 1)])
      ((>= i n))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "NUMERIC AUTODIFF" 1)
           
           (draw-box! frame 5 4 60 12)
           (draw-text! frame "Now: Optimize trajectory numerically" 8 6)
           (draw-text! frame "Goal: Find v to hit target (X)" 8 8)
           (draw-text! frame "Method: Gradient descent on loss" 8 10)
           (draw-text! frame "loss = ||final_pos - target||^2" 8 12)
           (draw-text! frame "Gradients via reverse-mode AD" 8 14)
           
           (draw-ground! frame)
           (draw-launcher! frame START-X START-Y)
           (draw-target! frame TARGET-X TARGET-Y)
           (video-add-frame! video frame))))

(define (make-initial-guess-frames video vx vy)
  "Show the initial (wrong) trajectory"
  (let* ([traj (simulate-trajectory vx vy)]
         [n-points (length traj)])
        (do ([step 0 (+ step 1)])
            ((>= step n-points))
            (let ([frame (make-frame WIDTH HEIGHT #\space)])
                 (draw-title! frame "Initial Guess" 1)
                 (draw-ground! frame)
                 (draw-launcher! frame START-X START-Y)
                 (draw-target! frame TARGET-X TARGET-Y)
                 ;; Draw trajectory so far
                 (do ([j 0 (+ j 1)])
                     ((> j step))
                     (when (< j (length traj))
                           (let ([pt (list-ref traj j)])
                                (draw-point! frame (car pt) (cdr pt) #\.))))
                 ;; Draw ball at current position
                 (when (< step (length traj))
                       (let ([pt (list-ref traj step)])
                            (draw-point! frame (car pt) (cdr pt) #\@)))
                 (video-add-frame! video frame)))))

(define (make-optimizing-frames video n)
  "Show optimization in progress"
  (do ([i 0 (+ i 1)])
      ((>= i n))
      (let ([frame (make-frame WIDTH HEIGHT #\space)])
           (draw-title! frame "Running Gradient Descent..." 1)
           (let ([iter-str (string-append "Iteration " (number->string (* i 2)))])
                (draw-text! frame iter-str 5 3))
           (draw-text! frame "Computing: d(loss)/d(vx), d(loss)/d(vy)" 5 5)
           (draw-text! frame "via reverse-mode autodiff" 5 7)
           (draw-text! frame "through 50 physics steps" 5 9)
           
           ;; Progress bar
           (draw-text! frame "[" 5 12)
           (do ([p 0 (+ p 1)])
               ((>= p 40))
               (frame-set! frame (+ 6 p) 12 (if (< p (* i 2)) #\= #\space)))
           (draw-text! frame "]" 46 12)
           
           (draw-ground! frame)
           (draw-launcher! frame START-X START-Y)
           (draw-target! frame TARGET-X TARGET-Y)
           (video-add-frame! video frame))))

(define (make-optimized-frames video vx vy)
  "Show the optimized trajectory hitting target"
  (let* ([traj (simulate-trajectory vx vy)]
         [n-points (length traj)])
        ;; Animate the ball moving
        (do ([step 0 (+ step 1)])
            ((>= step n-points))
            (let ([frame (make-frame WIDTH HEIGHT #\space)])
                 (draw-title! frame "Optimized Trajectory" 1)
                 (draw-ground! frame)
                 (draw-launcher! frame START-X START-Y)
                 (draw-target! frame TARGET-X TARGET-Y)
                 ;; Draw full trajectory
                 (do ([j 0 (+ j 1)])
                     ((> j step))
                     (when (< j (length traj))
                           (let ([pt (list-ref traj j)])
                                (draw-point! frame (car pt) (cdr pt) #\*))))
                 ;; Draw ball at current position
                 (when (< step (length traj))
                       (let ([pt (list-ref traj step)])
                            (draw-point! frame (car pt) (cdr pt) #\@)))
                 (video-add-frame! video frame)))
        ;; Hold on success
        (do ([i 0 (+ i 1)])
            ((>= i 20))
            (let ([frame (make-frame WIDTH HEIGHT #\space)])
                 (draw-title! frame "TARGET HIT!" 1)
                 (draw-text! frame "Symbolic: Gave us the formulas" 5 3)
                 (draw-text! frame "Numeric: Found the right velocity" 5 5)
                 (draw-ground! frame)
                 (draw-launcher! frame START-X START-Y)
                 (draw-target! frame TARGET-X TARGET-Y)
                 ;; Draw full trajectory
                 (for-each (lambda (pt) (draw-point! frame (car pt) (cdr pt) #\*)) traj)
                 ;; Ball at target
                 (draw-point! frame TARGET-X TARGET-Y #\@)
                 (video-add-frame! video frame)))))

;;; ============================================================
;;; Main Demo
;;; ============================================================

(define (run-symbolic-physics-demo)
  (display "=== Symbolic + Numeric Autodiff Physics Demo ===\n\n")
  
  ;; Show symbolic derivatives
  (display "Symbolic y equation: ")
  (display (expr->infix sym-y-equation))
  (newline)
  (display "dy/dvy (symbolic): ")
  (display (expr->infix dy/dvy))
  (newline)
  (display "dy/dt (symbolic): ")
  (display (expr->infix dy/dt))
  (newline)
  (newline)
  
  ;; Compute initial guess
  (let* ([dx (- TARGET-X START-X)]
         [dy (- TARGET-Y START-Y)]
         [time (* NUM-STEPS DT)]
         [initial-vx (/ dx time)]
         [initial-vy (+ (/ dy time) (* 0.5 (- GRAVITY) time))])
        
        (display "Initial guess: vx=") (display initial-vx)
        (display ", vy=") (display initial-vy) (newline)
        
        ;; Run numeric optimization
        (display "Running numeric gradient descent...\n")
        (let ([optimized (optimize-velocity)])
             (display "Optimized: vx=") (display (car optimized))
             (display ", vy=") (display (cdr optimized)) (newline)
             (newline)
             
             ;; Create video
             (display "Generating animation frames...\n")
             (let ([video (make-video)])
                  
                  ;; Title
                  (make-title-frames video 25)
                  
                  ;; Symbolic section
                  (make-symbolic-frames video 40)
                  (make-derivative-detail-frames video 40)
                  
                  ;; Numeric section
                  (make-numeric-intro-frames video 35)
                  (make-initial-guess-frames video initial-vx (- initial-vy 5)) ; Slightly wrong
                  (make-optimizing-frames video 20)
                  (make-optimized-frames video (car optimized) (cdr optimized))
                  
                  (display "Total frames: ") (display (video-frame-count video)) (newline)
                  
                  ;; Export
                  (display "Exporting to GIF...\n")
                  (video->gif video "user/creations/symbolic-physics-demo.gif" 80)
                  (display "Done! Saved to user/creations/symbolic-physics-demo.gif\n")
                  video))))

;; Run it!
(run-symbolic-physics-demo)
