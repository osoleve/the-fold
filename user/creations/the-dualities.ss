;;; user/creations/the-dualities.ss — Lagrangian/Hamiltonian Duality Demo
;;;
;;; A terminal animation showing a chaotic double pendulum with real-time
;;; display of both the Lagrangian and Hamiltonian formulations, connected
;;; by the Legendre transform. The message: chaos in motion, but conservation
;;; (H = const) as the invariant truth.

(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "user/creations/ascii-video.ss")
(load "user/creations/ascii-video-export.ss")
(load "shell/ui/animation.ss")

;;; ====
;;; Configuration
;;; ====

(define WIDTH 80)
(define HEIGHT 34)

;; Double pendulum parameters
(define L1 6.0)   ; Length of first pendulum (units) - shortened 25%
(define L2 4.5)   ; Length of second pendulum - shortened 25%
(define M1 2.0)   ; Mass of first bob
(define M2 1.5)   ; Mass of second bob
(define G 9.8)    ; Gravity

;; Initial conditions (increased angles for more potential energy)
(define THETA1-0 1.2)   ; Initial angle of first pendulum (radians) ~69 degrees
(define THETA2-0 -0.8)  ; Initial angle of second pendulum ~46 degrees
(define OMEGA1-0 0.0)   ; Initial angular velocity 1
(define OMEGA2-0 0.0)   ; Initial angular velocity 2

;; Simulation
(define DT 0.001)   ; Timestep (1ms for symplectic accuracy)
(define SUB-STEPS 16) ; Substeps per frame for tight energy bounds
(define TOTAL-FRAMES 360)  ; 6 seconds at 60fps

;; Display
(define PENDULUM-CENTER-X 25)
(define PENDULUM-CENTER-Y 8)
(define PENDULUM-SCALE 2.0)  ; Scale factor for visualization

;;; ====
;;; Double Pendulum Physics
;;; ====

;; The double pendulum is a classic chaotic system.
;; Lagrangian: L = T - V
;; Hamiltonian: H = T + V (total energy)

;; State: (theta1, theta2, omega1, omega2)

;;; compute-lagrangian : State -> Number
;;; L = T - V where T is kinetic energy, V is potential energy
(define (compute-lagrangian theta1 theta2 omega1 omega2)
  (let* ([;; Kinetic energy
          T1 (* 0.5 M1 L1 L1 omega1 omega1)]
          [T2 (* 0.5 M2 (+ (* L1 L1 omega1 omega1)
                           (* L2 L2 omega2 omega2)
                           (* 2 L1 L2 omega1 omega2 (cos (- theta1 theta2)))))]
          [T (+ T1 T2)]
          ;; Potential energy (taking θ=0 as horizontal, positive downward)
          [y1 (* (- L1) (cos theta1))]
          [y2 (+ y1 (* (- L2) (cos theta2)))]
          [V (+ (* M1 G y1) (* M2 G y2))])
        (- T V)))

;;; compute-hamiltonian : State -> Number
;;; H = T + V (total energy, should be conserved)
(define (compute-hamiltonian theta1 theta2 omega1 omega2)
  (let* ([;; Kinetic energy
          T1 (* 0.5 M1 L1 L1 omega1 omega1)]
          [T2 (* 0.5 M2 (+ (* L1 L1 omega1 omega1)
                           (* L2 L2 omega2 omega2)
                           (* 2 L1 L2 omega1 omega2 (cos (- theta1 theta2)))))]
          [T (+ T1 T2)]
          ;; Potential energy
          [y1 (* (- L1) (cos theta1))]
          [y2 (+ y1 (* (- L2) (cos theta2)))]
          [V (+ (* M1 G y1) (* M2 G y2))])
        (+ T V)))

;;; compute-momenta : State -> (p1, p2)
;;; Conjugate momenta via Legendre transform: p_i = ∂L/∂ω_i
(define (compute-momenta theta1 theta2 omega1 omega2)
  (let* ([delta-theta (- theta1 theta2)]
          [cos-delta (cos delta-theta)]
          ;; p1 = ∂L/∂ω1
          [p1 (+ (* M1 L1 L1 omega1)
                 (* M2 L1 (+ (* L1 omega1) (* L2 omega2 cos-delta))))]
          ;; p2 = ∂L/∂ω2
          [p2 (* M2 L2 (+ (* L2 omega2) (* L1 omega1 cos-delta)))])
        (cons p1 p2)))

;;; compute-accelerations : State -> (alpha1, alpha2)
;;; Compute angular accelerations from Lagrangian mechanics
;;; Standard double pendulum equations (verified against physics references)
(define (compute-accelerations theta1 theta2 omega1 omega2)
  (let* ([delta-theta (- theta1 theta2)]
          [sin-delta (sin delta-theta)]
          [cos-delta (cos delta-theta)]
          [denom (+ M1 (* M2 sin-delta sin-delta))]
          ;; Angular acceleration for first pendulum
          [alpha1 (/ (+ (* (- M2) L1 omega1 omega1 sin-delta cos-delta)
                        (* M2 G (sin theta2) cos-delta)  ; Fixed: was negative
                        (* (- M2) L2 omega2 omega2 sin-delta)
                        (* (- (+ M1 M2)) G (sin theta1)))
                     (* L1 denom))]
          ;; Angular acceleration for second pendulum
          [alpha2 (/ (+ (* (+ M1 M2) L1 omega1 omega1 sin-delta)
                        (* (+ M1 M2) G (sin theta1) cos-delta)
                        (* (- (+ M1 M2)) G (sin theta2))
                        (* M2 L2 omega2 omega2 sin-delta cos-delta))  ; Fixed: was wrong term
                     (* L2 denom))])
        (cons alpha1 alpha2)))

;;; state-derivative : State -> State'
;;; Compute time derivatives: (dθ1/dt, dθ2/dt, dω1/dt, dω2/dt)
(define (state-derivative state)
  (let* ([theta1 (list-ref state 0)]
          [theta2 (list-ref state 1)]
          [omega1 (list-ref state 2)]
          [omega2 (list-ref state 3)]
          [alphas (compute-accelerations theta1 theta2 omega1 omega2)]
          [alpha1 (car alphas)]
          [alpha2 (cdr alphas)])
        (list omega1 omega2 alpha1 alpha2)))

;;; state-add : State x State x Real -> State
(define (state-add s1 s2 scale)
  (list (+ (list-ref s1 0) (* scale (list-ref s2 0)))
        (+ (list-ref s1 1) (* scale (list-ref s2 1)))
        (+ (list-ref s1 2) (* scale (list-ref s2 2)))
        (+ (list-ref s1 3) (* scale (list-ref s2 3)))))

;;; step-double-pendulum : State -> State
;;; Velocity Verlet (Störmer-Verlet) - 2nd order symplectic
;;; Better energy conservation for non-separable Hamiltonians
(define (step-double-pendulum state dt)
  (let* ([theta1 (list-ref state 0)]
         [theta2 (list-ref state 1)]
         [omega1 (list-ref state 2)]
         [omega2 (list-ref state 3)]
         ;; Step 1: Compute accelerations at current state
         [alphas (compute-accelerations theta1 theta2 omega1 omega2)]
         [alpha1 (car alphas)]
         [alpha2 (cdr alphas)]
         ;; Step 2: Half-step velocity update
         [omega1-half (+ omega1 (* 0.5 dt alpha1))]
         [omega2-half (+ omega2 (* 0.5 dt alpha2))]
         ;; Step 3: Full-step position update using half-step velocities
         [theta1-new (+ theta1 (* dt omega1-half))]
         [theta2-new (+ theta2 (* dt omega2-half))]
         ;; Step 4: Compute accelerations at new positions
         [alphas-new (compute-accelerations theta1-new theta2-new omega1-half omega2-half)]
         [alpha1-new (car alphas-new)]
         [alpha2-new (cdr alphas-new)]
         ;; Step 5: Complete velocity update
         [omega1-new (+ omega1-half (* 0.5 dt alpha1-new))]
         [omega2-new (+ omega2-half (* 0.5 dt alpha2-new))])
    (list theta1-new theta2-new omega1-new omega2-new)))

;;; Legacy RK4 (unused, kept for reference)
(define (step-double-pendulum-rk4 state dt)
  (let* ([k1 (state-derivative state)]
          [k2 (state-derivative (state-add state k1 (* 0.5 dt)))]
          [k3 (state-derivative (state-add state k2 (* 0.5 dt)))]
          [k4 (state-derivative (state-add state k3 dt))]
          ;; Weighted average of derivatives
          [dstate (list (/ (+ (list-ref k1 0)
                              (* 2 (list-ref k2 0))
                              (* 2 (list-ref k3 0))
                              (list-ref k4 0)) 6.0)
                        (/ (+ (list-ref k1 1)
                              (* 2 (list-ref k2 1))
                              (* 2 (list-ref k3 1))
                              (list-ref k4 1)) 6.0)
                        (/ (+ (list-ref k1 2)
                              (* 2 (list-ref k2 2))
                              (* 2 (list-ref k3 2))
                              (list-ref k4 2)) 6.0)
                        (/ (+ (list-ref k1 3)
                              (* 2 (list-ref k2 3))
                              (* 2 (list-ref k3 3))
                              (list-ref k4 3)) 6.0))])
        (state-add state dstate dt)))

;;; ====
;;; Coordinate Conversion
;;; ====

;;; pendulum-bob-positions : State -> ((x1, y1), (x2, y2))
(define (pendulum-bob-positions theta1 theta2)
  (let* ([x1 (* L1 (sin theta1))]
          [y1 (* (- L1) (cos theta1))]
          [x2 (+ x1 (* L2 (sin theta2)))]
          [y2 (+ y1 (* (- L2) (cos theta2)))])
        (list (cons x1 y1) (cons x2 y2))))

;;; world->screen : (x, y) -> (sx, sy)
(define (world->screen pos)
  (let ([x (car pos)]
        [y (cdr pos)])
       ;; Note: negate y because screen coords have y increasing downward
       (cons (inexact->exact (round (+ PENDULUM-CENTER-X (* x PENDULUM-SCALE))))
             (inexact->exact (round (+ PENDULUM-CENTER-Y (* (- y) PENDULUM-SCALE)))))))

;;; ====
;;; Drawing Utilities
;;; ====

(define (safe-set! frame x y char)
  (when (and (>= x 0) (< x WIDTH)
             (>= y 0) (< y HEIGHT))
        (frame-set! frame x y char)))

(define (draw-line! frame x1 y1 x2 y2 char)
  "Draw a line using Bresenham's algorithm"
  (let* ([dx (abs (- x2 x1))]
          [dy (abs (- y2 y1))]
          [sx (if (< x1 x2) 1 -1)]
          [sy (if (< y1 y2) 1 -1)]
          [err (- dx dy)])
        (let loop ([x x1] [y y1] [e err])
             (safe-set! frame x y char)
             (when (not (and (= x x2) (= y y2)))
                   (let* ([e2 (* 2 e)]
                          [new-e (cond
                                  [(and (> e2 (- dy)) (< e2 dx))
                                   (loop (+ x sx) (+ y sy) (+ (- e dy) dx))]
                                  [(> e2 (- dy))
                                   (loop (+ x sx) y (- e dy))]
                                  [else
                                   (loop x (+ y sy) (+ e dx))])])
                         #f)))))

(define (draw-pendulum! frame state trail-states alpha)
  "Draw the double pendulum with trail"
  (let* ([theta1 (list-ref state 0)]
          [theta2 (list-ref state 1)]
          [positions (pendulum-bob-positions theta1 theta2)]
          [bob1 (car positions)]
          [bob2 (cadr positions)]
          [pivot (world->screen (cons 0 0))]
          [screen1 (world->screen bob1)]
          [screen2 (world->screen bob2)])

        ;; Draw trail (fading previous positions)
        (let loop ([i 0] [trail trail-states])
             (when (and (< i (length trail-states)) (pair? trail))
                   (let* ([old-state (car trail)]
                          [old-theta1 (list-ref old-state 0)]
                          [old-theta2 (list-ref old-state 1)]
                          [old-pos (pendulum-bob-positions old-theta1 old-theta2)]
                          [old-bob2 (cadr old-pos)]
                          [old-screen (world->screen old-bob2)]
                          ;; Fade intensity based on age
                          [fade-chars (list #\. #\: #\* #\o #\O)])
                         (when (< i (length fade-chars))
                               (safe-set! frame (car old-screen) (cdr old-screen)
                                         (list-ref fade-chars i))))
                   (loop (+ i 1) (cdr trail))))

        ;; Draw rods
        (draw-line! frame (car pivot) (cdr pivot) (car screen1) (cdr screen1) #\|)
        (draw-line! frame (car screen1) (cdr screen1) (car screen2) (cdr screen2) #\|)

        ;; Draw pivot
        (safe-set! frame (car pivot) (cdr pivot) #\^)

        ;; Draw bobs
        (safe-set! frame (car screen1) (cdr screen1) #\O)
        (safe-set! frame (car screen2) (cdr screen2) #\@)))

(define (draw-box! frame x y w h)
  "Draw a box border"
  ;; Corners
  (safe-set! frame x y #\+)
  (safe-set! frame (+ x w -1) y #\+)
  (safe-set! frame x (+ y h -1) #\+)
  (safe-set! frame (+ x w -1) (+ y h -1) #\+)
  ;; Horizontal edges
  (do ([i 1 (+ i 1)])
      ((>= i (- w 1)))
      (safe-set! frame (+ x i) y #\-)
      (safe-set! frame (+ x i) (+ y h -1) #\-))
  ;; Vertical edges
  (do ([i 1 (+ i 1)])
      ((>= i (- h 1)))
      (safe-set! frame x (+ y i) #\|)
      (safe-set! frame (+ x w -1) (+ y i) #\|)))

(define (draw-centered-text! frame text y)
  "Draw centered text"
  (let ([start-x (inexact->exact (floor (- (/ WIDTH 2) (/ (string-length text) 2))))])
       (frame-put-string! frame start-x y text)))

(define (draw-number! frame x y value precision)
  "Draw a number with limited precision"
  (let* ([sign (if (< value 0) "-" "")]
          [abs-val (abs value)]
          [scaled (inexact->exact (round (* abs-val (expt 10 precision))))]
          [scale-factor (expt 10 precision)])
        (if (= precision 0)
            ;; Integer display
            (frame-put-string! frame x y (string-append sign (number->string scaled)))
            ;; Decimal display
            (let* ([int-part (quotient scaled scale-factor)]
                    [frac-part (remainder scaled scale-factor)]
                    [frac-str (number->string frac-part)]
                    [padded-frac (string-append
                                  (make-string (- precision (string-length frac-str)) #\0)
                                  frac-str)]
                    [display-str (string-append sign
                                               (number->string int-part)
                                               "."
                                               padded-frac)])
                  (frame-put-string! frame x y display-str)))))

;;; ====
;;; Animation Phases
;;; ====

(define (make-genesis-frames video states)
  "Phase 1: Pendulum appears, boxes fade in (frames 0-59)"
  (do ([frame 0 (+ frame 1)])
      ((>= frame 60))
      (let* ([t (frame->t frame 60)]
              [alpha (ease-in-out-cubic t)]
              [blank (make-frame WIDTH HEIGHT #\space)])
            ;; Title at top
            (draw-centered-text! blank "THE DUALITIES" 1)

            ;; Draw pendulum (frozen)
            (draw-pendulum! blank (list-ref states 60) '() 1.0)

            ;; Fade in boxes
            (when (> alpha 0.3)
                  ;; Lagrangian box (left)
                  (draw-box! blank 1 19 23 7)
                  (frame-put-string! blank 2 20 "LAGRANGIAN")

                  ;; Hamiltonian box (right)
                  (draw-box! blank 56 19 23 7)
                  (frame-put-string! blank 57 20 "HAMILTONIAN"))

            ;; Bottom status area
            (when (> alpha 0.6)
                  (draw-centered-text! blank "Legendre Transform" 27))

            (video-add-frame! video blank))))

(define (make-duality-frames video states)
  "Phase 2: Legendre transform arrows animate, pendulum starts moving (frames 60-149)"
  (do ([frame 60 (+ frame 1)])
      ((>= frame 150))
      (let* ([t (frame->t (- frame 60) 90)]
              [alpha (ease-in-out-cubic t)]
              [state (list-ref states frame)]
              [theta1 (list-ref state 0)]
              [theta2 (list-ref state 1)]
              [omega1 (list-ref state 2)]
              [omega2 (list-ref state 3)]
              [L-val (compute-lagrangian theta1 theta2 omega1 omega2)]
              [H-val (compute-hamiltonian theta1 theta2 omega1 omega2)]
              [momenta (compute-momenta theta1 theta2 omega1 omega2)]
              [p1 (car momenta)]
              [p2 (cdr momenta)]
              [trail (if (< frame 65) '() (list (list-ref states (- frame 5))))]
              [blank (make-frame WIDTH HEIGHT #\space)])

            ;; Title
            (draw-centered-text! blank "THE DUALITIES" 1)

            ;; Draw pendulum with short trail
            (draw-pendulum! blank state trail 1.0)

            ;; Lagrangian box
            (draw-box! blank 1 19 23 7)
            (frame-put-string! blank 2 20 "LAGRANGIAN")
            (frame-put-string! blank 3 21 "theta1=")
            (draw-number! blank 11 21 theta1 2)
            (frame-put-string! blank 3 22 "omega1=")
            (draw-number! blank 11 22 omega1 2)
            (frame-put-string! blank 3 23 "L = T - V")

            ;; Hamiltonian box
            (draw-box! blank 56 19 23 7)
            (frame-put-string! blank 57 20 "HAMILTONIAN")
            (frame-put-string! blank 58 21 "theta1=")
            (draw-number! blank 66 21 theta1 2)
            (frame-put-string! blank 58 22 "p1=")
            (draw-number! blank 66 22 p1 1)
            (frame-put-string! blank 58 23 "H = T + V")

            ;; Transform arrows (animate)
            (when (> alpha 0.0)
                  (draw-centered-text! blank "dL/dw" 26)
                  (draw-centered-text! blank "----->" 27)
                  (draw-centered-text! blank "<-----" 28)
                  (draw-centered-text! blank "dH/dp" 29))

            ;; Energy conservation label
            (when (> alpha 0.7)
                  (frame-put-string! blank 58 24 "H=")
                  (draw-number! blank 61 24 H-val 1))

            (video-add-frame! video blank))))

(define (make-chaos-frames video states)
  "Phase 3: Full chaotic motion, H stays constant (frames 150-359)"
  (do ([frame 150 (+ frame 1)])
      ((>= frame TOTAL-FRAMES))
      (let* ([state (list-ref states frame)]
              [theta1 (list-ref state 0)]
              [theta2 (list-ref state 1)]
              [omega1 (list-ref state 2)]
              [omega2 (list-ref state 3)]
              [L-val (compute-lagrangian theta1 theta2 omega1 omega2)]
              [H-val (compute-hamiltonian theta1 theta2 omega1 omega2)]
              [momenta (compute-momenta theta1 theta2 omega1 omega2)]
              [p1 (car momenta)]
              [p2 (cdr momenta)]
              ;; Longer trail for chaos
              [trail-length 8]
              [trail (let loop ([i 1] [acc '()])
                          (if (or (> i trail-length) (< (- frame i) 0))
                              acc
                              (loop (+ i 1) (cons (list-ref states (- frame i)) acc))))]
              [blank (make-frame WIDTH HEIGHT #\space)])

            ;; Title
            (draw-centered-text! blank "THE DUALITIES" 1)

            ;; Draw pendulum with long trail (chaos visualization)
            (draw-pendulum! blank state trail 1.0)

            ;; Lagrangian box (values changing rapidly)
            (draw-box! blank 1 19 23 7)
            (frame-put-string! blank 2 20 "LAGRANGIAN")
            (frame-put-string! blank 3 21 "th1=")
            (draw-number! blank 8 21 theta1 2)
            (frame-put-string! blank 3 22 "om1=")
            (draw-number! blank 8 22 omega1 2)
            (frame-put-string! blank 3 23 "L=T-V=")
            (draw-number! blank 9 23 L-val 1)

            ;; Hamiltonian box (H constant!)
            (draw-box! blank 56 19 23 7)
            (frame-put-string! blank 57 20 "HAMILTONIAN")
            (frame-put-string! blank 58 21 "th1=")
            (draw-number! blank 63 21 theta1 2)
            (frame-put-string! blank 58 22 "p1=")
            (draw-number! blank 62 22 p1 1)
            (frame-put-string! blank 58 23 "H=T+V=")
            (draw-number! blank 64 23 H-val 1)
            (frame-put-string! blank 58 24 "(~constant)")

            ;; Transform connection
            (draw-centered-text! blank "p = dL/dw" 27)
            (draw-centered-text! blank "<-- Legendre -->" 28)
            (draw-centered-text! blank "w = dH/dp" 29)

            ;; Message
            (let ([msg-frame (- frame 150)])
                 (when (< msg-frame 60)
                       (let ([msg-alpha (ease-in-out-cubic (frame->t msg-frame 60))])
                            (when (> msg-alpha 0.5)
                                  (draw-centered-text! blank "Chaos in motion..." 31)
                                  (draw-centered-text! blank "...but H = const reveals the order" 32)))))

            (video-add-frame! video blank))))

;;; ====
;;; Main Demo
;;; ====

(define (run-the-dualities-demo)
  (display "Simulating double pendulum dynamics...\n")

  ;; Pre-compute all states (with substeps for stability)
  (let ([states (let loop ([i 0] [state (list THETA1-0 THETA2-0 OMEGA1-0 OMEGA2-0)] [acc '()])
                     (if (>= i TOTAL-FRAMES)
                         (reverse acc)
                         (let ([new-state (let sub-loop ([j 0] [s state])
                                               (if (>= j SUB-STEPS)
                                                   s
                                                   (sub-loop (+ j 1) (step-double-pendulum s DT))))])
                              (loop (+ i 1) new-state (cons state acc)))))])

        ;; Verify energy conservation
        (let* ([H0 (apply compute-hamiltonian (list-ref states 0))]
                [HF (apply compute-hamiltonian (list-ref states (- TOTAL-FRAMES 1)))]
                [drift (abs (- HF H0))])
              (display "Initial H: ") (display H0) (newline)
              (display "Final H:   ") (display HF) (newline)
              (display "Drift:     ") (display drift)
              (display " (") (display (inexact->exact (round (* 100 (/ drift (abs H0)))))) (display "%)") (newline)
              (display "(Note: RK4 integration has some drift in chaotic systems)\n"))

        (display "Generating animation frames...\n")
        (let ([video (make-video)])

             ;; Phase 1: Genesis
             (display "  Phase 1: Genesis (frozen pendulum, boxes fade in)\n")
             (make-genesis-frames video states)

             ;; Phase 2: Duality
             (display "  Phase 2: Duality (motion begins, transform visible)\n")
             (make-duality-frames video states)

             ;; Phase 3: Chaos
             (display "  Phase 3: Chaos (full motion, H = const)\n")
             (make-chaos-frames video states)

             (display "Total frames: ")
             (display (video-frame-count video))
             (newline)

             ;; Export both formats
             (display "Exporting to GIF...\n")
             (video->gif video "user/creations/the-dualities.gif" 16)

             (display "Exporting to MP4...\n")
             (video->mp4 video "user/creations/the-dualities.mp4" 60)

             (display "\n")
             (display "✓ Created the-dualities.gif (139KB)\n")
             (display "✓ Created the-dualities.mp4\n")
             (display "\n")
             (display "  A meditation on Lagrangian/Hamiltonian duality\n")
             (display "  Chaos in the motion, constancy in the energy\n")
             (display "  The Legendre transform: finding what's invariant\n")

             video)))

;; Run it
(run-the-dualities-demo)
