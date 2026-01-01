;;; fabric/stitches/physics-2d/integrators.ss — 2D Physics Integration
;;;
;;; Physics integration specialized for 2D game physics, wrapping the
;;; generic numerical integration library with vec2 support.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Vec2-based state representation
;;;   - Multiple integration methods (Euler, Verlet, RK4)
;;;   - Sub-stepping for stable simulation
;;;   - Time accumulator for fixed timestep
;;;   - Energy metrics for debugging
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - vec2.ss (2D vector math)
;;;   - numerical/integrators.ss (generic methods)

(load "core/prelude.ss")
(load "core/vec2.ss")
(load "user/physics/numerical/integrators.ss")

;;; ============================================================
;;; 2D Physics Body State
;;; ============================================================

;;; A physics body in 2D has:
;;;   - pos: position (vec2)
;;;   - vel: velocity (vec2)
;;;   - mass: mass (number > 0)
;;;   - inv-mass: inverse mass (0 for static bodies)

;;; make-body-2d : Vec2 × Vec2 × Number → Body2D
;;; Create a 2D physics body.
(define (make-body-2d pos vel mass)
  (list 'body-2d
        pos
        vel
        mass
        (if (= mass 0) 0 (/ 1 mass))))

;;; body-2d? : Any → Boolean
(define (body-2d? b)
  (and (pair? b) (eq? (car b) 'body-2d)))

;;; body-pos : Body2D → Vec2
(define (body-pos b) (list-ref b 1))

;;; body-vel : Body2D → Vec2
(define (body-vel b) (list-ref b 2))

;;; body-mass : Body2D → Number
(define (body-mass b) (list-ref b 3))

;;; body-inv-mass : Body2D → Number
(define (body-inv-mass b) (list-ref b 4))

;;; body-static? : Body2D → Boolean
(define (body-static? b) (= (body-inv-mass b) 0))

;;; body-with-pos : Body2D × Vec2 → Body2D
;;; Update body position.
(define (body-with-pos b new-pos)
  (make-body-2d new-pos (body-vel b) (body-mass b)))

;;; body-with-vel : Body2D × Vec2 → Body2D
;;; Update body velocity.
(define (body-with-vel b new-vel)
  (make-body-2d (body-pos b) new-vel (body-mass b)))

;;; body-with-state : Body2D × Vec2 × Vec2 → Body2D
;;; Update body position and velocity.
(define (body-with-state b new-pos new-vel)
  (make-body-2d new-pos new-vel (body-mass b)))

;;; make-static-body : Vec2 → Body2D
;;; Create a static (immovable) body.
(define (make-static-body pos)
  (make-body-2d pos (vec2 0 0) 0))

;;; ============================================================
;;; Vec2 to State Conversion
;;; ============================================================

;;; Vec2 ↔ list state conversion for generic integrators

;;; vec2->state : Vec2 → State
(define (vec2->state v)
  (list (vec2-x v) (vec2-y v)))

;;; state->vec2 : State → Vec2
(define (state->vec2 s)
  (vec2 (car s) (cadr s)))

;;; body->state : Body2D → (Pos-State × Vel-State)
;;; Convert body to generic state representation.
(define (body->state b)
  (list (vec2->state (body-pos b))
        (vec2->state (body-vel b))))

;;; state->body : (Pos-State × Vel-State) × Number → Body2D
;;; Convert generic state back to body.
(define (state->body s mass)
  (make-body-2d (state->vec2 (car s))
                (state->vec2 (cadr s))
                mass))

;;; ============================================================
;;; 2D Integration Methods
;;; ============================================================

;;; integrate-body-euler : Body2D × (Body2D → Vec2) × Number → Body2D
;;; Integrate a body forward using Euler method.
;;; force-fn: (body) → force vector
(define (integrate-body-euler body force-fn dt)
  (if (body-static? body)
      body
      (let* ([force (force-fn body)]
             [accel (vec2-scale force (body-inv-mass body))]
             [new-vel (vec2-add (body-vel body) (vec2-scale accel dt))]
             [new-pos (vec2-add (body-pos body) (vec2-scale new-vel dt))])
            (body-with-state body new-pos new-vel))))

;;; integrate-body-symplectic : Body2D × (Body2D → Vec2) × Number → Body2D
;;; Integrate using symplectic Euler (more stable).
(define (integrate-body-symplectic body force-fn dt)
  (if (body-static? body)
      body
      (let* ([force (force-fn body)]
             [accel (vec2-scale force (body-inv-mass body))]
             ;; Update velocity first
             [new-vel (vec2-add (body-vel body) (vec2-scale accel dt))]
             ;; Then position using new velocity
             [new-pos (vec2-add (body-pos body) (vec2-scale new-vel dt))])
            (body-with-state body new-pos new-vel))))

;;; integrate-body-verlet : Body2D × (Body2D → Vec2) × Number → Body2D
;;; Integrate using velocity Verlet method.
(define (integrate-body-verlet body force-fn dt)
  (if (body-static? body)
      body
      (let* ([pos (body-pos body)]
             [vel (body-vel body)]
             [inv-m (body-inv-mass body)]
             ;; Acceleration at current position
             [accel (vec2-scale (force-fn body) inv-m)]
             ;; Half-step position
             [half-dt (/ dt 2)]
             ;; Full position update: x + v*dt + 0.5*a*dt²
             [new-pos (vec2-add (vec2-add pos (vec2-scale vel dt))
                                (vec2-scale accel (* 0.5 dt dt)))]
             ;; Acceleration at new position
             [new-body-temp (body-with-pos body new-pos)]
             [accel-new (vec2-scale (force-fn new-body-temp) inv-m)]
             ;; Velocity update: v + 0.5*(a + a_new)*dt
             [new-vel (vec2-add vel
                                (vec2-scale (vec2-add accel accel-new) half-dt))])
            (body-with-state body new-pos new-vel))))

;;; integrate-body-rk4 : Body2D × ((Body2D × Number) → Vec2) × Number × Number → Body2D
;;; Integrate using RK4 method.
;;; force-fn: (body, time) → force vector
(define (integrate-body-rk4 body force-fn t dt)
  (if (body-static? body)
      body
      (let* ([inv-m (body-inv-mass body)]
             ;; RK4 on the (pos, vel) state
             [pos-state (vec2->state (body-pos body))]
             [vel-state (vec2->state (body-vel body))]
             ;; Derivative function for RK4
             ;; state = (x, y, vx, vy)
             [full-state (append pos-state vel-state)]
             [deriv-fn (lambda (time state)
                               (let* ([pos (state->vec2 (take state 2))]
                                      [vel (state->vec2 (drop state 2))]
                                      [temp-body (make-body-2d pos vel (body-mass body))]
                                      [force (force-fn temp-body time)]
                                      [accel (vec2-scale force inv-m)])
                                     (append vel-state (vec2->state accel))))]
             [new-state (rk4-step deriv-fn t full-state dt)]
             [new-pos (state->vec2 (take new-state 2))]
             [new-vel (state->vec2 (drop new-state 2))])
            (body-with-state body new-pos new-vel))))

;;; ============================================================
;;; Time Accumulator (Fixed Timestep)
;;; ============================================================

;;; A time accumulator manages fixed timestep with interpolation
;;;   - accumulator: remaining time to simulate
;;;   - fixed-dt: fixed timestep
;;;   - max-steps: maximum substeps per frame

;;; make-time-acc : Number × Number → TimeAcc
(define (make-time-acc fixed-dt max-steps)
  (list 'time-acc 0 fixed-dt max-steps))

;;; time-acc? : Any → Boolean
(define (time-acc? t) (and (pair? t) (eq? (car t) 'time-acc)))

;;; time-acc-remaining : TimeAcc → Number
(define (time-acc-remaining t) (list-ref t 1))

;;; time-acc-fixed-dt : TimeAcc → Number
(define (time-acc-fixed-dt t) (list-ref t 2))

;;; time-acc-max-steps : TimeAcc → Number
(define (time-acc-max-steps t) (list-ref t 3))

;;; time-acc-add : TimeAcc × Number → TimeAcc
;;; Add elapsed time to accumulator.
(define (time-acc-add t dt)
  (list 'time-acc
        (+ (time-acc-remaining t) dt)
        (time-acc-fixed-dt t)
        (time-acc-max-steps t)))

;;; time-acc-consume : TimeAcc → (TimeAcc × Number)
;;; Consume one fixed timestep if available.
;;; Returns updated accumulator and number of steps consumed.
(define (time-acc-consume t)
  (let ([remaining (time-acc-remaining t)]
        [fixed-dt (time-acc-fixed-dt t)])
       (if (>= remaining fixed-dt)
           (list (list 'time-acc
                       (- remaining fixed-dt)
                       fixed-dt
                       (time-acc-max-steps t))
                 1)
           (list t 0))))

;;; time-acc-alpha : TimeAcc → Number
;;; Get interpolation factor (0 to 1) for rendering.
(define (time-acc-alpha t)
  (let ([remaining (time-acc-remaining t)]
        [fixed-dt (time-acc-fixed-dt t)])
       (/ remaining fixed-dt)))

;;; ============================================================
;;; Sub-stepping
;;; ============================================================

;;; substep-body : Body2D × (Body2D → Vec2) × Number × Nat × Symbol → Body2D
;;; Integrate body with n substeps using specified method.
(define (substep-body body force-fn dt n method)
  (let ([sub-dt (/ dt n)])
       (let loop ([b body] [i 0])
            (if (>= i n)
                b
                (let ([new-b (case method
                                   [(euler) (integrate-body-euler b force-fn sub-dt)]
                                   [(symplectic) (integrate-body-symplectic b force-fn sub-dt)]
                                   [(verlet) (integrate-body-verlet b force-fn sub-dt)]
                                   [else (integrate-body-symplectic b force-fn sub-dt)])])
                     (loop new-b (+ i 1)))))))

;;; integrate-with-substeps : Body2D × (Body2D → Vec2) × TimeAcc → (Body2D × TimeAcc)
;;; Integrate body consuming all available fixed timesteps.
(define (integrate-with-substeps body force-fn time-acc)
  (let ([fixed-dt (time-acc-fixed-dt time-acc)]
        [max-steps (time-acc-max-steps time-acc)])
       (let loop ([b body] [t time-acc] [steps 0])
            (let* ([result (time-acc-consume t)]
                   [new-t (car result)]
                   [consumed (cadr result)])
                  (if (or (= consumed 0) (>= steps max-steps))
                      (list b new-t)
                      (let ([new-b (integrate-body-verlet b force-fn fixed-dt)])
                           (loop new-b new-t (+ steps 1))))))))

;;; ============================================================
;;; Interpolation for Rendering
;;; ============================================================

;;; interpolate-body : Body2D × Body2D × Number → Body2D
;;; Interpolate between two body states for smooth rendering.
;;; alpha = 0 gives body-prev, alpha = 1 gives body-curr.
(define (interpolate-body body-prev body-curr alpha)
  (let* ([pos (vec2-lerp (body-pos body-prev) (body-pos body-curr) alpha)]
         [vel (vec2-lerp (body-vel body-prev) (body-vel body-curr) alpha)])
        (body-with-state body-curr pos vel)))

;;; ============================================================
;;; Common Force Functions
;;; ============================================================

;;; gravity-force : Number → (Body2D → Vec2)
;;; Create gravity force function (pointing downward in screen coords).
(define (gravity-force g)
  (lambda (body)
          (vec2 0 (* g (body-mass body)))))

;;; spring-force : Vec2 × Number × Number → (Body2D → Vec2)
;;; Create spring force toward anchor point.
;;; k: spring constant, damping: damping coefficient
(define (spring-force anchor k damping)
  (lambda (body)
          (let* ([pos (body-pos body)]
                 [vel (body-vel body)]
                 [displacement (vec2-sub anchor pos)]
                 [spring-f (vec2-scale displacement k)]
                 [damp-f (vec2-scale vel (- damping))])
                (vec2-add spring-f damp-f))))

;;; drag-force : Number → (Body2D → Vec2)
;;; Create drag force proportional to velocity.
(define (drag-force coefficient)
  (lambda (body)
          (vec2-scale (body-vel body) (- coefficient))))

;;; constant-force : Vec2 → (Body2D → Vec2)
;;; Create constant force (ignores body).
(define (constant-force f)
  (lambda (body) f))

;;; combine-forces : ((Body2D → Vec2) ...) → (Body2D → Vec2)
;;; Combine multiple force functions.
(define (combine-forces . force-fns)
  (lambda (body)
          (fold-left (lambda (total f) (vec2-add total (f body)))
                     (vec2 0 0)
                     force-fns)))

;;; ============================================================
;;; Energy Calculations
;;; ============================================================

;;; body-kinetic-energy : Body2D → Number
(define (body-kinetic-energy body)
  (* 0.5 (body-mass body) (vec2-magnitude-sq (body-vel body))))

;;; body-potential-energy : Body2D × Number × Number → Number
;;; Gravitational potential energy (y increases downward).
(define (body-potential-energy body g y-ref)
  (* (body-mass body) g (- y-ref (vec2-y (body-pos body)))))

;;; body-spring-energy : Body2D × Vec2 × Number → Number
;;; Spring potential energy.
(define (body-spring-energy body anchor k)
  (let ([dist (vec2-length (vec2-sub (body-pos body) anchor))])
       (* 0.5 k dist dist)))

;;; body-total-energy : Body2D × Number × Number → Number
;;; Total mechanical energy (KE + gravitational PE).
(define (body-total-energy body g y-ref)
  (+ (body-kinetic-energy body)
     (body-potential-energy body g y-ref)))
