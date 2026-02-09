
(load "core/base/prelude.ss")
(load "lattice/linalg/vec2.ss")
(load "lattice/physics/classical/ode-integrators.ss")


(doc 'module 'integrators)
(doc 'description "2D physics integration with vec2 support")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'section '2d)

(doc "A physics body in 2D has:")
(doc "  - pos: position (vec2)")
(doc "  - vel: velocity (vec2)")
(doc "  - mass: mass (number > 0)")
(doc "  - inv-mass: inverse mass (0 for static bodies)")

(doc "make-body-2d : Vec2 × Vec2 × Number → Body2D")
(doc "Create a 2D physics body.")
(define (make-body-2d pos vel mass)
  (list 'body-2d
        pos
        vel
        mass
        (if (= mass 0) 0 (/ 1 mass))))

(doc "body-2d? : Any → Boolean")
(define (body-2d? b)
  (and (pair? b) (eq? (car b) 'body-2d)))

(define (body-pos b) (list-ref b 1))
  (doc 'type '(body-pos : Body2D → Vec2))

(define (body-vel b) (list-ref b 2))
  (doc 'type '(body-vel : Body2D → Vec2))

(define (body-mass b) (list-ref b 3))
  (doc 'type '(body-mass : Body2D → Number))

(define (body-inv-mass b) (list-ref b 4))
  (doc 'type '(body-inv-mass : Body2D → Number))

(doc "body-static? : Body2D → Boolean")
(define (body-static? b) (= (body-inv-mass b) 0))

(doc 'body-with-pos 'type 'Body2D × Vec2 → Body2D)
(doc "Update body position.")
(define (body-with-pos b new-pos)
  (make-body-2d new-pos (body-vel b) (body-mass b)))

(doc 'body-with-vel 'type 'Body2D × Vec2 → Body2D)
(doc "Update body velocity.")
(define (body-with-vel b new-vel)
  (make-body-2d (body-pos b) new-vel (body-mass b)))

(doc 'body-with-state 'type 'Body2D × Vec2 × Vec2 → Body2D)
(doc "Update body position and velocity.")
(define (body-with-state b new-pos new-vel)
  (make-body-2d new-pos new-vel (body-mass b)))

(doc 'make-static-body 'type 'Vec2 → Body2D)
(doc "Create a static (immovable) body.")
(define (make-static-body pos)
  (make-body-2d pos (vec2 0 0) 0))

(doc 'section 'vec2)

(doc "Vec2 ↔ list state conversion for generic integrators")

(doc "vec2->state : Vec2 → State")
(define (vec2->state v)
  (list (vec2-x v) (vec2-y v)))

(doc "state->vec2 : State → Vec2")
(define (state->vec2 s)
  (vec2 (car s) (cadr s)))

(doc "body->state : Body2D → (Pos-State × Vel-State)")
(doc "Convert body to generic state representation.")
(define (body->state b)
  (list (vec2->state (body-pos b))
        (vec2->state (body-vel b))))

(doc "state->body : (Pos-State × Vel-State) × Number → Body2D")
(doc "Convert generic state back to body.")
(define (state->body s mass)
  (make-body-2d (state->vec2 (car s))
                (state->vec2 (cadr s))
                mass))

(doc 'section '2d)

(doc 'integrate-body-euler 'type 'Body2D × (Body2D → Vec2) × Number → Body2D)
(doc "Integrate a body forward using Euler method.")
(doc "force-fn: (body) → force vector")
(define (integrate-body-euler body force-fn dt)
  (if (body-static? body)
      body
      (let* ([force (force-fn body)]
             [accel (vec2-scale force (body-inv-mass body))]
             [new-vel (vec2-add (body-vel body) (vec2-scale accel dt))]
             [new-pos (vec2-add (body-pos body) (vec2-scale new-vel dt))])
            (body-with-state body new-pos new-vel))))

(doc 'integrate-body-symplectic 'type 'Body2D × (Body2D → Vec2) × Number → Body2D)
(doc "Integrate using symplectic Euler (more stable).")
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

(doc 'integrate-body-verlet 'type 'Body2D × (Body2D → Vec2) × Number → Body2D)
(doc "Integrate using velocity Verlet method.")
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

(doc "integrate-body-rk4 : Body2D × ((Body2D × Number) → Vec2) × Number × Number → Body2D")
(doc "Integrate using RK4 method.")
(doc "force-fn: (body, time) → force vector")
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

(doc 'section 'time)

(doc "A time accumulator manages fixed timestep with interpolation")
(doc "  - accumulator: remaining time to simulate")
(doc "  - fixed-dt: fixed timestep")
(doc "  - max-steps: maximum substeps per frame")

(define (make-time-acc fixed-dt max-steps)
  (doc 'type '(make-time-acc : Number × Number → TimeAcc))
  (list 'time-acc 0 fixed-dt max-steps))

(doc "time-acc? : Any → Boolean")
(define (time-acc? t) (and (pair? t) (eq? (car t) 'time-acc)))

(define (time-acc-remaining t) (list-ref t 1))
  (doc 'type '(time-acc-remaining : TimeAcc → Number))

(define (time-acc-fixed-dt t) (list-ref t 2))
  (doc 'type '(time-acc-fixed-dt : TimeAcc → Number))

(define (time-acc-max-steps t) (list-ref t 3))
  (doc 'type '(time-acc-max-steps : TimeAcc → Number))

(doc 'time-acc-add 'type 'TimeAcc × Number → TimeAcc)
(doc "Add elapsed time to accumulator.")
(define (time-acc-add t dt)
  (list 'time-acc
        (+ (time-acc-remaining t) dt)
        (time-acc-fixed-dt t)
        (time-acc-max-steps t)))

(doc 'time-acc-consume 'type 'TimeAcc → (TimeAcc × Number))
(doc "Consume one fixed timestep if available.")
(doc "Returns updated accumulator and number of steps consumed.")
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

(doc 'time-acc-alpha 'type 'TimeAcc → Number)
(doc "Get interpolation factor (0 to 1) for rendering.")
(define (time-acc-alpha t)
  (let ([remaining (time-acc-remaining t)]
        [fixed-dt (time-acc-fixed-dt t)])
       (/ remaining fixed-dt)))

(doc 'section 'sub-stepping)

(doc 'substep-body 'type 'Body2D × (Body2D → Vec2) × Number × Nat × Symbol → Body2D)
(doc "Integrate body with n substeps using specified method.")
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

(doc 'integrate-with-substeps 'type 'Body2D × (Body2D → Vec2) × TimeAcc → (Body2D × TimeAcc))
(doc "Integrate body consuming all available fixed timesteps.")
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

(doc 'section 'interpolation)

(doc 'interpolate-body 'type 'Body2D × Body2D × Number → Body2D)
(doc "Interpolate between two body states for smooth rendering.")
(doc "alpha = 0 gives body-prev, alpha = 1 gives body-curr.")
(define (interpolate-body body-prev body-curr alpha)
  (let* ([pos (vec2-lerp (body-pos body-prev) (body-pos body-curr) alpha)]
         [vel (vec2-lerp (body-vel body-prev) (body-vel body-curr) alpha)])
        (body-with-state body-curr pos vel)))

(doc 'section 'common)

(doc 'gravity-force 'type 'Number → (Body2D → Vec2))
(doc "Create gravity force function (pointing downward in screen coords).")
(define (gravity-force g)
  (lambda (body)
          (vec2 0 (* g (body-mass body)))))

(doc 'spring-force 'type 'Vec2 × Number × Number → (Body2D → Vec2))
(doc "Create spring force toward anchor point.")
(doc "k: spring constant, damping: damping coefficient")
(define (spring-force anchor k damping)
  (lambda (body)
          (let* ([pos (body-pos body)]
                 [vel (body-vel body)]
                 [displacement (vec2-sub anchor pos)]
                 [spring-f (vec2-scale displacement k)]
                 [damp-f (vec2-scale vel (- damping))])
                (vec2-add spring-f damp-f))))

(doc 'drag-force 'type 'Number → (Body2D → Vec2))
(doc "Create drag force proportional to velocity.")
(define (drag-force coefficient)
  (lambda (body)
          (vec2-scale (body-vel body) (- coefficient))))

(doc 'constant-force 'type 'Vec2 → (Body2D → Vec2))
(doc "Create constant force (ignores body).")
(define (constant-force f)
  (lambda (body) f))

(doc 'combine-forces 'type '((Body2D → Vec2) ...) → (Body2D → Vec2))
(doc "Combine multiple force functions.")
(define (combine-forces . force-fns)
  (lambda (body)
          (fold-left (lambda (total f) (vec2-add total (f body)))
                     (vec2 0 0)
                     force-fns)))

(doc 'section 'energy)

(define (body-kinetic-energy body)
  (doc 'type '(body-kinetic-energy : Body2D → Number))
  (* 0.5 (body-mass body) (vec2-magnitude-sq (body-vel body))))

(doc 'body-potential-energy 'type 'Body2D × Number × Number → Number)
(doc "Gravitational potential energy (y increases downward).")
(define (body-potential-energy body g y-ref)
  (* (body-mass body) g (- y-ref (vec2-y (body-pos body)))))

(doc 'body-spring-energy 'type 'Body2D × Vec2 × Number → Number)
(doc "Spring potential energy.")
(define (body-spring-energy body anchor k)
  (let ([dist (vec2-length (vec2-sub (body-pos body) anchor))])
       (* 0.5 k dist dist)))

(doc 'body-total-energy 'type 'Body2D × Number × Number → Number)
(doc "Total mechanical energy (KE + gravitational PE).")
(define (body-total-energy body g y-ref)
  (+ (body-kinetic-energy body)
     (body-potential-energy body g y-ref)))

(doc 'section 'rigid)

(doc "Load rigid body module")
(load "lattice/physics/classical/rigid-body.ss")


(doc 'module 'integrators)
(doc 'description "2D physics integration with vec2 support")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'integrate-rigid-body-symplectic 'type 'RigidBody2D x Vec2 x Number x Number -> RigidBody2D)
(doc "Integrate a rigid body using symplectic Euler.")
(doc "linear-accel: linear acceleration (force/mass)")
(doc "angular-accel: angular acceleration (torque/inertia)")
(define (integrate-rigid-body-symplectic body linear-accel angular-accel dt)
  (if (rigid-body-static? body)
      body
      (let* (;; Update velocities first (symplectic)
             [new-vel (vec2-add (rigid-body-vel body)
                                (vec2-scale linear-accel dt))]
             [new-omega (+ (rigid-body-angular-vel body)
                           (* angular-accel dt))]
             ;; Then update positions using new velocities
             [new-pos (vec2-add (rigid-body-pos body)
                                (vec2-scale new-vel dt))]
             [new-angle (+ (rigid-body-angle body)
                           (* new-omega dt))])
            (rigid-body-with-state body new-pos new-vel new-angle new-omega))))

(doc 'integrate-rigid-body-euler 'type 'RigidBody2D x Vec2 x Number x Number -> RigidBody2D)
(doc "Integrate a rigid body using forward Euler.")
(define (integrate-rigid-body-euler body linear-accel angular-accel dt)
  (if (rigid-body-static? body)
      body
      (let* ([vel (rigid-body-vel body)]
             [omega (rigid-body-angular-vel body)]
             ;; Update positions using current velocities
             [new-pos (vec2-add (rigid-body-pos body)
                                (vec2-scale vel dt))]
             [new-angle (+ (rigid-body-angle body)
                           (* omega dt))]
             ;; Then update velocities
             [new-vel (vec2-add vel (vec2-scale linear-accel dt))]
             [new-omega (+ omega (* angular-accel dt))])
            (rigid-body-with-state body new-pos new-vel new-angle new-omega))))

(doc 'integrate-rigid-body-verlet 'type 'RigidBody2D x (RigidBody2D -> Vec2) x (RigidBody2D -> Number) x Number -> RigidBody2D)
(doc "Integrate a rigid body using velocity Verlet.")
(doc "force-fn: body -> force vector")
(doc "torque-fn: body -> torque scalar")
(define (integrate-rigid-body-verlet body force-fn torque-fn dt)
  (if (rigid-body-static? body)
      body
      (let* ([pos (rigid-body-pos body)]
             [vel (rigid-body-vel body)]
             [angle (rigid-body-angle body)]
             [omega (rigid-body-angular-vel body)]
             [inv-m (rigid-body-inv-mass body)]
             [inv-i (rigid-body-inv-inertia body)]
             ;; Current accelerations
             [accel (vec2-scale (force-fn body) inv-m)]
             [alpha (* (torque-fn body) inv-i)]
             [half-dt (/ dt 2)]
             ;; Position updates: x + v*dt + 0.5*a*dt^2
             [new-pos (vec2-add (vec2-add pos (vec2-scale vel dt))
                                (vec2-scale accel (* 0.5 dt dt)))]
             [new-angle (+ angle (* omega dt) (* 0.5 alpha dt dt))]
             ;; Temporary body at new position for force evaluation
             [temp-body (rigid-body-with-state body new-pos vel new-angle omega)]
             ;; New accelerations
             [accel-new (vec2-scale (force-fn temp-body) inv-m)]
             [alpha-new (* (torque-fn temp-body) inv-i)]
             ;; Velocity updates: v + 0.5*(a + a_new)*dt
             [new-vel (vec2-add vel
                                (vec2-scale (vec2-add accel accel-new) half-dt))]
             [new-omega (+ omega (* 0.5 (+ alpha alpha-new) dt))])
            (rigid-body-with-state body new-pos new-vel new-angle new-omega))))

(doc 'rigid-body-gravity-force 'type 'Vec2 -> (RigidBody2D -> Vec2))
(doc "Create gravity force function for rigid bodies.")
(define (rigid-body-gravity-force gravity)
  (lambda (body)
          (vec2-scale gravity (rigid-body-mass body))))

(doc 'rigid-body-damping-force 'type 'Number x Number -> (RigidBody2D -> Vec2) x (RigidBody2D -> Number))
(doc "Create linear and angular damping functions.")
(doc "Returns (values linear-damping-fn angular-damping-fn)")
(define (rigid-body-damping linear-coeff angular-coeff)
  (values
   (lambda (body)
           (vec2-scale (rigid-body-vel body) (- linear-coeff)))
   (lambda (body)
           (* (- angular-coeff) (rigid-body-angular-vel body)))))

(doc 'rigid-body-lerp 'type 'RigidBody2D x RigidBody2D x Number -> RigidBody2D)
(doc "Interpolate between two rigid body states.")
(define (rigid-body-lerp body-prev body-curr alpha)
  (let* ([pos (vec2-lerp (rigid-body-pos body-prev)
                         (rigid-body-pos body-curr)
                         alpha)]
         [vel (vec2-lerp (rigid-body-vel body-prev)
                         (rigid-body-vel body-curr)
                         alpha)]
         [angle (+ (* (- 1 alpha) (rigid-body-angle body-prev))
                   (* alpha (rigid-body-angle body-curr)))]
         [omega (+ (* (- 1 alpha) (rigid-body-angular-vel body-prev))
                   (* alpha (rigid-body-angular-vel body-curr)))])
        (rigid-body-with-state body-curr pos vel angle omega)))
