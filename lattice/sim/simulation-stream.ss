(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude stream vec2
(require 'prelude)
(require 'stream)
(require 'vec2)

(doc 'module 'simulation-stream)
(doc 'description "Lazy Stream Abstraction for Simulations - bridges physics simulation state with lazy streams for infinite sequences of timesteps")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Enables functional simulation patterns: (take 1000 (simulate initial-state)), (unfold step-fn state), (scan accumulate stream)")
(doc 'features "Pure simulation stepping (no mutation), infinite stream of simulation states, generalized unfold, scan for accumulating values, integration with lazy stream primitives, physics-specific simulation helpers")

(doc 'section 'core-simulation-stream-types)
(doc 'note "A SimState represents a snapshot of a simulation at a point in time. It is generic and can hold any simulation state (physics, particles, etc.)")
(doc 'note "sim-state : (tag, time, state-data, metadata)")

(define (make-sim-state time state-data metadata)
  (doc 'type '(-> Number Any Any SimState))
  (doc 'description "Create a simulation state snapshot")
  (list 'sim-state time state-data metadata))

(define (sim-state? s)
  (doc 'type '(-> Any Boolean))
  (and (pair? s) (eq? (car s) 'sim-state)))

(define (sim-state-time s)
  (doc 'type '(-> SimState Number))
  (list-ref s 1))

(define (sim-state-data s)
  (doc 'type '(-> SimState Any))
  (list-ref s 2))

(define (sim-state-metadata s)
  (doc 'type '(-> SimState Any))
  (list-ref s 3))

(define (sim-state-with-time s new-time)
  (doc 'type '(-> SimState Number SimState))
  (make-sim-state new-time (sim-state-data s) (sim-state-metadata s)))

(define (sim-state-with-data s new-data)
  (doc 'type '(-> SimState Any SimState))
  (make-sim-state (sim-state-time s) new-data (sim-state-metadata s)))

(doc 'section 'generalized-unfold)
(doc 'note "unfold is the fundamental corecursive operation for building streams. Given a step function and initial seed, it produces an infinite stream of values.")
(doc 'note "Two variants: (1) sim-unfold: step returns (value . next-seed) or #f to terminate, (2) sim-unfold-infinite: step always returns (value . next-seed)")

(define (sim-unfold step seed)
  (doc 'export #t)
  (doc 'type '(-> (-> seed (U (Pair value seed) #f)) seed (Stream value)))
  (doc 'description "Build a stream from a step function that may terminate. Step function returns (value . new-seed) to continue, or #f to stop")
  (let ([result (step seed)])
       (if (not result)
           stream-nil
           (stream-cons (car result)
                        (lambda () (sim-unfold step (cdr result)))))))

(define (sim-unfold-infinite step seed)
  (doc 'type '(-> (-> seed (Pair value seed)) seed (Stream value)))
  (doc 'description "Build an infinite stream from a step function. Step function always returns (value . new-seed)")
  (let ([result (step seed)])
       (stream-cons (car result)
                    (lambda () (sim-unfold-infinite step (cdr result))))))

(define (sim-unfold-with-state step seed)
  (doc 'type '(-> (-> s s) s (Stream s)))
  (doc 'description "Unfold producing the state itself at each step. Equivalent to stream-iterate but named for simulation context")
  (stream-cons seed
               (lambda () (sim-unfold-with-state step (step seed)))))

(doc 'section 'simulation-stream-constructor)

(define (simulate initial step-fn dt)
  (doc 'export #t)
  (doc 'type '(-> SimState (-> SimState SimState) Number (Stream SimState)))
  (doc 'description "Create an infinite stream of simulation states")
  (doc 'param 'initial "starting simulation state")
  (doc 'param 'step-fn "pure function that advances state by dt")
  (doc 'param 'dt "time delta per step (fixed timestep)")
  (let ([stepper (lambda (state)
                         (let* ([new-state (step-fn state)]
                                [new-time (+ (sim-state-time state) dt)]
                                [result (sim-state-with-time new-state new-time)])
                               (cons result result)))])
       (stream-cons initial
                    (lambda () (sim-unfold-infinite stepper initial)))))

(define (simulate-with-dt initial step-fn dt)
  (doc 'type '(-> SimState (-> SimState Number SimState) Number (Stream SimState)))
  (doc 'description "Like simulate, but step function also receives dt")
  (let ([stepper (lambda (state)
                         (let* ([new-state (step-fn state dt)]
                                [new-time (+ (sim-state-time state) dt)]
                                [result (sim-state-with-time new-state new-time)])
                               (cons result result)))])
       (stream-cons initial
                    (lambda () (sim-unfold-infinite stepper initial)))))

(define (simulate-varying-dt initial step-fn)
  (doc 'type '(-> SimState (-> SimState Number (Pair SimState Number)) (Stream SimState)))
  (doc 'description "Simulation with variable timestep. Step function returns new state AND next dt")
  (letrec ([make-stream
            (lambda (state dt)
                    (stream-cons state
                                 (lambda ()
                                         (let* ([result (step-fn state dt)]
                                                [new-state (car result)]
                                                [new-dt (cdr result)]
                                                [new-time (+ (sim-state-time state) dt)]
                                                [timed-state (sim-state-with-time new-state new-time)])
                                               (make-stream timed-state new-dt)))))])
          (make-stream initial (sim-state-time initial))))

(doc 'section 'scan-running-fold)
(doc 'note "scan produces a stream of accumulated values: scan f z [s1, s2, s3, ...] = [z, f(z,s1), f(f(z,s1),s2), ...]")

(doc sim-scan 'export #t)
(define sim-scan stream-scan)
(doc sim-scan 'type '(-> (-> acc value acc) acc (Stream value) (Stream acc)))
(doc sim-scan 'description "Running fold over a stream, producing stream of intermediate results. Alias for stream-scan with simulation-specific naming")

(define (sim-scan-map f init mapper s)
  (doc 'type '(-> (-> acc value acc) acc (-> value mapped) (Stream value) (Stream acc)))
  (doc 'description "Scan with an optional mapping function applied before accumulation")
  (stream-scan f init (stream-map mapper s)))

(define sim-accumulate stream-scan)
(doc sim-accumulate 'type '(-> (-> acc SimState acc) acc (Stream SimState) (Stream acc)))
(doc sim-accumulate 'description "Accumulate simulation states into running totals")

(doc 'section 'physics-specific-simulation-helpers)
(doc 'note "Pure physics body for stream-based simulation (unlike world.ss bodies which are mutated, these are purely functional)")

(define (make-physics-state pos vel mass time)
  (doc 'type '(-> Vec2 Vec2 Number Number PhysicsState))
  (doc 'description "Create a pure physics state for a single body")
  (doc 'param 'pos "position")
  (doc 'param 'vel "velocity")
  (doc 'param 'mass "mass (or 0 for static)")
  (doc 'param 'time "simulation time")
  (make-sim-state time (list 'physics pos vel mass) '()))

(define (physics-state? s)
  (doc 'type '(-> SimState Boolean))
  (and (sim-state? s)
       (let ([data (sim-state-data s)])
            (and (pair? data) (eq? (car data) 'physics)))))

(define (physics-pos s)
  (doc 'type '(-> PhysicsState Vec2))
  (list-ref (sim-state-data s) 1))

(define (physics-vel s)
  (doc 'type '(-> PhysicsState Vec2))
  (list-ref (sim-state-data s) 2))

(define (physics-mass s)
  (doc 'type '(-> PhysicsState Number))
  (list-ref (sim-state-data s) 3))

(define (physics-inv-mass s)
  (doc 'type '(-> PhysicsState Number))
  (let ([m (physics-mass s)])
       (if (= m 0) 0 (/ 1 m))))

(define (make-updated-physics s new-pos new-vel)
  (doc 'type '(-> PhysicsState Vec2 Vec2 PhysicsState))
  (doc 'description "Create new physics state with updated position and velocity")
  (make-sim-state (sim-state-time s)
                  (list 'physics new-pos new-vel (physics-mass s))
                  (sim-state-metadata s)))

(doc 'section 'physics-integration-as-pure-step-functions)

(define (euler-step force-fn dt)
  (doc 'type '(-> (-> PhysicsState Vec2) Number (-> PhysicsState PhysicsState)))
  (doc 'description "Create an Euler integration step function")
  (doc 'param 'force-fn "computes force given current state")
  (doc 'param 'dt "timestep")
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [inv-m (physics-inv-mass state)]
                 [force (force-fn state)]
                 [accel (vec2-scale force inv-m)]
                 [new-vel (vec2-add vel (vec2-scale accel dt))]
                 [new-pos (vec2-add pos (vec2-scale new-vel dt))])
                (make-updated-physics state new-pos new-vel))))

(define (symplectic-euler-step force-fn dt)
  (doc 'type '(-> (-> PhysicsState Vec2) Number (-> PhysicsState PhysicsState)))
  (doc 'description "Create a symplectic Euler step function (more stable)")
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [inv-m (physics-inv-mass state)]
                 [force (force-fn state)]
                 [accel (vec2-scale force inv-m)]
                 [new-vel (vec2-add vel (vec2-scale accel dt))]
                 [new-pos (vec2-add pos (vec2-scale new-vel dt))])
                (make-updated-physics state new-pos new-vel))))

(define (verlet-step force-fn dt)
  (doc 'type '(-> (-> PhysicsState Vec2) Number (-> PhysicsState PhysicsState)))
  (doc 'description "Create a velocity Verlet step function")
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [inv-m (physics-inv-mass state)]
                 [force (force-fn state)]
                 [accel (vec2-scale force inv-m)]
                 [half-dt (/ dt 2)]
                 [new-pos (vec2-add (vec2-add pos (vec2-scale vel dt))
                                    (vec2-scale accel (* 0.5 dt dt)))]
                 [new-state-temp (make-updated-physics state new-pos vel)]
                 [accel-new (vec2-scale (force-fn new-state-temp) inv-m)]
                 [new-vel (vec2-add vel (vec2-scale (vec2-add accel accel-new) half-dt))])
                (make-updated-physics state new-pos new-vel))))

(doc 'section 'common-force-functions-pure)

(define (const-gravity g)
  (doc 'type '(-> Number (-> PhysicsState Vec2)))
  (doc 'description "Constant gravity force (positive y is down)")
  (lambda (state)
          (vec2 0 (* g (physics-mass state)))))

(define (spring-force-fn anchor k damping)
  (doc 'type '(-> Vec2 Number Number (-> PhysicsState Vec2)))
  (doc 'description "Spring force toward anchor point")
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [displacement (vec2-sub anchor pos)]
                 [spring-f (vec2-scale displacement k)]
                 [damp-f (vec2-scale vel (- damping))])
                (vec2-add spring-f damp-f))))

(define (drag-force-fn coefficient)
  (doc 'type '(-> Number (-> PhysicsState Vec2)))
  (doc 'description "Drag force proportional to velocity")
  (lambda (state)
          (vec2-scale (physics-vel state) (- coefficient))))

(define (combine-force-fns force-fns)
  (doc 'type '(-> (List (-> PhysicsState Vec2)) (-> PhysicsState Vec2)))
  (doc 'description "Combine multiple force functions")
  (lambda (state)
          (fold-left (lambda (total f) (vec2-add total (f state)))
                     (vec2 0 0)
                     force-fns)))

(doc 'section 'simulation-stream-utilities)

(define sim-take stream->list)
(doc sim-take 'type '(-> Number (Stream SimState) (List SimState)))
(doc sim-take 'description "Take n simulation states as a list")

(define (sim-at-time target-time stream fuel)
  (doc 'type '(-> Number (Stream SimState) Number (U SimState #f)))
  (doc 'description "Find first state at or after given time")
  (let loop ([s stream] [n fuel])
       (cond
        [(<= n 0) #f]
        [(stream-nil? s) #f]
        [(>= (sim-state-time (stream-head s)) target-time)
         (stream-head s)]
        [else (loop (stream-tail s) (- n 1))])))

(define (sim-sample interval fuel stream)
  (doc 'type '(-> Number Number (Stream SimState) (Stream SimState)))
  (doc 'description "Sample simulation at given interval (skip intermediate states). Works correctly with variable-timestep simulations by tracking absolute time")
  (if (stream-nil? stream)
      stream-nil
      (let* ([first-state (stream-head stream)]
             [start-time (sim-state-time first-state)])
            (stream-cons first-state
                         (lambda ()
                                 (sim-sample-loop interval fuel stream (+ start-time interval) 1))))))

(define (sim-sample-loop interval fuel stream next-sample-time count)
  (doc 'type '(-> Number Number (Stream SimState) Number Number (Stream SimState)))
  (doc 'description "Helper: consume stream until we pass next-sample-time, then emit and continue")
  (if (>= count fuel)
      stream-nil
      (let loop ([s stream])
           (cond
            [(stream-nil? s) stream-nil]
            [else
             (let* ([state (stream-head s)]
                    [t (sim-state-time state)])
                   (if (>= t next-sample-time)
                       (stream-cons state
                                    (lambda ()
                                            (sim-sample-loop interval fuel s
                                                             (+ next-sample-time interval)
                                                             (+ count 1))))
                       (loop (stream-tail s))))]))))

(define (sim-until pred fuel stream)
  (doc 'type '(-> (-> SimState Boolean) Number (Stream SimState) (List SimState)))
  (doc 'description "Collect states until predicate returns true (with fuel limit)")
  (let loop ([s stream] [n fuel] [acc '()])
       (cond
        [(<= n 0) (reverse acc)]
        [(stream-nil? s) (reverse acc)]
        [(pred (stream-head s)) (reverse (cons (stream-head s) acc))]
        [else (loop (stream-tail s) (- n 1) (cons (stream-head s) acc))])))

(doc 'section 'observable-streams)
(doc 'note "Extract observable quantities from simulation streams")

(define (sim-positions stream)
  (doc 'type '(-> (Stream PhysicsState) (Stream Vec2)))
  (doc 'description "Extract position stream from physics simulation")
  (stream-map physics-pos stream))

(define (sim-velocities stream)
  (doc 'type '(-> (Stream PhysicsState) (Stream Vec2)))
  (doc 'description "Extract velocity stream from physics simulation")
  (stream-map physics-vel stream))

(define (sim-times stream)
  (doc 'type '(-> (Stream SimState) (Stream Number)))
  (doc 'description "Extract time stream from simulation")
  (stream-map sim-state-time stream))

(define (sim-kinetic-energy stream)
  (doc 'type '(-> (Stream PhysicsState) (Stream Number)))
  (doc 'description "Extract kinetic energy stream")
  (stream-map (lambda (s)
                      (* 0.5 (physics-mass s)
                         (vec2-magnitude-sq (physics-vel s))))
              stream))

(define (sim-potential-energy g y-ref stream)
  (doc 'type '(-> Number Number (Stream PhysicsState) (Stream Number)))
  (doc 'description "Extract gravitational potential energy stream")
  (doc 'param 'g "gravity")
  (doc 'param 'y-ref "reference height")
  (stream-map (lambda (s)
                      (* (physics-mass s) g (- y-ref (vec2-y (physics-pos s)))))
              stream))

(define (sim-total-energy g y-ref stream)
  (doc 'type '(-> Number Number (Stream PhysicsState) (Stream Number)))
  (doc 'description "Extract total mechanical energy stream")
  (stream-map (lambda (s)
                      (let ([ke (* 0.5 (physics-mass s)
                                   (vec2-magnitude-sq (physics-vel s)))]
                            [pe (* (physics-mass s) g
                                   (- y-ref (vec2-y (physics-pos s))))])
                           (+ ke pe)))
              stream))

(doc 'section 'multi-body-simulation)
(doc 'note "For simulations with multiple interacting bodies")

(define (make-n-body-state bodies time)
  (doc 'type '(-> (List PhysicsState) Number SimState))
  (doc 'description "Create state for n-body simulation")
  (make-sim-state time (list 'n-body bodies) '()))

(define (n-body-state? s)
  (doc 'type '(-> SimState Boolean))
  (and (sim-state? s)
       (let ([data (sim-state-data s)])
            (and (pair? data) (eq? (car data) 'n-body)))))

(define (n-body-states s)
  (doc 'type '(-> SimState (List PhysicsState)))
  (list-ref (sim-state-data s) 1))

(define (n-body-step force-fn dt)
  (doc 'type '(-> (-> (List PhysicsState) (List Vec2)) Number (-> SimState SimState)))
  (doc 'description "Create step function for n-body simulation")
  (doc 'param 'force-fn "computes list of forces for each body")
  (lambda (state)
          (let* ([bodies (n-body-states state)]
                 [forces (force-fn bodies)]
                 [new-bodies
                  (map (lambda (body force)
                               (let* ([pos (physics-pos body)]
                                      [vel (physics-vel body)]
                                      [inv-m (physics-inv-mass body)]
                                      [accel (vec2-scale force inv-m)]
                                      [new-vel (vec2-add vel (vec2-scale accel dt))]
                                      [new-pos (vec2-add pos (vec2-scale new-vel dt))])
                                     (make-updated-physics body new-pos new-vel)))
                       bodies forces)])
                (make-n-body-state new-bodies (+ (sim-state-time state) dt)))))

(doc 'section 'simulation-combinators)

(define (sim-par step-fns)
  (doc 'type '(-> (List (-> SimState SimState)) (-> (List SimState) (List SimState))))
  (doc 'description "Run multiple step functions in parallel (for independent subsystems)")
  (lambda (states)
          (map (lambda (step state) (step state))
               step-fns states)))

(define (sim-seq step-fns)
  (doc 'type '(-> (List (-> SimState SimState)) (-> SimState SimState)))
  (doc 'description "Chain step functions sequentially (for dependent updates)")
  (lambda (state)
          (fold-left (lambda (s step) (step s)) state step-fns)))

(define (sim-when pred step-fn)
  (doc 'type '(-> (-> SimState Boolean) (-> SimState SimState) (-> SimState SimState)))
  (doc 'description "Conditionally apply step function")
  (lambda (state)
          (if (pred state)
              (step-fn state)
              state)))

(define (sim-switch selector cases)
  (doc 'type '(-> (-> SimState Symbol) Alist (-> SimState SimState)))
  (doc 'description "Switch between step functions based on state")
  (lambda (state)
          (let* ([key (selector state)]
                 [step-fn (cdr (assq key cases))])
                (if step-fn
                    (step-fn state)
                    state))))
