;;; core/sim/simulation-stream.ss --- Lazy Stream Abstraction for Simulations
;;;
;;; Bridges physics simulation state with lazy streams for infinite
;;; sequences of timesteps. Enables functional simulation patterns:
;;;   (take 1000 (simulate initial-state))
;;;   (unfold step-fn state)
;;;   (scan accumulate stream)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Pure simulation stepping (no mutation)
;;;   - Infinite stream of simulation states
;;;   - Generalized unfold for any step function
;;;   - scan for accumulating values over streams
;;;   - Integration with lazy stream primitives
;;;   - Physics-specific simulation helpers
;;;
;;; Dependencies:
;;;   - base/prelude.ss
;;;   - fp/data/stream.ss (lazy streams)
;;;   - vec2.ss (2D vectors)

(load "core/base/prelude.ss")
(load "core/fp/data/stream.ss")
(load "core/linalg/vec2.ss")

;;; ============================================================
;;; Core Simulation Stream Types
;;; ============================================================

;;; A SimState represents a snapshot of a simulation at a point in time.
;;; It is generic and can hold any simulation state (physics, particles, etc.)
;;;
;;; sim-state : (tag, time, state-data, metadata)

;;; make-sim-state : Number x Any x Any -> SimState
;;; Create a simulation state snapshot.
(define (make-sim-state time state-data metadata)
  (list 'sim-state time state-data metadata))

;;; sim-state? : Any -> Boolean
(define (sim-state? s)
  (and (pair? s) (eq? (car s) 'sim-state)))

;;; sim-state-time : SimState -> Number
(define (sim-state-time s) (list-ref s 1))

;;; sim-state-data : SimState -> Any
(define (sim-state-data s) (list-ref s 2))

;;; sim-state-metadata : SimState -> Any
(define (sim-state-metadata s) (list-ref s 3))

;;; sim-state-with-time : SimState x Number -> SimState
(define (sim-state-with-time s new-time)
  (make-sim-state new-time (sim-state-data s) (sim-state-metadata s)))

;;; sim-state-with-data : SimState x Any -> SimState
(define (sim-state-with-data s new-data)
  (make-sim-state (sim-state-time s) new-data (sim-state-metadata s)))

;;; ============================================================
;;; Generalized Unfold
;;; ============================================================
;;;
;;; unfold is the fundamental corecursive operation for building streams.
;;; Given a step function and initial seed, it produces an infinite stream
;;; of values.
;;;
;;; There are two variants:
;;;   1. sim-unfold: step returns (value . next-seed) or #f to terminate
;;;   2. sim-unfold-infinite: step always returns (value . next-seed)

;;; sim-unfold : (seed -> (value . seed) | #f) x seed -> Stream value
;;; Build a stream from a step function that may terminate.
;;; Step function returns (value . new-seed) to continue, or #f to stop.
(define (sim-unfold step seed)
  (let ([result (step seed)])
       (if (not result)
           stream-nil
           (stream-cons (car result)
                        (lambda () (sim-unfold step (cdr result)))))))

;;; sim-unfold-infinite : (seed -> (value . seed)) x seed -> Stream value
;;; Build an infinite stream from a step function.
;;; Step function always returns (value . new-seed).
(define (sim-unfold-infinite step seed)
  (let ([result (step seed)])
       (stream-cons (car result)
                    (lambda () (sim-unfold-infinite step (cdr result))))))

;;; sim-unfold-with-state : (s -> s) x s -> Stream s
;;; Unfold producing the state itself at each step.
;;; Equivalent to stream-iterate but named for simulation context.
(define (sim-unfold-with-state step seed)
  (stream-cons seed
               (lambda () (sim-unfold-with-state step (step seed)))))

;;; ============================================================
;;; Simulation Stream Constructor
;;; ============================================================

;;; simulate : SimState x (SimState -> SimState) x Number -> Stream SimState
;;; Create an infinite stream of simulation states.
;;; - initial: starting simulation state
;;; - step-fn: pure function that advances state by dt
;;; - dt: time delta per step (fixed timestep)
(define (simulate initial step-fn dt)
  (let ([stepper (lambda (state)
                         (let* ([new-state (step-fn state)]
                                [new-time (+ (sim-state-time state) dt)]
                                [result (sim-state-with-time new-state new-time)])
                               (cons result result)))])
       (stream-cons initial
                    (lambda () (sim-unfold-infinite stepper initial)))))

;;; simulate-with-dt : SimState x (SimState x Number -> SimState) x Number -> Stream SimState
;;; Like simulate, but step function also receives dt.
(define (simulate-with-dt initial step-fn dt)
  (let ([stepper (lambda (state)
                         (let* ([new-state (step-fn state dt)]
                                [new-time (+ (sim-state-time state) dt)]
                                [result (sim-state-with-time new-state new-time)])
                               (cons result result)))])
       (stream-cons initial
                    (lambda () (sim-unfold-infinite stepper initial)))))

;;; simulate-varying-dt : SimState x (SimState x Number -> (SimState x Number)) -> Stream SimState
;;; Simulation with variable timestep.
;;; Step function returns new state AND next dt.
(define (simulate-varying-dt initial step-fn)
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

;;; ============================================================
;;; Scan (Running Fold)
;;; ============================================================
;;;
;;; scan produces a stream of accumulated values.
;;; scan f z [s1, s2, s3, ...] = [z, f(z,s1), f(f(z,s1),s2), ...]

;;; sim-scan : (acc x value -> acc) x acc x Stream value -> Stream acc
;;; Running fold over a stream, producing stream of intermediate results.
;;; This is an alias for stream-scan with simulation-specific naming.
(define sim-scan stream-scan)

;;; sim-scan-map : (acc x value -> acc) x acc x (value -> mapped) x Stream value -> Stream acc
;;; Scan with an optional mapping function applied before accumulation.
(define (sim-scan-map f init mapper s)
  (stream-scan f init (stream-map mapper s)))

;;; sim-accumulate : (acc x SimState -> acc) x acc x Stream SimState -> Stream acc
;;; Accumulate simulation states into running totals.
(define sim-accumulate stream-scan)

;;; ============================================================
;;; Physics-Specific Simulation Helpers
;;; ============================================================

;;; Pure physics body for stream-based simulation
;;; (Unlike world.ss bodies which are mutated, these are purely functional)

;;; make-physics-state : Vec2 x Vec2 x Number x Number -> PhysicsState
;;; Create a pure physics state for a single body.
;;; - pos: position
;;; - vel: velocity
;;; - mass: mass (or 0 for static)
;;; - time: simulation time
(define (make-physics-state pos vel mass time)
  (make-sim-state time (list 'physics pos vel mass) '()))

;;; physics-state? : SimState -> Boolean
(define (physics-state? s)
  (and (sim-state? s)
       (let ([data (sim-state-data s)])
            (and (pair? data) (eq? (car data) 'physics)))))

;;; physics-pos : PhysicsState -> Vec2
(define (physics-pos s)
  (list-ref (sim-state-data s) 1))

;;; physics-vel : PhysicsState -> Vec2
(define (physics-vel s)
  (list-ref (sim-state-data s) 2))

;;; physics-mass : PhysicsState -> Number
(define (physics-mass s)
  (list-ref (sim-state-data s) 3))

;;; physics-inv-mass : PhysicsState -> Number
(define (physics-inv-mass s)
  (let ([m (physics-mass s)])
       (if (= m 0) 0 (/ 1 m))))

;;; make-updated-physics : PhysicsState x Vec2 x Vec2 -> PhysicsState
;;; Create new physics state with updated position and velocity.
(define (make-updated-physics s new-pos new-vel)
  (make-sim-state (sim-state-time s)
                  (list 'physics new-pos new-vel (physics-mass s))
                  (sim-state-metadata s)))

;;; ============================================================
;;; Physics Integration as Pure Step Functions
;;; ============================================================

;;; euler-step : (PhysicsState -> Vec2) x Number -> (PhysicsState -> PhysicsState)
;;; Create an Euler integration step function.
;;; force-fn: computes force given current state
;;; dt: timestep
(define (euler-step force-fn dt)
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [inv-m (physics-inv-mass state)]
                 [force (force-fn state)]
                 [accel (vec2-scale force inv-m)]
                 [new-vel (vec2-add vel (vec2-scale accel dt))]
                 [new-pos (vec2-add pos (vec2-scale new-vel dt))])
                (make-updated-physics state new-pos new-vel))))

;;; symplectic-euler-step : (PhysicsState -> Vec2) x Number -> (PhysicsState -> PhysicsState)
;;; Create a symplectic Euler step function (more stable).
(define (symplectic-euler-step force-fn dt)
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [inv-m (physics-inv-mass state)]
                 [force (force-fn state)]
                 [accel (vec2-scale force inv-m)]
                 ;; Update velocity first
                 [new-vel (vec2-add vel (vec2-scale accel dt))]
                 ;; Then position using new velocity
                 [new-pos (vec2-add pos (vec2-scale new-vel dt))])
                (make-updated-physics state new-pos new-vel))))

;;; verlet-step : (PhysicsState -> Vec2) x Number -> (PhysicsState -> PhysicsState)
;;; Create a velocity Verlet step function.
(define (verlet-step force-fn dt)
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [inv-m (physics-inv-mass state)]
                 [force (force-fn state)]
                 [accel (vec2-scale force inv-m)]
                 [half-dt (/ dt 2)]
                 ;; Position: x + v*dt + 0.5*a*dt^2
                 [new-pos (vec2-add (vec2-add pos (vec2-scale vel dt))
                                    (vec2-scale accel (* 0.5 dt dt)))]
                 ;; Acceleration at new position
                 [new-state-temp (make-updated-physics state new-pos vel)]
                 [accel-new (vec2-scale (force-fn new-state-temp) inv-m)]
                 ;; Velocity: v + 0.5*(a + a_new)*dt
                 [new-vel (vec2-add vel (vec2-scale (vec2-add accel accel-new) half-dt))])
                (make-updated-physics state new-pos new-vel))))

;;; ============================================================
;;; Common Force Functions (Pure)
;;; ============================================================

;;; const-gravity : Number -> (PhysicsState -> Vec2)
;;; Constant gravity force (positive y is down).
(define (const-gravity g)
  (lambda (state)
          (vec2 0 (* g (physics-mass state)))))

;;; spring-force-fn : Vec2 x Number x Number -> (PhysicsState -> Vec2)
;;; Spring force toward anchor point.
(define (spring-force-fn anchor k damping)
  (lambda (state)
          (let* ([pos (physics-pos state)]
                 [vel (physics-vel state)]
                 [displacement (vec2-sub anchor pos)]
                 [spring-f (vec2-scale displacement k)]
                 [damp-f (vec2-scale vel (- damping))])
                (vec2-add spring-f damp-f))))

;;; drag-force-fn : Number -> (PhysicsState -> Vec2)
;;; Drag force proportional to velocity.
(define (drag-force-fn coefficient)
  (lambda (state)
          (vec2-scale (physics-vel state) (- coefficient))))

;;; combine-force-fns : (List (PhysicsState -> Vec2)) -> (PhysicsState -> Vec2)
;;; Combine multiple force functions.
(define (combine-force-fns force-fns)
  (lambda (state)
          (fold-left (lambda (total f) (vec2-add total (f state)))
                     (vec2 0 0)
                     force-fns)))

;;; ============================================================
;;; Simulation Stream Utilities
;;; ============================================================

;;; sim-take : Number x Stream SimState -> (List SimState)
;;; Take n simulation states as a list.
(define sim-take stream->list)

;;; sim-at-time : Number x Stream SimState -> SimState | #f
;;; Find first state at or after given time.
(define (sim-at-time target-time stream fuel)
  (let loop ([s stream] [n fuel])
       (cond
        [(<= n 0) #f]
        [(stream-nil? s) #f]
        [(>= (sim-state-time (stream-head s)) target-time)
         (stream-head s)]
        [else (loop (stream-tail s) (- n 1))])))

;;; sim-sample : Number x Number x Stream SimState -> Stream SimState
;;; Sample simulation at given interval (skip intermediate states).
;;; Works correctly with variable-timestep simulations by tracking absolute time.
(define (sim-sample interval fuel stream)
  (if (stream-nil? stream)
      stream-nil
      (let* ([first-state (stream-head stream)]
             [start-time (sim-state-time first-state)])
            ;; Emit first state, then find states at start-time + interval, start-time + 2*interval, etc.
            (stream-cons first-state
                         (lambda ()
                                 (sim-sample-loop interval fuel stream (+ start-time interval) 1))))))

;;; sim-sample-loop : Number x Number x Stream SimState x Number x Number -> Stream SimState
;;; Helper: consume stream until we pass next-sample-time, then emit and continue.
(define (sim-sample-loop interval fuel stream next-sample-time count)
  (if (>= count fuel)
      stream-nil
      (let loop ([s stream])
           (cond
            [(stream-nil? s) stream-nil]
            [else
             (let* ([state (stream-head s)]
                    [t (sim-state-time state)])
                   (if (>= t next-sample-time)
                       ;; Found a state at or past the sample time - emit it
                       (stream-cons state
                                    (lambda ()
                                            (sim-sample-loop interval fuel s
                                                             (+ next-sample-time interval)
                                                             (+ count 1))))
                       ;; Keep consuming
                       (loop (stream-tail s))))]))))

;;; sim-until : (SimState -> Boolean) x Number x Stream SimState -> (List SimState)
;;; Collect states until predicate returns true (with fuel limit).
(define (sim-until pred fuel stream)
  (let loop ([s stream] [n fuel] [acc '()])
       (cond
        [(<= n 0) (reverse acc)]
        [(stream-nil? s) (reverse acc)]
        [(pred (stream-head s)) (reverse (cons (stream-head s) acc))]
        [else (loop (stream-tail s) (- n 1) (cons (stream-head s) acc))])))

;;; ============================================================
;;; Observable Streams
;;; ============================================================
;;;
;;; Extract observable quantities from simulation streams.

;;; sim-positions : Stream PhysicsState -> Stream Vec2
;;; Extract position stream from physics simulation.
(define (sim-positions stream)
  (stream-map physics-pos stream))

;;; sim-velocities : Stream PhysicsState -> Stream Vec2
;;; Extract velocity stream from physics simulation.
(define (sim-velocities stream)
  (stream-map physics-vel stream))

;;; sim-times : Stream SimState -> Stream Number
;;; Extract time stream from simulation.
(define (sim-times stream)
  (stream-map sim-state-time stream))

;;; sim-kinetic-energy : Stream PhysicsState -> Stream Number
;;; Extract kinetic energy stream.
(define (sim-kinetic-energy stream)
  (stream-map (lambda (s)
                      (* 0.5 (physics-mass s)
                         (vec2-magnitude-sq (physics-vel s))))
              stream))

;;; sim-potential-energy : Number x Number x Stream PhysicsState -> Stream Number
;;; Extract gravitational potential energy stream.
;;; g: gravity, y-ref: reference height
(define (sim-potential-energy g y-ref stream)
  (stream-map (lambda (s)
                      (* (physics-mass s) g (- y-ref (vec2-y (physics-pos s)))))
              stream))

;;; sim-total-energy : Number x Number x Stream PhysicsState -> Stream Number
;;; Extract total mechanical energy stream.
(define (sim-total-energy g y-ref stream)
  (stream-map (lambda (s)
                      (let ([ke (* 0.5 (physics-mass s)
                                   (vec2-magnitude-sq (physics-vel s)))]
                            [pe (* (physics-mass s) g
                                   (- y-ref (vec2-y (physics-pos s))))])
                           (+ ke pe)))
              stream))

;;; ============================================================
;;; Multi-Body Simulation
;;; ============================================================
;;;
;;; For simulations with multiple interacting bodies.

;;; make-n-body-state : (List PhysicsState) x Number -> SimState
;;; Create state for n-body simulation.
(define (make-n-body-state bodies time)
  (make-sim-state time (list 'n-body bodies) '()))

;;; n-body-state? : SimState -> Boolean
(define (n-body-state? s)
  (and (sim-state? s)
       (let ([data (sim-state-data s)])
            (and (pair? data) (eq? (car data) 'n-body)))))

;;; n-body-states : SimState -> (List PhysicsState)
(define (n-body-states s)
  (list-ref (sim-state-data s) 1))

;;; n-body-step : ((List PhysicsState) -> (List Vec2)) x Number -> (SimState -> SimState)
;;; Create step function for n-body simulation.
;;; force-fn: computes list of forces for each body
(define (n-body-step force-fn dt)
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

;;; ============================================================
;;; Simulation Combinators
;;; ============================================================

;;; sim-par : (List (SimState -> SimState)) -> (List SimState -> List SimState)
;;; Run multiple step functions in parallel (for independent subsystems).
(define (sim-par step-fns)
  (lambda (states)
          (map (lambda (step state) (step state))
               step-fns states)))

;;; sim-seq : (List (SimState -> SimState)) -> (SimState -> SimState)
;;; Chain step functions sequentially (for dependent updates).
(define (sim-seq step-fns)
  (lambda (state)
          (fold-left (lambda (s step) (step s)) state step-fns)))

;;; sim-when : (SimState -> Boolean) x (SimState -> SimState) -> (SimState -> SimState)
;;; Conditionally apply step function.
(define (sim-when pred step-fn)
  (lambda (state)
          (if (pred state)
              (step-fn state)
              state)))

;;; sim-switch : (SimState -> Symbol) x Alist -> (SimState -> SimState)
;;; Switch between step functions based on state.
(define (sim-switch selector cases)
  (lambda (state)
          (let* ([key (selector state)]
                 [step-fn (cdr (assq key cases))])
                (if step-fn
                    (step-fn state)
                    state))))
