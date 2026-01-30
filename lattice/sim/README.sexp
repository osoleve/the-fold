(module sim
(title "Simulation Stream Abstraction")
(purpose "Bridges physics simulation state with lazy streams for infinite sequences of timesteps")
(exports
 (make-sim-state "Create simulation state snapshot")
 (sim-state? "Test for simulation state")
 (sim-state-time "Get simulation time")
 (sim-state-data "Get simulation data")
 (sim-state-metadata "Get simulation metadata")
 (sim-unfold "Build stream from step function (may terminate)")
 (sim-unfold-infinite "Build infinite stream from step function")
 (sim-unfold-with-state "Stream of states by repeated application")
 (simulate "Create infinite stream of simulation states")
 (simulate-with-dt "Simulate with dt passed to step function")
 (simulate-varying-dt "Simulation with variable timestep")
 (sim-scan "Running fold over stream (alias for stream-scan)")
 (sim-scan-map "Scan with mapping function")
 (sim-accumulate "Accumulate simulation states")
 (make-physics-state "Create pure physics state")
 (physics-pos "Get position from physics state")
 (physics-vel "Get velocity from physics state")
 (physics-mass "Get mass from physics state")
 (euler-step "Create Euler integration step")
 (symplectic-euler-step "Create symplectic Euler step")
 (verlet-step "Create velocity Verlet step")
 (const-gravity "Constant gravity force")
 (spring-force-fn "Spring force toward anchor")
 (drag-force-fn "Velocity-proportional drag")
 (combine-force-fns "Combine multiple forces")
 (sim-positions "Extract position stream")
 (sim-velocities "Extract velocity stream")
 (sim-times "Extract time stream")
 (sim-kinetic-energy "Extract kinetic energy stream")
 (sim-potential-energy "Extract potential energy stream")
 (sim-total-energy "Extract total energy stream")
 (make-n-body-state "Create n-body simulation state")
 (n-body-step "Create step function for n-body simulation")
 (sim-par "Parallel step functions")
 (sim-seq "Sequential step functions")
 (sim-when "Conditional step function")
 (sim-switch "Switch between step functions"))
(usage-examples 
  ((simulate-projectile "Create projectile simulation under gravity"
   (let* ((initial (make-physics-state
       (vec2 0 0)
       (vec2 10 10) 1.0 0.0))
     (gravity (const-gravity 9.8))
     (step-fn (symplectic-euler-step gravity 0.01))
     (stream (simulate initial step-fn 0.01)))
    (stream->list 100 stream)))
  (unfold-fibonacci "Generate Fibonacci sequence using unfold"
   (let ((step (lambda
       (pair)
       (cons
        (car pair)
        (cons
         (cdr pair)
         (+
          (car pair)
          (cdr pair)))))))
    (stream->list 10
     (sim-unfold-infinite step
      (cons 0 1)))))
  (running-sum "Compute running sum of simulation data"
   (sim-scan + 0
    (sim-times simulation-stream)))
  (energy-tracking "Track energy throughout simulation"
   (sim-total-energy 9.8 0 physics-stream))))
(dependencies ("core/base/prelude.ss" "Base utilities") ("lattice/fp/data/stream.ss" "Lazy stream primitives") ("lattice/linalg/vec2.ss" "2D vector math"))
(design-notes
 (pure-simulation "Unlike user/physics/world.ss which uses mutable state,\n       simulation-stream.ss provides purely functional simulation.\n       State is never mutated; each step produces a new state.")
 (infinite-streams "Simulations are modeled as potentially infinite streams.\n       Use stream-take, sim-until, or fuel parameters to bound\n       computation when needed.")
 (codata-pattern "This module follows the codata pattern from stream.ss.\n       Simulations are defined by their observations (step function)\n       rather than their construction (finite data).")
 (integration-methods "Three integration methods are provided:\n       - euler-step: Simple but energy drift\n       - symplectic-euler-step: Better energy preservation\n       - verlet-step: Second-order accurate, best for physics"))
(test-file "test-simulation-stream.ss")
(tests-count 46))
