;;; lattice/sim/manifest.sexp — Simulation Streams Skill Manifest

(skill sim
  (version "0.2.0")
  (tier 2)
  (path "lattice/sim")
  (purity total)  ; Pure stream-based simulation
  (stability experimental)
  (fuel-bound "O(n) per simulation step")
  (deps (data linalg random numeric fp geometry))

  (description
   "Stream-based simulation framework for continuous dynamical systems.
    Provides lazy simulation streams, physics state management, numerical
    integrators, chaos analysis, and strange attractor visualization.
    Supports n-body systems, Lyapunov exponents, and modular force functions.")

  (keywords (simulation streams dynamics physics lazy chaos
             integrators n-body continuous time lyapunov attractor
             lorenz rossler poincare bifurcation fractal))
  (aliases (simulation simulation-stream chaos dynamics))

  (exports
   (simulation-stream sim-unfold simulate sim-scan)
   (ode-system make-autonomous-ode make-nonautonomous-ode lorenz-system
               harmonic-oscillator van-der-pol lotka-volterra eval-vector-field)
   (stability compute-jacobian analyze-stability classify-stability-2d
              find-fixed-point-newton is-equilibrium?)
   (chaos integrate-rk4 generate-attractor lyapunov-exponents
          largest-lyapunov-exponent kaplan-yorke-dimension is-chaotic?
          poincare-section lorenz-classic rossler-classic chen-classic
          thomas-classic aizawa-classic rossler-system chen-system
          thomas-attractor aizawa-system chaos-summary)
   (attractor-render render-attractor render-attractor-colored
                     render-spinning-attractor render-spinning-attractor-colored
                     play-animation loop-animation demo-lorenz demo-rossler))

  (modules
   (simulation-stream "simulation-stream.ss" "Core simulation stream abstraction and utilities")
   (ode-system "dynamics/ode-system.ss" "ODE system representation and standard systems")
   (stability "dynamics/stability.ss" "Fixed point detection and stability analysis")
   (chaos "dynamics/chaos.ss" "Chaos detection, Lyapunov exponents, strange attractors")
   (attractor-render "dynamics/attractor-render.ss" "ASCII visualization of attractors")))
