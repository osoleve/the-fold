;;; lattice/sim/manifest.sexp — Simulation Streams Skill Manifest

(skill sim
  (version "0.3.0")
  (tier 2)
  (path "lattice/sim")
  (purity total)  ; Pure stream-based simulation
  (stability experimental)
  (fuel-bound "O(n) per simulation step")
  (deps (data linalg random numeric fp geometry))

  (description
   "Stream-based simulation framework for continuous dynamical systems.
    Provides lazy simulation streams, physics state management, numerical
    integrators, chaos analysis, bifurcation analysis, and strange attractor
    visualization. Supports n-body systems, Lyapunov exponents, parameter
    continuation, and bifurcation detection (saddle-node, Hopf, pitchfork).")

  (keywords (simulation streams dynamics physics lazy chaos
             integrators n-body continuous time lyapunov attractor
             lorenz rossler poincare bifurcation fractal continuation
             hopf saddle-node pitchfork period-doubling))
  (aliases (simulation simulation-stream chaos dynamics bifurcation))

  (concepts
    (concept dynamical-systems
      (description "Continuous-time systems: ODE solvers, chaos detection (Lyapunov exponents), attractors, and bifurcation analysis.")
      (parent physics-simulation)
      (synonyms simulation simulation-stream chaos dynamics chaos-theory lyapunov-exponents strange-attractor poincare-section bifurcation hopf saddle-node pitchfork period-doubling feigenbaum)))

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
   (bifurcation make-parameterized-ode instantiate-at continue-fixed-point
                find-all-fixed-points detect-bifurcations
                bifurcation-diagram-equilibria bifurcation-diagram-amplitude
                bifurcation-diagram-poincare find-hopf-bifurcation hopf-criticality
                detect-period-doubling feigenbaum-delta find-saddle-node-bifurcation
                analyze-bifurcations lorenz-rho van-der-pol-mu
                pitchfork-normal-form-param saddle-node-normal-form-param
                transcritical-normal-form-param hopf-normal-form-param)
   (attractor-render render-attractor render-attractor-colored
                     render-spinning-attractor render-spinning-attractor-colored
                     demo-lorenz demo-rossler)
   (ode-adaptive dp45-step ode-adaptive-integrate ode-solve ode-solve/tol
                 ode-system-integrate ode-result-trajectory ode-result-steps
                 ode-result-rejections ode-result-truncated? ode-result-final
                 ode-result-final-state ode-result-times default-ode-opts))

  (modules
   (simulation-stream "simulation-stream.ss" "Core simulation stream abstraction and utilities")
   (ode-system "dynamics/ode-system.ss" "ODE system representation and standard systems")
   (stability "dynamics/stability.ss" "Fixed point detection and stability analysis")
   (chaos "dynamics/chaos.ss" "Chaos detection, Lyapunov exponents, strange attractors")
   (bifurcation "dynamics/bifurcation.ss" "Bifurcation analysis: parameter continuation, detection, diagrams")
   (attractor-render "dynamics/attractor-render.ss" "ASCII visualization of attractors")
   (ode-adaptive "dynamics/ode-adaptive.ss" "Adaptive step-size ODE solver (Dormand-Prince RK4(5))")))
