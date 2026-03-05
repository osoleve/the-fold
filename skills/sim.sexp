;;; skills/sim.sexp — Skill curation file

(skill sim
  (description
    "Stream-based simulation framework for continuous dynamical systems.\n    Provides lazy simulation streams, physics state management, numerical\n    integrators, chaos analysis, bifurcation analysis, and strange attractor\n    visualization. Supports n-body systems, Lyapunov exponents, parameter\n    continuation, and bifurcation detection (saddle-node, Hopf, pitchfork).")

  (keywords (simulation streams dynamics physics lazy chaos integrators n-body continuous time lyapunov attractor lorenz rossler poincare bifurcation fractal continuation hopf saddle-node pitchfork period-doubling))

  (aliases (simulation simulation-stream chaos dynamics bifurcation))

  (concepts (dynamical-systems))

  (modules
    simulation-stream
    ode-system
    stability
    chaos
    bifurcation
    attractor-render
    ode-state-vec
    ode-adaptive
    sim/discrete
    ode-state-space
    des/event
    des/world
    des/engine
    des/schedule
    des/replicate
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
