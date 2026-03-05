;;; skills/control-systems.sexp — Skill curation file

(skill control-systems
  (description
    "Control theory and dynamical systems: state-space models, transfer functions,\n    controller synthesis (LQR, pole placement, PID), Kalman filtering, stability\n    analysis (Routh-Hurwitz, Lyapunov, Nyquist), and discrete-time methods.")

  (keywords (control-theory state-space transfer-function kalman-filter pid-controller lqr pole-placement stability z-transform discrete-control continuous-to-discrete))

  (concepts (control-theory))

  (modules
    state-space
    transfer-function
    z-transform
    tf-convert
    kalman
    stability
    controller-design
    digital-pid
    discrete-control
    hinf-synthesis
    poly-algebra
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
