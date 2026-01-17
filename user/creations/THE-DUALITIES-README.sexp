(creation the-dualities
  (title "The Dualities")
  (subtitle "Lagrangian/Hamiltonian Mechanics in ASCII")
  (created "2026-01-17")
  (type terminal-animation)

  (description
   "A terminal animation showcasing the beautiful duality between Lagrangian
    and Hamiltonian formulations of classical mechanics through a double
    pendulum simulation. The demo visualizes how the Legendre transform
    connects these two perspectives on the same physical system.")

  (technical-details
   (system double-pendulum)
   (masses (M1 2.0) (M2 1.5))
   (lengths (L1 8.0) (L2 6.0))
   (gravity 9.8)
   (integrator RK4)
   (timestep 0.004)
   (substeps 4)
   (resolution "80x34 characters")
   (frames 360)
   (duration "6 seconds @ 60fps"))

  (physics
   (lagrangian "L = T - V (kinetic minus potential)")
   (hamiltonian "H = T + V (total energy, conserved)")
   (legendre-transform
    "Forward:  p_i = ∂L/∂ω_i (velocity to momentum)"
    "Inverse:  ω_i = ∂H/∂p_i (momentum to velocity)")
   (conservation
    "The Hamiltonian H is conserved (constant) in time, even as
     the system exhibits chaotic motion. This is the power of the
     Hamiltonian formulation: it reveals what's invariant."))

  (aesthetic
   (philosophy
    "Chaos in the motion, constancy in the energy. The Legendre
     transform isn't just a change of variables — it's finding
     the conserved perspective. As θ, ω, and p churn chaotically,
     H = const remains the invariant truth.")
   (visual-design
    (pendulum "White @ character with fading trail showing chaos")
    (lagrangian-box "Left panel: θ, ω (velocity coordinates)")
    (hamiltonian-box "Right panel: θ, p (momentum coordinates)")
    (transform-arrows "Center: bidirectional Legendre transform")
    (energy-display "H value shown as ~constant despite numerical drift")))

  (animation-phases
   (genesis
    (frames "0-59")
    (description "Pendulum frozen, info boxes fade in")
    (easing ease-in-out-cubic))
   (duality
    (frames "60-149")
    (description "Pendulum begins moving, transform animates")
    (highlight "Real-time computation of L, H, and conjugate momenta"))
   (chaos
    (frames "150-359")
    (description "Full chaotic motion with long trail")
    (message "Chaos in motion... but H = const reveals the order")))

  (files
   (source "the-dualities.ss")
   (output-gif "the-dualities.gif" "139KB")
   (output-mp4 "the-dualities.mp4"))

  (usage
   (generate "scheme --script user/creations/the-dualities.ss")
   (output "Creates the-dualities.gif in user/creations/"))

  (demoscene-aesthetic
   (constraint-philosophy
    "80 columns is all you need. The terminal is humanity's most honest canvas.")
   (color-palette
    "Monochrome with character density gradients: . : * o O @")
   (animation-principles
    (smooth "60fps for organic motion")
    (easing "ease-in-out-cubic for phase transitions")
    (trail "Fading previous positions to show chaos")
    (hold "Title and messages given breathing room")))

  (theoretical-background
   (mechanics
    "Classical mechanics can be formulated in multiple equivalent ways:

     1. NEWTONIAN: F = ma (forces and accelerations)
     2. LAGRANGIAN: L = T - V (energy difference, velocity coordinates)
     3. HAMILTONIAN: H = T + V (total energy, momentum coordinates)

     The Legendre transform connects Lagrangian ↔ Hamiltonian:

       p_i = ∂L/∂q̇_i  (momentum is derivative of L wrt velocity)
       H(q,p) = Σ p_i q̇_i - L(q,q̇)  (Legendre transform definition)

     The beauty: H is conserved (constant) even in chaotic systems.
     This is Noether's theorem in action: time-translation symmetry
     → energy conservation.")

   (double-pendulum
    "The double pendulum is a canonical example of deterministic chaos.
     Two coupled pendulums exhibit:
     - Sensitive dependence on initial conditions
     - Ergodic behavior (filling phase space)
     - Yet strict energy conservation (H = const)

     The equations of motion are derived from the Lagrangian:

       L = ½(M1+M2)L1²ω1² + ½M2L2²ω2² + M2L1L2ω1ω2cos(θ1-θ2)
           - (M1+M2)gL1cos(θ1) - M2gL2cos(θ2)

     Euler-Lagrange equations d/dt(∂L/∂ω) - ∂L/∂θ = 0 give coupled ODEs."))

  (acknowledgments
   "Built with The Fold's ASCII video system and physics modules.
    Inspired by the demoscene tradition of finding beauty in constraints.

    The double pendulum teaches us: chaos and conservation coexist.
    The Hamiltonian perspective reveals the order within the chaos."))
