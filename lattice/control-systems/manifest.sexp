;;; lattice/control-systems/manifest.sexp — Control Systems Engineering Manifest

(skill control-systems
  (version "0.1.0")
  (tier 2)
  (path "lattice/control-systems")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n^3) for state-space operations")

  (deps (linalg numeric algebra))

  (description
   "Control theory and dynamical systems: state-space models, transfer functions,
    controller synthesis (LQR, pole placement, PID), Kalman filtering, stability
    analysis (Routh-Hurwitz, Lyapunov, Nyquist), and discrete-time methods.")

  (keywords (control-theory state-space transfer-function kalman-filter
             pid-controller lqr pole-placement stability z-transform
             discrete-control continuous-to-discrete))

  (modules
   (state-space "state-space.ss" "LTI state space models, controllability, observability")
   (transfer-function "transfer-function.ss" "Continuous transfer functions (S-domain)")
   (z-transform "z-transform.ss" "Discrete transfer functions (Z-domain)")
   (tf-convert "tf-convert.ss" "State-space <-> transfer function conversion")
   (kalman "kalman.ss" "Kalman filter for state estimation")
   (stability "stability.ss" "Stability analysis: Routh-Hurwitz, Lyapunov, Nyquist")
   (controller-design "controller-design.ss" "Controller synthesis: LQR, pole placement, PID tuning")
   (digital-pid "digital-pid.ss" "Discrete PID with anti-windup and tuning rules")
   (discrete-control "discrete-control.ss" "Continuous-to-discrete conversion (c2d-zoh, c2d-tustin)")
   (hinf-synthesis "hinf-synthesis.ss" "H-infinity robust controller synthesis")
   (poly-algebra "poly-algebra.ss" "Polynomial algebra: GCD, simplification, coprimality")))
