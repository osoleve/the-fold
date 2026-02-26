;;; lattice/control-systems/manifest.sexp — Control Systems Engineering Manifest

(skill control-systems
  (version "0.1.0")
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

  (concepts
    (concept control-theory
      (description "State-space models, transfer functions, PID controllers, LQR, Kalman filters, and H-infinity synthesis.")
      (parent mathematics)
      (synonyms control-systems)))

  (exports
   (state-space
    ss? make-ss ss-from-lists
    ss-A ss-B ss-C ss-D ss-order ss-inputs ss-outputs
    ss-state-equation ss-output-equation
    ss-transition-matrix matrix-exp
    ss-controllability-matrix ss-controllable?
    ss-observability-matrix ss-observable?
    ss-controllability-gramian-finite ss-observability-gramian-finite
    ss-transform ss-identity-output ss-scalar ss-integrator
    ss->string)

   (transfer-function
    tf? make-tf tf-from-coeffs tf-from-lists
    tf-num tf-den tf-order tf-relative-degree tf-proper? tf-strictly-proper?
    tf-poles tf-zeros tf-from-poles-zeros tf-gain
    tf-eval tf-eval-real tf-dc-gain
    tf-freq-response tf-magnitude tf-magnitude-db tf-phase tf-phase-deg
    tf-bode-data
    tf-series tf-parallel tf-feedback tf-unity-feedback
    tf-stable? tf-pole-real-parts
    tf->string)

   (z-transform
    dtf? make-dtf dtf-from-coeffs dtf-from-lists
    dtf-num dtf-den dtf-Ts dtf-order dtf-relative-degree dtf-proper?
    dtf-poles dtf-zeros dtf-gain dtf-from-poles-zeros
    dtf-eval dtf-eval-real
    dtf-freq-response dtf-magnitude dtf-magnitude-db dtf-phase dtf-phase-deg
    dtf-series dtf-parallel dtf-feedback dtf-unity-feedback
    dtf-stable? dtf-pole-magnitudes
    dss->dtf dtf->dss
    dtf->string)

   (tf-convert
    ss->tf tf->ss)

   (kalman
    kalman? make-kalman-filter
    kalman-mean kalman-variance kalman-Q kalman-R
    kalman-predict kalman-update kalman-estimate kalman-gain
    kalman-batch kalman-residual kalman-mahalanobis
    make-log-kalman-filter log-kalman-update log-kalman-estimate
    log-kalman-mean log-kalman-predict-cost log-kalman-confidence-interval
    kalman-with-Q kalman-boost-Q kalman-summary log-kalman-summary)

   (stability
    ss-poles ss-stable? tf-stable? ss-marginally-stable? discrete-stable?
    routh-array routh-hurwitz-stable? routh-hurwitz-count-rhp
    lyapunov-solve-continuous lyapunov-solve-discrete lyapunov-stable?
    root-locus-points root-locus-breakaway root-locus-asymptotes
    nyquist-plot-points nyquist-count-encirclements nyquist-stable?
    gain-margin phase-margin stability-margins
    bibo-stable? bibo-stable-discrete?
    controllability-matrix controllable? observability-matrix observable?
    stability-report)

   (controller-design
    pid-tf pid-design-zn-open-loop pid-design-zn-closed-loop
    pid-design-imc pid-design-lambda
    pole-placement-ackermann pole-placement-bass-gura
    pole-placement pole-placement-mimo
    observer-design-ackermann observer-design-place
    lqr solve-care dlqr solve-dare lqg
    hinf-norm hinf-bounded?
    closed-loop-tf sensitivity-tf complementary-sensitivity-tf
    lead-compensator lag-compensator lead-lag-compensator)

   (digital-pid
    make-digital-pid digital-pid?
    pid-Kp pid-Ki pid-Kd pid-Ts
    make-pid-state pid-state?
    pid-integral pid-prev-error pid-prev-output
    pid-step pid-step-antiwindup pid-step-filtered
    pid-reset pid-reset-to
    make-filtered-pid-state filtered-pid-state?
    fpid-integral fpid-prev-error fpid-prev-derivative
    ziegler-nichols-tune tyreus-luyben-tune cohen-coon-tune
    pid-simulate pid-simulate-antiwindup
    make-velocity-pid-state velocity-pid-state?
    vpid-prev-error vpid-prev-prev-error vpid-prev-output
    pid-step-velocity pid->string)

   (discrete-control
    make-dss dss? dss-Ts dss-A dss-B dss-C dss-D dss-order
    error-result?
    c2d-zoh c2d-tustin c2d-tustin-prewarp d2c-tustin
    dss-step dss-output dss-simulate
    dss-impulse-response dss-step-response dss-stable?
    recommend-sample-rate)

   (hinf-synthesis
    make-generalized-plant gp? gp-A gp-B1 gp-B2 gp-C1 gp-C2
    gp-D11 gp-D12 gp-D21 gp-D22
    gp-order gp-nw gp-nu gp-nz gp-ny
    hinf-mixed-sensitivity-plant
    hinf-gamma-iteration hinf-check-gamma
    hinf-construct-controller hinf-synthesize
    hinf-weight-first-order hinf-weight-constant
    hinf-closed-loop-norm)

   (poly-algebra
    alg-make-polynomial alg-polynomial?
    alg-poly-field alg-poly-coeffs alg-poly-degree alg-poly-leading-coeff
    alg-poly-zero? alg-poly-zero-over alg-poly-one-over
    alg-poly-add alg-poly-neg alg-poly-sub alg-poly-scale alg-poly-mul
    alg-poly-divmod alg-poly-monomial alg-poly-mod alg-poly-div
    alg-poly-gcd alg-poly-make-monic alg-poly-extended-gcd
    alg-poly-square-free alg-poly-derivative alg-poly-square-free-factorization
    alg-poly->string
    numeric->algebra-poly algebra->numeric-poly
    tf-simplify tf-simplify-exact
    routh-array-exact routh-hurwitz-exact
    tf-char-poly tf-char-poly-factored tf-bezout tf-coprime? tf-canceled-poles
    tf-exact->string))

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
