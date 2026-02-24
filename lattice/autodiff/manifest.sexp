;;; lattice/autodiff/manifest.sexp — Automatic Differentiation Skill Manifest

(skill autodiff
  (version "0.3.0")
  (tier 1)
  (path "lattice/autodiff")
  (purity partial)  ; Uses mutable tape for efficiency
  (stability stable)
  (fuel-bound "O(n) forward pass, O(n) backward pass where n is tape length")
  (deps (linalg numeric fp))  ; numeric for interval arithmetic, fp for optics

  (description
   "Reverse-mode automatic differentiation (backpropagation) using
    computational graphs. Supports gradient computation, Jacobian/Hessian
    matrices, higher-order derivatives, and a differentiable type class
    for generic AD code. Includes sparse autodiff for large systems,
    interval autodiff for verified optimization with monotonicity pruning,
    and traced-optics for computing gradients through optic-focused paths.")

  (keywords (autodiff automatic-differentiation gradient backpropagation
             reverse-mode forward-mode jacobian hessian interval-gradient
             computational-graph tape dual-numbers hyperdual monotonicity))
  (aliases (ad diff reverse-diff backprop))

  (concepts
    (concept automatic-differentiation
      (description "Computing exact derivatives by propagating through computational graphs: forward mode (dual numbers), reverse mode (backprop).")
      (parent mathematics)
      (synonyms autodiff ad diff reverse-diff backprop)))

  (exports
   (profiling tape-stats)
   (differentiable TC-Differentiable inst-Differentiable-Real
                   inst-Differentiable-Dual inst-Differentiable-Traced)
   (interval-autodiff interval-traced interval-traced?
                      iv-traced-add iv-traced-sub iv-traced-mul iv-traced-div
                      iv-traced-sqr iv-traced-sqrt iv-traced-exp iv-traced-log
                      iv-traced-sin iv-traced-cos iv-traced-pow
                      interval-gradient gradient-sign monotonicity-info
                      all-monotonic? reduce-box-by-monotonicity)
   (traced-optics trace-value untrace-value lift-at-optic
                  optic-gradient optic-gradient-list optic-gradient-maybe
                  grad-at optimize-at optimize-steps composed-gradient
                  lift-const traced-sum traced-mean traced-l2-norm-sq
                  optic-value-and-gradient)
   (optic-functors cat-Diff make-diff-optic diff-optic? diff-optic-kind
                   diff-optic-optic diff-optic-focus-type
                   register-diff-kind! diff-optic-gradient
                   diff-optic-value-and-gradient diff-optic-optimize
                   diff-optic-optimize-n diff-optic-compose
                   verify-chain-rule verify-registry-agreement
                   gradients-close? diff-optic->functor
                   verify-diff-functor-laws diff-optic-transform)
   (higher-order-diff jacobian jacobian-reverse jacobian-forward
                      hessian vjp jvp grad hessian-exact second-derivative-exact)
   (sparse-autodiff sparse-jacobian sparse-hessian))

  (modules
   ;; Note: comp-graph.ss and reverse-diff.ss are in core/autodiff/ (core layer)
   (higher-order-diff "higher-order-diff.ss" "Jacobian, Hessian, and vector products")
   (differentiable "differentiable.ss" "Differentiable type class for generic AD")
   (differentiable-signal "differentiable-signal.ss" "Differentiable signal processing")
   (sparse-autodiff "sparse-autodiff.ss" "Sparse autodiff for large systems")
   (typed-gradients "typed-gradients.ss" "Type-safe gradient computation")
   (profiling "profiling.ss" "Performance profiling and debugging tools")
   (symbolic-diff "symbolic-diff.ss" "Bridge symbolic expressions to traced autodiff")
   (interval-autodiff "interval-autodiff.ss" "Interval autodiff for verified optimization")
   (traced-optics "traced-optics.ss" "Optics integration for gradient through focused paths")
   (optic-functors "optic-functors.ss" "Categorical bridge: optics tower to autodiff via functors")))
