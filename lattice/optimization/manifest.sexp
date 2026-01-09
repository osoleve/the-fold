;;; lattice/optimization/manifest.sexp — Optimization Skill Manifest

(skill optimization
  (version "0.1.0")
  (tier 1)
  (purity total)
  (fuel-bound "O(iterations × n) for first-order, O(iterations × n²) for second-order")
  (deps (autodiff linalg))

  (description
   "Numerical optimization algorithms powered by automatic differentiation.
    Includes gradient descent variants, Newton's method, L-BFGS, and
    constrained optimization with line search and convergence criteria.")

  (exports
   (line-search armijo-backtrack wolfe-line-search)
   (convergence converged? make-convergence-criteria)
   (first-order gradient-descent sgd momentum adam rmsprop adagrad)
   (newton newton-method newton-cg)
   (lbfgs lbfgs minimize))

  (modules
   (line-search "line-search.ss" "Armijo, Wolfe line search strategies")
   (convergence "convergence.ss" "Convergence criteria and stopping conditions")
   (first-order "first-order.ss" "SGD, Momentum, Adam, RMSprop optimizers")
   (newton "newton.ss" "Newton's method with Hessian")
   (lbfgs "lbfgs.ss" "Limited-memory BFGS quasi-Newton")
   (optimize "optimize.ss" "Main API - combines all optimizers")))
