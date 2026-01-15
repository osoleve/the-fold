;;; lattice/optimization/manifest.sexp — Optimization Skill Manifest

(skill optimization
  (version "0.2.0")
  (tier 1)
  (path "lattice/optimization")
  (purity total)
  (stability stable)
  (fuel-bound "O(iterations × n) for first-order, O(iterations × n²) for second-order, O(iterations × m × n) for LP, O(2^k × LP) for ILP")
  (deps (autodiff linalg))

  (description
   "Numerical optimization algorithms powered by automatic differentiation.
    Includes gradient descent variants, Newton's method, L-BFGS,
    constrained optimization with line search and convergence criteria,
    linear programming via the simplex method, and integer linear programming
    via branch-and-bound with Gomory cutting planes.")

  (keywords (optimization gradient-descent sgd adam newton lbfgs
             minimize convergence line-search numerical
             linear-programming simplex dual sensitivity
             integer-programming ilp branch-and-bound knapsack set-cover))
  (aliases (optim opt minimize))

  (exports
   (line-search armijo-backtrack wolfe-line-search)
   (convergence converged? make-convergence-criteria)
   (first-order gradient-descent sgd momentum adam rmsprop adagrad)
   (newton newton-method newton-cg)
   (lbfgs lbfgs minimize)
   (lp make-lp lp-solve lp-dual lp-shadow-prices lp-reduced-costs)
   (ilp make-ilp ilp-solve ilp-solve-cutting-plane knapsack-solve set-cover-solve))

  (modules
   (line-search "line-search.ss" "Armijo, Wolfe line search strategies")
   (convergence "convergence.ss" "Convergence criteria and stopping conditions")
   (first-order "first-order.ss" "SGD, Momentum, Adam, RMSprop optimizers")
   (newton "newton.ss" "Newton's method with Hessian")
   (lbfgs "lbfgs.ss" "Limited-memory BFGS quasi-Newton")
   (optimize "optimize.ss" "Main API - combines all optimizers")
   (lp "lp.ss" "Linear programming via two-phase simplex method")
   (ilp "ilp.ss" "Integer linear programming via branch-and-bound")))
