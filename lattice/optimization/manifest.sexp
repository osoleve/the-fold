;;; lattice/optimization/manifest.sexp — Optimization Skill Manifest

(skill optimization
  (version "0.6.0")
  (tier 1)
  (path "lattice/optimization")
  (purity total)
  (stability stable)
  (fuel-bound "O(iterations × n) for first-order, O(iterations × n²) for second-order, O(iterations × m × n) for LP, O(2^k × LP) for ILP, O(2^d × iterations) for interval global")
  (deps (autodiff linalg numeric data))

  (description
   "Numerical optimization algorithms powered by automatic differentiation.
    Includes gradient descent variants, Newton's method, L-BFGS,
    constrained optimization with line search and convergence criteria,
    linear programming via the simplex method, integer linear programming
    via branch-and-bound with Gomory cutting planes, interval-based
    global optimization with guaranteed enclosure of the global minimum,
    monotonicity-enhanced optimization using interval gradients for
    automatic pruning, constraint contractors for constrained interval
    optimization, and interval Newton method for guaranteed root finding
    with existence proofs.")

  (keywords (optimization gradient-descent sgd adam muon newton lbfgs
             minimize convergence line-search numerical
             linear-programming simplex dual sensitivity
             integer-programming ilp branch-and-bound knapsack set-cover
             global-optimization interval-branch-and-bound verified-optimization
             constraint-propagation contractors constrained-optimization
             monotonicity-pruning interval-gradient
             root-finding interval-newton bisection krawczyk existence-proof))
  (aliases (optim opt minimize))

  (exports
   (line-search armijo-backtrack wolfe-line-search)
   (convergence converged? make-convergence-criteria)
   (first-order gradient-descent sgd momentum adam muon muon-full muon-matrix muon-matrix-full newton-schulz rmsprop adagrad)
   (newton newton-method newton-cg)
   (lbfgs lbfgs minimize)
   (lp make-lp lp-solve lp-dual lp-shadow-prices lp-reduced-costs)
   (ilp make-ilp ilp-solve ilp-solve-cutting-plane knapsack-solve set-cover-solve)
   (interval-global interval-minimize interval-maximize make-interval-convergence
                    interval-opt-result? ior-candidates ior-best-upper ior-best-point
                    ior-solution-box interval-find-minimum
                    interval-minimize-with-gradient interval-find-minimum-with-gradient
                    interval-sphere interval-rosenbrock interval-rastrigin)
   (interval-contract interval-minimize-constrained
                      make-bound-contractor make-equality-contractor
                      make-linear-le-contractor make-linear-ge-contractor make-linear-eq-contractor
                      make-sphere-contractor make-box-constraints
                      contract-all contract-fixpoint)
   (interval-newton interval-newton-step interval-newton-step-rigorous
                    interval-newton-root interval-find-all-roots
                    interval-bisection-root
                    interval-contains-unique-root? interval-contains-no-root?
                    krawczyk-step
                    find-root find-all-roots-simple
                    make-root-result root-result? root-result-roots
                    root-result-iterations root-result-reason root-result-count
                    root-result-unique-roots))

  (modules
   (line-search "line-search.ss" "Armijo, Wolfe line search strategies")
   (convergence "convergence.ss" "Convergence criteria and stopping conditions")
   (first-order "first-order.ss" "SGD, Momentum, Adam, Muon, RMSprop optimizers")
   (newton "newton.ss" "Newton's method with Hessian")
   (lbfgs "lbfgs.ss" "Limited-memory BFGS quasi-Newton")
   (optimize "optimize.ss" "Main API - combines all optimizers")
   (lp "lp.ss" "Linear programming via two-phase simplex method")
   (ilp "ilp.ss" "Integer linear programming via branch-and-bound")
   (interval-global "interval-global.ss" "Interval branch-and-bound global optimization with monotonicity pruning")
   (interval-contract "interval-contract.ss" "Constraint contractors for interval optimization")
   (interval-newton "interval-newton.ss" "Interval Newton method for guaranteed root finding with existence proofs")))
