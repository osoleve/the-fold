;;; lattice/optimization/manifest.sexp — Optimization Skill Manifest

(skill optimization
  (version "0.6.0")
  (path "lattice/optimization")
  (purity total)
  (stability stable)
  (fuel-bound "O(iterations × n) for first-order, O(iterations × n²) for second-order, O(iterations × m × n) for LP, O(2^k × LP) for ILP, O(2^d × iterations) for interval global")
  (deps (autodiff linalg interval data sat))

  (description
   "Numerical optimization algorithms powered by automatic differentiation.
    Includes gradient descent variants, Newton's method, L-BFGS,
    constrained optimization with line search and convergence criteria,
    linear programming via the simplex method, integer linear programming
    via branch-and-bound with Gomory cutting planes, interval-based
    global optimization with guaranteed enclosure of the global minimum,
    monotonicity-enhanced optimization using interval gradients for
    automatic pruning, constraint contractors for constrained interval
    optimization, interval Newton method for guaranteed root finding
    with existence proofs, weighted constraint optimization
    via MaxSAT with stratified solving and named variable domains,
    and weighted constraint optimization via MaxSAT with stratified solving.")

  (keywords (optimization gradient-descent sgd adam muon newton lbfgs
             minimize convergence line-search numerical
             linear-programming simplex dual sensitivity
             integer-programming ilp branch-and-bound knapsack set-cover
             global-optimization interval-branch-and-bound verified-optimization
             constraint-propagation contractors constrained-optimization
             monotonicity-pruning interval-gradient
             root-finding interval-newton bisection krawczyk existence-proof
             weighted-maxsat constraint-optimization weighted-constraints
             scheduling vertex-cover diagnosis))
  (aliases (optim opt minimize))

  (concepts
    (concept gradient-based-optimization
      (description "First-order methods (SGD, Adam, Muon) and second-order methods (Newton, L-BFGS) for smooth objectives.")
      (parent optimization)
      (synonyms gradient-descent sgd adam muon rmsprop adagrad momentum lbfgs quasi-newton limited-memory-bfgs))
    (concept constrained-optimization
      (description "Optimization subject to equality or inequality constraints: LP via simplex, ILP via branch-and-bound, and contractors.")
      (parent optimization)
      (synonyms linear-programming simplex-method lp dual sensitivity-analysis))
    (concept combinatorial-optimization
      (description "Discrete optimization: ILP, weighted MaxSAT, knapsack, vertex cover, and set cover problems.")
      (parent optimization)
      (synonyms integer-programming ilp branch-and-bound gomory-cuts knapsack set-cover))
    (concept global-optimization
      (description "Finding the global minimum of non-convex functions: interval branch-and-bound with guaranteed enclosure.")
      (parent optimization)
      (synonyms interval-optimization global-minimum verified-optimization monotonicity-pruning))
    )

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
                    root-result-unique-roots)
   (maxsat-bridge make-wco-domain wco-declare wco-declare* wco-var wco-neg-var wco-var-count
                  wco-implies wco-iff wco-mutex wco-exactly-one wco-at-most-k wco-at-least-k
                  make-wco-problem wco-require wco-prefer wco-prefer-true wco-prefer-false
                  wco->maxsat wco-solve
                  wco-result? wco-result-cost wco-result-model wco-result-assignment wco-result-value
                  weighted-vertex-cover weighted-diagnosis scheduling-problem)
   )

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
   (interval-newton "interval-newton.ss" "Interval Newton method for guaranteed root finding with existence proofs")
   (maxsat-bridge "maxsat-bridge.ss" "Weighted constraint optimization via stratified MaxSAT")))
