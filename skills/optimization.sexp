;;; skills/optimization.sexp — Skill curation file

(skill optimization
  (description
    "Numerical optimization algorithms powered by automatic differentiation.\n    Includes gradient descent variants, Newton's method, L-BFGS,\n    constrained optimization with line search and convergence criteria,\n    linear programming via the simplex method, integer linear programming\n    via branch-and-bound with Gomory cutting planes, interval-based\n    global optimization with guaranteed enclosure of the global minimum,\n    monotonicity-enhanced optimization using interval gradients for\n    automatic pruning, constraint contractors for constrained interval\n    optimization, interval Newton method for guaranteed root finding\n    with existence proofs, weighted constraint optimization\n    via MaxSAT with stratified solving and named variable domains,\n    and weighted constraint optimization via MaxSAT with stratified solving.")

  (keywords (optimization gradient-descent sgd adam muon newton lbfgs minimize convergence line-search numerical linear-programming simplex dual sensitivity integer-programming ilp branch-and-bound knapsack set-cover global-optimization interval-branch-and-bound verified-optimization constraint-propagation contractors constrained-optimization monotonicity-pruning interval-gradient root-finding interval-newton bisection krawczyk existence-proof weighted-maxsat constraint-optimization weighted-constraints scheduling vertex-cover diagnosis))

  (aliases (optim opt minimize))

  (concepts (gradient-based-optimization constrained-optimization combinatorial-optimization global-optimization))

  (modules
    line-search
    convergence
    first-order
    newton
    lbfgs
    optimize
    lp
    ilp
    interval-global
    interval-contract
    interval-newton
    maxsat-bridge
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
