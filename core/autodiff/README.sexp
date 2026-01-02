((name "autodiff")
 (purpose "Automatic differentiation")
 (description "Reverse-mode automatic differentiation (backpropagation)
using computational graphs. Supports higher-order derivatives and
provides a differentiable type class for extensibility.")
 (modules
  ((comp-graph.ss "Computational graph construction")
   (reverse-diff.ss "Reverse-mode AD (backpropagation)")
   (higher-order-diff.ss "Higher-order derivative computation")
   (differentiable.ss "Differentiable type class")))
 (dependencies (base linalg))
 (algorithm "Reverse-mode AD via tape-based computational graphs"))
