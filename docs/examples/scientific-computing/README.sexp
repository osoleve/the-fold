;;; README.sexp --- Scientific Computing Examples
;;;
;;; These examples demonstrate The Fold's autodiff and symbolic computation
;;; capabilities for scientific computing applications.

(readme
 (title "Scientific Computing")
 (description
  "Examples demonstrating automatic differentiation, symbolic computation,
   optimization, and related scientific computing tasks.")

 (examples
  ((file "gradient-descent.ss")
   (topic "First-Order Optimization")
   (demonstrates
    ("Reverse-mode autodiff for gradient computation"
     "Minimizing the Rosenbrock function"
     "Fixed learning rate gradient descent"
     "Backtracking line search for adaptive step size"
     "Numerical gradient verification"))
   (run "scheme --script examples/scientific-computing/gradient-descent.ss"))

  ((file "newton-optimization.ss")
   (topic "Second-Order Optimization")
   (demonstrates
    ("Hessian computation using jet numbers"
     "Newton's method for quadratic convergence"
     "Critical point classification (min/max/saddle)"
     "Comparison of convergence rates"))
   (run "scheme --script examples/scientific-computing/newton-optimization.ss"))

  ((file "curve-fitting.ss")
   (topic "Polynomial Regression")
   (demonstrates
    ("Autodiff for loss function gradients"
     "Linear, quadratic, and cubic fitting"
     "Gradient descent with momentum"
     "Underfitting vs overfitting concepts"
     "Gradient verification"))
   (run "scheme --script examples/scientific-computing/curve-fitting.ss"))

  ((file "symbolic-calculus.ss")
   (topic "Symbolic Calculus")
   (demonstrates
    ("Symbolic differentiation with deriv"
     "Symbolic integration with integrate"
     "Algebraic simplification"
     "Trigonometric identities"
     "Gradient and Hessian computation"
     "Chain rule via substitution"
     "Product expansion"))
   (run "scheme --script docs/examples/scientific-computing/symbolic-calculus.ss")))

 (autodiff-modules-used
  ("lattice/autodiff/reverse-diff.ss" . "Reverse-mode (backprop) gradients")
  ("lattice/autodiff/higher-order-diff.ss" . "Hessians, jets, higher derivatives")
  ("lattice/autodiff/comp-graph.ss" . "Dual numbers, forward mode"))

 (symbolic-modules-used
  ("lattice/fp/symbolic/expr.ss" . "Expression representation and manipulation")
  ("lattice/fp/symbolic/diff.ss" . "Symbolic differentiation")
  ("lattice/fp/symbolic/simplify.ss" . "Algebraic simplification")
  ("lattice/fp/symbolic/integrate.ss" . "Symbolic integration"))

 (key-concepts
  ((gradient "Vector of partial derivatives, points uphill")
   (hessian "Matrix of second derivatives, measures curvature")
   (jet-numbers "Truncated Taylor series for arbitrary-order derivatives")
   (reverse-mode "Efficient for many inputs, few outputs")
   (forward-mode "Efficient for few inputs, many outputs")
   (symbolic-differentiation "Exact derivatives via algebraic manipulation")
   (symbolic-integration "Antiderivatives via pattern matching and rules")
   (algebraic-simplification "Canonicalizing expressions to simplest form")))

 (see-also
  ("lattice/autodiff/test-*.ss" . "Autodiff test files for more usage examples")
  ("lattice/fp/symbolic/test-*.ss" . "Symbolic computation test files")
  ("docs/tutorials/symbolic-computation.md" . "Symbolic computation tutorial")))
