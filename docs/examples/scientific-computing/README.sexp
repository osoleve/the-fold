;;; README.sexp --- Scientific Computing Examples
;;;
;;; These examples demonstrate The Fold's autodiff capabilities for
;;; scientific computing applications.

(readme
 (title "Scientific Computing with Autodiff")
 (description
  "Examples demonstrating automatic differentiation for optimization,
   curve fitting, and related scientific computing tasks.")

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
   (run "scheme --script examples/scientific-computing/curve-fitting.ss")))

 (autodiff-modules-used
  ("lattice/autodiff/reverse-diff.ss" . "Reverse-mode (backprop) gradients")
  ("lattice/autodiff/higher-order-diff.ss" . "Hessians, jets, higher derivatives")
  ("lattice/autodiff/comp-graph.ss" . "Dual numbers, forward mode"))

 (key-concepts
  ((gradient "Vector of partial derivatives, points uphill")
   (hessian "Matrix of second derivatives, measures curvature")
   (jet-numbers "Truncated Taylor series for arbitrary-order derivatives")
   (reverse-mode "Efficient for many inputs, few outputs")
   (forward-mode "Efficient for few inputs, many outputs")))

 (see-also
  ("lattice/autodiff/test-*.ss" . "Test files for more usage examples")))
