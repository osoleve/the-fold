((name "optimization")
 (purpose "Numerical optimization algorithms powered by automatic differentiation")

 (description
  "A comprehensive optimization library providing gradient-based methods for
   minimizing differentiable functions. Includes first-order methods (SGD, Adam,
   RMSprop), second-order methods (Newton, Newton-CG), and quasi-Newton methods
   (L-BFGS) with line search and convergence monitoring.")

 (modules
  ((convergence.ss "Convergence criteria, stopping conditions, and result tracking")
   (line-search.ss "Armijo backtracking and Wolfe conditions line search")
   (first-order.ss "SGD, Momentum, Adam, RMSprop, Adagrad, Nesterov optimizers")
   (newton.ss "Newton's method, modified Newton, Newton-CG, Gauss-Newton")
   (lbfgs.ss "Limited-memory BFGS and L-BFGS-B (bound constrained)")
   (optimize.ss "Unified API combining all optimizers")
   (test-optimize.ss "Comprehensive test suite")))

 (dependencies
  (autodiff "For gradient computation via reverse-mode AD")
  (linalg "For matrix operations in second-order methods"))

 (quick-start
  ((minimize-sphere
    "Minimize a simple sphere function"
    ((load "lattice/optimization/optimize.ss")
     (define result (minimize sphere-2d '(5.0 5.0)))
     (opt-x result)))  ; Returns approximately (0.0 0.0)

   (minimize-with-method
    "Use a specific optimizer"
    ((define result (minimize sphere-2d '(5.0 5.0) 'adam))
     (opt-iterations result)))  ; Number of iterations

   (bounded-optimization
    "Optimize with box constraints"
    ((define result (minimize-bounded sphere-2d '(5.0 5.0)
                                      '(1.0 1.0)      ; lower bounds
                                      '(10.0 10.0)))  ; upper bounds
     (opt-x result)))  ; Returns approximately (1.0 1.0)

   (custom-convergence
    "Use custom convergence criteria"
    ((define cc (make-convergence-criteria
                 1e-8    ; gradient tolerance
                 1e-10   ; function value change tolerance
                 1e-10   ; parameter change tolerance
                 5000))  ; max iterations
     (define result (lbfgs my-function x0 cc))))))

 (available-optimizers
  ((first-order
    ((gradient-descent "Basic gradient descent with fixed learning rate")
     (sgd "Alias for gradient-descent")
     (momentum "Gradient descent with momentum (heavy ball method)")
     (nesterov "Nesterov accelerated gradient")
     (adam "Adaptive moment estimation - robust default for many problems")
     (adamw "Adam with decoupled weight decay regularization")
     (rmsprop "RMSprop with running average of squared gradients")
     (adagrad "Adaptive gradient with accumulated squared gradients")))

   (second-order
    ((newton-method "Newton's method using full Hessian")
     (modified-newton "Newton with positive-definite Hessian enforcement")
     (newton-cg "Truncated Newton using conjugate gradient")
     (gauss-newton "For nonlinear least squares problems")))

   (quasi-newton
    ((lbfgs "Limited-memory BFGS - memory-efficient for large problems")
     (lbfgs-b "L-BFGS with box constraints")))))

 (choosing-optimizer
  ((small-convex-smooth "Newton or L-BFGS for fastest convergence")
   (large-smooth "L-BFGS is memory-efficient and scales well")
   (non-smooth-or-noisy "Adam is robust to noise and non-smoothness")
   (constrained "L-BFGS-B for simple box constraints")
   (least-squares "Gauss-Newton exploits problem structure")
   (general-default "L-BFGS is a good default for smooth problems")))

 (convergence-criteria
  ((grad-tol "Stop when ||gradient|| < grad-tol (default: 1e-6)")
   (f-tol "Stop when |f_new - f_old| < f-tol (default: 1e-8)")
   (x-tol "Stop when ||x_new - x_old|| < x-tol (default: 1e-8)")
   (max-iter "Maximum iterations (default: 1000)")))

 (result-structure
  ((opt-x "Optimal parameters found")
   (opt-f "Function value at optimum")
   (opt-grad "Gradient at optimum")
   (opt-iterations "Number of iterations used")
   (opt-converged "Convergence reason: grad-converged, f-converged, x-converged, or max-iter")))

 (line-search
  ((armijo-backtrack "Backtracking with Armijo sufficient decrease condition")
   (wolfe-line-search "Full Wolfe conditions (sufficient decrease + curvature)")
   (constant-step "Fixed step size (for SGD-style optimizers)")))

 (test-functions
  ((rosenbrock "Classic banana-shaped valley, minimum at (1,1)")
   (sphere "Simple convex quadratic, minimum at origin")
   (rastrigin "Multimodal with many local minima")
   (beale "Non-convex with minimum at (3, 0.5)")))

 (complexity
  ((first-order "O(iterations × n) where n = number of parameters")
   (newton "O(iterations × n³) due to Hessian solve")
   (newton-cg "O(iterations × cg-iters × n) using Hessian-vector products")
   (lbfgs "O(iterations × m × n) where m = history size (typically 10)")))

 (references
  ((nocedal-wright "Numerical Optimization, 2nd Edition")
   (kingma-ba "Adam: A Method for Stochastic Optimization (2014)")
   (liu-nocedal "L-BFGS algorithm for large scale optimization")))

 (total-tests 35))
