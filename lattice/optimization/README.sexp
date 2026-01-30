((name "optimization")
(purpose "Numerical optimization algorithms powered by automatic differentiation")
(description "A comprehensive optimization library providing gradient-based and linear\n   methods. Includes first-order methods (SGD, Adam, RMSprop), second-order\n   methods (Newton, Newton-CG), quasi-Newton methods (L-BFGS), linear\n   programming (two-phase revised simplex), and integer linear programming\n   (branch-and-bound, Gomory cuts) with line search and convergence monitoring.")
(modules 
  ((convergence.ss "Convergence criteria, stopping conditions, and result tracking")
  (line-search.ss "Armijo backtracking and Wolfe conditions line search")
  (first-order.ss "SGD, Momentum, Adam, RMSprop, Adagrad, Nesterov optimizers")
  (newton.ss "Newton's method, modified Newton, Newton-CG, Gauss-Newton")
  (lbfgs.ss "Limited-memory BFGS and L-BFGS-B (bound constrained)")
  (lp.ss "Linear programming via two-phase revised simplex method")
  (ilp.ss "Integer linear programming via branch-and-bound")
  (optimize.ss "Unified API combining all optimizers")
  (test-optimize.ss "Comprehensive test suite for gradient-based optimizers")
  (test-lp.ss "LP test suite (21 tests)")
  (test-ilp.ss "ILP test suite (16 tests)")))
(dependencies
 (autodiff "For gradient computation via reverse-mode AD")
 (linalg "For matrix operations in second-order methods"))
(quick-start 
  ((minimize-sphere "Minimize a simple sphere function" ((load "lattice/optimization/optimize.ss")
    (define result
     (minimize sphere-2d
      (quote (5.0 5.0))))
    (opt-x result)))
  (minimize-with-method "Use a specific optimizer" ((define result
     (minimize sphere-2d
      (quote (5.0 5.0))
      (quote adam)))
    (opt-iterations result)))
  (bounded-optimization "Optimize with box constraints" ((define result
     (minimize-bounded sphere-2d
      (quote (5.0 5.0))
      (quote (1.0 1.0))
      (quote (10.0 10.0))))
    (opt-x result)))
  (custom-convergence "Use custom convergence criteria" ((define cc
     (make-convergence-criteria 1e-8 1e-10 1e-10 5000))
    (define result
     (lbfgs my-function x0 cc))))))
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
(total-tests 138)
(integer-programming 
  ((ilp-solve "Branch-and-bound solver for mixed-integer programs")
  (ilp-solve-cutting-plane "Cutting plane method with Gomory cuts")
  (knapsack-solve "0-1 knapsack optimization")
  (set-cover-solve "Minimum cost set cover")))
(ilp-quick-start 
  ((simple-ilp "Solve a simple integer program" ((load "lattice/optimization/ilp.ss")
    (define ilp
     (make-ilp c A b
      (quote (0 1 2))))
    (define result
     (ilp-solve ilp))
    (ilp-result-x result)))
  (knapsack-example "Solve 0-1 knapsack" ((define result
     (knapsack-solve
      (vector 60 100 120)
      (vector 10 20 30) 50))
    (car result)
    (cdr result)))
  (set-cover-example "Solve minimum set cover" ((define coverage
     (matrix-from-lists (quote 
        ((1 0 1) (1 1 0) (0 1 1)))))
    (define costs
     (vector 1 1 1))
    (set-cover-solve coverage costs))))))
