;;; examples/scientific-computing/gradient-descent.ss --- Gradient Descent Optimization
;;;
;;; Demonstrates gradient-based optimization using The Fold's autodiff system.
;;; We minimize the Rosenbrock function, a classic test case for optimization algorithms.
;;;
;;; The Rosenbrock function:
;;;   f(x, y) = (a - x)^2 + b * (y - x^2)^2
;;;
;;; With standard parameters a=1, b=100, the global minimum is at (1, 1)
;;; where f(1, 1) = 0.
;;;
;;; The function has a curved valley that makes naive gradient descent slow,
;;; but it's an excellent demonstration of autodiff capabilities.
;;;
;;; Run with: scheme --script examples/scientific-computing/gradient-descent.ss

(load "core/autodiff/reverse-diff.ss")

;;; ============================================================
;;; The Rosenbrock Function
;;; ============================================================

;;; rosenbrock : Traced x Traced -> Traced
;;; The Rosenbrock function using traced operations for autodiff.
;;;
;;; Mathematical definition:
;;;   f(x, y) = (1 - x)^2 + 100 * (y - x^2)^2
;;;
;;; The gradient is:
;;;   df/dx = -2(1 - x) - 400x(y - x^2)
;;;   df/dy = 200(y - x^2)
;;;
;;; We don't compute these by hand - autodiff does it for us!
(define (rosenbrock x y)
  (let* ([a 1]       ; First parameter (shifts the minimum)
         [b 100]     ; Second parameter (controls steepness of valley)
         ;; Term 1: (a - x)^2 = (1 - x)^2
         [term1 (traced-sq (traced-sub a x))]
         ;; Term 2: b * (y - x^2)^2 = 100 * (y - x^2)^2
         [x-squared (traced-sq x)]
         [y-minus-x2 (traced-sub y x-squared)]
         [term2 (traced-mul b (traced-sq y-minus-x2))])
        (traced-add term1 term2)))

;;; ============================================================
;;; Plain Rosenbrock for Evaluation
;;; ============================================================

;;; rosenbrock-plain : Number x Number -> Number
;;; Plain version for computing function value without tracing overhead.
(define (rosenbrock-plain x y)
  (let ([a 1] [b 100])
       (+ (expt (- a x) 2)
          (* b (expt (- y (* x x)) 2)))))

;;; ============================================================
;;; Gradient Descent Algorithm
;;; ============================================================

;;; gradient-descent : ((Traced Traced) -> Traced) x (List Number) x Number x Nat -> (List Number)
;;; Perform gradient descent optimization.
;;;
;;; Parameters:
;;;   f           - Function to minimize (takes traced arguments)
;;;   start       - Starting point (list of numbers)
;;;   alpha       - Learning rate (step size)
;;;   max-iters   - Maximum number of iterations
;;;
;;; Returns the final position after optimization.
;;;
;;; The algorithm:
;;;   1. Compute gradient at current point using reverse-mode autodiff
;;;   2. Take a step in the negative gradient direction: x_new = x - alpha * grad
;;;   3. Repeat until max iterations or convergence
(define (gradient-descent f start alpha max-iters)
  (let loop ([point start]
             [iter 0])
       (if (>= iter max-iters)
           point
           (let* ([grad (gradient (lambda args (apply f args)) point)]
                  [new-point (map (lambda (p g) (- p (* alpha g)))
                                  point grad)])
                 (loop new-point (+ iter 1))))))

;;; gradient-descent-verbose : ((Traced ...) -> Traced) x (List Number) x Number x Nat -> (List Number)
;;; Gradient descent with progress output every N iterations.
(define (gradient-descent-verbose f start alpha max-iters print-every)
  (let loop ([point start]
             [iter 0])
       ;; Print progress
       (when (= (modulo iter print-every) 0)
             (let ([val (apply rosenbrock-plain point)])
                  (printf "Iteration ~a: point = (~,6f, ~,6f), f(x,y) = ~,10f~n"
                          iter (car point) (cadr point) val)))
       
       (if (>= iter max-iters)
           (begin
            (printf "~nFinal result after ~a iterations:~n" max-iters)
            (printf "  Point: (~,6f, ~,6f)~n" (car point) (cadr point))
            (printf "  f(x,y) = ~,10f~n" (apply rosenbrock-plain point))
            (printf "  True minimum: (1, 1), f(1,1) = 0~n")
            point)
           (let* ([grad (gradient (lambda args (apply f args)) point)]
                  [new-point (map (lambda (p g) (- p (* alpha g)))
                                  point grad)])
                 (loop new-point (+ iter 1))))))

;;; ============================================================
;;; Gradient Descent with Line Search
;;; ============================================================

;;; backtracking-line-search : (Number x Number -> Number) x (List Number) x (List Number) x Number x Number -> Number
;;; Find a good step size using Armijo backtracking line search.
;;;
;;; Parameters:
;;;   f       - Objective function (plain numbers, not traced)
;;;   point   - Current point
;;;   grad    - Gradient at current point
;;;   alpha   - Initial step size guess
;;;   beta    - Reduction factor (e.g., 0.5)
;;;
;;; Returns a step size that satisfies Armijo condition.
(define (backtracking-line-search f point grad alpha beta)
  (let* ([c 0.0001]  ; Armijo constant (typically 1e-4)
         [f0 (apply f point)]
         [grad-sq (apply + (map (lambda (g) (* g g)) grad))])
        ;; Keep reducing alpha until Armijo condition is met
        (let loop ([a alpha])
             (let* ([new-point (map (lambda (p g) (- p (* a g)))
                                    point grad)]
                    [f-new (apply f new-point)])
                   (if (or (<= f-new (- f0 (* c a grad-sq)))
                           (< a 1e-10))  ; Minimum step size
                       a
                       (loop (* beta a)))))))

;;; gradient-descent-with-line-search : ((Traced ...) -> Traced) x (List Number) x Nat -> (List Number)
;;; Gradient descent with adaptive step size via line search.
(define (gradient-descent-with-line-search f start max-iters print-every)
  (let loop ([point start]
             [iter 0])
       ;; Print progress
       (when (= (modulo iter print-every) 0)
             (let ([val (apply rosenbrock-plain point)])
                  (printf "Iteration ~a: point = (~,6f, ~,6f), f(x,y) = ~,10f~n"
                          iter (car point) (cadr point) val)))
       
       (if (>= iter max-iters)
           (begin
            (printf "~nFinal result after ~a iterations:~n" max-iters)
            (printf "  Point: (~,6f, ~,6f)~n" (car point) (cadr point))
            (printf "  f(x,y) = ~,10f~n" (apply rosenbrock-plain point))
            point)
           (let* ([grad (gradient (lambda args (apply f args)) point)]
                  [alpha (backtracking-line-search rosenbrock-plain point grad 1.0 0.5)]
                  [new-point (map (lambda (p g) (- p (* alpha g)))
                                  point grad)])
                 (loop new-point (+ iter 1))))))

;;; ============================================================
;;; Main Demonstration
;;; ============================================================

(printf "~n")
(printf "=========================================================~n")
(printf "   Gradient Descent Optimization with Autodiff~n")
(printf "=========================================================~n")
(printf "~n")

(printf "Minimizing the Rosenbrock function:~n")
(printf "  f(x, y) = (1 - x)^2 + 100 * (y - x^2)^2~n")
(printf "~n")
(printf "The global minimum is at (1, 1) where f(1, 1) = 0~n")
(printf "~n")

;;; --- Demonstrating gradient computation ---
(printf "---------------------------------------------------------~n")
(printf "1. Demonstrating Gradient Computation via Autodiff~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(let* ([test-point '(-1.0 2.0)]
       [grad (gradient rosenbrock test-point)])
      (printf "At point (~a, ~a):~n" (car test-point) (cadr test-point))
      (printf "  f(x,y) = ~a~n" (apply rosenbrock-plain test-point))
      (printf "  Gradient = (~,4f, ~,4f)~n" (car grad) (cadr grad))
      (printf "  (computed automatically via reverse-mode autodiff)~n"))

(printf "~n")

;;; --- Standard gradient descent with fixed learning rate ---
(printf "---------------------------------------------------------~n")
(printf "2. Standard Gradient Descent (fixed learning rate)~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(printf "Starting point: (-1.5, 1.5)~n")
(printf "Learning rate: 0.001~n")
(printf "Max iterations: 10000~n")
(printf "~n")

(let ([result (gradient-descent-verbose rosenbrock '(-1.5 1.5) 0.001 10000 1000)])
     (printf "~n"))

;;; --- Gradient descent with line search ---
(printf "---------------------------------------------------------~n")
(printf "3. Gradient Descent with Backtracking Line Search~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(printf "Starting point: (-1.5, 1.5)~n")
(printf "Using Armijo backtracking line search for step size~n")
(printf "Max iterations: 10000~n")
(printf "~n")

(let ([result (gradient-descent-with-line-search rosenbrock '(-1.5 1.5) 10000 1000)])
     (printf "~n"))

;;; --- Verify the gradient numerically ---
(printf "---------------------------------------------------------~n")
(printf "4. Gradient Verification (Numerical vs Autodiff)~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(let* ([test-point '(0.5 0.5)]
       [autodiff-grad (gradient rosenbrock test-point)]
       [numerical-grad (numerical-gradient rosenbrock-plain test-point 1e-5)])
      (printf "At point (~a, ~a):~n" (car test-point) (cadr test-point))
      (printf "  Autodiff gradient:   (~,8f, ~,8f)~n"
              (car autodiff-grad) (cadr autodiff-grad))
      (printf "  Numerical gradient:  (~,8f, ~,8f)~n"
              (car numerical-grad) (cadr numerical-grad))
      (printf "  Difference:          (~,2e, ~,2e)~n"
              (abs (- (car autodiff-grad) (car numerical-grad)))
              (abs (- (cadr autodiff-grad) (cadr numerical-grad))))
      (printf "~n"))

(printf "=========================================================~n")
(printf "   Demonstration Complete~n")
(printf "=========================================================~n")
(printf "~n")
(printf "Key Takeaways:~n")
(printf "  - Gradients are computed automatically via reverse-mode autodiff~n")
(printf "  - No need to derive gradients by hand~n")
(printf "  - Works for arbitrarily complex compositions of traced operations~n")
(printf "  - Numerical verification confirms autodiff accuracy~n")
(printf "~n")
