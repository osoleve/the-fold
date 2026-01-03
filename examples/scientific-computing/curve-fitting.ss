;;; examples/scientific-computing/curve-fitting.ss --- Curve Fitting with Autodiff
;;;
;;; Demonstrates polynomial curve fitting using gradient-based optimization
;;; with automatic differentiation.
;;;
;;; The Problem:
;;;   Given data points (x_i, y_i), find polynomial coefficients c = [c0, c1, ..., cn]
;;;   that minimize the sum of squared errors:
;;;
;;;   L(c) = sum_i (y_i - p(x_i; c))^2
;;;
;;;   where p(x; c) = c0 + c1*x + c2*x^2 + ... + cn*x^n
;;;
;;; We use autodiff to compute gradients dL/dc_j automatically,
;;; then use gradient descent to find optimal coefficients.
;;;
;;; Run with: scheme --script examples/scientific-computing/curve-fitting.ss

(load "core/autodiff/reverse-diff.ss")

;;; ============================================================
;;; Data Generation
;;; ============================================================

;;; generate-data : (Number -> Number) x (List Number) x Number -> (List (Number . Number))
;;; Generate noisy data points from a function.
;;; Returns list of (x . y) pairs.
(define (generate-data f x-values noise-scale seed)
  ;; Simple linear congruential generator for reproducible "noise"
  (let loop ([xs x-values]
             [state seed]
             [result '()])
       (if (null? xs)
           (reverse result)
           (let* ([x (car xs)]
                  [true-y (f x)]
                  ;; Generate pseudo-random noise
                  [new-state (modulo (+ (* 1103515245 state) 12345) (expt 2 31))]
                  [noise (* noise-scale (- (/ new-state (expt 2 30)) 1))]
                  [noisy-y (+ true-y noise)])
                 (loop (cdr xs) new-state (cons (cons x noisy-y) result))))))

;;; linspace : Number x Number x Nat -> (List Number)
;;; Generate evenly spaced numbers from start to end.
(define (linspace start end n)
  (if (< n 2)
      (list start)
      (let ([step (/ (- end start) (- n 1))])
           (map (lambda (i) (+ start (* i step)))
                (iota n)))))

;;; ============================================================
;;; Polynomial Evaluation
;;; ============================================================

;;; poly-eval-traced : (List Traced) x Traced -> Traced
;;; Evaluate polynomial with traced coefficients at traced x.
;;; Coefficients are [c0, c1, c2, ...] for c0 + c1*x + c2*x^2 + ...
;;; Uses Horner's method for efficiency.
(define (poly-eval-traced coeffs x)
  (if (null? coeffs)
      0
      (let loop ([cs (reverse coeffs)]
                 [result 0])
           (if (null? cs)
               result
               (loop (cdr cs)
                     (traced-add (car cs)
                                 (traced-mul result x)))))))

;;; poly-eval-plain : (List Number) x Number -> Number
;;; Plain polynomial evaluation (no tracing).
(define (poly-eval-plain coeffs x)
  (if (null? coeffs)
      0
      (let loop ([cs (reverse coeffs)]
                 [result 0])
           (if (null? cs)
               result
               (loop (cdr cs)
                     (+ (car cs) (* result x)))))))

;;; ============================================================
;;; Loss Function
;;; ============================================================

;;; mse-loss-traced : (List (Number . Number)) x (List Traced) -> Traced
;;; Mean squared error loss function.
;;; data is list of (x . y) pairs, coeffs are traced polynomial coefficients.
(define (mse-loss-traced data coeffs)
  (let loop ([points data]
             [sum-sq 0])
       (if (null? points)
           ;; Divide by number of points for mean
           (traced-div sum-sq (length data))
           (let* ([point (car points)]
                  [x (car point)]
                  [y (cdr point)]
                  ;; Evaluate polynomial at x (x is constant, coeffs are traced)
                  [y-pred (poly-eval-traced coeffs x)]
                  [error (traced-sub y y-pred)]
                  [sq-error (traced-sq error)])
                 (loop (cdr points)
                       (traced-add sum-sq sq-error))))))

;;; mse-loss-plain : (List (Number . Number)) x (List Number) -> Number
;;; Plain MSE loss (no tracing).
(define (mse-loss-plain data coeffs)
  (let loop ([points data]
             [sum-sq 0])
       (if (null? points)
           (/ sum-sq (length data))
           (let* ([point (car points)]
                  [x (car point)]
                  [y (cdr point)]
                  [y-pred (poly-eval-plain coeffs x)]
                  [error (- y y-pred)]
                  [sq-error (* error error)])
                 (loop (cdr points)
                       (+ sum-sq sq-error))))))

;;; ============================================================
;;; Gradient Computation for Curve Fitting
;;; ============================================================

;;; loss-gradient : (List (Number . Number)) x (List Number) -> (List Number)
;;; Compute gradient of MSE loss with respect to coefficients.
(define (loss-gradient data coeffs)
  (gradient (lambda traced-coeffs
                    (mse-loss-traced data traced-coeffs))
            coeffs))

;;; ============================================================
;;; Gradient Descent for Curve Fitting
;;; ============================================================

;;; fit-polynomial : (List (Number . Number)) x Nat x Number x Nat -> (List Number)
;;; Fit polynomial of given degree to data using gradient descent.
;;;
;;; Parameters:
;;;   data        - List of (x . y) pairs
;;;   degree      - Polynomial degree (n means n+1 coefficients)
;;;   alpha       - Learning rate
;;;   max-iters   - Maximum iterations
;;;   print-every - Print progress every N iterations
(define (fit-polynomial data degree alpha max-iters print-every)
  ;; Initialize coefficients to small random values (zeros can be problematic)
  (let* ([n-coeffs (+ degree 1)]
         [init-coeffs (map (lambda (i) (* 0.01 (- i (/ n-coeffs 2))))
                           (iota n-coeffs))])
        (printf "Fitting degree-~a polynomial (~a coefficients)~n" degree n-coeffs)
        (printf "Learning rate: ~a, Max iterations: ~a~n" alpha max-iters)
        (printf "Initial coefficients: ~a~n" init-coeffs)
        (printf "~n")
        
        (let loop ([coeffs init-coeffs]
                   [iter 0])
             ;; Print progress
             (when (= (modulo iter print-every) 0)
                   (let ([loss (mse-loss-plain data coeffs)])
                        (printf "Iter ~a: MSE = ~,6f~n" iter loss)))
             
             (if (>= iter max-iters)
                 (begin
                  (printf "~nFinal coefficients after ~a iterations:~n" max-iters)
                  (printf "  ~a~n" coeffs)
                  (printf "Final MSE: ~,6f~n" (mse-loss-plain data coeffs))
                  coeffs)
                 (let* ([grad (loss-gradient data coeffs)]
                        [new-coeffs (map (lambda (c g) (- c (* alpha g)))
                                         coeffs grad)])
                       (loop new-coeffs (+ iter 1)))))))

;;; fit-polynomial-with-momentum : (List (Number . Number)) x Nat x Number x Number x Nat -> (List Number)
;;; Gradient descent with momentum for faster convergence.
(define (fit-polynomial-with-momentum data degree alpha momentum max-iters print-every)
  (let* ([n-coeffs (+ degree 1)]
         [init-coeffs (map (lambda (_) 0.0) (iota n-coeffs))]
         [init-velocity (map (lambda (_) 0.0) (iota n-coeffs))])
        
        (printf "Fitting degree-~a polynomial with momentum~n" degree)
        (printf "Learning rate: ~a, Momentum: ~a~n" alpha momentum)
        (printf "~n")
        
        (let loop ([coeffs init-coeffs]
                   [velocity init-velocity]
                   [iter 0])
             (when (= (modulo iter print-every) 0)
                   (let ([loss (mse-loss-plain data coeffs)])
                        (printf "Iter ~a: MSE = ~,6f~n" iter loss)))
             
             (if (>= iter max-iters)
                 (begin
                  (printf "~nFinal coefficients: ~a~n" coeffs)
                  (printf "Final MSE: ~,6f~n" (mse-loss-plain data coeffs))
                  coeffs)
                 (let* ([grad (loss-gradient data coeffs)]
                        ;; v_new = momentum * v - alpha * grad
                        [new-velocity (map (lambda (v g)
                                                   (- (* momentum v) (* alpha g)))
                                           velocity grad)]
                        ;; coeffs_new = coeffs + v_new
                        [new-coeffs (map + coeffs new-velocity)])
                       (loop new-coeffs new-velocity (+ iter 1)))))))

;;; ============================================================
;;; Visualization Helpers
;;; ============================================================

;;; print-comparison : (List (Number . Number)) x (List Number) x Nat -> Void
;;; Print a comparison of actual vs predicted values.
(define (print-comparison data coeffs num-samples)
  (printf "~n  x      |  y (actual)  |  y (predicted)  |  error~n")
  (printf "  -------+-------------+----------------+---------~n")
  (let loop ([points data]
             [count 0])
       (when (and (< count num-samples) (not (null? points)))
             (let* ([point (car points)]
                    [x (car point)]
                    [y-actual (cdr point)]
                    [y-pred (poly-eval-plain coeffs x)]
                    [error (- y-actual y-pred)])
                   (printf "  ~,4f  |  ~,6f  |  ~,6f     |  ~,4f~n"
                           x y-actual y-pred error)
                   (loop (cdr points) (+ count 1))))))

;;; ============================================================
;;; Main Demonstration
;;; ============================================================

(printf "~n")
(printf "=========================================================~n")
(printf "   Polynomial Curve Fitting with Autodiff~n")
(printf "=========================================================~n")
(printf "~n")

(printf "We'll fit polynomials to noisy data generated from known functions,~n")
(printf "using gradient descent with automatically computed gradients.~n")
(printf "~n")

;;; --- Example 1: Linear Fit ---
(printf "---------------------------------------------------------~n")
(printf "1. Linear Regression: y = 2x + 3 (with noise)~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(define linear-fn (lambda (x) (+ (* 2 x) 3)))
(define linear-data (generate-data linear-fn (linspace -5 5 20) 0.5 42))

(printf "True function: y = 2x + 3~n")
(printf "Number of data points: ~a~n" (length linear-data))
(printf "~n")

(define linear-coeffs
  (fit-polynomial linear-data 1 0.01 1000 100))

(printf "~nExpected coefficients: [3, 2] (c0 + c1*x)~n")
(printf "Fitted coefficients:   ~a~n" linear-coeffs)

(print-comparison linear-data linear-coeffs 5)
(printf "~n")

;;; --- Example 2: Quadratic Fit ---
(printf "---------------------------------------------------------~n")
(printf "2. Quadratic Regression: y = 0.5x^2 - 2x + 1~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(define quadratic-fn (lambda (x) (+ (* 0.5 x x) (* -2 x) 1)))
(define quadratic-data (generate-data quadratic-fn (linspace -3 5 25) 0.3 123))

(printf "True function: y = 0.5x^2 - 2x + 1~n")
(printf "Number of data points: ~a~n" (length quadratic-data))
(printf "~n")

(define quadratic-coeffs
  (fit-polynomial quadratic-data 2 0.001 2000 200))

(printf "~nExpected coefficients: [1, -2, 0.5] (c0 + c1*x + c2*x^2)~n")
(printf "Fitted coefficients:   ~a~n" quadratic-coeffs)

(print-comparison quadratic-data quadratic-coeffs 5)
(printf "~n")

;;; --- Example 3: Cubic Fit with Momentum ---
(printf "---------------------------------------------------------~n")
(printf "3. Cubic Regression with Momentum: y = x^3 - 3x^2 + x + 2~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(define cubic-fn (lambda (x) (+ (* x x x) (* -3 x x) x 2)))
(define cubic-data (generate-data cubic-fn (linspace -2 4 30) 0.5 456))

(printf "True function: y = x^3 - 3x^2 + x + 2~n")
(printf "Number of data points: ~a~n" (length cubic-data))
(printf "~n")

(define cubic-coeffs
  (fit-polynomial-with-momentum cubic-data 3 0.0001 0.9 3000 300))

(printf "~nExpected coefficients: [2, 1, -3, 1] (c0 + c1*x + c2*x^2 + c3*x^3)~n")
(printf "Fitted coefficients:   ~a~n" cubic-coeffs)

(print-comparison cubic-data cubic-coeffs 5)
(printf "~n")

;;; --- Example 4: Underfitting and Overfitting ---
(printf "---------------------------------------------------------~n")
(printf "4. Underfitting vs Overfitting~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(printf "Fitting the quadratic data with different polynomial degrees:~n")
(printf "~n")

;; Underfit: degree 1
(printf "Degree 1 (underfit - too simple):~n")
(define underfit-coeffs
  (fit-polynomial quadratic-data 1 0.01 1000 500))
(printf "~n")

;; Good fit: degree 2
(printf "Degree 2 (good fit - matches true function):~n")
(define goodfit-coeffs
  (fit-polynomial quadratic-data 2 0.001 2000 1000))
(printf "~n")

;; Degree 3: slightly more complex than needed
(printf "Degree 3 (slightly more complex - extra parameter):~n")
(define overfit-coeffs
  (fit-polynomial-with-momentum quadratic-data 3 0.0001 0.9 2000 1000))
(printf "~n")

(printf "Comparison of final MSE:~n")
(printf "  Degree 1: ~,6f (underfit - can't capture curvature)~n" (mse-loss-plain quadratic-data underfit-coeffs))
(printf "  Degree 2: ~,6f (good fit - matches true model)~n" (mse-loss-plain quadratic-data goodfit-coeffs))
(printf "  Degree 3: ~,6f (overfit - extra unused parameter)~n" (mse-loss-plain quadratic-data overfit-coeffs))
(printf "~n")

;;; --- Example 5: Gradient Verification ---
(printf "---------------------------------------------------------~n")
(printf "5. Verifying Autodiff Gradient Computation~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(define test-coeffs '(1.0 2.0 0.5))
(define autodiff-grad (loss-gradient quadratic-data test-coeffs))
(define numerical-grad
  (map (lambda (i)
               (let* ([eps 1e-5]
                      [coeffs+ (map (lambda (c j) (if (= i j) (+ c eps) c))
                                    test-coeffs (iota 3))]
                      [coeffs- (map (lambda (c j) (if (= i j) (- c eps) c))
                                    test-coeffs (iota 3))]
                      [loss+ (mse-loss-plain quadratic-data coeffs+)]
                      [loss- (mse-loss-plain quadratic-data coeffs-)])
                     (/ (- loss+ loss-) (* 2 eps))))
       (iota 3)))

(printf "At coefficients ~a:~n" test-coeffs)
(printf "~n")
(printf "Autodiff gradient:  ~a~n" autodiff-grad)
(printf "Numerical gradient: ~a~n" numerical-grad)
(printf "~n")
(printf "Difference: ~a~n"
        (map (lambda (a n) (abs (- a n))) autodiff-grad numerical-grad))
(printf "~n")

(printf "=========================================================~n")
(printf "   Demonstration Complete~n")
(printf "=========================================================~n")
(printf "~n")
(printf "Key Takeaways:~n")
(printf "  - Autodiff computes exact gradients for any polynomial degree~n")
(printf "  - No need to derive gradient formulas by hand~n")
(printf "  - Momentum accelerates convergence significantly~n")
(printf "  - Model complexity should match data complexity~n")
(printf "  - Gradient computation scales with number of parameters~n")
(printf "~n")
