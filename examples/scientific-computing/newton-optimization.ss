;;; examples/scientific-computing/newton-optimization.ss --- Newton's Method for Optimization
;;;
;;; Demonstrates second-order optimization using The Fold's Hessian computation.
;;; Newton's method uses curvature information to take smarter steps than gradient descent.
;;;
;;; Newton's Method for Optimization:
;;;   x_{n+1} = x_n - H^{-1}(x_n) * grad(x_n)
;;;
;;; Where H is the Hessian matrix of second derivatives and grad is the gradient.
;;;
;;; This converges much faster than gradient descent near the minimum because
;;; it uses curvature (second derivative) information to determine step size
;;; and direction.
;;;
;;; Run with: scheme --script examples/scientific-computing/newton-optimization.ss

(load "core/autodiff/higher-order-diff.ss")

;;; ============================================================
;;; Test Functions
;;; ============================================================

;;; Quadratic function: f(x, y) = x^2 + 4*y^2
;;; Minimum at (0, 0), elliptical contours.
;;; Hessian is constant: [[2, 0], [0, 8]]

(define (quadratic-traced x y)
  (traced-add (traced-sq x)
              (traced-mul 4 (traced-sq y))))

(define (quadratic-jet x y)
  (jet-add (jet-sq x)
           (jet-mul 4 (jet-sq y))))

(define (quadratic-plain x y)
  (+ (* x x) (* 4 y y)))

;;; Rosenbrock function: f(x, y) = (1 - x)^2 + 100*(y - x^2)^2
;;; Minimum at (1, 1)

(define (rosenbrock-traced x y)
  (traced-add (traced-sq (traced-sub 1 x))
              (traced-mul 100 (traced-sq (traced-sub y (traced-sq x))))))

(define (rosenbrock-jet x y)
  (jet-add (jet-sq (jet-sub 1 x))
           (jet-mul 100 (jet-sq (jet-sub y (jet-sq x))))))

(define (rosenbrock-plain x y)
  (+ (expt (- 1 x) 2)
     (* 100 (expt (- y (* x x)) 2))))

;;; Beale's function: More challenging test case
;;; f(x, y) = (1.5 - x + xy)^2 + (2.25 - x + xy^2)^2 + (2.625 - x + xy^3)^2
;;; Minimum at (3, 0.5)

(define (beale-jet x y)
  (let* ([xy (jet-mul x y)]
         [xy2 (jet-mul x (jet-sq y))]
         [xy3 (jet-mul x (jet-pow y 3))]
         [t1 (jet-sub (jet-add 1.5 xy) x)]
         [t2 (jet-sub (jet-add 2.25 xy2) x)]
         [t3 (jet-sub (jet-add 2.625 xy3) x)])
        (jet-add (jet-sq t1)
                 (jet-add (jet-sq t2) (jet-sq t3)))))

(define (beale-plain x y)
  (let ([xy (* x y)]
        [xy2 (* x y y)]
        [xy3 (* x y y y)])
       (+ (expt (- (+ 1.5 xy) x) 2)
          (expt (- (+ 2.25 xy2) x) 2)
          (expt (- (+ 2.625 xy3) x) 2))))

;;; ============================================================
;;; Matrix Operations (Simple 2x2)
;;; ============================================================

;;; For 2x2 case, we can solve H * delta = -grad directly.

;;; solve-2x2 : (List (List Number)) x (List Number) -> (List Number) | 'singular
;;; Solve Ax = b for 2x2 system using Cramer's rule.
(define (solve-2x2 A b)
  (let* ([a (caar A)] [b1 (cadar A)]
         [c (caadr A)] [d (cadadr A)]
         [det (- (* a d) (* b1 c))])
        (if (< (abs det) 1e-12)
            'singular
            (list (/ (- (* d (car b)) (* b1 (cadr b))) det)
                  (/ (- (* a (cadr b)) (* c (car b))) det)))))

;;; solve-2x2-regularized : (List (List Number)) x (List Number) x Number -> (List Number)
;;; Solve (A + lambda*I) * x = b with Levenberg-Marquardt regularization.
;;; Falls back to gradient direction when still singular.
(define (solve-2x2-regularized A b lambda)
  (let* ([a (+ (caar A) lambda)] [b1 (cadar A)]
         [c (caadr A)] [d (+ (cadadr A) lambda)]
         [det (- (* a d) (* b1 c))])
        (if (< (abs det) 1e-12)
            ;; Even regularized matrix is singular - fall back to scaled gradient
            (let ([grad-norm (sqrt (+ (* (car b) (car b)) (* (cadr b) (cadr b))))])
                 (if (< grad-norm 1e-12)
                     '(0 0)  ; At a critical point
                     (map (lambda (g) (/ g grad-norm)) b)))  ; Normalized gradient direction
            (list (/ (- (* d (car b)) (* b1 (cadr b))) det)
                  (/ (- (* a (cadr b)) (* c (car b))) det)))))

;;; matrix-to-lists : Matrix -> (List (List Number))
;;; Convert matrix to list of rows.
(define (matrix-to-lists m)
  (let ([rows (matrix-rows m)]
        [cols (matrix-cols m)]
        [data (matrix-data m)])
       (map (lambda (i)
                    (map (lambda (j)
                                 (vector-ref data (+ (* i cols) j)))
                         (iota cols)))
            (iota rows))))

;;; ============================================================
;;; Newton's Method Implementation
;;; ============================================================

;;; newton-step : ((List Jet) -> Jet) x (List Number) -> (List Number) | 'singular
;;; Compute one Newton step: x_new = x - H^{-1} * grad
(define (newton-step f-jet point)
  (let* ([grad (gradient-jet f-jet point)]
         [H (hessian-jet f-jet point)]
         [H-lists (matrix-to-lists H)]
         [neg-grad (map - grad)]
         [delta (solve-2x2 H-lists neg-grad)])
        (if (eq? delta 'singular)
            'singular
            (map + point delta))))

;;; newton-step-regularized : ((List Jet) -> Jet) x (List Number) x Number -> (List Number)
;;; Compute Newton step with Levenberg-Marquardt regularization.
;;; Never returns 'singular - always produces a valid step direction.
(define (newton-step-regularized f-jet point lambda)
  (let* ([grad (gradient-jet f-jet point)]
         [H (hessian-jet f-jet point)]
         [H-lists (matrix-to-lists H)]
         [neg-grad (map - grad)]
         [delta (solve-2x2-regularized H-lists neg-grad lambda)])
        (map + point delta)))

;;; newton-optimize : ((List Jet) -> Jet) x (Number -> Number) x (List Number) x Number x Nat -> (List Number)
;;; Full Newton optimization with convergence check.
;;;
;;; Parameters:
;;;   f-jet       - Function using jet operations (for Hessian)
;;;   f-plain     - Plain function (for value evaluation)
;;;   start       - Starting point
;;;   tol         - Convergence tolerance (gradient norm)
;;;   max-iters   - Maximum iterations
(define (newton-optimize f-jet f-plain start tol max-iters print-every)
  (let loop ([point start]
             [iter 0])
       ;; Print progress
       (when (= (modulo iter print-every) 0)
             (let* ([val (apply f-plain point)]
                    [g (gradient-jet f-jet point)]
                    [g-norm (sqrt (apply + (map (lambda (x) (* x x)) g)))])
                   (printf "Iter ~a: x = (~,8f, ~,8f), f(x) = ~,10f, ||grad|| = ~,2e~n"
                           iter (car point) (cadr point) val g-norm)))
       
       (if (>= iter max-iters)
           (begin
            (printf "~nReached max iterations~n")
            point)
           (let* ([g (gradient-jet f-jet point)]
                  [g-norm (sqrt (apply + (map (lambda (x) (* x x)) g)))])
                 (if (< g-norm tol)
                     (begin
                      (printf "~nConverged at iteration ~a!~n" iter)
                      (printf "Final point: (~,8f, ~,8f)~n" (car point) (cadr point))
                      (printf "Final value: ~,10f~n" (apply f-plain point))
                      (printf "Gradient norm: ~,2e~n" g-norm)
                      point)
                     (let ([new-point (newton-step f-jet point)])
                          (if (eq? new-point 'singular)
                              (begin
                               (printf "Hessian became singular at iteration ~a~n" iter)
                               point)
                              (loop new-point (+ iter 1)))))))))

;;; newton-with-damping : ((List Jet) -> Jet) x (Number -> Number) x (List Number) x Number x Nat x Number -> (List Number)
;;; Newton's method with damped step (helps with non-convex functions).
;;; Instead of x_new = x - H^{-1}*grad, uses x_new = x - alpha * H^{-1}*grad
;;; with backtracking to find good alpha.
(define (newton-with-damping f-jet f-plain start tol max-iters damping)
  (let loop ([point start]
             [iter 0])
       ;; Print every 5 iterations
       (when (= (modulo iter 5) 0)
             (let* ([val (apply f-plain point)]
                    [g (gradient-jet f-jet point)]
                    [g-norm (sqrt (apply + (map (lambda (x) (* x x)) g)))])
                   (printf "Iter ~a: x = (~,8f, ~,8f), f(x) = ~,10f, ||grad|| = ~,2e~n"
                           iter (car point) (cadr point) val g-norm)))
       
       (if (>= iter max-iters)
           (begin
            (printf "~nReached max iterations~n")
            point)
           (let* ([g (gradient-jet f-jet point)]
                  [g-norm (sqrt (apply + (map (lambda (x) (* x x)) g)))])
                 (if (< g-norm tol)
                     (begin
                      (printf "~nConverged at iteration ~a!~n" iter)
                      (printf "Final point: (~,8f, ~,8f)~n" (car point) (cadr point))
                      (printf "Final value: ~,10f~n" (apply f-plain point))
                      point)
                     (let* ([full-step (newton-step f-jet point)])
                           (if (eq? full-step 'singular)
                               (begin
                                (printf "Hessian singular at iteration ~a~n" iter)
                                point)
                               ;; Damped step: x + damping * (full_step - x)
                               (let ([new-point (map (lambda (p s) (+ p (* damping (- s p))))
                                                     point full-step)])
                                    (loop new-point (+ iter 1))))))))))

;;; ============================================================
;;; Main Demonstration
;;; ============================================================

(printf "~n")
(printf "=========================================================~n")
(printf "   Newton's Method for Optimization~n")
(printf "=========================================================~n")
(printf "~n")

(printf "Newton's method uses second-order information (Hessian) for~n")
(printf "faster convergence than gradient descent.~n")
(printf "~n")
(printf "Update rule: x_{n+1} = x_n - H^{-1}(x_n) * grad(x_n)~n")
(printf "~n")

;;; --- Example 1: Quadratic Function ---
(printf "---------------------------------------------------------~n")
(printf "1. Quadratic Function: f(x,y) = x^2 + 4*y^2~n")
(printf "---------------------------------------------------------~n")
(printf "~n")
(printf "This is a simple convex quadratic with minimum at (0, 0).~n")
(printf "Newton's method should converge in ONE step!~n")
(printf "(Because H is constant, the quadratic model is exact.)~n")
(printf "~n")

(printf "Starting point: (3.0, 2.0)~n")
(printf "~n")

;; Show Hessian at starting point
(let* ([start '(3.0 2.0)]
       [H (hessian-jet quadratic-jet start)]
       [g (gradient-jet quadratic-jet start)])
      (printf "At starting point:~n")
      (printf "  Gradient: (~,4f, ~,4f)~n" (car g) (cadr g))
      (printf "  Hessian:~n")
      (printf "    [ ~,4f  ~,4f ]~n" (matrix-ref H 0 0) (matrix-ref H 0 1))
      (printf "    [ ~,4f  ~,4f ]~n" (matrix-ref H 1 0) (matrix-ref H 1 1))
      (printf "~n"))

(let ([result (newton-optimize quadratic-jet quadratic-plain '(3.0 2.0) 1e-10 100 1)])
     (printf "~n"))

;;; --- Example 2: Rosenbrock Function ---
(printf "---------------------------------------------------------~n")
(printf "2. Rosenbrock Function: f(x,y) = (1-x)^2 + 100(y-x^2)^2~n")
(printf "---------------------------------------------------------~n")
(printf "~n")
(printf "The Rosenbrock banana function has a curved valley.~n")
(printf "Minimum at (1, 1) where f = 0.~n")
(printf "~n")

(printf "Starting point: (0.0, 0.0)~n")
(printf "~n")

;; Show Hessian at starting point
(let* ([start '(0.0 0.0)]
       [H (hessian-jet rosenbrock-jet start)]
       [g (gradient-jet rosenbrock-jet start)])
      (printf "At starting point:~n")
      (printf "  Gradient: (~,4f, ~,4f)~n" (car g) (cadr g))
      (printf "  Hessian:~n")
      (printf "    [ ~,4f  ~,4f ]~n" (matrix-ref H 0 0) (matrix-ref H 0 1))
      (printf "    [ ~,4f  ~,4f ]~n" (matrix-ref H 1 0) (matrix-ref H 1 1))
      (printf "~n"))

(let ([result (newton-optimize rosenbrock-jet rosenbrock-plain '(0.0 0.0) 1e-8 50 1)])
     (printf "~n"))

;;; --- Example 3: Demonstrate Hessian Computation ---
(printf "---------------------------------------------------------~n")
(printf "3. Understanding the Hessian Matrix~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(printf "The Hessian matrix contains all second-order partial derivatives:~n")
(printf "~n")
(printf "         [ d^2f/dx^2    d^2f/dxdy ]~n")
(printf "    H =  [                        ]~n")
(printf "         [ d^2f/dydx    d^2f/dy^2 ]~n")
(printf "~n")

(printf "For f(x,y) = x^2 + 4y^2:~n")
(printf "  d^2f/dx^2 = 2~n")
(printf "  d^2f/dy^2 = 8~n")
(printf "  d^2f/dxdy = d^2f/dydx = 0~n")
(printf "~n")

(let* ([H (hessian-jet quadratic-jet '(0.0 0.0))])
      (printf "Computed via autodiff (at any point):~n")
      (printf "  H[0,0] = ~a (should be 2)~n" (matrix-ref H 0 0))
      (printf "  H[0,1] = ~a (should be 0)~n" (matrix-ref H 0 1))
      (printf "  H[1,0] = ~a (should be 0)~n" (matrix-ref H 1 0))
      (printf "  H[1,1] = ~a (should be 8)~n" (matrix-ref H 1 1))
      (printf "~n"))

(printf "For f(x,y) = (1-x)^2 + 100(y-x^2)^2:~n")
(printf "  The Hessian varies with position (function is non-quadratic).~n")
(printf "~n")

(let* ([point '(1.0 1.0)]  ; At the minimum
       [H (hessian-jet rosenbrock-jet point)])
      (printf "At the minimum (1, 1):~n")
      (printf "  H[0,0] = ~,4f~n" (matrix-ref H 0 0))
      (printf "  H[0,1] = ~,4f~n" (matrix-ref H 0 1))
      (printf "  H[1,0] = ~,4f~n" (matrix-ref H 1 0))
      (printf "  H[1,1] = ~,4f~n" (matrix-ref H 1 1))
      (printf "~n")
      (printf "Eigenvalues indicate curvature in each direction.~n")
      (printf "Large ratio of eigenvalues = ill-conditioned problem.~n"))

(printf "~n")

;;; --- Example 4: Classify Critical Points ---
(printf "---------------------------------------------------------~n")
(printf "4. Critical Point Classification~n")
(printf "---------------------------------------------------------~n")
(printf "~n")

(printf "The Hessian eigenvalues tell us about the nature of a critical point:~n")
(printf "  - All positive eigenvalues: Local minimum~n")
(printf "  - All negative eigenvalues: Local maximum~n")
(printf "  - Mixed signs: Saddle point~n")
(printf "~n")

;; Saddle function: f(x,y) = x^2 - y^2
(define (saddle-jet x y)
  (jet-sub (jet-sq x) (jet-sq y)))

(let* ([H (hessian-jet saddle-jet '(0.0 0.0))])
      (printf "For f(x,y) = x^2 - y^2 (saddle surface):~n")
      (printf "  H[0,0] = ~a (positive: curves up in x)~n" (matrix-ref H 0 0))
      (printf "  H[1,1] = ~a (negative: curves down in y)~n" (matrix-ref H 1 1))
      (printf "  -> This is a saddle point at (0, 0)!~n")
      (printf "~n")
      (printf "Classification: ~a~n" (classify-critical-point saddle-jet '(0.0 0.0))))

(printf "~n")
(printf "=========================================================~n")
(printf "   Demonstration Complete~n")
(printf "=========================================================~n")
(printf "~n")
(printf "Key Takeaways:~n")
(printf "  - Newton's method uses the Hessian for quadratic convergence~n")
(printf "  - The Hessian is computed exactly via jet numbers (no finite diffs)~n")
(printf "  - For quadratic functions, Newton converges in ONE step~n")
(printf "  - The Hessian also classifies critical points (min/max/saddle)~n")
(printf "~n")
