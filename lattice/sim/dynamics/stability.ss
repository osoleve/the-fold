(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'vec)
(require 'matrix)
(require 'matrix-decomp)
(require 'matrix-eigen)
(require 'svd)
(require 'complex)
(require 'ode-system)

(doc 'module 'stability)
(doc 'description "Fixed point detection, linearization, eigenvalue analysis, and stability classification for continuous-time dynamical systems")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "Fixed point (equilibrium) detection, Jacobian matrix computation (linearization), Eigenvalue-based stability classification, Stability types: stable/unstable node, saddle, spiral, center")

(doc 'section 'fixed-point-detection)

(define (find-fixed-point-newton sys initial-guess tolerance step-size max-iter)
  (doc 'type '(-> Any Any Number Number Nat Any))
  (doc 'description "Find a fixed point using Newton's method")
  (doc 'param 'sys "ODE system")
  (doc 'param 'initial-guess "starting point for Newton iteration")
  (doc 'param 'tolerance "convergence tolerance")
  (doc 'param 'step-size "finite difference step for Jacobian")
  (doc 'param 'max-iter "maximum iterations")
  (doc 'returns "approximate equilibrium point or #f if not found")
  (let loop ([x initial-guess] [iter 0])
       (if (>= iter max-iter)
           #f  ; Failed to converge
           (let* ([fx (eval-vector-field sys 0 x)]
                  [norm-fx (vec-norm fx)])
                 (if (< norm-fx tolerance)
                     x  ; Converged!
                     ;; Newton step: x_new = x - J^(-1) * f(x)
                     (let* ([jac (compute-jacobian sys x step-size)]
                            [jac-inv (matrix-invert-simple jac)]
                            [delta (matrix-vec-mul jac-inv fx)]
                            [x-new (vec-sub x delta)])
                           (loop x-new (+ iter 1))))))))

(define (find-fixed-point-robust sys initial-guess tolerance step-size max-iter)
  (doc 'type '(-> Any Any Number Number Nat Any))
  (doc 'description "Find a fixed point using Newton's method with pseudoinverse for robustness near singularities")
  (doc 'param 'sys "ODE system")
  (doc 'param 'initial-guess "starting point for Newton iteration")
  (doc 'param 'tolerance "convergence tolerance")
  (doc 'param 'step-size "finite difference step for Jacobian")
  (doc 'param 'max-iter "maximum iterations")
  (doc 'returns "approximate equilibrium point or #f if not found")
  (doc 'note "Uses Moore-Penrose pseudoinverse via SVD, which handles singular/near-singular Jacobians that occur at bifurcation points")
  (let loop ([x initial-guess] [iter 0])
       (if (>= iter max-iter)
           #f  ; Failed to converge
           (let* ([fx (eval-vector-field sys 0 x)]
                  [norm-fx (vec-norm fx)])
                 (if (< norm-fx tolerance)
                     x  ; Converged!
                     ;; Newton step using pseudoinverse: x_new = x - J^+ * f(x)
                     (let* ([jac (compute-jacobian sys x step-size)]
                            [jac-pinv (pseudoinverse jac)])
                           ;; Check if pseudoinverse succeeded
                           (if (and (pair? jac-pinv) (eq? (car jac-pinv) 'error))
                               #f  ; SVD failed - give up
                               (let* ([delta (matrix-vec-mul jac-pinv fx)]
                                      [x-new (vec-sub x delta)])
                                     (loop x-new (+ iter 1))))))))))

(define (find-fixed-point-levenberg-marquardt sys initial-guess tolerance step-size max-iter)
  (doc 'type '(-> Any Any Number Number Nat Any))
  (doc 'description "Find a fixed point using Levenberg-Marquardt algorithm for robust convergence")
  (doc 'param 'sys "ODE system")
  (doc 'param 'initial-guess "starting point for iteration")
  (doc 'param 'tolerance "convergence tolerance")
  (doc 'param 'step-size "finite difference step for Jacobian")
  (doc 'param 'max-iter "maximum iterations")
  (doc 'returns "approximate equilibrium point or #f if not found")
  (doc 'note "Levenberg-Marquardt interpolates between Newton and gradient descent. The damping parameter λ makes (J^T J + λI) invertible even when J is singular.")
  (let ([lambda-init 1e-3]     ; Initial damping parameter
        [lambda-up 10.0]        ; Increase factor on rejection
        [lambda-down 0.1])      ; Decrease factor on acceptance
       (let loop ([x initial-guess] [lambda lambda-init] [iter 0])
            (if (>= iter max-iter)
                #f  ; Failed to converge
                (let* ([fx (eval-vector-field sys 0 x)]
                       [norm-fx (vec-norm fx)])
                      (if (< norm-fx tolerance)
                          x  ; Converged!
                          ;; LM step: solve (J^T J + λI) δ = J^T f(x)
                          (let* ([jac (compute-jacobian sys x step-size)]
                                 [jt (matrix-transpose jac)]
                                 [jtj (matrix-mul jt jac)]
                                 [n (matrix-rows jtj)]
                                 ;; Add damping: J^T J + λI
                                 [jtj-damped (matrix-add-diagonal jtj lambda)]
                                 [jtf (matrix-vec-mul jt fx)]
                                 ;; Solve for delta
                                 [delta (solve-linear-system jtj-damped jtf)])
                                (if (not delta)
                                    #f  ; Linear solve failed
                                    (let* ([x-new (vec-sub x delta)]
                                           [fx-new (eval-vector-field sys 0 x-new)]
                                           [norm-fx-new (vec-norm fx-new)])
                                          (if (< norm-fx-new norm-fx)
                                              ;; Accept step, decrease damping
                                              (loop x-new (* lambda lambda-down) (+ iter 1))
                                              ;; Reject step, increase damping
                                              (loop x (* lambda lambda-up) (+ iter 1))))))))))))

(define (matrix-add-diagonal m lambda)
  (doc 'type '(-> Matrix Number Matrix))
  (doc 'description "Add lambda to diagonal elements: M + λI")
  (let* ([n (matrix-rows m)]
         [data (vector-copy (matrix-data m))])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! data (+ (* i n) i)
                         (+ (vector-ref data (+ (* i n) i)) lambda)))
        (list 'matrix n n data)))

(define (solve-linear-system a b)
  (doc 'type '(-> Matrix Vec (Option Vec)))
  (doc 'description "Solve Ax = b using LU decomposition with partial pivoting")
  (doc 'returns "solution vector x or #f if singular")
  (let* ([n (matrix-rows a)]
         ;; Use simple Gaussian elimination with pivoting
         [aug (augment-matrix a b)])
        (gaussian-eliminate-with-pivot aug n)))

(define (augment-matrix a b)
  (doc 'type '(-> Matrix Vec Matrix))
  (doc 'description "Create augmented matrix [A|b]")
  (let* ([n (matrix-rows a)]
         [data (make-vector (* n (+ n 1)) 0)])
        ;; Copy A into augmented matrix
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (vector-set! data (+ (* i (+ n 1)) j)
                             (matrix-ref a i j)))
            ;; Copy b column
            (vector-set! data (+ (* i (+ n 1)) n)
                         (vector-ref b i)))
        (list 'matrix n (+ n 1) data)))

(define (gaussian-eliminate-with-pivot aug n)
  (doc 'type '(-> Matrix Nat (Option Vec)))
  (doc 'description "Gaussian elimination with partial pivoting")
  (let ([data (vector-copy (matrix-data aug))]
        [nc (+ n 1)])
       ;; Forward elimination
       (let elim-loop ([k 0])
            (if (= k n)
                ;; Back substitution
                (back-substitute data n nc)
                ;; Find pivot
                (let* ([pivot-row (find-pivot-row data k n nc)]
                       [pivot-val (vector-ref data (+ (* pivot-row nc) k))])
                      (if (< (abs pivot-val) 1e-12)
                          #f  ; Singular
                          (begin
                            ;; Swap rows
                            (swap-rows! data k pivot-row nc)
                            ;; Eliminate column
                            (do ([i (+ k 1) (+ i 1)])
                                ((= i n))
                                (let ([factor (/ (vector-ref data (+ (* i nc) k)) pivot-val)])
                                     (do ([j k (+ j 1)])
                                         ((= j nc))
                                         (vector-set! data (+ (* i nc) j)
                                                      (- (vector-ref data (+ (* i nc) j))
                                                         (* factor (vector-ref data (+ (* k nc) j))))))))
                            (elim-loop (+ k 1)))))))))

(define (find-pivot-row data k n nc)
  (doc 'type '(-> Vector Nat Nat Nat Nat))
  (let loop ([i k] [best-row k] [best-val (abs (vector-ref data (+ (* k nc) k)))])
       (if (= i n)
           best-row
           (let ([val (abs (vector-ref data (+ (* i nc) k)))])
                (if (> val best-val)
                    (loop (+ i 1) i val)
                    (loop (+ i 1) best-row best-val))))))

(define (swap-rows! data row1 row2 nc)
  (doc 'type '(-> Vector Nat Nat Nat Void))
  (unless (= row1 row2)
          (do ([j 0 (+ j 1)])
              ((= j nc))
              (let ([temp (vector-ref data (+ (* row1 nc) j))])
                   (vector-set! data (+ (* row1 nc) j)
                                (vector-ref data (+ (* row2 nc) j)))
                   (vector-set! data (+ (* row2 nc) j) temp)))))

(define (back-substitute data n nc)
  (doc 'type '(-> Vector Nat Nat Vec))
  (let ([x (make-vector n 0)])
       (let loop ([i (- n 1)])
            (if (< i 0)
                x
                (let ([sum 0])
                     (do ([j (+ i 1) (+ j 1)])
                         ((= j n))
                         (set! sum (+ sum (* (vector-ref data (+ (* i nc) j))
                                             (vector-ref x j)))))
                     (let ([diag (vector-ref data (+ (* i nc) i))])
                          (if (< (abs diag) 1e-12)
                              #f  ; Singular
                              (begin
                                (vector-set! x i (/ (- (vector-ref data (+ (* i nc) n)) sum) diag))
                                (loop (- i 1))))))))))

(define (is-fixed-point? sys point tolerance)
  (doc 'type '(-> Any Any Number Bool))
  (doc 'description "Check if a point is a fixed point (equilibrium) within tolerance")
  (< (vector-field-norm sys 0 point) tolerance))

(define (refine-fixed-point sys point tolerance step-size)
  (doc 'type '(-> Any Any Number Number Any))
  (doc 'description "Refine a fixed point estimate using Newton iteration")
  (find-fixed-point-newton sys point tolerance step-size 20))

(doc 'section 'jacobian-matrix-computation)

(define (compute-jacobian sys point h)
  (doc 'type '(-> Any Any Number Any))
  (doc 'description "Compute the Jacobian matrix of a vector field at a point using finite differences for numerical differentiation. For f : R^n → R^n, Jacobian J[i,j] = ∂f_i/∂x_j")
  (doc 'param 'sys "ODE system")
  (doc 'param 'point "point at which to compute Jacobian")
  (doc 'param 'h "step size for finite differences")
  (let* ([n (vector-length point)]
         [f0 (eval-vector-field sys 0 point)]
         [jac-data (make-vector (* n n) 0)])
        ;; For each column j (variable x_j)
        (do ([j 0 (+ j 1)])
            ((= j n))
            ;; Perturb x_j by h
            (let* ([point-plus-h (vec-copy point)]
                   [_ (vector-set! point-plus-h j (+ (vector-ref point j) h))]
                   [f-plus-h (eval-vector-field sys 0 point-plus-h)])
                  ;; Compute column j of Jacobian: (f(x + h*e_j) - f(x)) / h
                  (do ([i 0 (+ i 1)])
                      ((= i n))
                      (let ([df-dxj (/ (- (vector-ref f-plus-h i)
                                          (vector-ref f0 i))
                                       h)])
                           (vector-set! jac-data (+ (* i n) j) df-dxj)))))
        (list 'matrix n n jac-data)))

(define (linearize-at-equilibrium sys equilibrium step-size)
  (doc 'type '(-> Any Any Number Any))
  (doc 'description "Linearize an ODE system at an equilibrium point")
  (doc 'returns "the Jacobian matrix A where dx/dt ≈ A(x - x*)")
  (compute-jacobian sys equilibrium step-size))

(doc 'section 'matrix-utilities)

(define (matrix-invert-simple m)
  (doc 'type '(-> Any Any))
  (doc 'description "Simple matrix inversion using Gauss-Jordan elimination. Only works for small matrices - uses simple pivoting")
  (let* ([n (matrix-rows m)]
         [data (matrix-data m)]
         ;; Create augmented matrix [A | I]
         [aug (make-vector (* n (* n 2)) 0)])
        ;; Copy A into left half, I into right half
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (vector-set! aug (+ (* i (* n 2)) j)
                             (vector-ref data (+ (* i n) j))))
            (vector-set! aug (+ (* i (* n 2)) (+ n i)) 1.0))
        ;; Gauss-Jordan elimination
        (do ([k 0 (+ k 1)])
            ((= k n))
            ;; Find pivot
            (let ([pivot-row k]
                  [pivot-val (abs (vector-ref aug (+ (* k (* n 2)) k)))])
                 (do ([i (+ k 1) (+ i 1)])
                     ((= i n))
                     (let ([val (abs (vector-ref aug (+ (* i (* n 2)) k)))])
                          (when (> val pivot-val)
                                (set! pivot-row i)
                                (set! pivot-val val))))
                 ;; Swap rows
                 (unless (= pivot-row k)
                         (do ([j 0 (+ j 1)])
                             ((= j (* n 2)))
                             (let ([temp (vector-ref aug (+ (* k (* n 2)) j))])
                                  (vector-set! aug (+ (* k (* n 2)) j)
                                               (vector-ref aug (+ (* pivot-row (* n 2)) j)))
                                  (vector-set! aug (+ (* pivot-row (* n 2)) j) temp))))
                 ;; Scale pivot row
                 (let ([pivot (vector-ref aug (+ (* k (* n 2)) k))])
                      (when (> (abs pivot) 1e-10)
                            (do ([j 0 (+ j 1)])
                                ((= j (* n 2)))
                                (vector-set! aug (+ (* k (* n 2)) j)
                                             (/ (vector-ref aug (+ (* k (* n 2)) j)) pivot)))))
                 ;; Eliminate column k in other rows
                 (do ([i 0 (+ i 1)])
                     ((= i n))
                     (unless (= i k)
                             (let ([factor (vector-ref aug (+ (* i (* n 2)) k))])
                                  (do ([j 0 (+ j 1)])
                                      ((= j (* n 2)))
                                      (vector-set! aug (+ (* i (* n 2)) j)
                                                   (- (vector-ref aug (+ (* i (* n 2)) j))
                                                      (* factor (vector-ref aug (+ (* k (* n 2)) j)))))))))))
        ;; Extract inverse from right half
        (let ([inv-data (make-vector (* n n) 0)])
             (do ([i 0 (+ i 1)])
                 ((= i n))
                 (do ([j 0 (+ j 1)])
                     ((= j n))
                     (vector-set! inv-data (+ (* i n) j)
                                  (vector-ref aug (+ (* i (* n 2)) (+ n j))))))
             (list 'matrix n n inv-data))))

(doc 'section '2x2-eigenvalue-computation)

(define (matrix-eigenvalues-2d m)
  (doc 'type '(-> Any (List Complex)))
  (doc 'description "Compute eigenvalues of a 2x2 matrix analytically. For A = [[a, b], [c, d]], eigenvalues satisfy: λ² - (a+d)λ + (ad-bc) = 0, λ = (trace ± sqrt(trace² - 4*det)) / 2")
  (let* ([a (matrix-ref m 0 0)]
         [b (matrix-ref m 0 1)]
         [c (matrix-ref m 1 0)]
         [d (matrix-ref m 1 1)]
         [trace (+ a d)]
         [det (- (* a d) (* b c))]
         [discriminant (- (* trace trace) (* 4 det))])
        (if (< discriminant 0)
            ;; Complex eigenvalues: λ = (trace ± i*sqrt(-disc)) / 2
            (let ([real-part (/ trace 2)]
                  [imag-part (/ (sqrt (- discriminant)) 2)])
                 (list (make-complex real-part imag-part)
                       (make-complex real-part (- imag-part))))
            ;; Real eigenvalues: λ = (trace ± sqrt(disc)) / 2
            (let ([sqrt-disc (sqrt discriminant)]
                  [half-trace (/ trace 2)])
                 (list (make-complex (+ half-trace (/ sqrt-disc 2)) 0)
                       (make-complex (- half-trace (/ sqrt-disc 2)) 0))))))

(define (matrix-dominant-eigenvalue m)
  (doc 'type '(-> Any Complex))
  (doc 'description "Estimate dominant (largest magnitude) eigenvalue using power iteration")
  (let* ([n (matrix-rows m)]
         [v0 (vec-ones n)]
         [result (power-iteration m v0 100 1e-8)])
        (if (and (pair? result) (eq? (car result) 'error))
            (make-complex 0 0)  ; Failed - return zero
            (make-complex (car result) 0))))

(define (power-iteration m v max-iter tol)
  (doc 'type '(-> Any Any Nat Number (Pair Number Any)))
  (doc 'description "Simple power iteration to find dominant eigenvalue")
  (let loop ([v-curr v] [iter 0] [lambda-prev 0])
       (if (>= iter max-iter)
           (cons lambda-prev v-curr)
           (let* ([v-next-unnorm (matrix-vec-mul m v-curr)]
                  [norm (vec-norm v-next-unnorm)]
                  [v-next (if (> norm 1e-10)
                              (vec-scale (/ 1 norm) v-next-unnorm)
                              v-curr)]
                  [lambda-curr norm]
                  [change (abs (- lambda-curr lambda-prev))])
                 (if (< change tol)
                     (cons lambda-curr v-next)
                     (loop v-next (+ iter 1) lambda-curr))))))

(doc 'section 'eigenvalue-based-stability-classification)

(define (classify-stability-2d eigenvalues)
  (doc 'type '(-> (List Complex) Symbol))
  (doc 'description "Classify stability for 2D system based on eigenvalues")
  (doc 'returns "'stable-node, 'unstable-node, 'saddle, 'stable-spiral, 'unstable-spiral, 'center, or 'degenerate")
  (if (not (= (length eigenvalues) 2))
      'invalid-dimension
      (let* ([lambda1 (car eigenvalues)]
             [lambda2 (cadr eigenvalues)]
             [re1 (complex-real lambda1)]
             [re2 (complex-real lambda2)]
             [im1 (complex-imag lambda1)]
             [im2 (complex-imag lambda2)])
            (cond
             ;; Complex eigenvalues (spiral or center)
             [(> (abs im1) 1e-10)
              (cond
               [(< re1 -1e-10) 'stable-spiral]
               [(> re1 1e-10) 'unstable-spiral]
               [else 'center])]
             ;; Real eigenvalues (node or saddle)
             [(and (< re1 -1e-10) (< re2 -1e-10))
              'stable-node]
             [(and (> re1 1e-10) (> re2 1e-10))
              'unstable-node]
             [(< (* re1 re2) 0)
              'saddle]
             [else 'degenerate]))))

(define (analyze-stability sys equilibrium step-size)
  (doc 'type '(-> Any Any Number (Pair Symbol (List Complex))))
  (doc 'description "Analyze stability of an equilibrium point")
  (doc 'returns "(stability-type . eigenvalues)")
  (let* ([jac (linearize-at-equilibrium sys equilibrium step-size)]
         [eigenvalues (matrix-eigenvalues-2d jac)])
        (cons (classify-stability-2d eigenvalues) eigenvalues)))

(doc 'section 'stability-predicates)

(define (stable? stability-type)
  (doc 'type '(-> Symbol Bool))
  (doc 'description "Check if a stability type is stable")
  (if (memq stability-type '(stable-node stable-spiral)) #t #f))

(define (asymptotically-stable? stability-type)
  (doc 'type '(-> Symbol Bool))
  (doc 'description "Check if equilibrium is asymptotically stable")
  (if (memq stability-type '(stable-node stable-spiral)) #t #f))

(define (unstable? stability-type)
  (doc 'type '(-> Symbol Bool))
  (doc 'description "Check if a stability type is unstable")
  (if (memq stability-type '(unstable-node unstable-spiral saddle)) #t #f))

(doc 'section 'higher-dimensional-stability)

(define (classify-stability-nd eigenvalues)
  (doc 'type '(-> (List Complex) Symbol))
  (doc 'description "Classify stability for n-dimensional system based on eigenvalues")
  (doc 'returns "'stable, 'unstable, 'neutrally-stable, or 'unknown")
  (let ([max-real (apply max (map complex-real eigenvalues))]
        [min-real (apply min (map complex-real eigenvalues))])
       (cond
        [(< max-real -1e-10) 'stable]
        [(> min-real 1e-10) 'unstable]
        [(and (< max-real 1e-10) (> min-real -1e-10)) 'neutrally-stable]
        [else 'saddle-type])))

(define (analyze-stability-nd sys equilibrium step-size)
  (doc 'type '(-> Any Any Number (Pair Symbol (List Complex))))
  (doc 'description "Analyze stability for n-dimensional system using QR eigenvalues")
  (let* ([jac (linearize-at-equilibrium sys equilibrium step-size)]
         [n (matrix-rows jac)])
        (if (<= n 2)
            (analyze-stability sys equilibrium step-size)
            ;; For n > 2, use QR algorithm to find ALL eigenvalues
            ;; Stability depends on eigenvalue with largest REAL PART, not magnitude
            (let ([eigs (eigenvalues jac)])
                 (eigenvalue-result->stability eigs)))))

(define (eigenvalue-result->stability eig-result)
  (doc 'type '(-> Any (Pair Symbol (List Complex))))
  (doc 'description "Convert eigenvalue result to stability classification")
  (cond
   ;; Handle error case
   [(and (pair? eig-result) (eq? (car eig-result) 'error))
    (cons 'unknown (list (make-complex 0 0)))]
   ;; Handle complex-eigenvalues case: (complex-eigenvalues vec info)
   ;; info is list of (index real-part imag-part)
   [(and (pair? eig-result) (eq? (car eig-result) 'complex-eigenvalues))
    (let* ([real-vec (cadr eig-result)]
           [complex-info (caddr eig-result)]
           [eigenvalue-list (eigenvalues-from-qr-result real-vec complex-info)])
          (cons (classify-stability-nd eigenvalue-list) eigenvalue-list))]
   ;; Plain vector of real eigenvalues
   [(vector? eig-result)
    (let ([eigenvalue-list (map (lambda (r) (make-complex r 0))
                                (vector->list eig-result))])
         (cons (classify-stability-nd eigenvalue-list) eigenvalue-list))]
   [else (cons 'unknown (list (make-complex 0 0)))]))

(define (eigenvalues-from-qr-result real-vec complex-info)
  (doc 'type '(-> Vec (List Any) (List Complex)))
  (doc 'description "Reconstruct complex eigenvalues from QR result")
  ;; Build a list of complex eigenvalues
  ;; complex-info has entries like (index real-part imag-part)
  (let* ([n (vector-length real-vec)]
         [result (make-vector n #f)]
         [info-table (make-eq-hashtable)])
        ;; First, mark indices that are complex pairs
        (for-each (lambda (entry)
                    (let ([idx (car entry)]
                          [real-part (cadr entry)]
                          [imag-part (caddr entry)])
                         (hashtable-set! info-table idx (cons real-part imag-part))
                         (hashtable-set! info-table (+ idx 1) (cons real-part (- imag-part)))))
                  complex-info)
        ;; Build eigenvalue list
        (let loop ([i 0] [acc '()])
             (if (>= i n)
                 (reverse acc)
                 (let ([complex-entry (hashtable-ref info-table i #f)])
                      (if complex-entry
                          (loop (+ i 1)
                                (cons (make-complex (car complex-entry) (cdr complex-entry)) acc))
                          (loop (+ i 1)
                                (cons (make-complex (vector-ref real-vec i) 0) acc))))))))

(doc 'section 'standard-system-analysis)

(define (analyze-linear-system A)
  (doc 'type '(-> Any (Pair Symbol (List Complex))))
  (doc 'description "Analyze stability of a linear ODE system dx/dt = Ax")
  (let ([eigenvalues (matrix-eigenvalues-2d A)])
       (cons (classify-stability-2d eigenvalues) eigenvalues)))

(doc 'section 'trajectory-analysis)

(define (estimate-basin-of-attraction sys equilibrium radius grid-points)
  (doc 'type '(-> Any Any Number (List Any) (List Any)))
  (doc 'description "Estimate basin of attraction by testing which initial conditions converge to the given equilibrium")
  (doc 'returns "list of initial conditions that converge")
  (filter (lambda (point)
                  ;; Check if point is within radius of equilibrium
                  (< (vec-norm (vec-sub point equilibrium)) radius))
          grid-points))

(define (is-stable-numerically? sys equilibrium perturbation-size time-steps dt)
  (doc 'type '(-> Any Any Number Number Nat Bool))
  (doc 'description "Test stability by numerical integration from nearby initial condition")
  (doc 'returns "#t if trajectory appears to converge to equilibrium")
  ;; Create small perturbation
  (let* ([n (vector-length equilibrium)]
         [perturbation (vec-scale perturbation-size (vec-ones n))]
         [initial (vec-add equilibrium perturbation)]
         ;; Simple Euler integration (just for testing)
         [trajectory (integrate-euler sys 0 initial dt time-steps)])
        ;; Check if final state is close to equilibrium
        (let ([final-state (car (reverse trajectory))])
             (< (vec-norm (vec-sub final-state equilibrium))
                (* 2 perturbation-size)))))

(define (integrate-euler sys t0 state0 dt n)
  (doc 'type '(-> Any Number Any Number Nat (List Any)))
  (doc 'description "Simple forward Euler integration (for testing)")
  (let loop ([t t0] [state state0] [i 0] [result (list state0)])
       (if (>= i n)
           (reverse result)
           (let* ([f (eval-vector-field sys t state)]
                  [new-state (vec-add state (vec-scale dt f))])
                 (loop (+ t dt) new-state (+ i 1) (cons new-state result))))))
