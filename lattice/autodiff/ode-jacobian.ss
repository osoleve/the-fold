(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module autodiff/ode-jacobian
;;; @requires prelude reverse-diff matrix
(require 'prelude)
(require 'reverse-diff)
(require 'matrix)

(doc 'module 'autodiff/ode-jacobian)
(doc 'description "Bridge between autodiff exact Jacobians and ODE solver infrastructure.
Replaces numerical finite-difference Jacobians with exact reverse-mode AD Jacobians.
Eliminates epsilon-sensitivity and provides machine-precision derivatives for
linearization, stiffness detection, and future implicit solvers.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Exact Jacobian via Reverse-Mode AD
;;; ============================================================

(doc 'section 'jacobian)

(doc 'type '(-> (-> (List Traced) (List Traced)) (List Number) Nat Matrix))
(doc 'description "Compute the exact Jacobian of a vector-valued function using
reverse-mode AD. f must accept and return lists of traced values.
Each row of the Jacobian is one gradient (one backward pass per output).

  f     : (List Traced) → (List Traced)  (traced vector function)
  x     : (List Number)                  (evaluation point)
  n-out : Nat                            (output dimension)

Returns n-out × n-in Jacobian matrix where J[i,j] = ∂f_i/∂x_j.")
(define (autodiff-jacobian f x n-out)
  ;; Each row is a gradient: ∂f_i/∂x for all x
  (let ([rows (let loop ([i 0] [acc '()])
                (if (>= i n-out)
                    (reverse acc)
                    (loop (+ i 1)
                          (cons (gradient
                                  (lambda traced-args
                                    (let ([results (f traced-args)])
                                      (list-ref results i)))
                                  x)
                                acc))))])
    (matrix-from-lists rows)))

;;; ============================================================
;;; ODE Jacobian
;;; ============================================================

(doc 'section 'ode-jacobian)

(doc 'type '(-> (-> Number (List Traced) (List Traced)) Number (List Number) Matrix))
(doc 'description "Compute the Jacobian of an ODE right-hand side ∂f/∂y at (t, y).
f-traced takes time (plain number) and state (list of traced values),
returns list of traced values. Time is held constant (not differentiated).

  f-traced : (t, y-traced) → dy/dt-traced
  t        : Number  (fixed time)
  y        : (List Number)  (state point)")
(define (ode-jacobian f-traced t y)
  (let ([n (length y)])
    (autodiff-jacobian
      (lambda (traced-y)
        (f-traced t traced-y))
      y n)))

;;; ============================================================
;;; Numerical Jacobian (for comparison)
;;; ============================================================

(doc 'section 'numerical-jacobian)

(doc 'type '(-> (-> Number (List Number) (List Number))
               Number (List Number) Matrix))
(doc 'description "Compute numerical Jacobian via central finite differences.
For comparing with exact autodiff Jacobians.")
(define (numerical-jacobian-list f t y)
  (let* ([n (length y)]
         [eps 1e-7]
         [rows (let loop-i ([i 0] [acc '()])
                 (if (>= i n)
                     (reverse acc)
                     (loop-i (+ i 1)
                             (cons (let loop-j ([j 0] [row '()])
                                     (if (>= j n)
                                         (reverse row)
                                         (let* ([y+ (jac-list-update y j (lambda (x) (+ x eps)))]
                                                [y- (jac-list-update y j (lambda (x) (- x eps)))]
                                                [f+ (f t y+)]
                                                [f- (f t y-)]
                                                [deriv (/ (- (list-ref f+ i) (list-ref f- i))
                                                          (* 2 eps))])
                                           (loop-j (+ j 1) (cons deriv row)))))
                                   acc))))])
    (matrix-from-lists rows)))

(doc 'type '(-> (-> Number (List Number) (List Number))
               (-> Number (List Traced) (List Traced))
               Number (List Number) Alist))
(doc 'description "Compare numerical and exact Jacobians at a point.
Returns alist with both matrices and max absolute difference.")
(define (jacobian-check f-numeric f-traced t y)
  (let* ([n (length y)]
         [J-exact (ode-jacobian f-traced t y)]
         [J-num (numerical-jacobian-list f-numeric t y)]
         ;; Max absolute difference
         [max-diff (let loop ([i 0] [mx 0])
                     (if (>= i n)
                         mx
                         (loop (+ i 1)
                               (let inner ([j 0] [mx mx])
                                 (if (>= j n)
                                     mx
                                     (inner (+ j 1)
                                            (max mx (abs (- (matrix-ref J-exact i j)
                                                           (matrix-ref J-num i j))))))))))])
    (list (cons 'exact J-exact)
          (cons 'numerical J-num)
          (cons 'max-diff max-diff))))

;;; ============================================================
;;; Stiffness Detection
;;; ============================================================

(doc 'section 'stiffness)

(doc 'type '(-> Matrix Nat Number))
(doc 'description "Estimate the spectral radius (largest |eigenvalue|) of a matrix
using the power method. Returns the dominant eigenvalue magnitude.")
(define (spectral-radius-estimate J max-iter)
  (let* ([n (matrix-rows J)]
         ;; Start with unit vector e_0
         [v0 (let ([v (make-vector n 0)])
               (vector-set! v 0 1.0)
               v)])
    (let loop ([iter 0] [v v0] [lambda-est 1.0])
      (if (>= iter max-iter)
          (abs lambda-est)
          ;; w = J * v (manual matrix-vector multiply)
          (let* ([w (let row-loop ([i 0] [wv (make-vector n 0)])
                      (if (>= i n)
                          wv
                          (let col-loop ([j 0] [sum 0])
                            (if (>= j n)
                                (begin (vector-set! wv i sum)
                                       (row-loop (+ i 1) wv))
                                (col-loop (+ j 1)
                                          (+ sum (* (matrix-ref J i j)
                                                   (vector-ref v j))))))))]
                 ;; ||w||
                 [norm (sqrt (let norm-loop ([i 0] [s 0])
                               (if (>= i n) s
                                   (norm-loop (+ i 1) (+ s (* (vector-ref w i)
                                                              (vector-ref w i)))))))]
                 ;; Normalize
                 [new-v (if (> norm 1e-15)
                            (let ([nv (make-vector n 0)])
                              (let scale-loop ([i 0])
                                (if (>= i n) nv
                                    (begin (vector-set! nv i (/ (vector-ref w i) norm))
                                           (scale-loop (+ i 1))))))
                            v)])
            (loop (+ iter 1) new-v norm))))))

(doc 'type '(-> (-> Number (List Traced) (List Traced))
               Number (List Number) Number))
(doc 'description "Estimate stiffness at a point using exact Jacobian.
Large spectral radius suggests stiff dynamics.")
(define (stiffness-estimate f-traced t y)
  (let ([J (ode-jacobian f-traced t y)])
    (spectral-radius-estimate J 20)))

;;; ============================================================
;;; ode-system Bridge
;;; ============================================================

(doc 'section 'ode-system-bridge)

(doc 'type '(-> Any (-> Number (List Traced) (List Traced))
               Number (List Number) Matrix))
(doc 'description "Compute exact Jacobian of an ode-system's vector field.")
(define (ode-system-exact-jacobian sys f-traced t y)
  (ode-jacobian f-traced t y))

;;; ============================================================
;;; Linearization with Exact Jacobians
;;; ============================================================

(doc 'section 'linearization)

(doc 'type '(-> (-> (List Traced) (List Traced)) (List Number) Alist))
(doc 'description "Linearize an autonomous ODE around an equilibrium using exact
autodiff Jacobians. Returns alist with A matrix and equilibrium point.")
(define (autodiff-linearize f-traced y0)
  (let ([A (autodiff-jacobian f-traced y0 (length y0))])
    (list (cons 'A A)
          (cons 'equilibrium y0)
          (cons 'dimension (length y0)))))

;;; Helper: update element at index in a list
(define (jac-list-update lst idx f)
  (if (= idx 0)
      (cons (f (car lst)) (cdr lst))
      (cons (car lst) (jac-list-update (cdr lst) (- idx 1) f))))
