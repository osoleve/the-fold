(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ode-explicit
;;; @requires prelude vector-space
(require 'prelude)
(require 'vector-space)

(doc 'module 'ode-explicit)
(doc 'description "Generic explicit ODE integrators parameterized by a vector-space.
Each integrator takes a vspace as its first argument, enabling the same algorithms
to work over lists, Scheme vectors, or any representation that implements the
vector-space interface.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Single-Step Methods
;;; ============================================================

(doc 'section 'single-step)

;;; generic-euler-step : VSpace × f × Number × V × Number → V
;;; Forward Euler: y_{n+1} = y_n + dt * f(t_n, y_n)
(define (generic-euler-step vs f t state dt)
  (doc 'export #t)
  (let ([madd (vspace-madd vs)])
    (madd state dt (f t state))))

;;; generic-midpoint-step : VSpace × f × Number × V × Number → V
;;; Midpoint method (RK2): y_{n+1} = y_n + dt * f(t + dt/2, y_n + dt/2 * k1)
(define (generic-midpoint-step vs f t state dt)
  (doc 'export #t)
  (let ([madd (vspace-madd vs)])
    (let* ([k1 (f t state)]
           [half-dt (/ dt 2)]
           [mid-state (madd state half-dt k1)]
           [k2 (f (+ t half-dt) mid-state)])
      (madd state dt k2))))

;;; generic-heun-step : VSpace × f × Number × V × Number → V
;;; Heun's method (improved Euler / trapezoid RK2)
(define (generic-heun-step vs f t state dt)
  (doc 'export #t)
  (let ([add (vspace-add vs)]
        [madd (vspace-madd vs)])
    (let* ([k1 (f t state)]
           [euler-state (madd state dt k1)]
           [k2 (f (+ t dt) euler-state)]
           [avg (add k1 k2)])
      (madd state (/ dt 2) avg))))

;;; generic-rk4-step : VSpace × f × Number × V × Number → V
;;; Classic 4th-order Runge-Kutta
(define (generic-rk4-step vs f t state dt)
  (doc 'export #t)
  (let ([add (vspace-add vs)]
        [scale (vspace-scale vs)]
        [madd (vspace-madd vs)])
    (let* ([half-dt (/ dt 2)]
           [k1 (f t state)]
           [k2 (f (+ t half-dt) (madd state half-dt k1))]
           [k3 (f (+ t half-dt) (madd state half-dt k2))]
           [k4 (f (+ t dt) (madd state dt k3))]
           [weighted (add k1
                         (add (scale 2 k2)
                              (add (scale 2 k3) k4)))])
      (madd state (/ dt 6) weighted))))

;;; ============================================================
;;; Multi-Step Integration
;;; ============================================================

(doc 'section 'multi-step)

;;; generic-integrate : VSpace × step × f × Number × V × Number × Nat → (List V)
;;; Integrate an ODE over n steps using the given single-step method.
;;; step must have signature (vs f t state dt) → V.
;;; Returns list of states including the initial state.
(define (generic-integrate vs step f t0 state0 dt n)
  (doc 'export #t)
  (let loop ([t t0] [state state0] [i 0] [results (list state0)])
    (if (>= i n)
        (reverse results)
        (let ([new-state (step vs f t state dt)])
          (loop (+ t dt) new-state (+ i 1) (cons new-state results))))))
