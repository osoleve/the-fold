(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module ode-state-space
;;; @requires prelude ode-system control/state-space vec matrix
(require 'prelude)
(require 'ode-system)
(require 'control/state-space)
(require 'vec)
(require 'matrix)

(doc 'module 'ode-state-space)
(doc 'description "Bridge between ODE systems and state-space models. Provides linearization of nonlinear ODEs around equilibrium points, conversion of state-space models to ODE systems, and state-space simulation via ODE integration.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Numerical Jacobian (finite differences)
;;; ============================================================

(doc 'section 'numerical-jacobian)

(define *default-epsilon* 1e-7)

(define (numerical-jacobian f x n-out . eps-arg)
  (doc 'type '(-> (-> Vec Vec) Vec Nat Vec Matrix))
  (doc 'description "Compute the Jacobian matrix df/dx via central finite differences. f : Vec -> Vec, x is the point, n-out is the output dimension. Returns an n-out x n-in matrix.")
  (let* ([eps (if (null? eps-arg) *default-epsilon* (car eps-arg))]
         [n-in (vector-length x)]
         [J (make-matrix n-out n-in 0)])
    (do ([j 0 (+ j 1)])
        [(= j n-in) J]
      (let* ([x-plus  (let ([v (vector-copy x)])
                        (vector-set! v j (+ (vector-ref x j) eps))
                        v)]
             [x-minus (let ([v (vector-copy x)])
                        (vector-set! v j (- (vector-ref x j) eps))
                        v)]
             [f-plus  (f x-plus)]
             [f-minus (f x-minus)]
             [inv-2eps (/ 1.0 (* 2.0 eps))])
        (do ([i 0 (+ i 1)])
            [(= i n-out)]
          (matrix-set! J i j
                       (* inv-2eps
                          (- (vector-ref f-plus i)
                             (vector-ref f-minus i)))))))))

;;; ============================================================
;;; Linearization: ODE -> State-Space
;;; ============================================================

(doc 'section 'linearization)

(define (linearize-ode f-xu x0 u0 n-x n-u . opts)
  (doc 'type '(-> (-> Vec Vec Vec) Vec Vec Nat Nat ... SS))
  (doc 'description "Linearize a nonlinear ODE dx/dt = f(x, u) around equilibrium (x0, u0). Computes A = df/dx, B = df/du via numerical Jacobian. Output equation defaults to full-state observation (C = I, D = 0). Optional keyword arguments: pass a (list C-matrix D-matrix) as the first optional arg to override output matrices.")
  (doc 'param 'f-xu "Vector field: (x, u) -> dx/dt, where x is state vec, u is input vec")
  (doc 'param 'x0 "Equilibrium state vector")
  (doc 'param 'u0 "Equilibrium input vector")
  (doc 'param 'n-x "State dimension")
  (doc 'param 'n-u "Input dimension")
  (doc 'returns "A state-space system (ss A B C D)")
  (let* ([f-x (lambda (x) (f-xu x u0))]
         [A (numerical-jacobian f-x x0 n-x)]
         [f-u (lambda (u) (f-xu x0 u))]
         [B (numerical-jacobian f-u u0 n-x)]
         [C (if (and (pair? opts) (pair? (car opts)))
                (car (car opts))
                (matrix-identity n-x))]
         [D (if (and (pair? opts) (pair? (car opts)) (pair? (cdr (car opts))))
                (cadr (car opts))
                (make-matrix n-x n-u 0))])
    (make-ss A B C D)))

(define (linearize-autonomous-ode f x0 n-x)
  (doc 'type '(-> (-> Vec Vec) Vec Nat SS))
  (doc 'description "Linearize an autonomous ODE dx/dt = f(x) around equilibrium x0. Produces a state-space model with zero input (B=0, D=0) and full-state output (C=I).")
  (doc 'param 'f "Autonomous vector field: x -> dx/dt")
  (doc 'param 'x0 "Equilibrium state vector")
  (doc 'param 'n-x "State dimension")
  (let* ([A (numerical-jacobian f x0 n-x)]
         [B (make-matrix n-x 1 0)]
         [C (matrix-identity n-x)]
         [D (make-matrix n-x 1 0)])
    (make-ss A B C D)))

;;; ============================================================
;;; Conversion: State-Space -> ODE System
;;; ============================================================

(doc 'section 'state-space-to-ode)

(define (state-space->ode sys . input-fn-arg)
  (doc 'type '(-> SS ... ODE-System))
  (doc 'description "Convert a state-space model to an ODE system for integration. The optional input-fn argument is (t -> Vec) providing the input signal u(t). Defaults to zero input.")
  (doc 'param 'sys "State-space system (ss A B C D)")
  (doc 'param 'input-fn "Optional: (t -> Vec) input signal. Defaults to zero.")
  (doc 'returns "An ODE system whose vector field computes dx/dt = A*x + B*u(t)")
  (let* ([n (ss-order sys)]
         [m (ss-inputs sys)]
         [A (ss-A sys)]
         [B (ss-B sys)]
         [zero-u (make-vector m 0)]
         [input-fn (if (null? input-fn-arg)
                       (lambda (t) zero-u)
                       (car input-fn-arg))])
    (make-ode-system
     (lambda (t x)
       (let ([u (input-fn t)])
         (vec-add (matrix-vec-mul A x)
                  (matrix-vec-mul B u))))
     n
     #f)))

;;; ============================================================
;;; RK4 integration on Scheme vectors
;;; ============================================================

(doc 'section 'vector-rk4)

(define (vec-rk4-step f t x dt)
  (doc 'type '(-> (-> Number Vec Vec) Number Vec Number Vec))
  (doc 'description "Single RK4 step for Scheme-vector state. f : (t, x) -> dx/dt.")
  (let* ([half-dt (* 0.5 dt)]
         [k1 (f t x)]
         [k2 (f (+ t half-dt) (vec-add x (vec-scale half-dt k1)))]
         [k3 (f (+ t half-dt) (vec-add x (vec-scale half-dt k2)))]
         [k4 (f (+ t dt) (vec-add x (vec-scale dt k3)))]
         [weighted (vec-add k1
                            (vec-add (vec-scale 2 k2)
                                     (vec-add (vec-scale 2 k3)
                                              k4)))])
    (vec-add x (vec-scale (/ dt 6.0) weighted))))

(define (vec-rk4-integrate f t0 x0 dt n-steps)
  (doc 'type '(-> (-> Number Vec Vec) Number Vec Number Nat (List (Number Vec))))
  (doc 'description "Integrate dx/dt = f(t, x) using RK4 over n-steps. Returns list of (time state) pairs.")
  (let loop ([t t0] [x x0] [i 0] [results (list (list t0 x0))])
    (if (>= i n-steps)
        (reverse results)
        (let* ([x-new (vec-rk4-step f t x dt)]
               [t-new (+ t dt)])
          (loop t-new x-new (+ i 1) (cons (list t-new x-new) results))))))

;;; ============================================================
;;; State-space simulation
;;; ============================================================

(doc 'section 'simulation)

(define (simulate-state-space sys x0 dt n-steps . input-fn-arg)
  (doc 'type '(-> SS Vec Number Nat ... (List (Number Vec Vec))))
  (doc 'description "Simulate a state-space model from initial state x0. Returns list of (time state output) triples. Optional input-fn : (t -> Vec), defaults to zero.")
  (doc 'param 'sys "State-space system (ss A B C D)")
  (doc 'param 'x0 "Initial state vector")
  (doc 'param 'dt "Time step")
  (doc 'param 'n-steps "Number of integration steps")
  (doc 'param 'input-fn "Optional: (t -> Vec) input signal")
  (doc 'returns "List of (time state output) triples")
  (let* ([m (ss-inputs sys)]
         [zero-u (make-vector m 0)]
         [input-fn (if (null? input-fn-arg)
                       (lambda (t) zero-u)
                       (car input-fn-arg))]
         [ode (state-space->ode sys input-fn)]
         [vf (ode-vector-field ode)]
         [trajectory (vec-rk4-integrate vf 0 x0 dt n-steps)])
    (map (lambda (entry)
           (let* ([t (car entry)]
                  [x (cadr entry)]
                  [u (input-fn t)]
                  [y (ss-output-equation sys x u)])
             (list t x y)))
         trajectory)))

(define (simulate-state-space-final sys x0 dt n-steps . input-fn-arg)
  (doc 'type '(-> SS Vec Number Nat ... (Number Vec Vec)))
  (doc 'description "Like simulate-state-space but returns only the final (time state output) triple. More memory-efficient for long simulations.")
  (let* ([m (ss-inputs sys)]
         [zero-u (make-vector m 0)]
         [input-fn (if (null? input-fn-arg)
                       (lambda (t) zero-u)
                       (car input-fn-arg))]
         [ode (state-space->ode sys input-fn)]
         [vf (ode-vector-field ode)])
    (let loop ([t 0] [x x0] [i 0])
      (if (>= i n-steps)
          (let* ([u (input-fn t)]
                 [y (ss-output-equation sys x u)])
            (list t x y))
          (loop (+ t dt) (vec-rk4-step vf t x dt) (+ i 1))))))
