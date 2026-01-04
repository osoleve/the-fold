;;; core/dynamics/discrete.ss --- Discrete Dynamical Systems
;;;
;;; Pure implementation of discrete-time dynamical systems for simulation,
;;; analysis, and visualization. Supports both deterministic and stochastic
;;; transitions.
;;;
;;; A discrete dynamical system is: (X, f) where
;;;   X is the state space
;;;   f : X -> X is the transition function (or f : X x RNG -> X for stochastic)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/linalg/vec.ss
;;;   - core/random/prng.ss (for stochastic systems)

(load "core/base/prelude.ss")
(load "core/linalg/vec.ss")
(load "core/random/prng.ss")

;;; ============================================================
;;; Discrete Dynamical System Representation
;;; ============================================================

;;; A discrete dynamical system is represented as:
;;;   (dds transition-fn dimension stochastic?)
;;;
;;; where:
;;;   transition-fn : state -> state  (deterministic)
;;;                or state x rng -> (state . rng)  (stochastic)
;;;   dimension : Nat (dimension of state space, or 0 for scalar)
;;;   stochastic? : Boolean

;;; dds? : Any -> Boolean
;;; Check if value is a discrete dynamical system.
(define (dds? sys)
  (and (pair? sys)
       (eq? (car sys) 'dds)
       (= (length sys) 4)
       (procedure? (cadr sys))
       (integer? (caddr sys))
       (boolean? (cadddr sys))))

;;; Accessors
(define (dds-transition sys) (cadr sys))
(define (dds-dimension sys) (caddr sys))
(define (dds-stochastic? sys) (cadddr sys))

;;; make-dds : (a -> a) x Nat x Boolean -> DDS
;;; Create a discrete dynamical system.
(define (make-dds transition-fn dimension stochastic?)
  (list 'dds transition-fn dimension stochastic?))

;;; make-deterministic-dds : (a -> a) x Nat -> DDS
;;; Create a deterministic discrete dynamical system.
(define (make-deterministic-dds transition-fn dimension)
  (make-dds transition-fn dimension #f))

;;; make-stochastic-dds : (a x RNG -> (a . RNG)) x Nat -> DDS
;;; Create a stochastic discrete dynamical system.
(define (make-stochastic-dds transition-fn dimension)
  (make-dds transition-fn dimension #t))

;;; ============================================================
;;; Common Discrete Dynamical Systems
;;; ============================================================

;;; logistic-map : Num -> DDS
;;; The logistic map: x_{n+1} = r * x_n * (1 - x_n)
;;; Classic example of chaos for r in [3.57, 4].
(define (logistic-map r)
  (make-deterministic-dds
   (lambda (x) (* r x (- 1 x)))
   0))

;;; tent-map : Num -> DDS
;;; The tent map: f(x) = mu * min(x, 1-x)
(define (tent-map mu)
  (make-deterministic-dds
   (lambda (x) (* mu (min x (- 1 x))))
   0))

;;; henon-map : Num x Num -> DDS
;;; The Henon map: x_{n+1} = 1 - a*x_n^2 + y_n
;;;                y_{n+1} = b*x_n
;;; Classic 2D chaotic map.
(define (henon-map a b)
  (make-deterministic-dds
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)])
                (vector (+ 1 (- (* a x x)) y)
                        (* b x))))
   2))

;;; baker-map : DDS
;;; The baker's map (simplified version on [0,1]^2).
(define baker-map
  (make-deterministic-dds
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)])
                (if (< x 0.5)
                    (vector (* 2 x) (/ y 2))
                    (vector (- (* 2 x) 1) (+ (/ y 2) 0.5)))))
   2))

;;; shift-map : DDS
;;; The doubling map (mod 1): x_{n+1} = 2*x_n mod 1
;;; Also known as the Bernoulli shift.
(define shift-map
  (make-deterministic-dds
   (lambda (x)
           (let ([doubled (* 2 x)])
                (- doubled (floor doubled))))
   0))

;;; circle-rotation : Num -> DDS
;;; Rotation on the circle by angle alpha (mod 1).
;;; Quasiperiodic for irrational alpha.
(define (circle-rotation alpha)
  (make-deterministic-dds
   (lambda (x)
           (let ([rotated (+ x alpha)])
                (- rotated (floor rotated))))
   0))

;;; linear-congruence : Nat x Nat x Nat -> DDS
;;; Linear congruential generator as a DDS: x_{n+1} = (a*x_n + c) mod m
(define (linear-congruence a c m)
  (make-deterministic-dds
   (lambda (x) (modulo (+ (* a x) c) m))
   0))

;;; ============================================================
;;; Orbit Computation
;;; ============================================================

;;; orbit : DDS x a x Nat -> (List a)
;;; Compute the orbit of initial state for n iterations.
;;; Returns list of states: (x_0, x_1, ..., x_n)
(define (orbit sys initial-state n)
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0] [acc (list initial-state)])
            (if (>= k n)
                (reverse acc)
                (let ([next-state (f state)])
                     (loop next-state (+ k 1) (cons next-state acc)))))))

;;; orbit-stochastic : DDS x a x Nat x RNG -> ((List a) . RNG)
;;; Compute stochastic orbit for n iterations.
;;; Returns (orbit . final-rng).
(define (orbit-stochastic sys initial-state n rng)
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0] [acc (list initial-state)] [gen rng])
            (if (>= k n)
                (cons (reverse acc) gen)
                (let* ([result (f state gen)]
                       [next-state (car result)]
                       [next-gen (cdr result)])
                      (loop next-state (+ k 1) (cons next-state acc) next-gen))))))

;;; iterate : DDS x a x Nat -> a
;;; Compute the n-th iterate of the transition function.
;;; Returns f^n(x) where f^n = f composed with itself n times.
(define (iterate sys initial-state n)
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0])
            (if (>= k n)
                state
                (loop (f state) (+ k 1))))))

;;; iterate-until : DDS x a x (a -> Boolean) x Nat -> (a . Nat)
;;; Iterate until predicate is satisfied or fuel exhausted.
;;; Returns (final-state . iterations-taken).
(define (iterate-until sys initial-state pred fuel)
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0])
            (cond
             [(pred state) (cons state k)]
             [(>= k fuel) (cons state k)]
             [else (loop (f state) (+ k 1))]))))

;;; ============================================================
;;; Fixed Point Detection
;;; ============================================================

;;; fixed-point? : DDS x a x Num -> Boolean
;;; Check if state is a fixed point within tolerance.
(define (fixed-point? sys state tolerance)
  (let* ([f (dds-transition sys)]
         [next (f state)]
         [dim (dds-dimension sys)])
        (if (= dim 0)
            ;; Scalar case
            (< (abs (- next state)) tolerance)
            ;; Vector case
            (vec-approx-equal? next state tolerance))))

;;; find-fixed-point : DDS x a x Nat x Num -> (Maybe a)
;;; Attempt to find a fixed point by iteration.
;;; Returns (ok fixed-point) or (error not-found).
(define (find-fixed-point sys initial-state max-iter tolerance)
  (let ([f (dds-transition sys)]
        [dim (dds-dimension sys)])
       (let loop ([state initial-state] [k 0])
            (if (>= k max-iter)
                '(error not-found)
                (let ([next (f state)])
                     (if (if (= dim 0)
                             (< (abs (- next state)) tolerance)
                             (vec-approx-equal? next state tolerance))
                         `(ok ,next)
                         (loop next (+ k 1))))))))

;;; stability-jacobian-1d : (Num -> Num) x Num x Num -> Num
;;; Compute numerical derivative (Jacobian in 1D) at a point.
(define (stability-jacobian-1d f x h)
  (/ (- (f (+ x h)) (f (- x h))) (* 2 h)))

;;; classify-fixed-point-1d : DDS x Num x Num -> Symbol
;;; Classify a 1D fixed point by its derivative magnitude.
;;; Returns: stable, unstable, or neutral.
(define (classify-fixed-point-1d sys fixed-point h)
  (let* ([f (dds-transition sys)]
         [deriv (abs (stability-jacobian-1d f fixed-point h))])
        (cond
         [(< deriv 1) 'stable]
         [(> deriv 1) 'unstable]
         [else 'neutral])))

;;; ============================================================
;;; Periodic Orbit Detection
;;; ============================================================

;;; period : DDS x a x Nat x Num -> (Nat | #f)
;;; Detect the period of an orbit starting from a state.
;;; Returns period or #f if no period found within max-period.
;;; Uses Floyd's cycle detection (tortoise and hare).
(define (period sys initial-state max-period tolerance)
  (let ([f (dds-transition sys)]
        [dim (dds-dimension sys)])
       (let ([approx-eq? (if (= dim 0)
                             (lambda (a b) (< (abs (- a b)) tolerance))
                             (lambda (a b) (vec-approx-equal? a b tolerance)))])
            ;; Phase 1: Find a cycle (if it exists)
            (let loop ([tortoise (f initial-state)]
                       [hare (f (f initial-state))]
                       [steps 1])
                 (cond
                  [(approx-eq? tortoise hare)
                   ;; Phase 2: Find the period length
                   (let find-period ([state (f tortoise)] [p 1])
                        (cond
                         [(approx-eq? state tortoise) p]
                         [(> p max-period) #f]
                         [else (find-period (f state) (+ p 1))]))]
                  [(> steps max-period) #f]
                  [else (loop (f tortoise)
                              (f (f hare))
                              (+ steps 1))])))))

;;; periodic-orbit : DDS x a x Nat x Num -> (List a) | #f
;;; Extract the periodic orbit starting near initial-state.
;;; Returns the orbit as a list or #f if not periodic.
(define (periodic-orbit sys initial-state max-period tolerance)
  (let ([p (period sys initial-state max-period tolerance)])
       (if p
           ;; Skip transient, then collect one period
           (let* ([f (dds-transition sys)]
                  [transient-skip 100]  ;; Skip transient
                  [settled (iterate sys initial-state transient-skip)])
                 (let collect ([state settled] [k 0] [acc '()])
                      (if (= k p)
                          (reverse acc)
                          (collect (f state) (+ k 1) (cons state acc)))))
           #f)))

;;; ============================================================
;;; Lyapunov Exponent (1D)
;;; ============================================================

;;; lyapunov-exponent-1d : DDS x Num x Nat x Nat -> Num
;;; Estimate the Lyapunov exponent for a 1D map.
;;; Computes: lim (1/n) * sum(log|f'(x_i)|)
;;; skip: number of transient iterations to skip
;;; n: number of iterations for averaging
(define (lyapunov-exponent-1d sys initial-state skip n)
  (let* ([f (dds-transition sys)]
         [h 1e-8]  ;; Small perturbation for derivative
         ;; Skip transient
         [settled (iterate sys initial-state skip)])
        (let loop ([state settled] [k 0] [sum 0])
             (if (>= k n)
                 (/ sum n)
                 (let ([deriv (abs (stability-jacobian-1d f state h))])
                      (if (= deriv 0)
                          ;; Derivative is zero, can't compute log
                          (loop (f state) (+ k 1) sum)
                          (loop (f state) (+ k 1) (+ sum (log deriv)))))))))

;;; ============================================================
;;; Cobweb Diagram Data
;;; ============================================================

;;; Cobweb diagrams show the iteration process graphically.
;;; For y = f(x), we plot:
;;;   (x_0, x_0) -> (x_0, f(x_0))  [vertical to curve]
;;;   (x_0, f(x_0)) -> (f(x_0), f(x_0))  [horizontal to y=x]
;;;   (x_1, x_1) -> (x_1, f(x_1))  [vertical to curve]
;;;   ... and so on

;;; cobweb-points : DDS x Num x Nat -> (List (Pair Num Num))
;;; Generate cobweb diagram points for a 1D map.
;;; Returns list of (x, y) coordinates for plotting.
(define (cobweb-points sys initial-state n)
  (let ([f (dds-transition sys)])
       (let loop ([x initial-state] [k 0] [acc (list (cons initial-state initial-state))])
            (if (>= k n)
                (reverse acc)
                (let ([fx (f x)])
                     ;; Vertical step: (x, x) -> (x, fx)
                     ;; Horizontal step: (x, fx) -> (fx, fx)
                     (loop fx
                           (+ k 1)
                           (cons (cons fx fx)
                                 (cons (cons x fx) acc))))))))

;;; cobweb-segments : DDS x Num x Nat -> (List (List (Pair Num Num)))
;;; Generate cobweb diagram as line segments for visualization.
;;; Each segment is ((x1, y1) (x2, y2)).
(define (cobweb-segments sys initial-state n)
  (let ([f (dds-transition sys)])
       (let loop ([x initial-state] [k 0] [acc '()])
            (if (>= k n)
                (reverse acc)
                (let ([fx (f x)])
                     (loop fx
                           (+ k 1)
                           ;; Add horizontal then vertical segment
                           (cons (list (cons fx fx) (cons fx (f fx)))      ;; horizontal
                                 (cons (list (cons x x) (cons x fx))       ;; vertical
                                       acc))))))))

;;; ============================================================
;;; Bifurcation Diagram Data
;;; ============================================================

;;; bifurcation-data : (Num -> DDS) x Num x Num x Nat x Nat x Nat x Num
;;;                    -> (List (Pair Num Num))
;;; Generate data for a bifurcation diagram.
;;; make-sys: function from parameter to DDS
;;; param-min, param-max: parameter range
;;; param-steps: number of parameter values to sample
;;; skip: transient iterations to skip
;;; samples: number of points to collect per parameter
;;; initial: initial condition
;;; Returns list of (parameter, state) pairs.
(define (bifurcation-data make-sys param-min param-max param-steps skip samples initial)
  (if (<= param-steps 1)
      ;; Single parameter value, avoid division by zero
      (let* ([sys (make-sys param-min)]
             [settled (iterate sys initial skip)]
             [orb (orbit sys settled samples)])
            (map (lambda (x) (cons param-min x)) (cdr orb)))
      (let ([step (/ (- param-max param-min) (- param-steps 1))])
           (let param-loop ([i 0] [acc '()])
                (if (>= i param-steps)
                    (reverse acc)
                    (let* ([param (+ param-min (* i step))]
                           [sys (make-sys param)]
                           [settled (iterate sys initial skip)]
                           [orb (orbit sys settled samples)])
                          (param-loop (+ i 1)
                                      (append (map (lambda (x) (cons param x))
                                                   (cdr orb))  ;; Skip first (it's settled)
                                              acc))))))))

;;; ============================================================
;;; Attractor Analysis
;;; ============================================================

;;; attractor-bounds : DDS x a x Nat x Nat -> (List (Pair Num Num))
;;; Estimate the bounding box of the attractor.
;;; Returns list of (min, max) pairs for each dimension.
(define (attractor-bounds sys initial-state skip samples)
  (let* ([orb (orbit sys (iterate sys initial-state skip) samples)]
         [dim (dds-dimension sys)])
        (if (= dim 0)
            ;; Scalar case
            (let ([values (cdr orb)])  ;; Skip initial
                 (if (null? values)
                     '((0 . 1))
                     (list (cons (fold-left min (car values) values)
                                 (fold-left max (car values) values)))))
            ;; Vector case
            (let ([n dim])
                 (map (lambda (i)
                              (let ([vals (map (lambda (v) (vector-ref v i)) (cdr orb))])
                                   (if (null? vals)
                                       (cons 0 1)
                                       (cons (fold-left min (car vals) vals)
                                             (fold-left max (car vals) vals)))))
                      (iota n))))))

;;; ============================================================
;;; Stochastic System Utilities
;;; ============================================================

;;; add-noise : DDS x (a x RNG -> (a . RNG)) -> DDS
;;; Convert a deterministic system to a stochastic one by adding noise.
(define (add-noise det-sys noise-fn)
  (let ([f (dds-transition det-sys)]
        [dim (dds-dimension det-sys)])
       (make-stochastic-dds
        (lambda (state rng)
                (let* ([det-next (f state)]
                       [result (noise-fn det-next rng)])
                      result))
        dim)))

;;; gaussian-noise-1d : Num -> (Num x RNG -> (Num . RNG))
;;; Create a 1D Gaussian noise function with given standard deviation.
;;; Uses Box-Muller transform.
(define (gaussian-noise-1d sigma)
  (lambda (x rng)
          (let* ([r1 (run-state random-float rng)]
                 [u1 (car r1)]
                 [rng1 (cdr r1)]
                 [r2 (run-state random-float rng1)]
                 [u2 (car r2)]
                 [rng2 (cdr r2)]
                 ;; Box-Muller transform
                 [z (* sigma (sqrt (* -2 (log u1))) (cos (* 2 π u2)))])
                (cons (+ x z) rng2))))

;;; stochastic-logistic : Num x Num -> DDS
;;; Logistic map with additive Gaussian noise.
(define (stochastic-logistic r sigma)
  (add-noise (logistic-map r) (gaussian-noise-1d sigma)))

;;; ============================================================
;;; Time Series Analysis
;;; ============================================================

;;; time-average : DDS x a x Nat x Nat x (a -> Num) -> Num
;;; Compute time average of an observable.
;;; skip: transient iterations
;;; n: number of samples
;;; observable: function from state to number
(define (time-average sys initial-state skip n observable)
  (let* ([orb (orbit sys (iterate sys initial-state skip) n)]
         [values (map observable (cdr orb))])  ;; Skip initial
        (if (null? values)
            0
            (/ (fold-left + 0 values) (length values)))))

;;; autocorrelation : DDS x a x Nat x Nat x Nat x (a -> Num) -> (List Num)
;;; Compute autocorrelation function for lags 0 to max-lag.
(define (autocorrelation sys initial-state skip n max-lag observable)
  (if (<= n 0)
      ;; No data points, return zeros to avoid division by zero
      (replicate (+ max-lag 1) 0)
      (let* ([orb (orbit sys (iterate sys initial-state skip) (+ n max-lag))]
             [values (list->vector (map observable (cdr orb)))]
             [mean (/ (fold-left + 0 (vector->list values)) (vector-length values))]
             [centered (vec-map (lambda (x) (- x mean)) values)]
             [variance (/ (vec-dot centered centered) (vector-length centered))])
            (if (= variance 0)
                (replicate (+ max-lag 1) 0)
                (map (lambda (lag)
                             (let ([sum (let loop ([i 0] [acc 0])
                                             (if (>= i n)
                                                 acc
                                                 (loop (+ i 1)
                                                       (+ acc (* (vector-ref centered i)
                                                                 (vector-ref centered (+ i lag)))))))])
                                  (/ sum (* n variance))))
                     (iota (+ max-lag 1)))))))

;;; ============================================================
;;; Recurrence Analysis
;;; ============================================================

;;; recurrence-matrix : DDS x a x Nat x Nat x Num -> (List (List Boolean))
;;; Compute the recurrence matrix for an orbit.
;;; R[i,j] = 1 if ||x_i - x_j|| < epsilon
(define (recurrence-matrix sys initial-state skip n epsilon)
  (let* ([orb (list->vector (orbit sys (iterate sys initial-state skip) n))]
         [dim (dds-dimension sys)]
         [dist (if (= dim 0)
                   (lambda (a b) (abs (- a b)))
                   vec-distance)])
        (map (lambda (i)
                     (map (lambda (j)
                                  (< (dist (vector-ref orb i) (vector-ref orb j)) epsilon))
                          (iota n)))
             (iota n))))

;;; recurrence-rate : DDS x a x Nat x Nat x Num -> Num
;;; Compute the recurrence rate (fraction of recurrence matrix that is 1).
(define (recurrence-rate sys initial-state skip n epsilon)
  (let ([rm (recurrence-matrix sys initial-state skip n epsilon)])
       (/ (fold-left + 0
                     (map (lambda (row)
                                  (fold-left + 0
                                             (map (lambda (x) (if x 1 0)) row)))
                          rm))
          (* n n))))

;;; ============================================================
;;; System Composition
;;; ============================================================

;;; compose-dds : DDS x DDS -> DDS
;;; Compose two systems: (f o g)(x) = f(g(x))
;;; Both systems must be deterministic and have compatible dimensions.
(define (compose-dds sys1 sys2)
  (let ([f (dds-transition sys1)]
        [g (dds-transition sys2)]
        [dim (dds-dimension sys2)])  ;; Outer system's dimension
       (make-deterministic-dds
        (lambda (x) (f (g x)))
        dim)))

;;; iterate-system : DDS x Nat -> DDS
;;; Create a new system that is the n-th iterate of the original.
;;; f^n(x) as a single step.
(define (iterate-system sys n)
  (let ([f (dds-transition sys)]
        [dim (dds-dimension sys)])
       (make-deterministic-dds
        (lambda (x) (iterate sys x n))
        dim)))

;;; coupled-system : DDS x DDS x (a x b -> a) x (a x b -> b) -> DDS
;;; Create a coupled system from two subsystems.
;;; coupling1: how sys2's state affects sys1
;;; coupling2: how sys1's state affects sys2
;;; State is (vector state1 state2).
(define (coupled-system sys1 sys2 coupling1 coupling2)
  (let ([f1 (dds-transition sys1)]
        [f2 (dds-transition sys2)]
        [d1 (dds-dimension sys1)]
        [d2 (dds-dimension sys2)])
       (make-deterministic-dds
        (lambda (state)
                ;; For scalar systems (d=0), they occupy 1 slot in combined state
                (let* ([d1-slots (max d1 1)]
                       [s1 (if (= d1 0) (vector-ref state 0) (vec-take state d1))]
                       [s2 (if (= d2 0) (vector-ref state d1-slots) (vec-drop state d1-slots))])
                      (let ([next1 (coupling1 (f1 s1) s2)]
                            [next2 (coupling2 s1 (f2 s2))])
                           (if (and (= d1 0) (= d2 0))
                               (vector next1 next2)
                               ;; For vector systems, concatenate
                               (vec-append (if (= d1 0) (vector next1) next1)
                                           (if (= d2 0) (vector next2) next2))))))
        (+ (max d1 1) (max d2 1)))))

;;; ============================================================
;;; Display and Debugging
;;; ============================================================

;;; dds->string : DDS -> String
;;; String representation of a discrete dynamical system.
(define (dds->string sys)
  (string-append
   "Discrete Dynamical System:\n"
   "  Dimension: " (number->string (dds-dimension sys)) "\n"
   "  Stochastic: " (if (dds-stochastic? sys) "yes" "no")))
