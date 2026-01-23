(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/random/prng.ss")

(doc 'module 'discrete)
(doc 'description "Pure implementation of discrete-time dynamical systems for simulation, analysis, and visualization. Supports both deterministic and stochastic transitions")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "A discrete dynamical system is: (X, f) where X is the state space and f : X -> X is the transition function (or f : X x RNG -> X for stochastic)")

(doc 'section 'discrete-dynamical-system-representation)
(doc 'note "A discrete dynamical system is represented as: (dds transition-fn dimension stochastic?)")
(doc 'note "transition-fn : state -> state (deterministic) or state x rng -> (state . rng) (stochastic)")
(doc 'note "dimension : Nat (dimension of state space, or 0 for scalar)")
(doc 'note "stochastic? : Boolean")

(define (dds? sys)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is a discrete dynamical system")
  (and (pair? sys)
       (eq? (car sys) 'dds)
       (= (length sys) 4)
       (procedure? (cadr sys))
       (integer? (caddr sys))
       (boolean? (cadddr sys))))

(define (dds-transition sys)
  (doc 'type '(-> DDS (-> α α)))
  (cadr sys))

(define (dds-dimension sys)
  (doc 'type '(-> DDS Nat))
  (caddr sys))

(define (dds-stochastic? sys)
  (doc 'type '(-> DDS Boolean))
  (cadddr sys))

(define (make-dds transition-fn dimension stochastic?)
  (doc 'type '(-> (-> α α) Nat Boolean DDS))
  (doc 'description "Create a discrete dynamical system")
  (list 'dds transition-fn dimension stochastic?))

(define (make-deterministic-dds transition-fn dimension)
  (doc 'type '(-> (-> α α) Nat DDS))
  (doc 'description "Create a deterministic discrete dynamical system")
  (make-dds transition-fn dimension #f))

(define (make-stochastic-dds transition-fn dimension)
  (doc 'type '(-> (-> α RNG (Pair α RNG)) Nat DDS))
  (doc 'description "Create a stochastic discrete dynamical system")
  (make-dds transition-fn dimension #t))

(doc 'section 'common-discrete-dynamical-systems)

(define (logistic-map r)
  (doc 'type '(-> Number DDS))
  (doc 'description "The logistic map: x_{n+1} = r * x_n * (1 - x_n). Classic example of chaos for r in [3.57, 4]")
  (make-deterministic-dds
   (lambda (x) (* r x (- 1 x)))
   0))

(define (tent-map mu)
  (doc 'type '(-> Number DDS))
  (doc 'description "The tent map: f(x) = mu * min(x, 1-x)")
  (make-deterministic-dds
   (lambda (x) (* mu (min x (- 1 x))))
   0))

(define (henon-map a b)
  (doc 'type '(-> Number Number DDS))
  (doc 'description "The Henon map: x_{n+1} = 1 - a*x_n^2 + y_n, y_{n+1} = b*x_n. Classic 2D chaotic map")
  (make-deterministic-dds
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)])
                (vector (+ 1 (- (* a x x)) y)
                        (* b x))))
   2))

(doc baker-map 'type 'DDS)
(doc baker-map 'description "The baker's map (simplified version on [0,1]^2)")
(define baker-map
  (make-deterministic-dds
   (lambda (state)
           (let ([x (vector-ref state 0)]
                 [y (vector-ref state 1)])
                (if (< x 0.5)
                    (vector (* 2 x) (/ y 2))
                    (vector (- (* 2 x) 1) (+ (/ y 2) 0.5)))))
   2))

(doc shift-map 'type 'DDS)
(doc shift-map 'description "The doubling map (mod 1): x_{n+1} = 2*x_n mod 1. Also known as the Bernoulli shift")
(define shift-map
  (make-deterministic-dds
   (lambda (x)
           (let ([doubled (* 2 x)])
                (- doubled (floor doubled))))
   0))

(define (circle-rotation alpha)
  (doc 'type '(-> Number DDS))
  (doc 'description "Rotation on the circle by angle alpha (mod 1). Quasiperiodic for irrational alpha")
  (make-deterministic-dds
   (lambda (x)
           (let ([rotated (+ x alpha)])
                (- rotated (floor rotated))))
   0))

(define (linear-congruence a c m)
  (doc 'type '(-> Nat Nat Nat DDS))
  (doc 'description "Linear congruential generator as a DDS: x_{n+1} = (a*x_n + c) mod m")
  (make-deterministic-dds
   (lambda (x) (modulo (+ (* a x) c) m))
   0))

(doc 'section 'orbit-computation)

(define (orbit sys initial-state n)
  (doc 'type '(-> DDS α Nat (List α)))
  (doc 'description "Compute the orbit of initial state for n iterations")
  (doc 'returns "List of states: (x_0, x_1, ..., x_n)")
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0] [acc (list initial-state)])
            (if (>= k n)
                (reverse acc)
                (let ([next-state (f state)])
                     (loop next-state (+ k 1) (cons next-state acc)))))))

(define (orbit-stochastic sys initial-state n rng)
  (doc 'type '(-> DDS α Nat RNG (Pair (List α) RNG)))
  (doc 'description "Compute stochastic orbit for n iterations")
  (doc 'returns "(orbit . final-rng)")
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0] [acc (list initial-state)] [gen rng])
            (if (>= k n)
                (cons (reverse acc) gen)
                (let* ([result (f state gen)]
                       [next-state (car result)]
                       [next-gen (cdr result)])
                      (loop next-state (+ k 1) (cons next-state acc) next-gen))))))

(define (iterate sys initial-state n)
  (doc 'type '(-> DDS α Nat α))
  (doc 'description "Compute the n-th iterate of the transition function")
  (doc 'returns "f^n(x) where f^n = f composed with itself n times")
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0])
            (if (>= k n)
                state
                (loop (f state) (+ k 1))))))

(define (iterate-until sys initial-state pred fuel)
  (doc 'type '(-> DDS α (-> α Boolean) Nat (Pair α Nat)))
  (doc 'description "Iterate until predicate is satisfied or fuel exhausted")
  (doc 'returns "(final-state . iterations-taken)")
  (let ([f (dds-transition sys)])
       (let loop ([state initial-state] [k 0])
            (cond
             [(pred state) (cons state k)]
             [(>= k fuel) (cons state k)]
             [else (loop (f state) (+ k 1))]))))

(doc 'section 'fixed-point-detection)

(define (fixed-point? sys state tolerance)
  (doc 'type '(-> DDS α Number Boolean))
  (doc 'description "Check if state is a fixed point within tolerance")
  (let* ([f (dds-transition sys)]
         [next (f state)]
         [dim (dds-dimension sys)])
        (if (= dim 0)
            ;; Scalar case
            (< (abs (- next state)) tolerance)
            ;; Vector case
            (vec-approx-equal? next state tolerance))))

(define (find-fixed-point sys initial-state max-iter tolerance)
  (doc 'type '(-> DDS α Nat Number (Option α)))
  (doc 'description "Attempt to find a fixed point by iteration")
  (doc 'returns "(ok fixed-point) or (error not-found)")
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

(define (find-fixed-point-newton sys initial-state max-iter tolerance)
  (doc 'type '(-> DDS Number Nat Number (Option Number)))
  (doc 'description "Find fixed point using Newton's method on f(x) - x = 0")
  (doc 'note "Works for unstable fixed points where iteration fails")
  (doc 'returns "(ok fixed-point) or (error not-found)")
  (let* ([f (dds-transition sys)]
         [h 1e-8])
        ;; Newton iteration: solve f(x) - x = 0
        ;; x_{n+1} = x_n - (f(x_n) - x_n) / (f'(x_n) - 1)
        (let loop ([x initial-state] [k 0])
             (if (>= k max-iter)
                 '(error not-found)
                 (let* ([fx (f x)]
                        [residual (- fx x)])
                       (if (< (abs residual) tolerance)
                           `(ok ,x)
                           (let* ([f-deriv (/ (- (f (+ x h)) (f (- x h))) (* 2 h))]
                                  [denom (- f-deriv 1)])
                                 (if (< (abs denom) 1e-12)
                                     ;; Derivative is 1 (neutral fixed point) - can't use Newton
                                     '(error not-found)
                                     (loop (- x (/ residual denom)) (+ k 1))))))))))

(define (stability-jacobian-1d f x h)
  (doc 'type '(-> (-> Number Number) Number Number Number))
  (doc 'description "Compute numerical derivative (Jacobian in 1D) at a point")
  (/ (- (f (+ x h)) (f (- x h))) (* 2 h)))

(define (classify-fixed-point-1d sys fixed-point h)
  (doc 'type '(-> DDS Number Number Symbol))
  (doc 'description "Classify a 1D fixed point by its derivative magnitude")
  (doc 'returns "stable, unstable, or neutral")
  (let* ([f (dds-transition sys)]
         [deriv (abs (stability-jacobian-1d f fixed-point h))])
        (cond
         [(< deriv 1) 'stable]
         [(> deriv 1) 'unstable]
         [else 'neutral])))

(doc 'section 'periodic-orbit-detection)

(define (period sys initial-state max-period tolerance)
  (doc 'type '(-> DDS α Nat Number (Option Nat)))
  (doc 'description "Detect the period of an orbit starting from a state using Floyd's cycle detection (tortoise and hare)")
  (doc 'returns "period or #f if no period found within max-period")
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

(define (periodic-orbit sys initial-state max-period tolerance)
  (doc 'type '(-> DDS α Nat Number (Option (List α))))
  (doc 'description "Extract the periodic orbit starting near initial-state")
  (doc 'returns "the orbit as a list or #f if not periodic")
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

(doc 'section 'lyapunov-exponent-1d)

(define (lyapunov-exponent-1d sys initial-state skip n)
  (doc 'type '(-> DDS Number Nat Nat Number))
  (doc 'description "Estimate the Lyapunov exponent for a 1D map. Computes: lim (1/n) * sum(log|f'(x_i)|)")
  (doc 'param 'skip "number of transient iterations to skip")
  (doc 'param 'n "number of iterations for averaging")
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

(doc 'section 'cobweb-diagram-data)
(doc 'note "Cobweb diagrams show the iteration process graphically. For y = f(x), we plot: (x_0, x_0) -> (x_0, f(x_0)) [vertical to curve], (x_0, f(x_0)) -> (f(x_0), f(x_0)) [horizontal to y=x], (x_1, x_1) -> (x_1, f(x_1)) [vertical to curve], and so on")

(define (cobweb-points sys initial-state n)
  (doc 'type '(-> DDS Number Nat (List (Pair Number Number))))
  (doc 'description "Generate cobweb diagram points for a 1D map")
  (doc 'returns "list of (x, y) coordinates for plotting")
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

(define (cobweb-segments sys initial-state n)
  (doc 'type '(-> DDS Number Nat (List (List (Pair Number Number)))))
  (doc 'description "Generate cobweb diagram as line segments for visualization. Each segment is ((x1, y1) (x2, y2))")
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

(doc 'section 'bifurcation-diagram-data)

(define (bifurcation-data make-sys param-min param-max param-steps skip samples initial)
  (doc 'type '(-> (-> Number DDS) Number Number Nat Nat Nat Number (List (Pair Number Number))))
  (doc 'description "Generate data for a bifurcation diagram")
  (doc 'param 'make-sys "function from parameter to DDS")
  (doc 'param 'param-min "parameter range minimum")
  (doc 'param 'param-max "parameter range maximum")
  (doc 'param 'param-steps "number of parameter values to sample")
  (doc 'param 'skip "transient iterations to skip")
  (doc 'param 'samples "number of points to collect per parameter")
  (doc 'param 'initial "initial condition")
  (doc 'returns "list of (parameter, state) pairs")
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
                                          acc)))))))

(doc 'section 'attractor-analysis)

(define (attractor-bounds sys initial-state skip samples)
  (doc 'type '(-> DDS α Nat Nat (List (Pair Number Number))))
  (doc 'description "Estimate the bounding box of the attractor")
  (doc 'returns "list of (min, max) pairs for each dimension")
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

(doc 'section 'stochastic-system-utilities)

(define (add-noise det-sys noise-fn)
  (doc 'type '(-> DDS (-> α RNG (Pair α RNG)) DDS))
  (doc 'description "Convert a deterministic system to a stochastic one by adding noise")
  (let ([f (dds-transition det-sys)]
        [dim (dds-dimension det-sys)])
       (make-stochastic-dds
        (lambda (state rng)
                (let* ([det-next (f state)]
                       [result (noise-fn det-next rng)])
                      result))
        dim)))

(define (gaussian-noise-1d sigma)
  (doc 'type '(-> Number (-> Number RNG (Pair Number RNG))))
  (doc 'description "Create a 1D Gaussian noise function with given standard deviation. Uses Box-Muller transform")
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

(define (stochastic-logistic r sigma)
  (doc 'type '(-> Number Number DDS))
  (doc 'description "Logistic map with additive Gaussian noise")
  (add-noise (logistic-map r) (gaussian-noise-1d sigma)))

(doc 'section 'time-series-analysis)

(define (time-average sys initial-state skip n observable)
  (doc 'type '(-> DDS α Nat Nat (-> α Number) Number))
  (doc 'description "Compute time average of an observable")
  (doc 'param 'skip "transient iterations")
  (doc 'param 'n "number of samples")
  (doc 'param 'observable "function from state to number")
  (let* ([orb (orbit sys (iterate sys initial-state skip) n)]
         [values (map observable (cdr orb))])  ;; Skip initial
        (if (null? values)
            0
            (/ (fold-left + 0 values) (length values)))))

(define (autocorrelation sys initial-state skip n max-lag observable)
  (doc 'type '(-> DDS α Nat Nat Nat (-> α Number) (List Number)))
  (doc 'description "Compute autocorrelation function for lags 0 to max-lag")
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
                 (iota (+ max-lag 1))))))

(doc 'section 'recurrence-analysis)

(define (recurrence-matrix sys initial-state skip n epsilon)
  (doc 'type '(-> DDS α Nat Nat Number (List (List Boolean))))
  (doc 'description "Compute the recurrence matrix for an orbit. R[i,j] = 1 if ||x_i - x_j|| < epsilon")
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

(define (recurrence-rate sys initial-state skip n epsilon)
  (doc 'type '(-> DDS α Nat Nat Number Number))
  (doc 'description "Compute the recurrence rate (fraction of recurrence matrix that is 1)")
  (let ([rm (recurrence-matrix sys initial-state skip n epsilon)])
       (/ (fold-left + 0
                     (map (lambda (row)
                                  (fold-left + 0
                                             (map (lambda (x) (if x 1 0)) row)))
                          rm))
          (* n n))))

(doc 'section 'system-composition)

(define (compose-dds sys1 sys2)
  (doc 'type '(-> DDS DDS DDS))
  (doc 'description "Compose two systems: (f o g)(x) = f(g(x)). Both systems must be deterministic and have compatible dimensions")
  (let ([f (dds-transition sys1)]
        [g (dds-transition sys2)]
        [dim (dds-dimension sys2)])  ;; Outer system's dimension
       (make-deterministic-dds
        (lambda (x) (f (g x)))
        dim)))

(define (iterate-system sys n)
  (doc 'type '(-> DDS Nat DDS))
  (doc 'description "Create a new system that is the n-th iterate of the original. f^n(x) as a single step")
  (let ([f (dds-transition sys)]
        [dim (dds-dimension sys)])
       (make-deterministic-dds
        (lambda (x) (iterate sys x n))
        dim)))

(define (coupled-system sys1 sys2 coupling1 coupling2)
  (doc 'type '(-> DDS DDS (-> α β α) (-> α β β) DDS))
  (doc 'description "Create a coupled system from two subsystems. State is (vector state1 state2)")
  (doc 'param 'coupling1 "how sys2's state affects sys1")
  (doc 'param 'coupling2 "how sys1's state affects sys2")
  (let ([f1 (dds-transition sys1)]
        [f2 (dds-transition sys2)]
        [d1 (dds-dimension sys1)]
        [d2 (dds-dimension sys2)])
       (make-deterministic-dds
        (lambda (state)
                (let ([s1 (if (= d1 0) (vector-ref state 0) (vec-take state d1))]
                      [s2 (if (= d2 0) (vector-ref state 1) (vec-drop state d1))])
                     (let ([next1 (coupling1 (f1 s1) s2)]
                           [next2 (coupling2 s1 (f2 s2))])
                          (if (and (= d1 0) (= d2 0))
                              (vector next1 next2)
                              ;; For vector systems, concatenate
                              (vec-append (if (= d1 0) (vector next1) next1)
                                          (if (= d2 0) (vector next2) next2))))))
        (+ (max d1 1) (max d2 1)))))

(doc 'section 'period-doubling-detection)
(doc 'note "For discrete maps, period-doubling occurs when the multiplier (derivative at fixed point) crosses -1")

(define (map-multiplier sys fixed-point h)
  (doc 'type '(-> DDS Number Number Number))
  (doc 'description "Compute the multiplier (derivative) of a 1D map at a fixed point")
  (doc 'note "For stability: |multiplier| < 1 = stable, |multiplier| > 1 = unstable")
  (doc 'note "Period-doubling occurs when multiplier = -1")
  (stability-jacobian-1d (dds-transition sys) fixed-point h))

(define (track-multiplier-along-parameter make-sys param-min param-max param-steps fp-initial tolerance)
  (doc 'type '(-> (-> Number DDS) Number Number Nat Number Number
                  (List (List Number Number Number Symbol))))
  (doc 'description "Track a fixed point and its multiplier as parameter varies")
  (doc 'param 'make-sys "function from parameter to DDS")
  (doc 'param 'fp-initial "initial guess for fixed point")
  (doc 'returns "list of (param fixed-point multiplier stability) tuples")
  (let* ([step (/ (- param-max param-min) (max 1 (- param-steps 1)))]
         [h 1e-7])
        (let loop ([i 0] [fp-guess fp-initial] [acc '()])
             (if (>= i param-steps)
                 (reverse acc)
                 (let* ([param (+ param-min (* i step))]
                        [sys (make-sys param)]
                        ;; Find the fixed point using Newton (works for unstable fixed points)
                        [fp-result (find-fixed-point-newton sys fp-guess 50 tolerance)]
                        [fp (if (and (pair? fp-result) (eq? (car fp-result) 'ok))
                                (cadr fp-result)
                                fp-guess)]  ; Use guess if not found
                        ;; Compute multiplier
                        [mult (map-multiplier sys fp h)]
                        ;; Classify stability
                        [stab (cond
                               [(< (abs mult) (- 1 tolerance)) 'stable]
                               [(> (abs mult) (+ 1 tolerance)) 'unstable]
                               [(< mult (- tolerance)) 'period-doubling-boundary]  ; mult ≈ -1
                               [else 'neutral])]
                        [entry (list param fp mult stab)])
                       (loop (+ i 1) fp (cons entry acc)))))))

(define (detect-period-doubling-map make-sys param-min param-max param-steps fp-initial tolerance)
  (doc 'export #t)
  (doc 'type '(-> (-> Number DDS) Number Number Nat Number Number
                  (List (List Number Number))))
  (doc 'description "Detect period-doubling bifurcations in a parameterized discrete map")
  (doc 'note "Period-doubling occurs when the multiplier crosses -1")
  (doc 'returns "list of (param critical-point) pairs where period-doubling occurs")
  (let ([tracked (track-multiplier-along-parameter make-sys param-min param-max
                                                    param-steps fp-initial tolerance)])
       (if (< (length tracked) 2)
           '()
           ;; Find where multiplier crosses -1
           (let loop ([prev (car tracked)]
                      [rest (cdr tracked)]
                      [bifurcations '()])
                (if (null? rest)
                    (reverse bifurcations)
                    (let* ([curr (car rest)]
                           [prev-mult (caddr prev)]
                           [curr-mult (caddr curr)])
                          ;; Multiplier crosses -1 when sign of (mult + 1) changes
                          ;; and multiplier is negative
                          (if (and (< prev-mult 0) (< curr-mult 0)
                                   (< (* (+ prev-mult 1) (+ curr-mult 1)) 0))
                              ;; Interpolate to find approximate crossing point
                              (let* ([prev-param (car prev)]
                                     [curr-param (car curr)]
                                     [prev-fp (cadr prev)]
                                     [curr-fp (cadr curr)]
                                     ;; Linear interpolation
                                     [t (/ (+ prev-mult 1) (- prev-mult curr-mult))]
                                     [bif-param (+ prev-param (* t (- curr-param prev-param)))]
                                     [bif-fp (+ prev-fp (* t (- curr-fp prev-fp)))])
                                    (loop curr (cdr rest)
                                          (cons (list bif-param bif-fp) bifurcations)))
                              (loop curr (cdr rest) bifurcations))))))))

(define (find-period-2-orbit sys initial-guess tolerance max-iter)
  (doc 'type '(-> DDS Number Number Nat (Option (Pair Number Number))))
  (doc 'description "Find a period-2 orbit of a 1D map after period-doubling")
  (doc 'returns "(x1 . x2) where f(x1) = x2 and f(x2) = x1, or #f if not found")
  ;; After period-doubling, the iterate f^2 has the period-2 points as fixed points
  ;; Use Newton's method on f^2(x) - x = 0
  (let* ([f (dds-transition sys)]
         [f2 (lambda (x) (f (f x)))]  ; f composed with itself
         [h 1e-8])
        ;; Newton iteration on f^2(x) - x = 0
        (let newton ([x initial-guess] [iter 0])
             (if (>= iter max-iter)
                 #f
                 (let* ([f2x (f2 x)]
                        [residual (- f2x x)])
                       (if (< (abs residual) tolerance)
                           ;; Found a period-2 point (or fixed point)
                           (let ([x2 (f x)])
                                (if (< (abs (- x2 x)) tolerance)
                                    ;; This is actually a fixed point, not period-2
                                    ;; Try a different initial guess
                                    (if (> iter 0)
                                        #f  ; Already tried
                                        (newton (+ initial-guess 0.1) (+ iter 1)))
                                    ;; Genuine period-2 orbit
                                    (cons x x2)))
                           ;; Newton step: x' = x - (f²(x) - x) / (f²'(x) - 1)
                           (let* ([f2-deriv (/ (- (f2 (+ x h)) (f2 (- x h))) (* 2 h))]
                                  [denom (- f2-deriv 1)])
                                 (if (< (abs denom) 1e-12)
                                     #f  ; Derivative too small
                                     (newton (- x (/ residual denom)) (+ iter 1))))))))))

(define (follow-period-doubling-cascade make-sys param-start param-end max-doublings
                                         fp-initial tolerance)
  (doc 'export #t)
  (doc 'type '(-> (-> Number DDS) Number Number Nat Number Number
                  (List (List Number Nat Number))))
  (doc 'description "Follow a period-doubling cascade to multiple bifurcations")
  (doc 'returns "list of (bifurcation-param period multiplier) showing the cascade")
  (doc 'note "Classic example: logistic map r=3 to r=4 shows period 1→2→4→8→... cascade")
  (doc 'note "Uses f^k iterate to detect period-k orbit becoming unstable")
  (let ([param-steps 200])
       (let cascade-loop ([current-period 1]
                          [p-min param-start]
                          [fp-guess fp-initial]
                          [doublings 0]
                          [results '()])
            (if (>= doublings max-doublings)
                (reverse results)
                ;; To find period-doubling of a period-k orbit, analyze f^k
                ;; A period-k orbit is a fixed point of f^k
                (let* ([make-iterated-sys (lambda (p)
                                            (iterate-system (make-sys p) current-period))]
                       [bifurcations (detect-period-doubling-map make-iterated-sys p-min param-end
                                                                  param-steps fp-guess tolerance)])
                      (if (null? bifurcations)
                          (reverse results)  ; No more doublings found
                          (let* ([bif (car bifurcations)]
                                 [bif-param (car bif)]
                                 [bif-fp (cadr bif)]
                                 [new-period (* 2 current-period)]
                                 ;; Get multiplier of f^k at bifurcation
                                 [sys-iterated (make-iterated-sys bif-param)]
                                 [mult (map-multiplier sys-iterated bif-fp 1e-7)]
                                 [entry (list bif-param new-period mult)])
                                (cascade-loop new-period
                                              (+ bif-param (* 0.01 (- param-end param-start)))
                                              bif-fp
                                              (+ doublings 1)
                                              (cons entry results)))))))))

(define (estimate-feigenbaum-delta-discrete cascade-data)
  (doc 'type '(-> (List (List Number Nat Number)) Number))
  (doc 'description "Estimate Feigenbaum delta constant from period-doubling cascade data")
  (doc 'note "Universal constant δ ≈ 4.669... for unimodal maps")
  (doc 'note "Warning: Numerically unstable for later bifurcations due to catastrophic cancellation")
  (let ([params (map car cascade-data)]
        [min-diff 1e-12])  ; Guard against division by zero
       (if (< (length params) 3)
           0
           (let loop ([ps params] [deltas '()])
                (if (< (length ps) 3)
                    (if (null? deltas)
                        0
                        (/ (apply + deltas) (length deltas)))
                    (let* ([r1 (car ps)]
                           [r2 (cadr ps)]
                           [r3 (caddr ps)]
                           [diff (- r3 r2)])
                          ;; Guard against division by zero from numerical noise
                          (if (< (abs diff) min-diff)
                              (loop (cdr ps) deltas)  ; Skip this ratio
                              (let ([delta (/ (- r2 r1) diff)])
                                   (loop (cdr ps) (cons delta deltas))))))))))

(doc 'section 'display-and-debugging)

(define (dds->string sys)
  (doc 'type '(-> DDS String))
  (doc 'description "String representation of a discrete dynamical system")
  (string-append
   "Discrete Dynamical System:\n"
   "  Dimension: " (number->string (dds-dimension sys)) "\n"
   "  Stochastic: " (if (dds-stochastic? sys) "yes" "no")))
