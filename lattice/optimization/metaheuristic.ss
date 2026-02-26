(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude state prng convergence sort
(require 'prelude)
(require 'state)
(require 'prng)
(require 'convergence)
(require 'sort)

(doc 'module 'metaheuristic)
(doc 'description "Global optimization via metaheuristics: simulated annealing, genetic algorithms, particle swarm")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'overview)
(doc 'note "Black-box global optimization methods that don't require gradients.
These methods are particularly effective for:
  - Non-differentiable or discontinuous objectives
  - Multimodal functions with many local minima
  - Combinatorial optimization problems

All functions are parameterized by a PRNG state for reproducibility.
Thread the PRNG state through computations for deterministic results.")

;;; ============================================================================
;;; Common Utilities
;;; ============================================================================

(doc 'section 'utilities)

(define (random-point-in-box rng lower upper)
  (doc 'type '(-> PRNG (List Number) (List Number) (Pair PRNG (List Number))))
  (doc 'description "Generate a random point uniformly within the box [lower, upper]")
  (let loop ([rng rng]
             [lo lower]
             [hi upper]
             [acc '()])
       (if (null? lo)
           (cons rng (reverse acc))
           (let* ([result (run-state (random-float-range (car lo) (car hi)) rng)]
                  [val (car result)]
                  [rng2 (cdr result)])
                 (loop rng2 (cdr lo) (cdr hi) (cons val acc))))))

(define (clip-to-bounds x lower upper)
  (doc 'type '(-> (List Number) (List Number) (List Number) (List Number)))
  (doc 'description "Clip point x to stay within [lower, upper] bounds")
  (map (lambda (xi lo hi) (max lo (min hi xi))) x lower upper))

(define (point-add x y)
  (doc 'type '(-> (List Number) (List Number) (List Number)))
  (map + x y))

(define (point-sub x y)
  (doc 'type '(-> (List Number) (List Number) (List Number)))
  (map - x y))

(define (point-scale a x)
  (doc 'type '(-> Number (List Number) (List Number)))
  (map (lambda (xi) (* a xi)) x))

;;; ============================================================================
;;; Metaheuristic Result Type
;;; ============================================================================

(doc 'section 'result-type)

(define (make-meta-result x f-val iterations evaluations reason)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Number Nat Nat Symbol MetaResult))
  (doc 'description "Create a metaheuristic optimization result")
  (list 'meta-result x f-val iterations evaluations reason))

(define (meta-result? x)
  (doc 'type '(-> α Bool))
  (and (pair? x) (eq? (car x) 'meta-result)))

(define (meta-x r) (list-ref r 1))
(define (meta-f r) (list-ref r 2))
(define (meta-iterations r) (list-ref r 3))
(define (meta-evaluations r) (list-ref r 4))
(define (meta-reason r) (list-ref r 5))

;;; ============================================================================
;;; Simulated Annealing
;;; ============================================================================

(doc 'section 'simulated-annealing)

(doc 'note "Simulated Annealing (Kirkpatrick et al., 1983)

Probabilistically accepts worse solutions to escape local minima.
Temperature controls exploration vs. exploitation:
  - High T: accept almost any move (exploration)
  - Low T: accept only improvements (exploitation)

Cooling schedule determines how T decreases over iterations.")

(define (sa-exponential-cooling t0 alpha)
  (doc 'export #t)
  (doc 'type '(-> Number Number (-> Nat Number)))
  (doc 'description "Exponential cooling: T(k) = T0 * alpha^k")
  (doc 'param 't0 "Initial temperature")
  (doc 'param 'alpha "Cooling rate (typically 0.95-0.99)")
  (lambda (k) (* t0 (expt alpha k))))

(define (sa-linear-cooling t0 t-min max-iter)
  (doc 'export #t)
  (doc 'type '(-> Number Number Nat (-> Nat Number)))
  (doc 'description "Linear cooling: T(k) = T0 - k * (T0 - Tmin) / max_iter")
  (let ([rate (/ (- t0 t-min) max-iter)])
       (lambda (k) (max t-min (- t0 (* k rate))))))

(define (sa-logarithmic-cooling t0 c)
  (doc 'export #t)
  (doc 'type '(-> Number Number (-> Nat Number)))
  (doc 'description "Logarithmic cooling: T(k) = T0 / (1 + c * ln(1 + k))")
  (doc 'note "Provably converges to global optimum but very slow")
  (lambda (k) (/ t0 (+ 1 (* c (log (+ 1 k)))))))

(define (sa-neighbor-gaussian rng x sigma lower upper)
  (doc 'type '(-> PRNG (List Number) Number (List Number) (List Number) (Pair PRNG (List Number))))
  (doc 'description "Generate neighbor by adding Gaussian noise, clipped to bounds")
  (let loop ([rng rng]
             [xi x]
             [lo lower]
             [hi upper]
             [acc '()])
       (if (null? xi)
           (cons rng (reverse acc))
           (let* ([result (run-state (random-float-range -1.0 1.0) rng)]
                  [u1 (car result)]
                  [rng2 (cdr result)]
                  ;; Box-Muller would be better but this approximates Gaussian enough
                  ;; for perturbation purposes
                  [r2 (run-state (random-float-range -1.0 1.0) rng2)]
                  [u2 (car r2)]
                  [rng3 (cdr r2)]
                  [noise (* sigma (+ u1 u2))]  ; sum of uniforms approximates normal
                  [new-val (max (car lo) (min (car hi) (+ (car xi) noise)))])
                 (loop rng3 (cdr xi) (cdr lo) (cdr hi) (cons new-val acc))))))

(define (simulated-annealing f x0 lower upper cooling max-iter rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) (List Number)
                  (-> Nat Number) Nat PRNG (Pair PRNG MetaResult)))
  (doc 'description "Simulated annealing for continuous optimization")
  (doc 'param 'f "Objective function to minimize (black-box)")
  (doc 'param 'x0 "Initial point")
  (doc 'param 'lower "Lower bounds")
  (doc 'param 'upper "Upper bounds")
  (doc 'param 'cooling "Temperature schedule: iteration -> temperature")
  (doc 'param 'max-iter "Maximum iterations")
  (doc 'param 'rng "Random number generator state")
  (doc 'returns "(Pair PRNG MetaResult) - updated RNG and result")
  (let* ([n (length x0)]
         [sigma (/ (vec-norm (point-sub upper lower)) (* 10 (sqrt n)))]  ; adaptive step size
         [f0 (f x0)])
        (let loop ([rng rng]
                   [x x0]
                   [fx f0]
                   [best-x x0]
                   [best-f f0]
                   [iter 0]
                   [evals 1])
             (if (>= iter max-iter)
                 (cons rng (make-meta-result best-x best-f iter evals 'max-iter))
                 (let* ([temp (cooling iter)]
                        ;; Generate neighbor
                        [neighbor-result (sa-neighbor-gaussian rng x sigma lower upper)]
                        [rng2 (car neighbor-result)]
                        [x-new (cdr neighbor-result)]
                        [fx-new (f x-new)]
                        ;; Acceptance probability
                        [delta (- fx-new fx)]
                        [accept-result (run-state random-float rng2)]
                        [rand-val (car accept-result)]
                        [rng3 (cdr accept-result)]
                        ;; Accept if better, or with probability exp(-delta/T)
                        ;; When temp <= 0, only accept improvements (greedy)
                        [accept? (or (<= delta 0)
                                     (and (> temp 0)
                                          (< rand-val (exp (- (/ delta temp))))))])
                       (let ([new-x (if accept? x-new x)]
                             [new-fx (if accept? fx-new fx)]
                             [new-best-x (if (< fx-new best-f) x-new best-x)]
                             [new-best-f (if (< fx-new best-f) fx-new best-f)])
                            (loop rng3 new-x new-fx new-best-x new-best-f
                                  (+ iter 1) (+ evals 1))))))))

(define (sa-minimize f x0 lower upper rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) (List Number) PRNG
                  (Pair PRNG MetaResult)))
  (doc 'description "Simulated annealing with default parameters")
  (let* ([n (length x0)]
         [range-norm (vec-norm (point-sub upper lower))]
         [t0 (* range-norm 0.5)]  ; Initial temp proportional to search space
         [cooling (sa-exponential-cooling t0 0.97)]
         [max-iter (* 1000 n)])   ; Scale iterations with dimension
        (simulated-annealing f x0 lower upper cooling max-iter rng)))

;;; ============================================================================
;;; Genetic Algorithm
;;; ============================================================================

(doc 'section 'genetic-algorithm)

(doc 'note "Genetic Algorithm (Holland, 1975)

Population-based evolution using:
  - Selection: choose parents (tournament selection)
  - Crossover: combine parents to create offspring
  - Mutation: random perturbation

For continuous optimization, uses real-valued encoding with:
  - SBX (Simulated Binary Crossover)
  - Polynomial mutation")

(define (ga-tournament-select rng population fitness k)
  (doc 'type '(-> PRNG (Vector (List Number)) (Vector Number) Nat (Pair PRNG (List Number))))
  (doc 'description "Tournament selection: pick k random individuals, return best")
  (let* ([n (vector-length population)])
        (let loop ([rng rng]
                   [best #f]
                   [best-fit +inf.0]
                   [i 0])
             (if (>= i k)
                 (cons rng best)
                 (let* ([idx-result (run-state (random-int-range 0 (- n 1)) rng)]
                        [idx (car idx-result)]
                        [rng2 (cdr idx-result)]
                        [ind (vector-ref population idx)]
                        [fit (vector-ref fitness idx)])
                       (if (< fit best-fit)
                           (loop rng2 ind fit (+ i 1))
                           (loop rng2 best best-fit (+ i 1))))))))

(define (ga-sbx-crossover rng p1 p2 eta lower upper)
  (doc 'type '(-> PRNG (List Number) (List Number) Number (List Number) (List Number)
                  (Triple PRNG (List Number) (List Number))))
  (doc 'description "Simulated Binary Crossover (SBX)")
  (doc 'param 'eta "Distribution index (typically 2-5, higher = children closer to parents)")
  (let loop ([rng rng]
             [x1 p1]
             [x2 p2]
             [lo lower]
             [hi upper]
             [c1 '()]
             [c2 '()])
       (if (null? x1)
           (list rng (reverse c1) (reverse c2))
           (let* ([rand-result (run-state random-float rng)]
                  [u (car rand-result)]
                  [rng2 (cdr rand-result)]
                  ;; SBX beta calculation
                  [beta (if (< u 0.5)
                            (expt (* 2 u) (/ 1 (+ eta 1)))
                            (expt (/ 1 (* 2 (- 1 u))) (/ 1 (+ eta 1))))]
                  [xi1 (car x1)]
                  [xi2 (car x2)]
                  ;; Create offspring
                  [child1 (* 0.5 (+ (+ xi1 xi2) (* beta (- xi1 xi2))))]
                  [child2 (* 0.5 (+ (+ xi1 xi2) (* beta (- xi2 xi1))))]
                  ;; Clip to bounds
                  [c1-val (max (car lo) (min (car hi) child1))]
                  [c2-val (max (car lo) (min (car hi) child2))])
                 (loop rng2 (cdr x1) (cdr x2) (cdr lo) (cdr hi)
                       (cons c1-val c1) (cons c2-val c2))))))

(define (ga-polynomial-mutate rng x eta pm lower upper)
  (doc 'type '(-> PRNG (List Number) Number Number (List Number) (List Number) (Pair PRNG (List Number))))
  (doc 'description "Polynomial mutation")
  (doc 'param 'eta "Distribution index (typically 20)")
  (doc 'param 'pm "Mutation probability per gene")
  (let loop ([rng rng]
             [xi x]
             [lo lower]
             [hi upper]
             [acc '()])
       (if (null? xi)
           (cons rng (reverse acc))
           (let* ([mutate-result (run-state random-float rng)]
                  [mutate-val (car mutate-result)]
                  [rng2 (cdr mutate-result)]
                  [should-mutate? (< mutate-val pm)])
                 (if (not should-mutate?)
                     (loop rng2 (cdr xi) (cdr lo) (cdr hi) (cons (car xi) acc))
                     (let* ([u-result (run-state random-float rng2)]
                            [u (car u-result)]
                            [rng3 (cdr u-result)]
                            [delta (- (car hi) (car lo))]
                            ;; Polynomial distribution
                            [delta-q (if (< u 0.5)
                                         (- (expt (* 2 u) (/ 1 (+ eta 1))) 1)
                                         (- 1 (expt (* 2 (- 1 u)) (/ 1 (+ eta 1)))))]
                            [new-val (+ (car xi) (* delta delta-q))]
                            [clipped (max (car lo) (min (car hi) new-val))])
                           (loop rng3 (cdr xi) (cdr lo) (cdr hi) (cons clipped acc))))))))

(define (ga-initialize-population rng n pop-size lower upper)
  (doc 'type '(-> PRNG Nat Nat (List Number) (List Number) (Pair PRNG (Vector (List Number)))))
  (doc 'description "Initialize random population within bounds")
  (let loop ([rng rng]
             [i 0]
             [pop '()])
       (if (>= i pop-size)
           (cons rng (list->vector (reverse pop)))
           (let* ([pt-result (random-point-in-box rng lower upper)]
                  [rng2 (car pt-result)]
                  [pt (cdr pt-result)])
                 (loop rng2 (+ i 1) (cons pt pop))))))

(define (ga-evaluate-population f population)
  (doc 'type '(-> (-> (List Number) Number) (Vector (List Number)) (Vector Number)))
  (doc 'description "Evaluate fitness of entire population")
  (vector-map f population))

(define (ga-find-best population fitness)
  (doc 'type '(-> (Vector (List Number)) (Vector Number) (Pair (List Number) Number)))
  (doc 'description "Find best individual and its fitness")
  (let* ([n (vector-length population)])
        (let loop ([i 0]
                   [best-idx 0]
                   [best-fit (vector-ref fitness 0)])
             (if (>= i n)
                 (cons (vector-ref population best-idx) best-fit)
                 (if (< (vector-ref fitness i) best-fit)
                     (loop (+ i 1) i (vector-ref fitness i))
                     (loop (+ i 1) best-idx best-fit))))))

(define (genetic-algorithm f lower upper pop-size max-gen crossover-prob mutation-prob rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) Nat Nat Number Number PRNG
                  (Pair PRNG MetaResult)))
  (doc 'description "Genetic algorithm for continuous optimization")
  (doc 'param 'f "Objective function to minimize")
  (doc 'param 'lower "Lower bounds")
  (doc 'param 'upper "Upper bounds")
  (doc 'param 'pop-size "Population size (should be even)")
  (doc 'param 'max-gen "Maximum generations")
  (doc 'param 'crossover-prob "Crossover probability (typically 0.9)")
  (doc 'param 'mutation-prob "Mutation probability per gene (typically 1/n)")
  (doc 'param 'rng "Random number generator")
  (let* ([n (length lower)]
         [pop-result (ga-initialize-population rng n pop-size lower upper)]
         [rng1 (car pop-result)]
         [population (cdr pop-result)]
         [fitness (ga-evaluate-population f population)]
         [init-best (ga-find-best population fitness)])
        (let gen-loop ([rng rng1]
                       [pop population]
                       [fit fitness]
                       [best-x (car init-best)]
                       [best-f (cdr init-best)]
                       [gen 0]
                       [evals pop-size])
             (if (>= gen max-gen)
                 (cons rng (make-meta-result best-x best-f gen evals 'max-generations))
                 ;; Create next generation
                 (let offspring-loop ([rng rng]
                                      [offspring '()]
                                      [pairs 0])
                      (if (>= pairs (quotient pop-size 2))
                          ;; Evaluate new generation
                          (let* ([new-pop (list->vector offspring)]
                                 [new-fit (ga-evaluate-population f new-pop)]
                                 [gen-best (ga-find-best new-pop new-fit)]
                                 [new-best-x (if (< (cdr gen-best) best-f) (car gen-best) best-x)]
                                 [new-best-f (if (< (cdr gen-best) best-f) (cdr gen-best) best-f)])
                                (gen-loop rng new-pop new-fit new-best-x new-best-f
                                          (+ gen 1) (+ evals pop-size)))
                          ;; Create offspring pair
                          (let* (;; Select parents
                                 [p1-result (ga-tournament-select rng pop fit 3)]
                                 [rng2 (car p1-result)]
                                 [p1 (cdr p1-result)]
                                 [p2-result (ga-tournament-select rng2 pop fit 3)]
                                 [rng3 (car p2-result)]
                                 [p2 (cdr p2-result)]
                                 ;; Crossover
                                 [cross-rand (run-state random-float rng3)]
                                 [cross-val (car cross-rand)]
                                 [rng4 (cdr cross-rand)]
                                 [do-cross? (< cross-val crossover-prob)]
                                 [cross-result (if do-cross?
                                                   (ga-sbx-crossover rng4 p1 p2 2 lower upper)
                                                   (list rng4 p1 p2))]
                                 [rng5 (car cross-result)]
                                 [c1 (cadr cross-result)]
                                 [c2 (caddr cross-result)]
                                 ;; Mutation
                                 [m1-result (ga-polynomial-mutate rng5 c1 20 mutation-prob lower upper)]
                                 [rng6 (car m1-result)]
                                 [m1 (cdr m1-result)]
                                 [m2-result (ga-polynomial-mutate rng6 c2 20 mutation-prob lower upper)]
                                 [rng7 (car m2-result)]
                                 [m2 (cdr m2-result)])
                                (offspring-loop rng7 (cons m2 (cons m1 offspring)) (+ pairs 1)))))))))

(define (ga-minimize f lower upper rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) PRNG (Pair PRNG MetaResult)))
  (doc 'description "Genetic algorithm with default parameters")
  (let* ([n (length lower)]
         [pop-size (max 20 (* 10 n))]
         [max-gen (* 100 n)]
         [mutation-prob (/ 1.0 n)])
        (genetic-algorithm f lower upper pop-size max-gen 0.9 mutation-prob rng)))

;;; ============================================================================
;;; Particle Swarm Optimization
;;; ============================================================================

(doc 'section 'particle-swarm)

(doc 'note "Particle Swarm Optimization (Kennedy & Eberhart, 1995)

Particles move through search space, influenced by:
  - Inertia: continue in current direction
  - Cognitive: attraction to particle's best position
  - Social: attraction to swarm's best position

Standard PSO uses velocity update:
  v = w*v + c1*r1*(pbest - x) + c2*r2*(gbest - x)
  x = x + v")

(define (make-particle x v pbest pbest-f)
  (doc 'type '(-> (List Number) (List Number) (List Number) Number Particle))
  (list 'particle x v pbest pbest-f))

(define (particle? x)
  (and (pair? x) (eq? (car x) 'particle)))

(define (particle-x p) (list-ref p 1))
(define (particle-v p) (list-ref p 2))
(define (particle-pbest p) (list-ref p 3))
(define (particle-pbest-f p) (list-ref p 4))

(define (pso-initialize-swarm rng n swarm-size lower upper f)
  (doc 'type '(-> PRNG Nat Nat (List Number) (List Number) (-> (List Number) Number)
                  (Triple PRNG (List Particle) (Pair (List Number) Number))))
  (doc 'description "Initialize swarm with random positions and zero velocity")
  (let* ([v-max (point-scale 0.1 (point-sub upper lower))]  ; Max velocity
         [zero-v (make-list n 0)])
        (let loop ([rng rng]
                   [i 0]
                   [particles '()]
                   [gbest #f]
                   [gbest-f +inf.0])
             (if (>= i swarm-size)
                 (list rng (reverse particles) (cons gbest gbest-f))
                 (let* ([x-result (random-point-in-box rng lower upper)]
                        [rng2 (car x-result)]
                        [x (cdr x-result)]
                        [fx (f x)]
                        [p (make-particle x zero-v x fx)]
                        [new-gbest (if (< fx gbest-f) x gbest)]
                        [new-gbest-f (if (< fx gbest-f) fx gbest-f)])
                       (loop rng2 (+ i 1) (cons p particles) new-gbest new-gbest-f))))))

(define (pso-update-particle rng particle gbest w c1 c2 lower upper v-max)
  (doc 'type '(-> PRNG Particle (List Number) Number Number Number (List Number) (List Number) (List Number)
                  (Pair PRNG Particle)))
  (doc 'description "Update single particle's velocity and position")
  (let* ([x (particle-x particle)]
         [v (particle-v particle)]
         [pbest (particle-pbest particle)]
         [pbest-f (particle-pbest-f particle)]
         [n (length x)])
        ;; Generate random coefficients for each dimension
        (let loop ([rng rng]
                   [xi x]
                   [vi v]
                   [pbi pbest]
                   [gbi gbest]
                   [lo lower]
                   [hi upper]
                   [vm v-max]
                   [new-x '()]
                   [new-v '()])
             (if (null? xi)
                 (cons rng (make-particle (reverse new-x) (reverse new-v) pbest pbest-f))
                 (let* ([r1-result (run-state random-float rng)]
                        [r1 (car r1-result)]
                        [rng2 (cdr r1-result)]
                        [r2-result (run-state random-float rng2)]
                        [r2 (car r2-result)]
                        [rng3 (cdr r2-result)]
                        ;; Velocity update
                        [cognitive (* c1 r1 (- (car pbi) (car xi)))]
                        [social (* c2 r2 (- (car gbi) (car xi)))]
                        [vi-new (+ (* w (car vi)) cognitive social)]
                        ;; Clamp velocity
                        [vi-clamped (max (- (car vm)) (min (car vm) vi-new))]
                        ;; Position update
                        [xi-new (+ (car xi) vi-clamped)]
                        ;; Clamp position
                        [xi-clamped (max (car lo) (min (car hi) xi-new))])
                       (loop rng3 (cdr xi) (cdr vi) (cdr pbi) (cdr gbi)
                             (cdr lo) (cdr hi) (cdr vm)
                             (cons xi-clamped new-x)
                             (cons vi-clamped new-v)))))))

(define (particle-swarm f lower upper swarm-size max-iter w c1 c2 rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) Nat Nat Number Number Number PRNG
                  (Pair PRNG MetaResult)))
  (doc 'description "Particle swarm optimization")
  (doc 'param 'f "Objective function to minimize")
  (doc 'param 'lower "Lower bounds")
  (doc 'param 'upper "Upper bounds")
  (doc 'param 'swarm-size "Number of particles")
  (doc 'param 'max-iter "Maximum iterations")
  (doc 'param 'w "Inertia weight (typically 0.7-0.9)")
  (doc 'param 'c1 "Cognitive coefficient (typically 1.5-2.0)")
  (doc 'param 'c2 "Social coefficient (typically 1.5-2.0)")
  (doc 'param 'rng "Random number generator")
  (let* ([n (length lower)]
         [v-max (point-scale 0.2 (point-sub upper lower))]
         [init-result (pso-initialize-swarm rng n swarm-size lower upper f)]
         [rng1 (car init-result)]
         [swarm (cadr init-result)]
         [init-gbest (caddr init-result)]
         [gbest (car init-gbest)]
         [gbest-f (cdr init-gbest)])
        (let iter-loop ([rng rng1]
                        [swarm swarm]
                        [gbest gbest]
                        [gbest-f gbest-f]
                        [iter 0]
                        [evals swarm-size])
             (if (>= iter max-iter)
                 (cons rng (make-meta-result gbest gbest-f iter evals 'max-iter))
                 ;; Update all particles
                 (let particle-loop ([rng rng]
                                     [old-swarm swarm]
                                     [new-swarm '()]
                                     [new-gbest gbest]
                                     [new-gbest-f gbest-f])
                      (if (null? old-swarm)
                          (iter-loop rng (reverse new-swarm) new-gbest new-gbest-f
                                     (+ iter 1) (+ evals swarm-size))
                          (let* ([p (car old-swarm)]
                                 ;; Update velocity and position
                                 [update-result (pso-update-particle rng p gbest w c1 c2 lower upper v-max)]
                                 [rng2 (car update-result)]
                                 [p-moved (cdr update-result)]
                                 ;; Evaluate new position
                                 [x-new (particle-x p-moved)]
                                 [fx-new (f x-new)]
                                 ;; Update personal best
                                 [p-updated (if (< fx-new (particle-pbest-f p-moved))
                                                (make-particle x-new (particle-v p-moved) x-new fx-new)
                                                (make-particle x-new (particle-v p-moved)
                                                               (particle-pbest p-moved)
                                                               (particle-pbest-f p-moved)))]
                                 ;; Update global best
                                 [upd-gbest (if (< fx-new new-gbest-f) x-new new-gbest)]
                                 [upd-gbest-f (if (< fx-new new-gbest-f) fx-new new-gbest-f)])
                                (particle-loop rng2 (cdr old-swarm) (cons p-updated new-swarm)
                                               upd-gbest upd-gbest-f))))))))

(define (pso-minimize f lower upper rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) PRNG (Pair PRNG MetaResult)))
  (doc 'description "Particle swarm with default parameters")
  (let* ([n (length lower)]
         [swarm-size (max 20 (* 5 n))]
         [max-iter (* 200 n)])
        (particle-swarm f lower upper swarm-size max-iter 0.729 1.49 1.49 rng)))

;;; ============================================================================
;;; Multi-Objective Optimization (Pareto)
;;; ============================================================================

(doc 'section 'multi-objective)

(doc 'note "Multi-objective optimization finds the Pareto frontier:
the set of solutions where no objective can be improved without
worsening another.

A solution x dominates y if:
  - x is no worse than y in all objectives
  - x is strictly better than y in at least one objective")

(define (pareto-dominates? x-obj y-obj)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (List Number) Bool))
  (doc 'description "Does x dominate y? (all objectives minimized)")
  (and (for-all (lambda (pair) (<= (car pair) (cdr pair)))
                (map cons x-obj y-obj))  ; x no worse in all
       (exists (lambda (pair) (< (car pair) (cdr pair)))
               (map cons x-obj y-obj)))) ; x strictly better in at least one

(define (pareto-nondominated population objectives)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number)) (List (List Number)) (List (List Number))))
  (doc 'description "Filter population to only non-dominated solutions (Pareto front)")
  (doc 'param 'population "List of solutions")
  (doc 'param 'objectives "Corresponding objective values for each solution")
  (let loop ([pop population]
             [obj objectives]
             [result '()])
       (if (null? pop)
           result
           (let* ([x (car pop)]
                  [x-obj (car obj)]
                  ;; Check if x is dominated by any other
                  [dominated? (exists (lambda (y-obj) (pareto-dominates? y-obj x-obj))
                                      (append (cdr obj) (map cadr result)))])
                 (if dominated?
                     (loop (cdr pop) (cdr obj) result)
                     (loop (cdr pop) (cdr obj) (cons (list x x-obj) result)))))))

(define (nsga2-crowding-distance front)
  (doc 'type '(-> (List (Pair (List Number) (List Number))) (List Number)))
  (doc 'description "Calculate NSGA-II crowding distance for diversity preservation")
  (let* ([n (length front)]
         [m (if (null? front) 0 (length (cadar front)))]  ; number of objectives
         [distances (make-list n 0)])
        (if (< n 2)
            distances
            ;; For each objective, sort and assign boundary points infinite distance
            (let obj-loop ([obj-idx 0]
                           [dists distances])
                 (if (>= obj-idx m)
                     dists
                     ;; Sort by this objective
                     ;; Each element is ((solution objectives) distance)
                     ;; To get objectives: (cadr (car elem)) = (cadr (car ((sol objs) dist))) = objs
                     (let* ([sorted (merge-sort-by (lambda (a b)
                                                       (< (list-ref (cadr (car a)) obj-idx)
                                                          (list-ref (cadr (car b)) obj-idx)))
                                               (map (lambda (pt d) (list pt d))
                                                    front dists))]
                            [obj-min (list-ref (cadr (car (car sorted))) obj-idx)]
                            [obj-max (list-ref (cadr (car (last sorted))) obj-idx)]
                            [range (if (< (abs (- obj-max obj-min)) 1e-10)
                                       1
                                       (- obj-max obj-min))])
                           ;; Update distances (boundary = infinity, others = normalized)
                           (let inner-loop ([i 0]
                                            [s sorted]
                                            [new-dists '()])
                                (if (null? s)
                                    (obj-loop (+ obj-idx 1) (reverse new-dists))
                                    (let ([old-d (cadr (car s))])
                                         (cond
                                          [(= i 0) (inner-loop 1 (cdr s) (cons +inf.0 new-dists))]
                                          [(= i (- n 1)) (inner-loop (+ i 1) (cdr s) (cons +inf.0 new-dists))]
                                          [else
                                           (let* ([prev (list-ref (cadr (car (list-ref sorted (- i 1)))) obj-idx)]
                                                  [next (list-ref (cadr (car (list-ref sorted (+ i 1)))) obj-idx)]
                                                  [contrib (/ (- next prev) range)])
                                                 (inner-loop (+ i 1) (cdr s)
                                                             (cons (+ old-d contrib) new-dists)))]))))))))))

;;; ============================================================================
;;; Differential Evolution
;;; ============================================================================

(doc 'section 'differential-evolution)

(doc 'note "Differential Evolution (Storn & Price, 1997)

Simple and effective for continuous optimization:
  - Mutation: v = x_r1 + F * (x_r2 - x_r3) (using random population members)
  - Crossover: mix v with target x using crossover rate CR
  - Selection: keep child only if better than parent")

(define (de-mutate rng population target-idx f-param lower upper)
  (doc 'type '(-> PRNG (Vector (List Number)) Nat Number (List Number) (List Number) (Pair PRNG (List Number))))
  (doc 'description "DE/rand/1 mutation: v = x_r1 + F*(x_r2 - x_r3)")
  (let* ([n (vector-length population)]
         ;; Select 3 distinct random indices != target-idx
         [select-distinct (lambda (rng exclude)
                                  (let loop ([rng rng])
                                       (let* ([result (run-state (random-int-range 0 (- n 1)) rng)]
                                              [idx (car result)]
                                              [rng2 (cdr result)])
                                             (if (member idx exclude)
                                                 (loop rng2)
                                                 (cons rng2 idx)))))]
         [r1-res (select-distinct rng (list target-idx))]
         [rng2 (car r1-res)]
         [r1 (cdr r1-res)]
         [r2-res (select-distinct rng2 (list target-idx r1))]
         [rng3 (car r2-res)]
         [r2 (cdr r2-res)]
         [r3-res (select-distinct rng3 (list target-idx r1 r2))]
         [rng4 (car r3-res)]
         [r3 (cdr r3-res)]
         ;; v = x_r1 + F * (x_r2 - x_r3)
         [x1 (vector-ref population r1)]
         [x2 (vector-ref population r2)]
         [x3 (vector-ref population r3)]
         [diff (point-sub x2 x3)]
         [v (point-add x1 (point-scale f-param diff))]
         ;; Clip to bounds
         [v-clipped (clip-to-bounds v lower upper)])
        (cons rng4 v-clipped)))

(define (de-crossover rng target mutant cr)
  (doc 'type '(-> PRNG (List Number) (List Number) Number (Pair PRNG (List Number))))
  (doc 'description "Binomial crossover")
  (let* ([n (length target)]
         ;; Ensure at least one component from mutant
         [j-rand-res (run-state (random-int-range 0 (- n 1)) rng)]
         [j-rand (car j-rand-res)]
         [rng2 (cdr j-rand-res)])
        (let loop ([rng rng2]
                   [xi target]
                   [vi mutant]
                   [j 0]
                   [acc '()])
             (if (null? xi)
                 (cons rng (reverse acc))
                 (let* ([rand-res (run-state random-float rng)]
                        [rand-val (car rand-res)]
                        [rng3 (cdr rand-res)]
                        [use-mutant? (or (< rand-val cr) (= j j-rand))]
                        [val (if use-mutant? (car vi) (car xi))])
                       (loop rng3 (cdr xi) (cdr vi) (+ j 1) (cons val acc)))))))

(define (differential-evolution f lower upper pop-size max-gen f-param cr rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) Nat Nat Number Number PRNG
                  (Pair PRNG MetaResult)))
  (doc 'description "Differential evolution optimization")
  (doc 'param 'f "Objective function to minimize")
  (doc 'param 'lower "Lower bounds")
  (doc 'param 'upper "Upper bounds")
  (doc 'param 'pop-size "Population size (must be >= 4, typically 10*dimension)")
  (doc 'param 'max-gen "Maximum generations")
  (doc 'param 'f-param "Mutation scale factor F (typically 0.5-1.0)")
  (doc 'param 'cr "Crossover probability (typically 0.7-0.9)")
  (doc 'param 'rng "Random number generator")
  ;; DE mutation requires 4 distinct indices (target + 3 donors)
  (when (< pop-size 4)
        (error 'differential-evolution "pop-size must be >= 4" pop-size))
  (let* ([n (length lower)]
         [pop-result (ga-initialize-population rng n pop-size lower upper)]
         [rng1 (car pop-result)]
         [population (cdr pop-result)]
         [fitness (ga-evaluate-population f population)]
         [init-best (ga-find-best population fitness)])
        (let gen-loop ([rng rng1]
                       [pop population]
                       [fit fitness]
                       [best-x (car init-best)]
                       [best-f (cdr init-best)]
                       [gen 0]
                       [evals pop-size])
             (if (>= gen max-gen)
                 (cons rng (make-meta-result best-x best-f gen evals 'max-generations))
                 ;; Process each individual
                 (let ind-loop ([rng rng]
                                [i 0]
                                [new-pop (make-vector pop-size #f)]
                                [new-fit (make-vector pop-size 0)]
                                [gen-best-x best-x]
                                [gen-best-f best-f]
                                [gen-evals 0])
                      (if (>= i pop-size)
                          (gen-loop rng new-pop new-fit gen-best-x gen-best-f
                                    (+ gen 1) (+ evals gen-evals))
                          (let* ([target (vector-ref pop i)]
                                 [target-f (vector-ref fit i)]
                                 ;; Mutation
                                 [mutant-res (de-mutate rng pop i f-param lower upper)]
                                 [rng2 (car mutant-res)]
                                 [mutant (cdr mutant-res)]
                                 ;; Crossover
                                 [trial-res (de-crossover rng2 target mutant cr)]
                                 [rng3 (car trial-res)]
                                 [trial (cdr trial-res)]
                                 ;; Evaluation
                                 [trial-f (f trial)]
                                 ;; Selection
                                 [winner (if (<= trial-f target-f) trial target)]
                                 [winner-f (if (<= trial-f target-f) trial-f target-f)]
                                 ;; Update best
                                 [upd-best-x (if (< winner-f gen-best-f) winner gen-best-x)]
                                 [upd-best-f (if (< winner-f gen-best-f) winner-f gen-best-f)])
                                (begin
                                  (vector-set! new-pop i winner)
                                  (vector-set! new-fit i winner-f)
                                  (ind-loop rng3 (+ i 1) new-pop new-fit
                                            upd-best-x upd-best-f (+ gen-evals 1))))))))))

(define (de-minimize f lower upper rng)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) PRNG (Pair PRNG MetaResult)))
  (doc 'description "Differential evolution with default parameters")
  (let* ([n (length lower)]
         [pop-size (max 20 (* 10 n))]
         [max-gen (* 100 n)])
        (differential-evolution f lower upper pop-size max-gen 0.8 0.9 rng)))

;;; ============================================================================
;;; Unified Interface
;;; ============================================================================

(doc 'section 'unified-interface)

(define (global-minimize f lower upper rng . opts)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List Number) (List Number) PRNG (Pair PRNG MetaResult)))
  (doc 'description "Global minimization via metaheuristics")
  (doc 'param 'f "Objective function (black-box)")
  (doc 'param 'lower "Lower bounds")
  (doc 'param 'upper "Upper bounds")
  (doc 'param 'rng "Random number generator")
  (doc 'param 'method "Optional: 'sa, 'ga, 'pso, 'de (default: 'de)")
  (let ([method (if (and (pair? opts) (symbol? (car opts)))
                    (car opts)
                    'de)])
       (case method
             [(sa simulated-annealing)
              (sa-minimize f (map (lambda (lo hi) (/ (+ lo hi) 2)) lower upper)
                           lower upper rng)]
             [(ga genetic-algorithm)
              (ga-minimize f lower upper rng)]
             [(pso particle-swarm)
              (pso-minimize f lower upper rng)]
             [(de differential-evolution)
              (de-minimize f lower upper rng)]
             [else
              (de-minimize f lower upper rng)])))
