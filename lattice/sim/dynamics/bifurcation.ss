(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/numeric/complex.ss")
(load "lattice/sim/dynamics/ode-system.ss")
(load "lattice/sim/dynamics/stability.ss")
(load "lattice/sim/dynamics/chaos.ss")

(doc 'module 'bifurcation)
(doc 'description "Bifurcation analysis for dynamical systems: parameter continuation, bifurcation detection (saddle-node, transcritical, pitchfork, Hopf, period-doubling), bifurcation diagrams, and normal form computation")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "Parameter continuation, bifurcation point detection, stability monitoring, bifurcation diagrams, Hopf normal form")

;;; ============================================================
;;; Section: Utilities
;;; ============================================================

(doc 'section 'utilities)

(define (filter-map f lst)
  (doc 'type '(-> (-> α (Option β)) (List α) (List β)))
  (doc 'description "Map f over lst, keeping only non-#f results")
  (let loop ([xs lst] [acc '()])
       (if (null? xs)
           (reverse acc)
           (let ([result (f (car xs))])
                (loop (cdr xs)
                      (if result (cons result acc) acc))))))

(define (any pred lst)
  (doc 'type '(-> (-> α Boolean) (List α) Boolean))
  (doc 'description "Return #t if pred is true for any element")
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

(define (every pred lst)
  (doc 'type '(-> (-> α Boolean) (List α) Boolean))
  (doc 'description "Return #t if pred is true for every element")
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (every pred (cdr lst))]))

;;; ============================================================
;;; Section: 1D Stability Analysis Extensions
;;; ============================================================

(doc 'section '1d-stability)
(doc 'note "Extend stability analysis to handle 1D systems")

(define (analyze-stability-1d sys equilibrium step-size)
  (doc 'type '(-> ODE Vec Number (Pair Symbol (List Complex))))
  (doc 'description "Analyze stability of a 1D system")
  (let* ([jac (compute-jacobian sys equilibrium step-size)]
         [eigenvalue (vector-ref (matrix-data jac) 0)]  ; Single entry in 1x1 matrix
         [complex-eig (make-complex eigenvalue 0)])
        (cons (cond
               [(< eigenvalue -1e-10) 'stable-node]
               [(> eigenvalue 1e-10) 'unstable-node]
               [else 'degenerate])
              (list complex-eig))))

(define (analyze-stability-general sys equilibrium step-size)
  (doc 'type '(-> ODE Vec Number (Pair Symbol (List Complex))))
  (doc 'description "Analyze stability handling both 1D and 2D systems")
  (let ([dim (ode-dimension sys)])
       (cond
        [(= dim 1) (analyze-stability-1d sys equilibrium step-size)]
        [(= dim 2) (analyze-stability sys equilibrium step-size)]
        [else (analyze-stability-nd sys equilibrium step-size)])))

;;; ============================================================
;;; Section: Parameterized ODE Systems
;;; ============================================================

(doc 'section 'parameterized-ode-systems)
(doc 'note "A parameterized ODE is a function from parameter value to ODE system: p -> (dx/dt = f(x; p))")

(define (make-parameterized-ode make-sys)
  (doc 'export #t)
  (doc 'type '(-> (-> Number ODE) ParamODE))
  (doc 'description "Create a parameterized ODE system from a constructor function")
  (doc 'param 'make-sys "function that takes a parameter value and returns an ODE system")
  (list 'param-ode make-sys))

(define (param-ode? x)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'param-ode)))

(define (param-ode-maker psys)
  (doc 'type '(-> ParamODE (-> Number ODE)))
  (cadr psys))

(define (instantiate-at psys param)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number ODE))
  (doc 'description "Instantiate a parameterized ODE at a specific parameter value")
  ((param-ode-maker psys) param))

;;; ============================================================
;;; Section: Fixed Point Continuation
;;; ============================================================

(doc 'section 'fixed-point-continuation)
(doc 'note "Track fixed points as parameters vary using predictor-corrector continuation")

(define *continuation-tolerance* 1e-8)
(define *continuation-step-size* 1e-6)
(define *continuation-max-newton* 20)

;; Use robust Newton solver by default (handles singular Jacobians near bifurcations)
(define *use-robust-newton* #t)

(define (find-fixed-point-solver sys initial-guess tolerance step-size max-iter)
  (doc 'type '(-> ODE Vec Number Number Nat (Option Vec)))
  (doc 'description "Find fixed point using configured solver (robust or classic Newton)")
  (if *use-robust-newton*
      (find-fixed-point-robust sys initial-guess tolerance step-size max-iter)
      (find-fixed-point-solver sys initial-guess tolerance step-size max-iter)))

(define (continue-fixed-point psys param0 fp0 param-end param-step)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Number Number (List (List Number Vec Symbol (List Complex)))))
  (doc 'description "Continue a fixed point branch as parameter varies")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param0 "starting parameter value")
  (doc 'param 'fp0 "starting fixed point")
  (doc 'param 'param-end "ending parameter value")
  (doc 'param 'param-step "parameter step size (positive or negative)")
  (doc 'returns "list of (param fixed-point stability eigenvalues) tuples along the branch")
  (let ([direction (if (> param-end param0) 1 -1)]
        [step (abs param-step)])
       (let loop ([param param0]
                  [fp fp0]
                  [results '()])
            (if (if (> direction 0)
                    (> param param-end)
                    (< param param-end))
                (reverse results)
                ;; Analyze stability at current point
                (let* ([sys (instantiate-at psys param)]
                       [analysis (analyze-stability-general sys fp *continuation-step-size*)]
                       [stability (car analysis)]
                       [eigenvalues (cdr analysis)]
                       [entry (list param fp stability eigenvalues)]
                       ;; Predict next fixed point (use current as initial guess)
                       [next-param (+ param (* direction step))]
                       [next-sys (instantiate-at psys next-param)]
                       ;; Correct using Newton's method
                       [next-fp (find-fixed-point-newton next-sys fp
                                                         *continuation-tolerance*
                                                         *continuation-step-size*
                                                         *continuation-max-newton*)])
                      (if next-fp
                          (loop next-param next-fp (cons entry results))
                          ;; Continuation failed - return what we have
                          (reverse (cons entry results))))))))

(define (find-all-fixed-points sys search-region grid-density tolerance)
  (doc 'export #t)
  (doc 'type '(-> ODE (List (Pair Number Number)) Nat Number (List Vec)))
  (doc 'description "Find all fixed points in a search region using grid search + Newton refinement")
  (doc 'param 'search-region "list of (min . max) pairs for each dimension")
  (doc 'param 'grid-density "number of grid points per dimension")
  (let* ([dim (length search-region)]
         [grid (if (= dim 2)
                   (let ([xr (car search-region)]
                         [yr (cadr search-region)])
                        (make-phase-space-grid (car xr) (cdr xr) grid-density
                                               (car yr) (cdr yr) grid-density))
                   ;; For other dimensions, use recursive grid generation
                   (generate-nd-grid search-region grid-density))]
         ;; Refine each candidate
         [candidates (filter-map
                      (lambda (pt)
                              (find-fixed-point-solver sys pt tolerance
                                                       *continuation-step-size*
                                                       *continuation-max-newton*))
                      grid)]
         ;; Remove duplicates (points within tolerance of each other)
         [unique (remove-duplicate-points candidates tolerance)])
        unique))

(define (generate-nd-grid ranges density)
  (doc 'type '(-> (List (Pair Number Number)) Nat (List Vec)))
  (doc 'description "Generate a grid in n-dimensional space")
  (if (null? ranges)
      '(#())  ; Single point in 0D space
      (let* ([range (car ranges)]
             [rest-grid (generate-nd-grid (cdr ranges) density)]
             [step (/ (- (cdr range) (car range)) (max 1 (- density 1)))])
            (apply append
                   (map (lambda (i)
                                (let ([val (+ (car range) (* i step))])
                                     (map (lambda (pt)
                                                  (list->vector (cons val (vector->list pt))))
                                          rest-grid)))
                        (iota density))))))

(define (remove-duplicate-points points tolerance)
  (doc 'type '(-> (List Vec) Number (List Vec)))
  (doc 'description "Remove points that are within tolerance of each other")
  (let loop ([pts points] [unique '()])
       (if (null? pts)
           (reverse unique)
           (let ([pt (car pts)])
                (if (any (lambda (u) (< (vec-norm (vec-sub pt u)) tolerance))
                         unique)
                    (loop (cdr pts) unique)
                    (loop (cdr pts) (cons pt unique)))))))

;;; ============================================================
;;; Section: Bifurcation Detection
;;; ============================================================

(doc 'section 'bifurcation-detection)
(doc 'note "Detect bifurcations by monitoring eigenvalue crossings")

(define (detect-bifurcations continuation-data)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number Vec Symbol (List Complex))) (List (List Symbol Number Vec))))
  (doc 'description "Detect bifurcation points from continuation data by analyzing eigenvalue transitions")
  (doc 'returns "list of (bifurcation-type param fixed-point) for each detected bifurcation")
  (if (or (null? continuation-data) (null? (cdr continuation-data)))
      '()
      (let loop ([prev (car continuation-data)]
                 [rest (cdr continuation-data)]
                 [bifurcations '()])
           (if (null? rest)
               (reverse bifurcations)
               (let* ([curr (car rest)]
                      [prev-param (car prev)]
                      [curr-param (car curr)]
                      [prev-eigs (cadddr prev)]
                      [curr-eigs (cadddr curr)]
                      [prev-stab (caddr prev)]
                      [curr-stab (caddr curr)]
                      [curr-fp (cadr curr)]
                      ;; Check for bifurcations
                      [bif (detect-bifurcation-type prev-eigs curr-eigs
                                                    prev-stab curr-stab)])
                     (loop curr (cdr rest)
                           (if bif
                               (cons (list bif curr-param curr-fp) bifurcations)
                               bifurcations)))))))

(define (detect-bifurcation-type prev-eigs curr-eigs prev-stab curr-stab)
  (doc 'type '(-> (List Complex) (List Complex) Symbol Symbol (Option Symbol)))
  (doc 'description "Determine bifurcation type from eigenvalue transition")
  (let* ([prev-reals (map complex-real prev-eigs)]
         [curr-reals (map complex-real curr-eigs)]
         [prev-imags (map complex-imag prev-eigs)]
         [curr-imags (map complex-imag curr-eigs)])
        (cond
         ;; Saddle-node: real eigenvalue crosses zero
         [(saddle-node-condition? prev-reals curr-reals prev-imags)
          'saddle-node]
         ;; Hopf: complex conjugate pair crosses imaginary axis
         [(hopf-condition? prev-reals curr-reals prev-imags curr-imags)
          'hopf]
         ;; Pitchfork/Transcritical: stability change with real eigenvalue
         [(and (stability-changed? prev-stab curr-stab)
               (real-eigenvalue-zero-crossing? prev-reals curr-reals prev-imags))
          (if (symmetric-breaking? prev-reals curr-reals)
              'pitchfork
              'transcritical)]
         [else #f])))

(define (saddle-node-condition? prev-reals curr-reals prev-imags)
  (doc 'type '(-> (List Number) (List Number) (List Number) Boolean))
  (doc 'description "Check for saddle-node: real eigenvalue crosses zero")
  ;; At least one eigenvalue should be real (small imaginary part)
  ;; and cross zero
  (let ([tolerance 1e-6])
       (any (lambda (i)
                    (let ([pr (list-ref prev-reals i)]
                          [cr (list-ref curr-reals i)]
                          [im (list-ref prev-imags i)])
                         (and (< (abs im) tolerance)  ; Real eigenvalue
                              (< (* pr cr) 0))))      ; Sign change
            (iota (length prev-reals)))))

(define (hopf-condition? prev-reals curr-reals prev-imags curr-imags)
  (doc 'type '(-> (List Number) (List Number) (List Number) (List Number) Boolean))
  (doc 'description "Check for Hopf: complex conjugate pair crosses imaginary axis")
  (let ([tolerance 1e-6])
       ;; Look for a pair with nonzero imaginary part where real part changes sign
       (any (lambda (i)
                    (let ([pr (list-ref prev-reals i)]
                          [cr (list-ref curr-reals i)]
                          [pim (list-ref prev-imags i)]
                          [cim (list-ref curr-imags i)])
                         (and (> (abs pim) tolerance)  ; Complex eigenvalue
                              (> (abs cim) tolerance)
                              (< (* pr cr) 0))))       ; Real part sign change
            (iota (length prev-reals)))))

(define (stability-changed? prev-stab curr-stab)
  (doc 'type '(-> Symbol Symbol Boolean))
  (not (eq? prev-stab curr-stab)))

(define (real-eigenvalue-zero-crossing? prev-reals curr-reals prev-imags)
  (doc 'type '(-> (List Number) (List Number) (List Number) Boolean))
  (let ([tolerance 1e-6])
       (any (lambda (i)
                    (let ([pr (list-ref prev-reals i)]
                          [cr (list-ref curr-reals i)]
                          [im (list-ref prev-imags i)])
                         (and (< (abs im) tolerance)
                              (< (* pr cr) 0))))
            (iota (length prev-reals)))))

(define (symmetric-breaking? prev-reals curr-reals)
  (doc 'type '(-> (List Number) (List Number) Boolean))
  (doc 'description "Heuristic for pitchfork vs transcritical - check for symmetric eigenvalue structure")
  ;; Simplified: pitchfork often has eigenvalues that are symmetric around zero
  ;; This is a heuristic - proper detection needs normal form analysis
  (let ([prev-sum (apply + prev-reals)]
        [curr-sum (apply + curr-reals)])
       (< (abs prev-sum) 0.1)))  ; Roughly symmetric

;;; ============================================================
;;; Section: Bifurcation Diagrams
;;; ============================================================

(doc 'section 'bifurcation-diagrams)
(doc 'note "Generate data for bifurcation diagrams of continuous systems")

(define (bifurcation-diagram-equilibria psys param-min param-max param-steps
                                        search-region grid-density tolerance)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Nat (List (Pair Number Number)) Nat Number
                  (List (List Number Vec Symbol))))
  (doc 'description "Generate bifurcation diagram data showing equilibria vs parameter")
  (doc 'param 'search-region "(min . max) pairs for each state dimension")
  (doc 'returns "list of (param fixed-point stability) tuples")
  (let ([step (/ (- param-max param-min) (max 1 (- param-steps 1)))])
       (apply append
              (map (lambda (i)
                           (let* ([param (+ param-min (* i step))]
                                  [sys (instantiate-at psys param)]
                                  [fps (find-all-fixed-points sys search-region
                                                              grid-density tolerance)])
                                 (map (lambda (fp)
                                              (let* ([analysis (analyze-stability-general sys fp
                                                                                  *continuation-step-size*)]
                                                     [stability (car analysis)])
                                                    (list param fp stability)))
                                      fps)))
                   (iota param-steps)))))

(define (bifurcation-diagram-amplitude psys param-min param-max param-steps
                                       initial-state dt n-transient n-sample)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Nat Vec Number Nat Nat
                  (List (Pair Number Number))))
  (doc 'description "Generate bifurcation diagram showing attractor amplitude vs parameter")
  (doc 'note "For limit cycles, this shows the oscillation amplitude. For chaos, it shows the spread")
  (doc 'returns "list of (param, state-component) pairs for plotting")
  (let ([step (/ (- param-max param-min) (max 1 (- param-steps 1)))])
       (apply append
              (map (lambda (i)
                           (let* ([param (+ param-min (* i step))]
                                  [sys (instantiate-at psys param)]
                                  ;; Skip transient
                                  [settled (let loop ([t 0] [state initial-state] [k 0])
                                                (if (>= k n-transient)
                                                    state
                                                    (loop (+ t dt)
                                                          (rk4-step sys t state dt)
                                                          (+ k 1))))]
                                  ;; Collect samples
                                  [samples (let loop ([t 0] [state settled] [k 0] [acc '()])
                                                (if (>= k n-sample)
                                                    (reverse acc)
                                                    (loop (+ t dt)
                                                          (rk4-step sys t state dt)
                                                          (+ k 1)
                                                          (cons state acc))))])
                                 ;; Return first component of each sample
                                 (map (lambda (s) (cons param (vector-ref s 0)))
                                      samples)))
                   (iota param-steps)))))

(define (bifurcation-diagram-poincare psys param-min param-max param-steps
                                      initial-state dt n-transient n-crossings
                                      plane-axis plane-value direction)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Nat Vec Number Nat Nat Nat Number Symbol
                  (List (Pair Number Number))))
  (doc 'description "Generate bifurcation diagram using Poincare section crossings")
  (doc 'note "Shows period-doubling bifurcations clearly for limit cycles")
  (let ([step (/ (- param-max param-min) (max 1 (- param-steps 1)))])
       (apply append
              (map (lambda (i)
                           (let* ([param (+ param-min (* i step))]
                                  [sys (instantiate-at psys param)]
                                  [section (poincare-section sys initial-state dt
                                                            (+ n-transient (* n-crossings 100))
                                                            n-transient
                                                            plane-axis plane-value direction)]
                                  ;; Project to a coordinate for plotting
                                  [coord-idx (if (= plane-axis 0) 1 0)])
                                 (map (lambda (pt)
                                              (cons param (vector-ref pt coord-idx)))
                                      (take-up-to section n-crossings))))
                   (iota param-steps)))))

(define (take-up-to lst n)
  (doc 'type '(-> (List α) Nat (List α)))
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-up-to (cdr lst) (- n 1)))))

;;; ============================================================
;;; Section: Hopf Bifurcation Analysis
;;; ============================================================

(doc 'section 'hopf-bifurcation)
(doc 'note "Detailed analysis of Hopf bifurcations")

(define (find-hopf-bifurcation psys param-min param-max fp-guess tolerance)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Vec Number (Option (List Number Vec Number))))
  (doc 'description "Find the parameter value where a Hopf bifurcation occurs")
  (doc 'returns "(param fixed-point frequency) or #f if not found")
  ;; Binary search for the bifurcation point
  (let loop ([lo param-min] [hi param-max] [iter 0])
       (if (or (> iter 50) (< (- hi lo) tolerance))
           ;; Converged or max iterations
           (let* ([param (/ (+ lo hi) 2)]
                  [sys (instantiate-at psys param)]
                  [fp (find-fixed-point-solver sys fp-guess tolerance
                                               *continuation-step-size*
                                               *continuation-max-newton*)])
                 (if fp
                     (let* ([analysis (analyze-stability-general sys fp *continuation-step-size*)]
                            [eigs (cdr analysis)]
                            [freq (hopf-frequency eigs)])
                           (list param fp freq))
                     #f))
           ;; Check stability at midpoint
           (let* ([mid (/ (+ lo hi) 2)]
                  [sys (instantiate-at psys mid)]
                  [fp (find-fixed-point-solver sys fp-guess tolerance
                                               *continuation-step-size*
                                               *continuation-max-newton*)])
                 (if (not fp)
                     #f  ; Lost the fixed point
                     (let* ([analysis (analyze-stability-general sys fp *continuation-step-size*)]
                            [stab (car analysis)]
                            [eigs (cdr analysis)]
                            [has-complex (any (lambda (e) (> (abs (complex-imag e)) 1e-6))
                                              eigs)]
                            [max-real (apply max (map complex-real eigs))])
                           (cond
                            ;; If we have complex eigenvalues, check real part sign
                            [(and has-complex (> max-real 0))
                             (loop lo mid (+ iter 1))]  ; Unstable - search lower
                            [(and has-complex (< max-real 0))
                             (loop mid hi (+ iter 1))]  ; Stable - search higher
                            [else
                             ;; No complex eigenvalues - not a Hopf
                             #f])))))))

(define (hopf-frequency eigenvalues)
  (doc 'type '(-> (List Complex) Number))
  (doc 'description "Extract the frequency at Hopf bifurcation from eigenvalues")
  (let ([complex-eigs (filter (lambda (e) (> (abs (complex-imag e)) 1e-6))
                              eigenvalues)])
       (if (null? complex-eigs)
           0
           (abs (complex-imag (car complex-eigs))))))

(define (hopf-criticality psys param fp h)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Number Symbol))
  (doc 'description "Determine if Hopf bifurcation is supercritical or subcritical")
  (doc 'note "Supercritical: stable limit cycle emerges. Subcritical: unstable limit cycle")
  (doc 'returns "'supercritical, 'subcritical, or 'unknown")
  ;; Compute first Lyapunov coefficient (simplified approach)
  ;; Check amplitude growth rate just past bifurcation
  (let* ([sys-before (instantiate-at psys (- param h))]
         [sys-after (instantiate-at psys (+ param h))]
         [stab-before (car (analyze-stability-general sys-before fp *continuation-step-size*))]
         [stab-after (car (analyze-stability-general sys-after fp *continuation-step-size*))])
        ;; If stable before, unstable after, and limit cycle forms -> supercritical
        ;; This is a simplified heuristic
        (cond
         [(and (memq stab-before '(stable-node stable-spiral))
               (memq stab-after '(unstable-node unstable-spiral)))
          'supercritical]
         [(and (memq stab-before '(unstable-node unstable-spiral))
               (memq stab-after '(stable-node stable-spiral)))
          'subcritical]
         [else 'unknown])))

;;; ============================================================
;;; Section: Period-Doubling Analysis
;;; ============================================================

(doc 'section 'period-doubling)
(doc 'note "Detect period-doubling cascades (route to chaos)")

(define (detect-period-doubling psys param-min param-max param-steps
                                initial-state dt n-transient plane-axis plane-value)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Nat Vec Number Nat Nat Number
                  (List (Pair Number Nat))))
  (doc 'description "Detect period-doubling bifurcations by counting Poincare section returns")
  (doc 'returns "list of (param, detected-period) pairs")
  (let ([step (/ (- param-max param-min) (max 1 (- param-steps 1)))])
       (map (lambda (i)
                    (let* ([param (+ param-min (* i step))]
                           [sys (instantiate-at psys param)]
                           [section (poincare-section sys initial-state dt
                                                      (+ n-transient 2000)
                                                      n-transient
                                                      plane-axis plane-value 'positive)]
                           [period (estimate-period-from-section section 1e-3)])
                          (cons param period)))
            (iota param-steps))))

(define (estimate-period-from-section section-points tolerance)
  (doc 'type '(-> (List Vec) Number Nat))
  (doc 'description "Estimate period from Poincare section by finding return time")
  (if (< (length section-points) 2)
      1
      (let* ([first-pt (car section-points)]
             [rest (cdr section-points)])
            ;; Count how many points until we return close to the first
            (let loop ([pts rest] [count 1])
                 (cond
                  [(null? pts) count]  ; Didn't find return - use count
                  [(< (vec-norm (vec-sub (car pts) first-pt)) tolerance)
                   count]              ; Found return
                  [(> count 64) count] ; Cap at 64 (enough for period-doubling cascades)
                  [else (loop (cdr pts) (+ count 1))])))))

(define (feigenbaum-delta cascade-params)
  (doc 'export #t)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Estimate Feigenbaum delta constant from period-doubling cascade")
  (doc 'param 'cascade-params "list of bifurcation parameter values: r1, r2, r4, r8, ...")
  (doc 'note "Feigenbaum delta is approx 4.669... for maps with quadratic maximum")
  (if (< (length cascade-params) 3)
      0
      (let loop ([params cascade-params] [deltas '()])
           (if (< (length params) 3)
               (if (null? deltas)
                   0
                   (/ (apply + deltas) (length deltas)))  ; Average
               (let* ([r1 (car params)]
                      [r2 (cadr params)]
                      [r3 (caddr params)]
                      [delta (/ (- r2 r1) (- r3 r2))])
                     (loop (cdr params) (cons delta deltas)))))))

;;; ============================================================
;;; Section: Saddle-Node (Fold) Bifurcation
;;; ============================================================

(doc 'section 'saddle-node-bifurcation)

(define (find-saddle-node-bifurcation psys param-min param-max fp-guess tolerance)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Vec Number (Option (List Number Vec))))
  (doc 'description "Find parameter value where saddle-node (fold) bifurcation occurs")
  (doc 'note "At saddle-node, a real eigenvalue passes through zero")
  ;; Binary search where determinant of Jacobian changes sign
  (let loop ([lo param-min] [hi param-max] [iter 0])
       (if (or (> iter 50) (< (- hi lo) tolerance))
           (let* ([param (/ (+ lo hi) 2)]
                  [sys (instantiate-at psys param)]
                  [fp (find-fixed-point-solver sys fp-guess tolerance
                                               *continuation-step-size*
                                               *continuation-max-newton*)])
                 (if fp (list param fp) #f))
           (let* ([mid (/ (+ lo hi) 2)]
                  [sys (instantiate-at psys mid)]
                  [fp (find-fixed-point-solver sys fp-guess tolerance
                                               *continuation-step-size*
                                               *continuation-max-newton*)])
                 (if (not fp)
                     ;; Lost fixed point - bifurcation is nearby
                     (loop lo mid (+ iter 1))
                     (let* ([jac (compute-jacobian sys fp *continuation-step-size*)]
                            [det (matrix-determinant-2d jac)])
                           (if (> det 0)
                               (loop mid hi (+ iter 1))
                               (loop lo mid (+ iter 1)))))))))

(define (matrix-determinant-2d m)
  (doc 'type '(-> Matrix Number))
  (doc 'description "Compute determinant of 2x2 matrix")
  (let ([a (matrix-ref m 0 0)]
        [b (matrix-ref m 0 1)]
        [c (matrix-ref m 1 0)]
        [d (matrix-ref m 1 1)])
       (- (* a d) (* b c))))

;;; ============================================================
;;; Section: Bifurcation Summary
;;; ============================================================

(doc 'section 'bifurcation-summary)

(define (analyze-bifurcations psys param-min param-max param-steps
                              fp-guess search-region grid-density)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Nat Vec (List (Pair Number Number)) Nat
                  (List (Pair Symbol Any))))
  (doc 'description "Comprehensive bifurcation analysis of a parameterized system")
  (doc 'returns "association list of analysis results")
  (let* ([tolerance 1e-6]
         ;; Find initial fixed point
         [sys0 (instantiate-at psys param-min)]
         [fp0 (or (find-fixed-point-solver sys0 fp-guess tolerance
                                           *continuation-step-size*
                                           *continuation-max-newton*)
                  fp-guess)]
         ;; Continue the branch
         [step (/ (- param-max param-min) param-steps)]
         [continuation (continue-fixed-point psys param-min fp0 param-max step)]
         ;; Detect bifurcations
         [bifurcations (detect-bifurcations continuation)]
         ;; Get equilibrium diagram
         [eq-diagram (bifurcation-diagram-equilibria psys param-min param-max
                                                     param-steps search-region
                                                     grid-density tolerance)])
        (list
         (cons 'parameter-range (cons param-min param-max))
         (cons 'continuation-points (length continuation))
         (cons 'bifurcations bifurcations)
         (cons 'bifurcation-count (length bifurcations))
         (cons 'equilibria-found (length eq-diagram))
         (cons 'stability-changes
               (count-stability-changes continuation)))))

(define (count-stability-changes continuation-data)
  (doc 'type '(-> (List (List Number Vec Symbol (List Complex))) Nat))
  (if (null? continuation-data)
      0
      (let loop ([prev (caddr (car continuation-data))]
                 [rest (cdr continuation-data)]
                 [count 0])
           (if (null? rest)
               count
               (let ([curr (caddr (car rest))])
                    (loop curr (cdr rest)
                          (if (eq? prev curr) count (+ count 1))))))))

;;; ============================================================
;;; Section: Classic Parameterized Systems
;;; ============================================================

(doc 'section 'classic-parameterized-systems)

(doc lorenz-rho 'export #t)
(doc lorenz-rho 'description "Lorenz system parameterized by rho (Rayleigh number)")
(define lorenz-rho
  (make-parameterized-ode
   (lambda (rho) (lorenz-system 10 rho (/ 8 3)))))

(doc van-der-pol-mu 'export #t)
(doc van-der-pol-mu 'description "Van der Pol oscillator parameterized by mu (nonlinearity)")
(define van-der-pol-mu
  (make-parameterized-ode van-der-pol))

(define (pitchfork-normal-form-param)
  (doc 'export #t)
  (doc 'type '(-> ParamODE))
  (doc 'description "Normal form for pitchfork bifurcation: dx/dt = rx - x^3")
  (make-parameterized-ode
   (lambda (r)
           (make-autonomous-ode
            (lambda (state)
                    (let ([x (if (vector? state) (vector-ref state 0) state)])
                         (vector (- (* r x) (* x x x)))))
            1))))

(define (saddle-node-normal-form-param)
  (doc 'export #t)
  (doc 'type '(-> ParamODE))
  (doc 'description "Normal form for saddle-node bifurcation: dx/dt = r + x^2")
  (make-parameterized-ode
   (lambda (r)
           (make-autonomous-ode
            (lambda (state)
                    (let ([x (if (vector? state) (vector-ref state 0) state)])
                         (vector (+ r (* x x)))))
            1))))

(define (transcritical-normal-form-param)
  (doc 'export #t)
  (doc 'type '(-> ParamODE))
  (doc 'description "Normal form for transcritical bifurcation: dx/dt = rx - x^2")
  (make-parameterized-ode
   (lambda (r)
           (make-autonomous-ode
            (lambda (state)
                    (let ([x (if (vector? state) (vector-ref state 0) state)])
                         (vector (- (* r x) (* x x)))))
            1))))

(define (hopf-normal-form-param)
  (doc 'export #t)
  (doc 'type '(-> ParamODE))
  (doc 'description "Normal form for supercritical Hopf bifurcation in polar-like coordinates")
  (doc 'note "dx/dt = rx - y - x(x^2 + y^2), dy/dt = x + ry - y(x^2 + y^2)")
  (make-parameterized-ode
   (lambda (r)
           (make-autonomous-ode
            (lambda (state)
                    (let* ([x (vector-ref state 0)]
                           [y (vector-ref state 1)]
                           [r2 (+ (* x x) (* y y))])
                          (vector (- (- (* r x) y) (* x r2))
                                  (- (+ x (* r y)) (* y r2)))))
            2))))
