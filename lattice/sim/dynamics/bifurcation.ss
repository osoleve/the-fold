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

;;; ============================================================
;;; Section: Arc-Length Continuation
;;; ============================================================

(doc 'section 'arclength-continuation)
(doc 'note "Pseudo-arclength continuation follows branches through fold points by stepping along the solution curve tangent")

(define *arclength-step* 0.1)
(define *arclength-max-correct* 10)

(define (continue-fixed-point-arclength psys param0 fp0 num-steps step-size)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Nat Number (List (List Number Vec Symbol (List Complex)))))
  (doc 'description "Continue a fixed point branch using pseudo-arclength continuation")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param0 "starting parameter value")
  (doc 'param 'fp0 "starting fixed point (must satisfy f(x,p)=0)")
  (doc 'param 'num-steps "number of continuation steps")
  (doc 'param 'step-size "arc-length step size (can be negative for reverse direction)")
  (doc 'returns "list of (param fixed-point stability eigenvalues) tuples along the branch")
  (doc 'note "Unlike fixed-parameter continuation, this can follow branches through fold points where the parameter 'turns back'")
  (let* ([n (vector-length fp0)]
         [ds (abs step-size)]
         [sign (if (>= step-size 0) 1 -1)])
        ;; Compute initial tangent
        (let ([tangent0 (compute-tangent psys param0 fp0)])
             (if (not tangent0)
                 (list (list param0 fp0 'unknown '()))  ; Can't compute tangent
                 (let loop ([k 0]
                            [param param0]
                            [fp fp0]
                            [tangent tangent0]
                            [results '()])
                      (if (>= k num-steps)
                          (reverse results)
                          ;; Record current point with stability analysis
                          (let* ([sys (instantiate-at psys param)]
                                 [analysis (analyze-stability-general sys fp *continuation-step-size*)]
                                 [stability (car analysis)]
                                 [eigenvalues (cdr analysis)]
                                 [entry (list param fp stability eigenvalues)]
                                 ;; Predictor step along tangent
                                 [predicted (arclength-predict fp param tangent (* sign ds))]
                                 [pred-fp (car predicted)]
                                 [pred-param (cdr predicted)]
                                 ;; Corrector: solve bordered system
                                 [corrected (arclength-correct psys pred-fp pred-param tangent (* sign ds))])
                                (if (not corrected)
                                    ;; Correction failed - return what we have
                                    (reverse (cons entry results))
                                    (let* ([new-fp (car corrected)]
                                           [new-param (cdr corrected)]
                                           ;; Compute new tangent for next step
                                           [new-tangent (compute-tangent psys new-param new-fp)])
                                          (if (not new-tangent)
                                              (reverse (cons entry results))
                                              ;; Ensure tangent points in consistent direction
                                              (let ([oriented-tangent (orient-tangent new-tangent tangent)])
                                                   (loop (+ k 1) new-param new-fp oriented-tangent
                                                         (cons entry results)))))))))))))

(define (compute-tangent psys param fp)
  (doc 'type '(-> ParamODE Number Vec (Option (Pair Vec Number))))
  (doc 'description "Compute tangent vector (dx/ds, dp/ds) to the solution curve")
  (doc 'returns "(tangent-x . tangent-p) or #f if computation fails")
  ;; The tangent satisfies: J_x * dx/ds + J_p * dp/ds = 0
  ;; We solve the bordered system to get the tangent direction
  (let* ([sys (instantiate-at psys param)]
         [n (vector-length fp)]
         [jac-x (compute-jacobian sys fp *continuation-step-size*)]
         [jac-p (compute-parameter-jacobian psys param fp *continuation-step-size*)])
        ;; Solve J_x * v = -J_p to get v = dx/dp along the curve
        ;; Then tangent is proportional to (v, 1)
        (let ([v (solve-for-tangent-direction jac-x jac-p)])
             (if (not v)
                 ;; J_x is singular - we're at a fold point
                 ;; Use null vector approach instead
                 (compute-tangent-at-fold jac-x jac-p n)
                 ;; Normalize the tangent
                 (let* ([v-norm-sq (+ (vec-dot v v) 1.0)]
                        [norm (sqrt v-norm-sq)]
                        [tangent-x (vec-scale (/ 1.0 norm) v)]
                        [tangent-p (/ 1.0 norm)])
                       (cons tangent-x tangent-p))))))

(define (compute-parameter-jacobian psys param fp h)
  (doc 'type '(-> ParamODE Number Vec Number Vec))
  (doc 'description "Compute df/dp - how the vector field changes with parameter")
  (let* ([sys0 (instantiate-at psys param)]
         [sys1 (instantiate-at psys (+ param h))]
         [f0 (eval-vector-field sys0 0 fp)]
         [f1 (eval-vector-field sys1 0 fp)])
        (vec-scale (/ 1.0 h) (vec-sub f1 f0))))

(define (solve-for-tangent-direction jac-x jac-p)
  (doc 'type '(-> Matrix Vec (Option Vec)))
  (doc 'description "Solve J_x * v = -J_p for tangent direction")
  ;; Use our robust linear solver
  (let ([neg-jac-p (vec-scale -1.0 jac-p)])
       (solve-linear-system jac-x neg-jac-p)))

(define (compute-tangent-at-fold jac-x jac-p n)
  (doc 'type '(-> Matrix Vec Nat (Option (Pair Vec Number))))
  (doc 'description "Compute tangent when J_x is singular (at fold point)")
  ;; At a fold, the tangent is in the null space of [J_x | J_p]
  ;; Use SVD to find the null vector
  (let* ([augmented (augment-with-column jac-x jac-p)]
         [svd-result (svd augmented)])
        (if (and (pair? svd-result) (eq? (car svd-result) 'error))
            #f
            (let* ([v-matrix (caddr svd-result)]  ; V from SVD
                   [last-col (- (matrix-cols v-matrix) 1)]
                   ;; Last column of V is the null vector
                   [null-vec (extract-column v-matrix last-col)]
                   ;; Split into x and p components
                   [tangent-x (subvector null-vec 0 n)]
                   [tangent-p (vector-ref null-vec n)]
                   ;; Normalize
                   [norm (sqrt (+ (vec-dot tangent-x tangent-x) (* tangent-p tangent-p)))])
                  (if (< norm 1e-10)
                      #f
                      (cons (vec-scale (/ 1.0 norm) tangent-x)
                            (/ tangent-p norm)))))))

(define (augment-with-column m v)
  (doc 'type '(-> Matrix Vec Matrix))
  (doc 'description "Create [M | v] by appending column v to matrix M")
  (let* ([rows (matrix-rows m)]
         [cols (matrix-cols m)]
         [new-cols (+ cols 1)]
         [data (make-vector (* rows new-cols) 0)])
        ;; Copy M
        (do ([i 0 (+ i 1)])
            ((= i rows))
            (do ([j 0 (+ j 1)])
                ((= j cols))
                (vector-set! data (+ (* i new-cols) j)
                             (matrix-ref m i j)))
            ;; Add column from v
            (vector-set! data (+ (* i new-cols) cols)
                         (vector-ref v i)))
        (list 'matrix rows new-cols data)))

(define (extract-column m col)
  (doc 'type '(-> Matrix Nat Vec))
  (doc 'description "Extract column col from matrix as vector")
  (let* ([rows (matrix-rows m)]
         [v (make-vector rows 0)])
        (do ([i 0 (+ i 1)])
            ((= i rows) v)
            (vector-set! v i (matrix-ref m i col)))))

(define (subvector v start len)
  (doc 'type '(-> Vec Nat Nat Vec))
  (doc 'description "Extract subvector v[start:start+len]")
  (let ([result (make-vector len 0)])
       (do ([i 0 (+ i 1)])
           ((= i len) result)
           (vector-set! result i (vector-ref v (+ start i))))))

(define (arclength-predict fp param tangent ds)
  (doc 'type '(-> Vec Number (Pair Vec Number) Number (Pair Vec Number)))
  (doc 'description "Predictor step: move along tangent by arc-length ds")
  (let* ([tangent-x (car tangent)]
         [tangent-p (cdr tangent)]
         [new-fp (vec-add fp (vec-scale ds tangent-x))]
         [new-param (+ param (* ds tangent-p))])
        (cons new-fp new-param)))

(define (arclength-correct psys pred-fp pred-param tangent ds)
  (doc 'type '(-> ParamODE Vec Number (Pair Vec Number) Number (Option (Pair Vec Number))))
  (doc 'description "Corrector: solve bordered system F(x,p)=0 with arclength constraint")
  (doc 'returns "(corrected-fp . corrected-param) or #f if correction fails")
  (let* ([tangent-x (car tangent)]
         [tangent-p (cdr tangent)]
         [n (vector-length pred-fp)])
        ;; Newton iteration on the bordered system:
        ;; [ J_x   J_p  ] [dx]   [-f(x,p)                    ]
        ;; [ t_x^T t_p  ] [dp] = [-((x-x0)·t_x + (p-p0)·t_p) + ds]
        ;; where (x0, p0) is the previous point, (t_x, t_p) is the tangent
        (let loop ([fp pred-fp]
                   [param pred-param]
                   [iter 0])
             (if (>= iter *arclength-max-correct*)
                 #f  ; Failed to converge
                 (let* ([sys (instantiate-at psys param)]
                        [f-val (eval-vector-field sys 0 fp)]
                        [f-norm (vec-norm f-val)])
                       (if (< f-norm *continuation-tolerance*)
                           (cons fp param)  ; Converged
                           ;; Compute correction
                           (let* ([jac-x (compute-jacobian sys fp *continuation-step-size*)]
                                  [jac-p (compute-parameter-jacobian psys param fp *continuation-step-size*)]
                                  [correction (solve-bordered-system jac-x jac-p tangent-x tangent-p
                                                                     f-val)])
                                 (if (not correction)
                                     #f  ; Linear solve failed
                                     (let* ([dx (car correction)]
                                            [dp (cdr correction)]
                                            [new-fp (vec-sub fp dx)]
                                            [new-param (- param dp)])
                                           (loop new-fp new-param (+ iter 1)))))))))))

(define (solve-bordered-system jac-x jac-p tangent-x tangent-p rhs-f)
  (doc 'type '(-> Matrix Vec Vec Number Vec (Option (Pair Vec Number))))
  (doc 'description "Solve the bordered system for Newton correction")
  ;; Build and solve:
  ;; [ J_x   J_p  ] [dx]   [f ]
  ;; [ t_x^T t_p  ] [dp] = [0 ]  (arclength constraint linearized)
  (let* ([n (vector-length rhs-f)]
         ;; Build bordered matrix (n+1) x (n+1)
         [bordered (make-bordered-matrix jac-x jac-p tangent-x tangent-p)]
         ;; Build RHS: [f; 0]
         [rhs (make-vector (+ n 1) 0)])
        (do ([i 0 (+ i 1)])
            ((= i n))
            (vector-set! rhs i (vector-ref rhs-f i)))
        (vector-set! rhs n 0.0)  ; Arclength constraint RHS
        ;; Solve
        (let ([solution (solve-linear-system bordered rhs)])
             (if (not solution)
                 #f
                 (cons (subvector solution 0 n)
                       (vector-ref solution n))))))

(define (make-bordered-matrix jac-x jac-p tangent-x tangent-p)
  (doc 'type '(-> Matrix Vec Vec Number Matrix))
  (doc 'description "Build the (n+1)x(n+1) bordered matrix for arclength continuation")
  (let* ([n (matrix-rows jac-x)]
         [size (+ n 1)]
         [data (make-vector (* size size) 0)])
        ;; Copy J_x into top-left n×n block
        (do ([i 0 (+ i 1)])
            ((= i n))
            (do ([j 0 (+ j 1)])
                ((= j n))
                (vector-set! data (+ (* i size) j)
                             (matrix-ref jac-x i j)))
            ;; J_p column
            (vector-set! data (+ (* i size) n)
                         (vector-ref jac-p i)))
        ;; Bottom row: tangent
        (do ([j 0 (+ j 1)])
            ((= j n))
            (vector-set! data (+ (* n size) j)
                         (vector-ref tangent-x j)))
        (vector-set! data (+ (* n size) n) tangent-p)
        (list 'matrix size size data)))

(define (orient-tangent new-tangent old-tangent)
  (doc 'type '(-> (Pair Vec Number) (Pair Vec Number) (Pair Vec Number)))
  (doc 'description "Orient new tangent to point in same direction as old tangent")
  ;; Check dot product; flip if negative
  (let* ([dot (+ (vec-dot (car new-tangent) (car old-tangent))
                 (* (cdr new-tangent) (cdr old-tangent)))])
        (if (< dot 0)
            (cons (vec-scale -1.0 (car new-tangent))
                  (- (cdr new-tangent)))
            new-tangent)))

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

(define (detect-bifurcations continuation-data . opt-psys)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number Vec Symbol (List Complex))) (Option ParamODE)
                  (List (List Symbol Number Vec))))
  (doc 'description "Detect bifurcation points from continuation data by analyzing eigenvalue transitions")
  (doc 'param 'continuation-data "output from continue-fixed-point or continue-fixed-point-arclength")
  (doc 'param 'opt-psys "optional: parameterized ODE for normal form analysis (improves pitchfork/transcritical distinction)")
  (doc 'returns "list of (bifurcation-type param fixed-point) for each detected bifurcation")
  (let ([psys (if (null? opt-psys) #f (car opt-psys))])
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
                          ;; Refine pitchfork/transcritical using normal form if psys provided
                          (let ([refined-bif
                                 (if (and psys bif (eq? bif 'pitchfork-or-transcritical))
                                     (let ([sys (instantiate-at psys curr-param)])
                                          (classify-codim1-bifurcation sys curr-fp *normal-form-h*))
                                     bif)])
                               (loop curr (cdr rest)
                                     (if refined-bif
                                         (cons (list refined-bif curr-param curr-fp) bifurcations)
                                         bifurcations)))))))))

(define (detect-bifurcation-type prev-eigs curr-eigs prev-stab curr-stab)
  (doc 'type '(-> (List Complex) (List Complex) Symbol Symbol (Option Symbol)))
  (doc 'description "Determine bifurcation type from eigenvalue transition")
  (doc 'note "Order matters: pitchfork/transcritical checked first (stability change with persistence),
then Hopf (complex eigenvalues), then saddle-node (real eigenvalue crossing without stability change).")
  (let* ([prev-reals (map complex-real prev-eigs)]
         [curr-reals (map complex-real curr-eigs)]
         [prev-imags (map complex-imag prev-eigs)]
         [curr-imags (map complex-imag curr-eigs)]
         [stab-changed (stability-changed? prev-stab curr-stab)]
         [real-zero-crossing (real-eigenvalue-zero-crossing? prev-reals curr-reals prev-imags)])
        (cond
         ;; Hopf: complex conjugate pair crosses imaginary axis (checked first, most distinctive)
         [(hopf-condition? prev-reals curr-reals prev-imags curr-imags)
          'hopf]
         ;; Pitchfork/Transcritical: stability change with real eigenvalue zero crossing
         ;; (fixed point persists but changes stability - NOT saddle-node)
         ;; Returns 'pitchfork-or-transcritical; refined later via normal-form analysis
         ;; when the ODE system is available (see classify-codim1-bifurcation)
         [(and stab-changed real-zero-crossing)
          'pitchfork-or-transcritical]
         ;; Saddle-node: real eigenvalue crosses zero WITHOUT stability change
         ;; This typically indicates fixed point creation/annihilation
         [(and real-zero-crossing (not stab-changed)
               (saddle-node-condition? prev-reals curr-reals prev-imags))
          'saddle-node]
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

;;; ============================================================
;;; Section: Normal Form Coefficients for Bifurcation Classification
;;; ============================================================

(doc 'section 'normal-form-classification)
(doc 'note "Distinguish pitchfork from transcritical using normal form coefficients.
At a real eigenvalue zero-crossing:
  - Pitchfork: f''(x*) ≈ 0 (Z₂ symmetry), distinguished by f'''(x*)
  - Transcritical: f''(x*) ≠ 0")

(define *normal-form-tolerance* 1e-4)
(define *normal-form-h* 1e-4)  ; Step size for normal form derivative estimation
                               ; Must be larger than continuation step (1e-6) to avoid
                               ; roundoff error dominating in f''' computation

(define (compute-1d-derivatives sys fp h)
  (doc 'type '(-> ODE Vec Number (List Number Number)))
  (doc 'description "Compute 2nd and 3rd derivatives of 1D vector field at equilibrium")
  (doc 'returns "(f'' f''') at the equilibrium point")
  (let* ([x0 (vector-ref fp 0)]
         [f (lambda (x)
              (let ([result (eval-vector-field sys 0 (vector x))])
                   (vector-ref result 0)))]
         ;; Second derivative: f''(x) = (f(x+h) - 2f(x) + f(x-h)) / h²
         [fpp (/ (+ (f (+ x0 h)) (f (- x0 h)) (* -2 (f x0)))
                 (* h h))]
         ;; Third derivative: f'''(x) = (f(x+2h) - 2f(x+h) + 2f(x-h) - f(x-2h)) / (2h³)
         [fppp (/ (+ (f (+ x0 (* 2 h)))
                     (* -2 (f (+ x0 h)))
                     (* 2 (f (- x0 h)))
                     (- (f (- x0 (* 2 h)))))
                  (* 2 h h h))])
        (list fpp fppp)))

(define (normal-form-is-pitchfork? sys fp h)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Boolean))
  (doc 'description "Determine if bifurcation has pitchfork normal form (Z₂ symmetry)")
  (doc 'param 'sys "ODE system at the bifurcation parameter")
  (doc 'param 'fp "fixed point (equilibrium) at bifurcation")
  (doc 'param 'h "step size for numerical differentiation")
  (doc 'returns "#t if |f''(x*)| < tolerance (pitchfork), #f otherwise (transcritical)")
  (doc 'note "Only valid for 1D systems at real eigenvalue zero-crossing")
  (let ([dim (ode-dimension sys)])
       (if (not (= dim 1))
           ;; For higher dimensions, fall back to heuristic
           ;; (could extend to use Jacobian eigenspace analysis)
           #f
           (let* ([derivs (compute-1d-derivatives sys fp h)]
                  [fpp (car derivs)])
                 (< (abs fpp) *normal-form-tolerance*)))))

(define (classify-codim1-bifurcation sys fp h)
  (doc 'export #t)
  (doc 'type '(-> ODE Vec Number Symbol))
  (doc 'description "Classify a codimension-1 bifurcation using normal form analysis")
  (doc 'param 'sys "ODE system at the bifurcation parameter")
  (doc 'param 'fp "fixed point (equilibrium) at bifurcation")
  (doc 'param 'h "step size for numerical differentiation")
  (doc 'returns "'pitchfork, 'transcritical, or 'unknown")
  (let ([dim (ode-dimension sys)])
       (cond
        [(not (= dim 1)) 'unknown]  ; Need more sophisticated analysis for n > 1
        [else
         (let* ([derivs (compute-1d-derivatives sys fp h)]
                [fpp (car derivs)]
                [fppp (cadr derivs)])
               (cond
                ;; Pitchfork: f'' ≈ 0, f''' ≠ 0
                [(and (< (abs fpp) *normal-form-tolerance*)
                      (> (abs fppp) *normal-form-tolerance*))
                 'pitchfork]
                ;; Transcritical: f'' ≠ 0
                [(> (abs fpp) *normal-form-tolerance*)
                 'transcritical]
                ;; Degenerate case
                [else 'unknown]))])))

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
  (doc 'note "Supercritical (l₁ < 0): stable limit cycle emerges. Subcritical (l₁ > 0): unstable limit cycle")
  (doc 'returns "'supercritical, 'subcritical, or 'unknown")
  (let ([l1 (first-lyapunov-coefficient psys param fp h)])
       (cond
        [(not l1) 'unknown]
        [(< l1 0) 'supercritical]
        [(> l1 0) 'subcritical]
        [else 'degenerate])))

(define (first-lyapunov-coefficient psys param fp h)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Number (Option Number)))
  (doc 'description "Compute the First Lyapunov Coefficient at a Hopf bifurcation")
  (doc 'note "Uses Kuznetsov's formula with numerical derivatives. Returns #f if not at a Hopf point.")
  (doc 'returns "l₁ value: negative = supercritical, positive = subcritical")
  ;; Only valid for 2D systems at Hopf bifurcation
  (if (not (= (vector-length fp) 2))
      #f  ; Only implemented for 2D
      (let* ([sys (instantiate-at psys param)]
             [analysis (analyze-stability-general sys fp h)]
             [eigs (cdr analysis)])
            ;; Check that we're at a Hopf point (complex eigenvalues with small real part)
            (if (not (hopf-point? eigs))
                #f
                (let* ([omega (abs (complex-imag (car eigs)))]
                       ;; Compute second and third order derivatives
                       [derivs (compute-hopf-derivatives sys fp h)]
                       ;; Apply Kuznetsov's formula
                       [l1 (kuznetsov-l1 derivs omega)])
                      l1)))))

(define (hopf-point? eigenvalues)
  (doc 'type '(-> (List Complex) Boolean))
  (doc 'description "Check if eigenvalues indicate a Hopf bifurcation point")
  (and (>= (length eigenvalues) 2)
       (let ([e1 (car eigenvalues)]
             [e2 (cadr eigenvalues)])
            ;; Complex conjugate pair with small real part
            (and (> (abs (complex-imag e1)) 0.001)  ; Nonzero imaginary part
                 (< (abs (complex-real e1)) 0.1)    ; Near zero real part
                 (< (abs (+ (complex-imag e1) (complex-imag e2))) 0.001)))))  ; Conjugates

(define (compute-hopf-derivatives sys fp h)
  (doc 'type '(-> ODE Vec Number (List Number)))
  (doc 'description "Compute 2nd and 3rd order partial derivatives for Hopf normal form")
  (doc 'returns "list: (fxx fxy fyy gxx gxy gyy fxxx fxxy fxyy fyyy gxxx gxxy gxyy gyyy)")
  (let* ([x0 (vector-ref fp 0)]
         [y0 (vector-ref fp 1)]
         ;; Helper to evaluate f and g components
         [f (lambda (x y)
              (let ([result (eval-vector-field sys 0 (vector x y))])
                   (vector-ref result 0)))]
         [g (lambda (x y)
              (let ([result (eval-vector-field sys 0 (vector x y))])
                   (vector-ref result 1)))]
         ;; Second derivatives using central differences
         [fxx (second-deriv-xx f x0 y0 h)]
         [fxy (second-deriv-xy f x0 y0 h)]
         [fyy (second-deriv-yy f x0 y0 h)]
         [gxx (second-deriv-xx g x0 y0 h)]
         [gxy (second-deriv-xy g x0 y0 h)]
         [gyy (second-deriv-yy g x0 y0 h)]
         ;; Third derivatives
         [fxxx (third-deriv-xxx f x0 y0 h)]
         [fxxy (third-deriv-xxy f x0 y0 h)]
         [fxyy (third-deriv-xyy f x0 y0 h)]
         [fyyy (third-deriv-yyy f x0 y0 h)]
         [gxxx (third-deriv-xxx g x0 y0 h)]
         [gxxy (third-deriv-xxy g x0 y0 h)]
         [gxyy (third-deriv-xyy g x0 y0 h)]
         [gyyy (third-deriv-yyy g x0 y0 h)])
        (list fxx fxy fyy gxx gxy gyy fxxx fxxy fxyy fyyy gxxx gxxy gxyy gyyy)))

(define (second-deriv-xx f x y h)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number))
  (/ (+ (f (+ x h) y) (f (- x h) y) (* -2 (f x y)))
     (* h h)))

(define (second-deriv-xy f x y h)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number))
  (/ (+ (f (+ x h) (+ y h))
        (f (- x h) (- y h))
        (- (f (+ x h) (- y h)))
        (- (f (- x h) (+ y h))))
     (* 4 h h)))

(define (second-deriv-yy f x y h)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number))
  (/ (+ (f x (+ y h)) (f x (- y h)) (* -2 (f x y)))
     (* h h)))

(define (third-deriv-xxx f x y h)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number))
  (/ (+ (f (+ x (* 2 h)) y)
        (* -2 (f (+ x h) y))
        (* 2 (f (- x h) y))
        (- (f (- x (* 2 h)) y)))
     (* 2 h h h)))

(define (third-deriv-xxy f x y h)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number))
  (/ (+ (f (+ x h) (+ y h))
        (- (f (+ x h) (- y h)))
        (* -2 (f x (+ y h)))
        (* 2 (f x (- y h)))
        (f (- x h) (+ y h))
        (- (f (- x h) (- y h))))
     (* 2 h h h)))

(define (third-deriv-xyy f x y h)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number))
  (/ (+ (f (+ x h) (+ y h))
        (- (f (- x h) (+ y h)))
        (* -2 (f (+ x h) y))
        (* 2 (f (- x h) y))
        (f (+ x h) (- y h))
        (- (f (- x h) (- y h))))
     (* 2 h h h)))

(define (third-deriv-yyy f x y h)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number))
  (/ (+ (f x (+ y (* 2 h)))
        (* -2 (f x (+ y h)))
        (* 2 (f x (- y h)))
        (- (f x (- y (* 2 h)))))
     (* 2 h h h)))

(define (kuznetsov-l1 derivs omega)
  (doc 'type '(-> (List Number) Number Number))
  (doc 'description "Compute l₁ using Kuznetsov's formula for 2D Hopf bifurcation")
  ;; Unpack derivatives: (fxx fxy fyy gxx gxy gyy fxxx fxxy fxyy fyyy gxxx gxxy gxyy gyyy)
  (let* ([fxx (list-ref derivs 0)]
         [fxy (list-ref derivs 1)]
         [fyy (list-ref derivs 2)]
         [gxx (list-ref derivs 3)]
         [gxy (list-ref derivs 4)]
         [gyy (list-ref derivs 5)]
         [fxxx (list-ref derivs 6)]
         [fxxy (list-ref derivs 7)]
         [fxyy (list-ref derivs 8)]
         [fyyy (list-ref derivs 9)]
         [gxxx (list-ref derivs 10)]
         [gxxy (list-ref derivs 11)]
         [gxyy (list-ref derivs 12)]
         [gyyy (list-ref derivs 13)]
         ;; Kuznetsov's formula (simplified for standard 2D case)
         ;; l₁ = (1/(16ω)) × [fxxx + fxyy + gxxy + gyyy]
         ;;    + (1/(16ω²)) × [fxy(fxx + fyy) - gxy(gxx + gyy) - fxx·gxx + fyy·gyy]
         [term1 (/ (+ fxxx fxyy gxxy gyyy) (* 16 omega))]
         [term2 (/ (+ (* fxy (+ fxx fyy))
                      (- (* gxy (+ gxx gyy)))
                      (- (* fxx gxx))
                      (* fyy gyy))
                   (* 16 omega omega))])
        (+ term1 term2)))

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
                            [det (matrix-determinant jac)])
                           (if (> det 0)
                               (loop mid hi (+ iter 1))
                               (loop lo mid (+ iter 1)))))))))

(define (matrix-determinant m)
  (doc 'type '(-> Matrix Number))
  (doc 'description "Compute determinant of n×n matrix using LU decomposition")
  (doc 'note "Works for any dimension, not just 2×2")
  (let ([n (matrix-rows m)])
       (cond
        ;; Special case: 1x1
        [(= n 1) (matrix-ref m 0 0)]
        ;; Special case: 2x2 (fast path)
        [(= n 2)
         (let ([a (matrix-ref m 0 0)]
               [b (matrix-ref m 0 1)]
               [c (matrix-ref m 1 0)]
               [d (matrix-ref m 1 1)])
              (- (* a d) (* b c)))]
        ;; General case: LU decomposition
        [else (matrix-determinant-lu m)])))

(define (matrix-determinant-lu m)
  (doc 'type '(-> Matrix Number))
  (doc 'description "Compute determinant via LU decomposition with partial pivoting")
  (let* ([n (matrix-rows m)]
         [data (vector-copy (matrix-data m))]
         [sign 1])  ; Track row swap parity
        ;; Gaussian elimination with partial pivoting
        (let elim-loop ([k 0])
             (if (= k n)
                 ;; Determinant is product of diagonal * sign
                 (let diag-loop ([i 0] [det sign])
                      (if (= i n)
                          det
                          (diag-loop (+ i 1)
                                     (* det (vector-ref data (+ (* i n) i))))))
                 ;; Find pivot
                 (let* ([pivot-row (find-pivot-row-det data k n)]
                        [pivot-val (vector-ref data (+ (* pivot-row n) k))])
                       (if (< (abs pivot-val) 1e-15)
                           0  ; Singular matrix - determinant is zero
                           (begin
                             ;; Swap rows if needed
                             (unless (= pivot-row k)
                                     (swap-rows-det! data k pivot-row n)
                                     (set! sign (- sign)))  ; Flip sign on swap
                             ;; Eliminate column k
                             (do ([i (+ k 1) (+ i 1)])
                                 ((= i n))
                                 (let ([factor (/ (vector-ref data (+ (* i n) k)) pivot-val)])
                                      (do ([j k (+ j 1)])
                                          ((= j n))
                                          (vector-set! data (+ (* i n) j)
                                                       (- (vector-ref data (+ (* i n) j))
                                                          (* factor (vector-ref data (+ (* k n) j))))))))
                             (elim-loop (+ k 1)))))))))

(define (find-pivot-row-det data k n)
  (doc 'type '(-> Vector Nat Nat Nat))
  (let loop ([i k] [best-row k] [best-val (abs (vector-ref data (+ (* k n) k)))])
       (if (= i n)
           best-row
           (let ([val (abs (vector-ref data (+ (* i n) k)))])
                (if (> val best-val)
                    (loop (+ i 1) i val)
                    (loop (+ i 1) best-row best-val))))))

(define (swap-rows-det! data row1 row2 n)
  (doc 'type '(-> Vector Nat Nat Nat Void))
  (unless (= row1 row2)
          (do ([j 0 (+ j 1)])
              ((= j n))
              (let ([temp (vector-ref data (+ (* row1 n) j))])
                   (vector-set! data (+ (* row1 n) j)
                                (vector-ref data (+ (* row2 n) j)))
                   (vector-set! data (+ (* row2 n) j) temp)))))

;; Keep 2D version for backward compatibility
(define (matrix-determinant-2d m)
  (doc 'type '(-> Matrix Number))
  (doc 'description "Compute determinant of 2x2 matrix (deprecated, use matrix-determinant)")
  (matrix-determinant m))

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
