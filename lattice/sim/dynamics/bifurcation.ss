(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/svd.ss")  ; For null-space computation in branch switching
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

;;; filter-map provided by prelude

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
      (find-fixed-point-newton sys initial-guess tolerance step-size max-iter)))

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
                       ;; Correct using configured solver (robust or classic Newton)
                       [next-fp (find-fixed-point-solver next-sys fp
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
                                           [new-param (cadr corrected)]
                                           ;; corrected is (fp param iterations) - iterations available at (caddr corrected)
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
  (doc 'type '(-> ParamODE Vec Number (Pair Vec Number) Number (Option (List Vec Number Nat))))
  (doc 'description "Corrector: solve bordered system F(x,p)=0 with arclength constraint")
  (doc 'returns "(corrected-fp corrected-param iterations) or #f if correction fails")
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
                           (list fp param iter)  ; Converged with iteration count
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

;;; ============================================================
;;; Section: Adaptive Step-Size Control
;;; ============================================================

(doc 'section 'adaptive-step-size)
(doc 'note "Adaptive step-size control for continuation methods.
Shrinks steps near bifurcations (eigenvalue proximity), tight curves (curvature),
or when Newton has difficulty (iteration count). Expands steps in stable regions.")

;; Adaptive step-size parameters
(define *adaptive-eigenvalue-threshold* 0.1)   ; Shrink when |Re(λ)| < this
(define *adaptive-curvature-threshold* 0.26)   ; ~15 degrees in radians
(define *adaptive-residual-threshold* 4)       ; Shrink when iters > this
(define *adaptive-residual-critical* 8)        ; Strong shrink when iters > this
(define *adaptive-min-step* 1e-5)              ; Floor to prevent stalling
(define *adaptive-max-step-multiplier* 2.0)    ; Cap on expansion
(define *adaptive-expansion-count* 3)          ; Consecutive good steps to expand
(define *adaptive-shrink-factor* 0.5)          ; Shrink multiplier
(define *adaptive-expand-factor* 1.5)          ; Expand multiplier

(define (compute-eigenvalue-factor eigenvalues)
  (doc 'export #t)
  (doc 'type '(-> (List Complex) Number))
  (doc 'description "Compute step-size factor based on eigenvalue proximity to critical values")
  (doc 'returns "1.0 if safe, 0.5 if near bifurcation")
  (if (null? eigenvalues)
      1.0
      (let ([min-real-mag (apply min (map (lambda (e) (abs (complex-real e))) eigenvalues))])
           (if (< min-real-mag *adaptive-eigenvalue-threshold*)
               *adaptive-shrink-factor*
               1.0))))

(define (compute-curvature-factor old-tangent new-tangent)
  (doc 'export #t)
  (doc 'type '(-> (Pair Vec Number) (Pair Vec Number) Number))
  (doc 'description "Compute step-size factor based on tangent direction change (curvature)")
  (doc 'returns "1.0 if smooth, 0.5 if sharp turn")
  (if (or (not old-tangent) (not new-tangent))
      1.0
      (let* ([dot (+ (vec-dot (car old-tangent) (car new-tangent))
                     (* (cdr old-tangent) (cdr new-tangent)))]
             ;; Clamp dot to [-1, 1] to avoid acos domain errors
             [clamped-dot (max -1.0 (min 1.0 dot))]
             [angle (acos (abs clamped-dot))])  ; Use abs since tangents may be flipped
            (if (> angle *adaptive-curvature-threshold*)
                *adaptive-shrink-factor*
                1.0))))

(define (compute-residual-factor iterations)
  (doc 'export #t)
  (doc 'type '(-> Nat Number))
  (doc 'description "Compute step-size factor based on Newton iteration count")
  (doc 'returns "1.0 if fast convergence, 0.5 if slow, 0.25 if near failure")
  (cond
   [(>= iterations *adaptive-residual-critical*) 0.25]   ; 8+ iterations = near failure
   [(> iterations *adaptive-residual-threshold*) *adaptive-shrink-factor*]  ; 5-7 = slow
   [else 1.0]))

(define (compute-adaptive-step current-step factors base-step)
  (doc 'type '(-> Number (List Number) Number Number))
  (doc 'description "Compute new step size from factors, respecting min/max bounds")
  (let* ([combined-factor (apply min factors)]
         [new-step (* current-step combined-factor)]
         [min-step *adaptive-min-step*]
         [max-step (* base-step *adaptive-max-step-multiplier*)])
        (max min-step (min max-step new-step))))

(define (should-expand? good-step-count factors)
  (doc 'type '(-> Nat (List Number) Boolean))
  (doc 'description "Check if step size should be expanded")
  (and (>= good-step-count *adaptive-expansion-count*)
       (every (lambda (f) (>= f 1.0)) factors)))

(define (continue-fixed-point-arclength-adaptive psys param0 fp0 num-steps base-step)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Nat Number (List (List Number Vec Symbol (List Complex)))))
  (doc 'description "Continue a fixed point branch using adaptive pseudo-arclength continuation")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param0 "starting parameter value")
  (doc 'param 'fp0 "starting fixed point (must satisfy f(x,p)=0)")
  (doc 'param 'num-steps "maximum number of continuation steps")
  (doc 'param 'base-step "initial arc-length step size (can be negative for reverse direction)")
  (doc 'returns "list of (param fixed-point stability eigenvalues) tuples along the branch")
  (doc 'note "Adapts step size based on eigenvalue proximity, curvature, and Newton iterations")
  (let* ([n (vector-length fp0)]
         [sign (if (>= base-step 0) 1 -1)]
         [abs-base (abs base-step)])
        ;; Compute initial tangent
        (let ([tangent0 (compute-tangent psys param0 fp0)])
             (if (not tangent0)
                 (list (list param0 fp0 'unknown '()))  ; Can't compute tangent
                 (let loop ([k 0]
                            [param param0]
                            [fp fp0]
                            [tangent tangent0]
                            [current-step abs-base]
                            [good-steps 0]              ; Consecutive steps with all factors = 1.0
                            [prev-tangent #f]           ; For curvature computation
                            [results '()])
                      (if (>= k num-steps)
                          ;; Record final point before returning
                          (let* ([sys (instantiate-at psys param)]
                                 [analysis (analyze-stability-general sys fp *continuation-step-size*)]
                                 [stability (car analysis)]
                                 [eigenvalues (cdr analysis)]
                                 [final-entry (list param fp stability eigenvalues)])
                                (reverse (cons final-entry results)))
                          ;; Record current point with stability analysis
                          (let* ([sys (instantiate-at psys param)]
                                 [analysis (analyze-stability-general sys fp *continuation-step-size*)]
                                 [stability (car analysis)]
                                 [eigenvalues (cdr analysis)]
                                 [entry (list param fp stability eigenvalues)]
                                 ;; Corrector with retry logic (predicts internally from fp/param)
                                 [correct-result (arclength-correct-with-retry
                                                  psys fp param tangent
                                                  (* sign current-step) current-step)])
                                (if (not correct-result)
                                    ;; Correction failed even after retries - return what we have
                                    (reverse (cons entry results))
                                    (let* ([new-fp (car correct-result)]
                                           [new-param (cadr correct-result)]
                                           [iterations (caddr correct-result)]
                                           [used-step (cadddr correct-result)]  ; May be smaller if retried
                                           ;; Compute new tangent for next step
                                           [new-tangent (compute-tangent psys new-param new-fp)])
                                          (if (not new-tangent)
                                              (reverse (cons entry results))
                                              ;; Compute adaptive factors
                                              (let* ([oriented-tangent (orient-tangent new-tangent tangent)]
                                                     [eig-factor (compute-eigenvalue-factor eigenvalues)]
                                                     [curv-factor (compute-curvature-factor prev-tangent oriented-tangent)]
                                                     [res-factor (compute-residual-factor iterations)]
                                                     [factors (list eig-factor curv-factor res-factor)]
                                                     ;; Update good-steps counter
                                                     [all-good (every (lambda (f) (>= f 1.0)) factors)]
                                                     [new-good-steps (if all-good (+ good-steps 1) 0)]
                                                     ;; Compute new step size
                                                     [expanded-step (if (should-expand? new-good-steps factors)
                                                                        (min (* used-step *adaptive-expand-factor*)
                                                                             (* abs-base *adaptive-max-step-multiplier*))
                                                                        used-step)]
                                                     [new-step (compute-adaptive-step expanded-step factors abs-base)])
                                                    (loop (+ k 1) new-param new-fp oriented-tangent
                                                          new-step
                                                          (if (should-expand? new-good-steps factors) 0 new-good-steps)
                                                          oriented-tangent
                                                          (cons entry results)))))))))))))

(define (arclength-correct-with-retry psys orig-fp orig-param tangent ds current-step)
  (doc 'type '(-> ParamODE Vec Number (Pair Vec Number) Number Number
                  (Option (List Vec Number Nat Number))))
  (doc 'description "Attempt arclength correction with retry on failure")
  (doc 'param 'orig-fp "original successful fixed point (NOT the predicted point)")
  (doc 'param 'orig-param "original successful parameter value")
  (doc 'returns "(fp param iterations used-step) or #f if all retries fail")
  (let retry-loop ([step current-step]
                   [attempts 0])
       (if (or (>= attempts 5) (< step *adaptive-min-step*))
           #f  ; Give up after 5 attempts or step too small
           ;; Predict from ORIGINAL point with current step size
           (let* ([scaled-ds (* (/ step current-step) ds)]
                  [predicted (arclength-predict orig-fp orig-param tangent scaled-ds)]
                  [result (arclength-correct psys (car predicted) (cdr predicted) tangent scaled-ds)])
                 (if result
                     (list (car result) (cadr result) (caddr result) step)
                     ;; Retry with smaller step from original point
                     (retry-loop (* step *adaptive-shrink-factor*) (+ attempts 1)))))))

(define (continue-fixed-point-adaptive psys param0 fp0 param-end base-step)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Number Number (List (List Number Vec Symbol (List Complex)))))
  (doc 'description "Continue a fixed point branch with adaptive step size (fixed-parameter method)")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param0 "starting parameter value")
  (doc 'param 'fp0 "starting fixed point")
  (doc 'param 'param-end "ending parameter value")
  (doc 'param 'base-step "initial parameter step size")
  (doc 'returns "list of (param fixed-point stability eigenvalues) tuples along the branch")
  (doc 'note "Uses eigenvalue + residual indicators only (curvature disabled for fixed-param)")
  (let ([direction (if (> param-end param0) 1 -1)]
        [abs-base (abs base-step)])
       (let loop ([param param0]
                  [fp fp0]
                  [current-step abs-base]
                  [good-steps 0]
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
                       ;; Correct with retry logic (computes target param internally)
                       [next-result (find-fixed-point-with-retry psys param direction fp current-step)])
                      (if (not next-result)
                          ;; Continuation failed - return what we have
                          (reverse (cons entry results))
                          (let* ([next-fp (car next-result)]
                                 [iterations (cadr next-result)]
                                 [used-step (caddr next-result)]
                                 ;; Compute adaptive factors (no curvature for fixed-param)
                                 [eig-factor (compute-eigenvalue-factor eigenvalues)]
                                 [res-factor (compute-residual-factor iterations)]
                                 [factors (list eig-factor res-factor)]
                                 ;; Update good-steps counter
                                 [all-good (every (lambda (f) (>= f 1.0)) factors)]
                                 [new-good-steps (if all-good (+ good-steps 1) 0)]
                                 ;; Compute new step size
                                 [expanded-step (if (should-expand? new-good-steps factors)
                                                    (min (* used-step *adaptive-expand-factor*)
                                                         (* abs-base *adaptive-max-step-multiplier*))
                                                    used-step)]
                                 [new-step (compute-adaptive-step expanded-step factors abs-base)]
                                 ;; Actual next param using the step that worked
                                 [actual-next-param (+ param (* direction used-step))])
                                (loop actual-next-param next-fp new-step
                                      (if (should-expand? new-good-steps factors) 0 new-good-steps)
                                      (cons entry results)))))))))

(define (find-fixed-point-with-retry psys orig-param direction fp-guess current-step)
  (doc 'type '(-> ParamODE Number Number Vec Number (Option (List Vec Nat Number))))
  (doc 'description "Find fixed point with retry on failure, shrinking step toward original param")
  (doc 'param 'orig-param "original parameter value to step from")
  (doc 'param 'direction "direction of parameter change (+1 or -1)")
  (doc 'returns "(fp iterations used-step) or #f if all retries fail")
  (let retry-loop ([step current-step]
                   [attempts 0])
       (if (or (>= attempts 5) (< step *adaptive-min-step*))
           #f
           (let* ([target-param (+ orig-param (* direction step))]
                  [sys (instantiate-at psys target-param)]
                  [result (find-fixed-point-robust-with-iters sys fp-guess
                                                              *continuation-tolerance*
                                                              *continuation-step-size*
                                                              *continuation-max-newton*)])
                 (if result
                     (list (car result) (cadr result) step)
                     ;; Retry with smaller step (target param closer to orig-param)
                     (retry-loop (* step *adaptive-shrink-factor*) (+ attempts 1)))))))

(define (find-fixed-point-robust-with-iters sys initial-guess tolerance step-size max-iter)
  (doc 'type '(-> ODE Vec Number Number Nat (Option (List Vec Nat))))
  (doc 'description "Find fixed point using robust Newton, returning iteration count")
  (doc 'returns "(fixed-point iterations) or #f if not found")
  (let loop ([x initial-guess] [iter 0])
       (if (>= iter max-iter)
           #f
           (let* ([fx (eval-vector-field sys 0 x)]
                  [norm-fx (vec-norm fx)])
                 (if (< norm-fx tolerance)
                     (list x iter)
                     (let* ([jac (compute-jacobian sys x step-size)]
                            [jac-pinv (pseudoinverse jac)])
                           (if (and (pair? jac-pinv) (eq? (car jac-pinv) 'error))
                               #f
                               (let* ([delta (matrix-vec-mul jac-pinv fx)]
                                      [x-new (vec-sub x delta)])
                                     (loop x-new (+ iter 1))))))))))

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
            (append-map (lambda (i)
                                (let ([val (+ (car range) (* i step))])
                                     (map (lambda (pt)
                                                  (list->vector (cons val (vector->list pt))))
                                          rest-grid)))
                        (iota density)))))

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

;; Configuration for fold detection sensitivity
(define *fold-eigenvalue-tolerance* 0.05)  ; Tighter than previous 0.1
(define *fold-imaginary-tolerance* 1e-4)   ; Eigenvalue must be essentially real

(define (detect-fold-from-param-reversal prev-prev prev curr prev-eigs)
  (doc 'type '(-> (Option (List Number Vec Symbol (List Complex)))
                  (List Number Vec Symbol (List Complex))
                  (List Number Vec Symbol (List Complex))
                  (List Complex)
                  (Option Symbol)))
  (doc 'description "Detect fold (saddle-node) from parameter direction reversal in arc-length data")
  (doc 'note "At a fold, parameter reaches extremum and direction reverses.
A REAL eigenvalue crosses zero (distinguishes from Hopf where complex pair crosses).")
  (if (not prev-prev)
      #f  ; Need 3 points
      (let* ([pp-param (car prev-prev)]
             [p-param (car prev)]
             [c-param (car curr)]
             ;; Parameter deltas
             [dp1 (- p-param pp-param)]
             [dp2 (- c-param p-param)]
             ;; Check for direction reversal (sign change in dp)
             [reversal? (< (* dp1 dp2) 0)]
             ;; Get eigenvalues from all three points
             [pp-eigs (cadddr prev-prev)]
             [curr-eigs (cadddr curr)]
             ;; Check for a REAL eigenvalue crossing zero
             ;; (must have small imaginary part AND sign change in real part)
             [has-real-zero-crossing
              (fold-has-real-eigenvalue-zero-crossing pp-eigs prev-eigs curr-eigs)])
            (if (and reversal? has-real-zero-crossing)
                'saddle-node  ; Fold detected via parameter reversal + real eigenvalue crossing
                #f))))

(define (fold-has-real-eigenvalue-zero-crossing pp-eigs prev-eigs curr-eigs)
  (doc 'type '(-> (List Complex) (List Complex) (List Complex) Boolean))
  (doc 'description "Check if a real eigenvalue crosses zero across the fold point")
  (doc 'note "For a true saddle-node: eigenvalue is real (small imaginary part),
passes through near-zero at the fold, and changes sign across it.")
  (if (or (null? pp-eigs) (null? prev-eigs) (null? curr-eigs))
      #f
      ;; Find an eigenvalue that:
      ;; 1. Is essentially real (imaginary part small) at all three points
      ;; 2. Has small magnitude at the middle point (near the fold)
      ;; 3. Changes sign from pp to curr
      (let ([n (length prev-eigs)])
           (any (lambda (i)
                        (let* ([pp-e (list-ref pp-eigs i)]
                               [p-e (list-ref prev-eigs i)]
                               [c-e (list-ref curr-eigs i)]
                               [pp-re (complex-real pp-e)]
                               [pp-im (complex-imag pp-e)]
                               [p-re (complex-real p-e)]
                               [p-im (complex-imag p-e)]
                               [c-re (complex-real c-e)]
                               [c-im (complex-imag c-e)]
                               ;; All three must be essentially real
                               [all-real? (and (< (abs pp-im) *fold-imaginary-tolerance*)
                                               (< (abs p-im) *fold-imaginary-tolerance*)
                                               (< (abs c-im) *fold-imaginary-tolerance*))]
                               ;; Middle point (fold) should have small real part
                               [near-zero? (<= (abs p-re) *fold-eigenvalue-tolerance*)]
                               ;; Sign change from before to after fold
                               [sign-change? (< (* pp-re c-re) 0)])
                              (and all-real? near-zero? sign-change?)))
                (iota n)))))

(define (detect-bifurcations continuation-data . opt-psys)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number Vec Symbol (List Complex))) (Option ParamODE)
                  (List (List Symbol Number Vec))))
  (doc 'description "Detect bifurcation points from continuation data by analyzing eigenvalue transitions")
  (doc 'param 'continuation-data "output from continue-fixed-point or continue-fixed-point-arclength")
  (doc 'param 'opt-psys "optional: parameterized ODE for normal form analysis (improves pitchfork/transcritical distinction)")
  (doc 'returns "list of (bifurcation-type param fixed-point) for each detected bifurcation")
  (doc 'note "For arc-length continuation, also detects fold points via parameter direction reversal")
  (let ([psys (if (null? opt-psys) #f (car opt-psys))])
       (if (or (null? continuation-data) (null? (cdr continuation-data)))
           '()
           ;; Need 3 points to detect parameter reversal (for fold detection)
           (let loop ([prev-prev #f]
                      [prev (car continuation-data)]
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
                           ;; Check for fold (parameter reversal) first - very specific
                           ;; (requires reversal + near-zero eigenvalue)
                           [fold-bif (detect-fold-from-param-reversal
                                      prev-prev prev curr prev-eigs)]
                           ;; Check for eigenvalue-based bifurcations if no fold detected
                           [bif (if fold-bif
                                    #f  ; Skip eigenvalue detection if fold found
                                    (detect-bifurcation-type prev-eigs curr-eigs
                                                             prev-stab curr-stab))])
                          ;; Use whichever detection found something
                          (let* ([effective-bif (or fold-bif bif)]
                                 ;; Refine pitchfork/transcritical using normal form if psys provided
                                 [refined-bif
                                  (if (and psys effective-bif (eq? effective-bif 'pitchfork-or-transcritical))
                                      (let ([sys (instantiate-at psys curr-param)])
                                           (classify-codim1-bifurcation sys curr-fp *normal-form-h*))
                                      effective-bif)]
                                 ;; For fold, report prev (the extremum), not curr
                                 [bif-param (if fold-bif prev-param curr-param)]
                                 [bif-fp (if fold-bif (cadr prev) curr-fp)])
                               (loop prev curr (cdr rest)
                                     (if refined-bif
                                         (cons (list refined-bif bif-param bif-fp) bifurcations)
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
;;; Section: N-Dimensional Normal Form Classification
;;; ============================================================

(doc 'section 'nd-normal-form-classification)
(doc 'note "Extend normal form analysis to n-dimensional systems via center manifold reduction.
At a bifurcation with a zero eigenvalue, dynamics projects onto 1D center manifold.
Uses left/right critical eigenvectors to compute directional derivatives.")

(define (compute-critical-eigenvectors-svd jac)
  (doc 'type '(-> Matrix (Option (List Vec Vec))))
  (doc 'description "Compute both left and right critical eigenvectors using SVD")
  (doc 'returns "(right-eigenvec left-eigenvec) or #f if no zero eigenvalue")
  (let* ([n (matrix-rows jac)]
         [svd-result (svd jac)])
        (if (and (pair? svd-result) (eq? (car svd-result) 'error))
            #f
            (let* ([u (car svd-result)]
                   [sigma (cadr svd-result)]
                   [v (caddr svd-result)]
                   [min-idx (find-min-singular-value-index sigma n)]
                   [min-val (matrix-ref sigma min-idx min-idx)])
                  (if (> min-val 1e-6)
                      #f  ; Not at a bifurcation
                      ;; Right eigenvector: column of V
                      ;; Left eigenvector: column of U
                      (let ([right-vec (make-vector n 0)]
                            [left-vec (make-vector n 0)])
                           (do ([i 0 (+ i 1)])
                               ((= i n))
                               (vector-set! right-vec i (matrix-ref v i min-idx))
                               (vector-set! left-vec i (matrix-ref u i min-idx)))
                           (list right-vec left-vec)))))))

(define (normalize-eigenvector-pair right-vec left-vec)
  (doc 'type '(-> Vec Vec (List Vec Vec)))
  (doc 'description "Normalize eigenvector pair so that wᵀv = 1")
  (let ([dot (vec-dot left-vec right-vec)])
       (if (< (abs dot) 1e-10)
           (list right-vec left-vec)  ; Already orthogonal or degenerate
           (list right-vec (vec-scale (/ 1.0 dot) left-vec)))))

(define (compute-nd-directional-derivatives sys fp right-vec left-vec h)
  (doc 'type '(-> ODE Vec Vec Vec Number (List Number Number)))
  (doc 'description "Compute 2nd and 3rd directional derivatives along center manifold")
  (doc 'param 'right-vec "right critical eigenvector (v where Jv=0)")
  (doc 'param 'left-vec "left critical eigenvector (w where wᵀJ=0), normalized so wᵀv=1")
  (doc 'returns "(d²(wᵀf)/dξ²  d³(wᵀf)/dξ³) where ξ parameterizes center manifold")
  ;; Define the projected function g(ξ) = wᵀf(x* + ξv)
  (let* ([g (lambda (xi)
              (let* ([x (vec-add fp (vec-scale xi right-vec))]
                     [fx (eval-vector-field sys 0 x)])
                    (vec-dot left-vec fx)))]
         ;; Second derivative: g''(0) = (g(h) - 2g(0) + g(-h)) / h²
         [g0 (g 0)]
         [gpp (/ (+ (g h) (g (- h)) (* -2 g0)) (* h h))]
         ;; Third derivative: g'''(0) = (g(2h) - 2g(h) + 2g(-h) - g(-2h)) / (2h³)
         [gppp (/ (+ (g (* 2 h))
                     (* -2 (g h))
                     (* 2 (g (- h)))
                     (- (g (* -2 h))))
                  (* 2 h h h))])
        (list gpp gppp)))

(define (compute-projected-parameter-derivative psys param fp left-vec h)
  (doc 'type '(-> ParamODE Number Vec Vec Number Number))
  (doc 'description "Compute wᵀ(∂f/∂p)|_{x*} - parameter derivative projected onto center manifold")
  (doc 'note "At saddle-node: this is non-zero (fold is transverse).
At transcritical: this is zero (fixed point tracks parameter).")
  (let ([jac-p (compute-parameter-jacobian psys param fp h)])
       (vec-dot left-vec jac-p)))

(define (classify-codim1-bifurcation-nd psys param fp h)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Number Symbol))
  (doc 'description "Classify a codimension-1 bifurcation for n-dimensional systems")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param "parameter value at the bifurcation")
  (doc 'param 'fp "fixed point (equilibrium) at bifurcation")
  (doc 'param 'h "step size for numerical differentiation")
  (doc 'returns "'pitchfork, 'transcritical, 'saddle-node, or 'unknown")
  (doc 'note "Uses center manifold reduction via SVD to handle arbitrary dimensions.
Distinguishes saddle-node by checking wᵀ(∂f/∂p) (transversality condition).")
  (let* ([sys (instantiate-at psys param)]
         [n (ode-dimension sys)])
        (cond
         ;; 1D case: use existing fast path
         [(= n 1)
          (classify-codim1-bifurcation sys fp h)]
         ;; n > 1: use center manifold reduction
         [else
          (let* ([jac (compute-jacobian sys fp *continuation-step-size*)]
                 [eigenpair (compute-critical-eigenvectors-svd jac)])
                (if (not eigenpair)
                    'unknown  ; Not at a bifurcation (no zero eigenvalue)
                    (let* ([right-vec (car eigenpair)]
                           [left-vec (cadr eigenpair)]
                           [normalized (normalize-eigenvector-pair right-vec left-vec)]
                           [norm-right (car normalized)]
                           [norm-left (cadr normalized)]
                           [derivs (compute-nd-directional-derivatives sys fp norm-right norm-left h)]
                           [gpp (car derivs)]
                           [gppp (cadr derivs)]
                           ;; Compute parameter derivative for saddle-node detection
                           [param-deriv (compute-projected-parameter-derivative
                                         psys param fp norm-left h)])
                          (cond
                           ;; Pitchfork: g'' ≈ 0, g''' ≠ 0
                           ;; Symmetric system where quadratic term vanishes
                           [(and (< (abs gpp) *normal-form-tolerance*)
                                 (> (abs gppp) *normal-form-tolerance*))
                            'pitchfork]
                           ;; Saddle-node: wᵀ(∂f/∂p) ≠ 0 (fold is transverse)
                           ;; Fixed point is created/destroyed as parameter changes
                           ;; Note: saddle-node can have g'' ≠ 0 (e.g., r + x²)
                           [(> (abs param-deriv) *normal-form-tolerance*)
                            'saddle-node]
                           ;; Transcritical: g'' ≠ 0 AND wᵀ(∂f/∂p) ≈ 0
                           ;; Fixed point persists but exchanges stability
                           [(and (> (abs gpp) *normal-form-tolerance*)
                                 (< (abs param-deriv) *normal-form-tolerance*))
                            'transcritical]
                           ;; All conditions fail: degenerate or higher codimension
                           [else 'unknown]))))])))

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
       (append-map (lambda (i)
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
                   (iota param-steps))))

(define (bifurcation-diagram-amplitude psys param-min param-max param-steps
                                       initial-state dt n-transient n-sample)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Nat Vec Number Nat Nat
                  (List (Pair Number Number))))
  (doc 'description "Generate bifurcation diagram showing attractor amplitude vs parameter")
  (doc 'note "For limit cycles, this shows the oscillation amplitude. For chaos, it shows the spread")
  (doc 'returns "list of (param, state-component) pairs for plotting")
  (let ([step (/ (- param-max param-min) (max 1 (- param-steps 1)))])
       (append-map (lambda (i)
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
                   (iota param-steps))))

(define (bifurcation-diagram-poincare psys param-min param-max param-steps
                                      initial-state dt n-transient n-crossings
                                      plane-axis plane-value direction)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Number Nat Vec Number Nat Nat Nat Number Symbol
                  (List (Pair Number Number))))
  (doc 'description "Generate bifurcation diagram using Poincare section crossings")
  (doc 'note "Shows period-doubling bifurcations clearly for limit cycles")
  (let ([step (/ (- param-max param-min) (max 1 (- param-steps 1)))])
       (append-map (lambda (i)
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
                   (iota param-steps))))

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
;;; Section: Branch Switching
;;; ============================================================

(doc 'section 'branch-switching)
(doc 'note "After detecting a pitchfork or transcritical bifurcation, switch to
newly-created branches by perturbing along the critical eigenvector.")

(define *branch-switch-perturbation* 0.01)
(define *branch-switch-tolerance* 1e-8)
(define *branch-switch-max-iter* 50)

(define (compute-critical-eigenvector psys param fp)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec (Option Vec)))
  (doc 'description "Compute the eigenvector corresponding to the zero eigenvalue at a bifurcation")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param "parameter value at the bifurcation")
  (doc 'param 'fp "fixed point at the bifurcation")
  (doc 'returns "unit eigenvector in the null space of the Jacobian, or #f if none found")
  (doc 'note "Uses SVD to find the null space robustly. Near-zero singular values (< 1e-6) indicate the null space.")
  (let* ([sys (instantiate-at psys param)]
         [jac (compute-jacobian sys fp *continuation-step-size*)]
         [n (matrix-rows jac)]
         [svd-result (svd jac)])
        (if (and (pair? svd-result) (eq? (car svd-result) 'error))
            #f  ; SVD failed
            (let* ([u (car svd-result)]
                   [sigma (cadr svd-result)]
                   [v (caddr svd-result)]
                   ;; Find the smallest singular value
                   [min-idx (find-min-singular-value-index sigma n)]
                   [min-val (matrix-ref sigma min-idx min-idx)])
                  (if (> min-val 1e-6)
                      #f  ; No near-zero singular value - not at a bifurcation
                      ;; Extract the corresponding column of V
                      (let ([eigenvec (make-vector n 0)])
                           (do ([i 0 (+ i 1)])
                               ((= i n) eigenvec)
                               (vector-set! eigenvec i (matrix-ref v i min-idx)))))))))

(define (find-min-singular-value-index sigma n)
  (doc 'type '(-> Matrix Nat Nat))
  (doc 'description "Find index of smallest diagonal element in Σ matrix")
  (if (= n 0)
      0  ; Degenerate case: 0-dim system returns 0
      (let loop ([i 0] [min-idx 0] [min-val (abs (matrix-ref sigma 0 0))])
           (if (= i n)
               min-idx
               (let ([val (abs (matrix-ref sigma i i))])
                    (if (< val min-val)
                        (loop (+ i 1) i val)
                        (loop (+ i 1) min-idx min-val)))))))

(define (switch-branch psys continuation-data bifurcation-index direction)
  (doc 'export #t)
  (doc 'type '(-> ParamODE (List (List Number Vec Symbol (List Complex))) Nat Symbol
                  (Option (Pair Vec Number))))
  (doc 'description "Switch to a new branch after a pitchfork or transcritical bifurcation")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'continuation-data "output from continue-fixed-point or continue-fixed-point-arclength")
  (doc 'param 'bifurcation-index "index into continuation-data of the bifurcation point (0-based)")
  (doc 'param 'direction "which branch to switch to: 'upper or 'lower (sign of perturbation)")
  (doc 'returns "(new-fixed-point . param) on the new branch, or #f if switch failed")
  (doc 'note "After switching, use continue-fixed-point-arclength to follow the new branch")
  (if (>= bifurcation-index (length continuation-data))
      #f
      (let* ([bif-point (list-ref continuation-data bifurcation-index)]
             [param (car bif-point)]
             [fp (cadr bif-point)]
             ;; Get critical eigenvector at bifurcation
             [eigenvec (compute-critical-eigenvector psys param fp)])
            (if (not eigenvec)
                #f  ; Couldn't compute eigenvector
                ;; Perturb in the chosen direction
                (let* ([sign (if (eq? direction 'upper) 1.0 -1.0)]
                       [perturbation (vec-scale (* sign *branch-switch-perturbation*) eigenvec)]
                       [perturbed-fp (vec-add fp perturbation)]
                       ;; Use robust Newton iteration - near bifurcations the Jacobian is nearly singular
                       [sys (instantiate-at psys param)]
                       [new-fp (find-fixed-point-robust sys perturbed-fp
                                                        *branch-switch-tolerance*
                                                        *continuation-step-size*
                                                        *branch-switch-max-iter*)])
                      (if (not new-fp)
                          #f  ; Newton failed
                          ;; Verify we're on a different branch (not back on the original)
                          (if (< (vec-norm (vec-sub new-fp fp)) (* 2 *branch-switch-perturbation*))
                              #f  ; Converged back to original - try larger perturbation
                              (cons new-fp param))))))))

(define (switch-branch-adaptive psys param fp direction)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Symbol (Option (Pair Vec Number))))
  (doc 'description "Switch to a new branch with adaptive perturbation size")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param "parameter value at or near the bifurcation")
  (doc 'param 'fp "fixed point at or near the bifurcation")
  (doc 'param 'direction "'upper or 'lower")
  (doc 'returns "(new-fixed-point . new-param) or #f")
  (doc 'note "Steps forward in parameter and perturbs state to find new branch.
For pitchfork: new branches only exist past the bifurcation, so we step forward in parameter.")
  ;; First try to find eigenvector at current point
  (let ([eigenvec (compute-critical-eigenvector psys param fp)])
       ;; If no zero eigenvalue at current point, try nearby parameter values
       (let* ([effective-eigenvec
               (or eigenvec
                   ;; Try to find eigenvector at nearby parameters
                   (let search-param ([offsets '(-0.01 0.01 -0.05 0.05)])
                        (if (null? offsets)
                            #f
                            (let ([ev (compute-critical-eigenvector psys (+ param (car offsets)) fp)])
                                 (or ev (search-param (cdr offsets))))))
                   ;; Fallback: use unit vector as perturbation direction
                   (let ([n (vector-length fp)])
                        (if (= n 1)
                            (vector 1.0)
                            #f)))])
             (if (not effective-eigenvec)
                 #f
                 (let ([sign (if (eq? direction 'upper) 1.0 -1.0)])
                      ;; Try stepping in parameter (both directions for sub/supercritical) AND perturbing state
                      (let try-combo ([param-steps '(0.0 0.01 -0.01 0.05 -0.05 0.1 -0.1)]
                                      [state-scales '(0.01 0.05 0.1 0.5 1.0)])
                           (if (null? state-scales)
                               #f
                               (let try-param ([psteps param-steps])
                                    (if (null? psteps)
                                        (try-combo param-steps (cdr state-scales))
                                        (let* ([dparam (car psteps)]
                                               [new-param (+ param dparam)]
                                               [scale (car state-scales)]
                                               [perturbation (vec-scale (* sign scale) effective-eigenvec)]
                                               [perturbed-fp (vec-add fp perturbation)]
                                               [sys (instantiate-at psys new-param)]
                                               ;; Track where the original branch moved at new-param
                                               ;; For large param steps, comparing to the original fp causes false positives
                                               [original-at-new-param (find-fixed-point-robust sys fp
                                                                                               *branch-switch-tolerance*
                                                                                               *continuation-step-size*
                                                                                               *branch-switch-max-iter*)]
                                               ;; Use robust solver - near bifurcations the Jacobian is nearly singular
                                               [new-fp (find-fixed-point-robust sys perturbed-fp
                                                                                *branch-switch-tolerance*
                                                                                *continuation-step-size*
                                                                                *branch-switch-max-iter*)])
                                              (cond
                                               [(not new-fp)
                                                (try-param (cdr psteps))]
                                               ;; If we couldn't track where original branch went, reject this step
                                               ;; Falling back to comparing to fp would re-introduce false positives
                                               [(not original-at-new-param)
                                                (try-param (cdr psteps))]
                                               ;; Did we find a genuinely different point?
                                               ;; Compare to where original branch is at new-param (not original fp)
                                               [(< (vec-norm (vec-sub new-fp original-at-new-param)) (* 0.1 scale))
                                                (try-param (cdr psteps))]
                                               [else
                                                (cons new-fp new-param)])))))))))))

(define (continue-switched-branch psys param fp num-steps step-size)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Nat Number (List (List Number Vec Symbol (List Complex)))))
  (doc 'description "Continue along a new branch after switching")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'param "parameter value (from switch-branch result)")
  (doc 'param 'fp "fixed point on the new branch (from switch-branch result)")
  (doc 'param 'num-steps "number of continuation steps")
  (doc 'param 'step-size "arc-length step (positive to continue away from bifurcation)")
  (doc 'returns "continuation data for the new branch")
  (continue-fixed-point-arclength psys param fp num-steps step-size))

(define (find-all-branches-at-bifurcation psys param fp)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec (List (Pair Symbol Vec))))
  (doc 'description "Find all fixed point branches at a bifurcation")
  (doc 'returns "list of (branch-label . fixed-point) pairs, suitable for assq lookup")
  (doc 'note "At pitchfork: returns 3 branches (original + upper + lower).
At transcritical: returns 2 branches that exchange stability.")
  (let* ([upper (switch-branch-adaptive psys param fp 'upper)]
         [lower (switch-branch-adaptive psys param fp 'lower)]
         [branches (list (cons 'original fp))])
        (let* ([with-upper (if (and upper (car upper))
                               (cons (cons 'upper (car upper)) branches)
                               branches)]
               [with-lower (if (and lower (car lower)
                                    ;; Check it's not the same as upper
                                    ;; Must compare at same parameter - if params differ, project lower to upper's param
                                    (or (not upper)
                                        (not (car upper))
                                        (let* ([upper-param (cdr upper)]
                                               [lower-param (cdr lower)])
                                              (if (< (abs (- upper-param lower-param)) 1e-10)
                                                  ;; Same param - compare directly
                                                  (> (vec-norm (vec-sub (car lower) (car upper))) 1e-6)
                                                  ;; Different params - project lower to upper's param and compare
                                                  (let* ([sys-at-upper (instantiate-at psys upper-param)]
                                                         [lower-projected (find-fixed-point-robust
                                                                           sys-at-upper (car lower)
                                                                           *branch-switch-tolerance*
                                                                           *continuation-step-size*
                                                                           *branch-switch-max-iter*)])
                                                        (or (not lower-projected)
                                                            (> (vec-norm (vec-sub lower-projected (car upper))) 1e-6)))))))
                               (cons (cons 'lower (car lower)) with-upper)
                               with-upper)])
              with-lower)))

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

(define (saddle-node-2d-param)
  (doc 'export #t)
  (doc 'type '(-> ParamODE))
  (doc 'description "2D saddle-node: dx/dt = r + x^2, dy/dt = -y")
  (doc 'note "Decoupled y-direction is always stable. Used for testing n>1 classification.")
  (make-parameterized-ode
   (lambda (r)
           (make-autonomous-ode
            (lambda (state)
                    (let ([x (vector-ref state 0)]
                          [y (vector-ref state 1)])
                         (vector (+ r (* x x))    ; Saddle-node in x
                                 (- y))))         ; Stable y
            2))))

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

;;; ============================================================
;;; Section: Automatic Bifurcation Diagram Generation
;;; ============================================================

(doc 'section 'automatic-bifurcation-diagrams)
(doc 'note "Automatic exploration of complete bifurcation diagrams")

;; Default parameters for diagram tracing
(define *diagram-default-max-steps* 200)
(define *diagram-grid-divisions* 100)
(define *diagram-collision-tolerance* 1e-4)

;; Work item for the exploration queue
(define (make-work-item id param fp direction)
  (list 'work-item id param fp direction))

(define (work-item? x)
  (and (pair? x) (eq? (car x) 'work-item)))

(define (work-item-id item) (cadr item))
(define (work-item-param item) (caddr item))
(define (work-item-fp item) (cadddr item))
(define (work-item-direction item) (car (cddddr item)))

;; Bifurcation diagram structure
(define (make-bifurcation-diagram branches bifurcations metadata)
  (list 'bifurcation-diagram branches bifurcations metadata))

(define (bifurcation-diagram? x)
  (and (pair? x) (eq? (car x) 'bifurcation-diagram)))

(define (diagram-branches diag)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram (List (Pair Symbol (List ...)))))
  (doc 'description "Get all branches from a bifurcation diagram")
  (cadr diag))

(define (diagram-bifurcations diag)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram (List ...)))
  (doc 'description "Get all bifurcation records from a diagram")
  (caddr diag))

(define (diagram-metadata diag)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram (List (Pair Symbol Any))))
  (doc 'description "Get metadata from a bifurcation diagram")
  (cadddr diag))

(define (diagram-branch diag id)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram Symbol (Option (List ...))))
  (doc 'description "Get a specific branch by ID")
  (let ([entry (assq id (diagram-branches diag))])
       (and entry (cdr entry))))

(define (diagram-bifurcations-of-type diag type)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram Symbol (List ...)))
  (doc 'description "Get bifurcations of a specific type (pitchfork, hopf, etc.)")
  (filter (lambda (b) (eq? (car b) type))
          (diagram-bifurcations diag)))

;; Spatial hash for collision detection
(define (make-spatial-hash p-min p-max fp-scale divisions)
  (let ([dp (/ (- p-max p-min) divisions)]
        [table (make-hashtable equal-hash equal?)])
       (list 'spatial-hash p-min dp fp-scale table)))

(define (spatial-hash-cell hash param fp)
  ;; Use all dimensions of fp for proper multi-dimensional collision detection
  (let* ([p-min (cadr hash)]
         [dp (caddr hash)]
         [fp-scale (cadddr hash)]
         [p-idx (inexact->exact (floor (/ (- param p-min) dp)))]
         [fp-indices (if (vector? fp)
                         ;; Create list of indices for each dimension
                         (let loop ([i 0] [acc '()])
                              (if (>= i (vector-length fp))
                                  (reverse acc)
                                  (loop (+ i 1)
                                        (cons (inexact->exact (floor (/ (vector-ref fp i) fp-scale)))
                                              acc))))
                         ;; Scalar case
                         (list (inexact->exact (floor (/ fp fp-scale)))))])
        (cons p-idx fp-indices)))

(define (spatial-hash-register! hash param fp branch-id)
  (let* ([table (car (cddddr hash))]
         [cell (spatial-hash-cell hash param fp)])
        (hashtable-set! table cell branch-id)))

(define (spatial-hash-lookup hash param fp)
  (let* ([table (car (cddddr hash))]
         [cell (spatial-hash-cell hash param fp)])
        (hashtable-ref table cell #f)))

(define (register-branch-points! hash branch-id points)
  (for-each (lambda (pt)
              (spatial-hash-register! hash (car pt) (cadr pt) branch-id))
            points))

;; Filter continuation data to bounds
(define (filter-within-bounds cont-data p-min p-max)
  (filter (lambda (pt)
            (let ([p (car pt)])
                 (and (>= p p-min) (<= p p-max))))
          cont-data))

;; Branch status tracking
(define (make-branch-record id data status parent-bif)
  (list id data status parent-bif))

(define (branch-record-id rec) (car rec))
(define (branch-record-data rec) (cadr rec))
(define (branch-record-status rec) (caddr rec))
(define (branch-record-parent rec) (cadddr rec))

;; Spawn new branches at bifurcations
(define (spawn-branches-at-bifurcation psys bif parent-id p-min p-max spatial-hash next-id)
  (let* ([bif-type (car bif)]
         [bif-param (cadr bif)]
         [bif-fp (caddr bif)]
         [results '()]
         [children '()])
        ;; Try upper direction
        (let ([upper (switch-branch-adaptive psys bif-param bif-fp 'upper)])
             (when (and upper (car upper))
                   (let* ([new-fp (car upper)]
                          [new-param (cdr upper)]
                          ;; Check for collision, but ignore if it's the parent branch
                          ;; (parent is registered nearby since we're at a bifurcation)
                          [existing (spatial-hash-lookup spatial-hash new-param new-fp)]
                          [collision-ok (or (not existing) (eq? existing parent-id))])
                         (when (and (>= new-param p-min) (<= new-param p-max) collision-ok)
                               (let ([new-id (string->symbol (format "branch-~a" next-id))])
                                    (set! results (cons (make-work-item new-id new-param new-fp
                                                                        (if (> new-param bif-param) 'forward 'backward))
                                                        results))
                                    (set! children (cons (cons new-id 'upper) children))
                                    (set! next-id (+ next-id 1)))))))
        ;; Try lower direction
        (let ([lower (switch-branch-adaptive psys bif-param bif-fp 'lower)])
             (when (and lower (car lower))
                   (let* ([new-fp (car lower)]
                          [new-param (cdr lower)]
                          ;; Check for collision, but ignore if it's the parent branch
                          [existing (spatial-hash-lookup spatial-hash new-param new-fp)]
                          [collision-ok (or (not existing) (eq? existing parent-id))])
                         (when (and (>= new-param p-min) (<= new-param p-max) collision-ok)
                               (let ([new-id (string->symbol (format "branch-~a" next-id))])
                                    (set! results (cons (make-work-item new-id new-param new-fp
                                                                        (if (> new-param bif-param) 'forward 'backward))
                                                        results))
                                    (set! children (cons (cons new-id 'lower) children))
                                    (set! next-id (+ next-id 1)))))))
        (list results children next-id)))

;; Main entry point
(define (trace-bifurcation-diagram psys start-param start-fp p-min p-max . opts)
  (doc 'export #t)
  (doc 'type '(-> ParamODE Number Vec Number Number ... BifurcationDiagram))
  (doc 'description "Automatically trace a complete bifurcation diagram")
  (doc 'param 'psys "parameterized ODE system")
  (doc 'param 'start-param "initial parameter value")
  (doc 'param 'start-fp "initial fixed point")
  (doc 'param 'p-min "minimum parameter value")
  (doc 'param 'p-max "maximum parameter value")
  (doc 'param 'opts "optional: max-steps-per-branch (default 200)")
  (doc 'returns "bifurcation diagram with all branches and bifurcation records")
  (let* ([max-steps (if (null? opts) *diagram-default-max-steps* (car opts))]
         [base-step (/ (- p-max p-min) max-steps)]
         [fp-scale (max 0.1 (vec-norm start-fp))]  ; Scale for spatial hash
         [spatial-hash (make-spatial-hash p-min p-max fp-scale *diagram-grid-divisions*)]
         [branches '()]
         [bifurcations '()]
         [next-branch-id 1]
         [total-steps 0]
         ;; Start with initial branch going both directions
         [queue (list (make-work-item 'branch-0 start-param start-fp 'forward)
                      (make-work-item 'branch-0-back start-param start-fp 'backward))])

        ;; Process queue
        (let loop ([q queue])
             (if (null? q)
                 ;; Done - build result
                 (make-bifurcation-diagram
                  (reverse branches)
                  (reverse bifurcations)
                  `((param-range . (,p-min . ,p-max))
                    (total-steps . ,total-steps)
                    (branch-count . ,(length branches))))

                 ;; Process next work item
                 (let* ([item (car q)]
                        [branch-id (work-item-id item)]
                        [p-start (work-item-param item)]
                        [fp-start (work-item-fp item)]
                        [dir (work-item-direction item)]
                        [step (if (eq? dir 'forward) base-step (- base-step))])

                       ;; Continue this branch
                       (let* ([cont-result (continue-fixed-point-arclength-adaptive
                                            psys p-start fp-start max-steps step)]
                              [cont-data (if (null? cont-result) '() cont-result)]
                              [filtered (filter-within-bounds cont-data p-min p-max)]
                              [status (cond
                                       [(null? filtered) 'failed]
                                       [(< (length filtered) 3) 'stalled]
                                       [else 'complete])])

                             ;; Register points in spatial hash
                             (register-branch-points! spatial-hash branch-id filtered)

                             ;; Add branch to results
                             (set! branches (cons (cons branch-id filtered) branches))
                             (set! total-steps (+ total-steps (length filtered)))

                             ;; Detect bifurcations if we have enough data
                             (let* ([bifs (if (>= (length filtered) 2)
                                              (detect-bifurcations filtered psys)
                                              '())]
                                    [new-work '()])

                                   ;; Process each bifurcation
                                   (for-each
                                    (lambda (bif)
                                      (let* ([spawn-result (spawn-branches-at-bifurcation
                                                           psys bif branch-id p-min p-max
                                                           spatial-hash next-branch-id)]
                                             [new-items (car spawn-result)]
                                             [children (cadr spawn-result)]
                                             [new-next-id (caddr spawn-result)])
                                            ;; Record bifurcation
                                            (set! bifurcations
                                                  (cons (list (car bif)      ; type
                                                              (cadr bif)     ; param
                                                              (caddr bif)    ; fp
                                                              branch-id      ; parent
                                                              children)      ; children with directions
                                                        bifurcations))
                                            (set! new-work (append new-work new-items))
                                            (set! next-branch-id new-next-id)))
                                    bifs)

                                   ;; Continue with remaining queue
                                   (loop (append (cdr q) new-work)))))))))

;; Helper to check stability
(define (stability-is-stable? stab)
  (memq stab '(stable stable-node stable-focus stable-spiral)))

(define (stability-is-unstable? stab)
  (memq stab '(unstable unstable-node unstable-focus unstable-spiral saddle)))

;; Query functions
(define (diagram-stable-points diag)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram (List (List Number Vec))))
  (doc 'description "Get all stable points from all branches")
  (append-map (lambda (branch)
                (filter (lambda (pt) (stability-is-stable? (caddr pt)))
                        (cdr branch)))
              (diagram-branches diag)))

(define (diagram-unstable-points diag)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram (List (List Number Vec))))
  (doc 'description "Get all unstable points from all branches")
  (append-map (lambda (branch)
                (filter (lambda (pt) (stability-is-unstable? (caddr pt)))
                        (cdr branch)))
              (diagram-branches diag)))

(define (diagram-points-at-param diag p tol)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram Number Number (List (List Symbol Vec Symbol))))
  (doc 'description "Get all fixed points at a given parameter value (within tolerance)")
  (doc 'returns "list of (branch-id fixed-point stability)")
  (append-map (lambda (branch)
                (let ([id (car branch)]
                      [matches (filter (lambda (pt)
                                        (< (abs (- (car pt) p)) tol))
                                       (cdr branch))])
                     (map (lambda (pt) (list id (cadr pt) (caddr pt)))
                          matches)))
              (diagram-branches diag)))

(define (diagram-summary diag)
  (doc 'export #t)
  (doc 'type '(-> BifurcationDiagram (List (Pair Symbol Any))))
  (doc 'description "Get summary statistics for a bifurcation diagram")
  (let* ([branches (diagram-branches diag)]
         [bifs (diagram-bifurcations diag)]
         [meta (diagram-metadata diag)]
         [total-points (apply + (map (lambda (b) (length (cdr b))) branches))]
         [bif-types (map car bifs)]
         [type-counts (let loop ([types bif-types] [counts '()])
                           (if (null? types)
                               counts
                               (let* ([t (car types)]
                                      [entry (assq t counts)])
                                     (loop (cdr types)
                                           (if entry
                                               (map (lambda (c)
                                                      (if (eq? (car c) t)
                                                          (cons t (+ (cdr c) 1))
                                                          c))
                                                    counts)
                                               (cons (cons t 1) counts))))))])
        `((branch-count . ,(length branches))
          (total-points . ,total-points)
          (bifurcation-count . ,(length bifs))
          (bifurcation-types . ,type-counts)
          ,@meta)))
