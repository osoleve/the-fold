(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/diffgeo/curvature.ss")

(doc 'module 'geodesics)
(doc 'description "Geodesic Computation - Geodesic curves on Riemannian manifolds")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Provides: Geodesic tracing, exponential map, logarithm map, parallel transport, distance computation")
(doc 'note "A geodesic is a curve γ(t) that parallel transports its own tangent vector")
(doc 'note "Geodesic equation: d²x^k/dt² + Γ^k_{ij} (dx^i/dt)(dx^j/dt) = 0")
(doc 'note "Exponential map exp_p : T_p M → M shoots geodesic from point with initial velocity")
(doc 'note "Logarithm map log_p : M → T_p M is the (local) inverse of exp_p")

(doc 'section 'configuration)

(doc *geodesic-epsilon* 'description "For Christoffel symbol computation")
(define *geodesic-epsilon* 1e-7)

(doc *geodesic-tolerance* 'description "Convergence tolerance for log map")
(define *geodesic-tolerance* 1e-9)

(doc *geodesic-max-iterations* 'description "Max iterations for log map shooting")
(define *geodesic-max-iterations* 50)

(doc 'section 'geodesic-state)
(doc 'note "A geodesic state bundles position and velocity: (geodesic-state coords velocity)")
(doc 'note "This represents a point in the tangent bundle TM")

(define (make-geodesic-state coords velocity)
  (doc 'export #t)
  (list 'geodesic-state coords velocity))

(define (geodesic-state? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'geodesic-state)))

(define (geodesic-state-coords s) (list-ref s 1))
(doc 'export #t)
(define (geodesic-state-velocity s) (list-ref s 2))
(doc 'export #t)

;;; ============================================================================
;;; Geodesic ODE
;;; ============================================================================

;;; The geodesic equation d²x^k/dt² + Γ^k_{ij} v^i v^j = 0
;;; is converted to a first-order system:
;;;   dx^k/dt = v^k
;;;   dv^k/dt = -Γ^k_{ij} v^i v^j

;;; geodesic-acceleration : Metric × Vec × Vec → Vec
;;; Compute the geodesic acceleration: a^k = -Γ^k_{ij} v^i v^j
;;; Uses cached Christoffel symbols when possible.
(define (geodesic-acceleration metric coords velocity)
  (doc 'export #t)
  (let* ([n (metric-dim metric)]
         [gamma (cached-christoffel-symbols metric coords *geodesic-epsilon*)]
         [accel (make-vector n 0)])
    ;; a^k = -Σ_{ij} Γ^k_{ij} v^i v^j
    (do ([k 0 (+ k 1)])
        ((= k n) accel)
      (let ([sum 0])
        (do ([i 0 (+ i 1)])
            ((= i n))
          (do ([j 0 (+ j 1)])
              ((= j n))
            (set! sum (+ sum (* (christoffel-ref gamma k i j)
                                (vector-ref velocity i)
                                (vector-ref velocity j))))))
        (vector-set! accel k (- sum))))))

;;; geodesic-derivative : Metric × GeodesicState → (Vec . Vec)
;;; Compute the derivative of the geodesic state (for RK4 integration).
;;; Returns (dx/dt . dv/dt) = (v . -Γv²).
(define (geodesic-derivative metric state)
  (doc 'export #t)
  (let ([coords (geodesic-state-coords state)]
        [velocity (geodesic-state-velocity state)])
    (cons velocity (geodesic-acceleration metric coords velocity))))

;;; ============================================================================
;;; Numerical Integration (RK4)
;;; ============================================================================

;;; rk4-geodesic-step : Metric × GeodesicState × Num → GeodesicState
;;; Take one RK4 step of size dt.
(define (rk4-geodesic-step metric state dt)
  (doc 'export #t)
  (let* ([x0 (geodesic-state-coords state)]
         [v0 (geodesic-state-velocity state)]
         [n (vector-length x0)]

         ;; k1
         [d1 (geodesic-derivative metric state)]
         [dx1 (car d1)]
         [dv1 (cdr d1)]

         ;; k2: state at t + dt/2 using k1
         [x2 (vec-add x0 (vec-scale (* 0.5 dt) dx1))]
         [v2 (vec-add v0 (vec-scale (* 0.5 dt) dv1))]
         [d2 (geodesic-derivative metric (make-geodesic-state x2 v2))]
         [dx2 (car d2)]
         [dv2 (cdr d2)]

         ;; k3: state at t + dt/2 using k2
         [x3 (vec-add x0 (vec-scale (* 0.5 dt) dx2))]
         [v3 (vec-add v0 (vec-scale (* 0.5 dt) dv2))]
         [d3 (geodesic-derivative metric (make-geodesic-state x3 v3))]
         [dx3 (car d3)]
         [dv3 (cdr d3)]

         ;; k4: state at t + dt using k3
         [x4 (vec-add x0 (vec-scale dt dx3))]
         [v4 (vec-add v0 (vec-scale dt dv3))]
         [d4 (geodesic-derivative metric (make-geodesic-state x4 v4))]
         [dx4 (car d4)]
         [dv4 (cdr d4)]

         ;; Combine: x_new = x0 + (dt/6)(k1 + 2k2 + 2k3 + k4)
         [dx-weighted (vec-scale (/ dt 6.0)
                        (vec-add dx1
                          (vec-add (vec-scale 2 dx2)
                            (vec-add (vec-scale 2 dx3) dx4))))]
         [dv-weighted (vec-scale (/ dt 6.0)
                        (vec-add dv1
                          (vec-add (vec-scale 2 dv2)
                            (vec-add (vec-scale 2 dv3) dv4))))]

         [x-new (vec-add x0 dx-weighted)]
         [v-new (vec-add v0 dv-weighted)])

    (make-geodesic-state x-new v-new)))

;;; ============================================================================
;;; Geodesic Tracing
;;; ============================================================================

(doc trace-geodesic 'type '(-> Metric Vec Vec Num Nat (List GeodesicState)))
(doc trace-geodesic 'description "Trace a geodesic from initial position with initial velocity for time T")
(doc trace-geodesic 'returns "List of states sampled at n-steps+1 points (including start/end)")
(define (trace-geodesic metric initial-coords initial-velocity T n-steps)
  (doc 'export #t)
  (let* ([dt (/ T n-steps)]
         [state0 (make-geodesic-state initial-coords initial-velocity)])
    (let loop ([state state0] [i 0] [states (list state0)])
      (if (>= i n-steps)
          (reverse states)
          (let ([next-state (rk4-geodesic-step metric state dt)])
            (loop next-state (+ i 1) (cons next-state states)))))))

;;; trace-geodesic-final : Metric × Vec × Vec × Num × Nat → GeodesicState
;;; Trace a geodesic and return only the final state.
(define (trace-geodesic-final metric initial-coords initial-velocity T n-steps)
  (doc 'export #t)
  (let* ([dt (/ T n-steps)]
         [state (make-geodesic-state initial-coords initial-velocity)])
    (let loop ([state state] [i 0])
      (if (>= i n-steps)
          state
          (loop (rk4-geodesic-step metric state dt) (+ i 1))))))

;;; ============================================================================
;;; Exponential Map
;;; ============================================================================

(doc exp-map 'type '(-> Metric Vec Vec (Optional Nat) Vec))
(doc exp-map 'description "Compute the exponential map: exp_p(v) = geodesic from p with velocity v at t=1")
(doc exp-map 'param "n-steps: optional integration steps (default 100)")
(define (exp-map metric base-coords tangent-vec . opts)
  (doc 'export #t)
  (let ([n-steps (if (null? opts) 100 (car opts))])
    (geodesic-state-coords
     (trace-geodesic-final metric base-coords tangent-vec 1.0 n-steps))))

;;; exp-map-t : Metric × Vec × Vec × Num × [Nat] → Vec
;;; Exponential map evaluated at time t (not just t=1).
(define (exp-map-t metric base-coords tangent-vec t . opts)
  (doc 'export #t)
  (let ([n-steps (if (null? opts) 100 (car opts))])
    (geodesic-state-coords
     (trace-geodesic-final metric base-coords tangent-vec t n-steps))))

;;; ============================================================================
;;; Logarithm Map (Shooting Method)
;;; ============================================================================

;;; The logarithm map log_p(q) finds the initial velocity v such that exp_p(v) = q.
;;; This is solved via shooting: iteratively adjust v to hit the target q.
;;;
;;; We use a Newton-like iteration:
;;;   1. Shoot with current guess v
;;;   2. Compute error e = exp_p(v) - q
;;;   3. Estimate Jacobian J = d(exp_p)/dv numerically
;;;   4. Update v ← v - J^{-1} e

(doc log-map 'type '(-> Metric Vec Vec (Optional Nat) (Optional Num) (Optional Nat) (Or (Ok Vec) (Err String))))
(doc log-map 'description "Compute the logarithm map: find v such that exp_p(v) = q")
(doc log-map 'returns "(ok v) on success or (err message) if it fails to converge")
(doc log-map 'param "n-steps: integration steps for exp (default 100)")
(doc log-map 'param "tol: convergence tolerance (default *geodesic-tolerance*)")
(doc log-map 'param "max-iter: maximum iterations (default *geodesic-max-iterations*)")
(define (log-map metric p q . opts)
  (doc 'export #t)
  (let* ([n-steps (if (null? opts) 100 (car opts))]
         [tol (if (or (null? opts) (null? (cdr opts)))
                  *geodesic-tolerance*
                  (cadr opts))]
         [max-iter (if (or (null? opts) (null? (cdr opts)) (null? (cddr opts)))
                       *geodesic-max-iterations*
                       (caddr opts))]
         [n (vector-length p)]
         ;; Initial guess: straight-line velocity (works for small distances)
         [v0 (vec-sub q p)]
         [eps 1e-6])  ; For numerical Jacobian

    (let loop ([v v0] [iter 0])
      (if (>= iter max-iter)
          (list 'err "log-map: failed to converge")
          (let* ([q-shot (exp-map metric p v n-steps)]
                 [error (vec-sub q-shot q)]
                 [error-norm (vec-norm error)])
            (if (< error-norm tol)
                (list 'ok v)
                ;; Compute numerical Jacobian d(exp_p)/dv
                (let ([J (compute-exp-jacobian metric p v n-steps eps n)])
                  ;; Solve J * dv = -error for dv
                  (let ([dv (solve-linear-system J (vec-scale -1 error))])
                    (if dv
                        (loop (vec-add v dv) (+ iter 1))
                        ;; Jacobian singular, try gradient descent step
                        (loop (vec-sub v (vec-scale 0.1 error)) (+ iter 1)))))))))))

;;; compute-exp-jacobian : Metric × Vec × Vec × Nat × Num × Nat → Matrix
;;; Compute the Jacobian matrix of exp_p with respect to v.
(define (compute-exp-jacobian metric p v n-steps eps n)
  (let ([J (make-matrix n n 0)]
        [v-plus (vec-copy v)]
        [v-minus (vec-copy v)])
    (do ([j 0 (+ j 1)])
        ((= j n) J)
      (let ([vj (vector-ref v j)])
        ;; Perturb v[j]
        (vector-set! v-plus j (+ vj eps))
        (vector-set! v-minus j (- vj eps))
        (let* ([q-plus (exp-map metric p v-plus n-steps)]
               [q-minus (exp-map metric p v-minus n-steps)])
          ;; J[i][j] = d(exp_i)/d(v_j)
          (do ([i 0 (+ i 1)])
              ((= i n))
            (matrix-set! J i j
              (/ (- (vector-ref q-plus i) (vector-ref q-minus i))
                 (* 2 eps)))))
        ;; Reset
        (vector-set! v-plus j vj)
        (vector-set! v-minus j vj)))))

;;; solve-linear-system : Matrix × Vec → Vec | #f
;;; Solve Ax = b using Gaussian elimination with partial pivoting.
;;; Returns #f if the matrix is singular.
(define (solve-linear-system A b)
  (let* ([n (vector-length b)]
         ;; Create augmented matrix [A|b]
         [aug (make-vector n #f)])
    ;; Initialize augmented matrix
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([row (make-vector (+ n 1) 0)])
        (do ([j 0 (+ j 1)])
            ((= j n))
          (vector-set! row j (matrix-ref A i j)))
        (vector-set! row n (vector-ref b i))
        (vector-set! aug i row)))

    ;; Forward elimination with partial pivoting
    (let forward ([k 0])
      (if (>= k n)
          ;; Back substitution
          (let ([x (make-vector n 0)])
            (let back ([i (- n 1)])
              (if (< i 0)
                  x
                  (let ([row (vector-ref aug i)]
                        [sum (vector-ref row n)])
                    (do ([j (+ i 1) (+ j 1)])
                        ((= j n))
                      (set! sum (- sum (* (vector-ref row j) (vector-ref x j)))))
                    (let ([pivot (vector-ref row i)])
                      (if (< (abs pivot) 1e-15)
                          #f  ; Singular
                          (begin
                            (vector-set! x i (/ sum pivot))
                            (back (- i 1)))))))))
          ;; Find pivot
          (let* ([max-row k]
                 [max-val (abs (vector-ref (vector-ref aug k) k))])
            (do ([i (+ k 1) (+ i 1)])
                ((= i n))
              (let ([val (abs (vector-ref (vector-ref aug i) k))])
                (when (> val max-val)
                  (set! max-val val)
                  (set! max-row i))))
            (if (< max-val 1e-15)
                #f  ; Singular
                (begin
                  ;; Swap rows
                  (when (not (= k max-row))
                    (let ([tmp (vector-ref aug k)])
                      (vector-set! aug k (vector-ref aug max-row))
                      (vector-set! aug max-row tmp)))
                  ;; Eliminate
                  (let ([pivot-row (vector-ref aug k)]
                        [pivot (vector-ref (vector-ref aug k) k)])
                    (do ([i (+ k 1) (+ i 1)])
                        ((= i n))
                      (let* ([row (vector-ref aug i)]
                             [factor (/ (vector-ref row k) pivot)])
                        (do ([j k (+ j 1)])
                            ((= j (+ n 1)))
                          (vector-set! row j
                            (- (vector-ref row j)
                               (* factor (vector-ref pivot-row j))))))))
                  (forward (+ k 1)))))))))

;;; ============================================================================
;;; Parallel Transport
;;; ============================================================================

;;; Parallel transport moves a vector along a curve while keeping it "parallel"
;;; according to the connection. The parallel transport equation is:
;;;   dV^k/dt + Γ^k_{ij} (dx^i/dt) V^j = 0
;;;
;;; We solve this ODE alongside the geodesic equation.

;;; parallel-transport-derivative : Metric × Vec × Vec × Vec → Vec
;;; Compute dV^k/dt = -Γ^k_{ij} (dx^i/dt) V^j for a vector V along a curve
;;; with tangent vector (velocity).
;;; Uses cached Christoffel symbols when possible.
(define (parallel-transport-derivative metric coords velocity V)
  (let* ([n (metric-dim metric)]
         [gamma (cached-christoffel-symbols metric coords *geodesic-epsilon*)]
         [dV (make-vector n 0)])
    ;; dV^k/dt = -Σ_{ij} Γ^k_{ij} v^i V^j
    (do ([k 0 (+ k 1)])
        ((= k n) dV)
      (let ([sum 0])
        (do ([i 0 (+ i 1)])
            ((= i n))
          (do ([j 0 (+ j 1)])
              ((= j n))
            (set! sum (+ sum (* (christoffel-ref gamma k i j)
                                (vector-ref velocity i)
                                (vector-ref V j))))))
        (vector-set! dV k (- sum))))))

;;; parallel-transport-step : Metric × GeodesicState × Vec × Num → (GeodesicState . Vec)
;;; Take one RK4 step for both geodesic and parallel transport.
(define (parallel-transport-step metric state V dt)
  (let* ([x0 (geodesic-state-coords state)]
         [v0 (geodesic-state-velocity state)]
         [n (vector-length x0)]

         ;; k1 for geodesic and transport
         [d1 (geodesic-derivative metric state)]
         [dx1 (car d1)]
         [dv1 (cdr d1)]
         [dV1 (parallel-transport-derivative metric x0 v0 V)]

         ;; k2
         [x2 (vec-add x0 (vec-scale (* 0.5 dt) dx1))]
         [v2 (vec-add v0 (vec-scale (* 0.5 dt) dv1))]
         [V2 (vec-add V (vec-scale (* 0.5 dt) dV1))]
         [d2 (geodesic-derivative metric (make-geodesic-state x2 v2))]
         [dx2 (car d2)]
         [dv2 (cdr d2)]
         [dV2 (parallel-transport-derivative metric x2 v2 V2)]

         ;; k3
         [x3 (vec-add x0 (vec-scale (* 0.5 dt) dx2))]
         [v3 (vec-add v0 (vec-scale (* 0.5 dt) dv2))]
         [V3 (vec-add V (vec-scale (* 0.5 dt) dV2))]
         [d3 (geodesic-derivative metric (make-geodesic-state x3 v3))]
         [dx3 (car d3)]
         [dv3 (cdr d3)]
         [dV3 (parallel-transport-derivative metric x3 v3 V3)]

         ;; k4
         [x4 (vec-add x0 (vec-scale dt dx3))]
         [v4 (vec-add v0 (vec-scale dt dv3))]
         [V4 (vec-add V (vec-scale dt dV3))]
         [d4 (geodesic-derivative metric (make-geodesic-state x4 v4))]
         [dx4 (car d4)]
         [dv4 (cdr d4)]
         [dV4 (parallel-transport-derivative metric x4 v4 V4)]

         ;; Combine
         [x-new (vec-add x0 (vec-scale (/ dt 6.0)
                             (vec-add dx1 (vec-add (vec-scale 2 dx2)
                               (vec-add (vec-scale 2 dx3) dx4)))))]
         [v-new (vec-add v0 (vec-scale (/ dt 6.0)
                             (vec-add dv1 (vec-add (vec-scale 2 dv2)
                               (vec-add (vec-scale 2 dv3) dv4)))))]
         [V-new (vec-add V (vec-scale (/ dt 6.0)
                            (vec-add dV1 (vec-add (vec-scale 2 dV2)
                              (vec-add (vec-scale 2 dV3) dV4)))))])

    (cons (make-geodesic-state x-new v-new) V-new)))

;;; parallel-transport : Metric × Vec × Vec × Vec × Num × [Nat] → Vec
;;; Parallel transport vector V from point p along the geodesic with
;;; initial velocity tangent-vec for time T.
;;; Returns the transported vector at the endpoint.
(define (parallel-transport metric p tangent-vec V T . opts)
  (doc 'export #t)
  (let* ([n-steps (if (null? opts) 100 (car opts))]
         [dt (/ T n-steps)]
         [state (make-geodesic-state p tangent-vec)])
    (let loop ([state state] [V V] [i 0])
      (if (>= i n-steps)
          V
          (let ([result (parallel-transport-step metric state V dt)])
            (loop (car result) (cdr result) (+ i 1)))))))

;;; parallel-transport-along-geodesic : Metric × Vec × Vec × Vec × [Nat] → Vec
;;; Parallel transport V from p to exp_p(tangent-vec).
;;; Shorthand for parallel-transport with T=1.
(define (parallel-transport-along-geodesic metric p tangent-vec V . opts)
  (doc 'export #t)
  (apply parallel-transport metric p tangent-vec V 1.0 opts))

;;; ============================================================================
;;; Geodesic Distance
;;; ============================================================================

;;; geodesic-distance : Metric × Vec × Vec × [Nat × Num × Nat] → Num | (Err String)
;;; Compute the geodesic distance between two points.
;;; This is the length of the shortest geodesic connecting them.
;;;
;;; Algorithm:
;;;   1. Find initial velocity v = log_p(q)
;;;   2. Distance = ||v||_g = sqrt(g(v,v))
(define (geodesic-distance metric p q . opts)
  (doc 'export #t)
  (let ([result (apply log-map metric p q opts)])
    (if (and (pair? result) (eq? (car result) 'ok))
        (let ([v (cadr result)])
          (metric-norm metric p v))
        result)))  ; Return the error

;;; geodesic-length : Metric × (List GeodesicState) → Num
;;; Compute the arc length of a traced geodesic by numerical integration.
(define (geodesic-length metric states)
  (doc 'export #t)
  (if (or (null? states) (null? (cdr states)))
      0
      (let loop ([states states] [length 0])
        (if (null? (cdr states))
            length
            (let* ([s1 (car states)]
                   [s2 (cadr states)]
                   [x1 (geodesic-state-coords s1)]
                   [x2 (geodesic-state-coords s2)]
                   [dx (vec-sub x2 x1)]
                   ;; Use metric at midpoint for better accuracy
                   [x-mid (vec-scale 0.5 (vec-add x1 x2))]
                   [ds (metric-norm metric x-mid dx)])
              (loop (cdr states) (+ length ds)))))))

;;; ============================================================================
;;; Geodesic Interpolation
;;; ============================================================================

;;; geodesic-interpolate : Metric × Vec × Vec × Num × [Nat × Num × Nat] → Vec | (Err String)
;;; Interpolate between two points along the geodesic.
;;; t=0 gives p, t=1 gives q.
;;; Returns the interpolated point on success, or (err message) if log-map fails.
(define (geodesic-interpolate metric p q t . opts)
  (doc 'export #t)
  (let ([result (apply log-map metric p q opts)])
    (if (and (pair? result) (eq? (car result) 'ok))
        (let* ([v (cadr result)]
               [n-steps (if (null? opts) 100 (car opts))])
          (exp-map-t metric p v t n-steps))
        result)))  ; Propagate error instead of silent fallback

;;; ============================================================================
;;; Utilities
;;; ============================================================================

;;; geodesic-spray : Metric × Vec × Nat × Num × Nat → (List Vec)
;;; Shoot geodesics in n-rays evenly-spaced directions from p.
;;; Returns the endpoints after traveling distance r.
;;; Useful for visualizing the exponential map.
(define (geodesic-spray metric p n-rays radius n-steps)
  (doc 'export #t)
  (let ([dim (metric-dim metric)])
    (if (not (= dim 2))
        (error 'geodesic-spray "only 2D supported for now")
        (let ([endpoints '()])
          (do ([i 0 (+ i 1)])
              ((= i n-rays) (reverse endpoints))
            (let* ([angle (* 2 3.141592653589793 (/ i n-rays))]
                   [v (vector (* radius (cos angle)) (* radius (sin angle)))]
                   [endpoint (exp-map metric p v n-steps)])
              (set! endpoints (cons endpoint endpoints))))))))

;;; ============================================================================
;;; Christoffel Symbol Caching
;;; ============================================================================

;;; For repeated geodesic computations, we cache Christoffel symbols.
;;; This is especially useful during RK4 integration where intermediate
;;; stages evaluate at nearby coordinates.
;;;
;;; Cache invalidation strategy:
;;;   - Different metric object → invalidate
;;;   - Coords differ by more than tolerance → invalidate
;;;
;;; The tolerance is set larger than the epsilon used for numerical
;;; differentiation in christoffel-symbols, but small enough to maintain
;;; accuracy during geodesic integration.

(define *christoffel-cache* #f)
(define *christoffel-cache-coords* #f)
(define *christoffel-cache-metric* #f)
(define *christoffel-cache-epsilon* #f)
(define *christoffel-cache-tolerance* 1e-3)  ; Cache hit tolerance (sized for RK4 stages)
(define *christoffel-cache-hits* 0)
(define *christoffel-cache-misses* 0)

(define (clear-christoffel-cache!)
  (doc 'export #t)
  (set! *christoffel-cache* #f)
  (set! *christoffel-cache-coords* #f)
  (set! *christoffel-cache-metric* #f)
  (set! *christoffel-cache-epsilon* #f))

(define (reset-christoffel-cache-stats!)
  (doc 'export #t)
  (set! *christoffel-cache-hits* 0)
  (set! *christoffel-cache-misses* 0))

(define (christoffel-cache-stats)
  (doc 'export #t)
  (let ([total (+ *christoffel-cache-hits* *christoffel-cache-misses*)])
    (list 'hits *christoffel-cache-hits*
          'misses *christoffel-cache-misses*
          'hit-rate (if (> total 0)
                        (/ *christoffel-cache-hits* total)
                        0))))

;;; coords-close? : Vec × Vec × Num → Bool
;;; Check if two coordinate vectors are within tolerance (L∞ norm).
(define (coords-close? c1 c2 tol)
  (let ([n1 (vector-length c1)]
        [n2 (vector-length c2)])
    (and (= n1 n2)  ; Dimension must match
         (let loop ([i 0])
           (if (>= i n1)
               #t
               (if (> (abs (- (vector-ref c1 i) (vector-ref c2 i))) tol)
                   #f
                   (loop (+ i 1))))))))

;;; cached-christoffel-symbols : Metric × Vec × Num → ChristoffelTensor
;;; Return Christoffel symbols, using cache when possible.
;;; Cache key includes: metric identity, epsilon value, and coordinate proximity.
(define (cached-christoffel-symbols metric coords epsilon)
  (doc 'export #t)
  (if (and *christoffel-cache*
           (eq? metric *christoffel-cache-metric*)
           (= epsilon *christoffel-cache-epsilon*)  ; Epsilon must match exactly
           *christoffel-cache-coords*
           (coords-close? coords *christoffel-cache-coords* *christoffel-cache-tolerance*))
      ;; Cache hit
      (begin
        (set! *christoffel-cache-hits* (+ *christoffel-cache-hits* 1))
        *christoffel-cache*)
      ;; Cache miss - compute and store
      (let ([gamma (christoffel-symbols metric coords epsilon)])
        (set! *christoffel-cache-misses* (+ *christoffel-cache-misses* 1))
        (set! *christoffel-cache* gamma)
        (set! *christoffel-cache-coords* (vec-copy coords))
        (set! *christoffel-cache-metric* metric)
        (set! *christoffel-cache-epsilon* epsilon)
        gamma)))

;;; ============================================================================
;;; REPL Interface
;;; ============================================================================

(printf "geodesics.ss loaded — Geodesic Computation\n")
(printf "  Geodesic Tracing:\n")
(printf "    (trace-geodesic metric p v T n)       - Trace geodesic for time T\n")
(printf "    (trace-geodesic-final metric p v T n) - Final state only\n")
(printf "  Exponential Map:\n")
(printf "    (exp-map metric p v [n-steps])        - exp_p(v) at t=1\n")
(printf "    (exp-map-t metric p v t [n-steps])    - exp_p(v) at time t\n")
(printf "  Logarithm Map:\n")
(printf "    (log-map metric p q [n tol max])      - Find v: exp_p(v)=q\n")
(printf "  Parallel Transport:\n")
(printf "    (parallel-transport metric p v V T)   - Transport V along geodesic\n")
(printf "  Distance:\n")
(printf "    (geodesic-distance metric p q)        - Geodesic distance\n")
(printf "    (geodesic-length metric states)       - Arc length of path\n")
(printf "  Interpolation:\n")
(printf "    (geodesic-interpolate metric p q t)   - Interpolate along geodesic\n")
(printf "  Visualization:\n")
(printf "    (geodesic-spray metric p n r steps)   - Shoot rays from p\n")
(printf "  Caching:\n")
(printf "    (clear-christoffel-cache!)            - Clear symbol cache\n")
(printf "    (christoffel-cache-stats)             - Show cache hit/miss stats\n")
