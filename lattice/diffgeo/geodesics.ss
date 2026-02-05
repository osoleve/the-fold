(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/diffgeo/curvature.ss")
(load "lattice/data/sort.ss")

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
;;; Adaptive Step-Size Integration (Dormand-Prince RK5(4))
;;; ============================================================================

(doc 'section 'adaptive-integration)

(doc 'note "Dormand-Prince RK5(4) method provides embedded error estimation")
(doc 'note "Computes both 4th and 5th order solutions; difference estimates local error")
(doc 'note "Adaptive stepping: grow step when error small, shrink when large")

;; Dormand-Prince coefficients
;; c_i (time nodes)
(define dp-c '#(0 1/5 3/10 4/5 8/9 1 1))

;; a_ij (Butcher tableau lower triangle)
(define dp-a
  '#(#()
     #(1/5)
     #(3/40 9/40)
     #(44/45 -56/15 32/9)
     #(19372/6561 -25360/2187 64448/6561 -212/729)
     #(9017/3168 -355/33 46732/5247 49/176 -5103/18656)
     #(35/384 0 500/1113 125/192 -2187/6784 11/84)))

;; b_i (5th order weights)
(define dp-b5 '#(35/384 0 500/1113 125/192 -2187/6784 11/84 0))

;; b*_i (4th order weights for error estimation)
(define dp-b4 '#(5179/57600 0 7571/16695 393/640 -92097/339200 187/2100 1/40))

;; Safety factor and step bounds for adaptive control
(define *adaptive-safety* 0.9)
(define *adaptive-min-scale* 0.2)  ; Don't shrink by more than 5x
(define *adaptive-max-scale* 5.0)  ; Don't grow by more than 5x

;;; dp-geodesic-step : Metric × GeodesicState × Num × [k1-cache] → (GeodesicState × GeodesicState × Num × k7)
;;; Single Dormand-Prince step returning both 5th and 4th order results.
;;; Returns (y5, y4, err-norm, k7) where:
;;;   - err-norm = ||y5 - y4||
;;;   - k7 = derivative at (x5, v5), which can be reused as k1 of next step (FSAL)
;;;
;;; FSAL optimization: If k1-cache is provided as (kx1 . kv1), use it instead of
;;; recomputing the derivative. This saves one derivative evaluation per step.
(define (dp-geodesic-step metric state dt . k1-cache)
  (let* ([x0 (geodesic-state-coords state)]
         [v0 (geodesic-state-velocity state)]
         [n (vector-length x0)]
         ;; Store k values for position and velocity
         [kx (make-vector 7 #f)]
         [kv (make-vector 7 #f)])

    ;; FSAL: Use cached k1 if provided, otherwise compute fresh
    (if (and (not (null? k1-cache)) (car k1-cache))
        (let ([cached (car k1-cache)])
          (vector-set! kx 0 (car cached))
          (vector-set! kv 0 (cdr cached)))
        (let ([d1 (geodesic-derivative metric state)])
          (vector-set! kx 0 (car d1))
          (vector-set! kv 0 (cdr d1))))

    ;; Compute k2 through k7
    (do ([i 1 (+ i 1)])
        ((= i 7))
      (let* ([ai (vector-ref dp-a i)]
             ;; Compute x_i = x0 + dt * sum(a_ij * kx_j)
             [xi (vec-copy x0)]
             [vi (vec-copy v0)])
        (do ([j 0 (+ j 1)])
            ((= j i))
          (let ([aij (vector-ref ai j)])
            (when (not (= aij 0))
              (set! xi (vec-add xi (vec-scale (* dt aij) (vector-ref kx j))))
              (set! vi (vec-add vi (vec-scale (* dt aij) (vector-ref kv j)))))))
        (let ([di (geodesic-derivative metric (make-geodesic-state xi vi))])
          (vector-set! kx i (car di))
          (vector-set! kv i (cdr di)))))

    ;; Compute 5th order solution
    (let ([x5 (vec-copy x0)]
          [v5 (vec-copy v0)])
      (do ([i 0 (+ i 1)])
          ((= i 7))
        (let ([bi (vector-ref dp-b5 i)])
          (when (not (= bi 0))
            (set! x5 (vec-add x5 (vec-scale (* dt bi) (vector-ref kx i))))
            (set! v5 (vec-add v5 (vec-scale (* dt bi) (vector-ref kv i)))))))

      ;; Compute 4th order solution
      (let ([x4 (vec-copy x0)]
            [v4 (vec-copy v0)])
        (do ([i 0 (+ i 1)])
            ((= i 7))
          (let ([bi (vector-ref dp-b4 i)])
            (when (not (= bi 0))
              (set! x4 (vec-add x4 (vec-scale (* dt bi) (vector-ref kx i))))
              (set! v4 (vec-add v4 (vec-scale (* dt bi) (vector-ref kv i)))))))

        ;; Error estimate: ||y5 - y4||
        (let* ([err-x (vec-sub x5 x4)]
               [err-v (vec-sub v5 v4)]
               [err-norm (max (vec-norm err-x) (vec-norm err-v))]
               ;; FSAL: k7 becomes k1 of next step
               ;; k7 is the derivative at (x5, v5), which is where we ended up
               [k7 (cons (vector-ref kx 6) (vector-ref kv 6))])
          (list (make-geodesic-state x5 v5)
                (make-geodesic-state x4 v4)
                err-norm
                k7))))))

;;; adaptive-step-size : Num × Num × Num × Nat → Num
;;; Compute new step size based on error.
;;; Uses PI controller formula: h_new = h * safety * (tol/err)^(1/p)
;;; where p is the order of the error estimate (5 for RK5(4)).
(define (adaptive-step-size dt err tol order)
  (if (= err 0)
      (* dt *adaptive-max-scale*)  ; Error is zero, can grow aggressively
      (let* ([ratio (/ tol err)]
             ;; Standard scaling: err ~ h^p, so h_new/h = (tol/err)^(1/p)
             [scale (* *adaptive-safety* (expt ratio (/ 1.0 order)))]
             [bounded-scale (max *adaptive-min-scale*
                                (min *adaptive-max-scale* scale))])
        (* dt bounded-scale))))

;;; adaptive-geodesic-step : Metric × GeodesicState × Num × Num × [k1-cache] → (GeodesicState × Num × Num × Nat × k7)
;;; Take one adaptive step with error control.
;;; Returns (new-state, actual-dt, suggested-dt, n-rejects, k7-cache) where:
;;;   - n-rejects is number of step rejections
;;;   - k7-cache can be passed as k1-cache to the next step (FSAL optimization)
;;;
;;; FSAL optimization: When a step is accepted, k7 from the step equals k1 of
;;; the next step. This saves one derivative evaluation per accepted step.
;;; When a step is rejected, k1 must be recomputed (k1-cache becomes invalid).
(define (adaptive-geodesic-step metric state dt tol . k1-cache-opt)
  (doc 'export #t)
  (doc 'type '(-> Metric GeodesicState Num Num (Optional (Pair Vec Vec)) (List GeodesicState Num Num Nat (Pair Vec Vec))))
  (doc 'description "Single adaptive RK5(4) step with error control and FSAL optimization")
  (let loop ([h dt] [rejects 0] [k1-cache (if (null? k1-cache-opt) #f (car k1-cache-opt))])
    (let* ([result (dp-geodesic-step metric state h k1-cache)]
           [y5 (car result)]
           [_ (cadr result)]    ; y4 not used after error computed
           [err (caddr result)]
           [k7 (cadddr result)])
      (if (<= err tol)
          ;; Accept step - k7 becomes k1 of next step
          (let ([h-new (adaptive-step-size h err tol 5)])
            (list y5 h h-new rejects k7))
          ;; Reject step, try smaller
          ;; Note: k1-cache is invalidated because we're retrying at same point with smaller step
          ;; but the derivative at the starting point is the same, so we can keep k1-cache
          (let ([h-new (adaptive-step-size h err tol 5)])
            ;; Protect against too many rejections
            (if (< h-new (* h 0.001))
                (begin
                  ;; Step too small, accept anyway with warning
                  (list y5 h h rejects k7))
                (loop h-new (+ rejects 1) k1-cache)))))))

;;; ============================================================================
;;; Adaptive Geodesic Tracing
;;; ============================================================================

(doc trace-geodesic-adaptive 'type '(-> Metric Vec Vec Num Num (List GeodesicState)))
(doc trace-geodesic-adaptive 'description "Trace geodesic with adaptive step-size control")
(doc trace-geodesic-adaptive 'param "tol: local error tolerance (default 1e-8)")
(doc trace-geodesic-adaptive 'param "Returns list of states at actual integration points")
(doc trace-geodesic-adaptive 'note "Uses FSAL optimization to reuse derivative between steps")
(define (trace-geodesic-adaptive metric initial-coords initial-velocity T tol)
  (doc 'export #t)
  (let* ([state0 (make-geodesic-state initial-coords initial-velocity)]
         [dt0 (/ T 10)]  ; Initial step guess: 1/10 of total time
         [min-dt (* T 1e-10)])  ; Minimum step size
    ;; k1-cache starts as #f (compute fresh on first step)
    (let loop ([state state0] [t 0] [dt dt0] [states (list state0)] [total-rejects 0] [k1-cache #f])
      (if (>= t T)
          (reverse states)
          ;; Don't overshoot the end
          (let ([h (min dt (- T t))])
            (if (< h min-dt)
                ;; Time remaining is negligible, done
                (reverse states)
                ;; FSAL: pass k1-cache from previous step's k7
                (let* ([result (adaptive-geodesic-step metric state h tol k1-cache)]
                       [new-state (car result)]
                       [actual-h (cadr result)]
                       [suggested-h (caddr result)]
                       [rejects (cadddr result)]
                       [k7 (car (cddddr result))])  ; k7 becomes next step's k1
                  (loop new-state
                        (+ t actual-h)
                        suggested-h
                        (cons new-state states)
                        (+ total-rejects rejects)
                        k7))))))))

;;; trace-geodesic-adaptive-final : Metric × Vec × Vec × Num × Num → GeodesicState
;;; Trace geodesic adaptively and return only final state.
;;; Uses FSAL optimization to reuse derivative between steps.
(define (trace-geodesic-adaptive-final metric initial-coords initial-velocity T tol)
  (doc 'export #t)
  (let* ([state0 (make-geodesic-state initial-coords initial-velocity)]
         [dt0 (/ T 10)]
         [min-dt (* T 1e-10)])
    ;; k1-cache starts as #f (compute fresh on first step)
    (let loop ([state state0] [t 0] [dt dt0] [k1-cache #f])
      (if (>= t T)
          state
          (let ([h (min dt (- T t))])
            (if (< h min-dt)
                state
                ;; FSAL: pass k1-cache from previous step's k7
                (let* ([result (adaptive-geodesic-step metric state h tol k1-cache)]
                       [new-state (car result)]
                       [actual-h (cadr result)]
                       [suggested-h (caddr result)]
                       [k7 (car (cddddr result))])
                  (loop new-state (+ t actual-h) suggested-h k7))))))))

;;; trace-geodesic-adaptive-at-times : Metric × Vec × Vec × (List Num) × Num → (List GeodesicState)
;;; Trace geodesic adaptively but return states at specified times.
;;; When a requested time falls within the current step, integrates to that exact time.
;;; Uses FSAL optimization to reuse derivative between steps.
(define (trace-geodesic-adaptive-at-times metric initial-coords initial-velocity times tol)
  (doc 'export #t)
  (doc 'type '(-> Metric Vec Vec (List Num) Num (List GeodesicState)))
  (doc 'description "Trace adaptively, returning states at specified times")
  (if (null? times)
      '()
      (let* ([sorted-times (merge-sort-by < times)]
             [T (car (reverse sorted-times))]  ; Final time
             [state0 (make-geodesic-state initial-coords initial-velocity)]
             [dt0 (/ T 10)]
             [min-dt (* T 1e-10)])
        ;; k1-cache starts as #f (compute fresh on first step)
        (let loop ([state state0]
                   [t 0]
                   [dt dt0]
                   [remaining-times sorted-times]
                   [results '()]
                   [k1-cache #f])
          (cond
            [(null? remaining-times) (reverse results)]
            [(>= t T)
             ;; Reached end, output remaining times at final state
             (reverse (append (map (lambda (_) state) remaining-times) results))]
            [else
             (let* ([next-time (car remaining-times)]
                    [h (min dt (- T t))])
               (cond
                 [(< h min-dt)
                  ;; Near end, output at final state
                  (reverse (append (map (lambda (_) state) remaining-times) results))]
                 [(<= next-time t)
                  ;; Time already passed (shouldn't happen with sorted times)
                  (loop state t dt (cdr remaining-times) (cons state results) k1-cache)]
                 [(and (> next-time t) (<= next-time (+ t h)))
                  ;; Next requested time is within this step
                  ;; Integrate to that exact time
                  ;; Note: k1-cache is valid here since we're continuing from same point
                  (let* ([h-exact (- next-time t)]
                         [result (adaptive-geodesic-step metric state h-exact tol k1-cache)]
                         [new-state (car result)]
                         [k7 (car (cddddr result))])
                    (loop new-state next-time dt (cdr remaining-times) (cons new-state results) k7))]
                 [else
                  ;; Normal step, no output time within range
                  ;; FSAL: pass k1-cache from previous step's k7
                  (let* ([result (adaptive-geodesic-step metric state h tol k1-cache)]
                         [new-state (car result)]
                         [actual-h (cadr result)]
                         [suggested-h (caddr result)]
                         [k7 (car (cddddr result))])
                    (loop new-state (+ t actual-h) suggested-h remaining-times results k7))]))])))))

;;; ============================================================================
;;; Adaptive Exponential and Logarithm Maps
;;; ============================================================================

;;; exp-map-adaptive : Metric × Vec × Vec × [Num] → Vec
;;; Exponential map using adaptive integration.
(define (exp-map-adaptive metric base-coords tangent-vec . opts)
  (doc 'export #t)
  (doc 'type '(-> Metric Vec Vec (Optional Num) Vec))
  (doc 'description "Exponential map exp_p(v) using adaptive RK5(4)")
  (let ([tol (if (null? opts) 1e-10 (car opts))])
    (geodesic-state-coords
     (trace-geodesic-adaptive-final metric base-coords tangent-vec 1.0 tol))))

;;; exp-map-adaptive-t : Metric × Vec × Vec × Num × [Num] → Vec
;;; Exponential map at time t using adaptive integration.
(define (exp-map-adaptive-t metric base-coords tangent-vec t . opts)
  (doc 'export #t)
  (doc 'type '(-> Metric Vec Vec Num (Optional Num) Vec))
  (doc 'description "Exponential map exp_p(v) at time t using adaptive RK5(4)")
  (let ([tol (if (null? opts) 1e-10 (car opts))])
    (geodesic-state-coords
     (trace-geodesic-adaptive-final metric base-coords tangent-vec t tol))))

;;; log-map-adaptive : Metric × Vec × Vec × [Num × Nat] → (Or (Ok Vec) (Err String))
;;; Logarithm map using adaptive integration.
(define (log-map-adaptive metric p q . opts)
  (doc 'export #t)
  (doc 'type '(-> Metric Vec Vec (Optional Num) (Optional Nat) (Or (Ok Vec) (Err String))))
  (doc 'description "Logarithm map using adaptive shooting method")
  (let* ([tol (if (null? opts) 1e-10 (car opts))]
         [max-iter (if (or (null? opts) (null? (cdr opts)))
                       *geodesic-max-iterations*
                       (cadr opts))]
         [n (vector-length p)]
         [v0 (vec-sub q p)]  ; Initial guess
         [eps 1e-6])
    (let loop ([v v0] [iter 0])
      (if (>= iter max-iter)
          (list 'err "log-map-adaptive: failed to converge")
          (let ([q-shot (exp-map-adaptive metric p v tol)])
            ;; Check for geodesic divergence BEFORE computing error
            ;; (If q-shot is NaN/Inf, error would be NaN, making gradient descent useless)
            (if (not (vec-finite? q-shot))
                ;; Geodesic diverged - shrink velocity to find valid range
                (loop (vec-scale 0.5 v) (+ iter 1))
                (let* ([error (vec-sub q-shot q)]
                       [error-norm (vec-norm error)])
                  (if (< error-norm (* tol 100))  ; Looser convergence for outer loop
                      (list 'ok v)
                      (let ([J (compute-exp-jacobian-adaptive metric p v tol eps n)])
                        (if (not J)
                            ;; Jacobian computation failed, shrink velocity
                            (loop (vec-scale 0.5 v) (+ iter 1))
                            (let ([dv (solve-linear-system J (vec-scale -1 error))])
                              (if dv
                                  (loop (vec-add v dv) (+ iter 1))
                                  ;; Linear solve failed, try gradient descent
                                  (loop (vec-sub v (vec-scale 0.1 error)) (+ iter 1))))))))))))))

;;; vec-finite? : Vec → Boolean
;;; Check if all elements of a vector are finite (not NaN or Inf).
(define (vec-finite? v)
  (let ([n (vector-length v)])
    (let loop ([i 0])
      (if (>= i n)
          #t
          (let ([x (vector-ref v i)])
            (and (not (nan? x)) (not (infinite? x))
                 (loop (+ i 1))))))))

;;; compute-exp-jacobian-adaptive : Metric × Vec × Vec × Num × Num × Nat → Matrix | #f
;;; Compute Jacobian of exp_p using adaptive integration.
;;; Returns #f if exp-map produces non-finite results (geodesic diverged).
(define (compute-exp-jacobian-adaptive metric p v tol eps n)
  (let ([J (make-matrix n n 0)]
        [v-plus (vec-copy v)]
        [v-minus (vec-copy v)]
        [failed #f])
    (do ([j 0 (+ j 1)])
        ((or failed (= j n)) (if failed #f J))
      (let* ([vj (vector-ref v j)]
             [eps-j (* eps (max 1.0 (abs vj)))])
        (vector-set! v-plus j (+ vj eps-j))
        (vector-set! v-minus j (- vj eps-j))
        (let* ([q-plus (exp-map-adaptive metric p v-plus tol)]
               [q-minus (exp-map-adaptive metric p v-minus tol)])
          ;; Check for non-finite results (geodesic diverged)
          (if (or (not (vec-finite? q-plus)) (not (vec-finite? q-minus)))
              (set! failed #t)
              (do ([i 0 (+ i 1)])
                  ((= i n))
                (matrix-set! J i j
                  (/ (- (vector-ref q-plus i) (vector-ref q-minus i))
                     (* 2 eps-j))))))
        (vector-set! v-plus j vj)
        (vector-set! v-minus j vj)))))

;;; geodesic-distance-adaptive : Metric × Vec × Vec × [Num] → Num | (Err String)
;;; Geodesic distance using adaptive integration.
(define (geodesic-distance-adaptive metric p q . opts)
  (doc 'export #t)
  (doc 'type '(-> Metric Vec Vec (Optional Num) (Or Num (Err String))))
  (doc 'description "Geodesic distance using adaptive log map")
  (let* ([tol (if (null? opts) 1e-10 (car opts))]
         [result (log-map-adaptive metric p q tol)])
    (if (and (pair? result) (eq? (car result) 'ok))
        (let ([v (cadr result)])
          (metric-norm metric p v))
        result)))

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
          (let ([q-shot (exp-map metric p v n-steps)])
            ;; Check for geodesic divergence BEFORE computing error
            ;; (If q-shot is NaN/Inf, error would be NaN, making gradient descent useless)
            (if (not (vec-finite? q-shot))
                ;; Geodesic diverged - shrink velocity to find valid range
                (loop (vec-scale 0.5 v) (+ iter 1))
                (let* ([error (vec-sub q-shot q)]
                       [error-norm (vec-norm error)])
                  (if (< error-norm tol)
                      (list 'ok v)
                      ;; Compute numerical Jacobian d(exp_p)/dv
                      (let ([J (compute-exp-jacobian metric p v n-steps eps n)])
                        (if (not J)
                            ;; Jacobian computation failed, shrink velocity
                            (loop (vec-scale 0.5 v) (+ iter 1))
                            ;; Solve J * dv = -error for dv
                            (let ([dv (solve-linear-system J (vec-scale -1 error))])
                              (if dv
                                  (loop (vec-add v dv) (+ iter 1))
                                  ;; Jacobian singular, try gradient descent step
                                  (loop (vec-sub v (vec-scale 0.1 error)) (+ iter 1))))))))))))))

;;; compute-exp-jacobian : Metric × Vec × Vec × Nat × Num × Nat → Matrix | #f
;;; Compute the Jacobian matrix of exp_p with respect to v.
;;; The epsilon is scaled per-component by max(1, |v_j|) for numerical stability
;;; across different velocity magnitudes.
;;; Returns #f if exp-map produces non-finite results (geodesic diverged).
(define (compute-exp-jacobian metric p v n-steps eps n)
  (let ([J (make-matrix n n 0)]
        [v-plus (vec-copy v)]
        [v-minus (vec-copy v)]
        [failed #f])
    (do ([j 0 (+ j 1)])
        ((or failed (= j n)) (if failed #f J))
      (let* ([vj (vector-ref v j)]
             ;; Scale epsilon by component magnitude for numerical stability
             [eps-j (* eps (max 1.0 (abs vj)))])
        ;; Perturb v[j]
        (vector-set! v-plus j (+ vj eps-j))
        (vector-set! v-minus j (- vj eps-j))
        (let* ([q-plus (exp-map metric p v-plus n-steps)]
               [q-minus (exp-map metric p v-minus n-steps)])
          ;; Check for non-finite results (geodesic diverged)
          (if (or (not (vec-finite? q-plus)) (not (vec-finite? q-minus)))
              (set! failed #t)
              ;; J[i][j] = d(exp_i)/d(v_j)
              (do ([i 0 (+ i 1)])
                  ((= i n))
                (matrix-set! J i j
                  (/ (- (vector-ref q-plus i) (vector-ref q-minus i))
                     (* 2 eps-j))))))
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
(printf "  Fixed-Step Tracing (RK4):\n")
(printf "    (trace-geodesic metric p v T n)       - Trace geodesic for time T\n")
(printf "    (trace-geodesic-final metric p v T n) - Final state only\n")
(printf "  Adaptive Tracing (RK5(4) Dormand-Prince):\n")
(printf "    (trace-geodesic-adaptive metric p v T tol)      - Adaptive trace\n")
(printf "    (trace-geodesic-adaptive-final metric p v T tol) - Final state only\n")
(printf "    (trace-geodesic-adaptive-at-times metric p v times tol) - At specific times\n")
(printf "  Exponential Map:\n")
(printf "    (exp-map metric p v [n-steps])        - exp_p(v) at t=1 (fixed-step)\n")
(printf "    (exp-map-adaptive metric p v [tol])   - exp_p(v) at t=1 (adaptive)\n")
(printf "  Logarithm Map:\n")
(printf "    (log-map metric p q [n tol max])      - Find v: exp_p(v)=q (fixed)\n")
(printf "    (log-map-adaptive metric p q [tol])   - Find v: exp_p(v)=q (adaptive)\n")
(printf "  Parallel Transport:\n")
(printf "    (parallel-transport metric p v V T)   - Transport V along geodesic\n")
(printf "  Distance:\n")
(printf "    (geodesic-distance metric p q)        - Geodesic distance (fixed-step)\n")
(printf "    (geodesic-distance-adaptive metric p q [tol]) - Geodesic distance (adaptive)\n")
(printf "    (geodesic-length metric states)       - Arc length of path\n")
(printf "  Interpolation:\n")
(printf "    (geodesic-interpolate metric p q t)   - Interpolate along geodesic\n")
(printf "  Visualization:\n")
(printf "    (geodesic-spray metric p n r steps)   - Shoot rays from p\n")
(printf "  Caching:\n")
(printf "    (clear-christoffel-cache!)            - Clear symbol cache\n")
(printf "    (christoffel-cache-stats)             - Show cache hit/miss stats\n")
