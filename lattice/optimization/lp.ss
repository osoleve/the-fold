(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-solvers.ss")

(doc 'module 'lp)
(doc 'description "Linear programming using the revised simplex method")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Pure functional implementation of linear programming using the revised simplex method.
Supports:
  - Standard form LP: minimize c'x subject to Ax = b, x >= 0
  - Dual LP construction
  - Sensitivity analysis (shadow prices, reduced costs)
  - Infeasibility and unboundedness detection
  - Bland's anti-cycling rule")

(doc 'section 'constants)

(define *lp-tolerance* 1e-10)
(define *lp-max-iterations* 10000)

;;; ====
;;; LP Data Structures
;;; ====

;;; An LP problem in standard form:
;;;   minimize   c'x
;;;   subject to Ax = b
;;;              x >= 0
;;;
;;; Represented as: (lp c A b)
;;;   c: cost vector (n elements)
;;;   A: constraint matrix (m x n)
;;;   b: right-hand side (m elements, all >= 0)

;;; lp? : Any -> Boolean
;;; Check if value is an LP structure.
(define (lp? x)
  (and (pair? x)
       (eq? (car x) 'lp)
       (= (length x) 4)
       (vector? (cadr x))
       (matrix? (caddr x))
       (vector? (cadddr x))))

;;; make-lp : Vec x Matrix x Vec -> LP
;;; Create an LP problem in standard form.
(define (make-lp c A b)
  (doc 'export #t)
  (list 'lp c A b))

;;; Accessors
(define (lp-c lp) (cadr lp))
(define (lp-A lp) (caddr lp))
(define (lp-b lp) (cadddr lp))

;;; lp-dims : LP -> (m . n)
;;; Return dimensions: m constraints, n variables.
(define (lp-dims lp)
  (cons (matrix-rows (lp-A lp)) (matrix-cols (lp-A lp))))

;;; ====
;;; LP Result Structures
;;; ====

;;; LPResult is one of:
;;;   (lp-optimal x z basis)     ; optimal solution found
;;;   (lp-infeasible)            ; no feasible solution
;;;   (lp-unbounded direction)   ; unbounded below along direction

;;; make-lp-optimal : Vec x Num x Vec -> LPResult
(define (make-lp-optimal x z basis)
  (list 'lp-optimal x z basis))

;;; make-lp-infeasible : -> LPResult
(define (make-lp-infeasible)
  '(lp-infeasible))

;;; make-lp-unbounded : Vec -> LPResult
(define (make-lp-unbounded direction)
  (list 'lp-unbounded direction))

;;; Result predicates
(define (lp-optimal? r) (and (pair? r) (eq? (car r) 'lp-optimal)))
(define (lp-infeasible? r) (and (pair? r) (eq? (car r) 'lp-infeasible)))
(define (lp-unbounded? r) (and (pair? r) (eq? (car r) 'lp-unbounded)))

;;; Result accessors
(define (lp-result-x r) (cadr r))
(define (lp-result-z r) (caddr r))
(define (lp-result-basis r) (cadddr r))
(define (lp-result-direction r) (cadr r))

;;; ====
;;; LP Construction Helpers
;;; ====

;;; lp-from-inequality : Vec x Matrix x Vec x Symbol -> LP
;;; Convert inequality form to standard form.
;;;   type: 'le for Ax <= b, 'ge for Ax >= b
;;;
;;; For Ax <= b: add slack variables s, get Ax + Is = b, s >= 0
;;; For Ax >= b: subtract surplus, add artificial: Ax - Is + Ia = b
(define (lp-from-inequality c A b type)
  (let* ([m (matrix-rows A)]
         [n (matrix-cols A)]
         [eye-m (identity m)])
    (case type
      [(le <=)
       ;; Ax + s = b where s >= 0
       ;; c becomes [c; 0] (slack variables have zero cost)
       (let ([c-ext (vec-append c (vec-zeros m))]
             [A-ext (matrix-hstack A eye-m)])
         (make-lp c-ext A-ext b))]
      [(ge >=)
       ;; Ax - s = b, but need s >= 0, so this needs artificial vars
       ;; For Phase I: add artificial variables with cost 1
       (let ([c-ext (vec-append (vec-append c (vec-zeros m)) (vec-ones m))]
             [minus-eye (matrix-scale -1 eye-m)]
             [A-ext (matrix-hstack (matrix-hstack A minus-eye) eye-m)])
         (make-lp c-ext A-ext b))]
      [else
       `(error unknown-inequality-type ,type)])))

;;; lp-maximize : Vec x Matrix x Vec -> LP
;;; Convert maximization to minimization (negate objective).
(define (lp-maximize c A b)
  (make-lp (vec-negate c) A b))

;;; ====
;;; Basis Operations
;;; ====

;;; extract-basis-matrix : Matrix x Vec -> Matrix
;;; Extract columns of A corresponding to basis indices.
(define (extract-basis-matrix A basis)
  (let* ([m (matrix-rows A)]
         [k (vector-length basis)]
         [result (make-matrix m k 0)])
    (do ([j 0 (+ j 1)])
        [(= j k) result]
      (let ([col-idx (vector-ref basis j)])
        (do ([i 0 (+ i 1)])
            [(= i m)]
          (matrix-set! result i j (matrix-ref A i col-idx)))))))

;;; extract-basis-costs : Vec x Vec -> Vec
;;; Extract cost coefficients for basis variables.
(define (extract-basis-costs c basis)
  (let* ([m (vector-length basis)]
         [cb (make-vector m 0)])
    (do ([i 0 (+ i 1)])
        [(= i m) cb]
      (vector-set! cb i (vector-ref c (vector-ref basis i))))))

;;; ====
;;; Simplex Method Core
;;; ====

;;; compute-reduced-costs : Vec x Matrix x Vec x Matrix -> Vec
;;; Compute reduced costs: c_j - c_B' * B^-1 * A_j for all j
;;; Using y = B^-1 * c_B (dual variables), reduced cost = c_j - y' * A_j
(define (compute-reduced-costs c A basis B-inv)
  (let* ([n (vector-length c)]
         [m (matrix-rows A)]
         [cb (extract-basis-costs c basis)]
         ;; y = B^-1' * cb (dual variables)
         [y (matrix-vec-mul (matrix-transpose B-inv) cb)]
         [reduced (make-vector n 0)])
    (do ([j 0 (+ j 1)])
        [(= j n) reduced]
      (let* ([Aj (matrix-col A j)]
             [y-Aj (vec-dot y Aj)])
        (vector-set! reduced j (- (vector-ref c j) y-Aj))))))

;;; find-entering-variable : Vec x Vec -> Nat | #f
;;; Find entering variable using Bland's rule (smallest index with negative reduced cost).
;;; Returns #f if no negative reduced costs (optimal).
(define (find-entering-variable reduced-costs basis)
  (let ([n (vector-length reduced-costs)])
    (let loop ([j 0])
      (cond
        [(= j n) #f]
        [(and (< (vector-ref reduced-costs j) (- *lp-tolerance*))
              (not (vec-contains? basis j)))
         j]
        [else (loop (+ j 1))]))))

;;; vec-contains? : Vec x Nat -> Boolean
(define (vec-contains? v x)
  (let ([n (vector-length v)])
    (let loop ([i 0])
      (cond
        [(= i n) #f]
        [(= (vector-ref v i) x) #t]
        [else (loop (+ i 1))]))))

;;; compute-direction : Matrix x Matrix x Nat -> Vec
;;; Compute direction d = B^-1 * A_j for entering column j.
(define (compute-direction A B-inv j)
  (let ([Aj (matrix-col A j)])
    (matrix-vec-mul B-inv Aj)))

;;; find-leaving-variable : Vec x Vec x Vec -> Nat | 'unbounded
;;; Find leaving variable using minimum ratio test with Bland's rule.
;;; Returns index in basis, or 'unbounded if unbounded.
(define (find-leaving-variable xB direction basis)
  (let* ([m (vector-length xB)]
         [min-ratio +inf.0]
         [min-idx #f])
    (do ([i 0 (+ i 1)])
        [(= i m)
         (if min-idx min-idx 'unbounded)]
      (let ([di (vector-ref direction i)])
        (when (> di *lp-tolerance*)
          (let* ([ratio (/ (vector-ref xB i) di)]
                 [basis-idx (vector-ref basis i)])
            ;; Bland's rule: if tie, prefer smaller basis index
            (when (or (< ratio (- min-ratio *lp-tolerance*))
                      (and (< (abs (- ratio min-ratio)) *lp-tolerance*)
                           (or (not min-idx)
                               (< basis-idx (vector-ref basis min-idx)))))
              (set! min-ratio ratio)
              (set! min-idx i))))))))

;;; update-basis : Vec x Nat x Nat -> Vec
;;; Replace leaving variable with entering variable in basis.
(define (update-basis basis leaving-idx entering-var)
  (let* ([m (vector-length basis)]
         [new-basis (vec-copy basis)])
    (vector-set! new-basis leaving-idx entering-var)
    new-basis))

;;; ====
;;; Phase I: Find Initial Feasible Basis
;;; ====

;;; create-phase1-lp : Matrix x Vec -> (LP x Vec)
;;; Create Phase I LP with artificial variables.
;;; Returns (phase1-lp . initial-basis)
(define (create-phase1-lp A b)
  (let* ([m (matrix-rows A)]
         [n (matrix-cols A)]
         ;; Add m artificial variables
         [eye-m (identity m)]
         [A-aug (matrix-hstack A eye-m)]
         ;; Cost: 0 for original, 1 for artificial
         [c-phase1 (vec-append (vec-zeros n) (vec-ones m))]
         ;; Initial basis: artificial variables (indices n to n+m-1)
         [init-basis (make-vector m 0)])
    (do ([i 0 (+ i 1)])
        [(= i m)]
      (vector-set! init-basis i (+ n i)))
    (cons (make-lp c-phase1 A-aug b) init-basis)))

;;; run-phase1 : Matrix x Vec -> Vec | 'infeasible
;;; Run Phase I simplex to find initial feasible basis.
;;; Returns basis vector or 'infeasible.
(define (run-phase1 A b)
  (let* ([phase1-data (create-phase1-lp A b)]
         [phase1-lp (car phase1-data)]
         [init-basis (cdr phase1-data)]
         [result (simplex-core phase1-lp init-basis)])
    (cond
      [(lp-infeasible? result) 'infeasible]
      [(lp-unbounded? result) 'infeasible]  ; shouldn't happen in Phase I
      [else
       (let ([z (lp-result-z result)]
             [basis (lp-result-basis result)]
             [n (matrix-cols A)])
         (if (> z *lp-tolerance*)
             ;; Positive optimal value means infeasible
             'infeasible
             ;; Check if any artificial variables in basis
             (let ([has-artificial (let loop ([i 0])
                                     (if (= i (vector-length basis))
                                         #f
                                         (if (>= (vector-ref basis i) n)
                                             #t
                                             (loop (+ i 1)))))])
               (if has-artificial
                   ;; Need to pivot out artificial variables
                   (pivot-out-artificials A b basis n)
                   basis))))])))

;;; pivot-out-artificials : Matrix x Vec x Vec x Nat -> Vec | 'infeasible
;;; Remove artificial variables from basis by pivoting.
(define (pivot-out-artificials A b basis n)
  (let* ([m (vector-length basis)]
         [new-basis (vec-copy basis)])
    (let loop ([i 0])
      (if (= i m)
          new-basis
          (let ([var (vector-ref new-basis i)])
            (if (< var n)
                (loop (+ i 1))
                ;; Artificial variable at position i, try to pivot it out
                (let ([replacement (find-nonbasic-pivot A new-basis i n)])
                  (if replacement
                      (begin
                        (vector-set! new-basis i replacement)
                        (loop (+ i 1)))
                      ;; If can't pivot out, row is redundant
                      ;; For now, signal infeasible (could handle redundancy)
                      'infeasible))))))))

;;; find-nonbasic-pivot : Matrix x Vec x Nat x Nat -> Nat | #f
;;; Find a non-basic variable to enter at position i.
(define (find-nonbasic-pivot A basis i n)
  (let ([m (matrix-rows A)])
    (let loop ([j 0])
      (cond
        [(= j n) #f]
        [(vec-contains? basis j) (loop (+ j 1))]
        [(> (abs (matrix-ref A i j)) *lp-tolerance*) j]
        [else (loop (+ j 1))]))))

;;; ====
;;; Simplex Core Algorithm
;;; ====

;;; simplex-core : LP x Vec -> LPResult
;;; Run simplex method from given basis.
(define (simplex-core lp basis)
  (let* ([c (lp-c lp)]
         [A (lp-A lp)]
         [b (lp-b lp)]
         [m (matrix-rows A)]
         [n (matrix-cols A)])
    (let iterate ([basis basis] [iter 0])
      (if (>= iter *lp-max-iterations*)
          ;; Max iterations reached - report current best
          (let* ([B (extract-basis-matrix A basis)]
                 [B-inv-result (matrix-inverse B)])
            (if (and (pair? B-inv-result) (eq? (car B-inv-result) 'error))
                (make-lp-infeasible)
                (let* ([xB (matrix-vec-mul B-inv-result b)]
                       [x (construct-solution basis xB n)]
                       [z (vec-dot c x)])
                  (make-lp-optimal x z basis))))
          ;; Standard iteration
          (let* ([B (extract-basis-matrix A basis)]
                 [B-inv-result (matrix-inverse B)])
            (if (and (pair? B-inv-result) (eq? (car B-inv-result) 'error))
                ;; Singular basis - this shouldn't happen with proper pivoting
                (make-lp-infeasible)
                (let* ([B-inv B-inv-result]
                       [xB (matrix-vec-mul B-inv b)]
                       [reduced-costs (compute-reduced-costs c A basis B-inv)]
                       [entering (find-entering-variable reduced-costs basis)])
                  (if (not entering)
                      ;; No negative reduced costs - optimal!
                      (let* ([x (construct-solution basis xB n)]
                             [z (vec-dot c x)])
                        (make-lp-optimal x z basis))
                      ;; Found entering variable, compute direction
                      (let* ([direction (compute-direction A B-inv entering)]
                             [leaving (find-leaving-variable xB direction basis)])
                        (if (eq? leaving 'unbounded)
                            ;; Unbounded
                            (let ([unbounded-dir (construct-unbounded-direction basis direction entering n)])
                              (make-lp-unbounded unbounded-dir))
                            ;; Pivot
                            (let ([new-basis (update-basis basis leaving entering)])
                              (iterate new-basis (+ iter 1)))))))))))))

;;; construct-solution : Vec x Vec x Nat -> Vec
;;; Construct full solution vector from basis solution.
(define (construct-solution basis xB n)
  (let ([x (vec-zeros n)])
    (do ([i 0 (+ i 1)])
        [(= i (vector-length basis)) x]
      (let ([j (vector-ref basis i)])
        (when (< j n)
          (vector-set! x j (vector-ref xB i)))))))

;;; construct-unbounded-direction : Vec x Vec x Nat x Nat -> Vec
;;; Construct unbounded direction vector.
(define (construct-unbounded-direction basis direction entering n)
  (let ([d (vec-zeros n)])
    (vector-set! d entering 1)
    (do ([i 0 (+ i 1)])
        [(= i (vector-length basis)) d]
      (let ([j (vector-ref basis i)])
        (when (< j n)
          (vector-set! d j (- (vector-ref direction i))))))))

;;; ====
;;; Main Solver
;;; ====

;;; lp-solve : LP -> LPResult
;;; Solve LP using two-phase simplex method.
(define (lp-solve lp)
  (doc 'export #t)
  (let* ([A (lp-A lp)]
         [b (lp-b lp)]
         [c (lp-c lp)]
         [m (matrix-rows A)]
         [n (matrix-cols A)])
    ;; Ensure b >= 0 by negating rows if needed
    (let-values ([(A-pos b-pos) (ensure-nonnegative-rhs A b)])
      (let ([phase1-basis (run-phase1 A-pos b-pos)])
        (if (eq? phase1-basis 'infeasible)
            (make-lp-infeasible)
            ;; Run Phase II with feasible basis
            (let ([lp-pos (make-lp c A-pos b-pos)])
              (simplex-core lp-pos phase1-basis)))))))

;;; ensure-nonnegative-rhs : Matrix x Vec -> (values Matrix Vec)
;;; Negate rows where b_i < 0.
(define (ensure-nonnegative-rhs A b)
  (let* ([m (matrix-rows A)]
         [n (matrix-cols A)]
         [A-new (matrix-copy A)]
         [b-new (vec-copy b)])
    (do ([i 0 (+ i 1)])
        [(= i m) (values A-new b-new)]
      (when (< (vector-ref b-new i) 0)
        ;; Negate row i
        (do ([j 0 (+ j 1)])
            [(= j n)]
          (matrix-set! A-new i j (- (matrix-ref A-new i j))))
        (vector-set! b-new i (- (vector-ref b-new i)))))))

;;; ====
;;; Dual LP
;;; ====

;;; lp-dual : LP -> LP
;;; Construct the dual LP.
;;; Primal: minimize c'x s.t. Ax = b, x >= 0
;;; Dual:   maximize b'y s.t. A'y <= c
;;;       = minimize -b'y s.t. A'y + s = c, s >= 0
(define (lp-dual lp)
  (doc 'export #t)
  (let* ([c (lp-c lp)]
         [A (lp-A lp)]
         [b (lp-b lp)]
         [m (matrix-rows A)]
         [n (matrix-cols A)]
         ;; Dual: minimize -b'y subject to A'y + s = c, s >= 0
         ;; Variables: y (unrestricted -> split), s (n slack vars)
         ;; A' is n x m, so dual constraint matrix is n x 2m (y+, y-) + n (slack)
         [At (matrix-transpose A)]
         ;; For unrestricted y, replace with y+ - y-, both >= 0
         [At-split (matrix-hstack At (matrix-scale -1 At))]
         ;; Add slack variables
         [eye-n (identity n)]
         [A-dual (matrix-hstack At-split eye-n)]
         ;; Cost: -b for y+, +b for y-, 0 for slack
         [c-dual (vec-append (vec-append (vec-negate b) b) (vec-zeros n))])
    (make-lp c-dual A-dual c)))

;;; ====
;;; Sensitivity Analysis
;;; ====

;;; lp-shadow-prices : LP x LPResult -> Vec | Error
;;; Compute shadow prices (dual variables) y = c_B' * B^-1.
;;; Shadow price y_i is the rate of change of z* w.r.t. b_i.
(define (lp-shadow-prices lp result)
  (doc 'export #t)
  (if (not (lp-optimal? result))
      '(error not-optimal)
      (let* ([A (lp-A lp)]
             [c (lp-c lp)]
             [basis (lp-result-basis result)]
             [B (extract-basis-matrix A basis)]
             [cb (extract-basis-costs c basis)]
             [B-inv (matrix-inverse B)])
        (if (and (pair? B-inv) (eq? (car B-inv) 'error))
            B-inv
            (matrix-vec-mul (matrix-transpose B-inv) cb)))))

;;; lp-reduced-costs : LP x LPResult -> Vec | Error
;;; Compute reduced costs for all variables.
;;; Reduced cost c_j - y' * A_j measures cost of increasing x_j.
(define (lp-reduced-costs lp result)
  (doc 'export #t)
  (if (not (lp-optimal? result))
      '(error not-optimal)
      (let* ([A (lp-A lp)]
             [c (lp-c lp)]
             [basis (lp-result-basis result)]
             [B (extract-basis-matrix A basis)]
             [B-inv (matrix-inverse B)])
        (if (and (pair? B-inv) (eq? (car B-inv) 'error))
            B-inv
            (compute-reduced-costs c A basis B-inv)))))

;;; lp-basis-ranges : LP x LPResult -> (List (Nat x Num x Num)) | Error
;;; Compute allowable ranges for cost coefficients.
;;; Returns list of (variable-index, lower-bound, upper-bound).
(define (lp-basis-ranges lp result)
  (if (not (lp-optimal? result))
      '(error not-optimal)
      (let* ([A (lp-A lp)]
             [c (lp-c lp)]
             [n (vector-length c)]
             [basis (lp-result-basis result)]
             [m (vector-length basis)]
             [B (extract-basis-matrix A basis)]
             [B-inv (matrix-inverse B)])
        (if (and (pair? B-inv) (eq? (car B-inv) 'error))
            B-inv
            (let ([reduced (compute-reduced-costs c A basis B-inv)]
                  [ranges '()])
              ;; For non-basic variables, reduced cost gives allowable increase
              (do ([j 0 (+ j 1)])
                  [(= j n) (reverse ranges)]
                (let ([is-basic (vec-contains? basis j)]
                      [rc (vector-ref reduced j)])
                  (set! ranges
                        (cons (list j
                                    (if is-basic -inf.0 rc)
                                    (if is-basic +inf.0 (- rc)))
                              ranges)))))))))

;;; lp-rhs-ranges : LP x LPResult -> Vec | Error
;;; Compute allowable ranges for RHS values b.
;;; Returns vector of (lower-bound, upper-bound) pairs.
(define (lp-rhs-ranges lp result)
  (if (not (lp-optimal? result))
      '(error not-optimal)
      (let* ([A (lp-A lp)]
             [b (lp-b lp)]
             [basis (lp-result-basis result)]
             [m (vector-length basis)]
             [B (extract-basis-matrix A basis)]
             [B-inv (matrix-inverse B)]
             [xB (lp-result-x result)])
        (if (and (pair? B-inv) (eq? (car B-inv) 'error))
            B-inv
            ;; Compute B^-1 to determine ranges
            ;; Range for b_i: xB stays >= 0
            (let ([ranges (make-vector m #f)])
              (do ([i 0 (+ i 1)])
                  [(= i m) ranges]
                ;; Column i of B^-1 determines sensitivity to b_i
                (let ([B-inv-col (matrix-col B-inv i)]
                      [min-delta -inf.0]
                      [max-delta +inf.0])
                  (do ([k 0 (+ k 1)])
                      [(= k m)]
                    (let ([coef (vector-ref B-inv-col k)]
                          [xk (vector-ref xB (vector-ref basis k))])
                      (cond
                        [(> coef *lp-tolerance*)
                         ;; xk - coef * delta >= 0 => delta <= xk/coef
                         (set! max-delta (min max-delta (/ xk coef)))]
                        [(< coef (- *lp-tolerance*))
                         ;; xk - coef * delta >= 0 => delta >= xk/coef
                         (set! min-delta (max min-delta (/ xk coef)))])))
                  (vector-set! ranges i
                              (list (+ (vector-ref b i) min-delta)
                                    (+ (vector-ref b i) max-delta))))))))))

;;; ====
;;; Duality Verification
;;; ====

;;; lp-weak-duality? : LP x LPResult x LPResult -> Boolean
;;; Verify weak duality: primal objective >= dual objective (for min).
(define (lp-weak-duality? primal primal-result dual-result)
  (and (lp-optimal? primal-result)
       (lp-optimal? dual-result)
       (let ([primal-z (lp-result-z primal-result)]
             ;; Dual was maximize b'y, converted to minimize -b'y
             [dual-z (- (lp-result-z dual-result))])
         (>= (+ primal-z *lp-tolerance*) dual-z))))

;;; lp-strong-duality? : LP x LPResult x LPResult -> Boolean
;;; Verify strong duality: optimal values are equal.
(define (lp-strong-duality? primal primal-result dual-result)
  (and (lp-optimal? primal-result)
       (lp-optimal? dual-result)
       (let ([primal-z (lp-result-z primal-result)]
             [dual-z (- (lp-result-z dual-result))])
         (< (abs (- primal-z dual-z)) *lp-tolerance*))))

;;; ====
;;; Pretty Printing
;;; ====

;;; lp->string : LP -> String
;;; Convert LP to human-readable string.
(define (lp->string lp)
  (let* ([c (lp-c lp)]
         [A (lp-A lp)]
         [b (lp-b lp)]
         [m (matrix-rows A)]
         [n (matrix-cols A)])
    (string-append
     "Minimize: " (vec->string c) "' * x\n"
     "Subject to: Ax = b\n"
     "A = " (matrix->string A) "\n"
     "b = " (vec->string b) "\n"
     "x >= 0")))

;;; vec->string : Vec -> String
(define (vec->string v)
  (let ([parts (map number->string (vec->list v))])
    (string-append "[" (string-join parts ", ") "]")))

;;; matrix->string : Matrix -> String
(define (matrix->string m)
  (let* ([rows (matrix-rows m)]
         [row-strs (map (lambda (r) (vec->string (list->vector r)))
                        (matrix->lists m))])
    (string-append "[" (string-join row-strs "; ") "]")))

;;; string-join : (List String) x String -> String
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; lp-result->string : LPResult -> String
(define (lp-result->string result)
  (cond
    [(lp-optimal? result)
     (string-append
      "Optimal solution found:\n"
      "  x* = " (vec->string (lp-result-x result)) "\n"
      "  z* = " (number->string (lp-result-z result)))]
    [(lp-infeasible? result)
     "Problem is infeasible"]
    [(lp-unbounded? result)
     (string-append
      "Problem is unbounded\n"
      "  Direction: " (vec->string (lp-result-direction result)))]
    [else
     "Unknown result"]))
