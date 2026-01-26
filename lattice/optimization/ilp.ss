;;; lattice/optimization/ilp.ss — Integer Linear Programming
;;; @module ilp
;;; @requires lp

(load "lattice/optimization/lp.ss")

(doc 'module 'ilp)
(doc 'description "Integer linear programming with branch-and-bound and cutting planes")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Extends LP with integer constraints via branch-and-bound and Gomory cutting planes.
Supports:
  - Mixed integer programs (some variables integer, some continuous)
  - Pure integer programs (all variables integer)
  - Binary programs (0-1 variables)
  - Classic applications: knapsack, set cover")

(doc 'section 'constants)

(define *ilp-tolerance* 1e-6)
(define *ilp-max-nodes* 10000)
(define *ilp-max-cuts* 100)

(doc 'section 'data-structures)
(doc 'note "An ILP extends LP with integer variable constraints: (ilp c A b integer-vars)
where integer-vars is a list of variable indices that must be integer.
Empty list = pure LP, all indices = pure ILP")

(define (ilp? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if value is an ILP structure")
  (and (pair? x)
       (eq? (car x) 'ilp)
       (= (length x) 5)
       (vector? (cadr x))
       (matrix? (caddr x))
       (vector? (cadddr x))
       (list? (car (cddddr x)))))

;;; make-ilp : Vec x Matrix x Vec x List -> ILP
(define (make-ilp c A b integer-vars)
  (doc 'export #t)
  (list 'ilp c A b integer-vars))

;;; Accessors
(define (ilp-c ilp) (cadr ilp))
(define (ilp-A ilp) (caddr ilp))
(define (ilp-b ilp) (cadddr ilp))
(define (ilp-integers ilp) (car (cddddr ilp)))

;;; ilp-dims : ILP -> (m . n)
(define (ilp-dims ilp)
  (cons (matrix-rows (ilp-A ilp)) (matrix-cols (ilp-A ilp))))

;;; ilp->lp : ILP -> LP
;;; Get LP relaxation (ignore integer constraints).
(define (ilp->lp ilp)
  (make-lp (ilp-c ilp) (ilp-A ilp) (ilp-b ilp)))

;;; ====
;;; ILP Construction Helpers
;;; ====

;;; make-pure-ilp : Vec x Matrix x Vec -> ILP
;;; All variables must be integer.
(define (make-pure-ilp c A b)
  (let ([n (matrix-cols A)])
    (make-ilp c A b (iota n))))

;;; make-binary-ilp : Vec x Matrix x Vec -> ILP
;;; All variables are 0-1 (binary).
;;; Binary constraint is handled by adding x_i <= 1 constraints.
(define (make-binary-ilp c A b)
  (let* ([n (matrix-cols A)]
         [m (matrix-rows A)]
         ;; Add n constraints: x_i <= 1 (as x_i + s_i = 1)
         [eye-n (identity n)]
         [A-ext (matrix-vstack A eye-n)]
         [b-ext (vec-append b (vec-ones n))]
         ;; Add slack variables for x <= 1 constraints
         [A-slack (matrix-vstack (make-matrix m n 0) eye-n)]
         [A-full (matrix-hstack A-ext A-slack)]
         [c-full (vec-append c (vec-zeros n))])
    (make-ilp c-full A-full b-ext (iota n))))

;;; ====
;;; ILP Result Structures
;;; ====

;;; ILPResult is one of:
;;;   (ilp-optimal x z)        ; optimal integer solution
;;;   (ilp-infeasible)         ; no feasible integer solution
;;;   (ilp-unbounded)          ; unbounded (rare for ILP)

(define (make-ilp-optimal x z)
  (list 'ilp-optimal x z))

(define (make-ilp-infeasible)
  '(ilp-infeasible))

(define (make-ilp-unbounded)
  '(ilp-unbounded))

(define (ilp-optimal? r) (and (pair? r) (eq? (car r) 'ilp-optimal)))
(define (ilp-infeasible? r) (and (pair? r) (eq? (car r) 'ilp-infeasible)))
(define (ilp-unbounded? r) (and (pair? r) (eq? (car r) 'ilp-unbounded)))

(define (ilp-result-x r) (cadr r))
(define (ilp-result-z r) (caddr r))

;;; ====
;;; Integrality Checking
;;; ====

;;; is-integer? : Num -> Boolean
;;; Check if a number is (approximately) an integer.
(define (is-integer? x)
  (< (abs (- x (round x))) *ilp-tolerance*))

;;; solution-is-integer? : Vec x List -> Boolean
;;; Check if solution satisfies integer constraints.
(define (solution-is-integer? x integer-vars)
  (let loop ([vars integer-vars])
    (or (null? vars)
        (and (is-integer? (vector-ref x (car vars)))
             (loop (cdr vars))))))

;;; find-fractional-var : Vec x List -> Nat | #f
;;; Find first integer variable with fractional value.
;;; Returns variable index or #f if all integer vars are integral.
(define (find-fractional-var x integer-vars)
  (let loop ([vars integer-vars])
    (cond
      [(null? vars) #f]
      [(not (is-integer? (vector-ref x (car vars)))) (car vars)]
      [else (loop (cdr vars))])))

;;; round-solution : Vec x List -> Vec
;;; Round integer variables to nearest integer.
(define (round-solution x integer-vars)
  (let ([result (vec-copy x)])
    (for-each
      (lambda (i)
        (vector-set! result i (round (vector-ref result i))))
      integer-vars)
    result))

;;; ====
;;; Branch-and-Bound Core
;;; ====

;;; A B&B node: (bb-node lp lower-bounds upper-bounds)
;;; lower-bounds and upper-bounds are vectors of variable bounds

(define (make-bb-node lp lb ub)
  (list 'bb-node lp lb ub))

(define (bb-node-lp node) (cadr node))
(define (bb-node-lb node) (caddr node))
(define (bb-node-ub node) (cadddr node))

;;; add-bound-constraints : LP x Vec x Vec -> LP
;;; Add variable bound constraints to LP.
;;; For x_i >= l_i: add constraint x_i - s = l_i (s >= 0)
;;; For x_i <= u_i: add constraint x_i + s = u_i (s >= 0)
(define (add-bound-constraints lp lb ub)
  (let* ([c (lp-c lp)]
         [A (lp-A lp)]
         [b (lp-b lp)]
         [m (matrix-rows A)]
         [n (vector-length c)]
         [bound-rows '()]
         [bound-rhs '()]
         [slack-count 0])
    ;; Collect bound constraints
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (let ([li (vector-ref lb i)]
            [ui (vector-ref ub i)])
        ;; Lower bound: x_i >= l_i becomes x_i - s = l_i
        (when (> li -inf.0)
          (let ([row (make-vector n 0)])
            (vector-set! row i 1)
            (set! bound-rows (cons (cons row -1) bound-rows))  ; -1 for subtract slack
            (set! bound-rhs (cons li bound-rhs))
            (set! slack-count (+ slack-count 1))))
        ;; Upper bound: x_i <= u_i becomes x_i + s = u_i
        (when (< ui +inf.0)
          (let ([row (make-vector n 0)])
            (vector-set! row i 1)
            (set! bound-rows (cons (cons row 1) bound-rows))   ; +1 for add slack
            (set! bound-rhs (cons ui bound-rhs))
            (set! slack-count (+ slack-count 1))))))
    (if (= slack-count 0)
        lp
        ;; Build augmented system
        (let* ([num-bounds (length bound-rows)]
               [new-m (+ m num-bounds)]
               [new-n (+ n slack-count)]
               [new-A (make-matrix new-m new-n 0)]
               [new-b (make-vector new-m 0)]
               [new-c (vec-append c (vec-zeros slack-count))])
          ;; Copy original A and b
          (do ([i 0 (+ i 1)])
              [(= i m)]
            (do ([j 0 (+ j 1)])
                [(= j n)]
              (matrix-set! new-A i j (matrix-ref A i j)))
            (vector-set! new-b i (vector-ref b i)))
          ;; Add bound constraints
          (let loop ([rows (reverse bound-rows)]
                     [rhs (reverse bound-rhs)]
                     [row-idx m]
                     [slack-idx n])
            (when (not (null? rows))
              (let* ([row-data (car rows)]
                     [row (car row-data)]
                     [slack-sign (cdr row-data)])
                ;; Copy row coefficients
                (do ([j 0 (+ j 1)])
                    [(= j n)]
                  (matrix-set! new-A row-idx j (vector-ref row j)))
                ;; Add slack coefficient
                (matrix-set! new-A row-idx slack-idx slack-sign)
                ;; Set RHS
                (vector-set! new-b row-idx (car rhs))
                (loop (cdr rows) (cdr rhs) (+ row-idx 1) (+ slack-idx 1)))))
          (make-lp new-c new-A new-b)))))

;;; ilp-solve : ILP -> ILPResult
;;; Solve ILP using branch-and-bound.
(define (ilp-solve ilp)
  (doc 'export #t)
  (let* ([c (ilp-c ilp)]
         [A (ilp-A ilp)]
         [b (ilp-b ilp)]
         [integers (ilp-integers ilp)]
         [n (vector-length c)]
         ;; Initial bounds: x >= 0, x <= +inf
         [init-lb (vec-zeros n)]
         [init-ub (make-vector n +inf.0)]
         ;; Solve LP relaxation first
         [lp (ilp->lp ilp)]
         [lp-result (lp-solve lp)])
    (cond
      [(lp-infeasible? lp-result) (make-ilp-infeasible)]
      [(lp-unbounded? lp-result) (make-ilp-unbounded)]
      [else
       ;; Check if LP solution is already integer
       (let ([x (lp-result-x lp-result)]
             [z (lp-result-z lp-result)])
         (if (solution-is-integer? x integers)
             (make-ilp-optimal (round-solution x integers) z)
             ;; Run branch-and-bound
             (branch-and-bound ilp init-lb init-ub x z)))])))

;;; branch-and-bound : ILP x Vec x Vec x Vec x Num -> ILPResult
;;; Main branch-and-bound algorithm.
(define (branch-and-bound ilp init-lb init-ub init-x init-z)
  (let* ([c (ilp-c ilp)]
         [A (ilp-A ilp)]
         [b (ilp-b ilp)]
         [integers (ilp-integers ilp)]
         [n (vector-length c)]
         ;; Best integer solution found so far
         [best-x #f]
         [best-z +inf.0]
         ;; Node stack (depth-first search)
         [nodes (list (list init-lb init-ub))]
         [nodes-explored 0])
    (let explore ()
      (cond
        ;; No more nodes
        [(null? nodes)
         (if best-x
             (make-ilp-optimal best-x best-z)
             (make-ilp-infeasible))]
        ;; Max nodes reached
        [(>= nodes-explored *ilp-max-nodes*)
         (if best-x
             (make-ilp-optimal best-x best-z)
             (make-ilp-infeasible))]
        [else
         (let* ([node (car nodes)]
                [lb (car node)]
                [ub (cadr node)])
           (set! nodes (cdr nodes))
           (set! nodes-explored (+ nodes-explored 1))
           ;; Create LP with bound constraints
           (let* ([lp (make-lp c A b)]
                  [bounded-lp (add-bound-constraints lp lb ub)]
                  [result (lp-solve bounded-lp)])
             (cond
               ;; Infeasible - prune this branch
               [(lp-infeasible? result)
                (explore)]
               ;; Unbounded - shouldn't happen with proper bounds
               [(lp-unbounded? result)
                (explore)]
               [else
                (let* ([x-full (lp-result-x result)]
                       ;; Extract original variables (first n)
                       [x (vector-take x-full n)]
                       [z (vec-dot c x)])
                  (cond
                    ;; Prune if worse than best known
                    [(>= z (- best-z *ilp-tolerance*))
                     (explore)]
                    ;; Check if integer feasible
                    [(solution-is-integer? x integers)
                     (when (< z best-z)
                       (set! best-x (round-solution x integers))
                       (set! best-z z))
                     (explore)]
                    ;; Branch on fractional variable
                    [else
                     (let* ([frac-var (find-fractional-var x integers)]
                            [frac-val (vector-ref x frac-var)]
                            [floor-val (floor frac-val)]
                            [ceil-val (ceiling frac-val)]
                            ;; Create two child nodes
                            ;; Left: x_j <= floor(x_j)
                            [lb-left (vec-copy lb)]
                            [ub-left (vec-copy ub)]
                            ;; Right: x_j >= ceil(x_j)
                            [lb-right (vec-copy lb)]
                            [ub-right (vec-copy ub)])
                       (vector-set! ub-left frac-var floor-val)
                       (vector-set! lb-right frac-var ceil-val)
                       ;; Add both children to stack (DFS)
                       (set! nodes (cons (list lb-left ub-left)
                                        (cons (list lb-right ub-right)
                                              nodes)))
                       (explore))]))])))]))))

;;; vector-take : Vec x Nat -> Vec
;;; Take first n elements of vector.
(define (vector-take v n)
  (let ([result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        [(= i n) result]
      (vector-set! result i (vector-ref v i)))))

;;; ====
;;; Gomory Cutting Planes
;;; ====

;;; Gomory cuts tighten the LP relaxation by adding constraints that
;;; cut off fractional solutions but preserve all integer solutions.
;;;
;;; Given a basic row: x_i + sum(a_ij * x_j) = b_i where x_i is basic
;;; If b_i is fractional, the Gomory cut is:
;;;   sum(frac(a_ij) * x_j) >= frac(b_i)
;;; where frac(x) = x - floor(x) is the fractional part.

;;; frac : Num -> Num
;;; Fractional part of a number.
(define (frac x)
  (- x (floor x)))

;;; generate-gomory-cut : LP x LPResult x Nat -> (Vec . Num) | #f
;;; Generate Gomory cut from row i of the optimal tableau.
;;; Returns (coefficients . rhs) for the cut, or #f if row is already integral.
(define (generate-gomory-cut lp result row-idx)
  (if (not (lp-optimal? result))
      #f
      (let* ([A (lp-A lp)]
             [b (lp-b lp)]
             [basis (lp-result-basis result)]
             [m (vector-length basis)]
             [n (matrix-cols A)]
             [B (extract-basis-matrix A basis)]
             [B-inv (matrix-inverse B)])
        (if (and (pair? B-inv) (eq? (car B-inv) 'error))
            #f
            (let* ([xB (matrix-vec-mul B-inv b)]
                   [bi (vector-ref xB row-idx)])
              (if (is-integer? bi)
                  #f  ; Row is integral, no cut needed
                  ;; Generate cut coefficients
                  (let ([cut-coef (make-vector n 0)]
                        [cut-rhs (frac bi)])
                    ;; For each non-basic variable
                    (do ([j 0 (+ j 1)])
                        [(= j n)]
                      (when (not (vec-contains? basis j))
                        (let* ([Aj (matrix-col A j)]
                               [aij-bar (vec-dot (matrix-row B-inv row-idx) Aj)])
                          (vector-set! cut-coef j (frac aij-bar)))))
                    (cons cut-coef cut-rhs))))))))

;;; matrix-row : Matrix x Nat -> Vec
;;; Extract row i as a vector.
(define (matrix-row m i)
  (let* ([n (matrix-cols m)]
         [row (make-vector n 0)])
    (do ([j 0 (+ j 1)])
        [(= j n) row]
      (vector-set! row j (matrix-ref m i j)))))

;;; add-gomory-cut : LP x Vec x Num -> LP
;;; Add a Gomory cut to the LP.
;;; sum(coef_j * x_j) >= rhs becomes sum(coef_j * x_j) - s = rhs, s >= 0
(define (add-gomory-cut lp coef rhs)
  (let* ([c (lp-c lp)]
         [A (lp-A lp)]
         [b (lp-b lp)]
         [m (matrix-rows A)]
         [n (matrix-cols A)]
         ;; Add slack variable (surplus for >= constraint)
         [new-n (+ n 1)]
         [new-m (+ m 1)]
         [new-A (make-matrix new-m new-n 0)]
         [new-b (make-vector new-m 0)]
         [new-c (vec-append c (vector 0))])
    ;; Copy original A and b
    (do ([i 0 (+ i 1)])
        [(= i m)]
      (do ([j 0 (+ j 1)])
          [(= j n)]
        (matrix-set! new-A i j (matrix-ref A i j)))
      (vector-set! new-b i (vector-ref b i)))
    ;; Add cut row: coef * x - s = rhs
    (do ([j 0 (+ j 1)])
        [(= j n)]
      (matrix-set! new-A m j (vector-ref coef j)))
    (matrix-set! new-A m n -1)  ; slack variable coefficient
    (vector-set! new-b m rhs)
    (make-lp new-c new-A new-b)))

;;; ilp-solve-cutting-plane : ILP -> ILPResult
;;; Solve ILP using cutting plane method with Gomory cuts.
(define (ilp-solve-cutting-plane ilp)
  (doc 'export #t)
  (let* ([integers (ilp-integers ilp)]
         [lp (ilp->lp ilp)])
    (let iterate ([current-lp lp] [cuts-added 0])
      (if (>= cuts-added *ilp-max-cuts*)
          ;; Fall back to branch-and-bound
          (ilp-solve ilp)
          (let ([result (lp-solve current-lp)])
            (cond
              [(lp-infeasible? result) (make-ilp-infeasible)]
              [(lp-unbounded? result) (make-ilp-unbounded)]
              [else
               (let* ([x (lp-result-x result)]
                      [n (vector-length (ilp-c ilp))]
                      [x-orig (vector-take x n)])
                 (if (solution-is-integer? x-orig integers)
                     (make-ilp-optimal (round-solution x-orig integers)
                                      (vec-dot (ilp-c ilp) x-orig))
                     ;; Find fractional row and add cut
                     (let ([cut (find-and-generate-cut current-lp result integers n)])
                       (if cut
                           (let ([new-lp (add-gomory-cut current-lp (car cut) (cdr cut))])
                             (iterate new-lp (+ cuts-added 1)))
                           ;; No cut found, fall back to B&B
                           (ilp-solve ilp)))))]))))))

;;; find-and-generate-cut : LP x LPResult x List x Nat -> (Vec . Num) | #f
;;; Find a row to generate a cut from.
(define (find-and-generate-cut lp result integers n-orig)
  (let* ([basis (lp-result-basis result)]
         [m (vector-length basis)])
    (let loop ([i 0])
      (if (= i m)
          #f
          (let ([basic-var (vector-ref basis i)])
            (if (and (< basic-var n-orig)
                     (memv basic-var integers))
                (let ([cut (generate-gomory-cut lp result i)])
                  (if cut cut (loop (+ i 1))))
                (loop (+ i 1))))))))

;;; ====
;;; Classic ILP Applications
;;; ====

;;; knapsack-solve : Vec x Vec x Num -> (Vec . Num) | 'infeasible
;;; Solve 0-1 knapsack: maximize v'x s.t. w'x <= W, x in {0,1}
;;; Returns (solution . value) or 'infeasible.
(define (knapsack-solve values weights capacity)
  (doc 'export #t)
  (let* ([n (vector-length values)]
         [m 1]  ; one constraint
         ;; Minimize -v'x (to maximize v'x)
         [c (vec-negate values)]
         ;; w'x <= W -> w'x + s = W
         [A (make-matrix 1 (+ n 1) 0)]
         [b (vector capacity)])
    ;; Set weight coefficients
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (matrix-set! A 0 i (vector-ref weights i)))
    ;; Slack variable
    (matrix-set! A 0 n 1)
    ;; Add binary constraints: x_i <= 1
    (let* ([eye-n (identity n)]
           [A-bin (make-matrix n (+ n 1) 0)]
           [b-bin (vec-ones n)])
      ;; Copy identity for x_i <= 1
      (do ([i 0 (+ i 1)])
          [(= i n)]
        (matrix-set! A-bin i i 1))
      ;; Stack constraints
      (let* ([A-full (matrix-vstack A A-bin)]
             [b-full (vec-append b b-bin)]
             ;; Add slack for binary constraints
             [A-slack (make-matrix (+ 1 n) n 0)])
        (do ([i 0 (+ i 1)])
            [(= i n)]
          (matrix-set! A-slack (+ 1 i) i 1))
        (let* ([A-final (matrix-hstack A-full A-slack)]
               [c-final (vec-append (vec-append c (vector 0)) (vec-zeros n))]
               [ilp (make-ilp c-final A-final b-full (iota n))]
               [result (ilp-solve ilp)])
          (if (ilp-optimal? result)
              (let* ([x-full (ilp-result-x result)]
                     [x (vector-take x-full n)]
                     [val (vec-dot values x)])
                (cons x val))
              'infeasible))))))

;;; set-cover-solve : Matrix x Vec -> (List Nat) | 'infeasible
;;; Solve minimum cost set cover.
;;; A is m x n coverage matrix (A_ij = 1 if set j covers element i).
;;; c is cost vector for each set.
;;; Returns list of set indices to select.
(define (set-cover-solve coverage costs)
  (doc 'export #t)
  (let* ([m (matrix-rows coverage)]
         [n (matrix-cols coverage)]
         ;; Constraint: each element must be covered (sum of covering sets >= 1)
         ;; Ax >= 1 -> Ax - s + a = 1 (with artificial vars for Phase I)
         ;; Actually, let's use: -Ax + s = -1, s >= 0
         ;; Better: Ax >= e (ones vector) -> -Ax <= -e -> -Ax + s = -e
         ;; Simpler: Ax + surplus = 1, surplus <= 0... complex.
         ;; Use standard form: Ax - surplus = 1, surplus >= 0 doesn't work.
         ;; Let's add slack directly: for Ax >= 1, use Ax - s = 1 with s >= 0
         ;; But Ax >= 1 means we need surplus variables.
         ;; Alternative: express as Ax >= 1 using LP from-inequality helper.
         [A-ge coverage]
         [b-ge (vec-ones m)]
         ;; Convert to standard form using lp-from-inequality
         ;; This adds surplus and artificial variables
         ;; Actually, let's build it manually for more control
         ;;
         ;; For binary set cover: minimize c'x s.t. Ax >= 1, x in {0,1}
         ;; Convert: -Ax <= -1 -> -Ax + s = -1 (but -1 < 0 is problematic)
         ;; Better: multiply through: Ax - s = 1 where s is surplus (not slack)
         ;; and s >= 0... this is still >= form.
         ;;
         ;; Standard approach: add artificial variables for >= constraints
         ;; Ax - s + a = 1, minimize original + M*sum(a), s >= 0, a >= 0
         ;;
         ;; For simplicity, use Big-M method directly:
         [BIG-M 1000000]
         [n-total (+ n m m)]  ; n original + m surplus + m artificial
         [A-full (make-matrix m n-total 0)]
         [b-full (vec-ones m)]
         [c-full (make-vector n-total 0)])
    ;; Set up constraint matrix
    (do ([i 0 (+ i 1)])
        [(= i m)]
      ;; Coverage coefficients
      (do ([j 0 (+ j 1)])
          [(= j n)]
        (matrix-set! A-full i j (matrix-ref coverage i j)))
      ;; Surplus variable (subtract)
      (matrix-set! A-full i (+ n i) -1)
      ;; Artificial variable (add)
      (matrix-set! A-full i (+ n m i) 1))
    ;; Set up cost vector
    (do ([j 0 (+ j 1)])
        [(= j n)]
      (vector-set! c-full j (vector-ref costs j)))
    ;; Artificial variables have Big-M cost
    (do ([i 0 (+ i 1)])
        [(= i m)]
      (vector-set! c-full (+ n m i) BIG-M))
    ;; Add binary constraints for x_j <= 1
    (let* ([A-bin (make-matrix n n-total 0)]
           [b-bin (vec-ones n)])
      (do ([j 0 (+ j 1)])
          [(= j n)]
        (matrix-set! A-bin j j 1))
      ;; Stack and add slack for binary constraints
      (let* ([A-stack (matrix-vstack A-full A-bin)]
             [b-stack (vec-append b-full b-bin)]
             [n-slack n]
             [A-bin-slack (make-matrix m n-slack 0)]
             [A-bin-slack2 (identity n)]
             [A-final (matrix-hstack A-stack
                                      (matrix-vstack A-bin-slack A-bin-slack2))]
             [c-final (vec-append c-full (vec-zeros n))]
             [ilp (make-ilp c-final A-final b-stack (iota n))]
             [result (ilp-solve ilp)])
        (cond
          ((not (ilp-optimal? result)) 'infeasible)
          ;; Check if Big-M artificial variables are used (indicates infeasibility)
          ((> (ilp-result-z result) (/ BIG-M 2)) 'infeasible)
          (else
           (let* ((x-full (ilp-result-x result))
                  (selected '()))
             (do ((j 0 (+ j 1)))
                 ((= j n) (reverse selected))
               (when (> (vector-ref x-full j) 0.5)
                 (set! selected (cons j selected)))))))))))

;;; ====
;;; Helper Functions
;;; ====

;; iota is provided by prelude (via lp.ss)

;;; vec-ones : Nat -> Vec
;;; Create vector of ones.
(define (vec-ones n)
  (make-vector n 1))

;;; vec-zeros : Nat -> Vec
;;; Create vector of zeros.
(define (vec-zeros n)
  (make-vector n 0))

;;; vec-negate : Vec -> Vec
;;; Negate all elements.
(define (vec-negate v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        [(= i n) result]
      (vector-set! result i (- (vector-ref v i))))))

;;; vec-append : Vec x Vec -> Vec
;;; Concatenate two vectors.
(define (vec-append v1 v2)
  (let* ([n1 (vector-length v1)]
         [n2 (vector-length v2)]
         [result (make-vector (+ n1 n2) 0)])
    (do ([i 0 (+ i 1)])
        [(= i n1)]
      (vector-set! result i (vector-ref v1 i)))
    (do ([i 0 (+ i 1)])
        [(= i n2) result]
      (vector-set! result (+ n1 i) (vector-ref v2 i)))))

;;; vec-copy : Vec -> Vec
;;; Copy a vector.
(define (vec-copy v)
  (let* ([n (vector-length v)]
         [result (make-vector n 0)])
    (do ([i 0 (+ i 1)])
        [(= i n) result]
      (vector-set! result i (vector-ref v i)))))

;;; vec-dot : Vec x Vec -> Num
;;; Dot product.
(define (vec-dot v1 v2)
  (let ([n (vector-length v1)]
        [sum 0])
    (do ([i 0 (+ i 1)])
        [(= i n) sum]
      (set! sum (+ sum (* (vector-ref v1 i) (vector-ref v2 i)))))))

;;; matrix-vstack : Matrix x Matrix -> Matrix
;;; Vertically stack two matrices.
(define (matrix-vstack m1 m2)
  (let* ([r1 (matrix-rows m1)]
         [r2 (matrix-rows m2)]
         [c (matrix-cols m1)]
         [result (make-matrix (+ r1 r2) c 0)])
    (do ([i 0 (+ i 1)])
        [(= i r1)]
      (do ([j 0 (+ j 1)])
          [(= j c)]
        (matrix-set! result i j (matrix-ref m1 i j))))
    (do ([i 0 (+ i 1)])
        [(= i r2) result]
      (do ([j 0 (+ j 1)])
          [(= j c)]
        (matrix-set! result (+ r1 i) j (matrix-ref m2 i j))))))

;;; matrix-hstack : Matrix x Matrix -> Matrix
;;; Horizontally stack two matrices.
(define (matrix-hstack m1 m2)
  (let* ([r (matrix-rows m1)]
         [c1 (matrix-cols m1)]
         [c2 (matrix-cols m2)]
         [result (make-matrix r (+ c1 c2) 0)])
    (do ([i 0 (+ i 1)])
        [(= i r)]
      (do ([j 0 (+ j 1)])
          [(= j c1)]
        (matrix-set! result i j (matrix-ref m1 i j)))
      (do ([j 0 (+ j 1)])
          [(= j c2)]
        (matrix-set! result i (+ c1 j) (matrix-ref m2 i j))))
    result))

;;; identity : Nat -> Matrix
;;; Create identity matrix.
(define (identity n)
  (let ([m (make-matrix n n 0)])
    (do ([i 0 (+ i 1)])
        [(= i n) m]
      (matrix-set! m i i 1))))

;;; ====
;;; Pretty Printing
;;; ====

;;; ilp-result->string : ILPResult -> String
(define (ilp-result->string result)
  (cond
    [(ilp-optimal? result)
     (string-append
       "Optimal integer solution:\n"
       "  x* = " (vec->string (ilp-result-x result)) "\n"
       "  z* = " (number->string (ilp-result-z result)))]
    [(ilp-infeasible? result)
     "No feasible integer solution exists"]
    [(ilp-unbounded? result)
     "Problem is unbounded"]
    [else
     "Unknown result"]))

;;; vec->string : Vec -> String
(define (vec->string v)
  (let ([parts (map number->string (vector->list v))])
    (string-append "[" (string-join parts ", ") "]")))

;;; string-join : List x String -> String
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))
