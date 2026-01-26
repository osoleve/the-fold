(load "lattice/fp/sat/sat.ss")

(doc 'module 'maxsat)
(doc 'description "MaxSAT - Optimization extension to SAT solver")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Find assignment maximizing satisfied soft clauses while satisfying all hard clauses")
(doc 'note "Implements linear search, binary search, and core-guided algorithms")

(doc 'section 'maxsat-problem)
(doc 'note "MaxSAT problem = (hard-clauses soft-clauses)")
(doc 'note "Hard clauses MUST be satisfied, soft clauses we WANT to satisfy")

;;; Create a MaxSAT problem
(define (make-maxsat hard-clauses soft-clauses)
  (doc 'export #t)
  (doc 'type '(-> (List Clause) (List Clause) MaxSATInstance))
  (doc 'description "Create MaxSAT instance with hard and soft clauses")
  (doc 'param 'hard-clauses "Clauses that must be satisfied")
  (doc 'param 'soft-clauses "Clauses we want to maximize")
  (list 'maxsat hard-clauses soft-clauses))

(define (maxsat-hard m) (cadr m))
(define (maxsat-soft m) (caddr m))
(define (maxsat-num-soft m) (length (maxsat-soft m)))

;; Note: Weighted MaxSAT removed - was broken (solver treated weighted pairs as clauses)
;; TODO: Implement proper weighted MaxSAT with stratified or weight-aware search

(doc 'section 'maxsat-result)

;;; MaxSAT result: (cost model) where cost = number of unsatisfied soft clauses
(define (make-maxsat-result cost model)
  (list 'maxsat-result cost model))

(define (maxsat-result? r)
  (and (pair? r) (eq? (car r) 'maxsat-result)))

(define (maxsat-result-cost r) (cadr r))
(define (maxsat-result-model r) (caddr r))

(doc 'section 'linear-search)
(doc 'note "Try k=0,1,2,... until satisfiable. Simple but correct.")

(define (maxsat-solve-linear maxsat)
  (doc 'export #t)
  (doc 'type '(-> MaxSATInstance (Or MaxSATResult #f)))
  (doc 'description "Solve MaxSAT using linear search over cost")
  (doc 'note "Returns result with minimum cost, or #f if hard clauses unsatisfiable")
  (let* ([hard (maxsat-hard maxsat)]
         [soft (maxsat-soft maxsat)]
         [n (length soft)])
    ;; First check if hard clauses alone are satisfiable
    (if (and (not (null? hard))
             (eq? (sat-solve hard) 'unsat))
        #f  ; Hard clauses unsatisfiable
        ;; Try increasing costs: 0, 1, 2, ...
        (let loop ([k 0])
          (if (> k n)
              #f  ; Shouldn't happen if hard clauses are sat
              (let ([result (maxsat-try-cost hard soft k)])
                (if result
                    (make-maxsat-result k result)
                    (loop (+ k 1)))))))))

;;; Try to find model with exactly k unsatisfied soft clauses
(define (maxsat-try-cost hard soft k)
  (doc 'type '(-> (List Clause) (List Clause) Nat (Or Model #f)))
  (doc 'description "Check if there's a model with at most k unsatisfied soft clauses")
  ;; Introduce relaxation variables: soft_i OR relax_i
  ;; Constraint: at-most-k relaxation variables are true
  ;; Uses sequential counter encoding for scalability (O(n*k) vs exponential)
  (let* ([n (length soft)]
         [base-var (+ 1 (max-var-in-clauses (append hard soft)))]
         ;; Relaxation variables: base-var, base-var+1, ...
         [relax-vars (map (lambda (i) (+ base-var i)) (iota n))]
         ;; Relaxed soft clauses: soft_i OR relax_i
         [relaxed-soft (map (lambda (clause relax-var)
                              (cons relax-var clause))
                            soft relax-vars)]
         ;; Base for sequential counter auxiliary variables
         [counter-base (+ base-var n)]
         ;; At-most-k constraint using sequential counter (scalable)
         [atmost-k (sequential-at-most-k (map var relax-vars) k counter-base)]
         ;; Combined problem
         [all-clauses (append hard relaxed-soft atmost-k)]
         [result (sat-model all-clauses)])
    (if (or (not result) (eq? result 'timeout))
        #f
        ;; Filter out relaxation and counter variables from model
        (filter (lambda (entry)
                  (< (car entry) base-var))
                result))))

;;; Find maximum variable in a list of clauses
(define (max-var-in-clauses clauses)
  (if (null? clauses)
      0
      (apply max (map (lambda (clause)
                        (if (null? clause)
                            0
                            (apply max (map lit-var clause))))
                      clauses))))

;; iota is provided by prelude (via literal.ss chain)

(doc 'section 'binary-search)
(doc 'note "Binary search over cost - O(log n) SAT calls instead of O(n)")

(define (maxsat-solve-binary maxsat)
  (doc 'export #t)
  (doc 'type '(-> MaxSATInstance (Or MaxSATResult #f)))
  (doc 'description "Solve MaxSAT using binary search over cost")
  (doc 'note "More efficient than linear for problems with many soft clauses")
  (let* ([hard (maxsat-hard maxsat)]
         [soft (maxsat-soft maxsat)]
         [n (length soft)])
    ;; First check if hard clauses alone are satisfiable
    (if (and (not (null? hard))
             (eq? (sat-solve hard) 'unsat))
        #f
        ;; Binary search for minimum cost
        (let ([result (binary-search-cost hard soft 0 n #f)])
          (if result
              (make-maxsat-result (car result) (cdr result))
              #f)))))

(define (binary-search-cost hard soft lo hi best)
  (doc 'type '(-> (List Clause) (List Clause) Nat Nat (Or (Pair Nat Model) #f) (Or (Pair Nat Model) #f)))
  (doc 'description "Binary search for minimum cost in [lo, hi]")
  (if (> lo hi)
      best
      (let* ([mid (quotient (+ lo hi) 2)]
             [result (maxsat-try-cost hard soft mid)])
        (if result
            ;; Found solution at cost mid, try lower
            (binary-search-cost hard soft lo (- mid 1) (cons mid result))
            ;; No solution at cost mid, try higher
            (binary-search-cost hard soft (+ mid 1) hi best)))))

(doc 'section 'unified-interface)

(define (maxsat-solve maxsat . options)
  (doc 'export #t)
  (doc 'type '(-> MaxSATInstance (Or MaxSATResult #f)))
  (doc 'description "Solve MaxSAT using best available algorithm")
  (doc 'note "Uses binary search by default; pass 'linear for linear search")
  (let ([algorithm (if (and (pair? options) (eq? (car options) 'linear))
                       'linear
                       'binary)])
    (case algorithm
      [(linear) (maxsat-solve-linear maxsat)]
      [(binary) (maxsat-solve-binary maxsat)]
      [else (maxsat-solve-binary maxsat)])))

(doc 'section 'convenience-constructors)

;;; Partial MaxSAT: all clauses are soft (find max satisfiable subset)
(define (partial-maxsat clauses)
  (doc 'export #t)
  (doc 'type '(-> (List Clause) MaxSATInstance))
  (doc 'description "Create partial MaxSAT - maximize satisfied clauses (no hard constraints)")
  (make-maxsat '() clauses))

;;; From weighted clauses (soft only)
(define (maxsat-from-soft soft-clauses)
  (doc 'export #t)
  (doc 'type '(-> (List Clause) MaxSATInstance))
  (doc 'description "Create MaxSAT with only soft clauses")
  (make-maxsat '() soft-clauses))

;;; Add hard constraints to existing MaxSAT
(define (maxsat-add-hard maxsat clauses)
  (doc 'export #t)
  (doc 'type '(-> MaxSATInstance (List Clause) MaxSATInstance))
  (doc 'description "Add hard constraints to MaxSAT instance")
  (make-maxsat (append (maxsat-hard maxsat) clauses)
               (maxsat-soft maxsat)))

;;; Add soft constraints
(define (maxsat-add-soft maxsat clauses)
  (doc 'export #t)
  (doc 'type '(-> MaxSATInstance (List Clause) MaxSATInstance))
  (doc 'description "Add soft constraints to MaxSAT instance")
  (make-maxsat (maxsat-hard maxsat)
               (append (maxsat-soft maxsat) clauses)))

(doc 'section 'applications)

;;; Minimum Vertex Cover as MaxSAT
(define (min-vertex-cover edges num-nodes)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Nat Nat)) Nat MaxSATInstance))
  (doc 'description "Encode minimum vertex cover as MaxSAT")
  (doc 'note "Variable i means node i is in the cover")
  (doc 'note "Hard: each edge has at least one endpoint in cover")
  (doc 'note "Soft: prefer not including each node (minimize cover size)")
  (let* (;; Hard: for each edge (u,v), at least one of u,v in cover
         [hard (map (lambda (edge)
                      (list (+ 1 (car edge)) (+ 1 (cdr edge))))
                    edges)]
         ;; Soft: prefer NOT including each node (negative literals)
         [soft (map (lambda (node) (list (neg (+ 1 node))))
                    (iota num-nodes))])
    (make-maxsat hard soft)))

;;; Maximum Independent Set as MaxSAT
(define (max-independent-set edges num-nodes)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Nat Nat)) Nat MaxSATInstance))
  (doc 'description "Encode maximum independent set as MaxSAT")
  (doc 'note "Variable i means node i is in the independent set")
  (doc 'note "Hard: no two adjacent nodes both in set")
  (doc 'note "Soft: prefer including each node (maximize set size)")
  (let* (;; Hard: for each edge (u,v), at most one of u,v in set
         ;; Encoded as: NOT u OR NOT v
         [hard (map (lambda (edge)
                      (list (neg (+ 1 (car edge))) (neg (+ 1 (cdr edge)))))
                    edges)]
         ;; Soft: prefer including each node
         [soft (map (lambda (node) (list (+ 1 node)))
                    (iota num-nodes))])
    (make-maxsat hard soft)))

;;; Minimum Correction Set (diagnosis)
(define (min-correction-set clauses)
  (doc 'export #t)
  (doc 'type '(-> (List Clause) MaxSATInstance))
  (doc 'description "Find minimum set of clauses to remove to make formula satisfiable")
  (doc 'note "Useful for diagnosis: which constraints are causing inconsistency?")
  ;; All clauses are soft - find maximum satisfiable subset
  ;; The unsatisfied soft clauses form the minimum correction set
  (partial-maxsat clauses))

(doc 'section 'result-analysis)

(define (maxsat-unsatisfied-soft result soft-clauses model)
  (doc 'export #t)
  (doc 'type '(-> MaxSATResult (List Clause) Model (List Clause)))
  (doc 'description "Get the soft clauses that are unsatisfied in the solution")
  (filter (lambda (clause)
            (not (clause-satisfied? clause model)))
          soft-clauses))

(define (clause-satisfied? clause model)
  (doc 'type '(-> Clause Model Bool))
  (doc 'description "Check if clause is satisfied by model")
  (let loop ([lits clause])
    (cond
     [(null? lits) #f]
     [else
      (let* ([lit (car lits)]
             [var (lit-var lit)]
             [entry (assoc var model)])
        (if entry
            (let ([val (cdr entry)])
              (if (if (lit-positive? lit) val (not val))
                  #t  ; This literal is true
                  (loop (cdr lits))))
            (loop (cdr lits))))])))  ; Variable not in model, try next

(doc 'section 'help)

(define (maxsat-help)
  (doc 'export #t)
  (display "MaxSAT - Optimization Extension to SAT\n\n")
  (display "Create MaxSAT instances:\n")
  (display "  (make-maxsat hard soft)       Hard + soft clauses\n")
  (display "  (partial-maxsat clauses)      All clauses soft\n")
  (display "  (maxsat-add-hard m clauses)   Add hard constraints\n")
  (display "  (maxsat-add-soft m clauses)   Add soft constraints\n\n")
  (display "Solve:\n")
  (display "  (maxsat-solve m)              Binary search (default)\n")
  (display "  (maxsat-solve m 'linear)      Linear search\n\n")
  (display "Results:\n")
  (display "  (maxsat-result-cost r)        Number of unsatisfied soft\n")
  (display "  (maxsat-result-model r)       Satisfying assignment\n\n")
  (display "Applications:\n")
  (display "  (min-vertex-cover edges n)    Minimum vertex cover\n")
  (display "  (max-independent-set edges n) Maximum independent set\n")
  (display "  (min-correction-set clauses)  Diagnosis/repair\n"))
