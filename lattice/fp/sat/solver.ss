(load "lattice/fp/sat/cnf.ss")
(load "lattice/fp/sat/assignment.ss")
(load "lattice/fp/sat/watches.ss")

(doc 'module 'solver)
(doc 'description "CDCL SAT solver - Conflict-Driven Clause Learning")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Implements DPLL with: unit propagation, conflict analysis, clause learning, non-chronological backtracking")
(doc 'note "Uses Two-Watched Literals (2WL) for efficient unit propagation")
(doc 'fuel-bound "O(2^n) worst case, typically much better with clause learning")

(doc 'section 'solver-state)
(doc 'note "State = (state cnf assignment learned-clauses vsids-scores conflict-count watches)")

(define (make-solver-state cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF SolverState))
  (doc 'description "Create initial solver state from CNF")
  (let* ([nvars (cnf-num-vars cnf)]
         [assignment (make-assignment nvars)]
         [scores (make-vector (+ nvars 1) 0.0)]
         [watches (make-watches nvars)])
        ;; Initialize VSIDS scores from clause occurrences
        (for-each
         (lambda (clause)
                 (for-each
                  (lambda (lit)
                          (let ([var (lit-var lit)])
                               (vector-set! scores var
                                           (+ (vector-ref scores var) 1.0))))
                  clause))
         (cnf-clauses cnf))
        ;; Initialize watches for all clauses
        (watches-init-all! watches (cnf-clauses cnf))
        (list 'solver-state
              cnf
              assignment
              '()          ; learned clauses
              scores       ; VSIDS scores
              0            ; conflict count
              watches)))   ; 2WL watches

(define (state-cnf s) (list-ref s 1))
(define (state-assignment s) (list-ref s 2))
(define (state-learned s) (list-ref s 3))
(define (state-scores s) (list-ref s 4))
(define (state-conflicts s) (list-ref s 5))
(define (state-watches s) (list-ref s 6))

(define (state-set-assignment s a)
  (list 'solver-state (state-cnf s) a (state-learned s) (state-scores s) (state-conflicts s) (state-watches s)))

(define (state-add-learned s clause)
  ;; Also initialize watches for the learned clause
  (watches-init-clause! (state-watches s) clause)
  (list 'solver-state (state-cnf s) (state-assignment s)
        (cons clause (state-learned s)) (state-scores s) (state-conflicts s) (state-watches s)))

(define (state-inc-conflicts s)
  (list 'solver-state (state-cnf s) (state-assignment s)
        (state-learned s) (state-scores s) (+ 1 (state-conflicts s)) (state-watches s)))

(define (state-all-clauses s)
  (doc 'type '(-> SolverState (List Clause)))
  (doc 'description "Get all clauses including learned ones")
  (append (cnf-clauses (state-cnf s)) (state-learned s)))

(doc 'section 'unit-propagation)

;;; Unit propagation using Two-Watched Literals
;;; Much more efficient than scanning all clauses

(define (unit-propagate state level)
  (doc 'export #t)
  (doc 'type '(-> SolverState Nat (Values SolverState (Or #f Clause))))
  (doc 'description "Boolean Constraint Propagation using 2WL - propagate until fixpoint or conflict")
  (doc 'returns "Updated state and conflict clause (or #f if no conflict)")
  ;; First, find initial unit clauses (for initial propagation)
  (let ([initial-units (find-initial-units state)])
    (unit-propagate-loop state level initial-units)))

(define (unit-propagate-loop state level pending-units)
  (doc 'type '(-> SolverState Nat (List (Pair Literal Clause)) (Values SolverState (Or #f Clause))))
  (doc 'description "Process pending unit propagations using 2WL")
  (if (null? pending-units)
      (values state #f)  ; Fixpoint reached
      (let* ([unit (car pending-units)]
             [lit (car unit)]
             [clause (cdr unit)]
             [a (state-assignment state)])
        ;; Check if already assigned
        (if (assignment-assigned? a (lit-var lit))
            ;; Already assigned - check consistency
            (if (assignment-satisfies-lit? a lit)
                (unit-propagate-loop state level (cdr pending-units))
                ;; Conflict: propagating opposite of assigned value
                (values state clause))
            ;; Propagate this literal
            (let* ([var (lit-var lit)]
                   [val (if (lit-positive? lit) 'true 'false)]
                   [new-a (assignment-propagate a var val level clause)]
                   [new-state (state-set-assignment state new-a)]
                   ;; The negation of lit is now false - update watches
                   [false-lit (lit-negate lit)]
                   [prop-result (watches-propagate! (state-watches new-state) new-a false-lit)])
              (cond
               ;; Conflict detected
               [(and (pair? prop-result) (eq? (car prop-result) 'conflict))
                (values new-state (cadr prop-result))]
               ;; New units discovered
               [(and (pair? prop-result) (eq? (car prop-result) 'units))
                (unit-propagate-loop new-state level
                                     (append (cdr prop-result) (cdr pending-units)))]
               ;; No new units
               [else
                (unit-propagate-loop new-state level (cdr pending-units))]))))))

(define (find-initial-units state)
  (doc 'type '(-> SolverState (List (Pair Literal Clause))))
  (doc 'description "Find unit clauses in initial state (before any propagation)")
  (let ([a (state-assignment state)])
    (let loop ([clauses (state-all-clauses state)]
               [units '()])
      (cond
       [(null? clauses) units]
       [else
        (let ([lit (assignment-unit-lit a (car clauses))])
          (if lit
              (loop (cdr clauses) (cons (cons lit (car clauses)) units))
              (loop (cdr clauses) units)))]))))

;;; Legacy functions kept for compatibility and debugging
(define (find-unit-clause state)
  (doc 'type '(-> SolverState (Maybe (Pair Clause Literal))))
  (doc 'description "Find a unit clause and its unassigned literal (legacy, O(n) scan)")
  (let ([a (state-assignment state)])
       (let loop ([clauses (state-all-clauses state)])
            (cond
             [(null? clauses) #f]
             [else
              (let ([lit (assignment-unit-lit a (car clauses))])
                   (if lit
                       (cons (car clauses) lit)
                       (loop (cdr clauses))))]))))

(define (check-conflict state)
  (doc 'type '(-> SolverState (Maybe Clause)))
  (doc 'description "Check if any clause is falsified (legacy, O(n) scan)")
  (let ([a (state-assignment state)])
       (let loop ([clauses (state-all-clauses state)])
            (cond
             [(null? clauses) #f]
             [(eq? (assignment-eval-clause a (car clauses)) 'conflict)
              (car clauses)]
             [else (loop (cdr clauses))]))))

(doc 'section 'conflict-analysis)

(define (analyze-conflict state conflict-clause level)
  (doc 'export #t)
  (doc 'type '(-> SolverState Clause Nat (Values Clause Nat)))
  (doc 'description "Analyze conflict and compute learned clause via 1-UIP")
  (doc 'returns "Learned clause and backtrack level")
  (let ([a (state-assignment state)])
       (if (<= level 0)
           ;; Conflict at level 0 - unsatisfiable
           (values '() -1)
           ;; Do resolution until 1-UIP
           (let loop ([clause conflict-clause]
                      [trail (assignment-trail a)])
                (let ([count-at-level (count-lits-at-level clause a level)])
                     (if (= count-at-level 1)
                         ;; 1-UIP found
                         (values clause (compute-backtrack-level clause a level))
                         ;; Resolve with reason of most recent variable at current level
                         (let ([resolve-result (find-resolve-var clause trail a level)])
                              (if (not resolve-result)
                                  ;; Can't resolve further
                                  (values clause (max 0 (- level 1)))
                                  (let* ([var (car resolve-result)]
                                         [reason (cdr resolve-result)]
                                         [new-clause (clause-resolve clause reason var)])
                                        (if new-clause
                                            (loop new-clause (cdr trail))
                                            (values clause (max 0 (- level 1)))))))))))))

(define (count-lits-at-level clause a level)
  (doc 'type '(-> Clause Assignment Nat Nat))
  (doc 'description "Count literals in clause at given decision level")
  (let loop ([lits clause] [count 0])
       (cond
        [(null? lits) count]
        [else
         (let* ([var (lit-var (car lits))]
                [var-level (assignment-level a var)])
               (loop (cdr lits)
                     (if (= var-level level) (+ count 1) count)))])))

(define (find-resolve-var clause trail a level)
  (doc 'type '(-> Clause Trail Assignment Nat (Maybe (Pair VarId Clause))))
  (doc 'description "Find most recent variable at level with non-decision reason")
  (let loop ([trail trail])
       (cond
        [(null? trail) #f]
        [else
         (let* ([var (caar trail)]
                [var-level (assignment-level a var)]
                [reason (assignment-reason a var)]
                [lit-pos (pos-lit var)]
                [lit-neg (neg-lit var)]
                [in-clause (or (member lit-pos clause)
                              (member lit-neg clause))])
               (if (and (= var-level level)
                        in-clause
                        (list? reason))  ; Has clause reason (not 'decision)
                   (cons var reason)
                   (loop (cdr trail))))])))

(define (compute-backtrack-level clause a current-level)
  (doc 'type '(-> Clause Assignment Nat Nat))
  (doc 'description "Compute backtrack level (second highest level in clause)")
  (let ([levels (map (lambda (lit)
                             (assignment-level a (lit-var lit)))
                     clause)])
       (let ([sorted (list-sort > levels)])
            (if (or (null? sorted) (null? (cdr sorted)))
                0
                (cadr sorted)))))

(doc 'section 'variable-selection)

(define (pick-branching-var state)
  (doc 'export #t)
  (doc 'type '(-> SolverState (Maybe VarId)))
  (doc 'description "Select unassigned variable using VSIDS heuristic")
  (let* ([a (state-assignment state)]
         [scores (state-scores state)]
         [vars (cnf-vars (state-cnf state))])
        (let loop ([vars vars]
                   [best-var #f]
                   [best-score -1.0])
             (cond
              [(null? vars)
               best-var]
              [else
               (let ([var (car vars)])
                    (if (assignment-assigned? a var)
                        (loop (cdr vars) best-var best-score)
                        (let ([score (vector-ref scores var)])
                             (if (> score best-score)
                                 (loop (cdr vars) var score)
                                 (loop (cdr vars) best-var best-score)))))]))))

(define (bump-vsids-scores state clause)
  (doc 'type '(-> SolverState Clause SolverState))
  (doc 'description "Bump VSIDS scores for variables in conflict clause")
  (let ([scores (state-scores state)])
       (for-each
        (lambda (lit)
                (let ([var (lit-var lit)])
                     (vector-set! scores var
                                 (+ (vector-ref scores var) 1.0))))
        clause)
       state))

(define (decay-vsids-scores state)
  (doc 'type '(-> SolverState SolverState))
  (doc 'description "Decay all VSIDS scores")
  (let ([scores (state-scores state)]
        [nvars (cnf-num-vars (state-cnf state))])
       (let loop ([i 1])
            (when (<= i nvars)
                  (vector-set! scores i (* 0.95 (vector-ref scores i)))
                  (loop (+ i 1))))
       state))

(doc 'section 'main-solver)

(define (solve cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF (Or 'sat 'unsat 'unknown)))
  (doc 'description "Solve SAT instance, returning 'sat, 'unsat, or 'unknown (timeout)")
  (let ([result (solve-with-model cnf)])
       (cond
        [(eq? result 'timeout) 'unknown]
        [result 'sat]
        [else 'unsat])))

(define *sat-fuel-limit* 10000)
(doc *sat-fuel-limit* 'description "Maximum iterations before timeout (can be increased)")

(define (solve-with-model cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF (Or Assignment 'timeout #f)))
  (doc 'description "Solve SAT instance, returning assignment, 'timeout, or #f (unsat)")
  (cond
   [(cnf-trivially-unsat? cnf) #f]
   [(cnf-trivially-sat? cnf) (make-assignment 0)]
   [else (cdcl-solve (make-solver-state cnf))]))

(define (cdcl-solve state)
  (doc 'type '(-> SolverState (Or Assignment 'timeout #f)))
  (doc 'description "Main CDCL solving loop")
  (cdcl-loop state 0 *sat-fuel-limit*))

(define (cdcl-loop state level fuel)
  (doc 'type '(-> SolverState Nat Nat (Or Assignment 'timeout #f)))
  (if (<= fuel 0)
      'timeout  ; Out of fuel - return timeout, not unsat
      (let-values ([(state conflict) (unit-propagate state level)])
                  (if conflict
                      ;; Conflict - analyze and backtrack
                      (if (= level 0)
                          #f  ; Conflict at level 0 - unsatisfiable
                          (let-values ([(learned-clause bt-level)
                                        (analyze-conflict state conflict level)])
                                      (if (< bt-level 0)
                                          #f  ; Unsatisfiable
                                          (let* ([state (state-inc-conflicts state)]
                                                 [state (bump-vsids-scores state learned-clause)]
                                                 [state (if (= 0 (modulo (state-conflicts state) 100))
                                                            (decay-vsids-scores state)
                                                            state)]
                                                 [state (state-add-learned state learned-clause)]
                                                 [a (assignment-backtrack-to
                                                     (state-assignment state) bt-level)]
                                                 [state (state-set-assignment state a)])
                                                (cdcl-loop state bt-level (- fuel 1))))))
                      ;; No conflict - try to decide
                      (let ([var (pick-branching-var state)])
                           (if (not var)
                               ;; All variables assigned - SAT!
                               (state-assignment state)
                               ;; Make a decision
                               (let* ([new-level (+ level 1)]
                                      [a (assignment-decide
                                          (state-assignment state) var 'true new-level)]
                                      [state (state-set-assignment state a)])
                                     (cdcl-loop state new-level (- fuel 1)))))))))

(doc 'section 'solver-statistics)

(define (solver-stats state)
  (doc 'export #t)
  (doc 'type '(-> SolverState (List (Pair Symbol Any))))
  (doc 'description "Get solver statistics")
  (list (cons 'conflicts (state-conflicts state))
        (cons 'learned-clauses (length (state-learned state)))
        (cons 'variables (cnf-num-vars (state-cnf state)))
        (cons 'original-clauses (cnf-num-clauses (state-cnf state)))))
