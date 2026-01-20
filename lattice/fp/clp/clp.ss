(load "lattice/fp/clp/global-constraints.ss")
(load "lattice/fp/clp/label.ss")

(doc 'module 'clp)
(doc 'description "Constraint Logic Programming Unified API - main entry point for CLP(FD)")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Provides goal-based interface compatible with miniKanren, convenience macros for common patterns, and solution enumeration and extraction")
(doc 'usage "Use (run-clp n goal-fn vars) to get up to n solutions for query variables")
(doc 'dependencies "global-constraints.ss, label.ss, logic.ss (for goal combinators)")

(doc 'section 'goal-constructors)
(doc 'note "CLP goals are functions: CStore → (Stream CStore). They can fail (return empty stream) or succeed with modified store.")

(define (clp-succeed cs)
  (doc 'type '(-> CStore (Stream CStore)))
  (doc 'description "Goal that always succeeds with the given store")
  (stream-cons cs (lambda () stream-nil)))

(define (clp-fail cs)
  (doc 'type '(-> CStore (Stream CStore)))
  (doc 'description "Goal that always fails")
  stream-nil)

(define (clp-goal propagator)
  (doc 'type '(-> (-> CStore (Maybe CStore)) (-> CStore (Stream CStore))))
  (doc 'description "Lift a propagator to a goal")
  (lambda (cs)
          (let ([result (propagator cs)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(doc 'section 'goal-combinators)

(define (clp-conj g1 g2)
  (doc 'type '(-> Goal Goal Goal))
  (doc 'description "Conjunction: run g1 then g2")
  (lambda (cs)
          (stream-flatmap
           (lambda (cs1) (g2 cs1))
           (g1 cs))))

(define (clp-disj g1 g2)
  (doc 'type '(-> Goal Goal Goal))
  (doc 'description "Disjunction: try both g1 and g2")
  (lambda (cs)
          (stream-interleave
           (g1 cs)
           (lambda () (g2 cs)))))

(define (clp-conj* goals)
  (doc 'type '(-> (List Goal) Goal))
  (doc 'description "Conjunction of multiple goals")
  (if (null? goals)
      clp-succeed
      (clp-conj (car goals) (clp-conj* (cdr goals)))))

(define (clp-disj* goals)
  (doc 'type '(-> (List Goal) Goal))
  (doc 'description "Disjunction of multiple goals")
  (if (null? goals)
      clp-fail
      (clp-disj (car goals) (clp-disj* (cdr goals)))))

(doc 'section 'constraint-goals)
(doc 'note "These wrap constraint functions as goals")

(define (goal-in-range var lo hi)
  (doc 'type '(-> LVar Int Int Goal))
  (doc 'description "Constrain var to range [lo, hi]")
  (clp-goal (in-range var lo hi)))

(define (goal-in-domain var vals)
  (doc 'type '(-> LVar (List Int) Goal))
  (doc 'description "Constrain var to list of values")
  (clp-goal (in-domain var vals)))

(define (goal-=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x = y")
  (doc 'note "Uses post-=fd to register constraint for re-propagation during labeling")
  (lambda (cs)
          (let ([result (post-=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal-<fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x < y")
  (doc 'note "Uses post-<fd to register constraint for re-propagation during labeling")
  (lambda (cs)
          (let ([result (post-<fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal-<=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x <= y")
  (lambda (cs)
          (let ([result (post-<=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal->fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x > y")
  (lambda (cs)
          (let ([result (post->fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal->=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x >= y")
  (lambda (cs)
          (let ([result (post->=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal-=/=fd x y)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x ≠ y (disequality)")
  (lambda (cs)
          (let ([result (post-=/=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal-+fd x y z)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x + y = z")
  (lambda (cs)
          (let ([result (post-+fd cs x y z)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal--fd x y z)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x - y = z")
  (lambda (cs)
          (let ([result (post--fd cs x y z)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal-*fd x y z)
  (doc 'type '(-> (Or LVar Int) (Or LVar Int) (Or LVar Int) Goal))
  (doc 'description "Constraint x * y = z")
  (lambda (cs)
          (let ([result (post-*fd cs x y z)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

(define (goal-all-different vars)
  (doc 'type '(-> (List LVar) Goal))
  (doc 'description "Constraint that all variables have distinct values")
  (doc 'note "Uses post-constraint to register for re-propagation during labeling")
  (lambda (cs)
          (let ([cs1 (post-constraint cs 'all-different vars (all-different vars))])
               (if cs1
                   (clp-succeed cs1)
                   (clp-fail cs)))))

(define (goal-sum-fd vars result)
  (doc 'type '(-> (List LVar) (Or LVar Int) Goal))
  (doc 'description "Constraint that sum of vars equals result")
  (doc 'note "Uses post-constraint to register for re-propagation during labeling")
  (lambda (cs)
          (let ([cs1 (post-sum-fd cs vars result)])
               (if cs1
                   (clp-succeed cs1)
                   (clp-fail cs)))))

(doc 'section 'labeling-goals)

(define (goal-label vars)
  (doc 'type '(-> (List LVar) Goal))
  (doc 'description "Default labeling strategy (first-fail, min-value)")
  (lambda (cs)
          (label cs vars)))

(define (goal-label-with var-order val-select vars)
  (doc 'type '(-> VarOrder ValSelect (List LVar) Goal))
  (doc 'description "Custom labeling with specified variable ordering and value selection strategies")
  (lambda (cs)
          (label-with var-order val-select cs vars)))

(doc 'section 'running-clp)

(define (run-clp n goal-fn vars)
  (doc 'type '(-> Nat (-> Goal) (List (List Int))))
  (doc 'description "Run CLP goal and return up to n solutions")
  (doc 'note "The goal-fn receives fresh variables and returns a goal")
  (let* ([cs (make-cstore)]
         [goal (goal-fn)]
         [solutions (goal cs)])
        (take-solutions n solutions vars)))

(define (take-solutions n stream vars)
  (doc 'type '(-> Nat (Stream CStore) (List LVar) (List (List Int))))
  (doc 'description "Extract variable values from solution stores")
  (if (or (<= n 0) (stream-nil? stream))
      '()
      (let* ([cs (stream-head stream)]
             [values (map (lambda (v) (cstore-get-value cs v)) vars)])
            (cons values
                  (take-solutions (- n 1) (stream-tail stream) vars)))))

(define (run-clp* n vars goal)
  (doc 'type '(-> Nat (List LVar) Goal (List CStore)))
  (doc 'description "Run goal and return solution stores")
  (let* ([cs (make-cstore)]
         [solutions (goal cs)])
        (stream->list-limit solutions n)))

(doc 'section 'convenience-api)

(define (clp-solve vars goal)
  (doc 'type '(-> (List LVar) Goal (Maybe CStore)))
  (doc 'description "Find first solution or #f")
  (let* ([cs (make-cstore)]
         [solutions (goal cs)])
        (if (stream-nil? solutions)
            #f
            (stream-head solutions))))

(define (clp-all vars goal)
  (doc 'type '(-> (List LVar) Goal (List CStore)))
  (doc 'description "Find all solutions (with reasonable limit of 1000)")
  (run-clp* 1000 vars goal))

(define (clp-count goal)
  (doc 'type '(-> Goal Nat))
  (doc 'description "Count solutions (up to limit of 10000)")
  (let* ([cs (make-cstore)]
         [solutions (goal cs)])
        (stream-count solutions 10000)))

(define (stream-count stream limit)
  (doc 'type '(-> Stream Nat Nat))
  (doc 'description "Count stream elements up to limit")
  (let loop ([s stream] [n 0])
       (if (or (>= n limit) (stream-nil? s))
           n
           (loop (stream-tail s) (+ n 1)))))

(doc 'section 'examples-n-queens)

(define (n-queens n)
  (doc 'type '(-> Nat Goal))
  (doc 'description "Set up N-Queens problem as a CLP goal")
  (let ([queens (map (lambda (i) (make-lvar 'q)) (range 0 n))])
       (clp-conj*
        (append
         ;; Each queen in [1, n]
         (map (lambda (q) (goal-in-range q 1 n)) queens)
         ;; All different columns
         (list (goal-all-different queens))
         ;; All different diagonals
         (queens-diagonal-constraints queens 0)
         ;; Label to find solutions
         (list (goal-label queens))))))

(define (queens-diagonal-constraints queens row)
  (doc 'type '(-> (List LVar) Nat (List Goal)))
  (doc 'description "Generate diagonal constraints for N-Queens")
  (if (null? queens)
      '()
      (append
       (queens-diagonal-with-rest (car queens) (cdr queens) row (+ row 1))
       (queens-diagonal-constraints (cdr queens) (+ row 1)))))

(define (queens-diagonal-with-rest q1 rest row1 row2)
  (doc 'type '(-> LVar (List LVar) Nat Nat (List Goal)))
  (doc 'description "Generates constraints: |q1 - q2| ≠ diff for each q2 in rest")
  (doc 'note "Checks (q1 - q2) ≠ diff AND (q1 - q2) ≠ -diff, equivalent to q1 ≠ q2 + diff AND q1 ≠ q2 - diff")
  (if (null? rest)
      '()
      (let* ([q2 (car rest)]
             [diff (- row2 row1)])
            ;; We need to express: |q1 - q2| ≠ diff
            ;; Create temporary vars for q1 - q2
            (cons (make-diagonal-constraint q1 q2 diff)
                  (queens-diagonal-with-rest q1 (cdr rest) row1 (+ row2 1))))))

(define (=/=fd-diagonal q1 q2 diff)
  (doc 'type '(-> LVar LVar Int (-> CStore (Maybe CStore))))
  (doc 'description "Constrain q1 such that |q1 - q2| ≠ diff")
  (doc 'note "Uses domain narrowing when one is ground")
  (lambda (cs)
          (let ([q1-val (cstore-get-value cs q1)]
                [q2-val (cstore-get-value cs q2)])
               (cond
                ;; Both ground
                [(and q1-val q2-val)
                 (if (= (abs (- q1-val q2-val)) diff)
                     #f
                     cs)]
                ;; q2 is ground - remove q2+diff and q2-diff from q1's domain
                [q2-val
                 (let* ([forbidden1 (+ q2-val diff)]
                        [forbidden2 (- q2-val diff)]
                        [cs1 (cstore-remove-value cs q1 forbidden1)])
                       (and cs1 (cstore-remove-value cs1 q1 forbidden2)))]
                ;; q1 is ground - remove q1+diff and q1-diff from q2's domain
                [q1-val
                 (let* ([forbidden1 (- q1-val diff)]
                        [forbidden2 (+ q1-val diff)]
                        [cs1 (cstore-remove-value cs q2 forbidden1)])
                       (and cs1 (cstore-remove-value cs1 q2 forbidden2)))]
                ;; Neither ground - constraint will be checked later
                [else cs]))))

(define (make-diagonal-constraint q1 q2 diff)
  (doc 'type '(-> LVar LVar Int Goal))
  (doc 'description "Create constraint: |q1 - q2| ≠ diff")
  (doc 'note "Registers the constraint for re-propagation during labeling")
  (lambda (cs)
          ;; Post the constraint so it's re-checked during labeling
          (let ([cs1 (post-constraint cs 'diagonal (list q1 q2) (=/=fd-diagonal q1 q2 diff))])
               (if cs1
                   (clp-succeed cs1)
                   (clp-fail cs)))))

(doc 'section 'examples-send-more-money)

(define (send-more-money)
  (doc 'type '(-> Goal))
  (doc 'description "Classic cryptarithmetic puzzle: SEND + MORE = MONEY")
  (let ([s (make-lvar 's)]
        [e (make-lvar 'e)]
        [n (make-lvar 'n)]
        [d (make-lvar 'd)]
        [m (make-lvar 'm)]
        [o (make-lvar 'o)]
        [r (make-lvar 'r)]
        [y (make-lvar 'y)])
       (let ([vars (list s e n d m o r y)])
            (clp-conj*
             (append
              ;; All digits 0-9
              (map (lambda (v) (goal-in-range v 0 9)) vars)
              ;; All different
              (list (goal-all-different vars))
              ;; Leading digits non-zero
              (list (goal->fd s 0))
              (list (goal->fd m 0))
              ;; SEND + MORE = MONEY as linear constraint
              ;; 1000*S + 100*E + 10*N + D +
              ;; 1000*M + 100*O + 10*R + E =
              ;; 10000*M + 1000*O + 100*N + 10*E + Y
              (list (goal-send-more-money-eq s e n d m o r y))
              ;; Label
              (list (goal-label vars)))))))

(define (goal-send-more-money-eq s e n d m o r y)
  (doc 'type '(-> LVar LVar LVar LVar LVar LVar LVar LVar Goal))
  (doc 'description "The arithmetic constraint for SEND + MORE = MONEY")
  (doc 'note "Simplified: check after labeling since arithmetic propagation is weak")
  (lambda (cs)
          ;; Check if all are ground
          (let ([sv (cstore-get-value cs s)]
                [ev (cstore-get-value cs e)]
                [nv (cstore-get-value cs n)]
                [dv (cstore-get-value cs d)]
                [mv (cstore-get-value cs m)]
                [ov (cstore-get-value cs o)]
                [rv (cstore-get-value cs r)]
                [yv (cstore-get-value cs y)])
               (if (and sv ev nv dv mv ov rv yv)
                   ;; All ground - check equation
                   (let ([send (+ (* 1000 sv) (* 100 ev) (* 10 nv) dv)]
                         [more (+ (* 1000 mv) (* 100 ov) (* 10 rv) ev)]
                         [money (+ (* 10000 mv) (* 1000 ov) (* 100 nv) (* 10 ev) yv)])
                        (if (= (+ send more) money)
                            (clp-succeed cs)
                            (clp-fail cs)))
                   ;; Not all ground - propagate constraints
                   (clp-succeed cs)))))

(doc 'section 'help)

(define (clp-help)
  (doc 'type '(-> Void))
  (doc 'description "Print CLP(FD) help message with available constraints and operations")
  (display "CLP(FD) - Constraint Logic Programming over Finite Domains\n\n")
  (display "Domain Constraints:\n")
  (display "  (goal-in-range var lo hi)    Constrain var to [lo, hi]\n")
  (display "  (goal-in-domain var vals)    Constrain var to list of values\n\n")
  (display "Arithmetic Constraints:\n")
  (display "  (goal-=fd x y)               x = y\n")
  (display "  (goal-<fd x y)               x < y\n")
  (display "  (goal-<=fd x y)              x <= y\n")
  (display "  (goal->fd x y)               x > y\n")
  (display "  (goal->=fd x y)              x >= y\n")
  (display "  (goal-=/=fd x y)             x ≠ y\n")
  (display "  (goal-+fd x y z)             x + y = z\n")
  (display "  (goal--fd x y z)             x - y = z\n")
  (display "  (goal-*fd x y z)             x * y = z\n\n")
  (display "Global Constraints:\n")
  (display "  (goal-all-different vars)    All vars have distinct values\n")
  (display "  (goal-sum-fd vars result)    Sum of vars = result\n\n")
  (display "Search:\n")
  (display "  (goal-label vars)            Label vars (first-fail, min)\n")
  (display "  (goal-label-with vo vs vars) Custom variable/value ordering\n\n")
  (display "Running:\n")
  (display "  (clp-solve vars goal)        First solution or #f\n")
  (display "  (clp-all vars goal)          All solutions\n")
  (display "  (clp-count goal)             Count solutions\n\n")
  (display "Examples:\n")
  (display "  (n-queens 8)                 8-Queens puzzle\n")
  (display "  (send-more-money)            SEND + MORE = MONEY\n"))
