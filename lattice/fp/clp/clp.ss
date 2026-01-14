;;; lattice/fp/clp/clp.ss — Constraint Logic Programming Unified API
;;;
;;; Main entry point for CLP(FD). Provides:
;;;   - Goal-based interface compatible with miniKanren
;;;   - Convenience macros for common patterns
;;;   - Solution enumeration and extraction
;;;
;;; Usage:
;;;   (run-clp n (q) goals...)
;;;   Returns up to n solutions for query variable q.
;;;
;;; Dependencies:
;;;   - All CLP modules
;;;   - logic.ss (for goal combinators)

(load "lattice/fp/clp/global-constraints.ss")
(load "lattice/fp/clp/label.ss")

;;; ====
;;; Goal Constructors
;;; ====
;;;
;;; CLP goals are functions: CStore → (Stream CStore)
;;; They can fail (return empty stream) or succeed with modified store.

;;; clp-succeed : CStore → (Stream CStore)
(define (clp-succeed cs)
  (stream-cons cs (lambda () stream-nil)))

;;; clp-fail : CStore → (Stream CStore)
(define (clp-fail cs)
  stream-nil)

;;; clp-goal : (CStore → (Maybe CStore)) → (CStore → (Stream CStore))
;;; Lift a propagator to a goal.
(define (clp-goal propagator)
  (lambda (cs)
          (let ([result (propagator cs)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; ====
;;; Goal Combinators
;;; ====

;;; clp-conj : Goal × Goal → Goal
;;; Conjunction: run g1 then g2.
(define (clp-conj g1 g2)
  (lambda (cs)
          (stream-flatmap
           (lambda (cs1) (g2 cs1))
           (g1 cs))))

;;; clp-disj : Goal × Goal → Goal
;;; Disjunction: try both g1 and g2.
(define (clp-disj g1 g2)
  (lambda (cs)
          (stream-interleave
           (g1 cs)
           (lambda () (g2 cs)))))

;;; clp-conj* : (List Goal) → Goal
;;; Conjunction of multiple goals.
(define (clp-conj* goals)
  (if (null? goals)
      clp-succeed
      (clp-conj (car goals) (clp-conj* (cdr goals)))))

;;; clp-disj* : (List Goal) → Goal
;;; Disjunction of multiple goals.
(define (clp-disj* goals)
  (if (null? goals)
      clp-fail
      (clp-disj (car goals) (clp-disj* (cdr goals)))))

;;; ====
;;; Constraint Goals
;;; ====
;;;
;;; These wrap constraint functions as goals.

;;; goal-in-range : LVar × Int × Int → Goal
(define (goal-in-range var lo hi)
  (clp-goal (in-range var lo hi)))

;;; goal-in-domain : LVar × (List Int) → Goal
(define (goal-in-domain var vals)
  (clp-goal (in-domain var vals)))

;;; goal-=fd : (LVar | Int) × (LVar | Int) → Goal
;;; Uses post-=fd to register constraint for re-propagation during labeling.
(define (goal-=fd x y)
  (lambda (cs)
          (let ([result (post-=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal-<fd : (LVar | Int) × (LVar | Int) → Goal
;;; Uses post-<fd to register constraint for re-propagation during labeling.
(define (goal-<fd x y)
  (lambda (cs)
          (let ([result (post-<fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal-<=fd : (LVar | Int) × (LVar | Int) → Goal
(define (goal-<=fd x y)
  (lambda (cs)
          (let ([result (post-<=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal->fd : (LVar | Int) × (LVar | Int) → Goal
(define (goal->fd x y)
  (lambda (cs)
          (let ([result (post->fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal->=fd : (LVar | Int) × (LVar | Int) → Goal
(define (goal->=fd x y)
  (lambda (cs)
          (let ([result (post->=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal-=/=fd : (LVar | Int) × (LVar | Int) → Goal
(define (goal-=/=fd x y)
  (lambda (cs)
          (let ([result (post-=/=fd cs x y)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal-+fd : (LVar | Int) × (LVar | Int) × (LVar | Int) → Goal
(define (goal-+fd x y z)
  (lambda (cs)
          (let ([result (post-+fd cs x y z)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal--fd : (LVar | Int) × (LVar | Int) × (LVar | Int) → Goal
(define (goal--fd x y z)
  (lambda (cs)
          (let ([result (post--fd cs x y z)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal-*fd : (LVar | Int) × (LVar | Int) × (LVar | Int) → Goal
(define (goal-*fd x y z)
  (lambda (cs)
          (let ([result (post-*fd cs x y z)])
               (if result
                   (clp-succeed result)
                   (clp-fail cs)))))

;;; goal-all-different : (List LVar) → Goal
;;; Uses post-constraint to register for re-propagation during labeling.
(define (goal-all-different vars)
  (lambda (cs)
          (let ([cs1 (post-constraint cs 'all-different vars (all-different vars))])
               (if cs1
                   (clp-succeed cs1)
                   (clp-fail cs)))))

;;; goal-sum-fd : (List LVar) × (LVar | Int) → Goal
;;; Uses post-constraint to register for re-propagation during labeling.
(define (goal-sum-fd vars result)
  (lambda (cs)
          (let ([cs1 (post-sum-fd cs vars result)])
               (if cs1
                   (clp-succeed cs1)
                   (clp-fail cs)))))

;;; ====
;;; Labeling Goals
;;; ====

;;; goal-label : (List LVar) → Goal
;;; Default labeling (first-fail, min-value).
(define (goal-label vars)
  (lambda (cs)
          (label cs vars)))

;;; goal-label-with : VarOrder × ValSelect × (List LVar) → Goal
(define (goal-label-with var-order val-select vars)
  (lambda (cs)
          (label-with var-order val-select cs vars)))

;;; ====
;;; Running CLP
;;; ====

;;; run-clp : Nat × ((List LVar) → Goal) → (List (List Int))
;;; Run CLP goal and return up to n solutions.
;;; The goal-fn receives fresh variables and returns a goal.
(define (run-clp n goal-fn vars)
  (let* ([cs (make-cstore)]
         [goal (goal-fn)]
         [solutions (goal cs)])
        (take-solutions n solutions vars)))

;;; take-solutions : Nat × (Stream CStore) × (List LVar) → (List (List Int))
;;; Extract variable values from solution stores.
(define (take-solutions n stream vars)
  (if (or (<= n 0) (stream-nil? stream))
      '()
      (let* ([cs (stream-head stream)]
             [values (map (lambda (v) (cstore-get-value cs v)) vars)])
            (cons values
                  (take-solutions (- n 1) (stream-tail stream) vars)))))

;;; run-clp* : Nat × (List LVar) × Goal → (List CStore)
;;; Run goal and return solution stores.
(define (run-clp* n vars goal)
  (let* ([cs (make-cstore)]
         [solutions (goal cs)])
        (stream->list-limit solutions n)))

;;; ====
;;; Convenience API
;;; ====

;;; clp-solve : (List LVar) × Goal → (Maybe CStore)
;;; Find first solution or #f.
(define (clp-solve vars goal)
  (let* ([cs (make-cstore)]
         [solutions (goal cs)])
        (if (stream-nil? solutions)
            #f
            (stream-head solutions))))

;;; clp-all : (List LVar) × Goal → (List CStore)
;;; Find all solutions (with reasonable limit).
(define (clp-all vars goal)
  (run-clp* 1000 vars goal))

;;; clp-count : Goal → Nat
;;; Count solutions.
(define (clp-count goal)
  (let* ([cs (make-cstore)]
         [solutions (goal cs)])
        (stream-count solutions 10000)))

;;; stream-count : Stream × Nat → Nat
(define (stream-count stream limit)
  (let loop ([s stream] [n 0])
       (if (or (>= n limit) (stream-nil? s))
           n
           (loop (stream-tail s) (+ n 1)))))

;;; ====
;;; Example: N-Queens Helper
;;; ====

;;; n-queens : Nat → Goal
;;; Set up N-Queens problem.
(define (n-queens n)
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

;;; queens-diagonal-constraints : (List LVar) × Nat → (List Goal)
;;; Generate diagonal constraints for N-Queens.
(define (queens-diagonal-constraints queens row)
  (if (null? queens)
      '()
      (append
       (queens-diagonal-with-rest (car queens) (cdr queens) row (+ row 1))
       (queens-diagonal-constraints (cdr queens) (+ row 1)))))

;;; queens-diagonal-with-rest : LVar × (List LVar) × Nat × Nat → (List Goal)
;;; Generates constraints: |q1 - q2| ≠ diff for each q2 in rest.
;;; This requires checking that (q1 - q2) ≠ diff AND (q1 - q2) ≠ -diff
;;; which is equivalent to: q1 ≠ q2 + diff AND q1 ≠ q2 - diff
(define (queens-diagonal-with-rest q1 rest row1 row2)
  (if (null? rest)
      '()
      (let* ([q2 (car rest)]
             [diff (- row2 row1)])
            ;; We need to express: |q1 - q2| ≠ diff
            ;; Create temporary vars for q1 - q2
            (cons (make-diagonal-constraint q1 q2 diff)
                  (queens-diagonal-with-rest q1 (cdr rest) row1 (+ row2 1))))))

;;; =/=fd-diagonal : LVar × LVar × Int → (CStore → (Maybe CStore))
;;; Constrain q1 such that |q1 - q2| ≠ diff.
;;; Uses domain narrowing when one is ground.
(define (=/=fd-diagonal q1 q2 diff)
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

;;; make-diagonal-constraint : LVar × LVar × Int → Goal
;;; Create constraint: |q1 - q2| ≠ diff
;;; Registers the constraint for re-propagation during labeling.
(define (make-diagonal-constraint q1 q2 diff)
  (lambda (cs)
          ;; Post the constraint so it's re-checked during labeling
          (let ([cs1 (post-constraint cs 'diagonal (list q1 q2) (=/=fd-diagonal q1 q2 diff))])
               (if cs1
                   (clp-succeed cs1)
                   (clp-fail cs)))))

;;; ====
;;; Example: SEND + MORE = MONEY
;;; ====

;;; send-more-money : → Goal
;;; Classic cryptarithmetic puzzle.
(define (send-more-money)
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

;;; goal-send-more-money-eq : LVar^8 → Goal
;;; The arithmetic constraint for SEND + MORE = MONEY.
;;; Simplified: check after labeling since arithmetic propagation is weak.
(define (goal-send-more-money-eq s e n d m o r y)
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

;;; ====
;;; CLP Help
;;; ====

;;; clp-help : → Void
;;; Print help message.
(define (clp-help)
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
