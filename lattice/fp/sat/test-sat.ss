(load "core/testing/test-framework.ss")
(load "lattice/fp/sat/sat.ss")

(doc 'module 'test-sat)
(doc 'description "Tests for SAT solver")

(test-group "literal"
  (define-test "pos-lit creates positive literal"
    (assert-true (lit-positive? (pos-lit 5))))

  (define-test "neg-lit creates negative literal"
    (assert-true (lit-negative? (neg-lit 5))))

  (define-test "lit-var extracts variable"
    (assert-equal 5 (lit-var (pos-lit 5)))
    (assert-equal 5 (lit-var (neg-lit 5))))

  (define-test "lit-negate flips polarity"
    (assert-equal (neg-lit 3) (lit-negate (pos-lit 3)))
    (assert-equal (pos-lit 3) (lit-negate (neg-lit 3))))

  (define-test "lit-complement? detects x and ~x"
    (assert-true (lit-complement? (pos-lit 3) (neg-lit 3)))
    (assert-false (lit-complement? (pos-lit 3) (pos-lit 3)))))

(test-group "clause"
  (define-test "clause-from-list normalizes"
    (let ([c (clause-from-list '(3 1 2 1))])
      (assert-equal '(1 2 3) c)))

  (define-test "empty clause is empty"
    (assert-true (clause-empty? '())))

  (define-test "unit clause detected"
    (assert-true (clause-unit? '(5)))
    (assert-false (clause-unit? '(1 2))))

  (define-test "tautology detected"
    (assert-equal 'tautology (clause-from-list '(1 -1 2))))

  (define-test "clause-resolve resolves on variable"
    (let ([c1 '(1 2)]       ; x1 OR x2
          [c2 '(-1 3)])     ; ~x1 OR x3
      (let ([resolved (clause-resolve c1 c2 1)])
        (assert-equal '(2 3) resolved)))))

(test-group "cnf"
  (define-test "cnf-from-lists creates CNF"
    (let ([cnf (cnf-from-lists '((1 2) (-1 3)))])
      (assert-equal 2 (cnf-num-clauses cnf))))

  (define-test "cnf-trivially-unsat detects empty clause"
    (assert-true (cnf-trivially-unsat? (cnf-from-lists '((1 2) ())))))

  (define-test "cnf-trivially-sat detects no clauses"
    (assert-true (cnf-trivially-sat? cnf-empty)))

  (define-test "cnf-vars finds all variables"
    (let ([cnf (cnf-from-lists '((1 2) (-2 3)))])
      (assert-equal '(1 2 3) (cnf-vars cnf)))))

(test-group "assignment"
  (define-test "empty assignment has no values"
    (let ([a (make-assignment 5)])
      (assert-false (assignment-get a 1))
      (assert-false (assignment-assigned? a 1))))

  (define-test "assignment-decide adds to trail"
    (let* ([a0 (make-assignment 5)]
           [a1 (assignment-decide a0 1 'true 1)])
      (assert-equal 'true (assignment-get a1 1))
      (assert-equal 1 (assignment-level a1 1))))

  (define-test "assignment-backtrack-to removes later assignments"
    (let* ([a0 (make-assignment 5)]
           [a1 (assignment-decide a0 1 'true 1)]
           [a2 (assignment-decide a1 2 'false 2)]
           [a3 (assignment-backtrack-to a2 1)])
      (assert-equal 'true (assignment-get a3 1))
      (assert-false (assignment-get a3 2))))

  (define-test "eval-lit evaluates correctly"
    (let* ([a0 (make-assignment 5)]
           [a1 (assignment-decide a0 1 'true 1)])
      (assert-equal 'true (assignment-eval-lit a1 (pos-lit 1)))
      (assert-equal 'false (assignment-eval-lit a1 (neg-lit 1)))
      (assert-equal 'unknown (assignment-eval-lit a1 (pos-lit 2))))))

(test-group "sat-basic"
  (define-test "trivially satisfiable"
    (assert-equal 'sat (sat-solve '())))

  (define-test "trivially unsatisfiable"
    (assert-equal 'unsat (sat-solve '(()))))

  (define-test "single positive literal"
    (assert-equal 'sat (sat-solve '((1)))))

  (define-test "single negative literal"
    (assert-equal 'sat (sat-solve '((-1)))))

  (define-test "x AND ~x is unsat"
    (assert-equal 'unsat (sat-solve '((1) (-1)))))

  (define-test "simple satisfiable"
    (assert-equal 'sat (sat-solve '((1 2) (-1 2)))))

  (define-test "simple unsatisfiable"
    ;; (x1) AND (~x1 OR x2) AND (~x2) is unsatisfiable
    (assert-equal 'unsat (sat-solve '((1) (-1 2) (-2))))))

(test-group "sat-model"
  (define-test "model for single variable"
    (let ([model (sat-model '((1)))])
      (assert-true (pair? model))
      (assert-true (cdar model))))  ; x1 = true

  (define-test "model satisfies clauses"
    (let* ([clauses '((1 2) (-1 3) (-2 -3))]
           [model (sat-model clauses)])
      (assert-true (pair? model))
      ;; Verify model satisfies all clauses
      (assert-true (model-satisfies? model clauses)))))

(define (model-satisfies? model clauses)
  (doc 'description "Check if model satisfies all clauses")
  (all (lambda (clause)
               (any (lambda (lit)
                            (let* ([var (abs lit)]
                                   [entry (assv var model)])
                                  (and entry
                                       (if (> lit 0)
                                           (cdr entry)
                                           (not (cdr entry))))))
                    clause))
       clauses))

(define (all pred lst)
  (cond [(null? lst) #t]
        [(not (pred (car lst))) #f]
        [else (all pred (cdr lst))]))

(define (any pred lst)
  (cond [(null? lst) #f]
        [(pred (car lst)) #t]
        [else (any pred (cdr lst))]))

(test-group "cardinality"
  (define-test "at-most-one with two true is unsat"
    ;; x1 AND x2 AND at-most-one(x1, x2)
    (let ([clauses (cons '(1) (cons '(2) (at-most-one '(1 2))))])
      (assert-equal 'unsat (sat-solve clauses))))

  (define-test "exactly-one allows one"
    (let ([clauses (exactly-one '(1 2 3))])
      (assert-equal 'sat (sat-solve clauses))))

  (define-test "exactly-one with two forced is unsat"
    (let ([clauses (append '((1) (2)) (exactly-one '(1 2 3)))])
      (assert-equal 'unsat (sat-solve clauses)))))

(test-group "n-queens"
  (define-test "4-queens is satisfiable"
    (let ([clauses (n-queens-sat 4)])
      (assert-equal 'sat (sat-solve clauses))))

  (define-test "3-queens is unsatisfiable"
    ;; 3-queens has no solution
    (let ([clauses (n-queens-sat 3)])
      (assert-equal 'unsat (sat-solve clauses)))))

(test-group "graph-coloring"
  (define-test "triangle needs 3 colors"
    ;; Triangle: 0-1, 1-2, 0-2
    (let ([edges '((0 . 1) (1 . 2) (0 . 2))])
      ;; 2 colors should fail
      (assert-equal 'unsat (sat-solve (graph-coloring edges 3 2)))
      ;; 3 colors should work
      (assert-equal 'sat (sat-solve (graph-coloring edges 3 3)))))

  (define-test "path graph is 2-colorable"
    ;; Path: 0-1-2-3
    (let ([edges '((0 . 1) (1 . 2) (2 . 3))])
      (assert-equal 'sat (sat-solve (graph-coloring edges 4 2))))))

(run-all-tests)
