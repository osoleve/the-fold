(load "lattice/fp/sat/solver.ss")

(doc 'module 'sat)
(doc 'description "SAT Solver - Boolean Satisfiability with CDCL")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'exports "sat-solve, sat-satisfiable?, sat-model, cnf-from-clauses, and-clause, or-clause, implies, iff, at-most-one, exactly-one")
(doc 'usage "Create CNF with helpers, solve with sat-solve or sat-model")

(doc 'section 'high-level-api)

(define (sat-solve clauses)
  (doc 'export #t)
  (doc 'type '(-> (List (List Literal)) (Or 'sat 'unsat)))
  (doc 'description "Solve SAT from list of clause lists")
  (doc 'example "((1 2) (-1 3)) means (x1 OR x2) AND (NOT x1 OR x3)")
  (solve (cnf-from-lists clauses)))

(define (sat-satisfiable? clauses)
  (doc 'export #t)
  (doc 'type '(-> (List (List Literal)) Bool))
  (doc 'description "Check if clause set is satisfiable")
  (eq? (sat-solve clauses) 'sat))

(define (sat-model clauses)
  (doc 'export #t)
  (doc 'type '(-> (List (List Literal)) (Maybe (List (Pair VarId Bool)))))
  (doc 'description "Get satisfying assignment if exists")
  (let ([result (solve-with-model (cnf-from-lists clauses))])
       (if result
           (assignment-to-model result)
           #f)))

(define (assignment-to-model a)
  (doc 'type '(-> Assignment (List (Pair VarId Bool))))
  (doc 'description "Convert assignment to list of (var . bool) pairs")
  (map (lambda (entry)
               (cons (car entry) (eq? (cdr entry) 'true)))
       (reverse (assignment-trail a))))

(doc 'section 'cnf-building-helpers)

(define (var n)
  (doc 'export #t)
  (doc 'type '(-> Nat Literal))
  (doc 'description "Create positive literal for variable n")
  (doc 'note "Variables are numbered starting from 1")
  (pos-lit n))

(define (neg n)
  (doc 'export #t)
  (doc 'type '(-> Nat Literal))
  (doc 'description "Create negative literal for variable n")
  (neg-lit n))

(define (implies a b)
  (doc 'export #t)
  (doc 'type '(-> Literal Literal (List Literal)))
  (doc 'description "a => b as clause: (~a OR b)")
  (list (lit-negate a) b))

(define (iff a b)
  (doc 'export #t)
  (doc 'type '(-> Literal Literal (List (List Literal))))
  (doc 'description "a <=> b as clauses: (a => b) AND (b => a)")
  (list (implies a b) (implies b a)))

(define (at-most-one vars)
  (doc 'export #t)
  (doc 'type '(-> (List Literal) (List (List Literal))))
  (doc 'description "At most one of vars is true - pairwise encoding")
  (doc 'note "For each pair (xi, xj), add clause (~xi OR ~xj)")
  (let loop ([vs vars] [clauses '()])
       (if (null? vs)
           clauses
           (let inner ([rest (cdr vs)] [clauses clauses])
                (if (null? rest)
                    (loop (cdr vs) clauses)
                    (inner (cdr rest)
                           (cons (list (lit-negate (car vs))
                                      (lit-negate (car rest)))
                                 clauses)))))))

(define (at-least-one vars)
  (doc 'export #t)
  (doc 'type '(-> (List Literal) (List Literal)))
  (doc 'description "At least one of vars is true - single clause")
  vars)

(define (exactly-one vars)
  (doc 'export #t)
  (doc 'type '(-> (List Literal) (List (List Literal))))
  (doc 'description "Exactly one of vars is true")
  (cons (at-least-one vars) (at-most-one vars)))

(doc 'section 'cardinality-constraints)

(define (at-most-k vars k)
  (doc 'export #t)
  (doc 'type '(-> (List Literal) Nat (List (List Literal))))
  (doc 'description "At most k of vars are true - binomial encoding")
  (doc 'note "Generates all (n choose k+1) clauses")
  (if (>= k (length vars))
      '()  ; Trivially satisfied
      (map (lambda (subset)
                   (map lit-negate subset))
           (combinations vars (+ k 1)))))

(define (at-least-k vars k)
  (doc 'export #t)
  (doc 'type '(-> (List Literal) Nat (List (List Literal))))
  (doc 'description "At least k of vars are true")
  (doc 'note "Equivalent to: at most (n-k) negations are true")
  (if (<= k 0)
      '()  ; Trivially satisfied
      (at-most-k (map lit-negate vars) (- (length vars) k))))

(define (exactly-k vars k)
  (doc 'export #t)
  (doc 'type '(-> (List Literal) Nat (List (List Literal))))
  (doc 'description "Exactly k of vars are true")
  (append (at-least-k vars k) (at-most-k vars k)))

(define (combinations lst k)
  (doc 'type '(-> (List a) Nat (List (List a))))
  (doc 'description "Generate all k-combinations of list")
  (cond
   [(= k 0) '(())]
   [(null? lst) '()]
   [else
    (append
     (map (lambda (rest) (cons (car lst) rest))
          (combinations (cdr lst) (- k 1)))
     (combinations (cdr lst) k))]))

(doc 'section 'common-patterns)

(define (graph-coloring edges num-nodes num-colors)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Nat Nat)) Nat Nat (List (List Literal))))
  (doc 'description "Encode graph coloring as SAT")
  (doc 'note "Variable (node * num-colors + color + 1) represents node has color")
  (let ([var (lambda (n c) (+ (* n num-colors) c 1))])
       (append
        ;; Each node has exactly one color
        (apply append
               (map (lambda (node)
                            (exactly-one
                             (map (lambda (c) (var node c))
                                  (range 0 num-colors))))
                    (range 0 num-nodes)))
        ;; Adjacent nodes have different colors
        (apply append
               (map (lambda (edge)
                            (let ([u (car edge)]
                                  [v (cdr edge)])
                                 (map (lambda (c)
                                              (list (neg (var u c))
                                                    (neg (var v c))))
                                      (range 0 num-colors))))
                    edges)))))

(define (n-queens-sat n)
  (doc 'export #t)
  (doc 'type '(-> Nat (List (List Literal))))
  (doc 'description "Encode N-Queens as SAT")
  (doc 'note "Variable (row * n + col + 1) represents queen at (row, col)")
  (let ([var (lambda (r c) (+ (* r n) c 1))])
       (append
        ;; Each row has exactly one queen
        (apply append
               (map (lambda (r)
                            (exactly-one (map (lambda (c) (var r c)) (range 0 n))))
                    (range 0 n)))
        ;; Each column has at most one queen
        (apply append
               (map (lambda (c)
                            (at-most-one (map (lambda (r) (var r c)) (range 0 n))))
                    (range 0 n)))
        ;; Diagonals have at most one queen
        (queens-diagonal-constraints-sat n var))))

(define (queens-diagonal-constraints-sat n var)
  (doc 'type '(-> Nat (-> Nat Nat Literal) (List (List Literal))))
  (apply append
         (append
          ;; Down-right diagonals
          (map (lambda (d)
                       (at-most-one
                        (filter-map
                         (lambda (r)
                                 (let ([c (- r d)])
                                      (and (>= c 0) (< c n) (var r c))))
                         (range 0 n))))
               (range (- 1 n) n))
          ;; Anti-diagonals (r + c = constant)
          ;; c = s - r where s = r + c ranges from 0 to 2*(n-1)
          (map (lambda (s)
                       (at-most-one
                        (filter-map
                         (lambda (r)
                                 (let ([c (- s r)])
                                      (and (>= c 0) (< c n) (var r c))))
                         (range 0 n))))
               (range 0 (- (* 2 n) 1))))))

(define (range lo hi)
  (if (>= lo hi) '() (cons lo (range (+ lo 1) hi))))

(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [else
         (let ([result (f (car lst))])
              (if result
                  (loop (cdr lst) (cons result acc))
                  (loop (cdr lst) acc)))])))

(doc 'section 'utility)

(define (sat-help)
  (doc 'export #t)
  (doc 'type '(-> Void))
  (doc 'description "Print SAT solver help")
  (display "SAT Solver - Boolean Satisfiability with CDCL\n\n")
  (display "Basic Usage:\n")
  (display "  (sat-solve '((1 2) (-1 3)))     ; Check satisfiability\n")
  (display "  (sat-satisfiable? clauses)      ; Returns #t or #f\n")
  (display "  (sat-model clauses)             ; Returns assignment or #f\n\n")
  (display "Building Clauses:\n")
  (display "  (var n)                         ; Positive literal xn\n")
  (display "  (neg n)                         ; Negative literal ~xn\n")
  (display "  (implies a b)                   ; a => b as clause\n")
  (display "  (iff a b)                       ; a <=> b as clauses\n\n")
  (display "Cardinality:\n")
  (display "  (at-most-one vars)              ; At most one true\n")
  (display "  (at-least-one vars)             ; At least one true\n")
  (display "  (exactly-one vars)              ; Exactly one true\n")
  (display "  (at-most-k vars k)              ; At most k true\n")
  (display "  (exactly-k vars k)              ; Exactly k true\n\n")
  (display "Problems:\n")
  (display "  (graph-coloring edges nodes colors)  ; Graph coloring\n")
  (display "  (n-queens-sat n)                     ; N-Queens puzzle\n"))
