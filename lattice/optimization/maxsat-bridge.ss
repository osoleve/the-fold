(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))

;;; @module maxsat-bridge
;;; @requires maxsat sort

(require 'maxsat)
(require 'sort)

(doc 'module 'maxsat-bridge)
(doc 'description "Weighted constraint optimization via MaxSAT.
Bridges the SAT/MaxSAT solver to the optimization subsystem by providing:
  1. Named variable domains with automatic literal allocation
  2. Constraint compilation from named variables to SAT clauses
  3. Weighted soft constraints with stratified solving
  4. Solution extraction back to named variable assignments

The stratified solver handles weights by solving tier-by-tier from highest
weight to lowest. Satisfied clauses from higher tiers become hard constraints
for lower tiers, giving lexicographic optimality over weight tiers.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Variable Domain
;;; ============================================================

(doc 'section 'variable-domain)
(doc 'note "A variable domain maps symbolic names to SAT literal numbers.
Variables are allocated sequentially starting from 1.")

(define (make-wco-domain)
  (doc 'export #t)
  (doc 'type '(-> WCODomain))
  (doc 'description "Create an empty variable domain")
  (list 'wco-domain '() 1))

(define (wco-domain? x)
  (doc 'type '(-> Any Bool))
  (and (pair? x) (eq? (car x) 'wco-domain)))

(define (wco-domain-bindings d) (cadr d))
(define (wco-domain-next d) (caddr d))

(define (wco-declare domain name)
  (doc 'export #t)
  (doc 'type '(-> WCODomain Symbol WCODomain))
  (doc 'description "Declare a named boolean variable, allocating a fresh literal")
  (let ([bindings (wco-domain-bindings domain)]
        [next (wco-domain-next domain)])
    (if (assq name bindings)
        (error 'wco-declare "variable already declared" name)
        (list 'wco-domain
              (cons (cons name next) bindings)
              (+ next 1)))))

(define (wco-declare* domain names)
  (doc 'export #t)
  (doc 'type '(-> WCODomain (List Symbol) WCODomain))
  (doc 'description "Declare multiple named boolean variables at once")
  (fold-left wco-declare domain names))

(define (wco-var domain name)
  (doc 'export #t)
  (doc 'type '(-> WCODomain Symbol Literal))
  (doc 'description "Get the positive literal for a named variable")
  (let ([entry (assq name (wco-domain-bindings domain))])
    (if entry
        (pos-lit (cdr entry))
        (error 'wco-var "unknown variable" name))))

(define (wco-neg-var domain name)
  (doc 'export #t)
  (doc 'type '(-> WCODomain Symbol Literal))
  (doc 'description "Get the negated literal for a named variable")
  (lit-negate (wco-var domain name)))

(define (wco-var-count domain)
  (doc 'export #t)
  (doc 'type '(-> WCODomain Nat))
  (doc 'description "Number of declared variables")
  (length (wco-domain-bindings domain)))

;;; ============================================================
;;; Constraint Compilation
;;; ============================================================

(doc 'section 'constraint-compilation)
(doc 'note "Named-variable wrappers around sat.ss constraint helpers.
These return clause lists suitable for wco-require or wco-prefer.")

(define (wco-implies domain a b)
  (doc 'export #t)
  (doc 'type '(-> WCODomain Symbol Symbol (List Literal)))
  (doc 'description "a => b as a clause: (~a OR b)")
  (implies (wco-var domain a) (wco-var domain b)))

(define (wco-iff domain a b)
  (doc 'export #t)
  (doc 'type '(-> WCODomain Symbol Symbol (List (List Literal))))
  (doc 'description "a <=> b as two clauses")
  (iff (wco-var domain a) (wco-var domain b)))

(define (wco-mutex domain names)
  (doc 'export #t)
  (doc 'type '(-> WCODomain (List Symbol) (List (List Literal))))
  (doc 'description "At most one of the named variables is true")
  (at-most-one (map (lambda (n) (wco-var domain n)) names)))

(define (wco-exactly-one domain names)
  (doc 'export #t)
  (doc 'type '(-> WCODomain (List Symbol) (List (List Literal))))
  (doc 'description "Exactly one of the named variables is true")
  (exactly-one (map (lambda (n) (wco-var domain n)) names)))

(define (wco-at-most-k domain names k)
  (doc 'export #t)
  (doc 'type '(-> WCODomain (List Symbol) Nat (List (List Literal))))
  (doc 'description "At most k of the named variables are true")
  (at-most-k (map (lambda (n) (wco-var domain n)) names) k))

(define (wco-at-least-k domain names k)
  (doc 'export #t)
  (doc 'type '(-> WCODomain (List Symbol) Nat (List (List Literal))))
  (doc 'description "At least k of the named variables are true")
  (at-least-k (map (lambda (n) (wco-var domain n)) names) k))

;;; ============================================================
;;; Problem Type
;;; ============================================================

(doc 'section 'problem-type)
(doc 'note "A WCO problem consists of:
  - domain: named variable mappings
  - hard: clauses that MUST be satisfied
  - softs: weighted clauses we WANT to satisfy, as ((weight . clause) ...)")

(define (make-wco-problem domain)
  (doc 'export #t)
  (doc 'type '(-> WCODomain WCOProblem))
  (doc 'description "Create an empty weighted constraint optimization problem")
  (list 'wco-problem domain '() '()))

(define (wco-problem? x)
  (doc 'type '(-> Any Bool))
  (and (pair? x) (eq? (car x) 'wco-problem)))

(define (wco-problem-domain p) (list-ref p 1))
(define (wco-problem-hard p) (list-ref p 2))
(define (wco-problem-softs p) (list-ref p 3))

(define (wco-require problem . clause-groups)
  (doc 'export #t)
  (doc 'type '(-> WCOProblem (List Clause) ... WCOProblem))
  (doc 'description "Add hard constraints. Each argument is a list of clauses.")
  (list 'wco-problem
        (wco-problem-domain problem)
        (append (wco-problem-hard problem) (apply append clause-groups))
        (wco-problem-softs problem)))

(define (wco-prefer problem weight . clause-groups)
  (doc 'export #t)
  (doc 'type '(-> WCOProblem Number (List Clause) ... WCOProblem))
  (doc 'description "Add soft constraints at the given weight.
Higher weight = more important to satisfy. Each argument after weight
is a list of clauses, all assigned the same weight.")
  (let ([new-clauses (apply append clause-groups)])
    (list 'wco-problem
          (wco-problem-domain problem)
          (wco-problem-hard problem)
          (append (wco-problem-softs problem)
                  (map (lambda (c) (cons weight c)) new-clauses)))))

(define (wco-prefer-true problem weight name)
  (doc 'export #t)
  (doc 'type '(-> WCOProblem Number Symbol WCOProblem))
  (doc 'description "Soft preference: named variable should be true")
  (let ([domain (wco-problem-domain problem)])
    (wco-prefer problem weight
                (list (list (wco-var domain name))))))

(define (wco-prefer-false problem weight name)
  (doc 'export #t)
  (doc 'type '(-> WCOProblem Number Symbol WCOProblem))
  (doc 'description "Soft preference: named variable should be false")
  (let ([domain (wco-problem-domain problem)])
    (wco-prefer problem weight
                (list (list (wco-neg-var domain name))))))

(define (wco-num-hard problem)
  (doc 'type '(-> WCOProblem Nat))
  (length (wco-problem-hard problem)))

(define (wco-num-soft problem)
  (doc 'type '(-> WCOProblem Nat))
  (length (wco-problem-softs problem)))

;;; ============================================================
;;; Conversion
;;; ============================================================

(doc 'section 'conversion)

(define (wco->maxsat problem)
  (doc 'export #t)
  (doc 'type '(-> WCOProblem MaxSATInstance))
  (doc 'description "Convert WCO to unweighted MaxSAT (weights are discarded).
Useful when all constraints have equal priority.")
  (make-maxsat (wco-problem-hard problem)
               (map cdr (wco-problem-softs problem))))

;;; ============================================================
;;; Result Type
;;; ============================================================

(doc 'section 'result-type)

(define (make-wco-result cost model domain num-soft)
  (list 'wco-result cost model domain num-soft))

(define (wco-result? x)
  (doc 'export #t)
  (doc 'type '(-> Any Bool))
  (and (pair? x) (eq? (car x) 'wco-result)))

(define (wco-result-cost r)
  (doc 'export #t)
  (doc 'type '(-> WCOResult Number))
  (doc 'description "Total weighted cost of unsatisfied soft constraints")
  (list-ref r 1))

(define (wco-result-model r)
  (doc 'export #t)
  (doc 'type '(-> WCOResult (List (Pair VarId Bool))))
  (doc 'description "Raw SAT model: list of (variable-number . boolean) pairs")
  (list-ref r 2))

(define (wco-result-assignment r)
  (doc 'export #t)
  (doc 'type '(-> WCOResult (List (Pair Symbol Bool))))
  (doc 'description "Named variable assignments: list of (name . boolean) pairs")
  (let ([model (list-ref r 2)]
        [bindings (wco-domain-bindings (list-ref r 3))])
    (map (lambda (binding)
           (let* ([name (car binding)]
                  [var-num (cdr binding)]
                  [entry (assoc var-num model)])
             (cons name (if entry (cdr entry) #f))))
         bindings)))

(define (wco-result-value r name)
  (doc 'export #t)
  (doc 'type '(-> WCOResult Symbol Bool))
  (doc 'description "Get the boolean value of a named variable in the solution")
  (let* ([domain (list-ref r 3)]
         [entry (assq name (wco-domain-bindings domain))])
    (if (not entry)
        (error 'wco-result-value "unknown variable" name)
        (let ([model-entry (assoc (cdr entry) (list-ref r 2))])
          (if model-entry (cdr model-entry) #f)))))

(define (wco-result-num-soft r)
  (doc 'type '(-> WCOResult Nat))
  (list-ref r 4))

;;; ============================================================
;;; Stratified Solver
;;; ============================================================

(doc 'section 'solver)
(doc 'note "Stratified weighted MaxSAT: solve weight tiers from highest to lowest.
At each tier, satisfied clauses from higher tiers are promoted to hard
constraints, ensuring lexicographic optimality. Within a tier, the existing
MaxSAT binary search minimizes the number of unsatisfied clauses.")

;;; Group weighted soft clauses by weight tier (descending)
(define (group-by-weight weighted-softs)
  (doc 'type '(-> (List (Pair Number Clause)) (List (List Number Clause ...))))
  (doc 'description "Group by weight, sorted descending. Returns ((weight clause ...) ...)")
  (if (null? weighted-softs)
      '()
      (let ([sorted-ws (sort-by (lambda (a b) (> (car a) (car b)))
                                weighted-softs)])
        (let loop ([rest sorted-ws] [groups '()])
          (if (null? rest)
              (reverse groups)
              (let ([w (caar rest)]
                    [c (cdar rest)])
                (if (and (pair? groups) (= w (caar groups)))
                    ;; Same weight tier — add clause to current group
                    (loop (cdr rest)
                          (cons (cons w (cons c (cdar groups)))
                                (cdr groups)))
                    ;; New weight tier
                    (loop (cdr rest)
                          (cons (list w c) groups)))))))))

;;; Solve one tier: minimize unsatisfied clauses given hard constraints
(define (solve-tier hard-clauses tier-clauses)
  (doc 'type '(-> (List Clause) (List Clause) (Or (List Nat Model (List Clause)) #f)))
  (doc 'description "Returns (cost model satisfied-clauses) or #f if hard unsat")
  (let ([result (maxsat-solve (make-maxsat hard-clauses tier-clauses))])
    (if result
        (let* ([cost (maxsat-result-cost result)]
               [model (maxsat-result-model result)]
               [satisfied (filter
                           (lambda (clause)
                             (clause-satisfied? clause model))
                           tier-clauses)])
          (list cost model satisfied))
        #f)))

(define (wco-solve problem)
  (doc 'export #t)
  (doc 'type '(-> WCOProblem (Or WCOResult #f)))
  (doc 'description "Solve weighted constraint optimization via stratified MaxSAT.
Returns wco-result with minimum weighted cost, or #f if hard constraints
are unsatisfiable. Solves weight tiers from highest to lowest, promoting
satisfied clauses as hard constraints for lower tiers.")
  (let ([hard (wco-problem-hard problem)]
        [weighted-softs (wco-problem-softs problem)]
        [domain (wco-problem-domain problem)])
    (if (null? weighted-softs)
        ;; No soft constraints — just check hard satisfiability
        (let ([model (sat-model hard)])
          (cond
           [(not model) #f]
           [(eq? model 'timeout) #f]
           [else (make-wco-result 0 model domain 0)]))
        ;; Stratified solve by weight tiers
        (let ([tiers (group-by-weight weighted-softs)])
          (let loop ([remaining tiers]
                     [current-hard hard]
                     [total-cost 0]
                     [last-model #f])
            (if (null? remaining)
                (if last-model
                    (make-wco-result total-cost last-model domain
                                    (length weighted-softs))
                    #f)
                (let* ([tier (car remaining)]
                       [weight (car tier)]
                       [clauses (cdr tier)]
                       [tier-result (solve-tier current-hard clauses)])
                  (if (not tier-result)
                      #f  ; Hard constraints unsatisfiable
                      (let ([tier-cost (car tier-result)]
                            [model (cadr tier-result)]
                            [satisfied (caddr tier-result)])
                        (loop (cdr remaining)
                              (append current-hard satisfied)
                              (+ total-cost (* weight tier-cost))
                              model))))))))))

;;; ============================================================
;;; Applications
;;; ============================================================

(doc 'section 'applications)

(define (weighted-vertex-cover edges node-weights)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Nat Nat)) (List (Pair Symbol Number)) WCOProblem))
  (doc 'description "Weighted minimum vertex cover.
edges: list of (u . v) pairs where u,v are node symbols
node-weights: list of (node-symbol . cost) pairs
Higher cost = more expensive to include in cover.
Result minimizes total cost of covering nodes.")
  (let* ([node-names (map car node-weights)]
         [domain (wco-declare* (make-wco-domain) node-names)]
         ;; Hard: each edge has at least one endpoint covered
         [hard-clauses (map (lambda (edge)
                              (list (wco-var domain (car edge))
                                    (wco-var domain (cdr edge))))
                            edges)]
         [problem (wco-require (make-wco-problem domain) hard-clauses)])
    ;; Soft: prefer NOT including each node, weighted by cost
    (fold-left
     (lambda (p nw)
       (wco-prefer p (cdr nw)
                   (list (list (wco-neg-var domain (car nw))))))
     problem
     node-weights)))

(define (weighted-diagnosis weighted-clauses)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Number (List Literal))) WCOProblem))
  (doc 'description "Find minimum-cost set of clauses to remove for consistency.
weighted-clauses: list of (weight . clause) pairs.
Higher weight = more important to keep (costlier to remove).
Returns WCO problem; solve and check cost for repair cost.")
  (let ([domain (make-wco-domain)]
        [problem (make-wco-problem (make-wco-domain))])
    (fold-left
     (lambda (p wc)
       (wco-prefer p (car wc) (list (cdr wc))))
     problem
     weighted-clauses)))

(define (scheduling-problem tasks slots conflicts preferences)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) (List Symbol) (List (Pair Symbol Symbol)) (List (List Symbol Symbol Number)) WCOProblem))
  (doc 'description "Task scheduling as weighted constraint optimization.
tasks: list of task symbols
slots: list of slot symbols
conflicts: list of (task1 . task2) pairs that cannot share a slot
preferences: list of (task slot weight) triples for preferred assignments
Variables are named task:slot (e.g., 'a:morning).")
  (let* (;; Create variables: one per task-slot pair
         [var-names (append-map
                     (lambda (task)
                       (map (lambda (slot)
                              (string->symbol
                               (string-append (symbol->string task) ":"
                                              (symbol->string slot))))
                            slots))
                     tasks)]
         [domain (wco-declare* (make-wco-domain) var-names)]
         [task-slot (lambda (t s)
                      (string->symbol
                       (string-append (symbol->string t) ":"
                                      (symbol->string s))))]
         ;; Hard: each task assigned exactly one slot
         [assignment-clauses
          (append-map
           (lambda (task)
             (exactly-one
              (map (lambda (slot)
                     (wco-var domain (task-slot task slot)))
                   slots)))
           tasks)]
         ;; Hard: conflicting tasks get different slots
         [conflict-clauses
          (append-map
           (lambda (conflict)
             (let ([t1 (car conflict)]
                   [t2 (cdr conflict)])
               (append-map
                (lambda (slot)
                  ;; NOT (t1:slot AND t2:slot)
                  (list (list (wco-neg-var domain (task-slot t1 slot))
                              (wco-neg-var domain (task-slot t2 slot)))))
                slots)))
           conflicts)]
         [problem (wco-require (make-wco-problem domain)
                               assignment-clauses
                               conflict-clauses)])
    ;; Soft: preferences for task-slot assignments
    (fold-left
     (lambda (p pref)
       (let ([task (car pref)]
             [slot (cadr pref)]
             [weight (caddr pref)])
         (wco-prefer-true p weight (task-slot task slot))))
     problem
     preferences)))
