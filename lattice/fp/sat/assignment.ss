(load "lattice/fp/sat/literal.ss")

(doc 'module 'assignment)
(doc 'description "SAT solver assignment tracking - partial variable assignments with decision levels")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Tracks which variables are assigned, their values, decision levels, and antecedent clauses")

(doc 'section 'assignment-structure)
(doc 'note "Assignment = (assignment trail levels reasons)")
(doc 'note "  trail: list of (var . val) in assignment order (most recent first)")
(doc 'note "  levels: vector mapping var -> decision level (0 = unassigned)")
(doc 'note "  reasons: vector mapping var -> antecedent clause (or 'decision)")

(define (make-assignment nvars)
  (doc 'export #t)
  (doc 'type '(-> Nat Assignment))
  (doc 'description "Create empty assignment for nvars variables")
  (list 'assignment
        '()                              ; trail
        (make-vector (+ nvars 1) 0)      ; levels (0 = unassigned)
        (make-vector (+ nvars 1) #f)))   ; reasons

(define (assignment-copy a)
  (doc 'export #t)
  (doc 'type '(-> Assignment Assignment))
  (doc 'description "Create a deep copy of an assignment")
  (list 'assignment
        (assignment-trail a)
        (vector-copy (assignment-levels-vec a))
        (vector-copy (assignment-reasons-vec a))))

(doc 'section 'assignment-accessors)

(define (assignment-trail a)
  (doc 'export #t)
  (doc 'type '(-> Assignment (List (Pair VarId Bool))))
  (doc 'description "Get assignment trail (most recent first)")
  (cadr a))

(define (assignment-levels-vec a)
  (doc 'type '(-> Assignment (Vector Nat)))
  (caddr a))

(define (assignment-reasons-vec a)
  (doc 'type '(-> Assignment (Vector (Or Clause 'decision #f))))
  (cadddr a))

(define (assignment-get a var)
  (doc 'export #t)
  (doc 'type '(-> Assignment VarId (Maybe (Or 'true 'false))))
  (doc 'description "Get assigned value for variable, or #f if unassigned")
  (let loop ([trail (assignment-trail a)])
       (cond
        [(null? trail) #f]
        [(= (caar trail) var) (cdar trail)]
        [else (loop (cdr trail))])))

(define (assignment-level a var)
  (doc 'export #t)
  (doc 'type '(-> Assignment VarId Nat))
  (doc 'description "Get decision level of variable (0 if unassigned)")
  (vector-ref (assignment-levels-vec a) var))

(define (assignment-reason a var)
  (doc 'export #t)
  (doc 'type '(-> Assignment VarId (Or Clause 'decision #f)))
  (doc 'description "Get antecedent clause for variable, 'decision if branched, #f if unassigned")
  (vector-ref (assignment-reasons-vec a) var))

(define (assignment-decision-level a)
  (doc 'export #t)
  (doc 'type '(-> Assignment Nat))
  (doc 'description "Get current decision level")
  (let loop ([trail (assignment-trail a)] [max-level 0])
       (if (null? trail)
           max-level
           (let ([var (caar trail)])
                (loop (cdr trail)
                      (max max-level (assignment-level a var)))))))

(define (assignment-assigned? a var)
  (doc 'export #t)
  (doc 'type '(-> Assignment VarId Bool))
  (doc 'description "Check if variable is assigned")
  (not (not (assignment-get a var))))

(define (assignment-size a)
  (doc 'export #t)
  (doc 'type '(-> Assignment Nat))
  (doc 'description "Number of assigned variables")
  (length (assignment-trail a)))

(doc 'section 'assignment-operations)

(define (assignment-assign a var val level reason)
  (doc 'export #t)
  (doc 'type '(-> Assignment VarId (Or 'true 'false) Nat (Or Clause 'decision) Assignment))
  (doc 'description "Add assignment to trail")
  (let ([levels (assignment-levels-vec a)]
        [reasons (assignment-reasons-vec a)]
        [trail (assignment-trail a)])
       ;; Mutate vectors in place for efficiency
       (vector-set! levels var level)
       (vector-set! reasons var reason)
       (list 'assignment
             (cons (cons var val) trail)
             levels
             reasons)))

(define (assignment-decide a var val level)
  (doc 'export #t)
  (doc 'type '(-> Assignment VarId (Or 'true 'false) Nat Assignment))
  (doc 'description "Make a decision assignment (branch)")
  (assignment-assign a var val level 'decision))

(define (assignment-propagate a var val level clause)
  (doc 'export #t)
  (doc 'type '(-> Assignment VarId (Or 'true 'false) Nat Clause Assignment))
  (doc 'description "Make a propagated assignment (unit propagation)")
  (assignment-assign a var val level clause))

(define (assignment-backtrack-to a level)
  (doc 'export #t)
  (doc 'type '(-> Assignment Nat Assignment))
  (doc 'description "Backtrack to given decision level, undoing later assignments")
  (let ([levels (assignment-levels-vec a)]
        [reasons (assignment-reasons-vec a)])
       (let loop ([trail (assignment-trail a)]
                  [new-trail '()])
            (cond
             [(null? trail)
              ;; Reverse to maintain trail order (most recent first)
              (list 'assignment (reverse new-trail) levels reasons)]
             [else
              (let* ([entry (car trail)]
                     [var (car entry)]
                     [var-level (vector-ref levels var)])
                    (if (> var-level level)
                        (begin
                          ;; Unassign this variable
                          (vector-set! levels var 0)
                          (vector-set! reasons var #f)
                          (loop (cdr trail) new-trail))
                        ;; Keep this assignment
                        (loop (cdr trail) (cons entry new-trail))))]))))

(doc 'section 'assignment-literal-evaluation)

(define (assignment-eval-lit a lit)
  (doc 'export #t)
  (doc 'type '(-> Assignment Literal (Or 'true 'false 'unknown)))
  (doc 'description "Evaluate a literal under assignment")
  (let* ([var (lit-var lit)]
         [val (assignment-get a var)])
        (cond
         [(not val) 'unknown]
         [(and (eq? val 'true) (lit-positive? lit)) 'true]
         [(and (eq? val 'false) (lit-negative? lit)) 'true]
         [else 'false])))

(define (assignment-satisfies-lit? a lit)
  (doc 'export #t)
  (doc 'type '(-> Assignment Literal Bool))
  (doc 'description "Check if assignment satisfies a literal")
  (eq? (assignment-eval-lit a lit) 'true))

(define (assignment-falsifies-lit? a lit)
  (doc 'export #t)
  (doc 'type '(-> Assignment Literal Bool))
  (doc 'description "Check if assignment falsifies a literal")
  (eq? (assignment-eval-lit a lit) 'false))

(doc 'section 'assignment-clause-evaluation)

(define (assignment-eval-clause a clause)
  (doc 'export #t)
  (doc 'type '(-> Assignment Clause (Or 'satisfied 'conflict 'unit 'unresolved)))
  (doc 'description "Evaluate clause status under assignment")
  (let loop ([lits clause]
             [unknown-count 0]
             [last-unknown #f])
       (cond
        [(null? lits)
         (cond
          [(= unknown-count 0) 'conflict]   ; All false
          [(= unknown-count 1) 'unit]       ; One unknown
          [else 'unresolved])]              ; Multiple unknown
        [else
         (let ([eval (assignment-eval-lit a (car lits))])
              (cond
               [(eq? eval 'true) 'satisfied]
               [(eq? eval 'unknown)
                (loop (cdr lits) (+ unknown-count 1) (car lits))]
               [else
                (loop (cdr lits) unknown-count last-unknown)]))])))

(define (assignment-unit-lit a clause)
  (doc 'export #t)
  (doc 'type '(-> Assignment Clause (Maybe Literal)))
  (doc 'description "If clause is unit, return the unassigned literal")
  (let loop ([lits clause]
             [unknown #f])
       (cond
        [(null? lits) unknown]
        [else
         (let ([eval (assignment-eval-lit a (car lits))])
              (cond
               [(eq? eval 'true) #f]  ; Satisfied
               [(eq? eval 'unknown)
                (if unknown
                    #f  ; Multiple unknowns
                    (loop (cdr lits) (car lits)))]
               [else
                (loop (cdr lits) unknown)]))])))

(doc 'section 'assignment-display)

(define (assignment->string a)
  (doc 'export #t)
  (doc 'type '(-> Assignment String))
  (doc 'description "Convert assignment to string representation")
  (let ([trail (reverse (assignment-trail a))])
       (if (null? trail)
           "{}"
           (string-append
            "{"
            (apply string-append
                   (list-intersperse
                    (map (lambda (entry)
                                 (string-append
                                  "x" (number->string (car entry))
                                  "=" (if (eq? (cdr entry) 'true) "T" "F")))
                         trail)
                    ", "))
            "}"))))

(define (list-intersperse lst sep)
  (cond
   [(null? lst) '()]
   [(null? (cdr lst)) lst]
   [else (cons (car lst) (cons sep (list-intersperse (cdr lst) sep)))]))
