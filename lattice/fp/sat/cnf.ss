(load "lattice/fp/sat/clause.ss")

(doc 'module 'cnf)
(doc 'description "CNF formula representation - conjunction of clauses")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "CNF = Conjunctive Normal Form: (C1 AND C2 AND ... AND Cn)")
(doc 'note "Each clause Ci is a disjunction of literals")

(doc 'section 'cnf-constructors)

(define (make-cnf clauses)
  (doc 'export #t)
  (doc 'type '(-> (List Clause) CNF))
  (doc 'description "Create CNF from list of clauses")
  (doc 'note "Filters out tautological clauses")
  (list 'cnf
        (filter (lambda (c) (not (clause-tautology-marker? c))) clauses)))

(define (cnf-from-lists clause-lists)
  (doc 'export #t)
  (doc 'type '(-> (List (List Literal)) CNF))
  (doc 'description "Create CNF from lists of literal lists")
  (make-cnf (map clause-from-list clause-lists)))

(doc cnf-empty 'export #t)
(doc cnf-empty 'type 'CNF)
(doc cnf-empty 'description "Empty CNF (trivially satisfiable)")
(define cnf-empty (make-cnf '()))

(doc 'section 'cnf-predicates)

(define (cnf? x)
  (doc 'export #t)
  (doc 'type '(-> Any Bool))
  (doc 'description "Check if value is a CNF formula")
  (and (pair? x)
       (eq? (car x) 'cnf)
       (list? (cadr x))))

(define (cnf-trivially-unsat? cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF Bool))
  (doc 'description "Check if CNF contains empty clause (immediately unsatisfiable)")
  (any clause-empty? (cnf-clauses cnf)))

(define (cnf-trivially-sat? cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF Bool))
  (doc 'description "Check if CNF has no clauses (trivially satisfiable)")
  (null? (cnf-clauses cnf)))

(doc 'section 'cnf-accessors)

(define (cnf-clauses cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF (List Clause)))
  (doc 'description "Get list of clauses in CNF")
  (cadr cnf))

(define (cnf-num-clauses cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF Nat))
  (doc 'description "Number of clauses in CNF")
  (length (cnf-clauses cnf)))

(define (cnf-num-vars cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF Nat))
  (doc 'description "Number of distinct variables in CNF")
  (length (cnf-vars cnf)))

(define (cnf-vars cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF (List VarId)))
  (doc 'description "Get sorted list of all variable ids in CNF")
  (list-sort < (list-unique
                (apply append
                       (map clause-vars (cnf-clauses cnf))))))

(define (cnf-unit-clauses cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF (List Clause)))
  (doc 'description "Get all unit clauses in CNF")
  (filter clause-unit? (cnf-clauses cnf)))

(doc 'section 'cnf-operations)

(define (cnf-add-clause cnf clause)
  (doc 'export #t)
  (doc 'type '(-> CNF Clause CNF))
  (doc 'description "Add a clause to CNF")
  (if (clause-tautology-marker? clause)
      cnf
      (make-cnf (cons clause (cnf-clauses cnf)))))

(define (cnf-add-clauses cnf clauses)
  (doc 'export #t)
  (doc 'type '(-> CNF (List Clause) CNF))
  (doc 'description "Add multiple clauses to CNF")
  (fold-left cnf-add-clause cnf clauses))

(define (cnf-simplify cnf assignment)
  (doc 'export #t)
  (doc 'type '(-> CNF Assignment CNF))
  (doc 'description "Simplify CNF under partial assignment")
  (doc 'note "Removes satisfied clauses, shrinks unsatisfied ones")
  (make-cnf
   (filter-map
    (lambda (clause)
            (clause-simplify clause assignment))
    (cnf-clauses cnf))))

(define (clause-simplify clause assignment)
  (doc 'type '(-> Clause Assignment (Maybe Clause)))
  (doc 'description "Simplify clause under assignment. Returns #f if satisfied.")
  (let loop ([lits (clause-literals clause)]
             [remaining '()])
       (cond
        [(null? lits)
         ;; No literals satisfied - return simplified clause
         (if (null? remaining)
             '()  ; Empty clause - conflict
             (reverse remaining))]
        [else
         (let* ([lit (car lits)]
                [var (lit-var lit)]
                [val (assignment-get assignment var)])
               (cond
                ;; Variable unassigned - keep literal
                [(not val)
                 (loop (cdr lits) (cons lit remaining))]
                ;; Literal is true - clause satisfied
                [(and (eq? val 'true) (lit-positive? lit))
                 #f]
                [(and (eq? val 'false) (lit-negative? lit))
                 #f]
                ;; Literal is false - remove it
                [else
                 (loop (cdr lits) remaining)]))])))

(doc 'section 'cnf-display)

(define (cnf->string cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF String))
  (doc 'description "Convert CNF to string representation")
  (let ([clauses (cnf-clauses cnf)])
       (if (null? clauses)
           "TRUE"
           (apply string-append
                  (list-intersperse
                   (map clause->string clauses)
                   " & ")))))

(define (cnf->dimacs cnf)
  (doc 'export #t)
  (doc 'type '(-> CNF String))
  (doc 'description "Convert CNF to DIMACS format")
  (let* ([clauses (cnf-clauses cnf)]
         [nvars (cnf-num-vars cnf)]
         [nclauses (length clauses)]
         [header (string-append "p cnf "
                               (number->string nvars)
                               " "
                               (number->string nclauses)
                               "\n")]
         [clause-strs (map clause->dimacs clauses)])
        (apply string-append (cons header clause-strs))))

(define (clause->dimacs clause)
  (doc 'type '(-> Clause String))
  (doc 'description "Convert clause to DIMACS line")
  (string-append
   (apply string-append
          (list-intersperse
           (map number->string clause)
           " "))
   " 0\n"))

(doc 'section 'cnf-from-dimacs)

(define (dimacs-parse lines)
  (doc 'export #t)
  (doc 'type '(-> (List String) CNF))
  (doc 'description "Parse DIMACS format into CNF")
  (let loop ([lines lines]
             [clauses '()])
       (cond
        [(null? lines) (make-cnf (reverse clauses))]
        [else
         (let ([line (string-trim (car lines))])
              (cond
               ;; Comment
               [(or (string=? line "")
                    (char=? (string-ref line 0) #\c))
                (loop (cdr lines) clauses)]
               ;; Problem line
               [(char=? (string-ref line 0) #\p)
                (loop (cdr lines) clauses)]
               ;; Clause line
               [else
                (let ([clause (dimacs-parse-clause line)])
                     (loop (cdr lines)
                           (cons clause clauses)))]))])))

(define (dimacs-parse-clause line)
  (doc 'type '(-> String Clause))
  (doc 'description "Parse a DIMACS clause line")
  (let* ([nums (filter (lambda (n) (not (= n 0)))
                       (map string->number
                            (string-split line #\space)))])
        (clause-from-list nums)))

(define (string-trim s)
  (doc 'type '(-> String String))
  (list->string
   (let loop ([chars (string->list s)])
        (cond
         [(null? chars) '()]
         [(char-whitespace? (car chars))
          (loop (cdr chars))]
         [else (reverse (let loop2 ([chars (reverse chars)])
                             (cond
                              [(null? chars) '()]
                              [(char-whitespace? (car chars))
                               (loop2 (cdr chars))]
                              [else chars])))]))))

(define (string-split str delim)
  (doc 'type '(-> String Char (List String)))
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) delim)
         (if (null? current)
             (loop (cdr chars) '() result)
             (loop (cdr chars) '()
                   (cons (list->string (reverse current)) result)))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))

;; Helpers
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; filter-map provided by prelude

(define (list-unique lst)
  (let loop ([lst lst] [seen '()] [acc '()])
       (cond
        [(null? lst) (reverse acc)]
        [(member (car lst) seen) (loop (cdr lst) seen acc)]
        [else (loop (cdr lst)
                    (cons (car lst) seen)
                    (cons (car lst) acc))])))

;; Assignment stub for clause-simplify - will be replaced when assignment.ss loads
(define (assignment-get assignment var)
  (doc 'type '(-> Assignment VarId (Maybe (Or 'true 'false))))
  (let ([entry (assv var assignment)])
       (and entry (cdr entry))))
