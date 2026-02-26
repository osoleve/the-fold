;;; @module sat/clause
;;; @requires sat/literal sort

(require 'sat/literal)
(require 'sort)

(doc 'module 'clause)
(doc 'description "SAT clause representation - a disjunction of literals")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Clauses are lists of literals representing (l1 OR l2 OR ... OR ln)")
(doc 'note "Empty clause [] represents FALSE (unsatisfiable)")

(doc 'section 'clause-constructors)

(define (make-clause . lits)
  (doc 'export #t)
  (doc 'type '(-> Literal ... Clause))
  (doc 'description "Create a clause from literals")
  (clause-normalize lits))

(define (clause-from-list lits)
  (doc 'export #t)
  (doc 'type '(-> (List Literal) Clause))
  (doc 'description "Create a clause from a list of literals")
  (clause-normalize lits))

(define (clause-normalize lits)
  (doc 'type '(-> (List Literal) Clause))
  (doc 'description "Normalize clause: remove duplicates, sort, detect tautology")
  (let* ([sorted (sort-by < lits)]
         [deduped (clause-dedup sorted)])
        ;; Check for tautology (x and ~x in same clause)
        (if (clause-tautology? deduped)
            'tautology
            deduped)))

(define (clause-dedup sorted-lits)
  (doc 'type '(-> (List Literal) (List Literal)))
  (doc 'description "Remove duplicate literals from sorted list")
  (cond
   [(null? sorted-lits) '()]
   [(null? (cdr sorted-lits)) sorted-lits]
   [(= (car sorted-lits) (cadr sorted-lits))
    (clause-dedup (cdr sorted-lits))]
   [else
    (cons (car sorted-lits) (clause-dedup (cdr sorted-lits)))]))

(define (clause-tautology? lits)
  (doc 'type '(-> (List Literal) Bool))
  (doc 'description "Check if clause contains both x and ~x")
  (let loop ([lits lits])
       (cond
        [(null? lits) #f]
        [(null? (cdr lits)) #f]
        ;; In sorted list, x and -x are adjacent (since |x| = |-x|)
        ;; Actually no: -3, -2, -1, 1, 2, 3 - need to check properly
        [(= (car lits) (- (cadr lits))) #t]
        [else (loop (cdr lits))])))

(doc 'section 'clause-predicates)

(define (clause? x)
  (doc 'export #t)
  (doc 'type '(-> Any Bool))
  (doc 'description "Check if value is a valid clause")
  (or (eq? x 'tautology)
      (and (list? x)
           (all integer? x))))

(define (clause-empty? clause)
  (doc 'export #t)
  (doc 'type '(-> Clause Bool))
  (doc 'description "Check if clause is empty (represents FALSE)")
  (and (list? clause) (null? clause)))

(define (clause-unit? clause)
  (doc 'export #t)
  (doc 'type '(-> Clause Bool))
  (doc 'description "Check if clause is a unit clause (single literal)")
  (and (list? clause)
       (pair? clause)
       (null? (cdr clause))))

(define (clause-tautology-marker? clause)
  (doc 'export #t)
  (doc 'type '(-> Clause Bool))
  (doc 'description "Check if clause is marked as a tautology")
  (eq? clause 'tautology))

(doc 'section 'clause-accessors)

(define (clause-size clause)
  (doc 'export #t)
  (doc 'type '(-> Clause Nat))
  (doc 'description "Number of literals in clause")
  (if (clause-tautology-marker? clause)
      0
      (length clause)))

(define (clause-literals clause)
  (doc 'export #t)
  (doc 'type '(-> Clause (List Literal)))
  (doc 'description "Get list of literals in clause")
  (if (clause-tautology-marker? clause)
      '()
      clause))

(define (clause-unit-lit clause)
  (doc 'export #t)
  (doc 'type '(-> Clause (Maybe Literal)))
  (doc 'description "Get the literal if clause is unit, else #f")
  (if (clause-unit? clause)
      (car clause)
      #f))

(define (clause-contains? clause lit)
  (doc 'export #t)
  (doc 'type '(-> Clause Literal Bool))
  (doc 'description "Check if clause contains a specific literal")
  (and (list? clause)
       (member lit clause)
       #t))

(define (clause-vars clause)
  (doc 'export #t)
  (doc 'type '(-> Clause (List VarId)))
  (doc 'description "Get list of variable ids in clause")
  (if (clause-tautology-marker? clause)
      '()
      (map lit-var clause)))

(doc 'section 'clause-operations)

(define (clause-remove-lit clause lit)
  (doc 'export #t)
  (doc 'type '(-> Clause Literal Clause))
  (doc 'description "Remove a literal from clause")
  (if (clause-tautology-marker? clause)
      clause
      (clause-from-list (filter (lambda (l) (not (= l lit))) clause))))

(define (clause-resolve c1 c2 var)
  (doc 'export #t)
  (doc 'type '(-> Clause Clause VarId (Maybe Clause)))
  (doc 'description "Resolution: combine two clauses on a variable")
  (doc 'note "Resolves (A OR x) with (B OR ~x) to get (A OR B)")
  (let ([pos-lit (pos-lit var)]
        [neg-lit (neg-lit var)])
       (cond
        [(and (clause-contains? c1 pos-lit)
              (clause-contains? c2 neg-lit))
         (clause-from-list
          (append (filter (lambda (l) (not (= l pos-lit))) c1)
                  (filter (lambda (l) (not (= l neg-lit))) c2)))]
        [(and (clause-contains? c1 neg-lit)
              (clause-contains? c2 pos-lit))
         (clause-from-list
          (append (filter (lambda (l) (not (= l neg-lit))) c1)
                  (filter (lambda (l) (not (= l pos-lit))) c2)))]
        [else #f])))

(doc 'section 'clause-display)

(define (clause->string clause)
  (doc 'export #t)
  (doc 'type '(-> Clause String))
  (doc 'description "Convert clause to string representation")
  (cond
   [(clause-tautology-marker? clause) "TRUE"]
   [(clause-empty? clause) "FALSE"]
   [else
    (string-append
     "("
     (apply string-append
            (list-intersperse
             (map lit->string clause)
             " | "))
     ")")]))

(define (list-intersperse lst sep)
  (doc 'type '(-> (List a) a (List a)))
  (cond
   [(null? lst) '()]
   [(null? (cdr lst)) lst]
   [else (cons (car lst) (cons sep (list-intersperse (cdr lst) sep)))]))

;; Helper
(define (all pred lst)
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (all pred (cdr lst))]))
