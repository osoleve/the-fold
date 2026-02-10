;;; @module watches
;;; @requires literal

(require 'literal)

(doc 'module 'watches)
(doc 'description "Two-Watched Literals (2WL) for efficient unit propagation")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Each clause watches 2 literals. When a literal becomes false, only clauses watching it are visited.")
(doc 'note "This reduces unit propagation from O(clauses × literals) to O(affected clauses)")

(doc 'section 'watch-structure)
(doc 'note "Watches = vector indexed by literal-index, containing list of (clause . other-watched-lit)")
(doc 'note "Literal index: positive lit n -> 2n, negative lit n -> 2n+1")

;;; Convert literal to watch index
(define (lit->watch-index lit)
  (doc 'type '(-> Literal Nat))
  (if (lit-positive? lit)
      (* 2 (lit-var lit))
      (+ (* 2 (lit-var lit)) 1)))

;;; Convert watch index back to literal
(define (watch-index->lit idx)
  (doc 'type '(-> Nat Literal))
  (let ([var (quotient idx 2)])
    (if (even? idx)
        (pos-lit var)
        (neg-lit var))))

;;; Create empty watch structure for nvars variables
(define (make-watches nvars)
  (doc 'export #t)
  (doc 'type '(-> Nat Watches))
  (doc 'description "Create empty watch lists for nvars variables")
  ;; Index 0,1 unused (var 0 doesn't exist), then pairs for each var
  (make-vector (* 2 (+ nvars 1)) '()))

;;; Get watch list for a literal
(define (watches-get watches lit)
  (doc 'export #t)
  (doc 'type '(-> Watches Literal (List WatchEntry)))
  (vector-ref watches (lit->watch-index lit)))

;;; Set watch list for a literal
(define (watches-set! watches lit entries)
  (doc 'type '(-> Watches Literal (List WatchEntry) Void))
  (vector-set! watches (lit->watch-index lit) entries))

;;; Add a watch entry for a literal
(define (watches-add! watches lit clause other-lit)
  (doc 'export #t)
  (doc 'type '(-> Watches Literal Clause Literal Void))
  (doc 'description "Add clause to watch list of lit, with other-lit as the other watched literal")
  (let ([idx (lit->watch-index lit)])
    (vector-set! watches idx
                 (cons (cons clause other-lit)
                       (vector-ref watches idx)))))

(doc 'section 'watch-initialization)

;;; Initialize watches for a clause
(define (watches-init-clause! watches clause)
  (doc 'export #t)
  (doc 'type '(-> Watches Clause Void))
  (doc 'description "Initialize watches for a clause (pick first two literals)")
  (cond
   [(null? clause) (void)]  ; Empty clause - nothing to watch
   [(null? (cdr clause))
    ;; Unit clause: watch the single literal (other-lit is itself)
    (watches-add! watches (car clause) clause (car clause))]
   [else
    ;; Watch first two literals
    (let ([lit1 (car clause)]
          [lit2 (cadr clause)])
      (watches-add! watches lit1 clause lit2)
      (watches-add! watches lit2 clause lit1))]))

;;; Initialize watches for all clauses
(define (watches-init-all! watches clauses)
  (doc 'export #t)
  (doc 'type '(-> Watches (List Clause) Void))
  (doc 'description "Initialize watches for all clauses")
  (for-each (lambda (c) (watches-init-clause! watches c)) clauses))

(doc 'section 'watch-propagation)

;;; Propagate a literal becoming false
;;; Returns: 'ok, (unit . lit . clause), or (conflict . clause)
(define (watches-propagate! watches assignment false-lit)
  (doc 'export #t)
  (doc 'type '(-> Watches Assignment Literal (Or 'ok (List 'unit Literal Clause) (List 'conflict Clause))))
  (doc 'description "Process a literal becoming false. Returns units to propagate or conflict.")
  (doc 'note "Modifies watches in place. Collects all unit implications.")
  (let ([watch-list (watches-get watches false-lit)]
        [units '()])
    ;; Process each clause watching this literal
    (let loop ([entries watch-list]
               [new-entries '()])
      (cond
       [(null? entries)
        ;; Done - update watch list and return units
        (watches-set! watches false-lit new-entries)
        (if (null? units)
            'ok
            (cons 'units (reverse units)))]
       [else
        (let* ([entry (car entries)]
               [clause (car entry)]
               [other-lit (cdr entry)]
               [other-val (assignment-eval-lit assignment other-lit)])
          (cond
           ;; Other watched literal is true - clause satisfied, keep watching
           [(eq? other-val 'true)
            (loop (cdr entries) (cons entry new-entries))]
           [else
            ;; Try to find a new literal to watch
            (let ([new-watch (find-new-watch clause false-lit other-lit assignment)])
              (cond
               [new-watch
                ;; Found new literal - move watch to it
                (watches-add! watches new-watch clause other-lit)
                (loop (cdr entries) new-entries)]
               ;; Couldn't find new watch
               [(eq? other-val 'unknown)
                ;; Unit clause! Keep watching, record unit
                (set! units (cons (cons other-lit clause) units))
                (loop (cdr entries) (cons entry new-entries))]
               [else
                ;; Conflict! Both watched literals are false
                (watches-set! watches false-lit (append new-entries entries))
                (list 'conflict clause)]))]))]))))

;;; Find a new literal to watch in a clause
(define (find-new-watch clause false-lit other-lit assignment)
  (doc 'type '(-> Clause Literal Literal Assignment (Maybe Literal)))
  (doc 'description "Find a literal in clause that is not false and not already watched")
  (let loop ([lits clause])
    (cond
     [(null? lits) #f]
     [else
      (let ([lit (car lits)])
        (cond
         ;; Skip the two currently watched literals
         [(lit-equal? lit false-lit) (loop (cdr lits))]
         [(lit-equal? lit other-lit) (loop (cdr lits))]
         ;; Check if this literal is not false
         [(not (eq? (assignment-eval-lit assignment lit) 'false))
          lit]  ; Found a good replacement
         [else (loop (cdr lits))]))])))

;;; Helper: check literal equality
(define (lit-equal? a b)
  (= a b))

(doc 'section 'help)

(define (watches-help)
  (doc 'export #t)
  (display "Two-Watched Literals (2WL) for SAT Solving\n\n")
  (display "Core idea: Each clause watches exactly 2 literals.\n")
  (display "When a literal becomes false, only visit clauses watching it.\n\n")
  (display "Functions:\n")
  (display "  (make-watches nvars)              Create watch structure\n")
  (display "  (watches-init-clause! w clause)   Initialize watches for clause\n")
  (display "  (watches-init-all! w clauses)     Initialize all clauses\n")
  (display "  (watches-propagate! w a lit)      Propagate lit becoming false\n"))
