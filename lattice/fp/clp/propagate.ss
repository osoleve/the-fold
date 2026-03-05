;;; @module propagate
;;; @requires fd-constraints
;;; @description Arc/bounds consistency propagation engine
;;; @purity partial
;;; @stability experimental

(require 'fd-constraints)

(doc 'module 'propagate)
(doc 'purity 'total)
(doc 'description "AC-3 style arc consistency propagation engine. When a variable's domain changes, all constraints involving that variable are re-evaluated.")

(doc 'section 'propagation-loop)

(define (propagate cs)
  (doc 'export #t)
  (doc 'type '(-> CStore (Maybe CStore)))
  (doc 'description "Run propagation until fixpoint or failure using default fuel limit")
  (propagate-with-fuel cs 10000))

(define (propagate-with-fuel cs fuel)
  (doc 'type '(-> CStore Nat (Maybe CStore)))
  (doc 'description "Propagation with explicit fuel limit to prevent infinite loops")
  (if (<= fuel 0)
      cs  ; Out of fuel - return current state
      (let ([popped (cstore-pop-pending cs)])
           (if (not popped)
               cs  ; No pending vars - fixpoint reached
               (let* ([var-id (car popped)]
                      [cs1 (cdr popped)]
                      [constraints (cstore-constraints cs1)])
                     (let ([relevant (filter-constraints-by-var-id constraints var-id)])
                          (propagate-constraints cs1 relevant (- fuel 1))))))))

(define (filter-constraints-by-var-id constraints var-id)
  (doc 'type '(-> (List Constraint) LVarId (List Constraint)))
  (doc 'description "Find constraints involving a specific variable ID")
  (filter (lambda (c)
                  (any (lambda (v) (= (lvar-id v) var-id))
                       (constraint-vars c)))
          constraints))

(define (propagate-constraints cs constraints fuel)
  (doc 'type '(-> CStore (List Constraint) Nat (Maybe CStore)))
  (doc 'description "Run each constraint's propagator and continue propagation")
  (if (null? constraints)
      (propagate-with-fuel cs fuel)  ; Continue with next pending
      (let* ([constraint (car constraints)]
             [propagator (constraint-propagator constraint)]
             [cs1 (propagator cs)])
            (if (not cs1)
                #f  ; Propagation failed
                (propagate-constraints cs1 (cdr constraints) fuel)))))

(doc 'section 'posting-constraints)

(define (post-constraint cs type vars propagator)
  (doc 'export #t)
  (doc 'type '(-> CStore Symbol (List LVar) (-> CStore (Maybe CStore)) (Maybe CStore)))
  (doc 'description "Create a constraint, add it to the store, and run initial propagation")
  (let* ([constraint (make-constraint type vars propagator)]
         [cs1 (cstore-add-constraint cs constraint)]
         [cs2 (fold-left (lambda (s v) (cstore-add-pending s v))
                         cs1
                         vars)]
         [cs3 (propagator cs2)])
        (and cs3 (propagate cs3))))

(doc 'section 'high-level-constraint-posting)

(define (post-in-range cs var lo hi)
  (doc 'type '(-> CStore LVar Int Int (Maybe CStore)))
  (doc 'description "Post domain constraint and propagate")
  (let ([cs1 ((in-range var lo hi) cs)])
       (and cs1 (propagate cs1))))

(define (post-=fd cs x y)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post equality constraint and propagate")
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((=fd x y) cs)  ; Both constants - no constraint needed
           (post-constraint cs '=fd vars (=fd x y)))))

(define (post-<fd cs x y)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post less-than constraint and propagate")
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((<fd x y) cs)
           (post-constraint cs '<fd vars (<fd x y)))))

(define (post-<=fd cs x y)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post less-or-equal constraint and propagate")
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((<=fd x y) cs)
           (post-constraint cs '<=fd vars (<=fd x y)))))

(define (post->fd cs x y)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (post-<fd cs y x))

(define (post->=fd cs x y)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (post-<=fd cs y x))

(define (post-=/=fd cs x y)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post disequality constraint and propagate")
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((=/=fd x y) cs)
           (post-constraint cs '=/=fd vars (=/=fd x y)))))

(define (post-+fd cs x y z)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post addition constraint and propagate")
  (let ([vars (filter lvar? (list x y z))])
       (if (null? vars)
           ((+fd x y z) cs)
           (post-constraint cs '+fd vars (+fd x y z)))))

(define (post--fd cs x y z)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post subtraction constraint and propagate")
  (let ([vars (filter lvar? (list x y z))])
       (if (null? vars)
           ((-fd x y z) cs)
           (post-constraint cs '-fd vars (-fd x y z)))))

(define (post-*fd cs x y z)
  (doc 'export #t)
  (doc 'type '(-> CStore (Or LVar Int) (Or LVar Int) (Or LVar Int) (Maybe CStore)))
  (doc 'description "Post multiplication constraint and propagate")
  (let ([vars (filter lvar? (list x y z))])
       (if (null? vars)
           ((*fd x y z) cs)
           (post-constraint cs '*fd vars (*fd x y z)))))

(doc 'section 'constraint-satisfaction-check)

(define (satisfied? cs)
  (doc 'type '(-> CStore Boolean))
  (doc 'description "Check if all variables are ground (complete solution)")
  (let ([domains (cstore-domains cs)])
       (and (null? (cstore-pending cs))
            (all domain-singleton?
                 (map cdr domains)))))

(define (all pred lst)
  (doc 'type '(-> (-> α Boolean) (List α) Boolean))
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (all pred (cdr lst))]))

(doc 'section 'debugging-support)

(define (propagate-trace cs)
  (doc 'type '(-> CStore (Maybe CStore)))
  (doc 'description "Propagation with tracing output for debugging")
  (propagate-trace-with-fuel cs 100))

(define (propagate-trace-with-fuel cs fuel)
  (display "  Fuel: ") (display fuel)
  (display " Pending: ") (display (cstore-pending cs))
  (newline)
  (if (<= fuel 0)
      (begin (display "  OUT OF FUEL\n") cs)
      (let ([popped (cstore-pop-pending cs)])
           (if (not popped)
               (begin (display "  FIXPOINT\n") cs)
               (let* ([var-id (car popped)]
                      [cs1 (cdr popped)]
                      [constraints (cstore-constraints cs1)]
                      [relevant (filter-constraints-by-var-id constraints var-id)])
                     (display "  Propagating var _") (display var-id)
                     (display " (") (display (length relevant)) (display " constraints)\n")
                     (propagate-constraints-trace cs1 relevant (- fuel 1)))))))

(define (propagate-constraints-trace cs constraints fuel)
  (if (null? constraints)
      (propagate-trace-with-fuel cs fuel)
      (let* ([constraint (car constraints)]
             [propagator (constraint-propagator constraint)]
             [cs1 (propagator cs)])
            (display "    ") (display (constraint-type constraint))
            (if cs1
                (display " OK\n")
                (display " FAIL\n"))
            (if (not cs1)
                #f
                (propagate-constraints-trace cs1 (cdr constraints) fuel)))))
