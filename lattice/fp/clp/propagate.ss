;;; lattice/fp/clp/propagate.ss — Constraint Propagation Engine
;;;
;;; Implements AC-3 style arc consistency propagation.
;;; When a variable's domain changes, all constraints involving
;;; that variable are re-evaluated.
;;;
;;; The propagation loop:
;;; 1. Pop variable from pending queue
;;; 2. For each constraint involving that variable:
;;;    - Run constraint's propagator
;;;    - If any domain changes, add affected vars to pending
;;; 3. Repeat until queue empty or failure
;;;
;;; Dependencies:
;;;   - store.ss
;;;   - fd-constraints.ss

(load "lattice/fp/clp/fd-constraints.ss")

;;; ============================================================
;;; Propagation Loop
;;; ============================================================

;;; propagate : CStore → (Maybe CStore)
;;; Run propagation until fixpoint or failure.
;;; Uses fuel to prevent infinite loops.
(define (propagate cs)
  (propagate-with-fuel cs 10000))

;;; propagate-with-fuel : CStore × Nat → (Maybe CStore)
;;; Propagation with explicit fuel limit.
(define (propagate-with-fuel cs fuel)
  (if (<= fuel 0)
      cs  ; Out of fuel - return current state
      (let ([popped (cstore-pop-pending cs)])
           (if (not popped)
               cs  ; No pending vars - fixpoint reached
               (let* ([var-id (car popped)]
                      [cs1 (cdr popped)]
                      [constraints (cstore-constraints cs1)])
                     ;; Find constraints involving this variable
                     (let ([relevant (filter-constraints-by-var-id constraints var-id)])
                          (propagate-constraints cs1 relevant (- fuel 1))))))))

;;; filter-constraints-by-var-id : (List Constraint) × LVarId → (List Constraint)
;;; Find constraints involving a specific variable ID.
(define (filter-constraints-by-var-id constraints var-id)
  (filter (lambda (c)
                  (any (lambda (v) (= (lvar-id v) var-id))
                       (constraint-vars c)))
          constraints))

;;; propagate-constraints : CStore × (List Constraint) × Nat → (Maybe CStore)
;;; Run each constraint's propagator and continue propagation.
(define (propagate-constraints cs constraints fuel)
  (if (null? constraints)
      (propagate-with-fuel cs fuel)  ; Continue with next pending
      (let* ([constraint (car constraints)]
             [propagator (constraint-propagator constraint)]
             [cs1 (propagator cs)])
            (if (not cs1)
                #f  ; Propagation failed
                (propagate-constraints cs1 (cdr constraints) fuel)))))

;;; ============================================================
;;; Posting Constraints
;;; ============================================================

;;; post-constraint : CStore × Symbol × (List LVar) × (CStore → (Maybe CStore))
;;;                   → (Maybe CStore)
;;; Create a constraint, add it to the store, and run initial propagation.
(define (post-constraint cs type vars propagator)
  (let* ([constraint (make-constraint type vars propagator)]
         [cs1 (cstore-add-constraint cs constraint)]
         ;; Add all constraint vars to pending for initial propagation
         [cs2 (fold-left (lambda (s v) (cstore-add-pending s v))
                         cs1
                         vars)]
         ;; Run initial propagation
         [cs3 (propagator cs2)])
        (and cs3 (propagate cs3))))

;;; ============================================================
;;; High-Level Constraint Posting
;;; ============================================================

;;; These functions create constraints and post them with propagation.

;;; post-in-range : CStore × LVar × Int × Int → (Maybe CStore)
;;; Post domain constraint and propagate.
(define (post-in-range cs var lo hi)
  (let ([cs1 ((in-range var lo hi) cs)])
       (and cs1 (propagate cs1))))

;;; post-=fd : CStore × (LVar | Int) × (LVar | Int) → (Maybe CStore)
;;; Post equality constraint and propagate.
(define (post-=fd cs x y)
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((=fd x y) cs)  ; Both constants - no constraint needed
           (post-constraint cs '=fd vars (=fd x y)))))

;;; post-<fd : CStore × (LVar | Int) × (LVar | Int) → (Maybe CStore)
;;; Post less-than constraint and propagate.
(define (post-<fd cs x y)
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((<fd x y) cs)
           (post-constraint cs '<fd vars (<fd x y)))))

;;; post-<=fd : CStore × (LVar | Int) × (LVar | Int) → (Maybe CStore)
;;; Post less-or-equal constraint and propagate.
(define (post-<=fd cs x y)
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((<=fd x y) cs)
           (post-constraint cs '<=fd vars (<=fd x y)))))

;;; post->fd : CStore × (LVar | Int) × (LVar | Int) → (Maybe CStore)
(define (post->fd cs x y)
  (post-<fd cs y x))

;;; post->=fd : CStore × (LVar | Int) × (LVar | Int) → (Maybe CStore)
(define (post->=fd cs x y)
  (post-<=fd cs y x))

;;; post-=/=fd : CStore × (LVar | Int) × (LVar | Int) → (Maybe CStore)
;;; Post disequality constraint and propagate.
(define (post-=/=fd cs x y)
  (let ([vars (filter lvar? (list x y))])
       (if (null? vars)
           ((=/=fd x y) cs)
           (post-constraint cs '=/=fd vars (=/=fd x y)))))

;;; post-+fd : CStore × (LVar | Int) × (LVar | Int) × (LVar | Int) → (Maybe CStore)
;;; Post addition constraint and propagate.
(define (post-+fd cs x y z)
  (let ([vars (filter lvar? (list x y z))])
       (if (null? vars)
           ((+fd x y z) cs)
           (post-constraint cs '+fd vars (+fd x y z)))))

;;; post--fd : CStore × (LVar | Int) × (LVar | Int) × (LVar | Int) → (Maybe CStore)
;;; Post subtraction constraint and propagate.
(define (post--fd cs x y z)
  (let ([vars (filter lvar? (list x y z))])
       (if (null? vars)
           ((-fd x y z) cs)
           (post-constraint cs '-fd vars (-fd x y z)))))

;;; post-*fd : CStore × (LVar | Int) × (LVar | Int) × (LVar | Int) → (Maybe CStore)
;;; Post multiplication constraint and propagate.
(define (post-*fd cs x y z)
  (let ([vars (filter lvar? (list x y z))])
       (if (null? vars)
           ((*fd x y z) cs)
           (post-constraint cs '*fd vars (*fd x y z)))))

;;; ============================================================
;;; Constraint Satisfaction Check
;;; ============================================================

;;; satisfied? : CStore → Bool
;;; Check if all variables are ground (complete solution).
(define (satisfied? cs)
  (let ([domains (cstore-domains cs)])
       (and (null? (cstore-pending cs))
            (all domain-singleton?
                 (map cdr domains)))))

;;; all : (α → Bool) × (List α) → Bool
(define (all pred lst)
  (cond
   [(null? lst) #t]
   [(not (pred (car lst))) #f]
   [else (all pred (cdr lst))]))

;;; ============================================================
;;; Debugging Support
;;; ============================================================

;;; propagate-trace : CStore → (Maybe CStore)
;;; Propagation with tracing output for debugging.
(define (propagate-trace cs)
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
