(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires interval-global
(require 'interval-global)

(doc 'module 'interval-contract)
(doc 'description "Constraint contractors for interval optimization with constraint propagation")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Integrates constraint propagation with interval branch-and-bound.
Contractors shrink boxes based on constraints, enabling more aggressive pruning than bisection alone.

A contractor is: Box → Box | 'empty
It returns a (possibly smaller) box satisfying the constraint, or 'empty if no feasible point exists.

Key insight: Apply contractors BEFORE bisection. If propagation shrinks the box significantly,
we avoid unnecessary bisections. If a contractor returns 'empty, we prune immediately.")

(doc 'section 'contractors)

(define (contract-all contractors box)
  (doc 'export #t)
  (doc 'type '(-> (List Contractor) Box (Union Box 'empty)))
  (doc 'description "Apply all contractors in sequence until fixpoint or empty")
  (contract-fixpoint contractors box 100))

;;; contract-fixpoint : (List Contractor) × Box × Nat → Box | 'empty
;;; Apply contractors repeatedly until no change or fuel exhausted.
(define (contract-fixpoint contractors box fuel)
  (doc 'export #t)
  (if (<= fuel 0)
      box
      (let ([new-box (contract-once contractors box)])
        (cond
          [(eq? new-box 'empty) 'empty]
          [(box-equal? new-box box) box]  ; Fixpoint reached
          [else (contract-fixpoint contractors new-box (- fuel 1))]))))

;;; contract-once : (List Contractor) × Box → Box | 'empty
;;; Apply each contractor once in sequence.
(define (contract-once contractors box)
  (if (null? contractors)
      box
      (let ([result ((car contractors) box)])
        (if (eq? result 'empty)
            'empty
            (contract-once (cdr contractors) result)))))

;;; box-equal? : Box × Box → Boolean
;;; Check if two boxes are equal (same intervals).
(define (box-equal? box1 box2)
  (and (= (length box1) (length box2))
       (let loop ([b1 box1] [b2 box2])
         (or (null? b1)
             (and (= (interval-lo (car b1)) (interval-lo (car b2)))
                  (= (interval-hi (car b1)) (interval-hi (car b2)))
                  (loop (cdr b1) (cdr b2)))))))

(doc 'section 'basic-contractors)

(define (make-bound-contractor dim lo hi)
  (doc 'export #t)
  (doc 'type '(-> Nat Real Real Contractor))
  (doc 'description "Create contractor that constrains dimension i to [lo, hi]")
  (lambda (box)
    (let* ([iv (list-ref box dim)]
           [new-lo (max (interval-lo iv) lo)]
           [new-hi (min (interval-hi iv) hi)])
      (if (> new-lo new-hi)
          'empty
          (list-update box dim (interval new-lo new-hi))))))

;;; make-equality-contractor : Nat × Real → Contractor
;;; Constrain dimension i to equal value v.
(define (make-equality-contractor dim v)
  (doc 'export #t)
  (lambda (box)
    (let ([iv (list-ref box dim)])
      (if (interval-contains? iv v)
          (list-update box dim (interval-singleton v))
          'empty))))

(doc 'section 'linear-contractors)
(doc 'note "For constraints of the form: a₁x₁ + a₂x₂ + ... ≤ b
We can derive bounds on each variable from the others")

(define (make-linear-le-contractor coeffs rhs)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Nat Real)) Real Contractor))
  (doc 'description "Contractor for: sum(coef_i * x_i) <= rhs")
  (doc 'param 'coeffs "List of (dimension . coefficient) pairs")
  (lambda (box)
    (linear-le-contract coeffs rhs box)))

;;; linear-le-contract : (List (Nat × Real)) × Real × Box → Box | 'empty
;;; Contract box based on linear inequality.
(define (linear-le-contract coeffs rhs box)
  ;; For each variable x_i with coefficient a_i:
  ;; a_i * x_i <= rhs - sum(a_j * x_j for j ≠ i)
  ;; If a_i > 0: x_i <= (rhs - other_sum_hi) / a_i
  ;; If a_i < 0: x_i >= (rhs - other_sum_lo) / a_i
  (let loop ([remaining coeffs] [current-box box])
    (if (null? remaining)
        current-box
        (let* ([pair (car remaining)]
               [dim (car pair)]
               [coef (cdr pair)]
               [others (remove-dim dim coeffs)]
               [other-bounds (sum-interval-bounds others current-box)])
          (if (eq? other-bounds 'empty)
              'empty
              (let* ([other-lo (interval-lo other-bounds)]
                     [other-hi (interval-hi other-bounds)]
                     [iv (list-ref current-box dim)]
                     [new-iv (contract-linear-var iv coef rhs other-lo other-hi)])
                (if (eq? new-iv 'empty)
                    'empty
                    (loop (cdr remaining)
                          (list-update current-box dim new-iv)))))))))

;;; contract-linear-var : Interval × Real × Real × Real × Real → Interval | 'empty
;;; Contract interval for variable with coefficient in linear constraint.
;;; coef * x + other ≤ rhs, where other ∈ [other-lo, other-hi]
(define (contract-linear-var iv coef rhs other-lo other-hi)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (cond
      [(> coef 0)
       ;; x ≤ (rhs - other-lo) / coef  (use lo of others for tightest upper)
       (let* ([upper-bound (/ (- rhs other-lo) coef)]
              [new-hi (min hi upper-bound)])
         (if (> lo new-hi)
             'empty
             (interval lo new-hi)))]
      [(< coef 0)
       ;; x ≥ (rhs - other-lo) / coef  (use lo of others for loosest lower bound)
       ;; Both cases use other-lo: for feasibility, we need the loosest bound
       ;; that still guarantees existence of a valid assignment to other vars
       (let* ([lower-bound (/ (- rhs other-lo) coef)]
              [new-lo (max lo lower-bound)])
         (if (> new-lo hi)
             'empty
             (interval new-lo hi)))]
      [else
       ;; coef = 0, constraint doesn't involve this variable
       iv])))

;;; remove-dim : Nat × (List (Nat × Real)) → (List (Nat × Real))
;;; Remove entries for dimension dim.
(define (remove-dim dim coeffs)
  (filter (lambda (p) (not (= (car p) dim))) coeffs))

;;; sum-interval-bounds : (List (Nat × Real)) × Box → Interval | 'empty
;;; Compute interval bounds on sum of coef * x for given coefficients.
(define (sum-interval-bounds coeffs box)
  (if (null? coeffs)
      (interval-singleton 0)
      (let loop ([remaining coeffs] [acc (interval-singleton 0)])
        (if (null? remaining)
            acc
            (let* ([pair (car remaining)]
                   [dim (car pair)]
                   [coef (cdr pair)]
                   [iv (list-ref box dim)]
                   [scaled (interval-scale iv coef)]
                   [new-acc (interval-add acc scaled)])
              (loop (cdr remaining) new-acc))))))

;;; make-linear-ge-contractor : (List (Nat × Real)) × Real → Contractor
;;; Contractor for: sum(coef_i * x_i) >= rhs
;;; Equivalent to: sum(-coef_i * x_i) <= -rhs
(define (make-linear-ge-contractor coeffs rhs)
  (doc 'export #t)
  (let ([neg-coeffs (map (lambda (p) (cons (car p) (- (cdr p)))) coeffs)])
    (make-linear-le-contractor neg-coeffs (- rhs))))

;;; make-linear-eq-contractor : (List (Nat × Real)) × Real → Contractor
;;; Contractor for: sum(coef_i * x_i) = rhs
;;; Combines le and ge contractors.
(define (make-linear-eq-contractor coeffs rhs)
  (doc 'export #t)
  (let ([le-contractor (make-linear-le-contractor coeffs rhs)]
        [ge-contractor (make-linear-ge-contractor coeffs rhs)])
    (lambda (box)
      (let ([box1 (le-contractor box)])
        (if (eq? box1 'empty)
            'empty
            (ge-contractor box1))))))

(doc 'section 'quadratic-contractors)

(define (make-sphere-contractor center radius-sq)
  (doc 'export #t)
  (doc 'type '(-> (List Real) Real Contractor))
  (doc 'description "Contractor for: sum((x_i - center_i)²) ≤ radius²")
  (doc 'note "Useful for trust region constraints")
  (lambda (box)
    ;; For each dimension, compute bound from others
    (let loop ([dim 0] [current-box box])
      (if (>= dim (length box))
          current-box
          (let ([new-iv (contract-sphere-dim current-box center radius-sq dim)])
            (if (eq? new-iv 'empty)
                'empty
                (loop (+ dim 1) (list-update current-box dim new-iv))))))))

;;; contract-sphere-dim : Box × (List Real) × Real × Nat → Interval | 'empty
;;; Contract dimension dim based on sphere constraint.
(define (contract-sphere-dim box center radius-sq dim)
  (let* ([c-i (list-ref center dim)]
         [iv (list-ref box dim)]
         ;; Sum of squared distances from other dimensions
         [other-sq (sphere-other-sq box center dim)]
         [remaining-sq (- radius-sq other-sq)])
    (if (< remaining-sq 0)
        ;; Other dimensions already exceed radius - no feasible point
        'empty
        ;; (x_i - c_i)² ≤ remaining-sq
        ;; -sqrt(remaining) ≤ x_i - c_i ≤ sqrt(remaining)
        ;; c_i - sqrt(remaining) ≤ x_i ≤ c_i + sqrt(remaining)
        (let* ([delta (sqrt remaining-sq)]
               [bound-lo (- c-i delta)]
               [bound-hi (+ c-i delta)]
               [new-lo (max (interval-lo iv) bound-lo)]
               [new-hi (min (interval-hi iv) bound-hi)])
          (if (> new-lo new-hi)
              'empty
              (interval new-lo new-hi))))))

;;; sphere-other-sq : Box × (List Real) × Nat → Real
;;; Compute minimum sum of squared distances for dimensions other than dim.
;;; Uses interval midpoints as approximation.
(define (sphere-other-sq box center dim)
  (let loop ([d 0] [ivs box] [cs center] [sum 0])
    (cond
      [(null? ivs) sum]
      [(= d dim) (loop (+ d 1) (cdr ivs) (cdr cs) sum)]
      [else
       (let* ([iv (car ivs)]
              [c (car cs)]
              ;; Minimum distance: 0 if c in interval, else distance to nearest bound
              [min-dist (cond
                          [(interval-contains? iv c) 0]
                          [(< c (interval-lo iv)) (- (interval-lo iv) c)]
                          [else (- c (interval-hi iv))])])
         (loop (+ d 1) (cdr ivs) (cdr cs) (+ sum (* min-dist min-dist))))])))

(doc 'section 'constrained-optimization)

(define (interval-minimize-constrained f-interval initial-box criteria contractors)
  (doc 'export #t)
  (doc 'type '(-> (-> Box Interval) Box IntervalConvergence (List Contractor) IntervalOptResult))
  (doc 'description "Global minimization with constraint contractors")
  (doc 'note "Contractors are applied before each bisection to shrink boxes")
  (let* ([width-tol (ic-width-tol criteria)]
         [max-iter (ic-max-iter criteria)]
         [gap-tol (ic-gap-tol criteria)]
         ;; Contract initial box
         [contracted-box (contract-all contractors initial-box)])
    (if (eq? contracted-box 'empty)
        ;; No feasible region
        (make-interval-opt-result '() +inf.0 0 'infeasible)
        ;; Run optimization on contracted box
        (let* ([f0 (f-interval contracted-box)]
               [mid0 (box-midpoint contracted-box)]
               [f-mid0 (eval-at-point f-interval mid0)]
               [item0 (make-work-item contracted-box f0)]
               [work-list (heap-insert-by work-item-cmp item0 heap-empty)]
               [best-upper (min f-mid0 (interval-hi f0))])
          (bb-loop-constrained f-interval contractors work-list best-upper '() 1
                               width-tol max-iter gap-tol)))))

;;; bb-loop-constrained : Similar to bb-loop but applies contractors before bisection
(define (bb-loop-constrained f-interval contractors work-list best-upper candidates iter
                             width-tol max-iter gap-tol)
  (cond
    [(heap-empty? work-list)
     (make-interval-opt-result (filter-candidates candidates best-upper f-interval)
                               best-upper iter 'exhausted)]
    [(>= iter max-iter)
     (make-interval-opt-result (filter-candidates candidates best-upper f-interval)
                               best-upper iter 'max-iterations)]
    [else
     (let*-values
         ([(rest-heap item) (heap-pop-by work-item-cmp work-list)])
       (let* ([box (work-item-box item)]
              [f-iv (work-item-interval item)]
              [lo (interval-lo f-iv)])
         (cond
           ;; Prune: this box can't contain the global minimum
           [(> lo best-upper)
            (bb-loop-constrained f-interval contractors rest-heap best-upper candidates iter
                                 width-tol max-iter gap-tol)]
           ;; Converged: box is small enough
           [(< (box-max-width box) width-tol)
            (bb-loop-constrained f-interval contractors rest-heap best-upper
                                 (cons box candidates) iter
                                 width-tol max-iter gap-tol)]
           ;; Converged: gap is small enough
           [(< (- (interval-hi f-iv) lo) gap-tol)
            (bb-loop-constrained f-interval contractors rest-heap best-upper
                                 (cons box candidates) iter
                                 width-tol max-iter gap-tol)]
           ;; Contract and bisect
           [else
            (let* ([pair (bisect-box box)]
                   [box1-raw (car pair)]
                   [box2-raw (cdr pair)]
                   ;; Apply contractors to shrink boxes
                   [box1 (contract-all contractors box1-raw)]
                   [box2 (contract-all contractors box2-raw)])
              (add-contracted-boxes f-interval contractors rest-heap best-upper
                                    candidates (+ iter 2)
                                    width-tol max-iter gap-tol
                                    box1 box2))])))]))

;;; add-contracted-boxes : Helper to add contracted boxes to work list
(define (add-contracted-boxes f-interval contractors rest-heap best-upper
                              candidates iter width-tol max-iter gap-tol
                              box1 box2)
  (let* ([results1 (if (eq? box1 'empty)
                       (list rest-heap best-upper)
                       (add-box-to-heap f-interval box1 rest-heap best-upper))]
         [heap1 (car results1)]
         [best1 (cadr results1)]
         [results2 (if (eq? box2 'empty)
                       (list heap1 best1)
                       (add-box-to-heap f-interval box2 heap1 best1))]
         [heap2 (car results2)]
         [best2 (cadr results2)])
    (bb-loop-constrained f-interval contractors heap2 best2 candidates iter
                         width-tol max-iter gap-tol)))

;;; add-box-to-heap : Evaluate box and add to heap, updating best-upper
(define (add-box-to-heap f-interval box heap best-upper)
  (let* ([f-iv (f-interval box)]
         [mid (box-midpoint box)]
         [f-mid (eval-at-point f-interval mid)]
         [new-best (min best-upper f-mid (interval-hi f-iv))]
         [item (make-work-item box f-iv)]
         [new-heap (heap-insert-by work-item-cmp item heap)])
    (list new-heap new-best)))

;;; ============================================================================
;;; Convenience: Common Constraint Patterns
;;; ============================================================================

;;; make-box-constraint : (List (Real × Real)) → (List Contractor)
;;; Create bound contractors for each dimension from (lo, hi) pairs.
(define (make-box-constraints bounds)
  (doc 'export #t)
  (let loop ([dim 0] [bs bounds] [acc '()])
    (if (null? bs)
        (reverse acc)
        (let ([lo (caar bs)]
              [hi (cdar bs)])
          (loop (+ dim 1) (cdr bs)
                (cons (make-bound-contractor dim lo hi) acc))))))

