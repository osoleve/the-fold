(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude interval heap
(require 'prelude)
(require 'interval)
(require 'heap)

(doc 'module 'interval-global)
(doc 'description "Interval branch-and-bound global optimization with guaranteed bounds")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Guaranteed enclosure of global minima using interval arithmetic.
Unlike gradient-based methods (which find local minima), this provides rigorous bounds:
the true global minimum lies within the returned interval.

Algorithm: Branch-and-bound with best-first search
  1. Maintain work list of boxes (priority queue by interval lower bound)
  2. For each box, compute interval enclosure of objective
  3. Prune boxes whose lower bound exceeds best known upper bound
  4. Bisect boxes along widest dimension until tolerance met
  5. Return candidate boxes containing potential global minima

Key insight: Update best-upper on EVERY evaluation (not just when converged)
to enable aggressive pruning of other branches")

(doc 'section 'convergence-criteria)

(define (make-interval-convergence width-tol max-iter . gap-tol-opt)
  (doc 'export #t)
  (doc 'type '(-> Real Nat Real IntervalConvergence))
  (doc 'description "Create convergence criteria for interval optimization")
  (doc 'param 'width-tol "Stop when box width below this tolerance")
  (doc 'param 'max-iter "Stop after this many box evaluations")
  (doc 'param 'gap-tol "Stop when (upper - lower) bound gap below this tolerance (optional, defaults to width-tol)")
  (let ([gap-tol (if (null? gap-tol-opt) width-tol (car gap-tol-opt))])
    (list 'interval-convergence width-tol max-iter gap-tol)))

;;; interval-convergence? : Any → Boolean
(define (interval-convergence? x)
  (and (pair? x)
       (eq? (car x) 'interval-convergence)))

;;; Accessors
(define (ic-width-tol ic) (list-ref ic 1))
(define (ic-max-iter ic) (list-ref ic 2))
(define (ic-gap-tol ic) (list-ref ic 3))

;;; Default convergence criteria
(define *default-interval-convergence*
  (make-interval-convergence 1e-6 10000))

;;; ============================================================================
;;; Interval Optimization Result
;;; ============================================================================

;;; make-interval-opt-result : (List Box) × Real × Nat × Symbol → IntervalOptResult
;;; Create result structure.
;;;   candidates: list of small boxes potentially containing global min
;;;   best-upper: guaranteed upper bound on global minimum
;;;   iterations: number of box evaluations performed
;;;   reason: why terminated ('width-converged, 'gap-converged, 'max-iterations)
(define (make-interval-opt-result candidates best-upper iterations reason)
  (list 'interval-opt-result candidates best-upper iterations reason))

;;; interval-opt-result? : Any → Boolean
(define (interval-opt-result? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) 'interval-opt-result)))

;;; Accessors
(define (ior-candidates result) (list-ref result 1))
(doc 'export #t)
(define (ior-best-upper result) (list-ref result 2))
(doc 'export #t)
(define (ior-iterations result) (list-ref result 3))
(define (ior-reason result) (list-ref result 4))

;;; ior-best-box : IntervalOptResult → Box | #f
;;; Get the candidate box with lowest midpoint value, or #f if no candidates.
(define (ior-best-box result)
  (let ([candidates (ior-candidates result)])
    (if (null? candidates)
        #f
        (car candidates))))

;;; ior-best-point : IntervalOptResult → (List Real) | #f
;;; Get the midpoint of the best candidate box.
(define (ior-best-point result)
  (doc 'export #t)
  (let ([best (ior-best-box result)])
    (and best (box-midpoint best))))

;;; ior-solution-box : IntervalOptResult → Box | #f
;;; Get the hull (union) of all candidate boxes — a single conservative enclosure.
(define (ior-solution-box result)
  (doc 'export #t)
  (let ([candidates (ior-candidates result)])
    (if (null? candidates)
        #f
        (fold-left box-hull (car candidates) (cdr candidates)))))

;;; box-hull : Box × Box → Box
;;; Hull (union) of two boxes — smallest box containing both.
(define (box-hull box1 box2)
  (map interval-hull box1 box2))

;;; ============================================================================
;;; Box Operations
;;; ============================================================================

;;; box-midpoint : Box → (List Real)
;;; Get midpoint of each dimension.
(define (box-midpoint box)
  (map interval-mid box))

;;; box-max-width : Box → Real
;;; Get width of widest dimension.
(define (box-max-width box)
  (if (null? box)
      0
      (fold-left max 0 (map interval-width box))))

;;; box-widest-dimension : Box → Nat
;;; Get index of widest dimension.
(define (box-widest-dimension box)
  (let loop ([ivs box] [idx 0] [max-idx 0] [max-width 0])
    (if (null? ivs)
        max-idx
        (let ([w (interval-width (car ivs))])
          (if (> w max-width)
              (loop (cdr ivs) (+ idx 1) idx w)
              (loop (cdr ivs) (+ idx 1) max-idx max-width))))))

;;; bisect-box : Box → (Pair Box Box)
;;; Split box along widest dimension.
(define (bisect-box box)
  (let* ([dim (box-widest-dimension box)]
         [iv (list-ref box dim)]
         [pair (interval-bisect iv)]
         [iv-lo (car pair)]
         [iv-hi (cdr pair)])
    (cons (list-update box dim iv-lo)
          (list-update box dim iv-hi))))

;;; list-update : (List α) × Nat × α → (List α)
;;; Return list with element at index replaced.
(define (list-update lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) val))))

;;; ============================================================================
;;; Work List (Priority Queue by lower bound)
;;; ============================================================================
;;;
;;; Work items are (work-item box f-interval) where:
;;;   box: the search region
;;;   f-interval: interval enclosure of f over box

;;; make-work-item : Box × Interval → WorkItem
(define (make-work-item box f-interval)
  (list 'work-item box f-interval))

;;; work-item-box : WorkItem → Box
(define (work-item-box item) (list-ref item 1))

;;; work-item-interval : WorkItem → Interval
(define (work-item-interval item) (list-ref item 2))

;;; work-item-lo : WorkItem → Real
;;; Lower bound of objective over this box.
(define (work-item-lo item)
  (interval-lo (work-item-interval item)))

;;; work-item-cmp : WorkItem × WorkItem → Boolean
;;; Compare work items by lower bound (best-first: smaller lower bound wins).
(define (work-item-cmp a b)
  (<= (work-item-lo a) (work-item-lo b)))

;;; ============================================================================
;;; Point Evaluation via Interval Arithmetic
;;; ============================================================================

;;; point->box : (List Real) → Box
;;; Convert a point to a box of singleton intervals.
(define (point->box point)
  (map interval-singleton point))

;;; eval-at-point : (Box → Interval) × (List Real) → Real
;;; Evaluate interval function at a point, returning scalar.
;;; Uses inclusion property: f([x,x]) = [f(x), f(x)].
(define (eval-at-point f-interval point)
  (interval-lo (f-interval (point->box point))))

;;; ============================================================================
;;; Core Algorithm: Interval Branch-and-Bound
;;; ============================================================================

;;; interval-minimize : (Box → Interval) × Box × IntervalConvergence [× (List Real → Real)] → IntervalOptResult
;;; Global minimization via interval branch-and-bound.
;;;
;;; Arguments:
;;;   f-interval: objective function lifted to intervals (Box → Interval)
;;;   initial-box: search domain
;;;   criteria: convergence criteria
;;;   f-scalar (optional): scalar objective for faster midpoint test
;;;     If not provided, uses point-interval evaluation of f-interval.
;;;
;;; Returns IntervalOptResult with:
;;;   - candidates: small boxes potentially containing global minimum
;;;   - best-upper: guaranteed upper bound on global minimum f*
;;;   - iterations: number of evaluations
;;;   - reason: termination reason
;;;
;;; Guarantee: For any x in the domain, f(x) >= interval-lo of all candidates.
(define (interval-minimize f-interval initial-box criteria . f-scalar-opt)
  (doc 'export #t)
  (let* ([f-scalar (if (null? f-scalar-opt)
                       (lambda (pt) (eval-at-point f-interval pt))
                       (car f-scalar-opt))]
         [width-tol (ic-width-tol criteria)]
         [max-iter (ic-max-iter criteria)]
         [gap-tol (ic-gap-tol criteria)]
         ;; Evaluate initial box
         [f0 (f-interval initial-box)]
         [mid0 (box-midpoint initial-box)]
         [f-mid0 (f-scalar mid0)]
         [item0 (make-work-item initial-box f0)]
         [work-list (heap-insert-by work-item-cmp item0 heap-empty)]
         [best-upper (min f-mid0 (interval-hi f0))])
    (bb-loop f-interval f-scalar work-list best-upper '() 1
             width-tol max-iter gap-tol)))

;;; bb-loop : (Box → Interval) × ((List Real) → Real) × Heap × Real × (List Box) × Nat × Real × Nat × Real → IntervalOptResult
;;; Main branch-and-bound loop.
(define (bb-loop f-interval f-scalar work-list best-upper candidates iter
                 width-tol max-iter gap-tol)
  (cond
    ;; Work list exhausted
    [(heap-empty? work-list)
     (make-interval-opt-result (filter-candidates candidates best-upper f-interval)
                               best-upper iter 'exhausted)]
    ;; Max iterations reached
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
            (bb-loop f-interval f-scalar rest-heap best-upper candidates iter
                     width-tol max-iter gap-tol)]
           ;; Converged: box is small enough
           [(< (box-max-width box) width-tol)
            (bb-loop f-interval f-scalar rest-heap best-upper
                     (cons box candidates) iter
                     width-tol max-iter gap-tol)]
           ;; Converged: gap is small enough
           [(< (- (interval-hi f-iv) lo) gap-tol)
            (bb-loop f-interval f-scalar rest-heap best-upper
                     (cons box candidates) iter
                     width-tol max-iter gap-tol)]
           ;; Bisect and continue
           [else
            (let* ([pair (bisect-box box)]
                   [box1 (car pair)]
                   [box2 (cdr pair)]
                   ;; Evaluate both children
                   [f1 (f-interval box1)]
                   [f2 (f-interval box2)]
                   ;; Midpoint tests for tighter pruning
                   [mid1 (box-midpoint box1)]
                   [mid2 (box-midpoint box2)]
                   [f-mid1 (f-scalar mid1)]
                   [f-mid2 (f-scalar mid2)]
                   ;; Update best-upper from all sources
                   [new-best (min best-upper f-mid1 f-mid2
                                  (interval-hi f1) (interval-hi f2))]
                   ;; Create work items
                   [item1 (make-work-item box1 f1)]
                   [item2 (make-work-item box2 f2)]
                   ;; Add to heap (pruning will happen on next iteration)
                   [new-heap (heap-insert-by work-item-cmp item1
                               (heap-insert-by work-item-cmp item2 rest-heap))])
              (bb-loop f-interval f-scalar new-heap new-best candidates (+ iter 2)
                       width-tol max-iter gap-tol))])))]))

;;; heap-pop-by : (α × α → Boolean) × Heap → (Values Heap α)
;;; Pop from heap using custom comparator.
(define (heap-pop-by cmp heap)
  (values (heap-delete-top-by cmp heap) (heap-value heap)))

;;; filter-candidates : (List Box) × Real × (Box → Interval) → (List Box)
;;; Remove candidates that can no longer contain the global minimum.
(define (filter-candidates candidates best-upper f-interval)
  (filter (lambda (box)
            (let ([f-iv (f-interval box)])
              (<= (interval-lo f-iv) best-upper)))
          candidates))

;;; ============================================================================
;;; Maximization (via negation)
;;; ============================================================================

;;; interval-maximize : (Box → Interval) × Box × IntervalConvergence [× (List Real → Real)] → IntervalOptResult
;;; Global maximization by minimizing -f.
(define (interval-maximize f-interval initial-box criteria . f-scalar-opt)
  (doc 'export #t)
  (let* ([neg-f-interval (lambda (box) (interval-neg (f-interval box)))]
         [neg-f-scalar (if (null? f-scalar-opt)
                           #f  ; Let interval-minimize derive it
                           (lambda (pt) (- ((car f-scalar-opt) pt))))]
         [result (if neg-f-scalar
                     (interval-minimize neg-f-interval initial-box criteria neg-f-scalar)
                     (interval-minimize neg-f-interval initial-box criteria))])
    ;; Negate the best-upper to get best-lower for maximization
    (make-interval-opt-result (ior-candidates result)
                              (- (ior-best-upper result))
                              (ior-iterations result)
                              (ior-reason result))))

;;; ============================================================================
;;; Convenience: Simple interface
;;; ============================================================================

;;; interval-find-minimum : (Box → Interval) × Box → (List Real)
;;; Simplified interface returning just the best point.
(define (interval-find-minimum f-interval box)
  (doc 'export #t)
  (let ([result (interval-minimize f-interval box *default-interval-convergence*)])
    (or (ior-best-point result)
        (box-midpoint box))))

;;; ============================================================================
;;; Standard Test Functions (interval versions)
;;; ============================================================================

;;; interval-sphere : Box → Interval
;;; Sphere function: f(x) = sum(x_i^2). Global min at origin.
(define (interval-sphere box)
  (doc 'export #t)
  (fold-left interval-add
             (interval-singleton 0)
             (map interval-sqr box)))

;;; scalar-sphere : (List Real) → Real
(define (scalar-sphere point)
  (fold-left + 0 (map (lambda (x) (* x x)) point)))

;;; interval-rosenbrock : Box → Interval
;;; Rosenbrock function (2D): f(x,y) = (1-x)^2 + 100(y-x^2)^2
;;; Global min at (1, 1) with value 0.
(define (interval-rosenbrock box)
  (doc 'export #t)
  (let* ([x (car box)]
         [y (cadr box)]
         [one (interval-singleton 1)]
         [hundred (interval-singleton 100)]
         ;; (1 - x)^2
         [term1 (interval-sqr (interval-sub one x))]
         ;; (y - x^2)^2
         [term2 (interval-sqr (interval-sub y (interval-sqr x)))])
    (interval-add term1 (interval-mul hundred term2))))

;;; scalar-rosenbrock : (List Real) → Real
(define (scalar-rosenbrock point)
  (let ([x (car point)]
        [y (cadr point)])
    (+ (* (- 1 x) (- 1 x))
       (* 100 (- y (* x x)) (- y (* x x))))))

;;; interval-rastrigin : Box → Interval
;;; Rastrigin function: f(x) = 10n + sum(x_i^2 - 10*cos(2π*x_i))
;;; Highly multimodal, global min at origin.
;;; Note: Without interval cos, we use a conservative bound.
(define (interval-rastrigin box)
  (doc 'export #t)
  (let* ([n (length box)]
         [ten-n (interval-singleton (* 10 n))]
         ;; For each x_i: x_i^2 - 10*cos(2π*x_i)
         ;; cos range is [-1, 1], so -10*cos ∈ [-10, 10]
         ;; Thus x_i^2 - 10*cos ∈ [x_i^2 - 10, x_i^2 + 10]
         [terms (map (lambda (iv)
                       (let ([sq (interval-sqr iv)])
                         (interval-add sq (interval -10 10))))
                     box)])
    (fold-left interval-add ten-n terms)))

;;; scalar-rastrigin : (List Real) → Real
(define (scalar-rastrigin point)
  (let* ([n (length point)]
         [pi 3.141592653589793]
         [terms (map (lambda (x)
                       (- (* x x)
                          (* 10 (cos (* 2 pi x)))))
                     point)])
    (+ (* 10 n) (fold-left + 0 terms))))

;;; ============================================================================
;;; Display
;;; ============================================================================

;;; interval-opt-result->string : IntervalOptResult → String
(define (interval-opt-result->string result)
  (string-append
   "IntervalOptResult:\n"
   "  Candidates: " (number->string (length (ior-candidates result))) " boxes\n"
   "  Best upper bound: " (number->string (ior-best-upper result)) "\n"
   "  Iterations: " (number->string (ior-iterations result)) "\n"
   "  Reason: " (symbol->string (ior-reason result))))

(define (interval-opt-result-print result)
  (display (interval-opt-result->string result))
  (newline))

;;; ============================================================================
;;; Monotonicity-Enhanced Optimization
;;; ============================================================================
;;;
;;; When interval gradient bounds are available, use them for additional pruning:
;;; - If gradient doesn't contain zero in some dimension, function is monotonic
;;; - Monotonic dimensions can be reduced to boundary faces
;;; - This dramatically speeds up optimization for smooth functions

;;; interval-minimize-with-gradient : (Box -> Interval) x (Box -> (List Interval)) x Box x IntervalConvergence [x ((List Real) -> Real)] -> IntervalOptResult
;;; Global minimization with monotonicity pruning via interval gradients.
;;;
;;; Arguments:
;;;   f-interval: objective function (Box -> Interval)
;;;   grad-fn: interval gradient function (Box -> (List Interval))
;;;   initial-box: search domain
;;;   criteria: convergence criteria
;;;   f-scalar (optional): scalar objective for faster midpoint test
;;;
;;; The grad-fn should return a list of intervals bounding the partial
;;; derivatives over the input box. Use interval-gradient from
;;; lattice/autodiff/interval-autodiff.ss to compute this automatically.
(define (interval-minimize-with-gradient f-interval grad-fn initial-box criteria . f-scalar-opt)
  (doc 'export #t)
  (let* ([f-scalar (if (null? f-scalar-opt)
                       (lambda (pt) (eval-at-point f-interval pt))
                       (car f-scalar-opt))]
         [width-tol (ic-width-tol criteria)]
         [max-iter (ic-max-iter criteria)]
         [gap-tol (ic-gap-tol criteria)]
         ;; Evaluate initial box
         [f0 (f-interval initial-box)]
         [mid0 (box-midpoint initial-box)]
         [f-mid0 (f-scalar mid0)]
         [item0 (make-work-item initial-box f0)]
         [work-list (heap-insert-by work-item-cmp item0 heap-empty)]
         [best-upper (min f-mid0 (interval-hi f0))])
    (bb-loop-monotonic f-interval grad-fn f-scalar work-list best-upper '() 1
                       width-tol max-iter gap-tol)))

;;; bb-loop-monotonic : (Box -> Interval) x (Box -> (List Interval)) x ((List Real) -> Real) x Heap x Real x (List Box) x Nat x Real x Nat x Real -> IntervalOptResult
;;; Main branch-and-bound loop with monotonicity pruning.
(define (bb-loop-monotonic f-interval grad-fn f-scalar work-list best-upper candidates iter
                           width-tol max-iter gap-tol)
  (cond
    ;; Work list exhausted
    [(heap-empty? work-list)
     (make-interval-opt-result (filter-candidates candidates best-upper f-interval)
                               best-upper iter 'exhausted)]
    ;; Max iterations reached
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
            (bb-loop-monotonic f-interval grad-fn f-scalar rest-heap best-upper candidates iter
                               width-tol max-iter gap-tol)]
           ;; Converged: box is small enough
           [(< (box-max-width box) width-tol)
            (bb-loop-monotonic f-interval grad-fn f-scalar rest-heap best-upper
                               (cons box candidates) iter
                               width-tol max-iter gap-tol)]
           ;; Converged: gap is small enough
           [(< (- (interval-hi f-iv) lo) gap-tol)
            (bb-loop-monotonic f-interval grad-fn f-scalar rest-heap best-upper
                               (cons box candidates) iter
                               width-tol max-iter gap-tol)]
           [else
            ;; Try monotonicity reduction before bisecting
            (let* ([grad-ivs (grad-fn box)]
                   [reduced-box (maybe-reduce-box-monotonic box grad-ivs)]
                   [actually-reduced? (not (equal? reduced-box box))])
              (if actually-reduced?
                  ;; Box was reduced by monotonicity - re-evaluate and continue
                  (let* ([f-reduced (f-interval reduced-box)]
                         [mid-reduced (box-midpoint reduced-box)]
                         [f-mid-reduced (f-scalar mid-reduced)]
                         [new-best (min best-upper f-mid-reduced (interval-hi f-reduced))]
                         [item-reduced (make-work-item reduced-box f-reduced)]
                         [new-heap (heap-insert-by work-item-cmp item-reduced rest-heap)])
                    (bb-loop-monotonic f-interval grad-fn f-scalar new-heap new-best candidates (+ iter 1)
                                       width-tol max-iter gap-tol))
                  ;; No reduction possible - bisect as usual
                  (let* ([pair (bisect-box box)]
                         [box1 (car pair)]
                         [box2 (cdr pair)]
                         [f1 (f-interval box1)]
                         [f2 (f-interval box2)]
                         [mid1 (box-midpoint box1)]
                         [mid2 (box-midpoint box2)]
                         [f-mid1 (f-scalar mid1)]
                         [f-mid2 (f-scalar mid2)]
                         [new-best (min best-upper f-mid1 f-mid2
                                        (interval-hi f1) (interval-hi f2))]
                         [item1 (make-work-item box1 f1)]
                         [item2 (make-work-item box2 f2)]
                         [new-heap (heap-insert-by work-item-cmp item1
                                     (heap-insert-by work-item-cmp item2 rest-heap))])
                    (bb-loop-monotonic f-interval grad-fn f-scalar new-heap new-best candidates (+ iter 2)
                                       width-tol max-iter gap-tol))))])))]))

;;; maybe-reduce-box-monotonic : Box x (List Interval) -> Box
;;; Reduce box dimensions where gradient doesn't contain zero.
;;; Returns the same box if no reduction possible.
(define (maybe-reduce-box-monotonic box grad-ivs)
  (let loop ([box box] [grads grad-ivs] [new-box '()] [changed? #f])
    (cond
      [(null? box)
       (if changed?
           (reverse new-box)
           (reverse new-box))]  ; Return reduced or original
      [else
       (let* ([iv (car box)]
              [grad (car grads)]
              [lo (interval-lo grad)]
              [hi (interval-hi grad)])
         (cond
           ;; Gradient definitely positive: minimum at lower bound
           [(> lo 0)
            (loop (cdr box) (cdr grads)
                  (cons (interval-singleton (interval-lo iv)) new-box)
                  #t)]
           ;; Gradient definitely negative: minimum at upper bound
           [(< hi 0)
            (loop (cdr box) (cdr grads)
                  (cons (interval-singleton (interval-hi iv)) new-box)
                  #t)]
           ;; Mixed gradient: keep full interval
           [else
            (loop (cdr box) (cdr grads)
                  (cons iv new-box)
                  changed?)]))])))

;;; interval-find-minimum-with-gradient : (Box -> Interval) x (Box -> (List Interval)) x Box -> (List Real)
;;; Simplified interface with gradient-based monotonicity pruning.
(define (interval-find-minimum-with-gradient f-interval grad-fn box)
  (doc 'export #t)
  (let ([result (interval-minimize-with-gradient f-interval grad-fn box *default-interval-convergence*)])
    (or (ior-best-point result)
        (box-midpoint box))))

;;; ============================================================================
;;; Load message
;;; ============================================================================

(display "Use (interval-minimize f-interval box criteria [f-scalar]) for global minimization.\n")
(display "Use (interval-minimize-with-gradient f-interval grad-fn box criteria) for monotonicity pruning.\n")
