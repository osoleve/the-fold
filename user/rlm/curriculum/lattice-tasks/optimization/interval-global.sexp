;;; interval-global.sexp — Development tasks for lattice/optimization/interval-global.ss
;;;
;;; Module: interval-global (tier 0, depends on prelude, interval, heap)
;;; 576 lines, ~15 exported functions
;;; Interval branch-and-bound global optimization. Guaranteed enclosure
;;; of global minima via interval arithmetic. Best-first search with
;;; bisection, pruning by lower/upper bounds, and optional monotonicity
;;; pruning via interval gradients.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/optimization/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement box-midpoint
  ;; ──────────────────────────────────────────────
  ;; Midpoint of an n-dimensional box.

  (task
    (id "optimization/interval-global/box-midpoint-impl")
    (module interval-global)
    (tier 0)
    (type implement)
    (description "Implement box-midpoint: compute the midpoint of each dimension of a box. A box is a list of intervals, one per dimension. The midpoint is a list of the midpoints of each interval. For example, a 2D box [(1,3), (2,6)] has midpoint (2, 4). This is used to evaluate the objective function at the center of a search region for tighter upper bound estimates. Use map with interval-mid.")
    (context "Available: map, interval-mid. Single expression.")
    (before "(define (box-midpoint box)
  ;; TODO: midpoint of each dimension
  '())")
    (test-file "lattice/optimization/test-interval-global.ss")
    (test-forms (";; 2D box
(assert-equal '(2 4) (box-midpoint (list (interval 1 3) (interval 2 6))))"
                 ";; 1D box
(assert-equal '(5) (box-midpoint (list (interval 0 10))))"
                 ";; Point box (zero width)
(assert-equal '(3 7) (box-midpoint (list (interval 3 3) (interval 7 7))))"))
    (available-modules (prelude interval heap))
    (hints ("Map interval-mid over each dimension"
            "(map interval-mid box)"))
    (reference-solution "(define (box-midpoint box)
  (map interval-mid box))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement box-widest-dimension
  ;; ──────────────────────────────────────────────
  ;; Find the index of the widest dimension.

  (task
    (id "optimization/interval-global/box-widest-dimension-impl")
    (module interval-global)
    (tier 0)
    (type implement)
    (description "Implement box-widest-dimension: find the index of the widest dimension in a box. A box is a list of intervals. The widest dimension is the one with the largest interval-width. This determines where to bisect the box in the branch-and-bound algorithm — always splitting along the widest dimension gives the most balanced partition. Use a named let loop tracking the current index, best index, and best width seen so far.")
    (context "Available: null?, >, car, cdr, interval-width, let. Named let with four accumulators (remaining, idx, max-idx, max-width).")
    (before "(define (box-widest-dimension box)
  ;; TODO: index of dimension with largest width
  0)")
    (test-file "lattice/optimization/test-interval-global.ss")
    (test-forms (";; Second dimension is widest
(assert-equal 1 (box-widest-dimension (list (interval 0 2) (interval 0 10) (interval 0 5))))"
                 ";; First dimension is widest
(assert-equal 0 (box-widest-dimension (list (interval 0 100) (interval 0 1))))"
                 ";; Single dimension
(assert-equal 0 (box-widest-dimension (list (interval 0 5))))"
                 ";; Equal widths: returns first
(assert-equal 0 (box-widest-dimension (list (interval 0 5) (interval 0 5))))"))
    (available-modules (prelude interval heap))
    (hints ("Named let: (let loop ([ivs box] [idx 0] [max-idx 0] [max-width 0]) ...)"
            "Base: (null? ivs) → max-idx"
            "Step: compute width of (car ivs), update max if wider"))
    (reference-solution "(define (box-widest-dimension box)
  (let loop ([ivs box] [idx 0] [max-idx 0] [max-width 0])
    (if (null? ivs)
        max-idx
        (let ([w (interval-width (car ivs))])
          (if (> w max-width)
              (loop (cdr ivs) (+ idx 1) idx w)
              (loop (cdr ivs) (+ idx 1) max-idx max-width))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement list-update
  ;; ──────────────────────────────────────────────
  ;; Functional update of a list element at index.

  (task
    (id "optimization/interval-global/list-update-impl")
    (module interval-global)
    (tier 0)
    (type implement)
    (description "Implement list-update: return a new list with the element at position idx replaced by val. This is a functional (non-destructive) operation — the original list is not modified. When idx is 0, replace car with val and keep cdr. Otherwise, keep car and recurse on cdr with idx-1. This is used by bisect-box to replace a single dimension's interval when splitting a box.")
    (context "Available: =, cons, car, cdr, -, if. Simple recursion on index.")
    (before "(define (list-update lst idx val)
  ;; TODO: replace element at idx with val
  lst)")
    (test-file "lattice/optimization/test-interval-global.ss")
    (test-forms (";; Replace at index 2
(assert-equal '(a b X d) (list-update '(a b c d) 2 'X))"
                 ";; Replace at index 0
(assert-equal '(Z b c) (list-update '(a b c) 0 'Z))"
                 ";; Replace last element
(assert-equal '(1 2 99) (list-update '(1 2 3) 2 99))"
                 ";; Single element list
(assert-equal '(new) (list-update '(old) 0 'new))"))
    (available-modules (prelude interval heap))
    (hints ("Base: idx = 0 → (cons val (cdr lst))"
            "Step: (cons (car lst) (list-update (cdr lst) (- idx 1) val))"))
    (reference-solution "(define (list-update lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) val))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement point->box
  ;; ──────────────────────────────────────────────
  ;; Convert a point to a degenerate box.

  (task
    (id "optimization/interval-global/point-to-box-impl")
    (module interval-global)
    (tier 0)
    (type implement)
    (description "Implement point->box: convert a point (list of real numbers) to a box of singleton intervals. Each coordinate x becomes the interval [x, x]. This is used to evaluate an interval function at a specific point, leveraging the inclusion property: f([x,x]) = [f(x), f(x)]. The result is a list of intervals, one per coordinate. Use map with interval-singleton.")
    (context "Available: map, interval-singleton. Single expression.")
    (before "(define (point->box point)
  ;; TODO: each coordinate becomes [x, x]
  '())")
    (test-file "lattice/optimization/test-interval-global.ss")
    (test-forms (";; 2D point
(let ([box (point->box '(3 5))])
  (assert-equal 3 (interval-lo (car box)))
  (assert-equal 3 (interval-hi (car box)))
  (assert-equal 5 (interval-lo (cadr box)))
  (assert-equal 5 (interval-hi (cadr box))))"
                 ";; 1D point
(let ([box (point->box '(7))])
  (assert-equal 1 (length box))
  (assert-equal 7 (interval-lo (car box))))"
                 ";; 3D point
(assert-equal 3 (length (point->box '(1 2 3))))"))
    (available-modules (prelude interval heap))
    (hints ("Map interval-singleton over each coordinate"
            "(map interval-singleton point)"))
    (reference-solution "(define (point->box point)
  (map interval-singleton point))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement box-max-width
  ;; ──────────────────────────────────────────────
  ;; Width of the widest dimension.

  (task
    (id "optimization/interval-global/box-max-width-impl")
    (module interval-global)
    (tier 0)
    (type implement)
    (description "Implement box-max-width: compute the width of the widest dimension in a box. A box is a list of intervals. Compute the width of each interval using interval-width, then take the maximum. This is the convergence criterion: when the widest dimension is below the tolerance, the box is considered converged. Handle the empty box case by returning 0. Use fold-left with max over the mapped widths.")
    (context "Available: null?, fold-left, max, map, interval-width, if. Guard + fold pattern.")
    (before "(define (box-max-width box)
  ;; TODO: maximum width across all dimensions
  0)")
    (test-file "lattice/optimization/test-interval-global.ss")
    (test-forms (";; 3D box, widest dimension has width 8
(assert-equal 8 (box-max-width (list (interval 1 3) (interval 2 10) (interval 0 5))))"
                 ";; Point box: width 0
(assert-equal 0 (box-max-width (list (interval 3 3) (interval 7 7))))"
                 ";; Empty box
(assert-equal 0 (box-max-width '()))"
                 ";; 1D box
(assert-equal 10 (box-max-width (list (interval 0 10))))"))
    (available-modules (prelude interval heap))
    (hints ("Guard empty: (if (null? box) 0 ...)"
            "Compute widths: (map interval-width box)"
            "Take max: (fold-left max 0 widths)"))
    (reference-solution "(define (box-max-width box)
  (if (null? box)
      0
      (fold-left max 0 (map interval-width box))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
