;;; spectral-pde.sexp — Development tasks for lattice/numeric/spectral-pde.ss
;;;
;;; Module: spectral-pde (tier 0, depends on prelude, vec, matrix, complex, dft)
;;; 525 lines, ~15 exported functions
;;; Spectral methods for PDEs: Fourier spectral differentiation (periodic),
;;; Chebyshev spectral differentiation (non-periodic), pseudospectral
;;; collocation for boundary value problems. Exponential integrators
;;; for heat and advection equations.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/numeric/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement periodic-grid
  ;; ──────────────────────────────────────────────
  ;; Uniform grid for periodic problems.

  (task
    (id "numeric/spectral-pde/periodic-grid-impl")
    (module spectral-pde)
    (tier 0)
    (type implement)
    (description "Implement periodic-grid: create a uniform grid of n points on the interval [0, L) for periodic problems. The spacing is h = L/n and the points are x_i = i*h for i = 0, 1, ..., n-1. Note the interval is half-open: x=L is excluded because it would duplicate x=0 under periodic boundary conditions. Allocate a vector of n zeros, then fill with a do loop.")
    (context "Available: make-vector, vector-set!, /, *, do, =. One allocation + one loop.")
    (before "(define (periodic-grid n L)
  ;; TODO: uniform n points on [0, L)
  (make-vector n 0.0))")
    (test-file "lattice/numeric/test-spectral-pde.ss")
    (test-forms (";; 4 points on [0, 8): spacing = 2
(assert-equal '#(0 2.0 4.0 6.0) (periodic-grid 4 8.0))"
                 ";; 3 points on [0, 6): spacing = 2
(let ([g (periodic-grid 3 6.0)])
  (assert-equal 0 (vector-ref g 0))
  (assert-equal 2.0 (vector-ref g 1))
  (assert-equal 4.0 (vector-ref g 2)))"
                 ";; 1 point on [0, 1): just origin
(assert-equal '#(0) (periodic-grid 1 1.0))"))
    (available-modules (prelude vec matrix complex dft))
    (hints ("Spacing: (let ([h (/ L n)]) ...)"
            "Allocate: (let ([grid (make-vector n 0.0)]) ...)"
            "Fill: (do ([i 0 (+ i 1)]) ((= i n) grid) (vector-set! grid i (* i h)))"))
    (reference-solution "(define (periodic-grid n L)
  (let* ([h (/ L n)]
         [grid (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) grid)
      (vector-set! grid i (* i h)))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement sample-function
  ;; ──────────────────────────────────────────────
  ;; Evaluate function at grid points.

  (task
    (id "numeric/spectral-pde/sample-function-impl")
    (module spectral-pde)
    (tier 0)
    (type implement)
    (description "Implement sample-function: evaluate a scalar function f at each point in a grid vector, returning a new vector of function values. This discretizes a continuous function onto a computational grid — the first step in any numerical PDE method. Allocate a result vector, loop over grid indices, and set each element to f applied to the corresponding grid point.")
    (context "Available: vector-length, make-vector, vector-ref, vector-set!, do, =. One allocation + one loop.")
    (before "(define (sample-function f grid)
  ;; TODO: f evaluated at each grid point
  (make-vector (vector-length grid) 0.0))")
    (test-file "lattice/numeric/test-spectral-pde.ss")
    (test-forms (";; Square function on grid
(let ([values (sample-function (lambda (x) (* x x)) '#(0 1 2 3))])
  (assert-equal '#(0 1 4 9) values))"
                 ";; Identity function
(let ([values (sample-function (lambda (x) x) '#(1.0 2.0 3.0))])
  (assert-equal '#(1.0 2.0 3.0) values))"
                 ";; Constant function
(let ([values (sample-function (lambda (x) 42) '#(0 5 10))])
  (assert-equal '#(42 42 42) values))"))
    (available-modules (prelude vec matrix complex dft))
    (hints ("Get size: (let ([n (vector-length grid)]) ...)"
            "Allocate: (let ([values (make-vector n 0.0)]) ...)"
            "Fill: (do ([i 0 (+ i 1)]) ((= i n) values) (vector-set! values i (f (vector-ref grid i))))"))
    (reference-solution "(define (sample-function f grid)
  (let* ([n (vector-length grid)]
         [values (make-vector n 0.0)])
    (do ([i 0 (+ i 1)])
        ((= i n) values)
      (vector-set! values i (f (vector-ref grid i))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement chebyshev-poly
  ;; ──────────────────────────────────────────────
  ;; Evaluate Chebyshev polynomial via recurrence.

  (task
    (id "numeric/spectral-pde/chebyshev-poly-impl")
    (module spectral-pde)
    (tier 0)
    (type implement)
    (description "Implement chebyshev-poly: evaluate the Chebyshev polynomial T_n(x) using the three-term recurrence relation. T₀(x) = 1, T₁(x) = x, and T_{k+1}(x) = 2x·T_k(x) - T_{k-1}(x). Chebyshev polynomials are the basis functions for spectral methods on non-periodic domains. They can also be defined as T_n(x) = cos(n·arccos(x)) for x ∈ [-1,1], but the recurrence is more numerically stable. Handle n=0 and n=1 as base cases, then iterate the recurrence.")
    (context "Available: =, *, -, >, +, cond, let. Named let loop with two rolling values.")
    (before "(define (chebyshev-poly n x)
  ;; TODO: T_n(x) via recurrence
  0)")
    (test-file "lattice/numeric/test-spectral-pde.ss")
    (test-forms (";; T_0(x) = 1 for all x
(assert-equal 1.0 (chebyshev-poly 0 0.5))"
                 ";; T_1(x) = x
(assert-equal 0.5 (chebyshev-poly 1 0.5))"
                 ";; T_2(x) = 2x²-1: T_2(0.5) = 2*0.25-1 = -0.5
(assert-equal -0.5 (chebyshev-poly 2 0.5))"
                 ";; T_3(x) = 4x³-3x: T_3(0.5) = 4*0.125-1.5 = -1.0
(assert-equal -1.0 (chebyshev-poly 3 0.5))"
                 ";; T_n(1) = 1 for all n
(assert-equal 1.0 (chebyshev-poly 5 1.0))"))
    (available-modules (prelude vec matrix complex dft))
    (hints ("Base cases: n=0 → 1.0, n=1 → x"
            "Named let: (let loop ([k 2] [t-prev 1.0] [t-curr x]) ...)"
            "Recurrence: t-next = 2*x*t-curr - t-prev"
            "Loop until k > n, then return t-curr"))
    (reference-solution "(define (chebyshev-poly n x)
  (cond
    [(= n 0) 1.0]
    [(= n 1) x]
    [else
     (let loop ([k 2] [t-prev 1.0] [t-curr x])
       (if (> k n)
           t-curr
           (let ([t-next (- (* 2.0 x t-curr) t-prev)])
             (loop (+ k 1) t-curr t-next))))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement chebyshev-nodes
  ;; ──────────────────────────────────────────────
  ;; Gauss-Lobatto collocation points.

  (task
    (id "numeric/spectral-pde/chebyshev-nodes-impl")
    (module spectral-pde)
    (tier 0)
    (type implement)
    (description "Implement chebyshev-nodes: compute the Chebyshev-Gauss-Lobatto nodes x_j = cos(πj/N) for j = 0, 1, ..., N. This returns N+1 points on [-1, 1] including both endpoints. These nodes cluster near the boundaries (±1), which prevents Runge's phenomenon in polynomial interpolation. They are the optimal collocation points for spectral methods on non-periodic domains. Use (pi-value) to get π.")
    (context "Available: make-vector, vector-set!, cos, /, *, pi-value, do, >. One allocation + one loop.")
    (before "(define (chebyshev-nodes n)
  ;; TODO: cos(πj/n) for j = 0, ..., n
  (make-vector (+ n 1) 0.0))")
    (test-file "lattice/numeric/test-spectral-pde.ss")
    (test-forms (";; N=1: two endpoints cos(0)=1, cos(π)=-1
(let ([nodes (chebyshev-nodes 1)])
  (assert-equal 2 (vector-length nodes))
  (assert-true (< (abs (- (vector-ref nodes 0) 1.0)) 1e-10))
  (assert-true (< (abs (- (vector-ref nodes 1) -1.0)) 1e-10)))"
                 ";; N=2: cos(0)=1, cos(π/2)=0, cos(π)=-1
(let ([nodes (chebyshev-nodes 2)])
  (assert-equal 3 (vector-length nodes))
  (assert-true (< (abs (- (vector-ref nodes 0) 1.0)) 1e-10))
  (assert-true (< (abs (vector-ref nodes 1)) 1e-10))
  (assert-true (< (abs (- (vector-ref nodes 2) -1.0)) 1e-10)))"
                 ";; Always N+1 points
(assert-equal 6 (vector-length (chebyshev-nodes 5)))"))
    (available-modules (prelude vec matrix complex dft))
    (hints ("Size: (+ n 1) points"
            "Allocate: (let ([nodes (make-vector (+ n 1) 0.0)] [pi-val (pi-value)]) ...)"
            "Fill: (do ([j 0 (+ j 1)]) ((> j n) nodes) (vector-set! nodes j (cos (/ (* pi-val j) n))))"))
    (reference-solution "(define (chebyshev-nodes n)
  (let* ([nodes (make-vector (+ n 1) 0.0)]
         [pi-val (pi-value)])
    (do ([j 0 (+ j 1)])
        ((> j n) nodes)
      (vector-set! nodes j (cos (/ (* pi-val j) n))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement spectral-error-estimate
  ;; ──────────────────────────────────────────────
  ;; Max-norm error between two vectors.

  (task
    (id "numeric/spectral-pde/spectral-error-estimate-impl")
    (module spectral-pde)
    (tier 0)
    (type implement)
    (description "Implement spectral-error-estimate: compute the max-norm (L∞) error between a computed solution vector and an exact solution vector. The max-norm is max_i |computed_i - exact_i|. This is the standard error metric for spectral methods — it measures the worst-case pointwise error. Loop over all indices, tracking the maximum absolute difference seen so far. Use when to update the running maximum.")
    (context "Available: vector-length, vector-ref, abs, -, >, when, set!, do, let. One loop tracking max.")
    (before "(define (spectral-error-estimate computed exact)
  ;; TODO: max|computed_i - exact_i|
  0)")
    (test-file "lattice/numeric/test-spectral-pde.ss")
    (test-forms (";; Simple error
(assert-equal 0.2 (spectral-error-estimate '#(1.0 2.0 3.0) '#(1.1 1.8 3.0)))"
                 ";; Zero error (exact match)
(assert-equal 0 (spectral-error-estimate '#(1 2 3) '#(1 2 3)))"
                 ";; Single element
(assert-equal 5 (spectral-error-estimate '#(10) '#(5)))"))
    (available-modules (prelude vec matrix complex dft))
    (hints ("Track: (let ([n (vector-length computed)] [max-err 0.0]) ...)"
            "Loop: (do ([i 0 (+ i 1)]) ((= i n) max-err) ...)"
            "Each step: compute err = (abs (- computed_i exact_i)), update max-err if larger"))
    (reference-solution "(define (spectral-error-estimate computed exact)
  (let ([n (vector-length computed)]
        [max-err 0.0])
    (do ([i 0 (+ i 1)])
        ((= i n) max-err)
      (let ([err (abs (- (vector-ref computed i) (vector-ref exact i)))])
        (when (> err max-err)
          (set! max-err err))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
