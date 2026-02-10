(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'interval)
(require 'interval-global)

(doc 'module 'interval-newton)
(doc 'description "Interval Newton method for guaranteed root enclosure and existence proofs")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'author "Claude Opus 4.5")
(doc 'created "2026-01-30")

(doc 'section 'overview)
(doc 'note "Guaranteed root finding using interval arithmetic.

Unlike point-based Newton (which can diverge or miss roots), interval Newton
provides rigorous guarantees:

  1. If N(X) ⊂ X, there is EXACTLY ONE root in X (existence + uniqueness)
  2. If N(X) ∩ X = ∅, there are NO roots in X (non-existence proof)
  3. The returned interval is guaranteed to contain all roots

The interval Newton operator is:
  N(X) = m - f(m) / F'(X)

where:
  - m is the midpoint of X
  - f(m) is the function value at the midpoint
  - F'(X) is the interval extension of the derivative over X

Key insight: Division by an interval containing 0 produces extended intervals
(representing 'entire real line'), which we handle by bisection.")

;;; ============================================================================
;;; Root Finding Result
;;; ============================================================================

(doc 'section 'result-types)

;;; make-root-result : (List RootInfo) × Nat × Symbol → RootResult
;;; Create result structure for root finding.
;;;   roots: list of (interval . status) where status is 'unique, 'exists, or 'possible
;;;   iterations: number of iterations performed
;;;   reason: why terminated ('converged, 'max-iterations, 'no-roots)
(define (make-root-result roots iterations reason)
  (doc 'export #t)
  (list 'root-result roots iterations reason))

;;; root-result? : Any → Boolean
(define (root-result? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'root-result)))

;;; root-result-roots : RootResult → (List RootInfo)
(define (root-result-roots r)
  (doc 'export #t)
  (list-ref r 1))

;;; root-result-iterations : RootResult → Nat
(define (root-result-iterations r)
  (doc 'export #t)
  (list-ref r 2))

;;; root-result-reason : RootResult → Symbol
(define (root-result-reason r)
  (doc 'export #t)
  (list-ref r 3))

;;; root-result-count : RootResult → Nat
;;; Number of roots found.
(define (root-result-count r)
  (doc 'export #t)
  (length (root-result-roots r)))

;;; root-result-unique-roots : RootResult → (List Interval)
;;; Get only the roots proven to be unique.
(define (root-result-unique-roots r)
  (doc 'export #t)
  (map car (filter (lambda (ri) (eq? (cdr ri) 'unique))
                   (root-result-roots r))))

;;; ============================================================================
;;; Core Interval Newton Operations
;;; ============================================================================

(doc 'section 'newton-operator)

;;; interval-newton-step : (Real → Real) × (Interval → Interval) × Interval → NewtonStepResult
;;; Compute one interval Newton step (APPROXIMATE - not rigorous).
;;;
;;; WARNING: This function uses standard floating-point arithmetic and point
;;; evaluation for f(m). Results are heuristic, NOT mathematically guaranteed.
;;; For formal proofs of root existence/uniqueness, use interval-newton-step-rigorous.
;;;
;;;   f: the function (point evaluation)
;;;   f-deriv-iv: interval extension of derivative
;;;   X: current interval
;;;
;;; Returns one of:
;;;   ('contracted . new-interval)  - Newton produced a tighter interval
;;;   ('unique . new-interval)      - N(X) ⊂ X suggests unique root (not proven)
;;;   ('no-root . #f)               - N(X) ∩ X = ∅ suggests no root (not proven)
;;;   ('division-by-zero . X)       - F'(X) contains 0, need to bisect
;;;   ('no-improvement . X)         - No contraction achieved
(define (interval-newton-step f f-deriv-iv X)
  (doc 'export #t)
  (doc 'type '(-> (-> Real Real) (-> Interval Interval) Interval NewtonStepResult))
  (let* ([m (interval-mid X)]
         [fm (f m)]
         [F-prime-X (f-deriv-iv X)])
    (cond
      ;; Check if derivative interval contains zero
      [(interval-contains-zero? F-prime-X)
       (cons 'division-by-zero X)]
      [else
       ;; N(X) = m - f(m) / F'(X)
       (let* ([fm-iv (interval-singleton fm)]
              [quotient (interval-div fm-iv F-prime-X)])
         (if (error? quotient)
             (cons 'division-by-zero X)
             (let* ([newton-iv (interval-sub (interval-singleton m) quotient)]
                    ;; Intersect with original interval
                    [intersection (interval-intersection newton-iv X)])
               (cond
                 ;; No intersection means no root
                 [(error? intersection)
                  (cons 'no-root #f)]
                 ;; N(X) ⊂ X proves unique root
                 [(interval-subset? newton-iv X)
                  (cons 'unique intersection)]
                 ;; Check if we achieved contraction
                 [(< (interval-width intersection) (interval-width X))
                  (cons 'contracted intersection)]
                 [else
                  (cons 'no-improvement X)]))))])))

;;; interval-newton-step-rigorous : (Interval → Interval) × (Interval → Interval) × Interval → NewtonStepResult
;;; Interval Newton step using rigorous arithmetic for formal correctness.
;;; Uses interval extension f-iv for midpoint evaluation to eliminate rounding errors.
;;;
;;; The Newton step N(X) = m - f(m)/F'(X) is computed as:
;;;   m = midpoint of X
;;;   f(m) ≈ f-iv(singleton(m))  -- guaranteed to contain true f(m)
;;;   F'(X) = f-deriv-iv(X)      -- guaranteed to contain all derivatives
;;;
;;; All arithmetic uses directed rounding for rigorous enclosure.
(define (interval-newton-step-rigorous f-iv f-deriv-iv X)
  (doc 'export #t)
  (doc 'type '(-> (-> Interval Interval) (-> Interval Interval) Interval NewtonStepResult))
  (let* ([m (interval-mid X)]
         [m-iv (interval-singleton m)]
         ;; Use interval extension for f(m) - eliminates rounding errors
         [fm-iv (f-iv m-iv)]
         [F-prime-X (f-deriv-iv X)])
    (cond
      [(interval-contains-zero? F-prime-X)
       (cons 'division-by-zero X)]
      [else
       (let ([quotient (interval-div-rigorous fm-iv F-prime-X)])
         (if (error? quotient)
             (cons 'division-by-zero X)
             (let* ([newton-iv (interval-sub-rigorous m-iv quotient)]
                    [intersection (interval-intersection newton-iv X)])
               (cond
                 [(error? intersection) (cons 'no-root #f)]
                 [(interval-subset? newton-iv X) (cons 'unique intersection)]
                 [(< (interval-width intersection) (interval-width X))
                  (cons 'contracted intersection)]
                 [else (cons 'no-improvement X)]))))])))

;;; ============================================================================
;;; Iterative Root Finding
;;; ============================================================================

(doc 'section 'iterative-newton)

;;; interval-newton-root : (Interval → Interval) × (Interval → Interval) × Interval × Real × Nat → NewtonResult
;;; Find a root in interval using interval Newton iteration.
;;;   f-iv: interval extension of function
;;;   f-deriv-iv: interval extension of derivative
;;;   X0: initial interval (must contain a root)
;;;   tol: width tolerance for convergence
;;;   max-iter: maximum iterations
;;;
;;; Returns: ('found interval status iter) or ('failed reason iter)
(define (interval-newton-root f-iv f-deriv-iv X0 tol max-iter)
  (doc 'export #t)
  (doc 'type '(-> (-> Interval Interval) (-> Interval Interval) Interval Real Nat NewtonResult))
  (let loop ([X X0] [iter 0] [status 'possible])
    (cond
      ;; Max iterations
      [(>= iter max-iter)
       (list 'found X status iter)]
      ;; Converged by width
      [(< (interval-width X) tol)
       (list 'found X status iter)]
      [else
       ;; Use rigorous step for guaranteed enclosure with directed rounding
       (let ([step-result (interval-newton-step-rigorous f-iv f-deriv-iv X)])
         (case (car step-result)
           [(no-root)
            (list 'failed 'no-root iter)]
           [(unique)
            ;; Continue iterating to tighten, but we know it's unique
            (loop (cdr step-result) (+ iter 1) 'unique)]
           [(contracted)
            (loop (cdr step-result) (+ iter 1) status)]
           [(division-by-zero no-improvement)
            ;; Bisect and try both halves
            (let* ([pair (interval-bisect X)]
                   [left (car pair)]
                   [right (cdr pair)]
                   ;; Try left half
                   [left-result (interval-newton-root f-iv f-deriv-iv left tol (- max-iter iter))])
              (if (eq? (car left-result) 'found)
                  left-result
                  ;; Try right half
                  (interval-newton-root f-iv f-deriv-iv right tol (- max-iter iter))))]
           [else
            (list 'failed 'unknown-error iter)]))])))

;;; ============================================================================
;;; All Roots Finding (Branch and Bound)
;;; ============================================================================

(doc 'section 'all-roots)

;;; interval-find-all-roots : (Interval → Interval) × (Interval → Interval) × Interval × Real × Nat → RootResult
;;; Find ALL roots in an interval using branch-and-bound with interval Newton.
;;;   f-iv: interval extension of function
;;;   f-deriv-iv: interval extension of derivative
;;;   X0: search interval
;;;   tol: width tolerance
;;;   max-iter: maximum total iterations
;;;
;;; Uses work queue with interval Newton to systematically find all roots.
;;; Root exclusion uses rigorous interval arithmetic: if f-iv(X) does not
;;; contain zero, there provably cannot be roots in X.
;;;
;;; All operations use interval extensions for formal mathematical guarantees.
;;; Point evaluation f(m) is computed as f-iv(singleton(m)).
(define (interval-find-all-roots f-iv f-deriv-iv X0 tol max-iter)
  (doc 'export #t)
  (doc 'type '(-> (-> Interval Interval) (-> Interval Interval) Interval Real Nat RootResult))
  (let loop ([work-list (list X0)]
             [roots '()]
             [iter 0])
    (cond
      ;; Done: no more work
      [(null? work-list)
       (make-root-result roots iter 'converged)]
      ;; Max iterations
      [(>= iter max-iter)
       ;; Return what we have, marking remaining work as 'possible
       (let ([remaining-roots (map (lambda (iv) (cons iv 'possible)) work-list)])
         (make-root-result (append roots remaining-roots) iter 'max-iterations))]
      [else
       (let* ([X (car work-list)]
              [rest (cdr work-list)])
         ;; Rigorous check: if f-iv(X) doesn't contain 0, provably no roots
         (if (not (interval-contains-zero? (f-iv X)))
             ;; No root possible in this interval (proven by interval arithmetic)
             (loop rest roots (+ iter 1))
             ;; Might contain root - apply rigorous Newton step
             (let ([step-result (interval-newton-step-rigorous f-iv f-deriv-iv X)])
               (case (car step-result)
                 [(no-root)
                  ;; Proven: no root here
                  (loop rest roots (+ iter 1))]
                 [(unique)
                  ;; Proven unique root - add to results
                  (let ([refined (cdr step-result)])
                    (if (< (interval-width refined) tol)
                        ;; Converged
                        (loop rest (cons (cons refined 'unique) roots) (+ iter 1))
                        ;; Continue refining
                        (loop (cons refined rest) roots (+ iter 1))))]
                 [(contracted)
                  (let ([refined (cdr step-result)])
                    (if (< (interval-width refined) tol)
                        ;; Converged but not proven unique
                        (loop rest (cons (cons refined 'exists) roots) (+ iter 1))
                        ;; Continue refining
                        (loop (cons refined rest) roots (+ iter 1))))]
                 [(division-by-zero no-improvement)
                  ;; Need to bisect
                  (if (< (interval-width X) tol)
                      ;; Already at tolerance, add as possible root
                      (loop rest (cons (cons X 'possible) roots) (+ iter 1))
                      ;; Bisect and add both halves to work list
                      (let* ([pair (interval-bisect X)]
                             [left (car pair)]
                             [right (cdr pair)])
                        (loop (cons left (cons right rest)) roots (+ iter 1))))]
                 [else
                  (loop rest roots (+ iter 1))]))))])))

;;; interval-might-contain-root? : (Real → Real) × Interval → Boolean
;;; DEPRECATED: Use interval-contains-zero? with interval extension instead.
;;;
;;; This heuristic evaluates f at endpoints and midpoint. If all same sign,
;;; it assumes no root exists. This is UNSOUND: a function can be positive
;;; at all sample points while having roots between them (e.g., (x-0.5)²-0.001
;;; on [0,1] with unfortunate midpoint).
;;;
;;; For rigorous root finding, use: (interval-contains-zero? (f-iv X))
;;; where f-iv is the interval extension of f.
(define (interval-might-contain-root? f X)
  (let* ([lo (interval-lo X)]
         [hi (interval-hi X)]
         [mid (interval-mid X)]
         [f-lo (f lo)]
         [f-hi (f hi)]
         [f-mid (f mid)])
    ;; Root possible if signs differ or any value is near zero
    (or (< (* f-lo f-hi) 0)
        (< (* f-lo f-mid) 0)
        (< (* f-mid f-hi) 0)
        (< (abs f-lo) 1e-10)
        (< (abs f-hi) 1e-10)
        (< (abs f-mid) 1e-10))))

;;; ============================================================================
;;; Simple Bisection Root Finding
;;; ============================================================================

(doc 'section 'bisection)

;;; interval-bisection-root : (Real → Real) × Interval × Real × Nat → BisectionResult
;;; Classical bisection method for root finding.
;;; Requires f(lo) and f(hi) to have opposite signs.
;;;
;;; Returns: ('found interval iter) or ('failed reason iter)
(define (interval-bisection-root f X0 tol max-iter)
  (doc 'export #t)
  (doc 'type '(-> (-> Real Real) Interval Real Nat BisectionResult))
  (let ([lo (interval-lo X0)]
        [hi (interval-hi X0)])
    (let ([f-lo (f lo)]
          [f-hi (f hi)])
      (cond
        ;; Check for sign change
        [(> (* f-lo f-hi) 0)
         (list 'failed 'no-sign-change 0)]
        [else
         (bisection-loop f lo hi f-lo f-hi tol max-iter 0)]))))

;;; bisection-loop : (Real → Real) × Real × Real × Real × Real × Real × Nat × Nat → BisectionResult
(define (bisection-loop f lo hi f-lo f-hi tol max-iter iter)
  (cond
    [(>= iter max-iter)
     (list 'found (interval lo hi) iter)]
    [(< (- hi lo) tol)
     (list 'found (interval lo hi) iter)]
    [else
     (let* ([mid (/ (+ lo hi) 2)]
            [f-mid (f mid)])
       (cond
         ;; Exact root found
         [(= f-mid 0)
          (list 'found (interval-singleton mid) iter)]
         ;; Root in left half
         [(< (* f-lo f-mid) 0)
          (bisection-loop f lo mid f-lo f-mid tol max-iter (+ iter 1))]
         ;; Root in right half
         [else
          (bisection-loop f mid hi f-mid f-hi tol max-iter (+ iter 1))]))]))

;;; ============================================================================
;;; Existence and Uniqueness Tests
;;; ============================================================================

(doc 'section 'existence-tests)

;;; interval-contains-unique-root? : (Interval → Interval) × (Interval → Interval) × Interval → Boolean
;;; Test if interval is guaranteed to contain exactly one root.
;;; Uses the rigorous interval Newton containment test: N(X) ⊂ X implies unique root.
;;; All arithmetic uses directed rounding for formal mathematical guarantees.
(define (interval-contains-unique-root? f-iv f-deriv-iv X)
  (doc 'export #t)
  (doc 'type '(-> (-> Interval Interval) (-> Interval Interval) Interval Boolean))
  (let ([step-result (interval-newton-step-rigorous f-iv f-deriv-iv X)])
    (eq? (car step-result) 'unique)))

;;; interval-contains-no-root? : (Interval → Interval) × (Interval → Interval) × Interval → Boolean
;;; Test if interval is guaranteed to contain no roots.
;;; True if N(X) ∩ X = ∅, using rigorous directed-rounding arithmetic.
(define (interval-contains-no-root? f-iv f-deriv-iv X)
  (doc 'export #t)
  (doc 'type '(-> (-> Interval Interval) (-> Interval Interval) Interval Boolean))
  (let ([step-result (interval-newton-step-rigorous f-iv f-deriv-iv X)])
    (eq? (car step-result) 'no-root)))

;;; ============================================================================
;;; Krawczyk Operator (Alternative to Newton)
;;; ============================================================================

(doc 'section 'krawczyk)
(doc 'note "The Krawczyk operator is an alternative to interval Newton that
is sometimes more robust. It uses: K(X) = m - Y*f(m) + (I - Y*F'(X))*(X - m)
where Y is an approximate inverse of f'(m).

For 1D: K(X) = m - f(m)/f'(m) + (1 - F'(X)/f'(m))*(X - m)

If K(X) ⊂ X, there's a unique root in X.")

;;; krawczyk-step : (Real → Real) × (Real → Real) × (Interval → Interval) × Interval → KrawczykResult
;;; Compute one Krawczyk step (APPROXIMATE - not rigorous).
;;;
;;; WARNING: This function uses point evaluation for f(m) and f'(m), and standard
;;; floating-point arithmetic. Results are heuristic, not formally guaranteed.
;;; A rigorous version would require interval extensions for f and directed rounding.
;;;
;;;   f: the function (point evaluation)
;;;   f-deriv: point derivative
;;;   f-deriv-iv: interval derivative
;;;   X: current interval
(define (krawczyk-step f f-deriv f-deriv-iv X)
  (doc 'export #t)
  (let* ([m (interval-mid X)]
         [fm (f m)]
         [f-prime-m (f-deriv m)]
         [F-prime-X (f-deriv-iv X)])
    (cond
      ;; Check for zero derivative at midpoint
      [(< (abs f-prime-m) 1e-15)
       (cons 'degenerate X)]
      [else
       (let* ([Y (/ 1.0 f-prime-m)]  ; Approximate inverse
              ;; First term: m - Y*f(m) = m - f(m)/f'(m)
              [term1 (- m (* Y fm))]
              ;; Second term: (1 - Y*F'(X))*(X - m)
              ;; = (1 - F'(X)/f'(m)) * (X - m)
              [ratio (interval-scale F-prime-X Y)]
              [one-minus-ratio (interval-sub (interval-singleton 1) ratio)]
              [X-minus-m (interval-sub X (interval-singleton m))]
              [term2 (interval-mul one-minus-ratio X-minus-m)]
              ;; K(X) = term1 + term2
              [K-X (interval-add (interval-singleton term1) term2)]
              ;; Intersect with X
              [intersection (interval-intersection K-X X)])
         (cond
           [(error? intersection)
            (cons 'no-root #f)]
           [(interval-subset? K-X X)
            (cons 'unique intersection)]
           [(< (interval-width intersection) (interval-width X))
            (cons 'contracted intersection)]
           [else
            (cons 'no-improvement X)]))])))

;;; ============================================================================
;;; Multi-dimensional Interval Newton (for systems of equations)
;;; ============================================================================

(doc 'section 'multidimensional)
(doc 'todo "Implement multi-dimensional interval Newton for systems F(x) = 0.
Requires interval Jacobian matrix and interval Gauss-Seidel iteration.
See Hansen-Sengupta algorithm.")

;;; ============================================================================
;;; Convenience Functions
;;; ============================================================================

(doc 'section 'convenience)

;;; find-root : (Interval → Interval) × (Interval → Interval) × Real × Real → Interval | 'no-root
;;; Simple interface: find a root of f in [lo, hi].
;;;   f-iv: interval extension of function
;;;   f-deriv-iv: interval extension of derivative
(define (find-root f-iv f-deriv-iv lo hi)
  (doc 'export #t)
  (let* ([X (interval lo hi)]
         [result (interval-newton-root f-iv f-deriv-iv X 1e-10 1000)])
    (if (eq? (car result) 'found)
        (cadr result)
        'no-root)))

;;; find-all-roots-simple : (Interval → Interval) × (Interval → Interval) × Real × Real → (List Interval)
;;; Simple interface: find all roots of f in [lo, hi].
;;;   f-iv: interval extension of f
;;;   f-deriv-iv: interval extension of derivative
(define (find-all-roots-simple f-iv f-deriv-iv lo hi)
  (doc 'export #t)
  (let* ([X (interval lo hi)]
         [result (interval-find-all-roots f-iv f-deriv-iv X 1e-10 10000)])
    (map car (root-result-roots result))))

;;; ============================================================================
;;; Example Functions and Their Derivatives
;;; ============================================================================

(doc 'section 'examples)

;;; polynomial-root-example : Unit → RootResult
;;; Example: Find roots of x³ - x = x(x-1)(x+1)
;;; Roots are at x = -1, 0, 1
(define (polynomial-root-example)
  (doc 'export #t)
  (let ([f-iv (lambda (X)
                ;; x³ - x using interval arithmetic
                (interval-sub (interval-pow X 3) X))]
        [f-deriv-iv (lambda (X)
                      ;; d/dx(x³ - x) = 3x² - 1
                      (interval-sub (interval-scale (interval-sqr X) 3)
                                    (interval-singleton 1)))])
    (interval-find-all-roots f-iv f-deriv-iv (interval -2 2) 1e-10 1000)))

;;; sin-root-example : Unit → RootResult
;;; Example: Find roots of sin(x) in [0, 10]
;;; Roots at x = 0, π, 2π, 3π
(define (sin-root-example)
  (doc 'export #t)
  (let ([f-iv interval-sin]
        [f-deriv-iv interval-cos])
    (interval-find-all-roots f-iv f-deriv-iv (interval 0 10) 1e-10 1000)))

