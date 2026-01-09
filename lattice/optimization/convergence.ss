;;; lattice/optimization/convergence.ss --- Convergence Criteria and Stopping Conditions
;;;
;;; Provides flexible convergence testing for optimization algorithms.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Convergence conditions:
;;;   - Gradient norm below tolerance
;;;   - Function value change below tolerance
;;;   - Parameter change below tolerance
;;;   - Maximum iterations reached
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - linalg/vec.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")

;;; ============================================================
;;; Convergence Criteria Structure
;;; ============================================================

;;; A convergence criteria specifies when to stop optimization.
;;; Structure: (convergence-criteria grad-tol f-tol x-tol max-iter)

;;; make-convergence-criteria : Number × Number × Number × Nat → ConvergenceCriteria
;;; Create convergence criteria.
;;;   grad-tol: stop when ||gradient|| < grad-tol
;;;   f-tol: stop when |f_new - f_old| < f-tol
;;;   x-tol: stop when ||x_new - x_old|| < x-tol
;;;   max-iter: stop after max-iter iterations
(define (make-convergence-criteria grad-tol f-tol x-tol max-iter)
  (list 'convergence-criteria grad-tol f-tol x-tol max-iter))

;;; Default convergence criteria
(define *default-convergence*
  (make-convergence-criteria 1e-6 1e-8 1e-8 1000))

;;; convergence-criteria? : α → Bool
(define (convergence-criteria? x)
  (and (pair? x) (eq? (car x) 'convergence-criteria)))

;;; Accessors
(define (cc-grad-tol cc) (list-ref cc 1))
(define (cc-f-tol cc) (list-ref cc 2))
(define (cc-x-tol cc) (list-ref cc 3))
(define (cc-max-iter cc) (list-ref cc 4))

;;; ============================================================
;;; Convergence State
;;; ============================================================

;;; Tracks optimization progress for convergence checking.
;;; Structure: (convergence-state iter f-val grad-norm f-change x-change)

;;; make-convergence-state : Nat × Number × Number × Number × Number → ConvergenceState
(define (make-convergence-state iter f-val grad-norm f-change x-change)
  (list 'convergence-state iter f-val grad-norm f-change x-change))

;;; convergence-state? : α → Bool
(define (convergence-state? x)
  (and (pair? x) (eq? (car x) 'convergence-state)))

;;; Accessors
(define (cs-iter cs) (list-ref cs 1))
(define (cs-f-val cs) (list-ref cs 2))
(define (cs-grad-norm cs) (list-ref cs 3))
(define (cs-f-change cs) (list-ref cs 4))
(define (cs-x-change cs) (list-ref cs 5))

;;; ============================================================
;;; Convergence Checking
;;; ============================================================

;;; converged? : ConvergenceCriteria × ConvergenceState → (Either Bool Symbol)
;;; Check if optimization has converged.
;;; Returns #f if not converged, or a symbol indicating why:
;;;   'grad-converged - gradient norm below tolerance
;;;   'f-converged - function value change below tolerance
;;;   'x-converged - parameter change below tolerance
;;;   'max-iter - maximum iterations reached
(define (converged? criteria state)
  (cond
   ;; Check gradient norm
   [(< (cs-grad-norm state) (cc-grad-tol criteria))
    'grad-converged]
   ;; Check function value change (after first iteration)
   [(and (> (cs-iter state) 0)
         (< (cs-f-change state) (cc-f-tol criteria)))
    'f-converged]
   ;; Check parameter change (after first iteration)
   [(and (> (cs-iter state) 0)
         (< (cs-x-change state) (cc-x-tol criteria)))
    'x-converged]
   ;; Check max iterations
   [(>= (cs-iter state) (cc-max-iter criteria))
    'max-iter]
   ;; Not converged
   [else #f]))

;;; ============================================================
;;; Vector Norms and Differences
;;; ============================================================

;;; vec-norm : (List Number) → Number
;;; Compute L2 norm of a list-vector.
(define (vec-norm v)
  (sqrt (fold-left + 0 (map (lambda (x) (* x x)) v))))

;;; vec-diff-norm : (List Number) × (List Number) → Number
;;; Compute ||v1 - v2||.
(define (vec-diff-norm v1 v2)
  (vec-norm (map - v1 v2)))

;;; vec-inf-norm : (List Number) → Number
;;; Compute infinity norm (max absolute value).
(define (vec-inf-norm v)
  (fold-left max 0 (map abs v)))

;;; ============================================================
;;; Optimization Result
;;; ============================================================

;;; Represents the result of an optimization.
;;; Structure: (opt-result x f-val grad iterations converged-reason)

;;; make-opt-result : (List Number) × Number × (List Number) × Nat × Symbol → OptResult
(define (make-opt-result x f-val grad iterations converged-reason)
  (list 'opt-result x f-val grad iterations converged-reason))

;;; opt-result? : α → Bool
(define (opt-result? x)
  (and (pair? x) (eq? (car x) 'opt-result)))

;;; Accessors
(define (opt-x result) (list-ref result 1))
(define (opt-f result) (list-ref result 2))
(define (opt-grad result) (list-ref result 3))
(define (opt-iterations result) (list-ref result 4))
(define (opt-converged result) (list-ref result 5))

;;; ============================================================
;;; Convergence Monitoring
;;; ============================================================

;;; update-convergence-state : ConvergenceState × Number × (List Number) × (List Number) × (List Number) → ConvergenceState
;;; Update state with new iteration data.
;;;   old-state: previous state
;;;   new-f: new function value
;;;   new-grad: new gradient
;;;   old-x: previous parameters
;;;   new-x: new parameters
(define (update-convergence-state old-state new-f new-grad old-x new-x)
  (let* ([new-iter (+ (cs-iter old-state) 1)]
         [grad-norm (vec-norm new-grad)]
         [f-change (abs (- new-f (cs-f-val old-state)))]
         [x-change (vec-diff-norm new-x old-x)])
        (make-convergence-state new-iter new-f grad-norm f-change x-change)))

;;; initial-convergence-state : Number × (List Number) → ConvergenceState
;;; Create initial convergence state.
(define (initial-convergence-state f-val grad)
  (make-convergence-state 0 f-val (vec-norm grad) +inf.0 +inf.0))

;;; ============================================================
;;; Progress Reporting
;;; ============================================================

;;; convergence-progress : ConvergenceState → String
;;; Format convergence state as a progress string.
(define (convergence-progress state)
  (format "Iter ~a: f=~,6e |grad|=~,6e df=~,6e dx=~,6e"
          (cs-iter state)
          (cs-f-val state)
          (cs-grad-norm state)
          (cs-f-change state)
          (cs-x-change state)))

;;; ============================================================
;;; Convenience Constructors
;;; ============================================================

;;; loose-convergence : Unit → ConvergenceCriteria
;;; Loose convergence for quick approximate solutions.
(define (loose-convergence)
  (make-convergence-criteria 1e-3 1e-4 1e-4 100))

;;; tight-convergence : Unit → ConvergenceCriteria
;;; Tight convergence for high precision.
(define (tight-convergence)
  (make-convergence-criteria 1e-10 1e-12 1e-12 10000))

;;; iter-only-convergence : Nat → ConvergenceCriteria
;;; Only stop after n iterations (for benchmarking/fixed iteration count).
(define (iter-only-convergence n)
  (make-convergence-criteria 0 0 0 n))
