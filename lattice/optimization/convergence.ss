(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @requires prelude vec
(require 'prelude)
(require 'vec)

(doc 'module 'convergence)
(doc 'description "Convergence criteria and stopping conditions for optimization algorithms")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'overview)
(doc 'note "Provides flexible convergence testing with multiple stopping conditions:
  - Gradient norm below tolerance
  - Function value change below tolerance
  - Parameter change below tolerance
  - Maximum iterations reached")

(doc 'section 'convergence-criteria)

(doc 'note "A convergence criteria specifies when to stop optimization.
Structure: (convergence-criteria grad-tol f-tol x-tol max-iter)")

(define (make-convergence-criteria grad-tol f-tol x-tol max-iter)
  (doc 'export #t)
  (doc 'type '(-> Number Number Number Nat ConvergenceCriteria))
  (doc 'description "Create convergence criteria
  grad-tol: stop when ||gradient|| < grad-tol
  f-tol: stop when |f_new - f_old| < f-tol
  x-tol: stop when ||x_new - x_old|| < x-tol
  max-iter: stop after max-iter iterations")
  (list 'convergence-criteria grad-tol f-tol x-tol max-iter))

(define *default-convergence*
  (make-convergence-criteria 1e-6 1e-8 1e-8 1000))
(doc *default-convergence* 'description "Default convergence criteria for optimization")

(define (convergence-criteria? x)
  (doc 'type '(-> α Bool))
  (and (pair? x) (eq? (car x) 'convergence-criteria)))

(doc 'note "Accessors for convergence criteria fields")
(define (cc-grad-tol cc) (list-ref cc 1))
(define (cc-f-tol cc) (list-ref cc 2))
(define (cc-x-tol cc) (list-ref cc 3))
(define (cc-max-iter cc) (list-ref cc 4))

(doc 'section 'convergence-state)

(doc 'note "Tracks optimization progress for convergence checking.
Structure: (convergence-state iter f-val grad-norm f-change x-change)")

(define (make-convergence-state iter f-val grad-norm f-change x-change)
  (doc 'type '(-> Nat Number Number Number Number ConvergenceState))
  (list 'convergence-state iter f-val grad-norm f-change x-change))

(define (convergence-state? x)
  (doc 'type '(-> α Bool))
  (and (pair? x) (eq? (car x) 'convergence-state)))

(doc 'note "Accessors for convergence state fields")
(define (cs-iter cs) (list-ref cs 1))
(define (cs-f-val cs) (list-ref cs 2))
(define (cs-grad-norm cs) (list-ref cs 3))
(define (cs-f-change cs) (list-ref cs 4))
(define (cs-x-change cs) (list-ref cs 5))

(doc 'section 'convergence-checking)

(define (converged? criteria state)
  (doc 'export #t)
  (doc 'type '(-> ConvergenceCriteria ConvergenceState (Either Bool Symbol)))
  (doc 'description "Check if optimization has converged.
Returns #f if not converged, or a symbol indicating why:
  'grad-converged - gradient norm below tolerance
  'f-converged - function value change below tolerance
  'x-converged - parameter change below tolerance
  'max-iter - maximum iterations reached")
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

(doc 'section 'vector-norms)

(define (vec-norm v)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Compute L2 norm of a list-vector using scaling by max element to avoid overflow")
  (if (null? v)
      0
      (let ([max-abs (fold-left max 0 (map abs v))])
           (if (< max-abs 1e-300)
               0  ; All zeros or underflow
               ;; Scale by max to prevent overflow: ||v|| = max * ||v/max||
               (let ([scaled-sq-sum (fold-left + 0
                                               (map (lambda (x)
                                                            (let ([s (/ x max-abs)])
                                                                 (* s s)))
                                                    v))])
                    (* max-abs (sqrt scaled-sq-sum)))))))

(define (vec-diff-norm v1 v2)
  (doc 'type '(-> (List Number) (List Number) Number))
  (doc 'description "Compute ||v1 - v2||")
  (vec-norm (map - v1 v2)))

(define (vec-inf-norm v)
  (doc 'type '(-> (List Number) Number))
  (doc 'description "Compute infinity norm (max absolute value)")
  (fold-left max 0 (map abs v)))

(doc 'section 'optimization-result)

(doc 'note "Represents the result of an optimization.
Structure: (opt-result x f-val grad iterations converged-reason meta)
The meta field is an optional alist of key-value pairs (e.g., curvature-skips).")

;;; make-opt-result : (List Number) × Number × (List Number) × Nat × Symbol [× (List (Pair Symbol Any))] → OptResult
;;; The optional meta argument defaults to '() when omitted.
(define (make-opt-result x f-val grad iterations converged-reason . rest)
  (doc 'type '(-> (List Number) Number (List Number) Nat Symbol OptResult))
  (let ([meta (if (pair? rest) (car rest) '())])
       (list 'opt-result x f-val grad iterations converged-reason meta)))

;;; opt-result? : α → Bool
(define (opt-result? x)
  (and (pair? x) (eq? (car x) 'opt-result)))

;;; Accessors
(define (opt-x result) (list-ref result 1))
(define (opt-f result) (list-ref result 2))
(define (opt-grad result) (list-ref result 3))
(define (opt-iterations result) (list-ref result 4))
(define (opt-converged result) (list-ref result 5))

;;; opt-meta : OptResult → (List (Pair Symbol Any))
;;; Return metadata alist. Returns '() for results without metadata.
(define (opt-meta result)
  (if (> (length result) 6)
      (list-ref result 6)
      '()))

(doc 'section 'convergence-monitoring)

(define (update-convergence-state old-state new-f new-grad old-x new-x)
  (doc 'type '(-> ConvergenceState Number (List Number) (List Number) (List Number) ConvergenceState))
  (doc 'description "Update state with new iteration data")
  (let* ([new-iter (+ (cs-iter old-state) 1)]
         [grad-norm (vec-norm new-grad)]
         [f-change (abs (- new-f (cs-f-val old-state)))]
         [x-change (vec-diff-norm new-x old-x)])
        (make-convergence-state new-iter new-f grad-norm f-change x-change)))

(define (initial-convergence-state f-val grad)
  (doc 'type '(-> Number (List Number) ConvergenceState))
  (doc 'description "Create initial convergence state")
  (make-convergence-state 0 f-val (vec-norm grad) +inf.0 +inf.0))

(doc 'section 'progress-reporting)

(define (convergence-progress state)
  (doc 'type '(-> ConvergenceState String))
  (doc 'description "Format convergence state as a progress string")
  (format "Iter ~a: f=~,6e |grad|=~,6e df=~,6e dx=~,6e"
          (cs-iter state)
          (cs-f-val state)
          (cs-grad-norm state)
          (cs-f-change state)
          (cs-x-change state)))

(doc 'section 'convenience-constructors)

(define (loose-convergence)
  (doc 'type '(-> Unit ConvergenceCriteria))
  (doc 'description "Loose convergence for quick approximate solutions")
  (make-convergence-criteria 1e-3 1e-4 1e-4 100))

(define (tight-convergence)
  (doc 'type '(-> Unit ConvergenceCriteria))
  (doc 'description "Tight convergence for high precision")
  (make-convergence-criteria 1e-10 1e-12 1e-12 10000))

(define (iter-only-convergence n)
  (doc 'type '(-> Nat ConvergenceCriteria))
  (doc 'description "Only stop after n iterations (for benchmarking/fixed iteration count)")
  (make-convergence-criteria 0 0 0 n))
