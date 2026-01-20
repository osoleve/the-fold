(load "core/base/prelude.ss")
(load "lattice/numeric/interval.ss")

(doc 'module 'affine)
(doc 'description "Affine forms for tighter bounds via correlation tracking. Solves the dependency problem in interval arithmetic.")
(doc 'author "Claude Opus 4.5")
(doc 'created "2026-01-17")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "An affine form represents a quantity as: x̂ = x₀ + x₁ε₁ + x₂ε₂ + ... + xₙεₙ where x₀ is the central value, xᵢ are partial deviations, and εᵢ are noise symbols with values in [-1, 1].")
(doc 'note "Key insight: When two affine forms share noise symbols (from the same original variable), operations automatically account for correlations.")
(doc 'example "Dependency problem: Interval: x = [1,2], x - x = [1,2] - [1,2] = [-1, 1] (WRONG!) Affine: x̂ = 1.5 + 0.5ε₁, x̂ - x̂ = 0 (CORRECT!)")

(doc 'section 'noise-symbols)
(doc 'note "Each noise symbol ε_i represents an independent source of uncertainty. We track them globally to ensure unique IDs.")

(define *affine-next-noise-id* 0)

(define (affine-fresh-noise-id!)
  (doc 'export #t)
  (doc 'type '(-> Nat))
  (doc 'description "Generate a fresh noise symbol ID")
  (doc 'warning "IMPURE: modifies global state. In pure contexts, use explicit noise ID threading.")
  (let ([id *affine-next-noise-id*])
    (set! *affine-next-noise-id* (+ id 1))
    id))

(define (affine-reset-noise-counter!)
  (doc 'export #t)
  (doc 'type '(-> Void))
  (doc 'description "Reset noise counter. Useful for testing reproducibility.")
  (set! *affine-next-noise-id* 0))

(doc 'section 'affine-type)
(doc 'note "Representation: (affine x0 ((id1 . x1) (id2 . x2) ...)) where x0 is central value (real number), terms is association list of (noise-id . coefficient) pairs")
(doc 'invariant "Noise IDs in terms are unique")
(doc 'invariant "Terms are sorted by noise ID for efficient merge")
(doc 'invariant "Zero coefficients are not stored")

(define (make-affine x0 terms)
  (doc 'export #t)
  (doc 'type '(-> Real Alist Affine))
  (doc 'description "Create an affine form from center and terms. Terms are (noise-id . coefficient) pairs.")
  ;; Filter out zero terms and sort by noise ID
  (let ([nonzero (filter (lambda (term) (not (zero? (cdr term)))) terms)])
    (list 'affine x0 (sort (lambda (a b) (< (car a) (car b))) nonzero))))

(define (affine? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is an affine form")
  (and (pair? x)
       (eq? (car x) 'affine)
       (= (length x) 3)))

;;; affine-center : Affine → Real
;;; Get central value x₀.
(define (affine-center af)
  (doc 'export #t)
  (cadr af))

;;; affine-terms : Affine → Alist
;;; Get noise terms as ((id . coef) ...).
(define (affine-terms af)
  (doc 'export #t)
  (caddr af))

;;; affine-constant : Real → Affine
;;; Create a constant affine form (no noise terms).
(define (affine-constant x)
  (doc 'export #t)
  (make-affine x '()))

;;; affine-noise : Real × Real → Affine
;;; Create an affine form with one noise symbol: x0 + x1*ε_new
(define (affine-noise x0 x1)
  (doc 'export #t)
  (if (zero? x1)
      (affine-constant x0)
      (make-affine x0 (list (cons (affine-fresh-noise-id!) x1)))))

;;; ============================================================================
;;; Interval Conversion
;;; ============================================================================

;;; affine-from-interval : Interval → Affine
;;; Convert interval [lo, hi] to affine form: mid + radius*ε_new
;;; The new noise symbol represents uncertainty within the interval.
(define (affine-from-interval iv)
  (doc 'export #t)
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (let ([mid (/ (+ lo hi) 2)]
          [rad (/ (- hi lo) 2)])
      (affine-noise mid rad))))

;;; affine->interval : Affine → Interval
;;; Convert affine form back to interval.
;;; The interval is [x0 - Σ|xi|, x0 + Σ|xi|].
(define (affine->interval af)
  (doc 'export #t)
  (let ([x0 (affine-center af)]
        [terms (affine-terms af)])
    (let ([total-deviation (fold-left + 0 (map (lambda (t) (abs (cdr t))) terms))])
      (make-interval (- x0 total-deviation)
                     (+ x0 total-deviation)))))

;;; affine-radius : Affine → Real
;;; Total radius (sum of absolute deviations).
(define (affine-radius af)
  (doc 'export #t)
  (fold-left + 0 (map (lambda (t) (abs (cdr t))) (affine-terms af))))

;;; ============================================================================
;;; Affine Operations (Correlation-Preserving)
;;; ============================================================================
;;;
;;; These operations are EXACT for affine functions. They preserve
;;; correlations by combining coefficients of shared noise symbols.

;;; affine-neg : Affine → Affine
;;; Negate: -(x₀ + Σxᵢεᵢ) = -x₀ + Σ(-xᵢ)εᵢ
(define (affine-neg af)
  (doc 'export #t)
  (make-affine (- (affine-center af))
               (map (lambda (t) (cons (car t) (- (cdr t))))
                    (affine-terms af))))

;;; merge-terms : Alist × Alist × (Real × Real → Real) → Alist
;;; Merge two sorted term lists using binary operation on coefficients.
;;; This is the key operation that preserves correlations!
(define (merge-terms terms1 terms2 op)
  (let loop ([t1 terms1] [t2 terms2] [acc '()])
    (cond
      [(null? t1)
       (reverse (append (map (lambda (t) (cons (car t) (op 0 (cdr t)))) t2) acc))]
      [(null? t2)
       (reverse (append (map (lambda (t) (cons (car t) (op (cdr t) 0))) t1) acc))]
      [(< (caar t1) (caar t2))
       (loop (cdr t1) t2 (cons (cons (caar t1) (op (cdar t1) 0)) acc))]
      [(> (caar t1) (caar t2))
       (loop t1 (cdr t2) (cons (cons (caar t2) (op 0 (cdar t2))) acc))]
      [else  ; Same noise ID - combine coefficients!
       (loop (cdr t1) (cdr t2) (cons (cons (caar t1) (op (cdar t1) (cdar t2))) acc))])))

;;; affine-add : Affine × Affine → Affine
;;; Addition: (x₀ + Σxᵢεᵢ) + (y₀ + Σyᵢεᵢ) = (x₀+y₀) + Σ(xᵢ+yᵢ)εᵢ
(define (affine-add af1 af2)
  (doc 'export #t)
  (make-affine (+ (affine-center af1) (affine-center af2))
               (merge-terms (affine-terms af1) (affine-terms af2) +)))

;;; affine-sub : Affine × Affine → Affine
;;; Subtraction. When af1 = af2, all terms cancel! This solves the dependency problem.
(define (affine-sub af1 af2)
  (doc 'export #t)
  (make-affine (- (affine-center af1) (affine-center af2))
               (merge-terms (affine-terms af1) (affine-terms af2) -)))

;;; affine-scale : Affine × Real → Affine
;;; Scalar multiplication: k*(x₀ + Σxᵢεᵢ) = k*x₀ + Σ(k*xᵢ)εᵢ
(define (affine-scale af k)
  (doc 'export #t)
  (make-affine (* k (affine-center af))
               (map (lambda (t) (cons (car t) (* k (cdr t))))
                    (affine-terms af))))

;;; affine-add-constant : Affine × Real → Affine
;;; Add a constant: (x₀ + Σxᵢεᵢ) + c = (x₀+c) + Σxᵢεᵢ
(define (affine-add-constant af c)
  (doc 'export #t)
  (make-affine (+ (affine-center af) c)
               (affine-terms af)))

;;; ============================================================================
;;; Non-Affine Operations
;;; ============================================================================
;;;
;;; For non-affine operations (multiplication, division, etc.), we use
;;; Chebyshev approximation: approximate f(x) ≈ α + βx over the range,
;;; then add a new noise term for the approximation error.

;;; affine-mul : Affine × Affine → Affine
;;; Multiplication: (x₀ + Σxᵢεᵢ)(y₀ + Σyᵢεᵢ)
;;;
;;; Expanding: x₀y₀ + x₀Σyᵢεᵢ + y₀Σxᵢεᵢ + (Σxᵢεᵢ)(Σyᵢεᵢ)
;;;
;;; The last term (product of noise) is non-affine. We bound it with
;;; a new noise symbol. |Σxᵢεᵢ| ≤ rx, |Σyᵢεᵢ| ≤ ry, so the product
;;; is bounded by rx*ry.
(define (affine-mul af1 af2)
  (doc 'export #t)
  (let* ([x0 (affine-center af1)]
         [y0 (affine-center af2)]
         [terms1 (affine-terms af1)]
         [terms2 (affine-terms af2)]
         [rx (affine-radius af1)]
         [ry (affine-radius af2)])
    ;; Affine part: x₀y₀ + x₀*y_terms + y₀*x_terms
    (let* ([center (* x0 y0)]
           [scaled1 (map (lambda (t) (cons (car t) (* y0 (cdr t)))) terms1)]
           [scaled2 (map (lambda (t) (cons (car t) (* x0 (cdr t)))) terms2)]
           [linear-terms (merge-terms scaled1 scaled2 +)]
           ;; Non-affine approximation error: rx * ry
           [error-term (if (or (zero? rx) (zero? ry))
                           '()
                           (list (cons (affine-fresh-noise-id!) (* rx ry))))])
      (make-affine center (append linear-terms error-term)))))

;;; affine-sqr : Affine → Affine
;;; Square: x̂² = x₀² + 2x₀Σxᵢεᵢ + (Σxᵢεᵢ)²
;;;
;;; The quadratic term (Σxᵢεᵢ)² is bounded by r² where r = Σ|xᵢ|.
;;; But we can be smarter: (Σxᵢεᵢ)² ∈ [0, r²] with center r²/2.
(define (affine-sqr af)
  (doc 'export #t)
  (let* ([x0 (affine-center af)]
         [terms (affine-terms af)]
         [r (affine-radius af)])
    ;; x̂² ≈ (x₀² + r²/2) + 2x₀*Σxᵢεᵢ + (r²/2)*ε_new
    ;; The r²/2 shift centers the quadratic error term.
    (let* ([center (+ (* x0 x0) (/ (* r r) 2))]
           [linear-terms (map (lambda (t) (cons (car t) (* 2 x0 (cdr t)))) terms)]
           [error-coef (/ (* r r) 2)]
           [error-term (if (zero? error-coef)
                           '()
                           (list (cons (affine-fresh-noise-id!) error-coef)))])
      (make-affine center (append linear-terms error-term)))))

;;; affine-recip : Affine → Affine | 'division-by-zero
;;; Reciprocal 1/x̂ using linearization at center with conservative error bounds.
(define (affine-recip af)
  (doc 'export #t)
  (let* ([iv (affine->interval af)]
         [a (interval-lo iv)]
         [b (interval-hi iv)])
    (cond
      ;; Division by zero: interval contains zero
      [(and (<= a 0) (>= b 0)) 'division-by-zero]
      [else
       (let* ([x0 (affine-center af)]
              [r (affine-radius af)])
         (cond
           [(< r 1e-15)  ; Near-constant
            (affine-constant (/ 1 x0))]
           [else
            ;; 1/x is monotone decreasing, so 1/[a,b] = [1/b, 1/a] for positive
            ;; or [1/b, 1/a] for negative (reversed order since both negative)
            ;; Linearization: 1/x ≈ 1/x0 + (-1/x0²) * (x - x0) = 1/x0 - (x - x0)/x0²
            ;;              = 1/x0 - x/x0² + 1/x0 = 2/x0 - x/x0²
            ;; Derivative at center: -1/x0²
            (let* ([recip-a (/ 1 a)]
                   [recip-b (/ 1 b)]
                   [recip-x0 (/ 1 x0)]
                   [deriv (/ -1 (* x0 x0))]  ; Derivative at center
                   ;; Linear approximation at endpoints
                   [linear-lo (+ recip-x0 (* deriv (- a x0)))]
                   [linear-hi (+ recip-x0 (* deriv (- b x0)))]
                   ;; Error: difference between linear approx and true bounds
                   ;; 1/x is convex for positive x, so linear UNDERestimates
                   ;; For negative x, 1/x is also convex (shaped like positive but flipped)
                   [err-lo (- recip-a linear-lo)]
                   [err-hi (- recip-b linear-hi)]
                   [max-err (max (abs err-lo) (abs err-hi))]
                   [terms (affine-terms af)]
                   ;; Scale noise terms by derivative at center
                   [linear-terms (map (lambda (t) (cons (car t) (* deriv (cdr t)))) terms)]
                   [error-term (if (< max-err 1e-15)
                                   '()
                                   (list (cons (affine-fresh-noise-id!) max-err)))])
              (make-affine recip-x0 (append linear-terms error-term)))]))])))

;;; affine-div : Affine × Affine → Affine | 'division-by-zero
;;; Division: x̂/ŷ = x̂ * (1/ŷ)
(define (affine-div af1 af2)
  (doc 'export #t)
  (let ([recip (affine-recip af2)])
    (if (eq? recip 'division-by-zero)
        'division-by-zero
        (affine-mul af1 recip))))

;;; affine-sqrt : Affine → Affine | 'domain-error
;;; Square root using Chebyshev approximation.
(define (affine-sqrt af)
  (doc 'export #t)
  (let* ([iv (affine->interval af)]
         [lo (interval-lo iv)]
         [hi (interval-hi iv)])
    (cond
      [(< hi 0) 'domain-error]  ; Entirely negative
      [(< lo 0)
       ;; Partially negative: clamp to [0, hi]
       (affine-sqrt-positive (make-affine (/ hi 2) (list (cons (affine-fresh-noise-id!) (/ hi 2)))))]
      [else (affine-sqrt-positive af)])))

;;; affine-sqrt-positive : Affine → Affine
;;; Square root for affine forms known to be non-negative.
;;; Uses generic Chebyshev helper with clamp-lo=1e-300 to avoid numerical issues near zero.
;;; sqrt'(x) = 1/(2√x) = β  →  x = 1/(4β²)
(define (affine-sqrt-positive af)
  (affine-chebyshev-approx af sqrt (lambda (β) (/ 1 (* 4 β β))) 'concave 1e-300))

;;; ============================================================================
;;; Optimal Linear Approximation (Generic Chebyshev)
;;; ============================================================================
;;;
;;; For monotonic functions with known convexity, we use optimal Chebyshev
;;; approximation: the line midway between tangent and secant equalizes
;;; the maximum positive and negative errors, cutting bounds ~2x vs naive.

;;; affine-chebyshev-approx : Affine × (Num → Num) × (Num → Num) × Symbol [× Num] → Affine
;;; Generic optimal linear approximation for monotonic functions.
;;;   af           - the affine form to approximate f over
;;;   f            - the function to approximate
;;;   deriv-inverse - given slope β, returns x where f'(x) = β
;;;   convexity    - 'convex (secant > tangent) or 'concave (tangent > secant)
;;;   clamp-lo     - optional: clamp lower bound away from problematic values
;;;                  (e.g., sqrt needs a > 0 for the derivative to be defined)
;;;
;;; Examples of deriv-inverse:
;;;   sqrt: f'(x) = 1/(2√x) = β  →  x = 1/(4β²)   →  (lambda (β) (/ 1 (* 4 β β)))
;;;   exp:  f'(x) = exp(x) = β   →  x = log(β)    →  log
;;;   log:  f'(x) = 1/x = β      →  x = 1/β       →  (lambda (β) (/ 1 β))
(define (affine-chebyshev-approx af f deriv-inverse convexity . clamp-lo-opt)
  (let* ([iv (affine->interval af)]
         [a-raw (interval-lo iv)]
         [a (if (pair? clamp-lo-opt) (max (car clamp-lo-opt) a-raw) a-raw)]
         [b (interval-hi iv)]
         [x0 (affine-center af)]
         [r (affine-radius af)])
    (cond
      [(< r 1e-15)  ; Near-constant
       (affine-constant (f x0))]
      [else
       ;; Secant slope β = (f(b) - f(a)) / (b - a)
       (let* ([fa (f a)]
              [fb (f b)]
              [beta (/ (- fb fa) (- b a))]
              ;; Secant intercept: line through (a, f(a)) with slope beta
              [alpha-secant (- fa (* beta a))]
              ;; Tangent point: where f'(x) = beta, clamped to [a, b]
              [x-tangent (max a (min b (deriv-inverse beta)))]
              [alpha-tangent (- (f x-tangent) (* beta x-tangent))]
              ;; Optimal: midpoint between tangent and secant
              [alpha (/ (+ alpha-tangent alpha-secant) 2)]
              ;; Error bound: half the gap (use abs for robustness)
              [delta-raw (/ (abs (- alpha-secant alpha-tangent)) 2)]
              ;; Add safety margin for floating point
              [delta (+ delta-raw (* 1e-14 (max (abs fa) (abs fb))))]
              ;; Build result affine form
              [terms (affine-terms af)]
              [center (+ alpha (* beta x0))]
              [linear-terms (map (lambda (t) (cons (car t) (* beta (cdr t)))) terms)]
              [error-term (if (< delta 1e-15)
                              '()
                              (list (cons (affine-fresh-noise-id!) delta)))])
         (make-affine center (append linear-terms error-term)))])))

;;; ============================================================================
;;; Elementary Functions via Optimal Approximation
;;; ============================================================================
;;;
;;; Use affine-chebyshev-approx for ~2x tighter bounds vs naive linearization.

;;; affine-exp : Affine → Affine
;;; Exponential function. exp is monotonically increasing and convex.
;;; exp'(x) = exp(x) = β  →  x = log(β)
(define (affine-exp af)
  (doc 'export #t)
  (affine-chebyshev-approx af exp log 'convex))

;;; affine-log : Affine → Affine | 'domain-error
;;; Natural logarithm. log is monotonically increasing and concave.
;;; log'(x) = 1/x = β  →  x = 1/β
(define (affine-log af)
  (doc 'export #t)
  (let* ([iv (affine->interval af)]
         [a (interval-lo iv)]
         [b (interval-hi iv)])
    (cond
      [(<= b 0) 'domain-error]  ; Entirely non-positive
      [(<= a 0)
       ;; Partially non-positive: clamp to small positive
       (affine-log (make-affine (/ (+ 1e-300 b) 2)
                                (list (cons (affine-fresh-noise-id!) (/ (- b 1e-300) 2)))))]
      [else
       (affine-chebyshev-approx af log (lambda (beta) (/ 1 beta)) 'concave)])))

;;; ============================================================================
;;; Min/Max Operations
;;; ============================================================================

;;; affine-min : Affine × Affine → Affine
;;; Minimum of two affine forms. Conservative approximation.
(define (affine-min af1 af2)
  (doc 'export #t)
  (let ([iv1 (affine->interval af1)]
        [iv2 (affine->interval af2)])
    (cond
      ;; If ranges don't overlap, we know the answer
      [(interval-definitely< iv1 iv2) af1]
      [(interval-definitely< iv2 iv1) af2]
      ;; Ranges overlap: conservative approximation
      [else
       (let* ([lo (min (interval-lo iv1) (interval-lo iv2))]
              [hi (min (interval-hi iv1) (interval-hi iv2))]
              [mid (/ (+ lo hi) 2)]
              [rad (/ (- hi lo) 2)])
         (affine-noise mid rad))])))

;;; affine-max : Affine × Affine → Affine
;;; Maximum of two affine forms. Conservative approximation.
(define (affine-max af1 af2)
  (doc 'export #t)
  (let ([iv1 (affine->interval af1)]
        [iv2 (affine->interval af2)])
    (cond
      [(interval-definitely> iv1 iv2) af1]
      [(interval-definitely> iv2 iv1) af2]
      [else
       (let* ([lo (max (interval-lo iv1) (interval-lo iv2))]
              [hi (max (interval-hi iv1) (interval-hi iv2))]
              [mid (/ (+ lo hi) 2)]
              [rad (/ (- hi lo) 2)])
         (affine-noise mid rad))])))

;;; affine-abs : Affine → Affine
;;; Absolute value.
(define (affine-abs af)
  (doc 'export #t)
  (let ([iv (affine->interval af)])
    (cond
      [(>= (interval-lo iv) 0) af]              ; Entirely non-negative
      [(<= (interval-hi iv) 0) (affine-neg af)] ; Entirely non-positive
      [else
       ;; Contains zero: result is in [0, max(|lo|, |hi|)]
       (let* ([max-abs (interval-magnitude iv)]
              [mid (/ max-abs 2)]
              [rad (/ max-abs 2)])
         (affine-noise mid rad))])))

;;; ============================================================================
;;; Predicates and Comparisons
;;; ============================================================================

;;; affine-definitely-positive? : Affine → Boolean
(define (affine-definitely-positive? af)
  (doc 'export #t)
  (> (interval-lo (affine->interval af)) 0))

;;; affine-definitely-negative? : Affine → Boolean
(define (affine-definitely-negative? af)
  (doc 'export #t)
  (< (interval-hi (affine->interval af)) 0))

;;; affine-possibly-zero? : Affine → Boolean
(define (affine-possibly-zero? af)
  (doc 'export #t)
  (let ([iv (affine->interval af)])
    (and (<= (interval-lo iv) 0)
         (>= (interval-hi iv) 0))))

;;; affine-definitely< : Affine × Affine → Boolean
(define (affine-definitely< af1 af2)
  (doc 'export #t)
  (interval-definitely< (affine->interval af1) (affine->interval af2)))

;;; affine-definitely<= : Affine × Affine → Boolean
(define (affine-definitely<= af1 af2)
  (doc 'export #t)
  (interval-definitely<= (affine->interval af1) (affine->interval af2)))

;;; affine-definitely> : Affine × Affine → Boolean
(define (affine-definitely> af1 af2)
  (doc 'export #t)
  (affine-definitely< af2 af1))

;;; affine-definitely>= : Affine × Affine → Boolean
(define (affine-definitely>= af1 af2)
  (doc 'export #t)
  (affine-definitely<= af2 af1))

;;; ============================================================================
;;; Display
;;; ============================================================================

;;; affine->string : Affine → String
(define (affine->string af)
  (doc 'export #t)
  (let ([x0 (affine-center af)]
        [terms (affine-terms af)])
    (if (null? terms)
        (number->string x0)
        (string-append
         (number->string x0)
         (apply string-append
                (map (lambda (t)
                       (let ([id (car t)]
                             [coef (cdr t)])
                         (string-append (if (>= coef 0) " + " " - ")
                                        (number->string (abs coef))
                                        "*ε"
                                        (number->string id))))
                     terms))))))

;;; affine-print : Affine → Void
(define (affine-print af)
  (doc 'export #t)
  (display (affine->string af)))

;;; ============================================================================
;;; Convenience Aliases
;;; ============================================================================

(doc af+ 'export #t)
(define af+ affine-add)
(doc af- 'export #t)
(define af- affine-sub)
(doc af* 'export #t)
(define af* affine-mul)
(doc af/ 'export #t)
(define af/ affine-div)
(doc af-neg 'export #t)
(define af-neg affine-neg)
(doc af-sqr 'export #t)
(define af-sqr affine-sqr)
(doc af-sqrt 'export #t)
(define af-sqrt affine-sqrt)
(doc af-exp 'export #t)
(define af-exp affine-exp)
(doc af-log 'export #t)
(define af-log affine-log)

;;; ============================================================================
;;; Higher-Level Operations
;;; ============================================================================

;;; affine-sum : (Listof Affine) → Affine
;;; Sum of a list of affine forms.
(define (affine-sum afs)
  (doc 'export #t)
  (if (null? afs)
      (affine-constant 0)
      (fold-left affine-add (car afs) (cdr afs))))

;;; affine-product : (Listof Affine) → Affine
;;; Product of a list of affine forms.
(define (affine-product afs)
  (doc 'export #t)
  (if (null? afs)
      (affine-constant 1)
      (fold-left affine-mul (car afs) (cdr afs))))

;;; affine-linear-combination : (Listof Real) × (Listof Affine) → Affine
;;; Compute Σ cᵢ * x̂ᵢ (exact for affine operations).
(define (affine-linear-combination coeffs afs)
  (doc 'export #t)
  (affine-sum (map affine-scale afs coeffs)))

;;; ============================================================================
;;; Polynomial Evaluation
;;; ============================================================================

;;; affine-horner : (Listof Real) × Affine → Affine
;;; Evaluate polynomial using Horner's method.
;;; Coefficients are [a0, a1, a2, ...] for a0 + a1*x + a2*x^2 + ...
(define (affine-horner coeffs x)
  (doc 'export #t)
  (if (null? coeffs)
      (affine-constant 0)
      (let loop ([cs (reverse coeffs)] [acc (affine-constant 0)])
        (if (null? cs)
            acc
            (loop (cdr cs)
                  (affine-add (affine-constant (car cs))
                              (affine-mul x acc)))))))

;;; Display load message
(display "Affine arithmetic loaded. Use (affine-from-interval iv) to create affine forms.\n")

