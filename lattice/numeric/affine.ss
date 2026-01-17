;;; lattice/numeric/affine.ss — Affine Arithmetic
;;;
;;; Affine forms for tighter bounds via correlation tracking.
;;; Solves the "dependency problem" in interval arithmetic.
;;;
;;; Author: Claude Opus 4.5
;;; Created: 2026-01-17
;;;
;;; An affine form represents a quantity as:
;;;   x̂ = x₀ + x₁ε₁ + x₂ε₂ + ... + xₙεₙ
;;;
;;; where x₀ is the central value, xᵢ are partial deviations, and
;;; εᵢ are noise symbols with values in [-1, 1].
;;;
;;; Key insight: When two affine forms share noise symbols (from the same
;;; original variable), operations automatically account for correlations.
;;;
;;; Example of the dependency problem:
;;;   Interval: x = [1,2], x - x = [1,2] - [1,2] = [-1, 1]  (WRONG!)
;;;   Affine:   x̂ = 1.5 + 0.5ε₁, x̂ - x̂ = 0  (CORRECT!)
;;;
;;; Usage:
;;;   (load "lattice/numeric/affine.ss")
;;;   (define x (affine-from-interval (interval 1 2)))
;;;   (affine-sub x x)  ; => affine form with zero deviation
;;;   (affine->interval (affine-sub x x))  ; => [0, 0]

(load "core/base/prelude.ss")
(load "lattice/numeric/interval.ss")

;;; ============================================================================
;;; Noise Symbol Management
;;; ============================================================================
;;;
;;; Each noise symbol ε_i represents an independent source of uncertainty.
;;; We track them globally to ensure unique IDs.

(define *affine-next-noise-id* 0)

;;; affine-fresh-noise-id! : → Nat
;;; Generate a fresh noise symbol ID. IMPURE: modifies global state.
;;; In pure contexts, use explicit noise ID threading.
(define (affine-fresh-noise-id!)
  (let ([id *affine-next-noise-id*])
    (set! *affine-next-noise-id* (+ id 1))
    id))

;;; affine-reset-noise-counter! : → Void
;;; Reset noise counter. Useful for testing reproducibility.
(define (affine-reset-noise-counter!)
  (set! *affine-next-noise-id* 0))

;;; ============================================================================
;;; Affine Form Type
;;; ============================================================================
;;;
;;; Representation: (affine x0 ((id1 . x1) (id2 . x2) ...))
;;; - x0: central value (real number)
;;; - terms: association list of (noise-id . coefficient) pairs
;;;
;;; Invariants:
;;; - Noise IDs in terms are unique
;;; - Terms are sorted by noise ID (for efficient merge)
;;; - Zero coefficients are not stored

;;; make-affine : Real × Alist → Affine
;;; Create an affine form from center and terms.
;;; Terms are (noise-id . coefficient) pairs.
(define (make-affine x0 terms)
  ;; Filter out zero terms and sort by noise ID
  (let ([nonzero (filter (lambda (term) (not (zero? (cdr term)))) terms)])
    (list 'affine x0 (sort (lambda (a b) (< (car a) (car b))) nonzero))))

;;; affine? : Any → Boolean
(define (affine? x)
  (and (pair? x)
       (eq? (car x) 'affine)
       (= (length x) 3)))

;;; affine-center : Affine → Real
;;; Get central value x₀.
(define (affine-center af)
  (cadr af))

;;; affine-terms : Affine → Alist
;;; Get noise terms as ((id . coef) ...).
(define (affine-terms af)
  (caddr af))

;;; affine-constant : Real → Affine
;;; Create a constant affine form (no noise terms).
(define (affine-constant x)
  (make-affine x '()))

;;; affine-noise : Real × Real → Affine
;;; Create an affine form with one noise symbol: x0 + x1*ε_new
(define (affine-noise x0 x1)
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
  (let ([lo (interval-lo iv)]
        [hi (interval-hi iv)])
    (let ([mid (/ (+ lo hi) 2)]
          [rad (/ (- hi lo) 2)])
      (affine-noise mid rad))))

;;; affine->interval : Affine → Interval
;;; Convert affine form back to interval.
;;; The interval is [x0 - Σ|xi|, x0 + Σ|xi|].
(define (affine->interval af)
  (let ([x0 (affine-center af)]
        [terms (affine-terms af)])
    (let ([total-deviation (fold-left + 0 (map (lambda (t) (abs (cdr t))) terms))])
      (make-interval (- x0 total-deviation)
                     (+ x0 total-deviation)))))

;;; affine-radius : Affine → Real
;;; Total radius (sum of absolute deviations).
(define (affine-radius af)
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
  (make-affine (+ (affine-center af1) (affine-center af2))
               (merge-terms (affine-terms af1) (affine-terms af2) +)))

;;; affine-sub : Affine × Affine → Affine
;;; Subtraction. When af1 = af2, all terms cancel! This solves the dependency problem.
(define (affine-sub af1 af2)
  (make-affine (- (affine-center af1) (affine-center af2))
               (merge-terms (affine-terms af1) (affine-terms af2) -)))

;;; affine-scale : Affine × Real → Affine
;;; Scalar multiplication: k*(x₀ + Σxᵢεᵢ) = k*x₀ + Σ(k*xᵢ)εᵢ
(define (affine-scale af k)
  (make-affine (* k (affine-center af))
               (map (lambda (t) (cons (car t) (* k (cdr t))))
                    (affine-terms af))))

;;; affine-add-constant : Affine × Real → Affine
;;; Add a constant: (x₀ + Σxᵢεᵢ) + c = (x₀+c) + Σxᵢεᵢ
(define (affine-add-constant af c)
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
;;; Reciprocal 1/x̂ using Chebyshev approximation over the range.
(define (affine-recip af)
  (let* ([iv (affine->interval af)]
         [lo (interval-lo iv)]
         [hi (interval-hi iv)])
    (cond
      ;; Division by zero: interval contains zero
      [(and (<= lo 0) (>= hi 0)) 'division-by-zero]
      [else
       ;; Chebyshev approximation of 1/x over [lo, hi]
       ;; Best linear approx: α + βx where β = -1/(lo*hi), α chosen to minimize max error
       (let* ([a lo]
              [b hi]
              [beta (/ -1 (* a b))]
              [alpha (/ (+ (/ 1 a) (/ 1 b) (* beta (+ a b))) 2)]
              ;; Error bound: max|1/x - (α + βx)| over [a,b]
              ;; Occurs at x = sqrt(a*b) for same-sign intervals
              [xm (sqrt (* (abs a) (abs b)))]
              [xm-signed (if (> a 0) xm (- xm))]
              [delta (abs (- (/ 1 xm-signed) (+ alpha (* beta xm-signed))))])
         ;; Result: α + β*x̂ + δ*ε_new
         (let* ([x0 (affine-center af)]
                [terms (affine-terms af)]
                [center (+ alpha (* beta x0))]
                [linear-terms (map (lambda (t) (cons (car t) (* beta (cdr t)))) terms)]
                [error-term (if (zero? delta)
                                '()
                                (list (cons (affine-fresh-noise-id!) delta)))])
           (make-affine center (append linear-terms error-term))))])))

;;; affine-div : Affine × Affine → Affine | 'division-by-zero
;;; Division: x̂/ŷ = x̂ * (1/ŷ)
(define (affine-div af1 af2)
  (let ([recip (affine-recip af2)])
    (if (eq? recip 'division-by-zero)
        'division-by-zero
        (affine-mul af1 recip))))

;;; affine-sqrt : Affine → Affine | 'domain-error
;;; Square root using Chebyshev approximation.
(define (affine-sqrt af)
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
(define (affine-sqrt-positive af)
  (let* ([iv (affine->interval af)]
         [a (max 1e-300 (interval-lo iv))]  ; Avoid division by zero
         [b (interval-hi iv)])
    (cond
      [(< (- b a) 1e-15)  ; Near-singleton
       (affine-constant (sqrt (affine-center af)))]
      [else
       ;; Chebyshev approximation of sqrt(x) over [a, b]
       ;; Best linear approx: α + βx
       ;; For sqrt, minimum max error line passes through (a, sqrt(a)) and (b, sqrt(b))
       ;; with slope β = (sqrt(b) - sqrt(a)) / (b - a)
       (let* ([sqrt-a (sqrt a)]
              [sqrt-b (sqrt b)]
              [beta (/ (- sqrt-b sqrt-a) (- b a))]
              ;; Find alpha such that line is tangent to sqrt at some point
              ;; Tangent has slope 1/(2*sqrt(x)), so x = 1/(4*beta^2)
              [x-tangent (/ 1 (* 4 beta beta))]
              [x-tangent-clamped (max a (min b x-tangent))]
              ;; The error is largest at the tangent point
              [alpha (- (sqrt x-tangent-clamped) (* beta x-tangent-clamped))]
              ;; Error bound: check at endpoints and tangent point
              [err-a (abs (- sqrt-a (+ alpha (* beta a))))]
              [err-b (abs (- sqrt-b (+ alpha (* beta b))))]
              [delta (max err-a err-b)])
         ;; Result: α + β*x̂ + δ*ε_new
         (let* ([x0 (affine-center af)]
                [terms (affine-terms af)]
                [center (+ alpha (* beta x0))]
                [linear-terms (map (lambda (t) (cons (car t) (* beta (cdr t)))) terms)]
                [error-term (if (< delta 1e-15)
                                '()
                                (list (cons (affine-fresh-noise-id!) delta)))])
           (make-affine center (append linear-terms error-term))))])))

;;; ============================================================================
;;; Elementary Functions via Conservative Approximation
;;; ============================================================================
;;;
;;; For non-affine elementary functions, we use a conservative approach:
;;; 1. Compute function at center point
;;; 2. Bound the maximum deviation using interval evaluation
;;; 3. Add error term covering all possible deviations
;;;
;;; This is simpler and more robust than Chebyshev, at the cost of slightly
;;; wider bounds. The key insight is that for monotonic functions, the
;;; linearization error can be bounded by comparing against interval endpoints.

;;; affine-exp : Affine → Affine
;;; Exponential function. exp is monotonically increasing and convex.
(define (affine-exp af)
  (let* ([iv (affine->interval af)]
         [a (interval-lo iv)]
         [b (interval-hi iv)]
         [x0 (affine-center af)]
         [r (affine-radius af)])
    (cond
      [(< r 1e-15)  ; Near-constant
       (affine-constant (exp x0))]
      [else
       ;; exp is monotone, so exp([a,b]) = [exp(a), exp(b)]
       ;; We want: center = exp(x0), error covers deviation from linearity
       ;;
       ;; For a linear approximation f(x) ≈ f(x0) + f'(x0)*(x - x0):
       ;;   exp(x) ≈ exp(x0) + exp(x0)*(x - x0)
       ;;
       ;; But this doesn't bound the error well for exp. Instead, use
       ;; interval bounds: center at midpoint of [exp(a), exp(b)].
       (let* ([exp-a (exp a)]
              [exp-b (exp b)]
              [center (/ (+ exp-a exp-b) 2)]
              [radius (/ (- exp-b exp-a) 2)])
         ;; Create new affine form: preserves some correlation through linear part,
         ;; but adds error term for non-linearity
         ;;
         ;; Use linearization: exp(x) ≈ exp(x0) + exp(x0)*(x - x0)
         ;; with error bounded by difference from true interval bounds
         (let* ([exp-x0 (exp x0)]
                [linear-lo (+ exp-x0 (* exp-x0 (- a x0)))]
                [linear-hi (+ exp-x0 (* exp-x0 (- b x0)))]
                ;; Error: difference between linear approx and true bounds
                [err-lo (- exp-a linear-lo)]  ; Should be positive (convex)
                [err-hi (- exp-b linear-hi)]  ; Should be positive (convex)
                [max-err (max (abs err-lo) (abs err-hi))]
                [terms (affine-terms af)]
                ;; Scale noise terms by exp(x0) (derivative at center)
                [linear-terms (map (lambda (t) (cons (car t) (* exp-x0 (cdr t)))) terms)]
                [error-term (if (< max-err 1e-15)
                                '()
                                (list (cons (affine-fresh-noise-id!) max-err)))])
           (make-affine exp-x0 (append linear-terms error-term))))])))

;;; affine-log : Affine → Affine | 'domain-error
;;; Natural logarithm. log is monotonically increasing and concave.
(define (affine-log af)
  (let* ([iv (affine->interval af)]
         [a (interval-lo iv)]
         [b (interval-hi iv)])
    (cond
      [(<= b 0) 'domain-error]  ; Entirely non-positive
      [(<= a 0)
       ;; Partially negative: clamp to small positive
       (affine-log (make-affine (/ (+ 1e-300 b) 2)
                                (list (cons (affine-fresh-noise-id!) (/ (- b 1e-300) 2)))))]
      [else
       (let* ([x0 (affine-center af)]
              [r (affine-radius af)])
         (cond
           [(< r 1e-15)  ; Near-constant
            (affine-constant (log x0))]
           [else
            ;; log is monotone, so log([a,b]) = [log(a), log(b)]
            ;; Linearization: log(x) ≈ log(x0) + (1/x0)*(x - x0)
            (let* ([log-a (log a)]
                   [log-b (log b)]
                   [log-x0 (log x0)]
                   [deriv (/ 1 x0)]  ; Derivative at center
                   ;; Linear approximation at endpoints
                   [linear-lo (+ log-x0 (* deriv (- a x0)))]
                   [linear-hi (+ log-x0 (* deriv (- b x0)))]
                   ;; Error: difference between linear approx and true bounds
                   ;; log is concave so linear approx OVERestimates
                   [err-lo (- linear-lo log-a)]  ; Should be positive (concave)
                   [err-hi (- linear-hi log-b)]  ; Should be positive (concave)
                   [max-err (max (abs err-lo) (abs err-hi))]
                   [terms (affine-terms af)]
                   ;; Scale noise terms by 1/x0 (derivative at center)
                   [linear-terms (map (lambda (t) (cons (car t) (* deriv (cdr t)))) terms)]
                   [error-term (if (< max-err 1e-15)
                                   '()
                                   (list (cons (affine-fresh-noise-id!) max-err)))])
              (make-affine log-x0 (append linear-terms error-term)))]))])))

;;; ============================================================================
;;; Min/Max Operations
;;; ============================================================================

;;; affine-min : Affine × Affine → Affine
;;; Minimum of two affine forms. Conservative approximation.
(define (affine-min af1 af2)
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
  (> (interval-lo (affine->interval af)) 0))

;;; affine-definitely-negative? : Affine → Boolean
(define (affine-definitely-negative? af)
  (< (interval-hi (affine->interval af)) 0))

;;; affine-possibly-zero? : Affine → Boolean
(define (affine-possibly-zero? af)
  (let ([iv (affine->interval af)])
    (and (<= (interval-lo iv) 0)
         (>= (interval-hi iv) 0))))

;;; affine-definitely< : Affine × Affine → Boolean
(define (affine-definitely< af1 af2)
  (interval-definitely< (affine->interval af1) (affine->interval af2)))

;;; affine-definitely<= : Affine × Affine → Boolean
(define (affine-definitely<= af1 af2)
  (interval-definitely<= (affine->interval af1) (affine->interval af2)))

;;; affine-definitely> : Affine × Affine → Boolean
(define (affine-definitely> af1 af2)
  (affine-definitely< af2 af1))

;;; affine-definitely>= : Affine × Affine → Boolean
(define (affine-definitely>= af1 af2)
  (affine-definitely<= af2 af1))

;;; ============================================================================
;;; Display
;;; ============================================================================

;;; affine->string : Affine → String
(define (affine->string af)
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
  (display (affine->string af)))

;;; ============================================================================
;;; Convenience Aliases
;;; ============================================================================

(define af+ affine-add)
(define af- affine-sub)
(define af* affine-mul)
(define af/ affine-div)
(define af-neg affine-neg)
(define af-sqr affine-sqr)
(define af-sqrt affine-sqrt)
(define af-exp affine-exp)
(define af-log affine-log)

;;; ============================================================================
;;; Higher-Level Operations
;;; ============================================================================

;;; affine-sum : (Listof Affine) → Affine
;;; Sum of a list of affine forms.
(define (affine-sum afs)
  (if (null? afs)
      (affine-constant 0)
      (fold-left affine-add (car afs) (cdr afs))))

;;; affine-product : (Listof Affine) → Affine
;;; Product of a list of affine forms.
(define (affine-product afs)
  (if (null? afs)
      (affine-constant 1)
      (fold-left affine-mul (car afs) (cdr afs))))

;;; affine-linear-combination : (Listof Real) × (Listof Affine) → Affine
;;; Compute Σ cᵢ * x̂ᵢ (exact for affine operations).
(define (affine-linear-combination coeffs afs)
  (affine-sum (map affine-scale afs coeffs)))

;;; ============================================================================
;;; Polynomial Evaluation
;;; ============================================================================

;;; affine-horner : (Listof Real) × Affine → Affine
;;; Evaluate polynomial using Horner's method.
;;; Coefficients are [a0, a1, a2, ...] for a0 + a1*x + a2*x^2 + ...
(define (affine-horner coeffs x)
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

