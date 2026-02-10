;;; lattice/dataset/parameter.ss — Parameter Ranges, Sampling, and Interpolation
;;; @module parameter
;;; @requires prelude prng distributions

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'prng)
(require 'distributions)

(doc 'module 'parameter)
(doc 'description "Parameter specification for dataset generation. Supports ranges with constraints, multiple sampling strategies, and parameter interpolation for counterfactual generation.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ============================================================
;;; Part 0: Imperative PRNG Wrapper
;;; ============================================================
;;;
;;; The prng.ss module uses the State monad for pure random generation.
;;; For convenient imperative use in sampling, we provide a mutable wrapper
;;; that holds generator state in a box.

(doc 'section 'imperative-prng)

;;; make-prng : Nat → MutableRNG
;;; Create a mutable PRNG from a seed
(define (make-prng seed)
  (doc 'export #t)
  (box (make-pcg seed 1)))

;;; prng-next-float : MutableRNG → Number
;;; Get a random float in [0, 1) and update the RNG state
(define (prng-next-float rng)
  (doc 'export #t)
  (let* ([gen (unbox rng)]
         [result (run-state random-float gen)]
         [val (car result)]
         [new-gen (cdr result)])
    (set-box! rng new-gen)
    val))

;;; prng-next-int : MutableRNG × Nat → Nat
;;; Get a random integer in [0, n) and update the RNG state
(define (prng-next-int rng n)
  (doc 'export #t)
  (let* ([gen (unbox rng)]
         [result (run-state (random-int-range 0 (- n 1)) gen)]
         [val (car result)]
         [new-gen (cdr result)])
    (set-box! rng new-gen)
    val))

;;; ============================================================
;;; Part 1: Parameter Ranges
;;; ============================================================
;;;
;;; A parameter range specifies valid values and how to sample them.
;;; Supports:
;;;   - Continuous ranges with uniform/normal/log-uniform sampling
;;;   - Discrete sets
;;;   - Constraints (predicates that sampled values must satisfy)

(doc 'section 'ranges)

;;; make-param-range : Symbol × Number × Number × Symbol × (Any → Boolean) → ParamRange
;;; Create a continuous parameter range.
;;; Strategies: 'uniform, 'normal, 'log-uniform
(define (make-param-range name min-val max-val strategy constraint)
  (doc 'export #t)
  (doc 'type '(-> Symbol Number Number Symbol (-> Any Boolean) ParamRange))
  (list 'param-range name min-val max-val strategy constraint))

;;; param-range? : Any → Boolean
(define (param-range? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'param-range)))

(define (param-range-name r) (list-ref r 1))
(define (param-range-min r) (list-ref r 2))
(define (param-range-max r) (list-ref r 3))
(define (param-range-strategy r) (list-ref r 4))
(define (param-range-constraint r) (list-ref r 5))

;;; Convenience constructors

;;; uniform-range : Symbol × Number × Number → ParamRange
;;; Simple uniform range with no constraint
(define (uniform-range name min-val max-val)
  (doc 'export #t)
  (make-param-range name min-val max-val 'uniform (lambda (_) #t)))

;;; range-int : Integer × Integer → ParamRange
;;; Integer range (samples rounded)
(define (range-int name min-val max-val)
  (doc 'export #t)
  (make-param-range name min-val max-val 'uniform-int (lambda (_) #t)))

;;; range-normal : Number × Number × Number × Number → ParamRange
;;; Normal distribution clipped to [min, max] with given mean/stddev
(define (range-normal name min-val max-val mean stddev)
  (doc 'export #t)
  (list 'param-range-normal name min-val max-val mean stddev (lambda (_) #t)))

;;; range-choice : Symbol × (List Any) → ParamRange
;;; Discrete set of choices
(define (range-choice name choices)
  (doc 'export #t)
  (list 'param-range-choice name choices (lambda (_) #t)))

(define (param-range-choice? r)
  (and (pair? r) (eq? (car r) 'param-range-choice)))

(define (param-range-choice-name r) (list-ref r 1))
(define (param-range-choice-values r) (list-ref r 2))
(define (param-range-choice-constraint r) (list-ref r 3))

;;; range-with-constraint : ParamRange × (Any → Boolean) → ParamRange
;;; Add a constraint to any range
(define (range-with-constraint r constraint)
  (doc 'export #t)
  (cond
    [(param-range? r)
     (make-param-range (param-range-name r)
                       (param-range-min r)
                       (param-range-max r)
                       (param-range-strategy r)
                       constraint)]
    [(param-range-choice? r)
     (list 'param-range-choice
           (param-range-choice-name r)
           (param-range-choice-values r)
           constraint)]
    [else (error 'range-with-constraint "Unknown range type")]))

;;; ============================================================
;;; Part 2: Sampling
;;; ============================================================

(doc 'section 'sampling)

;;; param-sample-raw : ParamRange × RNG → Number
;;; Sample from range without constraint checking
(define (param-sample-raw r rng)
  (cond
    [(param-range? r)
     (let ([min-v (param-range-min r)]
           [max-v (param-range-max r)]
           [strategy (param-range-strategy r)])
       (case strategy
         [(uniform) (+ min-v (* (prng-next-float rng) (- max-v min-v)))]
         [(uniform-int) (+ min-v (prng-next-int rng (+ 1 (- max-v min-v))))]
         [(log-uniform)
          (let ([log-min (log (max 0.001 min-v))]
                [log-max (log max-v)])
            (exp (+ log-min (* (prng-next-float rng) (- log-max log-min)))))]
         [else (+ min-v (* (prng-next-float rng) (- max-v min-v)))]))]

    [(and (pair? r) (eq? (car r) 'param-range-normal))
     (let* ([min-v (list-ref r 2)]
            [max-v (list-ref r 3)]
            [mean (list-ref r 4)]
            [stddev (list-ref r 5)]
            [raw (sample-normal rng mean stddev)])
       (max min-v (min max-v raw)))]

    [(param-range-choice? r)
     (let* ([choices (param-range-choice-values r)]
            [n (length choices)]
            [idx (prng-next-int rng n)])
       (list-ref choices idx))]

    [else (error 'param-sample-raw "Unknown range type")]))

;;; sample-normal : RNG × Number × Number → Number
;;; Box-Muller transform for normal samples
(define (sample-normal rng mean stddev)
  (let* ([u1 (max 0.0001 (prng-next-float rng))]
         [u2 (prng-next-float rng)]
         [z (* (sqrt (* -2 (log u1))) (cos (* 2 3.141592653589793 u2)))])
    (+ mean (* stddev z))))

;;; param-sample : ParamRange × RNG × Nat → Any
;;; Sample from range, retrying up to max-tries if constraint fails
(define (param-sample r rng max-tries)
  (doc 'export #t)
  (doc 'type '(-> ParamRange RNG Nat Any))
  (let ([constraint (cond
                      [(param-range? r) (param-range-constraint r)]
                      [(param-range-choice? r) (param-range-choice-constraint r)]
                      [else (lambda (_) #t)])])
    (let loop ([tries 0])
      (if (>= tries max-tries)
          (error 'param-sample "Failed to sample valid value after max tries")
          (let ([val (param-sample-raw r rng)])
            (if (constraint val)
                val
                (loop (+ tries 1))))))))

;;; param-sample-default : ParamRange × RNG → Any
;;; Sample with default max-tries of 100
(define (param-sample-default r rng)
  (doc 'export #t)
  (param-sample r rng 100))

;;; ============================================================
;;; Part 3: Parameter Sets
;;; ============================================================
;;;
;;; A parameter set bundles multiple parameters together.

(doc 'section 'param-sets)

;;; make-param-set : (List ParamRange) × ((Alist → Boolean)) → ParamSet
;;; Create a parameter set with joint constraint
(define (make-param-set ranges joint-constraint)
  (doc 'export #t)
  (list 'param-set ranges joint-constraint))

;;; param-set? : Any → Boolean
(define (param-set? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'param-set)))

(define (param-set-ranges ps) (list-ref ps 1))
(define (param-set-joint-constraint ps) (list-ref ps 2))

;;; param-set-sample-raw : ParamSet × RNG → Alist
;;; Sample all parameters (no joint constraint check)
(define (param-set-sample-raw ps rng)
  (map (lambda (r)
         (let ([name (cond
                       [(param-range? r) (param-range-name r)]
                       [(param-range-choice? r) (param-range-choice-name r)]
                       [else 'unnamed])])
           (cons name (param-sample-default r rng))))
       (param-set-ranges ps)))

;;; param-set-sample : ParamSet × RNG × Nat → Alist
;;; Sample all parameters, retrying if joint constraint fails
(define (param-set-sample ps rng max-tries)
  (doc 'export #t)
  (doc 'type '(-> ParamSet RNG Nat Alist))
  (let ([constraint (param-set-joint-constraint ps)])
    (let loop ([tries 0])
      (if (>= tries max-tries)
          (error 'param-set-sample "Failed to sample valid parameter set")
          (let ([vals (param-set-sample-raw ps rng)])
            (if (constraint vals)
                vals
                (loop (+ tries 1))))))))

;;; param-get : Alist × Symbol → Any
;;; Get parameter value by name
(define (param-get params name)
  (doc 'export #t)
  (let ([pair (assq name params)])
    (if pair (cdr pair)
        (error 'param-get "Parameter not found" name))))

;;; ============================================================
;;; Part 4: Interpolation (for Counterfactuals)
;;; ============================================================
;;;
;;; Interpolation enables generating "nearby" parameter sets
;;; for counterfactual problems.

(doc 'section 'interpolation)

;;; param-interpolate : ParamRange × Any × Any × Number → Any
;;; Interpolate between two values at t ∈ [0,1]
(define (param-interpolate r v1 v2 t)
  (doc 'export #t)
  (cond
    [(param-range? r)
     (let ([strategy (param-range-strategy r)])
       (case strategy
         [(uniform uniform-int)
          (let ([result (+ (* (- 1 t) v1) (* t v2))])
            (if (eq? strategy 'uniform-int)
                (inexact->exact (round result))
                result))]
         [(log-uniform)
          (exp (+ (* (- 1 t) (log v1)) (* t (log v2))))]
         [else (+ (* (- 1 t) v1) (* t v2))]))]
    [(param-range-choice? r)
     ;; For discrete: use v1 if t < 0.5, else v2
     (if (< t 0.5) v1 v2)]
    [else (+ (* (- 1 t) v1) (* t v2))]))

;;; params-interpolate : ParamSet × Alist × Alist × Number → Alist
;;; Interpolate all parameters between two sets
(define (params-interpolate ps params1 params2 t)
  (doc 'export #t)
  (map (lambda (r)
         (let* ([name (cond
                        [(param-range? r) (param-range-name r)]
                        [(param-range-choice? r) (param-range-choice-name r)]
                        [else 'unnamed])]
                [v1 (param-get params1 name)]
                [v2 (param-get params2 name)])
           (cons name (param-interpolate r v1 v2 t))))
       (param-set-ranges ps)))

;;; ============================================================
;;; Part 5: Perturbation (for Distractors)
;;; ============================================================
;;;
;;; Perturbation generates slightly wrong parameter values
;;; for creating plausible distractors.

(doc 'section 'perturbation)

;;; param-perturb : ParamRange × Any × Number × RNG → Any
;;; Perturb a value by a relative amount (± epsilon)
(define (param-perturb r val epsilon rng)
  (doc 'export #t)
  (cond
    [(param-range? r)
     (let* ([min-v (param-range-min r)]
            [max-v (param-range-max r)]
            [range (- max-v min-v)]
            [delta (* range epsilon (- (* 2 (prng-next-float rng)) 1))]
            [result (+ val delta)])
       (max min-v (min max-v result)))]
    [(param-range-choice? r)
     ;; For discrete: randomly pick a different choice
     (let* ([choices (param-range-choice-values r)]
            [others (filter (lambda (c) (not (equal? c val))) choices)])
       (if (null? others)
           val
           (list-ref others (prng-next-int rng (length others)))))]
    [else val]))

;;; params-perturb-one : ParamSet × Alist × Symbol × Number × RNG → Alist
;;; Perturb a single named parameter
(define (params-perturb-one ps params name epsilon rng)
  (doc 'export #t)
  (map (lambda (r)
         (let ([rname (cond
                        [(param-range? r) (param-range-name r)]
                        [(param-range-choice? r) (param-range-choice-name r)]
                        [else 'unnamed])])
           (if (eq? rname name)
               (cons rname (param-perturb r (param-get params name) epsilon rng))
               (cons rname (param-get params rname)))))
       (param-set-ranges ps)))

;;; ============================================================
;;; Part 6: Text Interpolation
;;; ============================================================
;;;
;;; Interpolate parameter values into template strings.

(doc 'section 'text-interpolation)

;;; interpolate-text : String × Alist → String
;;; Replace ${name} placeholders with parameter values
(define (interpolate-text template params)
  (doc 'export #t)
  (doc 'description "Replace ${name} placeholders with parameter values from alist")
  (let loop ([chars (string->list template)]
             [result '()]
             [in-var #f]
             [var-chars '()])
    (cond
      [(null? chars)
       (if in-var
           ;; Unclosed variable - just append as-is
           (list->string (reverse (append var-chars (cons #\{ (cons #\$ result)))))
           (list->string (reverse result)))]

      [(and (not in-var) (char=? (car chars) #\$)
            (pair? (cdr chars)) (char=? (cadr chars) #\{))
       ;; Start of ${...}
       (loop (cddr chars) result #t '())]

      [(and in-var (char=? (car chars) #\}))
       ;; End of ${...}
       (let* ([var-name (string->symbol (list->string (reverse var-chars)))]
              [pair (assq var-name params)]
              [val-str (if pair
                          (format-param-value (cdr pair))
                          (string-append "${" (symbol->string var-name) "}"))])
         (loop (cdr chars)
               (append (reverse (string->list val-str)) result)
               #f '()))]

      [in-var
       ;; Accumulating variable name
       (loop (cdr chars) result #t (cons (car chars) var-chars))]

      [else
       ;; Normal character
       (loop (cdr chars) (cons (car chars) result) #f '())])))

;;; format-param-value : Any → String
;;; Format a parameter value for display
(define (format-param-value val)
  (doc 'export #t)
  (cond
    [(number? val)
     (if (integer? val)
         (number->string (inexact->exact val))
         (let ([s (number->string (exact->inexact val))])
           ;; Truncate to reasonable precision
           (if (> (string-length s) 6)
               (substring s 0 6)
               s)))]
    [(symbol? val) (symbol->string val)]
    [(string? val) val]
    [else (format "~a" val)]))
