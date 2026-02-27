;;; lattice/random/test-probability-properties.ss — QuickCheck properties for probability monad

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'probability)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

(define (sum xs)
  (fold-left + 0.0 xs))

(define (int->small-float n)
  (/ n 100.0))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-seed
  (gen-int-range 0 1000000))

(define gen-small-int
  (gen-int-range -200 200))

(define gen-small-float
  (gen-map int->small-float (gen-int-range -1000 1000)))

(define gen-range-args
  (gen-bind (gen-int-range -100 100)
    (lambda (lo)
      (gen-map (lambda (width) (list lo (+ lo width)))
               (gen-int-range 0 200)))))

(define gen-seed-count
  (gen-bind gen-seed
    (lambda (seed)
      (gen-map (lambda (n) (cons seed n))
               (gen-int-range 0 60)))))

(define gen-seed-seed-weight
  (gen-bind gen-seed
    (lambda (seed1)
      (gen-bind gen-seed
        (lambda (seed2)
          (gen-map (lambda (w-int) (list seed1 seed2 (int->small-float w-int)))
                   (gen-int-range -300 300)))))))

(define gen-seed-seed-weight-delta
  (gen-bind gen-seed-seed-weight
    (lambda (args)
      (gen-map (lambda (d-int)
                 (list (car args)
                       (cadr args)
                       (caddr args)
                       (int->small-float d-int)))
               (gen-int-range -200 200)))))

(define gen-seed-int-list
  (gen-bind gen-seed
    (lambda (seed)
      (gen-bind (gen-int-range 0 12)
        (lambda (n)
          (gen-map (lambda (xs) (cons seed xs))
                   (gen-list-of n gen-small-int)))))))

(define gen-nonempty-log-weights
  (gen-bind (gen-int-range 1 8)
    (lambda (n)
      (gen-map (lambda (ints) (map int->small-float ints))
               (gen-list-of n (gen-int-range -500 500))))))

(define gen-logsumexp-args
  (gen-bind gen-nonempty-log-weights
    (lambda (lws)
      (gen-map (lambda (shift) (cons lws (int->small-float shift)))
               (gen-int-range -300 300)))))

;;; ============================================================================
;;; Monad laws and core behavior
;;; ============================================================================

(test-group probability-monad-properties

  (define-property "prob-pure yields value and zero weight"
    (gen-bind gen-small-int
      (lambda (x)
        (gen-map (lambda (seed) (cons x seed)) gen-seed)))
    (lambda (pair)
      (let* ([x (car pair)]
             [seed (cdr pair)]
             [prng (make-pcg seed 1)]
             [out (run-prob (prob-pure x) prng)])
        (and (= (car (car out)) x)
             (= (cdr (car out)) 0.0))))
    'tests 240)

  (define-property "left identity for prob-bind"
    (gen-bind gen-small-int
      (lambda (x)
        (gen-map (lambda (seed) (cons x seed)) gen-seed)))
    (lambda (pair)
      (let* ([x (car pair)]
             [seed (cdr pair)]
             [prng (make-pcg seed 1)]
             [f (lambda (n) (prob-pure (+ (* 2 n) 3)))]
             [lhs (run-prob (prob-bind (prob-pure x) f) prng)]
             [rhs (run-prob (f x) prng)])
        (equal? lhs rhs)))
    'tests 220)

  (define-property "right identity for prob-bind"
    (gen-bind gen-range-args
      (lambda (bounds)
        (gen-map (lambda (seed) (list seed (car bounds) (cadr bounds)))
                 gen-seed)))
    (lambda (args)
      (let* ([seed (car args)]
             [lo (cadr args)]
             [hi (caddr args)]
             [prng (make-pcg seed 1)]
             [p (prob-uniform-int lo hi)]
             [lhs (run-prob (prob-bind p prob-pure) prng)]
             [rhs (run-prob p prng)])
        (equal? lhs rhs)))
    'tests 220)

  (define-property "associativity for prob-bind with pure f and g"
    (gen-bind gen-range-args
      (lambda (bounds)
        (gen-map (lambda (seed) (list seed (car bounds) (cadr bounds)))
                 gen-seed)))
    (lambda (args)
      (let* ([seed (car args)]
             [lo (cadr args)]
             [hi (caddr args)]
             [prng (make-pcg seed 1)]
             [p (prob-uniform-int lo hi)]
             [f (lambda (x) (prob-pure (+ x 1)))]
             [g (lambda (y) (prob-pure (* y 2)))]
             [lhs (run-prob (prob-bind (prob-bind p f) g) prng)]
             [rhs (run-prob (prob-bind p
                                       (lambda (x)
                                         (prob-bind (f x) g)))
                            prng)])
        (equal? lhs rhs)))
    'tests 200)

  (define-property "prob-map identity"
    (gen-bind gen-range-args
      (lambda (bounds)
        (gen-map (lambda (seed) (list seed (car bounds) (cadr bounds)))
                 gen-seed)))
    (lambda (args)
      (let* ([seed (car args)]
             [lo (cadr args)]
             [hi (caddr args)]
             [prng (make-pcg seed 1)]
             [p (prob-uniform-int lo hi)]
             [lhs (run-prob (prob-map (lambda (x) x) p) prng)]
             [rhs (run-prob p prng)])
        (equal? lhs rhs)))
    'tests 180)

  (define-property "prob-map composition"
    (gen-bind gen-range-args
      (lambda (bounds)
        (gen-map (lambda (seed) (list seed (car bounds) (cadr bounds)))
                 gen-seed)))
    (lambda (args)
      (let* ([seed (car args)]
             [lo (cadr args)]
             [hi (caddr args)]
             [prng (make-pcg seed 1)]
             [p (prob-uniform-int lo hi)]
             [f (lambda (x) (+ x 4))]
             [g (lambda (x) (* x 3))]
             [lhs (run-prob (prob-map (lambda (x) (f (g x))) p) prng)]
             [rhs (run-prob (prob-map f (prob-map g p)) prng)])
        (equal? lhs rhs)))
    'tests 180)
)

;;; ============================================================================
;;; Probability state surface
;;; ============================================================================

(test-group probability-state-surface-properties

  (define-property "make-prob creates prob values with accessible state"
    (gen-bind gen-small-int
      (lambda (x)
        (gen-map (lambda (seed) (cons x seed)) gen-seed)))
    (lambda (pair)
      (let* ([x (car pair)]
             [seed (cdr pair)]
             [base (prob-pure x)]
             [p (make-prob (prob-state base))]
             [out (run-prob p (make-pcg seed 1))])
        (and (prob? p)
             (equal? (prob-state p) (prob-state base))
             (= (car (car out)) x))))
    'tests 220)

  (define-property "prob-get-prng and prob-put-prng update PRNG part of state"
    gen-seed-seed-weight
    (lambda (args)
      (let* ([seed1 (car args)]
             [seed2 (cadr args)]
             [w (caddr args)]
             [old-prng (make-pcg seed1 1)]
             [new-prng (make-pcg seed2 7)]
             [st (cons old-prng w)]
             [get-res (run-state prob-get-prng st)]
             [put-res (run-state (prob-put-prng new-prng) st)]
             [put-state (cdr put-res)])
        (and (equal? (car get-res) old-prng)
             (equal? (cdr get-res) st)
             (null? (car put-res))
             (equal? (car put-state) new-prng)
             (approx= (cdr put-state) w 1e-12))))
    'tests 220)

  (define-property "prob-get-weight and prob-add-weight update weight part of state"
    gen-seed-seed-weight-delta
    (lambda (args)
      (let* ([seed1 (car args)]
             [seed2 (cadr args)]
             [w (caddr args)]
             [dw (cadddr args)]
             [prng (make-pcg seed1 seed2)]
             [st (cons prng w)]
             [get-res (run-state prob-get-weight st)]
             [add-res (run-state (prob-add-weight dw) st)]
             [add-state (cdr add-res)])
        (and (approx= (car get-res) w 1e-12)
             (equal? (cdr get-res) st)
             (null? (car add-res))
             (equal? (car add-state) prng)
             (approx= (cdr add-state) (+ w dw) 1e-12))))
    'tests 220)

  (define-property "sample-prob matches value from run-prob"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (bounds) (list seed (car bounds) (cadr bounds)))
                 gen-range-args)))
    (lambda (args)
      (let* ([seed (car args)]
             [lo (cadr args)]
             [hi (caddr args)]
             [prng (make-pcg seed 1)]
             [p (prob-uniform-int lo hi)]
             [sampled (sample-prob p prng)]
             [ran (run-prob p prng)])
        (= sampled (car (car ran)))))
    'tests 220)

  (define-property "prob-sequence preserves order of pure values"
    gen-seed-int-list
    (lambda (pair)
      (let* ([seed (car pair)]
             [xs (cdr pair)]
             [probs (map prob-pure xs)]
             [seq (prob-sequence probs)]
             [out (sample-prob seq (make-pcg seed 1))])
        (equal? out xs)))
    'tests 220)
)

;;; ============================================================================
;;; Sampling and weighting invariants
;;; ============================================================================

(test-group probability-sampling-properties

  (define-property "sample-many length equals request"
    gen-seed-count
    (lambda (pair)
      (let ([seed (car pair)]
            [n (cdr pair)])
        (= (length (sample-many prob-uniform seed n))
           n)))
    'tests 220)

  (define-property "weighted-samples carries value and weight"
    (gen-bind gen-small-float
      (lambda (w)
        (gen-bind gen-seed
          (lambda (seed)
            (gen-map (lambda (n) (list w seed n))
                     (gen-int-range 0 40))))))
    (lambda (args)
      (let* ([w (car args)]
             [seed (cadr args)]
             [n (caddr args)]
             [p (prob-then (factor w) (prob-pure 42))]
             [xs (weighted-samples p seed n)])
        (and (= (length xs) n)
             (all-satisfy? (lambda (vw)
                             (and (= (car vw) 42)
                                  (approx= (cdr vw) w 1e-12)))
                           xs))))
    'tests 200)

  (define-property "factor weights add under sequencing"
    (gen-bind gen-small-float
      (lambda (w1)
        (gen-map (lambda (w2) (cons w1 w2)) gen-small-float)))
	    (lambda (pair)
	      (let* ([w1 (car pair)]
	             [w2 (cdr pair)]
	             [p (prob-then (factor w1)
	                           (prob-then (factor w2)
	                                      (prob-pure 0)))]
	             [w (weight-prob p (make-pcg 123 1))])
	        (approx= w (+ w1 w2) 1e-12)))
	    'tests 220)

  (define-property "condition true keeps zero weight and false gives -inf"
    gen-small-int
    (lambda (x)
      (let* ([p-true (condition (lambda (y) (= y x)) (prob-pure x))]
             [p-false (condition (lambda (y) (= y (+ x 1))) (prob-pure x))]
             [w-true (weight-prob p-true (make-pcg 77 1))]
             [w-false (weight-prob p-false (make-pcg 77 1))])
        (and (= w-true 0.0)
             (= w-false -inf.0))))
    'tests 200)
)

;;; ============================================================================
;;; Numeric helper properties
;;; ============================================================================

(test-group probability-numeric-helper-properties

  (define-property "normalize-log-weights exponentiates to 1"
    gen-nonempty-log-weights
    (lambda (lws)
      (let* ([norm (normalize-log-weights lws)]
             [s (sum (map exp-num norm))])
        (approx= s 1.0 1e-10)))
    'tests 220)

  (define-property "log-sum-exp is translation invariant"
    gen-logsumexp-args
    (lambda (args)
      (let* ([xs (car args)]
             [c (cdr args)]
             [lhs (log-sum-exp (map (lambda (x) (+ x c)) xs))]
             [rhs (+ c (log-sum-exp xs))])
        (approx= lhs rhs 1e-10)))
    'tests 220)

  (define-property "importance expectation of constant function is constant"
    (gen-bind gen-seed
      (lambda (seed)
        (gen-map (lambda (c) (cons seed c))
                 gen-small-float)))
    (lambda (pair)
      (let* ([seed (car pair)]
             [c (cdr pair)]
             [estimate (importance-expectation (lambda (_) c)
                                               prob-uniform
                                               seed
                                               60)])
        (approx= estimate c 1e-10)))
    'tests 150)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
