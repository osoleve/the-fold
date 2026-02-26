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
