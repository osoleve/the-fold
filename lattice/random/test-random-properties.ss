;;; lattice/random/test-random-properties.ss — QuickCheck properties for random API consistency

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'prng)
(require 'distributions)

;;; ============================================================================
;;; Helpers and generators
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define gen-seed
  (gen-int-range 0 1000000))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group random-properties

  (define-property "with-random is deterministic for a fixed seed"
    gen-seed
    (lambda (seed)
      (let ([a (with-random seed (random-list 10 random-float))]
            [b (with-random seed (random-list 10 random-float))])
        (equal? a b)))
    'tests 220)

  (define-property "random-float outputs are always in [0,1)"
    gen-seed
    (lambda (seed)
      (let ([xs (with-random seed (random-list 50 random-float))])
        (let loop ([ys xs])
          (or (null? ys)
              (and (>= (car ys) 0.0)
                   (< (car ys) 1.0)
                   (loop (cdr ys)))))))
    'tests 220)

  (define-property "bernoulli pmf normalizes"
    (gen-map (lambda (n) (/ n 100.0)) (gen-int-range 0 100))
    (lambda (p)
      (approx= (+ (bernoulli-pmf #t p)
                  (bernoulli-pmf #f p))
               1.0
               1e-12))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
