;;; lattice/random/test-monte-carlo-properties.ss — QuickCheck properties for Monte Carlo summaries

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'monte-carlo)

;;; ============================================================================
;;; Helpers and generators
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define gen-nonempty-int-list
  (gen-bind (gen-int-range 1 40)
    (lambda (n)
      (gen-list-of n (gen-int-range -50 50)))))

(define gen-constant-samples
  (gen-bind (gen-int-range -30 30)
    (lambda (c)
      (gen-map (lambda (n) (cons c n))
               (gen-int-range 1 40)))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group monte-carlo-summary-properties

  (define-property "sample-mean of a constant list is the constant"
    gen-constant-samples
    (lambda (pair)
      (let* ([c (car pair)]
             [n (cdr pair)]
             [samples (make-list n c)])
        (= (sample-mean samples) c)))
    'tests 260)

  (define-property "sample-variance of a constant list is zero"
    gen-constant-samples
    (lambda (pair)
      (let* ([c (car pair)]
             [n (cdr pair)]
             [samples (make-list n c)])
        (approx= (sample-variance samples) 0 1e-12)))
    'tests 260)

  (define-property "quantile endpoints agree with min and max"
    gen-nonempty-int-list
    (lambda (xs)
      (and (= (sample-quantile xs 0.0) (sample-min xs))
           (= (sample-quantile xs 1.0) (sample-max xs))))
    'tests 220)

  (define-property "sample mean lies between min and max"
    gen-nonempty-int-list
    (lambda (xs)
      (let* ([mn (sample-min xs)]
             [mx (sample-max xs)]
             [mu (sample-mean xs)])
        (and (<= mn mu) (<= mu mx))))
    'tests 220)

  (define-property "effective-sample-size returns n for constant chains"
    gen-constant-samples
    (lambda (pair)
      (let* ([c (car pair)]
             [n (cdr pair)]
             [samples (make-list n c)])
        (= (effective-sample-size samples) n)))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
