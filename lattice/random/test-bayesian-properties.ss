;;; lattice/random/test-bayesian-properties.ss — QuickCheck properties for Bayesian updates

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'bayesian)

;;; ============================================================================
;;; Helpers and generators
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (sum xs)
  (fold-left + 0 xs))

(define gen-beta-obs
  (gen-bind (gen-int-range 1 30)
    (lambda (alpha)
      (gen-bind (gen-int-range 1 30)
        (lambda (beta)
          (gen-bind (gen-int-range 0 40)
            (lambda (n)
              (gen-map (lambda (k) (list alpha beta k n))
                       (gen-int-range 0 n)))))))))

(define gen-beta-prior-params
  (gen-bind (gen-int-range 1 50)
    (lambda (alpha)
      (gen-map (lambda (beta) (cons alpha beta))
               (gen-int-range 1 50)))))

(define gen-dirichlet-alphas
  (gen-bind (gen-int-range 1 6)
    (lambda (n)
      (gen-list-of n (gen-int-range 1 20)))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group bayesian-properties

  (define-property "beta-binomial posterior adds observed counts"
    gen-beta-obs
    (lambda (args)
      (let* ([alpha (car args)]
             [beta (cadr args)]
             [k (caddr args)]
             [n (cadddr args)]
             [prior (make-beta-prior alpha beta)]
             [post (beta-binomial-posterior prior k n)])
        (and (= (beta-prior-alpha post) (+ alpha k))
             (= (beta-prior-beta post) (+ beta (- n k))))))
    'tests 260)

  (define-property "beta posterior mean is in [0,1]"
    gen-beta-prior-params
    (lambda (pair)
      (let* ([alpha (car pair)]
             [beta (cdr pair)]
             [mu (beta-posterior-mean (make-beta-prior alpha beta))])
        (and (>= mu 0) (<= mu 1))))
    'tests 240)

  (define-property "predictive mean equals n * posterior mean"
    (gen-bind gen-beta-prior-params
      (lambda (pair)
        (gen-map (lambda (n) (list (car pair) (cdr pair) n))
                 (gen-int-range 0 40))))
    (lambda (args)
      (let* ([alpha (car args)]
             [beta (cadr args)]
             [n (caddr args)]
             [prior (make-beta-prior alpha beta)]
             [lhs (predictive-mean-beta-binomial prior n)]
             [rhs (* n (beta-posterior-mean prior))])
        (approx= lhs rhs 1e-12)))
    'tests 220)

  (define-property "dirichlet posterior mean entries sum to 1"
    gen-dirichlet-alphas
    (lambda (alphas)
      (let* ([prior (make-dirichlet-prior alphas)]
             [mu (dirichlet-posterior-mean prior)])
        (and (= (length mu) (length alphas))
             (approx= (sum mu) 1.0 1e-12))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
