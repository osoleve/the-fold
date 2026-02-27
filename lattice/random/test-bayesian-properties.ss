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

(define (mean xs)
  (/ (sum xs) (length xs)))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

(define (contains-symbol? x xs)
  (if (memq x xs) #t #f))

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

(define gen-beta-level-args
  (gen-bind gen-beta-prior-params
    (lambda (pair)
      (gen-map (lambda (lvl) (list (car pair) (cdr pair) lvl))
               (gen-elements '(0.90 0.95 0.99))))))

(define gen-normal-update-args
  (gen-bind (gen-int-range -300 300)
    (lambda (mu0-int)
      (gen-bind (gen-int-range 1 200)
        (lambda (var0-int)
          (gen-bind (gen-int-range -300 300)
            (lambda (xbar-int)
              (gen-bind (gen-int-range 1 40)
                (lambda (n)
                  (gen-map
                   (lambda (known-var-int)
                     (list (/ mu0-int 10.0)
                           (/ var0-int 10.0)
                           (/ xbar-int 10.0)
                           n
                           (/ known-var-int 10.0)))
                   (gen-int-range 1 200)))))))))))

(define gen-gamma-update-args
  (gen-bind (gen-int-range 1 40)
    (lambda (shape)
      (gen-bind (gen-int-range 1 40)
        (lambda (rate)
          (gen-bind (gen-int-range 0 120)
            (lambda (sum-obs)
              (gen-map (lambda (n) (list shape rate sum-obs n))
                       (gen-int-range 1 40)))))))))

(define gen-dirichlet-update-args
  (gen-bind (gen-int-range 2 6)
    (lambda (n)
      (gen-bind (gen-list-of n (gen-int-range 1 20))
        (lambda (alphas)
          (gen-map (lambda (counts) (list alphas counts))
                   (gen-list-of n (gen-int-range 0 30))))))))

(define gen-dirichlet-mode-alphas
  (gen-bind (gen-int-range 2 6)
    (lambda (n)
      (gen-list-of n (gen-int-range 2 20)))))

(define gen-beta-mass-args
  (gen-bind gen-beta-prior-params
    (lambda (pair)
      (gen-map (lambda (n) (list (car pair) (cdr pair) n))
               (gen-int-range 0 20)))))

(define gen-log-ml-args
  (gen-bind (gen-int-range -800 800)
    (lambda (a-int)
      (gen-map (lambda (b-int) (list (/ a-int 10.0) (/ b-int 10.0)))
               (gen-int-range -800 800)))))

(define gen-beta-sequential-args
  (gen-bind gen-beta-prior-params
    (lambda (pair)
      (gen-bind (gen-int-range 0 8)
        (lambda (m)
          (gen-map
           (lambda (obs) (list (car pair) (cdr pair) obs))
           (gen-list-of
            m
            (gen-bind (gen-int-range 0 20)
              (lambda (n)
                (gen-map (lambda (k) (cons k n))
                         (gen-int-range 0 n)))))))))))

(define gen-normal-sequential-args
  (gen-bind (gen-int-range -300 300)
    (lambda (mu0-int)
      (gen-bind (gen-int-range 1 200)
        (lambda (var0-int)
          (gen-bind (gen-int-range 1 200)
            (lambda (known-var-int)
              (gen-bind (gen-int-range 1 20)
                (lambda (n)
                  (gen-map
                   (lambda (obs-ints)
                     (list (/ mu0-int 10.0)
                           (/ var0-int 10.0)
                           (/ known-var-int 10.0)
                           (map (lambda (x) (/ x 10.0)) obs-ints)))
                   (gen-list-of n (gen-int-range -300 300))))))))))))

(define gen-gamma-sequential-args
  (gen-bind (gen-int-range 1 40)
    (lambda (shape)
      (gen-bind (gen-int-range 1 40)
        (lambda (rate)
          (gen-bind (gen-int-range 0 12)
            (lambda (n)
              (gen-map (lambda (obs) (list shape rate obs))
                       (gen-list-of n (gen-int-range 0 20))))))))))

(define gen-beta-self-params
  (gen-bind (gen-int-range 1 30)
    (lambda (a)
      (gen-map (lambda (b) (list a b))
               (gen-int-range 1 30)))))

(define gen-log-factorial-arg
  (gen-int-range 1 80))

(define gen-log-binom-args
  (gen-bind (gen-int-range 0 40)
    (lambda (n)
      (gen-map (lambda (k) (list n k))
               (gen-int-range 0 n)))))

(define gen-digamma-arg
  (gen-map (lambda (n) (/ n 10.0))
           (gen-int-range 10 250)))

(define gen-log-beta-pdf-args
  (gen-bind (gen-int-range 1 99)
    (lambda (x-int)
      (gen-bind (gen-int-range 1 50)
        (lambda (a-int)
          (gen-map (lambda (b-int)
                     (list (/ x-int 100.0)
                           (/ a-int 10.0)
                           (/ b-int 10.0)))
                   (gen-int-range 1 50)))))))

(define gen-log-normal-args
  (gen-bind (gen-int-range -200 200)
    (lambda (mu-int)
      (gen-bind (gen-int-range 1 200)
        (lambda (var-int)
          (gen-map (lambda (d-int)
                     (list (/ mu-int 10.0)
                           (/ var-int 10.0)
                           (/ d-int 10.0)))
                   (gen-int-range 0 120)))))))

(define gen-log-poisson-args
  (gen-bind (gen-int-range 0 60)
    (lambda (k)
      (gen-map (lambda (rate-int) (list k (/ rate-int 10.0)))
               (gen-int-range 1 200)))))

(define gen-beta-mode-args
  (gen-bind (gen-int-range 2 50)
    (lambda (a)
      (gen-map (lambda (b) (list a b))
               (gen-int-range 2 50)))))

(define gen-log-gamma-pdf-args
  (gen-bind (gen-int-range 1 200)
    (lambda (x-int)
      (gen-bind (gen-int-range 1 100)
        (lambda (shape-int)
          (gen-map (lambda (rate-int)
                     (list (/ x-int 10.0)
                           (/ shape-int 10.0)
                           (/ rate-int 10.0)))
                   (gen-int-range 1 100)))))))

(define gen-bic-aic-args
  (gen-bind (gen-int-range 2 500)
    (lambda (n)
      (gen-bind (gen-int-range -800 200)
        (lambda (ll-int)
          (gen-bind (gen-int-range 0 20)
            (lambda (k1)
              (gen-map (lambda (delta)
                         (list n (/ ll-int 10.0) k1 (+ k1 delta)))
                       (gen-int-range 0 20)))))))))

(define bayes-factor-labels
  '(decisive
    very-strong
    strong
    substantial
    barely-worth-mentioning
    barely-worth-mentioning-against
    substantial-against
    strong-against
    very-strong-against
    decisive-against))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group bayesian-properties

  (define-property "beta prior constructor satisfies predicate"
    gen-beta-prior-params
    (lambda (pair)
      (beta-prior? (make-beta-prior (car pair) (cdr pair))))
    'tests 220)

  (define-property "normal prior constructor satisfies predicate"
    gen-normal-update-args
    (lambda (args)
      (normal-prior? (make-normal-prior (car args) (cadr args))))
    'tests 220)

  (define-property "gamma prior constructor satisfies predicate"
    gen-gamma-update-args
    (lambda (args)
      (gamma-prior? (make-gamma-prior (car args) (cadr args))))
    'tests 220)

  (define-property "dirichlet prior constructor satisfies predicate"
    gen-dirichlet-alphas
    (lambda (alphas)
      (dirichlet-prior? (make-dirichlet-prior alphas)))
    'tests 220)

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

  (define-property "beta posterior mode follows closed form for alpha,beta>1"
    gen-beta-mode-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [prior (make-beta-prior a b)]
             [mode (beta-posterior-mode prior)])
        (approx= mode (/ (- a 1) (- (+ a b) 2)) 1e-12)))
    'tests 220)

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

  (define-property "beta posterior variance is positive and bounded by 1/4"
    gen-beta-prior-params
    (lambda (pair)
      (let* ([prior (make-beta-prior (car pair) (cdr pair))]
             [v (beta-posterior-variance prior)])
        (and (> v 0)
             (<= v 0.25))))
    'tests 220)

  (define-property "beta credible interval is ordered and contains posterior mean"
    gen-beta-level-args
    (lambda (args)
      (let* ([alpha (car args)]
             [beta (cadr args)]
             [level (caddr args)]
             [prior (make-beta-prior alpha beta)]
             [mu (beta-posterior-mean prior)]
             [ci (beta-credible-interval prior level)])
        (and (<= (car ci) (cdr ci))
             (<= (car ci) mu)
             (<= mu (cdr ci))
             (>= (car ci) 0.0)
             (<= (cdr ci) 1.0))))
    'tests 220)

  (define-property "normal-normal posterior variance shrinks and CI contains mean"
    gen-normal-update-args
    (lambda (args)
      (let* ([mu0 (car args)]
             [var0 (cadr args)]
             [xbar (caddr args)]
             [n (cadddr args)]
             [known-var (list-ref args 4)]
             [prior (make-normal-prior mu0 var0)]
             [post (normal-normal-posterior prior xbar n known-var)]
             [ci (normal-posterior-credible-interval post 0.95)]
             [mu-post (normal-prior-mean post)])
        (and (> (normal-prior-variance post) 0)
             (<= (normal-prior-variance post) var0)
             (<= (car ci) mu-post)
             (<= mu-post (cdr ci))
             (< (car ci) (cdr ci)))))
    'tests 220)

  (define-property "gamma-poisson posterior adds observations to shape/rate"
    gen-gamma-update-args
    (lambda (args)
      (let* ([shape (car args)]
             [rate (cadr args)]
             [sum-obs (caddr args)]
             [n (cadddr args)]
             [prior (make-gamma-prior shape rate)]
             [post (gamma-poisson-posterior prior sum-obs n)])
        (and (= (gamma-prior-shape post) (+ shape sum-obs))
             (= (gamma-prior-rate post) (+ rate n))
             (> (gamma-posterior-mean post) 0)
             (> (gamma-posterior-variance post) 0)
             (>= (gamma-posterior-mode post) 0))))
    'tests 220)

  (define-property "dirichlet-multinomial posterior adds counts elementwise"
    gen-dirichlet-update-args
    (lambda (args)
      (let* ([alphas (car args)]
             [counts (cadr args)]
             [prior (make-dirichlet-prior alphas)]
             [post (dirichlet-multinomial-posterior prior counts)]
             [post-alphas (dirichlet-prior-alphas post)])
        (equal? post-alphas (map + alphas counts))))
    'tests 220)

  (define-property "dirichlet posterior mode lies on simplex when alphas > 1"
    gen-dirichlet-mode-alphas
    (lambda (alphas)
      (let* ([prior (make-dirichlet-prior alphas)]
             [mode (dirichlet-posterior-mode prior)])
        (and (= (length mode) (length alphas))
             (all-satisfy? (lambda (x) (> x 0.0)) mode)
             (approx= (sum mode) 1.0 1e-10))))
    'tests 220)

  (define-property "predictive-beta-binomial normalizes over k = 0..n"
    gen-beta-mass-args
    (lambda (args)
      (let* ([alpha (car args)]
             [beta (cadr args)]
             [n (caddr args)]
             [prior (make-beta-prior alpha beta)]
             [mass (sum (map (lambda (k)
                               (predictive-beta-binomial prior k n))
                             (iota (+ n 1))))])
        (approx= mass 1.0 1e-8)))
    'tests 200)

  (define-property "bayes-factor agrees with log-bayes-factor"
    gen-log-ml-args
    (lambda (args)
      (let* ([log-ml-1 (car args)]
             [log-ml-2 (cadr args)]
             [bf (bayes-factor log-ml-1 log-ml-2)]
             [log-bf (log-bayes-factor log-ml-1 log-ml-2)])
        (approx= bf (exp log-bf) 1e-10)))
    'tests 220)

  (define-property "interpret-bayes-factor returns a known evidence label"
    (gen-map (lambda (n) (/ n 10.0))
             (gen-int-range 1 5000))
    (lambda (bf)
      (contains-symbol? (interpret-bayes-factor bf) bayes-factor-labels))
    'tests 220)

  (define-property "BIC and AIC are monotone in parameter count"
    gen-bic-aic-args
    (lambda (args)
      (let* ([n (car args)]
             [ll (cadr args)]
             [k1 (caddr args)]
             [k2 (cadddr args)])
        (and (<= (bic ll k1 n) (bic ll k2 n))
             (<= (aic ll k1) (aic ll k2)))))
    'tests 220)

  (define-property "sequential beta update matches aggregated-count update"
    gen-beta-sequential-args
    (lambda (args)
      (let* ([alpha (car args)]
             [beta (cadr args)]
             [obs (caddr args)]
             [prior (make-beta-prior alpha beta)]
             [seq-post (sequential-beta-update prior obs)]
             [k-total (sum (map car obs))]
             [n-total (sum (map cdr obs))]
             [batch-post (beta-binomial-posterior prior k-total n-total)])
        (and (= (beta-prior-alpha seq-post) (beta-prior-alpha batch-post))
             (= (beta-prior-beta seq-post) (beta-prior-beta batch-post)))))
    'tests 220)

  (define-property "sequential normal update matches one-shot posterior from sample mean"
    gen-normal-sequential-args
    (lambda (args)
      (let* ([mu0 (car args)]
             [var0 (cadr args)]
             [known-var (caddr args)]
             [obs (cadddr args)]
             [prior (make-normal-prior mu0 var0)]
             [seq-post (sequential-normal-update prior obs known-var)]
             [batch-post (normal-normal-posterior prior (mean obs) (length obs) known-var)])
        (and (approx= (normal-prior-mean seq-post)
                      (normal-prior-mean batch-post)
                      1e-10)
             (approx= (normal-prior-variance seq-post)
                      (normal-prior-variance batch-post)
                      1e-10))))
    'tests 180)

  (define-property "sequential gamma update matches aggregated update"
    gen-gamma-sequential-args
    (lambda (args)
      (let* ([shape (car args)]
             [rate (cadr args)]
             [obs (caddr args)]
             [prior (make-gamma-prior shape rate)]
             [seq-post (sequential-gamma-update prior obs)]
             [batch-post (gamma-poisson-posterior prior (sum obs) (length obs))])
        (and (= (gamma-prior-shape seq-post) (gamma-prior-shape batch-post))
             (= (gamma-prior-rate seq-post) (gamma-prior-rate batch-post)))))
    'tests 220)

  (define-property "posterior-summary for beta includes expected keys"
    gen-beta-prior-params
    (lambda (pair)
      (let* ([summary (posterior-summary
                       (make-beta-prior (car pair) (cdr pair)))])
        (and (equal? (cdr (assq 'type summary)) 'beta)
             (assq 'mean summary)
             (assq 'mode summary)
             (assq 'variance summary)
             (assq 'ci-95 summary))))
    'tests 180)

  (define-property "log-factorial is monotone nondecreasing"
    gen-log-factorial-arg
    (lambda (n)
      (<= (log-factorial n)
          (log-factorial (+ n 1))))
    'tests 220)

  (define-property "log-binomial-coeff is symmetric in k and n-k"
    gen-log-binom-args
    (lambda (args)
      (let ([n (car args)] [k (cadr args)])
        (approx= (log-binomial-coeff n k)
                 (log-binomial-coeff n (- n k))
                 1e-10)))
    'tests 220)

  (define-property "log-marginal-likelihood-normal-normal returns 0 for empty observations"
    gen-normal-update-args
    (lambda (args)
      (let* ([mu0 (car args)]
             [var0 (cadr args)]
             [known-var (list-ref args 4)]
             [prior (make-normal-prior mu0 var0)])
        (= (log-marginal-likelihood-normal-normal prior '() known-var) 0)))
    'tests 180)

  (define-property "kl-divergence-normal is zero when distributions are identical"
    gen-normal-update-args
    (lambda (args)
      (let* ([mu0 (car args)]
             [var0 (cadr args)]
             [prior (make-normal-prior mu0 var0)])
        (approx= (kl-divergence-normal prior prior) 0.0 1e-12)))
    'tests 200)

  (define-property "kl-divergence-beta is near zero when distributions are identical"
    gen-beta-self-params
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [prior (make-beta-prior a b)])
        (approx= (kl-divergence-beta prior prior) 0.0 5e-3)))
    'tests 200)

  (define-property "digamma recurrence psi(x+1)=psi(x)+1/x"
    gen-digamma-arg
    (lambda (x)
      (approx= (digamma (+ x 1.0))
               (+ (digamma x) (/ 1.0 x))
               2e-2))
    'tests 200)

  (define-property "log-beta-pdf is finite on interior support"
    gen-log-beta-pdf-args
    (lambda (args)
      (let* ([x (car args)]
             [a (cadr args)]
             [b (caddr args)]
             [lp (log-beta-pdf x a b)])
        (and (not (= lp +inf.0))
             (not (= lp -inf.0)))))
    'tests 220)

  (define-property "log-normal-pdf is symmetric around mean"
    gen-log-normal-args
    (lambda (args)
      (let* ([mu (car args)]
             [var (cadr args)]
             [d (caddr args)])
        (approx= (log-normal-pdf (+ mu d) mu var)
                 (log-normal-pdf (- mu d) mu var)
                 1e-10)))
    'tests 220)

  (define-property "log-poisson-pmf is never positive"
    gen-log-poisson-args
    (lambda (args)
      (let ([k (car args)] [rate (cadr args)])
        (<= (log-poisson-pmf k rate) 0.0)))
    'tests 220)

  (define-property "log-gamma-pdf is finite on positive support"
    gen-log-gamma-pdf-args
    (lambda (args)
      (let* ([x (car args)]
             [shape (cadr args)]
             [rate (caddr args)]
             [lp (log-gamma-pdf x shape rate)])
        (and (not (= lp +inf.0))
             (not (= lp -inf.0)))))
    'tests 220)

  (define-property "log-marginal-likelihood-beta-binomial is finite"
    gen-beta-obs
    (lambda (args)
      (let* ([alpha (car args)]
             [beta (cadr args)]
             [k (caddr args)]
             [n (cadddr args)]
             [prior (make-beta-prior alpha beta)]
             [v (log-marginal-likelihood-beta-binomial prior k n)])
        (and (not (= v +inf.0))
             (not (= v -inf.0)))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
