;;; Test harness for core/random/bayesian.ss -- Bayesian Inference Engine

(load "core/lang/module.ss")
(load "core/base/prelude.ss")
(load "lattice/random/bayesian.ss")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  ✗ ")
       (display name)
       (display "\n    expected: ")
       (display expected)
       (display "\n    got: ")
       (display actual)
       (newline))))

(define (test-approx name expected actual tolerance)
  (if (< (abs (- expected actual)) tolerance)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "  ✓ ")
       (display name)
       (newline))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "  ✗ ")
       (display name)
       (display "\n    expected: ")
       (display expected)
       (display " (± ")
       (display tolerance)
       (display ")\n    got: ")
       (display actual)
       (newline))))

(define (test-true name expr)
  (test name #t (if expr #t #f)))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Beta-Binomial Tests
;;; ====
(test-section "Beta-Binomial Conjugate Prior")

;; Prior construction
(define beta1 (make-beta-prior 1 1))  ; Uniform prior
(test-true "beta-prior? recognizes beta prior" (beta-prior? beta1))
(test "beta-prior-alpha extracts alpha" 1 (beta-prior-alpha beta1))
(test "beta-prior-beta extracts beta" 1 (beta-prior-beta beta1))

;; Uniform prior has mean 0.5
(test-approx "uniform prior mean is 0.5" 0.5 (beta-posterior-mean beta1) 0.001)

;; Posterior update: 7 successes in 10 trials
(define beta-post1 (beta-binomial-posterior beta1 7 10))
(test "posterior alpha updated" 8 (beta-prior-alpha beta-post1))
(test "posterior beta updated" 4 (beta-prior-beta beta-post1))

;; Posterior mean should be close to 7/10 = 0.7 but pulled toward 0.5
;; Mean = 8/12 = 0.667
(test-approx "posterior mean after observations" 0.667 (beta-posterior-mean beta-post1) 0.01)

;; Informative prior: Beta(10, 10) centered at 0.5
(define beta-informative (make-beta-prior 10 10))
(test-approx "informative prior mean" 0.5 (beta-posterior-mean beta-informative) 0.001)

;; Strong prior resists change
(define beta-post2 (beta-binomial-posterior beta-informative 7 10))
;; Mean = (10+7)/(10+10+10) = 17/30 = 0.567
(test-approx "strong prior resists extreme data" 0.567 (beta-posterior-mean beta-post2) 0.01)

;; Variance decreases with more data
(define var-before (beta-posterior-variance beta1))
(define var-after (beta-posterior-variance beta-post1))
(test-true "variance decreases with data" (< var-after var-before))

;; Posterior mode (for alpha, beta > 1)
(define beta-mode (make-beta-prior 5 3))
;; Mode = (5-1)/(5+3-2) = 4/6 = 0.667
(test-approx "posterior mode calculation" 0.667 (beta-posterior-mode beta-mode) 0.01)

;; Credible interval
(define ci (beta-credible-interval beta-post1 0.95))
(test-true "credible interval is valid" (and (pair? ci)
                                             (< (car ci) (beta-posterior-mean beta-post1))
                                             (> (cdr ci) (beta-posterior-mean beta-post1))))

;;; ====
;;; Normal-Normal Tests
;;; ====
(test-section "Normal-Normal Conjugate Prior")

;; Prior construction
(define norm1 (make-normal-prior 0 1))  ; Standard normal prior
(test-true "normal-prior? recognizes normal prior" (normal-prior? norm1))
(test "normal-prior-mean extracts mean" 0 (normal-prior-mean norm1))
(test "normal-prior-variance extracts variance" 1 (normal-prior-variance norm1))

;; Update with observations: sample mean 2.0, n=5, known variance=1
(define norm-post1 (normal-normal-posterior norm1 2.0 5 1.0))
;; Posterior precision = 1 + 5 = 6
;; Posterior mean = (0*1 + 2*5*1) / 6 = 10/6 = 1.667
(test-approx "normal posterior mean" 1.667 (normal-prior-mean norm-post1) 0.01)
;; Posterior variance = 1/6 = 0.167
(test-approx "normal posterior variance" 0.167 (normal-prior-variance norm-post1) 0.01)

;; With more observations, mean should be closer to data
(define norm-post2 (normal-normal-posterior norm1 2.0 100 1.0))
;; Posterior precision = 1 + 100 = 101
;; Posterior mean = (0*1 + 2*100*1) / 101 ≈ 1.98
(test-approx "many observations dominate prior" 1.98 (normal-prior-mean norm-post2) 0.02)

;; Strong prior resists change
(define norm-strong (make-normal-prior 0 0.01))  ; Very small variance = very certain
(define norm-post3 (normal-normal-posterior norm-strong 2.0 5 1.0))
;; Posterior mean should be much closer to 0 than to 2
(test-true "strong prior dominates few observations"
           (< (normal-prior-mean norm-post3) 1.0))

;; Credible interval
(define norm-ci (normal-posterior-credible-interval norm-post1 0.95))
(test-true "normal credible interval is valid"
           (and (pair? norm-ci)
                (< (car norm-ci) (normal-prior-mean norm-post1))
                (> (cdr norm-ci) (normal-prior-mean norm-post1))))

;;; ====
;;; Gamma-Poisson Tests
;;; ====
(test-section "Gamma-Poisson Conjugate Prior")

;; Prior construction
(define gamma1 (make-gamma-prior 1 1))  ; Exp(1) prior on rate
(test-true "gamma-prior? recognizes gamma prior" (gamma-prior? gamma1))
(test "gamma-prior-shape extracts shape" 1 (gamma-prior-shape gamma1))
(test "gamma-prior-rate extracts rate" 1 (gamma-prior-rate gamma1))

;; Prior mean = shape/rate = 1
(test-approx "gamma prior mean" 1.0 (gamma-posterior-mean gamma1) 0.001)

;; Update with Poisson observations: sum=20, n=5 (mean rate ~4)
(define gamma-post1 (gamma-poisson-posterior gamma1 20 5))
;; Posterior shape = 1 + 20 = 21
;; Posterior rate = 1 + 5 = 6
(test "gamma posterior shape" 21 (gamma-prior-shape gamma-post1))
(test "gamma posterior rate" 6 (gamma-prior-rate gamma-post1))
;; Posterior mean = 21/6 = 3.5
(test-approx "gamma posterior mean" 3.5 (gamma-posterior-mean gamma-post1) 0.01)

;; Variance should decrease
(define gamma-var-before (gamma-posterior-variance gamma1))
(define gamma-var-after (gamma-posterior-variance gamma-post1))
(test-true "gamma variance decreases with data" (< gamma-var-after gamma-var-before))

;; Posterior mode (for shape >= 1)
;; Mode = (21-1)/6 = 20/6 = 3.33
(test-approx "gamma posterior mode" 3.333 (gamma-posterior-mode gamma-post1) 0.01)

;;; ====
;;; Dirichlet-Multinomial Tests
;;; ====
(test-section "Dirichlet-Multinomial Conjugate Prior")

;; Prior construction: symmetric Dirichlet(1,1,1) = uniform over simplex
(define dir1 (make-dirichlet-prior '(1 1 1)))
(test-true "dirichlet-prior? recognizes dirichlet prior" (dirichlet-prior? dir1))
(test "dirichlet-prior-alphas extracts alphas" '(1 1 1) (dirichlet-prior-alphas dir1))

;; Uniform prior mean = (1/3, 1/3, 1/3)
(define dir-mean (dirichlet-posterior-mean dir1))
(test-approx "dirichlet uniform mean component" 0.333 (car dir-mean) 0.01)

;; Update with counts (10, 5, 5)
(define dir-post1 (dirichlet-multinomial-posterior dir1 '(10 5 5)))
(test "dirichlet posterior alphas" '(11 6 6) (dirichlet-prior-alphas dir-post1))

;; Posterior mean should reflect observed proportions
(define dir-post-mean (dirichlet-posterior-mean dir-post1))
;; Mean = (11, 6, 6) / 23 ≈ (0.478, 0.261, 0.261)
(test-approx "dirichlet posterior mean[0]" 0.478 (car dir-post-mean) 0.01)
(test-approx "dirichlet posterior mean[1]" 0.261 (cadr dir-post-mean) 0.01)

;; Posterior mode (when all alphas > 1)
(define dir-mode (dirichlet-posterior-mode dir-post1))
;; Mode = (10, 5, 5) / 20 = (0.5, 0.25, 0.25)
(test-approx "dirichlet posterior mode[0]" 0.5 (car dir-mode) 0.01)

;;; ====
;;; Log Probability Functions Tests
;;; ====
(test-section "Log Probability Functions")

;; log-beta: B(1,1) = 1, so log B(1,1) = 0
(test-approx "log-beta(1,1) = 0" 0 (log-beta 1 1) 0.001)

;; log-beta(2,2) = B(2,2) = Gamma(2)^2/Gamma(4) = 1*1/6 = 1/6
;; log(1/6) ≈ -1.79
(test-approx "log-beta(2,2)" -1.79 (log-beta 2 2) 0.1)

;; log-normal-pdf at mean should be maximum
(define log-p-at-mean (log-normal-pdf 0 0 1))
;; log(1/sqrt(2*pi)) ≈ -0.919
(test-approx "log-normal-pdf at mean" -0.919 log-p-at-mean 0.01)

;; log-gamma-pdf: Gamma(1,1) = Exp(1) at x=1
;; PDF = e^(-1) ≈ 0.368, log ≈ -1
(test-approx "log-gamma-pdf Exp(1) at 1" -1.0 (log-gamma-pdf 1 1 1) 0.01)

;; log-poisson-pmf: Poisson(5) at k=5
;; PMF = e^(-5) * 5^5 / 5! = e^(-5) * 3125 / 120 ≈ 0.175
;; log ≈ -1.74
(test-approx "log-poisson-pmf(5,5)" -1.74 (log-poisson-pmf 5 5) 0.1)

;;; ====
;;; Digamma Function Tests
;;; ====
(test-section "Digamma Function")

;; digamma(1) = -gamma (Euler-Mascheroni) ≈ -0.5772
(test-approx "digamma(1)" -0.5772 (digamma 1) 0.01)

;; digamma(2) = 1 - gamma ≈ 0.4228
(test-approx "digamma(2)" 0.4228 (digamma 2) 0.01)

;; digamma(10) ≈ 2.2517
(test-approx "digamma(10)" 2.25 (digamma 10) 0.05)

;;; ====
;;; KL Divergence Tests
;;; ====
(test-section "KL Divergence")

;; KL(N(0,1) || N(0,1)) = 0
(define n01 (make-normal-prior 0 1))
(test-approx "KL divergence same normal = 0" 0 (kl-divergence-normal n01 n01) 0.001)

;; KL(N(1,1) || N(0,1)) = (0-1)^2 / (2*1) = 0.5
(define n11 (make-normal-prior 1 1))
(test-approx "KL divergence shifted normal" 0.5 (kl-divergence-normal n11 n01) 0.01)

;; KL(Beta(1,1) || Beta(1,1)) = 0
(define b11 (make-beta-prior 1 1))
(test-approx "KL divergence same beta = 0" 0 (kl-divergence-beta b11 b11) 0.001)

;;; ====
;;; Model Selection Tests
;;; ====
(test-section "Bayesian Model Selection")

;; Bayes factor of 10 is "strong"
(test "interpret strong bayes factor" 'strong (interpret-bayes-factor 10))
(test "interpret substantial bayes factor" 'substantial (interpret-bayes-factor 5))
(test "interpret decisive bayes factor" 'decisive (interpret-bayes-factor 150))
(test "interpret evidence against" 'substantial-against (interpret-bayes-factor 0.2))

;; BIC calculation
;; BIC = k*ln(n) - 2*LL
;; For k=2, n=100, LL=-50: BIC = 2*ln(100) - 2*(-50) = 2*4.6 + 100 = 109.2
(test-approx "BIC calculation" 109.2 (bic -50 2 100) 0.5)

;; AIC calculation
;; AIC = 2k - 2*LL
;; For k=2, LL=-50: AIC = 4 + 100 = 104
(test-approx "AIC calculation" 104 (aic -50 2) 0.1)

;;; ====
;;; Sequential Update Tests
;;; ====
(test-section "Sequential Updates")

;; Sequential beta update equivalent to batch
(define beta-seq (sequential-beta-update (make-beta-prior 1 1)
                                         '((3 . 5) (4 . 5) (2 . 5))))
;; Total: 9 successes in 15 trials
;; Posterior: Beta(1+9, 1+6) = Beta(10, 7)
(test "sequential beta alpha" 10 (beta-prior-alpha beta-seq))
(test "sequential beta beta" 7 (beta-prior-beta beta-seq))

;; Sequential gamma update
(define gamma-seq (sequential-gamma-update (make-gamma-prior 1 1) '(3 5 2 4)))
;; Sum = 14, n = 4
;; Posterior: Gamma(1+14, 1+4) = Gamma(15, 5)
(test "sequential gamma shape" 15 (gamma-prior-shape gamma-seq))
(test "sequential gamma rate" 5 (gamma-prior-rate gamma-seq))

;;; ====
;;; Utility Functions Tests
;;; ====
(test-section "Utility Functions")

;; Posterior summary
(define summary (posterior-summary (make-beta-prior 10 5)))
(test "summary has type" 'beta (cdr (assq 'type summary)))
(test-true "summary has mean" (assq 'mean summary))
(test-true "summary has mode" (assq 'mode summary))
(test-true "summary has variance" (assq 'variance summary))
(test-true "summary has ci-95" (assq 'ci-95 summary))

;; Predictive probability
;; Beta(10, 10) predicting 1 success in 1 trial
(define pred-prob (predictive-beta-binomial (make-beta-prior 10 10) 1 1))
;; Should be close to 0.5 (the posterior mean)
(test-approx "predictive probability" 0.5 pred-prob 0.05)

;; Predictive mean
(define pred-mean (predictive-mean-beta-binomial (make-beta-prior 10 10) 10))
;; 10 trials * 0.5 mean = 5 expected successes
(test-approx "predictive mean" 5.0 pred-mean 0.01)

;;; ====
;;; Summary
;;; ====

(newline)
(display "===========================================================")
(newline)
(display "  Tests passed: ")
(display *tests-passed*)
(newline)
(display "  Tests failed: ")
(display *tests-failed*)
(newline)
(display "===========================================================")
(newline)

(if (= *tests-failed* 0)
    (display "✓ All Bayesian inference tests passed!\n")
    (begin
     (display "✗ Some tests failed!\n")
     (exit 1)))
