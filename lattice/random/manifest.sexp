;;; lattice/random/manifest.sexp — Random Number Generation Skill Manifest

(skill random
  (version "0.2.0")
  (path "lattice/random")
  (purity total)  ; All generators are pure (State monad)
  (stability stable)
  (fuel-bound "O(1) per sample for most distributions, O(iter) for VI")
  (deps (fp))  ; Uses fp/control/state, fp/numeric/transcendental

  (description
   "Pure, deterministic pseudorandom number generation using the State monad.
    All generators are fully reproducible given the same seed. Includes
    high-quality PRNGs (PCG, Xorshift128+, Splitmix64), probability
    distributions (normal, exponential, poisson, etc.), a probability
    monad for compositional probabilistic programming, and variational
    inference for scalable Bayesian computation.")

  (keywords (random prng pcg xorshift splitmix probability distribution
             sampling monte-carlo state-monad reproducible deterministic
             normal gaussian exponential poisson binomial variational-inference
             elbo reparameterization bayesian gradient-descent))
  (aliases (rand prng probability sampling vi))

  (concepts
    (concept probability-distributions
      (description "Continuous and discrete distributions (Normal, Poisson, Beta, Dirichlet) with pure state-monad sampling.")
      (parent probabilistic-reasoning)
      (synonyms random rand prng probability sampling))
    (concept bayesian-inference
      (description "Conjugate models (Beta-Binomial, Normal-Normal, Gamma-Poisson), posterior computation, and Bayes factors.")
      (parent probabilistic-reasoning))
    (concept monte-carlo-methods
      (description "Importance sampling, rejection sampling, Metropolis-Hastings MCMC, Gibbs sampling, and variance reduction.")
      (parent probabilistic-reasoning)
      (synonyms monte-carlo mcmc metropolis-hastings gibbs-sampling importance-sampling))
    (concept variational-methods
      (description "Variational inference via ELBO optimization, reparameterization trick, and mean-field Gaussian families.")
      (parent probabilistic-reasoning)
      (synonyms variational-inference elbo reparameterization vi)))

  (exports
   (prng
    ;; Bit manipulation
    u32 u64 rotl32 rotr32 rotl64 rotr64 mask-32 mask-64
    ;; Splitmix64
    make-splitmix splitmix? splitmix-state splitmix-next splitmix-random
    ;; PCG
    make-pcg pcg? pcg-state pcg-inc pcg-next pcg-random
    ;; Xorshift128+
    make-xorshift128 xorshift128? xorshift128-s0 xorshift128-s1
    xorshift128-next xorshift128-random
    ;; Uniform generation
    random-u32-from random-u64-from random-float random-float-range
    random-int-range random-bool
    ;; Sampling utilities
    random-element shuffle sample random-list)
   (distributions
    ;; Basic distributions
    random-bernoulli random-exponential
    random-normal-standard random-normal random-normal-pair
    random-geometric random-poisson log-gamma
    random-binomial random-gamma random-beta
    random-categorical random-normal-vector random-dirichlet)
   (probability
    ;; Probability monad
    make-prob prob? prob-state
    prob-get-prng prob-put-prng prob-get-weight prob-add-weight
    run-prob sample-prob weight-prob
    ;; Monad operations
    prob-pure prob-bind prob-map prob-sequence
    ;; Conditioning and inference
    condition observe factor
    sample-many importance-sample rejection-sample)
   (bayesian
    ;; Beta-Binomial conjugate model
    make-beta-prior beta-prior? beta-prior-alpha beta-prior-beta
    beta-binomial-posterior beta-posterior-mean beta-posterior-mode
    beta-posterior-variance beta-credible-interval
    ;; Normal-Normal conjugate model
    make-normal-prior normal-prior? normal-prior-mean normal-prior-variance
    normal-normal-posterior normal-posterior-credible-interval
    ;; Gamma-Poisson conjugate model
    make-gamma-prior gamma-prior? gamma-prior-shape gamma-prior-rate
    gamma-poisson-posterior gamma-posterior-mean gamma-posterior-variance
    gamma-posterior-mode
    ;; Dirichlet-Multinomial conjugate model
    make-dirichlet-prior dirichlet-prior? dirichlet-prior-alphas
    dirichlet-multinomial-posterior dirichlet-posterior-mean dirichlet-posterior-mode
    ;; Log PDFs and utilities
    log-beta-pdf log-beta log-normal-pdf log-gamma-pdf log-poisson-pmf log-factorial
    ;; Divergence and information
    kl-divergence-normal kl-divergence-beta digamma
    ;; Model selection
    log-marginal-likelihood-beta-binomial log-binomial-coeff
    log-marginal-likelihood-normal-normal
    bayes-factor log-bayes-factor interpret-bayes-factor
    bic aic
    ;; Sequential updates
    sequential-beta-update sequential-normal-update sequential-gamma-update
    ;; Posterior summaries and prediction
    posterior-summary predictive-beta-binomial predictive-mean-beta-binomial)

   (monte-carlo
    ;; Sample statistics
    sample-mean sample-variance sample-std sample-sem
    sample-quantile sample-median sample-min sample-max sample-summary
    ;; Integration
    mc-integrate mc-estimate
    ;; Importance sampling
    importance-sample self-normalized-importance-sample
    ;; Rejection sampling
    rejection-sample rejection-sample-n rejection-sample-bounded
    ;; Metropolis-Hastings MCMC
    mh-step mh-symmetric-step mh-chain mh-sample mh-thinned-sample
    random-walk-proposal multivariate-random-walk-proposal
    ;; Gibbs sampling
    gibbs-step gibbs-chain gibbs-sample
    ;; Variance reduction
    antithetic-estimate antithetic-integrate
    control-variate-estimate
    stratified-sample
    ;; Diagnostics
    effective-sample-size gelman-rubin autocorrelation
    integrated-autocorrelation-time mcmc-diagnostics
    ;; Batch and parallel runners
    parallel-chains batch-sample progressive-sample
    ;; Convenience
    run-mc mc-pi-estimate mc-normal-tail-prob)

   )

  (modules
   (prng "prng.ss" "PRNGs: PCG, Xorshift128+, Splitmix64 with State monad")
   (distributions "distributions.ss" "Probability distributions: normal, exponential, poisson, etc.")
   (probability "probability.ss" "Probability monad for probabilistic programming")
   (bayesian "bayesian.ss" "Bayesian inference primitives")
   (monte-carlo "monte-carlo.ss" "Monte Carlo methods and MCMC")
   ))
