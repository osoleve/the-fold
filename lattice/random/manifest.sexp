;;; lattice/random/manifest.sexp — Random Number Generation Skill Manifest

(skill random
  (version "0.1.0")
  (tier 0)
  (path "lattice/random")
  (purity total)  ; All generators are pure (State monad)
  (stability stable)
  (fuel-bound "O(1) per sample for most distributions")
  (deps (fp))  ; Uses fp/control/state and fp/numeric/transcendental

  (description
   "Pure, deterministic pseudorandom number generation using the State monad.
    All generators are fully reproducible given the same seed. Includes
    high-quality PRNGs (PCG, Xorshift128+, Splitmix64), probability
    distributions (normal, exponential, poisson, etc.), and a probability
    monad for compositional probabilistic programming.")

  (keywords (random prng pcg xorshift splitmix probability distribution
             sampling monte-carlo state-monad reproducible deterministic
             normal gaussian exponential poisson binomial))
  (aliases (rand prng probability sampling))

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
    sample-many expectation variance
    importance-sample rejection-sample))

  (modules
   (prng "prng.ss" "PRNGs: PCG, Xorshift128+, Splitmix64 with State monad")
   (distributions "distributions.ss" "Probability distributions: normal, exponential, poisson, etc.")
   (probability "probability.ss" "Probability monad for probabilistic programming")
   (bayesian "bayesian.ss" "Bayesian inference primitives")
   (monte-carlo "monte-carlo.ss" "Monte Carlo methods and MCMC")))
