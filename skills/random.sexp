;;; skills/random.sexp — Skill curation file

(skill random
  (description
    "Pure, deterministic pseudorandom number generation using the State monad.\n    All generators are fully reproducible given the same seed. Includes\n    high-quality PRNGs (PCG, Xorshift128+, Splitmix64), probability\n    distributions (normal, exponential, poisson, etc.), a probability\n    monad for compositional probabilistic programming, and variational\n    inference for scalable Bayesian computation.")

  (keywords (random prng pcg xorshift splitmix probability distribution sampling monte-carlo state-monad reproducible deterministic normal gaussian exponential poisson binomial variational-inference elbo reparameterization bayesian gradient-descent))

  (aliases (rand prng probability sampling vi))

  (concepts (probability-distributions bayesian-inference monte-carlo-methods variational-methods))

  (modules
    prng
    distributions
    probability
    bayesian
    monte-carlo
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
