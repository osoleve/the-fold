((name "random")
 (purpose "Pseudorandom number generation, probability, and variational inference")
 (description "Pure, deterministic pseudorandom number generators using the
State monad. All generators are fully reproducible given the same seed.
Includes probability distributions, sampling functions, Bayesian inference
primitives, Monte Carlo methods, and variational inference for scalable
gradient-based approximate Bayesian computation.")
 (modules
  ((prng.ss "PCG, Xorshift128+, Splitmix64 generators with State monad")
   (distributions.ss "Probability distributions (uniform, normal, exponential, etc.)")
   (probability.ss "Probability monad for compositional probabilistic programming")
   (bayesian.ss "Bayesian inference primitives and conjugate priors")
   (monte-carlo.ss "Monte Carlo methods including MCMC")
   (variational-inference.ss "Variational inference: ELBO optimization, reparameterization trick")))
 (dependencies (base fp/control autodiff))
 (key-concepts
  ((pure-random "Deterministic RNG via State monad - reproducible results")
   (pcg "Permuted Congruential Generator - high quality, fast")
   (splitmix "Fast seeding generator for initializing other PRNGs")
   (reparameterization "z = μ + σε enables gradient flow through samples")
   (elbo "Evidence Lower Bound - optimization target for variational inference")
   (variational-family "Parameterized distribution q(z|φ) approximating posterior"))))
