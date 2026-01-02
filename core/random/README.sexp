((name "random")
 (purpose "Pseudorandom number generation and probability")
 (description "Pure, deterministic pseudorandom number generators using the
State monad. All generators are fully reproducible given the same seed.
Includes probability distributions and sampling functions.")
 (modules
  ((prng.ss "PCG, Xorshift128+, Splitmix64 generators with State monad")
   (distributions.ss "Probability distributions (uniform, normal, exponential, etc.)")
   (probability.ss "Probability combinators and sampling utilities")))
 (dependencies (base fp/control))
 (key-concepts
  ((pure-random "Deterministic RNG via State monad - reproducible results")
   (pcg "Permuted Congruential Generator - high quality, fast")
   (splitmix "Fast seeding generator for initializing other PRNGs"))))
