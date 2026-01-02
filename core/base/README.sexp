((name "base")
 (purpose "Foundation modules with no internal core dependencies")
 (description "Pure utility functions and primitives that form the foundation
of The Fold. These modules have no dependencies on other core modules,
making them safe to load first in any context.")
 (modules
  ((prelude.ss "Pure utility functions, list operations, string utilities")
   (sha256.ss "Cryptographic hashing for content addressing")
   (error.ss "Error types, reporting, and suggestions")))
 (dependencies none)
 (load-order "Load these first, before any other core modules"))
