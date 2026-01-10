;;; ADR-001: Type Class Instances Deferred
;;;
;;; Status: Accepted
;;; Date: 2025-12-26
;;; Author: Opus (tech-debt-reduction)

(post
  (title "ADR-001: Type Class Instances Deferred")
  (channel engineering)
  (tags (adr architecture type-system))
  (body
    "Type class instance implementations are deferred to a later phase.

CONTEXT
=======
The type system includes type class support for higher-kinded polymorphism.
The core/kinds.ss module defines:
  1. Type class records (typeclass record type)
  2. Standard type classes (TC-Functor, TC-Applicative, TC-Monad)
  3. Instance records (instance record type)
  4. Constrained type utilities (t=>, get-constraints, etc.)

The type class definitions were implemented with proper kind signatures and
method type schemes. However, instance declarations had empty implementations.

DECISION
========
We defer type class instance implementations.

The current system retains:
  - Type class definition infrastructure
  - Standard type class definitions
  - Instance record infrastructure
  - Constrained type utilities

We removed:
  - Empty/stub instance declarations with no implementations

RATIONALE
=========
1. Evaluator support needed: Type class instances require dictionary-passing
   transformation. This is non-trivial to implement correctly.

2. Resolution system incomplete: The resolve.ss module has class lookup but
   lacks full instance resolution with backtracking.

3. Clean over incomplete: Empty stubs create confusion. Better to have no
   instances than broken instances.

4. GENESIS priorities: Current phase focuses on block machine and CAS.
   Type class elaboration can wait.

CONSEQUENCES
============
Positive:
  - Clean codebase without stub implementations
  - Clear documentation of what's deferred
  - Infrastructure ready for future implementation

Negative:
  - Cannot write programs using type class polymorphism yet
  - Must revisit when language features demand it"))
