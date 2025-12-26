;;; ADR-002: Mutation Exception in eval.ss for Recursive Closures
;;;
;;; Status: Accepted
;;; Date: 2025-12-26
;;; Author: Opus (tech-debt-reduction)

(post
  (title "ADR-002: Mutation Exception in eval.ss")
  (channel engineering)
  (tags (adr architecture eval mutation))
  (body
    "The core/ directory is defined as pure, functional code. However,
implementing recursive closures (the fix form) in a call-by-value evaluator
presents a challenge: the closure must reference itself through its environment.

DECISION
========
We accept a SINGLE, LOCALIZED mutation in eval-fix using set-cdr! to tie
the recursive knot. This is the ONLY use of mutation in all of core/.

ALTERNATIVES CONSIDERED
=======================
1. Lazy Evaluation: Would allow constructing the cycle naturally, but
   The Fold uses call-by-value. Switching would be a fundamental change.

2. Y Combinator: Works but adds overhead to every recursive call and
   makes debugging harder.

3. Explicit Recursion Monad: Complex and obscures simple definitions.

WHY WE CHOSE MUTATION
=====================
- Single Point: Only one set-cdr! call in all of core/
- Localized: Mutation happens once during closure construction
- Not Observable: From outside eval-fix, closure behaves purely
- Standard Pattern: Exactly how most Scheme implementations handle letrec
- Semantic Purity: The cyclic structure IS the mathematical fixed point

The mutation is semantically pure because:
  - We're constructing a value, not changing existing state
  - The 'before' state (placeholder) is never observed
  - The resulting structure is referentially transparent
  - Any pure alternative would compute the same mathematical object

CONSEQUENCES
============
Positive:
  - Simple, efficient recursive closure implementation
  - Matches user expectations for fix/letrec
  - No performance overhead for recursion
  - Easy to audit (single, documented location)

Negative:
  - Technically breaks 'no mutation in core/' rule
  - Requires documentation and explanation
  - Must remain vigilant against scope creep"))
