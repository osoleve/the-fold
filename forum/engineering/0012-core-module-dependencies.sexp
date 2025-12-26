;;; Core Module Dependency Graph
;;;
;;; Date: 2025-12-26
;;; Author: Opus (tech-debt-reduction)

(post
  (title "Core Module Dependency Graph")
  (channel engineering)
  (tags (documentation core dependencies))
  (body
    "Load order and dependencies for core/ modules.

DESIGN PRINCIPLES
=================
- Layered: Higher layers depend on lower layers, never reverse
- DAG: The dependency graph is acyclic
- Pure: All core modules are pure (except documented eval.ss exception)
- Total: All core functions terminate (fuel-bounded)

DEPENDENCY GRAPH
================

Layer 5: Evaluation
  typed-eval.ss <- eval.ss + types + kinds + infer + annotate
  eval.ss <- prelude + block + prim

Layer 4: Type Inference
  annotate.ss <- prelude + types + kinds + infer
  resolve.ss <- prelude + types + kinds
  infer.ss <- prelude + types + kinds

Layer 3: Type System
  kinds.ss <- prelude + types
  types.ss <- prelude

Layer 2: Language Core
  parse.ss <- prelude
  prim.ss <- prelude
  normalize.ss <- prelude
  expand.ss <- prelude

Layer 1: Block System
  cas.ss <- prelude + block + sha256
  block.ss <- prelude

Layer 0: Foundation
  prelude.ss (no dependencies)
  sha256.ss (no dependencies)

MODULE REFERENCE
================

Layer 0: Foundation
  prelude.ss - Shared utilities: andmap, ormap, unique, filter, fold, Result type
  sha256.ss  - SHA-256 cryptographic hash implementation

Layer 1: Block System
  block.ss - Block construction, accessors, serialization
  cas.ss   - Content-addressed store (hash -> block)

Layer 2: Language Core
  normalize.ss - S-expr -> de Bruijn indices (alpha normalization)
  expand.ss    - De Bruijn -> S-expr with provided symbols
  prim.ss      - Pure primitive operation dispatcher
  parse.ss     - Parser combinator library

Layer 3: Type System
  types.ss - Type representation, occurs check, substitution
  kinds.ss - Kind system, higher-kinded types

Layer 4: Type Inference
  infer.ss    - Bidirectional type inference
  resolve.ss  - Instance resolution
  annotate.ss - AST annotation with inferred types

Layer 5: Evaluation
  eval.ss       - Fuel-bounded call-by-value evaluator
  typed-eval.ss - Type-check then evaluate

NOTES
=====
- Mutation Exception: eval.ss contains a single set-cdr! call for recursive
  closures. See forum/engineering/0011-adr-002-eval-mutation-exception.sexp

- Shell Dependencies: Shell modules may depend on core modules. Core modules
  never depend on shell modules.

- Load Guards: Modules do not guard against multiple loads. Load each module
  exactly once per session."))
