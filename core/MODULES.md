# Core Module Dependencies

This document describes the dependency layers within the `core/` directory ("The Fabric").
Modules in lower layers must not depend on modules in higher layers.

## Layer 0: Base (`core/base/`)
Foundation utilities with minimal dependencies.
- **prelude.ss**: Basic combinators, list ops (pure Scheme).
- **error.ss**: Error handling types and formatters.
- **sha256.ss**: Hashing primitives.

## Layer 1: Block System (`core/blocks/`)
The content-addressed storage and normalization layer.
- **block.ss**: Defines the `Block` type and serialization.
- **cas.ss**: Content-Addressed Store (depends on `block.ss`, `sha256.ss`).
- **normalize.ss**: Canonicalization of expressions (De Bruijn indices).
- **expand.ss**: De-canonicalization (De Bruijn -> Names).

## Layer 1.5: Autodiff Foundation (`core/autodiff/`)
Foundational automatic differentiation structures required by the evaluator.
- **comp-graph.ss**: Computational graph (DAG) for tracking operations.
- **reverse-diff.ss**: Reverse-mode AD engine (Tape).

## Layer 2: Language Core (`core/lang/`)
The evaluation and execution engine.
- **prim.ss**: Pure primitive implementations.
- **span.ss**: Source location tracking.
- **parse.ss**: Parser combinators.
- **fold-parse.ss**: The Fold language parser (depends on `span.ss`, `parse.ss`).
- **eval.ss**: The Evaluator.
    - Depends on: `prelude.ss`, `block.ss`, `prim.ss`, `reverse-diff.ss`.
    - **Note**: `eval.ss` uses `reverse-diff.ss` to support differentiable programming primitives.

## Layer 3: Type System (`core/types/`)
Static analysis and type checking.
- **kinds.ss**: Kind system.
- **types.ss**: Type definitions and predicates.
- **infer.ss**: Bidirectional type inference.
- **resolve.ss**: Type class resolution.

## Layer 4: Utilities (`core/util/`)
High-level tools built on the core.
- **pretty.ss**: Pretty printing.
- **debug.ss**: Debugging helpers.

---

## Relationship with Lattice
`lattice/` is the standard library built *on top* of `core/`.
- `lattice/autodiff/higher-order-diff.ss` extends `core/autodiff/`.
- `lattice/linalg/` provides vector/matrix ops used by higher-level AD.
