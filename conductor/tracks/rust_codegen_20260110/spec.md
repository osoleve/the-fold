# Specification: Rust Codegen for Primitives and Layer 1

## Overview
This track focuses on designing and implementing a code generation pipeline that transforms The Fold's core primitives (Blocks) into optimized Rust code. This is a critical step for achieving the "strategic Rust acceleration" goal.

## Functional Requirements
- Define a mapping between Scheme primitives (lambdas, if-expressions, arithmetic, etc.) and Rust equivalents.
- Implement a code generator that produces valid, compilable Rust source code from alpha-normalized Blocks.
- Support "Layer 1" primitives: basic data structures, memory management (content-addressed storage interface), and core arithmetic.
- Ensure the generated code is compatible with the existing `fold-accel` Rust crate.

## Non-Functional Requirements
- **Performance:** Generated code should leverage Rust's zero-cost abstractions.
- **Totality:** The generator must ensure that only total/terminating constructs are emitted.
- **Maintainability:** The codegen logic should be modular and well-documented.

## Acceptance Criteria
- [ ] Valid Rust code can be generated for a subset of "Layer 1" primitives.
- [ ] Generated code compiles without errors using `cargo build`.
- [ ] Unit tests in Scheme verify the correctness of the generated Rust structure.

## Out of Scope
- Full application generation.
- Advanced optimization passes (beyond basic mapping).
- Integration with external Rust libraries (other than standard library and internal `fold-accel`).
