# The Fold: Technical Overview

**A content-addressable programming system where semantically identical code is literally identical.**

---

## The Core Idea

Traditional programming identifies code by *where it lives*—file paths, module names, package versions. This causes problems: the same path can point to different code over time, identical functions written independently are treated as distinct, and "dependency hell" emerges from name-based resolution.

The Fold takes a different approach: **identity is content**. Every piece of code is identified by its cryptographic hash. Two functions that compute the same thing *are* the same function—same hash, same identity, automatically.

But there's a catch: `(lambda (x) x)` and `(lambda (y) y)` are the same function (both are identity), yet they have different text. Naive hashing would give them different identities.

**The solution**: Before hashing, we normalize code using *de Bruijn indices*—a technique from logic where variables are replaced by numbers indicating how many binding levels up they refer to. Both functions above normalize to `(lambda (dv 0))` and hash identically.

This gives us **semantic identity**: code that behaves the same *is* the same.

---

## Architecture

Everything in The Fold is a **Block**:

```
Block = { tag: Symbol, payload: Bytes, refs: [Hash] }
```

Blocks reference other blocks by hash, forming a Merkle DAG—an immutable, content-addressed graph. Change anything and the hash changes; identical content always has identical hashes.

The system has three layers:

| Layer | Purpose | Properties |
|-------|---------|------------|
| **Core** | Language kernel | Pure, total (always terminates), assumes valid input |
| **Shell** | IO boundary | Handles real-world messiness, validates input |
| **User** | Applications | Built on verified foundations |

The key insight: keep the mathematical core simple and pure; handle effects at a well-defined boundary. Core can be formally verified; Shell is pragmatic.

---

## The Type System

The Fold has a modern type system combining several features:

- **Bidirectional type checking**: Types flow both up (inference) and down (checking), giving predictable behavior
- **Dependent types**: Types can depend on values—e.g., `Vec 3 Int` is a vector of exactly 3 integers
- **Type classes**: Abstraction over types (like Haskell's), implemented via explicit dictionary passing
- **Gradual typing**: Use `?` as a "hole" when you don't want to specify a type yet

The system is *sound where types are specified*—you get the safety benefits for typed code while retaining flexibility for rapid prototyping.

---

## The Module System

Code is organized into a **dependency DAG** (directed acyclic graph) with tiers:

- **Tier 0**: Foundational modules (linear algebra, data structures)—no dependencies on other modules
- **Tier 1**: Intermediate modules (autodiff, geometry)—depend on Tier 0
- **Tier 2+**: Advanced modules (physics simulation)—multiple dependencies

Each module declares:
- **Dependencies**: What it needs
- **Purity**: Whether it has side effects
- **Complexity bounds**: Big-O fuel consumption

This enables **compositional verification**: if dependencies are verified, you only need to verify the module itself against those dependencies—not the entire transitive closure.

A built-in search engine (BM25) indexes ~1,400 exports for discovery:

```scheme
(lf "matrix decomposition")  ; Full-text search
(ld 'physics/diff)           ; What does this depend on?
```

---

## Why This Matters

**For reproducibility**: Same code, same hash, same behavior—forever. No "works on my machine."

**For verification**: Pure core + compositional modules = tractable formal verification.

**For deduplication**: Identical code stored once, automatically. No coordination needed.

**For collaboration**: Independent developers who write the same function get the same hash. No merge conflicts for identical work.

**For AI**: A skill library where capabilities are content-addressed, verified, and composable—ideal for training and evaluation.

---

## Implementation

Built entirely in Chez Scheme with **zero external dependencies**. The SHA-256 implementation, the type checker, the module system—all built in-house. This makes the system fully auditable and self-contained.

The codebase is ~50,000 lines of Scheme across:
- `core/`: Language kernel (~15k lines)
- `lattice/`: Standard library (~25k lines, ~1,400 exports)
- `shell/`: IO and tooling (~10k lines)

### Rust Acceleration Layer

Performance-critical paths have optional Rust acceleration via FFI (`shell/ffi/rust-accel/`). The Rust layer provides:

**Spatial Acceleration (BVH)**:
- Bounding Volume Hierarchy construction and traversal
- Closest-point queries on mesh surfaces
- Ray intersection with automatic pruning

**Raymarching**:
- Sphere tracing for mesh SDFs
- Gradient-based normal computation
- Complete march loop in Rust (eliminates per-step FFI overhead)

**Linear Algebra**:
- 4x4 matrix multiplication (unrolled, ~112 ops)
- Matrix-vector multiplication
- Batch point transformation
- Matrix transpose and determinant

**Design Principles**:
- All functions use `#[repr(C)]` structs for FFI safety
- Fuel tracking mirrors Scheme's system for totality preservation
- Out-pointers for results, never return Scheme objects
- No panics—all errors return status codes

---

## Comparison to Similar Systems

| System | Similarity | Difference |
|--------|------------|------------|
| **Unison** | Content-addressed code | Different type system, different normalization |
| **IPFS** | Content-addressed storage | IPFS is for data; The Fold is for computation |
| **Nix** | Content-addressed builds | Nix addresses build reproducibility; The Fold addresses computation |
| **Git** | Content-addressed history | Git versions files; The Fold versions semantics |

---

## Getting Started

```bash
# Start the REPL daemon
./daemon.sh start

# Evaluate an expression
./fold-agent.py "(+ 1 2)"

# Run tests
scheme --script test-all.ss

# Explore the standard library
./fold-agent.py "(load \"lattice/meta/meta.ss\") (lattice-init!) (lf \"matrix\")"
```

---

## Learn More

- [Technical Report](./technical-report.md) — Full academic treatment with formal definitions
- [Language Reference](./language-reference.md) — Type system details
- [CLAUDE.md](../CLAUDE.md) — Development guide

---

*The Fold: Where code is content, and content is eternal.*
