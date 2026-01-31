# The Fold

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

A substrate for AI systems that need verifiable computation, automatic memoization, and complexity-sorted training data.

Built in Chez Scheme around a content-addressed store where semantically equivalent code shares identity. The skill lattice provides agents with verified, fuel-bounded capabilities—linear algebra, optimization, physics simulation, symbolic math, and more.

---

## The Insight

Everything in The Fold is identified by its cryptographic hash. This single decision has profound consequences:

- **Identity is content.** Two expressions with the same semantics produce the same hash, regardless of how they're written. Variable names are presentation, not semantics.
- **Deduplication is automatic.** The same computation stored twice is stored once.
- **Verification is inherent.** If you have the hash, you can verify the content.
- **Composition is natural.** References are hashes. Dependencies are immutable.

The Fold achieves this through **semantic normalization**—a multi-phase pipeline that detects equivalences beyond syntactic identity:

| Phase | Equivalence | Example |
|-------|-------------|---------|
| η-reduction | Function wrapper elimination | `(lambda (x) (f x))` → `f` |
| Polynomial canonicalization | Arithmetic equivalence | `(+ x x)` ≡ `(* 2 x)` |
| Algebraic canonicalization | Commutative/associative | `(+ b a)` → `(+ a b)` |
| Identity elimination | Neutral elements | `(+ x 0)` → `x` |
| α-normalization | Variable renaming | `(lambda (x) x)` → `(lambda (dv 0))` |

The result: `(lambda (x) (+ x 1))` and `(lambda (y) (+ 1 y))` hash identically because they *are* identical.

---

## The Block Machine

Everything is a **Block**—the universal primitive:

```
Block = { tag: Symbol, payload: Bytes, refs: [Hash] }
```

| Field | Purpose |
|----|----|
| `tag` | Type identifier (lambda, if, cons, vec, ...) |
| `payload` | Literal data or encoded S-expression |
| `refs` | Ordered list of hashes pointing to other blocks |

Code is blocks. Data is blocks. Documentation is blocks. The entire system is a directed acyclic graph of content-addressed S-expressions, introspectable at every level.

---

## Architecture

The Fold separates concerns into three layers with a strict purity boundary:

```
┌───────────────────────────────────────┐
│  user/       Applications             │
├───────────────────────────────────────┤
│  boundary/   IO, validation           │  ← Impure
╞═══════════════════════════════════════╡
│  lattice/    Verified skill DAG       │  ← Pure
├───────────────────────────────────────┤
│  core/       Language kernel          │  ← Pure
└───────────────────────────────────────┘
```

| Layer | Directory | Purity | Role |
|----|----|----|----|
| Core | `core/` | Pure | Minimal, axiomatic language kernel |
| Lattice | `lattice/` | Pure | Verified library DAG (36 skills, ~3,500 exports) |
| Boundary | `boundary/` | Impure | IO boundary, validation, capabilities |
| User | `user/` | Mixed | Applications and experiments |

**The key invariant:** Core and Lattice assume perfect input. Boundary provides all defensive logic, validation, and error handling. This separation keeps the pure layers simple and verifiable.

---

## Core Principles

**Content-addressed universality.** The hash is the identity. Same semantics, same hash, everywhere, forever.

**Homoiconicity.** Everything is an S-expression—code, data, configuration, logs, documentation. The system can introspect everything.

**Purity separation.** The pure layers (core, lattice) contain no IO, no mutation, no defensive code. The impure layer (boundary) handles the messy world.

**Fuel-based totality.** All core functions take a fuel parameter—a cost budget that ensures termination. Functions are total: they always return, either with a result or with fuel exhaustion.

**No external dependencies.** The entire system is built in-house. The substrate is Chez Scheme and nothing else.

---

## The Optimization Philosophy

The Fold optimizes for **cognitive efficiency of representation**—abstractions that make problem spaces tractable for bounded reasoners.

This is validated empirically: if a small model can solve problems more effectively with an abstraction than without it, that abstraction captures genuine structure. If not, the abstraction is wrong, no matter how elegant it appears.

Speed optimizations happen underneath; semantics stay stable above. The content-addressed foundation means that *what* something computes never changes—only *how fast* it computes can improve.

---

## Quick Start

```bash
# Evaluate an expression (daemon auto-starts, implicit parens)
./fold "+ 1 2"                          # → 3

# Named sessions persist state
./fold -s work "define x 42"            # Define in session
./fold -s work "* x 2"                  # → 84

# Run the test suite
scheme --script test-all.ss

# Explore the lattice (36 skills)
./fold "(lattice-init!)"                # Initialize search index
./fold 'lf "matrix"'                    # Full-text search
./fold "(li 'linalg)"                   # Inspect a skill
```

---

## Project Structure

| Directory | Purpose |
|----|----|
| `core/` | Language kernel: types, blocks, evaluation, normalization |
| `lattice/` | Skill DAG: verified libraries organized by tier and domain |
| `boundary/` | IO boundary: REPL, storage, diagnostics, tooling |
| `user/` | Applications, experiments, demos |
| `docs/` | Extended documentation |
| `ops/` | Deployment and operations |

---

## The Lattice

The lattice is The Fold's standard library (~284k lines, 36 skills), organized as a dependency DAG with tiers:

**Tier 0 (Foundational):** `linalg`, `data`, `algebra`, `random`—no lattice dependencies.

**Tier 1 (Intermediate):** `numeric`, `geometry`, `diffgeo`, `autodiff`, `fp`, `query`, `info`, `topology`, `crypto`, `optimization`, `statistics`, `dsl`, `egraph`, `dataset`—build on tier 0.

**Tier 2+ (Advanced):** `physics/diff`, `physics/diff3d`, `physics/classical`, `tiles`, `sim`, `automata`, `pipeline`—multiple dependencies.

Each skill has a `manifest.sexp` declaring its purity, fuel bounds, dependencies, and exports. The `lattice/meta/` subsystem provides search and navigation:

```scheme
(lf "matrix decomposition")   ; Full-text search
(li 'linalg)                  ; Skill description
(le 'linalg)                  ; List exports
(ld 'physics/diff)            ; What does this depend on?
(lu 'linalg)                  ; What depends on this?
```

---

## Status

The Fold is a production system, actively developed and deployed.

---

## Contributing

```bash
# Clone and setup
git clone https://github.com/osoleve/the-fold.git
cd the-fold

# Install git hooks (runs tests before commit)
./setup-hooks.sh

# Or with pre-commit framework
pip install pre-commit && pre-commit install
```

See [CLAUDE.md](./CLAUDE.md) for development guidelines.

---

## Further Reading

- [docs/technical-overview.md](./docs/technical-overview.md) — 2-page technical overview (start here)
- [docs/technical-report.md](./docs/technical-report.md) — Comprehensive technical report (academic style)
- [docs/language-reference.md](./docs/language-reference.md) — Type system, parallel evaluation, rank-N polymorphism
- [docs/normalization-v2.md](./docs/normalization-v2.md) — Semantic normalization pipeline specification
- [docs/physics-guide.md](./docs/physics-guide.md) — Differentiable physics simulation guide
- [docs/diffgeo-guide.md](./docs/diffgeo-guide.md) — Differential geometry and manifold computation
- [docs/examples/](./docs/examples/) — Worked examples by domain
- [CLAUDE.md](./CLAUDE.md) — Operational guide for working with The Fold
- [lattice/meta/](./lattice/meta/) — Skill navigation and search

---

## License

Apache 2.0 — See [LICENSE](./LICENSE) for details.
