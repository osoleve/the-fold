# The Fold

[![License](https://img.shields.io/badge/License-Apache%202.0-blue.svg)](LICENSE)

A **verifiable computation environment** in Chez Scheme. 
Built for AI agents that _need to show their work_, not just produce answers.

Everything—code, data, proofs, execution traces—is a content-addressed S-expression. 
This gives agents a shared memory of verified skills, automatic deduplication, 
and execution bounds that guarantee termination.

---

## Why This Exists

Language models are terrible at computation but excellent at decomposing problems. The Fold meets them where they are: a homoiconic environment where an agent can introspect its own tools, verify its own outputs, and build on prior work without re-deriving anything.

The skill lattice provides verified capabilities — linear algebra, optimization, physics simulation, symbolic math, differential geometry, and more — organized as a dependency DAG. Each skill declares its purity, fuel bounds, and exports. An agent doesn't guess what's available; it searches, inspects, and composes.

```scheme
(lf "matrix decomposition")   ; Full-text search across all skills
(li 'linalg)                  ; What does this skill do?
(le 'linalg)                  ; What functions does it export?
(ld 'physics/diff)            ; What does this depend on?
```

---

## The Trick

Everything is identified by its hash. This sounds simple. It isn't.

Two expressions with the same semantics produce the same hash, regardless of how they're written. `(lambda (x) (+ x 3))` and `(lambda (y) (+ 3 (* y 1))` are the same block because The Fold normalizes before hashing — η-reduction, polynomial canonicalization, commutativity sorting, identity elimination, α-normalization. Variable names are presentation, not identity.

This means deduplication is automatic, verification is inherent, and composition is just referencing hashes. An agent that derives a result once never needs to derive it again — the content-addressed store already has it.

---

## How It's Built

The universal primitive is a **Block**:

```
Block = { tag: Symbol, payload: Bytes, refs: [Hash] }
```

Tag says what it is (`lambda`, `if`, `cons`, `vec`, ...). Payload carries literal data. Refs point to other blocks by hash. The entire system is a DAG of these — introspectable at every level.

Architecture is three layers with a hard purity boundary:

```
  user/       Applications                    Mixed
  boundary/   IO, validation, error handling  Impure
  ─────────────────────────────────────────────────
  lattice/    Verified skill DAG              Pure
  core/       Language kernel                 Pure
```

Core and lattice assume perfect input and contain no IO, no mutation, no defensive code. Boundary handles the messy world. This isn't just aesthetics — it's what makes the pure layers verifiable.

All core functions take a **fuel** parameter: a cost budget that guarantees termination. Functions are total. They always return, either with a result or a fuel-exhaustion signal. An agent can't get stuck in an infinite loop.

---

## The Optimization Thesis

The Fold optimizes for **cognitive efficiency of representation** — abstractions that make problem spaces tractable for bounded reasoners.

The validation is empirical: if a small model solves problems more effectively with an abstraction than without it, that abstraction captures genuine structure. If not, the abstraction is wrong, no matter how elegant it looks.

Speed optimizations happen underneath; semantics stay stable above. What something computes never changes. How fast it computes can always improve.

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

# Explore the lattice
./fold "(lattice-init!)"                # Initialize search index
./fold 'lf "matrix"'                    # Full-text search
./fold "(li 'linalg)"                   # Inspect a skill
```

---

## The Lattice at a Glance

~284k lines across 36 skills, organized by dependency tier:

**Tier 0 — Foundations:** `linalg`, `data`, `algebra`, `random`. No lattice dependencies.

**Tier 1 — Core capabilities:** `numeric`, `geometry`, `diffgeo`, `autodiff`, `fp`, `query`, `info`, `topology`, `crypto`, `optimization`, `statistics`, `dsl`, `egraph`, `dataset`.

**Tier 2+ — Composed domains:** `physics/diff`, `physics/diff3d`, `physics/classical`, `tiles`, `sim`, `automata`, `pipeline`.

Each skill ships a `manifest.sexp` declaring everything an agent (or human) needs to know before using it.

---

## Project Layout

- **`core/`** — Language kernel: types, blocks, evaluation, normalization
- **`lattice/`** — Skill DAG: verified libraries by tier and domain
- **`boundary/`** — IO boundary: REPL, storage, diagnostics, tooling
- **`user/`** — Applications, experiments, demos
- **`docs/`** — Extended documentation
- **`ops/`** — Deployment and operations

---

## Further Reading

Start with the [two-page technical overview](./docs/technical-overview.md), then go where curiosity takes you:

- [Technical report](./docs/technical-report.md) — comprehensive, academic-style
- [Language reference](./docs/language-reference.md) — type system, parallel evaluation, rank-N polymorphism
- [Normalization spec](./docs/normalization-v2.md) — the semantic normalization pipeline in detail
- [Physics guide](./docs/physics-guide.md) — differentiable physics simulation
- [Differential geometry guide](./docs/diffgeo-guide.md) — manifold computation
- [Worked examples](./docs/examples/) — by domain
- [CLAUDE.md](./CLAUDE.md) — operational guide for working with The Fold

---

## Contributing

```bash
git clone https://github.com/osoleve/the-fold.git
cd the-fold
./setup-hooks.sh          # Runs tests before commit
```

See [CLAUDE.md](./CLAUDE.md) for development guidelines.

---

## License

Apache 2.0 — See [LICENSE](./LICENSE) for details.
