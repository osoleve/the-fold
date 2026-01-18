## Abstract


We present **The Fold**, a programming system built on a content-addressable homoiconic foundation. At its core lies a *block machine* where every computational unit—code, data, and types—is represented as a cryptographically-addressed immutable structure. Through a two-phase normalization process—α-normalization via de Bruijn indices and algebraic canonicalization (commutative sorting, associative flattening)—semantically equivalent expressions produce identical hashes, achieving true *semantic identity*: two functions that behave identically are the same function, regardless of variable naming or argument order in commutative operations.

The Fold implements a *gradual dependent type system* combining bidirectional type checking (following Dunfield & Krishnaswami), dependent function and pair types (Π, Σ), higher-kinded types, type classes via dictionary-passing, and GADTs with pattern refinement. Gradual typing through holes enables incremental specification without sacrificing soundness where types are known.

The system organizes verified code into a *module DAG* (internally called the "skill lattice")—a tiered directed acyclic graph where modules declare dependencies, purity guarantees, and complexity bounds. Functions are bounded rather than structurally total—fuel limits guarantee termination of any execution, though this is weaker than type-theoretic totality. This structure enables compositional verification: if dependencies are verified and a module is verified against those dependencies, the module is verified. A BM25-powered semantic search engine enables discovery across thousands of exports.

Key contributions: (1) a block calculus formalizing content-addressed computation with α-equivalence, (2) a dependent type system integrated with gradual typing, (3) a compositional module system with fuel-bounded complexity guarantees. The implementation, built entirely in Chez Scheme with no third-party dependencies, demonstrates that reproducible, verifiable computation can emerge from simple foundations.

---
