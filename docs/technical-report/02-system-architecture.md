## 2. System Architecture


The Fold employs a *three-layer architecture* separating pure computation from effectful boundaries:

```
┌─────────────────────────────────────────────────────────────┐
│                         User Layer                          │
│              Applications, experiments, scripts             │
├─────────────────────────────────────────────────────────────┤
│                       Boundary Layer                        │
│         IO, validation, capability minting, effects         │
├─────────────────────────────────────────────────────────────┤
│                        Core Layer                           │
│          Pure, total, content-addressed, verified           │
└─────────────────────────────────────────────────────────────┘
```

### 2.1 The Core Layer

The Core is the mathematical heart of The Fold. Code in Core satisfies three properties:

**Purity**: No side effects. Functions depend only on their arguments and produce only their return values. This enables equational reasoning—if `f(x) = y`, then `f(x)` can always be replaced with `y`.

**Bounded Computation**: Every computation terminates within a declared resource bound. This is enforced via *fuel-bounded execution*: every computation receives a fuel budget that decrements with each reduction step. Exhausting fuel yields an `out-of-fuel` error rather than infinite looping.

**Important distinction**: This is *not* totality in the type-theoretic sense. True totality (as in Agda or Idris) proves termination for all inputs via structural recursion checks or sized types—a property of the function itself. Fuel bounds instead guarantee that any particular execution completes—a property of the runtime. A function that exhausts fuel has *failed*, not *terminated normally*.

We choose bounded computation over structural totality for pragmatic reasons:
- Structural totality rejects useful programs (e.g., interpreters, fixpoint iterations)
- Fuel bounds are simple to implement and reason about
- The bound is explicit in module manifests, enabling composition

The tradeoff: Core functions cannot be safely evaluated during type checking (since they might exhaust fuel), limiting dependent type expressiveness compared to systems with true totality.

**Trust**: Core assumes *perfect input*. It performs no validation, no defensive checks, no error recovery. If you pass malformed data to Core, behavior is undefined. This simplicity enables formal verification.

Core contains:
- Block primitives (construction, hashing, serialization)
- Type system (checking, inference, normalization)
- Evaluation (fuel-bounded reduction)
- Normalization (de Bruijn transformation)

### 2.2 The Boundary Layer

The Boundary (internally called "the thimble" or "fallen layer") mediates between the pure Core and the impure world:

**Validation**: All external input is validated before reaching Core. Malformed S-expressions, invalid UTF-8, type mismatches—all caught at the Boundary.

**Capability Minting**: Effects require capabilities. The Boundary creates capability tokens (filesystem access, network access, time/randomness) that authorize specific operations.

**IO Operations**: File reading/writing, network communication, user interaction—all live in Boundary. Core never performs IO directly.

**Error Recovery**: Boundary handles exceptions, provides error messages, and maintains system stability when things go wrong.

The Boundary/Core interface is the *verification frontier*: Core can be formally verified; Boundary is trusted but unverified.

### 2.3 The User Layer

The User layer contains applications built on the verified foundations:

- Interactive REPL sessions
- Scripts and automation
- Domain-specific applications
- Experiments and prototypes

User code may be verified (if it uses only Core and verified Boundary interfaces) or unverified (if it uses arbitrary Boundary capabilities).

### 2.4 Design Rationale

This architecture reflects a fundamental insight: *purity and effects require different treatment*.

Pure code can be:
- Cached (same input → same output)
- Parallelized (no shared mutable state)
- Verified (equational reasoning applies)
- Deduplicated (content addressing works)

Effectful code cannot enjoy these properties unconditionally. Rather than compromise the entire system, we isolate effects to a well-defined boundary.

The Boundary is not a "second-class citizen"—it is essential for any useful system. But by separating it from Core, we preserve Core's mathematical properties while providing practical functionality.

---
