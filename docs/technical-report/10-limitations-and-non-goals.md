## 10. Limitations and Non-Goals


Honest acknowledgment of what The Fold does NOT provide.

### 10.1 Not True Totality

The Fold guarantees *bounded execution*, not *totality*. The difference:

| Property | Totality | Bounded Execution |
|----|----|----|
| Guarantee | Function terminates on all inputs | Execution stops within fuel limit |
| Mechanism | Structural recursion / sized types | Fuel counter |
| On failure | Rejected at compile time | Runtime `out-of-fuel` error |
| For type checking | Safe to evaluate during checking | Unsafe (might exhaust fuel) |

**Implication**: We cannot safely evaluate arbitrary Core functions during type checking. This limits dependent type expressiveness compared to Agda or Idris.

### 10.2 Limited Gradual + Dependent Integration

The Fold does NOT support:
- Holes in dependent positions (`(Π ((x : ?)) (Vec x A))`)
- Incremental addition of dependent types to untyped code
- Approximate normalization for gradual dependent terms

This is a deliberate simplification. Full gradual dependent types (Eremondi et al., 2019) require sophisticated runtime checks and approximate normalization. We chose separation over complexity.

### 10.3 No Proof Tactics

Unlike Agda, Idris, or Lean, The Fold provides no:
- Tactic language for proof construction
- Proof search or automation
- Holes with goal display
- Interactive proof development

Dependent types are for specification, not theorem proving. Use external proof assistants for serious verification.

### 10.4 Shell is Unverified

The Shell is *trusted but unverified*. We believe it maintains its invariants, but we have not mechanically verified this. The verification boundary is:

```
   ┌─────────────────────────┐
   │   Shell (trusted)       │  ← May have bugs
   ├─────────────────────────┤
   │   Core (verified*)      │  ← *Type-safe by construction
   └─────────────────────────┘
```

Core is verified in the sense that well-typed programs don't go wrong (within fuel bounds). Shell correctness is assured by testing and code review.

### 10.5 Single-Node Only

The current implementation is single-node:
- No distributed CAS
- No peer-to-peer code sharing
- No remote capability delegation

Distributed operation is future work (§11).

### 10.6 IDE Integration Limitations

The Fold includes an LSP implementation (`shell/lsp/`) providing:
- Hover-based type inference for top-level definitions
- Basic diagnostics
- Document synchronization

**Current limitations:**
- No editor plugins packaged (users must configure LSP clients manually)
- No jump-to-definition or find-references yet
- Type inference is per-file only (no project-wide analysis)
- Forward references in the same file may show incomplete types
- Local bindings inside `let` forms are not yet typed

The REPL and command-line tools remain the primary development interface, but LSP support enables basic IDE features for editors that support the protocol.

### 10.7 Floating-Point Algebraic Properties

Algebraic normalization assumes mathematical properties that don't hold perfectly for floating-point arithmetic:

```scheme
;; Mathematically: (+ (+ 1e20 1.0) -1e20) = (+ 1e20 (+ 1.0 -1e20))
;; IEEE 754: (+ (+ 1e20 1.0) -1e20) → 0.0
;;           (+ 1e20 (+ 1.0 -1e20)) → 1.0
```

**Current approach**: We apply associative flattening anyway, accepting that:
1. For exact numbers, the normalization is semantically correct
2. For floating-point, the normalization may change computed results
3. Hash identity implies mathematical equivalence, not IEEE 754 bit-identical results

**Future consideration**: Restrict algebraic canonicalization to exact arithmetic only, or provide an opt-out for numeric-sensitive code.

### 10.8 Metaprogramming Type Interactions

The `quote`/`eval` mechanism has limited type integration:

```scheme
;; quote produces an untyped S-expression
(quote (+ 1 2))  ; type: Sexpr (not (Expr Int))

;; eval has type (→ Sexpr ?)
;; We cannot statically know the result type
```

Typed quotation (as in MetaML or Typed Template Haskell) is not implemented. Metaprogramming operates at the untyped level.

---
