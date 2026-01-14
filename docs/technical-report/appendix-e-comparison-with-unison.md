## Appendix E: Comparison with Unison


| Feature | The Fold | Unison |
|----|----|----|
| **Foundation** | Chez Scheme | Haskell |
| **Content Addressing** | SHA-256 + de Bruijn | Hash + type-directed |
| **α-Normalization** | De Bruijn indices | Implicit in hashing |
| **Type System** | Gradual dependent | Abilities (effect types) |
| **Higher-Kinded Types** | Yes (kind system) | Yes |
| **Type Classes** | Dictionary-passing | Abilities |
| **Dependent Types** | Π, Σ, inductive | Limited |
| **GADTs** | Yes | No |
| **Effects** | Capability types + monads | First-class abilities |
| **Module System** | Tiered DAG + manifests | Namespaces |
| **Verification** | Compositional, fuel-bounded | Not emphasized |
| **Dependencies** | None (self-contained) | Haskell ecosystem |
| **Homoiconicity** | Full (S-expressions) | Partial |
| **Metaprogramming** | quote/eval | Limited |
| **Tooling** | BM25 search, DAG nav | Codebase manager |

**Key Philosophical Differences**:

1. **Effects**: Unison treats effects as first-class abilities integrated into the type system. The Fold separates effects into Shell with capability types.

2. **Verification**: The Fold emphasizes compositional verification with fuel bounds. Unison focuses on codebase management.

3. **Dependencies**: The Fold has zero external dependencies. Unison builds on Haskell.

4. **Metaprogramming**: The Fold's homoiconicity enables full quote/eval. Unison's approach is more restricted.

---
