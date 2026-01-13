## 9. Related Work


### 9.1 Content-Addressed Systems

**Unison** (Chiusano & Bjarnason) is the closest related work—a programming language with content-addressed definitions. Key differences:

| Aspect | Unison | The Fold |
|--------|--------|----------|
| Normalization | Ability-based hashing | De Bruijn α-normalization |
| Type system | Ability effects | Gradual dependent types |
| Module system | Namespace-based | Tiered DAG with manifests |
| Implementation | Haskell | Chez Scheme (self-contained) |
| Effects | First-class abilities | Capability types + monads |

The Fold's de Bruijn approach provides stronger α-equivalence guarantees. Unison's ability system is more integrated with the type system; The Fold separates effects into Shell.

**IPFS**: Content-addressed storage for arbitrary data. The Fold adapts similar Merkle DAG concepts for code specifically, adding normalization and typing.

**Git**: Content-addressed version control. The Fold can be seen as "Git for computations"—immutable, hash-identified, with verified composition.

**Nix**: Content-addressed builds. Nix addresses build reproducibility; The Fold addresses computation reproducibility at a finer grain.

### 9.2 Dependent Type Systems

**Agda, Idris, Lean**: Full-spectrum dependent types with proof capabilities. The Fold's type system is less powerful (no universe polymorphism, limited tactics) but more practical (gradual typing, dictionary-passing classes).

**Gradual Dependent Types** (Eremondi et al.): Theoretical foundations for combining gradual and dependent types. The Fold implements a conservative subset of these ideas.

### 9.3 Homoiconic Languages

**Lisp tradition**: The Fold continues McCarthy's vision of code-as-data. Unlike traditional Lisps, The Fold adds content addressing and dependent types to the homoiconic foundation.

**Racket**: Advanced macro system and language-oriented programming. The Fold's metaprogramming is simpler but content-addressed.

### 9.4 Module Systems

**ML Modules**: Sophisticated module system with functors and signatures. The Fold's module system is simpler (no functors) but adds verification metadata.

**Backpack**: Mixin modules for Haskell. Similar goals of flexible composition; different mechanisms.

---
