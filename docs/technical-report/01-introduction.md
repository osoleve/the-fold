## 1. Introduction


### 1.1 The Problem with File-Based Programming

Traditional programming systems identify code by *location*: file paths, module names, package versions. This conflation of identity with storage creates fundamental problems:

1. **Semantic drift**: The same file path can refer to different code at different times
2. **Dependency hell**: Version conflicts arise from name-based resolution
3. **α-equivalence violation**: `(λ x. x)` and `(λ y. y)` are stored differently despite identical semantics
4. **Non-reproducibility**: Builds depend on mutable external state

Consider two developers who independently write the identity function:

```scheme
;; Developer A
(define id-a (lambda (x) x))

;; Developer B
(define id-b (lambda (y) y))
```

In file-based systems, these are distinct entities requiring coordination. Yet semantically, they are the same function. This gap between syntax and semantics pervades software engineering.

### 1.2 The Proposal: Content-Addressed Homoiconic Computation

The Fold addresses these problems through three interlocking mechanisms:

1. **Content Addressing**: Every value's identity is its cryptographic hash. Two values with the same content have the same identity—automatically, universally, permanently.

2. **α-Normalization**: Before hashing, expressions are normalized using de Bruijn indices, eliminating variable naming from identity. `(λ x. x)` and `(λ y. y)` normalize to `(λ (dv 0))` and hash identically.

3. **Homoiconicity**: Code is data. Programs are S-expressions that serialize to blocks, enabling introspection, metaprogramming, and uniform treatment of all computational artifacts.

The result is a system where *semantic identity replaces syntactic identity*. Functions that behave the same are the same. Verified code stays verified. Dependencies are content, not names.

### 1.3 Contributions

This report presents three primary contributions:

**Contribution 1: Block Calculus with Multi-Phase Normalization**

We formalize a calculus where computation operates over content-addressed blocks. The key innovation is integrating a two-phase normalization pipeline with cryptographic hashing:

1. **Algebraic canonicalization**: Sort arguments of commutative operations, flatten associative operations, reorder independent bindings
2. **α-normalization**: Convert to de Bruijn indices, eliminating variable naming

This yields the semantic identity property:

```
α-equiv(e₁, e₂) ⟹ hash(normalize(e₁)) = hash(normalize(e₂))
(+ a b) ≡_hash (+ b a)           ; Commutative equivalence
(+ (+ a b) c) ≡_hash (+ a b c)   ; Associative equivalence
```

This provides semantic identity at the language level, not as an afterthought.

**Contribution 2: Gradual Dependent Type System**

We implement a type system combining:
- Bidirectional type checking for predictable inference
- Dependent types (Π, Σ) for precise specifications
- Higher-kinded types and type classes for abstraction
- Gradual typing through holes for incremental development

The system is *sound where types are known* while permitting incomplete specifications during development.

**Contribution 3: Compositional Module System**

We organize code into a tiered DAG where each module declares:
- Dependencies (other modules)
- Purity (total, partial, effectful)
- Complexity bounds (fuel consumption)

This enables *compositional verification*: verifying a module requires only verifying its code against already-verified dependencies, not the entire transitive closure.

### 1.4 Paper Organization

- **Section 2**: System architecture—the three-layer model and its rationale
- **Section 3**: The block machine—content addressing, normalization, storage
- **Section 4**: The block calculus—syntax, operational semantics, shell implementation, metaprogramming
- **Section 5**: The type theory—dependent types, bidirectional checking, gradual typing
- **Section 6**: The module system—DAG structure, verification, discovery
- **Section 7**: Implementation—technology choices, performance, developer experience
- **Section 8**: Evaluation—benchmarks and case study
- **Section 9**: Related work—comparison to Unison, IPFS, dependent type systems
- **Section 10**: Limitations and non-goals—honest scoping of what The Fold does not provide
- **Section 11**: Future work
- **Section 12**: Conclusion

---
