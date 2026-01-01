# Design: Differentiable Type System Extensions

## Status: DRAFT - For Review

## Context

The Fold currently has runtime support for automatic differentiation through type classes (`TC-Differentiable`) with instances for `Real`, `Dual`, `Traced`, and `Hyperdual`. However, differentiability is not tracked at the type level, meaning:

1. Type inference cannot determine which expressions are differentiable
2. No compile-time guarantees that only numeric types are differentiated
3. Gradient storage and retrieval must be managed manually
4. Integration with evaluation engine requires runtime checks

This design extends the type system to provide compile-time tracking of differentiable computations.

## Design Goals

1. **Type-Level Differentiability**: Track whether a value is differentiable in its type
2. **Gradient Types**: Explicit types for gradient storage
3. **Type Inference**: Automatically propagate differentiable types through expressions
4. **Safety**: Prevent differentiation of non-numeric types at compile time
5. **Integration**: Seamless integration with existing evaluation and fuel tracking

## Proposed Type System Extensions

### 1. Add `Real` Base Type

**Rationale**: The Differentiable type class uses `Real`, but it's not in `base-types`.

```scheme
(define base-types
  '(Nat Int Bool Char Symbol String Bytes Unit Void Hash
    Real))  ; Add floating-point numbers
```

**Type rules:**
- `Real` represents IEEE 754 double-precision floating-point numbers
- Nat → Real (widening)
- Int → Real (widening)
- Operations: `+, -, *, /, sqrt, exp, log, sin, cos, ...`

### 2. Differentiable Type Constructor `Diff`

**Syntax:**
```scheme
(Diff T)  ; A differentiable value of type T with gradient tracking
```

**Kind:**
```scheme
Diff : * → *  ; Takes a type, returns a type
```

**Examples:**
```scheme
(Diff Real)         ; A differentiable real number
(Diff (Vec Real))   ; A differentiable vector
(Diff (Matrix Real)); A differentiable matrix
```

**Invariants:**
- `T` must be a numeric type (Real, Nat, Int, Vec, Matrix)
- `Diff (Diff T)` is disallowed (no nested differentiation at type level)

### 3. Gradient Type `Grad`

**Purpose**: Explicitly represent gradient values with the same shape as the primal.

**Syntax:**
```scheme
(Grad T)  ; Gradient of type T
```

**Type rules:**
```scheme
primal  : (Diff T) → T       ; Extract primal value
gradient : (Diff T) → (Grad T)  ; Extract gradient
lift    : T → (Diff T)       ; Lift constant (zero gradient)
```

**Examples:**
```scheme
; Scalar
(primal (Diff Real))  → Real
(gradient (Diff Real)) → (Grad Real) ≅ Real

; Vector
(primal (Diff (Vec Real)))  → (Vec Real)
(gradient (Diff (Vec Real))) → (Grad (Vec Real)) ≅ (Vec Real)

; Matrix
(primal (Diff (Matrix Real)))  → (Matrix Real)
(gradient (Diff (Matrix Real))) → (Grad (Matrix Real)) ≅ (Matrix Real)
```

**Implementation note**: `(Grad T)` has the same runtime representation as `T` - it's a compile-time distinction.

### 4. Type Class Constraints

**Numeric Constraint**: Restrict `Diff` to numeric types.

```scheme
; Type class for numeric types
class Num a where
  (+) : a → a → a
  (*) : a → a → a
  (-) : a → a → a
  (/) : a → a → a
  negate : a → a
  fromInteger : Int → a

; Instances
instance Num Real
instance Num Int
instance Num Nat
instance (Num a) => Num (Vec a)
instance (Num a) => Num (Matrix a)
```

**Differentiable Constraint**: Only `Num` types can be `Diff`.

```scheme
; Type rule
(Diff T) is well-formed ⟺ (Num T) holds
```

### 5. Type Inference Rules

**Primitive Operations**: Operations on `Diff` propagate differentiability.

```scheme
; Arithmetic
x : (Diff Real), y : (Diff Real) ⟹ (+ x y) : (Diff Real)
x : (Diff Real), y : (Diff Real) ⟹ (* x y) : (Diff Real)

; Transcendental functions
x : (Diff Real) ⟹ (exp x) : (Diff Real)
x : (Diff Real) ⟹ (sin x) : (Diff Real)

; Mixed operations (constant + differentiable)
x : Real, y : (Diff Real) ⟹ (+ x y) : (Diff Real)
; Implementation: lift x to (Diff Real) implicitly
```

**Function Types**: Functions can be differentiable.

```scheme
; A differentiable function
f : (Diff Real) → (Diff Real)

; Gradient function type
grad : ((Diff Real) → (Diff Real)) → Real → Real

; Higher-order
(∀ a b. (Num a, Num b) => (Diff a) → (Diff b)) → ...
```

**Let Bindings**: Differentiability tracked through let.

```scheme
(let ([x (Diff Real)])
  (+ x x))  ; Result type: (Diff Real)

(let ([x Real])
  (+ x x))  ; Result type: Real (not differentiable)
```

### 6. Gradient Storage Mechanism

**Design**: Use computational graph nodes as gradient keys.

```scheme
; Gradient tape: map from node ID to gradient value
(define-type GradientTape
  (List (Pair NodeId Real)))

; Gradient storage in evaluation context
(define-type EvalContext
  (Record
    [fuel : Nat]
    [env : Environment]
    [grad-tape : GradientTape]))  ; NEW: gradient accumulator
```

**Operations:**
```scheme
; Store gradient for a node
store-gradient! : NodeId → Real → EvalContext → EvalContext

; Retrieve gradient for a node
get-gradient : NodeId → EvalContext → Maybe Real

; Accumulate gradient (for reverse mode)
accumulate-gradient! : NodeId → Real → EvalContext → EvalContext
```

### 7. Integration with Fuel Tracking

**Cost Model**: Differentiation operations consume additional fuel.

```scheme
; Operation costs (fuel units)
(+)  : 1     ; Basic arithmetic
(*)  : 1
(exp): 10    ; Transcendental

; Differentiation overhead
(d+ (Diff Real) (Diff Real)) : 3  ; 1 for op + 2 for gradient bookkeeping
(d* (Diff Real) (Diff Real)) : 3
(dexp (Diff Real))           : 15 ; 10 for exp + 5 for gradient
```

**Fuel exhaustion**: If fuel runs out during gradient computation, return:
```scheme
(left "fuel-exhausted")
```

### 8. Example Type Derivations

**Example 1: Simple gradient**
```scheme
(define f (lambda ([x : (Diff Real)])
            (* x x)))  ; x²

; Type derivation:
; x : (Diff Real)
; x * x : (Diff Real)  [by Diff arithmetic rule]
; f : (Diff Real) → (Diff Real)

; Gradient:
; (grad f 3.0) → 6.0  ; d/dx(x²)|_{x=3} = 2x = 6
```

**Example 2: Chain rule**
```scheme
(define g (lambda ([x : (Diff Real)])
            (exp (* x x))))  ; e^(x²)

; Type derivation:
; x : (Diff Real)
; x * x : (Diff Real)
; exp (x * x) : (Diff Real)
; g : (Diff Real) → (Diff Real)

; Gradient:
; (grad g 2.0) → (* 2 (exp 4) 4) = 8e⁴
```

**Example 3: Type error - non-numeric diff**
```scheme
(Diff String)  ; TYPE ERROR: String is not Num
(Diff Bool)    ; TYPE ERROR: Bool is not Num
```

**Example 4: Nested functions**
```scheme
(define compose-grad
  (lambda ([f : (Diff Real) → (Diff Real)]
           [g : (Diff Real) → (Diff Real)]
           [x : Real])
    (grad (lambda ([y : (Diff Real)]) (f (g y))) x)))

; Type: ((Diff Real) → (Diff Real)) → ((Diff Real) → (Diff Real)) → Real → Real
```

## Type Rules Specification

### Formation Rules

```
─────────────────────
Γ ⊢ Real : *

Γ ⊢ T : *    Num T
──────────────────────
Γ ⊢ (Diff T) : *

Γ ⊢ T : *
────────────────────
Γ ⊢ (Grad T) : *
```

### Introduction Rules

```
Γ ⊢ e : T    Num T
─────────────────────────
Γ ⊢ (lift e) : (Diff T)

Γ ⊢ x : (Diff T)    Γ ⊢ y : (Diff T)
─────────────────────────────────────
Γ ⊢ (d+ x y) : (Diff T)

Γ ⊢ x : (Diff T)    Γ ⊢ y : (Diff T)
─────────────────────────────────────
Γ ⊢ (d* x y) : (Diff T)
```

### Elimination Rules

```
Γ ⊢ e : (Diff T)
────────────────────
Γ ⊢ (primal e) : T

Γ ⊢ e : (Diff T)
──────────────────────────
Γ ⊢ (gradient e) : (Grad T)

Γ ⊢ f : (Diff T) → (Diff T)    Γ ⊢ x : T
──────────────────────────────────────────
Γ ⊢ (grad f x) : T
```

## Implementation Plan

### Phase 1: Type System Core (This Task)

1. ✅ Review existing type system, comp-graph, differentiable type class
2. ⬜ Add `Real` to base-types in `types.ss`
3. ⬜ Add `Diff` type constructor to type grammar
4. ⬜ Add `Grad` type synonym
5. ⬜ Implement `Diff` type predicate and accessors
6. ⬜ Add formation/intro/elim rules to `infer.ss`
7. ⬜ Update `type=?` to handle `Diff` and `Grad`
8. ⬜ Document type rules in this file

### Phase 2: Type Inference Integration (the-fold-018)

1. Extend unification to handle `Diff`
2. Add constraint solving for `Num` type class
3. Implement automatic lifting of constants
4. Add bidirectional type checking for `grad`

### Phase 3: Evaluation Integration (the-fold-cit)

1. Extend eval context with gradient tape
2. Implement gradient storage/retrieval
3. Add fuel costs for diff operations
4. Wire up `grad`, `primal`, `gradient` primitives

### Phase 4: Gradient Storage (the-fold-acv)

1. Design persistent gradient storage in CAS
2. Implement gradient checkpointing
3. Add gradient tape serialization
4. Memory-efficient reverse mode

## Open Questions

1. **Subtyping**: Should `Real <: (Diff Real)` (automatic lifting)?
   - **Decision**: No. Use explicit `lift` for clarity.

2. **Higher-order gradients**: Support `(Diff (Diff Real))`?
   - **Decision**: Defer to Phase 5. Use Hyperdual for now.

3. **Polymorphic gradients**: Allow `(∀ a. Num a => (Diff a) → (Diff a))`?
   - **Decision**: Yes. Essential for generic numeric code.

4. **Effect tracking**: Track AD as an effect `(Eff AD Real)`?
   - **Decision**: Defer. Current capability system is orthogonal.

5. **Vec/Matrix integration**: Should Vec/Matrix be in base-types or constructed?
   - **Decision**: Keep as type constructors. More flexible.

## References

- `core/types.ss` - Current type system
- `core/kinds.ss` - Kind system with HKT support
- `core/infer.ss` - Type inference
- `core/differentiable.ss` - Differentiable type class (runtime)
- `core/comp-graph.ss` - Computational graph
- `core/reverse-diff.ss` - Reverse-mode AD implementation

## Notes

This design is intentionally conservative:
- Leverages existing type class infrastructure
- Minimal changes to core type system
- Clear separation between type-level and term-level AD
- Backward compatible with existing code

The goal is static safety and better error messages, not replacing the runtime AD system.
