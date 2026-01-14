# Normalization Version 2: Enhanced Semantic Equivalence

**Technical Specification**

---

## Overview

Version 2 (v0x02) normalization extends The Fold's content-addressing system to detect a broader class of semantic equivalences. Where version 0x00 handles α-equivalence (variable renaming) and version 0x01 adds algebraic equivalence (commutative/associative operations), version 0x02 introduces:

1. **η-reduction** — Function wrapper elimination
2. **Identity element elimination** — `(+ x 0)` → `x`
3. **Absorbing element elimination** — `(* x 0)` → `0`
4. **Polynomial canonicalization** — `(+ x x)` ≡ `(* 2 x)`
5. **Hash-consing** — Structural deduplication

These transformations are applied in a specific order before α-normalization (de Bruijn conversion), maximizing the equivalence classes detected while preserving semantic correctness.

---

## Version Byte Semantics

| Version | Byte | Equivalences Detected |
|----|----|----|
| v0 | `0x00` | α-equivalence (variable renaming) |
| v1 | `0x01` | + Commutative, associative, parallel bindings |
| v2 | `0x02` | + η-equivalence, identity/absorbing elements, polynomial equivalence |

The version byte is the first byte of every 33-byte address. Hashes from different versions are guaranteed distinct—the same expression hashed with v0, v1, and v2 produces three different addresses.

---

## Pipeline Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    Input S-expression                │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                      η-Reduction                     │
│         (fn (x) (f x)) → f  when x ∉ FV(f)           │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│              Polynomial Canonicalization             │
│           Lift arithmetic to polynomial form,        |
|               lower to sum-of-products               │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│              Algebraic Canonicalization              │
│    Commutative sorting, associative flattening,      |
|                      binding sort                    │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│           Identity/Absorbing Elimination             │
│         (+ x 0) → x,  (* x 0) → 0                    │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│               α-Normalization                        │
│           Named variables → de Bruijn indices        │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│                    Hash-Consing                      │
│         Structural deduplication via global table    │
└─────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────┐
│              SHA-256 + Version Byte (0x02)           │
│                    33-byte Address                   │
└─────────────────────────────────────────────────────────────┘
```

**Critical: Phase Order**

Identity elimination is applied *after* algebraic canonicalization because flattening may expose new identity opportunities:

```scheme
(+ (+ a 0) b)
  → (+ a 0 b)     ; Associative flattening
  → (+ a b)       ; Identity elimination
```

If we eliminated identities first, we'd miss this case.

---

## Phase 1: η-Reduction

### Theory

In lambda calculus, η-equivalence states:

```
(λx. f x) ≡ f    when x ∉ FV(f)
```

A function that simply applies another function to its argument is equivalent to that function directly.

### Implementation

```scheme
(define (eta-reduce expr)
  (cond
    [(not (pair? expr)) expr]

    ;; (fn (x) (f x)) where x not free in f → f
    [(and (eq? (car expr) 'fn)
          (pair? (caddr expr))              ; Body is application
          (= (length (caddr expr)) 2)        ; Exactly (f x)
          (eq? (cadr (caddr expr))           ; Argument = bound var
               (caadr expr))
          (not (free-in? (caadr expr)        ; x not free in f
                         (car (caddr expr)))))
     (eta-reduce (car (caddr expr)))]        ; Recurse on f

    ;; Recurse into subexpressions
    [else (map eta-reduce expr)]))
```

### Examples

| Input | Output | Reason |
|----|----|----|
| `(fn (x) (f x))` | `f` | η-reducible |
| `(fn (x) (g x x))` | unchanged | x appears twice |
| `(fn (x) (x y))` | unchanged | x in operator position |
| `(fn (y) (fn (x) (f x)))` | `(fn (y) f)` | Nested reduction |
| `(fn (x) ((fn (y) y) x))` | `(fn (y) y)` | Inner function reduces |

### Non-Reduction Cases

η-reduction is intentionally conservative. We do NOT reduce:

1. **Multiple occurrences**: `(fn (x) (g x x))` — x is used twice
2. **Operator position**: `(fn (x) (x y))` — x is applied, not passed
3. **Free in function**: `(fn (x) ((fn (y) x) x))` — x appears in body of inner fn

---

## Phase 2: Polynomial Canonicalization

### Theory

Arithmetic expressions can be represented as multivariate polynomials. By lifting to polynomial form and lowering to a canonical sum-of-products representation, we detect equivalences invisible to syntactic comparison:

```scheme
(+ x x)           ≡ (* 2 x)
(+ (* a b) (* b a)) ≡ (* 2 a b)
(- x x)           ≡ 0
```

### Internal Representation

A polynomial is a list of terms. Each term is `(coefficient monomial)` where monomial is an alist of `(variable . exponent)` pairs, sorted alphabetically:

```scheme
;; 2xy + 3z represented as:
((2 ((x . 1) (y . 1)))    ; 2xy
 (3 ((z . 1))))           ; 3z

;; x² + 2x + 1 represented as:
((1 ((x . 2)))            ; x²
 (2 ((x . 1)))            ; 2x
 (1 ()))                  ; 1 (constant term)
```

### Operations

**Lifting** (`sexpr->poly`):
```scheme
(sexpr->poly 'x)           → ((1 ((x . 1))))
(sexpr->poly 42)           → ((42 ()))
(sexpr->poly '(+ a b))     → ((1 ((a . 1))) (1 ((b . 1))))
(sexpr->poly '(* a b))     → ((1 ((a . 1) (b . 1))))
```

**Polynomial arithmetic**:
- Addition: Concatenate term lists, combine like monomials
- Multiplication: Distribute (all pairs), multiply coefficients, add exponents
- Subtraction: Negate second operand, then add

**Simplification** (`poly-simplify`):
1. Sort terms by monomial (lexicographic on variables)
2. Combine adjacent terms with same monomial (add coefficients)
3. Remove zero-coefficient terms

**Lowering** (`poly->sexpr`):
```scheme
(poly->sexpr '())                        → 0
(poly->sexpr '((3 ())))                  → 3
(poly->sexpr '((1 ((x . 1)))))           → x
(poly->sexpr '((2 ((x . 1)))))           → (* 2 x)
(poly->sexpr '((1 ((a . 1))) (1 ((b . 1))))) → (+ a b)
```

### Constraints

**Exact numbers only**: Floating-point arithmetic violates associativity due to precision loss. We only canonicalize expressions containing exact numbers (integers, rationals):

```scheme
(arithmetic-expr? '(+ 1 x))    → #t   ; Integer, canonicalize
(arithmetic-expr? '(+ 1.5 x))  → #f   ; Float, leave unchanged
```

**Depth limit**: `*poly-canon-max-depth*` (default 10) prevents stack overflow on deeply nested expressions.

**Term limit**: `*poly-canon-max-terms*` (default 100) prevents exponential blowup from expressions like `(+ a b)^n`.

### Examples

| Input | Polynomial | Output |
|----|----|----|
| `(+ x x)` | `((2 ((x . 1))))` | `(* 2 x)` |
| `(+ (* a b) (* b a))` | `((2 ((a . 1) (b . 1))))` | `(* 2 a b)` |
| `(+ 1 2 3)` | `((6 ()))` | `6` |
| `(- x x)` | `()` | `0` |
| `(* (+ a 1) (+ b 1))` | `((1 ((a.1)(b.1))) (1 ((a.1))) (1 ((b.1))) (1 ()))` | `(+ (* a b) a b 1)` |

---

## Phase 3: Algebraic Canonicalization

This phase is inherited from v0x01 and applies:

1. **Commutative sorting**: `(+ b a)` → `(+ a b)`
2. **Associative flattening**: `(+ (+ a b) c)` → `(+ a b c)`
3. **Parallel binding reordering**: Independent `let*` bindings sorted alphabetically
4. **Pure sequence reordering**: Pure `begin` expressions sorted

See `docs/technical-report.md` Section 3.4.2 for full details.

---

## Phase 4: Identity/Absorbing Elimination

### Identity Elements

An identity element `e` satisfies `(op x e) = x` for all `x`:

| Operation | Identity | Example |
|----|----|----|
| `+` | `0` | `(+ x 0)` → `x` |
| `*` | `1` | `(* x 1)` → `x` |
| `-` | `0` (right) | `(- x 0)` → `x` |
| `append` | `()` | `(append xs ())` → `xs` |
| `string-append` | `""` | `(string-append s "")` → `s` |
| `bitwise-ior` | `0` | `(bitwise-ior x 0)` → `x` |
| `bitwise-xor` | `0` | `(bitwise-xor x 0)` → `x` |
| `bitwise-and` | `-1` | `(bitwise-and x -1)` → `x` |

### Absorbing Elements

An absorbing element `a` satisfies `(op x a) = a` for all `x`:

| Operation | Absorbing | Example |
|----|----|----|
| `*` | `0` | `(* x 0)` → `0` |
| `bitwise-and` | `0` | `(bitwise-and x 0)` → `0` |

### Implementation

```scheme
(define (eliminate-identities expr)
  (cond
    [(not (pair? expr)) expr]

    [(has-absorbing? (car expr) (cdr expr))
     (op-absorbing (car expr))]           ; Short-circuit to absorbing

    [(has-identity? (car expr))
     (let* ([args (map eliminate-identities (cdr expr))]
            [filtered (remove-identities (car expr) args)])
       (cond
         [(null? filtered) (op-identity (car expr))]  ; All identity → identity
         [(null? (cdr filtered)) (car filtered)]       ; Single arg → unwrap
         [else (cons (car expr) filtered)]))]

    [else (map eliminate-identities expr)]))
```

### Examples

| Input | Output | Reason |
|----|----|----|
| `(+ x 0)` | `x` | 0 is identity for + |
| `(* x 1)` | `x` | 1 is identity for * |
| `(+ a 0 b 0)` | `(+ a b)` | Multiple identities removed |
| `(+ 0 0 0)` | `0` | All identities → identity itself |
| `(* x 0 y)` | `0` | 0 absorbs for * |
| `(+ (* a 0) b)` | `b` | Inner absorbs to 0, then identity eliminated |

---

## Phase 5: α-Normalization

Convert named variables to de Bruijn indices:

```scheme
(fn (x) x)              → (fn (dv 0))
(fn (x) (fn (y) x))     → (fn (fn (dv 1)))
(fn (x) (fn (y) y))     → (fn (fn (dv 0)))
```

This is the same as v0x00/v0x01. See `docs/technical-report.md` Section 3.4.1.

---

## Phase 6: Hash-Consing

### Theory

Hash-consing ensures that structurally equal expressions share the same memory representation. This provides:

1. **Memory efficiency**: Duplicate structures stored once
2. **Fast equality**: `eq?` (pointer comparison) suffices for structural equality
3. **Memoization foundation**: Cached results keyed by pointer identity

### Implementation

```scheme
(define *cons-table* (make-hashtable equal-hash equal?))
(define *cons-table-hits* 0)
(define *cons-table-misses* 0)

(define (hash-cons x)
  (cond
    [(not (pair? x)) x]                    ; Atoms pass through
    [else
     (let* ([car-v (hash-cons (car x))]    ; Recursively canonicalize
            [cdr-v (hash-cons (cdr x))]
            [probe (cons car-v cdr-v)]
            [cached (hashtable-ref *cons-table* probe #f)])
       (if cached
           (begin (set! *cons-table-hits* (+ 1 *cons-table-hits*))
                  cached)
           (begin (set! *cons-table-misses* (+ 1 *cons-table-misses*))
                  (hashtable-set! *cons-table* probe probe)
                  probe)))]))
```

### Memory Management

The hash-cons table holds **strong references** and grows indefinitely. For long-running processes:

```scheme
(hash-cons-reset!)      ; Clear table, reset counters
(hash-cons-stats)       ; → (hits . misses)
```

Call `hash-cons-reset!` periodically (e.g., between major operations) to prevent unbounded growth.

### Example

```scheme
(define e1 (hash-cons '(+ a b)))
(define e2 (hash-cons '(+ a b)))
(eq? e1 e2)   → #t    ; Same object (pointer equality)
```

---

## API Reference

### Hashing Functions

```scheme
;; Hash with v2 normalization
(hash-sexpr-v2 'tag expr) → bytevector (33 bytes, version 0x02)

;; Compare versions
(hash-sexpr 'tag expr)           ; v0x00: α-only
(hash-sexpr-algebraic 'tag expr) ; v0x01: algebraic + α
(hash-sexpr-v2 'tag expr)        ; v0x02: full v2 pipeline
```

### Normalization Functions

```scheme
;; Full v2 pipeline (with hash-consing)
(normalize-v2 expr) → canonical-expr

;; Without hash-consing (for testing)
(normalize-v2-no-hashcons expr) → canonical-expr

;; Individual phases
(eta-reduce expr) → expr
(poly-canonicalize expr) → expr
(normalize-algebraic expr) → expr
(eliminate-identities expr) → expr
(normalize expr) → expr  ; α-normalization
(hash-cons expr) → expr
```

### Configuration

```scheme
*poly-canon-max-depth*  ; Default: 10
*poly-canon-max-terms*  ; Default: 100
```

### Introspection

```scheme
(hash-cons-stats)   → (hits . misses)
(hash-cons-reset!)  → void

(address-version-byte addr) → byte
(address-v2? addr) → boolean
```

---

## Files

| File | Purpose |
|----|----|
| `core/blocks/normalize.ss` | Main normalization pipeline, v2 integration |
| `core/blocks/hash-cons.ss` | Hash-consing infrastructure |
| `core/blocks/poly-canon.ss` | Polynomial representation and canonicalization |
| `core/blocks/op-properties.ss` | Identity/absorbing element declarations |
| `core/blocks/cas.ss` | `hash-sexpr-v2` and version byte constants |
| `core/blocks/test-normalize.ss` | Test suite (Tests 20-29 for v2) |

---

## Limitations

### Not Handled

1. **Floating-point arithmetic**: Excluded from polynomial canonicalization due to precision issues
2. **Non-arithmetic functions**: `(f x x)` is not recognized as `(* 2 (f x))`
3. **Conditional equivalences**: `(if #t a b)` is not reduced to `a`
4. **Distributive law**: `(* a (+ b c))` is not expanded to `(+ (* a b) (* a c))`
5. **Symbolic simplification**: `(/ (* x y) y)` is not reduced to `x`

### By Design

1. **Power expansion**: `x³` is represented as `(* x x x)`, not `(expt x 3)`. This maintains S-expression purity but is verbose for high powers.

2. **Strong references in hash-cons**: The table holds strong references. Use `hash-cons-reset!` to manage memory in long-running processes.

3. **No cyclic structure support**: The normalizer assumes tree/DAG structures. Cyclic S-expressions (via `#n=` reader syntax) cause infinite loops.

---

## Future Directions

1. **Weak references**: Use weak hash tables if Chez Scheme adds support
2. **Memoized normalization**: Cache normalize results keyed by hash-consed input
3. **Extended η**: Handle more complex wrapper patterns
4. **E-graphs**: Full equality saturation for optimal canonicalization

---

## References

- de Bruijn, N. G. (1972). "Lambda calculus notation with nameless dummies"
- Filliatre, J.-C. & Conchon, S. (2006). "Type-Safe Modular Hash-Consing"
- arXiv:2509.20534 — Hash-consing speedup analysis (3.2x speedup, 2x memory reduction)
