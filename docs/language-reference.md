# Language Reference

This document provides a reference for the core language features of The Fold, specifically focusing on evaluation strategies.

## Parallel & Sequential Evaluation

The language provides two special forms for controlling the order and concurrency of evaluation: `par` and `pseq`. These forms serve as semantic hints to the runtime system, allowing developers to optimize performance and enforce execution order.

### `par` - Parallel Evaluation Hint

The `par` form hints that two expressions can be evaluated in parallel.

**Syntax:** `(par a b)`

**Semantics:**
*   Evaluates expression `a` and expression `b`.
*   Returns the result of `b`.
*   **Parallelism:** If the runtime supports threading (via `fork-thread`), `a` is evaluated in a separate background thread while `b` is evaluated in the current thread. If threading is unavailable, it falls back to sequential evaluation (equivalent to `pseq`).
*   **Fuel (Cost Model):** In parallel mode, the fuel budget is effectively "cloned".
    *   The main thread continues evaluating `b` with the current fuel.
    *   The background thread evaluating `a` receives a copy of the current fuel.
    *   This models the wall-clock time benefit of parallelism: "spending" computational resources on `a` does not deplete the budget for `b`.

**Error Handling:**
*   If `a` fails (errors), the error is propagated, even if `b` succeeds. This ensures that "background" tasks are not silently ignored if they crash.
*   If `b` fails, its error is propagated as normal.

**Usage Example:**
```scheme
;; Hint that the expensive multiplication can happen
;; in the background while we compute the addition.
(par (prim 'mul 12345 67890)
     (prim 'add 1 2))
;; Returns: 3
```

### `pseq` - Sequential Force

The `pseq` form enforces strict sequential evaluation.

**Syntax:** `(pseq a b)`

**Semantics:**
*   Evaluates expression `a` to completion (value or error).
*   *Then* evaluates expression `b`.
*   Returns the result of `b`.
*   **Ordering:** Guarantees that `a` happens before `b`.

**Usage Example:**
```scheme
;; Ensure the "setup" operation completes before "compute" starts.
(pseq (setup-environment)
      (compute-result))
```

### Key Differences

| Feature | `(par a b)` | `(pseq a b)` |
| :--- | :--- | :--- |
| **Primary Goal** | Optimization / Concurrency | Ordering / Side-effects |
| **Execution** | `a` and `b` may run simultaneously | `a` completes before `b` starts |
| **Fuel** | Fuel is duplicated (if parallel) | Fuel is shared and consumed sequentially |
| **Returns** | Result of `b` | Result of `b` |

### Note on Side Effects
While `par` returns the value of `b`, it executes `a` for its potential side effects or simply to warm up caches/promises. However, be cautious with shared state in `par`, as execution order between the threads is non-deterministic. Use `pseq` when order matters.

## Type System

The Fold features a sophisticated type system supporting higher-rank polymorphism, dependent types, and type classes with functional dependencies.

### Rank-N Polymorphism

The type system supports full Rank-N polymorphism with impredicative instantiation, enabling first-class polymorphic functions without annotations in most cases.

#### What Rank-N Enables

- **First-class polymorphic functions**: Functions that take or return polymorphic functions
- **ST monad pattern**: `runST :: (∀s. ST s a) → a` works without annotation
- **Lens types**: `Lens s t a b = ∀f. Functor f => (a → f b) → s → f t`
- **Existential types via CPS**: `∃a. T ≅ ∀r. (∀a. T → r) → r`

#### Quick Look Inference

The inference algorithm uses "Quick Look" (Serrano et al., ICFP 2020) to guide instantiation decisions by analyzing argument structure before deciding whether to instantiate polymorphic types:

```scheme
;; runST-style: function taking polymorphic argument (NO annotation needed!)
(let* ([apply-id : (→ (∀ (a) (→ a a)) Int)])
      (apply-id (fn (x) x)))  ; Works! Infers identity as polymorphic

;; Polymorphic let bindings used at multiple types
(let ((id (fn (x) x)))
     (if (id #t) (id 1) (id 2)))  ; id used at Bool and Int
```

#### Type Annotations

For complex cases, use the annotation form `(: expr type)`:

```scheme
;; Annotate a lambda with a polymorphic type
(: (fn (x) x) (∀ (a) (→ a a)))

;; Annotate parameter with higher-rank type
(fn ((f : (∀ (a) (→ a a)))) (f 42))
```

#### Polymorphic Recursion

True polymorphic recursion (where `f` calls itself at different type arguments) is **undecidable** and requires explicit annotation:

```scheme
;; This requires annotation because length calls itself
;; at type [[a]] -> Int when processing nested lists
(fix (length : (∀ (a) (→ (List a) Int)))
  (fn (lst)
    (if (null? lst)
        0
        (+ 1 (length (cdr lst))))))
```

For standard recursion (calling at the same type), no annotation is needed:

```scheme
;; This works without annotation
(fix factorial
  (fn (n)
    (if (= n 0) 1 (* n (factorial (- n 1))))))
```

### Subsumption Rules

The type system uses proper higher-rank subtyping:

| Rule | Description |
|------|-------------|
| `∀a. T <: T[τ/a]` | Polymorphic types can be instantiated |
| `T <: ∀a. T` (if `a` not free in context) | Types can be generalized |
| `(A → B) <: (A' → B')` if `A' <: A` and `B <: B'` | Contravariant args, covariant returns |

### Impredicative Unification

Type variables can unify with polymorphic types, allowing:

```scheme
;; xs can be a list of polymorphic functions
(let ((xs : (List (∀ (a) (→ a a)))))
     (map (fn (f) (f 42)) xs))
```

### Skolem Escape Prevention

The type checker prevents unsound programs where rigid type variables (skolems) would escape their scope:

```scheme
;; This is rejected: identity has type ∀a. a→a, not ∀a. a
(let ((bad : (→ (∀ (a) a) Int)))
     (bad (fn (x) x)))  ; Error: type mismatch
```
