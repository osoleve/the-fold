# Design: Automatic Differentiation Integration with Evaluation Engine

## Status: IMPLEMENTATION PLAN

## Context

The Fold has a fuel-based evaluation engine (`eval.ss`) and reverse-mode automatic differentiation (`reverse-diff.ss`) that currently operate independently. This design integrates them so that differentiation can happen automatically during evaluation.

**Related work:**
- Issue: the-fold-cit (Integrate autodiff with evaluation engine)
- Dependency: the-fold-jqk (Implement reverse mode differentiation) ✅ COMPLETED
- Related: core/types/DESIGN-DIFF-TYPES.md (type-level differentiation - future work)

## Design Goals

1. **Automatic graph construction**: Build computational graph during evaluation
2. **Backward compatibility**: Existing eval code continues to work unchanged
3. **Explicit tracing**: Users explicitly request traced evaluation
4. **Fuel accounting**: Differentiation operations consume additional fuel
5. **Mixed values**: Handle traced and non-traced values together
6. **Integration with primitives**: Differentiable primitives automatically trace

## Current Architecture

### Evaluation Engine (eval.ss)

```scheme
;;; eval : expr × env × fuel → result
;;; Returns: (ok value fuel) | (suspended expr env) | (error ...)
```

**Key forms:**
- `quote`, `fn`, `call`, `let`, `fix`, `case`, `prim`, `if`
- Fuel-based: returns suspension when fuel exhausted
- Pure and total

### Reverse-Mode AD (reverse-diff.ss)

```scheme
;;; Traced values with computation tape
(make-reverse-tape)              ; Create gradient tape
(make-traced-var value tape)     ; Create input variable
(traced-add x y)                 ; Addition with gradient
(gradient f inputs)              ; Compute gradient vector
(reverse-diff f x)               ; Single-variable derivative
```

**Traced operations:**
- `traced-add`, `traced-mul`, `traced-sub`, `traced-div`
- `traced-neg`, `traced-sq`, `traced-sqrt`
- `traced-exp`, `traced-log`, `traced-sin`, `traced-cos`

### Primitive Operations (prim.ss)

**Differentiable primitives:**
- Arithmetic: `add`, `sub`, `mul`, `div`, `neg`, `abs`
- Roots/powers: `sqrt`, `expt`
- Transcendental: `log`, `sin`, `cos`, `tan`
- Rounding: `floor`, `ceiling`, `round`

**Fuel costs (current):**
- Basic arithmetic: 2 fuel
- Division/modulo: 3 fuel
- sqrt/expt: 4 fuel
- Transcendental: 5 fuel

## Proposed Design

### Option B: Separate Traced Evaluator (CHOSEN)

Add `eval-traced` as a parallel evaluator that builds computational graphs.

**Rationale:**
- Preserves existing `eval` contract unchanged
- Clear semantic difference between traced and normal evaluation
- Can share implementation via helper functions
- Users explicitly choose when to trace (no hidden complexity)
- Easier to reason about and test

### Traced Evaluation API

```scheme
;;; eval-traced : expr × env × fuel × tape → result
;;; Evaluate with automatic differentiation enabled.
;;; Returns: (ok traced-value fuel) | (suspended expr env fuel tape) | (error ...)
;;;
;;; All numeric operations automatically build computational graph on tape.
;;; Non-numeric operations behave like normal eval.
```

**Key differences from eval:**
1. Takes `tape` parameter (reverse tape for gradients)
2. Numeric primitives use `traced-*` operations
3. Returns traced values for numeric results
4. Suspension includes tape for resumption

### Implementation Strategy

#### 1. Shared Core (eval-core.ss)

Factor out common evaluation logic:

```scheme
;;; eval-form : form × env × fuel × mode → result
;;; Dispatch on form type.
;;; mode: (normal) | (traced tape)
```

Forms that are mode-independent:
- `quote` - return literal (wrap in traced-const if traced mode)
- `fn` - create closure (same in both modes)
- `if` - conditional (extract primal in traced mode for condition)
- `case` - pattern matching (extract primal for scrutinee)

Forms that differ by mode:
- `prim` - choose normal vs traced operations
- `call` - thread tape through recursive calls
- `let` - thread tape through bindings

#### 2. Primitive Dispatch

```scheme
;;; eval-prim : op × args × fuel × mode → result
(define (eval-prim op args fuel mode)
  (case mode
    [(normal) (apply-normal-prim op args fuel)]
    [(traced tape) (apply-traced-prim op args fuel tape)]))

;;; apply-traced-prim : op × args × fuel × tape → result
(define (apply-traced-prim op args fuel tape)
  (case op
    [(add) (traced-add-with-fuel (car args) (cadr args) fuel)]
    [(sub) (traced-sub-with-fuel (car args) (cadr args) fuel)]
    [(mul) (traced-mul-with-fuel (car args) (cadr args) fuel)]
    ;; ... all differentiable ops
    [else
     ;; Non-differentiable: extract primal, apply, wrap result
     (apply-normal-prim op (map traced-value args) fuel)]))
```

#### 3. Mixed Value Handling

Traced operations must handle mixed traced/constant arguments:

```scheme
;;; traced-add-with-fuel : (Traced|Real) × (Traced|Real) × Fuel → (Traced, Fuel)
(define (traced-add-with-fuel x y fuel)
  (let* ([x-traced (if (traced? x) x (make-traced-const x))]
         [y-traced (if (traced? y) y (make-traced-const y))]
         [fuel-cost 3]  ; base 2 + tracing overhead 1
         [fuel-remain (- fuel fuel-cost)])
    (if (< fuel-remain 0)
        (suspension ...)
        (ok (traced-add x-traced y-traced) fuel-remain))))
```

#### 4. Fuel Cost Model

Differentiation adds overhead to primitive operations:

| Operation | Normal Cost | Traced Cost | Overhead |
|-----------|-------------|-------------|----------|
| `add`     | 2           | 3           | +1       |
| `mul`     | 2           | 3           | +1       |
| `sqrt`    | 4           | 6           | +2       |
| `expt`    | 4           | 6           | +2       |
| `log`     | 5           | 7           | +2       |
| `sin`     | 5           | 7           | +2       |
| `cos`     | 5           | 7           | +2       |

**Overhead reasoning:**
- Simple ops (+, -, *, /): +1 fuel for tape recording
- Complex ops (sqrt, exp, log, trig): +2 fuel for tape + gradient func

#### 5. Suspension with Tape

When fuel exhausts during traced evaluation:

```scheme
(suspended expr env fuel tape)
```

Resumption:
```scheme
(eval-traced-resume suspension additional-fuel)
```

This preserves the computational graph across suspension/resumption cycles.

### User-Facing API

#### High-Level Differentiation

```scheme
;;; diff : expr × var-list → expr
;;; Symbolically differentiate expression.
;;; Returns expression for gradient function.
(define (diff expr vars)
  ...)

;;; Example:
;;; (diff '(* x x) '(x))  →  '(fn (x) (* 2 x))
```

#### Runtime Differentiation

```scheme
;;; eval-and-grad : expr × env × var-names × values × fuel → (value, gradients)
;;; Evaluate expression and compute gradients w.r.t. named variables.
;;;
;;; Example:
;;; (eval-and-grad
;;;   '(+ (* x x) (* y y))
;;;   empty-env
;;;   '(x y)
;;;   '(3 4)
;;;   1000)
;;; → (values 25 '(6 8))
```

Implementation:
```scheme
(define (eval-and-grad expr env var-names values fuel)
  (let* ([tape (make-reverse-tape)]
         ;; Extend environment with traced variables
         [env* (fold-left (lambda (e name val)
                            (env-extend e name (make-traced-var val tape)))
                          env var-names values)]
         ;; Traced evaluation
         [result (eval-traced expr env* fuel tape)])
    (match result
      [(ok traced-val fuel-remain)
       ;; Backward pass
       (backward-pass traced-val tape)
       ;; Extract gradients for input variables
       (let ([grads (map (lambda (name)
                           (get-gradient (env-lookup env* name) tape))
                         var-names)])
         (values (traced-value traced-val) grads))]
      [(suspended ...) (error "fuel exhausted during differentiation")]
      [(error msg) (error msg)])))
```

#### Primitive Gradient Operator

Add to primitive operations:

```scheme
;;; (prim 'grad f x) → gradient of f at x
;;; f : expr evaluating to (fn (x) body)
;;; x : numeric value
```

Implementation uses eval-and-grad internally.

### Integration with Existing Code

#### No Breaking Changes

Existing code using `eval` continues to work:

```scheme
;; Before
(eval '(+ 1 2) empty-env 1000)  → (ok 3 998)

;; After (unchanged)
(eval '(+ 1 2) empty-env 1000)  → (ok 3 998)
```

#### Opt-In Tracing

Users explicitly request traced evaluation:

```scheme
;; New traced evaluation
(let ([tape (make-reverse-tape)])
  (eval-traced '(+ 1 2) empty-env 1000 tape))
→ (ok #<traced:3> 997)
```

### Example: Computing Derivatives

```scheme
;;; Example: Compute d/dx[x²] at x=3

;; 1. Create tape
(define tape (make-reverse-tape))

;; 2. Create traced variable
(define x (make-traced-var 3 tape))

;; 3. Define environment
(define env (env-extend empty-env 'x x))

;; 4. Evaluate expression with tracing
(define result (eval-traced '(* x x) env 1000 tape))
; → (ok #<traced:9> 997)

;; 5. Backward pass
(match result
  [(ok traced-val fuel)
   (backward-pass traced-val tape)
   ;; 6. Extract gradient
   (get-gradient x tape)])
; → 6
```

Using high-level API:

```scheme
(eval-and-grad '(* x x) empty-env '(x) '(3) 1000)
→ (values 9 '(6))
```

## Implementation Plan

### Phase 1: Core Integration (Current Task)

1. ✅ Research existing architecture (eval.ss, reverse-diff.ss, prim.ss)
2. ⬜ Create `eval-traced` function in eval.ss
3. ⬜ Implement traced primitive dispatch
4. ⬜ Add fuel cost overhead for traced operations
5. ⬜ Handle suspension with tape
6. ⬜ Test basic traced evaluation

### Phase 2: Mixed Value Handling

1. ⬜ Implement automatic constant lifting
2. ⬜ Handle traced/non-traced value mixing
3. ⬜ Test mixed expressions

### Phase 3: High-Level API

1. ⬜ Implement `eval-and-grad`
2. ⬜ Add `grad` primitive operation
3. ⬜ Test multi-variable gradients

### Phase 4: Documentation and Examples

1. ⬜ Document traced evaluation API
2. ⬜ Add examples to documentation
3. ⬜ Write integration tests

## Testing Strategy

### Unit Tests (test-eval-traced.ss)

1. **Basic traced evaluation**
   - Traced literals
   - Traced variables
   - Traced arithmetic

2. **Primitive operations**
   - Each differentiable primitive
   - Mixed traced/constant args
   - Fuel consumption

3. **Control flow**
   - Traced if expressions
   - Traced let bindings
   - Traced function calls

4. **Gradients**
   - Single-variable derivatives
   - Multi-variable gradients
   - Chain rule verification

5. **Fuel and suspension**
   - Fuel exhaustion during tracing
   - Resumption with tape
   - Gradient computation after resumption

### Integration Tests

Compare eval-traced results with existing reverse-diff tests:

```scheme
;; reverse-diff.ss test
(reverse-diff (lambda (x) (traced-sq x)) 3)
→ 6

;; Equivalent eval-traced test
(eval-and-grad '(* x x) empty-env '(x) '(3) 1000)
→ (values 9 '(6))
```

## Open Questions

1. **Symbolic differentiation**: Should we also support compile-time differentiation?
   - **Decision**: Defer to future work. Runtime differentiation is more flexible.

2. **Higher-order derivatives**: Should eval-traced support nested tracing?
   - **Decision**: Not in initial implementation. Use hyperdual for second derivatives.

3. **Checkpointing**: Should we support gradient checkpointing for memory efficiency?
   - **Decision**: Defer to Phase 4 (the-fold-acv: gradient storage).

4. **Type integration**: How does this relate to core/types/DESIGN-DIFF-TYPES.md?
   - **Decision**: This is runtime AD. Type-level AD (the-fold-018) will add compile-time checking later.

## Performance Considerations

### Fuel Overhead

Traced evaluation costs ~50% more fuel than normal evaluation due to:
- Tape recording overhead (+1 fuel per operation)
- Gradient function storage (+1-2 fuel for complex ops)

**Example:**
```scheme
;; Normal evaluation of (+ (* x x) (* y y))
;; mul: 2×2 = 4 fuel
;; add: 1×2 = 2 fuel
;; Total: 6 fuel

;; Traced evaluation
;; mul: 2×3 = 6 fuel
;; add: 1×3 = 3 fuel
;; Total: 9 fuel (50% overhead)
```

### Memory Usage

Computational graph grows with expression size:
- Each operation creates a tape entry
- Each entry stores: operation, inputs, gradient function
- Memory cost: O(n) where n = number of operations

For large computations, consider:
- Gradient checkpointing (future work)
- Incremental backward passes
- Tape pruning for dead gradients

## References

- `core/eval.ss` - Fuel-based evaluator
- `core/reverse-diff.ss` - Reverse-mode AD implementation
- `core/prim.ss` - Pure primitive operations
- `core/comp-graph.ss` - Computational graph (forward mode)
- `core/core/types/DESIGN-DIFF-TYPES.md` - Type-level differentiation (future)
- `core/test-reverse-diff.ss` - Existing AD tests

## Notes

This design prioritizes:
1. **Simplicity**: Clear separation between traced and normal evaluation
2. **Correctness**: Leverage existing tested reverse-diff.ss infrastructure
3. **Compatibility**: Zero impact on existing eval code
4. **Extensibility**: Easy to add symbolic differentiation later

The goal is reliable automatic differentiation with minimal complexity, not a comprehensive AD framework. Advanced features (checkpointing, symbolic AD, higher-order derivatives) are deferred to future work.
