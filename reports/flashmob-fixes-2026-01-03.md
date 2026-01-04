# Flashmob QA Fixes - 2026-01-03

## Executive Summary

Over three rounds of flashmob QA review using Gemini Flash agents, we identified and fixed **25 bugs** across the core codebase:

| Priority | Found | Fixed | Categories |
|----------|-------|-------|------------|
| P0 (Critical) | 4 | 4 | Crashes, infinite loops |
| P1 (High) | 11 | 11 | Soundness, security, performance |
| P2 (Medium) | 10 | 10 | Edge cases, O(N²) algorithms |
| **Total** | **25** | **25** | |

---

## P0 Critical Fixes

### 1. Parser: many-till Infinite Loop
**File**: `core/fp/parsing/parser.ss:575`

The `many-till` combinator could hang forever when the body parser succeeded without consuming input. Added position tracking to detect non-consumption and fail gracefully.

```scheme
;; Now tracks position before/after body parser
(if (= start-offset end-offset)
    (left (make-parse-error pos "many-till: infinite loop detected" '()))
    (loop (cons val acc) new-state))
```

### 2. Convolution: Empty Vector Crash
**File**: `core/numeric/convolution.ss:38`

`convolve-direct-full` crashed with negative vector allocation when given empty inputs. Added early guard returning empty vector.

### 3. Cross-Entropy: Wrong Value on Support Mismatch
**File**: `core/info-theory/entropy.ss:167`

Cross-entropy returned 0 instead of +inf.0 when Q(x)=0 but P(x)>0. Fixed to detect support mismatch and return infinity.

### 4. Stage-Fanout: Breaks on Effects
**File**: `core/pipeline/stage.ss:235`

The `&&&` combinator failed when branches returned effects. Now properly composes effectful branches into `fanout-effect`.

---

## P1 High-Severity Fixes

### Effect System (core/fp/control/effects.ss)

1. **Effect Delegation**: `run-reader`, `run-nondet`, `run-async-sync` now delegate unknown effects instead of erroring. Enables proper effect composition.

2. **Writer Crash**: `writer-listen`/`writer-pass` now check if inner result is an effect before destructuring.

3. **O(N²) Bind**: Implemented Codensity transformation with bind queues for O(1) monadic bind operations.

### Type System (core/types/dep-types.ss)

4. **Capture-Avoiding Substitution**: `dep-subst-type` now alpha-renames to avoid variable capture. Fixed multi-parameter Pi type handling.

### Numerical (core/linalg/matrix-eigen.ss, core/numeric/complex.ss)

5. **QR Complex Eigenvalues**: Detects 2x2 blocks representing complex conjugate pairs. Returns structured result for complex eigenvalues.

6. **complex-pow 0^(bi)**: Returns NaN for mathematically undefined 0^(purely imaginary).

### Parsing (core/fp/parsing/fsm.ss)

7. **FSM Complement**: `fsm-complete` now handles explicit empty transition lists.

### Random Effects (core/fp/control/random-effect.ss)

8. **random-split**: Uses second child for continuation, ensuring independent streams.

### Query (core/query/query-dsl.ss)

9. **UTF-8 Crash**: Added `safe-utf8->string` wrapper for binary payloads.

### Pipeline (core/pipeline/dsl.ss, council.ss)

10. **Retry Policy**: Returns `stage-retry` with computed delay (pure core, interpreter handles wait).

11. **Effect Tags**: Standardized all stages to use `'stage-effect` tag.

---

## P2 Medium-Severity Fixes

### Parser Combinators

| Bug | File | Fix |
|-----|------|-----|
| decimal trailing dots | parser.ss:709 | `try` around fractional part |
| at-most no backtrack | parser.ss:857 | Wrap in `try` |
| INI O(N²) trim | parser-examples.ss:420 | Single-pass algorithm |

### Query System

| Bug | File | Fix |
|-----|------|-----|
| unique O(N²) | query.ss | Hash table |
| count-occurrences O(N²) | query.ss | Hash table |
| path traversal weak | patterns-parse.ss | Block absolute paths, all '..' |
| no hierarchical tags | patterns-parse.ss | Colon in char-value? |

### Data Structures

| Bug | File | Fix |
|-----|------|-----|
| collection-all? inconsistent | collection-utils.ss:142 | Skip missing blocks |
| collection-group-by O(N*G) | collection-utils.ss:175 | Hash table |
| same-hash-set? O(N*M) | graph-algorithms.ss:500 | Hash table |

---

## Patterns Observed

### Common Bug Types

1. **O(N²) Algorithms**: 8 instances of quadratic complexity from list operations (`member`, `assoc`, `reverse` in loops). All fixed with hash tables.

2. **Missing Edge Cases**: 6 instances of empty/null input handling. Added guards.

3. **Effect Composition**: 4 instances of handlers not delegating unknown effects. Fixed with consistent pattern.

4. **Backtracking**: 3 parser combinators not using `try` for proper backtracking.

### Hot Spots

- `core/fp/control/effects.ss` - 5 bugs
- `core/fp/parsing/*.ss` - 5 bugs  
- `core/query/*.ss` - 4 bugs
- `core/data/*.ss` - 3 bugs

---

## Commits

1. `cf22e30f` - Sparse matrix O(N²) and PRNG security
2. `0f9208f8` - many-till infinite loop, convolution crash
3. `b6ceef9a` - cross-entropy, stage-fanout
4. `75baed70` - 6 P1 fixes (random-split, UTF-8, complex-pow, fsm, retry, tags)
5. `d114be7c` - 5 complex P1 fixes (effects, types, linalg)
6. `c5d97dbd` - 10 P2 fixes (parser, query, data)

---

## Methodology

1. **QA Phase**: Gemini Flash agents review 3-5 files each in parallel
2. **Triage Phase**: Second agents validate findings (reduces ~40% false positives)
3. **Fix Phase**: Claude spawns parallel agents to fix confirmed bugs
4. **Test Phase**: All test suites run after each fix batch
5. **Commit Phase**: Atomic commits with detailed messages

Total test files: 27 | All passing after fixes.
