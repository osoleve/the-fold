# Gemini Flash QA Review - 2026-01-03

## Batch 1: Core Utilities

### 1. `core/fp/measure/units.ss` - **High Confidence**
- Division by Zero in `qty/` when divisor value is 0
- Square Root of Negatives in `qty-sqrt`
- SI Prefix floating-point precision drift (yocto/yotta)
- `qty->number` restrictive for normalized calculations

### 2. `shell/tools/archextract.ss` - **Medium-High Confidence**
- **BUG:** `extract-loads` recursion expects list, gets atom in some cases
- Module name ambiguity with same-named files in different directories
- Inefficient `(reverse (cdr (reverse ...)))` for butlast
- Top-level only definition extraction

### 3. `core/query/query-patterns.ss` - **Medium Confidence**
- Type safety: `bytevector=?` assumes all values are hashes
- **LOGIC BUG:** `eval-constraint` uses `bytevector-length` as placeholder
- Slow and unsafe `(eval op)` for comparison operators
- Overly permissive string-contains matching for relations

### 4. `core/types/dep-infer.ss` - **Medium-High Confidence**
- Wildcard `?` handling unclear in type equality
- Variable shadowing risk in substitution
- No check for malformed universe levels
- Non-dependent pair assumption limits inference

### 5. `shell/tools/coverage.ss` - **High Confidence**
- **SEVERE:** O(N²) performance in `increment-coverage!`
- Linear search on every hit
- `find-uncovered-lines` stubbed to return empty
- `instrument-file` not implemented

---

## Batch 2: DSL and Autodiff

### 1. `shell/concept-map.ss` - **High Confidence**
- O(N²) performance in `count-occurrences` and `merge-concepts`
- Shallow parsing misses nested definitions
- Host Scheme reader dependency
- Non-recursive directory scan

### 2. `core/autodiff/reverse-diff.ss` - **High Confidence**
- **THREAD SAFETY:** Global state makes non-thread-safe
- **NESTED AD FAILURE:** `reset-traced-ids!` in gradient breaks nested AD
- Power function limited to constant exponents

### 3. `core/autodiff/higher-order-diff.ss` - **Medium-High Confidence**
- Jacobian inefficiency: m passes instead of single tape
- Hessian O(n²) full gradient computations
- `diff-operator-naive` exponential complexity

### 4. `shell/ui/turtle-color.ss` - **High Confidence**
- Clamping hides bugs in calling code
- `for-all` dependency assumption
- (SVG scaling is correct)

### 5. `core/dsl/tagless.ss` - **High Confidence**
- **BUG:** Eager evaluation of both if-branches breaks conditionals
- `define-algebra` macro incomplete
- Shared memoization cache across programs

---

## Batch 3: Tests and Types

### 1. `core/types/infer.ss` - **High Confidence**
- Non-thread-safe global `*fresh-counter*`
- `infer-with-constraints` is a stub
- Case inference binds all patterns to Hash type

### 2. `core/numeric/test-convolution.ss` - **High Confidence**
- Empty vector edge case crashes `(apply max diffs)`
- Zero variance normalization returns zeros (deviates from z-score)

### 3. `shell/run-tests.ss` - **Medium Confidence**
- Limited coverage (only 2 test files)
- Chez-specific `format-condition`

### 4. `shell/tests/universe-serialize-test.ss` - **High Confidence**
- **BUG:** Missing `docs/decisions/` subdirectory creation
- Unix-specific `rm -rf`

### 5. `core/base/test-error.ss` - **High Confidence**
- Missing `string-split` definition
- Standard Levenshtein vs Damerau-Levenshtein for typo suggestions
