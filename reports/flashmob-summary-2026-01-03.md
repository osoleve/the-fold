# Flashmob QA Report - 2026-01-03

## Configuration
- **Agents**: 5
- **Files per agent**: 4
- **Total files reviewed**: 20
- **Target**: `core/**`

## Summary

| Metric | Count |
|--------|-------|
| Total Issues | 36 |
| High Severity | 7 |
| Medium Severity | 20 |
| Low Severity | 9 |

### By Category

| Category | Count |
|----------|-------|
| Performance | 16 |
| Correctness | 18 |
| Security | 2 |

## High Severity Issues (P1)

### 1. Type Inference Soundness Bug
- **File**: `core/types/infer.ss:245`
- **Category**: Correctness
- **Description**: `infer-let` fails to apply substitution to type environment before generalization, causing potential unsound type inference through over-generalization
- **Confidence**: 1.0

### 2. Exponential Pretty Printer
- **File**: `core/util/pretty.ss:238`
- **Category**: Performance
- **Description**: Wadler-Lindig layout algorithm has exponential complexity due to repeated `flatten` calls on nested groups
- **Confidence**: 0.95

### 3. Dense Matrix Memory Exhaustion
- **File**: `core/data/graph-matrix.ss:70`
- **Category**: Performance
- **Description**: `edges->adjacency-matrix` allocates N^2 vector for large graphs, causing OOM for N>10k
- **Confidence**: 0.95

### 4. Parser State O(N^2) String Copying
- **File**: `core/lang/span.ss:87`
- **Category**: Performance
- **Description**: Parser uses `substring` to copy entire remaining input on every character advance
- **Confidence**: 0.95

### 5. Missing `cons*` Definition
- **File**: `core/dsl/quasi.ss:120`
- **Category**: Correctness
- **Description**: Quasiquote expansion relies on undefined `cons*` function for improper lists
- **Confidence**: 0.9

### 6. Packrat Eviction O(N log N)
- **File**: `core/fp/parsing/parser.ss:1040`
- **Category**: Performance
- **Description**: LRU eviction sorts all 50k entries causing latency spikes
- **Confidence**: 0.95

### 7. Unchecked String Search in Autodoc
- **File**: `core/util/autodoc.ss:182`
- **Category**: Correctness
- **Description**: `parse-see-also` uses `string-find-char` result without null check, crashes on malformed input
- **Confidence**: 1.0

## Files Reviewed

### Agent 1
- `core/linalg/graph-laplacian.ss`
- `core/blocks/expand.ss`
- `core/autodiff/typed-gradients.ss`
- `core/fp/meta/logic.ss`

### Agent 2
- `core/lang/prim.ss`
- `core/quick-test.ss`
- `core/types/infer.ss`
- `core/util/debug.ss`

### Agent 3
- `core/util/pretty.ss`
- `core/fp/numeric/transcendental.ss`
- `core/linalg/vec2.ss`
- `core/data/graph-matrix.ss`

### Agent 4
- `core/lang/span.ss`
- `core/base/prelude.ss`
- `core/dsl/quasi.ss`
- `core/fp/control/effects.ss`

### Agent 5
- `core/bench-prim.ss`
- `core/dsl/tagless.ss`
- `core/fp/parsing/parser.ss`
- `core/util/autodoc.ss`

## Detailed Reports

See individual agent reports:
- `reports/gemini-flashmob-agent-1.json`
- `reports/gemini-flashmob-agent-2.json`
- `reports/gemini-flashmob-agent-3.json`
- `reports/gemini-flashmob-agent-4.json`
- `reports/gemini-flashmob-agent-5.json`
