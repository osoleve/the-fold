# Prelude Refactoring Roadmap

## Execution Plan

### Phase 1: Extract Canonical Versions (Day 1)
**Goal**: Create new module files with single canonical definitions

#### Step 1.1: Statistics Module
Extract numeric/statistical functions that are heavily duplicated:
- `stats.ss` (~300 lines)
  - mean, median, mode, variance, std-dev (each defined 4-6 times)
  - percentile, correlation, z-score
  - Canonical versions from `numeric.ss` or earliest definition

**Action**:
```bash
# Create stats.ss with deduplicated versions
# Update mod.rs to load stats.ss before missing.ss
# Run tests: should see 5-10 more passing
```

#### Step 1.2: Rotation & Permutation Module
- `combinatorics.ss` (~250 lines)
  - rotate-left, rotate-right (6 times each!)
  - permutations, combinations, factorial (4 times each)
  - cartesian-product, binomial

#### Step 1.3: Graph Module Extension
- `graph-ext.ss` (~200 lines)
  - graph-add-edge, graph-neighbors, graph-has-edge? (5 times each)
  - Already have `graph.ss`, this extends it

### Phase 2: Monad Cleanup (Day 1-2)
**Goal**: Single source of truth for monads

#### Step 2.1: Consolidate Monad Definitions
- `monad-core.ss` (~350 lines)
  - State monad (currently defined 3 times in missing.ss)
  - Writer monad (3 times)
  - Reader monad (3 times)
  - Keep only the versions we just fixed (with proper lists)

#### Step 2.2: Validation Unification
- Keep `validation.ss` for test framework stuff
- `validation-ext.ss` for additional validators
  - Rename conflicting versions we found:
    - `validate` (simple) vs `validate-result` (with ok/err)
    - `validate-all` (predicates) vs `validate-all-results` (validation types)

### Phase 3: String & List Extensions (Day 2)
#### Step 3.1: String Module Additions
- `string-ext.ss` (~200 lines)
  - string-pad-right (5 times), split-when (5 times)
  - string-take, string-drop, string-count (3 times each)

#### Step 3.2: List Module Additions
- `list-ext.ss` (~300 lines)
  - interleave (5 times), insert-at, remove-at (4 times each)
  - iterate-n, powers-of, triangular-numbers (4 times)

### Phase 4: Remove from missing.ss (Day 2-3)
**Goal**: Incrementally delete migrated sections

#### Process for each module:
1. Create new `{module}-ext.ss` file
2. Add to `mod.rs` load order (before `missing.ss`)
3. Comment out that section in `missing.ss`
4. Run tests - verify no regression
5. Delete commented section
6. Commit

### Phase 5: Final Cleanup (Day 3)
1. Delete `missing.ss` when empty
2. Update `EXPORTS.md` documentation
3. Run full test suite
4. Update PROGRESS.sexp

## Module Dependencies

Load order (respects dependencies):
```
core.ss             # Foundation
function.ss         # Higher-order functions
list.ss             # Basic list ops
equality.ss         # Comparisons
maybe.ss            # Maybe monad
either.ss           # Either monad
numeric.ss          # Basic math
comparison.ss       # Sorting, searching
string.ss           # Basic string ops
collection.ss       # Dict/set ops
control.ss          # Control flow
validation.ss       # Test framework
stream.ss           # Lazy evaluation
datastructures.ss   # Queue, stack, deque
graph.ss            # Basic graph

# NEW MODULES (load before missing.ss):
stats.ss            # Statistics (deduped)
combinatorics.ss    # Permutations, combinations
graph-ext.ss        # Extended graph algorithms
monad-core.ss       # Consolidated monads
validation-ext.ss   # Extended validation
string-ext.ss       # Extended string ops
list-ext.ss         # Extended list ops
interval.ss         # Interval operations
lens.ss             # Lens utilities
arrow.ss            # Arrow type class
sequence.ss         # Sequence generators
format.ss           # Formatting/display
debug.ss            # Debug utilities

tree.ss             # Tree operations
missing.ss          # (SHRINKING, eventually delete)
path.ss             # Path manipulation
encoding.ss         # Base64, hex, etc.
compat.ss           # Scheme compatibility
```

## Success Metrics

### Current State
- Tests: 451/512 passing (88%)
- Shadowing: 847 conflicts

### Target State (after each phase)
| Phase | Expected Passing | Shadowing |
|-------|-----------------|-----------|
| 1 (Stats + Combinatorics) | 465/512 (91%) | ~750 |
| 2 (Monads) | 475/512 (93%) | ~650 |
| 3 (String/List) | 485/512 (95%) | ~500 |
| 4 (Full migration) | 490+/512 (96%+) | ~100 |
| 5 (Delete missing.ss) | 495+/512 (97%+) | 0 |

### Quality Gates
- ✅ No new shadowing (verified by `find-shadows.py`)
- ✅ Test count doesn't decrease
- ✅ Each module < 500 lines
- ✅ Clear semantic boundaries

## Quick Wins (Start Here)

Top 5 functions to deduplicate first (biggest impact):
1. **rotate-left, rotate-right** (6 defs each) → `combinatorics.ss`
2. **variance, median** (6 defs each) → `stats.ss`
3. **graph-add-edge, graph-neighbors** (5 defs each) → `graph-ext.ss`
4. **state-*, writer-*, reader-*** (3 defs each) → `monad-core.ss`
5. **frequencies, split-when, string-pad-right** (5 defs each) → respective modules

Estimated impact: **~60 fewer shadowing conflicts** = 10-15 more tests passing

## Next Action

Start with Phase 1, Step 1.1:
```bash
cd fold-rs/src/tools/prelude
# Create stats.ss with canonical versions
# Will provide the exact content to create
```
