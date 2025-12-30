# Prelude Refactoring Plan

## Problem
- `missing.ss` is 14,793 lines with extensive shadowing/duplication
- Same functions defined 3-5 times with different signatures
- Impossible to maintain or debug
- 61 test failures due to wrong function versions winning

## Current State
- Core modules (core, function, list, etc.): Well-organized, ~50-500 lines each
- `missing.ss`: Chaotic dumping ground with duplicate sections:
  - "More Numeric Utilities" appears 3+ times
  - "More List Utilities" appears 3+ times
  - Monad utilities repeated across sections
  - Validation functions defined 4+ times with different arities

## Proposed Module Structure

### Phase 1: Analyze & Categorize
1. Extract all function definitions from `missing.ss`
2. Group by semantic category
3. Identify duplicates/conflicts
4. Determine canonical signature for each function

### Phase 2: Create Logical Modules
Break `missing.ss` into focused modules (~200-400 lines each):

- **monad.ss**: State, Reader, Writer, Maybe, Either combinators (deduplicated)
- **validation2.ss**: Extended validation beyond validation.ss
- **interval.ss**: Interval operations
- **queue-extended.ss**: Advanced queue/deque/priority queue
- **graph-extended.ss**: Advanced graph algorithms
- **sort-extended.ss**: Advanced sorting beyond comparison.ss
- **lens.ss**: Lens-like nested data utilities
- **combinator.ss**: Advanced combinators
- **predicate.ss**: Predicate combinators
- **sequence.ss**: Sequence generators and utilities
- **format.ss**: Formatting and display
- **debug.ss**: Debugging utilities
- **arrow.ss**: Arrow type class utilities

### Phase 3: Deduplication Strategy
For each conflicting function:
1. Choose ONE canonical version (prefer simpler, more general)
2. Rename specialized versions with suffixes:
   - `validate` (simple: pred → bool)
   - `validate-result` (returns ok/err)
   - `validate-all` (checks multiple predicates)
   - `validate-all-results` (works with validation types)

### Phase 4: Build System
Create `tools/build-prelude.sh`:
- Concatenates modules in dependency order
- Validates no shadowing conflicts
- Generates `prelude-assembled.ss` if needed
- Runs deduplication checks

### Phase 5: Update Assembly
Modify `mod.rs` to:
1. Load new focused modules
2. Remove `missing.ss`
3. Maintain load order for dependencies

## Benefits
- Each module < 500 lines, easily maintainable
- No shadowing conflicts
- Clear semantic organization
- Easier to add new functions
- Better test coverage (can test modules independently)

## Rollout
1. Create new module files alongside existing
2. Migrate functions incrementally
3. Run tests after each migration
4. Delete `missing.ss` when empty
5. Update documentation

## Estimated Impact
- Current: 451/512 tests passing (88%)
- After refactor: Should reach 490+/512 (95%+)
- Remaining failures will be actual semantic bugs, not shadowing
