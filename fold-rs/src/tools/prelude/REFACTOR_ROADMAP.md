# Prelude Refactoring Roadmap

## Current Status (2025-12-31)

### Completed Work
1. **Converted 18 utility files to let* binding format** (commit 571dcaf)
   - predicate.ss, char.ss, function-ext.ss, control-flow.ss
   - list-util.ss, numeric-util.ss, tree-util.ss, math-util.ss
   - general-util.ss, list-extra.ss, monad-util.ss, plist.ss
   - search-algorithms.ss, sorting.ss, statistics.ss
   - string-util.ss, vector-util.ss, lens.ss

2. **Removed 74 duplicate definitions from missing.ss** (843 lines)
   - Safe removals verified via dependency analysis
   - Backup at missing.ss.bak2
   - missing.ss reduced from 7525 → 6682 lines

3. **Tests passing**: `test_prelude_lowers` passes (171s)

### Remaining Work

#### Phase 1: Remove Internal Duplicates
The 21 remaining duplicates in missing.ss are needed by other definitions within the file.
These need to be reorganized so the canonical version is used instead.

**Duplicates that must stay (internal dependencies)**:
- `char-between?` - used by char-alphabetic?, char-numeric?
- `foldl`, `car`, `cdr` - used throughout
- `map`, `cons`, `list` - foundational
- And 14 more...

**Strategy**: Instead of removing, refactor the dependent code to use the canonical versions from earlier modules.

#### Phase 2: Categorize Unique Definitions
1236 definitions unique to missing.ss need organization:

| Category | Count | Target Module |
|----------|-------|---------------|
| COLLECTION | 54 | collection-advanced.ss (tries, finger trees, ring buffers) |
| COMPARISON | 23 | comparison-ext.ss (sorting algorithms) |
| CONTROL | 68 | control-ext.ss (loops, lifts, arrows) |
| FUNCTION | 34 | function-advanced.ss (partial, arrows) |
| GRAPH | 35 | graph-advanced.ss (BFS, DFS, union-find) |
| LENS | 22 | lens-ext.ss |
| LIST | 222 | list-advanced.ss (zippers, specialized ops) |
| MATRIX | 16 | matrix.ss |
| MONAD | 100+ | monad-advanced.ss (applicatives, alternatives) |
| NUMERIC | 50+ | numeric-advanced.ss (polynomials, special functions) |
| OTHER | 300+ | Needs categorization |

#### Phase 3: Create New Modules
For each category above:
1. Create `{category}.ss` with let* bindings
2. Move definitions from missing.ss
3. Update mod.rs load order
4. Verify tests pass

#### Phase 4: Delete Empty missing.ss
Once all definitions are migrated, delete missing.ss and remove from mod.rs.

## Tools Available

```bash
# Analyze all definitions
python3 analyze-definitions.py

# Find duplicates in missing.ss
python3 find-missing-duplicates.py

# Safely remove duplicates (won't break internal deps)
python3 safe-remove-duplicates.py

# Run prelude lower test
cargo test test_prelude_lowers --lib
```

## File Size Tracking

| File | Lines | Status |
|------|-------|--------|
| missing.ss | 6682 | Work in progress |
| core.ss | ~200 | Stable |
| function.ss | ~150 | Stable |
| list-core.ss | ~100 | Stable |
| ... | ... | ... |

## Success Metrics

- ✅ Prelude lowers without errors
- ⏳ Reduce missing.ss to 0 lines
- ⏳ Eliminate shadowing conflicts
- ⏳ All tests passing

## Next Action

Run full test suite to verify current state, then:
1. Create collection-advanced.ss for trie, ring-buffer, finger-tree operations
2. Create control-ext.ss for loop constructs and lifts
3. Continue migrating categories until missing.ss is empty
