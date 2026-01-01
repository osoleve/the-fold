# Repository Cleanup Plan

**Date:** 2026-01-01
**Context:** Post-Rust removal cleanup (commits 472d6a8, a77e4b6)

## Executive Summary

The Rust daemon (`fold-rs`) was removed in commit a77e4b6, but **26+ files** remain that reference or depend on the deleted Rust code. This plan identifies files for removal, archival, and updating.

**Impact:** ~100KB of dead code, 10 broken scripts, 12 experimental test files, 4 obsolete documentation files.

---

## Category 1: IMMEDIATE DELETION (High Priority)

### Broken Rust Maintenance Scripts
**Status:** Will fail if executed
**Location:** `ops/scripts/`

These 9 scripts reference the non-existent `fold-rs/` directory and will fail:

1. ✗ `ops/scripts/rust-maintenance.sh` - Runs cargo udeps/audit/update on deleted project
2. ✗ `ops/scripts/audit.sh` - Cargo audit wrapper
3. ✗ `ops/scripts/mutants.sh` - Mutation testing for Rust code
4. ✗ `ops/scripts/watch.sh` - File watcher for fold-rs
5. ✗ `ops/scripts/semver-checks.sh` - Semantic versioning checks
6. ✗ `ops/scripts/deny.sh` - Cargo dependency security checks
7. ✗ `ops/scripts/coverage.sh` - Test coverage for Rust (references `fold-rs/target/llvm-cov/`)
8. ✗ `ops/scripts/nextest.sh` - Next-gen Rust test runner
9. ✗ `ops/scripts/store-validator.sh` - Validation tool using fold-rs

### Broken Cron Job

10. ✗ `ops/cron/rust-maintenance` - Daily cron job at 2:00 AM running rust-maintenance.sh

**Action:** Delete all 10 files

---

## Category 2: OBSOLETE DOCUMENTATION

### Rust Coverage Report

1. ✗ `RUST-COVERAGE.md` (343 lines, 12KB)
   - Comprehensive coverage report of deleted Rust daemon
   - References `./fold-rs/target/release/fold-repl` binaries
   - Describes "currently running" Rust daemon (outdated)

**Action:** Delete

### Rust Migration Design Documents

**Location:** `forum/design/`

These document the abandoned Rust migration project:

2. `forum/design/rust-migration-tracker.ss` (28 KB)
   - Project tracker for "Chez → Core/Mantle/Crust" migration
   - Milestone plan from 2026-01-26 to 2026-02-06
   - **Archival value:** Documents decision-making process

3. `forum/design/rust-migration-architecture.ss` (10 KB)
   - Architectural proposal for Rust implementation
   - **Archival value:** Design rationale

4. `forum/design/rust-migration-review-findings.ss` (9.6 KB)
   - External review feedback identifying blocking issues
   - **Archival value:** Why the migration was abandoned

**Decision needed:** Delete or move to `archive/design-decisions/rust-migration/`?

**Recommendation:** Move to archive (preserves institutional knowledge)

---

## Category 3: FILES REQUIRING UPDATES

### fold.sh (143 lines)

**Dead code sections:**

- **Lines 10-14:** Comments about `FOLD_USE_SCHEME` and Rust backend
  ```bash
  # Environment:
  #   FOLD_USE_SCHEME=1  — Force Scheme backend (skip Rust)
  #
  # When daemon is not running, prefers Rust backend...
  ```

- **Lines 31-33:** Dead variable
  ```bash
  RUST_REPL="$SCRIPT_DIR/fold-rs/target/release/fold-repl"
  ```

- **Lines 49-66:** Large dead code block checking for Rust binary

- **Lines 70-71:** Error message suggests building Rust daemon

**Action:** Remove Rust references, simplify to Scheme-only

---

### ops/scripts/install-git-hooks.sh (162 lines)

**Dead code section:**

- **Lines 107-143:** Conditional Rust checks (cargo fmt, cargo clippy)
  - Runs only if `fold-rs/` exists (it doesn't)
  - Safe to delete this entire section

**Action:** Remove lines 107-143

---

### .gitignore

**Lines 45-47:** Rust artifact patterns
```
fold-rs/coverage.lcov
fold-rs/target/llvm-cov/
```

**Action:** Remove these 2 lines

---

## Category 4: EXPERIMENTAL TEST FILES (Optional Cleanup)

**Location:** Root directory
**Pattern:** `test-*.ss` (excluding `test-all.ss`)

These appear to be TDD/debugging artifacts from development:

| File | Size | Purpose | Keep? |
|------|------|---------|-------|
| `test-all.ss` | 10,886 bytes | **Main test runner** | ✓ KEEP |
| `test-bluegown-tags-opus.ss` | 3,939 bytes | Opus tagging test | ? |
| `test-enhanced-errors.ss` | 62 bytes | Minimal stub | ✗ |
| `test-error-core.ss` | 1,090 bytes | Error formatting | ? |
| `test-fold-simple.ss` | 949 bytes | Simple fold test | ✗ |
| `test-help-only.ss` | 33 bytes | Just loads help | ✗ |
| `test-minimal-error.ss` | 264 bytes | Minimal error test | ✗ |
| `test-simple-error.ss` | 210 bytes | Simple error test | ✗ |
| `test-simple-error2.ss` | 205 bytes | Error test variant | ✗ |
| `test-tdd-comprehensive.ss` | 1,020 bytes | TDD system test | ? |
| `test-tdd-final.ss` | 178 bytes | Minimal stub | ✗ |
| `test-tdd-simple.ss` | 441 bytes | Simple TDD test | ✗ |
| `test-tdd.ss` | 35 bytes | Just loads module | ✗ |

**Recommendation:**

- **Keep:** `test-all.ss` (main test runner)
- **Evaluate:** `test-bluegown-tags-opus.ss`, `test-error-core.ss`, `test-tdd-comprehensive.ss`
  *(Check if these are referenced anywhere or contain unique test logic)*
- **Delete:** 9 minimal stubs (< 500 bytes each, likely debug artifacts)

**Impact:** Removes ~510 lines of experimental code

---

## Category 5: FILES NEEDING THOROUGH REVIEW

These files require careful inspection before making decisions:

### 1. `fold.sh` (143 lines)
**Why:** Central script, needs surgical Rust removal
**Review for:**
- Ensure daemon path is correct
- Verify fallback behavior works
- Test with and without daemon running

### 2. `ops/scripts/install-git-hooks.sh` (162 lines)
**Why:** Git pre-commit infrastructure
**Review for:**
- Ensure Scheme checks still work after Rust removal
- Verify hooks still function correctly

### 3. `forum/design/rust-migration-*.ss` (3 files, ~47 KB)
**Why:** Archival vs. deletion decision
**Review for:**
- Institutional knowledge value
- Historical context for future decisions
- Whether to archive or delete

### 4. Experimental test files (3 files: bluegown-tags-opus, error-core, tdd-comprehensive)
**Why:** May contain unique test logic
**Review for:**
- Check if referenced in test-all.ss
- Check if any test unique functionality
- Verify no other scripts depend on them

### 5. `tests/` directory vs `thimble/tests/` directory
**Why:** Potential duplication
**Review for:**
- Consolidation opportunities
- Whether `tests/` should be moved to `archive/`
- Integration tests vs unit tests organization

---

## Cleanup Checklist

### Phase 1: Safe Deletions (No Risk)
- [ ] Delete 9 broken Rust scripts in `ops/scripts/`
- [ ] Delete `ops/cron/rust-maintenance`
- [ ] Delete `RUST-COVERAGE.md`
- [ ] Delete 9 minimal test stubs (<500 bytes)

**Total:** 19 files, ~15 KB

### Phase 2: Code Updates (Low Risk)
- [ ] Update `fold.sh` - remove Rust references
- [ ] Update `ops/scripts/install-git-hooks.sh` - remove lines 107-143
- [ ] Update `.gitignore` - remove lines 45-47

**Total:** 3 files, ~50 lines removed

### Phase 3: Archival Decision (Requires Review)
- [ ] Review `forum/design/rust-migration-*.ss` files
- [ ] Decision: Archive or delete?
- [ ] If archive: Create `archive/design-decisions/rust-migration/`
- [ ] Move 3 files to archive

**Total:** 3 files, ~47 KB

### Phase 4: Test File Evaluation (Requires Review)
- [ ] Inspect `test-bluegown-tags-opus.ss`
- [ ] Inspect `test-error-core.ss`
- [ ] Inspect `test-tdd-comprehensive.ss`
- [ ] Decision: Keep, consolidate, or delete?

**Total:** 3 files, ~5 KB

### Phase 5: Testing & Validation
- [ ] Run `test-all.ss` to verify no tests broken
- [ ] Test `fold.sh` with daemon running
- [ ] Test `fold.sh` without daemon
- [ ] Test git pre-commit hooks
- [ ] Verify cron jobs aren't broken

---

## Summary Statistics

| Category | Files | Size | Risk |
|----------|-------|------|------|
| Broken scripts | 10 | ~5-10 KB each | None (already broken) |
| Obsolete docs | 1 | 12 KB | None |
| Design docs (archival) | 3 | 47 KB | Low (move to archive) |
| Experimental tests | 12 | ~5 KB | Low (verify first) |
| Code updates | 3 | ~50 lines | Low (well-scoped) |
| **TOTAL** | **29 files** | **~100 KB** | **Mostly safe** |

---

## Risks & Mitigations

### Risk 1: Deleting actively used test files
**Mitigation:** Grep codebase for references before deletion

### Risk 2: Breaking git hooks
**Mitigation:** Test hooks after updating install-git-hooks.sh

### Risk 3: Breaking fold.sh fallback behavior
**Mitigation:** Test with daemon stopped

### Risk 4: Losing institutional knowledge
**Mitigation:** Archive Rust design docs instead of deleting

---

## Notes

- The Rust daemon was removed in commits:
  - **472d6a8:** "Delete fold-rs directory" (removed 39,496 lines)
  - **a77e4b6:** "refactor: Remove Rust daemon, use Chez Scheme only"

- All paths assume working directory: `/home/user/the-fold/`

- This cleanup focuses on **removing broken/obsolete code**, not refactoring working code

---

## Recommended Execution Order

1. **Phase 1** (safe deletions) → Immediate
2. **Phase 2** (code updates) → Test thoroughly
3. **Phase 5** (validation) → Verify everything works
4. **Phase 3** (archival decision) → Consult with team
5. **Phase 4** (test evaluation) → Low priority

---

## Questions for Review

1. Should Rust design docs be archived or deleted?
2. Are any of the experimental test files actually used?
3. Should `tests/` directory be consolidated with `thimble/tests/`?
4. Are there other Rust references in documentation we haven't found?
