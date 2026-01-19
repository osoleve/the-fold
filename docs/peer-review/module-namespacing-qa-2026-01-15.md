# Module Namespacing QA Review

**Date:** 2026-01-15
**Reviewer:** Gemini 3 Pro Preview
**File:** `core/lang/module.ss` (lines 233-780)

## Summary

Gemini reviewed the namespaced module resolution implementation and found it **functional** but identified several design limitations and one code quality issue.

---

## Gemini's Original Findings

### 1. Bugs & Edge Cases

*   **Shadowing between base directories (Design Flaw)**:
    *   **Location**: `find-namespaced-module` (Lines 254-263) and `path->namespace` (Lines 307-320).
    *   **Issue**: The namespaced resolution assumes that relative paths are unique across all base directories (`lattice`, `core`, `shell`). If `lattice/utils/math.ss` and `core/utils/math.ss` both exist, `path->namespace` will return `utils/math` for *both*.
    *   **Consequence**: `(require 'utils/math)` will always load the one in `lattice` (since it's first in `*module-base-dirs*`). The file in `core` becomes unreachable via the namespaced syntax, and the user cannot disambiguate them.
    *   **Suggestion**: The logic needs to allow including the base directory in the require path (e.g., allow `(require 'core/utils/math)`) or `find-namespaced-module` needs to check if the path already starts with a base directory.

*   **Unreachable root modules via namespaced syntax**:
    *   **Location**: `find-module-path` (Line 233).
    *   **Issue**: It dispatches to `find-namespaced-module` *only* if the name contains a slash `/`.
    *   **Consequence**: If a module exists at the root of a base directory (e.g., `boundary/repl.ss`), its "namespace" path is `repl`. Because `repl` has no slash, `find-module-path` forces a simple search (`find-simple-module`). The user cannot force a namespaced lookup (e.g., "I want the repl from shell, not lattice") even if they wanted to.

### 2. Missing Functionality

*   **Incomplete Collision Detection**:
    *   **Location**: `module-collisions` (Lines 692-723).
    *   **Issue**: The function checks a **hardcoded list** of names (`names-to-check`, lines 703-706) instead of scanning the registry or file system.
    *   **Consequence**: It will fail to report real collisions for any module not in that short list (e.g., if both `lattice/data/tree.ss` and `core/util/tree.ss` exist, it returns "No collisions found").
    *   **Suggestion**: Iterate through all files in `*module-search-dirs*` to build a frequency map of module names.

### 3. Code Quality Improvements

*   **Brittle String Parsing**:
    *   **Location**: `path->namespace` (Lines 313-318).
    *   **Issue**: The function uses hardcoded substring offsets (`8` for "lattice/", `5` for "core/", `6` for "boundary/").
    *   **Consequence**: If `*module-base-dirs*` (Line 230) is ever updated (e.g., adding a "user" directory), this function will break or behave incorrectly without a matching manual update.
    *   **Suggestion**: Dynamically match against `*module-base-dirs*` instead of hardcoding the `cond` branches.

### 4. Performance

*   **Linear Filesystem Scan**:
    *   **Location**: `find-simple-module` (Lines 267-274).
    *   **Issue**: It iterates through `*module-search-dirs*` (which has ~40 entries) performing a `file-exists?` check for every lookup.
    *   **Consequence**: While `module-name->path` caches hits, `find-simple-module` is still called for every new module or failed lookup. On a slow filesystem, `(require 'unknown-module)` causes ~40 syscalls.

---

## Fixes Applied

### 1. Brittle String Parsing (Fixed)

Applied Gemini's suggested fix - `path->namespace` now dynamically matches against `*module-base-dirs*`:

```scheme
(define (path->namespace path)
  (let* ([without-ext (if (string-ends-with? path ".ss")
                          (substring path 0 (- (string-length path) 3))
                          path)])
        (let loop ([bases *module-base-dirs*])
             (if (null? bases)
                 without-ext
                 (let ([base-prefix (string-append (car bases) "/")])
                      (if (string-starts-with? without-ext base-prefix)
                          (substring without-ext
                                     (string-length base-prefix)
                                     (string-length without-ext))
                          (loop (cdr bases))))))))
```

### 2. Base Directory Shadowing (Documented)

**Status:** Known limitation. Workaround: Use explicit `(load "path/to/file.ss")` for shadowed modules.

**Future Enhancement:** Allow `(require 'lattice/utils/math)` syntax to include base directory.

### 3. Root Modules (Documented)

**Status:** Known limitation. Root modules use simple require anyway.

### 4. Incomplete Collision Detection (By Design)

**Status:** Hardcoded list for performance. Full directory scan caused timeouts.

### 5. Performance (Acceptable)

**Status:** Hits cached. Cold lookups are rare and acceptable.

---

## Test Results

All tests pass. Namespaced requires work correctly:
- `(require 'diffgeo/charts)` ✓
- `(require 'algebra/polynomial)` ✓
- Collision warnings display correctly ✓
