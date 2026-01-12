# QA Pass 2 - Shell Areas Not Covered in Pass 1

**Date:** 2026-01-12
**Method:** Flashmob specialized agents (security, performance, correctness)
**Coverage:** shell/tools, shell/ui, shell/ffi, shell/lsp, shell/diagnostics, shell/debug, shell root-level files

## Executive Summary

| Area | Files | Security | Performance | Correctness | Total HIGH |
|------|-------|----------|-------------|-------------|------------|
| shell/tools | 31 | 2 HIGH, 6 MED | 3 HIGH, 3 MED | 5 HIGH, 5 MED | 10 |
| shell/ui | 20 | 1 HIGH, 3 MED, 1 LOW | 3 HIGH, 5 MED, 2 LOW | 1 HIGH, 2 MED, 2 LOW | 5 |
| shell/ffi | 15 | 1 HIGH, 5 MED, 2 LOW | 2 HIGH, 4 MED | 4 HIGH, 3 MED, 1 LOW | 7 |
| shell/lsp | 14 | 1 HIGH, 2 MED, 1 LOW | 2 HIGH, 3 MED, 1 LOW | 3 HIGH, 4 MED | 6 |
| shell/diagnostics | 9 | 2 HIGH, 3 MED, 1 LOW | 2 HIGH, 3 MED, 1 LOW | 6 HIGH, 4 MED | 10 |
| shell/debug | 8 | - | 1 HIGH, 4 MED, 1 LOW | - | 1 |
| shell root | 72 | 4 HIGH, 3 MED, 1 LOW | - | - | 4 |

**Total: ~98 issues found, 43 HIGH severity**

---

## P0 Security Issues (Immediate Action Required)

### 1. Command Injection - repl-daemon-mcp.ss:135
- **Bead:** fold-d916
- **Issue:** session-id from untrusted filenames passed directly to shell command
- **Fix:** Sanitize session-id to alphanumeric only, or use process spawning API

### 2. Arbitrary Code Execution - rust-loader.ss:159
- **Bead:** fold-lpez
- **Issue:** `eval` with backticked expression containing function name
- **Fix:** Avoid eval, validate name is string with allowed characters

### 3. Path Traversal - patches.ss:77
- **Bead:** fold-td79
- **Issue:** symbol->string conversion allows ../ in patch names
- **Fix:** Validate name doesn't contain path separators

### 4. Command Injection - init-project.ss:323
- **Bead:** fold-rikx
- **Issue:** User input in `name` passed to `system` without sanitization
- **Fix:** Use list-based process API or rigorous sanitization

### 5. DoS via Recursion - lsp/json.ss:349
- **Bead:** fold-xmy4
- **Issue:** JSON parser has no depth limit, can stack overflow
- **Fix:** Implement depth counter, error on max depth exceeded

---

## P1 Correctness Issues (High Impact)

### 1. Broken ANSI Escapes - color.ss:85
- **Bead:** fold-g0iq
- **Issue:** `\x1B;[` instead of `\x1B[` - all colored output broken
- **Fix:** Remove semicolon after escape character

### 2. Invalid JSON - lsp/json.ss:105
- **Bead:** fold-ye7b
- **Issue:** Scientific notation produces invalid JSON like `1e+20.0`
- **Fix:** Check for 'e'/'E' before appending `.0`

### 3. FFI Memory Leaks - bvh-ffi.ss, rust-loader.ss
- **Bead:** fold-zhsu
- **Issue:** foreign-alloc without dynamic-wind protection
- **Fix:** Wrap FFI calls in dynamic-wind for cleanup

### 4. BVH Cache Leak - bvh-cache.ss:15
- **Bead:** fold-vvwe
- **Issue:** Strong hashtable prevents GC, cache grows indefinitely
- **Fix:** Use weak-valued hashtable

### 5. Crash Bugs - shell/tools
- **Bead:** fold-kwj6
- scaffold.ss:60 - cdr of #f on missing template variable
- proof-repl.ss:75 - negative make-string when max-len < 3
- module-deps.ss:155 - directory? called on list instead of path

---

## P1 Performance Issues

### 1. O(N²) Algorithms - shell/tools
- **Bead:** fold-mrb0
- benchmark.ss:218 - O(N²) insertion sort (use list-sort)
- coverage.ss:139 - O(F*E) filtering (pre-group by file)
- dead-code.ss:105 - O(R*F) member on list (use hashtable)

### 2. Canvas Allocation - layout.ss:74, layout-color.ss:70
- **Bead:** fold-1si3
- **Issue:** vector-copy on every pixel change = O(N) per operation
- **Fix:** Use mutable canvas or batch updates

---

## Additional HIGH Severity Issues (Beads Not Created)

### Security
- watch.ss:435 - Command injection in auto-test
- history.ss:270 - Unsafe eval of history entries
- turtle-block.ss:60 - Unsafe deserialization from CAS
- music-gen.ss:185 - Path traversal in export-pattern
- svg-renderer.ss:355 - Arbitrary file write

### Correctness
- lsp/capabilities.ss:435 - All symbols reported on line 1
- lens/call-graph.ss:42 - Incorrect scope handling
- lens/jump.ss:138 - for-each length mismatch crash
- media/wave-synth.ss:45 - mod with real numbers (type error)
- ui/animation.ss:102 - Mathematical error in ease-in-out-expo
- ui/graphics-primitives.ss:485 - Incorrect quad-to Bezier
- ui/profile-viz.ss:45 - Crash with negative make-string

### Performance
- bvh-cache.ss:64 - O(N) serialization on every lookup
- rust-loader.ss:182 - foreign-alloc/free per call
- lsp/documents.ss:158 - O(C*N) document splitting
- lsp/json.ss:110 - O(N²) json-join
- text.ss:234 - O(N*M) Unicode normalization

---

## Files Reviewed

### shell/tools (31 files)
test-init-project.ss, string-utils.ss, effect-lint.ss, index.ss, test-format.ss,
rewrite-repl.ss, refactor-integrated.ss, docgen.ss, scaffold.ss, init-project.ss,
ast-format.ss, format.ss, test-markdown.ss, pattern-lint.ss, coverage.ss,
typed-holes.ss, flow-inspector.ss, termination-check.ss, test-coverage.ss,
manifest-gen.ss, dead-code.ss, validate.ss, refactor.ss, test-rewrite-repl.ss,
proof-repl.ss, test-ast-format.ss, benchmark.ss, markdown.ss, archextract.ss,
module-deps.ss, type-search.ss

### shell/ui (20 files)
layers.ss, text.ss, fuel-viz.ss, graphics.ss, turtle-block.ss, graphics-primitives.ss,
svg-renderer.ss, layout.ss, particles.ss, profile-viz.ss, turtle-svg.ss,
layout-color.ss, easing.ss, transforms.ss, color.ss, animation.ss, turtle-color.ss,
turtle.ss, layout-combinators.ss, turtle-path.ss

### shell/ffi (15 files)
test-bvh-ffi.ss, bvh-cache.ss, test-cache.ss, test-ffi.ss, serialize.ss,
ffi-core.ss, raymarch-ffi.ss, bench-bvh.ss, rust-loader.ss, bytevector-ffi.ss,
test-serialize.ss, bvh-ffi.ss, bench-raymarch.ss, test-rust-loader.ss, bench-bvh-accel.ss

### shell/lsp (14 files)
lsp-transport.ss, state.ss, json.ss, documents.ss, test-protocol.ss, test-json.ss,
test-documents.ss, protocol.ss, capabilities.ss, lsp-server.ss, test-capabilities.ss,
test-diagnostics.ss, diagnostics.ss, start-lsp.ss

### shell/diagnostics (9 files)
profile-repl.ss, profile-analysis.ss, fuel-analysis.ss, alloc-tracker.ss,
perf-monitor.ss, profiler-unified.ss, profile-call-graph.ss, profile-persist.ss,
fuel-profile.ss

### shell/debug (8 files)
xref.ss, session-debugger.ss, exploration-error-handler.ss, session-debug.ss,
debug-repl.ss, type-inspect.ss, error-fmt.ss, error-improvements.ss

### shell root (72 files)
All .ss files in shell/ root directory

---

## Created Beads

1. **fold-d916** - [P0 SECURITY] Command injection via session-id in repl-daemon-mcp.ss:135
2. **fold-lpez** - [P0 SECURITY] Arbitrary code execution via eval in rust-loader.ss:159
3. **fold-td79** - [P0 SECURITY] Path traversal in apply-patch patches.ss:77
4. **fold-rikx** - [P0 SECURITY] Command injection via git init in init-project.ss:323
5. **fold-xmy4** - [P0 SECURITY] DoS via recursive JSON parsing in lsp/json.ss:349
6. **fold-g0iq** - [P1 CORRECTNESS] Invalid ANSI escape sequences in color.ss:85
7. **fold-ye7b** - [P1 CORRECTNESS] Invalid JSON output for scientific notation in lsp/json.ss:105
8. **fold-zhsu** - [P1 CORRECTNESS] Memory leaks in FFI calls - bvh-ffi.ss and rust-loader.ss
9. **fold-vvwe** - [P1 CORRECTNESS] BVH cache grows indefinitely - bvh-cache.ss:15
10. **fold-kwj6** - [P1 CORRECTNESS] Multiple crash bugs in shell/tools
11. **fold-mrb0** - [P1 PERFORMANCE] O(N²) algorithms in shell/tools
12. **fold-1si3** - [P1 PERFORMANCE] Canvas operations O(N) per pixel

---

## Recommendations

1. **Immediate:** Address all P0 security issues before next deployment
2. **High Priority:** Fix ANSI escape bug (all colored output currently broken)
3. **Medium Priority:** Address memory leaks in FFI layer
4. **Batch Fix:** Create a shell security hardening pass to add input sanitization across all `system` calls
5. **Architecture:** Consider adding a centralized input validation layer for shell commands
