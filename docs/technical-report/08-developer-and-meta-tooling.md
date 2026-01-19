## 8. Developer and Meta-Tooling

The Fold's introspectable architecture enables powerful developer tooling built directly on the system's primitives. This chapter documents the meta-tooling ecosystem that supports both human developers and AI agents working on and with The Fold.

### 8.1 The Manifest-Driven Architecture

Every skill in the lattice is described by a `manifest.sexp` file—a machine-readable specification that serves as the single source of truth for the skill's metadata.

**Manifest Schema**:

```scheme
(skill <name>
  (version "x.y.z")
  (tier 0-2)                       ; 0=foundational, 1=intermediate, 2+=advanced
  (path "lattice/<name>")
  (purity total|partial)           ; total=pure, partial=may have effects
  (stability stable|experimental)
  (fuel-bound "O(...)")            ; Complexity bound
  (deps (<skill> ...))             ; Skill-level dependencies
  (description "...")
  (keywords (<keyword> ...))       ; For search
  (aliases (<alias> ...))          ; Alternative names
  (exports (<module> <symbol> ...) ...)
  (modules (<name> "<file>" "<desc>") ...))
```

**Manifest Parser** (`lattice/meta/manifest.ss`):

The manifest parser is pure Scheme code with no side effects—it takes S-expression input and produces structured records. This purity enables:

1. Deterministic parsing regardless of environment state
2. Easy testing without mocking file systems
3. Reuse across different contexts (CLI, LSP, agents)

```scheme
(define (parse-manifest sexpr)
  ;; Returns: (skill name version tier path deps exports modules ...)
  ...)
```

**Knowledge Graph Construction**:

Manifests are transformed into CAS blocks for indexing and querying:

| Block Tag | Contents | Purpose |
|-----------|----------|---------|
| `KG-SKILL` | Skill metadata | Root node for skill |
| `KG-MODULE` | Module within skill | File-level granularity |
| `KG-EXPORT` | Exported symbol | Function/value discovery |

The knowledge graph is built by `kg-build!` which scans all manifest files, parses them, and stores the resulting blocks in the CAS. This happens lazily on first access, with subsequent queries hitting a warm cache.

**Initialization Performance**:

| Scenario | Time |
|----------|------|
| Cold start (no cache) | ~2.5s |
| Warm start (cached) | <0.3s |
| Incremental (one skill changed) | ~0.5s |

The warm cache stores serialized BM25 indices and skill metadata, eliminating the need to re-scan the filesystem.

### 8.2 Semantic Search Infrastructure

The lattice provides full-text search over skills, modules, and exports using the BM25 ranking algorithm (`lattice/meta/bm25.ss`).

**Index Architecture**:

Three separate indices enable different query patterns:

| Index | Entries | Fields Indexed |
|-------|---------|----------------|
| skill-index | ~40 | name, description, keywords, aliases |
| module-index | ~200 | name, description, skill context |
| export-index | ~2,700 | symbol name, module context, type signature |

**Search Modes**:

| Function | Algorithm | Use Case |
|----------|-----------|----------|
| `lattice-find` / `lf` | BM25 full-text | Natural language queries |
| `lattice-find-exact` / `lfe` | Exact match | Known symbol lookup |
| `lattice-find-prefix` / `lfp` | Prefix match | Tab completion |
| `lattice-find-substring` / `lfs` | Substring | Fuzzy search |

**Type-Aware Search**:

Search by input or output type to find functions by signature:

```scheme
(lf-input 'Matrix)   ; Functions that take Matrix
(lf-output 'Vector)  ; Functions that return Vector
```

**Example Session**:

```scheme
> (lf "matrix inverse")
((matrix-inverse 0.92 export (linalg matrix))
 (solve-linear 0.78 export (linalg solvers))
 (lu-decompose 0.65 export (linalg decomp)))

> (lfe 'shapley-value)
((shapley-value exact export (fp/game coop-games))
 (shapley-shubik-index exact export (fp/game voting-games)))

> (lfp 'vec)
((vec+ vec- vec* vec-dot vec-cross vec-norm vec-normalize ...))
```

**BM25 Implementation**:

The BM25 algorithm (`lattice/meta/bm25.ss`) implements:

1. **TF (Term Frequency)**: `tf = freq / (freq + k1 * (1 - b + b * doc_len / avg_len))`
2. **IDF (Inverse Document Frequency)**: `idf = log((N - n + 0.5) / (n + 0.5))`
3. **Score**: `score = sum(tf * idf)` over query terms

Default parameters: `k1 = 1.2`, `b = 0.75` (tuned for code search).

### 8.3 DAG Navigation and Analysis

The lattice forms a Directed Acyclic Graph where nodes are skills and edges are dependency relationships. The meta-tooling provides comprehensive DAG navigation.

**Dependency Queries**:

| Function | Alias | Returns |
|----------|-------|---------|
| `lattice-deps` | `ld` | Direct dependencies of a skill |
| `lattice-deps-transitive` | — | All transitive dependencies |
| `lattice-uses` | `lu` | Skills that depend on this one |
| `lattice-uses-transitive` | — | All transitive dependents |

**Path Finding**:

```scheme
(lattice-path 'physics/diff 'linalg)
; => (physics/diff autodiff linalg)
```

This uses BFS to find the shortest dependency path between two skills.

**Structural Queries**:

| Function | Returns |
|----------|---------|
| `lattice-roots` | Skills with no dependencies (tier 0) |
| `lattice-leaves` | Skills with no dependents |
| `lattice-hubs` | Most-connected skills (high in/out degree) |
| `lattice-orphans` | Skills not reachable from any entry point |

**DAG Validation**:

```scheme
(lc 'new-skill)                        ; Check for cycles
(lattice-would-cycle? 'from 'to)       ; Proactive cycle detection before adding edge
```

The cycle checker uses DFS with a visited set, flagging any back-edges that would create cycles.

**Analytics**:

```scheme
> (lattice-stats)
((skills . 42)
 (modules . 198)
 (exports . 2714)
 (edges . 87)
 (max-depth . 4)
 (avg-deps . 2.1))

> (lattice-health)
((orphaned-skills . 0)
 (cycles . 0)
 (missing-manifests . 0)
 (parse-errors . 0))

> (lattice-coverage)
((skills-with-tests . 38)
 (skills-without-tests . 4)
 (coverage-pct . 90.5))
```

### 8.4 Cross-Reference System

The cross-reference system (`boundary/introspect/xref.ss`) provides function-level dependency analysis, enabling "who calls this?" and "what does this call?" queries.

**Index Construction**:

The xref builder parses all Scheme source files, extracting:

1. **Definitions**: `define`, `define-syntax`, `define-record-type`
2. **References**: All symbol occurrences in expression position
3. **File locations**: Path, line, column for jump-to-definition

**Scale**:

| Metric | Count |
|--------|-------|
| Definitions indexed | ~11,000 |
| Call edges | ~25,000 |
| Files scanned | ~400 |
| Build time (cold) | ~3s |

**Query API**:

```scheme
(xref-callers 'matrix-mul)   ; Who calls matrix-mul?
(xref-callees 'gradient)     ; What does gradient call?
(xref-location 'vec+)        ; Where is vec+ defined?
```

**Quick Aliases**:

| Alias | Function |
|-------|----------|
| `lxu` | `xref-callers` (who **u**ses this) |
| `lxc` | `xref-callees` (what this **c**alls) |

**Integration with Source Locations**:

The `source-loc.ss` module enables jump-to-definition by maintaining a mapping from symbol → file:line:column. This powers the LSP `textDocument/definition` handler.

```scheme
> (xref-location 'traced-set)
("boundary/provenance/traced-optics.ss" 45 1)
```

### 8.5 Refactoring Toolkit

The refactoring toolkit (`boundary/tools/refactor-toolkit.ss`) provides a unified interface for codebase-wide transformations with preview-before-apply semantics.

**Unified Dispatcher**:

```scheme
(refactor 'help)                           ; Show all operations
(refactor 'rename 'old-name 'new-name)     ; Preview rename
(refactor 'apply)                          ; Apply staged changes
```

**Operations**:

| Operation | Description |
|-----------|-------------|
| `rename` | Rename symbol across codebase |
| `move` | Move symbol to different module |
| `extract` | Extract expression to named function |
| `inline` | Inline function at call sites |
| `dead-code` | Find unused definitions |
| `deps` | Show callers/callees |

**Staged Changes**:

All refactoring operations are staged before application:

```scheme
> (refactor 'rename 'vec+ 'vector-add)
Staged changes:
  lattice/linalg/vec.ss:45 - definition
  lattice/linalg/matrix.ss:23 - reference
  lattice/physics/diff/body.ss:67 - reference
  ... (12 more)

> (refactor 'status)
15 files, 23 changes pending

> (refactor 'apply)
Applied 23 changes to 15 files.
```

**Dead Code Detection**:

```scheme
> (refactor 'dead-code)
Dead code analysis:
  HIGH confidence (no callers):
    - legacy-parse (boundary/old/parser.ss:12)
    - unused-helper (lattice/fp/internal.ss:89)
  MEDIUM confidence (internal only):
    - format-debug (core/util/debug.ss:34)
```

Confidence levels:
- **HIGH**: No callers found anywhere in codebase
- **MEDIUM**: Only called from same file (possibly internal)
- **LOW**: Called but from deprecated/test code

**Quick Aliases**:

| Alias | Operation |
|-------|-----------|
| `rr` | `(refactor 'rename ...)` |
| `rm` | `(refactor 'move ...)` |
| `rd` | `(refactor 'deps ...)` |
| `rdc` | `(refactor 'dead-code)` |

### 8.6 Template DSL for Code Generation

The template DSL (`lattice/dsl/template/`) enables grammar-driven code construction, particularly useful for AI-assisted code generation where tracking parentheses is error-prone.

**Core Concept**: Templates are S-expressions with named holes (`$name`) that can be filled incrementally.

**Batch Mode** (Recommended):

```scheme
(tp-batch "
  define (qs lst) $body
  --- $body := if $cond $then $else
  --- $cond := null? lst
  --- $then := '()
  --- $else := append (qs (filter $pred (cdr lst)))
                      (cons (car lst) (qs (filter $pred2 (cdr lst))))
  --- $pred := lambda (x) (< x (car lst))
  --- $pred2 := lambda (x) (>= x (car lst))
")
```

Produces:

```scheme
(define (qs lst)
  (if (null? lst)
      '()
      (append (qs (filter (lambda (x) (< x (car lst))) (cdr lst)))
              (cons (car lst) (qs (filter (lambda (x) (>= x (car lst))) (cdr lst)))))))
```

**Key Features**:

1. **Implicit parentheses**: Multi-token lines auto-wrap
2. **Incremental filling**: Build complex expressions step by step
3. **Validation**: Type-check partial templates
4. **Composition**: Templates can reference other templates

**Grammar Rules**:

| Input | Result |
|-------|--------|
| `"+ 1 2"` | `(+ 1 2)` |
| `"x"` | `x` (single token stays unwrapped) |
| `"'(a b)"` | `'(a b)` (quoted stays as-is) |
| `"$hole"` | Placeholder for later filling |

**Session Mode**:

For interactive development:

```scheme
> (ts-start)
> (ts-template '(define (f x) $body))
> (ts-fill '$body '(+ x 1))
> (ts-show)
(define (f x) (+ x 1))
> (ts-done)
```

**Files**:
- `lattice/dsl/template/template.ss` — Core template engine
- `boundary/tools/template-session.ss` — Interactive session
- `boundary/tools/template-parser.ss` — Batch mode parser

### 8.7 Issue Tracking (BBS)

The Bulletin Board System (`boundary/bbs/`) is a CAS-native issue tracker that stores issues and posts as immutable blocks with head pointers for current state.

**Issue Schema** (tag: `bbs-issue`):

```scheme
((id . "fold-001")
 (title . "Implement fuel tracking for FFI calls")
 (status . open)             ; open, in_progress, blocked, closed
 (priority . 2)              ; 0=critical, 1=high, 2=normal, 3=low, 4=backlog
 (type . feature)            ; task, bug, feature, epic
 (assignee . #f)
 (labels . (ffi performance))
 (created . "2026-01-15T...")
 (updated . "2026-01-17T...")
 (description . "...")
 (version . 1))
```

**Core Operations**:

```scheme
(bbs-list)                              ; List open issues
(bbs-ready)                             ; Unblocked work items
(bbs-show 'fold-001)                    ; View issue details
(bbs-create "Title")                    ; Create issue
(bbs-update 'fold-001 'status 'in_progress)
(bbs-close 'fold-001)                   ; Close issue
```

**Posts** (for changelogs, announcements):

```scheme
(post-create "Title" "Body..." 'changelog)
(post-list 'type 'changelog)
(post-show 'post-1)
```

Post types: `changelog`, `note`, `announcement`, `session-summary`.

**Dependency Tracking**:

Issues can declare dependencies on other issues:

```scheme
(bbs-add-dep! 'fold-002 'fold-001)      ; fold-002 blocks on fold-001
(bbs-deps 'fold-002)                    ; => (fold-001)
(bbs-blocked-by 'fold-001)              ; => (fold-002)
```

**CAS Architecture**:

| Path | Purpose |
|------|---------|
| `.store/heads/bbs/fold-*.head` | Current hash for each issue |
| `.store/heads/bbs/post-*.head` | Current hash for each post |
| `.bbs/counter` | Next issue number |
| `.bbs/post-counter` | Next post number |
| `.bbs/deps` | Dependency graph |
| `.bbs/index.cache` | In-memory index cache |

Updates are atomic via compare-and-swap on head files (see §7.6.3).

### 8.8 Design Principles

The meta-tooling ecosystem follows several key design principles:

**Everything Queryable Through Manifests**:

All skill metadata flows through manifest files. There's no hidden configuration—if a skill has dependencies, exports, or keywords, they're declared in its manifest. This enables:

- Automated documentation generation
- Dependency analysis without loading code
- Search indexing from metadata alone

**CAS-Native Persistence**:

The knowledge graph, BBS issues, and provenance records are all stored as CAS blocks. This provides:

- Immutable history (every state is preserved)
- Content deduplication
- Merkle DAG structure for lineage

**Lazy Loading**:

Tools load their dependencies on demand:

```scheme
;; refactor-toolkit.ss
(define (refactor op . args)
  (case op
    [(rename) (load-once "refactor-rename.ss") ...]
    [(move) (load-once "refactor-move.ss") ...]
    ...))
```

This keeps startup fast while providing rich functionality.

**Pure/Impure Boundary**:

- **Pure** (lattice): Manifest parsing, BM25 scoring, dependency analysis
- **Impure** (shell): File I/O, index persistence, head file updates

The pure components are testable and reusable; the impure components handle the messy reality of file systems and concurrent access.

**Agent-Oriented Design**:

All query functions return structured data, not formatted strings:

```scheme
(lf "query")   ; Returns ((name score type context) ...)
(ld 'skill)    ; Returns (dep1 dep2 ...)
```

This enables agents to process results programmatically rather than parsing human-readable output.

---

