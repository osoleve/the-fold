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
((shapley-value exact export (game-theory coop-games))
 (shapley-shubik-index exact export (game-theory voting-games)))

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

### 8.5 Typed Comment Extraction

The doc extraction system (`lattice/meta/docs.ss`) provides searchable, introspectable annotations via `(doc ...)` forms that survive in source code (unlike `;;;` comments which are stripped by the reader).

**Syntax**:

```scheme
;; Contextual (belongs to enclosing definition)
(define (add x y)
  (doc 'type (-> Int Int Int))
  (doc 'description "Adds two integers")
  (+ x y))

;; Targeted (names what it documents)
(doc factorial 'type (-> Int Int))
(define (factorial n) ...)
```

**Standard Tags**: `'type`, `'description`, `'param`, `'returns`, `'todo`, `'fixme`, `'deprecated`, `'since`, `'see`, `'note`

**Query API**:

```scheme
(lf-docs 'todo)           ; Find all TODOs
(lf-docs 'type)           ; Find all type annotations
(docs-for 'factorial)     ; Find docs for specific target
(doc-stats)               ; Summary counts by tag
```

**Convenience Aliases**:

| Function | Searches |
|----------|----------|
| `lf-todo` | `'todo` tags |
| `lf-fixme` | `'fixme` tags |
| `lf-types` | `'type` tags |
| `lf-deprecated` | `'deprecated` tags |

#### 8.5.1 Type Checker Integration

Doc type annotations are **authoritative**—they take precedence over type inference. The integration works at two levels:

**Type Checker** (`core/types/infer.ss`):

The type inference system maintains a `*declared-types*` hashtable populated from doc annotations:

```scheme
;; Register a declared type
(register-declared-type! 'my-fn '(-> Int Int Int))

;; Look up declared type (returns #f if not found)
(lookup-declared-type 'my-fn)  ; => (-> Int Int Int)

;; Bulk registration from doc annotations
(register-doc-types! '((foo . (-> Int Bool))
                       (bar . (-> String Int))))
```

When inferring a variable's type, the system checks declared types as a fallback:

```scheme
;; In infer.ss, variable lookup now does:
(or (tenv-lookup env expr)           ; 1. Check local environment
    (lookup-declared-type expr))      ; 2. Check declared types
```

**LSP Integration** (`boundary/lsp/capabilities.ss`):

The LSP hover provider uses doc types as the **first** source of type information:

```
Priority order for hover types:
1. (doc symbol 'type ...) annotation  ← Author's explicit declaration
2. Local type inference (let bindings)
3. Global type inference (top-level defs)
4. Symbol index / primitive database
```

The bridge function `load-doc-types-into-checker!` lazily populates the type checker's declared types from the doc index:

```scheme
;; Data flow:
(doc f 'type '(-> Int Int)) in source
         ↓
docs.ss extracts to *doc-index*
         ↓
load-doc-types-into-checker! registers in *declared-types*
         ↓
infer.ss uses declared type when inferring 'f
```

**Indexing Performance**:

The indexer uses a cons+reverse pattern to avoid O(N²) append overhead when processing hundreds of files:

```scheme
;; O(N) instead of O(N²)
(let ([acc '()])
  (for-each (lambda (file)
              (set! acc (append (extract-docs file) acc)))
            files)
  (reverse acc))
```

An `*doc-index-built?*` flag distinguishes "not yet indexed" from "indexed but found nothing", preventing redundant rebuilds.

**Normalization Semantics**:

Doc forms are stripped during α-normalization—code with and without doc forms hashes identically. This makes them pure metadata that doesn't affect content addressing.

### 8.6 Refactoring Toolkit

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

### 8.7 Template DSL for Code Generation

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

### 8.8 Issue Tracking (BBS)

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

### 8.9 Language Server Protocol

The Fold includes a full LSP implementation (`boundary/lsp/`) that enables rich IDE integration. The server is written entirely in Scheme and leverages the existing introspection infrastructure.

**Architecture**:

| Module | Purpose |
|--------|---------|
| `lsp-server.ss` | Main message loop and lifecycle |
| `protocol.ss` | JSON-RPC types, LSP constants, response constructors |
| `transport.ss` | Stdio transport with Content-Length framing |
| `documents.ss` | Open document state management |
| `capabilities.ss` | All language feature implementations |
| `diagnostics.ss` | Parse/semantic error detection |
| `json.ss` | JSON parser/serializer for S-expressions |

**Supported Capabilities**:

| Capability | Description |
|------------|-------------|
| `textDocumentSync` | Incremental sync (change deltas, not full text) |
| `hoverProvider` | Type info, docstrings, primitive signatures |
| `completionProvider` | Keywords, primitives, snippets, lattice symbols |
| `signatureHelpProvider` | Parameter hints for known functions |
| `definitionProvider` | Jump to definition (via xref integration) |
| `referencesProvider` | Find all references across workspace |
| `documentSymbolProvider` | Document outline (nested definitions) |
| `workspaceSymbolProvider` | Search symbols across workspace |
| `documentFormattingProvider` | Pretty-print Scheme code |
| `renameProvider` | Rename symbol with scope awareness |
| `codeActionProvider` | Quick fixes (undefined → define stub) |
| `semanticTokensProvider` | Rich syntax highlighting (10 token types) |

**Semantic Token Types**:

| Index | Type | Description |
|-------|------|-------------|
| 0 | keyword | `define`, `lambda`, `if`, `let`, etc. |
| 1 | function | Function definitions and calls |
| 2 | variable | Local and global variables |
| 3 | string | String literals |
| 4 | number | Numeric literals |
| 5 | comment | Comments |
| 6 | operator | Operators like `+`, `-`, `*` |
| 7 | macro | Macro definitions (`define-syntax`) |
| 8 | parameter | Lambda/let parameters |
| 9 | type | Type annotations |

**Integration with Meta-Tooling**:

The LSP server leverages existing Fold infrastructure:

- **Type Inference**: Hover uses `core/types/infer.ss` for type information
- **Cross-References**: Definition/references use `boundary/introspect/xref.ss`
- **Lattice Search**: Workspace symbols query the BM25 indices from `lattice/meta/`
- **Pretty Printer**: Formatting uses `core/util/pretty.ss`

**Scope-Aware Rename**:

The rename provider handles Scheme's lexical scoping:

```scheme
;; Before rename 'x' → 'value'
(let ([x 10])
  (let ([x 20])  ; Different binding
    (+ x 1))
  x)             ; Original x

;; After rename (only renames the outer x)
(let ([value 10])
  (let ([x 20])
    (+ x 1))
  value)
```

The implementation tracks shadowing via `symbol-shadowed-in-form?` and filters references to the correct binding.

**Running the Server**:

```bash
scheme --script boundary/lsp/lsp-server.ss
```

The server communicates over stdio using the LSP wire protocol (JSON-RPC 2.0 with Content-Length headers).

**MCP Integration**:

The LSP is also exposed via MCP tools (`fold_lsp_*`) for use by AI agents:

```scheme
;; Available MCP tools
fold_lsp_hover      ; Get hover info at position
fold_lsp_definition ; Jump to definition
fold_lsp_references ; Find all references
fold_lsp_symbols    ; Search workspace symbols
fold_lsp_lookup     ; Combined hover + definition + references
fold_lsp_completion ; Get completions at position
fold_lsp_format     ; Format document
fold_lsp_diagnostics; Get errors/warnings
```

This enables agents to navigate and understand code using the same infrastructure as human developers.

### 8.10 Design Principles

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
- **Impure** (boundary): File I/O, index persistence, head file updates

The pure components are testable and reusable; the impure components handle the messy reality of file systems and concurrent access.

**Agent-Oriented Design**:

All query functions return structured data, not formatted strings:

```scheme
(lf "query")   ; Returns ((name score type context) ...)
(ld 'skill)    ; Returns (dep1 dep2 ...)
```

This enables agents to process results programmatically rather than parsing human-readable output.

### 8.11 Case Study: Purity Extraction of the Meta-Tooling Layer

This section documents a real development effort that demonstrates the meta-tooling ecosystem supporting a substantial refactoring task. The work was completed in January 2026, tracked as BBS issue `fold-zxvr`.

**The Problem**: The `lattice/meta/` directory—the system's own search, navigation, and introspection infrastructure—contained impure code. Several modules performed file I/O directly, violating the lattice's purity invariant. This was not merely aesthetic: impure lattice code cannot be trusted by fuel-bounded agents, cannot be safely memoized, and cannot be compiled to alternative backends.

**Discovery Phase**:

The meta-tooling was used to audit itself. Lattice introspection (`li 'meta`) revealed 17 modules with mixed purity declarations. Cross-reference queries mapped the call graph between pure transforms and I/O operations:

```scheme
> (li 'meta)
;; 17 modules, 340 exports
;; purity: partial (6 modules perform file I/O)

> (lxc 'load-export-index!)
;; Calls: file-exists?, call-with-input-file, read
;; → I/O: must move to boundary

> (lxc 'export-search)
;; Calls: bm25-search, filter, map
;; → Pure: stays in lattice
```

**Classification**:

Each module was classified as pure (data transforms, scoring, parsing) or impure (file reads, index persistence). The pattern was consistent: pure functions that transform data stay in lattice; thin I/O wrappers that read or write files move to boundary.

| Module | Classification | Rationale |
|--------|---------------|-----------|
| `bm25.ss` | Pure | BM25 scoring operates on in-memory indices |
| `manifest.ss` | Pure | S-expression parsing, no file I/O |
| `kg-build.ss` | Pure | Graph construction from parsed data |
| `exports.ss` | Mixed → split | `export-search` pure, `load-export-index!` impure |
| `docs.ss` | Mixed → split | `docs-for` pure, `build-doc-index!` impure |
| `source-loc.ss` | Mixed → split | Location records pure, file scanning impure |

**Implementation**:

Six new boundary orchestrators were created, each pairing a lattice module's I/O functions with the file system:

| Boundary Orchestrator | Extracted From | I/O Operations |
|-----------------------|----------------|----------------|
| `boundary/meta/exports-io.ss` | `lattice/meta/exports.ss` | Index load/save |
| `boundary/meta/docs-io.ss` | `lattice/meta/docs.ss` | Doc index build from files |
| `boundary/meta/xref-io.ss` | `lattice/meta/xref.ss` | Source file scanning |
| `boundary/meta/source-loc-io.ss` | `lattice/meta/source-loc.ss` | File system traversal |
| `boundary/meta/persist-io.ss` | `lattice/meta/persist.ss` | Cache persistence |
| `boundary/meta/docstrings-io.ss` | `lattice/meta/docstrings.ss` | Comment extraction from files |

Three developer tools that were entirely I/O-dependent moved wholesale to `boundary/tools/`: `audit.ss`, `manifest-sync.ss`, and `export-annotator.ss`.

**Problems Encountered**:

1. **R6RS expression context**: `(doc ...)` forms are expressions in The Fold's prelude. In R6RS, internal `(define ...)` forms are only valid at the start of a body—placing them after `(doc ...)` expressions triggered syntax errors. Fix: replace internal defines with named `let` bindings after doc forms.

2. **Load path cascades**: Moving modules required updating 22 load paths across the codebase. The cross-reference system (`xref-callers`) identified all affected files before any code was moved, preventing broken references.

3. **Bootstrap circularity**: `meta.ss` itself must load boundary orchestrators to function as a user-facing entry point. It remains the only `'purity 'partial` file in `lattice/meta/`—an intentional architectural choice. The pure modules it coordinates are individually total.

**Verification**:

Three verification mechanisms operated throughout the refactoring:

- **Test suite**: 340/340 tests passed after every commit. No test modifications were required—the API surface was unchanged.
- **Pre-commit hook**: The layer boundary checker flagged any staged `.ss` file that imported across layers incorrectly, catching two accidental `(load "boundary/...")` calls in lattice code before they reached the repository.
- **Lattice health check**: `(lattice-health)` confirmed zero orphaned skills, zero broken dependency edges, and zero manifest parse errors after the restructuring.

**Results**:

| Metric | Before | After |
|--------|--------|-------|
| Lattice/meta files with `'purity 'total` | 11/17 | 17/17 |
| I/O operations in lattice/meta/ | 23 | 0 |
| Boundary orchestrator files | 0 | 6 |
| Net lines removed from lattice | — | 1,093 |
| Purity exceptions in entire lattice | 7 | 1 |

The remaining exception (`lattice/data/graph/graph-algorithms.ss`) requires splitting store-dependent graph traversals from pure data structures—a more delicate decomposition that remains as tracked work.

**Lessons**:

The refactoring validated two architectural claims. First, the pure/impure boundary is a tractable decomposition even for self-referential infrastructure—the meta-tooling could be purified without changing its API or breaking its consumers. Second, the meta-tooling ecosystem (cross-references, search, pre-commit hooks) provided sufficient visibility to execute a 17-file refactoring with zero regressions. The tools built for developers worked equally well when turned on themselves.

---

