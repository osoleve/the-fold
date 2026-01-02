;;; Auto-Doc Extractor Implementation Plan
;;; Issue: fold-c1c
;;;
;;; Goal: Generate REPL-friendly docs from code: signatures, examples,
;;;       and derived summaries. Store as data for inspection (no new .md files).

;;; ============================================================
;;; Overview
;;; ============================================================
;;;
;;; Build on existing infrastructure:
;;;   - index.ss    — Symbol index with basic signature/docstring extraction
;;;   - help.ss     — Model for structured doc format (primitives)
;;;   - module.ss   — Module headers (@module/@requires)
;;;   - commands.ss — Command registration pattern
;;;
;;; Create: core/util/autodoc.ss
;;; Placement: core/util/ (alongside help.ss)
;;; Authority: Shell-like (performs I/O to scan files)

;;; ============================================================
;;; Data Structures
;;; ============================================================

;;; Doc entry structure (extends index.ss symbol entry):
;;;   ((name      . Symbol)
;;;    (kind      . define|define-syntax|macro)
;;;    (file      . String)      ; source file path
;;;    (line      . Number)      ; line number
;;;    (signature . String)      ; type signature (from ;;; name : sig)
;;;    (docstring . String)      ; description text
;;;    (category  . Symbol)      ; derived or annotated category
;;;    (examples  . (List Example))
;;;    (see-also  . (List Symbol))
;;;    (module    . Symbol))     ; module name from @module annotation

;;; Example structure:
;;;   ((source   . String)      ; code as string, e.g., "(fold-left + 0 '(1 2 3))"
;;;    (expected . String)      ; expected result, e.g., "6"
;;;    (label    . String)      ; description, e.g., "sum of list"
;;;    (from     . String))     ; source file (test file path)

;;; ============================================================
;;; Extraction Pipeline (5 stages)
;;; ============================================================

;;; Stage 1: Module Metadata
;;;   - Parse @module/@requires/@exports from file headers
;;;   - Already exists in module.ss, reuse

;;; Stage 2: Signature Extraction
;;;   - Parse ;;; name : signature comments (index.ss does this)
;;;   - Parse inline type annotations if present
;;;   - Enhance: multi-line signatures, signature continuation

;;; Stage 3: Docstring Extraction
;;;   - Parse ;;; comment blocks above definitions
;;;   - Support structured sections:
;;;     ;;; Summary one-liner
;;;     ;;;
;;;     ;;; Detailed description...
;;;     ;;;
;;;     ;;; See-also: related-fn, other-fn

;;; Stage 4: Example Extraction from Tests
;;;   - Scan test-*.ss files
;;;   - Extract from test framework calls:
;;;     (test "label" expected (expr))        → Example
;;;     (assert-equal expected actual)        → Example (no label)
;;;     (define-test name body)               → Example group
;;;   - Associate examples with definitions by name matching

;;; Stage 5: Derived Summaries
;;;   - Generate category from module path (core/linalg → 'linalg)
;;;   - Generate short description from function name if none provided
;;;   - Infer see-also from load dependencies and similar names

;;; ============================================================
;;; REPL Integration
;;; ============================================================

;;; New commands (registered in shell):
;;;
;;; (doc 'name)
;;;   Show full documentation for a symbol:
;;;     - Signature, description, category
;;;     - Examples (from tests or manual)
;;;     - See-also links
;;;     - Source location
;;;
;;; (doc-search "pattern")
;;;   Search docs by name/description pattern
;;;   Returns list of matching symbols
;;;
;;; (doc-category 'cat)
;;;   List all symbols in a category
;;;   Categories: linalg, types, blocks, etc.
;;;
;;; (doc-examples 'name)
;;;   Show only the examples for a symbol
;;;
;;; (doc-stats)
;;;   Show documentation coverage stats:
;;;     - Symbols with signatures: N/M
;;;     - Symbols with docstrings: N/M
;;;     - Symbols with examples: N/M

;;; ============================================================
;;; Storage
;;; ============================================================

;;; Store extracted docs as S-expression data:
;;;   .fold-repl/autodocs.sexp   — Cached doc database
;;;
;;; Cache invalidation:
;;;   - Track file mtimes
;;;   - Rebuild incrementally when files change
;;;   - Full rebuild: (doc-refresh!)

;;; ============================================================
;;; Implementation Tasks
;;; ============================================================

;;; 1. Create core/util/autodoc.ss skeleton
;;;    - Define data structures
;;;    - Define *autodoc-registry* hashtable
;;;    - Stub functions

;;; 2. Implement signature extraction
;;;    - Enhance index.ss pattern for multi-line
;;;    - Handle continuation comments

;;; 3. Implement docstring parsing
;;;    - Parse structured docstrings
;;;    - Extract see-also references

;;; 4. Implement example extraction
;;;    - Parse test files for (test ...) calls
;;;    - Match examples to function definitions
;;;    - Handle (define-test ...) blocks

;;; 5. Implement REPL commands
;;;    - (doc 'name) — main entry point
;;;    - (doc-search), (doc-category), etc.
;;;    - Register commands in shell/commands.ss

;;; 6. Implement caching
;;;    - Save to .fold-repl/autodocs.sexp
;;;    - Load on daemon start
;;;    - Incremental updates

;;; 7. Testing
;;;    - Unit tests for each extraction stage
;;;    - Integration test with real modules

;;; ============================================================
;;; Questions for Review
;;; ============================================================

;;; Q1: Should examples be stored per-module or per-symbol?
;;;     - Per-symbol enables (doc-examples 'fold-left)
;;;     - Per-module groups related examples
;;;     → Recommend: Per-symbol with module cross-reference

;;; Q2: How to handle private/internal functions?
;;;     - Prefix with underscore (_helper)?
;;;     - Omit from public docs?
;;;     → Recommend: Include all, mark visibility

;;; Q3: Should we extract types from the type system?
;;;     - infer.ss can give inferred types
;;;     - Would require loading entire type system
;;;     → Recommend: Start with declared signatures only

;;; Q4: Should autodoc be lazy or eager?
;;;     - Lazy: extract on first (doc 'name) call
;;;     - Eager: extract all on (doc-refresh!)
;;;     → Recommend: Eager with caching

;;; ============================================================
;;; Gemini 3 Pro Review Feedback (2026-01-02)
;;; ============================================================

;;; ANSWERS TO DESIGN QUESTIONS:
;;;   Q1: Per-symbol (enables direct lookup for doc-examples)
;;;   Q2: Include all, mark visibility (add 'visibility field)
;;;   Q3: Declared signatures only (full inference too heavy)
;;;   Q4: Eager with caching (search needs global index)

;;; KEY IMPROVEMENTS TO INCORPORATE:
;;;
;;; 1. Use (read) for AST-based test parsing
;;;    - Parse test files as s-expressions, not regex
;;;    - Handles multi-line, nesting, comments correctly
;;;
;;; 2. Unify help and doc commands
;;;    - Overload (help) to dispatch: primitives → help.ss, else → autodoc
;;;    - Keep (doc) as detailed/structured view
;;;
;;; 3. Track re-exports and aliases
;;;    - Add (original-module . sym) and (exporting-modules . list)
;;;
;;; 4. Macros need special handling
;;;    - Add (usage . string) field for syntax examples
;;;
;;; 5. Keep orphaned examples
;;;    - Examples with no matching definition → diagnostic bucket
;;;
;;; 6. Don't block REPL startup
;;;    - Run doc-refresh! in background or on-demand only

;;; End of plan
