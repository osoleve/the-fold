;;; lattice/meta/README.sexp — Lattice Meta-Tooling Documentation
;;;
;;; Agent-facing navigation and introspection for the skill lattice.

(documentation meta
  (purpose
   "Provides search, navigation, and analysis tools for the lattice skill DAG.
    Builds a knowledge graph from manifest files with BM25 search ranking,
    type-aware search, and function-level cross-reference tracking.")

  (quick-start
   "Initialize once per session, then use search and navigation commands:"
   (code
    "(load \"lattice/meta/meta.ss\")"
    "(lattice-init!)              ; Build KG + indices"
    ""
    ";; Text search"
    "(lf \"matrix multiply\")     ; Full-text BM25 search"
    "(lfe 'vec3)                  ; Exact symbol lookup"
    "(lfp 'matrix)                ; Prefix search"
    "(lfs 'c2d)                   ; Substring search"
    ""
    ";; Type-aware search"
    "(load \"lattice/meta/type-search.ss\")"
    "(lf-type \"Monad\")          ; Find by type signature"
    "(lf-input \"Matrix\")        ; Functions taking Matrix"
    "(lf-output \"Maybe\")        ; Functions returning Maybe"
    ""
    ";; Cross-references"
    "(load \"lattice/meta/xref.ss\")"
    "(build-xref-cache!)          ; Build call graph"
    "(lxu 'matrix-rows)           ; Who calls this?"
    "(lxc 'floyd-warshall)        ; What does this call?"
    ""
    ";; Skill navigation"
    "(li 'linalg)                 ; Describe skill"
    "(le 'linalg)                 ; List exports"
    "(ld 'physics/diff)           ; Dependencies"
    "(lu 'linalg)                 ; Dependents"
    ""
    ";; Testing"
    "(lt 'linalg)                 ; List test files"
    "(ltr 'linalg)                ; Run skill tests"
    "(lattice-tests-summary)      ; Coverage overview"))

  (modules
   ((name "meta.ss")
    (purpose "Unified entry point; loads all submodules")
    (exports (lattice-init! lattice-help)))

   ((name "kg.ss")
    (purpose "CAS-backed knowledge graph from manifests")
    (exports (kg-build! kg-skills kg-skill kg-modules kg-deps kg-uses kg-stats kg-exports)))

   ((name "bm25.ss")
    (purpose "BM25 search engine with TF-IDF ranking")
    (exports (bm25-create bm25-add-doc bm25-search bm25-search-string bm25-score)))

   ((name "search.ss")
    (purpose "Unified search API integrating BM25 with KG")
    (exports (lattice-find lattice-find-exact lattice-complete lf lfe lfp lfs)))

   ((name "type-search.ss")
    (purpose "Hoogle-style type-aware search over docstring signatures")
    (exports (lf-type lf-input lf-output type-search))
    (notes "Uses existing BM25 with post-filtering on type signatures"))

   ((name "xref.ss")
    (purpose "Function-level cross-reference and call graph analysis")
    (exports (build-xref-cache! xref-callers xref-callees
              xref-callers-transitive xref-callees-transitive
              xref-stats xref-most-called lxu lxc))
    (notes "Parses source files to build call graph; ~11k defs, ~25k edges"))

   ((name "dag.ss")
    (purpose "DAG traversal, dependency paths, tier analysis")
    (exports (lattice-deps lattice-deps-transitive lattice-uses lattice-uses-transitive
              lattice-path lattice-roots lattice-tier-0 lattice-leaves lattice-hubs lattice-graph ld lu)))

   ((name "analytics.ss")
    (purpose "Statistics, health checks, and coverage analysis")
    (exports (lattice-stats lattice-health lattice-coverage ls lh)))

   ((name "inspect.ss")
    (purpose "Skill introspection, descriptions, test discovery, and export verification")
    (exports (lattice-describe lattice-info lattice-summary li le lm
              lattice-tests lattice-tests-run lattice-tests-summary lt ltr
              lattice-verify-exports lv)))

   ((name "docstrings.ss")
    (purpose "Extract docstrings from source files for search indexing")
    (exports (build-docstring-cache! get-docstring docstring-terms)))

   ((name "source-loc.ss")
    (purpose "Track source locations for jump-to-definition")
    (exports (build-source-location-cache! get-source-location format-source-location)))

   ((name "persist.ss")
    (purpose "Cache KG to disk for fast initialization")
    (exports (lattice-cache-valid? lattice-save-cache! lattice-load-cache!)))

   ((name "audit.ss")
    (purpose "Find gaps between source code and manifests")
    (exports (audit-skill audit-skill-pretty suggest-missing))))

  (command-reference
   (section "Text Search"
    (lf "query" "Full-text BM25 search across skills, modules, exports")
    (lfe 'symbol "Exact symbol lookup (falls back to substring)")
    (lfp 'prefix "Prefix search (matrix*, matrix-*)")
    (lfs 'substr "Substring search (finds c2d-zoh from c2d)"))

   (section "Type-Aware Search"
    (lf-type "query" "Search by type pattern in signatures")
    (lf-input "Type" "Find functions taking Type as input")
    (lf-output "Type" "Find functions returning Type"))

   (section "Cross-References"
    (lxu 'fn "What functions call fn?")
    (lxc 'fn "What functions does fn call?")
    (xref-callers-transitive 'fn "All transitive callers")
    (xref-most-called n "Top n most-called functions"))

   (section "Skill Navigation"
    (li 'skill "Full skill description")
    (le 'skill "List all exports")
    (lm 'skill "List modules with descriptions")
    (ld 'skill "Direct dependencies")
    (lu 'skill "Direct dependents")
    (lattice-path 'from 'to "Find dependency path")
    (lattice-roots "Dependency-free skills (no deps)")
    (lattice-tier-0 "Tier 0 foundational skills")
    (lattice-leaves "Skills with no dependents")
    (lattice-hubs "Most-depended-on skills"))

   (section "Testing"
    (lt 'skill "List test files for skill")
    (ltr 'skill "Run tests and show results")
    (lattice-tests 'skill "Get test file paths as list")
    (lattice-tests-run 'skill "Run tests, return structured result")
    (lattice-tests-summary "Test coverage overview for all skills"))

   (section "Verification"
    (lv 'skill "Verify manifest exports match code")
    (lattice-verify-exports 'skill "Compare manifest vs code exports, return alist"))

   (section "Analytics"
    (ls "Lattice statistics")
    (lh "Health check (missing deps, cycles)")
    (lattice-graph "Print full DAG structure")))

  (performance-notes
   "- KG builds from cache in <1s if manifests unchanged"
   "- Search indices hold ~2700 exports"
   "- Docstring cache: ~8700 entries from ~440 files"
   "- Xref cache: ~11k definitions, ~25k call edges"
   "- Type search is lazy (post-filters BM25 results)")

  (see-also
   "CLAUDE.md (Meta-Tooling section)"
   "lattice/meta/manifest.sexp"
   "docs/agent-operating-manual.md"))
