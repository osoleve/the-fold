(documentation meta
(purpose "Provides search, navigation, and analysis tools for the lattice skill DAG.\n    Builds a knowledge graph from manifest files with BM25 search ranking,\n    type-aware search, and function-level cross-reference tracking.")
(quick-start "Initialize once per session, then use search and navigation commands:"
 (code "(load \"lattice/meta/meta.ss\")" "(lattice-init!)              ; Build KG + indices" "" ";; Text search" "(lf \"matrix multiply\")     ; Full-text BM25 search" "(lfe 'vec3)                  ; Exact symbol lookup" "(lfp 'matrix)                ; Prefix search" "(lfs 'c2d)                   ; Substring search" "" ";; Type-aware search" "(load \"lattice/meta/type-search.ss\")" "(lf-type \"Monad\")          ; Find by type signature" "(lf-input \"Matrix\")        ; Functions taking Matrix" "(lf-output \"Maybe\")        ; Functions returning Maybe" "" ";; Cross-references" "(load \"lattice/meta/xref.ss\")" "(build-xref-cache!)          ; Build call graph" "(lxu 'matrix-rows)           ; Who calls this?" "(lxc 'floyd-warshall)        ; What does this call?" "" ";; Skill navigation" "(li 'linalg)                 ; Describe skill" "(le 'linalg)                 ; List exports" "(ld 'physics/diff)           ; Dependencies" "(lu 'linalg)                 ; Dependents" "" ";; Testing" "(lt 'linalg)                 ; List test files" "(ltr 'linalg)                ; Run skill tests" "(lattice-tests-summary)      ; Coverage overview"))
(modules 
  ((name "meta.ss")
  (purpose "Unified entry point; loads all submodules")
  (exports (lattice-init! lattice-help))))
(command-reference
 (section "Text Search"
  (lf "query" "Full-text BM25 search across skills, modules, exports")
  (lfe
   (quote symbol) "Exact symbol lookup (falls back to substring)")
  (lfp
   (quote prefix) "Prefix search (matrix*, matrix-*)")
  (lfs
   (quote substr) "Substring search (finds c2d-zoh from c2d)"))
 (section "Type-Aware Search"
  (lf-type "query" "Search by type pattern in signatures")
  (lf-input "Type" "Find functions taking Type as input")
  (lf-output "Type" "Find functions returning Type"))
 (section "Cross-References"
  (lxu
   (quote fn) "What functions call fn?")
  (lxc
   (quote fn) "What functions does fn call?")
  (xref-callers-transitive
   (quote fn) "All transitive callers")
  (xref-most-called n "Top n most-called functions"))
 (section "Skill Navigation"
  (li
   (quote skill) "Full skill description")
  (le
   (quote skill) "List all exports")
  (lm
   (quote skill) "List modules with descriptions")
  (ld
   (quote skill) "Direct dependencies")
  (lu
   (quote skill) "Direct dependents")
  (lattice-path
   (quote from)
   (quote to) "Find dependency path")
  (lattice-roots "Dependency-free skills (no deps)")
  (lattice-tier-0 "Tier 0 foundational skills")
  (lattice-leaves "Skills with no dependents")
  (lattice-hubs "Most-depended-on skills"))
 (section "Testing"
  (lt
   (quote skill) "List test files for skill")
  (ltr
   (quote skill) "Run tests and show results")
  (lattice-tests
   (quote skill) "Get test file paths as list")
  (lattice-tests-run
   (quote skill) "Run tests, return structured result")
  (lattice-tests-summary "Test coverage overview for all skills"))
 (section "Verification"
  (lv
   (quote skill) "Verify manifest exports match code")
  (lattice-verify-exports
   (quote skill) "Compare manifest vs code exports, return alist"))
 (section "Analytics"
  (ls "Lattice statistics")
  (lh "Health check (missing deps, cycles)")
  (lattice-graph "Print full DAG structure")))
(performance-notes "- KG builds from cache in <1s if manifests unchanged" "- Search indices hold ~2700 exports" "- Docstring cache: ~8700 entries from ~440 files" "- Xref cache: ~11k definitions, ~25k call edges" "- Type search is lazy (post-filters BM25 results)")
(see-also "CLAUDE.md (Meta-Tooling section)" "lattice/meta/manifest.sexp" "docs/agent-operating-manual.md"))
