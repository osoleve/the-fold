(documentation meta
(purpose "KG-first lattice tooling. The knowledge graph is the source of truth,\n    stored as content-addressed blocks in the CAS. A single root hash identifies\n    the entire lattice state. Provides BM25 search, concept-based semantic bridging,\n    DAG navigation, analytics, and introspection.")
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
 (section "Concepts"
  (lk
   (quote concept) "Skills providing a concept")
  (lkk
   (quote skill) "Concepts of a skill")
  (lkb
   (quote skill-a)
   (quote skill-b) "Concept bridge between two skills")
  (lkr
   (quote skill) "Skills related by shared concepts"))
 (section "Analytics"
  (ls "Lattice statistics")
  (lh "Health check (missing deps, cycles)")
  (lattice-graph "Print full DAG structure")))
(performance-notes "- CAS-first init: <0.2s (root hash → block tree → full state)" "- Sexp cache fallback: <0.3s" "- Cold start (manifest build): ~2.5s" "- KG: 39 skills, 357 modules, 5723 exports, 657 concepts, 830 edges" "- Docstring cache: ~8300 entries from ~840 files" "- Xref cache: ~11k definitions, ~25k call edges" "- Type search is lazy (post-filters BM25 results)")
(see-also "CLAUDE.md (Meta-Tooling section)" "lattice/meta/manifest.sexp" "docs/agent-operating-manual.md"))
