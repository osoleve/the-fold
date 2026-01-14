;;; lattice/meta/manifest.sexp — Lattice Meta-Tooling Manifest

(skill meta
  (version "0.1.0")
  (tier 1)
  (path "lattice/meta")
  (purity partial)  ; Uses mutable state for indexing
  (stability stable)
  (fuel-bound "O(n) for most operations, O(n log n) for search")
  (deps (data))

  (description
   "Lattice navigation tooling for agents. Provides BM25 search,
    DAG navigation, analytics, and introspection for the skill lattice.
    Builds a CAS-backed knowledge graph from manifest files.")

  (keywords (meta navigation search bm25 dag analytics introspection
             knowledge-graph manifest skill-lattice indexing))
  (aliases (lattice-meta lattice-tools skills))

  (exports
   (kg kg-build! kg-skills kg-skill kg-modules kg-deps kg-uses kg-stats kg-exports)
   (bm25 bm25-create bm25-add-doc bm25-search bm25-search-string bm25-score)
   (search lattice-find lattice-find-exact lattice-complete lf lfe lfp lfs)
   (type-search lf-type lf-input lf-output type-search)
   (xref build-xref-cache! xref-callers xref-callees xref-callers-transitive
         xref-callees-transitive xref-stats xref-most-called lxu lxc)
   (dag lattice-deps lattice-deps-transitive lattice-uses lattice-uses-transitive
        lattice-path lattice-roots lattice-leaves lattice-hubs lattice-graph ld lu)
   (analytics lattice-stats lattice-health lattice-coverage ls lh)
   (inspect lattice-describe lattice-info lattice-summary li le lm))

  (modules
   (kg "kg.ss" "CAS-backed knowledge graph builder from manifests")
   (bm25 "bm25.ss" "BM25 search engine for term-based ranking")
   (search "search.ss" "Unified search API integrating BM25 with KG")
   (type-search "type-search.ss" "Hoogle-style type-aware search over signatures")
   (xref "xref.ss" "Function-level cross-reference and call graph analysis")
   (dag "dag.ss" "DAG navigation and dependency analysis")
   (analytics "analytics.ss" "Statistics, health checks, and coverage")
   (inspect "inspect.ss" "Skill introspection and detailed descriptions")))
