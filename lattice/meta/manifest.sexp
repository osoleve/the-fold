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
   ;; No exports annotated with (doc 'export #t) yet
   )

  (modules
   (kg "kg.ss" "CAS-backed knowledge graph builder from manifests")
   (bm25 "bm25.ss" "BM25 search engine for term-based ranking")
   (search "search.ss" "Unified search API integrating BM25 with KG")
   (type-search "type-search.ss" "Hoogle-style type-aware search over signatures")
   (xref "xref.ss" "Function-level cross-reference and call graph analysis")
   (dag "dag.ss" "DAG navigation and dependency analysis")
   (analytics "analytics.ss" "Statistics, health checks, and coverage")
   (inspect "inspect.ss" "Skill introspection and detailed descriptions")))
