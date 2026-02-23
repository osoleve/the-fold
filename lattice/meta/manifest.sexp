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
   "KG-first lattice tooling. The knowledge graph is the source of truth,
    stored as content-addressed blocks in the CAS. A single root hash
    identifies the entire lattice state. Provides BM25 search, concept-based
    bridging, DAG navigation, analytics, and introspection.")

  (keywords (meta navigation search bm25 dag analytics introspection
             knowledge-graph manifest skill-lattice indexing concepts
             content-addressed cas-first))
  (aliases (lattice-meta lattice-tools skills))

  (exports
   ;; No exports annotated with (doc 'export #t) yet
   )

  (modules
   (kg "kg.ss" "KG-first knowledge graph — entities, concepts, typed edges, CAS hydration")
   (bm25 "bm25.ss" "BM25 search engine for term-based ranking")
   (search "search.ss" "Unified search API integrating BM25 with KG")
   (type-search "type-search.ss" "Hoogle-style type-aware search over signatures")
   (xref "xref.ss" "Function-level cross-reference and call graph analysis")
   (dag "dag.ss" "DAG navigation and dependency analysis")
   (analytics "analytics.ss" "Statistics, health checks, and coverage")
   (inspect "inspect.ss" "Skill introspection and detailed descriptions")
   (persist "persist.ss" "Cache serialization with concept state")
   (manifest "manifest.ss" "Pure manifest parser")
   (docstrings "docstrings.ss" "Docstring extraction and caching")
   (serendipity "serendipity.ss" "Cross-domain discovery and related symbols")))
