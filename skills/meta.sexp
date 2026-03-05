;;; skills/meta.sexp — Skill curation file

(skill meta
  (description
    "KG-first lattice tooling. The knowledge graph is the source of truth,\n    stored as content-addressed blocks in the CAS. A single root hash\n    identifies the entire lattice state. Provides BM25 search, concept-based\n    bridging, DAG navigation, analytics, and introspection.")

  (keywords (meta navigation search bm25 dag analytics introspection knowledge-graph manifest skill-lattice indexing concepts content-addressed cas-first))

  (aliases (lattice-meta lattice-tools skills))

  (concepts (mathematics computation programming-paradigms optimization geometry probabilistic-reasoning physics-simulation machine-learning system-design game-and-decision-theory knowledge-infrastructure algorithms content-addressing protocol-design lattice-navigation search-tooling composability differentiability categorical-structure algebraic-structure fuel-bounded-evaluation content-addressed-identity probabilistic-programming topological-analysis graph-theoretic-reasoning law-verification))

  (modules
    kg
    bm25
    search
    type-search
    xref
    dag
    analytics
    inspect
    persist
    manifest
    docstrings
    serendipity
    unused-requires
    skill-map
    derive-deps
    fuel-normalize
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
