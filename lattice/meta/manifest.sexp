;;; lattice/meta/manifest.sexp — Lattice Meta-Tooling Manifest

(skill meta
  (version "0.1.0")
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

  (concepts
    ;; Root concepts — cross-domain, no single skill owns them
    (concept mathematics
      (description "The domain of abstract structures, formal reasoning, and quantitative relationships underlying all computational skills."))
    (concept computation
      (description "The domain of algorithms, data structures, languages, and the structures that make programs tractable and correct."))
    (concept programming-paradigms
      (description "Organizing principles and abstractions for structuring programs: functional, categorical, logic, and declarative."))
    (concept optimization
      (description "Finding minima or maxima of objective functions, including gradient-based, combinatorial, and verified methods.")
      (synonyms optim opt minimize))
    (concept geometry
      (description "The domain of spatial structures, shapes, proximity queries, and geometric transformations.")
      (synonyms geom 3d-geometry spatial))
    (concept probabilistic-reasoning
      (description "Reasoning under uncertainty using probability distributions, sampling, and Bayesian inference."))
    (concept physics-simulation
      (description "Computational modeling of physical systems: rigid bodies, particles, dynamical systems, and differentiable simulation."))
    (concept machine-learning
      (description "The domain of learning systems: optimization for training, data pipelines, benchmarking, and agent orchestration."))
    (concept system-design
      (description "Architecture-level concerns: content addressing, IPC protocols, security, validation, and query infrastructure."))
    (concept game-and-decision-theory
      (description "The mathematical study of strategic interaction, equilibria, fair allocation, and mechanism design.")
      (synonyms game-theory game games))
    (concept knowledge-infrastructure
      (description "The tooling for navigating, searching, and understanding the lattice: search, cross-reference, and analytics.")
      (synonyms meta lattice-meta lattice-tools skills))

    ;; Meta-owned leaf concepts
    (concept algorithms
      (description "Systematic procedures for solving computational problems with bounded complexity guarantees.")
      (parent computation))
    (concept content-addressing
      (description "Identity via cryptographic hash: blocks as (tag, payload, refs[]), CAS storage, and schema migration via optics.")
      (parent system-design)
      (synonyms cas-storage hash-identity block-machine bidirectional-transformation migration schema-evolution rollback))
    (concept protocol-design
      (description "Wire protocols, IPC message formats, and extensible open dispatch systems for inter-component communication.")
      (parent system-design))
    (concept lattice-navigation
      (description "DAG traversal, dependency analysis, skill inspection, module analytics, and serendipitous cross-domain discovery.")
      (parent knowledge-infrastructure))
    (concept search-tooling
      (description "BM25 text search over skill exports, type-aware Hoogle-style search, and KG-backed concept bridging.")
      (parent knowledge-infrastructure))

    ;; Cross-cutting concepts — span 3+ skills
    (concept composability
      (description "The ability to combine simple pieces into complex wholes without loss of correctness or reasoning transparency — the organizing principle of the lattice itself.")
      (cross-cutting #t)
      (skills (fp optics dsl pipeline algebra egraph query)))
    (concept differentiability
      (description "Smooth numerical functions whose derivatives can be computed automatically, enabling gradient-based learning and optimization through arbitrary computation graphs.")
      (cross-cutting #t)
      (skills (autodiff optimization random physics/diff physics/diff3d control-systems sim)))
    (concept categorical-structure
      (description "Organizing computation in terms of objects, morphisms, functors, and natural transformations — making algebraic structure explicit and compositionally safe.")
      (cross-cutting #t)
      (skills (fp algebra optics egraph diffgeo)))
    (concept algebraic-structure
      (description "Mathematical structures (groups, rings, fields, monoids) that impose axioms enabling verified correctness, generalization, and code reuse across domains.")
      (cross-cutting #t)
      (skills (algebra fp linalg number-theory crypto statistics)))
    (concept fuel-bounded-evaluation
      (description "All lattice functions are total and resource-bounded by a fuel parameter — the containment mechanism for Fold-native models. Skills with non-trivial fuel characteristics include recursive decomposition, graph traversal, and iterative solvers.")
      (cross-cutting #t)
      (skills (algebra autodiff data fp linalg optics optimization numeric geometry game-theory statistics)))
    (concept content-addressed-identity
      (description "Objects are identified by the hash of their canonical content, making equality, caching, and provenance trivially correct and globally consistent.")
      (cross-cutting #t)
      (skills (meta validation query optics)))
    (concept probabilistic-programming
      (description "Composing probabilistic models from pure distribution primitives using monadic sequencing, conditioning, and inference.")
      (cross-cutting #t)
      (skills (random statistics info autodiff optimization)))
    (concept topological-analysis
      (description "Using Betti numbers, simplicial homology, and persistent homology to characterize the shape and connectivity of discrete or continuous objects.")
      (cross-cutting #t)
      (skills (topology data geometry tiles optimization)))
    (concept graph-theoretic-reasoning
      (description "Encoding problems as graphs and applying graph algorithms for connectivity, reachability, centrality, community structure, and shortest paths.")
      (cross-cutting #t)
      (skills (data linalg topology game-theory tiles meta)))
    (concept law-verification
      (description "Checking that implementations satisfy the algebraic laws their abstractions promise: monad laws, lens laws, group axioms, and functor laws.")
      (cross-cutting #t)
      (skills (fp optics algebra egraph))))

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
