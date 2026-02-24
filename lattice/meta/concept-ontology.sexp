;;; lattice/meta/concept-ontology.sexp — Concept Hierarchy for Lattice Knowledge Graph
;;;
;;; Pure data. No code. Defines the concept hierarchy and cross-cutting themes.
;;; Version 2: synonyms are embedded in concept entries (no separate synonym-groups).
;;; Manifest aliases are registered dynamically at build time via register-manifest-aliases!.
;;;
;;; Schema:
;;;   (concept name (description "...") [(parent name)] [(children (...))] [(synonyms ...)])
;;;   (cross-cutting (concept name (description "...") (skills (...))))

(concept-ontology
  (version 2)

  ;;; ====
  ;;; Concept Hierarchy
  ;;; ====
  ;;; ~15 roots, 2-3 levels deep.
  ;;; Synonyms are true aliases — not child concepts.
  ;;; Child concepts have their own entries and are NOT listed as synonyms.

  (concepts

    ;; ----------------------------------------------------------------
    ;; ROOT: mathematics
    ;; ----------------------------------------------------------------
    (concept mathematics
      (description "The domain of abstract structures, formal reasoning, and quantitative relationships underlying all computational skills."))

    (concept abstract-algebra
      (description "The study of algebraic structures — groups, rings, fields, and modules — and the axioms and morphisms that govern them.")
      (parent mathematics)
      (children (group-theory ring-theory field-theory polynomial-algebra galois-theory tropical-algebra))
      (synonyms algebra))

    (concept group-theory
      (description "The study of symmetry via groups: sets with an associative binary operation, identity, and inverses.")
      (parent abstract-algebra))

    (concept ring-theory
      (description "Algebraic structures with two operations (addition, multiplication) satisfying distributivity; generalizes integers.")
      (parent abstract-algebra))

    (concept field-theory
      (description "Algebraic structures where every nonzero element has a multiplicative inverse; enables exact polynomial division.")
      (parent abstract-algebra)
      (synonyms galois-fields galois-field finite-field gf2n binary-field aes-field))

    (concept polynomial-algebra
      (description "Arithmetic of polynomials over fields or rings, including GCD, factorization, interpolation, and Groebner bases.")
      (parent abstract-algebra)
      (synonyms groebner-basis groebner buchberger polynomial-reduction polynomial-identity egraph-groebner rewrite-rules normal-form))

    (concept galois-theory
      (description "The correspondence between field extensions and symmetry groups of polynomial roots, connecting solvability to group structure.")
      (parent abstract-algebra))

    (concept tropical-algebra
      (description "Semirings over min/max and plus operations, modeling shortest paths and scheduling as algebraic structure.")
      (parent abstract-algebra))

    (concept linear-algebra
      (description "The mathematics of vectors, matrices, linear maps, and their decompositions and solvers.")
      (parent mathematics)
      (children (matrix-theory vector-spaces eigenvalue-theory sparse-methods spectral-methods))
      (synonyms la lin-alg linear matrix-math linalg))

    (concept matrix-theory
      (description "Dense matrix representations, arithmetic, decompositions (LU, QR, Cholesky), and direct solvers.")
      (parent linear-algebra))

    (concept vector-spaces
      (description "Abstract vector operations including 2D, 3D, and n-dimensional vectors with norms, dot products, and transformations.")
      (parent linear-algebra)
      (synonyms quaternion rotation slerp rodrigues))

    (concept eigenvalue-theory
      (description "Computation of eigenvalues, eigenvectors, SVD, spectral decomposition, and related matrix properties.")
      (parent linear-algebra)
      (synonyms eigenvalue eigenvalues eigenvectors svd singular-value-decomposition))

    (concept sparse-methods
      (description "Efficient representations (COO, CSR, CSC) and algorithms for matrices with mostly zero entries.")
      (parent linear-algebra))

    (concept spectral-methods
      (description "Graph Laplacians, spectral clustering, Fiedler vectors, and algebraic connectivity of graphs.")
      (parent linear-algebra)
      (synonyms spectral spectral-clustering graph-laplacian fiedler-vector algebraic-connectivity))

    (concept number-theory
      (description "Properties of integers: primality, modular arithmetic, factorization, and number-theoretic functions.")
      (parent mathematics)
      (children (modular-arithmetic primality fast-arithmetic))
      (synonyms nt))

    (concept modular-arithmetic
      (description "Arithmetic modulo an integer, including Chinese Remainder Theorem, Montgomery multiplication, and quadratic residues.")
      (parent number-theory)
      (synonyms modular mod-arith))

    (concept primality
      (description "Primality testing (Miller-Rabin), integer factorization (Pollard rho), Euler totient, and prime navigation.")
      (parent number-theory)
      (synonyms primes))

    (concept fast-arithmetic
      (description "Sub-quadratic multiplication algorithms: Karatsuba and Toom-Cook for large integer arithmetic.")
      (parent number-theory)
      (synonyms fast-mult))

    (concept differential-geometry
      (description "The mathematics of smooth manifolds, coordinate charts, tangent spaces, curvature, and geodesics.")
      (parent mathematics)
      (children (manifold-theory riemannian-geometry lie-groups exterior-calculus geodesics))
      (synonyms diffgeo diffgeom manifolds smooth-manifolds))

    (concept manifold-theory
      (description "Coordinate charts, atlases, transition functions, and the foundational apparatus of smooth manifolds.")
      (parent differential-geometry))

    (concept riemannian-geometry
      (description "Metric tensors, Christoffel symbols, Riemann curvature tensor, Ricci tensor, and surface curvatures.")
      (parent differential-geometry)
      (synonyms riemannian-metric christoffel-symbols riemann-tensor ricci-tensor scalar-curvature))

    (concept lie-groups
      (description "Smooth groups SO(2), SO(3), SE(2), SE(3) with exponential/log maps, adjoints, and BCH approximations.")
      (parent differential-geometry)
      (synonyms lie-group so2 so3 se2 se3 exponential-map))

    (concept exterior-calculus
      (description "Differential forms, wedge product, exterior derivative, Hodge star, and integration of forms over manifolds.")
      (parent differential-geometry)
      (synonyms differential-forms wedge-product exterior-derivative hodge-star cartan-calculus))

    (concept geodesics
      (description "Shortest paths on Riemannian manifolds: numerical tracing, exponential maps, parallel transport, geodesic distance.")
      (parent differential-geometry))

    (concept computational-topology
      (description "Discrete topological invariants: simplicial complexes, homology, Betti numbers, and persistent homology.")
      (parent mathematics)
      (children (simplicial-homology persistent-homology))
      (synonyms topology topo))

    (concept simplicial-homology
      (description "Chain complexes, boundary operators, Betti numbers, and Euler characteristic of simplicial complexes over Z2.")
      (parent computational-topology)
      (synonyms simplicial-complex simplicial betti-numbers euler-characteristic homology))

    (concept persistent-homology
      (description "Topological data analysis via filtrations: Vietoris-Rips complexes, persistence diagrams, and barcodes.")
      (parent computational-topology)
      (synonyms tda barcode persistence-diagram vietoris-rips persistent))

    (concept information-theory
      (description "Shannon's framework for quantifying information: entropy, mutual information, channel capacity, and coding.")
      (parent mathematics)
      (children (entropy-measures channel-theory coding-theory rate-distortion))
      (synonyms information))

    (concept entropy-measures
      (description "Shannon entropy and variants (Renyi, min, collision), KL divergence, mutual information, and related measures.")
      (parent information-theory)
      (synonyms entropy))

    (concept channel-theory
      (description "Communication channel models (BSC, BEC, AWGN), capacity computation via Blahut-Arimoto, and achievable rates.")
      (parent information-theory))

    (concept coding-theory
      (description "Source coding (Huffman, arithmetic, LZ78) and channel coding (Hamming, parity) for compression and error correction.")
      (parent information-theory)
      (synonyms coding))

    (concept rate-distortion
      (description "The tradeoff between compression rate and reconstruction fidelity: R(D) curves, quantization, and Lloyd-Max.")
      (parent information-theory))

    ;; ----------------------------------------------------------------
    ;; ROOT: computation
    ;; ----------------------------------------------------------------
    (concept computation
      (description "The domain of algorithms, data structures, languages, and the structures that make programs tractable and correct."))

    (concept data-structures
      (description "Organized representations for efficient storage and retrieval: trees, graphs, heaps, queues, and spatial indices.")
      (parent computation)
      (children (tree-structures graph-structures spatial-structures priority-structures sequential-structures))
      (synonyms ds structures collections))

    (concept tree-structures
      (description "Hierarchical data structures including AVL trees, finger trees, and zippers for ordered key-value storage.")
      (parent data-structures))

    (concept graph-structures
      (description "Adjacency matrices, graph algorithms (Dijkstra, Floyd-Warshall, BFS/DFS), centrality, and community detection.")
      (parent data-structures)
      (synonyms community-detection label-propagation modularity modularity-ilp
                pagerank eigenvector-centrality katz-centrality closeness-centrality betweenness-centrality))

    (concept spatial-structures
      (description "K-d trees, quadtrees, BVH, and octrees for efficient nearest-neighbor and range queries in 2D/3D space.")
      (parent data-structures)
      (synonyms bvh-tree bvh octree spatial-acceleration))

    (concept priority-structures
      (description "Heaps, priority queues, and leftist heaps providing O(log n) insert and min/max extraction.")
      (parent data-structures))

    (concept sequential-structures
      (description "Stacks, queues, streams, difference lists, ropes, and ring buffers for ordered sequential access.")
      (parent data-structures))

    (concept algorithms
      (description "Systematic procedures for solving computational problems with bounded complexity guarantees.")
      (parent computation)
      (children (sorting-algorithms graph-algorithms string-algorithms search-algorithms)))

    (concept sorting-algorithms
      (description "Comparison-based and non-comparison sorting: merge sort, quicksort, heapsort, and their stable variants.")
      (parent algorithms))

    (concept graph-algorithms
      (description "BFS, DFS, shortest paths, minimum spanning trees, topological sort, and connected components.")
      (parent algorithms)
      (synonyms shortest-path dijkstra floyd-warshall bfs dfs
                minimum-spanning-tree mst prim kruskal union-find))

    (concept string-algorithms
      (description "Pattern matching (Aho-Corasick) and string search algorithms operating over character sequences.")
      (parent algorithms)
      (synonyms aho-corasick multi-pattern-search string-matching))

    (concept search-algorithms
      (description "BFS, Dijkstra, A* pathfinding, and constraint-based search for combinatorial problem solving.")
      (parent algorithms))

    (concept satisfiability
      (description "Boolean constraint solving: SAT, MaxSAT, and CDCL solvers for NP-complete decision problems.")
      (parent computation)
      (children (sat-solving maxsat constraint-logic-programming))
      (synonyms sat boolean-sat))

    (concept sat-solving
      (description "CDCL SAT solving with two-watched literals, clause learning, non-chronological backtracking, and VSIDS heuristic.")
      (parent satisfiability)
      (synonyms cdcl))

    (concept maxsat
      (description "Optimization over weighted soft clauses: minimize unsatisfied constraints subject to hard clause satisfaction.")
      (parent satisfiability))

    (concept constraint-logic-programming
      (description "CLP(FD) combining miniKanren-style logic variables with finite-domain constraint propagation and arc consistency.")
      (parent satisfiability)
      (synonyms clp fd ckanren constraint-logic constraint-propagation arc-consistency contractors))

    (concept automata-theory
      (description "Formal models of computation: DFA, NFA, statecharts, and hierarchical state machines.")
      (parent computation)
      (synonyms automata state-machines fsm statecharts state-machine statechart harel hierarchical-state parallel-region))

    (concept formal-languages
      (description "Regular expressions, parser combinators, DFA-backed parsing, and the theory connecting languages to automata.")
      (parent computation)
      (synonyms parser-combinators monadic-parsing packrat-memoization regex-dfa))

    (concept equality-saturation
      (description "E-graphs that represent multiple equivalent program forms simultaneously for cost-based optimal extraction.")
      (parent computation)
      (synonyms egraph e-graph))

    ;; ----------------------------------------------------------------
    ;; ROOT: programming-paradigms
    ;; ----------------------------------------------------------------
    (concept programming-paradigms
      (description "Organizing principles and abstractions for structuring programs: functional, categorical, logic, and declarative."))

    (concept functional-programming
      (description "Programming with pure functions, immutable data, and compositional abstractions like monads and type classes.")
      (parent programming-paradigms)
      (children (type-classes monadic-programming optics-paradigm rewriting))
      (synonyms fp functional))

    (concept type-classes
      (description "Dictionary-passing polymorphism implementing Haskell-style abstractions: Functor, Monad, Traversable, Monoid.")
      (parent functional-programming))

    (concept monadic-programming
      (description "Sequencing effects via monads: State, Reader, Writer, Continuation, Free, and algebraic effect handlers.")
      (parent functional-programming))

    (concept optics-paradigm
      (description "Composable data accessors (Lens, Prism, Traversal, Iso, Grate) for focused read/write on nested structures.")
      (parent functional-programming)
      (synonyms optics lens prism profunctor-optics profunctor strong-profunctor choice-profunctor closed-profunctor wander))

    (concept rewriting
      (description "Term rewriting systems, rewrite rules, fusion optimization, and proof tactics for equational reasoning.")
      (parent functional-programming))

    (concept category-theory
      (description "Abstract mathematical framework: functors, natural transformations, adjunctions, and Kan extensions applied to programming.")
      (parent programming-paradigms))

    (concept logic-programming
      (description "Computation via unification and search: miniKanren-style relational goals, substitutions, and backtracking.")
      (parent programming-paradigms))

    (concept meta-programming
      (description "Programs that generate, analyze, or transform other programs: DSLs, staging, partial evaluation, and macros.")
      (parent programming-paradigms)
      (children (domain-specific-languages partial-evaluation multi-stage-programming)))

    (concept domain-specific-languages
      (description "Embedded languages designed for narrow problem domains, built via tagless final, free monads, or interpreters.")
      (parent meta-programming)
      (synonyms dsl meta-dsl language-tools tagless tagless-final interpreter-composition free-monad algebraic-effects))

    (concept partial-evaluation
      (description "Specializing programs with respect to known inputs: online/offline PE, Futamura projections, and BTA annotation.")
      (parent meta-programming)
      (synonyms futamura-projections binding-time-analysis))

    (concept multi-stage-programming
      (description "Programs that generate and run code in multiple stages, enabling safe code generation with type-aware splicing.")
      (parent meta-programming)
      (synonyms code-generation splice staging))

    ;; ----------------------------------------------------------------
    ;; numerical-computing (child of mathematics)
    ;; ----------------------------------------------------------------
    (concept numerical-computing
      (description "The domain of floating-point algorithms, approximation methods, and rigorous error analysis.")
      (parent mathematics)
      (children (signal-processing interpolation-approximation verified-computation numerical-integration pde-methods))
      (synonyms numerics))

    (concept signal-processing
      (description "Digital processing of time-series signals: FFT, digital filters (FIR/IIR), wavelets, convolution, and spectral analysis.")
      (parent numerical-computing)
      (synonyms signal dsp fft dft fast-fourier-transform spectral-analysis
                digital-filter fir iir butterworth chebyshev biquad
                wavelet haar daubechies dwt))

    (concept interpolation-approximation
      (description "Constructing smooth functions from discrete data: polynomial interpolation, splines, Bezier, Chebyshev, and B-splines.")
      (parent numerical-computing)
      (synonyms interp interpolation spline bezier hermite lagrange))

    (concept verified-computation
      (description "Numerics with guaranteed error bounds: interval arithmetic, affine arithmetic, and rigorous enclosure methods.")
      (parent numerical-computing)
      (synonyms interval interval-arithmetic rigorous-bounds affine affine-arithmetic correlation-tracking dependency-problem))

    (concept numerical-integration
      (description "Numerical integration of ODEs: Euler, Runge-Kutta (RK4, DP45), symplectic integrators, and adaptive step control.")
      (parent numerical-computing))

    (concept pde-methods
      (description "Numerical methods for partial differential equations: FEM on triangular meshes, Method of Lines, and Crank-Nicolson.")
      (parent numerical-computing)
      (synonyms fem pde-time finite-element-method poisson-equation elliptic-pde
                pde-time-stepping forward-euler backward-euler crank-nicolson method-of-lines rk4))

    ;; ----------------------------------------------------------------
    ;; ROOT: optimization
    ;; ----------------------------------------------------------------
    (concept optimization
      (description "Finding minima or maxima of objective functions, including gradient-based, combinatorial, and verified methods.")
      (children (gradient-based-optimization constrained-optimization combinatorial-optimization global-optimization topological-optimization))
      (synonyms optim opt minimize))

    (concept gradient-based-optimization
      (description "First-order methods (SGD, Adam, Muon) and second-order methods (Newton, L-BFGS) for smooth objectives.")
      (parent optimization)
      (synonyms gradient-descent sgd adam muon rmsprop adagrad momentum
                lbfgs quasi-newton limited-memory-bfgs))

    (concept constrained-optimization
      (description "Optimization subject to equality or inequality constraints: LP via simplex, ILP via branch-and-bound, and contractors.")
      (parent optimization)
      (synonyms linear-programming simplex-method lp dual sensitivity-analysis))

    (concept combinatorial-optimization
      (description "Discrete optimization: ILP, weighted MaxSAT, knapsack, vertex cover, and set cover problems.")
      (parent optimization)
      (synonyms integer-programming ilp branch-and-bound gomory-cuts knapsack set-cover))

    (concept global-optimization
      (description "Finding the global minimum of non-convex functions: interval branch-and-bound with guaranteed enclosure.")
      (parent optimization)
      (synonyms interval-optimization global-minimum verified-optimization monotonicity-pruning))

    (concept topological-optimization
      (description "Using persistent homology to analyze loss surfaces: basin counting, saddle detection, and landscape comparison.")
      (parent optimization))

    ;; ----------------------------------------------------------------
    ;; ROOT: geometry
    ;; ----------------------------------------------------------------
    (concept geometry
      (description "The domain of spatial structures, shapes, proximity queries, and geometric transformations.")
      (children (computational-geometry rendering-geometry mesh-processing))
      (synonyms geom 3d-geometry spatial))

    (concept computational-geometry
      (description "Algorithms for geometric problems: convex hulls, Delaunay triangulation, Voronoi diagrams, and collision detection.")
      (parent geometry)
      (synonyms convex-hull graham-scan quickhull minkowski
                delaunay-triangulation voronoi lloyd-relaxation laplacian-smoothing))

    (concept rendering-geometry
      (description "Raymarching, BVH/octree acceleration structures, signed distance fields, and mesh-SDF computation for rendering.")
      (parent geometry)
      (synonyms marching-cubes isosurface sdf signed-distance-field))

    (concept mesh-processing
      (description "Triangular mesh generation, Laplacian smoothing, adaptive refinement, and topological validation via Betti numbers.")
      (parent geometry))

    ;; ----------------------------------------------------------------
    ;; ROOT: probabilistic-reasoning
    ;; ----------------------------------------------------------------
    (concept probabilistic-reasoning
      (description "Reasoning under uncertainty using probability distributions, sampling, and Bayesian inference.")
      (children (probability-distributions bayesian-inference monte-carlo-methods variational-methods statistical-modeling)))

    (concept probability-distributions
      (description "Continuous and discrete distributions (Normal, Poisson, Beta, Dirichlet) with pure state-monad sampling.")
      (parent probabilistic-reasoning)
      (synonyms random rand prng probability sampling))

    (concept bayesian-inference
      (description "Conjugate models (Beta-Binomial, Normal-Normal, Gamma-Poisson), posterior computation, and Bayes factors.")
      (parent probabilistic-reasoning))

    (concept monte-carlo-methods
      (description "Importance sampling, rejection sampling, Metropolis-Hastings MCMC, Gibbs sampling, and variance reduction.")
      (parent probabilistic-reasoning)
      (synonyms monte-carlo mcmc metropolis-hastings gibbs-sampling importance-sampling))

    (concept variational-methods
      (description "Variational inference via ELBO optimization, reparameterization trick, and mean-field Gaussian families.")
      (parent probabilistic-reasoning)
      (synonyms variational-inference elbo reparameterization vi))

    (concept statistical-modeling
      (description "Linear models, GLM (logistic, Poisson), regularized regression (Ridge, Lasso), and time series (AR, MA, Holt-Winters).")
      (parent probabilistic-reasoning)
      (synonyms statistics stats stat regression models))

    ;; ----------------------------------------------------------------
    ;; ROOT: physics-simulation
    ;; ----------------------------------------------------------------
    (concept physics-simulation
      (description "Computational modeling of physical systems: rigid bodies, particles, dynamical systems, and differentiable simulation.")
      (children (rigid-body-physics dynamical-systems differentiable-physics)))

    (concept rigid-body-physics
      (description "2D and 3D rigid body simulation: collision detection/response, constraints, joints, and spatial hashing.")
      (parent physics-simulation)
      (synonyms rigid-body collision-detection impulse-response spatial-hash
                rigid-body-physics-2d physics-2d classical-physics physics/classical
                rigid-body-physics-3d physics-3d classical-physics-3d physics/classical3d))

    (concept dynamical-systems
      (description "Continuous-time systems: ODE solvers, chaos detection (Lyapunov exponents), attractors, and bifurcation analysis.")
      (parent physics-simulation)
      (synonyms simulation simulation-stream chaos dynamics
                chaos-theory lyapunov-exponents strange-attractor poincare-section
                bifurcation hopf saddle-node pitchfork period-doubling feigenbaum))

    (concept differentiable-physics
      (description "Physics simulation where autodiff traces through rollouts, enabling gradient-based policy and trajectory optimization.")
      (parent physics-simulation)
      (synonyms differentiable-physics-2d diff-physics diff-sim physics/diff
                differentiable-physics-3d diff-physics-3d diff-sim-3d physics/diff3d
                physics-optics physics-lens physics-lenses-3d))

    ;; ----------------------------------------------------------------
    ;; ROOT: machine-learning
    ;; ----------------------------------------------------------------
    (concept machine-learning
      (description "The domain of learning systems: optimization for training, data pipelines, benchmarking, and agent orchestration.")
      (children (automatic-differentiation agent-orchestration dataset-engineering)))

    (concept automatic-differentiation
      (description "Computing exact derivatives by propagating through computational graphs: forward mode (dual numbers), reverse mode (backprop).")
      (parent machine-learning)
      (synonyms autodiff ad diff reverse-diff backprop))

    (concept agent-orchestration
      (description "Multi-stage AI pipelines, LLM effects, council-based deliberation, and arrow-based workflow composition.")
      (parent machine-learning)
      (synonyms pipeline workflow agent-pipeline stages arrows
                council-deliberation multi-model llm-orchestration consensus))

    (concept dataset-engineering
      (description "Structured dataset construction: sample schemas, parameter sampling, distractor generation, and JSONL export.")
      (parent machine-learning)
      (synonyms dataset data sample-gen))

    ;; ----------------------------------------------------------------
    ;; ROOT: system-design
    ;; ----------------------------------------------------------------
    (concept system-design
      (description "Architecture-level concerns: content addressing, IPC protocols, security, validation, and query infrastructure.")
      (children (content-addressing cryptographic-primitives protocol-design query-infrastructure validation-contracts)))

    (concept content-addressing
      (description "Identity via cryptographic hash: blocks as (tag, payload, refs[]), CAS storage, and schema migration via optics.")
      (parent system-design)
      (synonyms cas-storage hash-identity block-machine
                bidirectional-transformation migration schema-evolution rollback))

    (concept cryptographic-primitives
      (description "Hash functions (SHA-256/384/512, BLAKE2b), HMAC, and elliptic curve arithmetic (secp256k1, P-256, ECDH).")
      (parent system-design)
      (synonyms cryptography crypto hash hashing ec ecc
                sha256 sha512 sha384 blake2b hmac message-authentication
                elliptic-curve secp256k1 p256 ecdh ecdsa))

    (concept protocol-design
      (description "Wire protocols, IPC message formats, and extensible open dispatch systems for inter-component communication.")
      (parent system-design))

    (concept query-infrastructure
      (description "Block query DSL, optic-based declarative queries, Aho-Corasick multi-pattern search, and SQL-like syntax.")
      (parent system-design)
      (synonyms optic-query declarative-query sql-like-dsl block-query block-query-dsl))

    (concept validation-contracts
      (description "Content-addressed declarative data shape specifications with structural and numeric constraints, hash-versioned.")
      (parent system-design)
      (synonyms validation contracts))

    ;; ----------------------------------------------------------------
    ;; ROOT: game-and-decision-theory
    ;; ----------------------------------------------------------------
    (concept game-and-decision-theory
      (description "The mathematical study of strategic interaction, equilibria, fair allocation, and mechanism design.")
      (children (strategic-games cooperative-games social-choice mechanism-design evolutionary-game-theory multi-criteria-decision))
      (synonyms game-theory game games))

    (concept strategic-games
      (description "Normal and extensive form games, Nash equilibrium, backward induction, and subgame perfect equilibrium.")
      (parent game-and-decision-theory)
      (synonyms nash-equilibrium best-response pure-nash mixed-nash))

    (concept cooperative-games
      (description "Coalitional games, Shapley value, core, nucleolus, Banzhaf index, and Nash bargaining solutions.")
      (parent game-and-decision-theory)
      (synonyms shapley-value cooperative-value banzhaf-index power-index))

    (concept social-choice
      (description "Voting systems (plurality, Borda, Schulze, STV, PAV), Condorcet winners, and Gibbard-Satterthwaite impossibility.")
      (parent game-and-decision-theory)
      (synonyms voting-rules plurality borda condorcet schulze))

    (concept mechanism-design
      (description "Auction theory (first-price, VCG, double), incentive compatibility, revelation principle, and Myerson optimal auctions.")
      (parent game-and-decision-theory)
      (synonyms vcg vickrey auction incentive-compatibility
                stable-matching gale-shapley two-sided-matching))

    (concept evolutionary-game-theory
      (description "Replicator dynamics, evolutionarily stable strategies (ESS), invasion analysis, and population dynamics.")
      (parent game-and-decision-theory)
      (synonyms replicator-dynamics evolutionarily-stable-strategy ess hawk-dove))

    (concept multi-criteria-decision
      (description "Decision making over multiple objectives: Pareto frontiers, weighted Borda, sensitivity analysis, and robustness.")
      (parent game-and-decision-theory))

    ;; ----------------------------------------------------------------
    ;; ROOT: knowledge-infrastructure
    ;; ----------------------------------------------------------------
    (concept knowledge-infrastructure
      (description "The tooling for navigating, searching, and understanding the lattice: search, cross-reference, and analytics.")
      (children (lattice-navigation search-tooling))
      (synonyms meta lattice-meta lattice-tools skills))

    (concept lattice-navigation
      (description "DAG traversal, dependency analysis, skill inspection, module analytics, and serendipitous cross-domain discovery.")
      (parent knowledge-infrastructure))

    (concept search-tooling
      (description "BM25 text search over skill exports, type-aware Hoogle-style search, and KG-backed concept bridging.")
      (parent knowledge-infrastructure))

    ;; ----------------------------------------------------------------
    ;; Standalone concepts (no hierarchy parent — for normalization)
    ;; ----------------------------------------------------------------

    (concept board-games
      (description "Board game SDK for hex, square, and triangle grids: pathfinding, FOV, line-of-sight, and tactical reasoning.")
      (synonyms board-game hex-grid square-grid pathfinding fov line-of-sight roguelike boardcraft game-board grid-sdk tactical))

    (concept parallel-computation
      (description "Divide-and-conquer parallelism, parallel reduction, and scan/prefix operations.")
      (synonyms parallel par strategies divide-and-conquer parallel-reduction scan-prefix))

    (concept control-theory
      (description "State-space models, transfer functions, PID controllers, LQR, Kalman filters, and H-infinity synthesis.")
      (synonyms control-systems))

  )

  ;;; ====
  ;;; Cross-Cutting Concepts
  ;;; ====
  ;;; Concepts that span 3+ skills without being any skill's primary keyword.

  (cross-cutting

    (concept composability
      (description "The ability to combine simple pieces into complex wholes without loss of correctness or reasoning transparency — the organizing principle of the lattice itself.")
      (skills (fp optics dsl pipeline algebra egraph query)))

    (concept differentiability
      (description "Smooth numerical functions whose derivatives can be computed automatically, enabling gradient-based learning and optimization through arbitrary computation graphs.")
      (skills (autodiff optimization random physics/diff physics/diff3d control-systems sim)))

    (concept categorical-structure
      (description "Organizing computation in terms of objects, morphisms, functors, and natural transformations — making algebraic structure explicit and compositionally safe.")
      (skills (fp algebra optics egraph diffgeo)))

    (concept algebraic-structure
      (description "Mathematical structures (groups, rings, fields, monoids) that impose axioms enabling verified correctness, generalization, and code reuse across domains.")
      (skills (algebra fp linalg number-theory crypto statistics)))

    (concept fuel-bounded-evaluation
      (description "All lattice functions are total and resource-bounded by a fuel parameter, enabling safe AI composition and predictable computational budgets.")
      (skills (algebra autodiff data fp linalg optics optimization)))

    (concept content-addressed-identity
      (description "Objects are identified by the hash of their canonical content, making equality, caching, and provenance trivially correct and globally consistent.")
      (skills (meta validation query optics)))

    (concept probabilistic-programming
      (description "Composing probabilistic models from pure distribution primitives using monadic sequencing, conditioning, and inference.")
      (skills (random statistics info autodiff optimization)))

    (concept topological-analysis
      (description "Using Betti numbers, simplicial homology, and persistent homology to characterize the shape and connectivity of discrete or continuous objects.")
      (skills (topology data geometry tiles optimization sim)))

    (concept graph-theoretic-reasoning
      (description "Encoding problems as graphs and applying graph algorithms for connectivity, reachability, centrality, community structure, and shortest paths.")
      (skills (data linalg topology game-theory tiles meta)))

    (concept law-verification
      (description "Checking that implementations satisfy the algebraic laws their abstractions promise: monad laws, lens laws, group axioms, and functor laws.")
      (skills (fp optics algebra egraph)))

  )
)
