;;; lattice/meta/concept-ontology.sexp — Concept Hierarchy for Lattice Knowledge Graph
;;;
;;; Pure data. No code. Defines the concept hierarchy, synonym groups, and
;;; cross-cutting themes for the lattice skill system.
;;;
;;; Schema:
;;;   (concept name (description "...") [(parent name)] [(children (...))])
;;;   (synonym-groups (canonical alias1 alias2 ...))
;;;   (cross-cutting (concept name (description "...") (skills (...))))

(concept-ontology
  (version 1)

  ;;; ====
  ;;; Concept Hierarchy
  ;;; ====
  ;;; ~10-15 roots, 2-3 levels deep.
  ;;; Every manifest keyword maps to something here or via synonym normalization.

  (concepts

    ;; ----------------------------------------------------------------
    ;; ROOT: mathematics
    ;; Abstract mathematical structures and operations.
    ;; ----------------------------------------------------------------
    (concept mathematics
      (description "The domain of abstract structures, formal reasoning, and quantitative relationships underlying all computational skills."))

    (concept abstract-algebra
      (description "The study of algebraic structures — groups, rings, fields, and modules — and the axioms and morphisms that govern them.")
      (parent mathematics)
      (children (group-theory ring-theory field-theory polynomial-algebra galois-theory tropical-algebra)))

    (concept group-theory
      (description "The study of symmetry via groups: sets with an associative binary operation, identity, and inverses.")
      (parent abstract-algebra))

    (concept ring-theory
      (description "Algebraic structures with two operations (addition, multiplication) satisfying distributivity; generalizes integers.")
      (parent abstract-algebra))

    (concept field-theory
      (description "Algebraic structures where every nonzero element has a multiplicative inverse; enables exact polynomial division.")
      (parent abstract-algebra))

    (concept polynomial-algebra
      (description "Arithmetic of polynomials over fields or rings, including GCD, factorization, interpolation, and Gröbner bases.")
      (parent abstract-algebra))

    (concept galois-theory
      (description "Finite fields (GF(p), GF(p^n), GF(2^n)) and their extensions, essential for cryptography and coding theory.")
      (parent abstract-algebra))

    (concept tropical-algebra
      (description "Semirings over min/max and plus operations, modeling shortest paths and scheduling as algebraic structure.")
      (parent abstract-algebra))

    (concept linear-algebra
      (description "The mathematics of vectors, matrices, linear maps, and their decompositions and solvers.")
      (parent mathematics)
      (children (matrix-theory vector-spaces eigenvalue-theory sparse-methods spectral-methods)))

    (concept matrix-theory
      (description "Dense matrix representations, arithmetic, decompositions (LU, QR, Cholesky), and direct solvers.")
      (parent linear-algebra))

    (concept vector-spaces
      (description "Abstract vector operations including 2D, 3D, and n-dimensional vectors with norms, dot products, and transformations.")
      (parent linear-algebra))

    (concept eigenvalue-theory
      (description "Computation of eigenvalues, eigenvectors, SVD, spectral decomposition, and related matrix properties.")
      (parent linear-algebra))

    (concept sparse-methods
      (description "Efficient representations (COO, CSR, CSC) and algorithms for matrices with mostly zero entries.")
      (parent linear-algebra))

    (concept spectral-methods
      (description "Graph Laplacians, spectral clustering, Fiedler vectors, and algebraic connectivity of graphs.")
      (parent linear-algebra))

    (concept number-theory
      (description "Properties of integers: primality, modular arithmetic, factorization, and number-theoretic functions.")
      (parent mathematics)
      (children (modular-arithmetic primality fast-arithmetic)))

    (concept modular-arithmetic
      (description "Arithmetic modulo an integer, including Chinese Remainder Theorem, Montgomery multiplication, and quadratic residues.")
      (parent number-theory))

    (concept primality
      (description "Primality testing (Miller-Rabin), integer factorization (Pollard rho), Euler totient, and prime navigation.")
      (parent number-theory))

    (concept fast-arithmetic
      (description "Sub-quadratic multiplication algorithms: Karatsuba and Toom-Cook for large integer arithmetic.")
      (parent number-theory))

    (concept differential-geometry
      (description "The mathematics of smooth manifolds, coordinate charts, tangent spaces, curvature, and geodesics.")
      (parent mathematics)
      (children (manifold-theory riemannian-geometry lie-groups exterior-calculus geodesics)))

    (concept manifold-theory
      (description "Coordinate charts, atlases, transition functions, and the foundational apparatus of smooth manifolds.")
      (parent differential-geometry))

    (concept riemannian-geometry
      (description "Metric tensors, Christoffel symbols, Riemann curvature tensor, Ricci tensor, and surface curvatures.")
      (parent differential-geometry))

    (concept lie-groups
      (description "Smooth groups SO(2), SO(3), SE(2), SE(3) with exponential/log maps, adjoints, and BCH approximations.")
      (parent differential-geometry))

    (concept exterior-calculus
      (description "Differential forms, wedge product, exterior derivative, Hodge star, and integration of forms over manifolds.")
      (parent differential-geometry))

    (concept geodesics
      (description "Shortest paths on Riemannian manifolds: numerical tracing, exponential maps, parallel transport, geodesic distance.")
      (parent differential-geometry))

    (concept computational-topology
      (description "Discrete topological invariants: simplicial complexes, homology, Betti numbers, and persistent homology.")
      (parent mathematics)
      (children (simplicial-homology persistent-homology)))

    (concept simplicial-homology
      (description "Chain complexes, boundary operators, Betti numbers, and Euler characteristic of simplicial complexes over Z2.")
      (parent computational-topology))

    (concept persistent-homology
      (description "Topological data analysis via filtrations: Vietoris-Rips complexes, persistence diagrams, and barcodes.")
      (parent computational-topology))

    (concept information-theory
      (description "Shannon's framework for quantifying information: entropy, mutual information, channel capacity, and coding.")
      (parent mathematics)
      (children (entropy-measures channel-theory coding-theory rate-distortion)))

    (concept entropy-measures
      (description "Shannon entropy and variants (Renyi, min, collision), KL divergence, mutual information, and related measures.")
      (parent information-theory))

    (concept channel-theory
      (description "Communication channel models (BSC, BEC, AWGN), capacity computation via Blahut-Arimoto, and achievable rates.")
      (parent information-theory))

    (concept coding-theory
      (description "Source coding (Huffman, arithmetic, LZ78) and channel coding (Hamming, parity) for compression and error correction.")
      (parent information-theory))

    (concept rate-distortion
      (description "The tradeoff between compression rate and reconstruction fidelity: R(D) curves, quantization, and Lloyd-Max.")
      (parent information-theory))

    ;; ----------------------------------------------------------------
    ;; ROOT: computation
    ;; Algorithms, data structures, languages, and system design.
    ;; ----------------------------------------------------------------
    (concept computation
      (description "The domain of algorithms, data structures, languages, and the structures that make programs tractable and correct."))

    (concept data-structures
      (description "Organized representations for efficient storage and retrieval: trees, graphs, heaps, queues, and spatial indices.")
      (parent computation)
      (children (tree-structures graph-structures spatial-structures priority-structures sequential-structures)))

    (concept tree-structures
      (description "Hierarchical data structures including AVL trees, finger trees, and zippers for ordered key-value storage.")
      (parent data-structures))

    (concept graph-structures
      (description "Adjacency matrices, graph algorithms (Dijkstra, Floyd-Warshall, BFS/DFS), centrality, and community detection.")
      (parent data-structures))

    (concept spatial-structures
      (description "K-d trees, quadtrees, BVH, and octrees for efficient nearest-neighbor and range queries in 2D/3D space.")
      (parent data-structures))

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
      (parent algorithms))

    (concept string-algorithms
      (description "Pattern matching (Aho-Corasick) and string search algorithms operating over character sequences.")
      (parent algorithms))

    (concept search-algorithms
      (description "BFS, Dijkstra, A* pathfinding, and constraint-based search for combinatorial problem solving.")
      (parent algorithms))

    (concept satisfiability
      (description "Boolean constraint solving: SAT, MaxSAT, and CDCL solvers for NP-complete decision problems.")
      (parent computation)
      (children (sat-solving maxsat constraint-logic-programming)))

    (concept sat-solving
      (description "CDCL SAT solving with two-watched literals, clause learning, non-chronological backtracking, and VSIDS heuristic.")
      (parent satisfiability))

    (concept maxsat
      (description "Optimization over weighted soft clauses: minimize unsatisfied constraints subject to hard clause satisfaction.")
      (parent satisfiability))

    (concept constraint-logic-programming
      (description "CLP(FD) combining miniKanren-style logic variables with finite-domain constraint propagation and arc consistency.")
      (parent satisfiability))

    (concept automata-theory
      (description "Formal models of computation: DFA, NFA, statecharts, and hierarchical state machines.")
      (parent computation))

    (concept formal-languages
      (description "Regular expressions, parser combinators, DFA-backed parsing, and the theory connecting languages to automata.")
      (parent computation))

    (concept equality-saturation
      (description "E-graphs that represent multiple equivalent program forms simultaneously for cost-based optimal extraction.")
      (parent computation))

    ;; ----------------------------------------------------------------
    ;; ROOT: programming-paradigms
    ;; Styles and abstractions for structuring programs.
    ;; ----------------------------------------------------------------
    (concept programming-paradigms
      (description "Organizing principles and abstractions for structuring programs: functional, categorical, logic, and declarative."))

    (concept functional-programming
      (description "Programming with pure functions, immutable data, and compositional abstractions like monads and type classes.")
      (parent programming-paradigms)
      (children (type-classes monadic-programming optics-paradigm rewriting)))

    (concept type-classes
      (description "Dictionary-passing polymorphism implementing Haskell-style abstractions: Functor, Monad, Traversable, Monoid.")
      (parent functional-programming))

    (concept monadic-programming
      (description "Sequencing effects via monads: State, Reader, Writer, Continuation, Free, and algebraic effect handlers.")
      (parent functional-programming))

    (concept optics-paradigm
      (description "Composable data accessors (Lens, Prism, Traversal, Iso, Grate) for focused read/write on nested structures.")
      (parent functional-programming))

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
      (parent meta-programming))

    (concept partial-evaluation
      (description "Specializing programs with respect to known inputs: online/offline PE, Futamura projections, and BTA annotation.")
      (parent meta-programming))

    (concept multi-stage-programming
      (description "Programs that generate and run code in multiple stages, enabling safe code generation with type-aware splicing.")
      (parent meta-programming))

    ;; ----------------------------------------------------------------
    ;; ROOT: numerical-computing
    ;; Floating-point computation, approximation, and simulation.
    ;; ----------------------------------------------------------------
    (concept numerical-computing
      (description "The domain of floating-point algorithms, approximation methods, and rigorous error analysis.")
      (parent mathematics)
      (children (signal-processing interpolation-approximation verified-computation numerical-integration pde-methods)))

    (concept signal-processing
      (description "Digital processing of time-series signals: FFT, digital filters (FIR/IIR), wavelets, convolution, and spectral analysis.")
      (parent numerical-computing))

    (concept interpolation-approximation
      (description "Constructing smooth functions from discrete data: polynomial interpolation, splines, Bezier, Chebyshev, and B-splines.")
      (parent numerical-computing))

    (concept verified-computation
      (description "Numerics with guaranteed error bounds: interval arithmetic, affine arithmetic, and rigorous enclosure methods.")
      (parent numerical-computing))

    (concept numerical-integration
      (description "Numerical integration of ODEs: Euler, Runge-Kutta (RK4, DP45), symplectic integrators, and adaptive step control.")
      (parent numerical-computing))

    (concept pde-methods
      (description "Numerical methods for partial differential equations: FEM on triangular meshes, Method of Lines, and Crank-Nicolson.")
      (parent numerical-computing))

    ;; ----------------------------------------------------------------
    ;; ROOT: optimization
    ;; Finding extrema of functions under constraints.
    ;; ----------------------------------------------------------------
    (concept optimization
      (description "Finding minima or maxima of objective functions, including gradient-based, combinatorial, and verified methods.")
      (children (gradient-based-optimization constrained-optimization combinatorial-optimization global-optimization topological-optimization)))

    (concept gradient-based-optimization
      (description "First-order methods (SGD, Adam, Muon) and second-order methods (Newton, L-BFGS) for smooth objectives.")
      (parent optimization))

    (concept constrained-optimization
      (description "Optimization subject to equality or inequality constraints: LP via simplex, ILP via branch-and-bound, and contractors.")
      (parent optimization))

    (concept combinatorial-optimization
      (description "Discrete optimization: ILP, weighted MaxSAT, knapsack, vertex cover, and set cover problems.")
      (parent optimization))

    (concept global-optimization
      (description "Finding the global minimum of non-convex functions: interval branch-and-bound with guaranteed enclosure.")
      (parent optimization))

    (concept topological-optimization
      (description "Using persistent homology to analyze loss surfaces: basin counting, saddle detection, and landscape comparison.")
      (parent optimization))

    ;; ----------------------------------------------------------------
    ;; ROOT: geometry
    ;; Spatial reasoning, shapes, and rendering.
    ;; ----------------------------------------------------------------
    (concept geometry
      (description "The domain of spatial structures, shapes, proximity queries, and geometric transformations.")
      (children (computational-geometry rendering-geometry mesh-processing)))

    (concept computational-geometry
      (description "Algorithms for geometric problems: convex hulls, Delaunay triangulation, Voronoi diagrams, and collision detection.")
      (parent geometry))

    (concept rendering-geometry
      (description "Raymarching, BVH/octree acceleration structures, signed distance fields, and mesh-SDF computation for rendering.")
      (parent geometry))

    (concept mesh-processing
      (description "Triangular mesh generation, Laplacian smoothing, adaptive refinement, and topological validation via Betti numbers.")
      (parent geometry))

    ;; ----------------------------------------------------------------
    ;; ROOT: probabilistic-reasoning
    ;; Uncertainty, randomness, and statistical inference.
    ;; ----------------------------------------------------------------
    (concept probabilistic-reasoning
      (description "Reasoning under uncertainty using probability distributions, sampling, and Bayesian inference.")
      (children (probability-distributions bayesian-inference monte-carlo-methods variational-methods statistical-modeling)))

    (concept probability-distributions
      (description "Continuous and discrete distributions (Normal, Poisson, Beta, Dirichlet) with pure state-monad sampling.")
      (parent probabilistic-reasoning))

    (concept bayesian-inference
      (description "Conjugate models (Beta-Binomial, Normal-Normal, Gamma-Poisson), posterior computation, and Bayes factors.")
      (parent probabilistic-reasoning))

    (concept monte-carlo-methods
      (description "Importance sampling, rejection sampling, Metropolis-Hastings MCMC, Gibbs sampling, and variance reduction.")
      (parent probabilistic-reasoning))

    (concept variational-methods
      (description "Variational inference via ELBO optimization, reparameterization trick, and mean-field Gaussian families.")
      (parent probabilistic-reasoning))

    (concept statistical-modeling
      (description "Linear models, GLM (logistic, Poisson), regularized regression (Ridge, Lasso), and time series (AR, MA, Holt-Winters).")
      (parent probabilistic-reasoning))

    ;; ----------------------------------------------------------------
    ;; ROOT: physics-simulation
    ;; Modeling physical systems computationally.
    ;; ----------------------------------------------------------------
    (concept physics-simulation
      (description "Computational modeling of physical systems: rigid bodies, particles, dynamical systems, and differentiable simulation.")
      (children (rigid-body-physics dynamical-systems differentiable-physics)))

    (concept rigid-body-physics
      (description "2D and 3D rigid body simulation: collision detection/response, constraints, joints, and spatial hashing.")
      (parent physics-simulation))

    (concept dynamical-systems
      (description "Continuous-time systems: ODE solvers, chaos detection (Lyapunov exponents), attractors, and bifurcation analysis.")
      (parent physics-simulation))

    (concept differentiable-physics
      (description "Physics simulation where autodiff traces through rollouts, enabling gradient-based policy and trajectory optimization.")
      (parent physics-simulation))

    ;; ----------------------------------------------------------------
    ;; ROOT: machine-learning
    ;; Learning, generalization, and model training.
    ;; ----------------------------------------------------------------
    (concept machine-learning
      (description "The domain of learning systems: optimization for training, data pipelines, benchmarking, and agent orchestration.")
      (children (automatic-differentiation agent-orchestration dataset-engineering)))

    (concept automatic-differentiation
      (description "Computing exact derivatives by propagating through computational graphs: forward mode (dual numbers), reverse mode (backprop).")
      (parent machine-learning))

    (concept agent-orchestration
      (description "Multi-stage AI pipelines, LLM effects, council-based deliberation, and arrow-based workflow composition.")
      (parent machine-learning))

    (concept dataset-engineering
      (description "Structured dataset construction: sample schemas, parameter sampling, distractor generation, and JSONL export.")
      (parent machine-learning))

    ;; ----------------------------------------------------------------
    ;; ROOT: system-design
    ;; Architectural patterns, protocols, and infrastructure.
    ;; ----------------------------------------------------------------
    (concept system-design
      (description "Architecture-level concerns: content addressing, IPC protocols, security, validation, and query infrastructure.")
      (children (content-addressing cryptographic-primitives protocol-design query-infrastructure validation-contracts)))

    (concept content-addressing
      (description "Identity via cryptographic hash: blocks as (tag, payload, refs[]), CAS storage, and schema migration via optics.")
      (parent system-design))

    (concept cryptographic-primitives
      (description "Hash functions (SHA-256/384/512, BLAKE2b), HMAC, and elliptic curve arithmetic (secp256k1, P-256, ECDH).")
      (parent system-design))

    (concept protocol-design
      (description "Wire protocols, IPC message formats, and extensible open dispatch systems for inter-component communication.")
      (parent system-design))

    (concept query-infrastructure
      (description "Block query DSL, optic-based declarative queries, Aho-Corasick multi-pattern search, and SQL-like syntax.")
      (parent system-design))

    (concept validation-contracts
      (description "Content-addressed declarative data shape specifications with structural and numeric constraints, hash-versioned.")
      (parent system-design))

    ;; ----------------------------------------------------------------
    ;; ROOT: game-and-decision-theory
    ;; Strategic interaction and multi-agent reasoning.
    ;; ----------------------------------------------------------------
    (concept game-and-decision-theory
      (description "The mathematical study of strategic interaction, equilibria, fair allocation, and mechanism design.")
      (children (strategic-games cooperative-games social-choice mechanism-design evolutionary-game-theory multi-criteria-decision)))

    (concept strategic-games
      (description "Normal and extensive form games, Nash equilibrium, backward induction, and subgame perfect equilibrium.")
      (parent game-and-decision-theory))

    (concept cooperative-games
      (description "Coalitional games, Shapley value, core, nucleolus, Banzhaf index, and Nash bargaining solutions.")
      (parent game-and-decision-theory))

    (concept social-choice
      (description "Voting systems (plurality, Borda, Schulze, STV, PAV), Condorcet winners, and Gibbard-Satterthwaite impossibility.")
      (parent game-and-decision-theory))

    (concept mechanism-design
      (description "Auction theory (first-price, VCG, double), incentive compatibility, revelation principle, and Myerson optimal auctions.")
      (parent game-and-decision-theory))

    (concept evolutionary-game-theory
      (description "Replicator dynamics, evolutionarily stable strategies (ESS), invasion analysis, and population dynamics.")
      (parent game-and-decision-theory))

    (concept multi-criteria-decision
      (description "Decision making over multiple objectives: Pareto frontiers, weighted Borda, sensitivity analysis, and robustness.")
      (parent game-and-decision-theory))

    ;; ----------------------------------------------------------------
    ;; ROOT: knowledge-infrastructure
    ;; Tools for navigating and introspecting the lattice itself.
    ;; ----------------------------------------------------------------
    (concept knowledge-infrastructure
      (description "The tooling for navigating, searching, and understanding the lattice: search, cross-reference, and analytics.")
      (children (lattice-navigation search-tooling)))

    (concept lattice-navigation
      (description "DAG traversal, dependency analysis, skill inspection, module analytics, and serendipitous cross-domain discovery.")
      (parent knowledge-infrastructure))

    (concept search-tooling
      (description "BM25 text search over skill exports, type-aware Hoogle-style search, and KG-backed concept bridging.")
      (parent knowledge-infrastructure))

  )

  ;;; ====
  ;;; Synonym Groups
  ;;; ====
  ;;; First symbol = canonical. All others are recognized aliases.
  ;;; Covers every (aliases ...) and most (keywords ...) entries across manifests.

  (synonym-groups

    ;; Core skills
    (automatic-differentiation autodiff ad diff reverse-diff backprop)
    (linear-algebra la lin-alg linear matrix-math linalg)
    (functional-programming fp functional)
    (differential-geometry diffgeo diffgeom manifolds smooth-manifolds lie-groups riemannian-geometry geodesics exterior-calculus cartan-calculus)
    (abstract-algebra algebra group-theory ring-theory polynomial-algebra galois-fields)
    (data-structures ds structures collections)
    (numerical-computing numerics signal dsp interp interval affine fem pde-time)
    (optimization optim opt minimize)
    (statistics stats stat regression models)
    (topology topo simplicial persistent tda)
    (cryptography crypto hash hashing ec ecc)
    (number-theory modular mod-arith nt primes fast-mult)
    (satisfiability sat boolean-sat cdcl maxsat)
    (constraint-logic-programming clp fd ckanren constraint-logic)
    (optics lens prism profunctor-optics)
    (egraph e-graph equality-saturation)
    (domain-specific-languages dsl meta-dsl language-tools tagless)
    (automata state-machines fsm statecharts)
    (random rand prng probability sampling vi)
    (game-theory game games)
    (pipeline workflow agent-pipeline stages arrows)
    (knowledge-infrastructure meta lattice-meta lattice-tools skills)
    (geometry geom 3d-geometry spatial)
    (parallel par strategies)
    (information-theory information entropy coding)
    (simulation simulation-stream chaos dynamics bifurcation)
    (control-theory control-systems)
    (dataset data sample-gen)
    (validation contracts)
    (markdown md markdown-parser)
    (template grammar-dsl code-builder sexpr-template)
    (information-bridge info-stat mi-bridge)

    ;; Aliases for concept keywords
    (groebner-basis groebner buchberger polynomial-reduction)
    (galois-field finite-field gf2n binary-field aes-field)
    (monte-carlo mcmc metropolis-hastings gibbs-sampling importance-sampling)
    (variational-inference elbo reparameterization bayesian-optimization vi)
    (persistent-homology tda barcode persistence-diagram vietoris-rips)
    (simplicial-complex simplicial betti-numbers euler-characteristic homology)
    (eigenvalue-theory eigenvalue eigenvalues eigenvectors svd singular-value-decomposition spectral)
    (spectral-clustering graph-laplacian fiedler-vector algebraic-connectivity)
    (community-detection label-propagation modularity modularity-ilp)
    (pagerank eigenvector-centrality katz-centrality closeness-centrality betweenness-centrality)
    (minimum-spanning-tree mst prim kruskal union-find)
    (shortest-path dijkstra floyd-warshall bfs dfs)
    (convex-hull graham-scan quickhull minkowski)
    (delaunay-triangulation voronoi lloyd-relaxation laplacian-smoothing)
    (marching-cubes isosurface sdf signed-distance-field)
    (bvh-tree bvh octree spatial-acceleration)
    (quaternion rotation slerp rodrigues)
    (lie-group so2 so3 se2 se3 exponential-map)
    (riemannian-metric christoffel-symbols riemann-tensor ricci-tensor scalar-curvature)
    (differential-forms wedge-product exterior-derivative hodge-star)
    (signal-processing fft dft fast-fourier-transform spectral-analysis)
    (digital-filter fir iir butterworth chebyshev biquad)
    (wavelet haar daubechies dwt)
    (interpolation spline bezier hermite lagrange)
    (interval-arithmetic verified-computation rigorous-bounds)
    (affine-arithmetic correlation-tracking dependency-problem)
    (finite-element-method fem poisson-equation elliptic-pde)
    (pde-time-stepping forward-euler backward-euler crank-nicolson method-of-lines rk4)
    (gradient-descent sgd adam muon rmsprop adagrad momentum)
    (lbfgs quasi-newton limited-memory-bfgs)
    (linear-programming simplex-method lp dual sensitivity-analysis)
    (integer-programming ilp branch-and-bound gomory-cuts knapsack set-cover)
    (interval-optimization global-minimum verified-optimization monotonicity-pruning)
    (constraint-propagation arc-consistency contractors)
    (polynomial-identity egraph-groebner rewrite-rules normal-form)
    (nash-equilibrium best-response pure-nash mixed-nash)
    (shapley-value cooperative-value banzhaf-index power-index)
    (stable-matching gale-shapley two-sided-matching)
    (voting-rules plurality borda condorcet schulze)
    (mechanism-design vcg vickrey auction incentive-compatibility)
    (replicator-dynamics evolutionarily-stable-strategy ess hawk-dove)
    (state-machine statechart harel hierarchical-state parallel-region)
    (parser-combinators monadic-parsing packrat-memoization regex-dfa)
    (tagless-final interpreter-composition free-monad algebraic-effects)
    (partial-evaluation futamura-projections binding-time-analysis)
    (multi-stage-programming code-generation splice staging)
    (content-addressing cas-storage hash-identity block-machine)
    (sha256 sha512 sha384 blake2b hmac message-authentication)
    (elliptic-curve ecc secp256k1 p256 ecdh ecdsa)
    (aho-corasick multi-pattern-search string-matching)
    (optic-query declarative-query sql-like-dsl)
    (rigid-body collision-detection impulse-response spatial-hash)
    (chaos-theory lyapunov-exponents strange-attractor poincare-section)
    (bifurcation hopf saddle-node pitchfork period-doubling feigenbaum)
    (parallel-computation divide-and-conquer parallel-reduction scan-prefix)
    (board-game hex-grid square-grid pathfinding fov line-of-sight roguelike boardcraft game-board grid-sdk tactical)
    (rigid-body-physics-2d physics-2d classical-physics physics/classical)
    (rigid-body-physics-3d physics-3d classical-physics-3d physics/classical3d)
    (differentiable-physics-2d diff-physics diff-sim physics/diff)
    (differentiable-physics-3d diff-physics-3d diff-sim-3d physics/diff3d)
    (physics-optics physics-lens physics-lenses-3d physics-optics)
    (block-query optic-query block-query-dsl)
    (profunctor strong-profunctor choice-profunctor closed-profunctor wander)
    (bidirectional-transformation migration schema-evolution rollback)
    (council-deliberation multi-model llm-orchestration consensus)
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
