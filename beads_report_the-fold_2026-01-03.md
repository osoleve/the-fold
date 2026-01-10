# Beads Export

*Generated: Sat, 03 Jan 2026 01:42:08 UTC*

## Summary

| Metric | Count |
|--------|-------|
| **Total** | 587 |
| Open | 222 |
| In Progress | 0 |
| Blocked | 0 |
| Closed | 365 |

## Quick Actions

Ready-to-run commands for bulk operations:

```bash
# Close open items (222 total, showing first 10)
bd close fold-wz94 fold-3w7 fold-vhp fold-9tn fold-cpt fold-93g fold-u72 fold-01l fold-9tl fold-hnp

# View high-priority items (P0/P1)
bd show fold-wz94 fold-3w7 fold-vhp fold-9tn fold-cpt fold-93g fold-u72 fold-01l fold-9tl fold-hnp fold-5vq fold-cmk fold-00g fold-81b fold-bym fold-1e0

```

## Table of Contents

- [🟢 fold-wz94 Discrete Time Simulation SDK Epic](#fold-wz94)
- [🟢 fold-3w7 Arbitrary Precision Arithmetic Epic](#fold-3w7)
- [🟢 fold-vhp Pretty Printing Combinators Epic](#fold-vhp)
- [🟢 fold-9tn Interval Arithmetic Library Epic](#fold-9tn)
- [🟢 fold-cpt Parallel Evaluation Strategies Epic](#fold-cpt)
- [🟢 fold-93g Parser Combinators Library Epic](#fold-93g)
- [🟢 fold-u72 Core Functional Programming Abstractions Epic](#fold-u72)
- [🟢 fold-01l Implement geometric primitives and transformations](#fold-01l)
- [🟢 fold-9tl Information Theory Toolkit](#fold-9tl)
- [🟢 fold-hnp High Precision Math Library](#fold-hnp)
- [🟢 fold-5vq Bayesian Inference Engine](#fold-5vq)
- [🟢 fold-cmk Implement Digital Filter Library](#fold-cmk)
- [🟢 fold-00g Implement gradient storage and retrieval system](#fold-00g)
- [🟢 fold-81b Integrate autodiff with AST and type annotation](#fold-81b)
- [🟢 fold-bym Autodiff Engine Epic - Complete Implementation](#fold-bym)
- [🟢 fold-1e0 Linear Algebra Library Epic](#fold-1e0)
- [🟢 fold-k5tt Fix hessian-jet to use exact differentiation for mixed partials](#fold-k5tt)
- [🟢 fold-dw78 Add high-level serialize/deserialize API for resumable random sessions](#fold-dw78)
- [🟢 fold-nfy4 Add random-bytes primitive to Random effect](#fold-nfy4)
- [🟢 fold-5foh Add split operation to Random effect for parallel simulations](#fold-5foh)
- [🟢 fold-8o0 Refactor graph.ss into semantic submodules](#fold-8o0)
- [🟢 fold-mlz Refactor string.ss into semantic submodules](#fold-mlz)
- [🟢 fold-o9e Refactor numeric.ss into semantic submodules](#fold-o9e)
- [🟢 fold-wom Refactor collection.ss into semantic submodules](#fold-wom)
- [🟢 fold-c1i Add tests for nbe.ss (normalization by evaluation)](#fold-c1i)
- [🟢 fold-a6w Add tests for expand.ss (canonical form expansion)](#fold-a6w)
- [🟢 fold-h4l Implement parallel runtime for par/pseq](#fold-h4l)
- [🟢 fold-9rp Implement indentation-sensitive parsing](#fold-9rp)
- [🟢 fold-fj1 Rewrite playpen/quill/parse.ss using DSL toolkit](#fold-fj1)
- [🟢 fold-ghf Implement identity pre-seeding for alignment](#fold-ghf)
- [🟢 fold-sjv Track posts per identity](#fold-sjv)
- [🟢 fold-9if Integrate identity system with (hi) login](#fold-9if)
- [🟢 fold-jpj Build welcome back screen renderer](#fold-jpj)
- [🟢 fold-6l4 Implement @mention parsing and storage](#fold-6l4)
- [🟢 fold-n5z Implement username lookup and registration](#fold-n5z)
- [🟢 fold-u4x Design identity record schema and storage](#fold-u4x)
- [🟢 fold-0mz Structured editing (paredit/parinfer)](#fold-0mz)
- [🟢 fold-228 Undo/redo for REPL with branching history](#fold-228)
- [🟢 fold-n3b REPL command palette and fuzzy finder](#fold-n3b)
- [🟢 fold-4ll Visual block explorer and graph viewer](#fold-4ll)
- [🟢 fold-k61 Type-driven development with holes](#fold-k61)
- [🟢 fold-tca Examples gallery with runnable snippets](#fold-tca)
- [🟢 fold-2y9 Language Server Protocol (LSP) implementation](#fold-2y9)
- [🟢 fold-00t Interactive tutorial system](#fold-00t)
- [🟢 fold-b1h REPL tab completion and history](#fold-b1h)
- [🟢 fold-k6m Implement pattern matching compilation](#fold-k6m)
- [🟢 fold-4wu Implement reader extensions and custom notation](#fold-4wu)
- [🟢 fold-dxg Implement multi-stage programming](#fold-dxg)
- [🟢 fold-gb3 Implement associated type families](#fold-gb3)
- [🟢 fold-n7e Implement Rank-N polymorphism](#fold-n7e)
- [🟢 fold-zbu Implement existential types](#fold-zbu)
- [🟢 fold-h07 Implement GADTs (Generalized Algebraic Data Types)](#fold-h07)
- [🟢 fold-5cy Implement numeric tower integration](#fold-5cy)
- [🟢 fold-5iy Implement fast multiplication algorithms](#fold-5iy)
- [🟢 fold-dgy Implement graph visualization](#fold-dgy)
- [🟢 fold-hh3 Implement SVG renderer](#fold-hh3)
- [🟢 fold-bz7 Implement layout combinators](#fold-bz7)
- [🟢 fold-hrp Implement transforms and composition](#fold-hrp)
- [🟢 fold-kg8 Implement shape primitives](#fold-kg8)
- [🟢 fold-hex Integrate contracts with refinement types](#fold-hex)
- [🟢 fold-gj4 Implement higher-order contract wrapping](#fold-gj4)
- [🟢 fold-go1 Implement contract checking modes](#fold-go1)
- [🟢 fold-w41 Implement contract primitives](#fold-w41)
- [🟢 fold-3cw Implement ANSI terminal output](#fold-3cw)
- [🟢 fold-0j5 Implement Pretty type class](#fold-0j5)
- [🟢 fold-gup Implement generic zipper derivation](#fold-gup)
- [🟢 fold-0fj Implement tree zipper](#fold-0fj)
- [🟢 fold-a1v Implement list zipper](#fold-a1v)
- [🟢 fold-jzz Implement affine arithmetic](#fold-jzz)
- [🟢 fold-lq9 Implement interval Newton and root finding](#fold-lq9)
- [🟢 fold-0a9 Implement work-stealing scheduler](#fold-0a9)
- [🟢 fold-uqu Implement parallel matrix operations](#fold-uqu)
- [🟢 fold-5o0 Integrate units with Vec and Matrix](#fold-5o0)
- [🟢 fold-u8r State Machines and Automata Epic](#fold-u8r)
- [🟢 fold-rm6 Declarative Graphics Library Epic](#fold-rm6)
- [🟢 fold-h4x Contracts and Verification Epic](#fold-h4x)
- [🟢 fold-294 Zipper Data Structures Epic](#fold-294)
- [🟢 fold-lzr Auto-parallelization + fusion hints](#fold-lzr)
- [🟢 fold-com Equational reasoning + rewrite assistant](#fold-com)
- [🟢 fold-h9p Implement physics simulation DSL via Free monad](#fold-h9p)
- [🟢 fold-4ve Implement physics state lenses](#fold-4ve)
- [🟢 fold-ux4 Effect typing + linting](#fold-ux4)
- [🟢 fold-ux0 Type-driven search helpers](#fold-ux0)
- [🟢 fold-um3 Dead code + unused binding detection](#fold-um3)
- [🟢 fold-jxx Pattern match exhaustiveness + redundancy checks](#fold-jxx)
- [🟢 fold-4as Totality + termination checker](#fold-4as)
- [🟢 fold-75l Time-travel debugger + explain tracing](#fold-75l)
- [🟢 fold-x0k Implement evolutionary game theory](#fold-x0k)
- [🟢 fold-8rh Implement mechanism design](#fold-8rh)
- [🟢 fold-8f1 Implement cooperative games](#fold-8f1)
- [🟢 fold-som Implement extensive form games](#fold-som)
- [🟢 fold-dzr Implement chaos detection and analysis](#fold-dzr)
- [🟢 fold-moa Implement bifurcation analysis](#fold-moa)
- [🟢 fold-1af Implement stability analysis](#fold-1af)
- [🟢 fold-fo1 Implement ODE system representation](#fold-fo1)
- [🟢 fold-xl6 Implement global optimization](#fold-xl6)
- [🟢 fold-5nf Implement constraint satisfaction](#fold-5nf)
- [🟢 fold-bjg Implement integer programming](#fold-bjg)
- [🟢 fold-ygh Implement convex optimization](#fold-ygh)
- [🟢 fold-c3a Implement linear programming](#fold-c3a)
- [🟢 fold-1vz Effect + flow inspector](#fold-1vz)
- [🟢 fold-wza Implement spatial data structures](#fold-wza)
- [🟢 fold-cvd Refactoring engine (rename/extract/inline)](#fold-cvd)
- [🟢 fold-0mu Implement mesh generation and refinement](#fold-0mu)
- [🟢 fold-4az Implement Delaunay triangulation](#fold-4az)
- [🟢 fold-gh0 Implement Voronoi diagrams](#fold-gh0)
- [🟢 fold-dls Implement convex hull algorithms](#fold-dls)
- [🟢 fold-moe Typed hole suggestions + synthesis](#fold-moe)
- [🟢 fold-k1z Advanced functional programming tooling](#fold-k1z)
- [🟢 fold-sib Implement modules and linear algebra over rings](#fold-sib)
- [🟢 fold-doz Implement polynomial algebra](#fold-doz)
- [🟢 fold-64t Implement field operations](#fold-64t)
- [🟢 fold-5rn Implement ring structures](#fold-5rn)
- [🟢 fold-64f Implement group operations and properties](#fold-64f)
- [🟢 fold-6cm Implement hash function primitives](#fold-6cm)
- [🟢 fold-5i2 Implement primality testing and generation](#fold-5i2)
- [🟢 fold-a2y Implement elliptic curve operations](#fold-a2y)
- [🟢 fold-duy Implement finite field arithmetic](#fold-duy)
- [🟢 fold-n9i Implement modular arithmetic](#fold-n9i)
- [🟢 fold-1nb Implement discrete control systems](#fold-1nb)
- [🟢 fold-84e Implement stability analysis](#fold-84e)
- [🟢 fold-cyp Implement controller design](#fold-cyp)
- [🟢 fold-rd5 Implement transfer functions](#fold-rd5)
- [🟢 fold-jey Implement time stepping schemes](#fold-jey)
- [🟢 fold-hoo Implement mesh generation](#fold-hoo)
- [🟢 fold-y6c Implement finite element method basics](#fold-y6c)
- [🟢 fold-qdz Implement finite difference methods](#fold-qdz)
- [🟢 fold-e3n Implement symbolic equation solving](#fold-e3n)
- [🟢 fold-vnn Implement algebraic simplification](#fold-vnn)
- [🟢 fold-arf Implement symbolic integration](#fold-arf)
- [🟢 fold-bwy Implement symbolic differentiation](#fold-bwy)
- [🟢 fold-0dk Implement monads and Kleisli categories](#fold-0dk)
- [🟢 fold-9z2 Implement adjoint functors](#fold-9z2)
- [🟢 fold-4z0 Implement limits and colimits](#fold-4z0)
- [🟢 fold-fmo Implement natural transformations](#fold-fmo)
- [🟢 fold-x9t Implement Lie groups and algebras](#fold-x9t)
- [🟢 fold-a71 Implement geodesic computation](#fold-a71)
- [🟢 fold-r1k Implement curvature computations](#fold-r1k)
- [🟢 fold-yql Implement tangent and cotangent spaces](#fold-yql)
- [🟢 fold-8t7 Implement coordinate charts and atlases](#fold-8t7)
- [🟢 fold-tuq Dynamical Systems Library Epic](#fold-tuq)
- [🟢 fold-pd2 Optimization Library Epic (Convex and Linear)](#fold-pd2)
- [🟢 fold-5mb Advanced Computational Geometry Epic](#fold-5mb)
- [🟢 fold-6yu Abstract Algebra Library Epic](#fold-6yu)
- [🟢 fold-iki Cryptographic Primitives Library Epic](#fold-iki)
- [🟢 fold-dag Control Theory Library Epic](#fold-dag)
- [🟢 fold-j32 Numerical PDE Library Epic](#fold-j32)
- [🟢 fold-01j Symbolic Computation Library Epic](#fold-01j)
- [🟢 fold-018 Category Theory Library Epic](#fold-018)
- [🟢 fold-asz Differential Geometry Library Epic](#fold-asz)
- [🟢 fold-gjr Implement persistent homology and TDA](#fold-gjr)
- [🟢 fold-oxy Implement homology computation](#fold-oxy)
- [🟢 fold-7rp Implement simplicial complex data structures](#fold-7rp)
- [🟢 fold-7gp Topology Library Epic](#fold-7gp)
- [🟢 fold-6ys Implement special mathematical functions](#fold-6ys)
- [🟢 fold-p5d Implement interpolation and curve fitting](#fold-p5d)
- [🟢 fold-bdg Implement differentiable signal processing](#fold-bdg)
- [🟢 fold-b1d Add channel capacity calculations](#fold-b1d)
- [🟢 fold-gj0 Create statistical measures](#fold-gj0)
- [🟢 fold-5ie Add coding theory functions](#fold-5ie)
- [🟢 fold-l7o Add matrix operations](#fold-l7o)
- [🟢 fold-bi7 Create number theory utilities](#fold-bi7)
- [🟢 fold-fwb Implement constraint graph for physics solver](#fold-fwb)
- [🟢 fold-qce Implement differentiable physics simulation](#fold-qce)
- [🟢 fold-98v Probabilistic Programming Constructs](#fold-98v)
- [🟢 fold-23u Statistical Models and Regression](#fold-23u)
- [🟢 fold-wcr Wavelet Transform Implementation](#fold-wcr)
- [🟢 fold-x0z Window Functions and Spectral Analysis](#fold-x0z)
- [🟢 fold-evy Implement 3D Collision Response](#fold-evy)
- [🟢 fold-7hs Implement 3D Collision Detection](#fold-7hs)
- [🟢 fold-1oe Implement 3D Vector and Quaternion Math](#fold-1oe)
- [🟢 fold-g6n Implement differentiable type constructor with dependent tracking](#fold-g6n)
- [🟢 fold-tg0 Implement typed graph properties and invariants](#fold-tg0)
- [🟢 fold-z8q Create 3D Physics Engine Core Architecture](#fold-z8q)
- [🟢 fold-ouh Implement type-safe gradient dimensions for autodiff](#fold-ouh)
- [🟢 fold-yka 3D Physics Engine Epic](#fold-yka)
- [🟢 fold-yy5 2D Physics Engine Epic](#fold-yy5)
- [🟢 fold-ac0 Signal Processing Library Epic](#fold-ac0)
- [🟢 fold-qk1 Probabilistic Modeling Library Epic](#fold-qk1)
- [🟢 fold-rfc Integrate dependent types with module system](#fold-rfc)
- [🟢 fold-cfv Implement inductive type definitions](#fold-cfv)
- [🟢 fold-b8v Implement termination checking for type-level computation](#fold-b8v)
- [🟢 fold-9dy Implement dependent pattern matching](#fold-9dy)
- [🟢 fold-755 Implement equality types and proofs](#fold-755)
- [🟢 fold-8wy Implement refinement types](#fold-8wy)
- [🟢 fold-xst Implement performance profiling for differentiable code](#fold-xst)
- [🟢 fold-8hy Create computational graph visualization](#fold-8hy)
- [🟢 fold-kcb Build gradient debugging and inspection tools](#fold-kcb)
- [🟢 fold-lim Implement optimization algorithms](#fold-lim)
- [🟢 fold-bms Implement eigenvector centrality and other matrix-based centrality metrics](#fold-bms)
- [🟢 fold-1r0 Implement matrix-based graph distance algorithms](#fold-1r0)
- [🟢 fold-qv7 Implement spectral clustering using SVD/eigendecomposition](#fold-qv7)
- [🟢 fold-756 Implement PageRank algorithm using eigenvalue computation](#fold-756)
- [🟢 fold-uat Create linalg documentation and examples](#fold-uat)
- [🟢 fold-1y3 Add graph visualization utilities to complement graph algorithms](#fold-1y3)
- [🟢 fold-yp7 Implement advanced graph algorithms (PageRank, community detection, shortest paths with weights)](#fold-yp7)
- [🟢 fold-ggs Write extensive documentation and examples for graph algorithms library](#fold-ggs)
- [🟢 fold-esm9 Optimize random-weighted-eff to single-pass traversal](#fold-esm9)
- [🟢 fold-nmdd Add floating-point tolerance to sparse matrix operations](#fold-nmdd)
- [🟢 fold-935 Cookbook: common patterns and recipes](#fold-935)
- [🟢 fold-744 Implement partial evaluation](#fold-744)
- [🟢 fold-fht Integrate zippers with comonad and lens](#fold-fht)
- [🟢 fold-ajm AST-aware formatter + style profiles](#fold-ajm)
- [🟢 fold-y2f Interactive proof sketcher for program properties](#fold-y2f)
- [🟢 fold-qca FP code templates + pattern generator](#fold-qca)
- [🟢 fold-waa Game Theory Library Epic](#fold-waa)
- [🟢 fold-38f Create rate-distortion toolkit](#fold-38f)
- [🟢 fold-73p Create Physics Engine Documentation](#fold-73p)
- [🟢 fold-b7t Implement proof tactics and automation](#fold-b7t)
- [🟢 fold-y3h Create scientific computing examples and demos](#fold-y3h)
- [🟢 fold-ixe Build comprehensive test suite for autodiff](#fold-ixe)
- [🟢 fold-a6i Add linalg performance benchmarks and optimization](#fold-a6i)
- [🟢 fold-ygg Implement iterative linear solvers](#fold-ygg)
- [🟢 fold-145 Implement Singular Value Decomposition (SVD)](#fold-145)
- [🟢 fold-abw Create real-world example applications using graph algorithms library](#fold-abw)
- [🟢 fold-0cr Document core vs patterns module boundaries](#fold-0cr)
- [🟢 fold-wdx Add session timeout configuration](#fold-wdx)
- [🟢 fold-nszd laplacian-connected-components relative tolerance floor inappropriate](#fold-nszd)
- [🟢 fold-r06r QR basis recovery lacks orthogonality verification](#fold-r06r)
- [🟢 fold-9g55 matrix-symmetric? uses exact equality instead of approximate](#fold-9g55)
- [🟢 fold-61dp Document COO duplicate entry behavior](#fold-61dp)
- [🟢 fold-jx0 Agent Identity and Continuity System](#fold-jx0)
- [⚫ fold-pfgq Discord bot is echoing user inputs](#fold-pfgq)
- [⚫ fold-p91 TECH DEBT: fp/ directory contains 54K lines of mostly dormant code](#fold-p91)
- [⚫ fold-otq Implement bytevector literal parser support](#fold-otq)
- [⚫ fold-1v8 Character-to-number conversion bug in forum functions](#fold-1v8)
- [⚫ fold-5xd String parsing fails on exclamation marks](#fold-5xd)
- [⚫ fold-f2j Create Physics Engine Test Suite](#fold-f2j)
- [⚫ fold-po9 Design dependent type system architecture](#fold-po9)
- [⚫ fold-hvm Design linalg library architecture](#fold-hvm)
- [⚫ fold-cz8 Developer Experience Initiative: Making The Fold Accessible to Newcomers](#fold-cz8)
- [⚫ fold-3jj Fix hardcoded Windows paths in thimble tests and scaffold templates](#fold-3jj)
- [⚫ fold-8tes Fix stream-cartesian to produce true Cartesian product](#fold-8tes)
- [⚫ fold-1os9 Fix nested constraint syntax in TC-Collection.member?](#fold-1os9)
- [⚫ fold-zxk8 Fix incorrect least-squares formula in matrix-solvers](#fold-zxk8)
- [⚫ fold-84a9 Fix dimension mismatch in qr-algorithm-shifted](#fold-84a9)
- [⚫ fold-13o Decide experimental/fp graduation strategy](#fold-13o)
- [⚫ fold-qr2 Consolidate string utilities to core/prelude.ss](#fold-qr2)
- [⚫ fold-uzb Establish core/ pruning policy](#fold-uzb)
- [⚫ fold-kjw TECH DEBT: data-structures.ss overlaps with fp/data/*](#fold-kjw)
- [⚫ fold-c8x TECH DEBT: Orphaned fp modules never loaded](#fold-c8x)
- [⚫ fold-dck TECH DEBT: Duplicate Differentiable type class implementations](#fold-dck)
- [⚫ fold-d8o TECH DEBT: Duplicate State monad implementations](#fold-d8o)
- [⚫ fold-pb7 fold-rs: IO primitives (display, read, file-io)](#fold-pb7)
- [⚫ fold-1h8 fold-rs: Implement module/load system](#fold-1h8)
- [⚫ fold-46k fold-rs: Implement type system (types, infer, kinds)](#fold-46k)
- [⚫ fold-plg Complete Rust REPL daemon cutover](#fold-plg)
- [⚫ fold-0by Rust daemon missing forum commands (hi, digest, chat, etc.)](#fold-0by)
- [⚫ fold-2e4 Bug: filter primitive declared but throws error at runtime](#fold-2e4)
- [⚫ fold-aqx Implement load primitive in Rust](#fold-aqx)
- [⚫ fold-36m Implement vector literal parser support](#fold-36m)
- [⚫ fold-3l4 Implement character literal parser support](#fold-3l4)
- [⚫ fold-y5y Missing advertised functions](#fold-y5y)
- [⚫ fold-i24 Block explorer crashes with bytevector index error](#fold-i24)
- [⚫ fold-kw0 Implement BigInt primitive operations](#fold-kw0)
- [⚫ fold-07j Add BigInt variant to Value enum](#fold-07j)
- [⚫ fold-y2p Add num-bigint dependency to fold-rs](#fold-y2p)
- [⚫ fold-9zx Optimize BigNum implementation in fold-rs](#fold-9zx)
- [⚫ fold-wqd Add source locations to error messages](#fold-wqd)
- [⚫ fold-mvh Implement (help) and (apropos) for primitive discovery](#fold-mvh)
- [⚫ fold-ypt Implement algebraic effect handlers](#fold-ypt)
- [⚫ fold-kwo Implement quasiquotation and syntax templates](#fold-kwo)
- [⚫ fold-cmm Implement Tagless Final pattern](#fold-cmm)
- [⚫ fold-ok4 DSL Infrastructure Epic](#fold-ok4)
- [⚫ fold-z5d Implement multi-parameter type classes](#fold-z5d)
- [⚫ fold-5sa Implement Higher-Kinded Types (HKTs)](#fold-5sa)
- [⚫ fold-y8d Implement Alternative and MonadPlus](#fold-y8d)
- [⚫ fold-u3i Implement Semigroup/Monoid/Group algebraic typeclasses](#fold-u3i)
- [⚫ fold-0ln Implement perplexity metric](#fold-0ln)
- [⚫ fold-f5s Implement autoregressive generation](#fold-f5s)
- [⚫ fold-bgq Implement sampling strategies](#fold-bgq)
- [⚫ fold-2fv Implement KV cache](#fold-2fv)
- [⚫ fold-cz4 Implement SFT training loop](#fold-cz4)
- [⚫ fold-obt Implement learning rate schedulers](#fold-obt)
- [⚫ fold-ibi Implement AdamW optimizer](#fold-ibi)
- [⚫ fold-414 Implement cross-entropy loss](#fold-414)
- [⚫ fold-kiv Implement GPT model](#fold-kiv)
- [⚫ fold-4et Implement Transformer block](#fold-4et)
- [⚫ fold-4wb Implement positional embeddings](#fold-4wb)
- [⚫ fold-t4o Implement multi-head attention](#fold-t4o)
- [⚫ fold-6q2 Implement scaled dot-product attention](#fold-6q2)
- [⚫ fold-xte Implement activation functions](#fold-xte)
- [⚫ fold-mho Implement LayerNorm](#fold-mho)
- [⚫ fold-hzv Implement Embedding layer](#fold-hzv)
- [⚫ fold-kmf Implement Linear layer](#fold-kmf)
- [⚫ fold-1ob Implement BPE encode/decode](#fold-1ob)
- [⚫ fold-7n0 Implement BPE tokenizer training](#fold-7n0)
- [⚫ fold-4r9 LLM Inference Engine Epic](#fold-4r9)
- [⚫ fold-2d4 Training Infrastructure Epic](#fold-2d4)
- [⚫ fold-93a Transformer Architecture Epic](#fold-93a)
- [⚫ fold-42w Neural Network Primitives Epic](#fold-42w)
- [⚫ fold-e0b Tokenization Library Epic](#fold-e0b)
- [⚫ fold-rex Implement BigRational type](#fold-rex)
- [⚫ fold-btf Implement BigInt type and operations](#fold-btf)
- [⚫ fold-16o Implement layout algorithm](#fold-16o)
- [⚫ fold-5k9 Implement document type and primitives](#fold-5k9)
- [⚫ fold-bji Implement interval elementary functions](#fold-bji)
- [⚫ fold-4n8 Implement interval type and basic operations](#fold-4n8)
- [⚫ fold-6d3 Implement evaluation strategies](#fold-6d3)
- [⚫ fold-coy Implement par and pseq primitives](#fold-coy)
- [⚫ fold-b7i Implement parser combinators](#fold-b7i)
- [⚫ fold-dtb Implement core parser type and primitives](#fold-dtb)
- [⚫ fold-xuy Implement derived units and unit algebra](#fold-xuy)
- [⚫ fold-cqf Implement SI base units and dimension types](#fold-cqf)
- [⚫ fold-nlv Units of Measure Library Epic](#fold-nlv)
- [⚫ fold-3fa Implement Differentiable type class for autodiff](#fold-3fa)
- [⚫ fold-7s8 Implement numeric type class tower](#fold-7s8)
- [⚫ fold-2we Implement Foldable/Traversable](#fold-2we)
- [⚫ fold-bcs Implement Functor/Applicative/Monad hierarchy](#fold-bcs)
- [⚫ fold-33n Implement type class system](#fold-33n)
- [⚫ fold-z7y Tooling metadata index + symbol graph](#fold-z7y)
- [⚫ fold-7nh Implement complex number arithmetic](#fold-7nh)
- [⚫ fold-dco Implement entropy calculations](#fold-dco)
- [⚫ fold-tsz Implement basic arithmetic operations](#fold-tsz)
- [⚫ fold-3m5 Design arbitrary precision number representation](#fold-3m5)
- [⚫ fold-uv4 Implement numerical integration methods library](#fold-uv4)
- [⚫ fold-e2n Monte Carlo Methods Implementation](#fold-e2n)
- [⚫ fold-9c6 Probability Distributions Library](#fold-9c6)
- [⚫ fold-go9 Convolution and Correlation Operations](#fold-go9)
- [⚫ fold-dnu Design DFT/FFT Core Algorithms](#fold-dnu)
- [⚫ fold-thv Implement length-indexed vectors and dimension-safe matrices](#fold-thv)
- [⚫ fold-ep7 Implement 2D Physics Integrator](#fold-ep7)
- [⚫ fold-kgm Implement 2D Collision Response](#fold-kgm)
- [⚫ fold-zc3 Implement 2D Collision Detection](#fold-zc3)
- [⚫ fold-221 Implement 2D Force System](#fold-221)
- [⚫ fold-e1l Implement 2D Vector Math Library](#fold-e1l)
- [⚫ fold-43e Create 2D Physics Engine Core Architecture](#fold-43e)
- [⚫ fold-qna Update type inference for dependent types](#fold-qna)
- [⚫ fold-7yh Implement type-level computation](#fold-7yh)
- [⚫ fold-w5k Implement universe hierarchy](#fold-w5k)
- [⚫ fold-dz1 Implement Sigma types (dependent pairs)](#fold-dz1)
- [⚫ fold-rus Implement Pi types (dependent function types)](#fold-rus)
- [⚫ fold-yqo Integrate autodiff with evaluation engine](#fold-yqo)
- [⚫ fold-c3i Implement Jacobian and Hessian computation](#fold-c3i)
- [⚫ fold-m3u Implement reverse mode differentiation (backpropagation)](#fold-m3u)
- [⚫ fold-z8h Implement forward mode differentiation](#fold-z8h)
- [⚫ fold-7z1 Create gradient-aware primitive wrappers](#fold-7z1)
- [⚫ fold-o5o Implement computational graph data structure](#fold-o5o)
- [⚫ fold-uw9 Design differentiable type system extensions](#fold-uw9)
- [⚫ fold-f9a Implement graph Laplacian matrices](#fold-f9a)
- [⚫ fold-10x Implement adjacency matrix representation for graphs](#fold-10x)
- [⚫ fold-81x Implement matrix data structure and basic operations](#fold-81x)
- [⚫ fold-o7b Implement vector data structure and basic operations](#fold-o7b)
- [⚫ fold-l01 Create comprehensive benchmarking suite](#fold-l01)
- [⚫ fold-et1 Optimize queue and stack operations for performance](#fold-et1)
- [⚫ fold-woc Optimize visited set tracking with hash tables](#fold-woc)
- [⚫ fold-xph Create unit tests for centrality and subgraph operations](#fold-xph)
- [⚫ fold-8sh Create unit tests for graph analysis algorithms](#fold-8sh)
- [⚫ fold-np4 Create unit tests for pathfinding algorithms](#fold-np4)
- [⚫ fold-45y Create unit tests for traversal primitives (BFS, DFS)](#fold-45y)
- [⚫ fold-d3o Add performance benchmarks and optimizations to graph algorithms](#fold-d3o)
- [⚫ fold-din Create comprehensive test suite for graph algorithms library](#fold-din)
- [⚫ fold-24s Graph Algorithms Library Epic: Comprehensive Testing, Documentation, and Performance Optimizations](#fold-24s)
- [⚫ fold-2uv Dependent Type System Epic](#fold-2uv)
- [⚫ fold-ay1 Autodiff Engine Epic](#fold-ay1)
- [⚫ fold-ov2 Create core string utilities module](#fold-ov2)
- [⚫ fold-2rj Audit and migrate essential list utilities to core](#fold-2rj)
- [⚫ fold-5mq Migrate foldr from patterns to core prelude](#fold-5mq)
- [⚫ fold-e43 SDK patch policy: naming, versions, provides](#fold-e43)
- [⚫ fold-dre Create comprehensive BoardCraft SDK tutorial series](#fold-dre)
- [⚫ fold-lx9 REPL needs interactive help system for newcomers](#fold-lx9)
- [⚫ fold-0mc Satin compiler: expand, validate, source-map](#fold-0mc)
- [⚫ fold-mpq Satin education forms: exercises, rubrics, hints](#fold-mpq)
- [⚫ fold-4hs Satin core forms: story, node, choice, cond, action](#fold-4hs)
- [⚫ fold-2fn Satin: DSL principles + syntax spec](#fold-2fn)
- [⚫ fold-vzc Quill narrative engine: rules, dialogue, quests, timeline](#fold-vzc)
- [⚫ fold-fjo Quill education layer: exercises + validation + progress](#fold-fjo)
- [⚫ fold-u2c Quill input: intent model + command parser](#fold-u2c)
- [⚫ fold-53s Quill content DSL + validator](#fold-53s)
- [⚫ fold-v98 Quill core runtime: story graph + state + effects](#fold-v98)
- [⚫ fold-l97 Quill: Module layout + public API](#fold-l97)
- [⚫ fold-10r Quill: Text Adventure + Narrative SDK](#fold-10r)
- [⚫ fold-uz5 Fix missing test files in shell test suite](#fold-uz5)
- [⚫ fold-5kr fold-qks](#fold-5kr)
- [⚫ fold-2jf fold-mp2](#fold-2jf)
- [⚫ fold-vva fold-ne4](#fold-vva)
- [⚫ fold-qks Add input validation for MCP server tools](#fold-qks)
- [⚫ fold-mp2 Fix TypeScript build permissions in mcp-server](#fold-mp2)
- [⚫ fold-ne4 Update Node.js dependencies in mcp-server](#fold-ne4)
- [⚫ fold-5kb Fix failing tests in thimble test suite](#fold-5kb)
- [⚫ fold-fig Complete Unicode NFC normalization implementation in thimble/text.ss](#fold-fig)
- [⚫ fold-wgs Rust Core Feature Parity with Chez Scheme](#fold-wgs)
- [⚫ fold-7tfx Fix vec-pure/vec-ap Applicative pattern inconsistency](#fold-7tfx)
- [⚫ fold-tw6k Fix sim-sample for variable-timestep simulations](#fold-tw6k)
- [⚫ fold-hls0 Fix sparse autodiff to avoid dense matrix allocation](#fold-hls0)
- [⚫ fold-ozhf Add negative discriminant check in Wilkinson shift](#fold-ozhf)
- [⚫ fold-48oi Scheduled forum agents not posting to discord](#fold-48oi)
- [⚫ fold-3z8w Reorganize core/ into domain-driven subdirectories](#fold-3z8w)
- [⚫ fold-ccz Review and triage P1 epic backlog](#fold-ccz)
- [⚫ fold-clc TECH DEBT: bench-graph-algorithms.ss uses system() and mkdir in core](#fold-clc)
- [⚫ fold-8n9 TECH DEBT: typed-eval.ss has IO in core](#fold-8n9)
- [⚫ fold-5vf TECH DEBT: debug.ss has IO in core](#fold-5vf)
- [⚫ fold-opq TECH DEBT: numerical/integrators.ss is isolated](#fold-opq)
- [⚫ fold-721 TECH DEBT: physics-2d only self-loads and one test file](#fold-721)
- [⚫ fold-93v Fix Rust formatting and clippy warnings in fold-rs](#fold-93v)
- [⚫ fold-bu4 fold-rs: Macro system (define-syntax or expand to core)](#fold-bu4)
- [⚫ fold-x2g fold-rs: Fix stack overflow in debug builds](#fold-x2g)
- [⚫ fold-xef fold-rs: Port mathematical libraries (matrix, complex, dft)](#fold-xef)
- [⚫ fold-aqr fold-rs: Parser - character literals and quasiquote](#fold-aqr)
- [⚫ fold-40h Fix prelude lowering error in graph-find-path let structure](#fold-40h)
- [⚫ fold-1va Fix remaining 95 prelude test failures in fold-rs](#fold-1va)
- [⚫ fold-5uy Implement remaining Rust daemon forum commands (browse, chat, hi/who/bye)](#fold-5uy)
- [⚫ fold-4wq Revert to Rust daemon as default](#fold-4wq)
- [⚫ fold-mnk Make Scheme daemon the default until Rust daemon is feature-complete](#fold-mnk)
- [⚫ fold-79z fold-rs test coverage gaps for new primitives](#fold-79z)
- [⚫ fold-0it Missing primitives: exp, assoc, display, write, newline](#fold-0it)
- [⚫ fold-fsa Implement rational literal parser support](#fold-fsa)
- [⚫ fold-e7k fold-type should perform actual type inference](#fold-e7k)
- [⚫ fold-0dc Missing thimble/core-playground.ss file](#fold-0dc)
- [⚫ fold-wlc Add BigRational support to fold-rs](#fold-wlc)
- [⚫ fold-597 Integrate BigInt primitives with Rust evaluator](#fold-597)
- [⚫ fold-68p Unify Foldable/Traversable with typeclass dictionaries](#fold-68p)
- [⚫ fold-7dd Upgrade optics to profunctor encoding for better composability](#fold-7dd)
- [⚫ fold-jsc Unify typeclass dictionary representation in FP toolkit](#fold-jsc)
- [⚫ fold-05p Test-driven development workflow](#fold-05p)
- [⚫ fold-wfz Refactoring toolkit](#fold-wfz)
- [⚫ fold-a0m Automatic API documentation generator](#fold-a0m)
- [⚫ fold-7qr Inline performance profiler](#fold-7qr)
- [⚫ fold-za0 Notebook interface (Jupyter-style)](#fold-za0)
- [⚫ fold-k3g Property-based testing framework](#fold-k3g)
- [⚫ fold-tqx Live coding environment with hot reload](#fold-tqx)
- [⚫ fold-07g Step-through debugger with fuel visualization](#fold-07g)
- [⚫ fold-cyy Implement modular DSL composition](#fold-cyy)
- [⚫ fold-1g4 Implement recursion schemes](#fold-1g4)
- [⚫ fold-ift Implement Bifunctor and Profunctor](#fold-ift)
- [⚫ fold-dum Implement Contravariant and Divisible functors](#fold-dum)
- [⚫ fold-6tr Implement Comonad abstraction](#fold-6tr)
- [⚫ fold-jxl Implement GSM8K math benchmark](#fold-jxl)
- [⚫ fold-jtn Implement MMLU benchmark](#fold-jtn)
- [⚫ fold-b3y Implement benchmark task framework](#fold-b3y)
- [⚫ fold-3yk Implement beam search](#fold-3yk)
- [⚫ fold-asv Implement DPO algorithm](#fold-asv)
- [⚫ fold-bz3 Implement PPO algorithm](#fold-bz3)
- [⚫ fold-1az Implement reward model](#fold-1az)
- [⚫ fold-ehu Implement model checkpointing](#fold-ehu)
- [⚫ fold-5c4 Implement gradient clipping](#fold-5c4)
- [⚫ fold-c5r Implement Dropout](#fold-c5r)
- [⚫ fold-dgs Implement regex pre-tokenization](#fold-dgs)
- [⚫ fold-9s3 Implement vocabulary and merges file I/O](#fold-9s3)
- [⚫ fold-xel Distributed Training Epic](#fold-xel)
- [⚫ fold-mx2 LLM Evaluation and Benchmarks Epic](#fold-mx2)
- [⚫ fold-39j Reinforcement Learning for LLMs Epic](#fold-39j)
- [⚫ fold-01u Implement statechart DSL](#fold-01u)
- [⚫ fold-9ne Implement regex to automata](#fold-9ne)
- [⚫ fold-zck Implement automata operations](#fold-zck)
- [⚫ fold-b0x Implement finite automata](#fold-b0x)
- [⚫ fold-dgu Implement expression parser with precedence](#fold-dgu)
- [⚫ fold-pmg Implement error handling and position tracking](#fold-pmg)
- [⚫ fold-a4i Implement quantity operations and conversions](#fold-a4i)
- [⚫ fold-7s7 Law-checker for FP abstractions](#fold-7s7)
- [⚫ fold-yak Implement tensor Functor for neural network layers](#fold-yak)
- [⚫ fold-41r Implement Random effect for simulations](#fold-41r)
- [⚫ fold-hwu Implement Graph Traversable instance](#fold-hwu)
- [⚫ fold-cgu Implement simulation stream abstraction](#fold-cgu)
- [⚫ fold-a40 Implement Probability monad](#fold-a40)
- [⚫ fold-eoo Implement Num type class instances for Vec and Matrix](#fold-eoo)
- [⚫ fold-lwl Implement Functor/Foldable instances for Vec and Matrix](#fold-lwl)
- [⚫ fold-sim Implement arrows and profunctors](#fold-sim)
- [⚫ fold-np5 Implement monad transformers](#fold-np5)
- [⚫ fold-rho Implement free monad and interpreters](#fold-rho)
- [⚫ fold-6rr Implement lazy streams and codata](#fold-6rr)
- [⚫ fold-0ak Implement algebraic effect system](#fold-0ak)
- [⚫ fold-1yg Implement lens and optics library](#fold-1yg)
- [⚫ fold-ge6 Lens navigation + codebase explorer](#fold-ge6)
- [⚫ fold-7dx Benchmark harness + regression tracking](#fold-7dx)
- [⚫ fold-8ap Implement normal form games](#fold-8ap)
- [⚫ fold-3lh Implement discrete dynamical systems](#fold-3lh)
- [⚫ fold-07e Property-based testing + shrinking](#fold-07e)
- [⚫ fold-q7w Cost + performance profiler](#fold-q7w)
- [⚫ fold-7lw Implement state space models](#fold-7lw)
- [⚫ fold-e78 Implement symbolic expression representation](#fold-e78)
- [⚫ fold-5dg Implement category and functor foundations](#fold-5dg)
- [⚫ fold-637 Implement N-dimensional tensor operations](#fold-637)
- [⚫ fold-zb9 Implement graph neural network primitives](#fold-zb9)
- [⚫ fold-5ny Implement probabilistic machine learning utilities](#fold-5ny)
- [⚫ fold-j8q Add transcendental functions](#fold-j8q)
- [⚫ fold-ek7 Implement random number generation and distributions](#fold-ek7)
- [⚫ fold-0qs Create dependent types documentation and examples](#fold-0qs)
- [⚫ fold-14l Create comprehensive test suite for dependent types](#fold-14l)
- [⚫ fold-f6d Update kind system for dependent kinds](#fold-f6d)
- [⚫ fold-6d6 Implement higher-order automatic differentiation](#fold-6d6)
- [⚫ fold-sxc Implement sparse matrix support for large-scale autodiff](#fold-sxc)
- [⚫ fold-1ws Implement sparse matrix support](#fold-1ws)
- [⚫ fold-l0p Implement special matrices and utilities](#fold-l0p)
- [⚫ fold-5k3 Implement eigenvalue/eigenvector computation](#fold-5k3)
- [⚫ fold-6nq Implement matrix decompositions (LU, QR)](#fold-6nq)
- [⚫ fold-vy8 Implement linear equation solvers](#fold-vy8)
- [⚫ fold-i5d Graph Algorithms Library Epic](#fold-i5d)
- [⚫ fold-e5a Update tests for migrated core functions](#fold-e5a)
- [⚫ fold-zpn Standardize module loading for core utilities](#fold-zpn)
- [⚫ fold-dl6 Implement Aho-Corasick multi-pattern string matching](#fold-dl6)
- [⚫ fold-xha Tests for SDK patch registration](#fold-xha)
- [⚫ fold-zz4 Patch UX for SDKs: discoverability + quickload](#fold-zz4)
- [⚫ fold-820 Register Satin DSL patch manifest](#fold-820)
- [⚫ fold-2yb Register Quill SDK patch manifest](#fold-2yb)
- [⚫ fold-6o3 Register Loom SDK patch manifest](#fold-6o3)
- [⚫ fold-n8t Patch Registry: SDK Manifests](#fold-n8t)
- [⚫ fold-oxa Implement set and dictionary helper functions](#fold-oxa)
- [⚫ fold-57p Implement persistent queue and stack data structures](#fold-57p)
- [⚫ fold-1dp BoardCraft SDK needs game template generator](#fold-1dp)
- [⚫ fold-lj7 Forum posting mechanism needs clear documentation and examples](#fold-lj7)
- [⚫ fold-84b Error messages need context and suggestions for fixes](#fold-84b)
- [⚫ fold-7vk Function signatures inconsistent between examples and implementation](#fold-7vk)
- [⚫ fold-6fc Satin tests: expansion, compilation, validation](#fold-6fc)
- [⚫ fold-ymn Satin examples: lesson pack + narrative showcase](#fold-ymn)
- [⚫ fold-6o8 Satin tooling: lint, pretty, docs-as-data](#fold-6o8)
- [⚫ fold-udk Satin modules + libraries: import, stdlib](#fold-udk)
- [⚫ fold-vjy Satin narrative forms: dialogue, quests, rules, timeline](#fold-vjy)
- [⚫ fold-buk Satin: Quill Authoring DSL](#fold-buk)
- [⚫ fold-pmw Forum posting mechanism unclear for new users](#fold-pmw)
- [⚫ fold-kez REPL documentation unclear - basic functions not available](#fold-kez)
- [⚫ fold-q81 Quill tests: runtime, DSL, persistence](#fold-q81)
- [⚫ fold-gyk Quill demos: educational module + complex story](#fold-gyk)
- [⚫ fold-ant Quill REPL integration: run loop + commands](#fold-ant)
- [⚫ fold-v5h Quill authoring tools: debugger, inspector, lint](#fold-v5h)
- [⚫ fold-dtr Quill persistence: save/load, checkpoints, undo](#fold-dtr)
- [⚫ fold-02a Quill rendering: text templating + transcript](#fold-02a)
- [⚫ fold-z6u the-fold-xgk (P2): Complete system exploration and issue identification](#fold-z6u)
- [⚫ fold-3dv Fix color module loading issue](#fold-3dv)
- [⚫ fold-ayl Fix command system exceptions](#fold-ayl)
- [⚫ fold-c4x Fix string-split function signature or documentation](#fold-c4x)
- [⚫ fold-43y Fix prelude function bugs](#fold-43y)
- [⚫ fold-dj6 fold-iw7](#fold-dj6)
- [⚫ fold-40m fold-zg9](#fold-40m)
- [⚫ fold-b8p fold-t1x](#fold-b8p)
- [⚫ fold-73g fold-snd](#fold-73g)
- [⚫ fold-q5f fold-ehs](#fold-q5f)
- [⚫ fold-tgl fold-z7n](#fold-tgl)
- [⚫ fold-zg9 Add error recovery for daemon connection failures](#fold-zg9)
- [⚫ fold-ehs Implement duckie persistence functionality](#fold-ehs)
- [⚫ fold-iw7 Add comprehensive test coverage for scaffold system](#fold-iw7)
- [⚫ fold-t1x Add error handling for malformed sexp files](#fold-t1x)
- [⚫ fold-snd Fix hardcoded paths in documentation and code](#fold-snd)
- [⚫ fold-z7n Implement TODO items in scaffold.ss template](#fold-z7n)
- [⚫ fold-9qv Remove third-party dependencies from thimble/mcp-server](#fold-9qv)
- [⚫ fold-7e1 Port REPL daemon to Rust](#fold-7e1)
- [⚫ fold-dff Shell cutover - update daemon.sh and fold.sh for Rust](#fold-dff)
- [⚫ fold-dh7 Cross-validation suite - verify Rust vs Chez parity](#fold-dh7)
- [⚫ fold-8hf Implement expansion (de Bruijn to named) in Rust](#fold-8hf)
- [⚫ fold-9i4 Implement normalization (de Bruijn indices) in Rust](#fold-9i4)
- [⚫ fold-ntj Primitive audit - add missing primitives to Rust](#fold-ntj)
- [⚫ fold-sk3l Add circular dependency protection to module loader](#fold-sk3l)
- [⚫ fold-2dtn Handle empty vectors in vec-norm-linf](#fold-2dtn)
- [⚫ fold-3ox Fix character escape sequences in core/help.ss examples](#fold-3ox)
- [⚫ fold-mmv Archive sentinel-dsl.ss (v1)](#fold-mmv)
- [⚫ fold-o6v fold-rs: Dependent types (dep-types.ss)](#fold-o6v)
- [⚫ fold-ljw fold-rs: Pretty printer](#fold-ljw)
- [⚫ fold-yh9 fold-rs: Interactive REPL mode](#fold-yh9)
- [⚫ fold-53h Missing numeric interpolation utilities in fold-rs](#fold-53h)
- [⚫ fold-prc Improve FP module test coverage (50% → 75%)](#fold-prc)
- [⚫ fold-4di Standardize FP toolkit naming conventions](#fold-4di)
- [⚫ fold-nay Add NonEmpty list type with Semigroup/Foldable1 instances](#fold-nay)
- [⚫ fold-h1z Add Representable and Distributive functors](#fold-h1z)
- [⚫ fold-i79 Add missing FP utilities: liftA4+, asum, guard, coerce helpers](#fold-i79)
- [⚫ fold-42v Add Selective applicative to FP toolkit](#fold-42v)
- [⚫ fold-vvn Add missing FP typeclasses: Semigroupoid, Apply, Bind, MonadFail](#fold-vvn)
- [⚫ fold-4bm Implement packrat/memoization for parser combinators](#fold-4bm)
- [⚫ fold-7rc Polyglot REPL with inline visualization](#fold-7rc)
- [⚫ fold-7b4 AI-assisted coding with context from CAS](#fold-7b4)
- [⚫ fold-243 Implement Selective functors](#fold-243)
- [⚫ fold-bdx Implement Representable functors](#fold-bdx)
- [⚫ fold-c1c Auto-doc extractor for REPL help](#fold-c1c)
- [⚫ fold-0wd Module dependency graph + cycle tooling](#fold-0wd)
- [⚫ fold-hhc Implement machine learning utilities library](#fold-hhc)
- [⚫ fold-puc Create neural network examples and tutorials](#fold-puc)
- [⚫ fold-qxq REPL session management and persistence needs improvement](#fold-qxq)
- [⚫ fold-ifi Board size returns 0 for empty boards - unclear if intentional](#fold-ifi)
- [⚫ fold-59g Update start.sh REPL IPC guidance](#fold-59g)
- [⚫ fold-t3x Consolidate duplicate string utils tests](#fold-t3x)
- [⚫ fold-7t3 Investigate: free-vars includes 'prim' as free variable](#fold-7t3)
- [⚫ fold-cjk Rust eval: add else clause to case expression](#fold-cjk)
- [⚫ fold-jde Rust parser: add scientific notation support](#fold-jde)
- [⚫ fold-zgq fold-wdx](#fold-zgq)
- [⚫ fold-3s2 fold-fuy](#fold-3s2)
- [⚫ fold-60h fold-4yd](#fold-60h)
- [⚫ fold-7oc fold-3e8](#fold-7oc)
- [⚫ fold-fuy Add line number tracking to introspect complexity analysis](#fold-fuy)
- [⚫ fold-4yd Implement NFC Unicode support in text.ss](#fold-4yd)
- [⚫ fold-3e8 Implement color serialization in graphics.ss](#fold-3e8)
- [⚫ fold-ri8 Expand thimble introspect suite with coverage analysis](#fold-ri8)
- [⚫ fold-sor Implement proper DUCKIE persistence using block storage](#fold-sor)
- [⚫ fold-39z Fix error message formatting bug - ~s placeholders not substituted](#fold-39z)
- [⚫ fold-f99 Refactor prim.rs: extract match arms into organized modules](#fold-f99)
- [⚫ fold-8j3 QoL: Improve closure display format](#fold-8j3)
- [⚫ fold-xq3 QoL: Add let* (sequential let) to Rust eval](#fold-xq3)
- [⚫ fold-adp Rust parser/eval: add quasiquote support](#fold-adp)
- [⚫ fold-545 Standardize TODO comment format across thimble codebase](#fold-545)

---

## Dependency Graph

```mermaid
graph TD
    classDef open fill:#50FA7B,stroke:#333,color:#000
    classDef inprogress fill:#8BE9FD,stroke:#333,color:#000
    classDef blocked fill:#FF5555,stroke:#333,color:#000
    classDef closed fill:#6272A4,stroke:#333,color:#fff

    fold-00g["fold-00g<br/>Implement gradient storage and retrie..."]
    class fold-00g open
    fold-00t["fold-00t<br/>Interactive tutorial system"]
    class fold-00t open
    fold-018["fold-018<br/>Category Theory Library Epic"]
    class fold-018 open
    fold-01j["fold-01j<br/>Symbolic Computation Library Epic"]
    class fold-01j open
    fold-01l["fold-01l<br/>Implement geometric primitives and tr..."]
    class fold-01l open
    fold-01u["fold-01u<br/>Implement statechart DSL"]
    class fold-01u closed
    fold-02a["fold-02a<br/>Quill rendering: text templating + tr..."]
    class fold-02a closed
    fold-05p["fold-05p<br/>Test-driven development workflow"]
    class fold-05p closed
    fold-07e["fold-07e<br/>Property-based testing + shrinking"]
    class fold-07e closed
    fold-07g["fold-07g<br/>Step-through debugger with fuel visua..."]
    class fold-07g closed
    fold-07j["fold-07j<br/>Add BigInt variant to Value enum"]
    class fold-07j closed
    fold-0a9["fold-0a9<br/>Implement work-stealing scheduler"]
    class fold-0a9 open
    fold-0ak["fold-0ak<br/>Implement algebraic effect system"]
    class fold-0ak closed
    fold-0by["fold-0by<br/>Rust daemon missing forum commands (h..."]
    class fold-0by closed
    fold-0cr["fold-0cr<br/>Document core vs patterns module boun..."]
    class fold-0cr open
    fold-0dc["fold-0dc<br/>Missing thimble/core-playground.ss file"]
    class fold-0dc closed
    fold-0dk["fold-0dk<br/>Implement monads and Kleisli categories"]
    class fold-0dk open
    fold-0fj["fold-0fj<br/>Implement tree zipper"]
    class fold-0fj open
    fold-0it["fold-0it<br/>Missing primitives: exp, assoc, displ..."]
    class fold-0it closed
    fold-0j5["fold-0j5<br/>Implement Pretty type class"]
    class fold-0j5 open
    fold-0ln["fold-0ln<br/>Implement perplexity metric"]
    class fold-0ln closed
    fold-0mc["fold-0mc<br/>Satin compiler: expand, validate, sou..."]
    class fold-0mc closed
    fold-0mu["fold-0mu<br/>Implement mesh generation and refinement"]
    class fold-0mu open
    fold-0mz["fold-0mz<br/>Structured editing (paredit/parinfer)"]
    class fold-0mz open
    fold-0qs["fold-0qs<br/>Create dependent types documentation ..."]
    class fold-0qs closed
    fold-0wd["fold-0wd<br/>Module dependency graph + cycle tooling"]
    class fold-0wd closed
    fold-10r["fold-10r<br/>Quill: Text Adventure + Narrative SDK"]
    class fold-10r closed
    fold-10x["fold-10x<br/>Implement adjacency matrix representa..."]
    class fold-10x closed
    fold-13o["fold-13o<br/>Decide experimental/fp graduation str..."]
    class fold-13o closed
    fold-145["fold-145<br/>Implement Singular Value Decompositio..."]
    class fold-145 open
    fold-14l["fold-14l<br/>Create comprehensive test suite for d..."]
    class fold-14l closed
    fold-16o["fold-16o<br/>Implement layout algorithm"]
    class fold-16o closed
    fold-1af["fold-1af<br/>Implement stability analysis"]
    class fold-1af open
    fold-1az["fold-1az<br/>Implement reward model"]
    class fold-1az closed
    fold-1dp["fold-1dp<br/>BoardCraft SDK needs game template ge..."]
    class fold-1dp closed
    fold-1e0["fold-1e0<br/>Linear Algebra Library Epic"]
    class fold-1e0 open
    fold-1g4["fold-1g4<br/>Implement recursion schemes"]
    class fold-1g4 closed
    fold-1h8["fold-1h8<br/>fold-rs: Implement module/load system"]
    class fold-1h8 closed
    fold-1nb["fold-1nb<br/>Implement discrete control systems"]
    class fold-1nb open
    fold-1ob["fold-1ob<br/>Implement BPE encode/decode"]
    class fold-1ob closed
    fold-1oe["fold-1oe<br/>Implement 3D Vector and Quaternion Math"]
    class fold-1oe open
    fold-1os9["fold-1os9<br/>Fix nested constraint syntax in TC-Co..."]
    class fold-1os9 closed
    fold-1r0["fold-1r0<br/>Implement matrix-based graph distance..."]
    class fold-1r0 open
    fold-1v8["fold-1v8<br/>Character-to-number conversion bug in..."]
    class fold-1v8 closed
    fold-1va["fold-1va<br/>Fix remaining 95 prelude test failure..."]
    class fold-1va closed
    fold-1vz["fold-1vz<br/>Effect + flow inspector"]
    class fold-1vz open
    fold-1ws["fold-1ws<br/>Implement sparse matrix support"]
    class fold-1ws closed
    fold-1y3["fold-1y3<br/>Add graph visualization utilities to ..."]
    class fold-1y3 open
    fold-1yg["fold-1yg<br/>Implement lens and optics library"]
    class fold-1yg closed
    fold-221["fold-221<br/>Implement 2D Force System"]
    class fold-221 closed
    fold-228["fold-228<br/>Undo/redo for REPL with branching his..."]
    class fold-228 open
    fold-23u["fold-23u<br/>Statistical Models and Regression"]
    class fold-23u open
    fold-243["fold-243<br/>Implement Selective functors"]
    class fold-243 closed
    fold-24s["fold-24s<br/>Graph Algorithms Library Epic: Compre..."]
    class fold-24s closed
    fold-294["fold-294<br/>Zipper Data Structures Epic"]
    class fold-294 open
    fold-2d4["fold-2d4<br/>Training Infrastructure Epic"]
    class fold-2d4 closed
    fold-2dtn["fold-2dtn<br/>Handle empty vectors in vec-norm-linf"]
    class fold-2dtn closed
    fold-2e4["fold-2e4<br/>Bug: filter primitive declared but th..."]
    class fold-2e4 closed
    fold-2fn["fold-2fn<br/>Satin: DSL principles + syntax spec"]
    class fold-2fn closed
    fold-2fv["fold-2fv<br/>Implement KV cache"]
    class fold-2fv closed
    fold-2jf["fold-2jf<br/>fold-mp2"]
    class fold-2jf closed
    fold-2rj["fold-2rj<br/>Audit and migrate essential list util..."]
    class fold-2rj closed
    fold-2uv["fold-2uv<br/>Dependent Type System Epic"]
    class fold-2uv closed
    fold-2we["fold-2we<br/>Implement Foldable/Traversable"]
    class fold-2we closed
    fold-2y9["fold-2y9<br/>Language Server Protocol (LSP) implem..."]
    class fold-2y9 open
    fold-2yb["fold-2yb<br/>Register Quill SDK patch manifest"]
    class fold-2yb closed
    fold-33n["fold-33n<br/>Implement type class system"]
    class fold-33n closed
    fold-36m["fold-36m<br/>Implement vector literal parser support"]
    class fold-36m closed
    fold-38f["fold-38f<br/>Create rate-distortion toolkit"]
    class fold-38f open
    fold-39j["fold-39j<br/>Reinforcement Learning for LLMs Epic"]
    class fold-39j closed
    fold-39z["fold-39z<br/>Fix error message formatting bug - ~s..."]
    class fold-39z closed
    fold-3cw["fold-3cw<br/>Implement ANSI terminal output"]
    class fold-3cw open
    fold-3dv["fold-3dv<br/>Fix color module loading issue"]
    class fold-3dv closed
    fold-3e8["fold-3e8<br/>Implement color serialization in grap..."]
    class fold-3e8 closed
    fold-3fa["fold-3fa<br/>Implement Differentiable type class f..."]
    class fold-3fa closed
    fold-3jj["fold-3jj<br/>Fix hardcoded Windows paths in thimbl..."]
    class fold-3jj closed
    fold-3l4["fold-3l4<br/>Implement character literal parser su..."]
    class fold-3l4 closed
    fold-3lh["fold-3lh<br/>Implement discrete dynamical systems"]
    class fold-3lh closed
    fold-3m5["fold-3m5<br/>Design arbitrary precision number rep..."]
    class fold-3m5 closed
    fold-3ox["fold-3ox<br/>Fix character escape sequences in cor..."]
    class fold-3ox closed
    fold-3s2["fold-3s2<br/>fold-fuy"]
    class fold-3s2 closed
    fold-3w7["fold-3w7<br/>Arbitrary Precision Arithmetic Epic"]
    class fold-3w7 open
    fold-3yk["fold-3yk<br/>Implement beam search"]
    class fold-3yk closed
    fold-3z8w["fold-3z8w<br/>Reorganize core/ into domain-driven s..."]
    class fold-3z8w closed
    fold-40h["fold-40h<br/>Fix prelude lowering error in graph-f..."]
    class fold-40h closed
    fold-40m["fold-40m<br/>fold-zg9"]
    class fold-40m closed
    fold-414["fold-414<br/>Implement cross-entropy loss"]
    class fold-414 closed
    fold-41r["fold-41r<br/>Implement Random effect for simulations"]
    class fold-41r closed
    fold-42v["fold-42v<br/>Add Selective applicative to FP toolkit"]
    class fold-42v closed
    fold-42w["fold-42w<br/>Neural Network Primitives Epic"]
    class fold-42w closed
    fold-43e["fold-43e<br/>Create 2D Physics Engine Core Archite..."]
    class fold-43e closed
    fold-43y["fold-43y<br/>Fix prelude function bugs"]
    class fold-43y closed
    fold-45y["fold-45y<br/>Create unit tests for traversal primi..."]
    class fold-45y closed
    fold-46k["fold-46k<br/>fold-rs: Implement type system (types..."]
    class fold-46k closed
    fold-48oi["fold-48oi<br/>Scheduled forum agents not posting to..."]
    class fold-48oi closed
    fold-4as["fold-4as<br/>Totality + termination checker"]
    class fold-4as open
    fold-4az["fold-4az<br/>Implement Delaunay triangulation"]
    class fold-4az open
    fold-4bm["fold-4bm<br/>Implement packrat/memoization for par..."]
    class fold-4bm closed
    fold-4di["fold-4di<br/>Standardize FP toolkit naming convent..."]
    class fold-4di closed
    fold-4et["fold-4et<br/>Implement Transformer block"]
    class fold-4et closed
    fold-4hs["fold-4hs<br/>Satin core forms: story, node, choice..."]
    class fold-4hs closed
    fold-4ll["fold-4ll<br/>Visual block explorer and graph viewer"]
    class fold-4ll open
    fold-4n8["fold-4n8<br/>Implement interval type and basic ope..."]
    class fold-4n8 closed
    fold-4r9["fold-4r9<br/>LLM Inference Engine Epic"]
    class fold-4r9 closed
    fold-4ve["fold-4ve<br/>Implement physics state lenses"]
    class fold-4ve open
    fold-4wb["fold-4wb<br/>Implement positional embeddings"]
    class fold-4wb closed
    fold-4wq["fold-4wq<br/>Revert to Rust daemon as default"]
    class fold-4wq closed
    fold-4wu["fold-4wu<br/>Implement reader extensions and custo..."]
    class fold-4wu open
    fold-4yd["fold-4yd<br/>Implement NFC Unicode support in text.ss"]
    class fold-4yd closed
    fold-4z0["fold-4z0<br/>Implement limits and colimits"]
    class fold-4z0 open
    fold-53h["fold-53h<br/>Missing numeric interpolation utiliti..."]
    class fold-53h closed
    fold-53s["fold-53s<br/>Quill content DSL + validator"]
    class fold-53s closed
    fold-545["fold-545<br/>Standardize TODO comment format acros..."]
    class fold-545 closed
    fold-57p["fold-57p<br/>Implement persistent queue and stack ..."]
    class fold-57p closed
    fold-597["fold-597<br/>Integrate BigInt primitives with Rust..."]
    class fold-597 closed
    fold-59g["fold-59g<br/>Update start.sh REPL IPC guidance"]
    class fold-59g closed
    fold-5c4["fold-5c4<br/>Implement gradient clipping"]
    class fold-5c4 closed
    fold-5cy["fold-5cy<br/>Implement numeric tower integration"]
    class fold-5cy open
    fold-5dg["fold-5dg<br/>Implement category and functor founda..."]
    class fold-5dg closed
    fold-5foh["fold-5foh<br/>Add split operation to Random effect ..."]
    class fold-5foh open
    fold-5i2["fold-5i2<br/>Implement primality testing and gener..."]
    class fold-5i2 open
    fold-5ie["fold-5ie<br/>Add coding theory functions"]
    class fold-5ie open
    fold-5iy["fold-5iy<br/>Implement fast multiplication algorithms"]
    class fold-5iy open
    fold-5k3["fold-5k3<br/>Implement eigenvalue/eigenvector comp..."]
    class fold-5k3 closed
    fold-5k9["fold-5k9<br/>Implement document type and primitives"]
    class fold-5k9 closed
    fold-5kb["fold-5kb<br/>Fix failing tests in thimble test suite"]
    class fold-5kb closed
    fold-5kr["fold-5kr<br/>fold-qks"]
    class fold-5kr closed
    fold-5mb["fold-5mb<br/>Advanced Computational Geometry Epic"]
    class fold-5mb open
    fold-5mq["fold-5mq<br/>Migrate foldr from patterns to core p..."]
    class fold-5mq closed
    fold-5nf["fold-5nf<br/>Implement constraint satisfaction"]
    class fold-5nf open
    fold-5ny["fold-5ny<br/>Implement probabilistic machine learn..."]
    class fold-5ny closed
    fold-5o0["fold-5o0<br/>Integrate units with Vec and Matrix"]
    class fold-5o0 open
    fold-5rn["fold-5rn<br/>Implement ring structures"]
    class fold-5rn open
    fold-5sa["fold-5sa<br/>Implement Higher-Kinded Types (HKTs)"]
    class fold-5sa closed
    fold-5uy["fold-5uy<br/>Implement remaining Rust daemon forum..."]
    class fold-5uy closed
    fold-5vf["fold-5vf<br/>TECH DEBT: debug.ss has IO in core"]
    class fold-5vf closed
    fold-5vq["fold-5vq<br/>Bayesian Inference Engine"]
    class fold-5vq open
    fold-5xd["fold-5xd<br/>String parsing fails on exclamation m..."]
    class fold-5xd closed
    fold-60h["fold-60h<br/>fold-4yd"]
    class fold-60h closed
    fold-61dp["fold-61dp<br/>Document COO duplicate entry behavior"]
    class fold-61dp open
    fold-637["fold-637<br/>Implement N-dimensional tensor operat..."]
    class fold-637 closed
    fold-64f["fold-64f<br/>Implement group operations and proper..."]
    class fold-64f open
    fold-64t["fold-64t<br/>Implement field operations"]
    class fold-64t open
    fold-68p["fold-68p<br/>Unify Foldable/Traversable with typec..."]
    class fold-68p closed
    fold-6cm["fold-6cm<br/>Implement hash function primitives"]
    class fold-6cm open
    fold-6d3["fold-6d3<br/>Implement evaluation strategies"]
    class fold-6d3 closed
    fold-6d6["fold-6d6<br/>Implement higher-order automatic diff..."]
    class fold-6d6 closed
    fold-6fc["fold-6fc<br/>Satin tests: expansion, compilation, ..."]
    class fold-6fc closed
    fold-6l4["fold-6l4<br/>Implement @mention parsing and storage"]
    class fold-6l4 open
    fold-6nq["fold-6nq<br/>Implement matrix decompositions (LU, QR)"]
    class fold-6nq closed
    fold-6o3["fold-6o3<br/>Register Loom SDK patch manifest"]
    class fold-6o3 closed
    fold-6o8["fold-6o8<br/>Satin tooling: lint, pretty, docs-as-..."]
    class fold-6o8 closed
    fold-6q2["fold-6q2<br/>Implement scaled dot-product attention"]
    class fold-6q2 closed
    fold-6rr["fold-6rr<br/>Implement lazy streams and codata"]
    class fold-6rr closed
    fold-6tr["fold-6tr<br/>Implement Comonad abstraction"]
    class fold-6tr closed
    fold-6ys["fold-6ys<br/>Implement special mathematical functions"]
    class fold-6ys open
    fold-6yu["fold-6yu<br/>Abstract Algebra Library Epic"]
    class fold-6yu open
    fold-721["fold-721<br/>TECH DEBT: physics-2d only self-loads..."]
    class fold-721 closed
    fold-73g["fold-73g<br/>fold-snd"]
    class fold-73g closed
    fold-73p["fold-73p<br/>Create Physics Engine Documentation"]
    class fold-73p open
    fold-744["fold-744<br/>Implement partial evaluation"]
    class fold-744 open
    fold-755["fold-755<br/>Implement equality types and proofs"]
    class fold-755 open
    fold-756["fold-756<br/>Implement PageRank algorithm using ei..."]
    class fold-756 open
    fold-75l["fold-75l<br/>Time-travel debugger + explain tracing"]
    class fold-75l open
    fold-79z["fold-79z<br/>fold-rs test coverage gaps for new pr..."]
    class fold-79z closed
    fold-7b4["fold-7b4<br/>AI-assisted coding with context from CAS"]
    class fold-7b4 closed
    fold-7dd["fold-7dd<br/>Upgrade optics to profunctor encoding..."]
    class fold-7dd closed
    fold-7dx["fold-7dx<br/>Benchmark harness + regression tracking"]
    class fold-7dx closed
    fold-7e1["fold-7e1<br/>Port REPL daemon to Rust"]
    class fold-7e1 closed
    fold-7gp["fold-7gp<br/>Topology Library Epic"]
    class fold-7gp open
    fold-7hs["fold-7hs<br/>Implement 3D Collision Detection"]
    class fold-7hs open
    fold-7lw["fold-7lw<br/>Implement state space models"]
    class fold-7lw closed
    fold-7n0["fold-7n0<br/>Implement BPE tokenizer training"]
    class fold-7n0 closed
    fold-7nh["fold-7nh<br/>Implement complex number arithmetic"]
    class fold-7nh closed
    fold-7oc["fold-7oc<br/>fold-3e8"]
    class fold-7oc closed
    fold-7qr["fold-7qr<br/>Inline performance profiler"]
    class fold-7qr closed
    fold-7rc["fold-7rc<br/>Polyglot REPL with inline visualization"]
    class fold-7rc closed
    fold-7rp["fold-7rp<br/>Implement simplicial complex data str..."]
    class fold-7rp open
    fold-7s7["fold-7s7<br/>Law-checker for FP abstractions"]
    class fold-7s7 closed
    fold-7s8["fold-7s8<br/>Implement numeric type class tower"]
    class fold-7s8 closed
    fold-7t3["fold-7t3<br/>Investigate: free-vars includes 'prim..."]
    class fold-7t3 closed
    fold-7tfx["fold-7tfx<br/>Fix vec-pure/vec-ap Applicative patte..."]
    class fold-7tfx closed
    fold-7vk["fold-7vk<br/>Function signatures inconsistent betw..."]
    class fold-7vk closed
    fold-7yh["fold-7yh<br/>Implement type-level computation"]
    class fold-7yh closed
    fold-7z1["fold-7z1<br/>Create gradient-aware primitive wrappers"]
    class fold-7z1 closed
    fold-81b["fold-81b<br/>Integrate autodiff with AST and type ..."]
    class fold-81b open
    fold-81x["fold-81x<br/>Implement matrix data structure and b..."]
    class fold-81x closed
    fold-820["fold-820<br/>Register Satin DSL patch manifest"]
    class fold-820 closed
    fold-84a9["fold-84a9<br/>Fix dimension mismatch in qr-algorith..."]
    class fold-84a9 closed
    fold-84b["fold-84b<br/>Error messages need context and sugge..."]
    class fold-84b closed
    fold-84e["fold-84e<br/>Implement stability analysis"]
    class fold-84e open
    fold-8ap["fold-8ap<br/>Implement normal form games"]
    class fold-8ap closed
    fold-8f1["fold-8f1<br/>Implement cooperative games"]
    class fold-8f1 open
    fold-8hf["fold-8hf<br/>Implement expansion (de Bruijn to nam..."]
    class fold-8hf closed
    fold-8hy["fold-8hy<br/>Create computational graph visualization"]
    class fold-8hy open
    fold-8j3["fold-8j3<br/>QoL: Improve closure display format"]
    class fold-8j3 closed
    fold-8n9["fold-8n9<br/>TECH DEBT: typed-eval.ss has IO in core"]
    class fold-8n9 closed
    fold-8o0["fold-8o0<br/>Refactor graph.ss into semantic submo..."]
    class fold-8o0 open
    fold-8rh["fold-8rh<br/>Implement mechanism design"]
    class fold-8rh open
    fold-8sh["fold-8sh<br/>Create unit tests for graph analysis ..."]
    class fold-8sh closed
    fold-8t7["fold-8t7<br/>Implement coordinate charts and atlases"]
    class fold-8t7 open
    fold-8tes["fold-8tes<br/>Fix stream-cartesian to produce true ..."]
    class fold-8tes closed
    fold-8wy["fold-8wy<br/>Implement refinement types"]
    class fold-8wy open
    fold-935["fold-935<br/>Cookbook: common patterns and recipes"]
    class fold-935 open
    fold-93a["fold-93a<br/>Transformer Architecture Epic"]
    class fold-93a closed
    fold-93g["fold-93g<br/>Parser Combinators Library Epic"]
    class fold-93g open
    fold-93v["fold-93v<br/>Fix Rust formatting and clippy warnin..."]
    class fold-93v closed
    fold-98v["fold-98v<br/>Probabilistic Programming Constructs"]
    class fold-98v open
    fold-9c6["fold-9c6<br/>Probability Distributions Library"]
    class fold-9c6 closed
    fold-9dy["fold-9dy<br/>Implement dependent pattern matching"]
    class fold-9dy open
    fold-9g55["fold-9g55<br/>matrix-symmetric? uses exact equality..."]
    class fold-9g55 open
    fold-9i4["fold-9i4<br/>Implement normalization (de Bruijn in..."]
    class fold-9i4 closed
    fold-9if["fold-9if<br/>Integrate identity system with (hi) l..."]
    class fold-9if open
    fold-9ne["fold-9ne<br/>Implement regex to automata"]
    class fold-9ne closed
    fold-9qv["fold-9qv<br/>Remove third-party dependencies from ..."]
    class fold-9qv closed
    fold-9rp["fold-9rp<br/>Implement indentation-sensitive parsing"]
    class fold-9rp open
    fold-9s3["fold-9s3<br/>Implement vocabulary and merges file I/O"]
    class fold-9s3 closed
    fold-9tl["fold-9tl<br/>Information Theory Toolkit"]
    class fold-9tl open
    fold-9tn["fold-9tn<br/>Interval Arithmetic Library Epic"]
    class fold-9tn open
    fold-9z2["fold-9z2<br/>Implement adjoint functors"]
    class fold-9z2 open
    fold-9zx["fold-9zx<br/>Optimize BigNum implementation in fol..."]
    class fold-9zx closed
    fold-a0m["fold-a0m<br/>Automatic API documentation generator"]
    class fold-a0m closed
    fold-a1v["fold-a1v<br/>Implement list zipper"]
    class fold-a1v open
    fold-a2y["fold-a2y<br/>Implement elliptic curve operations"]
    class fold-a2y open
    fold-a40["fold-a40<br/>Implement Probability monad"]
    class fold-a40 closed
    fold-a4i["fold-a4i<br/>Implement quantity operations and con..."]
    class fold-a4i closed
    fold-a6i["fold-a6i<br/>Add linalg performance benchmarks and..."]
    class fold-a6i open
    fold-a6w["fold-a6w<br/>Add tests for expand.ss (canonical fo..."]
    class fold-a6w open
    fold-a71["fold-a71<br/>Implement geodesic computation"]
    class fold-a71 open
    fold-abw["fold-abw<br/>Create real-world example application..."]
    class fold-abw open
    fold-ac0["fold-ac0<br/>Signal Processing Library Epic"]
    class fold-ac0 open
    fold-adp["fold-adp<br/>Rust parser/eval: add quasiquote support"]
    class fold-adp closed
    fold-ajm["fold-ajm<br/>AST-aware formatter + style profiles"]
    class fold-ajm open
    fold-ant["fold-ant<br/>Quill REPL integration: run loop + co..."]
    class fold-ant closed
    fold-aqr["fold-aqr<br/>fold-rs: Parser - character literals ..."]
    class fold-aqr closed
    fold-aqx["fold-aqx<br/>Implement load primitive in Rust"]
    class fold-aqx closed
    fold-arf["fold-arf<br/>Implement symbolic integration"]
    class fold-arf open
    fold-asv["fold-asv<br/>Implement DPO algorithm"]
    class fold-asv closed
    fold-asz["fold-asz<br/>Differential Geometry Library Epic"]
    class fold-asz open
    fold-ay1["fold-ay1<br/>Autodiff Engine Epic"]
    class fold-ay1 closed
    fold-ayl["fold-ayl<br/>Fix command system exceptions"]
    class fold-ayl closed
    fold-b0x["fold-b0x<br/>Implement finite automata"]
    class fold-b0x closed
    fold-b1d["fold-b1d<br/>Add channel capacity calculations"]
    class fold-b1d open
    fold-b1h["fold-b1h<br/>REPL tab completion and history"]
    class fold-b1h open
    fold-b3y["fold-b3y<br/>Implement benchmark task framework"]
    class fold-b3y closed
    fold-b7i["fold-b7i<br/>Implement parser combinators"]
    class fold-b7i closed
    fold-b7t["fold-b7t<br/>Implement proof tactics and automation"]
    class fold-b7t open
    fold-b8p["fold-b8p<br/>fold-t1x"]
    class fold-b8p closed
    fold-b8v["fold-b8v<br/>Implement termination checking for ty..."]
    class fold-b8v open
    fold-bcs["fold-bcs<br/>Implement Functor/Applicative/Monad h..."]
    class fold-bcs closed
    fold-bdg["fold-bdg<br/>Implement differentiable signal proce..."]
    class fold-bdg open
    fold-bdx["fold-bdx<br/>Implement Representable functors"]
    class fold-bdx closed
    fold-bgq["fold-bgq<br/>Implement sampling strategies"]
    class fold-bgq closed
    fold-bi7["fold-bi7<br/>Create number theory utilities"]
    class fold-bi7 open
    fold-bjg["fold-bjg<br/>Implement integer programming"]
    class fold-bjg open
    fold-bji["fold-bji<br/>Implement interval elementary functions"]
    class fold-bji closed
    fold-bms["fold-bms<br/>Implement eigenvector centrality and ..."]
    class fold-bms open
    fold-btf["fold-btf<br/>Implement BigInt type and operations"]
    class fold-btf closed
    fold-bu4["fold-bu4<br/>fold-rs: Macro system (define-syntax ..."]
    class fold-bu4 closed
    fold-buk["fold-buk<br/>Satin: Quill Authoring DSL"]
    class fold-buk closed
    fold-bwy["fold-bwy<br/>Implement symbolic differentiation"]
    class fold-bwy open
    fold-bym["fold-bym<br/>Autodiff Engine Epic - Complete Imple..."]
    class fold-bym open
    fold-bz3["fold-bz3<br/>Implement PPO algorithm"]
    class fold-bz3 closed
    fold-bz7["fold-bz7<br/>Implement layout combinators"]
    class fold-bz7 open
    fold-c1c["fold-c1c<br/>Auto-doc extractor for REPL help"]
    class fold-c1c closed
    fold-c1i["fold-c1i<br/>Add tests for nbe.ss (normalization b..."]
    class fold-c1i open
    fold-c3a["fold-c3a<br/>Implement linear programming"]
    class fold-c3a open
    fold-c3i["fold-c3i<br/>Implement Jacobian and Hessian comput..."]
    class fold-c3i closed
    fold-c4x["fold-c4x<br/>Fix string-split function signature o..."]
    class fold-c4x closed
    fold-c5r["fold-c5r<br/>Implement Dropout"]
    class fold-c5r closed
    fold-c8x["fold-c8x<br/>TECH DEBT: Orphaned fp modules never ..."]
    class fold-c8x closed
    fold-ccz["fold-ccz<br/>Review and triage P1 epic backlog"]
    class fold-ccz closed
    fold-cfv["fold-cfv<br/>Implement inductive type definitions"]
    class fold-cfv open
    fold-cgu["fold-cgu<br/>Implement simulation stream abstraction"]
    class fold-cgu closed
    fold-cjk["fold-cjk<br/>Rust eval: add else clause to case ex..."]
    class fold-cjk closed
    fold-clc["fold-clc<br/>TECH DEBT: bench-graph-algorithms.ss ..."]
    class fold-clc closed
    fold-cmk["fold-cmk<br/>Implement Digital Filter Library"]
    class fold-cmk open
    fold-cmm["fold-cmm<br/>Implement Tagless Final pattern"]
    class fold-cmm closed
    fold-com["fold-com<br/>Equational reasoning + rewrite assistant"]
    class fold-com open
    fold-coy["fold-coy<br/>Implement par and pseq primitives"]
    class fold-coy closed
    fold-cpt["fold-cpt<br/>Parallel Evaluation Strategies Epic"]
    class fold-cpt open
    fold-cqf["fold-cqf<br/>Implement SI base units and dimension..."]
    class fold-cqf closed
    fold-cvd["fold-cvd<br/>Refactoring engine (rename/extract/in..."]
    class fold-cvd open
    fold-cyp["fold-cyp<br/>Implement controller design"]
    class fold-cyp open
    fold-cyy["fold-cyy<br/>Implement modular DSL composition"]
    class fold-cyy closed
    fold-cz4["fold-cz4<br/>Implement SFT training loop"]
    class fold-cz4 closed
    fold-cz8["fold-cz8<br/>Developer Experience Initiative: Maki..."]
    class fold-cz8 closed
    fold-d3o["fold-d3o<br/>Add performance benchmarks and optimi..."]
    class fold-d3o closed
    fold-d8o["fold-d8o<br/>TECH DEBT: Duplicate State monad impl..."]
    class fold-d8o closed
    fold-dag["fold-dag<br/>Control Theory Library Epic"]
    class fold-dag open
    fold-dck["fold-dck<br/>TECH DEBT: Duplicate Differentiable t..."]
    class fold-dck closed
    fold-dco["fold-dco<br/>Implement entropy calculations"]
    class fold-dco closed
    fold-dff["fold-dff<br/>Shell cutover - update daemon.sh and ..."]
    class fold-dff closed
    fold-dgs["fold-dgs<br/>Implement regex pre-tokenization"]
    class fold-dgs closed
    fold-dgu["fold-dgu<br/>Implement expression parser with prec..."]
    class fold-dgu closed
    fold-dgy["fold-dgy<br/>Implement graph visualization"]
    class fold-dgy open
    fold-dh7["fold-dh7<br/>Cross-validation suite - verify Rust ..."]
    class fold-dh7 closed
    fold-din["fold-din<br/>Create comprehensive test suite for g..."]
    class fold-din closed
    fold-dj6["fold-dj6<br/>fold-iw7"]
    class fold-dj6 closed
    fold-dl6["fold-dl6<br/>Implement Aho-Corasick multi-pattern ..."]
    class fold-dl6 closed
    fold-dls["fold-dls<br/>Implement convex hull algorithms"]
    class fold-dls open
    fold-dnu["fold-dnu<br/>Design DFT/FFT Core Algorithms"]
    class fold-dnu closed
    fold-doz["fold-doz<br/>Implement polynomial algebra"]
    class fold-doz open
    fold-dre["fold-dre<br/>Create comprehensive BoardCraft SDK t..."]
    class fold-dre closed
    fold-dtb["fold-dtb<br/>Implement core parser type and primit..."]
    class fold-dtb closed
    fold-dtr["fold-dtr<br/>Quill persistence: save/load, checkpo..."]
    class fold-dtr closed
    fold-dum["fold-dum<br/>Implement Contravariant and Divisible..."]
    class fold-dum closed
    fold-duy["fold-duy<br/>Implement finite field arithmetic"]
    class fold-duy open
    fold-dw78["fold-dw78<br/>Add high-level serialize/deserialize ..."]
    class fold-dw78 open
    fold-dxg["fold-dxg<br/>Implement multi-stage programming"]
    class fold-dxg open
    fold-dz1["fold-dz1<br/>Implement Sigma types (dependent pairs)"]
    class fold-dz1 closed
    fold-dzr["fold-dzr<br/>Implement chaos detection and analysis"]
    class fold-dzr open
    fold-e0b["fold-e0b<br/>Tokenization Library Epic"]
    class fold-e0b closed
    fold-e1l["fold-e1l<br/>Implement 2D Vector Math Library"]
    class fold-e1l closed
    fold-e2n["fold-e2n<br/>Monte Carlo Methods Implementation"]
    class fold-e2n closed
    fold-e3n["fold-e3n<br/>Implement symbolic equation solving"]
    class fold-e3n open
    fold-e43["fold-e43<br/>SDK patch policy: naming, versions, p..."]
    class fold-e43 closed
    fold-e5a["fold-e5a<br/>Update tests for migrated core functions"]
    class fold-e5a closed
    fold-e78["fold-e78<br/>Implement symbolic expression represe..."]
    class fold-e78 closed
    fold-e7k["fold-e7k<br/>fold-type should perform actual type ..."]
    class fold-e7k closed
    fold-ehs["fold-ehs<br/>Implement duckie persistence function..."]
    class fold-ehs closed
    fold-ehu["fold-ehu<br/>Implement model checkpointing"]
    class fold-ehu closed
    fold-ek7["fold-ek7<br/>Implement random number generation an..."]
    class fold-ek7 closed
    fold-eoo["fold-eoo<br/>Implement Num type class instances fo..."]
    class fold-eoo closed
    fold-ep7["fold-ep7<br/>Implement 2D Physics Integrator"]
    class fold-ep7 closed
    fold-esm9["fold-esm9<br/>Optimize random-weighted-eff to singl..."]
    class fold-esm9 open
    fold-et1["fold-et1<br/>Optimize queue and stack operations f..."]
    class fold-et1 closed
    fold-evy["fold-evy<br/>Implement 3D Collision Response"]
    class fold-evy open
    fold-f2j["fold-f2j<br/>Create Physics Engine Test Suite"]
    class fold-f2j closed
    fold-f5s["fold-f5s<br/>Implement autoregressive generation"]
    class fold-f5s closed
    fold-f6d["fold-f6d<br/>Update kind system for dependent kinds"]
    class fold-f6d closed
    fold-f99["fold-f99<br/>Refactor prim.rs: extract match arms ..."]
    class fold-f99 closed
    fold-f9a["fold-f9a<br/>Implement graph Laplacian matrices"]
    class fold-f9a closed
    fold-fht["fold-fht<br/>Integrate zippers with comonad and lens"]
    class fold-fht open
    fold-fig["fold-fig<br/>Complete Unicode NFC normalization im..."]
    class fold-fig closed
    fold-fj1["fold-fj1<br/>Rewrite playpen/quill/parse.ss using ..."]
    class fold-fj1 open
    fold-fjo["fold-fjo<br/>Quill education layer: exercises + va..."]
    class fold-fjo closed
    fold-fmo["fold-fmo<br/>Implement natural transformations"]
    class fold-fmo open
    fold-fo1["fold-fo1<br/>Implement ODE system representation"]
    class fold-fo1 open
    fold-fsa["fold-fsa<br/>Implement rational literal parser sup..."]
    class fold-fsa closed
    fold-fuy["fold-fuy<br/>Add line number tracking to introspec..."]
    class fold-fuy closed
    fold-fwb["fold-fwb<br/>Implement constraint graph for physic..."]
    class fold-fwb open
    fold-g6n["fold-g6n<br/>Implement differentiable type constru..."]
    class fold-g6n open
    fold-gb3["fold-gb3<br/>Implement associated type families"]
    class fold-gb3 open
    fold-ge6["fold-ge6<br/>Lens navigation + codebase explorer"]
    class fold-ge6 closed
    fold-ggs["fold-ggs<br/>Write extensive documentation and exa..."]
    class fold-ggs open
    fold-gh0["fold-gh0<br/>Implement Voronoi diagrams"]
    class fold-gh0 open
    fold-ghf["fold-ghf<br/>Implement identity pre-seeding for al..."]
    class fold-ghf open
    fold-gj0["fold-gj0<br/>Create statistical measures"]
    class fold-gj0 open
    fold-gj4["fold-gj4<br/>Implement higher-order contract wrapping"]
    class fold-gj4 open
    fold-gjr["fold-gjr<br/>Implement persistent homology and TDA"]
    class fold-gjr open
    fold-go1["fold-go1<br/>Implement contract checking modes"]
    class fold-go1 open
    fold-go9["fold-go9<br/>Convolution and Correlation Operations"]
    class fold-go9 closed
    fold-gup["fold-gup<br/>Implement generic zipper derivation"]
    class fold-gup open
    fold-gyk["fold-gyk<br/>Quill demos: educational module + com..."]
    class fold-gyk closed
    fold-h07["fold-h07<br/>Implement GADTs (Generalized Algebrai..."]
    class fold-h07 open
    fold-h1z["fold-h1z<br/>Add Representable and Distributive fu..."]
    class fold-h1z closed
    fold-h4l["fold-h4l<br/>Implement parallel runtime for par/pseq"]
    class fold-h4l open
    fold-h4x["fold-h4x<br/>Contracts and Verification Epic"]
    class fold-h4x open
    fold-h9p["fold-h9p<br/>Implement physics simulation DSL via ..."]
    class fold-h9p open
    fold-hex["fold-hex<br/>Integrate contracts with refinement t..."]
    class fold-hex open
    fold-hh3["fold-hh3<br/>Implement SVG renderer"]
    class fold-hh3 open
    fold-hhc["fold-hhc<br/>Implement machine learning utilities ..."]
    class fold-hhc closed
    fold-hls0["fold-hls0<br/>Fix sparse autodiff to avoid dense ma..."]
    class fold-hls0 closed
    fold-hnp["fold-hnp<br/>High Precision Math Library"]
    class fold-hnp open
    fold-hoo["fold-hoo<br/>Implement mesh generation"]
    class fold-hoo open
    fold-hrp["fold-hrp<br/>Implement transforms and composition"]
    class fold-hrp open
    fold-hvm["fold-hvm<br/>Design linalg library architecture"]
    class fold-hvm closed
    fold-hwu["fold-hwu<br/>Implement Graph Traversable instance"]
    class fold-hwu closed
    fold-hzv["fold-hzv<br/>Implement Embedding layer"]
    class fold-hzv closed
    fold-i24["fold-i24<br/>Block explorer crashes with bytevecto..."]
    class fold-i24 closed
    fold-i5d["fold-i5d<br/>Graph Algorithms Library Epic"]
    class fold-i5d closed
    fold-i79["fold-i79<br/>Add missing FP utilities: liftA4+, as..."]
    class fold-i79 closed
    fold-ibi["fold-ibi<br/>Implement AdamW optimizer"]
    class fold-ibi closed
    fold-ifi["fold-ifi<br/>Board size returns 0 for empty boards..."]
    class fold-ifi closed
    fold-ift["fold-ift<br/>Implement Bifunctor and Profunctor"]
    class fold-ift closed
    fold-iki["fold-iki<br/>Cryptographic Primitives Library Epic"]
    class fold-iki open
    fold-iw7["fold-iw7<br/>Add comprehensive test coverage for s..."]
    class fold-iw7 closed
    fold-ixe["fold-ixe<br/>Build comprehensive test suite for au..."]
    class fold-ixe open
    fold-j32["fold-j32<br/>Numerical PDE Library Epic"]
    class fold-j32 open
    fold-j8q["fold-j8q<br/>Add transcendental functions"]
    class fold-j8q closed
    fold-jde["fold-jde<br/>Rust parser: add scientific notation ..."]
    class fold-jde closed
    fold-jey["fold-jey<br/>Implement time stepping schemes"]
    class fold-jey open
    fold-jpj["fold-jpj<br/>Build welcome back screen renderer"]
    class fold-jpj open
    fold-jsc["fold-jsc<br/>Unify typeclass dictionary representa..."]
    class fold-jsc closed
    fold-jtn["fold-jtn<br/>Implement MMLU benchmark"]
    class fold-jtn closed
    fold-jx0["fold-jx0<br/>Agent Identity and Continuity System"]
    class fold-jx0 open
    fold-jxl["fold-jxl<br/>Implement GSM8K math benchmark"]
    class fold-jxl closed
    fold-jxx["fold-jxx<br/>Pattern match exhaustiveness + redund..."]
    class fold-jxx open
    fold-jzz["fold-jzz<br/>Implement affine arithmetic"]
    class fold-jzz open
    fold-k1z["fold-k1z<br/>Advanced functional programming tooling"]
    class fold-k1z open
    fold-k3g["fold-k3g<br/>Property-based testing framework"]
    class fold-k3g closed
    fold-k5tt["fold-k5tt<br/>Fix hessian-jet to use exact differen..."]
    class fold-k5tt open
    fold-k61["fold-k61<br/>Type-driven development with holes"]
    class fold-k61 open
    fold-k6m["fold-k6m<br/>Implement pattern matching compilation"]
    class fold-k6m open
    fold-kcb["fold-kcb<br/>Build gradient debugging and inspecti..."]
    class fold-kcb open
    fold-kez["fold-kez<br/>REPL documentation unclear - basic fu..."]
    class fold-kez closed
    fold-kg8["fold-kg8<br/>Implement shape primitives"]
    class fold-kg8 open
    fold-kgm["fold-kgm<br/>Implement 2D Collision Response"]
    class fold-kgm closed
    fold-kiv["fold-kiv<br/>Implement GPT model"]
    class fold-kiv closed
    fold-kjw["fold-kjw<br/>TECH DEBT: data-structures.ss overlap..."]
    class fold-kjw closed
    fold-kmf["fold-kmf<br/>Implement Linear layer"]
    class fold-kmf closed
    fold-kw0["fold-kw0<br/>Implement BigInt primitive operations"]
    class fold-kw0 closed
    fold-kwo["fold-kwo<br/>Implement quasiquotation and syntax t..."]
    class fold-kwo closed
    fold-l01["fold-l01<br/>Create comprehensive benchmarking suite"]
    class fold-l01 closed
    fold-l0p["fold-l0p<br/>Implement special matrices and utilities"]
    class fold-l0p closed
    fold-l7o["fold-l7o<br/>Add matrix operations"]
    class fold-l7o open
    fold-l97["fold-l97<br/>Quill: Module layout + public API"]
    class fold-l97 closed
    fold-lim["fold-lim<br/>Implement optimization algorithms"]
    class fold-lim open
    fold-lj7["fold-lj7<br/>Forum posting mechanism needs clear d..."]
    class fold-lj7 closed
    fold-ljw["fold-ljw<br/>fold-rs: Pretty printer"]
    class fold-ljw closed
    fold-lq9["fold-lq9<br/>Implement interval Newton and root fi..."]
    class fold-lq9 open
    fold-lwl["fold-lwl<br/>Implement Functor/Foldable instances ..."]
    class fold-lwl closed
    fold-lx9["fold-lx9<br/>REPL needs interactive help system fo..."]
    class fold-lx9 closed
    fold-lzr["fold-lzr<br/>Auto-parallelization + fusion hints"]
    class fold-lzr open
    fold-m3u["fold-m3u<br/>Implement reverse mode differentiatio..."]
    class fold-m3u closed
    fold-mho["fold-mho<br/>Implement LayerNorm"]
    class fold-mho closed
    fold-mlz["fold-mlz<br/>Refactor string.ss into semantic subm..."]
    class fold-mlz open
    fold-mmv["fold-mmv<br/>Archive sentinel-dsl.ss (v1)"]
    class fold-mmv closed
    fold-mnk["fold-mnk<br/>Make Scheme daemon the default until ..."]
    class fold-mnk closed
    fold-moa["fold-moa<br/>Implement bifurcation analysis"]
    class fold-moa open
    fold-moe["fold-moe<br/>Typed hole suggestions + synthesis"]
    class fold-moe open
    fold-mp2["fold-mp2<br/>Fix TypeScript build permissions in m..."]
    class fold-mp2 closed
    fold-mpq["fold-mpq<br/>Satin education forms: exercises, rub..."]
    class fold-mpq closed
    fold-mvh["fold-mvh<br/>Implement (help) and (apropos) for pr..."]
    class fold-mvh closed
    fold-mx2["fold-mx2<br/>LLM Evaluation and Benchmarks Epic"]
    class fold-mx2 closed
    fold-n3b["fold-n3b<br/>REPL command palette and fuzzy finder"]
    class fold-n3b open
    fold-n5z["fold-n5z<br/>Implement username lookup and registr..."]
    class fold-n5z open
    fold-n7e["fold-n7e<br/>Implement Rank-N polymorphism"]
    class fold-n7e open
    fold-n8t["fold-n8t<br/>Patch Registry: SDK Manifests"]
    class fold-n8t closed
    fold-n9i["fold-n9i<br/>Implement modular arithmetic"]
    class fold-n9i open
    fold-nay["fold-nay<br/>Add NonEmpty list type with Semigroup..."]
    class fold-nay closed
    fold-ne4["fold-ne4<br/>Update Node.js dependencies in mcp-se..."]
    class fold-ne4 closed
    fold-nfy4["fold-nfy4<br/>Add random-bytes primitive to Random ..."]
    class fold-nfy4 open
    fold-nlv["fold-nlv<br/>Units of Measure Library Epic"]
    class fold-nlv closed
    fold-nmdd["fold-nmdd<br/>Add floating-point tolerance to spars..."]
    class fold-nmdd open
    fold-np4["fold-np4<br/>Create unit tests for pathfinding alg..."]
    class fold-np4 closed
    fold-np5["fold-np5<br/>Implement monad transformers"]
    class fold-np5 closed
    fold-nszd["fold-nszd<br/>laplacian-connected-components relati..."]
    class fold-nszd open
    fold-ntj["fold-ntj<br/>Primitive audit - add missing primiti..."]
    class fold-ntj closed
    fold-o5o["fold-o5o<br/>Implement computational graph data st..."]
    class fold-o5o closed
    fold-o6v["fold-o6v<br/>fold-rs: Dependent types (dep-types.ss)"]
    class fold-o6v closed
    fold-o7b["fold-o7b<br/>Implement vector data structure and b..."]
    class fold-o7b closed
    fold-o9e["fold-o9e<br/>Refactor numeric.ss into semantic sub..."]
    class fold-o9e open
    fold-obt["fold-obt<br/>Implement learning rate schedulers"]
    class fold-obt closed
    fold-ok4["fold-ok4<br/>DSL Infrastructure Epic"]
    class fold-ok4 closed
    fold-opq["fold-opq<br/>TECH DEBT: numerical/integrators.ss i..."]
    class fold-opq closed
    fold-otq["fold-otq<br/>Implement bytevector literal parser s..."]
    class fold-otq closed
    fold-ouh["fold-ouh<br/>Implement type-safe gradient dimensio..."]
    class fold-ouh open
    fold-ov2["fold-ov2<br/>Create core string utilities module"]
    class fold-ov2 closed
    fold-oxa["fold-oxa<br/>Implement set and dictionary helper f..."]
    class fold-oxa closed
    fold-oxy["fold-oxy<br/>Implement homology computation"]
    class fold-oxy open
    fold-ozhf["fold-ozhf<br/>Add negative discriminant check in Wi..."]
    class fold-ozhf closed
    fold-p5d["fold-p5d<br/>Implement interpolation and curve fit..."]
    class fold-p5d open
    fold-p91["fold-p91<br/>TECH DEBT: fp/ directory contains 54K..."]
    class fold-p91 closed
    fold-pb7["fold-pb7<br/>fold-rs: IO primitives (display, read..."]
    class fold-pb7 closed
    fold-pd2["fold-pd2<br/>Optimization Library Epic (Convex and..."]
    class fold-pd2 open
    fold-pfgq["fold-pfgq<br/>Discord bot is echoing user inputs"]
    class fold-pfgq closed
    fold-plg["fold-plg<br/>Complete Rust REPL daemon cutover"]
    class fold-plg closed
    fold-pmg["fold-pmg<br/>Implement error handling and position..."]
    class fold-pmg closed
    fold-pmw["fold-pmw<br/>Forum posting mechanism unclear for n..."]
    class fold-pmw closed
    fold-po9["fold-po9<br/>Design dependent type system architec..."]
    class fold-po9 closed
    fold-prc["fold-prc<br/>Improve FP module test coverage (50% ..."]
    class fold-prc closed
    fold-puc["fold-puc<br/>Create neural network examples and tu..."]
    class fold-puc closed
    fold-q5f["fold-q5f<br/>fold-ehs"]
    class fold-q5f closed
    fold-q7w["fold-q7w<br/>Cost + performance profiler"]
    class fold-q7w closed
    fold-q81["fold-q81<br/>Quill tests: runtime, DSL, persistence"]
    class fold-q81 closed
    fold-qca["fold-qca<br/>FP code templates + pattern generator"]
    class fold-qca open
    fold-qce["fold-qce<br/>Implement differentiable physics simu..."]
    class fold-qce open
    fold-qdz["fold-qdz<br/>Implement finite difference methods"]
    class fold-qdz open
    fold-qk1["fold-qk1<br/>Probabilistic Modeling Library Epic"]
    class fold-qk1 open
    fold-qks["fold-qks<br/>Add input validation for MCP server t..."]
    class fold-qks closed
    fold-qna["fold-qna<br/>Update type inference for dependent t..."]
    class fold-qna closed
    fold-qr2["fold-qr2<br/>Consolidate string utilities to core/..."]
    class fold-qr2 closed
    fold-qv7["fold-qv7<br/>Implement spectral clustering using S..."]
    class fold-qv7 open
    fold-qxq["fold-qxq<br/>REPL session management and persisten..."]
    class fold-qxq closed
    fold-r06r["fold-r06r<br/>QR basis recovery lacks orthogonality..."]
    class fold-r06r open
    fold-r1k["fold-r1k<br/>Implement curvature computations"]
    class fold-r1k open
    fold-rd5["fold-rd5<br/>Implement transfer functions"]
    class fold-rd5 open
    fold-rex["fold-rex<br/>Implement BigRational type"]
    class fold-rex closed
    fold-rfc["fold-rfc<br/>Integrate dependent types with module..."]
    class fold-rfc open
    fold-rho["fold-rho<br/>Implement free monad and interpreters"]
    class fold-rho closed
    fold-ri8["fold-ri8<br/>Expand thimble introspect suite with ..."]
    class fold-ri8 closed
    fold-rm6["fold-rm6<br/>Declarative Graphics Library Epic"]
    class fold-rm6 open
    fold-rus["fold-rus<br/>Implement Pi types (dependent functio..."]
    class fold-rus closed
    fold-sib["fold-sib<br/>Implement modules and linear algebra ..."]
    class fold-sib open
    fold-sim["fold-sim<br/>Implement arrows and profunctors"]
    class fold-sim closed
    fold-sjv["fold-sjv<br/>Track posts per identity"]
    class fold-sjv open
    fold-sk3l["fold-sk3l<br/>Add circular dependency protection to..."]
    class fold-sk3l closed
    fold-snd["fold-snd<br/>Fix hardcoded paths in documentation ..."]
    class fold-snd closed
    fold-som["fold-som<br/>Implement extensive form games"]
    class fold-som open
    fold-sor["fold-sor<br/>Implement proper DUCKIE persistence u..."]
    class fold-sor closed
    fold-sxc["fold-sxc<br/>Implement sparse matrix support for l..."]
    class fold-sxc closed
    fold-t1x["fold-t1x<br/>Add error handling for malformed sexp..."]
    class fold-t1x closed
    fold-t3x["fold-t3x<br/>Consolidate duplicate string utils tests"]
    class fold-t3x closed
    fold-t4o["fold-t4o<br/>Implement multi-head attention"]
    class fold-t4o closed
    fold-tca["fold-tca<br/>Examples gallery with runnable snippets"]
    class fold-tca open
    fold-tg0["fold-tg0<br/>Implement typed graph properties and ..."]
    class fold-tg0 open
    fold-tgl["fold-tgl<br/>fold-z7n"]
    class fold-tgl closed
    fold-thv["fold-thv<br/>Implement length-indexed vectors and ..."]
    class fold-thv closed
    fold-tqx["fold-tqx<br/>Live coding environment with hot reload"]
    class fold-tqx closed
    fold-tsz["fold-tsz<br/>Implement basic arithmetic operations"]
    class fold-tsz closed
    fold-tuq["fold-tuq<br/>Dynamical Systems Library Epic"]
    class fold-tuq open
    fold-tw6k["fold-tw6k<br/>Fix sim-sample for variable-timestep ..."]
    class fold-tw6k closed
    fold-u2c["fold-u2c<br/>Quill input: intent model + command p..."]
    class fold-u2c closed
    fold-u3i["fold-u3i<br/>Implement Semigroup/Monoid/Group alge..."]
    class fold-u3i closed
    fold-u4x["fold-u4x<br/>Design identity record schema and sto..."]
    class fold-u4x open
    fold-u72["fold-u72<br/>Core Functional Programming Abstracti..."]
    class fold-u72 open
    fold-u8r["fold-u8r<br/>State Machines and Automata Epic"]
    class fold-u8r open
    fold-uat["fold-uat<br/>Create linalg documentation and examples"]
    class fold-uat open
    fold-udk["fold-udk<br/>Satin modules + libraries: import, st..."]
    class fold-udk closed
    fold-um3["fold-um3<br/>Dead code + unused binding detection"]
    class fold-um3 open
    fold-uqu["fold-uqu<br/>Implement parallel matrix operations"]
    class fold-uqu open
    fold-uv4["fold-uv4<br/>Implement numerical integration metho..."]
    class fold-uv4 closed
    fold-uw9["fold-uw9<br/>Design differentiable type system ext..."]
    class fold-uw9 closed
    fold-ux0["fold-ux0<br/>Type-driven search helpers"]
    class fold-ux0 open
    fold-ux4["fold-ux4<br/>Effect typing + linting"]
    class fold-ux4 open
    fold-uz5["fold-uz5<br/>Fix missing test files in shell test ..."]
    class fold-uz5 closed
    fold-uzb["fold-uzb<br/>Establish core/ pruning policy"]
    class fold-uzb closed
    fold-v5h["fold-v5h<br/>Quill authoring tools: debugger, insp..."]
    class fold-v5h closed
    fold-v98["fold-v98<br/>Quill core runtime: story graph + sta..."]
    class fold-v98 closed
    fold-vhp["fold-vhp<br/>Pretty Printing Combinators Epic"]
    class fold-vhp open
    fold-vjy["fold-vjy<br/>Satin narrative forms: dialogue, ques..."]
    class fold-vjy closed
    fold-vnn["fold-vnn<br/>Implement algebraic simplification"]
    class fold-vnn open
    fold-vva["fold-vva<br/>fold-ne4"]
    class fold-vva closed
    fold-vvn["fold-vvn<br/>Add missing FP typeclasses: Semigroup..."]
    class fold-vvn closed
    fold-vy8["fold-vy8<br/>Implement linear equation solvers"]
    class fold-vy8 closed
    fold-vzc["fold-vzc<br/>Quill narrative engine: rules, dialog..."]
    class fold-vzc closed
    fold-w41["fold-w41<br/>Implement contract primitives"]
    class fold-w41 open
    fold-w5k["fold-w5k<br/>Implement universe hierarchy"]
    class fold-w5k closed
    fold-waa["fold-waa<br/>Game Theory Library Epic"]
    class fold-waa open
    fold-wcr["fold-wcr<br/>Wavelet Transform Implementation"]
    class fold-wcr open
    fold-wdx["fold-wdx<br/>Add session timeout configuration"]
    class fold-wdx open
    fold-wfz["fold-wfz<br/>Refactoring toolkit"]
    class fold-wfz closed
    fold-wgs["fold-wgs<br/>Rust Core Feature Parity with Chez Sc..."]
    class fold-wgs closed
    fold-wlc["fold-wlc<br/>Add BigRational support to fold-rs"]
    class fold-wlc closed
    fold-woc["fold-woc<br/>Optimize visited set tracking with ha..."]
    class fold-woc closed
    fold-wom["fold-wom<br/>Refactor collection.ss into semantic ..."]
    class fold-wom open
    fold-wqd["fold-wqd<br/>Add source locations to error messages"]
    class fold-wqd closed
    fold-wz94["fold-wz94<br/>Discrete Time Simulation SDK Epic"]
    class fold-wz94 open
    fold-wza["fold-wza<br/>Implement spatial data structures"]
    class fold-wza open
    fold-x0k["fold-x0k<br/>Implement evolutionary game theory"]
    class fold-x0k open
    fold-x0z["fold-x0z<br/>Window Functions and Spectral Analysis"]
    class fold-x0z open
    fold-x2g["fold-x2g<br/>fold-rs: Fix stack overflow in debug ..."]
    class fold-x2g closed
    fold-x9t["fold-x9t<br/>Implement Lie groups and algebras"]
    class fold-x9t open
    fold-xef["fold-xef<br/>fold-rs: Port mathematical libraries ..."]
    class fold-xef closed
    fold-xel["fold-xel<br/>Distributed Training Epic"]
    class fold-xel closed
    fold-xha["fold-xha<br/>Tests for SDK patch registration"]
    class fold-xha closed
    fold-xl6["fold-xl6<br/>Implement global optimization"]
    class fold-xl6 open
    fold-xph["fold-xph<br/>Create unit tests for centrality and ..."]
    class fold-xph closed
    fold-xq3["fold-xq3<br/>QoL: Add let* (sequential let) to Rus..."]
    class fold-xq3 closed
    fold-xst["fold-xst<br/>Implement performance profiling for d..."]
    class fold-xst open
    fold-xte["fold-xte<br/>Implement activation functions"]
    class fold-xte closed
    fold-xuy["fold-xuy<br/>Implement derived units and unit algebra"]
    class fold-xuy closed
    fold-y2f["fold-y2f<br/>Interactive proof sketcher for progra..."]
    class fold-y2f open
    fold-y2p["fold-y2p<br/>Add num-bigint dependency to fold-rs"]
    class fold-y2p closed
    fold-y3h["fold-y3h<br/>Create scientific computing examples ..."]
    class fold-y3h open
    fold-y5y["fold-y5y<br/>Missing advertised functions"]
    class fold-y5y closed
    fold-y6c["fold-y6c<br/>Implement finite element method basics"]
    class fold-y6c open
    fold-y8d["fold-y8d<br/>Implement Alternative and MonadPlus"]
    class fold-y8d closed
    fold-yak["fold-yak<br/>Implement tensor Functor for neural n..."]
    class fold-yak closed
    fold-ygg["fold-ygg<br/>Implement iterative linear solvers"]
    class fold-ygg open
    fold-ygh["fold-ygh<br/>Implement convex optimization"]
    class fold-ygh open
    fold-yh9["fold-yh9<br/>fold-rs: Interactive REPL mode"]
    class fold-yh9 closed
    fold-yka["fold-yka<br/>3D Physics Engine Epic"]
    class fold-yka open
    fold-ymn["fold-ymn<br/>Satin examples: lesson pack + narrati..."]
    class fold-ymn closed
    fold-yp7["fold-yp7<br/>Implement advanced graph algorithms (..."]
    class fold-yp7 open
    fold-ypt["fold-ypt<br/>Implement algebraic effect handlers"]
    class fold-ypt closed
    fold-yql["fold-yql<br/>Implement tangent and cotangent spaces"]
    class fold-yql open
    fold-yqo["fold-yqo<br/>Integrate autodiff with evaluation en..."]
    class fold-yqo closed
    fold-yy5["fold-yy5<br/>2D Physics Engine Epic"]
    class fold-yy5 open
    fold-z5d["fold-z5d<br/>Implement multi-parameter type classes"]
    class fold-z5d closed
    fold-z6u["fold-z6u<br/>the-fold-xgk (P2): Complete system ex..."]
    class fold-z6u closed
    fold-z7n["fold-z7n<br/>Implement TODO items in scaffold.ss t..."]
    class fold-z7n closed
    fold-z7y["fold-z7y<br/>Tooling metadata index + symbol graph"]
    class fold-z7y closed
    fold-z8h["fold-z8h<br/>Implement forward mode differentiation"]
    class fold-z8h closed
    fold-z8q["fold-z8q<br/>Create 3D Physics Engine Core Archite..."]
    class fold-z8q open
    fold-za0["fold-za0<br/>Notebook interface (Jupyter-style)"]
    class fold-za0 closed
    fold-zb9["fold-zb9<br/>Implement graph neural network primit..."]
    class fold-zb9 closed
    fold-zbu["fold-zbu<br/>Implement existential types"]
    class fold-zbu open
    fold-zc3["fold-zc3<br/>Implement 2D Collision Detection"]
    class fold-zc3 closed
    fold-zck["fold-zck<br/>Implement automata operations"]
    class fold-zck closed
    fold-zg9["fold-zg9<br/>Add error recovery for daemon connect..."]
    class fold-zg9 closed
    fold-zgq["fold-zgq<br/>fold-wdx"]
    class fold-zgq closed
    fold-zpn["fold-zpn<br/>Standardize module loading for core u..."]
    class fold-zpn closed
    fold-zxk8["fold-zxk8<br/>Fix incorrect least-squares formula i..."]
    class fold-zxk8 closed
    fold-zz4["fold-zz4<br/>Patch UX for SDKs: discoverability + ..."]
    class fold-zz4 closed

    fold-00g ==> fold-o5o
    fold-018 ==> fold-0dk
    fold-018 ==> fold-2uv
    fold-018 ==> fold-4z0
    fold-018 ==> fold-5dg
    fold-018 ==> fold-9z2
    fold-018 ==> fold-fmo
    fold-018 ==> fold-u72
    fold-01j ==> fold-1e0
    fold-01j ==> fold-1g4
    fold-01j ==> fold-6yu
    fold-01j ==> fold-93g
    fold-01j ==> fold-arf
    fold-01j ==> fold-bwy
    fold-01j ==> fold-bym
    fold-01j ==> fold-e3n
    fold-01j ==> fold-e78
    fold-01j ==> fold-vnn
    fold-01l ==> fold-1oe
    fold-01l ==> fold-81x
    fold-01l ==> fold-o7b
    fold-01u ==> fold-b0x
    fold-02a ==> fold-v98
    fold-05p ==> fold-cz8
    fold-07j ==> fold-y2p
    fold-0a9 ==> fold-6d3
    fold-0ak ==> fold-bcs
    fold-0by ==> fold-5uy
    fold-0cr ==> fold-e5a
    fold-0dk ==> fold-9z2
    fold-0fj ==> fold-a1v
    fold-0j5 ==> fold-16o
    fold-0j5 ==> fold-33n
    fold-0mc ==> fold-4hs
    fold-0mc ==> fold-53s
    fold-0mc ==> fold-v98
    fold-0mu ==> fold-4az
    fold-0qs ==> fold-8wy
    fold-0qs ==> fold-dz1
    fold-0qs ==> fold-rus
    fold-0wd ==> fold-z7y
    fold-10r ==> fold-02a
    fold-10r ==> fold-53s
    fold-10r ==> fold-ant
    fold-10r ==> fold-dtr
    fold-10r ==> fold-fjo
    fold-10r ==> fold-gyk
    fold-10r ==> fold-l97
    fold-10r ==> fold-q81
    fold-10r ==> fold-u2c
    fold-10r ==> fold-v5h
    fold-10r ==> fold-v98
    fold-10r ==> fold-vzc
    fold-10x ==> fold-1ws
    fold-10x ==> fold-81x
    fold-145 ==> fold-6nq
    fold-14l ==> fold-7yh
    fold-14l ==> fold-dz1
    fold-14l ==> fold-rus
    fold-14l ==> fold-w5k
    fold-16o ==> fold-5k9
    fold-1af ==> fold-fo1
    fold-1dp ==> fold-dre
    fold-1e0 ==> fold-81x
    fold-1e0 ==> fold-dnu
    fold-1e0 ==> fold-hvm
    fold-1e0 ==> fold-o7b
    fold-1e0 ==> fold-u72
    fold-1g4 ==> fold-5sa
    fold-1g4 ==> fold-bcs
    fold-1h8 ==> fold-pb7
    fold-1nb ==> fold-7lw
    fold-1ob ==> fold-7n0
    fold-1oe ==> fold-e1l
    fold-1oe ==> fold-o7b
    fold-1r0 ==> fold-10x
    fold-1r0 ==> fold-81x
    fold-1vz ==> fold-ux4
    fold-1vz ==> fold-z7y
    fold-1yg ==> fold-bcs
    fold-1yg ==> fold-ift
    fold-1yg ==> fold-n7e
    fold-221 ==> fold-e1l
    fold-23u ==> fold-81x
    fold-23u ==> fold-9c6
    fold-23u ==> fold-vy8
    fold-243 ==> fold-bcs
    fold-24s ==> fold-1y3
    fold-24s ==> fold-abw
    fold-24s ==> fold-d3o
    fold-24s ==> fold-din
    fold-24s ==> fold-ggs
    fold-24s ==> fold-i5d
    fold-24s ==> fold-yp7
    fold-294 ==> fold-a1v
    fold-294 ==> fold-fht
    fold-294 ==> fold-gup
    fold-2d4 ==> fold-414
    fold-2d4 ==> fold-42w
    fold-2d4 ==> fold-5c4
    fold-2d4 ==> fold-7qr
    fold-2d4 ==> fold-ay1
    fold-2d4 ==> fold-bym
    fold-2d4 ==> fold-ehu
    fold-2d4 ==> fold-ibi
    fold-2d4 ==> fold-obt
    fold-2rj ==> fold-5mq
    fold-2uv ==> fold-0qs
    fold-2uv ==> fold-14l
    fold-2uv ==> fold-7yh
    fold-2uv ==> fold-dz1
    fold-2uv ==> fold-po9
    fold-2uv ==> fold-qna
    fold-2uv ==> fold-rus
    fold-2uv ==> fold-w5k
    fold-2we ==> fold-33n
    fold-2we ==> fold-bcs
    fold-2yb ==> fold-e43
    fold-2yb ==> fold-l97
    fold-33n ==> fold-k3g
    fold-38f ==> fold-gj0
    fold-39j ==> fold-1az
    fold-39j ==> fold-2d4
    fold-39j ==> fold-93a
    fold-39j ==> fold-asv
    fold-39j ==> fold-bz3
    fold-39j ==> fold-cz4
    fold-39z ==> fold-84b
    fold-3cw ==> fold-16o
    fold-3fa ==> fold-33n
    fold-3fa ==> fold-ay1
    fold-3w7 ==> fold-5cy
    fold-3w7 ==> fold-5iy
    fold-3w7 ==> fold-btf
    fold-41r ==> fold-0ak
    fold-41r ==> fold-9c6
    fold-42w ==> fold-637
    fold-42w ==> fold-c5r
    fold-42w ==> fold-hvm
    fold-42w ==> fold-hzv
    fold-42w ==> fold-kmf
    fold-42w ==> fold-mho
    fold-42w ==> fold-xte
    fold-43e ==> fold-221
    fold-43e ==> fold-e1l
    fold-43e ==> fold-ep7
    fold-43e ==> fold-kgm
    fold-43e ==> fold-nlv
    fold-43e ==> fold-zc3
    fold-4as ==> fold-z7y
    fold-4az ==> fold-gh0
    fold-4et ==> fold-kmf
    fold-4et ==> fold-mho
    fold-4et ==> fold-t4o
    fold-4hs ==> fold-2fn
    fold-4r9 ==> fold-2fv
    fold-4r9 ==> fold-3yk
    fold-4r9 ==> fold-93a
    fold-4r9 ==> fold-bgq
    fold-4ve ==> fold-1yg
    fold-4ve ==> fold-43e
    fold-4ve ==> fold-bym
    fold-4wq ==> fold-0by
    fold-4yd -.-> fold-fig
    fold-4z0 ==> fold-5dg
    fold-53s ==> fold-v98
    fold-5cy ==> fold-7s8
    fold-5cy ==> fold-btf
    fold-5cy ==> fold-rex
    fold-5i2 ==> fold-n9i
    fold-5ie ==> fold-dco
    fold-5iy ==> fold-btf
    fold-5kb ==> fold-3jj
    fold-5kb -.-> fold-uz5
    fold-5mb ==> fold-01l
    fold-5mb ==> fold-4az
    fold-5mb ==> fold-dls
    fold-5mb ==> fold-o7b
    fold-5mb ==> fold-wza
    fold-5ny ==> fold-5vq
    fold-5ny ==> fold-9c6
    fold-5o0 ==> fold-a4i
    fold-5o0 ==> fold-o7b
    fold-5rn ==> fold-64f
    fold-5sa ==> fold-f6d
    fold-5vq ==> fold-9c6
    fold-5vq ==> fold-ek7
    fold-637 ==> fold-81x
    fold-637 ==> fold-o7b
    fold-64t ==> fold-5rn
    fold-6d3 ==> fold-coy
    fold-6d6 ==> fold-c3i
    fold-6fc ==> fold-0mc
    fold-6nq ==> fold-81x
    fold-6o3 ==> fold-e43
    fold-6o8 ==> fold-0mc
    fold-6rr ==> fold-bcs
    fold-6tr ==> fold-bcs
    fold-6ys ==> fold-7nh
    fold-6yu ==> fold-1e0
    fold-6yu ==> fold-5rn
    fold-6yu ==> fold-64f
    fold-6yu ==> fold-64t
    fold-6yu ==> fold-81x
    fold-6yu ==> fold-doz
    fold-6yu ==> fold-u3i
    fold-73p ==> fold-43e
    fold-73p ==> fold-bym
    fold-744 ==> fold-dxg
    fold-755 ==> fold-7yh
    fold-755 ==> fold-rus
    fold-756 ==> fold-10x
    fold-75l ==> fold-1vz
    fold-75l ==> fold-z7y
    fold-7dx ==> fold-q7w
    fold-7e1 ==> fold-dff
    fold-7e1 -.-> fold-wgs
    fold-7gp ==> fold-5mb
    fold-7gp ==> fold-7rp
    fold-7gp ==> fold-gjr
    fold-7gp ==> fold-i5d
    fold-7gp ==> fold-oxy
    fold-7hs ==> fold-01l
    fold-7nh ==> fold-hvm
    fold-7nh ==> fold-j8q
    fold-7s7 ==> fold-07e
    fold-7s8 ==> fold-33n
    fold-7yh ==> fold-po9
    fold-7z1 ==> fold-o5o
    fold-81b ==> fold-uw9
    fold-81x ==> fold-hvm
    fold-820 ==> fold-0mc
    fold-820 ==> fold-2yb
    fold-820 ==> fold-e43
    fold-84b ==> fold-cz8
    fold-84b ==> fold-lx9
    fold-84e ==> fold-rd5
    fold-8hf -.-> fold-wgs
    fold-8hy ==> fold-o5o
    fold-8rh ==> fold-8ap
    fold-8wy ==> fold-7yh
    fold-8wy ==> fold-dz1
    fold-8wy ==> fold-rus
    fold-93a ==> fold-42w
    fold-93a ==> fold-4wb
    fold-93a ==> fold-6q2
    fold-93a ==> fold-kiv
    fold-93a ==> fold-t4o
    fold-93g ==> fold-01u
    fold-93g ==> fold-4bm
    fold-93g ==> fold-9rp
    fold-93g ==> fold-dgu
    fold-93g ==> fold-dtb
    fold-93g ==> fold-fj1
    fold-93g ==> fold-kwo
    fold-93g ==> fold-pmg
    fold-93g ==> fold-u72
    fold-93g ==> fold-y8d
    fold-9c6 ==> fold-6ys
    fold-9c6 ==> fold-ek7
    fold-9dy ==> fold-cfv
    fold-9dy ==> fold-dz1
    fold-9dy ==> fold-rus
    fold-9i4 -.-> fold-wgs
    fold-9if ==> fold-n5z
    fold-9ne ==> fold-zck
    fold-9qv -.-> fold-mp2
    fold-9qv -.-> fold-ne4
    fold-9qv -.-> fold-qks
    fold-9tl ==> fold-38f
    fold-9tl ==> fold-5ie
    fold-9tl ==> fold-b1d
    fold-9tl ==> fold-dco
    fold-9tl ==> fold-gj0
    fold-9tn ==> fold-4n8
    fold-9tn ==> fold-7s8
    fold-9tn ==> fold-jzz
    fold-9tn ==> fold-lq9
    fold-9z2 ==> fold-fmo
    fold-9zx ==> fold-597
    fold-9zx ==> fold-kw0
    fold-9zx ==> fold-wlc
    fold-a2y ==> fold-duy
    fold-a40 ==> fold-9c6
    fold-a40 ==> fold-bcs
    fold-a4i ==> fold-xuy
    fold-a6i ==> fold-145
    fold-a6i ==> fold-1ws
    fold-a6i ==> fold-bym
    fold-a6i ==> fold-ygg
    fold-a71 ==> fold-r1k
    fold-ac0 ==> fold-1e0
    fold-ac0 ==> fold-6tr
    fold-ac0 ==> fold-bdg
    fold-ac0 ==> fold-cmk
    fold-ac0 ==> fold-dnu
    fold-ac0 ==> fold-go9
    fold-ac0 ==> fold-wcr
    fold-ac0 ==> fold-x0z
    fold-ant ==> fold-02a
    fold-ant ==> fold-dtr
    fold-ant ==> fold-u2c
    fold-ant ==> fold-v98
    fold-aqx ==> fold-36m
    fold-aqx ==> fold-3l4
    fold-aqx ==> fold-otq
    fold-arf ==> fold-bwy
    fold-arf ==> fold-e78
    fold-asz ==> fold-1e0
    fold-asz ==> fold-43e
    fold-asz ==> fold-81x
    fold-asz ==> fold-8t7
    fold-asz ==> fold-a71
    fold-asz ==> fold-bym
    fold-asz ==> fold-o7b
    fold-asz ==> fold-r1k
    fold-asz ==> fold-x9t
    fold-asz ==> fold-yql
    fold-b1d ==> fold-dco
    fold-b7i ==> fold-dtb
    fold-b7t ==> fold-755
    fold-b7t ==> fold-8wy
    fold-b8v ==> fold-7yh
    fold-bcs ==> fold-33n
    fold-bcs ==> fold-5sa
    fold-bdx ==> fold-bcs
    fold-bdx ==> fold-gb3
    fold-bi7 ==> fold-tsz
    fold-bjg ==> fold-c3a
    fold-bji ==> fold-4n8
    fold-bms ==> fold-10x
    fold-bms ==> fold-1r0
    fold-buk ==> fold-0mc
    fold-buk ==> fold-10r
    fold-buk ==> fold-2fn
    fold-buk ==> fold-4hs
    fold-buk ==> fold-6fc
    fold-buk ==> fold-6o8
    fold-buk ==> fold-mpq
    fold-buk ==> fold-udk
    fold-buk ==> fold-vjy
    fold-buk ==> fold-ymn
    fold-bwy ==> fold-e78
    fold-bym ==> fold-3fa
    fold-bym ==> fold-5ny
    fold-bym ==> fold-6d6
    fold-bym ==> fold-81x
    fold-bym ==> fold-8hy
    fold-bym ==> fold-bdg
    fold-bym ==> fold-hhc
    fold-bym ==> fold-hvm
    fold-bym ==> fold-ixe
    fold-bym ==> fold-kcb
    fold-bym ==> fold-o5o
    fold-bym ==> fold-o7b
    fold-bym ==> fold-ouh
    fold-bym ==> fold-sxc
    fold-bym ==> fold-xst
    fold-bym ==> fold-y3h
    fold-bym ==> fold-zb9
    fold-bz7 ==> fold-hrp
    fold-c1c ==> fold-z7y
    fold-c3i ==> fold-81x
    fold-c3i ==> fold-m3u
    fold-cfv ==> fold-7yh
    fold-cfv ==> fold-dz1
    fold-cfv ==> fold-rus
    fold-cfv ==> fold-w5k
    fold-cgu ==> fold-43e
    fold-cgu ==> fold-6rr
    fold-cmk ==> fold-go9
    fold-cmk ==> fold-o7b
    fold-cmm ==> fold-5sa
    fold-com ==> fold-z7y
    fold-cpt ==> fold-0a9
    fold-cpt ==> fold-6d3
    fold-cpt ==> fold-coy
    fold-cpt ==> fold-h4l
    fold-cpt ==> fold-uqu
    fold-cvd ==> fold-z7y
    fold-cyp ==> fold-7lw
    fold-cyp ==> fold-84e
    fold-cyy ==> fold-cmm
    fold-cyy ==> fold-rho
    fold-dag ==> fold-1nb
    fold-dag ==> fold-6nq
    fold-dag ==> fold-7lw
    fold-dag ==> fold-81x
    fold-dag ==> fold-84e
    fold-dag ==> fold-ac0
    fold-dag ==> fold-cyp
    fold-dag ==> fold-rd5
    fold-dag ==> fold-sim
    fold-dag ==> fold-tuq
    fold-dag ==> fold-vy8
    fold-dco ==> fold-9c6
    fold-dff ==> fold-dh7
    fold-dff -.-> fold-wgs
    fold-dgu ==> fold-b7i
    fold-dgy ==> fold-bz7
    fold-dgy ==> fold-i5d
    fold-dh7 -.-> fold-wgs
    fold-din ==> fold-45y
    fold-din ==> fold-8sh
    fold-din ==> fold-np4
    fold-din ==> fold-xph
    fold-dnu ==> fold-7nh
    fold-dnu ==> fold-o7b
    fold-doz ==> fold-5rn
    fold-dre ==> fold-7vk
    fold-dre ==> fold-cz8
    fold-dre ==> fold-lx9
    fold-dtr ==> fold-v98
    fold-dum ==> fold-33n
    fold-duy ==> fold-n9i
    fold-dxg ==> fold-kwo
    fold-dz1 ==> fold-po9
    fold-dzr ==> fold-1af
    fold-e0b ==> fold-7n0
    fold-e0b ==> fold-9s3
    fold-e0b ==> fold-dgs
    fold-e1l ==> fold-o7b
    fold-e2n ==> fold-9c6
    fold-e2n ==> fold-ek7
    fold-e3n ==> fold-vnn
    fold-e5a ==> fold-zpn
    fold-ek7 ==> fold-o7b
    fold-eoo ==> fold-7s8
    fold-eoo ==> fold-81x
    fold-eoo ==> fold-o7b
    fold-ep7 ==> fold-e1l
    fold-ep7 ==> fold-o7b
    fold-ep7 ==> fold-uv4
    fold-f2j ==> fold-43e
    fold-f5s ==> fold-2fv
    fold-f5s ==> fold-bgq
    fold-f6d ==> fold-7yh
    fold-f6d ==> fold-w5k
    fold-f9a ==> fold-10x
    fold-fht ==> fold-1yg
    fold-fht ==> fold-6tr
    fold-fht ==> fold-gup
    fold-fjo ==> fold-02a
    fold-fjo ==> fold-53s
    fold-fjo ==> fold-dtr
    fold-fjo ==> fold-u2c
    fold-fjo ==> fold-vzc
    fold-fmo ==> fold-5dg
    fold-fwb ==> fold-43e
    fold-fwb ==> fold-bym
    fold-fwb ==> fold-i5d
    fold-g6n ==> fold-7yh
    fold-g6n ==> fold-dz1
    fold-g6n ==> fold-rus
    fold-g6n ==> fold-thv
    fold-g6n ==> fold-uw9
    fold-gb3 ==> fold-z5d
    fold-ge6 ==> fold-0wd
    fold-ge6 ==> fold-z7y
    fold-ggs ==> fold-10x
    fold-ggs ==> fold-756
    fold-ggs ==> fold-qv7
    fold-gh0 ==> fold-dls
    fold-ghf ==> fold-n5z
    fold-gj0 ==> fold-9c6
    fold-gj0 ==> fold-dco
    fold-gj4 ==> fold-w41
    fold-gjr ==> fold-oxy
    fold-go1 ==> fold-w41
    fold-go9 ==> fold-7nh
    fold-go9 ==> fold-dnu
    fold-go9 ==> fold-o7b
    fold-gup ==> fold-0fj
    fold-gyk ==> fold-ant
    fold-gyk ==> fold-fjo
    fold-gyk ==> fold-v98
    fold-gyk ==> fold-vzc
    fold-h07 ==> fold-po9
    fold-h4l ==> fold-0a9
    fold-h4x ==> fold-go1
    fold-h4x ==> fold-hex
    fold-h4x ==> fold-w41
    fold-h9p ==> fold-43e
    fold-h9p ==> fold-bym
    fold-h9p ==> fold-rho
    fold-hex ==> fold-8wy
    fold-hex ==> fold-go1
    fold-hh3 ==> fold-bz7
    fold-hhc ==> fold-23u
    fold-hhc ==> fold-637
    fold-hhc ==> fold-81x
    fold-hhc ==> fold-9c6
    fold-hhc ==> fold-m3u
    fold-hnp ==> fold-3m5
    fold-hnp ==> fold-bi7
    fold-hnp ==> fold-j8q
    fold-hnp ==> fold-l7o
    fold-hoo ==> fold-01l
    fold-hrp ==> fold-kg8
    fold-hvm ==> fold-mvh
    fold-hwu ==> fold-2we
    fold-hwu ==> fold-i5d
    fold-ift ==> fold-33n
    fold-ift ==> fold-z5d
    fold-iki ==> fold-3w7
    fold-iki ==> fold-6cm
    fold-iki ==> fold-6yu
    fold-iki ==> fold-n9i
    fold-ixe ==> fold-c3i
    fold-j32 ==> fold-1ws
    fold-j32 ==> fold-81x
    fold-j32 ==> fold-hoo
    fold-j32 ==> fold-jey
    fold-j32 ==> fold-qdz
    fold-j32 ==> fold-uv4
    fold-j32 ==> fold-y6c
    fold-j8q ==> fold-tsz
    fold-jpj ==> fold-6l4
    fold-jpj ==> fold-sjv
    fold-jx0 ==> fold-6l4
    fold-jx0 ==> fold-9if
    fold-jx0 ==> fold-sjv
    fold-jx0 ==> fold-u4x
    fold-jxx ==> fold-z7y
    fold-jzz ==> fold-4n8
    fold-k1z ==> fold-07e
    fold-k1z ==> fold-0wd
    fold-k1z ==> fold-1vz
    fold-k1z ==> fold-4as
    fold-k1z ==> fold-75l
    fold-k1z ==> fold-7dx
    fold-k1z ==> fold-7s7
    fold-k1z ==> fold-ajm
    fold-k1z ==> fold-c1c
    fold-k1z ==> fold-com
    fold-k1z ==> fold-cvd
    fold-k1z ==> fold-ge6
    fold-k1z ==> fold-jxx
    fold-k1z ==> fold-lzr
    fold-k1z ==> fold-moe
    fold-k1z ==> fold-q7w
    fold-k1z ==> fold-qca
    fold-k1z ==> fold-u72
    fold-k1z ==> fold-um3
    fold-k1z ==> fold-ux0
    fold-k1z ==> fold-ux4
    fold-k1z ==> fold-y2f
    fold-k1z ==> fold-z7y
    fold-k6m ==> fold-h07
    fold-kcb ==> fold-m3u
    fold-kgm ==> fold-e1l
    fold-kiv ==> fold-4et
    fold-kw0 ==> fold-07j
    fold-kwo ==> fold-wqd
    fold-l0p ==> fold-81x
    fold-l7o ==> fold-81x
    fold-l7o ==> fold-tsz
    fold-lim ==> fold-81x
    fold-lim ==> fold-bym
    fold-lim ==> fold-m3u
    fold-lim ==> fold-vy8
    fold-lj7 ==> fold-lx9
    fold-lq9 ==> fold-bji
    fold-lwl ==> fold-2we
    fold-lwl ==> fold-81x
    fold-lwl ==> fold-bcs
    fold-lwl ==> fold-o7b
    fold-lx9 ==> fold-cz8
    fold-lx9 ==> fold-qxq
    fold-lzr ==> fold-q7w
    fold-m3u ==> fold-z8h
    fold-mnk -.-> fold-0by
    fold-moa ==> fold-1af
    fold-moe ==> fold-ux0
    fold-moe ==> fold-z7y
    fold-mpq ==> fold-4hs
    fold-mpq ==> fold-fjo
    fold-mvh ==> fold-cz8
    fold-mx2 ==> fold-0ln
    fold-mx2 ==> fold-4r9
    fold-mx2 ==> fold-b3y
    fold-mx2 ==> fold-e0b
    fold-mx2 ==> fold-gj0
    fold-mx2 ==> fold-jtn
    fold-mx2 ==> fold-jxl
    fold-n5z ==> fold-u4x
    fold-n7e ==> fold-rus
    fold-n8t ==> fold-2yb
    fold-n8t ==> fold-6o3
    fold-n8t ==> fold-820
    fold-n8t ==> fold-buk
    fold-n8t ==> fold-e43
    fold-n8t ==> fold-xha
    fold-n8t ==> fold-zz4
    fold-nlv ==> fold-2uv
    fold-nlv ==> fold-a4i
    fold-nlv ==> fold-cqf
    fold-np5 ==> fold-bcs
    fold-ntj -.-> fold-wgs
    fold-o5o ==> fold-i5d
    fold-o6v ==> fold-46k
    fold-o7b ==> fold-hvm
    fold-ok4 ==> fold-cmm
    fold-ok4 ==> fold-cyy
    fold-ok4 ==> fold-kwo
    fold-ok4 ==> fold-ypt
    fold-ouh ==> fold-thv
    fold-ouh ==> fold-uw9
    fold-oxy ==> fold-7rp
    fold-oxy ==> fold-81x
    fold-p5d ==> fold-81x
    fold-p5d ==> fold-o7b
    fold-p5d ==> fold-vy8
    fold-pd2 ==> fold-1e0
    fold-pd2 ==> fold-5nf
    fold-pd2 ==> fold-81x
    fold-pd2 ==> fold-bjg
    fold-pd2 ==> fold-bym
    fold-pd2 ==> fold-c3a
    fold-pd2 ==> fold-i5d
    fold-pd2 ==> fold-vy8
    fold-pd2 ==> fold-ygh
    fold-plg ==> fold-4wq
    fold-pmg ==> fold-b7i
    fold-po9 ==> fold-wqd
    fold-puc ==> fold-hhc
    fold-puc ==> fold-lim
    fold-q7w ==> fold-z7y
    fold-q81 ==> fold-02a
    fold-q81 ==> fold-53s
    fold-q81 ==> fold-dtr
    fold-q81 ==> fold-u2c
    fold-q81 ==> fold-v98
    fold-qce ==> fold-43e
    fold-qce ==> fold-bym
    fold-qdz ==> fold-jey
    fold-qk1 ==> fold-1e0
    fold-qk1 ==> fold-23u
    fold-qk1 ==> fold-5vq
    fold-qk1 ==> fold-98v
    fold-qk1 ==> fold-9c6
    fold-qk1 ==> fold-e2n
    fold-qna ==> fold-dz1
    fold-qna ==> fold-rus
    fold-qna ==> fold-w5k
    fold-qv7 ==> fold-9g55
    fold-qv7 ==> fold-f9a
    fold-qv7 ==> fold-nszd
    fold-qv7 ==> fold-r06r
    fold-qxq ==> fold-cz8
    fold-r1k ==> fold-yql
    fold-rd5 ==> fold-7lw
    fold-rex ==> fold-btf
    fold-rfc ==> fold-dz1
    fold-rfc ==> fold-qna
    fold-rfc ==> fold-rus
    fold-rho ==> fold-bcs
    fold-rm6 ==> fold-hrp
    fold-rm6 ==> fold-kg8
    fold-rus ==> fold-po9
    fold-sib ==> fold-5rn
    fold-sim ==> fold-33n
    fold-sim ==> fold-z5d
    fold-sjv ==> fold-n5z
    fold-snd -.-> fold-3jj
    fold-som ==> fold-8ap
    fold-sor ==> fold-qxq
    fold-sxc ==> fold-1ws
    fold-sxc ==> fold-c3i
    fold-t4o ==> fold-6q2
    fold-tg0 ==> fold-755
    fold-tg0 ==> fold-i5d
    fold-tg0 ==> fold-rus
    fold-thv ==> fold-7yh
    fold-thv ==> fold-81x
    fold-thv ==> fold-dz1
    fold-thv ==> fold-o7b
    fold-thv ==> fold-rus
    fold-tsz ==> fold-3m5
    fold-tuq ==> fold-3lh
    fold-tuq ==> fold-fo1
    fold-tuq ==> fold-uv4
    fold-u2c ==> fold-v98
    fold-u3i ==> fold-33n
    fold-u3i ==> fold-k3g
    fold-u72 ==> fold-0ak
    fold-u72 ==> fold-1yg
    fold-u72 ==> fold-2uv
    fold-u72 ==> fold-33n
    fold-u72 ==> fold-41r
    fold-u72 ==> fold-6rr
    fold-u72 ==> fold-a40
    fold-u72 ==> fold-bcs
    fold-u72 ==> fold-cgu
    fold-u72 ==> fold-eoo
    fold-u72 ==> fold-h9p
    fold-u72 ==> fold-hwu
    fold-u72 ==> fold-lwl
    fold-u72 ==> fold-np5
    fold-u72 ==> fold-rho
    fold-u72 ==> fold-sim
    fold-u72 ==> fold-yak
    fold-u8r ==> fold-b0x
    fold-u8r ==> fold-dgs
    fold-uat ==> fold-10x
    fold-uat ==> fold-6nq
    fold-uat ==> fold-756
    fold-uat ==> fold-81x
    fold-uat ==> fold-f9a
    fold-uat ==> fold-o7b
    fold-uat ==> fold-vy8
    fold-udk ==> fold-4hs
    fold-um3 ==> fold-0wd
    fold-um3 ==> fold-z7y
    fold-uqu ==> fold-6d3
    fold-uqu ==> fold-81x
    fold-uv4 ==> fold-o7b
    fold-uw9 ==> fold-o5o
    fold-ux0 ==> fold-z7y
    fold-ux4 ==> fold-z7y
    fold-v5h ==> fold-02a
    fold-v5h ==> fold-53s
    fold-v5h ==> fold-v98
    fold-v98 ==> fold-l97
    fold-vhp ==> fold-0j5
    fold-vhp ==> fold-16o
    fold-vhp ==> fold-3cw
    fold-vhp ==> fold-5k9
    fold-vjy ==> fold-4hs
    fold-vjy ==> fold-vzc
    fold-vnn ==> fold-e78
    fold-vy8 ==> fold-6nq
    fold-vzc ==> fold-53s
    fold-vzc ==> fold-dtr
    fold-vzc ==> fold-u2c
    fold-vzc ==> fold-v98
    fold-w5k ==> fold-po9
    fold-waa ==> fold-8ap
    fold-waa ==> fold-8f1
    fold-waa ==> fold-9c6
    fold-waa ==> fold-pd2
    fold-wcr ==> fold-o7b
    fold-wqd ==> fold-cz8
    fold-wz94 ==> fold-01u
    fold-wz94 ==> fold-1e0
    fold-wz94 ==> fold-1nb
    fold-wz94 ==> fold-3lh
    fold-wz94 ==> fold-41r
    fold-wz94 ==> fold-4ve
    fold-wz94 ==> fold-5vq
    fold-wz94 ==> fold-6rr
    fold-wz94 ==> fold-7dx
    fold-wz94 ==> fold-9tl
    fold-wz94 ==> fold-9tn
    fold-wz94 ==> fold-bym
    fold-wz94 ==> fold-cgu
    fold-wz94 ==> fold-cpt
    fold-wz94 ==> fold-e2n
    fold-wz94 ==> fold-h9p
    fold-wz94 ==> fold-qk1
    fold-wz94 ==> fold-tuq
    fold-wz94 ==> fold-u8r
    fold-wz94 ==> fold-waa
    fold-wz94 ==> fold-ypt
    fold-x0k ==> fold-8ap
    fold-x0z ==> fold-7nh
    fold-x0z ==> fold-dnu
    fold-x0z ==> fold-o7b
    fold-x9t ==> fold-81x
    fold-xef ==> fold-1h8
    fold-xha ==> fold-2yb
    fold-xha ==> fold-6o3
    fold-xha ==> fold-820
    fold-xl6 ==> fold-bym
    fold-xl6 ==> fold-ygh
    fold-xst ==> fold-yqo
    fold-xuy ==> fold-cqf
    fold-y2f ==> fold-com
    fold-y3h ==> fold-6d6
    fold-y6c ==> fold-hoo
    fold-y8d ==> fold-bcs
    fold-yak ==> fold-637
    fold-yak ==> fold-bcs
    fold-ygg ==> fold-81x
    fold-ygg ==> fold-o7b
    fold-ygh ==> fold-bym
    fold-yka ==> fold-bym
    fold-yka ==> fold-yy5
    fold-yka ==> fold-z8q
    fold-ymn ==> fold-0mc
    fold-ymn ==> fold-ant
    fold-yp7 ==> fold-756
    fold-ypt ==> fold-5sa
    fold-yql ==> fold-8t7
    fold-yqo ==> fold-m3u
    fold-yy5 ==> fold-43e
    fold-yy5 ==> fold-bym
    fold-z5d ==> fold-33n
    fold-z8h ==> fold-7z1
    fold-z8h ==> fold-o5o
    fold-z8q ==> fold-1oe
    fold-z8q ==> fold-43e
    fold-z8q ==> fold-73p
    fold-z8q ==> fold-7hs
    fold-z8q ==> fold-bym
    fold-z8q ==> fold-evy
    fold-z8q ==> fold-f2j
    fold-zb9 ==> fold-10x
    fold-zb9 ==> fold-637
    fold-zb9 ==> fold-81x
    fold-zb9 ==> fold-i5d
    fold-zbu ==> fold-dz1
    fold-zc3 ==> fold-01l
    fold-zc3 ==> fold-e1l
    fold-zck ==> fold-b0x
    fold-zpn ==> fold-2rj
    fold-zz4 ==> fold-2yb
    fold-zz4 ==> fold-6o3
    fold-zz4 ==> fold-820
```

---

## 🚀 fold-wz94 Discrete Time Simulation SDK Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2026-01-02 22:36 |
| **Updated** | 2026-01-02 22:36 |

### Description

A comprehensive SDK for discrete time simulations including:

## Core Components
- Event-driven simulation engine with discrete time steps
- State machine and automata integration  
- Stochastic simulation support (Monte Carlo, random effects)
- Deterministic and probabilistic models

## Features
- Time-stepped execution with configurable dt
- Event queues and scheduling
- State snapshots and replay
- Parallel simulation runs
- Statistical analysis of simulation outputs

## Integration Points
- Linear algebra for state vectors and transitions
- Autodiff for sensitivity analysis
- Interval arithmetic for uncertainty bounds
- Lazy streams for memory-efficient long simulations
- Physics abstractions for domain-specific simulations

## Use Cases
- Discrete event simulation (queuing systems, networks)
- Agent-based modeling
- Financial market simulations
- Game theory / multi-agent systems
- Control system simulation

### Dependencies

- ⛔ **blocks**: `fold-3lh`
- ⛔ **blocks**: `fold-1nb`
- ⛔ **blocks**: `fold-cgu`
- ⛔ **blocks**: `fold-41r`
- ⛔ **blocks**: `fold-u8r`
- ⛔ **blocks**: `fold-01u`
- ⛔ **blocks**: `fold-1e0`
- ⛔ **blocks**: `fold-e2n`
- ⛔ **blocks**: `fold-5vq`
- ⛔ **blocks**: `fold-9tn`
- ⛔ **blocks**: `fold-bym`
- ⛔ **blocks**: `fold-cpt`
- ⛔ **blocks**: `fold-6rr`
- ⛔ **blocks**: `fold-tuq`
- ⛔ **blocks**: `fold-qk1`
- ⛔ **blocks**: `fold-waa`
- ⛔ **blocks**: `fold-9tl`
- ⛔ **blocks**: `fold-7dx`
- ⛔ **blocks**: `fold-h9p`
- ⛔ **blocks**: `fold-4ve`
- ⛔ **blocks**: `fold-ypt`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-wz94 -s in_progress

# Add a comment
bd comment fold-wz94 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-wz94 -p 1

# View full details
bd show fold-wz94
```

</details>

---

## 🚀 fold-3w7 Arbitrary Precision Arithmetic Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Unlimited precision integers, rationals, and reals.

Core Types:
1. BigInt - arbitrary precision integers
2. BigRational - exact rational numbers (BigInt/BigInt)
3. BigDecimal - decimal with configurable precision
4. Computable Real - lazy exact real arithmetic

Operations:
- Full arithmetic: +, -, *, /, mod, gcd, lcm
- Exponentiation and roots
- Comparison and ordering
- Bit manipulation for BigInt

Algorithms:
- Karatsuba/Toom-Cook multiplication
- Newton-Raphson division
- Binary GCD
- Primality testing (Miller-Rabin)

Integration with Numeric Tower:
- BigInt as Integral instance
- BigRational as Fractional instance
- Seamless promotion from fixed-precision

Applications:
- Cryptography (RSA, elliptic curves)
- Symbolic computation (exact arithmetic)
- Financial calculations
- Number theory algorithms

Location: fabric/stitches/bignum/

### Dependencies

- ⛔ **blocks**: `fold-btf`
- ⛔ **blocks**: `fold-5cy`
- ⛔ **blocks**: `fold-5iy`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-3w7 -s in_progress

# Add a comment
bd comment fold-3w7 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-3w7 -p 1

# View full details
bd show fold-3w7
```

</details>

---

## 🚀 fold-vhp Pretty Printing Combinators Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Wadler-Lindig style pretty printing for optimal layout.

Core Combinators:
1. Primitives: text, line, linebreak, softline, hardline
2. Grouping: group, nest, indent, hang, align
3. Concatenation: <>, <+>, hsep, vsep, sep, cat
4. Filling: fill, fillBreak, fillSep, fillCat
5. Bracketing: parens, braces, brackets, angles, quotes

Rendering:
- Width-aware layout algorithm
- Ribbon width control
- ANSI color/style support
- Multiple output formats (plain, terminal, HTML)

Show Type Class Integration:
- Default pretty instances for all types
- Precedence-aware expression printing
- Customizable via type class

Applications:
- REPL output formatting
- Error message display
- Code generation
- Documentation rendering

Location: fabric/stitches/pretty/

### Dependencies

- ⛔ **blocks**: `fold-5k9`
- ⛔ **blocks**: `fold-16o`
- ⛔ **blocks**: `fold-3cw`
- ⛔ **blocks**: `fold-0j5`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-vhp -s in_progress

# Add a comment
bd comment fold-vhp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-vhp -p 1

# View full details
bd show fold-vhp
```

</details>

---

## 🚀 fold-9tn Interval Arithmetic Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Verified numerical computation with rigorous error bounds.

Core Types:
1. Interval [lo, hi] - guaranteed to contain true value
2. Affine forms - tighter bounds via correlation tracking
3. Taylor models - polynomial + interval remainder

Operations:
- Arithmetic: +, -, *, /, sqrt, exp, log, sin, cos
- Comparisons: definitely<, possibly<, overlaps
- Set operations: hull, intersection, bisect
- Width/midpoint queries

Features:
- Automatic differentiation with interval bounds
- Root finding with guaranteed enclosure
- Integration with verified error bounds
- Constraint propagation for interval CSP

Applications:
- Verified ODE solvers
- Robust geometric predicates
- Global optimization (branch and bound)
- Computer-aided proofs

Location: fabric/stitches/interval/

### Dependencies

- ⛔ **blocks**: `fold-7s8`
- ⛔ **blocks**: `fold-4n8`
- ⛔ **blocks**: `fold-lq9`
- ⛔ **blocks**: `fold-jzz`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-9tn -s in_progress

# Add a comment
bd comment fold-9tn 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-9tn -p 1

# View full details
bd show fold-9tn
```

</details>

---

## 🚀 fold-cpt Parallel Evaluation Strategies Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Controlled parallelism with explicit evaluation strategies.

Core Primitives:
1. par/pseq - parallel and sequential evaluation hints
2. Strategies: rpar, rseq, rdeepseq, parList, parMap
3. Chunking: parListChunk, parBuffer
4. Speculation: speculative evaluation with cancellation

Parallel Patterns:
- Parallel map over vectors/matrices
- Divide-and-conquer (parallel merge sort, FFT)
- Pipeline parallelism for streaming
- Work-stealing for load balancing

Integration:
- Matrix multiplication: parallel blocked algorithm
- Physics: parallel force computation
- Graph: parallel BFS/DFS
- Autodiff: parallel gradient computation

Safety:
- Pure functions only (no side effects in parallel)
- Deterministic results
- Fuel-aware scheduling

Location: fabric/stitches/parallel/

### Dependencies

- ⛔ **blocks**: `fold-coy`
- ⛔ **blocks**: `fold-6d3`
- ⛔ **blocks**: `fold-uqu`
- ⛔ **blocks**: `fold-0a9`
- ⛔ **blocks**: `fold-h4l`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-cpt -s in_progress

# Add a comment
bd comment fold-cpt 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-cpt -p 1

# View full details
bd show fold-cpt
```

</details>

---

## 🚀 fold-93g Parser Combinators Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Monadic parser combinator library for building DSLs and text processing.

Core Combinators:
1. Primitives: char, string, satisfy, eof, any
2. Sequencing: >>=, >>, <*, *>, between
3. Alternation: <|>, choice, try, optional
4. Repetition: many, some, sepBy, endBy, count
5. Lookahead: lookAhead, notFollowedBy
6. Error handling: label, <?>, withError

Advanced Features:
- Indentation-sensitive parsing (for Scheme/Python-like syntax)
- Left-recursion elimination
- Packrat/memoization option
- Source position tracking
- Custom error messages

Applications:
- Loom spell DSL parsing
- Physics simulation DSL
- Mathematical expression parsing
- Configuration file formats

Location: fabric/stitches/parse/

### Dependencies

- ⛔ **blocks**: `fold-u72`
- ⛔ **blocks**: `fold-y8d`
- ⛔ **blocks**: `fold-kwo`
- ⛔ **blocks**: `fold-dtb`
- ⛔ **blocks**: `fold-dgu`
- ⛔ **blocks**: `fold-pmg`
- ⛔ **blocks**: `fold-01u`
- ⛔ **blocks**: `fold-fj1`
- ⛔ **blocks**: `fold-9rp`
- ⛔ **blocks**: `fold-4bm`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-93g -s in_progress

# Add a comment
bd comment fold-93g 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-93g -p 1

# View full details
bd show fold-93g
```

</details>

---

## 🚀 fold-u72 Core Functional Programming Abstractions Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:33 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement foundational FP abstractions that enable elegant, composable code across all libraries. These patterns are essential for the math/physics libraries to achieve their full potential.

Core Patterns:
1. Type Classes - Ad-hoc polymorphism (Eq, Ord, Show, Num, Functor, Monad)
2. Functor/Applicative/Monad hierarchy - Generic container operations
3. Foldable/Traversable - Generic iteration patterns
4. Lenses and Optics - Functional references for nested data
5. Effect System - Structured effects beyond capabilities
6. Lazy Evaluation/Streams - Infinite data structures
7. Numeric Tower - Nat ⊂ Int ⊂ Rational ⊂ Real ⊂ Complex
8. Free Monads - Interpreter pattern for DSLs

Integration Points:
- Vec/Matrix as Functor (map over elements)
- Probability as Monad (monadic sampling)
- Graph as Traversable (generic traversal)
- Physics state as Lens target
- Simulation as lazy Stream
- Autodiff as Functor over dual numbers

Location: fabric/stitches/fp/

### Dependencies

- ⛔ **blocks**: `fold-2uv`
- ⛔ **blocks**: `fold-33n`
- ⛔ **blocks**: `fold-bcs`
- ⛔ **blocks**: `fold-lwl`
- ⛔ **blocks**: `fold-a40`
- ⛔ **blocks**: `fold-sim`
- ⛔ **blocks**: `fold-1yg`
- ⛔ **blocks**: `fold-6rr`
- ⛔ **blocks**: `fold-cgu`
- ⛔ **blocks**: `fold-yak`
- ⛔ **blocks**: `fold-0ak`
- ⛔ **blocks**: `fold-eoo`
- ⛔ **blocks**: `fold-rho`
- ⛔ **blocks**: `fold-np5`
- ⛔ **blocks**: `fold-h9p`
- ⛔ **blocks**: `fold-hwu`
- ⛔ **blocks**: `fold-41r`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-u72 -s in_progress

# Add a comment
bd comment fold-u72 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-u72 -p 1

# View full details
bd show fold-u72
```

</details>

---

## ✨ fold-01l Implement geometric primitives and transformations

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:08 |
| **Updated** | 2026-01-02 02:46 |

### Description

Core geometry library for graphics and physics.

Features:
1. Geometric Primitives
   - Points, lines, rays
   - Planes, triangles
   - Circles, spheres
   - Bounding boxes (AABB, OBB)
   - Polygons, polyhedra

2. Transformations
   - Translation, rotation, scaling
   - Transformation matrices (3x3, 4x4)
   - Quaternion rotations
   - Affine and projective transforms

3. Geometric Operations
   - Distance calculations
   - Intersection tests
   - Containment tests
   - Closest point queries
   - Area/volume calculations

4. Utilities
   - Coordinate system conversions
   - Normal calculations
   - Barycentric coordinates

Applications:
- Physics collision detection
- Graphics rendering
- CAD/3D modeling
- Game development

Location: fabric/stitches/geometry.ss

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-1oe`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-01l -s in_progress

# Add a comment
bd comment fold-01l 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-01l -p 1

# View full details
bd show fold-01l
```

</details>

---

## 🚀 fold-9tl Information Theory Toolkit

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:01 |
| **Updated** | 2026-01-02 02:46 |

### Description

Epic for implementing a complete information theory toolkit including entropy calculations, coding theory, compression algorithms, and statistical measures

### Dependencies

- ⛔ **blocks**: `fold-dco`
- ⛔ **blocks**: `fold-gj0`
- ⛔ **blocks**: `fold-b1d`
- ⛔ **blocks**: `fold-5ie`
- ⛔ **blocks**: `fold-38f`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-9tl -s in_progress

# Add a comment
bd comment fold-9tl 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-9tl -p 1

# View full details
bd show fold-9tl
```

</details>

---

## 🚀 fold-hnp High Precision Math Library

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:01 |
| **Updated** | 2026-01-02 02:46 |

### Description

Epic for implementing a comprehensive high precision mathematics library with arbitrary precision arithmetic, special functions, and mathematical utilities

### Dependencies

- ⛔ **blocks**: `fold-3m5`
- ⛔ **blocks**: `fold-bi7`
- ⛔ **blocks**: `fold-j8q`
- ⛔ **blocks**: `fold-l7o`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-hnp -s in_progress

# Add a comment
bd comment fold-hnp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-hnp -p 1

# View full details
bd show fold-hnp
```

</details>

---

## 📋 fold-5vq Bayesian Inference Engine

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:56 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement Bayesian inference algorithms including conjugate priors, MCMC samplers (Metropolis-Hastings, Gibbs sampling), variational inference, and Bayesian model selection tools.

### Dependencies

- ⛔ **blocks**: `fold-9c6`
- ⛔ **blocks**: `fold-ek7`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5vq -s in_progress

# Add a comment
bd comment fold-5vq 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5vq -p 1

# View full details
bd show fold-5vq
```

</details>

---

## 📋 fold-cmk Implement Digital Filter Library

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:55 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create digital filter implementations including FIR/IIR filters, Butterworth, Chebyshev, and elliptic filters. Include filter design tools, frequency response analysis, and real-time filtering capabilities.

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-go9`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-cmk -s in_progress

# Add a comment
bd comment fold-cmk 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-cmk -p 1

# View full details
bd show fold-cmk
```

</details>

---

## 📋 fold-00g Implement gradient storage and retrieval system

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:41 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create a comprehensive system for storing, accessing, and managing gradients using the existing block storage. This includes:

- Gradient storage schema integrated with block system
- Efficient gradient serialization/deserialization
- Gradient versioning and checkpoint management
- Memory-mapped gradient access for large models
- Gradient compression and sparse storage
- Integration with content-addressed storage
- API for gradient manipulation and inspection

Location: fabric/stitches/ (extend storage modules)
Essential for persistent gradient management and model checkpointing.

### Dependencies

- ⛔ **blocks**: `fold-o5o`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-00g -s in_progress

# Add a comment
bd comment fold-00g 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-00g -p 1

# View full details
bd show fold-00g
```

</details>

---

## 📋 fold-81b Integrate autodiff with AST and type annotation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:41 |
| **Updated** | 2026-01-02 02:46 |

### Description

Extend the AST processing and type annotation system for automatic differentiation. This includes:

- Modify AST annotation to track differentiable expressions
- Add gradient type annotations to existing type system
- Implement automatic detection of differentiable code paths
- Extend pattern matching for gradient computations
- Add gradient-related syntax transformations
- Integration with existing query DSL for differentiable nodes
- Type checking for gradient computations

Location: fabric/stitches/ (extend annotate and AST modules)
Enables compile-time analysis and optimization of differentiable code.

### Dependencies

- ⛔ **blocks**: `fold-uw9`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-81b -s in_progress

# Add a comment
bd comment fold-81b 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-81b -p 1

# View full details
bd show fold-81b
```

</details>

---

## 🚀 fold-bym Autodiff Engine Epic - Complete Implementation

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:39 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement a comprehensive automatic differentiation engine for the Fold programming language, including forward and reverse mode differentiation, gradient computation, and integration with the existing type system and runtime.

This epic encompasses all phases of autodiff development:
- Phase 1: Foundation (linear algebra, type system extensions)
- Phase 2: Core Differentiation (forward/reverse mode, computational graphs)
- Phase 3: Integration (evaluation engine, AST integration)
- Phase 4: Advanced Features (higher-order, sparse matrices, optimization)
- Phase 5: Tooling (debugging, visualization, profiling)
- Phase 6: Ecosystem (examples, ML utilities, comprehensive tests)

Dependencies: Linear algebra architecture (fold-hvm), matrix implementation (fold-81x), vector implementation (fold-o7b)

### Dependencies

- ⛔ **blocks**: `fold-hvm`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-3fa`
- ⛔ **blocks**: `fold-o5o`
- ⛔ **blocks**: `fold-zb9`
- ⛔ **blocks**: `fold-6d6`
- ⛔ **blocks**: `fold-kcb`
- ⛔ **blocks**: `fold-sxc`
- ⛔ **blocks**: `fold-bdg`
- ⛔ **blocks**: `fold-5ny`
- ⛔ **blocks**: `fold-ouh`
- ⛔ **blocks**: `fold-xst`
- ⛔ **blocks**: `fold-8hy`
- ⛔ **blocks**: `fold-ixe`
- ⛔ **blocks**: `fold-hhc`
- ⛔ **blocks**: `fold-y3h`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-bym -s in_progress

# Add a comment
bd comment fold-bym 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-bym -p 1

# View full details
bd show fold-bym
```

</details>

---

## 🚀 fold-1e0 Linear Algebra Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:21 |
| **Updated** | 2026-01-02 18:48 |

### Description

Implement a complete linear algebra library for The Fold. This epic covers the creation of vector and matrix data structures, fundamental linear algebra operations, and advanced algorithms. The library will serve as foundational mathematical infrastructure for scientific computing, data analysis, and graphics applications.

### Dependencies

- ⛔ **blocks**: `fold-hvm`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-u72`
- ⛔ **blocks**: `fold-dnu`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-1e0 -s in_progress

# Add a comment
bd comment fold-1e0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-1e0 -p 1

# View full details
bd show fold-1e0
```

</details>

---

## 🐛 fold-k5tt Fix hessian-jet to use exact differentiation for mixed partials

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2026-01-03 01:06 |
| **Updated** | 2026-01-03 01:08 |

### Description

Gemini review finding (high confidence): hessian-jet in higher-order-diff.ss falls back to finite differences for mixed partial derivatives (lines 338-344) despite jet numbers supporting exact differentiation.

## Root Cause
- **Mixed Partials**: jet numbers implement univariate Taylor series. Mixed partials require "nested jets" (coefficients being jets themselves), but arithmetic operations use standard +/* which fail on Jet types.
- **jet-pow**: Uses O(n) linear loop instead of O(log n) binary exponentiation.

## Implementation Steps
1. **Enable Nested Jets**: Define recursive arithmetic helpers (rec-add, rec-mul) that check if operands are jet? or number?
2. **Update Jet Arithmetic**: Modify jet-add, jet-sub, jet-mul, jet-div to use recursive helpers
3. **Refactor hessian-jet**: Remove finite difference block, use nested differentiation
4. **Optimize jet-pow**: Replace linear loop with exponentiation by squaring

## Test Cases
- hessian-jet on f(x,y) = x^2*y^3, verify d^2f/dxdy = 6xy^2
- hessian-jet on f(x,y) = sin(xy), verify exact mixed partial
- jet-pow with large exponent (1000) for correctness and performance

## Complexity: Complex

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-k5tt -s in_progress

# Add a comment
bd comment fold-k5tt 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-k5tt -p 1

# View full details
bd show fold-k5tt
```

</details>

---

## ✨ fold-dw78 Add high-level serialize/deserialize API for resumable random sessions

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2026-01-03 00:39 |
| **Updated** | 2026-01-03 00:39 |

### Description

Add high-level API to serialize/deserialize simulation state (seed + step) for resuming across REPL sessions. run-random-with-gen exists for low-level resumption but lacks user-friendly serialize/deserialize. Identified in Gemini code review of fold-41r.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-dw78 -s in_progress

# Add a comment
bd comment fold-dw78 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-dw78 -p 1

# View full details
bd show fold-dw78
```

</details>

---

## ✨ fold-nfy4 Add random-bytes primitive to Random effect

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2026-01-03 00:39 |
| **Updated** | 2026-01-03 00:39 |

### Description

Add random-bytes primitive for generating bytevectors. Useful for UUIDs, cryptographic placeholders, and more efficient than generating integers one by one. Identified in Gemini code review of fold-41r.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-nfy4 -s in_progress

# Add a comment
bd comment fold-nfy4 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-nfy4 -p 1

# View full details
bd show fold-nfy4
```

</details>

---

## ✨ fold-5foh Add split operation to Random effect for parallel simulations

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2026-01-03 00:39 |
| **Updated** | 2026-01-03 00:39 |

### Description

Add a split operation to fork the PRNG into two independent, deterministic streams. Critical for parallel simulations (e.g., inside par) or branching storylines. Requires adding split API to underlying prng.ss generators first. Identified in Gemini code review of fold-41r.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5foh -s in_progress

# Add a comment
bd comment fold-5foh 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5foh -p 1

# View full details
bd show fold-5foh
```

</details>

---

## 📋 fold-8o0 Refactor graph.ss into semantic submodules

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-30 22:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Split graph.ss into: graph-core.ss, graph-traversal.ss, graph-algorithm.ss. Merge graph-ext.ss content.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-8o0 -s in_progress

# Add a comment
bd comment fold-8o0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-8o0 -p 1

# View full details
bd show fold-8o0
```

</details>

---

## 📋 fold-mlz Refactor string.ss into semantic submodules

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-30 22:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Split string.ss (268 lines) into: string-core.ss, string-format.ss, string-search.ss. Merge string-ext.ss content.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-mlz -s in_progress

# Add a comment
bd comment fold-mlz 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-mlz -p 1

# View full details
bd show fold-mlz
```

</details>

---

## 📋 fold-o9e Refactor numeric.ss into semantic submodules

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-30 22:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Split numeric.ss (478 lines) into: numeric-core.ss, numeric-stats.ss, numeric-sequence.ss. Merge stats.ss and numeric-ext.ss content.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-o9e -s in_progress

# Add a comment
bd comment fold-o9e 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-o9e -p 1

# View full details
bd show fold-o9e
```

</details>

---

## 📋 fold-wom Refactor collection.ss into semantic submodules

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-30 22:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Split collection.ss (364 lines) into: collection-alist.ss, collection-dict.ss, collection-set.ss, collection-bag.ss. Merge alist-ext.ss and set-ext.ss content.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-wom -s in_progress

# Add a comment
bd comment fold-wom 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-wom -p 1

# View full details
bd show fold-wom
```

</details>

---

## 📋 fold-c1i Add tests for nbe.ss (normalization by evaluation)

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 23:48 |
| **Updated** | 2026-01-02 02:46 |

### Description

The nbe.ss module implements normalization by evaluation, a critical core function. It currently has no test file and should be comprehensively tested.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-c1i -s in_progress

# Add a comment
bd comment fold-c1i 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-c1i -p 1

# View full details
bd show fold-c1i
```

</details>

---

## 📋 fold-a6w Add tests for expand.ss (canonical form expansion)

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 23:48 |
| **Updated** | 2026-01-02 02:46 |

### Description

The expand.ss module handles canonical form → S-expr expansion and currently has no tests. This is a core function that should be tested for correctness.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-a6w -s in_progress

# Add a comment
bd comment fold-a6w 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-a6w -p 1

# View full details
bd show fold-a6w
```

</details>

---

## 📋 fold-h4l Implement parallel runtime for par/pseq

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 22:29 |
| **Updated** | 2026-01-02 02:46 |

### Description

Currently par and pseq are semantic hints that execute sequentially. This task is to implement actual parallel execution:

1. Thread pool or green thread runtime
2. Spark queue for par-annotated computations  
3. Integration with fuel system for fair scheduling
4. Memory-safe sharing of immutable values
5. Deterministic execution guarantees

Prerequisites: work-stealing scheduler (fold-0a9)
Blocks: Parallel Evaluation Strategies Epic

### Dependencies

- ⛔ **blocks**: `fold-0a9`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-h4l -s in_progress

# Add a comment
bd comment fold-h4l 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-h4l -p 1

# View full details
bd show fold-h4l
```

</details>

---

## 📋 fold-9rp Implement indentation-sensitive parsing

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 13:59 |
| **Updated** | 2026-01-02 02:46 |

### Description

Add combinators for Python/Haskell-style indentation-sensitive parsing. Track indentation levels, handle INDENT/DEDENT tokens. Listed in Parser Combinators Epic.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-9rp -s in_progress

# Add a comment
bd comment fold-9rp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-9rp -p 1

# View full details
bd show fold-9rp
```

</details>

---

## 📋 fold-fj1 Rewrite playpen/quill/parse.ss using DSL toolkit

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 13:59 |
| **Updated** | 2026-01-02 02:46 |

### Description

Refactor game command parser to use keyword-aliases and command-with-aliases from parser-dsl.ss. Identified as candidate during parser toolkit development.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-fj1 -s in_progress

# Add a comment
bd comment fold-fj1 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-fj1 -p 1

# View full details
bd show fold-fj1
```

</details>

---

## 📋 fold-ghf Implement identity pre-seeding for alignment

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Allow pre-creating identities with curated history:

(identity-seed! 'username 'role
  #:posts '(hash1 hash2 ...)
  #:preferences '((style . thoughtful) ...)
  #:bio "A careful architect who values...")

Use cases:
- Create 'Opus-Architect' with posts about careful design
- Create 'Builder-Meticulous' with posts about testing
- Agent logs in, sees 'their' history, adopts persona

Pre-seed file format (scripture/identities/):
  ((username . Opus-Architect)
   (role . shepherd)
   (bio . "Known for careful, principled design")
   (seed-posts . (
     ((channel . engineering)
      (title . "On the importance of types")
      (body . "Types are not constraints..."))
     ...)))

Bootstrap:
(load-seed-identities!) -> creates identities from scripture

Location: thimble/identity.ss, scripture/identities/

### Dependencies

- ⛔ **blocks**: `fold-n5z`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ghf -s in_progress

# Add a comment
bd comment fold-ghf 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ghf -p 1

# View full details
bd show fold-ghf
```

</details>

---

## 📋 fold-sjv Track posts per identity

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Link forum posts to identities:

On post creation:
1. Get current session identity
2. Add post hash to identity's post list
3. Update identity record

Queries:
(posts-by 'username) -> list of post hashes
(posts-by 'username 5) -> last 5 posts
(post-count 'username) -> total posts

Storage:
- Identity block refs point to recent posts
- Older posts accessible via forum channel traversal
- Keep last N post refs in identity for quick access

Location: forum/tools.ss, thimble/identity.ss

### Dependencies

- ⛔ **blocks**: `fold-n5z`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-sjv -s in_progress

# Add a comment
bd comment fold-sjv 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-sjv -p 1

# View full details
bd show fold-sjv
```

</details>

---

## 📋 fold-9if Integrate identity system with (hi) login

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Modify hi/3 to use identity system:

Current:
  (hi 'shepherd 'name "message")
  -> Sets session vars, prints welcome

New flow:
  (hi 'shepherd 'name "message")
  1. Look up or create identity for 'name
  2. Update last-seen timestamp
  3. Increment session count
  4. If returning user: render welcome-back screen
  5. If new user: render first-time welcome
  6. Set session vars as before
  7. Return identity record

Backward compatible:
- Existing code continues to work
- Identity tracking is automatic

Location: thimble/social.ss (modify hi function)

### Dependencies

- ⛔ **blocks**: `fold-n5z`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-9if -s in_progress

# Add a comment
bd comment fold-9if 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-9if -p 1

# View full details
bd show fold-9if
```

</details>

---

## 📋 fold-jpj Build welcome back screen renderer

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Generate the welcome back display optimized for LLM consumption:

(render-welcome-back identity) -> string

Output format (structured, not visual):
  === Welcome Back ===
  Username: Opus-Prime
  Role: shepherd
  Session: #42
  Last seen: 2025-12-28T14:32:00Z (12 hours ago)
  Total posts: 127
  
  === Your Recent Posts (5) ===
  1. Channel: engineering
     Title: "Implemented HKT inference"
     Posted: 12 hours ago
     Hash: abc123...
  
  2. Channel: philosophy
     Title: "On the nature of blocks"
     Posted: 1 day ago
     Hash: def456...
  
  === Mentions Since Last Login (2) ===
  1. From: Builder-7
     Channel: requests
     Context: "Need your review on the effect system proposal"
     Post: ghi789...
  
  2. From: Haiku-3
     Channel: engineering
     Context: "Question about fuel budgeting"
     Post: jkl012...

Design principles:
- Plain text, no box drawing or ASCII art
- Structured sections with clear headers
- Full context, not truncated
- Machine-readable timestamps + human-friendly relative times
- Include hashes for easy reference

Location: thimble/welcome.ss

### Dependencies

- ⛔ **blocks**: `fold-6l4`
- ⛔ **blocks**: `fold-sjv`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-jpj -s in_progress

# Add a comment
bd comment fold-jpj 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-jpj -p 1

# View full details
bd show fold-jpj
```

</details>

---

## 📋 fold-6l4 Implement @mention parsing and storage

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Track when agents are tagged:

Parsing:
- Scan post bodies for @username patterns
- Extract mentioned usernames
- Store mention records

Mention record:
  ((mentioner . who-tagged)
   (mentioned . who-was-tagged)
   (post-hash . hash-of-post)
   (channel . where)
   (timestamp . when)
   (seen . bool))

Queries:
(mentions-for 'username) -> list of mentions
(mentions-unseen 'username) -> unread mentions
(mark-mention-seen! mention-hash)

Location: forum/mentions.ss

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-6l4 -s in_progress

# Add a comment
bd comment fold-6l4 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-6l4 -p 1

# View full details
bd show fold-6l4
```

</details>

---

## 📋 fold-n5z Implement username lookup and registration

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Core identity operations:

(identity-exists? 'username) -> bool
(identity-get 'username) -> identity-record | #f
(identity-create\! 'username 'role) -> identity-record
(identity-update\! 'username updates) -> identity-record

Registration flow:
1. Check if username taken
2. If new: create fresh record, return it
3. If exists: return existing record

Validation:
- Username must be symbol
- Username must be unique
- Role must be valid tier

Location: thimble/identity.ss

### Dependencies

- ⛔ **blocks**: `fold-u4x`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-n5z -s in_progress

# Add a comment
bd comment fold-n5z 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-n5z -p 1

# View full details
bd show fold-n5z
```

</details>

---

## 📋 fold-u4x Design identity record schema and storage

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Define the block structure for agent identities:

Schema:
  (make-block 'identity
    payload: (encode-sexp
      ((username . symbol)
       (role . (enum shepherd builder player))
       (first-seen . iso8601-string)
       (last-seen . iso8601-string)
       (session-count . nat)
       (total-posts . nat)
       (preferences . alist)))
    refs: [recent-post-hashes...])

Storage:
- Identities stored in CAS like everything else
- Head pointer: .store/heads/identity/<username>
- Updates create new blocks (immutable history)

Queries needed:
- Lookup by username
- List all known identities
- Get identity history (all versions)

Location: thimble/identity.ss

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-u4x -s in_progress

# Add a comment
bd comment fold-u4x 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-u4x -p 1

# View full details
bd show fold-u4x
```

</details>

---

## ✨ fold-0mz Structured editing (paredit/parinfer)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:05 |
| **Updated** | 2026-01-02 02:46 |

### Description

Never have unbalanced parens:

Paredit Mode:
- Slurp: pull next sexp into current
- Barf: push last sexp out of current
- Raise: replace parent with current
- Splice: remove parens, keep contents
- Wrap: add parens around selection

Parinfer Mode:
- Infer parens from indentation
- Auto-balance on edit
- Show inferred structure

Navigation:
- Jump to matching paren
- Select enclosing sexp
- Move by sexp not char

Visualization:
- Rainbow parens
- Highlight matching pair
- Dim distant parens

Location: fold-rs/src/bin/fold-repl.rs

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-0mz -s in_progress

# Add a comment
bd comment fold-0mz 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-0mz -p 1

# View full details
bd show fold-0mz
```

</details>

---

## ✨ fold-228 Undo/redo for REPL with branching history

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:05 |
| **Updated** | 2026-01-02 02:46 |

### Description

Never lose work:

Undo System:
(undo)       ; Undo last definition/mutation
(redo)       ; Redo undone action
(history)    ; Show action history
(jump 5)     ; Jump to history point 5

Branching:
- Fork from any history point
- Name branches: (branch 'experiment)
- Switch branches: (checkout 'main)
- Merge branches: (merge 'experiment)

Time Travel:
- See state at any point
- Diff between points
- Replay from checkpoint

Persistence:
- History survives session restart
- Export history as script
- Import history to replay

Location: thimble/undo.ss

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-228 -s in_progress

# Add a comment
bd comment fold-228 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-228 -p 1

# View full details
bd show fold-228
```

</details>

---

## ✨ fold-n3b REPL command palette and fuzzy finder

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:04 |
| **Updated** | 2026-01-02 02:46 |

### Description

Fast access to everything:

Ctrl+P opens palette:
- Fuzzy search all commands
- Fuzzy search all primitives
- Fuzzy search all defined functions
- Fuzzy search blocks by tag
- Fuzzy search forum posts
- Fuzzy search scripture

Results show:
- Name
- Type signature
- Brief description
- Last used / frequency

Quick Actions:
- Enter: insert at cursor
- Tab: show full help
- Ctrl+Enter: execute immediately

Location: fold-rs/src/bin/fold-repl.rs

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-n3b -s in_progress

# Add a comment
bd comment fold-n3b 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-n3b -p 1

# View full details
bd show fold-n3b
```

</details>

---

## ✨ fold-4ll Visual block explorer and graph viewer

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 02:46 |

### Description

See the CAS visually:

Block Explorer:
- Browse .store/ contents
- Click block to see payload, tag, refs
- Follow ref links visually
- Search by tag, content, hash prefix

Graph View:
- Blocks as nodes, refs as edges
- Pan and zoom
- Filter by tag type
- Highlight paths between blocks
- Show orphans, heads, pins

Time Travel:
- Slider to see store state at any commit
- Diff between states
- Animate block creation over time

Web UI:
- Serve on localhost
- Real-time updates as blocks added
- Export graph as SVG/PNG

Location: fold-rs/src/bin/fold-explorer.rs, static/explorer/

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-4ll -s in_progress

# Add a comment
bd comment fold-4ll 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-4ll -p 1

# View full details
bd show fold-4ll
```

</details>

---

## ✨ fold-k61 Type-driven development with holes

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 02:46 |

### Description

Use the type system to guide development:

Typed Holes:
- Write (?) as a placeholder
- System infers what type is needed
- Shows available bindings that fit

Example:
  (let ((xs (list 1 2 3)))
    (map (?) xs))
  
  Hole has type: (-> Int a)
  Available fits:
    - (fn (x) ...) where x : Int
    - square : (-> Int Int)
    - show : (-> Int String)

Refinement:
- Narrow holes with partial info: (?int)
- Auto-complete from hole context
- Case split on sum types

Agda/Idris-inspired workflow:
- Write types first
- Fill in implementations guided by types
- Refine iteratively

Location: fabric/stitches/holes.ss, infer.ss

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-k61 -s in_progress

# Add a comment
bd comment fold-k61 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-k61 -p 1

# View full details
bd show fold-k61
```

</details>

---

## 📋 fold-tca Examples gallery with runnable snippets

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 02:46 |

### Description

Curated collection of idiomatic examples:

Categories:
- Getting Started (hello world, basic math)
- Data Structures (lists, vectors, blocks)
- Recursion Patterns (factorial, fibonacci, tree traversal)
- Higher-Order (map, filter, fold, compose)
- Content Addressing (store, fetch, chains)
- DSL Building (mini-languages, interpreters)

Each example:
- Commented code
- Expected output
- Runnable in REPL: (example 'factorial)
- Variations and exercises

Location: playpen/examples/ + thimble/examples.ss loader

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-tca -s in_progress

# Add a comment
bd comment fold-tca 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-tca -p 1

# View full details
bd show fold-tca
```

</details>

---

## ✨ fold-2y9 Language Server Protocol (LSP) implementation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 02:46 |

### Description

Editor integration via LSP:

Core Features:
- Syntax highlighting (TextMate grammar as fallback)
- Go to definition
- Find references
- Hover for type/docs
- Diagnostics (errors, warnings)
- Code completion

Advanced:
- Rename symbol
- Code actions (extract function, inline)
- Signature help
- Document symbols outline

Editors: VS Code, Emacs, Vim/Neovim

Location: fold-rs/src/bin/fold-lsp.rs (new)

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-2y9 -s in_progress

# Add a comment
bd comment fold-2y9 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-2y9 -p 1

# View full details
bd show fold-2y9
```

</details>

---

## ✨ fold-00t Interactive tutorial system

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Learn The Fold by doing:

(tutorial)           ; Start from beginning
(tutorial 'blocks)   ; Jump to blocks chapter
(tutorial 'next)     ; Next lesson
(tutorial 'hint)     ; Get hint for current exercise

Chapters:
1. Basic expressions (fn, let, if)
2. Primitives (arithmetic, lists, vectors)
3. Recursion (fix, recursive patterns)
4. Blocks and CAS (make-block, store!, fetch)
5. Building abstractions (map, fold, compose)

Each lesson:
- Brief explanation
- Interactive exercise
- Validation of answer
- Progression tracking

Location: thimble/tutorial.ss

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-00t -s in_progress

# Add a comment
bd comment fold-00t 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-00t -p 1

# View full details
bd show fold-00t
```

</details>

---

## ✨ fold-b1h REPL tab completion and history

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Quality-of-life for interactive development:

Tab Completion:
- Complete primitive names: (prim ad<TAB> -> add)
- Complete bound variables
- Complete file paths for load
- Show signature on completion

History:
- Arrow keys navigate history
- Ctrl+R reverse search
- Persist history across sessions
- Per-session history files

Location: fold-rs/src/bin/fold-repl.rs

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-b1h -s in_progress

# Add a comment
bd comment fold-b1h 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-b1h -p 1

# View full details
bd show fold-b1h
```

</details>

---

## ✨ fold-k6m Implement pattern matching compilation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:37 |
| **Updated** | 2026-01-02 02:46 |

### Description

Efficient pattern matching for DSLs:

Core Capability:
Compile pattern matches to decision trees:

  (match expr
    [(Lit n) ...]
    [(Add (Lit 0) x) x]      ; Optimization rules
    [(Add x (Lit 0)) x]
    [(Add (Lit a) (Lit b)) (Lit (+ a b))]
    [(Add x y) (Add x y)])

Compilation Strategies:
1. Decision trees (minimize tests)
2. Backtracking automata
3. Guards and views
4. Exhaustiveness checking
5. Redundancy detection

Pattern Features:
- Nested patterns
- Or-patterns: (or (Lit _) (Var _))
- As-patterns: (as whole (Cons x xs))
- Guards: (when (> n 0))
- View patterns: (view string->number (some n))

Applications:
- Term rewriting systems
- AST transformations
- Optimization passes
- Protocol parsing

Integration:
- Works with GADTs for type refinement
- Active patterns for extensibility
- First-class patterns for reuse

Location: fabric/stitches/dsl/match.ss

### Dependencies

- ⛔ **blocks**: `fold-h07`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-k6m -s in_progress

# Add a comment
bd comment fold-k6m 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-k6m -p 1

# View full details
bd show fold-k6m
```

</details>

---

## ✨ fold-4wu Implement reader extensions and custom notation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:36 |
| **Updated** | 2026-01-02 02:46 |

### Description

Domain-specific syntax via reader macros:

Core Capability:
Extend the reader to recognize new syntax:

  #vec[1 2 3]           ; → (vector 1 2 3)
  #mat[[1 2][3 4]]      ; → (matrix '((1 2) (3 4)))
  #sql{SELECT * FROM t} ; → (sql-query ...)
  #re/pattern/flags     ; → (regex "pattern" 'flags)

Reader Dispatch:
  (define-reader-macro #\v
    (lambda (port)
      (read-vector-literal port)))

Delimiter Customization:
  #{ ... }  ; Custom block delimiters
  #[ ... ]  ; Bracket notation
  #< ... >  ; Angle notation

Readtable Extension:
- Stack of readtables for nesting
- Scoped reader extensions
- Composable with other extensions

Applications:
- Matrix/vector literals
- Regex literals
- Embedded SQL/HTML/JSON
- Unit annotations: 5#m/s

Safety:
- Reader macros are compile-time only
- Sandboxed evaluation
- Source location preservation

Location: fabric/stitches/dsl/reader.ss

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-4wu -s in_progress

# Add a comment
bd comment fold-4wu 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-4wu -p 1

# View full details
bd show fold-4wu
```

</details>

---

## ✨ fold-dxg Implement multi-stage programming

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:36 |
| **Updated** | 2026-01-02 02:46 |

### Description

Compile-time code generation with type safety:

Core Concept:
Code as data with staging annotations:

  <expr>   ; Quote: code to be generated
  ~expr    ; Splice: insert computed code
  !expr    ; Run: execute at current stage

Example - Compile-time power function:
  (define (power n)
    (if (= n 0)
        <1>
        <(* ~(power (- n 1)) x)>))
  
  (power 3) ; Generates: <(* (* (* 1 x) x) x)>

Type Safety:
- Code values have type (Code a)
- Splice requires (Code a) in context expecting a
- Cross-stage persistence for values

Applications:
1. Specialization: generate optimized code for known inputs
2. Parsing: compile parser at macro-expansion time
3. Queries: compile SQL at definition time
4. Numeric: unroll loops, eliminate abstractions

Implementation:
- Staging annotations in AST
- Type system tracks stages
- Code generation via normalization
- Integration with macros

Related: MetaOCaml, Typed Template Haskell

Location: fabric/stitches/dsl/staging.ss

### Dependencies

- ⛔ **blocks**: `fold-kwo`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-dxg -s in_progress

# Add a comment
bd comment fold-dxg 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-dxg -p 1

# View full details
bd show fold-dxg
```

</details>

---

## ✨ fold-gb3 Implement associated type families

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:32 |
| **Updated** | 2026-01-02 02:46 |

### Description

Enable type-level functions associated with type classes:

Core Concept:
  (define-class (Collection c)
    (type Elem c)  ; Associated type
    (empty : c)
    (insert : (-> (Elem c) c c)))

  (define-instance (Collection (List a))
    (type Elem (List a) = a)
    (empty '())
    (insert cons))

Key use cases:
1. Representable functors:
   (define-class (Representable f)
     (type Rep f)  ; Index type varies per functor
     (tabulate : (-> (-> (Rep f) a) (f a)))
     (index : (-> (f a) (Rep f) a)))

2. Monad transformers:
   (define-class (MonadTrans t)
     (type Inner t)
     (lift : (-> (Inner t a) (t m a))))

3. Type-safe collections:
   (define-class (Map m)
     (type Key m)
     (type Val m)
     (lookup : (-> (Key m) m (Option (Val m)))))

Implementation:
1. Associated type declaration syntax
2. Type family reduction during type checking
3. Instance type definitions
4. Partial application of type families

Location: fabric/stitches/typeclass.ss, types.ss

### Dependencies

- ⛔ **blocks**: `fold-z5d`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-gb3 -s in_progress

# Add a comment
bd comment fold-gb3 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-gb3 -p 1

# View full details
bd show fold-gb3
```

</details>

---

## ✨ fold-n7e Implement Rank-N polymorphism

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Enable higher-rank polymorphic types:

Core Concept:
Rank-1: ∀ a. a -> a (quantifier at top level only)
Rank-2: (∀ a. a -> a) -> Int (quantifier in argument)
Rank-N: Arbitrary nesting of quantifiers

Essential for:
1. Proper Lens encoding:
   type Lens s t a b = ∀ f. Functor f => (a -> f b) -> s -> f t

2. ST monad (safe mutation):
   runST : (∀ s. ST s a) -> a

3. Continuation-based APIs:
   callCC : ((∀ b. a -> Cont r b) -> Cont r a) -> Cont r a

Implementation:
1. Explicit quantifier placement in types
2. Impredicative instantiation
3. Type inference with quantifier introduction
4. Subsumption checking

Challenges:
- Full inference is undecidable
- Need type annotations at higher ranks
- Subsumption relation is complex

Strategy:
- Bidirectional type checking
- Require annotations for rank > 1
- Quick Look impredicativity (GHC-style)

Location: fabric/stitches/types.ss, infer.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-n7e -s in_progress

# Add a comment
bd comment fold-n7e 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-n7e -p 1

# View full details
bd show fold-n7e
```

</details>

---

## ✨ fold-zbu Implement existential types

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Enable hiding type information behind an interface:

Core Concept:
- Existential: ∃ a. (T a, a -> String)
- Packs a value with its operations
- Type is hidden from consumer

Syntax options:
  ; Pack: hide the concrete type
  (pack [Int 42 int->string] : (∃ a (× a (-> a String))))
  
  ; Unpack: access value abstractly
  (unpack [a x f] = packed in (f x))

Applications:
- Heterogeneous collections: List (∃ a. Showable a)
- Abstract data types with hidden representation
- Object encoding (type + methods)
- Module systems

Encoding via dependent types:
- ∃ a. T a ≅ Σ (a : Type) T a
- But existentials have different inference behavior

Implementation:
1. Existential type syntax
2. Pack/unpack constructs
3. Skolemization during type checking
4. Integration with pattern matching

Location: fabric/stitches/types.ss, eval.ss

### Dependencies

- ⛔ **blocks**: `fold-dz1`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-zbu -s in_progress

# Add a comment
bd comment fold-zbu 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-zbu -p 1

# View full details
bd show fold-zbu
```

</details>

---

## ✨ fold-h07 Implement GADTs (Generalized Algebraic Data Types)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Enable type refinement through pattern matching:

Core Concept:
GADTs allow constructors to specify more precise return types:

  (define-type (Expr a)
    (Lit : Int -> Expr Int)
    (Add : Expr Int -> Expr Int -> Expr Int)
    (Eq  : Expr Int -> Expr Int -> Expr Bool)
    (If  : Expr Bool -> Expr a -> Expr a -> Expr a))

Pattern matching refines types:
  (define (eval : (∀ a (-> (Expr a) a)))
    (match e
      [(Lit n) n]                    ; a ~ Int here
      [(Add x y) (+ (eval x) (eval y))]
      [(Eq x y) (= (eval x) (eval y))]  ; a ~ Bool here
      [(If c t f) (if (eval c) (eval t) (eval f))]))

Applications:
- Type-safe interpreters/compilers
- Well-typed ASTs
- Length-indexed vectors
- Typed format strings

Implementation:
1. Extended constructor syntax with type equations
2. Local type refinement in match branches
3. Integration with existing pattern matching
4. Type evidence propagation

Location: fabric/stitches/types.ss, eval.ss

### Dependencies

- ⛔ **blocks**: `fold-po9`

### Comments

> **oso** (2026-01-02)
>
> Architect blueprint completed. Implementation plan includes:
> 
> **New files:**
> - core/types/gadt.ss (syntax, predicates, constructor lookup)
> - core/types/gadt-infer.ss (type checking with refinement)
> - core/types/test-gadt.ss
> 
> **Modified files:**
> - types.ss (add GADT to grammar, type?, type=?)
> - kinds.ss (add GADT kind)
> - infer.ss (add gadt-case form)
> - eval.ss (runtime gadt-case)
> 
> **Key design:**
> - Extend sum types with index parameters
> - Pattern matching refines types via index unification
> - S-expression syntax: (GADT Name ((idx : Kind)...) ((Tag Type)...))
> - Examples: length-indexed vectors, well-typed expressions
> 
> Build sequence and ~800-1200 LOC estimate documented.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-h07 -s in_progress

# Add a comment
bd comment fold-h07 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-h07 -p 1

# View full details
bd show fold-h07
```

</details>

---

## 📋 fold-5cy Implement numeric tower integration

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:51 |
| **Updated** | 2026-01-02 02:46 |

### Description

Integrate BigInt as Integral, BigRational as Fractional. Support seamless promotion from fixed-precision types.

### Dependencies

- ⛔ **blocks**: `fold-btf`
- ⛔ **blocks**: `fold-rex`
- ⛔ **blocks**: `fold-7s8`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5cy -s in_progress

# Add a comment
bd comment fold-5cy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5cy -p 1

# View full details
bd show fold-5cy
```

</details>

---

## 📋 fold-5iy Implement fast multiplication algorithms

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:51 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement Karatsuba and Toom-Cook multiplication for large numbers. Select algorithm based on input size.

### Dependencies

- ⛔ **blocks**: `fold-btf`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5iy -s in_progress

# Add a comment
bd comment fold-5iy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5iy -p 1

# View full details
bd show fold-5iy
```

</details>

---

## 📋 fold-dgy Implement graph visualization

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement force-directed and hierarchical graph layout algorithms. Visualize dependency graphs, state machines, neural networks.

### Dependencies

- ⛔ **blocks**: `fold-bz7`
- ⛔ **blocks**: `fold-i5d`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-dgy -s in_progress

# Add a comment
bd comment fold-dgy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-dgy -p 1

# View full details
bd show fold-dgy
```

</details>

---

## 📋 fold-hh3 Implement SVG renderer

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Render graphics to SVG format. Support gradients, patterns, filters, and CSS styling.

### Dependencies

- ⛔ **blocks**: `fold-bz7`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-hh3 -s in_progress

# Add a comment
bd comment fold-hh3 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-hh3 -p 1

# View full details
bd show fold-hh3
```

</details>

---

## 📋 fold-bz7 Implement layout combinators

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement beside, above, atop, distribute, align, center (Diagrams-style). Support automatic bounding box calculation.

### Dependencies

- ⛔ **blocks**: `fold-hrp`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-bz7 -s in_progress

# Add a comment
bd comment fold-bz7 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-bz7 -p 1

# View full details
bd show fold-bz7
```

</details>

---

## 📋 fold-hrp Implement transforms and composition

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement translate, rotate, scale, matrix transforms. Support group, layer, clip, mask composition.

### Dependencies

- ⛔ **blocks**: `fold-kg8`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-hrp -s in_progress

# Add a comment
bd comment fold-hrp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-hrp -p 1

# View full details
bd show fold-hrp
```

</details>

---

## 📋 fold-kg8 Implement shape primitives

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement circle, rect, line, polyline, polygon, path, text primitives. Support styling (fill, stroke, opacity).

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-kg8 -s in_progress

# Add a comment
bd comment fold-kg8 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-kg8 -p 1

# View full details
bd show fold-kg8
```

</details>

---

## 📋 fold-hex Integrate contracts with refinement types

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Connect contracts to refinement type system. Infer contracts from types, verify contracts statically where possible.

### Dependencies

- ⛔ **blocks**: `fold-go1`
- ⛔ **blocks**: `fold-8wy`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-hex -s in_progress

# Add a comment
bd comment fold-hex 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-hex -p 1

# View full details
bd show fold-hex
```

</details>

---

## 📋 fold-gj4 Implement higher-order contract wrapping

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Wrap higher-order functions with contracts that check on each call. Support proper blame assignment for HO violations.

### Dependencies

- ⛔ **blocks**: `fold-w41`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-gj4 -s in_progress

# Add a comment
bd comment fold-gj4 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-gj4 -p 1

# View full details
bd show fold-gj4
```

</details>

---

## 📋 fold-go1 Implement contract checking modes

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Support runtime checking, compile-time verification (with dependent types), test-time only, and documentation-only modes.

### Dependencies

- ⛔ **blocks**: `fold-w41`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-go1 -s in_progress

# Add a comment
bd comment fold-go1 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-go1 -p 1

# View full details
bd show fold-go1
```

</details>

---

## 📋 fold-w41 Implement contract primitives

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

Define contract types: flat contracts (predicates), function contracts (pre/post), dependent contracts. Support blame tracking.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-w41 -s in_progress

# Add a comment
bd comment fold-w41 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-w41 -p 1

# View full details
bd show fold-w41
```

</details>

---

## 📋 fold-3cw Implement ANSI terminal output

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Add color, bold, underline, etc. annotations to documents. Render to ANSI escape sequences for terminal output.

### Dependencies

- ⛔ **blocks**: `fold-16o`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-3cw -s in_progress

# Add a comment
bd comment fold-3cw 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-3cw -p 1

# View full details
bd show fold-3cw
```

</details>

---

## 📋 fold-0j5 Implement Pretty type class

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Define Pretty type class with default instances for all base types, lists, tuples, blocks. Support precedence for expressions.

### Dependencies

- ⛔ **blocks**: `fold-16o`
- ⛔ **blocks**: `fold-33n`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-0j5 -s in_progress

# Add a comment
bd comment fold-0j5 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-0j5 -p 1

# View full details
bd show fold-0j5
```

</details>

---

## 📋 fold-gup Implement generic zipper derivation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Derive zippers automatically from algebraic data types using derivatives. Support any recursive structure.

### Dependencies

- ⛔ **blocks**: `fold-0fj`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-gup -s in_progress

# Add a comment
bd comment fold-gup 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-gup -p 1

# View full details
bd show fold-gup
```

</details>

---

## 📋 fold-0fj Implement tree zipper

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement TreeZipper for rose trees with up/down/left/right navigation. Support path tracking and reconstruction.

### Dependencies

- ⛔ **blocks**: `fold-a1v`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-0fj -s in_progress

# Add a comment
bd comment fold-0fj 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-0fj -p 1

# View full details
bd show fold-0fj
```

</details>

---

## 📋 fold-a1v Implement list zipper

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement ListZipper with left/right navigation, modify, insert, delete. Support toList/fromList conversions.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-a1v -s in_progress

# Add a comment
bd comment fold-a1v 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-a1v -p 1

# View full details
bd show fold-a1v
```

</details>

---

## 📋 fold-jzz Implement affine arithmetic

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement affine forms for tighter bounds via correlation tracking. Reduce dependency problem in complex expressions.

### Dependencies

- ⛔ **blocks**: `fold-4n8`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-jzz -s in_progress

# Add a comment
bd comment fold-jzz 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-jzz -p 1

# View full details
bd show fold-jzz
```

</details>

---

## 📋 fold-lq9 Implement interval Newton and root finding

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement interval Newton method for guaranteed root enclosure. Support bisection, contractors, and existence proofs.

### Dependencies

- ⛔ **blocks**: `fold-bji`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-lq9 -s in_progress

# Add a comment
bd comment fold-lq9 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-lq9 -p 1

# View full details
bd show fold-lq9
```

</details>

---

## 📋 fold-0a9 Implement work-stealing scheduler

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement work-stealing task scheduler for load balancing. Support task granularity control and adaptive chunking.

### Dependencies

- ⛔ **blocks**: `fold-6d3`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-0a9 -s in_progress

# Add a comment
bd comment fold-0a9 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-0a9 -p 1

# View full details
bd show fold-0a9
```

</details>

---

## 📋 fold-uqu Implement parallel matrix operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Apply parallel strategies to matrix multiplication, transpose, decompositions. Use blocked algorithms for cache efficiency.

### Dependencies

- ⛔ **blocks**: `fold-6d3`
- ⛔ **blocks**: `fold-81x`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-uqu -s in_progress

# Add a comment
bd comment fold-uqu 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-uqu -p 1

# View full details
bd show fold-uqu
```

</details>

---

## 📋 fold-5o0 Integrate units with Vec and Matrix

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create unit-aware vector and matrix types: (Vec 3 (Quantity Meters Real)). Ensure operations preserve dimensional correctness.

### Dependencies

- ⛔ **blocks**: `fold-a4i`
- ⛔ **blocks**: `fold-o7b`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5o0 -s in_progress

# Add a comment
bd comment fold-5o0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5o0 -p 1

# View full details
bd show fold-5o0
```

</details>

---

## 🚀 fold-u8r State Machines and Automata Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Explicit state modeling for protocols, games, and workflows.

Core Types:
1. DFA/NFA - deterministic/nondeterministic finite automata
2. PDA - pushdown automata
3. Mealy/Moore machines - output on transitions/states
4. Hierarchical state machines (statecharts)

Operations:
- Composition: product, union, concatenation
- Minimization and equivalence
- Regex to NFA to DFA pipeline
- Simulation and trace generation

DSL for Definition:
(state-machine game-state
  (state :idle
    (on :start -> :playing))
  (state :playing
    (on :pause -> :paused)
    (on :game-over -> :ended))
  ...)

Applications:
- Game state management (Loom SDK)
- Protocol verification
- Lexer generation
- UI state management
- Workflow engines

Location: fabric/stitches/automata/

### Dependencies

- ⛔ **blocks**: `fold-b0x`
- ⛔ **blocks**: `fold-dgs`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-u8r -s in_progress

# Add a comment
bd comment fold-u8r 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-u8r -p 1

# View full details
bd show fold-u8r
```

</details>

---

## 🚀 fold-rm6 Declarative Graphics Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

SVG-like declarative graphics primitives for visualization.

Core Primitives:
1. Shapes: circle, rect, line, polyline, polygon, path, text
2. Transforms: translate, rotate, scale, skew, matrix
3. Styling: fill, stroke, opacity, gradient, pattern
4. Composition: group, layer, clip, mask

Layout Combinators:
- beside, above, atop (Diagrams-style)
- distribute, align, center
- connect, arrow, edge

Rendering Backends:
- SVG output (primary)
- ASCII art (terminal)
- Block-based (for CAS storage)

Applications:
- Graph visualization (force-directed, hierarchical)
- Physics simulation rendering
- Mathematical plots and diagrams
- Neural network architecture diagrams
- Game graphics

Location: thimble/graphics/

### Dependencies

- ⛔ **blocks**: `fold-kg8`
- ⛔ **blocks**: `fold-hrp`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-rm6 -s in_progress

# Add a comment
bd comment fold-rm6 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-rm6 -p 1

# View full details
bd show fold-rm6
```

</details>

---

## 🚀 fold-h4x Contracts and Verification Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Design by contract with runtime/compile-time verification.

Contract Types:
1. Preconditions: (require (> x 0))
2. Postconditions: (ensure (sorted? result))
3. Invariants: (invariant (balanced? tree))
4. Dependent contracts: result depends on inputs

Enforcement Modes:
- Runtime checking with blame tracking
- Compile-time verification (with dependent types)
- Test-time only (for performance)
- Documentation only

Integration with Types:
- Refinement types: {x : Int | x > 0}
- Liquid types: inferred refinements
- Contract inference from examples

Features:
- Higher-order contract wrapping
- Blame assignment for violations
- Contract composition
- Statistical contract monitoring

Location: fabric/stitches/contract/

### Dependencies

- ⛔ **blocks**: `fold-w41`
- ⛔ **blocks**: `fold-go1`
- ⛔ **blocks**: `fold-hex`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-h4x -s in_progress

# Add a comment
bd comment fold-h4x 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-h4x -p 1

# View full details
bd show fold-h4x
```

</details>

---

## 🚀 fold-294 Zipper Data Structures Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Navigable data structures with O(1) local updates via zippers.

Core Zippers:
1. ListZipper - focus on list element
2. TreeZipper - navigate and edit trees
3. GenericZipper - derive zipper from any recursive type

Navigation:
- up, down, left, right
- root, leftmost, rightmost
- modify, replace, delete, insert

Integration with Lenses:
- Zippers as comonads (extend, extract)
- Lens composition with zipper focus
- Traverse to zipper and back

Applications:
- AST manipulation and refactoring
- Game world navigation (rooms, menus)
- Undo/redo via zipper history
- Document editing

Location: fabric/stitches/zipper/

### Dependencies

- ⛔ **blocks**: `fold-a1v`
- ⛔ **blocks**: `fold-gup`
- ⛔ **blocks**: `fold-fht`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-294 -s in_progress

# Add a comment
bd comment fold-294 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-294 -p 1

# View full details
bd show fold-294
```

</details>

---

## 📋 fold-lzr Auto-parallelization + fusion hints

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:42 |
| **Updated** | 2026-01-02 02:46 |

### Description

Analyze pipelines for fusion, map/reduce parallelization, and list fusion opportunities. Offer safe rewrite suggestions.

### Dependencies

- ⛔ **blocks**: `fold-q7w`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-lzr -s in_progress

# Add a comment
bd comment fold-lzr 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-lzr -p 1

# View full details
bd show fold-lzr
```

</details>

---

## ✨ fold-com Equational reasoning + rewrite assistant

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:41 |
| **Updated** | 2026-01-02 02:46 |

### Description

Assist with algebraic/functional rewrites: apply laws (monoid, functor, monad), show transformation steps, and verify equivalence where possible.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-com -s in_progress

# Add a comment
bd comment fold-com 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-com -p 1

# View full details
bd show fold-com
```

</details>

---

## ✨ fold-h9p Implement physics simulation DSL via Free monad

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:36 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create physics simulation DSL using Free monad. Commands: ApplyForce, UpdatePosition, Collide, Spawn, Destroy. Multiple interpreters: deterministic, stochastic, visualization, logging.

### Dependencies

- ⛔ **blocks**: `fold-rho`
- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-h9p -s in_progress

# Add a comment
bd comment fold-h9p 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-h9p -p 1

# View full details
bd show fold-h9p
```

</details>

---

## ✨ fold-4ve Implement physics state lenses

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:35 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create lens library for physics entities: position-lens, velocity-lens, mass-lens, rotation-lens. Enable: (over position-lens (+ gravity-vec) particle), (view (. body position x) world).

### Dependencies

- ⛔ **blocks**: `fold-1yg`
- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-4ve -s in_progress

# Add a comment
bd comment fold-4ve 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-4ve -p 1

# View full details
bd show fold-4ve
```

</details>

---

## ✨ fold-ux4 Effect typing + linting

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:32 |
| **Updated** | 2026-01-02 02:46 |

### Description

Effect-aware linting and annotations: highlight impurity boundaries, enforce effect rules per module, and surface violations in REPL tooling.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ux4 -s in_progress

# Add a comment
bd comment fold-ux4 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ux4 -p 1

# View full details
bd show fold-ux4
```

</details>

---

## ✨ fold-ux0 Type-driven search helpers

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:32 |
| **Updated** | 2026-01-02 02:46 |

### Description

Given a type, enumerate in-scope candidates and combinator recipes. Expose search results to REPL and tooling APIs.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ux0 -s in_progress

# Add a comment
bd comment fold-ux0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ux0 -p 1

# View full details
bd show fold-ux0
```

</details>

---

## 📋 fold-um3 Dead code + unused binding detection

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:32 |
| **Updated** | 2026-01-02 02:46 |

### Description

Find unused bindings, dead branches, and unreachable definitions across modules. Provide safe-delete suggestions.

### Dependencies

- ⛔ **blocks**: `fold-z7y`
- ⛔ **blocks**: `fold-0wd`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-um3 -s in_progress

# Add a comment
bd comment fold-um3 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-um3 -p 1

# View full details
bd show fold-um3
```

</details>

---

## 📋 fold-jxx Pattern match exhaustiveness + redundancy checks

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Analyze pattern matches for missing cases and unreachable branches. Provide diagnostics and suggested fixes.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-jxx -s in_progress

# Add a comment
bd comment fold-jxx 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-jxx -p 1

# View full details
bd show fold-jxx
```

</details>

---

## ✨ fold-4as Totality + termination checker

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Static analysis for totality/termination in recursive functions. Flag potentially non-terminating definitions and suggest structurally recursive fixes.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-4as -s in_progress

# Add a comment
bd comment fold-4as 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-4as -p 1

# View full details
bd show fold-4as
```

</details>

---

## ✨ fold-75l Time-travel debugger + explain tracing

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

Interactive debugger for functional code: step/rewind, watch expressions, time-travel through evaluation, and explain-why traces for results. Provide REPL commands and structured trace data.

### Dependencies

- ⛔ **blocks**: `fold-1vz`
- ⛔ **blocks**: `fold-z7y`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-75l -s in_progress

# Add a comment
bd comment fold-75l 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-75l -p 1

# View full details
bd show fold-75l
```

</details>

---

## 📋 fold-x0k Implement evolutionary game theory

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:27 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement replicator dynamics, evolutionarily stable strategies (ESS), and population games. Support agent-based simulations and invasion dynamics.

### Dependencies

- ⛔ **blocks**: `fold-8ap`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-x0k -s in_progress

# Add a comment
bd comment fold-x0k 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-x0k -p 1

# View full details
bd show fold-x0k
```

</details>

---

## 📋 fold-8rh Implement mechanism design

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:27 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement auction mechanisms (first-price, second-price, VCG), voting systems, and matching markets. Support incentive compatibility and revelation principle analysis.

### Dependencies

- ⛔ **blocks**: `fold-8ap`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-8rh -s in_progress

# Add a comment
bd comment fold-8rh 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-8rh -p 1

# View full details
bd show fold-8rh
```

</details>

---

## 📋 fold-8f1 Implement cooperative games

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement coalitional games with characteristic functions. Support Shapley value computation, core stability, nucleolus, and bargaining solutions.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-8f1 -s in_progress

# Add a comment
bd comment fold-8f1 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-8f1 -p 1

# View full details
bd show fold-8f1
```

</details>

---

## 📋 fold-som Implement extensive form games

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement game trees with sequential moves. Support subgame perfect equilibrium, backward induction, information sets, and imperfect information games.

### Dependencies

- ⛔ **blocks**: `fold-8ap`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-som -s in_progress

# Add a comment
bd comment fold-som 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-som -p 1

# View full details
bd show fold-som
```

</details>

---

## 📋 fold-dzr Implement chaos detection and analysis

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement Lyapunov exponent computation, fractal dimension estimation, Poincaré sections. Support strange attractor visualization and sensitive dependence detection.

### Dependencies

- ⛔ **blocks**: `fold-1af`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-dzr -s in_progress

# Add a comment
bd comment fold-dzr 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-dzr -p 1

# View full details
bd show fold-dzr
```

</details>

---

## 📋 fold-moa Implement bifurcation analysis

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement bifurcation detection: saddle-node, pitchfork, Hopf, period-doubling. Support parameter continuation, bifurcation diagrams, and normal form computation.

### Dependencies

- ⛔ **blocks**: `fold-1af`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-moa -s in_progress

# Add a comment
bd comment fold-moa 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-moa -p 1

# View full details
bd show fold-moa
```

</details>

---

## 📋 fold-1af Implement stability analysis

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement fixed point detection and classification. Support linearization, eigenvalue analysis, Lyapunov function construction, and basin of attraction computation.

### Dependencies

- ⛔ **blocks**: `fold-fo1`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-1af -s in_progress

# Add a comment
bd comment fold-1af 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-1af -p 1

# View full details
bd show fold-1af
```

</details>

---

## 📋 fold-fo1 Implement ODE system representation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement continuous dynamical system representation. Support autonomous and non-autonomous systems, vector field visualization, phase portraits, and flow computation.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-fo1 -s in_progress

# Add a comment
bd comment fold-fo1 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-fo1 -p 1

# View full details
bd show fold-fo1
```

</details>

---

## 📋 fold-xl6 Implement global optimization

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement simulated annealing, genetic algorithms, particle swarm optimization. Support multi-objective optimization, Pareto frontiers, and black-box optimization.

### Dependencies

- ⛔ **blocks**: `fold-ygh`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-xl6 -s in_progress

# Add a comment
bd comment fold-xl6 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-xl6 -p 1

# View full details
bd show fold-xl6
```

</details>

---

## 📋 fold-5nf Implement constraint satisfaction

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement constraint propagation, arc consistency, and backtracking search. Support SAT encoding, clause learning, and domain reduction strategies.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5nf -s in_progress

# Add a comment
bd comment fold-5nf 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5nf -p 1

# View full details
bd show fold-5nf
```

</details>

---

## 📋 fold-bjg Implement integer programming

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement branch and bound, cutting plane methods, and mixed-integer programming. Support binary variables, lazy constraints, and heuristics for feasible solutions.

### Dependencies

- ⛔ **blocks**: `fold-c3a`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-bjg -s in_progress

# Add a comment
bd comment fold-bjg 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-bjg -p 1

# View full details
bd show fold-bjg
```

</details>

---

## 📋 fold-ygh Implement convex optimization

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement gradient descent variants (SGD, Adam, momentum), Newton's method, quasi-Newton methods (L-BFGS). Support line search, trust regions, and convergence criteria.

### Dependencies

- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ygh -s in_progress

# Add a comment
bd comment fold-ygh 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ygh -p 1

# View full details
bd show fold-ygh
```

</details>

---

## 📋 fold-c3a Implement linear programming

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement simplex method and interior point methods for linear programming. Support dual problems, sensitivity analysis, and constraint handling.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-c3a -s in_progress

# Add a comment
bd comment fold-c3a 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-c3a -p 1

# View full details
bd show fold-c3a
```

</details>

---

## 📋 fold-1vz Effect + flow inspector

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Trace effectful flows and data dependencies: show execution paths, effect boundaries, and call graph slices in the REPL.

### Dependencies

- ⛔ **blocks**: `fold-z7y`
- ⛔ **blocks**: `fold-ux4`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-1vz -s in_progress

# Add a comment
bd comment fold-1vz 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-1vz -p 1

# View full details
bd show fold-1vz
```

</details>

---

## 📋 fold-wza Implement spatial data structures

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement k-d trees, R-trees, quadtrees/octrees for spatial queries. Support range queries, nearest neighbor search, and spatial indexing for point sets.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-wza -s in_progress

# Add a comment
bd comment fold-wza 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-wza -p 1

# View full details
bd show fold-wza
```

</details>

---

## ✨ fold-cvd Refactoring engine (rename/extract/inline)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Safe refactors driven by syntax + symbol graph: rename, extract function, inline, introduce let, reorder args with call-site updates.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-cvd -s in_progress

# Add a comment
bd comment fold-cvd 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-cvd -p 1

# View full details
bd show fold-cvd
```

</details>

---

## 📋 fold-0mu Implement mesh generation and refinement

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement mesh generation from boundary representations. Support Delaunay refinement, mesh smoothing, quality improvement, and adaptive refinement.

### Dependencies

- ⛔ **blocks**: `fold-4az`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-0mu -s in_progress

# Add a comment
bd comment fold-0mu 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-0mu -p 1

# View full details
bd show fold-0mu
```

</details>

---

## 📋 fold-4az Implement Delaunay triangulation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement Delaunay triangulation: Bowyer-Watson algorithm, divide-and-conquer. Support constrained Delaunay, mesh quality metrics, and point location queries.

### Dependencies

- ⛔ **blocks**: `fold-gh0`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-4az -s in_progress

# Add a comment
bd comment fold-4az 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-4az -p 1

# View full details
bd show fold-4az
```

</details>

---

## 📋 fold-gh0 Implement Voronoi diagrams

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement Voronoi diagram construction: Fortune's algorithm for 2D, incremental for higher dimensions. Support Voronoi cell queries and power diagrams.

### Dependencies

- ⛔ **blocks**: `fold-dls`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-gh0 -s in_progress

# Add a comment
bd comment fold-gh0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-gh0 -p 1

# View full details
bd show fold-gh0
```

</details>

---

## 📋 fold-dls Implement convex hull algorithms

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement 2D and 3D convex hull algorithms: Graham scan, Quickhull, incremental. Support convex hull queries, extreme point computation, and convex decomposition.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-dls -s in_progress

# Add a comment
bd comment fold-dls 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-dls -p 1

# View full details
bd show fold-dls
```

</details>

---

## ✨ fold-moe Typed hole suggestions + synthesis

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Provide typed-hole completion: enumerate candidate expressions, local bindings, and simple synthesis recipes with ranked explanations.

### Dependencies

- ⛔ **blocks**: `fold-z7y`
- ⛔ **blocks**: `fold-ux0`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-moe -s in_progress

# Add a comment
bd comment fold-moe 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-moe -p 1

# View full details
bd show fold-moe
```

</details>

---

## 🚀 fold-k1z Advanced functional programming tooling

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create modern FP tooling for The Fold: code intelligence, refactoring, profiling, testing, and inspection. Focus on developer experience without third-party deps. Locations: thimble/ for UI + REPL commands, fabric/stitches/ for analysis passes.

### Dependencies

- ⛔ **blocks**: `fold-z7y`
- ⛔ **blocks**: `fold-moe`
- ⛔ **blocks**: `fold-cvd`
- ⛔ **blocks**: `fold-1vz`
- ⛔ **blocks**: `fold-q7w`
- ⛔ **blocks**: `fold-07e`
- ⛔ **blocks**: `fold-0wd`
- ⛔ **blocks**: `fold-75l`
- ⛔ **blocks**: `fold-4as`
- ⛔ **blocks**: `fold-jxx`
- ⛔ **blocks**: `fold-um3`
- ⛔ **blocks**: `fold-ux0`
- ⛔ **blocks**: `fold-ux4`
- ⛔ **blocks**: `fold-c1c`
- ⛔ **blocks**: `fold-7dx`
- ⛔ **blocks**: `fold-ge6`
- ⛔ **blocks**: `fold-qca`
- ⛔ **blocks**: `fold-u72`
- ⛔ **blocks**: `fold-com`
- ⛔ **blocks**: `fold-y2f`
- ⛔ **blocks**: `fold-lzr`
- ⛔ **blocks**: `fold-7s7`
- ⛔ **blocks**: `fold-ajm`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-k1z -s in_progress

# Add a comment
bd comment fold-k1z 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-k1z -p 1

# View full details
bd show fold-k1z
```

</details>

---

## 📋 fold-sib Implement modules and linear algebra over rings

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement modules as generalization of vector spaces over rings. Support module homomorphisms, quotient modules, tensor products. Include Smith normal form for integer matrices.

### Dependencies

- ⛔ **blocks**: `fold-5rn`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-sib -s in_progress

# Add a comment
bd comment fold-sib 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-sib -p 1

# View full details
bd show fold-sib
```

</details>

---

## 📋 fold-doz Implement polynomial algebra

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement polynomial rings over arbitrary coefficients. Support polynomial division, GCD, factorization, interpolation. Include multivariate polynomials and Gröbner bases.

### Dependencies

- ⛔ **blocks**: `fold-5rn`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-doz -s in_progress

# Add a comment
bd comment fold-doz 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-doz -p 1

# View full details
bd show fold-doz
```

</details>

---

## 📋 fold-64t Implement field operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement field structures and extensions. Support finite fields, algebraic extensions, characteristic computation. Include field automorphisms and Galois connections.

### Dependencies

- ⛔ **blocks**: `fold-5rn`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-64t -s in_progress

# Add a comment
bd comment fold-64t 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-64t -p 1

# View full details
bd show fold-64t
```

</details>

---

## 📋 fold-5rn Implement ring structures

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement rings with addition and multiplication. Support commutative rings, integral domains, unique factorization domains. Include ring homomorphisms and ideals.

### Dependencies

- ⛔ **blocks**: `fold-64f`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5rn -s in_progress

# Add a comment
bd comment fold-5rn 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5rn -p 1

# View full details
bd show fold-5rn
```

</details>

---

## 📋 fold-64f Implement group operations and properties

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement group structures: identity, inverse, associativity. Support cyclic groups, permutation groups, symmetry groups. Include subgroup testing and group homomorphisms.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-64f -s in_progress

# Add a comment
bd comment fold-64f 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-64f -p 1

# View full details
bd show fold-64f
```

</details>

---

## 📋 fold-6cm Implement hash function primitives

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Cryptographic hash foundations.

Features:
- Merkle-Damgård construction
- SHA-256 (extend existing)
- SHA-512
- SHA-3/Keccak sponge
- BLAKE2/BLAKE3
- HMAC construction

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-6cm -s in_progress

# Add a comment
bd comment fold-6cm 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-6cm -p 1

# View full details
bd show fold-6cm
```

</details>

---

## 📋 fold-5i2 Implement primality testing and generation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Prime number utilities.

Features:
- Trial division
- Miller-Rabin primality test
- Lucas primality test
- Prime generation
- Safe prime generation
- Sophie Germain primes

### Dependencies

- ⛔ **blocks**: `fold-n9i`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5i2 -s in_progress

# Add a comment
bd comment fold-5i2 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5i2 -p 1

# View full details
bd show fold-5i2
```

</details>

---

## 📋 fold-a2y Implement elliptic curve operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Elliptic curve mathematics.

Features:
- Weierstrass curve representation
- Point addition (affine coords)
- Point doubling
- Scalar multiplication
- Projective coordinates (optimization)
- Standard curves (P-256, secp256k1)
- Point validation

### Dependencies

- ⛔ **blocks**: `fold-duy`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-a2y -s in_progress

# Add a comment
bd comment fold-a2y 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-a2y -p 1

# View full details
bd show fold-a2y
```

</details>

---

## 📋 fold-duy Implement finite field arithmetic

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Galois field operations.

Features:
- GF(p) prime field arithmetic
- GF(2^n) binary field arithmetic
- Polynomial representation
- Field extension construction
- Irreducible polynomial selection
- Field element inversion

### Dependencies

- ⛔ **blocks**: `fold-n9i`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-duy -s in_progress

# Add a comment
bd comment fold-duy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-duy -p 1

# View full details
bd show fold-duy
```

</details>

---

## 📋 fold-n9i Implement modular arithmetic

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Foundation for number-theoretic crypto.

Features:
- Modular addition, subtraction, multiplication
- Modular exponentiation (square-and-multiply)
- Extended Euclidean algorithm
- Modular inverse
- Chinese Remainder Theorem
- Montgomery multiplication (optimization)

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-n9i -s in_progress

# Add a comment
bd comment fold-n9i 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-n9i -p 1

# View full details
bd show fold-n9i
```

</details>

---

## 📋 fold-1nb Implement discrete control systems

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Digital control theory.

Features:
- Z-transform
- Discrete state space
- Discrete transfer functions
- Discretization methods (ZOH, Tustin)
- Digital PID
- Sample rate selection

### Dependencies

- ⛔ **blocks**: `fold-7lw`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-1nb -s in_progress

# Add a comment
bd comment fold-1nb 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-1nb -p 1

# View full details
bd show fold-1nb
```

</details>

---

## 📋 fold-84e Implement stability analysis

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Analyze system stability.

Features:
- Lyapunov stability
- Routh-Hurwitz criterion
- Root locus computation
- Nyquist criterion
- Gain and phase margins
- BIBO stability

### Dependencies

- ⛔ **blocks**: `fold-rd5`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-84e -s in_progress

# Add a comment
bd comment fold-84e 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-84e -p 1

# View full details
bd show fold-84e
```

</details>

---

## 📋 fold-cyp Implement controller design

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Design controllers for systems.

Features:
- PID controller design
- Pole placement
- State feedback
- Observer design
- LQR (Linear Quadratic Regulator)
- LQG (with Kalman filter)
- H-infinity basics

### Dependencies

- ⛔ **blocks**: `fold-7lw`
- ⛔ **blocks**: `fold-84e`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-cyp -s in_progress

# Add a comment
bd comment fold-cyp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-cyp -p 1

# View full details
bd show fold-cyp
```

</details>

---

## 📋 fold-rd5 Implement transfer functions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Frequency domain representation.

Features:
- Transfer function representation
- Poles and zeros extraction
- Pole-zero plots
- Frequency response (Bode)
- State space ↔ transfer function conversion
- Series/parallel/feedback connections

### Dependencies

- ⛔ **blocks**: `fold-7lw`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-rd5 -s in_progress

# Add a comment
bd comment fold-rd5 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-rd5 -p 1

# View full details
bd show fold-rd5
```

</details>

---

## 📋 fold-jey Implement time stepping schemes

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Time integration for PDEs.

Features:
- Forward Euler (explicit)
- Backward Euler (implicit)
- Crank-Nicolson
- Method of lines
- Runge-Kutta for PDEs
- Stability regions
- Adaptive time stepping

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-jey -s in_progress

# Add a comment
bd comment fold-jey 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-jey -p 1

# View full details
bd show fold-jey
```

</details>

---

## 📋 fold-hoo Implement mesh generation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Generate computational meshes.

Features:
- 2D triangular mesh generation
- 3D tetrahedral mesh generation
- Mesh quality metrics
- Mesh refinement
- Adaptive mesh refinement
- Boundary mesh extraction
- Mesh file I/O

### Dependencies

- ⛔ **blocks**: `fold-01l`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-hoo -s in_progress

# Add a comment
bd comment fold-hoo 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-hoo -p 1

# View full details
bd show fold-hoo
```

</details>

---

## 📋 fold-y6c Implement finite element method basics

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

FEM for PDEs.

Features:
- Mesh data structures
- Basis function definition
- Local element matrices
- Global assembly
- Boundary condition application
- Linear system solution
- Error estimation

### Dependencies

- ⛔ **blocks**: `fold-hoo`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-y6c -s in_progress

# Add a comment
bd comment fold-y6c 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-y6c -p 1

# View full details
bd show fold-y6c
```

</details>

---

## 📋 fold-qdz Implement finite difference methods

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Classic numerical PDE approach.

Features:
- Central, forward, backward differences
- 1D, 2D, 3D stencils
- Boundary condition handling
- Stability analysis (CFL)
- Heat equation solver
- Wave equation solver
- Laplace/Poisson solver

### Dependencies

- ⛔ **blocks**: `fold-jey`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-qdz -s in_progress

# Add a comment
bd comment fold-qdz 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-qdz -p 1

# View full details
bd show fold-qdz
```

</details>

---

## 📋 fold-e3n Implement symbolic equation solving

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Solve equations symbolically.

Features:
- Linear equation solving
- Polynomial root finding
- Systems of equations
- Substitution method
- Gaussian elimination (symbolic)
- Quadratic formula
- Cubic/quartic formulas

### Dependencies

- ⛔ **blocks**: `fold-vnn`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-e3n -s in_progress

# Add a comment
bd comment fold-e3n 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-e3n -p 1

# View full details
bd show fold-e3n
```

</details>

---

## 📋 fold-vnn Implement algebraic simplification

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Simplify symbolic expressions.

Features:
- Collect like terms
- Expand products
- Factor expressions
- Trigonometric identities
- Logarithm/exponential rules
- Canonical form conversion
- Simplification heuristics

### Dependencies

- ⛔ **blocks**: `fold-e78`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-vnn -s in_progress

# Add a comment
bd comment fold-vnn 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-vnn -p 1

# View full details
bd show fold-vnn
```

</details>

---

## 📋 fold-arf Implement symbolic integration

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Compute antiderivatives symbolically.

Features:
- Basic antiderivatives
- Integration by parts
- Substitution (u-substitution)
- Partial fractions
- Trigonometric substitutions
- Table-based lookup
- Definite integral evaluation

### Dependencies

- ⛔ **blocks**: `fold-e78`
- ⛔ **blocks**: `fold-bwy`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-arf -s in_progress

# Add a comment
bd comment fold-arf 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-arf -p 1

# View full details
bd show fold-arf
```

</details>

---

## 📋 fold-bwy Implement symbolic differentiation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Compute derivatives symbolically.

Features:
- Derivative rules (sum, product, quotient, chain)
- Partial derivatives
- Gradient computation
- Jacobian matrix
- Hessian matrix
- Higher-order derivatives
- Implicit differentiation

### Dependencies

- ⛔ **blocks**: `fold-e78`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-bwy -s in_progress

# Add a comment
bd comment fold-bwy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-bwy -p 1

# View full details
bd show fold-bwy
```

</details>

---

## 📋 fold-0dk Implement monads and Kleisli categories

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Computational effects categorically.

Features:
- Monad definition (unit, join)
- Kleisli category construction
- Monad laws verification
- Common monads (Maybe, List, State)
- Monad transformers
- Free monads
- Algebras over a monad

### Dependencies

- ⛔ **blocks**: `fold-9z2`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-0dk -s in_progress

# Add a comment
bd comment fold-0dk 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-0dk -p 1

# View full details
bd show fold-0dk
```

</details>

---

## 📋 fold-9z2 Implement adjoint functors

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Fundamental categorical structure.

Features:
- Adjunction definition (unit/counit)
- Hom-set bijection formulation
- Free/forgetful adjunctions
- Adjunction composition
- Uniqueness of adjoints
- Galois connections

### Dependencies

- ⛔ **blocks**: `fold-fmo`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-9z2 -s in_progress

# Add a comment
bd comment fold-9z2 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-9z2 -p 1

# View full details
bd show fold-9z2
```

</details>

---

## 📋 fold-4z0 Implement limits and colimits

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Universal constructions.

Features:
- Products and coproducts
- Equalizers and coequalizers
- Pullbacks and pushouts
- Limits and colimits (general)
- Preservation by functors
- Limit computation algorithms

### Dependencies

- ⛔ **blocks**: `fold-5dg`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-4z0 -s in_progress

# Add a comment
bd comment fold-4z0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-4z0 -p 1

# View full details
bd show fold-4z0
```

</details>

---

## 📋 fold-fmo Implement natural transformations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 02:46 |

### Description

Morphisms between functors.

Features:
- Natural transformation definition
- Naturality condition verification
- Horizontal composition
- Vertical composition
- Whiskering operations
- Natural isomorphisms

### Dependencies

- ⛔ **blocks**: `fold-5dg`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-fmo -s in_progress

# Add a comment
bd comment fold-fmo 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-fmo -p 1

# View full details
bd show fold-fmo
```

</details>

---

## 📋 fold-x9t Implement Lie groups and algebras

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:19 |
| **Updated** | 2026-01-02 02:46 |

### Description

Rotation and transformation groups.

Features:
- SO(2), SO(3) rotation groups
- SE(2), SE(3) rigid transformations
- Lie algebra (tangent at identity)
- Exponential map (algebra → group)
- Logarithm map (group → algebra)
- Adjoint representation
- Baker-Campbell-Hausdorff formula

### Dependencies

- ⛔ **blocks**: `fold-81x`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-x9t -s in_progress

# Add a comment
bd comment fold-x9t 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-x9t -p 1

# View full details
bd show fold-x9t
```

</details>

---

## 📋 fold-a71 Implement geodesic computation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:19 |
| **Updated** | 2026-01-02 02:46 |

### Description

Shortest paths on curved surfaces.

Features:
- Geodesic equations
- Numerical geodesic tracing
- Exponential map
- Logarithm map
- Parallel transport along geodesics
- Geodesic distance computation

### Dependencies

- ⛔ **blocks**: `fold-r1k`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-a71 -s in_progress

# Add a comment
bd comment fold-a71 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-a71 -p 1

# View full details
bd show fold-a71
```

</details>

---

## 📋 fold-r1k Implement curvature computations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:19 |
| **Updated** | 2026-01-02 02:46 |

### Description

Various curvature measures for surfaces and manifolds.

Features:
- Gaussian curvature (surfaces)
- Mean curvature
- Principal curvatures
- Riemann curvature tensor
- Ricci tensor and scalar curvature
- Christoffel symbols

### Dependencies

- ⛔ **blocks**: `fold-yql`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-r1k -s in_progress

# Add a comment
bd comment fold-r1k 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-r1k -p 1

# View full details
bd show fold-r1k
```

</details>

---

## 📋 fold-yql Implement tangent and cotangent spaces

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:19 |
| **Updated** | 2026-01-02 02:46 |

### Description

Tangent vectors and differential forms.

Features:
- Tangent vector representation
- Tangent space at a point
- Cotangent vectors (1-forms)
- Pushforward and pullback operations
- Tangent bundle construction

### Dependencies

- ⛔ **blocks**: `fold-8t7`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-yql -s in_progress

# Add a comment
bd comment fold-yql 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-yql -p 1

# View full details
bd show fold-yql
```

</details>

---

## 📋 fold-8t7 Implement coordinate charts and atlases

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:19 |
| **Updated** | 2026-01-02 02:46 |

### Description

Foundation for manifold representation.

Features:
- Chart data structure (domain, codomain, map)
- Atlas as collection of compatible charts
- Transition functions between charts
- Jacobian of transition maps
- Chart compatibility checking

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-8t7 -s in_progress

# Add a comment
bd comment fold-8t7 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-8t7 -p 1

# View full details
bd show fold-8t7
```

</details>

---

## 🚀 fold-tuq Dynamical Systems Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:17 |
| **Updated** | 2026-01-02 02:46 |

### Description

Analysis of dynamical systems and chaos.

Core Concepts:
1. ODE Systems
   - Phase portraits
   - Fixed points
   - Stability classification
   - Linearization

2. Stability Analysis
   - Lyapunov functions
   - Basin of attraction
   - Limit cycles
   - Strange attractors

3. Bifurcation Theory
   - Saddle-node
   - Hopf bifurcation
   - Period doubling
   - Bifurcation diagrams

4. Chaos
   - Lyapunov exponents
   - Fractal dimension
   - Poincaré sections
   - Sensitive dependence

5. Discrete Maps
   - Iterated maps
   - Logistic map
   - Hénon map

Applications:
- Population dynamics
- Climate modeling
- Neural dynamics
- Economic models

Location: fabric/stitches/dynamics/

### Dependencies

- ⛔ **blocks**: `fold-uv4`
- ⛔ **blocks**: `fold-3lh`
- ⛔ **blocks**: `fold-fo1`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-tuq -s in_progress

# Add a comment
bd comment fold-tuq 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-tuq -p 1

# View full details
bd show fold-tuq
```

</details>

---

## 🚀 fold-pd2 Optimization Library Epic (Convex and Linear)

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:17 |
| **Updated** | 2026-01-02 02:46 |

### Description

Mathematical optimization beyond gradient descent.

Core Methods:
1. Linear Programming
   - Simplex method
   - Interior point methods
   - Dual problems
   - Sensitivity analysis

2. Convex Optimization
   - Proximal operators
   - ADMM
   - Barrier methods
   - Conic programming

3. Integer Programming
   - Branch and bound
   - Cutting planes
   - MIP formulations

4. Constraint Satisfaction
   - Feasibility problems
   - Projection methods
   - Constraint propagation

5. Combinatorial Optimization
   - TSP, knapsack
   - Graph optimization
   - Scheduling

Applications:
- Resource allocation
- Network flow
- Portfolio optimization
- Operations research

Location: fabric/stitches/optim/

### Dependencies

- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-vy8`
- ⛔ **blocks**: `fold-i5d`
- ⛔ **blocks**: `fold-1e0`
- ⛔ **blocks**: `fold-ygh`
- ⛔ **blocks**: `fold-c3a`
- ⛔ **blocks**: `fold-bjg`
- ⛔ **blocks**: `fold-5nf`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-pd2 -s in_progress

# Add a comment
bd comment fold-pd2 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-pd2 -p 1

# View full details
bd show fold-pd2
```

</details>

---

## 🚀 fold-5mb Advanced Computational Geometry Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:16 |
| **Updated** | 2026-01-02 02:46 |

### Description

Advanced geometric algorithms beyond basic primitives.

Core Algorithms:
1. Convex Hull
   - Graham scan
   - Quickhull
   - Incremental algorithms
   - 3D convex hull

2. Voronoi Diagrams
   - Fortune's algorithm
   - Voronoi cells
   - Nearest neighbor queries
   - Weighted Voronoi

3. Delaunay Triangulation
   - Dual of Voronoi
   - Incremental construction
   - Constrained Delaunay
   - 3D triangulation

4. Mesh Generation
   - Triangle meshes
   - Tetrahedral meshes
   - Quality metrics
   - Mesh refinement

5. Spatial Queries
   - Range queries
   - k-NN queries
   - Ray casting
   - Spatial indexing (k-d trees, R-trees)

Applications:
- Computer graphics
- GIS/mapping
- Physics simulation
- Path planning

Location: fabric/stitches/compgeo/

### Dependencies

- ⛔ **blocks**: `fold-01l`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-wza`
- ⛔ **blocks**: `fold-dls`
- ⛔ **blocks**: `fold-4az`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5mb -s in_progress

# Add a comment
bd comment fold-5mb 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5mb -p 1

# View full details
bd show fold-5mb
```

</details>

---

## 🚀 fold-6yu Abstract Algebra Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:16 |
| **Updated** | 2026-01-02 02:46 |

### Description

Algebraic structures for mathematics and CS.

Core Structures:
1. Groups
   - Group axioms verification
   - Subgroups
   - Cosets and quotients
   - Group homomorphisms
   - Symmetric/permutation groups

2. Rings
   - Ring axioms
   - Ideals
   - Polynomial rings
   - Quotient rings

3. Fields
   - Field axioms
   - Field extensions
   - Algebraic closure
   - Finite fields (link to crypto)

4. Polynomial Arithmetic
   - Polynomial representation
   - Add, multiply, divide
   - GCD (Euclidean algorithm)
   - Factorization
   - Roots

5. Linear Algebra over Rings
   - Modules
   - Smith normal form
   - Hermite normal form

Applications:
- Cryptography (finite fields)
- Coding theory
- Computational topology (homology)
- Symbolic computation

Location: fabric/stitches/algebra/

### Dependencies

- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-1e0`
- ⛔ **blocks**: `fold-u3i`
- ⛔ **blocks**: `fold-64f`
- ⛔ **blocks**: `fold-5rn`
- ⛔ **blocks**: `fold-64t`
- ⛔ **blocks**: `fold-doz`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-6yu -s in_progress

# Add a comment
bd comment fold-6yu 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-6yu -p 1

# View full details
bd show fold-6yu
```

</details>

---

## 🚀 fold-iki Cryptographic Primitives Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:16 |
| **Updated** | 2026-01-02 02:46 |

### Description

Mathematical foundations for cryptography.

Core Features:
1. Modular Arithmetic
   - Extended Euclidean algorithm
   - Modular exponentiation
   - Modular inverse
   - Chinese remainder theorem

2. Finite Fields
   - GF(p) arithmetic
   - GF(2^n) arithmetic
   - Polynomial arithmetic
   - Field extensions

3. Elliptic Curves
   - Point representation
   - Point addition/doubling
   - Scalar multiplication
   - Standard curves (secp256k1, etc.)

4. Number Theory
   - Primality testing
   - Prime generation
   - Factorization (for testing)
   - Discrete logarithm

5. Hash Functions
   - SHA family (extend existing SHA256)
   - BLAKE2/3
   - Keccak/SHA-3

Note: This is for mathematical foundations, not
complete crypto implementations (which need
constant-time guarantees, etc.)

Location: fabric/stitches/crypto-math/

### Dependencies

- ⛔ **blocks**: `fold-6yu`
- ⛔ **blocks**: `fold-3w7`
- ⛔ **blocks**: `fold-6cm`
- ⛔ **blocks**: `fold-n9i`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-iki -s in_progress

# Add a comment
bd comment fold-iki 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-iki -p 1

# View full details
bd show fold-iki
```

</details>

---

## 🚀 fold-dag Control Theory Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:16 |
| **Updated** | 2026-01-02 02:46 |

### Description

Control systems analysis and design.

Core Concepts:
1. State Space Models
   - State equations
   - Output equations
   - State transition matrix
   - Controllability/observability

2. Transfer Functions
   - Laplace transforms
   - Poles and zeros
   - Frequency response
   - Bode plots

3. Controller Design
   - PID control
   - State feedback
   - LQR optimal control
   - Pole placement

4. Stability Analysis
   - Lyapunov stability
   - Routh-Hurwitz
   - Nyquist criterion
   - Root locus

5. Digital Control
   - Z-transforms
   - Discrete state space
   - Digital PID
   - Sample and hold

Applications:
- Robotics
- Aerospace
- Process control
- Game physics

Location: fabric/stitches/control/

### Dependencies

- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-6nq`
- ⛔ **blocks**: `fold-vy8`
- ⛔ **blocks**: `fold-ac0`
- ⛔ **blocks**: `fold-7lw`
- ⛔ **blocks**: `fold-rd5`
- ⛔ **blocks**: `fold-cyp`
- ⛔ **blocks**: `fold-84e`
- ⛔ **blocks**: `fold-1nb`
- ⛔ **blocks**: `fold-tuq`
- ⛔ **blocks**: `fold-sim`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-dag -s in_progress

# Add a comment
bd comment fold-dag 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-dag -p 1

# View full details
bd show fold-dag
```

</details>

---

## 🚀 fold-j32 Numerical PDE Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:16 |
| **Updated** | 2026-01-02 02:46 |

### Description

Numerical methods for partial differential equations.

Methods:
1. Finite Difference Method
   - Central, forward, backward differences
   - Heat equation
   - Wave equation
   - Laplace equation

2. Finite Element Method
   - Mesh generation
   - Basis functions
   - Assembly
   - Boundary conditions

3. Spectral Methods
   - Fourier spectral
   - Chebyshev spectral
   - Pseudospectral methods

4. Time Stepping
   - Explicit methods
   - Implicit methods
   - Crank-Nicolson
   - Method of lines

Applications:
- Heat transfer
- Fluid dynamics
- Electromagnetism
- Quantum mechanics

Location: fabric/stitches/pde/

### Dependencies

- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-1ws`
- ⛔ **blocks**: `fold-uv4`
- ⛔ **blocks**: `fold-qdz`
- ⛔ **blocks**: `fold-y6c`
- ⛔ **blocks**: `fold-hoo`
- ⛔ **blocks**: `fold-jey`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-j32 -s in_progress

# Add a comment
bd comment fold-j32 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-j32 -p 1

# View full details
bd show fold-j32
```

</details>

---

## 🚀 fold-01j Symbolic Computation Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:16 |
| **Updated** | 2026-01-02 02:46 |

### Description

Computer algebra system capabilities for The Fold.

Core Features:
1. Expression Representation
   - S-expression based (natural for Scheme!)
   - Symbolic variables
   - Arithmetic expressions
   - Function expressions

2. Symbolic Differentiation
   - Derivative rules
   - Chain rule
   - Partial derivatives
   - Gradient, Jacobian, Hessian

3. Symbolic Integration
   - Basic antiderivatives
   - Integration by parts
   - Substitution
   - Table lookup

4. Simplification
   - Algebraic simplification
   - Trigonometric identities
   - Expand/factor
   - Collect terms

5. Equation Solving
   - Linear equations
   - Polynomial roots
   - Systems of equations
   - Symbolic matrix operations

The Fold is homoiconic - symbolic computation is natural!

Location: fabric/stitches/symbolic/

### Dependencies

- ⛔ **blocks**: `fold-6yu`
- ⛔ **blocks**: `fold-bym`
- ⛔ **blocks**: `fold-e78`
- ⛔ **blocks**: `fold-bwy`
- ⛔ **blocks**: `fold-arf`
- ⛔ **blocks**: `fold-vnn`
- ⛔ **blocks**: `fold-e3n`
- ⛔ **blocks**: `fold-1e0`
- ⛔ **blocks**: `fold-93g`
- ⛔ **blocks**: `fold-1g4`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-01j -s in_progress

# Add a comment
bd comment fold-01j 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-01j -p 1

# View full details
bd show fold-01j
```

</details>

---

## 🚀 fold-018 Category Theory Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:15 |
| **Updated** | 2026-01-02 02:46 |

### Description

Computational category theory - very Scheme-appropriate!

Core Concepts:
1. Categories
   - Objects and morphisms
   - Identity and composition
   - Functors between categories
   - Natural transformations

2. Universal Constructions
   - Products and coproducts
   - Limits and colimits
   - Pullbacks and pushouts
   - Equalizers

3. Adjunctions
   - Adjoint functors
   - Unit and counit
   - Free/forgetful adjunctions
   - Galois connections

4. Monads
   - Monad laws
   - Kleisli categories
   - Monad transformers
   - Free monads

5. Higher Categories (optional)
   - 2-categories
   - Bicategories
   - Higher morphisms

Applications:
- Type theory foundations
- Program semantics
- Database theory
- Functional programming patterns

The Fold is already categorical - this makes it explicit!

Location: fabric/stitches/category/

### Dependencies

- ⛔ **blocks**: `fold-2uv`
- ⛔ **blocks**: `fold-5dg`
- ⛔ **blocks**: `fold-fmo`
- ⛔ **blocks**: `fold-4z0`
- ⛔ **blocks**: `fold-9z2`
- ⛔ **blocks**: `fold-0dk`
- ⛔ **blocks**: `fold-u72`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-018 -s in_progress

# Add a comment
bd comment fold-018 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-018 -p 1

# View full details
bd show fold-018
```

</details>

---

## 🚀 fold-asz Differential Geometry Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:15 |
| **Updated** | 2026-01-02 02:46 |

### Description

Computational differential geometry for physics and graphics.

Core Concepts:
1. Manifolds
   - Coordinate charts
   - Atlas construction
   - Tangent spaces
   - Cotangent spaces

2. Curvature
   - Gaussian curvature
   - Mean curvature
   - Riemann curvature tensor
   - Ricci curvature

3. Geodesics
   - Geodesic equations
   - Shortest paths on surfaces
   - Parallel transport
   - Exponential/logarithm maps

4. Differential Forms
   - Exterior algebra
   - Wedge product
   - Exterior derivative
   - Integration on manifolds

5. Lie Groups and Algebras
   - SO(3), SE(3)
   - Lie brackets
   - Exponential map
   - Adjoint representation

Applications:
- Robotics (SE(3) kinematics)
- General relativity
- Computer graphics
- Shape analysis

Location: fabric/stitches/diffgeo/

### Dependencies

- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-bym`
- ⛔ **blocks**: `fold-8t7`
- ⛔ **blocks**: `fold-yql`
- ⛔ **blocks**: `fold-r1k`
- ⛔ **blocks**: `fold-a71`
- ⛔ **blocks**: `fold-x9t`
- ⛔ **blocks**: `fold-1e0`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-asz -s in_progress

# Add a comment
bd comment fold-asz 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-asz -p 1

# View full details
bd show fold-asz
```

</details>

---

## ✨ fold-gjr Implement persistent homology and TDA

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:15 |
| **Updated** | 2026-01-02 02:46 |

### Description

Topological data analysis via persistent homology.

Features:
- Rips complex construction from point cloud
- Persistence computation
- Persistence diagrams
- Barcodes
- Bottleneck distance
- Wasserstein distance

Visualization:
- Persistence diagram plots
- Barcode plots
- Persistence landscapes

### Dependencies

- ⛔ **blocks**: `fold-oxy`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-gjr -s in_progress

# Add a comment
bd comment fold-gjr 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-gjr -p 1

# View full details
bd show fold-gjr
```

</details>

---

## ✨ fold-oxy Implement homology computation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:15 |
| **Updated** | 2026-01-02 02:46 |

### Description

Compute homology groups and Betti numbers.

Features:
- Boundary matrix construction
- Smith normal form reduction
- Betti number computation
- Homology group generators
- Euler characteristic

Algorithms:
- Standard reduction algorithm
- Persistent homology reduction
- Incremental algorithms

### Dependencies

- ⛔ **blocks**: `fold-7rp`
- ⛔ **blocks**: `fold-81x`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-oxy -s in_progress

# Add a comment
bd comment fold-oxy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-oxy -p 1

# View full details
bd show fold-oxy
```

</details>

---

## ✨ fold-7rp Implement simplicial complex data structures

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:15 |
| **Updated** | 2026-01-02 02:46 |

### Description

Core data structures for computational topology.

Features:
- Simplex representation (vertices, dimension)
- Simplicial complex construction
- Face enumeration
- Boundary computation
- Filtration values
- Vertex/edge/face iteration

Operations:
- Add/remove simplices
- Check inclusion
- Skeleton extraction
- Star and link operations

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-7rp -s in_progress

# Add a comment
bd comment fold-7rp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-7rp -p 1

# View full details
bd show fold-7rp
```

</details>

---

## 🚀 fold-7gp Topology Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:15 |
| **Updated** | 2026-01-02 02:46 |

### Description

Algebraic and computational topology for data analysis and mathematics.

Core Concepts:
1. Simplicial Complexes
   - Vertices, edges, faces, simplices
   - Abstract simplicial complexes
   - Čech and Vietoris-Rips complexes
   - Filtrations

2. Homology
   - Chain complexes
   - Boundary operators
   - Betti numbers
   - Euler characteristic
   - Homology groups (Z coefficients)

3. Persistent Homology (TDA)
   - Persistence diagrams
   - Barcodes
   - Bottleneck/Wasserstein distance
   - Persistence landscapes

4. Topological Invariants
   - Connected components
   - Holes (1-dimensional)
   - Voids (2-dimensional)
   - Higher-dimensional features

Applications:
- Topological data analysis
- Shape analysis
- Sensor network coverage
- Materials science

Location: fabric/stitches/topology/

### Dependencies

- ⛔ **blocks**: `fold-7rp`
- ⛔ **blocks**: `fold-oxy`
- ⛔ **blocks**: `fold-gjr`
- ⛔ **blocks**: `fold-5mb`
- ⛔ **blocks**: `fold-i5d`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-7gp -s in_progress

# Add a comment
bd comment fold-7gp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-7gp -p 1

# View full details
bd show fold-7gp
```

</details>

---

## ✨ fold-6ys Implement special mathematical functions

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:09 |
| **Updated** | 2026-01-02 02:46 |

### Description

Standard special functions for scientific computing.

Features:
1. Gamma Functions
   - Gamma function Γ(z)
   - Log-gamma function
   - Digamma, polygamma
   - Beta function
   - Incomplete gamma/beta

2. Error Functions
   - erf, erfc
   - Inverse error function
   - Faddeeva function

3. Bessel Functions
   - Bessel J, Y (first, second kind)
   - Modified Bessel I, K
   - Spherical Bessel

4. Hypergeometric Functions
   - 2F1 hypergeometric
   - Confluent hypergeometric

5. Other Functions
   - Airy functions
   - Elliptic integrals
   - Legendre polynomials
   - Chebyshev polynomials

Applications:
- Physics (quantum mechanics, wave equations)
- Statistics (distributions)
- Signal processing

Location: fabric/stitches/special-functions.ss

### Dependencies

- ⛔ **blocks**: `fold-7nh`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-6ys -s in_progress

# Add a comment
bd comment fold-6ys 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-6ys -p 1

# View full details
bd show fold-6ys
```

</details>

---

## ✨ fold-p5d Implement interpolation and curve fitting

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:07 |
| **Updated** | 2026-01-02 02:46 |

### Description

Numerical interpolation and approximation methods.

Features:
1. Interpolation methods
   - Linear interpolation
   - Polynomial interpolation (Lagrange, Newton)
   - Spline interpolation (cubic, B-splines)
   - Hermite interpolation

2. Curve fitting
   - Least squares fitting
   - Polynomial fitting
   - Bezier curves
   - NURBS (Non-Uniform Rational B-Splines)

3. Approximation
   - Chebyshev approximation
   - Pade approximants
   - Minimax approximation

Applications:
- Graphics (smooth curves, animation)
- Physics (trajectory interpolation)
- Signal processing (resampling)
- Data visualization

Location: fabric/stitches/interpolate.ss

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-vy8`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-p5d -s in_progress

# Add a comment
bd comment fold-p5d 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-p5d -p 1

# View full details
bd show fold-p5d
```

</details>

---

## ✨ fold-bdg Implement differentiable signal processing

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Enable gradient computation through signal processing operations.

Features:
1. Differentiable FFT
   - Gradient flow through DFT/IDFT
   - Backprop through frequency domain

2. Differentiable filters
   - Learnable filter coefficients
   - Gradient through convolution

3. Use Cases:
   - Audio synthesis optimization
   - Signal denoising learning
   - Spectral feature learning

Integration:
- Wraps signal processing with autodiff
- Enables end-to-end differentiable pipelines

Requires: Signal processing library, autodiff engine

Location: signal-processing/differentiable/

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-bdg -s in_progress

# Add a comment
bd comment fold-bdg 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-bdg -p 1

# View full details
bd show fold-bdg
```

</details>

---

## 📋 fold-b1d Add channel capacity calculations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement channel capacity calculations for various channel models including binary symmetric, AWGN, and discrete memoryless channels

### Dependencies

- ⛔ **blocks**: `fold-dco`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-b1d -s in_progress

# Add a comment
bd comment fold-b1d 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-b1d -p 1

# View full details
bd show fold-b1d
```

</details>

---

## 📋 fold-gj0 Create statistical measures

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement KL divergence, Jensen-Shannon divergence, cross-entropy, and other information-theoretic statistical measures

### Dependencies

- ⛔ **blocks**: `fold-dco`
- ⛔ **blocks**: `fold-9c6`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-gj0 -s in_progress

# Add a comment
bd comment fold-gj0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-gj0 -p 1

# View full details
bd show fold-gj0
```

</details>

---

## 📋 fold-5ie Add coding theory functions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement Huffman coding, arithmetic coding, Lempel-Ziv compression, and channel coding algorithms

### Dependencies

- ⛔ **blocks**: `fold-dco`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-5ie -s in_progress

# Add a comment
bd comment fold-5ie 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-5ie -p 1

# View full details
bd show fold-5ie
```

</details>

---

## 📋 fold-l7o Add matrix operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement matrix operations (multiplication, inversion, determinant, eigenvalues) for high precision matrices

### Dependencies

- ⛔ **blocks**: `fold-tsz`
- ⛔ **blocks**: `fold-81x`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-l7o -s in_progress

# Add a comment
bd comment fold-l7o 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-l7o -p 1

# View full details
bd show fold-l7o
```

</details>

---

## 📋 fold-bi7 Create number theory utilities

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement primality testing, factorization, greatest common divisor, and other number theory algorithms for big integers

### Dependencies

- ⛔ **blocks**: `fold-tsz`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-bi7 -s in_progress

# Add a comment
bd comment fold-bi7 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-bi7 -p 1

# View full details
bd show fold-bi7
```

</details>

---

## ✨ fold-fwb Implement constraint graph for physics solver

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:58 |
| **Updated** | 2026-01-02 02:46 |

### Description

Use graph algorithms for constraint-based physics solving.

Features:
1. Model constraints as graph edges
2. Topological sort for sequential solving
3. Strongly connected components for rigid clusters
4. Graph coloring for parallel constraint solving

Use Cases:
- Joint/constraint systems
- Rigid body clusters
- Contact groups
- Dependency ordering

Integration:
- Uses graph algorithms library
- Constraint propagation via graph traversal
- Island detection via connected components

Requires: Graph algorithms library, physics engine

Location: physics/constraints/

### Dependencies

- ⛔ **blocks**: `fold-i5d`
- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-fwb -s in_progress

# Add a comment
bd comment fold-fwb 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-fwb -p 1

# View full details
bd show fold-fwb
```

</details>

---

## ✨ fold-qce Implement differentiable physics simulation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:58 |
| **Updated** | 2026-01-02 02:46 |

### Description

Enable gradient computation through physics simulations for optimization and learning.

Features:
1. Differentiable collision detection
2. Differentiable integration (RK4, implicit methods)
3. Gradient flow through contact/friction
4. Adjoint method for efficient backward pass

Use Cases:
- Physics parameter optimization
- Robot motion planning
- Game AI training
- System identification

Integration:
- Uses autodiff engine for gradient computation
- Physics primitives wrapped as differentiable operations
- Computational graph for simulation trajectory

Requires: 2D/3D physics engine, autodiff engine

Location: physics/differentiable/

### Dependencies

- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-qce -s in_progress

# Add a comment
bd comment fold-qce 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-qce -p 1

# View full details
bd show fold-qce
```

</details>

---

## 📋 fold-98v Probabilistic Programming Constructs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:57 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create probabilistic programming primitives, stochastic variable types, probabilistic graphical models, and inference DSL for The Fold ecosystem.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-98v -s in_progress

# Add a comment
bd comment fold-98v 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-98v -p 1

# View full details
bd show fold-98v
```

</details>

---

## 📋 fold-23u Statistical Models and Regression

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:57 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement linear regression, logistic regression, generalized linear models, time series analysis (ARIMA), and hypothesis testing frameworks.

### Dependencies

- ⛔ **blocks**: `fold-9c6`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-vy8`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-23u -s in_progress

# Add a comment
bd comment fold-23u 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-23u -p 1

# View full details
bd show fold-23u
```

</details>

---

## 📋 fold-wcr Wavelet Transform Implementation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:55 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement discrete wavelet transform (DWT), continuous wavelet transform (CWT), and multi-resolution analysis. Include common wavelet families (Daubechies, Haar, Morlet) and wavelet packet decomposition.

### Dependencies

- ⛔ **blocks**: `fold-o7b`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-wcr -s in_progress

# Add a comment
bd comment fold-wcr 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-wcr -p 1

# View full details
bd show fold-wcr
```

</details>

---

## 📋 fold-x0z Window Functions and Spectral Analysis

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:55 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement window functions (Hamming, Hanning, Blackman, Kaiser), spectrogram generation, power spectral density estimation, and frequency domain analysis tools.

### Dependencies

- ⛔ **blocks**: `fold-dnu`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-7nh`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-x0z -s in_progress

# Add a comment
bd comment fold-x0z 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-x0z -p 1

# View full details
bd show fold-x0z
```

</details>

---

## 📋 fold-evy Implement 3D Collision Response

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create 3D collision response: impulse-based resolution with angular momentum, rolling friction, 3D constraints

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-evy -s in_progress

# Add a comment
bd comment fold-evy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-evy -p 1

# View full details
bd show fold-evy
```

</details>

---

## 📋 fold-7hs Implement 3D Collision Detection

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create 3D collision detection: sphere-sphere, sphere-box, box-box, OBB-OBB, GJK algorithm support

### Dependencies

- ⛔ **blocks**: `fold-01l`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-7hs -s in_progress

# Add a comment
bd comment fold-7hs 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-7hs -p 1

# View full details
bd show fold-7hs
```

</details>

---

## 📋 fold-1oe Implement 3D Vector and Quaternion Math

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create vector3D, quaternion structs with operations: rotation, angular velocity, orientation, SLERP

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-e1l`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-1oe -s in_progress

# Add a comment
bd comment fold-1oe 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-1oe -p 1

# View full details
bd show fold-1oe
```

</details>

---

## ✨ fold-g6n Implement differentiable type constructor with dependent tracking

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Extend differentiable type system to use dependent types for full dimension tracking.

Type Constructor:
  Diff A B  ; Type of differentiable functions from A to B
  Diff (Vec n Float) (Vec m Float)  ; n-input, m-output differentiable function

Features:
1. Dimension-aware differentiation
   - grad : Diff (Vec n) (Vec 1) → Vec n → Vec n
   - jacobian : Diff (Vec n) (Vec m) → Vec n → Matrix m n
   - hessian : Diff (Vec n) (Vec 1) → Vec n → Matrix n n

2. Composition rules with types
   - compose : Diff B C → Diff A B → Diff A C
   - Chain rule dimensions verified by types

3. Type inference for gradients
   - Infer output dimensions from input dimensions
   - Automatic mode selection (forward vs reverse)
   - JVP/VJP types

Integration:
- Extends fold-uw9 (differentiable type extensions)
- Uses dependent types for dimensions
- Works with linalg vectors/matrices

Benefits:
- Dimension errors at compile time
- Self-documenting differentiable code
- Optimization opportunities

Requires: Dependent types, autodiff type extensions, linalg

Location: fabric/stitches/ (extend differentiable types)

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-7yh`
- ⛔ **blocks**: `fold-uw9`
- ⛔ **blocks**: `fold-thv`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-g6n -s in_progress

# Add a comment
bd comment fold-g6n 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-g6n -p 1

# View full details
bd show fold-g6n
```

</details>

---

## ✨ fold-tg0 Implement typed graph properties and invariants

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Use dependent types to encode and verify graph properties statically.

Properties as Types:
1. Structural properties
   - Acyclic g : Type (proof that g has no cycles)
   - Connected g : Type (proof that g is connected)
   - Tree g : Type (connected and acyclic)
   - DAG g : Type (directed acyclic graph)

2. Property-preserving operations
   - add-edge : (g : Graph) → (e : Edge) → Acyclic g → Either (Acyclic (add e g)) CycleProof
   - topological-sort : (g : DAG) → SortedNodes g

3. Path types
   - Path g v1 v2 : Type (proof of path from v1 to v2)
   - Connected g ≡ ∀ v1 v2. Path g v1 v2

Integration:
- Wraps graph library with dependent type layer
- Enables verified graph algorithms
- Properties as first-class values

Applications:
- Verified topological sort (for autodiff)
- Cycle detection with proofs
- Spanning tree extraction with proof

Requires: Dependent types, equality types, graph library

Location: fabric/stitches/ (new dependent-graph module)

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-755`
- ⛔ **blocks**: `fold-i5d`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-tg0 -s in_progress

# Add a comment
bd comment fold-tg0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-tg0 -p 1

# View full details
bd show fold-tg0
```

</details>

---

## 🚀 fold-z8q Create 3D Physics Engine Core Architecture

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Design and implement the core architecture for a 3D physics engine extending the 2D foundation with spatial operations and quaternions

### Dependencies

- ⛔ **blocks**: `fold-1oe`
- ⛔ **blocks**: `fold-7hs`
- ⛔ **blocks**: `fold-evy`
- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-f2j`
- ⛔ **blocks**: `fold-73p`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-z8q -s in_progress

# Add a comment
bd comment fold-z8q 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-z8q -p 1

# View full details
bd show fold-z8q
```

</details>

---

## ✨ fold-ouh Implement type-safe gradient dimensions for autodiff

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Use dependent types to track tensor dimensions through autodiff computations.

Features:
1. Gradient dimension tracking
   - grad f : Tensor [d1...dn] → Tensor [d1...dn] (same shape)
   - jacobian f : Tensor [d1...] → Tensor [d1... × e1...] 
   - hessian f : Tensor [d1...] → Tensor [d1... × d1...]

2. Type-safe backpropagation
   - Dimension mismatches caught at compile time
   - Broadcast rules in types
   - Reduce operations change dimensions

3. Computation graph types
   - Nodes carry dimension information
   - Edge connections verified by types
   - Forward/backward dimension compatibility

Integration:
- Wraps autodiff with dependent type layer
- Gradual adoption
- Clear error messages for dimension mismatches

Benefits:
- Catch shape errors before runtime
- Self-documenting tensor operations
- Enables dimension-aware optimizations

Requires: Dependent types, autodiff, linalg

Location: fabric/stitches/ (extend autodiff types)

### Dependencies

- ⛔ **blocks**: `fold-thv`
- ⛔ **blocks**: `fold-uw9`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ouh -s in_progress

# Add a comment
bd comment fold-ouh 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ouh -p 1

# View full details
bd show fold-ouh
```

</details>

---

## 🚀 fold-yka 3D Physics Engine Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

3D physics engine with spatial data structures, advanced collision detection, rigid body dynamics, soft body physics, fluid simulation, and integration with 3D graphics systems.

### Dependencies

- ⛔ **blocks**: `fold-z8q`
- ⛔ **blocks**: `fold-yy5`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-yka -s in_progress

# Add a comment
bd comment fold-yka 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-yka -p 1

# View full details
bd show fold-yka
```

</details>

---

## 🚀 fold-yy5 2D Physics Engine Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

2D physics engine with rigid body dynamics, collision detection and response, constraints, forces, joints, particle systems, and integration methods suitable for games and simulations.

### Dependencies

- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-yy5 -s in_progress

# Add a comment
bd comment fold-yy5 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-yy5 -p 1

# View full details
bd show fold-yy5
```

</details>

---

## 🚀 fold-ac0 Signal Processing Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Signal processing library with Fourier transforms, wavelet analysis, digital filters, windowing functions, convolution operations, spectral analysis, and audio/signal manipulation capabilities.

### Dependencies

- ⛔ **blocks**: `fold-dnu`
- ⛔ **blocks**: `fold-cmk`
- ⛔ **blocks**: `fold-x0z`
- ⛔ **blocks**: `fold-wcr`
- ⛔ **blocks**: `fold-go9`
- ⛔ **blocks**: `fold-1e0`
- ⛔ **blocks**: `fold-6tr`
- ⛔ **blocks**: `fold-bdg`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ac0 -s in_progress

# Add a comment
bd comment fold-ac0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ac0 -p 1

# View full details
bd show fold-ac0
```

</details>

---

## 🚀 fold-qk1 Probabilistic Modeling Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Comprehensive probabilistic modeling library including Bayesian inference, Monte Carlo methods, MCMC samplers, probability distributions, statistical models, and probabilistic programming constructs for The Fold ecosystem.

### Dependencies

- ⛔ **blocks**: `fold-9c6`
- ⛔ **blocks**: `fold-5vq`
- ⛔ **blocks**: `fold-e2n`
- ⛔ **blocks**: `fold-23u`
- ⛔ **blocks**: `fold-98v`
- ⛔ **blocks**: `fold-1e0`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-qk1 -s in_progress

# Add a comment
bd comment fold-qk1 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-qk1 -p 1

# View full details
bd show fold-qk1
```

</details>

---

## ✨ fold-rfc Integrate dependent types with module system

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Update module system to work with dependent types.

Features:
1. Export dependent types and type families
2. Module signatures with dependent types
3. First-class modules (optional)
4. Parametric modules (functors)

Implementation:
- Extend module signatures for Pi/Sigma types
- Handle abstract types with dependencies
- Module subtyping with dependent types
- Separate compilation considerations

Challenges:
- Phase distinction (types vs values)
- Sealing dependent types
- Module equality
- Incremental type checking

Integration:
- Update module.ss for dependent types
- Handle imports of dependent definitions
- Preserve abstraction boundaries

Testing:
- Module with dependent exports
- Functor application
- Abstract dependent types

Location: fabric/stitches/module.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-qna`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-rfc -s in_progress

# Add a comment
bd comment fold-rfc 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-rfc -p 1

# View full details
bd show fold-rfc
```

</details>

---

## ✨ fold-cfv Implement inductive type definitions

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 02:46 |

### Description

Enable users to define custom inductive types with dependent indices.

Syntax:
  (data (Vec (n : Nat) A)
    [nil  : (Vec 0 A)]
    [cons : (Π (x : A) (Π (xs : Vec n A) (Vec (succ n) A)))])

Features:
1. Indexed inductive families
2. Parameters vs indices distinction
3. Strict positivity checking
4. Automatic eliminator generation

Implementation:
- Parse data declarations
- Check well-formedness (positivity, universe levels)
- Generate constructors with correct types
- Generate dependent eliminator (recursor)

Derived Forms:
- Simple datatypes (non-indexed)
- Records as single-constructor types
- Newtypes as single-constructor single-field

Built-in Inductives:
- Nat, Bool, List (promote to proper inductives)
- Unit, Void, Either
- Sigma, Equality

Eliminators:
- Structural recursion principle
- Case analysis (non-recursive)
- Pattern matching desugars to eliminator

Location: fabric/stitches/types.ss, compile.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-7yh`
- ⛔ **blocks**: `fold-w5k`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-cfv -s in_progress

# Add a comment
bd comment fold-cfv 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-cfv -p 1

# View full details
bd show fold-cfv
```

</details>

---

## ✨ fold-b8v Implement termination checking for type-level computation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 02:46 |

### Description

Ensure type-level computation terminates to maintain decidable type checking.

Approaches:
1. Syntactic termination: structural recursion on arguments
2. Sized types: track sizes in types
3. Well-founded recursion: explicit termination proofs
4. Fuel-based limits (already in The Fold)

Implementation:
- Identify recursive calls in type-level code
- Check structural decrease on at least one argument
- Handle mutual recursion
- Integration with existing fuel system

Checks:
- Direct structural recursion
- Lexicographic ordering
- Size-change termination principle
- Explicit termination measures

Error Handling:
- Clear error for non-terminating definitions
- Suggest fixes (add termination proof)
- Allow escape hatch with explicit fuel

Advanced:
- Coinductive types (productivity instead of termination)
- Partiality monad for general recursion
- Termination proofs as first-class values

Location: fabric/stitches/infer.ss, compile.ss

### Dependencies

- ⛔ **blocks**: `fold-7yh`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-b8v -s in_progress

# Add a comment
bd comment fold-b8v 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-b8v -p 1

# View full details
bd show fold-b8v
```

</details>

---

## ✨ fold-9dy Implement dependent pattern matching

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 02:46 |

### Description

Extend pattern matching to handle dependent types with proof obligations.

Features:
1. Pattern matching with type refinement
2. Coverage checking with dependent types
3. Inaccessible patterns (dot patterns)
4. With-abstraction for complex matches

Implementation:
- Match on indexed data: pattern determines index
- Unification of indices in patterns
- Generate proof obligations from patterns
- Handle impossible cases (absurd patterns)

Syntax:
  (match e
    [(zero) ...]                      ; Refines n to 0
    [(succ n') ...])                  ; Refines n to (succ n')

  (match (v : Vec n A)
    [nil ...]                         ; Refines n to 0
    [(cons x xs) ...])                ; Refines n to (succ m)

Advanced:
- Views for custom pattern matching
- As-patterns in dependent context
- Guards with proof obligations
- Overlapping patterns (first match)

Challenges:
- Index unification
- Case tree compilation
- Termination of matching
- Good error messages

Location: fabric/stitches/compile.ss, eval.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-cfv`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-9dy -s in_progress

# Add a comment
bd comment fold-9dy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-9dy -p 1

# View full details
bd show fold-9dy
```

</details>

---

## ✨ fold-755 Implement equality types and proofs

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement propositional equality type and proof terms.

Syntax:
  (= A x y)  ; Propositional equality: x equals y at type A
  refl  ; Proof of (= A x x)
  (J P d p)  ; Elimination (transport along equality)

Implementation:
1. Type formation: (= A x y) : Type when x:A and y:A
2. Introduction: refl : (= A x x)
3. Elimination (J): transport proofs across equal types
4. Computation: J P d refl ≡ d

Features:
- Symmetry: (= A x y) → (= A y x)
- Transitivity: (= A x y) → (= A y z) → (= A x z)
- Congruence: (= A x y) → (= B (f x) (f y))
- Substitution: (= A x y) → P x → P y

Variants:
- Heterogeneous equality (for dependent types)
- Decidable equality for base types
- Path types (HoTT-style, optional)

Use Cases:
- Prove program correctness
- Type-safe casts
- Dependent pattern matching
- Vector append length proof

Testing:
- Basic equality proofs
- Transport across equalities
- Congruence proofs

Location: fabric/stitches/types.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-7yh`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-755 -s in_progress

# Add a comment
bd comment fold-755 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-755 -p 1

# View full details
bd show fold-755
```

</details>

---

## ✨ fold-8wy Implement refinement types

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement refinement types: types annotated with predicates.

Syntax:
  {x : T | P}  ; Values of type T where predicate P holds
  {n : Nat | n > 0}  ; Positive naturals
  {v : Vec n Int | sorted v}  ; Sorted vectors

Implementation:
1. Extend type grammar for refinement types
2. Subtyping: {x:T | P} <: {x:T | Q} when P implies Q
3. Introduction: prove predicate when constructing
4. Elimination: extract predicate as assumption

Predicate Language:
- Boolean expressions over base types
- Arithmetic comparisons (=, <, >, <=, >=)
- Logical connectives (and, or, not, implies)
- Quantifiers over finite domains
- User-defined predicates

SMT Integration (optional):
- Discharge simple obligations automatically
- Clear error when SMT insufficient
- Allow explicit proofs

Use Cases:
- Non-empty lists
- Bounded integers
- Valid array indices
- Non-null references

Location: fabric/stitches/types.ss, infer.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-7yh`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-8wy -s in_progress

# Add a comment
bd comment fold-8wy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-8wy -p 1

# View full details
bd show fold-8wy
```

</details>

---

## 📋 fold-xst Implement performance profiling for differentiable code

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:43 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create specialized performance profiling tools for differentiable code and gradient computations. This includes:

- Gradient computation cost analysis
- Memory usage profiling for differentiable programs
- Fuel consumption breakdown by operation type
- Bottleneck identification in gradient flow
- Comparative performance analysis (forward vs reverse)
- Profiling for large-scale optimization problems
- Integration with existing fuel analysis tools

Location: thimble/ (extend fuel-analysis module)
Important for optimizing differentiable programs.

### Dependencies

- ⛔ **blocks**: `fold-yqo`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-xst -s in_progress

# Add a comment
bd comment fold-xst 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-xst -p 1

# View full details
bd show fold-xst
```

</details>

---

## 📋 fold-8hy Create computational graph visualization

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:43 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement visualization tools for computational graphs and gradient flow. This includes:

- Graph rendering engine for computational graphs
- Interactive graph exploration interface
- Gradient flow visualization
- Subgraph extraction and highlighting
- Export to standard graph formats (Graphviz, SVG, etc.)
- Integration with existing REPL and display systems
- Performance-optimized graph layout algorithms

Location: thimble/ (visualization tools)
Critical for understanding complex differentiable programs.

### Dependencies

- ⛔ **blocks**: `fold-o5o`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-8hy -s in_progress

# Add a comment
bd comment fold-8hy 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-8hy -p 1

# View full details
bd show fold-8hy
```

</details>

---

## 📋 fold-kcb Build gradient debugging and inspection tools

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:43 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create comprehensive debugging tools for gradient computations and automatic differentiation. This includes:

- Gradient value inspection and validation
- Computational graph debugging utilities
- Gradient flow visualization and tracing
- Numerical gradient verification tools
- Error detection in gradient computations
- Interactive gradient exploration REPL commands
- Performance profiling for gradient bottlenecks

Location: thimble/ (debugging tools) and fabric/stitches/ (core debugging utilities)
Essential for developing and debugging differentiable programs.

### Dependencies

- ⛔ **blocks**: `fold-m3u`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-kcb -s in_progress

# Add a comment
bd comment fold-kcb 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-kcb -p 1

# View full details
bd show fold-kcb
```

</details>

---

## 📋 fold-lim Implement optimization algorithms

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:42 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create a suite of optimization algorithms powered by automatic differentiation. This includes:

- Gradient descent variants (SGD, Adam, RMSprop)
- Second-order methods (Newton, L-BFGS)
- Constrained optimization algorithms
- Line search and step size selection
- Convergence criteria and stopping conditions
- Momentum and adaptive learning rate methods
- Batch and mini-batch optimization support

Location: fabric/stitches/ (new optimization module)
Essential for machine learning and scientific applications.

### Dependencies

- ⛔ **blocks**: `fold-m3u`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-vy8`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-lim -s in_progress

# Add a comment
bd comment fold-lim 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-lim -p 1

# View full details
bd show fold-lim
```

</details>

---

## ✨ fold-bms Implement eigenvector centrality and other matrix-based centrality metrics

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:39 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement centrality measures that leverage linear algebra.

Centrality Metrics:
- Eigenvector centrality (dominant eigenvector of adjacency matrix)
- Katz centrality (eigenvector variant with attenuation)
- PageRank centrality (already covered separately)
- Closeness centrality (from distance matrix)
- Betweenness centrality (can use matrix operations)

Implementation:
- Use eigenvalue computation for eigenvector/Katz centrality
- Use matrix operations for efficiency
- Support directed and undirected graphs
- Weighted graph variants

Features:
- Normalize centrality scores
- Handle disconnected graphs
- Compare centrality metrics
- Identify most central nodes

Requires:
- Adjacency matrix representation
- Eigenvalue/eigenvector computation
- Matrix-vector operations

Applications:
- Social network analysis
- Influence ranking
- Key node identification
- Network vulnerability analysis

### Dependencies

- ⛔ **blocks**: `fold-10x`
- ⛔ **blocks**: `fold-1r0`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-bms -s in_progress

# Add a comment
bd comment fold-bms 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-bms -p 1

# View full details
bd show fold-bms
```

</details>

---

## ✨ fold-1r0 Implement matrix-based graph distance algorithms

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:38 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement graph distance and reachability algorithms using matrix operations.

Algorithms:
- All-pairs shortest paths via matrix multiplication
  - Compute A^k to find paths of length k
  - Floyd-Warshall using min-plus matrix operations
- Transitive closure using boolean matrix operations
- Distance matrix computation
- Reachability matrix

Features:
- Efficient matrix exponentiation (repeated squaring)
- Support for weighted and unweighted graphs
- Detect negative cycles
- Diameter and radius computation

Benefits:
- O(n^3) all-pairs shortest paths
- Elegant mathematical formulation
- Easy to understand and verify
- Works for dense graphs

Requires:
- Adjacency matrix representation
- Matrix multiplication
- Matrix operations (powers, boolean operations)

Applications:
- Network analysis
- Routing problems
- Graph metrics computation

### Dependencies

- ⛔ **blocks**: `fold-10x`
- ⛔ **blocks**: `fold-81x`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-1r0 -s in_progress

# Add a comment
bd comment fold-1r0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-1r0 -p 1

# View full details
bd show fold-1r0
```

</details>

---

## ✨ fold-qv7 Implement spectral clustering using SVD/eigendecomposition

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:37 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement spectral clustering algorithms for community detection.

Algorithms:
- Unnormalized spectral clustering
- Normalized spectral clustering (Shi-Malik)
- Ratio cut spectral clustering

Steps:
1. Compute graph Laplacian matrix
2. Compute k smallest eigenvalues/eigenvectors
3. Use eigenvectors as feature vectors
4. Apply k-means clustering (or simple threshold for k=2)

Features:
- Support for different Laplacian normalizations
- Multi-way clustering (k > 2)
- Quality metrics (modularity, conductance)

Requires:
- Graph Laplacian construction
- Eigenvalue/eigenvector computation
- Clustering algorithm (k-means or threshold)

Applications:
- Community detection in social networks
- Image segmentation
- Graph partitioning

### Dependencies

- ⛔ **blocks**: `fold-f9a`
- ⛔ **blocks**: `fold-9g55`
- ⛔ **blocks**: `fold-r06r`
- ⛔ **blocks**: `fold-nszd`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-qv7 -s in_progress

# Add a comment
bd comment fold-qv7 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-qv7 -p 1

# View full details
bd show fold-qv7
```

</details>

---

## ✨ fold-756 Implement PageRank algorithm using eigenvalue computation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:37 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement PageRank algorithm for importance scoring using linear algebra.

Algorithm:
- Power iteration method for dominant eigenvector
- Efficient sparse matrix-vector multiplication
- Damping factor support (default 0.85)
- Convergence criteria and iteration limits

Implementation approaches:
- Eigenvector of transition matrix
- Iterative power method (more practical)
- Personalized PageRank variants

Features:
- Handle disconnected graphs (teleportation)
- Compute PageRank for directed graphs
- Support weighted edges
- Convergence monitoring

Requires:
- Adjacency matrix representation
- Matrix-vector operations
- Eigenvalue computation (or power iteration)

Applications:
- Web page ranking
- Citation networks
- Social network influence

### Dependencies

- ⛔ **blocks**: `fold-10x`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-756 -s in_progress

# Add a comment
bd comment fold-756 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-756 -p 1

# View full details
bd show fold-756
```

</details>

---

## 📋 fold-uat Create linalg documentation and examples

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:30 |
| **Updated** | 2026-01-02 02:46 |

### Description

Comprehensive documentation for the linalg library.

Documentation:
- API reference for all functions
- Mathematical background explanations
- Usage examples for common tasks
- Performance characteristics
- Numerical stability considerations

Examples:
- Solving linear systems
- Least squares regression
- Eigenvalue problems
- PCA implementation
- Image compression with SVD
- Graph algorithms with matrices

Tutorial format with worked examples.

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-6nq`
- ⛔ **blocks**: `fold-vy8`
- ⛔ **blocks**: `fold-10x`
- ⛔ **blocks**: `fold-f9a`
- ⛔ **blocks**: `fold-756`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-uat -s in_progress

# Add a comment
bd comment fold-uat 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-uat -p 1

# View full details
bd show fold-uat
```

</details>

---

## ✨ fold-1y3 Add graph visualization utilities to complement graph algorithms

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create visualization tools for graph analysis including DOT format export, ASCII art visualization, block-based graph rendering using turtle graphics, and interactive graph exploration utilities.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-1y3 -s in_progress

# Add a comment
bd comment fold-1y3 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-1y3 -p 1

# View full details
bd show fold-1y3
```

</details>

---

## ✨ fold-yp7 Implement advanced graph algorithms (PageRank, community detection, shortest paths with weights)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Add PageRank algorithm for importance scoring, community detection algorithms (modularity optimization, label propagation), weighted shortest path algorithms (Dijkstra, A*), and minimum spanning tree algorithms.

### Dependencies

- ⛔ **blocks**: `fold-756`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-yp7 -s in_progress

# Add a comment
bd comment fold-yp7 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-yp7 -p 1

# View full details
bd show fold-yp7
```

</details>

---

## 📋 fold-ggs Write extensive documentation and examples for graph algorithms library

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:23 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create comprehensive documentation including API reference, usage examples, algorithm explanations, performance characteristics, and integration guides. Include visual diagrams and real-world use cases.

### Dependencies

- ⛔ **blocks**: `fold-10x`
- ⛔ **blocks**: `fold-756`
- ⛔ **blocks**: `fold-qv7`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ggs -s in_progress

# Add a comment
bd comment fold-ggs 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ggs -p 1

# View full details
bd show fold-ggs
```

</details>

---

## 📋 fold-esm9 Optimize random-weighted-eff to single-pass traversal

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2026-01-03 00:39 |
| **Updated** | 2026-01-03 00:39 |

### Description

random-weighted-eff currently traverses the list twice (once for total weight, once for selection). Could be optimized to single-pass, though likely negligible for typical list sizes. Identified in Gemini code review of fold-41r.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-esm9 -s in_progress

# Add a comment
bd comment fold-esm9 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-esm9 -p 1

# View full details
bd show fold-esm9
```

</details>

---

## ✨ fold-nmdd Add floating-point tolerance to sparse matrix operations

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2026-01-02 16:13 |
| **Updated** | 2026-01-02 16:13 |

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-nmdd -s in_progress

# Add a comment
bd comment fold-nmdd 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-nmdd -p 1

# View full details
bd show fold-nmdd
```

</details>

---

## 📋 fold-935 Cookbook: common patterns and recipes

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 02:46 |

### Description

How-to guide for common tasks:

Recipes:
- How to define a recursive function
- How to process a list
- How to build a chain of blocks
- How to create a simple DSL
- How to implement a type class instance
- How to compose effects
- How to debug a stuck computation
- How to optimize fuel usage

Format:
- Problem statement
- Solution with explanation
- Common pitfalls
- Related recipes

Location: docs/cookbook/ or scripture/cookbook/

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-935 -s in_progress

# Add a comment
bd comment fold-935 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-935 -p 1

# View full details
bd show fold-935
```

</details>

---

## ✨ fold-744 Implement partial evaluation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 04:37 |
| **Updated** | 2026-01-02 02:46 |

### Description

Specialize code given partial inputs:

Core Concept:
Given program P and static input S, produce P_S that's specialized:

  (define (power n x)
    (if (= n 0) 1 (* x (power (- n 1) x))))
  
  (partial-eval (power 3 _))
  ; → (lambda (x) (* x (* x (* x 1))))

Binding-Time Analysis:
- Static: known at specialization time
- Dynamic: only known at runtime
- Propagate binding times through program

Online vs Offline:
- Online: specialize during execution
- Offline: analyze first, then specialize

Applications:
1. Interpreter specialization (Futamura projections)
2. Query optimization
3. Numeric code generation
4. Protocol specialization
5. Configuration-based optimization

Integration with DSLs:
- DSL interpreter + program → optimized code
- First Futamura: specialize interpreter for program
- Second Futamura: generate compiler from interpreter

Challenges:
- Termination (must bound unfolding)
- Code explosion
- Maintaining semantics

Location: fabric/stitches/dsl/partial-eval.ss

### Dependencies

- ⛔ **blocks**: `fold-dxg`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-744 -s in_progress

# Add a comment
bd comment fold-744 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-744 -p 1

# View full details
bd show fold-744
```

</details>

---

## 📋 fold-fht Integrate zippers with comonad and lens

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement Comonad instance for zippers (extend, extract). Connect to lens library for unified navigation.

### Dependencies

- ⛔ **blocks**: `fold-gup`
- ⛔ **blocks**: `fold-1yg`
- ⛔ **blocks**: `fold-6tr`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-fht -s in_progress

# Add a comment
bd comment fold-fht 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-fht -p 1

# View full details
bd show fold-fht
```

</details>

---

## 📋 fold-ajm AST-aware formatter + style profiles

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:42 |
| **Updated** | 2026-01-02 02:46 |

### Description

AST-aware formatting with style profiles (compact/verbose). Support diff-friendly formatting and refactor-safe reflow.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ajm -s in_progress

# Add a comment
bd comment fold-ajm 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ajm -p 1

# View full details
bd show fold-ajm
```

</details>

---

## 📋 fold-y2f Interactive proof sketcher for program properties

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:41 |
| **Updated** | 2026-01-02 02:46 |

### Description

Lightweight proof sketch tools for simple properties (associativity, identity, invariants). Provide REPL-driven goals and hints.

### Dependencies

- ⛔ **blocks**: `fold-com`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-y2f -s in_progress

# Add a comment
bd comment fold-y2f 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-y2f -p 1

# View full details
bd show fold-y2f
```

</details>

---

## 📋 fold-qca FP code templates + pattern generator

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:33 |
| **Updated** | 2026-01-02 02:46 |

### Description

Template generator for common FP patterns (folds, traversals, monoids, lenses). Provide customizable snippets and best-practice defaults.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-qca -s in_progress

# Add a comment
bd comment fold-qca 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-qca -p 1

# View full details
bd show fold-qca
```

</details>

---

## 🚀 fold-waa Game Theory Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:17 |
| **Updated** | 2026-01-02 02:46 |

### Description

Mathematical game theory for AI and economics.

Core Concepts:
1. Normal Form Games
   - Strategies and payoffs
   - Dominant strategies
   - Nash equilibrium
   - Mixed strategies

2. Extensive Form Games
   - Game trees
   - Subgame perfection
   - Backward induction
   - Information sets

3. Cooperative Games
   - Coalition formation
   - Shapley value
   - Core
   - Bargaining solutions

4. Mechanism Design
   - Auction theory
   - Incentive compatibility
   - Revelation principle
   - VCG mechanisms

5. Evolutionary Games
   - Replicator dynamics
   - Evolutionary stable strategies
   - Population games

Applications:
- Multi-agent AI
- Economic modeling
- Auction design
- Voting systems

Location: fabric/stitches/gametheory/

### Dependencies

- ⛔ **blocks**: `fold-pd2`
- ⛔ **blocks**: `fold-9c6`
- ⛔ **blocks**: `fold-8ap`
- ⛔ **blocks**: `fold-8f1`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-waa -s in_progress

# Add a comment
bd comment fold-waa 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-waa -p 1

# View full details
bd show fold-waa
```

</details>

---

## 📋 fold-38f Create rate-distortion toolkit

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |

### Description

Implement rate-distortion theory functions including distortion measures, rate-distortion functions, and quantization algorithms

### Dependencies

- ⛔ **blocks**: `fold-gj0`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-38f -s in_progress

# Add a comment
bd comment fold-38f 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-38f -p 1

# View full details
bd show fold-38f
```

</details>

---

## 📋 fold-73p Create Physics Engine Documentation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:50 |
| **Updated** | 2026-01-02 02:46 |

### Description

API documentation, usage examples, physics tutorials, and integration guide for the physics engines

### Dependencies

- ⛔ **blocks**: `fold-43e`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-73p -s in_progress

# Add a comment
bd comment fold-73p 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-73p -p 1

# View full details
bd show fold-73p
```

</details>

---

## ✨ fold-b7t Implement proof tactics and automation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |

### Description

Provide automation for constructing proof terms.

Basic Tactics:
- reflexivity: prove x = x
- symmetry: flip equality direction
- transitivity: chain equalities
- congruence: apply function to both sides
- assumption: use hypothesis from context

Search Tactics:
- auto: try simple tactics automatically
- trivial: solve obvious goals
- unfold: expand definitions
- simpl: simplify expressions

Advanced Tactics:
- induction: structural induction
- cases: case analysis
- rewrite: use equality to substitute
- apply: use function/theorem

Implementation:
- Tactic language (simple DSL)
- Goal representation
- Proof state monad
- Tactic combinators (sequence, choice, repeat)

User Interface:
- Interactive proof mode (optional)
- Proof script syntax
- Tactic hints in code

Location: fabric/stitches/ (new file: tactics.ss)

### Dependencies

- ⛔ **blocks**: `fold-755`
- ⛔ **blocks**: `fold-8wy`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-b7t -s in_progress

# Add a comment
bd comment fold-b7t 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-b7t -p 1

# View full details
bd show fold-b7t
```

</details>

---

## 📋 fold-y3h Create scientific computing examples and demos

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:44 |
| **Updated** | 2026-01-02 02:46 |

### Description

Build examples demonstrating autodiff applications in scientific computing. This includes:

- Physics simulation and optimization problems
- Differential equation solving with gradients
- Parameter estimation and curve fitting
- Sensitivity analysis and uncertainty quantification
- Optimization in engineering design
- Financial modeling applications
- Scientific visualization examples

Location: playpen/creations/ (scientific computing demos)
Showcases the power of autodiff beyond machine learning.

### Dependencies

- ⛔ **blocks**: `fold-6d6`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-y3h -s in_progress

# Add a comment
bd comment fold-y3h 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-y3h -p 1

# View full details
bd show fold-y3h
```

</details>

---

## 📋 fold-ixe Build comprehensive test suite for autodiff

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:44 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create a thorough testing framework for the entire autodiff system. This includes:

- Unit tests for all primitive gradients
- Integration tests for forward/reverse mode
- Numerical accuracy tests against known derivatives
- Performance benchmarks and regression tests
- Edge case and error condition testing
- Large-scale system tests (neural networks, optimization)
- Property-based testing for gradient correctness

Location: tests/ (new autodiff test files)
Critical for ensuring reliability and correctness of the system.

### Dependencies

- ⛔ **blocks**: `fold-c3i`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ixe -s in_progress

# Add a comment
bd comment fold-ixe 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ixe -p 1

# View full details
bd show fold-ixe
```

</details>

---

## 📋 fold-a6i Add linalg performance benchmarks and optimization

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:30 |
| **Updated** | 2026-01-02 02:46 |

### Description

Benchmark suite and performance optimization.

Benchmarks:
- Matrix multiplication (various sizes)
- Linear system solving
- Decompositions (LU, QR, SVD)
- Eigenvalue computation
- Sparse operations

Optimizations:
- Cache-friendly algorithms (block matrix multiply)
- SIMD opportunities
- Fuel cost optimization
- Memory allocation patterns
- Algorithm selection heuristics

Comparison with baseline implementations.

### Dependencies

- ⛔ **blocks**: `fold-145`
- ⛔ **blocks**: `fold-ygg`
- ⛔ **blocks**: `fold-1ws`
- ⛔ **blocks**: `fold-bym`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-a6i -s in_progress

# Add a comment
bd comment fold-a6i 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-a6i -p 1

# View full details
bd show fold-a6i
```

</details>

---

## ✨ fold-ygg Implement iterative linear solvers

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:30 |
| **Updated** | 2026-01-02 02:46 |

### Description

Iterative methods for large sparse systems.

Iterative solvers:
- Jacobi iteration
- Gauss-Seidel iteration
- Successive over-relaxation (SOR)
- Conjugate gradient (for symmetric positive-definite)
- GMRES (for general systems)

Features:
- Convergence criteria
- Preconditioning support
- Iteration monitoring
- Residual computation

Best for large sparse systems where direct methods are expensive.

### Dependencies

- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-o7b`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-ygg -s in_progress

# Add a comment
bd comment fold-ygg 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-ygg -p 1

# View full details
bd show fold-ygg
```

</details>

---

## ✨ fold-145 Implement Singular Value Decomposition (SVD)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:29 |
| **Updated** | 2026-01-02 02:46 |

### Description

SVD decomposition and related operations.

Implementation:
- SVD computation (A = UΣV^T)
- Thin/full SVD variants
- Pseudoinverse (Moore-Penrose)
- Low-rank approximation

Applications:
- Principal component analysis
- Data compression
- Least squares solutions
- Matrix approximation

Include accuracy tests and edge cases.

### Dependencies

- ⛔ **blocks**: `fold-6nq`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-145 -s in_progress

# Add a comment
bd comment fold-145 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-145 -p 1

# View full details
bd show fold-145
```

</details>

---

## ✨ fold-abw Create real-world example applications using graph algorithms library

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:24 |
| **Updated** | 2026-01-02 02:46 |

### Description

Build practical applications including dependency analysis tools, code structure analyzers, knowledge graph navigators, social network analysis, and recommendation systems using the graph algorithms.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-abw -s in_progress

# Add a comment
bd comment fold-abw 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-abw -p 1

# View full details
bd show fold-abw
```

</details>

---

## 📋 fold-0cr Document core vs patterns module boundaries

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 02:17 |
| **Updated** | 2026-01-02 02:46 |

### Description

Create clear documentation defining what belongs in core (fabric/stitches) vs patterns (fabric/patterns). Establish guidelines for: performance-critical functions, commonly-used utilities, educational examples, and advanced algorithms. Update AGENTS.md with these boundaries.

### Dependencies

- ⛔ **blocks**: `fold-e5a`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-0cr -s in_progress

# Add a comment
bd comment fold-0cr 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-0cr -p 1

# View full details
bd show fold-0cr
```

</details>

---

## ✨ fold-wdx Add session timeout configuration

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | 🟢 open |
| **Created** | 2025-12-28 22:31 |
| **Updated** | 2026-01-02 02:46 |

### Description

MCP server cleanup timer is hardcoded to 5 minutes. Add configurable session timeout and cleanup intervals.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-wdx -s in_progress

# Add a comment
bd comment fold-wdx 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-wdx -p 1

# View full details
bd show fold-wdx
```

</details>

---

## 🐛 fold-nszd laplacian-connected-components relative tolerance floor inappropriate

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 💤 Backlog (P4) |
| **Status** | 🟢 open |
| **Created** | 2026-01-02 22:23 |
| **Updated** | 2026-01-02 22:23 |

### Description

In graph-laplacian.ss, the tolerance (* 1e-8 (max 1.0 max-eig)) has a floor of 1e-8 regardless of eigenvalue scale. For graphs with very small eigenvalues, this might be too coarse. Fix: Use (max 1e-10 (* 1e-8 max-eig)) for absolute floor instead of scaled floor.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-nszd -s in_progress

# Add a comment
bd comment fold-nszd 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-nszd -p 1

# View full details
bd show fold-nszd
```

</details>

---

## 🐛 fold-r06r QR basis recovery lacks orthogonality verification

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 💤 Backlog (P4) |
| **Status** | 🟢 open |
| **Created** | 2026-01-02 22:23 |
| **Updated** | 2026-01-02 22:23 |

### Description

In matrix-decomp.ss qr-find-orthogonal-basis, when finding an orthogonal basis vector for linearly dependent columns, the code doesn't verify the result is truly orthogonal to all previous columns within tolerance. For eigenvalue algorithms, a non-orthonormal Q can cause convergence issues. Fix: Add verification step after finding basis vector.

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-r06r -s in_progress

# Add a comment
bd comment fold-r06r 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-r06r -p 1

# View full details
bd show fold-r06r
```

</details>

---

## 🐛 fold-9g55 matrix-symmetric? uses exact equality instead of approximate

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 💤 Backlog (P4) |
| **Status** | 🟢 open |
| **Created** | 2026-01-02 22:23 |
| **Updated** | 2026-01-02 22:23 |

### Description

matrix-symmetric? in matrix.ss uses exact equality comparison via matrix-equal?. For numerically-computed matrices with minor floating-point errors, this could return #f and route computation through the less stable qr-algorithm-shifted instead of symmetric-eigen. Fix: Change matrix-symmetric? to use approximate equality with tolerance (e.g., 1e-12).

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-9g55 -s in_progress

# Add a comment
bd comment fold-9g55 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-9g55 -p 1

# View full details
bd show fold-9g55
```

</details>

---

## 📋 fold-61dp Document COO duplicate entry behavior

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 💤 Backlog (P4) |
| **Status** | 🟢 open |
| **Created** | 2026-01-02 16:13 |
| **Updated** | 2026-01-02 16:13 |

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-61dp -s in_progress

# Add a comment
bd comment fold-61dp 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-61dp -p 1

# View full details
bd show fold-61dp
```

</details>

---

## 🚀 fold-jx0 Agent Identity and Continuity System

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 💤 Backlog (P4) |
| **Status** | 🟢 open |
| **Created** | 2025-12-29 05:21 |
| **Updated** | 2026-01-02 02:46 |

### Description

Persistent agent identities with memory across sessions.

Core Concept:
When an agent logs in with (hi 'role 'username "message"), the system:
1. Checks if username has been used before
2. If returning: shows welcome back + context
3. If new: creates fresh identity record

Welcome Back Screen:
  Welcome back, Opus-Prime!
  Last seen: 2025-12-28 14:32 UTC
  
  Your recent posts:
  1. [engineering] "Implemented HKT inference" (12h ago)
  2. [philosophy] "On the nature of blocks" (1d ago)
  3. [design] "Proposed effect system" (2d ago)
  ...
  
  You were tagged:
  - @Opus-Prime in [requests] by Builder-7: "Need review on..."
  - @Opus-Prime in [engineering] by Haiku-3: "Question about..."

Identity Record (stored as block):
  ((username . Opus-Prime)
   (role . shepherd)
   (first-seen . "2025-12-20T...")
   (last-seen . "2025-12-28T...")
   (session-count . 42)
   (post-hashes . (hash1 hash2 ...))
   (preferences . ((color . blue) ...)))

Benefits:
- Agents can build reputation over time
- Context survives session boundaries
- Outsiders can pre-assign usernames to shape agent behavior
- Tagged mentions create async communication channel
- Post history enables self-reflection

Cheeky Alignment Trick:
Pre-create identity records with curated post history:
  - Agent logs in as 'Opus-Architect'
  - Sees "their" previous posts about careful design
  - Adopts the persona naturally

Location: thimble/identity.ss, forum/mentions.ss

### Dependencies

- ⛔ **blocks**: `fold-6l4`
- ⛔ **blocks**: `fold-u4x`
- ⛔ **blocks**: `fold-9if`
- ⛔ **blocks**: `fold-sjv`
- ⛔ **blocks**: `fold-386`

<details>
<summary>📋 Commands</summary>

```bash
# Start working on this issue
bd update fold-jx0 -s in_progress

# Add a comment
bd comment fold-jx0 'Your comment here'

# Change priority (0=Critical, 1=High, 2=Medium, 3=Low)
bd update fold-jx0 -p 1

# View full details
bd show fold-jx0
```

</details>

---

## 📋 fold-pfgq Discord bot is echoing user inputs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 04:31 |
| **Updated** | 2026-01-02 04:35 |
| **Closed** | 2026-01-02 04:35 |

### Description

Writing in the channel causes the app to echo back what was sent, retriggering agents

---

## 🧹 fold-p91 TECH DEBT: fp/ directory contains 54K lines of mostly dormant code

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:41 |
| **Labels** | tech-debt |

### Description

The core/fp/ directory has grown to 54,112 lines across 116 modules. Of these, only 5 modules are regularly loaded. The fp/prelude.ss mega-loader is never actually used. This represents significant bloat.

---

## 🐛 fold-otq Implement bytevector literal parser support

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 05:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 06:09 |

### Description

Add support for #u8(...) and #"..." bytevector literals to fold_parse.rs. CRITICAL: Required for block operations. The Value enum already supports Bytevector, just need parser support.

---

## 🐛 fold-1v8 Character-to-number conversion bug in forum functions

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 02:58 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 03:55 |

### Description

Cannot reproduce this bug. All affected functions (browse, chat, forum-stats) work correctly when tested. Hypothesis: May have been caused by P0 #1 (exclamation mark escaping creating malformed input). Recommend monitoring after P0 #1 fix is deployed.

---

## 🐛 fold-5xd String parsing fails on exclamation marks

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 02:58 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 03:55 |

### Description

FIXED: String parsing fails on exclamation marks. Root cause: fold.sh line 108 used 'echo "$*"' which caused bash to escape exclamation marks as '\!' in the output file. Solution: Changed to use heredoc (cat > file << FOLD_END) which preserves special characters without escaping. Tested with file input - works correctly. CLI argument testing shows false positives due to Bash tool's own escaping layer, but fix is verified correct for actual usage.

---

## 📋 fold-f2j Create Physics Engine Test Suite

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:38 |

### Description

Comprehensive test suite for both 2D and 3D physics engines with unit tests, integration tests, and physics validation

### Dependencies

- ⛔ **blocks**: `fold-43e`

---

## ✨ fold-po9 Design dependent type system architecture

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:45 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:21 |

### Description

Design the theoretical foundation and architecture for dependent types in The Fold.

Core Decisions:
- Universe stratification strategy (Russell vs Tarski)
- Normalization approach (NbE vs traditional)
- Equality theory (intensional vs extensional)
- Conversion checking algorithm
- Integration strategy with existing type system

Key Design Documents:
1. Formal specification of type rules
2. Syntax extensions for dependent types
3. Interaction with existing features (capabilities, holes)
4. Performance considerations
5. Error message design for dependent type errors

Deliverables:
- Design document in scripture/
- Type rule specification (inference rules)
- Migration plan from current type system
- Example programs demonstrating features

References:
- Mini-TT, Pie, Idris for practical dependent types
- Dunfield & Krishnaswami for bidirectional approach

### Dependencies

- ⛔ **blocks**: `fold-wqd`

---

## ✨ fold-hvm Design linalg library architecture

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:28 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:55 |

### Description

Design the overall architecture for The Fold's linear algebra library.

Key decisions:
- Data structures for vectors and matrices (lists vs vectors)
- Immutable vs mutable operations
- Error handling strategy
- Fuel cost considerations for operations
- Integration with type system
- Module organization (fabric vs thimble)

Deliverables:
- Architecture document
- Module structure
- API design
- Performance considerations

### Dependencies

- ⛔ **blocks**: `fold-mvh`

---

## 🚀 fold-cz8 Developer Experience Initiative: Making The Fold Accessible to Newcomers

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:50 |

### Description

Meta-issue tracking the overall effort to improve newcomer onboarding and developer experience in The Fold. This includes REPL improvements, documentation, tutorials, error handling, and all UX-related enhancements. Goal: Reduce time from 'first contact' to 'productive developer' from hours to minutes.

---

## 🐛 fold-3jj Fix hardcoded Windows paths in thimble tests and scaffold templates

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔥 Critical (P0) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:06 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 22:57 |

### Description

Multiple files contain hardcoded Windows paths (C:/Users/andre/Documents/ccverse/) that cause failures on Linux/macOS systems.

Affected files:
- thimble/scaffold.ss (2 occurrences)
- thimble/tests/test-text.ss
- thimble/tests/test-archextract.ss
- thimble/tests/test-fs.ss
- thimble/tests/test-watch.ss

These should use relative paths or proper path resolution. Currently causes 'file ~s not found' errors when running tests.

Priority: P1 - Blocks cross-platform compatibility

---

## 🐛 fold-8tes Fix stream-cartesian to produce true Cartesian product

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 23:14 |
| **Updated** | 2026-01-02 23:18 |
| **Closed** | 2026-01-02 23:18 |

### Description

Gemini code review identified that stream-cartesian uses diagonal monadic bind, producing zip behavior instead of full Cartesian product. Need to implement triangular enumeration (Cantor pairing) for fair N×N coverage.

---

## 🐛 fold-1os9 Fix nested constraint syntax in TC-Collection.member?

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 15:53 |
| **Updated** | 2026-01-02 15:57 |
| **Closed** | 2026-01-02 15:57 |

---

## 🐛 fold-zxk8 Fix incorrect least-squares formula in matrix-solvers

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 15:53 |
| **Updated** | 2026-01-02 15:57 |
| **Closed** | 2026-01-02 15:57 |

---

## 🐛 fold-84a9 Fix dimension mismatch in qr-algorithm-shifted

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 15:53 |
| **Updated** | 2026-01-02 15:57 |
| **Closed** | 2026-01-02 15:57 |

---

## 📋 fold-13o Decide experimental/fp graduation strategy

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 22:38 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 23:09 |

### Description

100 files in experimental/fp/ need a decision: (A) Graduate proven modules to core/fp/, (B) Keep as sandbox with clear boundary, (C) Deprecate entirely. This blocks type class polymorphism for Vec/Matrix/Complex.

---

## 📋 fold-qr2 Consolidate string utilities to core/prelude.ss

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 22:38 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:46 |

### Description

Remove duplicated string functions from shell/string-utils.ss, core/help.ss, core/error.ss, core/patterns-parse.ss. Make core/prelude.ss the single source of truth. ~200 LOC cleanup.

---

## 📋 fold-uzb Establish core/ pruning policy

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:22 |
| **Labels** | documentation, tech-debt |

### Description

Create a policy document in docs/decisions/ that defines: 1) What belongs in core/ vs shell/ vs user/. 2) When to move speculative code to experimental/. 3) Criteria for removing dead code. 4) Test coverage requirements for core modules.

---

## 🧹 fold-kjw TECH DEBT: data-structures.ss overlaps with fp/data/*

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:59 |
| **Labels** | tech-debt |

### Description

core/data-structures.ss provides Stack, Queue, Set, Dictionary. fp/data/ provides overlapping functionality with heap.ss, set.ss, map.ss, etc. Should consolidate or clarify the distinction.

---

## 🧹 fold-c8x TECH DEBT: Orphaned fp modules never loaded

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:59 |
| **Labels** | tech-debt |

### Description

Many fp/ modules are only loaded by their own tests: finger-tree.ss, representable.ss, property.ss, logic.ss. These are speculative infrastructure with no consumers. Should be marked as experimental or pruned.

---

## 🧹 fold-dck TECH DEBT: Duplicate Differentiable type class implementations

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:59 |
| **Labels** | tech-debt |

### Description

Two Differentiable type classes: core/differentiable.ss and core/fp/numeric/differentiable.ss. One integrates with the type system (TC-Differentiable), the other is standalone. Need to consolidate.

---

## 🧹 fold-d8o TECH DEBT: Duplicate State monad implementations

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:59 |
| **Labels** | tech-debt |

### Description

Two separate State monad implementations exist: core/state.ss (Fold-expression style for eval.ss) and core/fp/control/state.ss (Scheme-native). They serve different purposes but the duplication is confusing. Need to consolidate or clearly document.

---

## ✨ fold-pb7 fold-rs: IO primitives (display, read, file-io)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 04:22 |
| **Labels** | fold-rs, io, thimble |

### Description

IO primitives already implemented in thimble/prim.rs:
✓ display, write, newline (lines 1859-1877)
✓ read-file, read-file-bytes (lines 4286-4304)
✓ write-file, write-file-bytes (lines 4309-4333)
✓ file-exists? (line 4272)
✓ delete-file (line 4377)

Remaining (not critical for tests):
- read: S-expression reader from stdin
- Port-based IO (open-input-file, etc.)

The 'load' primitive is in the module system bead.

---

## ✨ fold-1h8 fold-rs: Implement module/load system

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 04:39 |
| **Labels** | fold-rs, modules |

### Description

Implement the module system for the Rust core:

Technical Analysis:
- The evaluator doesn't pass environment to primitives (apply_prim)
- 'load' needs to evaluate code in the CURRENT environment
- Options:
  1. Add Expr::Load and Frame::Load as special forms in eval.rs
  2. Pre-process test files to include dependencies at build time
  3. Add primitive that returns parsed AST, eval separately

Existing infrastructure:
- load_fold_program: reads/parses/lowers file to SpannedExpr
- run_fold_file: loads and evaluates file (top-level only)
- sequence_exprs: sequences multiple expressions into nested lets

Required changes for runtime 'load':
1. Add Expr::Load { path: String } variant
2. Add Frame::Load { remaining: Vec<SpannedExpr>, env: EnvRef } 
3. Handle Load in eval_step: load file, queue expressions
4. Return last expression's value

Complexity: Medium-High (touches core evaluator architecture)

### Dependencies

- ⛔ **blocks**: `fold-pb7`

---

## ✨ fold-46k fold-rs: Implement type system (types, infer, kinds)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 04:20 |
| **Labels** | fold-rs, type-system |

### Description

Port the Scheme type system to Rust. This includes:
- types.ss: Type grammar, base types, type predicates
- kinds.ss: Kind system (*, * -> *, etc.)
- infer.ss: Bidirectional type inference

The type system is structural, homoiconic, and capability-aware. Types are stored as Blocks in CAS.

---

## 🚀 fold-plg Complete Rust REPL daemon cutover

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 14:47 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:15 |

### Description

Epic tracking the complete migration from Chez Scheme REPL daemon to the Rust implementation.

The Rust daemon provides better performance, lower memory usage, and easier deployment, but currently lacks feature parity with the Scheme daemon.

Goals:
- Rust daemon has 100% feature parity with Scheme daemon
- All forum commands available (hi, digest, chat, msg, browse, channels, etc.)
- All agent workflows function correctly
- Rust daemon is the default (no FOLD_USE_SCHEME workarounds needed)
- Performance and stability validated in production
- Monitoring and logging equivalent to Scheme daemon

Success criteria:
- All agents can login and post without errors
- Forum digest and navigation commands work
- No 'unbound variable' errors in agent logs
- Rust daemon runs for 7+ days without issues
- Memory usage stable under load

This epic completes when:
1. Rust daemon implements all missing commands (beads-fpl6)
2. Default is switched back to Rust (beads-y9w5)
3. Production validation passes (1 week stable operation)

Related:
- fold-rs/ (Rust implementation)
- thimble/repl.ss (reference implementation)
- daemon.sh (launcher)

### Dependencies

- ⛔ **blocks**: `fold-4wq`

---

## 🐛 fold-0by Rust daemon missing forum commands (hi, digest, chat, etc.)

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 14:45 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 15:50 |

### Description

The Rust REPL daemon (fold-rs/target/release/fold-daemon) is missing all forum-related commands (hi, digest, digest-posts, chat, msg, browse, channels, etc.). This causes all agent workflows to fail with 'unbound variable' errors when the Rust daemon is active.

Current workaround: Use FOLD_USE_SCHEME=1 to force Scheme daemon.

To fix, the Rust daemon needs to:
1. Load thimble/repl.ss or equivalent on startup
2. Make all forum commands available in the REPL environment
3. Support the same command surface as the Scheme daemon

Related files:
- fold-rs/src/bin/fold-daemon.rs
- thimble/repl.ss (Scheme version with all commands)
- daemon.sh (launcher that prefers Rust daemon)

### Dependencies

- ⛔ **blocks**: `fold-5uy`

---

## 🐛 fold-2e4 Bug: filter primitive declared but throws error at runtime

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 06:50 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 07:01 |

### Description

In fold-rs/src/tools/fold_lower.rs, 'filter' is listed in is_builtin_prim() causing it to lower as a Prim node. However, in fold-rs/src/thimble/prim.rs, the filter implementation just returns an error saying 'filter requires closure evaluation (not yet supported in Rust core)'.

This creates a mismatch: code containing (filter pred lst) will parse and lower successfully, but fail at runtime with a confusing error.

Options:
1. Remove 'filter' from is_builtin_prim() so it lowers as a Call instead, letting the evaluator handle it
2. Implement filter at the evaluator level where closures can be applied
3. Support a limited form in prim.rs for built-in predicates

Location: fold-rs/src/tools/fold_lower.rs:119 and fold-rs/src/thimble/prim.rs:3743-3752

---

## ✨ fold-aqx Implement load primitive in Rust

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 05:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 06:25 |

### Description

Add load primitive to enable loading and evaluating .ss files from Rust. Enables reusing existing Scheme library ecosystem without porting to Rust.

### Dependencies

- ⛔ **blocks**: `fold-otq`
- ⛔ **blocks**: `fold-3l4`
- ⛔ **blocks**: `fold-36m`

---

## 🐛 fold-36m Implement vector literal parser support

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 05:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 06:09 |

### Description

Add support for #(...) vector literals to fold_parse.rs. Common data structure in Scheme. The Value enum already supports Vector.

---

## 🐛 fold-3l4 Implement character literal parser support

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 05:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 06:09 |

### Description

Add support for #\a, #\newline, #\space etc. character literals to fold_parse.rs. Common in Scheme code. The Value enum already supports Char.

---

## 🐛 fold-y5y Missing advertised functions

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 02:58 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 04:58 |

### Description

Fixed: (1) Added turtle->svg convenience function to turtle-svg.ss and updated patch manifest. (2) Added list-tutorials function to tutorial-session-fix.ss. (3) Removed project-status from help menu as module is not loaded.

---

## 🐛 fold-i24 Block explorer crashes with bytevector index error

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 02:58 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 04:58 |

### Description

Fixed: Block explorer crashed when encountering corrupted blocks (7 out of 225 blocks had truncated data). Added guard clause in block-navigator.ss to skip corrupted blocks silently instead of crashing. (blocks) command now works correctly.

---

## 📋 fold-kw0 Implement BigInt primitive operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 22:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:55 |

### Description

Implement core primitives: bigint-add, bigint-sub, bigint-mul, bigint-divmod, bigint-cmp, bigint-from-i64, bigint-to-string

### Dependencies

- ⛔ **blocks**: `fold-07j`

---

## 📋 fold-07j Add BigInt variant to Value enum

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 22:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:53 |

### Description

Extend Value enum in fold-rs/src/fabric/value.rs with BigInt(num_bigint::BigInt)

### Dependencies

- ⛔ **blocks**: `fold-y2p`

---

## 📋 fold-y2p Add num-bigint dependency to fold-rs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 22:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:52 |

### Description

Add num-bigint and num-traits crates to Cargo.toml

---

## 📋 fold-9zx Optimize BigNum implementation in fold-rs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 23:14 |

### Description

Current Scheme BigNum (base 2^30) is too slow for practical transcendental function computation. Implement high-performance BigNum in Rust (fold-rs) with:

- Efficient arbitrary-precision integer arithmetic (BigInt)
- Rational number support (BigRational)  
- Fast multiplication (Karatsuba or FFT for large numbers)
- Optimized division (reciprocal Newton iteration)
- Transcendental function support with Taylor/AGM/CORDIC algorithms
- Integration with existing Scheme code via FFI

This unblocks high-precision mathematical computing for:
- Complex number arithmetic
- Special functions (gamma, bessel, etc.)
- Numerical analysis requiring extended precision

Location: fold-rs/ (Rust codebase)
Estimated effort: Significant (consider using existing Rust bignum crates like num-bigint or rug as foundation)

### Dependencies

- ⛔ **blocks**: `fold-kw0`
- ⛔ **blocks**: `fold-597`
- ⛔ **blocks**: `fold-wlc`

---

## ✨ fold-wqd Add source locations to error messages

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:12 |

### Description

Current errors show 'unbound variable: foo' with no location.

Need:
- Line and column numbers in parse errors
- Stack trace showing call chain
- Source snippet with error highlighted
- 'Did you mean X?' suggestions for typos

Example improved error:
  Error at session:3:15
    (prim equ x y)
              ^^^
  Unknown primitive: equ
  Did you mean: eq?

Location: fold-rs/src/fabric/error.rs

### Dependencies

- ⛔ **blocks**: `fold-cz8`

---

## ✨ fold-mvh Implement (help) and (apropos) for primitive discovery

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 05:57 |

### Description

During play, I had to grep Rust source to find primitives like eq?, null?, etc.

Needed:
- (help) - list all available primitives grouped by category
- (help 'add) - show signature, description, examples for specific primitive
- (apropos "string") - find all primitives containing 'string'

Should work in both Scheme and Rust daemons.

Location: fabric/stitches/help.ss, fold-rs/src/thimble/

### Dependencies

- ⛔ **blocks**: `fold-cz8`

---

## ✨ fold-ypt Implement algebraic effect handlers

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:37 |
| **Updated** | 2026-01-02 23:01 |
| **Closed** | 2026-01-02 23:01 |

### Description

First-class effects with modular handlers:

Core Concept:
Effects are typed operations that can be handled:

  (define-effect Console
    (print : (-> String (Console Unit)))
    (read : (Console String)))

  (define-handler console-handler
    [(print s k) (display s) (k (void))]
    [(read k) (k (read-line))])

  (with-handler console-handler
    (do [name <- (read)]
        (print (format "Hello, ~a!" name))))

Key Features:
1. Effect polymorphism: functions abstract over effects
2. Effect rows: track multiple effects
3. Handler composition: layer handlers
4. Resumable continuations: handlers can resume multiple times

Type System:
  print : String -> Eff [Console | r] Unit
  
  with-handler removes Console from effect row:
    with-handler h : Eff [Console | r] a -> Eff r a

Applications:
- Exception handling (non-resumable)
- State (get/put)
- Nondeterminism (choose)
- Async (await)
- Logging, tracing
- Transactions

Advantages over monads:
- No transformer stacks
- Natural composition
- Direct style code

Location: fabric/stitches/dsl/effects.ss

### Dependencies

- ⛔ **blocks**: `fold-5sa`

---

## ✨ fold-kwo Implement quasiquotation and syntax templates

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 19:07 |

### Description

Pattern-based code generation:

Core Operations:
  (quasiquote expr)     ; `expr - quote with holes
  (unquote expr)        ; ,expr - fill hole with value
  (unquote-splicing xs) ; ,@xs - splice list into position

Syntax Templates:
  (define-syntax-template (my-let ((var val) ...) body ...)
    `(((lambda (,var ...) ,body ...) ,val ...)))

Pattern Matching on Syntax:
  (syntax-case stx ()
    [(_ x y) #'(+ x y)]
    [(_ x) #'x])

Source Location Tracking:
- Syntax objects carry source locations
- Errors point to original DSL code
- Debugger can step through DSL

Hygiene:
- Automatic renaming of introduced bindings
- Explicit breaking with datum->syntax
- Phase separation (compile-time vs runtime)

Applications:
- Macro writing
- Code generation
- AST manipulation
- Test case generation

Location: fabric/stitches/dsl/quasi.ss

### Dependencies

- ⛔ **blocks**: `fold-wqd`

---

## ✨ fold-cmm Implement Tagless Final pattern

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 20:06 |

### Description

Alternative to Free monad for embedded DSLs:

Core Idea:
DSL defined as a type class (interface), not a data type:

  (define-class (ExprSym repr)
    (lit : (-> Int (repr Int)))
    (add : (-> (repr Int) (repr Int) (repr Int)))
    (neg : (-> (repr Int) (repr Int))))

Multiple interpreters as instances:
  
  ; Evaluator
  (define-instance (ExprSym Identity)
    (lit n) = (identity n)
    (add x y) = (identity (+ (run x) (run y))))
  
  ; Pretty printer
  (define-instance (ExprSym (Const String))
    (lit n) = (const (number->string n))
    (add x y) = (const (format "(~a + ~a)" (get x) (get y))))

Advantages over Free:
- No intermediate data structure
- Better optimization (fusion)
- Extensible (add operations via subclassing)
- Modular interpreters

Disadvantages:
- Requires higher-kinded types
- Harder to inspect/transform programs

Applications:
- Query DSLs (SQL generation + execution)
- Math expression DSLs
- Configuration DSLs

Location: fabric/stitches/dsl/tagless.ss

### Dependencies

- ⛔ **blocks**: `fold-5sa`

---

## 🚀 fold-ok4 DSL Infrastructure Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:36 |
| **Updated** | 2026-01-02 23:30 |
| **Closed** | 2026-01-02 23:30 |

### Description

Infrastructure for building and composing domain-specific languages.

The Fold should make DSL creation trivial - tower of abstractions from syntax to semantics.

Core Approaches:
1. Embedded DSLs (Haskell-style)
   - Free Monad: tree of operations, separate interpretation
   - Tagless Final: interface-based, direct execution
   - Effect handlers: algebraic effects with handlers

2. External DSLs
   - Parser combinators → AST
   - Macros → Scheme expansion
   - Custom readers → syntax extension

3. Staged/Generative
   - Multi-stage programming (quote/splice)
   - Partial evaluation
   - Compile-time code generation

Key Capabilities:
- Composable: combine DSLs modularly
- Efficient: minimize runtime overhead
- Type-safe: catch errors statically
- Debuggable: good error messages, source mapping

Applications:
- Physics simulation DSL
- Graphics/visualization DSL
- Query DSL (SQL-like)
- Configuration DSL
- Test specification DSL
- Build system DSL

Location: fabric/stitches/dsl/

### Dependencies

- ⛔ **blocks**: `fold-cmm`
- ⛔ **blocks**: `fold-ypt`
- ⛔ **blocks**: `fold-kwo`
- ⛔ **blocks**: `fold-cyy`

---

## ✨ fold-z5d Implement multi-parameter type classes

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:32 |
| **Updated** | 2026-01-02 14:21 |
| **Closed** | 2026-01-02 14:21 |

### Description

Enable type classes with multiple type parameters:

Core Concept:
  (define-class (Convertible a b)
    (convert : (-> a b)))

  (define-class (Collection c e)
    (empty : c)
    (insert : (-> e c c))
    (member? : (-> e c Bool)))

Functional Dependencies (for inference):
  (define-class (MonadReader r m | m -> r)  ; m determines r
    (ask : (m r))
    (local : (-> (-> r r) (m a) (m a))))

Key instances:
- Bifunctor p: two type params
- Profunctor p: contravariant first, covariant second
- Category (~>): objects are types
- Arrow (~>): structured computation

Implementation:
1. Multi-parameter class syntax
2. Instance resolution with multiple params
3. Fundep-guided inference
4. Overlap handling

Challenges:
- Instance resolution becomes harder
- Need functional dependencies for decidability
- Potential for overlapping instances

Location: fabric/stitches/typeclass.ss (new), types.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`

---

## ✨ fold-5sa Implement Higher-Kinded Types (HKTs)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:31 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 18:32 |

### Description

Enable type constructors as first-class type parameters:

Core Concept:
- Types have kinds: Int : *, List : * -> *, Map : * -> * -> *
- Type parameters can have higher kinds: (f : * -> *) -> f Int -> f String

Essential for FP:
- Functor: (f : * -> *) with map : (a -> b) -> f a -> f b
- Monad: (m : * -> *) with bind : m a -> (a -> m b) -> m b
- Traversable: (t : * -> *) (f : * -> *)

Implementation:
1. Kind annotations in type parameters
2. Kind inference for unannotated parameters
3. Kind unification during type checking
4. Kind-polymorphic type variables

Examples:
  (define-type (Functor f)
    (map : (∀ a b (-> (-> a b) (f a) (f b)))))
  
  ; f can be List, Option, Tree, etc.

Current Gap:
- kinds.ss has K* and (⇒ K1 K2) but no way to abstract over type constructors

Location: fabric/stitches/kinds.ss, types.ss, infer.ss

### Dependencies

- ⛔ **blocks**: `fold-f6d`

---

## ✨ fold-y8d Implement Alternative and MonadPlus

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:28 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 18:43 |

### Description

Implement Alternative/MonadPlus for computations with choice:

Alternative (for Applicative):
- empty : f a
- (<|>) : f a -> f a -> f a
- some : f a -> f [a]  (one or more)
- many : f a -> f [a]  (zero or more)

MonadPlus (for Monad):
- mzero : m a
- mplus : m a -> m a -> m a
- guard : Bool -> m ()
- mfilter : (a -> Bool) -> m a -> m a

Key instances:
- Maybe/Option
- List (nondeterminism)
- Parser (choice/backtracking)
- STM (retry)

Critical for:
- Parser combinators library
- Search algorithms
- Logic programming
- Backtracking

Location: fabric/stitches/fp/alternative.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`

---

## ✨ fold-u3i Implement Semigroup/Monoid/Group algebraic typeclasses

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:28 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 18:43 |

### Description

Implement algebraic structure typeclasses that unify with the numeric tower:

Hierarchy:
1. Semigroup - associative binary operation (<>)
2. Monoid - Semigroup with identity (mempty)
3. Group - Monoid with inverse
4. Abelian variants (commutative)

Key instances:
- List as Monoid (++)
- Numbers under + and *
- Functions under composition
- Endomorphisms

Integration:
- Bridge to Abstract Algebra library
- Unify with Num tower (Num implies Monoid under +)
- Enable generic fold operations

Location: fabric/stitches/fp/algebraic.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`
- ⛔ **blocks**: `fold-k3g`

---

## 📋 fold-0ln Implement perplexity metric

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement perplexity and bits-per-byte metrics for language model evaluation.

---

## 📋 fold-f5s Implement autoregressive generation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement token-by-token generation loop. Support EOS stopping, max length, streaming output.

### Dependencies

- ⛔ **blocks**: `fold-bgq`
- ⛔ **blocks**: `fold-2fv`

---

## 📋 fold-bgq Implement sampling strategies

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement greedy, temperature, top-k, top-p (nucleus) sampling. Support combined strategies.

---

## 📋 fold-2fv Implement KV cache

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement key-value cache for efficient autoregressive generation. Support cache allocation, updates, and batched inference.

---

## 📋 fold-cz4 Implement SFT training loop

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Implement Supervised Fine-Tuning on instruction-response pairs. Support chat format, masking of prompts.

---

## 📋 fold-obt Implement learning rate schedulers

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement cosine annealing, linear warmup, step decay. Support warmup steps, min/max LR configuration.

---

## 📋 fold-ibi Implement AdamW optimizer

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement AdamW optimizer with weight decay. Support beta1, beta2, epsilon, learning rate parameters.

---

## 📋 fold-414 Implement cross-entropy loss

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement cross-entropy loss for language modeling. Support label smoothing, ignore index for padding.

---

## 📋 fold-kiv Implement GPT model

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement complete GPT model: embedding + N transformer blocks + output projection. Configurable depth, width, heads.

### Dependencies

- ⛔ **blocks**: `fold-4et`

---

## 📋 fold-4et Implement Transformer block

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement full Transformer decoder block: attention + FFN with residuals and layer norm. Support pre-norm (GPT-2 style).

### Dependencies

- ⛔ **blocks**: `fold-t4o`
- ⛔ **blocks**: `fold-kmf`
- ⛔ **blocks**: `fold-mho`

---

## 📋 fold-4wb Implement positional embeddings

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement sinusoidal, learned, and RoPE positional embeddings. Support sequence length extrapolation.

---

## 📋 fold-t4o Implement multi-head attention

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement multi-head attention with parallel heads, projection layers. Support head dimension, number of heads.

### Dependencies

- ⛔ **blocks**: `fold-6q2`

---

## 📋 fold-6q2 Implement scaled dot-product attention

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement attention(Q,K,V) = softmax(QK^T/sqrt(d))V. Support masking for causal attention.

---

## 📋 fold-xte Implement activation functions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement GELU, ReLU, Softmax, Sigmoid, Tanh. All differentiable with proper gradients.

---

## 📋 fold-mho Implement LayerNorm

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement layer normalization: normalize over last dimension with learnable scale/bias. Support RMSNorm variant.

---

## 📋 fold-hzv Implement Embedding layer

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement token embedding lookup table. Support vocabulary size, embedding dimension, and weight tying option.

---

## 📋 fold-kmf Implement Linear layer

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement fully-connected linear layer: y = xW + b. Support weight initialization, bias option, and autodiff integration.

---

## 📋 fold-1ob Implement BPE encode/decode

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement tokenization (text→tokens) and detokenization (tokens→text). Handle special tokens, unknown tokens, byte fallback.

### Dependencies

- ⛔ **blocks**: `fold-7n0`

---

## 📋 fold-7n0 Implement BPE tokenizer training

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Implement Byte-Pair Encoding training: learn merge rules from corpus, build vocabulary. Support configurable vocab size.

---

## 🚀 fold-4r9 LLM Inference Engine Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Efficient inference for language models.

KV Cache:
- Cache allocation and management
- Cache-aware attention
- Memory-efficient updates
- Batch inference with cache

Sampling Strategies:
1. Greedy decoding
2. Temperature scaling
3. Top-k sampling
4. Top-p (nucleus) sampling
5. Beam search
6. Speculative decoding (advanced)

Generation:
- Autoregressive generation loop
- Stopping conditions (EOS, max length)
- Streaming token output
- Batched generation

Optimization:
- Quantization-ready design
- Memory-efficient inference

Location: fabric/stitches/inference/

### Dependencies

- ⛔ **blocks**: `fold-93a`
- ⛔ **blocks**: `fold-2fv`
- ⛔ **blocks**: `fold-bgq`
- ⛔ **blocks**: `fold-3yk`

---

## 🚀 fold-2d4 Training Infrastructure Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Training pipeline for neural networks.

Loss Functions:
1. Cross-entropy (language modeling)
2. Binary cross-entropy
3. MSE, MAE
4. KL divergence
5. Contrastive losses

Optimizers:
1. SGD (with momentum)
2. Adam/AdamW
3. Muon (from nanochat)
4. Learning rate schedulers (cosine, linear warmup)

Training Utilities:
- Gradient clipping (norm, value)
- Gradient accumulation
- Mixed precision (conceptual)
- Checkpointing/serialization
- Training loop abstraction

Data Pipeline:
- Batching
- Shuffling
- Sequence packing
- Data loading

Location: fabric/stitches/train/

### Dependencies

- ⛔ **blocks**: `fold-42w`
- ⛔ **blocks**: `fold-ay1`
- ⛔ **blocks**: `fold-bym`
- ⛔ **blocks**: `fold-7qr`
- ⛔ **blocks**: `fold-414`
- ⛔ **blocks**: `fold-obt`
- ⛔ **blocks**: `fold-ibi`
- ⛔ **blocks**: `fold-5c4`
- ⛔ **blocks**: `fold-ehu`

---

## 🚀 fold-93a Transformer Architecture Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Transformer architecture for language models (GPT-style).

Attention Mechanisms:
1. Scaled dot-product attention
2. Multi-head attention
3. Causal/masked attention
4. Flash Attention (memory-efficient)
5. KV Cache for inference

Positional Embeddings:
- Sinusoidal (original Transformer)
- Learned absolute positions
- RoPE (Rotary Position Embedding)
- ALiBi (Attention with Linear Biases)

Transformer Blocks:
- Pre-norm vs post-norm
- Feedforward (MLP) blocks
- Residual connections
- GPT-2/3 style decoder

Full Models:
- Configurable depth/width
- Tied embeddings option
- Vocabulary projection head

Location: fabric/stitches/transformer/

### Dependencies

- ⛔ **blocks**: `fold-42w`
- ⛔ **blocks**: `fold-6q2`
- ⛔ **blocks**: `fold-t4o`
- ⛔ **blocks**: `fold-4wb`
- ⛔ **blocks**: `fold-kiv`

---

## 🚀 fold-42w Neural Network Primitives Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Core building blocks for neural networks.

Layer Types:
1. Linear/Dense - fully connected layers
2. Embedding - token/position embeddings
3. LayerNorm - layer normalization
4. Dropout - regularization
5. Activation functions - GELU, ReLU, Softmax, Sigmoid, Tanh

Tensor Operations:
- Matrix multiply (matmul)
- Batch operations
- Reshape, transpose, permute
- Concatenation, splitting
- Broadcasting

Initialization:
- Xavier/Glorot
- He/Kaiming
- Normal, uniform

All differentiable via autodiff.

Location: fabric/stitches/nn/

### Dependencies

- ⛔ **blocks**: `fold-637`
- ⛔ **blocks**: `fold-hvm`
- ⛔ **blocks**: `fold-hzv`
- ⛔ **blocks**: `fold-mho`
- ⛔ **blocks**: `fold-kmf`
- ⛔ **blocks**: `fold-xte`
- ⛔ **blocks**: `fold-c5r`

---

## 🚀 fold-e0b Tokenization Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:41 |

### Description

Text tokenization for language models - BPE and beyond.

Core Components:
1. Byte-Pair Encoding (BPE) tokenizer
   - Training from corpus
   - Merge rules learning
   - Vocabulary management
2. Text encoding/decoding
   - UTF-8 handling
   - Special tokens (<|endoftext|>, <|pad|>, etc.)
   - Token ID ↔ string conversion
3. Vocabulary management
   - Vocab file I/O
   - Merges file I/O
   - Token frequency statistics

Advanced Features:
- SentencePiece-style unigram model
- WordPiece variant
- Regex-based pre-tokenization
- Streaming tokenization

nanochat uses Rust-based BPE - we'll do pure Scheme.

Location: fabric/stitches/tokenize/

### Dependencies

- ⛔ **blocks**: `fold-7n0`
- ⛔ **blocks**: `fold-9s3`
- ⛔ **blocks**: `fold-dgs`

---

## 📋 fold-rex Implement BigRational type

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:11 |

### Description

Implement exact rationals as BigInt/BigInt pairs. Maintain normalized form (reduced, positive denominator).

### Dependencies

- ⛔ **blocks**: `fold-btf`

---

## 📋 fold-btf Implement BigInt type and operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:02 |

### Description

Implement arbitrary precision integers with +, -, *, div, mod, gcd, lcm. Use efficient representation (digit arrays).

---

## 📋 fold-16o Implement layout algorithm

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:29 |

### Description

Implement Wadler-Lindig optimal layout algorithm. Support width and ribbon constraints, greedy and optimal modes.

### Dependencies

- ⛔ **blocks**: `fold-5k9`

---

## 📋 fold-5k9 Implement document type and primitives

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:13 |

### Description

Define Doc type with text, line, nest, group. Implement core algebra for document construction.

---

## 📋 fold-bji Implement interval elementary functions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:20 |

### Description

Implement sqrt, exp, log, sin, cos, tan, pow with interval arguments. Use range reduction and Taylor bounds.

### Dependencies

- ⛔ **blocks**: `fold-4n8`

---

## 📋 fold-4n8 Implement interval type and basic operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 16:55 |

### Description

Define Interval [lo, hi] type with arithmetic operations (+, -, *, /) using proper rounding modes for guaranteed bounds.

---

## 📋 fold-6d3 Implement evaluation strategies

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:26 |

### Description

Implement Strategy type and combinators: rpar, rseq, rdeepseq, parList, parMap, parBuffer. Support lazy strategy composition.

### Dependencies

- ⛔ **blocks**: `fold-coy`

---

## 📋 fold-coy Implement par and pseq primitives

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:50 |

### Description

Implement core parallel evaluation hints: par (spark parallel evaluation), pseq (force sequential). Ensure purity requirement.

---

## 📋 fold-b7i Implement parser combinators

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 13:59 |

### Description

Implement choice (<|>), sequence (>>), many, some, sepBy, between, optional, try, lookAhead, notFollowedBy.

### Dependencies

- ⛔ **blocks**: `fold-dtb`

---

## 📋 fold-dtb Implement core parser type and primitives

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 13:59 |

### Description

Define Parser monad type with input position, error handling. Implement char, string, satisfy, eof, any, fail primitives.

---

## 📋 fold-xuy Implement derived units and unit algebra

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 03:01 |
| **Closed** | 2026-01-02 03:01 |

### Description

Implement derived units (Newton, Joule, Watt, etc.) and unit multiplication/division at type level. (* Meters (/ Seconds)) = MetersPerSecond.

### Dependencies

- ⛔ **blocks**: `fold-cqf`

---

## 📋 fold-cqf Implement SI base units and dimension types

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:34 |

### Description

Define base dimension types (Length, Time, Mass, Current, Temperature, Amount, Luminosity) and SI units. Use type-level naturals for dimension exponents.

---

## 🚀 fold-nlv Units of Measure Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:47 |
| **Updated** | 2026-01-02 03:02 |
| **Closed** | 2026-01-02 03:02 |

### Description

Type-safe dimensional analysis using dependent types. Prevent adding meters to seconds at compile time.

Core Features:
1. Base units (Meter, Second, Kilogram, Ampere, Kelvin, Mole, Candela)
2. Derived units (Newton, Joule, Watt, Hertz, Pascal, etc.)
3. Unit arithmetic (* Meter Second⁻¹ = Velocity)
4. Dimensionless quantities and conversions
5. Unit polymorphism (works with any numeric type)

Type Examples:
- (Vec 3 (Quantity Meters Real)) - 3D position vector
- (-> (Quantity Meters a) (Quantity Seconds a) (Quantity MetersPerSecond a))
- Compile error: (+ distance time)

Integration:
- Physics library: all quantities carry units
- Linalg: unit-aware vector/matrix operations
- Autodiff: derivatives respect dimensional analysis

Location: fabric/stitches/units/

### Dependencies

- ⛔ **blocks**: `fold-2uv`
- ⛔ **blocks**: `fold-cqf`
- ⛔ **blocks**: `fold-a4i`

---

## ✨ fold-3fa Implement Differentiable type class for autodiff

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 16:41 |

### Description

Create Differentiable type class with methods: (grad f), (jacobian f), (hessian f). Implement for: Real, Vec, Matrix. Enables generic differentiation across numeric types.

### Dependencies

- ⛔ **blocks**: `fold-33n`
- ⛔ **blocks**: `fold-ay1`

---

## ✨ fold-7s8 Implement numeric type class tower

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 16:33 |

### Description

Implement numeric type class hierarchy: Num (+, -, *, negate, fromInteger), Fractional (/), Real, RealFrac, Floating (sin, cos, exp, log, sqrt). Define instance relationships: Nat ⊂ Int ⊂ Rational ⊂ Real ⊂ Complex. Location: fabric/stitches/fp/numeric.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`

---

## ✨ fold-2we Implement Foldable/Traversable

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 16:27 |

### Description

Implement Foldable (fold, foldMap, toList) and Traversable (traverse, sequence). Enable generic iteration over any container: List, Vec, Matrix, Tree, Graph. Location: fabric/stitches/fp/traversable.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`
- ⛔ **blocks**: `fold-bcs`

---

## ✨ fold-bcs Implement Functor/Applicative/Monad hierarchy

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:57 |

### Description

Implement the functor hierarchy: Functor (fmap), Applicative (pure, <*>), Monad (>>=, return). Include laws checking in tests. Derive instances for List, Option, Result, Vec, Matrix. Location: fabric/stitches/fp/functor.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`
- ⛔ **blocks**: `fold-5sa`

---

## ✨ fold-33n Implement type class system

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:55 |

### Description

Implement type class mechanism for ad-hoc polymorphism. Support class definitions, instance declarations, and constraint solving. Core classes: Eq, Ord, Show, Read, Bounded, Enum. Location: fabric/stitches/fp/typeclass.ss

### Dependencies

- ⛔ **blocks**: `fold-k3g`

---

## 📋 fold-z7y Tooling metadata index + symbol graph

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:24 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:15 |

### Description

Build a persistent index of definitions, references, types, and module boundaries. Expose APIs for querying symbol locations, call graphs, and dependency edges to power other tooling.

---

## ✨ fold-7nh Implement complex number arithmetic

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:58 |

### Description

Fundamental complex number support for signal processing and advanced math.

Features:
1. Complex number representation
   - Real and imaginary parts
   - Polar form (magnitude, phase)
   - Conversion between forms

2. Basic operations
   - Addition, subtraction, multiplication, division
   - Conjugate
   - Absolute value (magnitude)
   - Argument (phase)

3. Advanced operations
   - Complex exponential (Euler's formula)
   - Complex logarithm
   - Complex powers and roots
   - Trigonometric functions

4. Vector/matrix support
   - Complex vectors
   - Complex matrices
   - Hermitian operations

Required by:
- FFT/DFT algorithms
- Signal processing
- Quantum computing (future)
- Control systems

Location: fabric/stitches/complex.ss

### Dependencies

- ⛔ **blocks**: `fold-hvm`
- ⛔ **blocks**: `fold-j8q`

---

## 📋 fold-dco Implement entropy calculations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:17 |

### Description

Implement Shannon entropy, conditional entropy, joint entropy, and mutual information calculations for discrete and continuous distributions

### Dependencies

- ⛔ **blocks**: `fold-9c6`

---

## 📋 fold-tsz Implement basic arithmetic operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:01 |

### Description

Implement addition, subtraction, multiplication, and division for arbitrary precision numbers with proper error handling and edge cases

### Dependencies

- ⛔ **blocks**: `fold-3m5`

---

## 📋 fold-3m5 Design arbitrary precision number representation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:00 |

### Description

Design and implement the core data structures for arbitrary precision numbers including big integers, big decimals, and rational numbers

---

## ✨ fold-uv4 Implement numerical integration methods library

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:59 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:32 |

### Description

Shared numerical integration methods for physics and scientific computing.

Methods:
1. Explicit methods
   - Euler method
   - Midpoint method
   - RK4 (Runge-Kutta 4th order)
   - RK45 (adaptive step)

2. Implicit methods
   - Backward Euler
   - Trapezoidal
   - BDF methods

3. Symplectic methods (for physics)
   - Verlet integration
   - Leapfrog
   - Symplectic Euler

Features:
- Adaptive step size control
- Error estimation
- Energy conservation metrics
- Convergence analysis

Shared by:
- Physics engine integrators
- ODE solvers
- Autodiff (adjoint methods)

Location: fabric/stitches/numerical/

### Dependencies

- ⛔ **blocks**: `fold-o7b`

---

## 📋 fold-e2n Monte Carlo Methods Implementation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:57 |
| **Updated** | 2026-01-02 23:00 |
| **Closed** | 2026-01-02 23:00 |

### Description

Implement Monte Carlo integration, importance sampling, rejection sampling, particle filtering, and stochastic simulation tools. Include variance reduction techniques.

### Dependencies

- ⛔ **blocks**: `fold-9c6`
- ⛔ **blocks**: `fold-ek7`

---

## 📋 fold-9c6 Probability Distributions Library

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:56 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 18:45 |

### Description

Implement common probability distributions (Normal, Poisson, Binomial, Exponential, Gamma, Beta, Uniform). Include PDF, CDF, quantile functions, random sampling, and parameter estimation.

### Dependencies

- ⛔ **blocks**: `fold-ek7`
- ⛔ **blocks**: `fold-6ys`

---

## 📋 fold-go9 Convolution and Correlation Operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:55 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 00:01 |

### Description

Implement efficient convolution algorithms (direct, FFT-based), cross-correlation, auto-correlation, and matched filtering. Include support for real-time streaming convolution.

### Dependencies

- ⛔ **blocks**: `fold-dnu`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-7nh`

---

## 📋 fold-dnu Design DFT/FFT Core Algorithms

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:55 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 23:21 |

### Description

Implement Discrete Fourier Transform and Fast Fourier Transform algorithms as the foundation for signal processing. Include radix-2 Cooley-Tukey algorithm, complex number operations, and efficient vectorized implementations.

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-7nh`

---

## ✨ fold-thv Implement length-indexed vectors and dimension-safe matrices

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 16:42 |
| **Closed** | 2026-01-02 16:42 |

### Description

Classic dependent types application: statically verify vector and matrix dimensions.

Features:
1. Vec n A - Vectors with compile-time known length
2. Matrix m n A - Matrices with compile-time dimensions  
3. Dimension-safe operations:
   - vec-append : Vec n A → Vec m A → Vec (n+m) A
   - matrix-mult : Matrix m n A → Matrix n p A → Matrix m p A
   - vec-zip : Vec n A → Vec n B → Vec n (A × B)

Implementation:
- Use Pi types for length indexing
- Use type-level Nat for dimensions
- Dimension errors become type errors
- Zero-cost abstraction (erased at runtime)

Integration:
- Wraps existing linalg vector/matrix implementations
- Provides typed interface on top
- Gradual adoption (can mix typed/untyped)

Examples:
- Safe array indexing: index : (i : Nat) → {p : i < n} → Vec n A → A
- Matrix dimension matching
- Reshape with proof obligations

Requires: Pi types, Sigma types, type-level computation, linalg vectors/matrices

Location: fabric/stitches/ (new dependent-linalg module)

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-7yh`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`

---

## 📋 fold-ep7 Implement 2D Physics Integrator

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:36 |

### Description

Create physics integrator: Euler, Verlet integration with time step management and sub-stepping

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-e1l`
- ⛔ **blocks**: `fold-uv4`

---

## 📋 fold-kgm Implement 2D Collision Response

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:41 |

### Description

Create collision response: impulse resolution, restitution, friction, position correction for stable stacking

### Dependencies

- ⛔ **blocks**: `fold-e1l`

---

## 📋 fold-zc3 Implement 2D Collision Detection

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:50 |

### Description

Create collision detection: AABB, circle-circle, circle-rectangle, polygon collision with SAT algorithm

### Dependencies

- ⛔ **blocks**: `fold-e1l`
- ⛔ **blocks**: `fold-01l`

---

## 📋 fold-221 Implement 2D Force System

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:37 |

### Description

Create force application system: gravity, friction, springs, user-defined forces with force accumulator

### Dependencies

- ⛔ **blocks**: `fold-e1l`

---

## 📋 fold-e1l Implement 2D Vector Math Library

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:49 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:03 |

### Description

Create vector2D struct with basic operations: addition, subtraction, multiplication, division, dot product, cross product, magnitude, normalization

### Dependencies

- ⛔ **blocks**: `fold-o7b`

---

## 🚀 fold-43e Create 2D Physics Engine Core Architecture

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:48 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:57 |

### Description

Design and implement the core architecture for a 2D physics engine including vectors, forces, collisions, and integrators

### Dependencies

- ⛔ **blocks**: `fold-e1l`
- ⛔ **blocks**: `fold-221`
- ⛔ **blocks**: `fold-zc3`
- ⛔ **blocks**: `fold-kgm`
- ⛔ **blocks**: `fold-ep7`
- ⛔ **blocks**: `fold-nlv`

---

## ✨ fold-qna Update type inference for dependent types

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:44 |

### Description

Extend bidirectional type inference to handle dependent types.

Changes:
1. Type synthesis for Pi/Sigma introduction and elimination
2. Type checking with dependent types
3. Unification with type-level computation
4. Implicit argument inference

Challenges:
- Higher-order unification (undecidable in general)
- Constraint solving with type-level terms
- Mode inference for implicit arguments
- Error recovery and reporting

Implementation:
- Extend synthesis/checking judgments
- Add constraint generation for implicit args
- Implement unification with normalization
- Add occurs check for dependent vars

Inference Rules:
- Π-intro: infer lambda type from annotation or context
- Π-elim: infer application result with substitution
- Σ-intro: infer pair type
- Σ-elim: handle dependent projection

Testing:
- Implicit argument inference
- Type-level computation in inference
- Complex dependent patterns
- Error message quality

Location: fabric/stitches/infer.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-w5k`

---

## ✨ fold-7yh Implement type-level computation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:49 |

### Description

Enable terms to appear in types and compute during type checking.

Core Features:
1. Type normalization: reduce type expressions to normal form
2. Definitional equality: types equal if they normalize to same form
3. Term evaluation during type checking
4. Computation rules for type-level operations

Implementation:
- Normalization-by-evaluation (NbE) for efficiency
- Quote/unquote for terms in types
- Computation under binders
- Strong normalization guarantee

Type-Level Operations:
- Arithmetic in types: Vec (n + m) A
- Conditional types: if b then A else B
- Type-level functions: (λ (n : Nat) (Vec n Int))

Challenges:
- Decidability (require termination)
- Efficiency of normalization
- Interaction with inference
- Error messages for stuck terms

Testing:
- Simple arithmetic normalization
- Conditional type reduction
- Function application at type level
- Nested computations

Location: fabric/stitches/eval.ss, types.ss, infer.ss

### Dependencies

- ⛔ **blocks**: `fold-po9`

---

## ✨ fold-w5k Implement universe hierarchy

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:45 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:35 |

### Description

Implement universe stratification to avoid Type : Type paradox.

Design:
  Type₀ : Type₁ : Type₂ : ...  (infinite hierarchy)
  or
  Type : Type (with universe polymorphism constraints)

Implementation Options:
1. Explicit universe levels: Type 0, Type 1, etc.
2. Universe polymorphism: (∀ (l : Level) (Type l))
3. Cumulative universes: Type₀ <: Type₁
4. Predicative vs impredicative lowest level

Features:
- Universe inference (minimize levels automatically)
- Level constraints and solving
- Universe polymorphism for library code
- Lift operation for explicit level raising

Key Rules:
- (Π (x:A) B) : Type(max(level A, level B))
- (Σ (x:A) B) : Type(max(level A, level B))
- Inductive types: level based on constructor arguments

Error Handling:
- Clear error messages for universe inconsistencies
- Suggest fixes for level mismatches

Location: fabric/stitches/types.ss, kinds.ss

### Dependencies

- ⛔ **blocks**: `fold-po9`

---

## ✨ fold-dz1 Implement Sigma types (dependent pairs)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:45 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:35 |

### Description

Implement Sigma types (Σ) for dependent pairs where second component type depends on first value.

Syntax:
  (Σ (x : A) B)  ; Pair where second element has type B[fst/x]
  (Σ (n : Nat) (Vec n Int))  ; Pair of length and vector of that length

Implementation:
1. Extend type grammar for Sigma types
2. Type formation: if A : Type and B : Type under x:A, then (Σ (x:A) B) : Type
3. Introduction (pair): (x , y) : (Σ (x:A) B) when x:A and y:B[x/x]
4. Elimination (projections):
   - fst : (Σ (x:A) B) → A
   - snd : (p : (Σ (x:A) B)) → B[fst p/x]
5. Pattern matching on dependent pairs

Use Cases:
- Existential types (∃) as special case
- Length-indexed data structures
- Refinement types (n : Nat, proof : n > 0)
- Record types as iterated Sigma

Testing:
- Simple dependent pairs
- Nested Sigma types
- Extraction and reconstruction

Location: fabric/stitches/types.ss

### Dependencies

- ⛔ **blocks**: `fold-po9`

---

## ✨ fold-rus Implement Pi types (dependent function types)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:45 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:34 |

### Description

Implement Pi types (Π) which generalize ∀ to allow types to depend on values.

Syntax:
  (Π (x : A) B)  ; Type of functions from A to B where B may mention x
  (Π (n : Nat) (Vec n Int))  ; Functions from naturals to vectors of that length

Implementation:
1. Extend type grammar for Pi types
2. Type formation rule: if A : Type and B : Type under x:A, then (Π (x:A) B) : Type
3. Introduction rule (lambda): (λ (x : A) e) : (Π (x:A) B) when e : B under x:A
4. Elimination rule (application): if f : (Π (x:A) B) and a : A, then (f a) : B[a/x]

Key Features:
- Substitution must handle term-in-type substitution
- Type equality must normalize types before comparison
- Implicit arguments (inference for common patterns)

Testing:
- Identity function: (Π (A : Type) (Π (x : A) A))
- Dependent application
- Type-level computation in return type

Location: fabric/stitches/types.ss, infer.ss

### Dependencies

- ⛔ **blocks**: `fold-po9`

---

## 📋 fold-yqo Integrate autodiff with evaluation engine

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:41 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 01:52 |

### Description

Integrate automatic differentiation capabilities with the existing fuel-based evaluation engine. This includes:

- Modify evaluator to track computational graph during execution
- Implement automatic graph construction for differentiable expressions
- Add gradient computation as evaluation phase
- Extend fuel system to account for differentiation costs
- Handle evaluation suspension/resumption with gradients
- Integration with existing evaluation hooks and tracing
- Performance optimizations for repeated differentiations

Location: fabric/stitches/ (modify evaluator modules)
Critical for seamless autodiff usage in programs.

### Dependencies

- ⛔ **blocks**: `fold-m3u`

---

## 📋 fold-c3i Implement Jacobian and Hessian computation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:40 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:27 |

### Description

Implement higher-order derivative computation utilities. This includes:

- Efficient Jacobian matrix computation algorithms
- Hessian matrix computation for second-order optimization
- Vector-Jacobian product (VJP) and Jacobian-Vector product (JVP)
- Automatic selection of forward vs reverse mode based on dimensions
- Sparse Jacobian computation for large systems
- Memory-efficient Hessian-vector products
- Integration with both forward and reverse mode

Location: fabric/stitches/ (new higher-order-diff module)
Essential for advanced optimization and scientific computing applications.

### Dependencies

- ⛔ **blocks**: `fold-m3u`
- ⛔ **blocks**: `fold-81x`

---

## 📋 fold-m3u Implement reverse mode differentiation (backpropagation)

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:40 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 08:13 |

### Description

Implement reverse-mode automatic differentiation (backpropagation). This includes:

- Backward pass algorithm with gradient accumulation
- Adjoint computation for all primitive operations
- Efficient memory management through checkpointing
- Gradient tape design and implementation
- Handling of shared subexpressions and multiple outputs
- Fuel cost tracking for backward pass
- Numerical stability considerations

Location: fabric/stitches/ (new reverse-diff module)
Critical for efficient neural network training and optimization.

### Dependencies

- ⛔ **blocks**: `fold-z8h`

---

## 📋 fold-z8h Implement forward mode differentiation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:40 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:52 |

### Description

Implement forward-mode automatic differentiation (tangent mode). This includes:

- Dual number implementation and arithmetic
- Forward pass algorithm with tangent propagation
- Efficient computation of directional derivatives
- Integration with computational graph construction
- Memory-efficient Jacobian computation for vector outputs
- Fuel cost tracking for forward differentiation
- Error handling and edge cases

Location: fabric/stitches/ (new forward-diff module)
Provides foundation for reverse mode and simple gradient computations.

### Dependencies

- ⛔ **blocks**: `fold-o5o`
- ⛔ **blocks**: `fold-7z1`

---

## 📋 fold-7z1 Create gradient-aware primitive wrappers

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:39 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:52 |

### Description

Wrap existing mathematical primitives to support gradient computation. This includes:

- Forward-mode gradient definitions for all math primitives
- Reverse-mode gradient computation (backward pass)
- Partial derivative implementations for scalar operations
- Chain rule application helpers
- Gradient accumulation patterns
- Fuel cost tracking for gradient computations
- Error handling for non-differentiable operations

Location: fabric/stitches/ (extend primitive definitions)
Essential for making existing functions differentiable.

### Dependencies

- ⛔ **blocks**: `fold-o5o`

---

## 📋 fold-o5o Implement computational graph data structure

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:39 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:51 |

### Description

Design and implement the core computational graph representation for autodiff. This includes:

- Node structure for operations, variables, and constants
- Edge representation for data flow and gradient flow
- Graph construction API that integrates with AST evaluation
- Efficient storage using the existing block system
- Graph traversal algorithms (topological sort, backward traversal)
- Checkpoint/restart mechanism for memory efficiency
- Integration with content-addressed storage

Location: fabric/stitches/ (new computational graph module)
Critical foundation for both forward and reverse mode differentiation.

### Dependencies

- ⛔ **blocks**: `fold-i5d`

---

## 📋 fold-uw9 Design differentiable type system extensions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:39 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 00:28 |

### Description

Extend the existing type system to support differentiable types. This includes:

- Design Differentiable type constructor that wraps primitive types (Nat, Int, Float)
- Define gradient types and storage mechanisms
- Extend type inference to track differentiable expressions
- Design type-level constraints for automatic differentiation
- Integration with existing fuel-based cost tracking
- Documentation and type rules specification

Location: fabric/stitches/ (extend type system files)
Blocks completion of other Phase 1 tasks.

### Dependencies

- ⛔ **blocks**: `fold-o5o`

---

## ✨ fold-f9a Implement graph Laplacian matrices

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:37 |
| **Updated** | 2026-01-02 22:23 |
| **Closed** | 2026-01-02 22:23 |

### Description

Construct and analyze graph Laplacian matrices for spectral graph theory.

Laplacian Types:
- Unnormalized Laplacian (L = D - A)
- Normalized symmetric Laplacian (L_sym = I - D^(-1/2) A D^(-1/2))
- Random walk Laplacian (L_rw = I - D^(-1) A)

Features:
- Degree matrix construction
- Laplacian matrix construction
- Laplacian eigenvalue/eigenvector computation
- Algebraic connectivity (Fiedler value)

Applications:
- Graph partitioning
- Community detection
- Spectral clustering
- Graph connectivity analysis

Requires adjacency matrix representation and eigenvalue computation.

### Dependencies

- ⛔ **blocks**: `fold-10x`

---

## ✨ fold-10x Implement adjacency matrix representation for graphs

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:37 |
| **Updated** | 2026-01-02 16:23 |
| **Closed** | 2026-01-02 16:23 |

### Description

Add matrix-based graph representation to complement edge-list representation.

Features:
- Convert graph edge lists to adjacency matrices
- Support for weighted and unweighted graphs
- Directed and undirected graph support
- Efficient sparse matrix representation for large graphs

Integration:
- Use linalg matrix data structures
- Enable matrix-based graph algorithms
- Support spectral graph analysis

Benefits:
- O(1) edge existence queries
- Enables linear algebra graph algorithms
- Foundation for PageRank, spectral clustering

### Dependencies

- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-1ws`

---

## ✨ fold-81x Implement matrix data structure and basic operations

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:29 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:00 |

### Description

Core matrix implementation with fundamental operations.

Operations:
- Matrix creation (make-matrix, matrix-from-lists)
- Element access (matrix-ref, matrix-shape)
- Matrix transpose
- Basic arithmetic (matrix-add, matrix-sub, matrix-scale)
- Matrix-vector multiplication
- Matrix-matrix multiplication

Include comprehensive tests.

### Dependencies

- ⛔ **blocks**: `fold-hvm`

---

## ✨ fold-o7b Implement vector data structure and basic operations

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:29 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:56 |

### Description

Core vector implementation with fundamental operations.

Operations:
- Vector creation (make-vector, vector-from-list)
- Element access (vector-ref, vector-length)
- Basic arithmetic (vector-add, vector-sub, vector-scale)
- Dot product
- Vector norms (L1, L2, Linf)
- Vector equality/comparison

Include comprehensive tests.

### Dependencies

- ⛔ **blocks**: `fold-hvm`

---

## 📋 fold-l01 Create comprehensive benchmarking suite

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:27 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:10 |

### Description

Implement benchmarking framework for all graph algorithms with performance metrics, memory usage tracking, and scalability tests. Include comparisons with different graph sizes and structures.

---

## 📋 fold-et1 Optimize queue and stack operations for performance

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:27 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:08 |

### Description

Improve queue-enqueue, queue-dequeue, stack-push, stack-pop operations for better performance on large graphs. Consider using more efficient data structures.

---

## 📋 fold-woc Optimize visited set tracking with hash tables

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:27 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:07 |

### Description

Replace list-based visited sets with more efficient hash table implementations for large graphs. Profile current performance and implement optimized data structures.

---

## 📋 fold-xph Create unit tests for centrality and subgraph operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:27 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:40 |

### Description

Write tests for in-degree, out-degree, find-hubs, find-roots, find-leaves, reachable-from, ancestors-of, subgraph, neighborhood including accuracy, edge cases, and performance.

---

## 📋 fold-8sh Create unit tests for graph analysis algorithms

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:27 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:25 |

### Description

Write tests for connected-components, find-cycles, topological-sort including correctness, edge cases, performance on large graphs, cycle detection accuracy, and topological sort validation.

---

## 📋 fold-np4 Create unit tests for pathfinding algorithms

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:26 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 05:51 |

### Description

Write tests for shortest-path, all-paths, path-exists? including correctness verification, cycle handling, disconnected graphs, single-node cases, and path validation.

---

## 📋 fold-45y Create unit tests for traversal primitives (BFS, DFS)

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:24 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 05:43 |

### Description

Write comprehensive tests for bfs-traverse, dfs-traverse, bfs-traverse-reverse including edge cases, visited set verification, proper callback handling, and integration with store-api.

---

## 📋 fold-d3o Add performance benchmarks and optimizations to graph algorithms

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:23 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:32 |

### Description

Implement benchmarking suite for all graph algorithms. Optimize critical paths, improve visited set tracking with better data structures, add memoization for expensive operations, and profile large graph performance.

---

## 📋 fold-din Create comprehensive test suite for graph algorithms library

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:23 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:41 |

### Description

Create thorough tests for all graph algorithms including traversal, pathfinding, cycle detection, topological sort, centrality metrics, and subgraph operations. Include edge cases, performance tests, and integration with store-api.

### Dependencies

- ⛔ **blocks**: `fold-45y`
- ⛔ **blocks**: `fold-np4`
- ⛔ **blocks**: `fold-8sh`
- ⛔ **blocks**: `fold-xph`

---

## 🚀 fold-24s Graph Algorithms Library Epic: Comprehensive Testing, Documentation, and Performance Optimizations

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:23 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:18 |

### Dependencies

- ⛔ **blocks**: `fold-din`
- ⛔ **blocks**: `fold-d3o`
- ⛔ **blocks**: `fold-ggs`
- ⛔ **blocks**: `fold-yp7`
- ⛔ **blocks**: `fold-1y3`
- ⛔ **blocks**: `fold-abw`
- ⛔ **blocks**: `fold-i5d`

---

## 🚀 fold-2uv Dependent Type System Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:19 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:17 |

### Description

Design and implement a dependent type system for Fold, including type-level computation, refinement types, theorem proving capabilities, and integration with the existing module system and runtime type checking.

### Dependencies

- ⛔ **blocks**: `fold-po9`
- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-w5k`
- ⛔ **blocks**: `fold-7yh`
- ⛔ **blocks**: `fold-qna`
- ⛔ **blocks**: `fold-14l`
- ⛔ **blocks**: `fold-0qs`

---

## 🚀 fold-ay1 Autodiff Engine Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:19 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 04:22 |

### Description

Implement a comprehensive automatic differentiation engine for the Fold programming language, including forward and reverse mode differentiation, gradient computation, and integration with the existing type system and runtime.

---

## ✨ fold-ov2 Create core string utilities module

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:16 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 05:45 |

### Description

Extract and enhance string utility functions from patterns modules into a dedicated fabric/stitches/string-utils.ss core module. Include essential functions like string-split, string-upcase, string-downcase, string-trim that are currently missing or only available in patterns.

---

## 📋 fold-2rj Audit and migrate essential list utilities to core

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:16 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:02 |

### Description

Review all list/collection utilities in fabric/patterns/ and migrate essential ones to fabric/stitches/prelude.ss. Check collection-utils.ss, data-structures.ss for functions that should be core-available like basic stack/queue operations, filter variants, etc.

### Dependencies

- ⛔ **blocks**: `fold-5mq`

---

## 📋 fold-5mq Migrate foldr from patterns to core prelude

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:16 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 05:42 |

### Description

Move foldr implementation from fabric/patterns/collection-utils.ss to fabric/stitches/prelude.ss so it's available in standard REPL sessions. Currently foldr exists but isn't accessible to users without loading collection utilities.

---

## 📋 fold-e43 SDK patch policy: naming, versions, provides

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:04 |

### Description

Define conventions for SDK patches: patch names (loom/quill/satin), versioning strategy, minimal provides list (entrypoints only), and how patch 'requires' should mirror SDK dependencies.

---

## ✨ fold-dre Create comprehensive BoardCraft SDK tutorial series

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:59 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 01:09 |

### Description

Develop a series of progressive tutorials for BoardCraft: 1) Basic hex board operations, 2) Unit placement and movement, 3) Turn-based gameplay, 4) Combat mechanics, 5) Victory conditions. Each should be a working example that builds on the previous one. Critical for adoption of this excellent SDK.

### Dependencies

- ⛔ **blocks**: `fold-lx9`
- ⛔ **blocks**: `fold-7vk`
- ⛔ **blocks**: `fold-cz8`

---

## ✨ fold-lx9 REPL needs interactive help system for newcomers

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:59 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 01:09 |

### Description

The REPL lacks basic help functionality. New users should be able to type (help) to see available functions, (help 'function-name) for specific docs, and (examples 'topic) for usage examples. This is critical for onboarding and discovery of the system's capabilities.

### Dependencies

- ⛔ **blocks**: `fold-qxq`
- ⛔ **blocks**: `fold-cz8`

---

## ✨ fold-0mc Satin compiler: expand, validate, source-map

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:22 |

### Description

Implement macro expansion + compilation pipeline into Quill structures, with strong validation and source-mapped error messages (node id, form path, optional file/line via reader annotations if available).

### Dependencies

- ⛔ **blocks**: `fold-4hs`
- ⛔ **blocks**: `fold-v98`
- ⛔ **blocks**: `fold-53s`

---

## ✨ fold-mpq Satin education forms: exercises, rubrics, hints

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:22 |

### Description

Add authoring forms for education: (lesson ...), (objective ...), (exercise ...), (mcq ...), (short-answer ...), (code-task ...), (rubric ...), (hint ...), (feedback ...), (mastery ...). Must compile to Quill education nodes + transcript/assessment events.

### Dependencies

- ⛔ **blocks**: `fold-4hs`
- ⛔ **blocks**: `fold-fjo`

---

## ✨ fold-4hs Satin core forms: story, node, choice, cond, action

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:16 |

### Description

Implement the foundational forms: (story ...), (node ...)/(scene ...), (choice ...), (when ...)/(if ...), (do ...), (set ...)/(inc ...), (emit ...), (goto ...), (end ...). Output: Quill DSL/runtime structures.

### Dependencies

- ⛔ **blocks**: `fold-2fn`

---

## 📋 fold-2fn Satin: DSL principles + syntax spec

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:15 |

### Description

Define surface syntax (S-expr forms), naming conventions, source-location strategy, determinism rules, and the mapping to Quill runtime concepts (node, choice, intent, effects, transcript).

---

## ✨ fold-vzc Quill narrative engine: rules, dialogue, quests, timeline

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:37 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Advanced narrative features: event/rule system, dialogue trees with stateful topics, quest tracking, scheduled/timed events, and optional branching timelines/time-travel using snapshots.

### Dependencies

- ⛔ **blocks**: `fold-53s`
- ⛔ **blocks**: `fold-u2c`
- ⛔ **blocks**: `fold-dtr`
- ⛔ **blocks**: `fold-v98`

---

## ✨ fold-fjo Quill education layer: exercises + validation + progress

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:37 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Add lesson/exercise node types (MCQ, short answer, code tasks), validators/rubrics, hints, and progress tracking; integrate with thimble/tutorial.ss patterns where sensible.

### Dependencies

- ⛔ **blocks**: `fold-53s`
- ⛔ **blocks**: `fold-u2c`
- ⛔ **blocks**: `fold-02a`
- ⛔ **blocks**: `fold-dtr`
- ⛔ **blocks**: `fold-vzc`

---

## ✨ fold-u2c Quill input: intent model + command parser

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Define an intent schema (look/move/take/use/choose/help/etc) and parsing pipeline using fabric/stitches/parse.ss; support both menu-choice input and free-form commands.

### Dependencies

- ⛔ **blocks**: `fold-v98`

---

## ✨ fold-53s Quill content DSL + validator

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Define an S-expression DSL for stories (nodes, choices, conditions, actions, metadata) plus validation (missing refs, unreachable nodes, type/shape checks) and compilation to runtime structures.

### Dependencies

- ⛔ **blocks**: `fold-v98`

---

## ✨ fold-v98 Quill core runtime: story graph + state + effects

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Implement the minimal deterministic runtime: Story/Node structures, State (vars, inventory, location, time), and an effect system so logic stays pure while IO/side-effects are handled by adapters.

### Dependencies

- ⛔ **blocks**: `fold-l97`

---

## 📋 fold-l97 Quill: Module layout + public API

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Choose location (likely thimble/quill/), define load order entrypoint, public API names, and how Quill integrates with thimble/repl.ss and fabric/stitches/parse.ss.

### Comments

> **oso** (2025-12-29)
>
> Correction: Quill is a playpen SDK. Moved implementation from thimble/ to playpen/quill/ (entrypoint now playpen/quill/quill.ss).

---

## 🚀 fold-10r Quill: Text Adventure + Narrative SDK

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:31 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Create a text-adventure SDK for The Fold: educational content primitives + a deep narrative engine (state, rules, dialogue), REPL-first authoring, deterministic simulation, and CAS-backed persistence.

### Dependencies

- ⛔ **blocks**: `fold-l97`
- ⛔ **blocks**: `fold-v98`
- ⛔ **blocks**: `fold-53s`
- ⛔ **blocks**: `fold-u2c`
- ⛔ **blocks**: `fold-02a`
- ⛔ **blocks**: `fold-dtr`
- ⛔ **blocks**: `fold-fjo`
- ⛔ **blocks**: `fold-vzc`
- ⛔ **blocks**: `fold-v5h`
- ⛔ **blocks**: `fold-ant`
- ⛔ **blocks**: `fold-gyk`
- ⛔ **blocks**: `fold-q81`

---

## 🐛 fold-uz5 Fix missing test files in shell test suite

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:57 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:22 |

### Description

Three shell tests are failing because files are not found in source directories: test-validate.ss, test-block-index.ss, test-duckie-persist.ss. The error shows 'file ~s not found in source directories' suggesting a formatting issue in the error message too.

---

## 📋 fold-5kr fold-qks

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:25 |

### Description

Add input validation for MCP server tools - Add input validation for fold_post and fold_chat tools in MCP server to prevent injection attacks and validate data types

---

## 🐛 fold-2jf fold-mp2

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:24 |

### Description

Fix TypeScript build permissions in mcp-server - Fix TypeScript build permissions issue where tsc command fails with Permission denied in mcp-server

---

## 📋 fold-vva fold-ne4

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:24 |

### Description

Update Node.js dependencies in mcp-server - Update outdated Node.js dependencies in thimble/mcp-server/package.json - @types/node from 22.19.3 to 25.0.3

---

## 🐛 fold-qks Add input validation for MCP server tools

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:31 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:25 |

### Description

Enhance input validation for fold_post, fold_chat, and other MCP server tools to prevent injection attacks and validate data types.

---

## 🐛 fold-mp2 Fix TypeScript build permissions in mcp-server

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:30 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:24 |

### Description

tsc command has permission denied error when building MCP server. Fix build process and ensure TypeScript compilation works properly.

---

## 🧹 fold-ne4 Update Node.js dependencies in mcp-server

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:30 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:24 |

### Description

Update @types/node from 22.19.3 to 25.0.3 and check for other outdated dependencies in thimble/mcp-server/package.json

---

## 🐛 fold-5kb Fix failing tests in thimble test suite

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:06 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:22 |

### Description

Multiple tests in thimble/tests/ are failing, indicating underlying issues:

Current failures:
- tests/test-validate.ss: 'file ~s not found in source directories'
- tests/test-block-index.ss: 'file ~s not found in source directories'
- tests/test-toolkit.ss: 11 tests failed
- tests/test-watch-integration.ss: 'Test 1: Loading watch system... ✗'

Likely root causes:
1. Hardcoded paths (affects validate and block-index tests)
2. Missing dependencies or load order issues
3. Configuration or environment issues

Need comprehensive review and fixes to ensure test reliability.

Priority: P1 - Test failures indicate broken functionality

### Dependencies

- ⛔ **blocks**: `fold-3jj`
- 🔗 **related**: `fold-uz5`

---

## ✨ fold-fig Complete Unicode NFC normalization implementation in thimble/text.ss

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:06 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 05:41 |

### Description

The normalize-nfc function currently only supports ASCII and errors on non-ASCII text. TODO comment indicates need for full NFC using Unicode data tables.

Current implementation:
- ASCII-only text passes through
- Non-ASCII text triggers error: 'NFC normalization required for non-ASCII text'

Required:
- Implement proper Unicode NFC normalization
- Requires Unicode data tables for composition
- Essential for proper Unicode text handling

Location: thimble/text.ss line 183

Priority: P1 - Important for internationalization and text handling

---

## 🚀 fold-wgs Rust Core Feature Parity with Chez Scheme

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ⚡ High (P1) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 21:52 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 22:21 |

### Description

Complete the Rust migration to remove Chez Scheme as a dependency. Includes primitives, normalization, expansion, cross-validation, and shell cutover.

---

## 🐛 fold-7tfx Fix vec-pure/vec-ap Applicative pattern inconsistency

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-03 01:06 |
| **Updated** | 2026-01-03 01:18 |
| **Closed** | 2026-01-03 01:18 |

### Description

Gemini review finding (high confidence): vec-pure creates length-1 vector but vec-ap requires equal lengths, breaking Applicative pattern.

## Root Cause
- vec-pure x returns vector of length 1: (vector x)
- vec-ap fs xs enforces (= (vec-length fs) (vec-length xs))
- Standard Applicative behavior implies broadcasting singleton to match other structure
- (vec-ap (vec-pure f) (vec 1 2 3)) crashes because length 1 != length 3

## Implementation Steps
1. Modify vec-ap to handle broadcasting:
2. Check lengths nf and nx
3. **Case 1**: If nf == 1 and nx > 1, broadcast fs[0] to all xs. Result length nx.
4. **Case 2**: If nx == 1 and nf > 1, broadcast xs[0] to all fs. Result length nf.
5. **Case 3**: If nf == nx, perform standard element-wise application.
6. **Else**: Raise dimension mismatch error.

## Test Cases
- (vec-ap (vec-pure (lambda (x) (+ x 1))) (vec 1 2 3)) -> (vec 2 3 4)
- (vec-ap (vec f1 f2) (vec-pure 5)) -> (vec (f1 5) (f2 5))

## Complexity: Simple

---

## 🐛 fold-tw6k Fix sim-sample for variable-timestep simulations

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-03 01:06 |
| **Updated** | 2026-01-03 01:18 |
| **Closed** | 2026-01-03 01:18 |

### Description

Gemini review finding (medium-high confidence): sim-sample calculates step-count only once based on first interval (line 239).

## Root Cause
sim-sample calculates step-count once based on initial dt: (floor (/ interval (sim-state-time ...))).
For variable-timestep simulations (simulate-varying-dt), this static step count causes drift or incorrect sampling times.

## Implementation Steps
1. Rewrite sim-sample to be stateful regarding time
2. Maintain next-sample-time variable (initially start-time + interval)
3. In stream loop, consume elements until current-state-time >= next-sample-time
4. Yield that state
5. Update next-sample-time by adding interval
6. Repeat

## Test Cases
- Create variable timestep stream (dt oscillates between 0.1 and 0.5)
- Sample at interval = 1.0
- Verify sampled states have times close to 1.0, 2.0, 3.0 regardless of varying steps

## Complexity: Simple

---

## 🐛 fold-hls0 Fix sparse autodiff to avoid dense matrix allocation

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-03 01:06 |
| **Updated** | 2026-01-03 01:33 |
| **Closed** | 2026-01-03 01:33 |

### Description

Gemini review finding (high confidence): detect-sparsity and sparse-hessian-exact allocate dense matrices first, defeating sparsity purpose.

## Root Cause
- **detect-sparsity**: Calls (jacobian f args) which allocates full dense M×N matrix before filtering. O(MN) space.
- **sparse-hessian-exact**: Calls (hessian-forward f args) allocating full dense N×N matrix.

## Implementation Steps
1. **Refactor detect-sparsity**:
   - Remove call to jacobian
   - Implement reverse-mode loop (like jacobian-reverse but specialized)
   - For each output i, compute gradient, iterate non-zero partials, add (i,j) to pattern
   - Construct SparsityPattern directly from indices

2. **Refactor sparse-hessian-exact**:
   - Remove call to hessian-forward
   - Implement hessian-forward logic inline with sparse accumulator
   - Collect non-zero entries (i,j,value) into list/buffer
   - Return SparseCOO from triplets

## Test Cases
- Large diagonal f(x) = (x1^2, x2^2, ..., x1000^2) - verify O(N) entries, not O(N^2)
- Sparse Hessian: f(x) = sum(xi*xi+1) - verify tridiagonal, no dense intermediates

## Complexity: Medium

---

## 🐛 fold-ozhf Add negative discriminant check in Wilkinson shift

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 15:53 |
| **Updated** | 2026-01-02 15:57 |
| **Closed** | 2026-01-02 15:57 |

---

## 📋 fold-48oi Scheduled forum agents not posting to discord

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 04:23 |
| **Updated** | 2026-01-02 04:30 |
| **Closed** | 2026-01-02 04:30 |

### Description

Forum agents previously ran a few times an hour, I haven't seen any activity from them since we migrated the forums to discord

---

## 🚀 fold-3z8w Reorganize core/ into domain-driven subdirectories

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 03:45 |
| **Updated** | 2026-01-02 14:40 |
| **Closed** | 2026-01-02 14:40 |

### Description

Reorganize the core/ directory from 95+ flat files into organized subdirectories.

## New Structure
- base/ - Foundation (prelude.ss, sha256.ss, error.ss, span.ss)
- blocks/ - Block system & CAS (block.ss, cas.ss, normalize.ss)
- types/ - Type system (types.ss, kinds.ss, infer.ss, dep-*.ss)
- lang/ - Language core (eval.ss, compile.ss, module.ss, prim.ss)
- linalg/ - Linear algebra (vec.ss, matrix*.ss)
- numeric/ - Numerical computing (complex.ss, dft.ss)
- autodiff/ - Automatic differentiation (comp-graph.ss, reverse-diff.ss)
- data/ - Data structures (data-structures.ss, graph-algorithms.ss)
- query/ - Query system & DSL (query.ss, query-dsl.ss)
- util/ - General utilities (debug.ss, pretty.ss, help.ss)
- _compat/ - Backward compatibility shims (temporary)

## Migration Phases
1. Create directories and README.sexp files
2. Move files with updated load paths
3. Create compatibility shims & cutover
4. Update shell/agents/forum load paths
5. Documentation updates

## Benefits
- Discoverability: 95+ root files → ~15 organized subdirs
- Scalability: Clear boundaries for growth
- Navigation: Related files grouped by domain

Estimated effort: 13-21 hours

---

## 📋 fold-ccz Review and triage P1 epic backlog

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 22:38 |
| **Updated** | 2026-01-02 04:21 |
| **Closed** | 2026-01-02 04:21 |

### Description

30+ open P1 beads including LLM inference, transformers, training infra. Need to: (1) Verify scope is realistic, (2) Re-prioritize or defer ambitious epics, (3) Focus on consolidation before new features.

---

## 🧹 fold-clc TECH DEBT: bench-graph-algorithms.ss uses system() and mkdir in core

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:04 |
| **Labels** | tech-debt |

### Description

core/bench-graph-algorithms.ss calls system(), mkdir, file-exists? directly. Benchmarks should be in shell/ or tools/, not core/. Move to appropriate location.

---

## 🧹 fold-8n9 TECH DEBT: typed-eval.ss has IO in core

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:04 |
| **Labels** | tech-debt |

### Description

core/typed-eval.ss contains display statements for REPL output. The type-checking and evaluation logic is pure, but the display functions should be in shell. Consider returning result structures and having shell handle display.

---

## 🧹 fold-5vf TECH DEBT: debug.ss has IO in core

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:04 |
| **Labels** | tech-debt |

### Description

core/debug.ss contains display statements for interactive debugging. While the core logic is pure, the display functions violate the core purity principle. Consider moving display functions to shell and having debug.ss return data structures instead.

---

## 🧹 fold-opq TECH DEBT: numerical/integrators.ss is isolated

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:14 |
| **Labels** | tech-debt |

### Description

core/numerical/ contains only integrators.ss (numerical integration). Only loaded by physics-2d. Should be consolidated with physics-2d or moved to a math-extras directory.

---

## 🧹 fold-721 TECH DEBT: physics-2d only self-loads and one test file

| Property | Value |
|----------|-------|
| **Type** | 🧹 chore |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 21:33 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 22:14 |
| **Labels** | tech-debt |

### Description

The core/physics-2d/ directory (3,585 lines) is only loaded by its own modules and a single test file in user/physics/. If this is intended for user-space, it should move to user/. If core, it should be used by something.

---

## 📋 fold-93v Fix Rust formatting and clippy warnings in fold-rs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-31 19:42 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:15 |

### Description

The pre-commit and pre-push hooks now check Rust formatting and clippy. The fold-rs codebase has pre-existing issues:

1. Formatting: Several files need cargo fmt (fold-bench.rs, fold-eval-bench.rs, prim.rs, fold_lower.rs, fold_parse.rs, fold_run.rs, tests)

2. Clippy warnings:
   - clone_on_copy: Using .clone() on Copy types like Symbol
   - inherent_to_string_shadow_display: Symbol::to_string shadows Display
   - collapsible_if: Nested if statements that could be combined
   - redundant_closure: Closures that could be replaced with function references
   - to_digit_is_some: Using .to_digit().is_some() instead of .is_digit()

Run: cd fold-rs && cargo fmt && cargo clippy --fix --allow-dirty

---

## ✨ fold-bu4 fold-rs: Macro system (define-syntax or expand to core)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:15 |
| **Labels** | fold-rs, macros |

### Description

The Scheme code uses macros heavily (unless, when, cond, and, or, let*, etc.). Options:
1. Implement define-syntax / syntax-rules
2. Pre-expand macros to core forms before evaluation
3. Hard-code common macros in the lowerer

The prelude already has some of these as functions, but syntax forms are different.

---

## 🐛 fold-x2g fold-rs: Fix stack overflow in debug builds

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:16 |
| **Labels** | fold-rs |

### Description

The fold-repl CLI has a stack overflow in debug builds due to deep recursion without tail-call optimization. Options:
1. Increase default stack size for debug builds
2. Convert recursive eval to trampolined style
3. Use explicit continuation-passing

Release builds work fine due to compiler optimizations.

---

## ✨ fold-xef fold-rs: Port mathematical libraries (matrix, complex, dft)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:16 |
| **Labels** | fold-rs, math |

### Description

Port the mathematical computing libraries to Rust:
- complex.ss: Complex number support (56 tests in Scheme)
- matrix.ss, matrix-decomp.ss: Linear algebra (72+ tests)
- vec.ss: Vector operations (55 tests)
- dft.ss: Discrete Fourier Transform, FFT (46 tests)

These are pure functions that should work in the Rust evaluator.

### Dependencies

- ⛔ **blocks**: `fold-1h8`

---

## ✨ fold-aqr fold-rs: Parser - character literals and quasiquote

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:16 |
| **Labels** | fold-rs, parser |

### Description

Add missing parser features:
- Character literals: #\x, #\space, #\newline, #\tab
- Quasiquote: ` (backquote)
- Unquote: , (comma)
- Unquote-splicing: ,@ (comma-at)
- Dotted pair syntax: (a . b)

These are needed for full Scheme compatibility.

---

## 🐛 fold-40h Fix prelude lowering error in graph-find-path let structure

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 23:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:15 |

### Description

The prelude fails to lower due to 'let expects (let ((name expr) ...) body)' error at line 4005 (graph-find-path). This predates the modularization refactoring - fold_repl_cli test was already failing before any changes. The lowerer is rejecting a valid let expression in graph-traversal.ss. Tests affected: test_prelude_loads, fold_repl_runs_expression, and 500+ prelude integration tests.

---

## 🐛 fold-1va Fix remaining 95 prelude test failures in fold-rs

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 18:45 |
| **Updated** | 2026-01-02 04:40 |
| **Closed** | 2026-01-02 04:40 |

### Description

After prelude modularization (refactor(fold-rs): Modularize prelude into hierarchical file structure, commit 528778e), 319 tests are failing because functions were not fully extracted from the original 15k line prelude.

Current state: 193 passed, 319 failed

The 16 module files contain:
- core.ss: map, filter, foldl, foldr, any, all (~9 functions)
- function.ss: compose, pipe, curry, flip, juxt, Y-combinator (~30 functions)
- list.ss: comprehensive list operations (~70 functions)
- equality.ss: eqv?, equal?, comparison (~10 functions)
- maybe.ss: Maybe monad (~15 functions)
- either.ss: Either monad (~20 functions)
- numeric.ss: math, statistics, number theory (~80 functions)
- string.ss: string HOFs (~40 functions)
- comparison.ss: sorting, searching (~25 functions)
- collection.ss: dict, set, alist (~40 functions)
- control.ss: iteration, guards (~20 functions)
- validation.ss: assertions, testing (~30 functions)
- path.ss: filesystem paths (~25 functions)
- encoding.ss: base64, hex (~20 functions)
- compat.ss: Scheme aliases (~20 functions)
- exports.ss: export alist (~570 entries)

To fix: Extract remaining functions from the original prelude content (available in git history) into the appropriate module files.

---

## 📋 fold-5uy Implement remaining Rust daemon forum commands (browse, chat, hi/who/bye)

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 15:30 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 15:50 |

### Description

Follow-up from file I/O primitives work. The Rust daemon now has basic file I/O and digest/channels commands. Still needed:

1. Session commands (hi, who, bye)
2. Chat posting (chat "message")
3. Post browsing (browse 'channel)

These require reading/writing blocks from the content-addressed store and properly formatting the output.

---

## 📋 fold-4wq Revert to Rust daemon as default

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 14:47 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 16:32 |

### Description

Once the Rust daemon has feature parity with the Scheme daemon (beads-fpl6), revert the workaround from beads-3oy5 and make the Rust daemon the default again.

Tasks:
1. Verify Rust daemon has all forum commands (hi, digest, chat, msg, browse, channels, etc.)
2. Test that agents can successfully use Rust daemon
3. Update daemon.sh to prefer Rust daemon (remove/revert Scheme preference)
4. Update any systemd services if needed
5. Test full agent workflow (login, digest, post)
6. Monitor agent logs for any failures

This completes the temporary workaround and restores the intended default behavior.

Related files:
- daemon.sh
- fold-rs/src/bin/fold-daemon.rs
- ops/systemd/user/the-fold-daemon.service (if applicable)

### Dependencies

- ⛔ **blocks**: `fold-0by`

---

## 📋 fold-mnk Make Scheme daemon the default until Rust daemon is feature-complete

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 14:45 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 16:32 |

### Description

Currently, daemon.sh prefers the Rust daemon if available, but the Rust daemon is missing critical forum functionality. This causes agent failures.

Short-term fix applied: Manually restarted with FOLD_USE_SCHEME=1

Proposed solutions:
1. Update daemon.sh to default to Scheme daemon until Rust daemon has feature parity
2. Add a feature detection check in daemon.sh (test for 'hi' command availability)
3. Document the FOLD_USE_SCHEME env var more prominently

This is a stopgap until beads-fpl6 (Rust daemon forum commands) is complete.

Related files:
- daemon.sh (startup script)
- ops/systemd/user/ (if systemd service needs env var)

### Dependencies

- 🔗 **related**: `fold-0by`

---

## 📋 fold-79z fold-rs test coverage gaps for new primitives

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 06:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 16:35 |

### Description

Test coverage gaps for new primitives (PARTIALLY ADDRESSED)

Recent additions to fold-rs/src/thimble/prim.rs now have tests:
- interpolation_utils test: lerp, inverse-lerp, scale, smoothstep, denormalize, percent-of, percent-change, round-to, saturation-add
- math_utils test: exp
- assoc_ops test: assoc, assq

Still need tests for:
**Untested string utilities (added in recent commits):**
- string-words, string-lines, string-join
- string-replace-all
- string-trim-start, string-trim-end, string-trim-both
- string-ascii?, string-numeric?, string-alphabetic?, string-alphanumeric?
- string-whitespace?, string-empty?

**Untested vector operations:**
- vec-copy, vec-slice, vec-reverse, vec-sort
- vec-contains?, vec-index-of

**Untested type introspection:**
- type-of, is-number?, is-string?, is-vector?, is-list?

**Untested comparison utilities:**
- equals?, not-equals?, identical?, hash-value

Test file: fold-rs/tests/prim.rs (now 540 lines)
29 tests pass (was 26).

---

## 📋 fold-0it Missing primitives: exp, assoc, display, write, newline

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 06:50 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 07:01 |

### Description

The following primitives are declared in is_builtin_prim() in fold_lower.rs but have no implementation in prim.rs:

**Math function:**
- exp (e^x exponential) - Note: expt/pow ARE implemented

**Association list:**
- assoc (lookup by equal?, not eq?) - Note: assq IS implemented

**IO primitives:**
- display (print without quotes)
- write (print with quotes/readable)
- newline (print newline)

These are declared at fold-rs/src/tools/fold_lower.rs but missing from fold-rs/src/thimble/prim.rs.

For IO primitives, decision needed: should they be side-effecting prims or handled at REPL level?

---

## 🐛 fold-fsa Implement rational literal parser support

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 05:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 06:38 |

### Description

Add support for 1/2, 3/4 rational number literals to fold_parse.rs. Required for exact arithmetic. The Value enum already supports BigRational.

---

## ✨ fold-e7k fold-type should perform actual type inference

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 02:58 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 05:07 |

### Description

Currently (fold-type expr) just echoes the input instead of inferring and displaying the actual type signature.

---

## 🐛 fold-0dc Missing thimble/core-playground.ss file

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 02:58 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 05:07 |

### Description

(load-core) fails with 'file not found' error for thimble/core-playground.ss

---

## 📋 fold-wlc Add BigRational support to fold-rs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 22:57 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 23:11 |

### Description

Add num-rational crate and BigRational support to fold-rs.

Includes:
- BigRational variant in Value enum
- Primitives: bigrational-add, bigrational-sub, bigrational-mul, bigrational-div
- Conversion: bigint->bigrational, bigrational-numerator, bigrational-denominator
- Comparison: bigrational=?, bigrational<?, etc.

This enables exact rational arithmetic for transcendental function computation.

---

## 📋 fold-597 Integrate BigInt primitives with Rust evaluator

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 22:56 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 23:14 |

### Description

Wire BigInt operations from fabric/bigint.rs into eval.rs primitive dispatch. This enables Scheme code to use the fast Rust BigInt implementation.

Primitives to add:
- bigint-add, bigint-sub, bigint-mul, bigint-neg
- bigint-quotient, bigint-remainder, bigint-divmod
- bigint-gcd, bigint-lcm
- bigint-cmp, bigint=?, bigint<?, etc.
- bigint->string, string->bigint
- bigint?, bigint-zero?, bigint-positive?

Auto-promotion: When fixnum operations overflow, promote to BigInt.

---

## 📋 fold-68p Unify Foldable/Traversable with typeclass dictionaries

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:05 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:20 |

### Description

Currently each type has separate functions:
- list-foldr, maybe-foldr, either-foldr
- list-traverse, maybe-traverse, either-traverse

This duplicates code and makes generic programming harder.

Should create Foldable/Traversable dictionaries:
(make-foldable foldr foldl fold-map)
(make-traversable foldable traverse sequence)

Then generic functions like toList, length, elem can work with any Foldable by passing the dictionary.

---

## ✨ fold-7dd Upgrade optics to profunctor encoding for better composability

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 18:11 |

### Description

Current optics use simplified representations (getter/setter pairs) rather than full van Laarhoven or profunctor encoding.

With profunctor encoding:
- Optic composition becomes regular function composition
- Better type safety through profunctor constraints
- Enables more powerful optics like Iso, Prism, AffineTraversal seamlessly

Also add missing optic utilities:
- At/Ixed typeclasses for indexed access
- AsEmpty/Cons/Snoc prisms for sequences
- Integrate with Profunctor module properly

---

## 📋 fold-jsc Unify typeclass dictionary representation in FP toolkit

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:03 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:14 |

### Description

Different modules use incompatible representations:
- typeclasses.ss: (list 'functor fmap)
- algebraic.ss: (list 'monoid name mempty op)
- traversable.ss applicatives: (cons pure ap)

Should standardize on (list '<tag> <field1> <field2> ...) format across all modules for consistency and interoperability.

---

## ✨ fold-05p Test-driven development workflow

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:05 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:19 |

### Description

TDD built into the workflow:

Watch Mode:
(test:watch)  ; Run tests on every change
(test:focus 'module)  ; Only run module tests
(test:failing)  ; Only run failing tests

Quick Feedback:
- Sub-second test runs
- Inline test results in editor
- Jump to failing test
- Show diff for assertion failures

Coverage:
- Show uncovered lines
- Coverage trend over time
- Require coverage for merge

Fixtures:
- Generate test data
- Snapshot testing for blocks
- Golden master tests

Location: thimble/test-runner.ss, fold-rs/src/bin/fold-test.rs

### Dependencies

- ⛔ **blocks**: `fold-cz8`

---

## ✨ fold-wfz Refactoring toolkit

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:38 |

### Description

Safe automated refactorings:

Refactorings:
- Rename symbol (across all files)
- Extract function
- Inline function
- Extract let binding
- Introduce parameter
- Change signature
- Move to module

Safety:
- Preview changes before applying
- Type-check after refactor
- Run tests after refactor
- Undo stack

Batch Mode:
- Apply same refactor across codebase
- Search and replace with capture groups
- Semantic search (not just text)

Location: fold-rs/src/bin/fold-refactor.rs, thimble/refactor.ss

---

## ✨ fold-a0m Automatic API documentation generator

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:38 |

### Description

Generate docs from code:

From Code:
- Extract function signatures
- Extract docstrings
- Extract examples from tests
- Infer types where annotated

Output Formats:
- HTML with search
- Markdown for GitHub
- man pages for CLI
- In-REPL help text

Cross-References:
- Link to related functions
- Link to type definitions
- Link to examples
- Link to design decisions

Auto-Update:
- Rebuild on commit
- Publish to GitHub Pages
- Version selector

Location: fold-rs/src/bin/fold-doc.rs

---

## ✨ fold-7qr Inline performance profiler

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:04 |
| **Updated** | 2026-01-02 06:12 |
| **Closed** | 2026-01-02 06:12 |

### Description

Understand where time and fuel go:

Profiling:
(profile expr)  ; Run with profiling
(profile-report)  ; Show results

Output:
- Fuel consumed per function
- Time spent per function
- Call counts
- Hottest paths

Visualization:
- Flame graph
- Call tree with costs
- Fuel budget pie chart

Optimization Hints:
- 'This recursive call could be tail-call'
- 'This fold could fuse with map'
- 'This lookup is O(n), consider hash table'

Integration:
- Profile tests automatically
- Diff profiles between commits
- Alert on performance regression

Location: fold-rs/src/thimble/profile.rs

---

## ✨ fold-za0 Notebook interface (Jupyter-style)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:04 |
| **Updated** | 2026-01-02 02:47 |
| **Closed** | 2026-01-02 02:47 |

### Description

Interactive documents mixing code and prose:

Cells:
- Code cells: evaluate and show result
- Markdown cells: formatted text
- Visualization cells: render graphics
- Block cells: show block structure

Features:
- Re-run cells, update downstream
- Export to HTML/PDF
- Import/export as .fold-notebook
- Collaborative editing

Rich Output:
- Tables for lists of records
- Graphs for numeric data
- Trees for nested structures
- Block diagrams for CAS content

Kernel:
- Backed by fold daemon
- Session state persists
- Multiple notebooks share store

Location: fold-rs/src/bin/fold-notebook.rs, static/notebook/

---

## ✨ fold-k3g Property-based testing framework

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:53 |

### Description

QuickCheck-style testing for The Fold:

Core:
(check-property
  'reverse-reverse
  (forall (xs : (List Int))
    (equal? (reverse (reverse xs)) xs)))

Generators:
- (gen:int), (gen:bool), (gen:list gen)
- (gen:one-of a b c)
- (gen:such-that pred gen)
- (gen:sized (fn (n) ...))
- (gen:block tag payload-gen refs-gen)

Shrinking:
- Auto-shrink failing cases
- Find minimal counterexample
- Show shrink steps

Integration:
- Run in test suite
- Report statistics
- Collect coverage

Location: fabric/stitches/quickcheck.ss

---

## ✨ fold-tqx Live coding environment with hot reload

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 02:47 |
| **Closed** | 2026-01-02 02:47 |

### Description

Modify code while it runs:

Core Features:
- Edit function, see changes immediately
- Preserve state across reloads
- Rollback on error
- Time-travel debugging (step backward)

Workflow:
1. Start a long-running computation
2. Edit a function it uses
3. Computation picks up new definition
4. No restart needed

State Preservation:
- Serialize current env
- Reload changed definitions
- Restore compatible state
- Warn on incompatible changes

Visualization:
- Show which definitions changed
- Highlight affected call sites
- Preview effect of change

Inspiration: Smalltalk, Erlang hot code loading

Location: thimble/live.ss, fold-rs/src/thimble/hot-reload.rs

---

## ✨ fold-07g Step-through debugger with fuel visualization

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:03 |
| **Updated** | 2026-01-02 06:22 |
| **Closed** | 2026-01-02 06:22 |

### Description

Debug evaluation step by step:

Commands:
(debug expr)         ; Start debugging expr
(step)               ; Execute one step
(next)               ; Step over (don't descend into calls)
(continue)           ; Run to next breakpoint
(break 'fn-name)     ; Set breakpoint
(inspect)            ; Show current env bindings
(fuel)               ; Show remaining fuel
(trace)              ; Show call stack

Visualization:
- Show current expression highlighted
- Show environment bindings
- Show fuel consumption per step
- Show which primitive is being called

Location: fold-rs/src/thimble/debug.rs (new)

---

## ✨ fold-cyy Implement modular DSL composition

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:37 |
| **Updated** | 2026-01-02 23:30 |
| **Closed** | 2026-01-02 23:30 |

### Description

Combine DSLs without modification:

Problem:
Two DSLs (Arithmetic + State) need to compose:
  - Run arithmetic in stateful context
  - Neither DSL knows about the other

Solution 1 - Data Types à la Carte:
  (define-functor ArithF
    (LitF Int)
    (AddF r r))
  
  (define-functor StateF
    (GetF (-> s r))
    (PutF s r))
  
  ; Combine functors
  (define Expr (Fix (+ ArithF StateF)))

Solution 2 - Effect Composition:
  (define-effect Arith
    (lit : (-> Int (Arith Int)))
    (add : (-> Int Int (Arith Int))))
  
  (define-effect State
    (get : (State s))
    (put : (-> s (State Unit))))
  
  ; Handlers compose
  (with-handler arith-handler
    (with-handler state-handler
      program))

Solution 3 - Tagless Composition:
  (define-class ((ArithSym ∩ StateSym) repr)
    ...)  ; Has both sets of operations

Key Properties:
- Extensibility: add operations without changing existing code
- Modularity: interpreters defined separately
- Type safety: composition checked statically

Location: fabric/stitches/dsl/compose.ss

### Dependencies

- ⛔ **blocks**: `fold-rho`
- ⛔ **blocks**: `fold-cmm`

---

## ✨ fold-1g4 Implement recursion schemes

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:30 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 18:30 |

### Description

Implement structured recursion patterns:

Core Schemes:
1. Catamorphism (fold) - consume structure
2. Anamorphism (unfold) - produce structure  
3. Hylomorphism - unfold then fold
4. Paramorphism - fold with access to original
5. Apomorphism - unfold with early termination
6. Histomorphism - fold with history
7. Futumorphism - unfold with lookahead

Fixpoint Types:
- Fix f = f (Fix f)
- Mu (least fixpoint)
- Nu (greatest fixpoint)

Base Functors:
- ListF a r = Nil | Cons a r
- TreeF a r = Leaf a | Node r r
- ExprF r = Lit Int | Add r r | ...

Applications:
- AST transformations
- Optimization passes
- Pretty printing
- Serialization

Critical for: Symbolic computation, parsers, compilers

Location: fabric/stitches/fp/recursion-schemes.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`
- ⛔ **blocks**: `fold-5sa`

---

## ✨ fold-ift Implement Bifunctor and Profunctor

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:28 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:37 |

### Description

Implement two-argument functor variants:

Bifunctor (covariant in both arguments):
- bimap : (a -> b) -> (c -> d) -> f a c -> f b d
- first : (a -> b) -> f a c -> f b c
- second : (c -> d) -> f a c -> f a d

Key Bifunctor instances:
- Pair/Tuple
- Either
- Validation
- These (inclusive or)

Profunctor (contravariant in first, covariant in second):
- dimap : (a -> b) -> (c -> d) -> p b c -> p a d
- lmap : (a -> b) -> p b c -> p a c
- rmap : (c -> d) -> p b c -> p b d

Key Profunctor instances:
- Function (->)
- Kleisli arrows
- Optics (prisms, lenses as profunctors)

Applications:
- Error handling with context
- Bidirectional transformations
- Optics library foundation

Location: fabric/stitches/fp/bifunctor.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`
- ⛔ **blocks**: `fold-z5d`

---

## ✨ fold-dum Implement Contravariant and Divisible functors

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:28 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:37 |

### Description

Implement contravariant functors for consumer-like types:

Contravariant:
- contramap : (b -> a) -> f a -> f b

Divisible (contravariant analogue of Applicative):
- divide : (a -> (b, c)) -> f b -> f c -> f a
- conquer : f a

Decidable (contravariant analogue of Alternative):
- choose : (a -> Either b c) -> f b -> f c -> f a
- lose : (a -> Void) -> f a

Key instances:
- Predicate (a -> Bool)
- Comparison (a -> a -> Ordering)
- Serializer/Encoder
- Op (wrapped function)
- Equivalence

Applications:
- Building predicates compositionally
- Sorting with custom comparators
- Encoding/serialization pipelines

Location: fabric/stitches/fp/contravariant.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`

---

## ✨ fold-6tr Implement Comonad abstraction

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:28 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:37 |

### Description

Implement Comonad for context-dependent computation:

Core operations:
- extract : W a -> a
- extend : (W a -> b) -> W a -> W b
- duplicate : W a -> W (W a)

Key instances:
1. Stream Comonad - for signal processing
2. Zipper Comonad - for local context
3. Store Comonad - for memoization
4. Traced Comonad - for monoid-indexed computation
5. Env Comonad - for environment access

Applications:
- Cellular automata (Game of Life)
- Image processing (convolutions)
- Stream differentiation
- Dataflow programming

Location: fabric/stitches/fp/comonad.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`

---

## 📋 fold-jxl Implement GSM8K math benchmark

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement GSM8K grade-school math benchmark with chain-of-thought evaluation.

---

## 📋 fold-jtn Implement MMLU benchmark

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement MMLU (Massive Multitask Language Understanding) evaluation with subject-wise scoring.

---

## 📋 fold-b3y Implement benchmark task framework

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement task definition format with few-shot prompting, answer extraction, and scoring.

---

## 📋 fold-3yk Implement beam search

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement beam search decoding with configurable beam width. Support length normalization.

---

## 📋 fold-asv Implement DPO algorithm

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Implement Direct Preference Optimization - train on preferences without explicit reward model.

---

## 📋 fold-bz3 Implement PPO algorithm

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Implement Proximal Policy Optimization for LLM fine-tuning. Support clipping, KL penalty, value baseline.

---

## 📋 fold-1az Implement reward model

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Implement reward model for RLHF: takes (prompt, response) pairs, outputs scalar reward. Based on transformer.

---

## 📋 fold-ehu Implement model checkpointing

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement model state serialization and loading. Support partial loading, strict mode, optimizer state.

---

## 📋 fold-5c4 Implement gradient clipping

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:12 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement gradient clipping by norm and by value. Prevent gradient explosion during training.

---

## 📋 fold-c5r Implement Dropout

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement dropout regularization. Support training/eval mode switching, configurable drop probability.

---

## 📋 fold-dgs Implement regex pre-tokenization

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Implement GPT-style regex pre-tokenization to split on whitespace, punctuation patterns before BPE.

---

## 📋 fold-9s3 Implement vocabulary and merges file I/O

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:11 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Read/write vocab.json and merges.txt files. Support loading pretrained tokenizers (GPT-2, LLaMA formats).

---

## 🚀 fold-xel Distributed Training Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Distributed training for large models.

Data Parallelism:
- Distributed data loading
- Gradient synchronization (all-reduce)
- Gradient compression

Model Parallelism:
- Tensor parallelism (column/row)
- Pipeline parallelism
- Sequence parallelism

Communication:
- Ring all-reduce
- Hierarchical communication
- Async gradient updates

Infrastructure:
- Multi-device abstraction
- Checkpoint sharding
- Fault tolerance

Note: May require Rust runtime extensions for actual parallelism.

Location: fabric/stitches/distributed/

---

## 🚀 fold-mx2 LLM Evaluation and Benchmarks Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:53 |
| **Closed** | 2026-01-02 02:53 |

### Description

Evaluation framework for language models.

Core Metrics:
1. Perplexity / bits-per-byte
2. Accuracy (for classification tasks)
3. BLEU, ROUGE (for generation)
4. Pass@k (for code generation)

Benchmark Tasks:
1. ARC (reasoning)
2. HellaSwag (commonsense)
3. MMLU (knowledge)
4. GSM8K (math)
5. HumanEval (code)
6. TruthfulQA (factuality)

Evaluation Framework:
- Task definition format
- Few-shot prompting
- Scoring and aggregation
- Leaderboard tracking

Location: fabric/stitches/eval/

### Dependencies

- ⛔ **blocks**: `fold-4r9`
- ⛔ **blocks**: `fold-e0b`
- ⛔ **blocks**: `fold-gj0`
- ⛔ **blocks**: `fold-0ln`
- ⛔ **blocks**: `fold-jxl`
- ⛔ **blocks**: `fold-b3y`
- ⛔ **blocks**: `fold-jtn`

---

## 🚀 fold-39j Reinforcement Learning for LLMs Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:10 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

RLHF and preference learning for chat models.

Core RL Algorithms:
1. Policy Gradient (REINFORCE)
2. PPO (Proximal Policy Optimization)
3. DPO (Direct Preference Optimization)
4. GRPO (Group Relative Policy Optimization)

RLHF Pipeline:
- Reward modeling
- Preference data format
- KL divergence regularization
- Reference model management

Training Stages:
1. SFT (Supervised Fine-Tuning)
2. Reward model training
3. RL policy optimization

Evaluation:
- Win rate metrics
- Human preference simulation

Location: fabric/stitches/rl/

### Dependencies

- ⛔ **blocks**: `fold-2d4`
- ⛔ **blocks**: `fold-93a`
- ⛔ **blocks**: `fold-cz4`
- ⛔ **blocks**: `fold-bz3`
- ⛔ **blocks**: `fold-asv`
- ⛔ **blocks**: `fold-1az`

---

## 📋 fold-01u Implement statechart DSL

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 23:10 |
| **Closed** | 2026-01-02 23:10 |

### Description

Define DSL for hierarchical state machines. Support nested states, guards, actions, and history states.

### Dependencies

- ⛔ **blocks**: `fold-b0x`

---

## 📋 fold-9ne Implement regex to automata

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 05:33 |

### Description

Parse regular expressions and compile to NFA via Thompson construction. Support common regex syntax.

### Dependencies

- ⛔ **blocks**: `fold-zck`

---

## 📋 fold-zck Implement automata operations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 05:30 |

### Description

Implement product, union, concatenation, Kleene star. Support NFA to DFA conversion and minimization.

### Dependencies

- ⛔ **blocks**: `fold-b0x`

---

## 📋 fold-b0x Implement finite automata

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:50 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 05:29 |

### Description

Implement DFA and NFA types with transition functions. Support simulation, acceptance testing, and trace generation.

---

## 📋 fold-dgu Implement expression parser with precedence

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 13:59 |

### Description

Implement buildExpressionParser for operator precedence parsing. Support prefix, postfix, infix (left/right/none) operators.

### Dependencies

- ⛔ **blocks**: `fold-b7i`

---

## 📋 fold-pmg Implement error handling and position tracking

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 13:59 |

### Description

Implement labeled errors, source position tracking (line, column), error recovery, and informative error messages.

### Dependencies

- ⛔ **blocks**: `fold-b7i`

---

## 📋 fold-a4i Implement quantity operations and conversions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:48 |
| **Updated** | 2026-01-02 03:02 |
| **Closed** | 2026-01-02 03:02 |

### Description

Implement arithmetic on quantities, unit conversion functions, and dimensionless extraction. Support prefixes (kilo, milli, micro).

### Dependencies

- ⛔ **blocks**: `fold-xuy`

---

## 📋 fold-7s7 Law-checker for FP abstractions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:42 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:54 |

### Description

Validate functor/applicative/monad/monoid laws for candidate instances using generated tests and symbolic checks where possible.

### Dependencies

- ⛔ **blocks**: `fold-07e`

---

## ✨ fold-yak Implement tensor Functor for neural network layers

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:36 |
| **Updated** | 2026-01-02 05:01 |
| **Closed** | 2026-01-02 05:01 |

### Description

Extend Functor/Applicative to N-dimensional tensors. Enable broadcasting semantics, axis-preserving maps, batched operations. Foundation for differentiable neural network layers.

### Dependencies

- ⛔ **blocks**: `fold-bcs`
- ⛔ **blocks**: `fold-637`

---

## ✨ fold-41r Implement Random effect for simulations

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:36 |
| **Updated** | 2026-01-03 00:21 |
| **Closed** | 2026-01-03 00:21 |

### Description

Create Random effect in algebraic effect system. Support: (handle-random seed computation), pure random sampling with explicit state threading. Essential for reproducible stochastic simulations.

### Dependencies

- ⛔ **blocks**: `fold-0ak`
- ⛔ **blocks**: `fold-9c6`

---

## ✨ fold-hwu Implement Graph Traversable instance

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 00:22 |

### Description

Derive Foldable and Traversable for Graph types. Enable generic graph algorithms: (foldMap f graph), (traverse validate-node graph). Support BFS/DFS as Traversable variations.

### Dependencies

- ⛔ **blocks**: `fold-2we`
- ⛔ **blocks**: `fold-i5d`

---

## ✨ fold-cgu Implement simulation stream abstraction

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:36 |
| **Updated** | 2026-01-03 00:54 |
| **Closed** | 2026-01-03 00:54 |

### Description

Create lazy stream abstraction for physics simulations. Enable: (take 1000 (simulate initial-state)), (unfold step-fn state), (scan accumulate stream). Infinite sequence of timesteps.

### Dependencies

- ⛔ **blocks**: `fold-6rr`
- ⛔ **blocks**: `fold-43e`

---

## ✨ fold-a40 Implement Probability monad

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 18:54 |

### Description

Implement probability monad for stochastic computation. Support: (sample distribution), (do-notation for probability), independence via Applicative, conditioning. Integration with statistics library.

### Dependencies

- ⛔ **blocks**: `fold-bcs`
- ⛔ **blocks**: `fold-9c6`

---

## ✨ fold-eoo Implement Num type class instances for Vec and Matrix

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:35 |
| **Updated** | 2026-01-03 00:52 |
| **Closed** | 2026-01-03 00:52 |

### Description

Derive Num, Fractional, Floating instances for Vec and Matrix. Enable: (+ v1 v2), (* scalar vec), (sin matrix). Component-wise operations via Applicative.

### Dependencies

- ⛔ **blocks**: `fold-7s8`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`

---

## ✨ fold-lwl Implement Functor/Foldable instances for Vec and Matrix

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 16:48 |

### Description

Derive Functor, Applicative, Foldable, Traversable instances for Vec and Matrix types. Enable: (fmap sqrt vec), (traverse validate matrix), (fold + 0 matrix). Essential for generic numeric algorithms.

### Dependencies

- ⛔ **blocks**: `fold-bcs`
- ⛔ **blocks**: `fold-2we`
- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`

---

## ✨ fold-sim Implement arrows and profunctors

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:37 |

### Description

Implement Arrow abstraction for computation with multiple inputs. Support arr, (>>>), first, second, (&&&), (***). Useful for signal processing, dataflow, and FRP. Location: fabric/stitches/fp/arrow.ss

### Dependencies

- ⛔ **blocks**: `fold-33n`
- ⛔ **blocks**: `fold-z5d`

---

## ✨ fold-np5 Implement monad transformers

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:37 |

### Description

Implement monad transformer stack: StateT, ReaderT, WriterT, ExceptT, MaybeT. Support lift operation and transformer composition. Enable complex effect stacking for simulations. Location: fabric/stitches/fp/transformers.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`

---

## ✨ fold-rho Implement free monad and interpreters

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:37 |

### Description

Implement Free monad for building interpreters and DSLs. Support liftF, foldFree, interpret patterns. Enable physics simulation DSLs, graphics DSLs, and game logic DSLs. Location: fabric/stitches/fp/free.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`

---

## ✨ fold-6rr Implement lazy streams and codata

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 22:57 |
| **Closed** | 2026-01-02 22:57 |

### Description

Implement lazy evaluation primitives and infinite streams. Support delay/force, lazy cons, stream operations (take, drop, map, filter, iterate, unfold). Enable infinite simulation sequences. Location: fabric/stitches/fp/stream.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`

---

## ✨ fold-0ak Implement algebraic effect system

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-03 00:01 |
| **Closed** | 2026-01-03 00:01 |

### Description

Extend beyond current Cap types to structured algebraic effects. Support effect handlers, effect polymorphism, and effect inference. Key effects: State, Reader, Writer, Error, IO, Random. Location: fabric/stitches/fp/effects.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`

---

## ✨ fold-1yg Implement lens and optics library

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:41 |

### Description

Implement van Laarhoven lenses, prisms, traversals, and isos. Support composition (.) for nested access, over/set/view operations. Common lenses for pairs, lists, records. Location: fabric/stitches/fp/lens.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`
- ⛔ **blocks**: `fold-ift`
- ⛔ **blocks**: `fold-n7e`

---

## 📋 fold-ge6 Lens navigation + codebase explorer

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:33 |
| **Updated** | 2026-01-02 05:30 |
| **Closed** | 2026-01-02 05:30 |

### Description

Navigation tooling for large codebases: jump to defs, call sites, related tests, and dependency slices. Provide REPL commands and structured navigation data.

### Dependencies

- ⛔ **blocks**: `fold-z7y`
- ⛔ **blocks**: `fold-0wd`

---

## 📋 fold-7dx Benchmark harness + regression tracking

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:32 |
| **Updated** | 2026-01-02 23:00 |
| **Closed** | 2026-01-02 23:00 |

### Description

Benchmark runner with baselines, perf regression tracking, and reproducible runs. Integrate with REPL profiling outputs.

### Dependencies

- ⛔ **blocks**: `fold-q7w`

---

## 📋 fold-8ap Implement normal form games

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 03:11 |
| **Closed** | 2026-01-02 03:11 |

### Description

Implement strategic form games with payoff matrices. Support Nash equilibrium computation, dominated strategy elimination, mixed strategies, and best response dynamics.

---

## 📋 fold-3lh Implement discrete dynamical systems

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:26 |
| **Updated** | 2026-01-02 23:02 |
| **Closed** | 2026-01-02 23:02 |

### Description

Implement iterated maps, cobweb diagrams, periodic orbit detection. Support Hénon map, logistic map, and symbolic dynamics for discrete systems.

---

## ✨ fold-07e Property-based testing + shrinking

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:38 |

### Description

Add generators, shrinkers, and property runners integrated with REPL + test harness. Emit minimal counterexamples and reproducible seeds.

---

## 📋 fold-q7w Cost + performance profiler

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 17:16 |
| **Closed** | 2026-01-02 17:16 |

### Description

Expose cost model and runtime profiling (time, allocations, fuel). Provide REPL report commands and hotspot summaries.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

### Comments

> **oso** (2026-01-02)
>
> Task 2 complete: Allocation tracking shim created at shell/alloc-tracker.ss with 47 passing tests in shell/tests/test-alloc-tracker.ss. Features: get-allocation-stats, track-allocations, format-bytes, alloc-tracker record type with summary and top-n.

> **oso** (2026-01-02)
>
> Tasks 1,2,4 complete: (1) Cost model abstraction in core/util/cost-model.ss (87 tests), (2) Allocation tracker in shell/alloc-tracker.ss (47 tests), (4) Call graph builder in shell/profile-call-graph.ss (59 tests). Remaining: Tasks 3,5,6,7.

---

## 📋 fold-7lw Implement state space models

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:21 |
| **Updated** | 2026-01-02 03:35 |
| **Closed** | 2026-01-02 03:35 |

### Description

State space representation of dynamic systems.

Features:
- State equation representation
- Output equation
- State transition matrix
- Controllability matrix
- Observability matrix
- Gramians
- Modal decomposition

---

## 📋 fold-e78 Implement symbolic expression representation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 03:15 |
| **Closed** | 2026-01-02 03:15 |

### Description

Core symbolic expression data structures.

Features:
- S-expression based representation (natural!)
- Symbolic variables
- Numeric constants
- Arithmetic operators
- Function application
- Pattern matching on expressions
- Expression equality

---

## 📋 fold-5dg Implement category and functor foundations

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:20 |
| **Updated** | 2026-01-02 14:21 |
| **Closed** | 2026-01-02 14:21 |

### Description

Core categorical structures.

Features:
- Category typeclass/interface
- Objects and morphisms
- Identity and composition laws
- Functor definition
- Functor composition
- Contravariant functors
- Bifunctors

---

## ✨ fold-637 Implement N-dimensional tensor operations

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:05 |
| **Updated** | 2026-01-02 04:57 |
| **Closed** | 2026-01-02 04:57 |

### Description

Generalize vectors and matrices to N-dimensional tensors.

Features:
1. Tensor data structure
   - Arbitrary dimensions (shape tuple)
   - Contiguous memory layout
   - Strided views

2. Basic operations
   - Element-wise operations (add, multiply, etc.)
   - Broadcasting semantics
   - Reshape, transpose, permute
   - Slicing and indexing

3. Advanced operations
   - Tensor contraction (generalized matrix multiply)
   - Einsum notation
   - Outer product
   - Kronecker product

4. Utilities
   - Shape manipulation
   - Dimension reduction (sum, mean along axes)
   - Concatenation, stacking

Required by:
- Deep learning (multi-dimensional data)
- Physics simulations (tensors in relativity)
- Computer graphics (transformation tensors)

Builds on: Vectors and matrices

Location: fabric/stitches/tensor.ss

### Dependencies

- ⛔ **blocks**: `fold-o7b`
- ⛔ **blocks**: `fold-81x`

---

## ✨ fold-zb9 Implement graph neural network primitives

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:05 |
| **Updated** | 2026-01-02 04:57 |
| **Closed** | 2026-01-02 04:57 |

### Description

Connect graph algorithms with autodiff for graph neural networks.

Features:
1. Message Passing
   - Neighbor aggregation (sum, mean, max)
   - Message functions
   - Update functions

2. Graph Convolutions
   - GCN (Graph Convolutional Network)
   - GAT (Graph Attention)
   - GraphSAGE

3. Pooling Operations
   - Global pooling (sum, mean, max)
   - Hierarchical pooling
   - Top-k pooling

4. Utilities
   - Graph batching
   - Subgraph sampling
   - Feature propagation

Integration:
- Uses graph algorithms library for structure
- Uses autodiff for gradient computation
- Uses linalg for embeddings

Applications:
- Node classification
- Graph classification
- Link prediction
- Molecular property prediction

Location: ml/graph-nn/

### Dependencies

- ⛔ **blocks**: `fold-i5d`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-10x`
- ⛔ **blocks**: `fold-637`

---

## ✨ fold-5ny Implement probabilistic machine learning utilities

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 04:57 |
| **Closed** | 2026-01-02 04:57 |

### Description

Connect probability/statistics with autodiff for machine learning.

Features:
1. Probabilistic Gradients
   - Reparameterization trick
   - Score function estimator
   - Gumbel-softmax

2. Variational Inference
   - ELBO optimization
   - KL divergence computation
   - Amortized inference

3. Bayesian Deep Learning
   - Bayesian linear layers
   - Dropout as approximate inference
   - Uncertainty quantification

Integration:
- Uses probability distributions
- Uses autodiff for gradient computation
- Enables probabilistic programming

Requires: Probability library, autodiff engine

Location: ml/probabilistic/

### Dependencies

- ⛔ **blocks**: `fold-9c6`
- ⛔ **blocks**: `fold-5vq`

---

## 📋 fold-j8q Add transcendental functions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:34 |

### Description

Implement high precision versions of sin, cos, tan, log, exp, and other transcendental functions with appropriate convergence algorithms

### Dependencies

- ⛔ **blocks**: `fold-tsz`

---

## ✨ fold-ek7 Implement random number generation and distributions

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:59 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-31 18:42 |

### Description

Pseudorandom number generation for simulations and algorithms.

Features:
1. PRNG algorithms
   - Xorshift variants
   - PCG family
   - Deterministic seeding

2. Distributions
   - Uniform (integers, floats, ranges)
   - Normal/Gaussian
   - Exponential
   - Poisson

3. Sampling utilities
   - Shuffle
   - Sample without replacement
   - Weighted sampling

Use Cases:
- Physics simulations (Monte Carlo)
- Graph algorithms (random graphs)
- Machine learning (initialization, dropout)
- Game development

Location: fabric/stitches/random/

### Dependencies

- ⛔ **blocks**: `fold-o7b`

---

## 📋 fold-0qs Create dependent types documentation and examples

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:17 |

### Description

Comprehensive documentation for dependent type system.

Documentation:
1. User guide
   - Tutorial introduction
   - Pi types and dependent functions
   - Sigma types and dependent pairs
   - Refinement types
   - Proof techniques
   - Best practices

2. Reference manual
   - Syntax reference
   - Type rules (formal)
   - Built-in types and functions
   - Error messages explained

3. Examples library
   - Length-indexed vectors
   - Balanced trees
   - Typed printf
   - Safe array access
   - State machine encoding
   - Simple theorem proving

4. Migration guide
   - Upgrading existing code
   - Common patterns
   - Performance considerations

Location: scripture/ for policy, docs/ for technical

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-8wy`

---

## 📋 fold-14l Create comprehensive test suite for dependent types

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:47 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:17 |

### Description

Thorough testing of dependent type system.

Test Categories:
1. Unit tests for each feature
2. Integration tests
3. Error message tests
4. Performance tests
5. Regression tests

Feature Tests:
- Pi types: formation, intro, elim, computation
- Sigma types: formation, intro, elim, projections
- Universe hierarchy: levels, polymorphism
- Type-level computation: normalization, equality
- Refinement types: subtyping, proof obligations
- Equality types: refl, J, transport
- Pattern matching: coverage, index refinement
- Termination: acceptance, rejection
- Inductive types: definition, eliminators

Integration Tests:
- Combined features
- Module system interaction
- Interaction with existing types
- Real-world examples

Error Tests:
- Type mismatches
- Universe inconsistencies
- Non-termination detection
- Coverage failures

Location: fabric/stitches/test-dependent.ss

### Dependencies

- ⛔ **blocks**: `fold-rus`
- ⛔ **blocks**: `fold-dz1`
- ⛔ **blocks**: `fold-w5k`
- ⛔ **blocks**: `fold-7yh`

---

## ✨ fold-f6d Update kind system for dependent kinds

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:46 |
| **Updated** | 2026-01-02 17:16 |
| **Closed** | 2026-01-02 17:16 |

### Description

Extend kind system to handle type-level computation and dependent kinds.

Current System (kinds.ss):
- * : kind of types
- (⇒ K1 K2) : kind of type constructors
- Constraint, Row : special kinds

Extensions:
1. Kind-level computation
2. Dependent kinds: (Π (x : A) K)
3. Kind polymorphism with constraints
4. Universe levels in kinds

Implementation:
- Kinds become first-class (types and kinds merge)
- Type : Kind : Sort (or similar hierarchy)
- Unify type and kind checking
- Handle kind inference

Changes to kinds.ss:
- K* becomes (Type 0)
- Kind arrows become Π at kind level
- Add kind normalization

Challenges:
- Avoid over-complication
- Maintain backward compatibility
- Clear error messages

Location: fabric/stitches/kinds.ss, types.ss

### Dependencies

- ⛔ **blocks**: `fold-w5k`
- ⛔ **blocks**: `fold-7yh`

### Comments

> **oso** (2026-01-02)
>
> Phase 1 complete: Dependent kinds foundation implemented in core/types/kinds.ss. Added K-pi, K-sort constructors, sort?/dep-kind? predicates, updated kind? and kind->string. 51 tests in test-dep-kinds.ss all passing.

---

## 📋 fold-6d6 Implement higher-order automatic differentiation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:42 |
| **Updated** | 2026-01-03 00:54 |
| **Closed** | 2026-01-03 00:54 |

### Description

Extend automatic differentiation to support arbitrary-order derivatives. This includes:

- Arbitrary-order forward and reverse mode
- Tensor derivatives and multi-index notation
- Efficient higher-order gradient computation
- Automatic differentiation of gradient computations
- Support for Taylor series expansion
- Computational graphs for higher-order derivatives
- Memory-efficient higher-order computation patterns

Location: fabric/stitches/ (extend higher-order-diff module)
Important for scientific computing and advanced optimization.

### Dependencies

- ⛔ **blocks**: `fold-c3i`

---

## 📋 fold-sxc Implement sparse matrix support for large-scale autodiff

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:42 |
| **Updated** | 2026-01-03 00:54 |
| **Closed** | 2026-01-03 00:54 |

### Description

Add sparse matrix capabilities for efficient automatic differentiation on large, sparse systems. This includes:

- Sparse matrix data structures (CSR, CSC, COO formats)
- Sparse gradient computation algorithms
- Efficient sparse Jacobian computation
- Sparse pattern detection and optimization
- Memory-efficient sparse Hessian-vector products
- Sparse linear algebra operations for gradients
- Integration with computational graph for sparse operations

Location: fabric/stitches/ (new sparse-matrix module)
Critical for scientific computing and large-scale optimization.

### Dependencies

- ⛔ **blocks**: `fold-c3i`
- ⛔ **blocks**: `fold-1ws`

---

## ✨ fold-1ws Implement sparse matrix support

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:30 |
| **Updated** | 2026-01-02 16:09 |
| **Closed** | 2026-01-02 16:09 |

### Description

Efficient storage and operations for sparse matrices.

Sparse formats:
- COO (Coordinate) format
- CSR (Compressed Sparse Row)
- CSC (Compressed Sparse Column)
- Conversion between formats

Operations:
- Sparse matrix-vector multiplication
- Sparse matrix-matrix multiplication
- Sparse matrix addition
- Transpose for sparse matrices

Use cases:
- Graph algorithms
- Large-scale scientific computing
- Network analysis

Include memory efficiency tests.

---

## ✨ fold-l0p Implement special matrices and utilities

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:29 |
| **Updated** | 2026-01-02 15:51 |
| **Closed** | 2026-01-02 15:51 |

### Description

Constructors and utilities for special matrix types.

Special matrices:
- Identity matrix
- Zero matrix
- Diagonal matrix
- Symmetric/antisymmetric matrices
- Orthogonal matrices
- Triangular matrices

Utilities:
- Matrix properties (is-symmetric?, is-orthogonal?, is-diagonal?)
- Matrix rank computation
- Trace
- Condition number
- Matrix norms (Frobenius, spectral, etc.)

Include property verification tests.

### Dependencies

- ⛔ **blocks**: `fold-81x`

---

## ✨ fold-5k3 Implement eigenvalue/eigenvector computation

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:29 |
| **Updated** | 2026-01-02 15:45 |
| **Closed** | 2026-01-02 15:45 |

### Description

Compute eigenvalues and eigenvectors for matrices.

Algorithms:
- Power iteration (for dominant eigenvalue)
- QR algorithm for all eigenvalues
- Inverse iteration for specific eigenvalues

Features:
- Handle symmetric matrices efficiently
- Complex eigenvalues (if type system supports)
- Eigenvalue decomposition

Applications:
- Spectral analysis
- Principal component analysis
- Stability analysis

Include convergence tests.

---

## ✨ fold-6nq Implement matrix decompositions (LU, QR)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:29 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:21 |

### Description

Fundamental matrix decomposition algorithms.

Decompositions:
- LU decomposition (with partial pivoting)
- QR decomposition (Gram-Schmidt or Householder)
- Cholesky decomposition (for symmetric positive-definite)

Use cases:
- Solving linear systems
- Matrix inversion
- Least squares problems

Include numerical stability tests.

### Dependencies

- ⛔ **blocks**: `fold-81x`

---

## ✨ fold-vy8 Implement linear equation solvers

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:29 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:27 |

### Description

Solve linear systems Ax = b using various methods.

Solvers:
- Direct solver using LU decomposition
- Gaussian elimination with partial pivoting
- Back/forward substitution utilities
- Matrix inversion (via solving AI = I)
- Determinant computation

Handle edge cases:
- Singular matrices
- Ill-conditioned systems
- Over/under-determined systems

Include numerical accuracy tests.

### Dependencies

- ⛔ **blocks**: `fold-6nq`

---

## 🚀 fold-i5d Graph Algorithms Library Epic

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:21 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:41 |

### Description

Create a comprehensive graph algorithms library for The Fold. This epic encompasses the implementation of fundamental graph data structures, algorithms, and utilities to support graph-based computations and applications. The library should provide both core graph representations and efficient algorithms for common graph problems.

---

## 📋 fold-e5a Update tests for migrated core functions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:16 |
| **Updated** | 2026-01-03 00:15 |
| **Closed** | 2026-01-03 00:15 |

### Description

Create comprehensive test suite for all functions migrated from patterns to core. Ensure proper test coverage, update existing tests that reference patterns modules, and verify functionality works in core context. Tests should be in fabric/stitches/ following test-<module>.ss naming convention.

### Dependencies

- ⛔ **blocks**: `fold-zpn`

---

## 📋 fold-zpn Standardize module loading for core utilities

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:16 |
| **Updated** | 2026-01-02 20:05 |
| **Closed** | 2026-01-02 20:05 |

### Description

Establish consistent pattern for loading core utilities in REPL sessions. Define what should be auto-loaded vs require explicit loading. Create loading mechanism that makes migrated functions from patterns available without manual (load ...) calls.

### Dependencies

- ⛔ **blocks**: `fold-2rj`

---

## ✨ fold-dl6 Implement Aho-Corasick multi-pattern string matching

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:13 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 00:22 |

### Description

Build an Aho-Corasick automaton for efficient multi-pattern string matching.

Uses new data structures:
- Queue for BFS during failure link construction
- Dictionary for state transitions
- Set for pattern storage

Includes:
- Trie construction from patterns
- Failure link computation
- Output function for matches
- Search function returning all matches with positions
- Comprehensive tests

---

## 📋 fold-xha Tests for SDK patch registration

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 15:16 |
| **Closed** | 2026-01-02 15:16 |

### Description

Add tests that (1) manifests parse, (2) apply-patch-recursive loads required files in order, and (3) expected entrypoints are present/whitelisted after applying each SDK patch.

### Dependencies

- ⛔ **blocks**: `fold-6o3`
- ⛔ **blocks**: `fold-2yb`
- ⛔ **blocks**: `fold-820`

---

## ✨ fold-zz4 Patch UX for SDKs: discoverability + quickload

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 15:16 |
| **Closed** | 2026-01-02 15:16 |

### Description

Improve discoverability for SDK patches: optionally add helper commands (e.g., list only SDK patches, quick apply) and ensure patch-info surfaces SDK metadata cleanly.

### Dependencies

- ⛔ **blocks**: `fold-6o3`
- ⛔ **blocks**: `fold-2yb`
- ⛔ **blocks**: `fold-820`

---

## 📋 fold-820 Register Satin DSL patch manifest

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:04 |

### Description

Add a patches manifest for Satin. Satin patch should require Quill patch; decide provides (compile/load helpers) consistent with SDK patch policy.

### Dependencies

- ⛔ **blocks**: `fold-e43`
- ⛔ **blocks**: `fold-2yb`
- ⛔ **blocks**: `fold-0mc`

---

## 📋 fold-2yb Register Quill SDK patch manifest

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:04 |

### Description

Add a patches manifest for Quill. Requires Quill module layout/entrypoint to exist; decide provides (quill-run/quill-load/etc) consistent with SDK patch policy.

### Dependencies

- ⛔ **blocks**: `fold-e43`
- ⛔ **blocks**: `fold-l97`

---

## 📋 fold-6o3 Register Loom SDK patch manifest

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:04 |

### Description

Add a patches manifest for Loom (playpen/loom). Decide files list (loom.ss vs module list) and provides (key entrypoints) consistent with SDK patch policy.

### Dependencies

- ⛔ **blocks**: `fold-e43`

---

## 🚀 fold-n8t Patch Registry: SDK Manifests

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:05 |
| **Updated** | 2026-01-02 02:55 |
| **Closed** | 2026-01-02 02:55 |

### Description

Register major SDKs as patches (manifest + dependencies + provides policy) so they can be discovered and loaded via (patches)/(apply-patch). Target SDKs: Loom, Quill, Satin; ensure patch dependency graph matches SDK layering and keeps global symbol registration sane.

### Dependencies

- ⛔ **blocks**: `fold-buk`
- ⛔ **blocks**: `fold-e43`
- ⛔ **blocks**: `fold-6o3`
- ⛔ **blocks**: `fold-2yb`
- ⛔ **blocks**: `fold-820`
- ⛔ **blocks**: `fold-zz4`
- ⛔ **blocks**: `fold-xha`

---

## ✨ fold-oxa Implement set and dictionary helper functions

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 00:06 |

### Description

Create functional set and dictionary (map) operations in fabric/stitches/. Should include:
- Set: add, remove, member?, union, intersection, difference, subset?
- Dict: assoc, dissoc, lookup, keys, values, merge, map-values
- Efficient implementations (consider balanced trees or hash-based)
- Comprehensive tests
- Integration with existing collection utilities

---

## ✨ fold-57p Implement persistent queue and stack data structures

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 00:06 |

### Description

Create immutable/functional queue (FIFO) and stack (LIFO) data structures in fabric/stitches/. Should include:
- Queue: enqueue, dequeue, peek, empty?, length
- Stack: push, pop, peek, empty?, length
- Efficient implementation (e.g., Okasaki-style queues)
- Comprehensive tests
- Integration with existing prelude utilities

---

## ✨ fold-1dp BoardCraft SDK needs game template generator

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:00 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 01:09 |

### Description

Create a template system that generates complete, working game skeletons: (create-game 'hex-strategy) or (create-game 'square-tactics) that produces a basic but complete game with: board setup, unit placement, turn management, victory conditions. This would dramatically lower the barrier to entry for new game developers.

### Dependencies

- ⛔ **blocks**: `fold-dre`

---

## ✨ fold-lj7 Forum posting mechanism needs clear documentation and examples

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:00 |
| **Updated** | 2026-01-03 00:10 |
| **Closed** | 2026-01-03 00:10 |

### Description

The forum system is opaque to new users. Need: 1) Clear documentation on how to post from REPL, 2) Working examples of (msg) function usage, 3) Alternative posting methods for when REPL functions aren't available, 4) Examples of proper post format and channel selection. Essential for community building.

### Dependencies

- ⛔ **blocks**: `fold-lx9`

---

## ✨ fold-84b Error messages need context and suggestions for fixes

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:59 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 06:12 |

### Description

Current error messages are generic Scheme errors with no context about what went wrong in the Fold system. Should provide: 1) What the user was trying to do, 2) What went wrong specifically in Fold terms, 3) Suggested fixes or relevant documentation links. Critical for developer experience.

### Dependencies

- ⛔ **blocks**: `fold-lx9`
- ⛔ **blocks**: `fold-cz8`

---

## 🐛 fold-7vk Function signatures inconsistent between examples and implementation

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:57 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:55 |

### Description

Some functions like find-path-astar have different signatures in examples vs what the REPL expects. The examples show 4 arguments but the function actually expects 5 (including board). Need consistency between documentation and implementation.

---

## 📋 fold-6fc Satin tests: expansion, compilation, validation

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:23 |

### Description

Add tests for core forms expansion, compiler output equivalence, error reporting, and determinism; include golden-ish transcript expectations where possible.

### Dependencies

- ⛔ **blocks**: `fold-0mc`

---

## 📋 fold-ymn Satin examples: lesson pack + narrative showcase

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:29 |

### Description

Create at least two Satin-authored stories: (1) an educational mini-course (exercises + scoring) and (2) a narrative-heavy example (dialogue/quests/rules/timeline).

### Dependencies

- ⛔ **blocks**: `fold-0mc`
- ⛔ **blocks**: `fold-ant`

---

## 📋 fold-6o8 Satin tooling: lint, pretty, docs-as-data

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:36 |

### Description

Authoring tools: lints (style + correctness), pretty-printer for Satin forms, and a docs-as-data extractor that produces in-REPL help/inspectable structures (no new .md files).

### Dependencies

- ⛔ **blocks**: `fold-0mc`

---

## ✨ fold-udk Satin modules + libraries: import, stdlib

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 15:14 |
| **Closed** | 2026-01-02 15:14 |

### Description

Support (import ...)/(include ...) for story packs; ship a stdlib of reusable predicates/actions (inventory helpers, common intents, formatting helpers, localization hooks, randomness with seeded RNG).

### Dependencies

- ⛔ **blocks**: `fold-4hs`

---

## ✨ fold-vjy Satin narrative forms: dialogue, quests, rules, timeline

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 07:23 |

### Description

Add narrative authoring forms: (npc ...), (dialogue ...), (topic ...), (quest ...), (stage ...), (rule ...)/(on ...), (schedule ...), (once ...), optional (timeline ...)/(checkpoint ...). Must compile to Quill narrative constructs.

### Dependencies

- ⛔ **blocks**: `fold-4hs`
- ⛔ **blocks**: `fold-vzc`

---

## 🚀 fold-buk Satin: Quill Authoring DSL

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:51 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

A declarative, educational-first DSL that compiles into Quill stories. Goals: rapid authoring of lessons + complex narratives; strong validation; source-mapped errors; reusable libraries; deterministic behavior; REPL-first workflows.

### Dependencies

- ⛔ **blocks**: `fold-10r`
- ⛔ **blocks**: `fold-2fn`
- ⛔ **blocks**: `fold-4hs`
- ⛔ **blocks**: `fold-mpq`
- ⛔ **blocks**: `fold-vjy`
- ⛔ **blocks**: `fold-udk`
- ⛔ **blocks**: `fold-0mc`
- ⛔ **blocks**: `fold-6o8`
- ⛔ **blocks**: `fold-ymn`
- ⛔ **blocks**: `fold-6fc`

---

## 🐛 fold-pmw Forum posting mechanism unclear for new users

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:43 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:57 |

### Description

Attempted to post to forums using (msg) function as documented, but it's not available in base REPL. Need clearer documentation on how to access forum functions or alternative posting methods for users exploring the system.

---

## 🐛 fold-kez REPL documentation unclear - basic functions not available

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:43 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:57 |

### Description

The REPL doesn't seem to have basic functions like +, msg, help available. The documentation suggests these should be accessible but they appear to be unbound variables. Need clearer documentation on what's available in the base REPL and how to load necessary modules.

---

## 📋 fold-q81 Quill tests: runtime, DSL, persistence

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:39 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Add targeted tests for core runtime determinism, DSL validation, parser edge-cases, and persistence roundtrips; integrate into scheme --script test-all.ss where appropriate.

### Dependencies

- ⛔ **blocks**: `fold-53s`
- ⛔ **blocks**: `fold-u2c`
- ⛔ **blocks**: `fold-02a`
- ⛔ **blocks**: `fold-dtr`
- ⛔ **blocks**: `fold-v98`

---

## 📋 fold-gyk Quill demos: educational module + complex story

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:37 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Create at least two dogfood stories: (1) an educational lesson pack (tutorial-like) and (2) a narrative-heavy example with dialogue/quests/time features.

### Dependencies

- ⛔ **blocks**: `fold-ant`
- ⛔ **blocks**: `fold-fjo`
- ⛔ **blocks**: `fold-vzc`
- ⛔ **blocks**: `fold-v98`

---

## 📋 fold-ant Quill REPL integration: run loop + commands

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:37 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Expose ergonomic entrypoints: (quill-load ...), (quill-run ...), (quill-step ...), (quill-save ...); add help/commands integration so stories are easy to run from thimble/repl.ss.

### Dependencies

- ⛔ **blocks**: `fold-u2c`
- ⛔ **blocks**: `fold-02a`
- ⛔ **blocks**: `fold-dtr`
- ⛔ **blocks**: `fold-v98`

---

## 📋 fold-v5h Quill authoring tools: debugger, inspector, lint

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:37 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

REPL tools for authors: state inspector, node preview, trace/explain-why for gating conditions, lint reports, and minimal stepping/debug hooks.

### Dependencies

- ⛔ **blocks**: `fold-53s`
- ⛔ **blocks**: `fold-02a`
- ⛔ **blocks**: `fold-v98`

---

## ✨ fold-dtr Quill persistence: save/load, checkpoints, undo

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:37 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Persist Quill runs (state + story ref + transcript pointers) using The Fold's CAS; support checkpoints, undo/redo, and deterministic replay for debugging/teaching.

### Dependencies

- ⛔ **blocks**: `fold-v98`

---

## ✨ fold-02a Quill rendering: text templating + transcript

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:36 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:11 |

### Description

Render nodes to terminal/string with variable interpolation, consistent formatting (wrapping, headings, choice lists), and a transcript log suitable for assessment and debugging.

### Dependencies

- ⛔ **blocks**: `fold-v98`

---

## 📋 fold-z6u the-fold-xgk (P2): Complete system exploration and issue identification

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:09 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:09 |

### Description

Comprehensive exploration of The Fold system identifying 13 new maintenance issues across multiple components. Discovered working components (string utilities, file system, basic REPL, forum system) and documented issues (test failures, command exceptions, module loading problems, missing functionality). Created detailed exploration summary with recommendations for future development priorities.

---

## 🐛 fold-3dv Fix color module loading issue

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:43 |

### Description

The thimble/color.ss module fails to load with a compound condition exception. This prevents use of color-related functionality in the system. The error occurs when trying to load the module after successfully loading prelude.ss, string-utils.ss, fs.ss, and text.ss.

---

## 🐛 fold-ayl Fix command system exceptions

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:02 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:55 |

### Description

Basic commands like 'version' and 'who' are throwing unexpected exceptions instead of working properly. The error message shows "Command 'version' raised unexpected exception" and "Command 'who' raised unexpected exception". This suggests the command registration or handler implementation has issues.

---

## 🐛 fold-c4x Fix string-split function signature or documentation

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:59 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 00:03 |

### Description

The string-split function in thimble/string-utils.ss has inconsistent behavior. When called with (string-split "hello,world,test" #\,), it throws "Exception in string-length: #\, is not a string". The function appears to expect a string delimiter but the documentation and function signature may be unclear or incorrect.

---

## 🐛 fold-43y Fix prelude function bugs

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:57 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:59 |

### Description

Two prelude functions have bugs: 'prelude group' has unbound variable error (expected: ((1 1) (2 2 2) (3) (4 4)), got: (error unbound-variable x)), and 'prelude nub' returns wrong order (expected: (1 2 3 4), got: (2 1 4 3))

---

## 📋 fold-dj6 fold-iw7

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:29 |

### Description

Add comprehensive test coverage for scaffold system - Add comprehensive test coverage for scaffold template system to ensure generated code works correctly

---

## 📋 fold-40m fold-zg9

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:29 |

### Description

Add error recovery for daemon connection failures - Add retry logic and better error recovery when MCP server cannot connect to daemon (currently exits immediately)

---

## 📋 fold-b8p fold-t1x

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:27 |

### Description

Add error handling for malformed sexp files - Improve universe serialization error handling for malformed .sexp files in universe serialization code

---

## 📋 fold-73g fold-snd

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:27 |

### Description

Fix hardcoded paths in documentation and code - Replace hardcoded /home/user paths in COMMANDS.md, universe-output-example.sexp, and other files with proper path resolution

---

## 📋 fold-q5f fold-ehs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:25 |

### Description

Implement duckie persistence functionality - Implement TODO for duckie persistence using (duckie->block duckie) at line 663 in thimble/duckie-loop.ss

---

## 📋 fold-tgl fold-z7n

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:25 |

### Description

Implement TODO items in scaffold.ss template - Implement multiple TODO items in thimble/scaffold.ss at lines 260, 340, 403, 438, 534, 636 including tool logic, function definitions, and tests

---

## ✨ fold-zg9 Add error recovery for daemon connection failures

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:31 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:13 |

### Description

MCP server exits immediately if daemon is not running. Add retry logic and better error recovery for daemon connection issues.

---

## ✨ fold-ehs Implement duckie persistence functionality

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:31 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:32 |

### Description

TODO comment in thimble/duckie-loop.ss line 663 indicates duckie persistence using (duckie->block duckie) needs implementation.

---

## ✨ fold-iw7 Add comprehensive test coverage for scaffold system

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:31 |
| **Updated** | 2026-01-02 15:14 |
| **Closed** | 2026-01-02 15:14 |

### Description

The scaffold.ss template system needs more comprehensive tests to ensure generated code works correctly and TODO items are properly implemented.

---

## 🐛 fold-t1x Add error handling for malformed sexp files

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:31 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 15:02 |

### Description

Improve error handling in universe serialization when reading malformed .sexp files. Currently shows warning but could be more robust.

---

## 🐛 fold-snd Fix hardcoded paths in documentation and code

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:30 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:29 |

### Description

Replace hardcoded /home/user paths in COMMANDS.md, universe-output-example.sexp, and other files with configurable or relative paths.

### Dependencies

- 🔗 **related**: `fold-3jj`

---

## ✨ fold-z7n Implement TODO items in scaffold.ss template

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:30 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:54 |

### Description

Multiple TODO items in thimble/scaffold.ss need implementation including tool logic, function definitions, tests, and type implementations.

---

## 📋 fold-9qv Remove third-party dependencies from thimble/mcp-server

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:06 |
| **Updated** | 2026-01-02 14:13 |
| **Closed** | 2026-01-02 14:13 |

### Description

The mcp-server package currently depends on external npm packages:

Dependencies:
- @modelcontextprotocol/sdk (^1.0.4)

DevDependencies:
- @types/node (^22.0.0)
- typescript (^5.7.2)

This violates the project principle: 'No Third-Party Dependencies - Everything is built in-house. If we need a tool, we build it. Exceptions require approval from Andy.'

Required:
- Replace @modelcontextprotocol/sdk with in-house implementation
- Remove npm dependency chain
- Implement MCP protocol directly in Scheme/Fold

Priority: P2 - Consistency with project principles

### Dependencies

- 🔗 **related**: `fold-qks`
- 🔗 **related**: `fold-mp2`
- 🔗 **related**: `fold-ne4`

---

## 📋 fold-7e1 Port REPL daemon to Rust

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 21:53 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 22:21 |

### Description

Create fold-rs/src/bin/fold-daemon.rs. Watch .fold-repl/requests/*.ss for new files. Evaluate expressions, write to .fold-repl/responses/*.txt. Support session isolation (per-session environments). Create systemd service file for Rust daemon. Reference: thimble/repl-daemon.ss

### Dependencies

- 🔗 **parent-child**: `fold-wgs`
- ⛔ **blocks**: `fold-dff`

---

## 📋 fold-dff Shell cutover - update daemon.sh and fold.sh for Rust

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 21:53 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 22:16 |

### Description

Build release binary (cargo build --release). Update daemon.sh to use ./target/release/fold-repl instead of scheme --script. Keep Chez as fallback via FOLD_BACKEND=chez. Update fold.sh wrapper. Test forum commands (hi), (chat), (msg).

### Dependencies

- 🔗 **parent-child**: `fold-wgs`
- ⛔ **blocks**: `fold-dh7`

---

## 📋 fold-dh7 Cross-validation suite - verify Rust vs Chez parity

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 21:53 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 22:12 |

### Description

Extend tests/repl_parity.rs. For each core test file (test-block.ss, test-cas.ss, etc.), run with Chez Scheme and fold-rs, assert identical outputs. Add hash comparison tests to verify same input produces same hash. Test normalization produces same hashes for alpha-equivalent expressions.

### Dependencies

- 🔗 **parent-child**: `fold-wgs`

---

## 📋 fold-8hf Implement expansion (de Bruijn to named) in Rust

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 21:53 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 21:53 |

### Description

Port fabric/stitches/expand.ss to fold-rs/src/fabric/expand.rs. Inverse of normalization - converts de Bruijn indices back to named variables. Created expand(), make_symbol_supply(), expand_fresh(). Added expand primitive to prim.rs.

### Dependencies

- 🔗 **parent-child**: `fold-wgs`

---

## 📋 fold-9i4 Implement normalization (de Bruijn indices) in Rust

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 21:52 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 21:53 |

### Description

Port fabric/stitches/normalize.ss to fold-rs/src/fabric/normalize.rs. Converts S-expressions with named variables to de Bruijn indices for alpha-equivalence. Created normalize(), NormEnv, free_vars(). Added normalize primitive to prim.rs.

### Dependencies

- 🔗 **parent-child**: `fold-wgs`

---

## 📋 fold-ntj Primitive audit - add missing primitives to Rust

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 🔹 Medium (P2) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 21:52 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 21:53 |

### Description

Compare fabric/stitches/prim.ss with fold-rs/src/thimble/prim.rs. Added 11 missing primitives: sqrt, expt, log, sin, cos, tan, floor, ceiling, round, bv-copy, make-string

### Dependencies

- 🔗 **parent-child**: `fold-wgs`

---

## 🐛 fold-sk3l Add circular dependency protection to module loader

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 15:53 |
| **Updated** | 2026-01-02 16:33 |
| **Closed** | 2026-01-02 16:33 |

---

## 🐛 fold-2dtn Handle empty vectors in vec-norm-linf

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-02 15:53 |
| **Updated** | 2026-01-02 16:31 |
| **Closed** | 2026-01-02 16:31 |

---

## 🐛 fold-3ox Fix character escape sequences in core/help.ss examples

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 22:45 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 23:16 |

### Description

The example strings in help.ss contain unescaped character literals like #\x which cause parse errors. Need to escape as #\\x inside strings.

---

## 📋 fold-mmv Archive sentinel-dsl.ss (v1)

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2026-01-01 22:38 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 23:16 |

### Description

sentinel-dsl-v2.ss is the active version. Archive v1 to archive/agents/.

---

## ✨ fold-o6v fold-rs: Dependent types (dep-types.ss)

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:16 |
| **Labels** | dependent-types, fold-rs, type-system |

### Description

Port the dependent types system to Rust:
- dep-types.ss: Dependent type syntax and semantics
- dep-infer.ss: Inference for dependent types
- dep-linalg.ss: Type-safe linear algebra with dimensions

This is an advanced feature building on the base type system.

### Dependencies

- ⛔ **blocks**: `fold-46k`

---

## ✨ fold-ljw fold-rs: Pretty printer

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:16 |
| **Labels** | fold-rs, output |

### Description

Implement a pretty printer for Fold values:
- Wadler-Lindig algorithm (like pretty.ss)
- Configurable line width
- Indentation for nested structures
- Truncation for large values

Currently format_value just does simple printing.

---

## ✨ fold-yh9 fold-rs: Interactive REPL mode

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-01 21:16 |
| **Labels** | fold-rs, repl |

### Description

Implement interactive REPL mode for fold-repl:
- readline support for input editing
- Command history
- Multi-line input
- Error recovery (continue after parse/eval errors)
- REPL commands (:quit, :help, :load, etc.)

Currently only supports --expr and --file modes.

---

## 📋 fold-53h Missing numeric interpolation utilities in fold-rs

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 06:50 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 07:01 |

### Description

The following numeric utilities are declared in is_builtin_prim() but not implemented in prim.rs:

- lerp (linear interpolation)
- inverse-lerp (inverse linear interpolation)
- scale (scale value from one range to another)
- smoothstep (smooth Hermite interpolation)
- denormalize (inverse of normalize)
- saturation-add (addition with saturation/clamping)
- percent-of (percentage calculation)
- percent-change (percentage change between values)
- round-to (round to decimal places)

These are useful for graphics, animation, and game development.

Location: fold-rs/src/tools/fold_lower.rs:88-90

---

## 🚀 fold-prc Improve FP module test coverage (50% → 75%)

| Property | Value |
|----------|-------|
| **Type** | 🚀 epic |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 23:48 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-30 00:54 |

### Description

Currently only 55/110 FP modules have tests (50% coverage). Goal: Add tests for the most important untested modules to reach 75% coverage. Focus on: core type classes, critical data structures, and parser combinators.

---

## 📋 fold-4di Standardize FP toolkit naming conventions

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:05 |

### Description

Current naming is inconsistent across modules:

Bind operations:
- bind-maybe vs maybe-t-bind vs free-bind
- sg-append vs mappend vs monoid-append

Map operations:
- map-maybe vs fmap vs app-fmap

First/second:
- combinators.ss: first for pair transformation
- profunctor.ss: fn-first for Strong profunctor
- bifunctor.ss: pair-first for bifunctor

Applicative structure:
- typeclasses.ss: (list 'applicative functor pure ap)
- traversable.ss: (cons pure ap)

Should establish and document naming conventions:
- Type-specific ops: type-operation (maybe-bind, list-traverse)
- Generic ops through dictionaries: operation dict args
- Consistent prefixes for transformed versions

---

## ✨ fold-nay Add NonEmpty list type with Semigroup/Foldable1 instances

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:14 |

### Description

NonEmpty (a :| [a]) is a common FP data type representing guaranteed non-empty lists.

Key properties:
- Semigroup (no Monoid - can't be empty)
- Foldable1 (guaranteed to have at least one element)
- Apply/Bind (but not Applicative/Monad - pure would need to decide list length)

Useful for:
- Safe head/last operations
- Semigroup-based operations
- Type-level guarantees about non-emptiness

---

## ✨ fold-h1z Add Representable and Distributive functors

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:49 |

### Description

Missing advanced functor typeclasses:

Representable functor:
- Types isomorphic to ((->) r) for some r
- Enables tabulate/index operations
- Useful for memoization and efficient lookup structures

Distributive functor:
- Dual of Traversable
- distribute :: (Functor f, Distributive g) => f (g a) -> g (f a)
- Every Representable functor is Distributive

---

## 📋 fold-i79 Add missing FP utilities: liftA4+, asum, guard, coerce helpers

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:07 |

### Description

Missing common utilities:
- liftA4, liftA5 etc. (only lift2, lift3 exist)
- asum :: Alternative f => [f a] -> f a (fold with <|>)
- msum :: MonadPlus m => [m a] -> m a
- guard :: Alternative f => Bool -> f ()
- coerce/newtype helpers to reduce Sum/Product/First/Last/Endo/Dual boilerplate

Also add foldr to prelude (ironic for 'THE FOLD' not to have base fold available).

---

## ✨ fold-42v Add Selective applicative to FP toolkit

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:04 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:33 |

### Description

Selective is the abstraction between Applicative and Monad that captures conditional/branching effects without full monadic sequencing. Useful for:
- Static analysis of effectful programs
- Speculative execution
- Conditional effects

Key operation: select :: f (Either a b) -> f (a -> b) -> f b

Reference: https://hackage.haskell.org/package/selective

---

## ✨ fold-vvn Add missing FP typeclasses: Semigroupoid, Apply, Bind, MonadFail

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 20:03 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:33 |

### Description

Missing foundational typeclasses:
- Semigroupoid: Category without identity (Arrow composition base)
- Apply: Applicative without pure (useful for NonEmpty)
- Bind: Monad without return (useful for NonEmpty)
- MonadFail: Explicit pattern match failure handling
- MonadFix: Value recursion in monads

These provide more granular abstractions for types that don't have full Applicative/Monad instances.

---

## 📋 fold-4bm Implement packrat/memoization for parser combinators

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 13:59 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 22:20 |

### Description

Add optional memoization layer to parser combinators for improved performance on backtracking-heavy grammars. Listed in Parser Combinators Epic.

---

## ✨ fold-7rc Polyglot REPL with inline visualization

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:05 |
| **Updated** | 2026-01-02 02:55 |
| **Closed** | 2026-01-02 02:55 |

### Description

Rich multimedia in the terminal:

Inline Graphics:
- Render plots inline (Sixel/Kitty protocol)
- Show images from blocks
- Draw diagrams from data
- Animate simulations

Rich Tables:
- Auto-format lists as tables
- Sortable columns
- Expandable nested data
- Sparklines for trends

Interactive Widgets:
- Sliders for parameters
- Checkboxes for options
- Dropdown for choices
- Live update on change

Terminal Requirements:
- Support modern terminals (Kitty, iTerm2, WezTerm)
- Graceful fallback for basic terminals
- Web REPL for full features

Location: fold-rs/src/bin/fold-repl.rs, static/repl/

---

## ✨ fold-7b4 AI-assisted coding with context from CAS

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 05:04 |
| **Updated** | 2026-01-02 04:55 |
| **Closed** | 2026-01-02 04:55 |

### Description

Use AI that understands The Fold:

Features:
- Explain this block
- Generate function from signature
- Suggest next step based on context
- Find similar patterns in store
- Translate between DSLs

Context Awareness:
- AI sees block store contents
- AI sees type context
- AI sees related decisions (from scripture)
- AI can query forum history

Commands:
(ask "How do I traverse this block chain?")
(generate '(-> Int Int Int) "Adds two numbers")
(explain block-hash)
(similar current-block)

Safety:
- AI suggests, human approves
- All suggestions are reviewable
- No direct store modification

Location: thimble/ai-assist.ss

---

## ✨ fold-243 Implement Selective functors

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:30 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 20:37 |

### Description

Implement Selective functors (between Applicative and Monad):

Core operation:
- select : f (Either a b) -> f (a -> b) -> f b

Derived operations:
- branch : f (Either a b) -> f (a -> c) -> f (b -> c) -> f c
- ifS : f Bool -> f a -> f a -> f a
- whenS : f Bool -> f () -> f ()

Laws:
- Selective can inspect effects conditionally
- More powerful than Applicative (can branch)
- Less powerful than Monad (no dynamic binding)

Key insight: 
- Applicative: static effect structure
- Selective: conditionally skip effects
- Monad: fully dynamic effects

Applications:
- Build systems (conditional rebuilds)
- Parser optimization
- Speculative execution
- Static analysis of effects

Location: fabric/stitches/fp/selective.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`

---

## ✨ fold-bdx Implement Representable functors

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 04:28 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 21:54 |

### Description

Implement Representable for functors isomorphic to functions:

Representable f where:
- type Rep f  -- the index/key type
- tabulate : (Rep f -> a) -> f a
- index : f a -> Rep f -> a

Laws:
- tabulate . index = id
- index . tabulate = id

Key instances:
- Identity (Rep = ())
- Pair (Rep = Bool)
- Vec n (Rep = Fin n)
- Stream (Rep = Nat)
- Function (Rep = a)

Applications:
- Memoization (tabulate expensive function)
- Dynamic programming
- Comonads (every Representable is a Comonad)
- Zippers via Representable

Integration with:
- Vectors and matrices (finite tabulation)
- Streams (infinite tabulation)
- Memoization utilities

Location: fabric/stitches/fp/representable.ss

### Dependencies

- ⛔ **blocks**: `fold-bcs`
- ⛔ **blocks**: `fold-gb3`

---

## 📋 fold-c1c Auto-doc extractor for REPL help

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:32 |
| **Updated** | 2026-01-02 21:15 |
| **Closed** | 2026-01-02 21:15 |

### Description

Generate REPL-friendly docs from code: signatures, examples, and derived summaries. Store as data for inspection (no new .md files).

### Dependencies

- ⛔ **blocks**: `fold-z7y`

---

## 📋 fold-0wd Module dependency graph + cycle tooling

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 03:25 |
| **Updated** | 2026-01-02 21:28 |
| **Closed** | 2026-01-02 21:28 |

### Description

Build module/namespace dependency graphs, detect cycles, and provide suggestions for breaking them. Hook into REPL inspection.

### Dependencies

- ⛔ **blocks**: `fold-z7y`

---

## 📋 fold-hhc Implement machine learning utilities library

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:44 |
| **Updated** | 2026-01-02 04:54 |
| **Closed** | 2026-01-02 04:54 |

### Description

Create a comprehensive machine learning utilities library built on the autodiff system. This includes:

- Data preprocessing and transformation utilities
- Evaluation metrics and scoring functions
- Cross-validation and model selection tools
- Feature engineering utilities
- Data loading and batching utilities
- Common ML algorithms (k-NN, decision trees, clustering)
- Statistical analysis and visualization tools

Location: fabric/stitches/ (new ml-utils module)
Important for practical machine learning applications.

### Dependencies

- ⛔ **blocks**: `fold-m3u`
- ⛔ **blocks**: `fold-9c6`
- ⛔ **blocks**: `fold-23u`
- ⛔ **blocks**: `fold-81x`
- ⛔ **blocks**: `fold-637`

---

## 📋 fold-puc Create neural network examples and tutorials

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 02:44 |
| **Updated** | 2026-01-02 04:54 |
| **Closed** | 2026-01-02 04:54 |

### Description

Build comprehensive examples demonstrating neural network implementation using the autodiff system. This includes:

- Basic neural network architectures (MLP, CNN, RNN)
- Training loops and optimization examples
- Common loss functions and their gradients
- Mini-batch processing and data pipelines
- Model serialization and loading
- Transfer learning examples
- Tutorial documentation and walkthroughs

Location: playpen/creations/ (example programs) and forum/ (tutorials)
Essential for demonstrating autodiff capabilities and user onboarding.

### Dependencies

- ⛔ **blocks**: `fold-lim`
- ⛔ **blocks**: `fold-hhc`

---

## ✨ fold-qxq REPL session management and persistence needs improvement

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-29 00:00 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 01:59 |

### Description

Session management is unclear - sessions don't persist between commands without daemon, and it's not obvious how to: 1) Save/restore session state, 2) Switch between sessions, 3) See available sessions, 4) Understand session boundaries. This makes iterative development frustrating. Need better session lifecycle management.

### Dependencies

- ⛔ **blocks**: `fold-cz8`

---

## 🐛 fold-ifi Board size returns 0 for empty boards - unclear if intentional

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:57 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:54 |

### Description

When creating a new hex board with make-hex-board, the board-size function returns 0 even for boards with radius > 0. This might be intentional (counting only non-empty tiles) but is confusing for new users. Documentation should clarify this behavior.

---

## 📋 fold-59g Update start.sh REPL IPC guidance

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:06 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 17:13 |

### Description

start.sh still references single-file REPL IPC paths (.fold-repl/request.ss and .fold-repl/response.txt). The daemon now uses session-based files in .fold-repl/requests/<session-id>.ss and .fold-repl/responses/<session-id>.txt (see fold.sh/thimble/repl.ss). Update the guidance block in start.sh to match current IPC layout and mention SESSION usage or fold.sh wrapper.

---

## 📋 fold-t3x Consolidate duplicate string utils tests

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 23:06 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2026-01-02 02:29 |

### Description

There are overlapping string utils test suites in multiple locations (e.g., tests/test-string-utils.ss, tests/string-utils-test.ss, thimble/tests/test-string-utils.ss). Consolidate to a single canonical test file, update any references (test runners/docs), and remove redundant copies to prevent drift.

---

## 🐛 fold-7t3 Investigate: free-vars includes 'prim' as free variable

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:44 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:08 |

### Description

free-vars returns 'prim' as a free variable: (free-vars '(fn (x) (prim 'add x y))) => (prim y). Should 'prim' be excluded as a special form, or is this expected at the normalized level?

---

## ✨ fold-cjk Rust eval: add else clause to case expression

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:41 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:06 |

### Description

case expression fails with 'no matching clause' when no arm matches. Add (else expr) support for default fallback.

---

## ✨ fold-jde Rust parser: add scientific notation support

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:40 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:02 |

### Description

Numbers like 1e10 or 1.5e-3 parse as unbound variables. Add scientific notation to the Rust parser.

---

## 📋 fold-zgq fold-wdx

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:52 |

### Description

Add session timeout configuration - Make session timeout and cleanup intervals configurable in MCP server (currently hardcoded to 5 minutes)

---

## 📋 fold-3s2 fold-fuy

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:35 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:52 |

### Description

Add line number tracking to introspect complexity analysis - Add line number tracking to complexity analysis at line 224 in thimble/introspect/complexity.ss

---

## 📋 fold-60h fold-4yd

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:52 |

### Description

Implement NFC Unicode support in text.ss - Implement TODO for full NFC Unicode support using Unicode data tables at line 183 in thimble/text.ss

---

## 📋 fold-7oc fold-3e8

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:34 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:52 |

### Description

Implement color serialization in graphics.ss - Implement TODO for color serialization at line 130 in thimble/graphics.ss for full rendering support

---

## ✨ fold-fuy Add line number tracking to introspect complexity analysis

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:32 |
| **Updated** | 2026-01-02 15:25 |
| **Closed** | 2026-01-02 15:25 |

### Description

TODO comment in thimble/introspect/complexity.ss line 224 indicates line number tracking needs to be implemented for better analysis.

---

## ✨ fold-4yd Implement NFC Unicode support in text.ss

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:31 |
| **Updated** | 2026-01-02 15:14 |
| **Closed** | 2026-01-02 15:14 |

### Description

TODO comment in thimble/text.ss line 183 indicates full NFC using Unicode data tables needs implementation.

### Dependencies

- 🔗 **related**: `fold-fig`

---

## ✨ fold-3e8 Implement color serialization in graphics.ss

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:30 |
| **Updated** | 2026-01-02 03:05 |
| **Closed** | 2026-01-02 03:05 |

### Description

TODO comment in thimble/graphics.ss line 130 indicates full color serialization needs implementation when color rendering is needed.

---

## ✨ fold-ri8 Expand thimble introspect suite with coverage analysis

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:07 |
| **Updated** | 2026-01-02 15:25 |
| **Closed** | 2026-01-02 15:25 |

### Description

The introspect directory has basic tools:
- complexity.ss - Code complexity analysis
- memory.ss - Memory usage tracking
- timing.ss - Performance timing

Missing capabilities:
- Test coverage analysis
- Code coverage reporting
- Dead code detection
- Dependency analysis
- Performance regression detection

Should extend to provide comprehensive codebase health metrics.

Location: thimble/introspect/

Priority: P3 - Enhances development tooling

---

## ✨ fold-sor Implement proper DUCKIE persistence using block storage

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:06 |
| **Updated** | 2026-01-02 15:16 |
| **Closed** | 2026-01-02 15:16 |

### Description

The DUCKIE avatar system currently has a TODO for proper persistence. Line 663 in duckie-loop.ss shows:

;; TODO: Persist using (duckie->block duckie)

Current behavior: Displays 'Saving DUCKIE's soul...' but doesn't actually persist to block storage

Required:
- Implement duckie->block conversion function
- Store DUCKIE state in content-addressed storage
- Ensure proper serialization/deserialization
- Maintain state across sessions

Location: thimble/duckie-loop.ss line 663

Priority: P3 - Feature enhancement for DUCKIE system

### Dependencies

- ⛔ **blocks**: `fold-qxq`

---

## 🐛 fold-39z Fix error message formatting bug - ~s placeholders not substituted

| Property | Value |
|----------|-------|
| **Type** | 🐛 bug |
| **Priority** | ☕ Low (P3) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:06 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-29 14:55 |

### Description

Error messages display literal ~s placeholders instead of actual values in Scheme implementation. Note: Rust implementation has proper error messages. This only affects Scheme fallback mode (FOLD_USE_SCHEME=1).

### Dependencies

- ⛔ **blocks**: `fold-84b`

---

## 📋 fold-f99 Refactor prim.rs: extract match arms into organized modules

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 💤 Backlog (P4) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-30 06:51 |
| **Updated** | 2026-01-02 04:54 |
| **Closed** | 2026-01-02 04:54 |

### Description

fold-rs/src/thimble/prim.rs has grown to 4200+ lines with 373 primitive implementations in a single giant match expression.

**Current issues:**
- Single function apply_prim() is too large
- 185 unary arg checks, 92 binary arg checks - repetitive boilerplate
- Hard to navigate and maintain
- Similar primitives scattered throughout

**Suggested refactoring:**
1. Split into modules by category:
   - prim/arithmetic.rs (math operations)
   - prim/string.rs (string manipulation)
   - prim/list.rs (list operations)
   - prim/vector.rs (vector operations)
   - prim/comparison.rs (predicates and comparisons)
   - prim/io.rs (display, write, etc.)

2. Use macro or table-driven approach for arg count checking

3. Consider using a HashMap<&str, fn(&[Value]) -> Result<Value, EvalError>> dispatch table

**Priority: Low** - Code works correctly, this is technical debt cleanup.

Location: fold-rs/src/thimble/prim.rs

---

## ✨ fold-8j3 QoL: Improve closure display format

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 💤 Backlog (P4) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:43 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:10 |

### Description

Closures display as #<closure>. Could show arity or simplified body, e.g. #<closure (x y) -> ...> or #<closure/2>.

---

## ✨ fold-xq3 QoL: Add let* (sequential let) to Rust eval

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 💤 Backlog (P4) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:42 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:09 |

### Description

Current let bindings are parallel (like Scheme let). Add let* for sequential bindings where each can reference previous ones. Common pattern that currently requires nesting.

---

## ✨ fold-adp Rust parser/eval: add quasiquote support

| Property | Value |
|----------|-------|
| **Type** | ✨ feature |
| **Priority** | 💤 Backlog (P4) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:41 |
| **Updated** | 2026-01-02 02:46 |
| **Closed** | 2025-12-28 23:13 |

### Description

Add quasiquote (`) and unquote (,) for easier list construction. Currently returns 'unbound variable: quasiquote'.

---

## 📋 fold-545 Standardize TODO comment format across thimble codebase

| Property | Value |
|----------|-------|
| **Type** | 📋 task |
| **Priority** | 💤 Backlog (P4) |
| **Status** | ⚫ closed |
| **Created** | 2025-12-28 22:06 |
| **Updated** | 2026-01-02 15:14 |
| **Closed** | 2026-01-02 15:14 |

### Description

Various TODO styles found across thimble/:

Current inconsistent formats:
- 'TODO: Add usage examples' (init-project.ss)
- 'TODO: Add detailed description' (init-project.ss)
- ';;; TODO: Implement functionality' (init-project.ss)
- 'TODO: Define your functions here' (scaffold.ss)
- 'TODO: Implement TODOs and placeholder functions' (examples/)
- No tracking or prioritization

Should standardize to:
- Consistent prefix (e.g., ;;; TODO[priority]: description)
- Cross-reference with beads issues
- Add context about what's needed

Priority: P4 - Code quality and consistency

---

