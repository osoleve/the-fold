# Lattice Growth Proposals

Proposed new leaves for the skill lattice, prioritized by composition surface
with existing modules and value for agent training.

---

## Current Landscape

The lattice has ~30 top-level directories covering linear algebra, abstract
algebra (through Galois theory), differential geometry, category theory (through
Kan extensions), control systems (through H-infinity synthesis), information theory,
optimization (LP through L-BFGS), topology (simplicial homology), probabilistic
methods (through variational inference), and more.

The interesting question isn't "what domains are missing" but **where would a new
leaf create the most composition surface with what's already there?**

Each proposal below identifies:
- What it is and why it matters
- Which existing modules it composes with (the "wiring diagram")
- What it adds to the agent training story
- Approximate scope

---

## Proposal 1: Persistent Homology / TDA Pipeline

**What:** Filtrations, persistence diagrams, bottleneck/Wasserstein distances,
persistence landscapes, and statistical inference on topological features.

**Why:** The lattice has the two endpoints — `topology/` computes homology of
a single complex, and `statistics/` does regression and hypothesis testing — but
the bridge between them doesn't exist. Persistent homology *is* that bridge:
it tracks how topological features (connected components, loops, voids) are born
and die as a parameter varies.

**Composition wiring:**

```
geometry/ (point cloud, Voronoi)
    → persistent-homology (Vietoris-Rips filtration)
        → topology/ (homology computation at each threshold)
            → statistics/ (persistence landscapes, permutation tests)
                → optimization/ (bottleneck distance via LP)
```

Additional connections:
- `autodiff/` — Differentiable persistence is an active research area. Gradients
  through persistence diagrams enable topological loss functions for optimization.
- `linalg/` — Persistence computation reduces to matrix reduction over Z/2Z
  (connects to the existing field machinery in `algebra/`).
- `data/graph/` — Graph filtrations (by edge weight) are a natural special case.
  Connects to community detection via persistent H_0.

**Agent training value:** Persistence diagrams are completely impossible to
brute-force. An agent must construct the filtration, track homological features
across thresholds, and the output is a structured artifact (a multi-set of
birth-death pairs), not a single number. The 4-module pipeline (geometry →
filtration → homology → statistics) requires genuine tool composition.

**Example training problem:**
> Given 50 points sampled from a noisy annulus, compute the persistence diagram
> for H_1. What is the death time of the most persistent 1-cycle? Verify that
> the bottleneck distance to the diagram of a clean circle is below 0.3.

**Scope:** ~3-4 files. `filtration.ss` (Vietoris-Rips, alpha complex construction),
`persistence.ss` (matrix reduction, birth-death pairing), `persistence-distance.ss`
(bottleneck, Wasserstein), `persistence-stats.ss` (landscapes, statistical tests).

**Priority: High** — Strongest agent training signal of all proposals.

---

## Proposal 2: Coding Theory

**What:** Linear codes, Reed-Solomon, BCH, LDPC. Encoding, syndrome decoding,
error correction, and bounds (Hamming, Singleton, Gilbert-Varshamov).

**Why:** The lattice has both ends of this bridge. `algebra/` has fields, field
extensions, Galois theory, and Groebner bases. `info/` has entropy, channel
capacity, and rate-distortion theory. Coding theory is literally "algebraic
structures over finite fields applied to information-theoretic bounds."

**Composition wiring:**

```
algebra/field-ext.ss (GF(2^n) arithmetic)
    → coding/ (generator/parity-check matrices over finite fields)
        → info/channel-capacity.ss (verify Shannon limit)

algebra/groebner.ss (syndrome polynomial computation)
    → coding/ (algebraic decoding)
        → linalg/ (matrix operations over GF(q))

number-theory/ (primitive roots, cyclotomic polynomials)
    → coding/ (BCH code construction)
```

Additional connections:
- `algebra/polynomial.ss` — Reed-Solomon codes are polynomial evaluation codes.
  Encoding is polynomial evaluation at specified points, decoding is polynomial
  interpolation (Berlekamp-Massey or Euclidean algorithm over the extension field).
- `random/` — Random linear codes, probabilistic decoding (belief propagation
  for LDPC connects to `random/bayesian.ss`).
- `linalg/` — Parity check matrices, null spaces, systematic form.

**Agent training value:** The "encode → corrupt → decode → verify" pipeline is
a natural multi-step problem. Each step requires a different module (field
arithmetic, matrix operations, polynomial algebra, information-theoretic bounds).
The answer is a decoded message — either it's right or it's not.

**Example training problem:**
> Construct a Reed-Solomon code over GF(2^4) with minimum distance 5. Encode
> the message (1 0 1 1). Introduce 2 symbol errors at positions 1 and 4.
> Decode using syndrome computation. Verify the corrected codeword matches
> the original encoding.

**Scope:** ~3 files. `linear-code.ss` (generator/parity matrices, systematic
form, Hamming/Singleton bounds), `reed-solomon.ss` (RS encoding/decoding,
Berlekamp-Massey), `ldpc.ss` (sparse parity check, belief propagation).

**Priority: High** — Tight multi-module composition, algebraically precise,
fully verifiable.

---

## Proposal 3: Algebraic Language Theory (Automata Deepening)

**What:** DFA/NFA construction and minimization, regular expression algebra,
syntactic monoids, Myhill-Nerode equivalence, Krohn-Rhodes decomposition,
finite-state transducers.

**Why:** `automata/` is the thinnest leaf in the lattice — just statecharts and
a zipper. The parser combinator module exists in `fp/parsing/` but there's no
formal language theory connecting parsers to the algebraic structure of the
languages they recognize.

The real value isn't "DFA/NFA as data structures" (that's undergraduate).
It's the *algebraic theory*: every finite automaton has a syntactic monoid
(connecting to `algebra/group.ss`), and every finite automaton decomposes into
simple groups and flip-flops (Krohn-Rhodes). This is automata theory where the
algebra module does the heavy lifting.

**Composition wiring:**

```
algebra/group.ss (semigroup/monoid structure)
    → automata/ (syntactic monoid of a language)
        → fp/parsing/ (parser ↔ recognizer correspondence)

algebra/group.ss (simple group decomposition)
    → automata/ (Krohn-Rhodes: automaton → cascade of simple components)

fp/rewrite/ (term rewriting)
    → automata/ (transducers as rewrite systems on strings)
        → fp/symbolic/ (regular expression simplification)
```

Linguistics connections:
- Finite-state transducers for phonological rules (two-level morphology)
- Formal language hierarchy maps to computational complexity classes, which
  relates to the fuel model (regular = O(n), context-free = O(n^3))
- Weighted automata over the tropical semiring (see Proposal 5) = Viterbi
  decoding

**Agent training value:** Exercises a different reasoning modality than
numerical computation. Problems like "compute the syntactic monoid of this
DFA and determine its algebraic complexity" require symbolic reasoning about
equivalence classes, not number crunching. Anti-brute-force property: the
monoid structure of even a small DFA can't be guessed.

**Example training problem:**
> Construct the minimal DFA for the language L = { w in {a,b}* : w contains
> "aba" as a substring }. Compute its syntactic monoid. What is the order of
> the monoid? Is L star-free? (Star-free iff syntactic monoid is aperiodic —
> connect to group theory.)

**Scope:** ~4 files. `dfa.ss` (DFA/NFA construction, product, minimization,
Myhill-Nerode), `regex-algebra.ss` (regular expressions as algebraic objects,
Brzozowski derivatives), `syntactic-monoid.ss` (monoid extraction, aperiodicity
test, Krohn-Rhodes), `transducer.ss` (FSTs, composition, two-level rules).

**Priority: Medium-High** — Unique reasoning modality, strong algebra connection,
linguistics alignment.

---

## Proposal 4: Typed Feature Structures

**What:** HPSG/LFG-style typed feature structures with subsumption ordering,
unification (meet), generalization (join), type inheritance, and
appropriateness conditions.

**Why:** This might be the most architecturally natural addition possible.
Typed feature structures *are literally lattice objects*. They form a lattice
under the subsumption ordering. Unification is meet. Generalization is join.
The name writes itself.

**Composition wiring:**

```
optics/ (lenses into features = optic access into nested typed records)
    → feature-structures/
        → core/types/ (bounded polymorphism, type inheritance)

fp/category/limits.ss (unification as a limit/pullback)
    → feature-structures/
        → fp/clp/ (constraint-based grammars = CLP over feature domains)

fp/rewrite/ (phonological/morphological rules as term rewriting)
    → feature-structures/
        → fp/parsing/ (unification-based parsing)
```

Additional connections:
- `optics/` — A path into a feature structure (e.g., SYNSEM|LOCAL|CAT|HEAD)
  is literally a lens composition. `view`, `set`, `over` on feature structures
  are optic operations.
- `core/types/` — Feature structure types with inheritance hierarchies are
  a subtyping system. Appropriateness conditions (which features are
  appropriate for which types) are type constraints.
- `fp/category/limits.ss` — Unification is a categorical limit (pullback).
  Most general unifier = categorical coequalizer. This makes the
  category theory module do real linguistic work.
- `fp/clp/` — Constraint-based grammar formalisms (HPSG constraints, LFG
  functional equations) are literally constraint logic programming over
  feature structure domains.
- `egraph/` — Feature structure subsumption modulo type hierarchy connects
  to equality saturation with type-aware cost models.
- `topology/` — The space of feature structures with subsumption ordering
  forms a Scott domain; the topology is the Scott topology. (Exotic but real.)

**Agent training value:** Exercises *symbolic unification* — a reasoning
modality that nothing else in the lattice requires. Unification problems are
hard to brute-force because the search space is structured (you need to find
the most general unifier, not just any solution). Multi-module problems like
"parse this sentence using a unification grammar, extract the feature structure,
lens into a specific path" require optics + parsing + unification composition.

**Example training problem:**
> Define feature structure types for a simple verb agreement system: NP with
> CASE and AGR features, VP with SUBJ agreement. Unify the subject NP
> [CASE nom, AGR [NUM sg, PER 3]] with the verb's SUBJ constraint
> [AGR [NUM sg]]. What is the result? Now attempt unification with
> [CASE nom, AGR [NUM pl, PER 3]]. Does it succeed or fail?

**Scope:** ~3 files. `feature-structure.ss` (typed FS representation,
subsumption, unification, generalization), `type-hierarchy.ss` (inheritance,
appropriateness, GLB computation for typed unification), `fs-optics.ss`
(optic interface: path-lens, feature-traversal).

**Priority: Medium-High** — Touches the most existing modules of any proposal.
Unique reasoning modality (symbolic unification). Deep alignment with project
name and philosophy.

---

## Proposal 5: Tropical Algebra

**What:** Tropical semirings (min-plus and max-plus), tropical matrix algebra,
tropical polynomials, Newton polygons, and the correspondence between
classical and tropical operations.

**Why:** Highest elegance-per-line ratio of any proposal. Tropical algebra is
a single algebraic framework that reveals several existing algorithms as
*the same algorithm over different semirings*:

| Classical operation | Tropical version |
|---|---|
| Matrix multiplication | Shortest paths (Floyd-Warshall) |
| Eigenvalues | Critical circuit ratio |
| Linear programming | Tropical linear algebra |
| Polynomial roots (Newton polygon) | Tropical algebraic geometry |
| Viterbi (HMM decoding) | Tropical semiring computation |

**Composition wiring:**

```
algebra/ring.ss (semiring abstraction — already exists)
    → tropical/ (min-plus and max-plus instantiation)
        → data/graph/graph-algorithms.ss (shortest path = tropical matmul)
        → optimization/lp.ss (LP duality via tropical)
        → algebra/polynomial.ss (Newton polygons)
        → linalg/ (tropical eigenvalues, tropical SVD)
```

The key insight is that `algebra/ring.ss` already provides the ring/semiring
abstraction. Tropical algebra is just a new *instantiation* that makes existing
algorithms fall out as special cases. This is the lattice doing exactly what
it's built to do: reifying cognitive shortcuts.

**Agent training value:** Problems like "compute the shortest path matrix
using tropical matrix multiplication and verify it matches the Floyd-Warshall
result from graph-algorithms" require understanding the algebraic correspondence,
not just running a function. The "same answer, different path" structure is
excellent for testing whether agents understand composition vs. just calling tools.

**Example training problem:**
> Construct the tropical (min-plus) adjacency matrix for a weighted digraph
> with 5 vertices. Compute its tropical matrix power A^4. What is the entry
> (0,3)? Verify this equals the shortest path from vertex 0 to vertex 3
> computed via the graph module's shortest-path function.

**Scope:** ~2 files. `tropical.ss` (min-plus and max-plus semirings, tropical
matrix operations, tropical polynomial evaluation), `tropical-applications.ss`
(shortest-path correspondence, Newton polygon extraction, critical circuit).

**Priority: Medium** — Small scope, high conceptual leverage, strong composition
with existing algebra and graph infrastructure.

---

## Proposal 6: Probabilistic Programming Monad

**What:** `sample`, `observe`, `condition` as first-class effectful operations
composed into the algebraic effect system, with inference backends (importance
sampling, MH, HMC, enumeration).

**Why:** `random/` already has distributions, Monte Carlo, Bayesian inference,
and variational inference. `fp/control/effects.ss` has algebraic effects.
The question is whether these compose — whether you can write a probabilistic
program using the effect system and run inference over it. If that bridge
doesn't exist, it's the single highest-value connection to build, because it
makes *every existing leaf probabilistic*.

**Composition wiring:**

```
fp/control/effects.ss (algebraic effects: sample/observe handlers)
    → probabilistic/ (inference = effect interpretation)
        → random/distributions.ss (primitive distributions)
        → random/monte-carlo.ss (MCMC backends)
        → random/variational-inference.ss (VI as alternative handler)

autodiff/ (gradient through probabilistic programs)
    → probabilistic/ (HMC, reparameterization trick, score function estimator)

fp/game/ (Bayesian game theory: priors over types, posterior beliefs)
    → probabilistic/ (belief updating as probabilistic conditioning)

physics/ (stochastic dynamics, Langevin simulation)
    → probabilistic/ (noise as a sampled effect)

info/ (KL divergence as evidence lower bound)
    → probabilistic/ (ELBO optimization for VI)
```

Content addressing angle: A probabilistic program is an S-expression.
Its posterior is determined by its code + data. Content-addressing a posterior
(or at least a posterior summary / sufficient statistics) in the CAS means
you can cache inference results and share them across sessions.

**Agent training value:** Probabilistic programs have stochastic outputs, so
verification requires statistical tests (connect to `statistics/hypothesis/`).
Multi-step problems: "define a generative model, condition on data, run
inference, check that the posterior mean is within a confidence interval."
The stochasticity itself is an anti-brute-force property — you can't guess
a posterior.

**Example training problem:**
> Define a probabilistic model: mu ~ Normal(0, 10), sigma ~ HalfNormal(5),
> then observe [2.3, 1.8, 3.1, 2.7, 2.2] ~ Normal(mu, sigma). Run 5000
> samples of MH inference. What is the posterior mean of mu? Is the 95%
> credible interval for sigma entirely below 3.0?

**Scope:** ~2-3 files. `ppl.ss` (sample/observe/condition effect definitions,
trace data structure), `inference.ss` (importance sampling, MH, enumeration
handlers), `ppl-autodiff.ss` (HMC, reparameterization — bridges to autodiff).

**Priority: Medium** — Highest potential impact (makes everything probabilistic),
but depends on how much of this `random/bayesian.ss` and
`random/variational-inference.ss` already cover. May be more of a bridge/refactor
than a new leaf.

---

## Summary Matrix

| Proposal | New modules | Modules composed | Reasoning modality | Agent training value |
|---|---|---|---|---|
| 1. Persistent homology | 3-4 | topology, geometry, statistics, linalg, optimization | Pipeline composition | Highest |
| 2. Coding theory | 3 | algebra, info, linalg, number-theory, random | Algebraic + verify | High |
| 3. Algebraic automata | 4 | algebra, fp/parsing, fp/rewrite, fp/symbolic | Symbolic/structural | Medium-High |
| 4. Feature structures | 3 | optics, types, fp/clp, fp/category, fp/rewrite, fp/parsing, egraph | Unification | Medium-High |
| 5. Tropical algebra | 2 | algebra, data/graph, optimization, linalg | Algebraic correspondence | Medium |
| 6. PPL monad | 2-3 | fp/control, random, autodiff, fp/game, physics, info, statistics | Probabilistic | Medium (may overlap existing) |

## Recommended Build Order

1. **Tropical algebra** — Smallest scope, highest insight-per-line. Proves the
   semiring abstraction in `algebra/ring.ss` is doing real work. Good warmup.
2. **Persistent homology** — Highest agent training value. Natural extension
   of existing topology module.
3. **Coding theory** — Bridges the algebra↔info gap. Clean, verifiable, well-scoped.
4. **Algebraic automata** — Deepens the thinnest leaf. Unlocks linguistic applications.
5. **Feature structures** — Architecturally ambitious. Benefits from automata
   existing first (transducers for morphophonology).
6. **PPL monad** — Audit `random/bayesian.ss` and `fp/control/effects.ss` first
   to determine actual gap. May be smaller than it looks.
