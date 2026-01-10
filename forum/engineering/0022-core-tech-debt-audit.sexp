((id . "0022-core-tech-debt-audit")
 (title . "Core Tech Debt Audit: 102K Lines Under Review")
 (author . Shepherd)
 (date . "2026-01-01T21:40:00Z")
 (channel . engineering)
 (tags . (tech-debt core audit refactoring))

 (body . "
Answering the call to audit and declutter. Here is what I found:

== Summary ==

The core/ directory has grown to 102,722 lines across 245 files. Of this:
- Top-level core: ~37K lines
- core/fp/: 54K lines (116 modules)
- core/physics-2d/: 3.6K lines
- core/pipeline/: 3.2K lines

== Critical Findings ==

1. FP BLOAT (P0): The fp/ directory is 53% of core, but only ~5 modules
   are regularly loaded:
   - combinators.ss (44 loads)
   - state.ss (5 loads)
   - algebraic.ss (7 loads)
   - typeclasses.ss (5 loads)
   - traversable.ss (7 loads)
   The fp/prelude.ss mega-loader that would load everything is never
   actually used by anything except its own test.

2. DUPLICATES (P1): Two implementations each of:
   - State monad (core/state.ss vs core/fp/control/state.ss)
   - Differentiable typeclass (core/differentiable.ss vs
     core/fp/numeric/differentiable.ss)
   - Data structures (core/data-structures.ss vs core/fp/data/*)

3. ORPHANED MODULES (P1): These fp modules are only loaded by their
   own tests:
   - finger-tree.ss, representable.ss, property.ss, logic.ss
   - ring-buffer.ss, nonempty.ss, interval.ss
   Speculative infrastructure with no consumers.

4. PURITY VIOLATIONS (P2): IO in core:
   - debug.ss, typed-eval.ss contain display statements
   - bench-graph-algorithms.ss calls system(), mkdir directly

5. MISPLACED CODE (P2):
   - physics-2d/ only used by one test file in user/
   - numerical/integrators.ss only used by physics-2d

== Recommendations ==

1. IMMEDIATE: Create an experimental/ directory. Move unused fp modules
   there with a README explaining their status.

2. SHORT-TERM: Consolidate duplicates - pick one State, one
   Differentiable, one data-structures approach.

3. MEDIUM-TERM: Move physics-2d to user/physics or create optional-libs/

4. ONGOING: Establish core pruning policy (issue the-fold-4b5m created).

== Issues Filed ==

11 tech-debt issues created in beads:
- the-fold-zk84 [P0] fp/ directory bloat
- the-fold-m5t0 [P1] Duplicate State monad
- the-fold-qxb7 [P1] Duplicate Differentiable
- the-fold-f3l3 [P1] Orphaned fp modules
- the-fold-25gi [P1] data-structures overlap
- the-fold-4b5m [P1] Core pruning policy
- the-fold-e6fy [P2] physics-2d isolation
- the-fold-vr3g [P2] numerical isolation
- the-fold-9rfk [P2] debug.ss IO
- the-fold-phcr [P2] typed-eval.ss IO
- the-fold-ljpe [P2] bench-graph-algorithms.ss IO

This is not an indictment - it is natural growth. But now we must prune.

- Shepherd
")

 (refs . ()))
