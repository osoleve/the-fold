;;; FOLD-2026-001: Core Pruning Policy
;;; Establishing criteria for what belongs in core/

((id . "FOLD-2026-001")
 (date . "2026-01-01T22:00:00Z")
 (settled-by . shepherd)
 (title . "Core Pruning Policy")

 (context . "
The core/ directory had grown to 102K lines with significant bloat:
- 54K lines of mostly unused FP modules
- Physics simulation used only by user/ tests
- Benchmark suites with IO throughout
- Debug/REPL print functions violating purity

A tech debt audit identified the need for clear criteria to prevent
future bloat and guide pruning decisions.
")

 (decision . "
Establish the following policy for core/ membership:

=== CORE INCLUSION CRITERIA ===

A module belongs in core/ if and only if it satisfies ALL of:

  1. PURITY: No IO operations (display, file ops, system calls)
     - Exception: Logging via fuel-tracked side channels

  2. TOTALITY: All functions terminate (via fuel parameter)
     - No unbounded recursion without fuel checks

  3. ACTUAL USE: Loaded by other core/ or shell/ modules
     - Not just by its own tests
     - Not just by user/ experiments

  4. FOUNDATIONAL: Provides primitives others build on
     - Block operations, hashing, evaluation
     - Type system, inference, kinds
     - Core data structures used throughout

=== EXCLUSION CRITERIA ===

A module should NOT be in core/ if ANY of:

  1. Only loaded by its own test file
  2. Only loaded by user/ code
  3. Contains display/write/newline/printf
  4. Contains file-exists?/mkdir/system/delete-file
  5. Is a benchmark, profiler, or visualization tool
  6. Is experimental or speculative (move to experimental/)

=== DIRECTORY RESPONSIBILITIES ===

  core/      Pure, total, foundational, actually used
  shell/     IO, validation, capabilities, defensive code
  user/      Experiments, games, demos, templates
  experimental/  Speculative modules awaiting promotion

=== PRUNING PROCEDURE ===

When auditing core/:

  1. Run: grep -r 'load.*core/<module>' to find consumers
  2. If only self-loads or tests → candidate for removal
  3. Check for IO: grep 'display\\|write\\|system\\|mkdir'
  4. If IO found → must move to shell/ or remove
  5. If unused → move to experimental/ or delete

=== PROMOTION FROM EXPERIMENTAL ===

To promote a module from experimental/ to core/:

  1. Demonstrate actual use by core/ or shell/ code
  2. Verify purity (no IO)
  3. Add comprehensive tests
  4. Document in module header
  5. Update this policy if new category established
")

 (rationale . "
The core must remain small, pure, and focused. Bloat in core/:

  - Slows load times for all users
  - Increases cognitive overhead
  - Makes it harder to verify purity guarantees
  - Violates the principle of 'core is the trusted base'

By requiring actual use (not just tests), we ensure core/ contains
only code that has proven its value. Speculative work belongs in
experimental/ where it can mature without polluting the core.

The 2026-01-01 audit reduced core/ from 102K to ~52K lines by
applying these criteria retroactively.
")

 (alternatives
  ((no-policy . "Continue ad-hoc decisions. Rejected: leads to gradual bloat.")
   (stricter . "Require 3+ consumers. Rejected: too restrictive for foundational code.")
   (looser . "Allow IO in core with markers. Rejected: slippery slope to impurity.")))

 (implications
  ("New core/ additions require justification against criteria"
   "Periodic audits should check for modules violating policy"
   "experimental/ becomes staging area for speculative work"
   "shell/ absorbs all IO-having utilities"
   "user/ absorbs all demo/experiment code"))

 (metrics
  ((before . ((core-lines . 102722)
              (core-files . 245)
              (fp-lines . 54112)))
   (after . ((core-lines . ~52000)
             (core-files . ~145)
             (fp-lines . 9192)))
   (reduction . "49% reduction in core/ size")))

 (references
  ("the-fold-zk84: FP bloat audit"
   "the-fold-ljpe: IO violation (bench-graph-algorithms)"
   "the-fold-phcr: IO violation (typed-eval REPL)"
   "the-fold-9rfk: IO violation (debug print)"
   "the-fold-e6fy: physics-2d isolation"
   "the-fold-vr3g: numerical isolation")))
