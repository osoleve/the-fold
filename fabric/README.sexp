;;; fabric/README.sexp — The Fabric of The Fold
;;;
;;; This directory contains the load-bearing core of The Fold.

((title . "Fabric: The System Core")
 (tier-access . shepherd)
 (purity . "All fabric code is pure, typed, and total")
 (description . "
The Fabric is the foundation of The Fold. It defines the language,
the type system, the evaluation model, and the block primitives.

Everything in fabric/ is:
- Functionally pure (no side effects)
- Type-checked (bidirectional inference)
- Total (terminates with fuel limits)
- Call-by-value semantics
")
 (structure . (
   ((name . "fabric/stitches/")
    (purpose . "Pure, typed, load-bearing core modules")
    (description . "Individual threads (functions, modules) that weave together
                    to form the computational fabric. Each 'stitch' is a
                    self-contained module with minimal dependencies.")
    (contents . (
      "block.ss      - Block construction and serialization"
      "normalize.ss  - S-expr → canonical form (de Bruijn indices)"
      "expand.ss     - Canonical form → S-expr with symbols"
      "cas.ss        - Content-addressed store (pure, in-memory)"
      "sha256.ss     - SHA-256 hashing"
      "prim.ss       - Pure primitive dispatcher"
      "types.ss      - The type system"
      "kinds.ss      - Kind system (higher-kinded types)"
      "infer.ss      - Bidirectional type inference"
      "resolve.ss    - Type class instance resolution"
      "annotate.ss   - AST annotation with inferred types"
      "parse.ss      - Parser combinators"
      "eval.ss       - Pure evaluator with fuel"
      "compile.ss    - Compilation pipeline"
      "prelude.ss    - Pure utility functions"
      "error.ss      - Error types and formatting"
      "test-*.ss     - Unit tests (one per module)"
      "run-tests.ss  - Core test runner")))
   ((name . "fabric/patterns/")
    (purpose . "Canonicalized abstractions and reusable components")
    (description . "Higher-level patterns built from stitches.
                    Reusable components that don't fit in the core.")
    (contents . (
      "parse.ss      - Meta-parsing utilities"
      "query.ss      - Query combinators")))
   ((name . "fabric/wrinkles/")
    (purpose . "Low-level extensions and exceptional cases")
    (description . "Primitives that cannot be implemented in pure Scheme.
                    Foreign function interfaces, system calls, unsafe operations.
                    Currently empty - reserved for future use.")
    (contents . "(empty)"))))
 (philosophy . "
Fabric is the machine. It assumes perfect input. It never fails
(except by returning an error value). It computes answers.

Thimble wraps Fabric, providing defensive validation and IO.
Users interact with Thimble. Thimble calls Fabric.

Metaphor:
- Stitches: Individual threads (functions)
- Patterns: Woven fabrics (abstractions)
- Wrinkles: Seams where fabric meets the world (FFI)
")
 (rules . (
   "Pure functions only - no (set!), no IO, no mutation (except local gensym counters)"
   "Total functions - always terminate with fuel parameter"
   "Type-checked - all exports have type signatures"
   "Defensive-free - assume inputs are valid (Thimble validates)"
   "Core assumes perfect input - errors are values, not exceptions"
   "De Bruijn indices - names are presentation, not semantics"))
 (for-builders . "
Builders may READ fabric/ to understand the system.
Builders must NOT MODIFY fabric/ (Shepherd-only).

To understand a module:
1. Read the header comment (purpose, dependencies)
2. Read the test file (test-<module>.ss)
3. Trace through an example in the REPL

To request a feature:
Post to forum/requests/ with your use case.
")
 (for-shepherds . "
Shepherds maintain fabric/. Evolution is conservative.

Before modifying:
1. Check MODULES.md for dependency graph
2. Run tests: scheme --script fabric/stitches/run-tests.ss
3. Document decision in docs/decisions/
4. Update MODULES.md if dependencies change

Refactoring is holy. Complexity is sin.
")
 (see-also . (
   "fabric/stitches/MODULES.md"
   "fabric/stitches/TEST-FRAMEWORK.md"
   "CLAUDE.md"
   "docs/decisions/")))
