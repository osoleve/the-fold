; PRELUDE-ALPHA.sexp
; Comprehensive Prelude Refactoring Plan v2.0
; Goal: Transform prelude from monolithic chaos to clean semantic taxonomy
; Status: In Progress (missing.ss still exists, needs complete elimination)
; Target: Zero-line missing.ss, all functions properly categorized

((metadata
  (created . "2025-12-30")
  (version . "2.0")
  (goal . "Complete prelude refactoring to eliminate missing.ss")
  (critical-promise . "missing.ss must be completely eliminated, not just reduced")))

(current-state
  ((total-prelude-lines . 18500)  ; All .ss files combined
   (missing-ss-lines . 13054)     ; 71% of entire prelude!
   (shadow-conflicts . 800+)      ; Functions defined 3-5+ times
   (test-passing . "430/512 (84%)")
   (test-failing . 82)))

(problem-statement
  ((issue-1
    (title . "Monolithic missing.ss")
    (description . "13,054 lines of chaos with zero semantic organization")
    (impact . "Impossible to maintain, debug, or extend")
    (examples
      ("rotate-left defined 6 times with different signatures")
      ("variance defined 6 times, each losing to later shadowing")
      ("state-monad defined 3 times in different formats")
      ("Similar sections repeated: 'More Numeric Utilities' appears 3x"))))

  ((issue-2
    (title . "Shadowing Cascades")
    (description . "Functions defined multiple times, later versions win")
    (impact . "Users get wrong arities, wrong algorithms, wrong semantics")
    (resolution . "Must break into separate modules, load in dependency order")))

  ((issue-3
    (title . "Test Failures")
    (description . "82 tests failing, many due to wrong function versions")
    (impact . "Cannot reach >90% pass rate without fixing shadowing")
    (evidence . "test_group_by returns wrong structure, test_all_equal has type errors"))))

(current-module-taxonomy
  (note . "Existing modules (organized by semantic domain)")

  (foundation
    (core.ss . 78)         ; map, filter, foldl, foldr
    (function.ss . 110)    ; Higher-order function combinators
    (equality.ss . 44)     ; eqv?, equal?, predicates)

  (collection-operations
    (list.ss . 455)        ; Comprehensive list manipulation
    (list-ext.ss . 120)    ; Extended list operations
    (collection.ss . 364)  ; Dicts, sets, alists
    (alist-ext.ss . 43)    ; Extended alist operations
    (set-ext.ss . 59)      ; Extended set operations
    (stream.ss . 74)       ; Lazy stream operations

  (numeric-operations
    (numeric.ss . 478)     ; Basic arithmetic, statistics
    (numeric-ext.ss . 107) ; Extended numeric utilities
    (stats.ss . 107)       ; Deduped statistics functions

  (string-operations
    (string.ss . 268)      ; Basic string manipulation
    (string-ext.ss . 87)   ; Extended string utilities

  (comparison-and-ordering
    (comparison.ss . 266)  ; Sorting, searching, selection

  (monadic-abstractions
    (maybe.ss . 87)        ; Maybe monad
    (either.ss . 120)      ; Either monad
    (monad-core.ss . 100)  ; State, Reader, Writer monads

  (validation-and-testing
    (validation.ss . 227)  ; Test framework, assertions

  (structural-data
    (datastructures.ss . 127) ; Queue, stack, deque, priority queue
    (tree.ss . 56)         ; Tree traversal and manipulation
    (graph.ss . 83)        ; Basic graph algorithms
    (graph-ext.ss . 74)    ; Extended graph algorithms

  (combinatorial-operations
    (combinatorics.ss . 70) ; Permutations, combinations, rotations

  (miscellaneous
    (logic-ext.ss . 34)    ; Extended logic operations
    (path.ss . 201)        ; Filesystem path manipulation
    (encoding.ss . 206)    ; Base64, hex, checksums
    (compat.ss . 234)      ; Scheme compatibility aliases
    (exports.ss . 2161)    ; Final export alist

  (chaos-zone
    (missing.ss . 13054)   ; ⚠️ ELIMINATION TARGET - contains 1000+ functions
                           ; scattered across 15 semantic domains))

(functions-in-missing-ss-analysis
  (note . "Mapping what's in missing.ss to proper homes")

  (category-intervals
    (count . ~200)
    (proper-home . "intervals.ss (NEW)")
    (functions . (interval-contains interval-width interval-overlaps
                  interval-union interval-intersection interval-midpoint))
    (status . "NEW MODULE NEEDED"))

  (category-lenses
    (count . ~150)
    (proper-home . "lenses.ss (NEW)")
    (functions . (lens-get lens-set lens-update lens-compose))
    (status . "NEW MODULE NEEDED"))

  (category-arrows
    (count . ~100)
    (proper-home . "arrows.ss (NEW)")
    (functions . (arrow-first arrow-second arrow-split arrow-fanout))
    (status . "NEW MODULE NEEDED"))

  (category-sequences
    (count . ~180)
    (proper-home . "sequences.ss (NEW)")
    (functions . (naturals powers-of iterate-n iterate-until
                  triangular-numbers fibonacci-seq))
    (status . "NEW MODULE NEEDED"))

  (category-predicates
    (count . ~120)
    (proper-home . "predicates.ss (NEW)")
    (functions . (negate-predicate conjoin disjoin all-of any-of))
    (status . "NEW MODULE NEEDED"))

  (category-formatting
    (count . ~90)
    (proper-home . "formatting.ss (NEW)")
    (functions . (format-number format-string pad-left pad-right
                  pluralize humanize-list))
    (status . "NEW MODULE NEEDED"))

  (category-math-extended
    (count . ~150)
    (proper-home . "math-advanced.ss (NEW)")
    (functions . (matrix operations, linear algebra, number theory))
    (status . "NEW MODULE NEEDED"))

  (category-bag-and-frequency
    (count . ~100)
    (proper-home . "bags.ss (NEW)")
    (functions . (bag-union bag-intersection frequencies run-length-encode
                  run-length-decode))
    (status . "NEW MODULE NEEDED"))

  (category-partition-and-group
    (count . ~120)
    (proper-home . "partitioning.ss (NEW)")
    (functions . (group-by partition partition-eithers split-when
                  split-at chunks index-where))
    (status . "NEW MODULE NEEDED"))

  (category-state-and-effects
    (count . ~80)
    (proper-home . "state-monad.ss (expand monad-core.ss)")
    (functions . (exec-state modify get put local))
    (status . "EXPAND EXISTING"))

  (category-validation-extended
    (count . ~140)
    (proper-home . "validation-extended.ss (NEW)")
    (functions . (ensure assert-eq validate-all validate-result))
    (status . "NEW MODULE NEEDED"))

  (category-duplicate-removal-known
    (source . "Found via find-shadows.py analysis")
    (move-to-existing-modules
      (rotate-left . list-ext.ss)
      (rotate-right . list-ext.ss)
      (variance . stats.ss)
      (median . stats.ss)
      (mean . stats.ss)
      (percentile . stats.ss)
      (correlation . stats.ss)
      (graph-add-edge . graph-ext.ss)
      (graph-neighbors . graph-ext.ss)
      (graph-has-edge? . graph-ext.ss))))

(refactoring-strategy
  (phase . "COMPLETE ELIMINATION")
  (approach . "Break missing.ss into ~12-15 new domain-specific modules")

  (new-modules-to-create
    (intervals.ss
      (purpose . "Interval arithmetic and set-like operations")
      (size-estimate . "200-250 lines")
      (load-order . "After comparison.ss"))

    (lenses.ss
      (purpose . "Functional lenses for nested data access")
      (size-estimate . "180-200 lines")
      (load-order . "After function.ss"))

    (arrows.ss
      (purpose . "Arrow type class operations and combinators")
      (size-estimate . "120-140 lines")
      (load-order . "After function.ss"))

    (sequences.ss
      (purpose . "Sequence generators (naturals, fibonacci, powers)")
      (size-estimate . "200-250 lines")
      (load-order . "After numeric.ss"))

    (predicates.ss
      (purpose . "Predicate combinators and logic gates")
      (size-estimate . "140-160 lines")
      (load-order . "After core.ss"))

    (formatting.ss
      (purpose . "Number and string formatting utilities")
      (size-estimate . "120-150 lines")
      (load-order . "After string.ss"))

    (math-advanced.ss
      (purpose . "Advanced mathematics (linear algebra, matrices, etc.)")
      (size-estimate . "250-300 lines")
      (load-order . "After numeric-ext.ss"))

    (bags.ss
      (purpose . "Bag (multiset) operations and frequency analysis")
      (size-estimate . "140-170 lines")
      (load-order . "After collection.ss"))

    (partitioning.ss
      (purpose . "Group, partition, chunk, and split operations")
      (size-estimate . "180-200 lines")
      (load-order . "After list-ext.ss"))

    (state-monad.ss
      (purpose . "Expand monad-core.ss with more state operations")
      (size-estimate . "150-180 lines")
      (action . "EXPAND monad-core.ss, don't create separate file"))

    (validation-extended.ss
      (purpose . "Extended validation combinators beyond validation.ss")
      (size-estimate . "180-200 lines")
      (load-order . "After validation.ss"))

    (misc-utilities.ss
      (purpose . "Remaining miscellaneous functions from missing.ss")
      (size-estimate . "300-400 lines")
      (note . "Catchall for functions that don't fit elsewhere"))

  (total-new-lines . "~2100-2500 lines of organized code")
  (consolidation . "Reduces from 13,054 chaotic lines to clean semantic modules"))

(target-module-load-order
  (note . "Dependency-ordered prelude assembly (after refactoring)")

  (1-foundation
    (core.ss . "Primitive function combinators"))

  (2-function-tools
    (function.ss . "Higher-order function utilities")
    (predicates.ss . "Predicate combinators"))

  (3-equality
    (equality.ss . "Equality and comparison"))

  (4-collections
    (list.ss . "List operations")
    (list-ext.ss . "Extended list operations")
    (collection.ss . "Dict/set/alist operations")
    (alist-ext.ss . "Extended alist operations")
    (set-ext.ss . "Extended set operations")
    (stream.ss . "Lazy streams"))

  (5-numeric
    (numeric.ss . "Basic arithmetic")
    (numeric-ext.ss . "Extended numeric utilities")
    (stats.ss . "Statistical functions")
    (sequences.ss . "Sequence generators")
    (math-advanced.ss . "Linear algebra, advanced math"))

  (6-string
    (string.ss . "String operations")
    (string-ext.ss . "Extended string utilities")
    (formatting.ss . "String/number formatting"))

  (7-ordering
    (comparison.ss . "Sorting and searching"))

  (8-intervals
    (intervals.ss . "Interval arithmetic"))

  (9-algebraic
    (maybe.ss . "Maybe monad")
    (either.ss . "Either monad")
    (monad-core.ss . "State, Reader, Writer (expanded)"))

  (10-validation
    (validation.ss . "Test framework and assertions")
    (validation-extended.ss . "Extended validators"))

  (11-structural
    (datastructures.ss . "Queue, stack, deque, priority queue")
    (tree.ss . "Tree operations")
    (graph.ss . "Graph algorithms")
    (graph-ext.ss . "Extended graph algorithms")
    (bags.ss . "Bag/multiset operations")
    (partitioning.ss . "Group, partition, chunk operations"))

  (12-functional-patterns
    (lenses.ss . "Functional lenses")
    (arrows.ss . "Arrow combinators")
    (logic-ext.ss . "Extended logic operations")
    (combinatorics.ss . "Permutations, combinations"))

  (13-system
    (path.ss . "Filesystem paths")
    (encoding.ss . "Base64, hex, checksums")
    (compat.ss . "Scheme compatibility"))

  (14-miscellaneous
    (misc-utilities.ss . "Remaining utilities"))

  (15-export
    (exports.ss . "Final export alist")))

(execution-plan
  (approach . "Incremental extraction with validation at each step")

  (step-1-analysis
    (duration . "2-3 hours")
    (task . "Deep analysis of missing.ss to categorize every function")
    (inputs . ("missing.ss", "find-shadows.py output"))
    (outputs . ("function-inventory.sexp", "shadowing-report.txt"))
    (criteria-for-success
      ("Every function in missing.ss has a categorization")
      ("Identified overlaps with existing modules")
      ("Priorities assigned (fix high-shadowing first)")))

  (step-2-creation
    (duration . "4-6 hours")
    (task . "Create new module files with deduped canonical versions")
    (order . ("intervals.ss" "sequences.ss" "bags.ss" "partitioning.ss"
              "lenses.ss" "arrows.ss" "formatting.ss"
              "math-advanced.ss" "predicates.ss"
              "validation-extended.ss" "misc-utilities.ss"))
    (criteria-for-success
      ("Each new module < 300 lines")
      ("Clear semantic boundaries")
      ("No function defined in multiple files")))

  (step-3-integration
    (duration . "2-3 hours")
    (task . "Update mod.rs to load new modules in dependency order")
    (steps
      ("Add const declarations for new modules")
      ("Update assemble_prelude() to include new modules")
      ("Verify compilation succeeds")
      ("Run full test suite")))

  (step-4-removal
    (duration . "2-3 hours")
    (task . "Remove migrated sections from missing.ss")
    (steps
      ("Comment out each section as it's migrated")
      ("Run tests after each removal")
      ("Delete commented sections once verified")
      ("Repeat until missing.ss is empty")))

  (step-5-cleanup
    (duration . "1-2 hours")
    (task . "Final verification and documentation")
    (steps
      ("Delete now-empty missing.ss file")
      ("Update mod.rs to remove MISSING constant")
      ("Run full test suite")
      ("Update EXPORTS.md documentation")))

  (total-estimated-time . "11-17 hours on beefier machine"))

(success-criteria
  (primary-goal . "missing.ss must be eliminated completely")
  (not-acceptable . ("Just shrinking it", "Moving functions but keeping file",
                    "Partial migration"))

  (must-achieve
    ((missing-ss-deleted . true)
     (all-functions-homed . true)
     (no-shadowing-in-new-structure . true)
     (tests-passing >= 450)
     (each-module < 350 lines)))

  (quality-gates
    ("Run find-shadows.py after each module: expect 0 in new code")
    ("No duplicate function definitions across any pair of files")
    ("Each module has clear semantic purpose documented in comments")
    ("Test pass rate never decreases during refactoring")
    ("Load order preserves all dependencies")))

(critical-dependencies
  (must-complete-before-deletion
    ("All functions extracted from missing.ss to proper homes")
    ("All new modules created and integrated")
    ("All tests passing or only test semantic bugs remain")))

(risk-mitigation
  (risk-1
    (description . "Missing some functions during migration")
    (mitigation . "Use systematic line-by-line review of missing.ss")
    (validation . "grep each function name in new modules to confirm"))

  (risk-2
    (description . "Shadowing conflicts created during consolidation")
    (mitigation . "Run find-shadows.py after each module created")
    (validation . "Zero conflicts in each new module"))

  (risk-3
    (description . "Test regressions during refactoring")
    (mitigation . "Run full test suite after each step")
    (validation . "Test count never decreases")))

(final-state-vision
  (prelude-structure
    (total-modules . 28)
    (max-module-size . 300-350)
    (total-prelude-lines . "~5500-6000 (vs current 18500)")
    (missing-ss . "DELETED")
    (shadowing-conflicts . 0)))

(commit-message
  ("refactor(fold-rs): Complete prelude taxonomy refactoring

- Eliminate missing.ss by distributing 13,054 lines across 11 new semantic modules
- Create: intervals, lenses, arrows, sequences, predicates, formatting,
  math-advanced, bags, partitioning, validation-extended, misc-utilities
- Update mod.rs with new dependency-ordered load sequence
- All functions now have proper semantic homes
- Deduplication eliminates shadowing conflicts
- Tests: ~450+/512 expected to pass (up from 430/512)

Target: Zero missing.ss, clean prelude taxonomy, <95% test pass rate

Generated with Ralph Loop iteration")))
