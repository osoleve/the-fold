; PRELUDE-ALPHA.sexp
; Comprehensive Prelude Refactoring Plan v4.0
; Goal: Transform prelude from monolithic chaos to clean semantic taxonomy
; Status: Planning (revised structure with Python-style module pattern)
; Target: Zero-line missing.ss, per-module exports, no file >500 lines

((metadata
  (created . "2025-12-30")
  (updated . "2025-12-30T23:30:00Z")
  (version . "4.0")
  (revision-note . "Python-style stub files replace X/X-ext pattern")
  (goal . "Complete prelude refactoring with clean semantic taxonomy")
  (critical-promises
    ("missing.ss must be completely eliminated")
    ("exports.ss replaced with per-module exports")
    ("No .ss file may exceed 500 lines (soft) / 800 lines (hard)")
    ("Stub files import submodules, users get flat namespace"))))

(current-state
  ((total-prelude-lines . 19773)
   (files-exceeding-500-lines . 4)
   (missing-ss-lines . 13054)     ; 66% of entire prelude
   (exports-ss-lines . 2161)      ; Hand-maintained export list
   (list-ss-lines . 455)
   (numeric-ss-lines . 478)
   (shadow-conflicts . 800+)
   (test-passing . "430/512 (84%)")
   (test-failing . 82)))

(design-principles
  ((python-style-modules
    (description . "Stub files load and re-export from semantic submodules")
    (example . "list.ss loads list-core.ss, list-search.ss, list-partition.ss")
    (benefit . "Users get flat namespace, maintainers get clear organization"))

   (semantic-naming
    (description . "Submodule names describe content, not 'extended'")
    (bad . "list.ss, list-ext.ss")
    (good . "list.ss → list-core.ss, list-search.ss, list-partition.ss")
    (benefit . "Self-documenting file names"))

   (size-discipline
    (description . "Small, focused files over monolithic modules")
    (soft-limit . 500)
    (hard-limit . 800)
    (typical . "150-350 lines per submodule")
    (benefit . "Easy to navigate, review, and maintain"))

   (per-module-exports
    (description . "Each submodule declares its own exports")
    (pattern . "(module-exports fn1 fn2 fn3 ...)")
    (assembly . "Rust concatenates all module-exports into final alist")
    (benefit . "No separate exports.ss, single source of truth"))))

; ============================================================
; TARGET TAXONOMY
; ============================================================

(target-taxonomy

  ; ------------------------------------------------------------
  ; TIER 1: FOUNDATION
  ; Pure, dependency-free primitives
  ; ------------------------------------------------------------
  (foundation
    (core.ss
      (type . standalone)
      (size . 78)
      (purpose . "Primitive HOFs: map, filter, foldl, foldr")
      (dependencies . none)
      (status . keep))

    (function.ss
      (type . standalone)
      (size . 110)
      (purpose . "Function combinators: compose, curry, pipe, flip, memoize")
      (dependencies . (core.ss))
      (status . keep))

    (equality.ss
      (type . standalone)
      (size . 44)
      (purpose . "Equality predicates: eqv?, equal?, type checks")
      (dependencies . none)
      (status . keep))

    (predicate.ss
      (type . standalone)
      (size-estimate . "100-140")
      (purpose . "Predicate combinators: negate, conjoin, disjoin, all-of, any-of")
      (dependencies . (function.ss))
      (status . new)
      (source . "Extract from missing.ss")))

  ; ------------------------------------------------------------
  ; TIER 2: COLLECTIONS
  ; List, dict, set, bag operations
  ; ------------------------------------------------------------
  (collections

    ; --- LIST MODULE (stub + submodules) ---
    (list.ss
      (type . stub)
      (purpose . "List operations - loads and re-exports submodules")
      (loads . (list-core.ss list-search.ss list-transform.ss list-partition.ss))
      (status . refactor))

    (list-core.ss
      (type . submodule)
      (parent . list.ss)
      (size-estimate . "180-220")
      (purpose . "Core list ops: take, drop, append, reverse, cons variants")
      (functions . (take-n drop-n take-right drop-right take-while drop-while
                    butlast snoc cons* append reverse length null?
                    car cdr cadr caddr first second third rest last))
      (status . new)
      (source . "Split from list.ss lines 1-150"))

    (list-search.ss
      (type . submodule)
      (parent . list.ss)
      (size-estimate . "120-160")
      (purpose . "Search and find: find-if, index-of, member, contains, any, all")
      (functions . (find-if find index-of index-where member memq memv
                    contains? any? all? none? count count-if
                    before after adjacent-pairs))
      (status . new)
      (source . "Split from list.ss + missing.ss"))

    (list-transform.ss
      (type . submodule)
      (parent . list.ss)
      (size-estimate . "150-200")
      (purpose . "Transformations: map variants, zip, unzip, transpose, flatten")
      (functions . (map filter flat-map concat zip zip-with unzip
                    transpose flatten intersperse interleave
                    rotate-left rotate-right shuffle))
      (status . new)
      (source . "Split from list.ss + list-ext.ss + missing.ss"))

    (list-partition.ss
      (type . submodule)
      (parent . list.ss)
      (size-estimate . "150-180")
      (purpose . "Partitioning: partition, group-by, split, chunk, span")
      (functions . (partition partition-eithers group-by split-at split-when
                    chunks chunk-by span break take-until drop-until
                    separate unique unique-by dedupe))
      (status . new)
      (source . "Split from list.ss + missing.ss partitioning.ss proposal"))

    ; --- COLLECTION MODULE (stub + submodules) ---
    (collection.ss
      (type . stub)
      (purpose . "Collection types - loads and re-exports submodules")
      (loads . (collection-alist.ss collection-dict.ss collection-set.ss collection-bag.ss))
      (status . refactor))

    (collection-alist.ss
      (type . submodule)
      (parent . collection.ss)
      (size-estimate . "100-130")
      (purpose . "Association lists: assoc, alist-ref, alist-set, alist-update")
      (functions . (assoc assq assv alist-ref alist-set alist-update
                    alist-delete alist-keys alist-values alist->dict))
      (status . new)
      (source . "Split from collection.ss + alist-ext.ss"))

    (collection-dict.ss
      (type . submodule)
      (parent . collection.ss)
      (size-estimate . "120-150")
      (purpose . "Dictionaries/hash tables: dict-ref, dict-set, dict-update, dict-merge")
      (functions . (make-dict dict-ref dict-set dict-update dict-delete
                    dict-keys dict-values dict-items dict-has-key?
                    dict-merge dict-map dict-filter dict->alist))
      (status . new)
      (source . "Split from collection.ss"))

    (collection-set.ss
      (type . submodule)
      (parent . collection.ss)
      (size-estimate . "100-130")
      (purpose . "Sets: set-union, set-intersection, set-difference, set-member?")
      (functions . (make-set set-add set-remove set-member? set-size
                    set-union set-intersection set-difference set-symmetric-difference
                    set-subset? set-equal? set->list list->set))
      (status . new)
      (source . "Split from collection.ss + set-ext.ss"))

    (collection-bag.ss
      (type . submodule)
      (parent . collection.ss)
      (size-estimate . "100-130")
      (purpose . "Bags/multisets: frequencies, bag-union, run-length-encode")
      (functions . (frequencies bag-add bag-remove bag-count
                    bag-union bag-intersection bag-difference
                    run-length-encode run-length-decode mode modes))
      (status . new)
      (source . "Extract from missing.ss bags.ss proposal"))

    ; --- STREAM (standalone, small) ---
    (stream.ss
      (type . standalone)
      (size . 74)
      (purpose . "Lazy streams: stream-cons, stream-take, stream-map")
      (dependencies . (list-core.ss))
      (status . keep)))

  ; ------------------------------------------------------------
  ; TIER 3: NUMERIC
  ; Arithmetic, statistics, sequences
  ; ------------------------------------------------------------
  (numeric

    ; --- NUMERIC MODULE (stub + submodules) ---
    (numeric.ss
      (type . stub)
      (purpose . "Numeric operations - loads and re-exports submodules")
      (loads . (numeric-core.ss numeric-stats.ss numeric-sequence.ss))
      (status . refactor))

    (numeric-core.ss
      (type . submodule)
      (parent . numeric.ss)
      (size-estimate . "200-250")
      (purpose . "Core arithmetic: +, -, *, /, mod, gcd, lcm, abs, sign, clamp")
      (functions . (add sub mul div mod quotient remainder
                    gcd lcm abs sign sgn floor ceiling round truncate
                    min max clamp between? positive? negative? zero?
                    even? odd? exact? inexact? integer? number?))
      (status . new)
      (source . "Split from numeric.ss"))

    (numeric-stats.ss
      (type . submodule)
      (parent . numeric.ss)
      (size-estimate . "180-220")
      (purpose . "Statistics: mean, median, variance, stddev, percentile, correlation")
      (functions . (sum product mean average median mode
                    variance stddev standard-deviation
                    percentile quartiles iqr range
                    covariance correlation z-score normalize
                    histogram bin))
      (status . new)
      (source . "Merge stats.ss + numeric.ss stats + missing.ss stats"))

    (numeric-sequence.ss
      (type . submodule)
      (parent . numeric.ss)
      (size-estimate . "150-180")
      (purpose . "Numeric sequences: range, naturals, fibonacci, iterate, powers-of")
      (functions . (range iota naturals integers-from
                    fibonacci-seq triangular-numbers
                    powers-of iterate iterate-n iterate-until
                    geometric-series arithmetic-series
                    primes prime? factors factorize))
      (status . new)
      (source . "Extract from missing.ss sequences.ss proposal + numeric.ss")))

  ; ------------------------------------------------------------
  ; TIER 4: STRING
  ; String manipulation and formatting
  ; ------------------------------------------------------------
  (string-ops

    ; --- STRING MODULE (stub + submodules) ---
    (string.ss
      (type . stub)
      (purpose . "String operations - loads and re-exports submodules")
      (loads . (string-core.ss string-format.ss string-search.ss))
      (status . refactor))

    (string-core.ss
      (type . submodule)
      (parent . string.ss)
      (size-estimate . "150-180")
      (purpose . "Core string ops: concat, substring, length, char-at, upcase, downcase")
      (functions . (string-append string-concat string-length substring
                    string-ref string->list list->string
                    string-upcase string-downcase string-titlecase
                    string-trim string-trim-left string-trim-right
                    string-reverse string-copy))
      (status . new)
      (source . "Split from string.ss"))

    (string-format.ss
      (type . submodule)
      (parent . string.ss)
      (size-estimate . "120-150")
      (purpose . "Formatting: pad, justify, pluralize, humanize, format-number")
      (functions . (pad-left pad-right center
                    left-justify right-justify
                    format-number number->string-padded
                    pluralize singularize
                    humanize-list oxford-comma
                    repeat-string))
      (status . new)
      (source . "Extract from missing.ss formatting.ss proposal + string-ext.ss"))

    (string-search.ss
      (type . submodule)
      (parent . string.ss)
      (size-estimate . "130-160")
      (purpose . "Search and split: split, find, replace, starts-with, ends-with")
      (functions . (string-split string-join
                    string-find string-contains?
                    string-starts-with? string-ends-with?
                    string-replace string-replace-all
                    string-index string-index-right
                    string-prefix? string-suffix?))
      (status . new)
      (source . "Split from string.ss + string-ext.ss")))

  ; ------------------------------------------------------------
  ; TIER 5: COMPARISON & ORDERING
  ; Sorting, searching, intervals
  ; ------------------------------------------------------------
  (ordering

    (comparison.ss
      (type . standalone)
      (size . 266)
      (purpose . "Sorting and searching: sort, sort-by, binary-search, min-by, max-by")
      (dependencies . (list-core.ss function.ss))
      (status . keep))

    (interval.ss
      (type . standalone)
      (size-estimate . "150-200")
      (purpose . "Interval arithmetic: interval-contains, overlaps, union, intersection")
      (functions . (make-interval interval? interval-start interval-end
                    interval-contains? interval-width interval-midpoint
                    interval-overlaps? interval-adjacent?
                    interval-union interval-intersection interval-difference
                    intervals-merge intervals-gaps))
      (status . new)
      (source . "Extract from missing.ss intervals.ss proposal")))

  ; ------------------------------------------------------------
  ; TIER 6: ALGEBRAIC TYPES
  ; Maybe, Either, Monads
  ; ------------------------------------------------------------
  (algebraic

    (maybe.ss
      (type . standalone)
      (size . 87)
      (purpose . "Maybe monad: just, nothing, maybe-map, maybe-bind")
      (dependencies . (function.ss))
      (status . keep))

    (either.ss
      (type . standalone)
      (size . 120)
      (purpose . "Either monad: left, right, either-map, either-bind")
      (dependencies . (function.ss))
      (status . keep))

    ; --- MONAD MODULE (stub + submodules) ---
    (monad.ss
      (type . stub)
      (purpose . "Monad operations - loads and re-exports submodules")
      (loads . (monad-core.ss monad-state.ss monad-transformers.ss))
      (status . refactor))

    (monad-core.ss
      (type . submodule)
      (parent . monad.ss)
      (size . 100)
      (purpose . "Core monad interface: return, bind, join, sequence, mapM")
      (status . existing)
      (note . "Keep as-is, already well-sized"))

    (monad-state.ss
      (type . submodule)
      (parent . monad.ss)
      (size-estimate . "120-150")
      (purpose . "State monad: get, put, modify, exec-state, eval-state, run-state")
      (functions . (state-return state-bind state-get state-put state-modify
                    run-state eval-state exec-state
                    state-map state-sequence state-local))
      (status . new)
      (source . "Expand from monad-core.ss + missing.ss state-monad.ss proposal"))

    (monad-transformers.ss
      (type . submodule)
      (parent . monad.ss)
      (size-estimate . "100-130")
      (purpose . "Monad transformers: StateT, ReaderT, lift")
      (functions . (state-t reader-t writer-t
                    lift hoist
                    run-state-t run-reader-t run-writer-t))
      (status . new)
      (source . "Extract from missing.ss")))

  ; ------------------------------------------------------------
  ; TIER 7: FUNCTIONAL PATTERNS
  ; Lenses, arrows, combinators
  ; ------------------------------------------------------------
  (patterns

    (lens.ss
      (type . standalone)
      (size-estimate . "150-180")
      (purpose . "Functional lenses: lens-get, lens-set, lens-over, lens-compose")
      (functions . (make-lens lens-get lens-set lens-over lens-update
                    lens-compose lens-identity
                    lens-first lens-second lens-nth
                    lens-at lens-key lens-path
                    view set over))
      (dependencies . (function.ss))
      (status . new)
      (source . "Extract from missing.ss lenses.ss proposal"))

    (arrow.ss
      (type . standalone)
      (size-estimate . "100-130")
      (purpose . "Arrow combinators: arr, >>>, &&&, ***, first, second")
      (functions . (arr arrow-compose arrow-pipe
                    arrow-first arrow-second
                    arrow-split arrow-fanout
                    arrow-loop arrow-left arrow-right))
      (dependencies . (function.ss))
      (status . new)
      (source . "Extract from missing.ss arrows.ss proposal"))

    (combinatorics.ss
      (type . standalone)
      (size . 70)
      (purpose . "Permutations, combinations, rotations")
      (dependencies . (list-core.ss numeric-core.ss))
      (status . keep)))

  ; ------------------------------------------------------------
  ; TIER 8: DATA STRUCTURES
  ; Trees, graphs, queues
  ; ------------------------------------------------------------
  (structures

    (datastructures.ss
      (type . standalone)
      (size . 127)
      (purpose . "Queue, stack, deque, priority queue")
      (dependencies . (list-core.ss))
      (status . keep))

    (tree.ss
      (type . standalone)
      (size . 56)
      (purpose . "Tree traversal: pre-order, post-order, in-order, level-order")
      (dependencies . (list-core.ss))
      (status . keep))

    ; --- GRAPH MODULE (stub + submodules) ---
    (graph.ss
      (type . stub)
      (purpose . "Graph operations - loads and re-exports submodules")
      (loads . (graph-core.ss graph-traversal.ss graph-algorithm.ss))
      (status . refactor))

    (graph-core.ss
      (type . submodule)
      (parent . graph.ss)
      (size-estimate . "100-130")
      (purpose . "Graph construction: make-graph, add-edge, remove-edge, neighbors")
      (functions . (make-graph graph-add-vertex graph-add-edge graph-remove-edge
                    graph-neighbors graph-vertices graph-edges
                    graph-has-vertex? graph-has-edge?
                    graph-in-degree graph-out-degree))
      (status . new)
      (source . "Split from graph.ss"))

    (graph-traversal.ss
      (type . submodule)
      (parent . graph.ss)
      (size-estimate . "100-130")
      (purpose . "Graph traversal: BFS, DFS, paths, reachability")
      (functions . (graph-bfs graph-dfs graph-path
                    graph-reachable? graph-connected?
                    graph-shortest-path graph-all-paths
                    graph-traverse graph-fold))
      (status . new)
      (source . "Split from graph.ss + graph-ext.ss"))

    (graph-algorithm.ss
      (type . submodule)
      (parent . graph.ss)
      (size-estimate . "100-130")
      (purpose . "Graph algorithms: topo-sort, cycles, components, spanning tree")
      (functions . (graph-topological-sort graph-has-cycle? graph-find-cycle
                    graph-connected-components graph-strongly-connected
                    graph-spanning-tree graph-minimum-spanning-tree
                    graph-transpose graph-reverse))
      (status . new)
      (source . "Split from graph-ext.ss + missing.ss")))

  ; ------------------------------------------------------------
  ; TIER 9: VALIDATION & TESTING
  ; Assertions, test framework
  ; ------------------------------------------------------------
  (validation-tier

    ; --- VALIDATION MODULE (stub + submodules) ---
    (validation.ss
      (type . stub)
      (purpose . "Validation and testing - loads and re-exports submodules")
      (loads . (validation-core.ss validation-assert.ss))
      (status . refactor))

    (validation-core.ss
      (type . submodule)
      (parent . validation.ss)
      (size-estimate . "120-150")
      (purpose . "Core validation: validate, check, ensure, guard")
      (functions . (validate check ensure guard
                    validate-all validate-any
                    validate-result validation-errors
                    predicate->validator compose-validators))
      (status . new)
      (source . "Split from validation.ss + missing.ss validation-extended.ss proposal"))

    (validation-assert.ss
      (type . submodule)
      (parent . validation.ss)
      (size-estimate . "100-130")
      (purpose . "Test assertions: assert-eq, assert-true, test-case, test-suite")
      (functions . (assert assert-eq assert-equal assert-true assert-false
                    assert-null assert-not-null assert-throws
                    test-case test-suite run-tests
                    expect expect-eq expect-true))
      (status . new)
      (source . "Split from validation.ss"))

    (control.ss
      (type . standalone)
      (size . 279)
      (purpose . "Control flow: when, unless, cond, case, and-let, if-let")
      (dependencies . (core.ss))
      (status . keep)))

  ; ------------------------------------------------------------
  ; TIER 10: SYSTEM
  ; Paths, encoding, compatibility
  ; ------------------------------------------------------------
  (system

    (path.ss
      (type . standalone)
      (size . 201)
      (purpose . "Filesystem paths: path-join, path-split, extension, basename")
      (dependencies . (string-core.ss list-core.ss))
      (status . keep))

    (encoding.ss
      (type . standalone)
      (size . 206)
      (purpose . "Encoding: base64, hex, checksums")
      (dependencies . (string-core.ss))
      (status . keep))

    (compat.ss
      (type . standalone)
      (size . 234)
      (purpose . "Scheme compatibility aliases")
      (dependencies . none)
      (status . keep))

    (logic.ss
      (type . standalone)
      (size-estimate . "50-80")
      (purpose . "Extended logic: xor, implies, iff, boolean combinators")
      (dependencies . none)
      (status . new)
      (source . "Rename from logic-ext.ss, possibly expand")))

  ; ------------------------------------------------------------
  ; TIER 11: MISCELLANEOUS
  ; Remaining utilities
  ; ------------------------------------------------------------
  (miscellaneous

    (misc.ss
      (type . standalone)
      (size-estimate . "200-300")
      (purpose . "Remaining utilities that don't fit elsewhere")
      (note . "Catchall - should be as small as possible")
      (status . new)
      (source . "Remainder from missing.ss after all extractions")))

  ; ------------------------------------------------------------
  ; ELIMINATED FILES
  ; ------------------------------------------------------------
  (eliminated
    (missing.ss
      (action . delete)
      (reason . "All functions distributed to proper semantic homes")
      (current-size . 13054))

    (exports.ss
      (action . delete)
      (reason . "Replaced with per-module exports")
      (current-size . 2161))

    (list-ext.ss
      (action . merge)
      (into . "list-transform.ss, list-partition.ss")
      (current-size . 120))

    (numeric-ext.ss
      (action . merge)
      (into . "numeric-core.ss, numeric-stats.ss")
      (current-size . 107))

    (string-ext.ss
      (action . merge)
      (into . "string-format.ss, string-search.ss")
      (current-size . 87))

    (graph-ext.ss
      (action . merge)
      (into . "graph-traversal.ss, graph-algorithm.ss")
      (current-size . 74))

    (alist-ext.ss
      (action . merge)
      (into . "collection-alist.ss")
      (current-size . 43))

    (set-ext.ss
      (action . merge)
      (into . "collection-set.ss")
      (current-size . 59))

    (stats.ss
      (action . merge)
      (into . "numeric-stats.ss")
      (current-size . 107))

    (logic-ext.ss
      (action . rename)
      (to . "logic.ss")
      (current-size . 34))))

; ============================================================
; STUB FILE PATTERN
; ============================================================

(stub-file-pattern
  (description . "Each stub file loads submodules and optionally adds glue")

  (example-list-ss
    ("; list.ss - List operations"
     "; Loads and re-exports all list submodules"
     ""
     "; Load submodules in dependency order"
     "(load \"list-core.ss\")"
     "(load \"list-search.ss\")"
     "(load \"list-transform.ss\")"
     "(load \"list-partition.ss\")"
     ""
     "; Stub files are ~10-20 lines"))

  (example-submodule
    ("; list-partition.ss - Partitioning operations"
     "; Part of list.ss module"
     ""
     "; partition: Split list into (matches, non-matches)"
     "(partition (fix partition ...))"
     ""
     "; ... more functions ..."
     ""
     "; --- Module Exports ---"
     "(module-exports"
     "  partition partition-eithers group-by"
     "  split-at split-when chunks span break)"))

  (rust-assembly
    (description . "mod.rs concatenates all module-exports forms")
    (approach . "Scan each .ss file for (module-exports ...) form")
    (result . "Single exports alist generated at assembly time")))

; ============================================================
; LOAD ORDER
; ============================================================

(target-load-order
  (note . "Dependency-ordered prelude assembly")

  (phase-1-foundation
    (1 . core.ss)
    (2 . function.ss)
    (3 . equality.ss)
    (4 . predicate.ss))

  (phase-2-collections
    (5 . list.ss)           ; stub loads list-*.ss
    (6 . collection.ss)     ; stub loads collection-*.ss
    (7 . stream.ss))

  (phase-3-numeric
    (8 . numeric.ss))       ; stub loads numeric-*.ss

  (phase-4-string
    (9 . string.ss))        ; stub loads string-*.ss

  (phase-5-ordering
    (10 . comparison.ss)
    (11 . interval.ss))

  (phase-6-algebraic
    (12 . maybe.ss)
    (13 . either.ss)
    (14 . monad.ss))        ; stub loads monad-*.ss

  (phase-7-patterns
    (15 . lens.ss)
    (16 . arrow.ss)
    (17 . combinatorics.ss))

  (phase-8-structures
    (18 . datastructures.ss)
    (19 . tree.ss)
    (20 . graph.ss))        ; stub loads graph-*.ss

  (phase-9-validation
    (21 . validation.ss)    ; stub loads validation-*.ss
    (22 . control.ss))

  (phase-10-system
    (23 . path.ss)
    (24 . encoding.ss)
    (25 . compat.ss)
    (26 . logic.ss))

  (phase-11-misc
    (27 . misc.ss)))

; ============================================================
; EXECUTION PLAN
; ============================================================

(execution-plan
  (approach . "Incremental extraction with validation at each step")

  (step-1-create-stubs
    (task . "Create stub files for list, collection, numeric, string, graph, monad, validation")
    (outputs . ("list.ss" "collection.ss" "numeric.ss" "string.ss"
                "graph.ss" "monad.ss" "validation.ss"))
    (pattern . "Each stub just loads its submodules")
    (validation . "Prelude still assembles and tests pass"))

  (step-2-split-list
    (task . "Split current list.ss (455 lines) into submodules")
    (outputs . ("list-core.ss" "list-search.ss" "list-transform.ss" "list-partition.ss"))
    (merge . "list-ext.ss content goes to list-transform.ss and list-partition.ss")
    (validation . "All list-related tests pass"))

  (step-3-split-collection
    (task . "Split current collection.ss (364 lines) into submodules")
    (outputs . ("collection-alist.ss" "collection-dict.ss" "collection-set.ss"))
    (merge . ("alist-ext.ss → collection-alist.ss"
              "set-ext.ss → collection-set.ss"))
    (new . "collection-bag.ss from missing.ss")
    (validation . "All collection tests pass"))

  (step-4-split-numeric
    (task . "Split current numeric.ss (478 lines) into submodules")
    (outputs . ("numeric-core.ss" "numeric-stats.ss" "numeric-sequence.ss"))
    (merge . ("numeric-ext.ss → numeric-core.ss"
              "stats.ss → numeric-stats.ss"))
    (new . "sequences from missing.ss → numeric-sequence.ss")
    (validation . "All numeric tests pass"))

  (step-5-split-string
    (task . "Split current string.ss (268 lines) into submodules")
    (outputs . ("string-core.ss" "string-format.ss" "string-search.ss"))
    (merge . "string-ext.ss distributed across submodules")
    (new . "formatting from missing.ss → string-format.ss")
    (validation . "All string tests pass"))

  (step-6-split-graph
    (task . "Split graph.ss into submodules")
    (outputs . ("graph-core.ss" "graph-traversal.ss" "graph-algorithm.ss"))
    (merge . "graph-ext.ss distributed across submodules")
    (validation . "All graph tests pass"))

  (step-7-expand-monad
    (task . "Expand monad module with state operations")
    (outputs . ("monad-state.ss" "monad-transformers.ss"))
    (keep . "monad-core.ss as-is")
    (source . "Extract from missing.ss")
    (validation . "All monad tests pass"))

  (step-8-split-validation
    (task . "Split validation.ss into submodules")
    (outputs . ("validation-core.ss" "validation-assert.ss"))
    (validation . "All validation tests pass"))

  (step-9-new-modules
    (task . "Create new standalone modules from missing.ss")
    (outputs . ("predicate.ss" "interval.ss" "lens.ss" "arrow.ss" "logic.ss" "misc.ss"))
    (validation . "New tests for new modules pass"))

  (step-10-extract-missing
    (task . "Extract all remaining functions from missing.ss to proper homes")
    (process . ("For each function in missing.ss:"
                "1. Identify correct semantic home"
                "2. Check for duplicates in target module"
                "3. Move canonical version to target"
                "4. Delete from missing.ss"))
    (validation . "missing.ss reduced to 0 lines"))

  (step-11-per-module-exports
    (task . "Add (module-exports ...) to each module")
    (pattern . "Each file ends with exports list")
    (rust-change . "Modify mod.rs to scan and concatenate exports")
    (validation . "All 570+ functions still accessible"))

  (step-12-cleanup
    (task . "Delete eliminated files, verify constraints")
    (delete . ("missing.ss" "exports.ss" "list-ext.ss" "numeric-ext.ss"
               "string-ext.ss" "graph-ext.ss" "alist-ext.ss" "set-ext.ss"
               "stats.ss" "logic-ext.ss"))
    (verify . ("No file > 500 lines (soft) / 800 lines (hard)"
               "wc -l *.ss | awk '$1 > 500' shows at most misc.ss"
               "All tests pass"
               "No shadowing conflicts"))))

; ============================================================
; SUCCESS CRITERIA
; ============================================================

(success-criteria
  (files-eliminated
    (missing.ss . deleted)
    (exports.ss . deleted)
    (all-ext-files . "merged into semantic submodules"))

  (size-constraints
    (hard-limit . 800)
    (soft-limit . 500)
    (typical-submodule . "150-300 lines")
    (verification . "wc -l *.ss | awk '$1 > 800' returns empty"))

  (quality-gates
    ("Each stub file < 30 lines")
    ("Each submodule has clear semantic purpose")
    ("No duplicate function definitions")
    ("All functions have single canonical home")
    ("Per-module exports match defined functions")
    ("Test pass rate >= 90% (450+/512)"))

  (final-file-count
    (stub-files . 7)
    (submodules . 22)
    (standalone . 15)
    (total . 44)
    (vs-current . "More files, but each is small and focused")))

; ============================================================
; FINAL STATE VISION
; ============================================================

(final-state
  (structure
    ("; Stub files (load submodules)"
     "list.ss → list-core.ss, list-search.ss, list-transform.ss, list-partition.ss"
     "collection.ss → collection-alist.ss, collection-dict.ss, collection-set.ss, collection-bag.ss"
     "numeric.ss → numeric-core.ss, numeric-stats.ss, numeric-sequence.ss"
     "string.ss → string-core.ss, string-format.ss, string-search.ss"
     "graph.ss → graph-core.ss, graph-traversal.ss, graph-algorithm.ss"
     "monad.ss → monad-core.ss, monad-state.ss, monad-transformers.ss"
     "validation.ss → validation-core.ss, validation-assert.ss"
     ""
     "; Standalone files"
     "core.ss, function.ss, equality.ss, predicate.ss"
     "stream.ss, comparison.ss, interval.ss"
     "maybe.ss, either.ss"
     "lens.ss, arrow.ss, combinatorics.ss"
     "datastructures.ss, tree.ss"
     "control.ss, path.ss, encoding.ss, compat.ss, logic.ss"
     "misc.ss"))

  (metrics
    (total-files . 44)
    (max-file-size . "~500 lines")
    (total-lines . "~7000 (vs 19773 current)")
    (shadowing-conflicts . 0)
    (test-pass-rate . "~90%+"))))
