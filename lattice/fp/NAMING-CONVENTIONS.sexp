;;; core/fp/NAMING-CONVENTIONS.sexp
;;;
;;; FP Toolkit Naming Conventions
;;;
;;; This document establishes naming conventions for the functional
;;; programming toolkit to ensure consistency across modules.

((document-type . naming-conventions)
 (scope . fp-toolkit)
 (version . "1.0")
 (date . "2025-12-29")

 (conventions

  ;;; ====
  ;;; 1. Type-Specific Operations
  ;;; ====
  ;;;
  ;;; Pattern: type-operation
  ;;; Examples: maybe-bind, reader-fmap, state-run
  ;;;
  ;;; For operations that are specific to a particular type,
  ;;; prefix with the type name followed by the operation.

  (type-specific
   (pattern . "type-operation")
   (examples . (maybe-bind maybe-fmap maybe-pure
                reader-bind reader-ask reader-local
                state-bind state-get state-put
                parser-bind parser-run
                writer-bind writer-tell
                cont-bind cont-callcc
                ne-bind ne-head ne-tail
                list-traverse list-foldl))
   (rationale . "Consistent prefix groups all operations for a type,
                 enables tab-completion discovery, matches Haskell
                 qualified import conventions"))

  ;;; ====
  ;;; 2. Generic/Dictionary Operations
  ;;; ====
  ;;;
  ;;; Pattern: operation dict args...
  ;;; Examples: (fmap functor f fa), (bind monad ma f)
  ;;;
  ;;; For polymorphic operations that take a typeclass dictionary
  ;;; as the first argument.

  (generic-operations
   (pattern . "operation dict args...")
   (examples . ((fmap functor f fa)
                (bind monad ma f)
                (pure applicative a)
                (traverse traversable applicative f ta)
                (foldr foldable f z ta)))
   (rationale . "Puts the operation name first for readability,
                 dictionary parameter enables ad-hoc polymorphism"))

  ;;; ====
  ;;; 3. Typeclass Accessors
  ;;; ====
  ;;;
  ;;; Pattern: typeclass-field
  ;;; Examples: functor-fmap, monad-bind, applicative-pure
  ;;;
  ;;; For extracting methods from typeclass dictionaries.

  (typeclass-accessors
   (pattern . "typeclass-field")
   (examples . (functor-fmap
                applicative-functor applicative-pure applicative-ap
                monad-applicative monad-bind
                foldable-foldr foldable-foldl
                traversable-traverse))
   (rationale . "Distinguishes accessors from operations,
                 follows record accessor conventions"))

  ;;; ====
  ;;; 4. Constructors and Predicates
  ;;; ====
  ;;;
  ;;; Constructors: make-type or type (for simple wrappers)
  ;;; Predicates: type?
  ;;; Examples: make-functor, just, nothing, maybe?, reader?

  (constructors-predicates
   (constructor-pattern . "make-type or type")
   (predicate-pattern . "type?")
   (examples . (make-functor make-applicative make-monad
                just nothing either-left either-right
                reader state writer cont
                functor? applicative? monad? maybe? reader?))
   (rationale . "make- prefix for complex constructors,
                 bare name for simple wrappers like just/nothing,
                 ? suffix follows Scheme convention for predicates"))

  ;;; ====
  ;;; 5. Monad Transformers
  ;;; ====
  ;;;
  ;;; Pattern: type-t-operation
  ;;; Examples: maybe-t-bind, reader-t-ask, state-t-get
  ;;;
  ;;; Transformers use -t- infix to distinguish from base types.

  (transformers
   (pattern . "type-t-operation")
   (examples . (maybe-t-bind maybe-t-lift maybe-t-run
                reader-t-ask reader-t-local reader-t-lift
                state-t-get state-t-put state-t-lift
                writer-t-tell writer-t-lift))
   (rationale . "Clear distinction between Maybe and MaybeT,
                 consistent with Haskell naming"))

  ;;; ====
  ;;; 6. Arrow and Profunctor Operations
  ;;; ====
  ;;;
  ;;; Strong: strong-first, strong-second
  ;;; Choice: choice-left, choice-right
  ;;; Arrow: arrow-first, arrow-second, arrow-fanout
  ;;; Bifunctor: bf-first, bf-second, bf-bimap

  (arrow-profunctor
   (strong . (strong-first strong-second))
   (choice . (choice-left choice-right))
   (arrow . (arrow-arr arrow-compose arrow-first arrow-second
             arrow-fanout arrow-split))
   (bifunctor . (bf-first bf-second bf-bimap bimap))
   (rationale . "Each concept gets its own prefix to avoid collision
                 between Strong first and Arrow first"))

  ;;; ====
  ;;; 7. Pair/Tuple Combinators
  ;;; ====
  ;;;
  ;;; For simple pair operations (not typeclass methods):
  ;;; Pattern: pair-operation
  ;;; Examples: pair-first, pair-second, pair-swap

  (pair-combinators
   (pattern . "pair-operation")
   (examples . (pair-first pair-second pair-swap pair-dup
                pair-map-first pair-map-second pair-bimap))
   (rationale . "Avoids collision with arrow/strong/bifunctor first,
                 makes it clear these operate on pairs"))

  ;;; ====
  ;;; 8. Semigroup/Monoid Operations
  ;;; ====
  ;;;
  ;;; Generic: sg-append (semigroup), mappend (monoid), mempty
  ;;; Type-specific: type-append, type-empty
  ;;; Examples: list-append, string-append, ne-append

  (algebraic-operations
   (semigroup-generic . sg-append)
   (monoid-generic . (mappend mempty mconcat))
   (type-specific . (list-append string-append ne-append
                     dlist-append rope-append vec-append))
   (rationale . "sg-append for explicit semigroup operations,
                 mappend follows Haskell convention,
                 type-append for concrete implementations"))

  ;;; ====
  ;;; 9. Data Structure Operations
  ;;; ====
  ;;;
  ;;; Pattern: type-operation
  ;;; Examples: map-insert, set-member?, trie-lookup
  ;;;
  ;;; Note: 'map' refers to the data structure, not the operation.
  ;;; Use type-fmap or type-map for functor mapping.

  (data-structures
   (pattern . "type-operation")
   (map-structure . (map-empty map-insert map-lookup map-delete
                     map-keys map-values map-fold))
   (set-structure . (set-empty set-insert set-member? set-delete
                     set-union set-intersection))
   (note . "When 'map' is a data structure name, the fmap operation
            should be map-fmap to avoid confusion"))

  ;;; ====
  ;;; 10. Reserved/Discouraged Names
  ;;; ====
  ;;;
  ;;; Avoid these patterns:
  ;;; - operation-type (e.g., bind-maybe) - use type-operation instead
  ;;; - Bare first/second without qualifier - always use prefix
  ;;; - fmap without qualifier for type-specific - use type-fmap

  (discouraged
   (patterns . (bind-type map-type first second))
   (prefer . (type-bind type-fmap pair-first pair-second))
   (rationale . "Consistency trumps brevity,
                 prefixes enable discovery and prevent collision")))

 (migration-notes
  ((issue . "combinators.ss uses bind-maybe, map-maybe")
   (fix . "Renamed to maybe-bind, maybe-fmap")
   (status . complete))

  ((issue . "combinators.ss uses bare first, second")
   (fix . "Renamed to pair-first, pair-second")
   (status . complete))

  ((issue . "combinators.ss uses bind-right, map-right, map-left")
   (fix . "Renamed to either-bind, either-fmap, either-fmap-left")
   (status . complete))

  ((issue . "traversable.ss applicative has 3 fields, typeclasses.ss has 4")
   (fix . "Standardize on 4-field representation with functor")
   (status . pending)))

 (decisions
  ((decision . "Use type-fmap for functor mapping")
   (date . "2025-12-29")
   (rationale . "Follows Haskell convention, clear distinction from Scheme's map"))

  ((decision . "Either's primary fmap/bind operate on Right side")
   (date . "2025-12-29")
   (rationale . "Standard Functor instance, matches Haskell Either"))

  ((decision . "Clean break - no deprecated aliases")
   (date . "2025-12-29")
   (rationale . "Forces immediate update, cleaner codebase"))))
