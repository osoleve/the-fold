;;; lattice/optics/manifest.sexp — Optics Tower Manifest
;;;
;;; A comprehensive hierarchy of composable optics for data access,
;;; enabling principled navigation and transformation of nested structures.

(skill optics
  (version "1.3.0")
  (tier 1)
  (path "lattice/optics")
  (purity total)
  (stability stable)
  (fuel-bound 5000)

  (deps (fp))  ; Depends on fp for base lens/prism in templates.ss

  (description
   "A complete optics tower with bidirectional transformations for composable
data access and reversible migrations. Each optic type provides different
capabilities for focusing on parts of data structures:

                  Fold
                 /    \\
            Getter    Traversal
                 \\    /    \\
                  Affine   Setter
                 /    \\     |
              Prism   Lens  |
                 \\    /    /
                   Iso ---- Grate
                        \\   /
                         \\ /

Grate is the categorical dual of Lens:
- Lens: get/set a single focus (requires Strong profunctor)
- Grate: cotraverse/zipWith over a structure (requires Closed profunctor)

Key features:
- Unified composition that respects the hierarchy (lens+prism=affine, etc.)
- Law verification for all optic types including Grate
- Operator syntax (^., ^?, ^.., .~, %~) for ergonomic use
- Comprehensive instances for Maybe, Either, List, and pairs
- Automatic type-preserving composition
- Grate for zipping multiple structures together
- Bidirectional transformations: migrations with automatic rollback
- Schema DSL for field-level operations (rename, add, transform)
- Block migrations for CAS schema evolution
- Bottom-up tree traversal for Merkle DAG correctness")

  (keywords (optics lenses prisms affines traversals folds getters setters
             isomorphisms grates data-access composition functional-programming
             profunctor profunctor-optics strong choice closed cotraverse zipWith
             bidirectional migrations schema-evolution format-conversion
             rollback reversible block-migration merkle-dag))

  (aliases (optics lens prism profunctor-optics))

  (concepts
    (concept optics-paradigm
      (description "Composable data accessors (Lens, Prism, Traversal, Iso, Grate) for focused read/write on nested structures.")
      (parent functional-programming)
      (synonyms optics lens prism profunctor-optics profunctor strong-profunctor choice-profunctor closed-profunctor wander)))

  ;;; ====
  ;;; Exports
  ;;; ====

  (exports
    (optics
      ;; Isomorphisms
      make-iso iso? iso-forward iso-backward
      iso-view iso-review iso-over iso-flip iso-compose
      iso-id iso-curried iso-flipped iso-swapped iso-reversed
      iso-assoc-list iso-maybe-either iso-cons
      ;; Lenses (base re-exported from templates + extensions)
      make-lens lens? lens-getter lens-setter
      view set-lens over lens-compose
      lens-fst lens-snd lens-head lens-tail lens-nth lens-key
      lens-id
      ;; Prisms (base re-exported from templates + extensions)
      make-prism prism? prism-match prism-build
      preview review
      prism-just prism-left prism-right
      prism-over prism-set prism-compose
      prism-id prism-nil prism-cons
      ;; Affines
      make-affine affine? affine-getter affine-setter
      affine-preview affine-set affine-over affine-compose
      affine-id affine-nth lens->affine prism->affine
      ;; Vector optics
      vec-element-lens vec-element-affine vec-elements-each
      ;; Traversals
      make-traversal traversal? traversal-traverse traversal-fold
      traversal-to-list traversal-over traversal-set traversal-compose
      traversal-each traversal-filtered filtered traversal-both
      traversal-left traversal-right traversal-just
      lens->traversal prism->traversal affine->traversal
      ;; Folds
      make-fold fold-optic? fold-optic-fn
      fold-to-list fold-preview fold-has fold-length
      fold-all fold-any fold-sum fold-compose
      fold-each fold-filtered fold-taking fold-dropping
      traversal->fold lens->fold prism->fold
      ;; Getters
      make-getter getter? getter-fn
      getter-view getter-compose
      getter-id getter-fst getter-snd getter-to
      lens->getter iso->getter
      ;; Setters
      make-setter setter? setter-over-fn
      setter-over setter-set setter-compose
      setter-mapped setter-arg setter-result
      lens->setter traversal->setter iso->setter
      ;; Grates (categorical dual of Lens)
      make-grate grate? grate-cotraverse-fn
      grate-review grate-over grate-set grate-compose
      grate-zipWith grate-zipWith3 grate-zip
      grate-id grate-fn grate-pair-same grate-list-rep
      iso->grate grate->setter
      ;; Unified composition
      optic-type optic-compose >>>
      ->traversal ->fold ->setter
      iso->lens iso->prism getter->fold
      ;; Operators
      ^. ^? ^.. .~ %~ &
      ;; Law verification
      verify-iso-laws verify-lens-laws verify-prism-laws
      verify-traversal-laws verify-grate-laws)

    (block-optics
      block-tag-lens block-payload-lens block-refs-lens
      block-ref-at block-refs-each block-refs-count
      follow-ref block-child-at block-children-each
      block-type-prism block-lambda-prism block-app-prism block-ref-prism
      block-literal-prism block-expr-prism
      block-payload-string-iso block-payload-as-string-lens block-payload-sexpr-affine
      block-has-refs? block-is-leaf? block-tag-is? collect-block-tree)

    (profunctor-optics
      ;; Profunctor type class
      make-profunctor profunctor? profunctor-dimap profunctor-lmap profunctor-rmap
      dimap lmap rmap
      ;; Strong profunctor (for lenses)
      make-strong strong? strong-profunctor strong-first strong-second
      pfirst psecond
      ;; Choice profunctor (for prisms)
      make-choice choice? choice-profunctor choice-left choice-right
      pleft pright
      ;; Closed profunctor (for grates)
      make-closed closed-profunctor closed-fn pclosed closed-fn-instance
      ;; Wander profunctor (for traversals)
      make-wander wander? wander-strong wander-choice wander-wander
      pwander wander-fn
      ;; Profunctor instances
      profunctor-fn strong-fn choice-fn
      make-forget forget? run-forget profunctor-forget
      make-tagged tagged? run-tagged profunctor-tagged choice-tagged
      ;; Profunctor Iso
      make-p-iso p-iso? p-iso-forward p-iso-backward
      p-iso-compose p-iso-id p-iso-swapped
      ;; Profunctor Lens
      make-p-lens p-lens? p-lens-getter p-lens-setter
      p-view p-over p-set p-lens-compose p-lens-fst p-lens-snd
      ;; Profunctor Prism
      make-p-prism p-prism? p-prism-match p-prism-build
      p-preview p-review p-prism-compose
      p-prism-just p-prism-left p-prism-right
      ;; Profunctor Affine
      make-p-affine p-affine? p-affine-preview p-affine-set
      p-affine-set-fn p-affine-compose p-affine-nth
      p-lens->p-affine p-prism->p-affine
      ;; Profunctor Grate
      make-p-grate p-grate? p-grate-cotraverse
      run-p-grate p-grate-compose
      p-grate-review p-grate-over p-grate-zipWith
      p-grate-id p-grate-fn p-grate-pair-same p-grate-list-rep
      grate->p-grate p-grate->grate p-iso->p-grate
      ;; Profunctor Traversal
      make-p-traversal p-traversal? p-traversal-traverse p-traversal-fold-fn
      run-p-traversal p-traversal-to-list p-traversal-over p-traversal-set
      p-traversal-compose
      p-traversal-each p-traversal-both p-traversal-filtered
      traversal->p-traversal p-traversal->traversal
      p-lens->p-traversal p-prism->p-traversal p-affine->p-traversal
      ;; Profunctor Fold
      make-p-fold p-fold? p-fold-fn
      p-fold-to-list p-fold-preview p-fold-has p-fold-length
      p-fold-all p-fold-any p-fold-compose
      p-fold-each p-fold-filtered p-fold-taking
      p-traversal->p-fold p-lens->p-fold p-prism->p-fold
      ->p-fold p-iso->p-lens
      ;; Unified profunctor composition
      p-optic-type p-optic-compose
      ;; Conversions between concrete and profunctor optics
      iso->p-iso p-iso->iso
      lens->p-lens p-lens->lens
      prism->p-prism p-prism->prism
      affine->p-affine p-affine->affine)

    (bidirectional
      make-migration migration? migration-name
      migration-from migration-to migration-iso
      migrate rollback migration-apply
      migration-compose migration-chain migration-flip
      make-migration-from-functions make-identity-migration
      migration-versions-match? migration-compatible?
      verify-migration-laws migration-describe)

    (format-iso
      iso-utf8 iso-utf8-flip
      iso-sexpr-string iso-sexpr-bytevector
      sexpr->bytevector bytevector->sexpr
      iso-list-vector iso-alist-keys-sorted
      iso-number-string iso-exact-inexact
      iso-bool-int iso-bool-symbol
      iso-null-nothing iso-false-nothing
      iso-time-unix iso-symbol-string
      make-tag-iso iso-ok-unwrap
      format-compose format-flip)

    (schema
      field-rename-iso field-add-iso field-remove-iso
      field-transform-iso field-transform-if-present-iso
      field-split-iso field-merge-iso
      field-move-to-front-iso nested-field-iso
      field-ensure-iso field-with-default-iso
      fields-rename-iso fields-keep-only-iso fields-remove-iso
      schema-compose
      field-coerce-to-string-iso field-coerce-to-number-iso)

    (block-migration
      make-block-migration block-migration?
      block-migration-from-tag block-migration-to-tag block-migration-payload-iso
      block-migration-applies? block-migrate-payload block-rollback-payload
      block-with-refs block-migrate-with-refs
      parse-versioned-tag make-versioned-tag
      block-tag-version block-tag-base
      block-migration-compose block-migration-flip
      make-tag-only-migration make-schema-migration
      block-has-sexpr-payload? known-sexpr-tags block-is-sexpr-type?
      block-migration->migration))

  ;;; ====
  ;;; Modules
  ;;; ====

  (modules (
    ("optics" "optics.ss"
     "Complete optics tower with all optic types, composition, and operators")
    ("block-optics" "block-optics.ss"
     "Optics for The Fold's block system (content-addressed store)")
    ("profunctor-optics" "profunctor-optics.ss"
     "Profunctor encoding of optics: optics as polymorphic functions p a b -> p s t")
    ("bidirectional" "bidirectional.ss"
     "Bidirectional transformations via optics: migrations, schema evolution, format conversion")
    ("format-iso" "format-iso.ss"
     "Standard format isomorphisms: UTF-8, S-expr, bytevector, booleans, numbers")
    ("schema" "schema.ss"
     "Field-level schema operations DSL: rename, add, remove, transform, split, merge")
    ("block-migration" "block-migration.ss"
     "CAS block-specific migrations: tag transforms, payload migrations, tree traversal")))

  ;;; ====
  ;;; Optic Hierarchy
  ;;; ====

  (type-hierarchy (
    ;; Each entry: (type targets read? write? laws)
    (iso       "exactly 1"   yes yes (forward-backward backward-forward))
    (lens      "exactly 1"   yes yes (get-put put-get put-put))
    (prism     "0 or 1"      yes yes (preview-review review-preview))
    (affine    "0 or 1"      yes yes (get-set set-get))
    (grate     "exactly 1"   no  yes (review-over zipWith-identity))  ; dual of lens
    (traversal "0 or more"   yes yes (identity composition))
    (fold      "0 or more"   yes no  ())
    (getter    "exactly 1"   yes no  ())
    (setter    "0 or more"   no  yes (identity composition))))

  ;;; ====
  ;;; Composition Rules
  ;;; ====

  (composition-rules (
    ;; (outer inner) → result
    (iso iso)           → iso
    (iso lens)          → lens
    (lens iso)          → lens
    (iso prism)         → prism
    (prism iso)         → prism
    (iso grate)         → grate
    (grate iso)         → grate
    (lens lens)         → lens
    (prism prism)       → prism
    (grate grate)       → grate
    (lens prism)        → affine
    (prism lens)        → affine
    (affine affine)     → affine
    (grate setter)      → setter
    (* traversal)       → traversal
    (* fold)            → fold
    (getter getter)     → getter
    (setter setter)     → setter))

  ;;; ====
  ;;; Usage Examples
  ;;; ====

  (examples (
    ;; View through lens
    ((^. '(1 . 2) lens-fst) → 1)

    ;; Preview through prism
    ((^? (just 42) prism-just) → (just 42))
    ((^? nothing prism-just) → nothing)

    ;; Get all targets through traversal
    ((^.. '(1 2 3 4 5) (traversal-filtered even?)) → (2 4))

    ;; Set through lens
    ((& '(1 . 2) (.~ lens-fst 99)) → (99 . 2))

    ;; Modify through traversal
    ((& '(1 2 3) (%~ traversal-each (lambda (x) (* x 2)))) → (2 4 6))

    ;; Compose lens with prism → affine
    ((define affine (optic-compose lens-fst prism-just))
     (affine-preview affine (cons (just 42) "hello")) → (just 42))

    ;; Iso round-trip
    ((iso-review iso-swapped (iso-view iso-swapped '(1 . 2))) → (1 . 2))

    ;; Grate: zip two pairs element-wise
    ((grate-zipWith grate-pair-same + '(1 . 2) '(3 . 4)) → (4 . 6))

    ;; Grate: apply same argument to two functions
    ((let ([add1 (lambda (x) (+ x 1))]
           [double (lambda (x) (* x 2))])
       ((grate-zipWith grate-fn + add1 double) 5)) → 16)))  ; (5+1) + (5*2)

  ;;; ====
  ;;; Testing
  ;;; ====

  (testing (
    ("test-optics.ss" "Comprehensive tests for all optic types and composition")
    ("test-block-optics.ss" "Tests for block system optics")
    ("test-profunctor-optics.ss" "Tests for profunctor optics encoding")
    ("test-bidirectional.ss" "Tests for bidirectional transformations and migrations")))

  ;;; ====
  ;;; References
  ;;; ====

  (references (
    "Lenses, Folds, and Traversals (Edward Kmett)"
    "Profunctor Optics: Modular Data Accessors (Boisseau & Gibbons)"
    "The Essence of the Iterator Pattern (Gibbons & Oliveira)"
    "Categories of Optics (Riley)"
    "Grate: A Corepresentable Functor Hierarchy (O'Connor)")))
