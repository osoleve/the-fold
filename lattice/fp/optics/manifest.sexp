;;; lattice/fp/optics/manifest.sexp — Optics Tower Manifest
;;;
;;; A comprehensive hierarchy of composable optics for data access,
;;; enabling principled navigation and transformation of nested structures.

(skill optics
  (version "1.1.0")
  (tier 1)
  (path "lattice/fp/optics")
  (purity total)
  (stability stable)
  (fuel-bound 5000)

  (deps (fp))  ; Depends on fp for base lens/prism in templates.ss

  (description
   "A complete optics tower implementing the standard hierarchy of composable
data accessors. Each optic type provides different capabilities for focusing
on parts of data structures:

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
- Grate for zipping multiple structures together")

  (keywords (optics lenses prisms affines traversals folds getters setters
             isomorphisms grates data-access composition functional-programming
             profunctor profunctor-optics strong choice closed cotraverse zipWith))

  (aliases (optics lens prism profunctor-optics))

  ;;; ====
  ;;; Exports
  ;;; ====

  (exports (
    ;; Isomorphisms
    make-iso iso? iso-forward iso-backward
    iso-view iso-review iso-over iso-flip iso-compose
    iso-id iso-curried iso-flipped iso-swapped iso-reversed
    iso-assoc-list iso-maybe-either iso-cons

    ;; Lens extensions
    lens-id

    ;; Prism extensions
    prism-over prism-set prism-compose
    prism-id prism-nil prism-cons

    ;; Affines
    make-affine affine? affine-getter affine-setter
    affine-preview affine-set affine-over affine-compose
    affine-id affine-nth lens->affine prism->affine

    ;; Traversals
    make-traversal traversal? traversal-traverse traversal-fold
    traversal-to-list traversal-over traversal-set traversal-compose
    traversal-each traversal-filtered traversal-both
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

    ;; Unified Composition
    optic-type optic-compose
    ->traversal ->fold ->setter
    iso->lens iso->prism getter->fold

    ;; Operators
    ^.   ; view (s × Optic → a)
    ^?   ; preview (s × Optic → Maybe a)
    ^..  ; to-list (s × Optic → List a)
    .~   ; set (Optic × b → s → t)
    %~   ; modify (Optic × (a → b) → s → t)
    &    ; reverse apply for chaining

    ;; Law Verification
    verify-iso-laws verify-lens-laws verify-prism-laws verify-traversal-laws
    verify-grate-laws

    ;; Block Optics (block-optics.ss)
    block-tag-lens block-payload-lens block-refs-lens
    block-ref-at block-refs-each block-refs-count
    follow-ref block-child-at block-children-each
    block-type-prism block-lambda-prism block-app-prism block-ref-prism
    block-literal-prism block-expr-prism
    block-payload-string-iso block-payload-as-string-lens block-payload-sexpr-affine
    >>>  ; left-to-right optic composition
    block-has-refs? block-is-leaf? block-tag-is? collect-block-tree

    ;; Profunctor Optics (profunctor-optics.ss)
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
    make-closed closed? closed-profunctor closed-fn
    pclosed closed-fn-instance

    ;; Basic profunctor instances
    profunctor-fn strong-fn choice-fn
    make-forget forget? run-forget profunctor-forget
    make-tagged tagged? run-tagged profunctor-tagged choice-tagged

    ;; Profunctor Iso
    make-p-iso p-iso? p-iso-forward p-iso-backward
    p-iso-run p-iso-view p-iso-review p-iso-compose
    p-iso-id p-iso-swapped p-iso-flipped

    ;; Profunctor Lens
    make-p-lens p-lens? p-lens-getter p-lens-setter
    p-lens-run p-view p-over p-set p-lens-compose
    p-lens-fst p-lens-snd p-lens-id

    ;; Profunctor Prism
    make-p-prism p-prism? p-prism-match p-prism-build
    p-prism-run p-preview p-review p-prism-compose
    p-prism-just p-prism-left p-prism-right p-prism-id

    ;; Profunctor Affine
    make-p-affine p-affine? p-affine-preview p-affine-set
    p-affine-run p-affine-get p-affine-set-fn p-affine-compose
    p-affine-id p-affine-nth
    p-lens->p-affine p-prism->p-affine

    ;; Profunctor Grate (categorical dual of Lens)
    make-p-grate p-grate? p-grate-cotraverse
    run-p-grate p-grate-compose
    p-grate-review p-grate-over p-grate-zipWith
    p-grate-id p-grate-fn p-grate-pair-same p-grate-list-rep
    grate->p-grate p-grate->grate p-iso->p-grate

    ;; Unified profunctor optic composition
    p-optic-type p-optic-compose

    ;; Conversions between concrete and profunctor optics
    iso->p-iso p-iso->iso
    lens->p-lens p-lens->lens
    prism->p-prism p-prism->prism
    affine->p-affine p-affine->affine
  ))

  ;;; ====
  ;;; Modules
  ;;; ====

  (modules (
    ("optics" "optics.ss"
     "Complete optics tower with all optic types, composition, and operators")
    ("block-optics" "block-optics.ss"
     "Optics for The Fold's block system (content-addressed store)")
    ("profunctor-optics" "profunctor-optics.ss"
     "Profunctor encoding of optics: optics as polymorphic functions p a b -> p s t")))

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
    ("test-profunctor-optics.ss" "Tests for profunctor optics encoding")))

  ;;; ====
  ;;; References
  ;;; ====

  (references (
    "Lenses, Folds, and Traversals (Edward Kmett)"
    "Profunctor Optics: Modular Data Accessors (Boisseau & Gibbons)"
    "The Essence of the Iterator Pattern (Gibbons & Oliveira)"
    "Categories of Optics (Riley)"
    "Grate: A Corepresentable Functor Hierarchy (O'Connor)")))
