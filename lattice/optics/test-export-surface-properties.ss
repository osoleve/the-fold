;;; lattice/optics/test-export-surface-properties.ss — QuickCheck export-surface wiring for optics

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'optics)
(require 'block-optics)
(require 'profunctor-optics)
(require 'bidirectional)
(require 'format-iso)
(require 'schema)
(require 'block-migration)

;;; ============================================================================
;;; Export Surface
;;; ============================================================================

;; Keep this list aligned with lattice/optics/manifest.sexp exports.
(define optics-export-groups
  '((optics
      make-iso iso? iso-forward iso-backward
      iso-view iso-review iso-over iso-flip iso-compose
      iso-id iso-curried iso-flipped iso-swapped iso-reversed
      iso-assoc-list iso-maybe-either iso-cons
      make-lens lens? lens-getter lens-setter
      view set-lens over lens-compose
      lens-fst lens-snd lens-head lens-tail lens-nth lens-key
      lens-id
      make-prism prism? prism-match prism-build
      preview review
      prism-just prism-left prism-right
      prism-over prism-set prism-compose
      prism-id prism-nil prism-cons
      make-affine affine? affine-getter affine-setter
      affine-preview affine-set affine-over affine-compose
      affine-id affine-nth lens->affine prism->affine
      vec-element-lens vec-element-affine vec-elements-each
      make-traversal traversal? traversal-traverse traversal-fold
      traversal-to-list traversal-over traversal-set traversal-compose
      traversal-each traversal-filtered filtered traversal-both
      traversal-left traversal-right traversal-just
      lens->traversal prism->traversal affine->traversal
      make-fold fold-optic? fold-optic-fn
      fold-to-list fold-preview fold-has fold-length
      fold-all fold-any fold-sum fold-compose
      fold-each fold-filtered fold-taking fold-dropping
      traversal->fold lens->fold prism->fold
      make-getter getter? getter-fn
      getter-view getter-compose
      getter-id getter-fst getter-snd getter-to
      lens->getter iso->getter
      make-setter setter? setter-over-fn
      setter-over setter-set setter-compose
      setter-mapped setter-arg setter-result
      lens->setter traversal->setter iso->setter
      make-grate grate? grate-cotraverse-fn
      grate-review grate-over grate-set grate-compose
      grate-zipWith grate-zipWith3 grate-zip
      grate-id grate-fn grate-pair-same grate-list-rep
      iso->grate grate->setter
      optic-type optic-compose >>>
      ->traversal ->fold ->setter
      iso->lens iso->prism getter->fold
      ^. ^? ^.. .~ %~ &
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
      make-profunctor profunctor? profunctor-dimap profunctor-lmap profunctor-rmap
      dimap lmap rmap
      make-strong strong? strong-profunctor strong-first strong-second
      pfirst psecond
      make-choice choice? choice-profunctor choice-left choice-right
      pleft pright
      make-closed closed-profunctor closed-fn pclosed closed-fn-instance
      make-wander wander? wander-strong wander-choice wander-wander
      pwander wander-fn
      profunctor-fn strong-fn choice-fn
      make-forget forget? run-forget profunctor-forget
      make-tagged tagged? run-tagged profunctor-tagged choice-tagged
      make-p-iso p-iso? p-iso-forward p-iso-backward
      p-iso-compose p-iso-id p-iso-swapped
      make-p-lens p-lens? p-lens-getter p-lens-setter
      p-view p-over p-set p-lens-compose p-lens-fst p-lens-snd
      make-p-prism p-prism? p-prism-match p-prism-build
      p-preview p-review p-prism-compose
      p-prism-just p-prism-left p-prism-right
      make-p-affine p-affine? p-affine-preview p-affine-set
      p-affine-set-fn p-affine-compose p-affine-nth
      p-lens->p-affine p-prism->p-affine
      make-p-grate p-grate? p-grate-cotraverse
      run-p-grate p-grate-compose
      p-grate-review p-grate-over p-grate-zipWith
      p-grate-id p-grate-fn p-grate-pair-same p-grate-list-rep
      grate->p-grate p-grate->grate p-iso->p-grate
      make-p-traversal p-traversal? p-traversal-traverse p-traversal-fold-fn
      run-p-traversal p-traversal-to-list p-traversal-over p-traversal-set
      p-traversal-compose
      p-traversal-each p-traversal-both p-traversal-filtered
      traversal->p-traversal p-traversal->traversal
      p-lens->p-traversal p-prism->p-traversal p-affine->p-traversal
      make-p-fold p-fold? p-fold-fn
      p-fold-to-list p-fold-preview p-fold-has p-fold-length
      p-fold-all p-fold-any p-fold-compose
      p-fold-each p-fold-filtered p-fold-taking
      p-traversal->p-fold p-lens->p-fold p-prism->p-fold
      ->p-fold p-iso->p-lens
      p-optic-type p-optic-compose
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
      block-migration->migration)))

(define (collect-export-symbols groups)
  (let outer ([gs groups] [acc '()])
    (if (null? gs)
        (reverse acc)
        (let inner ([syms (cdr (car gs))] [acc* acc])
          (if (null? syms)
              (outer (cdr gs) acc*)
              (inner (cdr syms) (cons (car syms) acc*)))))))

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (all pred xs)
  (if (null? xs)
      #t
      (and (pred (car xs)) (all pred (cdr xs)))))

(define (all-true xs)
  (all (lambda (x) x) xs))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-small-int
  (gen-int-range -1000 1000))

(define gen-small-pair
  (gen-pair gen-small-int gen-small-int))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group optics-export-surface-properties

  (define-property "manifest optics export surface list is present and comprehensive"
    (gen-pure #t)
    (lambda (_)
      (let ([syms (collect-export-symbols optics-export-groups)])
        (> (length syms) 350)))
    'tests 5)

  (define-property "format isos and schema isos roundtrip representative values"
    gen-small-int
    (lambda (n)
      (let* ([s (number->string n)]
             [bv ((p-iso-forward iso-utf8) s)]
             [s* ((p-iso-backward iso-utf8) bv)]
             [expr `((count . ,n) (name . "x"))]
             [as-bv (sexpr->bytevector expr)]
             [expr* (bytevector->sexpr as-bv)]
             [schema (schema-compose
                      (list (field-rename-iso 'count 'n)
                            (field-add-iso 'v 1)
                            (field-transform-if-present-iso 'n (make-p-iso add1 sub1))))]
             [migrated ((p-iso-forward schema) expr)]
             [rolled ((p-iso-backward schema) migrated)])
        (and (string? s*)
             (equal? s s*)
             (equal? expr expr*)
             (pair? (assq 'n migrated))
             (equal? expr rolled))))
    'tests 80)

  (define-property "block migration helpers are well-formed and reversible on matching tags"
    gen-small-int
    (lambda (n)
      (let* ([base `((x . ,n))]
             [bm (make-block-migration 'v1 'v2 (field-add-iso 'migrated #t))]
             [blk (make-block 'v1 (sexpr->bytevector base) (vector))]
             [applies? (block-migration-applies? bm blk)]
             [forward (block-migrate-payload bm blk)]
             [backward (block-rollback-payload bm forward)]
             [ver (make-versioned-tag 'issue 2)]
             [parsed (parse-versioned-tag ver)]
             [bridged (block-migration->migration bm)])
        (and applies?
             (block-migration? bm)
             (eq? (block-migration-from-tag bm) 'v1)
             (eq? (block-migration-to-tag bm) 'v2)
             (eq? (block-tag forward) 'v2)
             (eq? (block-tag backward) 'v1)
             (equal? (bytevector->sexpr (block-payload backward)) base)
             (eq? (car parsed) 'issue)
             (= (cdr parsed) 2)
             (migration? bridged))))
    'tests 80)

  (define-property "core optics operators and profunctor bridges stay consistent"
    gen-small-pair
    (lambda (pair)
      (let* ([x (car pair)]
             [y (cdr pair)]
             [p (cons x y)]
             [setter ((.~ lens-fst (+ x 1)) p)]
             [modded ((%~ lens-snd (lambda (z) (+ z 2))) p)]
             [from-op (& p (%~ lens-fst add1))]
             [pl (lens->p-lens lens-fst)]
             [round (p-lens->lens pl)]
             [piso (iso->p-iso iso-swapped)]
             [round-iso (p-iso->iso piso)]
             [g (getter->fold getter-fst)]
             [pg (p-lens->p-fold pl)])
        (and (= (^. p lens-fst) x)
             (just? (^? p lens-fst))
             (= (car (^.. p lens-fst)) x)
             (= (car setter) (+ x 1))
             (= (cdr modded) (+ y 2))
             (= (car from-op) (+ x 1))
             (equal? (view round p) x)
             (equal? (iso-view round-iso p) (iso-view iso-swapped p))
             (fold-optic? g)
             (p-fold? pg)
             (all-true (list (profunctor? profunctor-fn)
                             (strong? strong-fn)
                             (choice? choice-fn)
                             (closed-profunctor closed-fn-instance)
                             (wander? wander-fn))))))
    'tests 120)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
