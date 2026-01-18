;;; lattice/fp/optics/bidirectional.ss — Bidirectional Transformations via Optics
;;;
;;; A migration DSL built on top of profunctor optics for reversible:
;;;   - Schema migrations (versioned block transformations)
;;;   - Format conversions (JSON <-> S-expr <-> binary)
;;;   - AST transformations (normalize/expand)
;;;
;;; Key insight: profunctor isos ALREADY encode bidirectionality.
;;; We leverage `p-iso-forward`/`p-iso-backward` and composition rules.
;;;
;;; Migration = Named, versioned isomorphism with:
;;;   - name: Symbol identifying the migration
;;;   - from-version: Source version (symbol or number)
;;;   - to-version: Target version (symbol or number)
;;;   - iso: The underlying p-iso encoding forward/backward transforms
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - profunctor-optics.ss (for p-iso infrastructure)
;;;   - fp/meta/combinators.ss (for compose2)

(load "lattice/fp/optics/profunctor-optics.ss")

;;; ============================================================
;;; Part 1: Migration Type
;;; ============================================================
;;;
;;; A migration encapsulates a versioned, named isomorphism.
;;; The iso provides both forward (migrate) and backward (rollback)
;;; directions automatically.

;;; make-migration : Symbol -> Version -> Version -> PIso -> Migration
;;; Create a named migration between two versions.
;;; The iso's forward transforms from-version to to-version,
;;; and the backward transforms to-version back to from-version.
(define (make-migration name from-version to-version iso)
  (list 'migration name from-version to-version iso))

;;; migration? : Any -> Boolean
(define (migration? x)
  (and (pair? x) (eq? (car x) 'migration)))

;;; migration-name : Migration -> Symbol
(define (migration-name m)
  (cadr m))

;;; migration-from : Migration -> Version
;;; The source version this migration transforms from.
(define (migration-from m)
  (caddr m))

;;; migration-to : Migration -> Version
;;; The target version this migration transforms to.
(define (migration-to m)
  (cadddr m))

;;; migration-iso : Migration -> PIso
;;; The underlying profunctor iso.
(define (migration-iso m)
  (car (cddddr m)))

;;; ============================================================
;;; Part 2: Core Migration Operations
;;; ============================================================

;;; migrate : Migration -> Data -> Data
;;; Apply the migration in the forward direction.
;;; Transforms data from `from-version` to `to-version`.
(define (migrate m data)
  ((p-iso-forward (migration-iso m)) data))

;;; rollback : Migration -> Data -> Data
;;; Apply the migration in the backward direction.
;;; Transforms data from `to-version` back to `from-version`.
(define (rollback m data)
  ((p-iso-backward (migration-iso m)) data))

;;; migration-apply : Migration -> Data -> Direction -> Data
;;; Apply migration in specified direction ('forward or 'backward).
(define (migration-apply m data direction)
  (case direction
    [(forward) (migrate m data)]
    [(backward) (rollback m data)]
    [else (error 'migration-apply "Invalid direction" direction)]))

;;; ============================================================
;;; Part 3: Migration Composition
;;; ============================================================
;;;
;;; Migrations can be composed to form chains:
;;;   v1 -> v2 -> v3 becomes a single v1 -> v3 migration.
;;;
;;; The key insight: composing isos composes both directions automatically.

;;; migration-compose : Migration -> Migration -> Migration
;;; Compose two migrations: (m1 ; m2) where m1.to = m2.from.
;;; Result transforms from m1.from to m2.to.
;;;
;;; Forward:  m1.forward then m2.forward
;;; Backward: m2.backward then m1.backward
(define (migration-compose m1 m2)
  ;; Validate version compatibility
  (unless (equal? (migration-to m1) (migration-from m2))
    (error 'migration-compose
           "Version mismatch: ~a.to (~a) != ~a.from (~a)"
           (migration-name m1) (migration-to m1)
           (migration-name m2) (migration-from m2)))

  (let* ([iso1 (migration-iso m1)]
         [iso2 (migration-iso m2)]
         [combined-iso (make-p-iso
                        ;; Forward: apply m1 then m2
                        (compose2 (p-iso-forward iso2) (p-iso-forward iso1))
                        ;; Backward: apply m2 then m1 (reverse order)
                        (compose2 (p-iso-backward iso1) (p-iso-backward iso2)))]
         [combined-name (string->symbol
                         (format "~a->~a"
                                 (migration-from m1)
                                 (migration-to m2)))])
    (make-migration combined-name
                    (migration-from m1)
                    (migration-to m2)
                    combined-iso)))

;;; migration-chain : (List Migration) -> Migration
;;; Compose a list of migrations into a single migration.
;;; Migrations must form a valid chain: each m.to = next m.from.
(define (migration-chain migrations)
  (cond
    [(null? migrations) (error 'migration-chain "Empty migration list")]
    [(null? (cdr migrations)) (car migrations)]
    [else (fold-left migration-compose (car migrations) (cdr migrations))]))

;;; ============================================================
;;; Part 4: Migration Reversal
;;; ============================================================

;;; migration-flip : Migration -> Migration
;;; Reverse a migration: swap forward/backward and from/to versions.
(define (migration-flip m)
  (let* ([iso (migration-iso m)]
         [flipped-iso (make-p-iso
                       (p-iso-backward iso)
                       (p-iso-forward iso))]
         [flipped-name (string->symbol
                        (format "~a-reversed" (migration-name m)))])
    (make-migration flipped-name
                    (migration-to m)     ; Swap from/to
                    (migration-from m)
                    flipped-iso)))

;;; ============================================================
;;; Part 5: Migration Builders
;;; ============================================================
;;;
;;; Convenience constructors for common migration patterns.

;;; make-migration-from-functions : Symbol -> Version -> Version
;;;                                 -> (a -> b) -> (b -> a) -> Migration
;;; Create a migration from forward and backward functions.
(define (make-migration-from-functions name from-ver to-ver forward backward)
  (make-migration name from-ver to-ver (make-p-iso forward backward)))

;;; make-identity-migration : Symbol -> Version -> Migration
;;; Create a no-op migration (useful as a base case).
(define (make-identity-migration name version)
  (make-migration name version version p-iso-id))

;;; ============================================================
;;; Part 6: Migration Predicates and Queries
;;; ============================================================

;;; migration-versions-match? : Migration -> Version -> Version -> Boolean
;;; Check if migration transforms between given versions.
(define (migration-versions-match? m from to)
  (and (equal? (migration-from m) from)
       (equal? (migration-to m) to)))

;;; migration-compatible? : Migration -> Migration -> Boolean
;;; Check if two migrations can be composed (m1 ; m2).
(define (migration-compatible? m1 m2)
  (equal? (migration-to m1) (migration-from m2)))

;;; ============================================================
;;; Part 7: Law Verification
;;; ============================================================
;;;
;;; Migrations must satisfy the iso laws:
;;;   - Roundtrip forward: rollback(migrate(x)) = x
;;;   - Roundtrip backward: migrate(rollback(y)) = y

;;; verify-migration-laws : Migration -> (List a) -> (List b) -> Boolean
;;; Verify that migration satisfies iso laws on test data.
;;; test-as: data in from-version format
;;; test-bs: data in to-version format
(define (verify-migration-laws m test-as test-bs)
  (let ([iso (migration-iso m)])
    (verify-p-iso-laws iso test-as test-bs)))

;;; ============================================================
;;; Part 8: Migration Description
;;; ============================================================

;;; migration-describe : Migration -> String
;;; Generate a human-readable description of a migration.
(define (migration-describe m)
  (format "Migration '~a': ~a -> ~a"
          (migration-name m)
          (migration-from m)
          (migration-to m)))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Migration Type:
;;;   make-migration, migration?, migration-name
;;;   migration-from, migration-to, migration-iso
;;;
;;; Core Operations:
;;;   migrate, rollback, migration-apply
;;;
;;; Composition:
;;;   migration-compose, migration-chain
;;;
;;; Reversal:
;;;   migration-flip
;;;
;;; Builders:
;;;   make-migration-from-functions, make-identity-migration
;;;
;;; Predicates:
;;;   migration-versions-match?, migration-compatible?
;;;
;;; Verification:
;;;   verify-migration-laws
;;;
;;; Description:
;;;   migration-describe

(display "Loaded: lattice/fp/optics/bidirectional.ss\n")
