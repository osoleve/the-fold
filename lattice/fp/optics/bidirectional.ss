(load "lattice/fp/optics/profunctor-optics.ss")

(doc 'module 'bidirectional)
(doc 'description "Bidirectional Transformations via Optics

A migration DSL built on top of profunctor optics for reversible:
  - Schema migrations (versioned block transformations)
  - Format conversions (JSON <-> S-expr <-> binary)
  - AST transformations (normalize/expand)

Key insight: profunctor isos ALREADY encode bidirectionality.
We leverage `p-iso-forward`/`p-iso-backward` and composition rules.

Migration = Named, versioned isomorphism with:
  - name: Symbol identifying the migration
  - from-version: Source version (symbol or number)
  - to-version: Target version (symbol or number)
  - iso: The underlying p-iso encoding forward/backward transforms")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'migration-type)

(define (make-migration name from-version to-version iso)
  (doc 'export #t)
  (doc 'type '(-> Symbol Version Version PIso Migration))
  (doc 'description "Create a named migration between two versions.
The iso's forward transforms from-version to to-version,
and the backward transforms to-version back to from-version.")
  (list 'migration name from-version to-version iso))

(define (migration? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x) (eq? (car x) 'migration)))

(define (migration-name m)
  (doc 'export #t)
  (doc 'type '(-> Migration Symbol))
  (cadr m))

(define (migration-from m)
  (doc 'export #t)
  (doc 'type '(-> Migration Version))
  (doc 'description "The source version this migration transforms from.")
  (caddr m))

(define (migration-to m)
  (doc 'export #t)
  (doc 'type '(-> Migration Version))
  (doc 'description "The target version this migration transforms to.")
  (cadddr m))

(define (migration-iso m)
  (doc 'export #t)
  (doc 'type '(-> Migration PIso))
  (doc 'description "The underlying profunctor iso.")
  (car (cddddr m)))

(doc 'section 'core-operations)

(define (migrate m data)
  (doc 'export #t)
  (doc 'type '(-> Migration Data Data))
  (doc 'description "Apply the migration in the forward direction.
Transforms data from `from-version` to `to-version`.")
  ((p-iso-forward (migration-iso m)) data))

(define (rollback m data)
  (doc 'export #t)
  (doc 'type '(-> Migration Data Data))
  (doc 'description "Apply the migration in the backward direction.
Transforms data from `to-version` back to `from-version`.")
  ((p-iso-backward (migration-iso m)) data))

(define (migration-apply m data direction)
  (doc 'export #t)
  (doc 'type '(-> Migration Data Direction Data))
  (doc 'description "Apply migration in specified direction ('forward or 'backward).")
  (case direction
    [(forward) (migrate m data)]
    [(backward) (rollback m data)]
    [else (error 'migration-apply "Invalid direction" direction)]))

(doc 'section 'composition)

(define (migration-compose m1 m2)
  (doc 'export #t)
  (doc 'type '(-> Migration Migration Migration))
  (doc 'description "Compose two migrations: (m1 ; m2) where m1.to = m2.from.
Result transforms from m1.from to m2.to.

Forward:  m1.forward then m2.forward
Backward: m2.backward then m1.backward")
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

(define (migration-chain migrations)
  (doc 'export #t)
  (doc 'type '(-> (List Migration) Migration))
  (doc 'description "Compose a list of migrations into a single migration.
Migrations must form a valid chain: each m.to = next m.from.")
  (cond
    [(null? migrations) (error 'migration-chain "Empty migration list")]
    [(null? (cdr migrations)) (car migrations)]
    [else (fold-left migration-compose (car migrations) (cdr migrations))]))

(doc 'section 'reversal)

(define (migration-flip m)
  (doc 'export #t)
  (doc 'type '(-> Migration Migration))
  (doc 'description "Reverse a migration: swap forward/backward and from/to versions.")
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

(doc 'section 'builders)

(define (make-migration-from-functions name from-ver to-ver forward backward)
  (doc 'export #t)
  (doc 'type '(-> Symbol Version Version (-> a b) (-> b a) Migration))
  (doc 'description "Create a migration from forward and backward functions.")
  (make-migration name from-ver to-ver (make-p-iso forward backward)))

(define (make-identity-migration name version)
  (doc 'export #t)
  (doc 'type '(-> Symbol Version Migration))
  (doc 'description "Create a no-op migration (useful as a base case).")
  (make-migration name version version p-iso-id))

(doc 'section 'predicates)

(define (migration-versions-match? m from to)
  (doc 'export #t)
  (doc 'type '(-> Migration Version Version Boolean))
  (doc 'description "Check if migration transforms between given versions.")
  (and (equal? (migration-from m) from)
       (equal? (migration-to m) to)))

(define (migration-compatible? m1 m2)
  (doc 'export #t)
  (doc 'type '(-> Migration Migration Boolean))
  (doc 'description "Check if two migrations can be composed (m1 ; m2).")
  (equal? (migration-to m1) (migration-from m2)))

(doc 'section 'verification)

(define (verify-migration-laws m test-as test-bs)
  (doc 'export #t)
  (doc 'type '(-> Migration (List a) (List b) Boolean))
  (doc 'description "Verify that migration satisfies iso laws on test data.
test-as: data in from-version format
test-bs: data in to-version format")
  (let ([iso (migration-iso m)])
    (verify-p-iso-laws iso test-as test-bs)))

(doc 'section 'description)

(define (migration-describe m)
  (doc 'export #t)
  (doc 'type '(-> Migration String))
  (doc 'description "Generate a human-readable description of a migration.")
  (format "Migration '~a': ~a -> ~a"
          (migration-name m)
          (migration-from m)
          (migration-to m)))

(display "Loaded: lattice/fp/optics/bidirectional.ss\n")
