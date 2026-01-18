;;; lattice/meta/manifest-migration.ss — Bidirectional Manifest Schema Migrations
;;;
;;; Enables smooth evolution of the manifest.sexp schema across the lattice.
;;; When the manifest schema changes, migrations automatically transform
;;; old manifests to the new format on read.
;;;
;;; Use cases:
;;;   - Add required fields with defaults
;;;   - Rename fields (e.g., 'deps' -> 'dependencies')
;;;   - Restructure nested data (e.g., flatten exports)
;;;   - Transform values (e.g., string tiers to integers)
;;;
;;; This is Lattice code: pure transformations on manifest S-expressions.
;;;
;;; Dependencies:
;;;   lattice/fp/optics/bidirectional.ss
;;;   lattice/fp/optics/schema.ss

(load "lattice/fp/optics/bidirectional.ss")
(load "lattice/fp/optics/schema.ss")

;;; ============================================================
;;; Part 1: Manifest Version Detection
;;; ============================================================
;;;
;;; Manifests don't have explicit schema versions, so we detect
;;; based on structural features (presence of fields, value types).

;;; manifest-schema-version : SExp -> Symbol
;;; Detect manifest schema version from its structure.
;;; Returns: 'v1 (original), 'v2 (with schema-version field), etc.
(define (manifest-schema-version manifest)
  (cond
    ;; Explicit schema-version field takes precedence
    [(assq 'schema-version (cdr manifest))
     => (lambda (pair) (cadr pair))]
    ;; Detection heuristics for older manifests
    [(and (assq 'deps manifest)
          (not (assq 'dependencies manifest)))
     'v1]  ; Uses 'deps' not 'dependencies'
    [else 'v1]))  ; Default to v1

;;; ============================================================
;;; Part 2: Manifest Migration Registry
;;; ============================================================

(define *manifest-migrations* (make-eq-hashtable))

;;; register-manifest-migration! : Symbol -> Symbol -> PIso -> Void
(define (register-manifest-migration! from-version to-version iso)
  (hashtable-set! *manifest-migrations*
                  (cons from-version to-version)
                  (make-migration
                   (string->symbol (format "manifest-~a->~a" from-version to-version))
                   from-version to-version iso)))

;;; get-manifest-migration : Symbol -> Symbol -> Migration | #f
(define (get-manifest-migration from to)
  (hashtable-ref *manifest-migrations* (cons from to) #f))

;;; ============================================================
;;; Part 3: Common Manifest Transformations
;;; ============================================================
;;;
;;; Building blocks for manifest migrations.

;;; manifest-add-schema-version : Symbol -> PIso
;;; Add or update schema-version field.
(define (manifest-add-schema-version version)
  (make-p-iso
   (lambda (manifest)
     (cons (car manifest)  ; 'skill
           (cons (list 'schema-version version)
                 (filter (lambda (f) (not (and (pair? f) (eq? (car f) 'schema-version))))
                         (cdr manifest)))))
   (lambda (manifest)
     ;; Backward: remove schema-version
     (cons (car manifest)
           (filter (lambda (f) (not (and (pair? f) (eq? (car f) 'schema-version))))
                   (cdr manifest))))))

;;; manifest-rename-field : Symbol -> Symbol -> PIso
;;; Rename a top-level manifest field.
(define (manifest-rename-field old-name new-name)
  (make-p-iso
   (lambda (manifest)
     (cons (car manifest)
           (map (lambda (field)
                  (if (and (pair? field) (eq? (car field) old-name))
                      (cons new-name (cdr field))
                      field))
                (cdr manifest))))
   (lambda (manifest)
     (cons (car manifest)
           (map (lambda (field)
                  (if (and (pair? field) (eq? (car field) new-name))
                      (cons old-name (cdr field))
                      field))
                (cdr manifest))))))

;;; manifest-add-field-default : Symbol -> Any -> PIso
;;; Add a field with default if not present.
(define (manifest-add-field-default field default)
  (make-p-iso
   (lambda (manifest)
     (if (assq field (cdr manifest))
         manifest  ; Already has field
         (cons (car manifest)
               (cons (list field default) (cdr manifest)))))
   (lambda (manifest)
     ;; Backward: remove field if it has the default value
     (let ([pair (assq field (cdr manifest))])
       (if (and pair (equal? (cadr pair) default))
           (cons (car manifest)
                 (filter (lambda (f) (not (and (pair? f) (eq? (car f) field))))
                         (cdr manifest)))
           manifest)))))

;;; manifest-transform-field : Symbol -> PIso -> PIso
;;; Transform a manifest field's value.
(define (manifest-transform-field field value-iso)
  (make-p-iso
   (lambda (manifest)
     (cons (car manifest)
           (map (lambda (f)
                  (if (and (pair? f) (eq? (car f) field))
                      (cons field ((p-iso-forward value-iso) (cdr f)))
                      f))
                (cdr manifest))))
   (lambda (manifest)
     (cons (car manifest)
           (map (lambda (f)
                  (if (and (pair? f) (eq? (car f) field))
                      (cons field ((p-iso-backward value-iso) (cdr f)))
                      f))
                (cdr manifest))))))

;;; ============================================================
;;; Part 4: Manifest Normalization
;;; ============================================================
;;;
;;; Normalize manifests to the current schema version for consistent processing.

(define CURRENT-MANIFEST-SCHEMA 'v1)

;;; normalize-manifest : SExp -> SExp
;;; Migrate manifest to current schema version.
(define (normalize-manifest manifest)
  (let ([version (manifest-schema-version manifest)])
    (if (eq? version CURRENT-MANIFEST-SCHEMA)
        manifest
        ;; Try to find migration path
        (let ([path (find-manifest-migration-path version CURRENT-MANIFEST-SCHEMA)])
          (if (not path)
              manifest  ; No migration, use as-is
              (fold-left (lambda (m migration) (migrate migration m))
                         manifest
                         path))))))

;;; find-manifest-migration-path : Symbol -> Symbol -> (List Migration) | #f
(define (find-manifest-migration-path from to)
  (if (eq? from to)
      '()
      (let ([direct (get-manifest-migration from to)])
        (if direct
            (list direct)
            ;; Could implement BFS for multi-step, but manifests rarely need it
            #f))))

;;; ============================================================
;;; Part 5: Example Migrations
;;; ============================================================
;;;
;;; Uncomment and modify as the manifest schema evolves.

;;; Example: v1 -> v2 migration (adding schema-version field)
;; (register-manifest-migration! 'v1 'v2
;;   (p-iso-compose
;;     (manifest-add-schema-version 'v2)
;;     (manifest-add-field-default 'stability 'experimental)))

;;; Example: Rename 'deps' to 'dependencies'
;; (register-manifest-migration! 'v2 'v3
;;   (p-iso-compose
;;     (manifest-rename-field 'deps 'dependencies)
;;     (manifest-add-schema-version 'v3)))

;;; ============================================================
;;; Part 6: Manifest Linting with Migration Hints
;;; ============================================================

;;; lint-manifest : SExp -> (List String)
;;; Check manifest for issues, suggest migrations.
(define (lint-manifest manifest)
  (let ([issues '()])
    ;; Check for deprecated field names
    (when (assq 'deps manifest)
      (set! issues (cons "Warning: 'deps' is deprecated, use 'dependencies'" issues)))
    ;; Check for missing recommended fields
    (unless (assq 'stability manifest)
      (set! issues (cons "Hint: Consider adding 'stability' field" issues)))
    (unless (assq 'schema-version manifest)
      (set! issues (cons "Hint: Consider adding explicit 'schema-version'" issues)))
    (reverse issues)))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Version detection:
;;;   manifest-schema-version
;;;
;;; Registration:
;;;   register-manifest-migration!, get-manifest-migration
;;;
;;; Transformations:
;;;   manifest-add-schema-version, manifest-rename-field
;;;   manifest-add-field-default, manifest-transform-field
;;;
;;; Normalization:
;;;   normalize-manifest, find-manifest-migration-path
;;;
;;; Linting:
;;;   lint-manifest

(display "Loaded: lattice/meta/manifest-migration.ss\n")
