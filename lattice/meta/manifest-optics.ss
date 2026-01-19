;;; lattice/meta/manifest-optics.ss — Optics for Manifest Navigation
;;;
;;; Provides composable optics for accessing nested manifest structures.
;;; Replaces the common `(cdr (or (assq 'key alist) '(key . default)))` pattern.
;;;
;;; This is Lattice code: pure, uses fp/optics primitives.
;;;
;;; Usage:
;;;   (^. manifest (manifest-field 'version))        ; Get version
;;;   (^? manifest (manifest-field 'fuel-bound))     ; Get optional field
;;;   (& manifest (.~ (manifest-field 'tier) 2))     ; Set tier
;;;
;;; Dependencies:
;;;   lattice/fp/optics/optics.ss

(load "lattice/fp/optics/optics.ss")

;;; ============================================================
;;; Part 1: Alist Optics
;;; ============================================================
;;;
;;; Alists are the primary data structure for manifest data.
;;; We provide both lenses (for required fields) and affines (for optional).

;;; alist-affine : Symbol → Affine Alist α
;;; Focus on a key in an alist. Returns nothing if key absent.
;;; This is an affine (not a lens) because the key may not exist.
(define (alist-affine key)
  (make-affine
   ;; getter: preview
   (lambda (alist)
     (let ([entry (assq key alist)])
       (if entry
           (just (cdr entry))
           nothing)))
   ;; setter: update or insert
   (lambda (val alist)
     (cons (cons key val)
           (filter (lambda (p) (not (eq? (car p) key))) alist)))))

;;; alist-affine-default : Symbol × α → Affine Alist α
;;; Like alist-affine but returns a default when key is absent.
;;; Note: Technically this is a lens since it always succeeds,
;;; but we keep the affine representation for composition consistency.
(define (alist-affine-default key default)
  (make-affine
   ;; getter: always succeeds
   (lambda (alist)
     (let ([entry (assq key alist)])
       (just (if entry (cdr entry) default))))
   ;; setter: update or insert
   (lambda (val alist)
     (cons (cons key val)
           (filter (lambda (p) (not (eq? (car p) key))) alist)))))

;;; alist-lens : Symbol → Lens Alist α
;;; Focus on a required key in an alist. Assumes key exists.
;;; Use this only when you know the key is present.
(define (alist-lens key)
  (make-lens
   ;; getter
   (lambda (alist)
     (cdr (assq key alist)))
   ;; setter
   (lambda (val alist)
     (cons (cons key val)
           (filter (lambda (p) (not (eq? (car p) key))) alist)))))

;;; alist-at : Symbol → Affine Alist α
;;; Alias for alist-affine (shorter name for composition chains).
(define alist-at alist-affine)

;;; ============================================================
;;; Part 2: Manifest Field Optics
;;; ============================================================
;;;
;;; Pre-built optics for common manifest fields.
;;; These use sensible defaults matching the manifest spec.

;;; manifest-field : Symbol → Affine Manifest Value
;;; Generic field accessor with standard defaults.
(define (manifest-field key)
  (case key
    [(name)        (alist-lens 'name)]  ; Required
    [(version)     (alist-affine-default 'version "0.0.0")]
    [(tier)        (alist-affine-default 'tier 0)]
    [(path)        (alist-affine-default 'path "")]
    [(purity)      (alist-affine-default 'purity 'unknown)]
    [(stability)   (alist-affine-default 'stability 'experimental)]
    [(fuel-bound)  (alist-affine-default 'fuel-bound "O(?)")]
    [(description) (alist-affine-default 'description "")]
    [(keywords)    (alist-affine-default 'keywords '())]
    [(aliases)     (alist-affine-default 'aliases '())]
    [(deps)        (alist-affine-default 'deps '())]
    [(exports)     (alist-affine-default 'exports '())]
    [(modules)     (alist-affine-default 'modules '())]
    [else          (alist-affine key)]))

;;; Named optics for most common fields (for import convenience)
(define mf-name        (manifest-field 'name))
(define mf-version     (manifest-field 'version))
(define mf-tier        (manifest-field 'tier))
(define mf-path        (manifest-field 'path))
(define mf-purity      (manifest-field 'purity))
(define mf-stability   (manifest-field 'stability))
(define mf-fuel-bound  (manifest-field 'fuel-bound))
(define mf-description (manifest-field 'description))
(define mf-keywords    (manifest-field 'keywords))
(define mf-aliases     (manifest-field 'aliases))
(define mf-deps        (manifest-field 'deps))
(define mf-exports     (manifest-field 'exports))
(define mf-modules     (manifest-field 'modules))

;;; ============================================================
;;; Part 3: Nested Access Helpers
;;; ============================================================
;;;
;;; Composition shortcuts for common nested patterns.

;;; nested-field : Symbol × Symbol → Affine Manifest Value
;;; Access a field within a nested alist field.
;;; E.g., (nested-field 'exports 'linalg) to get linalg exports group.
(define (nested-field outer inner)
  (optic-compose (manifest-field outer) (alist-affine inner)))

;;; ============================================================
;;; Part 4: Traversals for Collections
;;; ============================================================
;;;
;;; Traversals for manifest fields that contain lists.

;;; exports-each : Traversal Manifest Symbol
;;; Focus on each export symbol (flattened from groups).
(define exports-each
  (let ([flatten-exports
         (lambda (exports-raw)
           (apply append
                  (map (lambda (group)
                         (if (and (pair? group) (list? group))
                             (cdr group)
                             '()))
                       (if (list? exports-raw) exports-raw '()))))])
    (make-traversal
     ;; traverse: (a -> b) -> manifest -> manifest (read-only, identity)
     (lambda (f manifest) manifest)
     ;; fold: manifest -> List Symbol
     (lambda (manifest)
       (let ([maybe-exports (affine-preview mf-exports manifest)])
         (if (just? maybe-exports)
             (flatten-exports (from-just maybe-exports))
             '()))))))

;;; modules-each : Traversal Manifest ModuleEntry
;;; Focus on each module entry.
(define modules-each
  (make-traversal
   ;; traverse: (a -> b) -> manifest -> manifest
   (lambda (f manifest)
     (let ([maybe-modules (affine-preview mf-modules manifest)])
       (if (just? maybe-modules)
           (let* ([mods (from-just maybe-modules)]
                  [new-mods (map f (if (list? mods) mods '()))])
             (affine-set mf-modules new-mods manifest))
           manifest)))
   ;; fold: manifest -> List ModuleEntry
   (lambda (manifest)
     (let ([maybe-modules (affine-preview mf-modules manifest)])
       (if (just? maybe-modules)
           (let ([mods (from-just maybe-modules)])
             (if (list? mods) mods '()))
           '())))))

;;; keywords-each : Traversal Manifest Symbol
;;; Focus on each keyword.
(define keywords-each
  (make-traversal
   ;; traverse: (a -> b) -> manifest -> manifest
   (lambda (f manifest)
     (let ([maybe-kw (affine-preview mf-keywords manifest)])
       (if (just? maybe-kw)
           (let* ([kws (from-just maybe-kw)]
                  [new-kws (map f (if (list? kws) kws '()))])
             (affine-set mf-keywords new-kws manifest))
           manifest)))
   ;; fold: manifest -> List Symbol
   (lambda (manifest)
     (let ([maybe-kw (affine-preview mf-keywords manifest)])
       (if (just? maybe-kw)
           (let ([kws (from-just maybe-kw)])
             (if (list? kws) kws '()))
           '())))))

;;; deps-each : Traversal Manifest Symbol
;;; Focus on each dependency.
(define deps-each
  (make-traversal
   ;; traverse: (a -> b) -> manifest -> manifest
   (lambda (f manifest)
     (let ([maybe-deps (affine-preview mf-deps manifest)])
       (if (just? maybe-deps)
           (let* ([ds (from-just maybe-deps)]
                  [new-ds (map f (if (list? ds) ds '()))])
             (affine-set mf-deps new-ds manifest))
           manifest)))
   ;; fold: manifest -> List Symbol
   (lambda (manifest)
     (let ([maybe-deps (affine-preview mf-deps manifest)])
       (if (just? maybe-deps)
           (let ([ds (from-just maybe-deps)])
             (if (list? ds) ds '()))
           '())))))

;;; ============================================================
;;; Part 5: Convenience Functions
;;; ============================================================
;;;
;;; High-level functions that use optics internally.

;;; manifest-get : Manifest × Symbol → Value | #f
;;; Get a manifest field with standard defaults.
;;; Returns #f only if field is truly absent and has no default.
(define (manifest-get manifest key)
  (let ([result (affine-preview (manifest-field key) manifest)])
    (if (just? result)
        (from-just result)
        #f)))

;;; manifest-get* : Manifest × Symbol... → (Values...)
;;; Get multiple fields at once.
(define (manifest-get* manifest . keys)
  (map (lambda (k) (manifest-get manifest k)) keys))

;;; manifest-set : Manifest × Symbol × Value → Manifest
;;; Set a manifest field.
(define (manifest-set manifest key val)
  (affine-set (manifest-field key) val manifest))

;;; manifest-update : Manifest × Symbol × (α → α) → Manifest
;;; Update a manifest field with a function.
(define (manifest-update manifest key f)
  (affine-over (manifest-field key) f manifest))

;;; ============================================================
;;; Part 6: Module Entry Optics
;;; ============================================================
;;;
;;; Module entries are 3-tuples: (name filename description)
;;; These optics provide access to the components.

;;; module-name-lens : Lens ModuleEntry Symbol
(define module-name-lens
  (make-lens car (lambda (v entry) (cons v (cdr entry)))))

;;; module-file-lens : Lens ModuleEntry String
(define module-file-lens
  (make-lens
   cadr
   (lambda (v entry) (list (car entry) v (caddr entry)))))

;;; module-desc-lens : Lens ModuleEntry String
(define module-desc-lens
  (make-lens
   caddr
   (lambda (v entry) (list (car entry) (cadr entry) v))))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "manifest-optics.ss loaded.\n")
(printf "  (alist-affine 'key)              - Affine for alist key\n")
(printf "  (manifest-field 'field)          - Affine for manifest field\n")
(printf "  (manifest-get manifest 'field)   - Get field with default\n")
(printf "  (manifest-set manifest 'k v)     - Set field\n")
(printf "  mf-version, mf-tier, etc.        - Pre-built field optics\n")
(printf "  exports-each, modules-each       - Collection traversals\n")
