;;; lattice/fp/optics/schema.ss — Field-Level Schema Operations DSL
;;;
;;; Bidirectional operations for manipulating alist-based schemas:
;;;
;;;   - field-rename-iso : Rename a field bidirectionally
;;;   - field-add-iso : Add/remove a field with default
;;;   - field-remove-iso : Remove/add a field (inverse of add)
;;;   - field-transform-iso : Transform a field's value bidirectionally
;;;   - field-split-iso : Split one field into multiple
;;;   - field-merge-iso : Merge multiple fields into one
;;;
;;; Alists are the primary schema representation in The Fold.
;;; These operations compose with migrations to create versioned
;;; schema transformations.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - profunctor-optics.ss (for p-iso infrastructure)

(load "lattice/fp/optics/profunctor-optics.ss")

;;; ============================================================
;;; Part 1: Field Rename
;;; ============================================================

;;; field-rename-iso : Symbol -> Symbol -> PIso Alist Alist
;;; Create an iso that renames a field.
;;; Forward: old-name -> new-name
;;; Backward: new-name -> old-name
(define (field-rename-iso old-name new-name)
  (make-p-iso
   ;; Forward: rename old-name to new-name
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) old-name)
                (cons new-name (cdr pair))
                pair))
          alist))
   ;; Backward: rename new-name back to old-name
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) new-name)
                (cons old-name (cdr pair))
                pair))
          alist))))

;;; ============================================================
;;; Part 2: Field Add/Remove
;;; ============================================================

;;; field-add-iso : Symbol -> Any -> PIso Alist Alist
;;; Create an iso that adds a field with a default value.
;;; Forward: add field with default at the front
;;; Backward: remove the field
;;;
;;; Note: The default is used when migrating forward.
;;; Rolling back simply removes the field (data may be lost).
(define (field-add-iso field default)
  (make-p-iso
   ;; Forward: add field at front
   (lambda (alist)
     (cons (cons field default) alist))
   ;; Backward: remove field
   (lambda (alist)
     (filter (lambda (pair) (not (eq? (car pair) field))) alist))))

;;; field-remove-iso : Symbol -> Any -> PIso Alist Alist
;;; Create an iso that removes a field (inverse of add).
;;; Forward: remove the field
;;; Backward: add field with default
(define (field-remove-iso field default)
  (make-p-iso
   ;; Forward: remove field
   (lambda (alist)
     (filter (lambda (pair) (not (eq? (car pair) field))) alist))
   ;; Backward: add field at front
   (lambda (alist)
     (cons (cons field default) alist))))

;;; ============================================================
;;; Part 3: Field Value Transformation
;;; ============================================================

;;; field-transform-iso : Symbol -> PIso a b -> PIso Alist Alist
;;; Create an iso that transforms a specific field's value.
;;; The value-iso transforms the field value bidirectionally.
(define (field-transform-iso field value-iso)
  (make-p-iso
   ;; Forward: apply value-iso's forward to field value
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) field)
                (cons field ((p-iso-forward value-iso) (cdr pair)))
                pair))
          alist))
   ;; Backward: apply value-iso's backward to field value
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) field)
                (cons field ((p-iso-backward value-iso) (cdr pair)))
                pair))
          alist))))

;;; field-transform-if-present-iso : Symbol -> PIso a b -> PIso Alist Alist
;;; Like field-transform-iso but safely handles missing fields.
(define (field-transform-if-present-iso field value-iso)
  (make-p-iso
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) field)
                (cons field ((p-iso-forward value-iso) (cdr pair)))
                pair))
          alist))
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) field)
                (cons field ((p-iso-backward value-iso) (cdr pair)))
                pair))
          alist))))

;;; ============================================================
;;; Part 4: Field Split/Merge
;;; ============================================================

;;; field-split-iso : Symbol -> (List Symbol) -> (a -> Alist) -> (Alist -> a)
;;;                   -> PIso Alist Alist
;;; Split one field into multiple fields.
;;; - source-field: the field to split
;;; - target-fields: names of new fields
;;; - splitter: function to split value into alist
;;; - merger: function to merge alist back into single value
(define (field-split-iso source-field target-fields splitter merger)
  (make-p-iso
   ;; Forward: remove source, add targets
   (lambda (alist)
     (let* ([pair (assq source-field alist)]
            [value (if pair (cdr pair) #f)])
       (if value
           (let ([split-values (splitter value)])
             (append
              split-values
              (filter (lambda (p) (not (eq? (car p) source-field))) alist)))
           alist)))
   ;; Backward: remove targets, add source
   (lambda (alist)
     (let ([target-pairs (filter (lambda (p) (memq (car p) target-fields)) alist)])
       (if (= (length target-pairs) (length target-fields))
           (cons (cons source-field (merger target-pairs))
                 (filter (lambda (p) (not (memq (car p) target-fields))) alist))
           alist)))))

;;; field-merge-iso : (List Symbol) -> Symbol -> (Alist -> a) -> (a -> Alist)
;;;                   -> PIso Alist Alist
;;; Merge multiple fields into one (inverse of split).
(define (field-merge-iso source-fields target-field merger splitter)
  (make-p-iso
   ;; Forward: remove sources, add target
   (lambda (alist)
     (let ([source-pairs (filter (lambda (p) (memq (car p) source-fields)) alist)])
       (if (= (length source-pairs) (length source-fields))
           (cons (cons target-field (merger source-pairs))
                 (filter (lambda (p) (not (memq (car p) source-fields))) alist))
           alist)))
   ;; Backward: remove target, add sources
   (lambda (alist)
     (let* ([pair (assq target-field alist)]
            [value (if pair (cdr pair) #f)])
       (if value
           (let ([split-values (splitter value)])
             (append
              split-values
              (filter (lambda (p) (not (eq? (car p) target-field))) alist)))
           alist)))))

;;; ============================================================
;;; Part 5: Field Move/Reorder
;;; ============================================================

;;; field-move-to-front-iso : Symbol -> PIso Alist Alist
;;; Move a field to the front of the alist.
;;; Backward doesn't restore position (just identity).
(define (field-move-to-front-iso field)
  (make-p-iso
   (lambda (alist)
     (let ([pair (assq field alist)])
       (if pair
           (cons pair (filter (lambda (p) (not (eq? (car p) field))) alist))
           alist)))
   identity))  ; Can't restore original position, just identity

;;; ============================================================
;;; Part 6: Nested Field Operations
;;; ============================================================

;;; nested-field-iso : (List Symbol) -> PIso a b -> PIso Alist Alist
;;; Transform a nested field value.
;;; Path is a list of symbols representing the path to the field.
(define (nested-field-iso path value-iso)
  (if (null? path)
      value-iso
      (make-p-iso
       (lambda (alist)
         (let* ([field (car path)]
                [rest-path (cdr path)])
           (map (lambda (pair)
                  (if (eq? (car pair) field)
                      (cons field
                            (if (null? rest-path)
                                ((p-iso-forward value-iso) (cdr pair))
                                ((p-iso-forward (nested-field-iso rest-path value-iso))
                                 (cdr pair))))
                      pair))
                alist)))
       (lambda (alist)
         (let* ([field (car path)]
                [rest-path (cdr path)])
           (map (lambda (pair)
                  (if (eq? (car pair) field)
                      (cons field
                            (if (null? rest-path)
                                ((p-iso-backward value-iso) (cdr pair))
                                ((p-iso-backward (nested-field-iso rest-path value-iso))
                                 (cdr pair))))
                      pair))
                alist))))))

;;; ============================================================
;;; Part 7: Field Default Value
;;; ============================================================

;;; field-ensure-iso : Symbol -> Any -> PIso Alist Alist
;;; Ensure a field exists with default if missing.
;;; Forward: add field if missing
;;; Backward: identity (don't remove field that might have been present)
(define (field-ensure-iso field default)
  (make-p-iso
   (lambda (alist)
     (if (assq field alist)
         alist
         (cons (cons field default) alist)))
   identity))

;;; field-with-default-iso : Symbol -> Any -> PIso (a | #f) a
;;; Provide default for a field value.
;;; Used with field-transform-iso when field might be missing.
(define (field-with-default-iso default)
  (make-p-iso
   (lambda (v) (if v v default))
   identity))

;;; ============================================================
;;; Part 8: Bulk Operations
;;; ============================================================

;;; fields-rename-iso : Alist -> PIso Alist Alist
;;; Rename multiple fields. Mapping is (old-name . new-name).
(define (fields-rename-iso mapping)
  (make-p-iso
   (lambda (alist)
     (map (lambda (pair)
            (let ([rename (assq (car pair) mapping)])
              (if rename
                  (cons (cdr rename) (cdr pair))
                  pair)))
          alist))
   (lambda (alist)
     (let ([reverse-mapping (map (lambda (p) (cons (cdr p) (car p))) mapping)])
       (map (lambda (pair)
              (let ([rename (assq (car pair) reverse-mapping)])
                (if rename
                    (cons (cdr rename) (cdr pair))
                    pair)))
            alist)))))

;;; fields-keep-only-iso : (List Symbol) -> PIso Alist Alist
;;; Keep only specified fields (destructive forward, cannot restore).
;;; Backward is identity since removed fields are gone.
(define (fields-keep-only-iso fields)
  (make-p-iso
   (lambda (alist)
     (filter (lambda (pair) (memq (car pair) fields)) alist))
   identity))

;;; fields-remove-iso : (List Symbol) -> PIso Alist Alist
;;; Remove specified fields (destructive, cannot restore).
(define (fields-remove-iso fields)
  (make-p-iso
   (lambda (alist)
     (filter (lambda (pair) (not (memq (car pair) fields))) alist))
   identity))

;;; ============================================================
;;; Part 9: Schema Composition
;;; ============================================================

;;; schema-compose : (List PIso) -> PIso Alist Alist
;;; Compose multiple schema operations into one.
(define (schema-compose isos)
  (if (null? isos)
      p-iso-id
      (fold-left p-iso-compose (car isos) (cdr isos))))

;;; ============================================================
;;; Part 10: Type Coercion Helpers
;;; ============================================================

;;; field-coerce-to-string-iso : Symbol -> PIso Alist Alist
;;; Convert a field's value to string representation.
(define (field-coerce-to-string-iso field)
  (field-transform-iso field
    (make-p-iso
     (lambda (v)
       (cond
         [(string? v) v]
         [(number? v) (number->string v)]
         [(symbol? v) (symbol->string v)]
         [(boolean? v) (if v "true" "false")]
         [else (format "~s" v)]))
     identity)))  ; String->original type is lossy

;;; field-coerce-to-number-iso : Symbol -> PIso Alist Alist
;;; Convert a field's value to number.
(define (field-coerce-to-number-iso field)
  (field-transform-iso field
    (make-p-iso
     (lambda (v)
       (cond
         [(number? v) v]
         [(string? v) (or (string->number v) 0)]
         [else 0]))
     number->string)))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Core Operations:
;;;   field-rename-iso, field-add-iso, field-remove-iso
;;;   field-transform-iso, field-transform-if-present-iso
;;;
;;; Split/Merge:
;;;   field-split-iso, field-merge-iso
;;;
;;; Reorder:
;;;   field-move-to-front-iso
;;;
;;; Nested:
;;;   nested-field-iso
;;;
;;; Defaults:
;;;   field-ensure-iso, field-with-default-iso
;;;
;;; Bulk:
;;;   fields-rename-iso, fields-keep-only-iso, fields-remove-iso
;;;
;;; Composition:
;;;   schema-compose
;;;
;;; Coercion:
;;;   field-coerce-to-string-iso, field-coerce-to-number-iso

(display "Loaded: lattice/fp/optics/schema.ss\n")
