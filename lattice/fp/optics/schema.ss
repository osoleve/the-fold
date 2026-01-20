(load "lattice/fp/optics/profunctor-optics.ss")

(doc 'module 'schema)
(doc 'description "Field-Level Schema Operations DSL - Bidirectional operations for manipulating alist-based schemas")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'field-rename)

(define (field-rename-iso old-name new-name)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol (PIso Alist Alist)))
  (doc 'description "Create an iso that renames a field. Forward: old-name -> new-name, Backward: new-name -> old-name")
  (make-p-iso
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) old-name)
                (cons new-name (cdr pair))
                pair))
          alist))
   (lambda (alist)
     (map (lambda (pair)
            (if (eq? (car pair) new-name)
                (cons old-name (cdr pair))
                pair))
          alist))))

(doc 'section 'field-add-remove)

(define (field-add-iso field default)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any (PIso Alist Alist)))
  (doc 'description "Create an iso that adds a field with a default value. Forward: add field with default at the front (idempotent: skips if exists). Backward: remove the field (only if value equals default)")
  (doc 'note "Idempotent - if field already exists, no change is made. This ensures roundtrip safety: rollback(migrate(x)) = x")
  (make-p-iso
   (lambda (alist)
     (if (assq field alist)
         alist
         (cons (cons field default) alist)))
   (lambda (alist)
     (let ([existing (assq field alist)])
       (if (and existing (equal? (cdr existing) default))
           (filter (lambda (pair) (not (eq? (car pair) field))) alist)
           alist)))))

(define (field-remove-iso field default)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any (PIso Alist Alist)))
  (doc 'description "Create an iso that removes a field (inverse of add). Forward: remove the field. Backward: add field with default")
  (make-p-iso
   (lambda (alist)
     (filter (lambda (pair) (not (eq? (car pair) field))) alist))
   (lambda (alist)
     (cons (cons field default) alist))))

(doc 'section 'field-transform)

(define (field-transform-iso field value-iso)
  (doc 'export #t)
  (doc 'type '(-> Symbol PIso (PIso Alist Alist)))
  (doc 'description "Create an iso that transforms a specific field's value. The value-iso transforms the field value bidirectionally")
  (doc 'note "Errors if field is not present. Use field-transform-if-present-iso for optional fields")
  (make-p-iso
   (lambda (alist)
     (unless (assq field alist)
       (error 'field-transform-iso "field not found" field))
     (map (lambda (pair)
            (if (eq? (car pair) field)
                (cons field ((p-iso-forward value-iso) (cdr pair)))
                pair))
          alist))
   (lambda (alist)
     (unless (assq field alist)
       (error 'field-transform-iso "field not found" field))
     (map (lambda (pair)
            (if (eq? (car pair) field)
                (cons field ((p-iso-backward value-iso) (cdr pair)))
                pair))
          alist))))

(define (field-transform-if-present-iso field value-iso)
  (doc 'export #t)
  (doc 'type '(-> Symbol PIso (PIso Alist Alist)))
  (doc 'description "Like field-transform-iso but safely skips if field is missing. Use this for optional fields where absence is valid")
  (make-p-iso
   (lambda (alist)
     (if (assq field alist)
         (map (lambda (pair)
                (if (eq? (car pair) field)
                    (cons field ((p-iso-forward value-iso) (cdr pair)))
                    pair))
              alist)
         alist))
   (lambda (alist)
     (if (assq field alist)
         (map (lambda (pair)
                (if (eq? (car pair) field)
                    (cons field ((p-iso-backward value-iso) (cdr pair)))
                    pair))
              alist)
         alist))))

(doc 'section 'field-split-merge)

(define (field-split-iso source-field target-fields splitter merger)
  (doc 'export #t)
  (doc 'type '(-> Symbol (List Symbol) (-> a Alist) (-> Alist a) (PIso Alist Alist)))
  (doc 'description "Split one field into multiple fields")
  (doc 'param 'source-field "the field to split")
  (doc 'param 'target-fields "names of new fields")
  (doc 'param 'splitter "function to split value into alist")
  (doc 'param 'merger "function to merge alist back into single value")
  (make-p-iso
   (lambda (alist)
     (let* ([pair (assq source-field alist)]
            [value (if pair (cdr pair) #f)])
       (if value
           (let ([split-values (splitter value)])
             (append
              split-values
              (filter (lambda (p) (not (eq? (car p) source-field))) alist)))
           alist)))
   (lambda (alist)
     (let ([target-pairs (filter (lambda (p) (memq (car p) target-fields)) alist)])
       (if (= (length target-pairs) (length target-fields))
           (cons (cons source-field (merger target-pairs))
                 (filter (lambda (p) (not (memq (car p) target-fields))) alist))
           alist)))))

(define (field-merge-iso source-fields target-field merger splitter)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) Symbol (-> Alist a) (-> a Alist) (PIso Alist Alist)))
  (doc 'description "Merge multiple fields into one (inverse of split)")
  (make-p-iso
   (lambda (alist)
     (let ([source-pairs (filter (lambda (p) (memq (car p) source-fields)) alist)])
       (if (= (length source-pairs) (length source-fields))
           (cons (cons target-field (merger source-pairs))
                 (filter (lambda (p) (not (memq (car p) source-fields))) alist))
           alist)))
   (lambda (alist)
     (let* ([pair (assq target-field alist)]
            [value (if pair (cdr pair) #f)])
       (if value
           (let ([split-values (splitter value)])
             (append
              split-values
              (filter (lambda (p) (not (eq? (car p) target-field))) alist)))
           alist)))))

(doc 'section 'field-move)

(define (field-move-to-front-iso field)
  (doc 'export #t)
  (doc 'type '(-> Symbol (PIso Alist Alist)))
  (doc 'description "Move a field to the front of the alist")
  (doc 'note "Backward doesn't restore position (just identity)")
  (make-p-iso
   (lambda (alist)
     (let ([pair (assq field alist)])
       (if pair
           (cons pair (filter (lambda (p) (not (eq? (car p) field))) alist))
           alist)))
   identity))

(doc 'section 'nested-fields)

(define (nested-field-iso path value-iso)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) PIso (PIso Alist Alist)))
  (doc 'description "Transform a nested field value. Path is a list of symbols representing the path to the field")
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

(doc 'section 'field-defaults)

(define (field-ensure-iso field default)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any (PIso Alist Alist)))
  (doc 'description "Ensure a field exists with default if missing. Forward: add field if missing. Backward: identity (don't remove field that might have been present)")
  (make-p-iso
   (lambda (alist)
     (if (assq field alist)
         alist
         (cons (cons field default) alist)))
   identity))

(define (field-with-default-iso default)
  (doc 'export #t)
  (doc 'type '(-> Any (PIso (Maybe a) a)))
  (doc 'description "Provide default for a field value. Used with field-transform-iso when field might be missing")
  (make-p-iso
   (lambda (v) (if v v default))
   identity))

(doc 'section 'bulk-operations)

(define (fields-rename-iso mapping)
  (doc 'export #t)
  (doc 'type '(-> Alist (PIso Alist Alist)))
  (doc 'description "Rename multiple fields. Mapping is (old-name . new-name)")
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

(define (fields-keep-only-iso fields)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) (PIso Alist Alist)))
  (doc 'description "Keep only specified fields (destructive forward, cannot restore). Backward is identity since removed fields are gone")
  (make-p-iso
   (lambda (alist)
     (filter (lambda (pair) (memq (car pair) fields)) alist))
   identity))

(define (fields-remove-iso fields)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol) (PIso Alist Alist)))
  (doc 'description "Remove specified fields (destructive, cannot restore)")
  (make-p-iso
   (lambda (alist)
     (filter (lambda (pair) (not (memq (car pair) fields))) alist))
   identity))

(doc 'section 'composition)

(define (schema-compose isos)
  (doc 'export #t)
  (doc 'type '(-> (List PIso) (PIso Alist Alist)))
  (doc 'description "Compose multiple schema operations into one")
  (if (null? isos)
      p-iso-id
      (fold-left p-iso-compose (car isos) (cdr isos))))

(doc 'section 'type-coercion)

(define (field-coerce-to-string-iso field)
  (doc 'export #t)
  (doc 'type '(-> Symbol (PIso Alist Alist)))
  (doc 'description "Convert a field's value to string representation")
  (field-transform-iso field
    (make-p-iso
     (lambda (v)
       (cond
         [(string? v) v]
         [(number? v) (number->string v)]
         [(symbol? v) (symbol->string v)]
         [(boolean? v) (if v "true" "false")]
         [else (format "~s" v)]))
     identity)))

(define (field-coerce-to-number-iso field)
  (doc 'export #t)
  (doc 'type '(-> Symbol (PIso Alist Alist)))
  (doc 'description "Convert a field's value to number")
  (field-transform-iso field
    (make-p-iso
     (lambda (v)
       (cond
         [(number? v) v]
         [(string? v) (or (string->number v) 0)]
         [else 0]))
     number->string)))

(display "Loaded: lattice/fp/optics/schema.ss\n")
