;;; lattice/fp/protocol-bundle.ss — Protocol Bundles
;;; @module protocol-bundle
;;; @requires protocol

(require 'protocol)

(doc 'module 'protocol-bundle)
(doc 'purity 'total)
(doc 'description "Bundle Data Structures A bundle is metadata describing a set of protocol pairs (getter/setter). Structure: (protocol-bundle name (slot ...)) Each slot: (bundle-slot getter-proto setter-proto label) make-protocol-bundle : Symbol × (List Slot) → Bundle")
(doc 'layer 'lattice)
(define (make-protocol-bundle name slots)
  (list 'protocol-bundle name slots))

;;; bundle? : Any → Boolean
(define (bundle? x)
  (and (pair? x)
       (eq? 'protocol-bundle (car x))))

;;; bundle-name : Bundle → Symbol
(define (bundle-name b)
  (cadr b))

;;; bundle-slots : Bundle → (List Slot)
(define (bundle-slots b)
  (caddr b))

;;; make-bundle-slot : Symbol × Symbol × String → Slot
(define (make-bundle-slot getter-proto setter-proto label)
  (list 'bundle-slot getter-proto setter-proto label))

;;; slot? : Any → Boolean
(define (slot? x)
  (and (pair? x)
       (eq? 'bundle-slot (car x))))

;;; slot-getter : Slot → Symbol
(define (slot-getter s)
  (cadr s))

;;; slot-setter : Slot → Symbol
(define (slot-setter s)
  (caddr s))

;;; slot-label : Slot → String
(define (slot-label s)
  (cadddr s))

;;; ====
;;; Bundle Registry (for introspection)
;;; ====

(define *bundle-registry* (make-hashtable symbol-hash eq?))

;;; register-bundle! : Bundle → Void
(define (register-bundle! bundle)
  (hashtable-set! *bundle-registry* (bundle-name bundle) bundle))

;;; get-bundle : Symbol → Bundle | #f
(define (get-bundle name)
  (hashtable-ref *bundle-registry* name #f))

;;; list-bundles : → (List Symbol)
(define (list-bundles)
  (vector->list (hashtable-keys *bundle-registry*)))

;;; ====
;;; Bundle Definition Macro
;;; ====

;;; define-protocol-bundle : Name Slot-Spec ... → Definition
;;;
;;; Defines a bundle of related protocol pairs. The protocols themselves
;;; must be defined separately via define-protocol.
;;;
;;; Slot-Spec: ((getter-proto setter-proto) label)
;;;   - getter-proto: Symbol naming the getter protocol
;;;   - setter-proto: Symbol naming the setter protocol
;;;   - label: String identifying the field (used for naming convention lookup)
;;;
;;; Example:
;;;   (define-protocol-bundle body-ops
;;;     ((body-pos body-set-pos) "pos")
;;;     ((body-vel body-set-vel) "vel"))
;;;
;;; Creates:
;;;   - A bundle definition bound to 'body-ops'
;;;   - Registers the bundle in *bundle-registry* for introspection
(define-syntax define-protocol-bundle
  (syntax-rules ()
    [(_ name ((getter setter) label) ...)
     (begin
       (define name
         (make-protocol-bundle 'name
           (list (make-bundle-slot 'getter 'setter label) ...)))
       (register-bundle! name))]))

;;; ====
;;; Bundle Implementation Macros
;;; ====

;;; Helper: Build getter name from prefix and label
;;; Convention: <prefix>-<label> (e.g., rigid-body-pos)
(define (build-getter-name prefix label)
  (string->symbol
   (string-append (symbol->string prefix) "-" label)))

;;; Helper: Build setter name from prefix and label
;;; Convention: <prefix>-with-<label> (e.g., rigid-body-with-pos)
(define (build-setter-name prefix label)
  (string->symbol
   (string-append (symbol->string prefix) "-with-" label)))

;;; derive-bundle! : Bundle × TypeTag × Prefix [× Override ...] → Void
;;;
;;; Register protocol implementations using naming convention.
;;; For each slot with label L and prefix P:
;;;   - Getter: looks up symbol P-L
;;;   - Setter: looks up symbol P-with-L and wraps as (lambda (obj val) (P-with-L obj val))
;;;
;;; Overrides: (label getter-fn setter-fn)
;;;   Specify explicit functions for slots that don't follow the convention.
;;;
;;; Usage:
;;;   (derive-bundle! body-ops 'rigid-body-2d rigid-body)
;;;   (derive-bundle! body-ops 'particle particle ("mass" const-getter no-op))
;;;
;;; Note: type-tag is quoted (e.g., 'car), prefix is unquoted (e.g., car)
;;;
;;; Overrides are 3-tuples: (label getter-expr setter-expr)
;;; They're converted to lists at expansion time.
(define-syntax derive-bundle!
  (lambda (stx)
    (syntax-case stx ()
      ;; Match bundle, type-tag, prefix, and any number of override triples
      [(_ bundle-expr type-tag prefix (label getter setter) ...)
       #'(derive-bundle-runtime! bundle-expr type-tag (quote prefix)
           (list (list label getter setter) ...))])))

;;; derive-bundle-runtime! : Bundle × Symbol × Symbol × (List Override) → Void
;;;
;;; Runtime implementation of derive-bundle!.
;;; Looks up functions by name at expansion time using the naming convention.
(define (derive-bundle-runtime! bundle type-tag prefix overrides)
  (let ([slots (bundle-slots bundle)])
    (for-each
     (lambda (slot)
       (let* ([label (slot-label slot)]
              [getter-proto (slot-getter slot)]
              [setter-proto (slot-setter slot)]
              [override (find-override label overrides)])
         (if override
             ;; Use explicit override
             (begin
               (implement-protocol! getter-proto type-tag (cadr override))
               (implement-protocol! setter-proto type-tag (caddr override)))
             ;; Use naming convention (eval required for runtime symbol lookup)
             ;; Note: We eval both names once at registration time, not per-call
             (let* ([getter-name (build-getter-name prefix label)]
                    [setter-name (build-setter-name prefix label)]
                    [getter-fn (eval getter-name)]
                    [raw-setter-fn (eval setter-name)]
                    [setter-fn (lambda (obj val) (raw-setter-fn obj val))])
               (implement-protocol! getter-proto type-tag getter-fn)
               (implement-protocol! setter-proto type-tag setter-fn)))))
     slots)))

;;; find-override : String × (List Override) → Override | #f
;;; Find override for a given label.
(define (find-override label overrides)
  (cond
   [(null? overrides) #f]
   [(string=? label (car (car overrides))) (car overrides)]
   [else (find-override label (cdr overrides))]))

;;; implement-bundle! : Bundle × TypeTag × Impl-Spec ... → Void
;;;
;;; Register protocol implementations with explicit function mappings.
;;; Use when naming conventions don't apply.
;;;
;;; Impl-Spec: (label getter-fn setter-fn)
;;;
;;; Example:
;;;   (implement-bundle! body-ops 'custom-body
;;;     ("pos" custom-get-pos custom-set-pos)
;;;     ("vel" custom-get-vel custom-set-vel)
;;;     ("mass" custom-get-mass custom-set-mass))
;;;
;;; Note: type-tag is quoted (e.g., 'scooter)
(define-syntax implement-bundle!
  (syntax-rules ()
    [(_ bundle-expr type-tag (label getter setter) ...)
     (implement-bundle-runtime! bundle-expr type-tag
       (list (list label getter setter) ...))]))

;;; implement-bundle-runtime! : Bundle × Symbol × (List Impl-Spec) → Void
;;;
;;; Runtime implementation of implement-bundle!.
(define (implement-bundle-runtime! bundle type-tag impl-specs)
  (let ([slots (bundle-slots bundle)])
    ;; Verify all slots are covered
    (for-each
     (lambda (slot)
       (let* ([label (slot-label slot)]
              [impl (find-impl-spec label impl-specs)])
         (unless impl
           (error 'implement-bundle!
                  (format "Missing implementation for slot '~a'" label)))))
     slots)
    ;; Register implementations
    (for-each
     (lambda (impl-spec)
       (let* ([label (car impl-spec)]
              [getter-fn (cadr impl-spec)]
              [setter-fn (caddr impl-spec)]
              [slot (find-slot-by-label label slots)])
         (unless slot
           (error 'implement-bundle!
                  (format "Unknown slot label '~a'" label)))
         (implement-protocol! (slot-getter slot) type-tag getter-fn)
         (implement-protocol! (slot-setter slot) type-tag setter-fn)))
     impl-specs)))

;;; find-impl-spec : String × (List Impl-Spec) → Impl-Spec | #f
(define (find-impl-spec label specs)
  (cond
   [(null? specs) #f]
   [(string=? label (car (car specs))) (car specs)]
   [else (find-impl-spec label (cdr specs))]))

;;; find-slot-by-label : String × (List Slot) → Slot | #f
(define (find-slot-by-label label slots)
  (cond
   [(null? slots) #f]
   [(string=? label (slot-label (car slots))) (car slots)]
   [else (find-slot-by-label label (cdr slots))]))

;;; ====
;;; Introspection
;;; ====

;;; bundle-types : Bundle → (List Symbol)
;;; List all types that have registered implementations for all slots of a bundle.
(define (bundle-types bundle)
  (let* ([slots (bundle-slots bundle)]
         [first-slot (car slots)]
         [first-proto (slot-getter first-slot)]
         [candidates (protocol-implementations first-proto)])
    ;; Filter to types that implement ALL slots
    (filter
     (lambda (type-tag)
       (and-map (lambda (slot)
                  (type-implements? type-tag (slot-getter slot)))
                slots))
     candidates)))

;;; bundle-protocols : Bundle → (List Symbol)
;;; List all protocol names in a bundle (both getters and setters).
(define (bundle-protocols bundle)
  (let ([slots (bundle-slots bundle)])
    (append
     (map slot-getter slots)
     (map slot-setter slots))))

;;; bundle-slot-count : Bundle → Number
(define (bundle-slot-count bundle)
  (length (bundle-slots bundle)))

;;; ====
;;; Type Requirements (Composable Constraints)
;;; ====

;;; implements-bundle? : Symbol × Bundle → Boolean
;;; Check if a type tag implements all protocols in a bundle.
;;;
;;; Example:
;;;   (implements-bundle? 'rigid-body-2d body-ops) → #t
;;;   (implements-bundle? 'string body-ops) → #f
(define (implements-bundle? type-tag bundle)
  (let ([slots (bundle-slots bundle)])
    (and-map (lambda (slot)
               (and (type-implements? type-tag (slot-getter slot))
                    (type-implements? type-tag (slot-setter slot))))
             slots)))

;;; compose-bundles : Symbol × Bundle ... → Bundle
;;; Combine multiple bundles into a new named bundle.
;;; Slots are merged; duplicates (by label) are kept from the first bundle.
;;;
;;; Example:
;;;   (define movable-ops ...)
;;;   (define drawable-ops ...)
;;;   (define game-object-ops (compose-bundles 'game-object-ops movable-ops drawable-ops))
(define (compose-bundles name . bundles)
  (let* ([all-slots (append-map bundle-slots bundles)]
         [unique-slots (dedupe-slots all-slots)])
    (let ([composed (make-protocol-bundle name unique-slots)])
      (register-bundle! composed)
      composed)))

;;; dedupe-slots : (List Slot) → (List Slot)
;;; Remove duplicate slots by label, keeping first occurrence.
(define (dedupe-slots slots)
  (let loop ([remaining slots] [seen '()] [result '()])
    (cond
     [(null? remaining) (reverse result)]
     [(member (slot-label (car remaining)) seen)
      (loop (cdr remaining) seen result)]
     [else
      (loop (cdr remaining)
            (cons (slot-label (car remaining)) seen)
            (cons (car remaining) result))])))

;;; assert-bundle! : Any × Bundle → Any | Error
;;; Assert that a value's type implements a bundle. Returns the value if ok.
;;; Throws an error if the type doesn't implement all bundle protocols.
;;;
;;; Example:
;;;   (define (move-body body delta)
;;;     (assert-bundle! body body-ops)  ; Validates body type
;;;     (body-set-pos body (vec2-add (body-pos body) delta)))
(define (assert-bundle! value bundle)
  (let ([type-tag (get-type-tag value)])
    (if (implements-bundle? type-tag bundle)
        value
        (error 'assert-bundle!
               (format "Type '~a' does not implement bundle '~a'. Missing protocols: ~a"
                       type-tag
                       (bundle-name bundle)
                       (missing-protocols type-tag bundle))))))

;;; missing-protocols : Symbol × Bundle → (List Symbol)
;;; List protocols from a bundle that a type doesn't implement.
(define (missing-protocols type-tag bundle)
  (let ([slots (bundle-slots bundle)])
    (filter
     (lambda (proto)
       (not (type-implements? type-tag proto)))
     (bundle-protocols bundle))))

;;; ====
;;; Helper: and-map
;;; ====

(define (and-map pred lst)
  (cond
   [(null? lst) #t]
   [(pred (car lst)) (and-map pred (cdr lst))]
   [else #f]))

;;; ====
;;; Print Help
;;; ====

(display "  Define:    (define-protocol-bundle name ((getter setter) label) ...)\n")
(display "  Derive:    (derive-bundle! bundle 'type-tag prefix [(label get set) ...])\n")
(display "  Implement: (implement-bundle! bundle 'type-tag (label get set) ...)\n")
(display "  Compose:   (compose-bundles 'name bundle1 bundle2 ...)\n")
(display "  Check:     (implements-bundle? 'type-tag bundle)\n")
(display "  Assert:    (assert-bundle! value bundle)\n")
(display "  Inspect:   (bundle-types bundle), (bundle-protocols bundle)\n")
