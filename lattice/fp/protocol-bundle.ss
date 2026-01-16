;;; lattice/fp/protocol-bundle.ss — Protocol Bundle System
;;;
;;; Reduces boilerplate when implementing multiple related protocols for a type.
;;; Enables registering 6+ protocol implementations with a single call.
;;;
;;; Usage:
;;;   ;; Define a bundle of related protocols (metadata only)
;;;   (define-protocol-bundle body-ops
;;;     ((body-pos body-set-pos) "pos")
;;;     ((body-vel body-set-vel) "vel")
;;;     ((body-mass body-set-mass) "mass"))
;;;
;;;   ;; Derive from naming convention: <prefix>-<field>, <prefix>-with-<field>
;;;   (derive-bundle! body-ops 'rigid-body-2d rigid-body)
;;;
;;;   ;; Derive with overrides for semantic exceptions
;;;   (derive-bundle! body-ops 'particle particle
;;;     ("mass" (lambda (p) 1.0) (lambda (p m) p)))
;;;
;;;   ;; Explicit implementation (when convention doesn't apply)
;;;   (implement-bundle! body-ops 'custom-body
;;;     ("pos" custom-get-pos custom-set-pos)
;;;     ("vel" custom-get-vel custom-set-vel)
;;;     ("mass" custom-get-mass custom-set-mass))
;;;
;;; Design Principles:
;;;   - Bundles are metadata: protocols must be defined separately via define-protocol
;;;   - derive-bundle! uses naming conventions for minimal boilerplate
;;;   - implement-bundle! provides explicit control when conventions don't apply
;;;   - Compile-time symbol construction via syntax-case (not runtime eval)
;;;
;;; Dependencies:
;;;   - lattice/fp/protocol.ss

(load "lattice/fp/protocol.ss")

;;; ====
;;; Bundle Data Structures
;;; ====

;;; A bundle is metadata describing a set of protocol pairs (getter/setter).
;;; Structure: (protocol-bundle name (slot ...))
;;; Each slot: (bundle-slot getter-proto setter-proto label)

;;; make-protocol-bundle : Symbol × (List Slot) → Bundle
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
  (syntax-rules ()
    ;; No overrides
    [(_ bundle-expr type-tag prefix)
     (derive-bundle-runtime! bundle-expr type-tag (quote prefix) '())]
    ;; With one override
    [(_ bundle-expr type-tag prefix (label1 getter1 setter1))
     (derive-bundle-runtime! bundle-expr type-tag (quote prefix)
       (list (list label1 getter1 setter1)))]
    ;; With two overrides
    [(_ bundle-expr type-tag prefix (label1 getter1 setter1) (label2 getter2 setter2))
     (derive-bundle-runtime! bundle-expr type-tag (quote prefix)
       (list (list label1 getter1 setter1)
             (list label2 getter2 setter2)))]
    ;; With three overrides
    [(_ bundle-expr type-tag prefix (label1 getter1 setter1) (label2 getter2 setter2) (label3 getter3 setter3))
     (derive-bundle-runtime! bundle-expr type-tag (quote prefix)
       (list (list label1 getter1 setter1)
             (list label2 getter2 setter2)
             (list label3 getter3 setter3)))]))

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

(display "protocol-bundle.ss loaded.\n")
(display "  Define:    (define-protocol-bundle name ((getter setter) label) ...)\n")
(display "  Derive:    (derive-bundle! bundle 'type-tag prefix [(label get set) ...])\n")
(display "  Implement: (implement-bundle! bundle 'type-tag (label get set) ...)\n")
(display "  Inspect:   (bundle-types bundle), (bundle-protocols bundle)\n")
