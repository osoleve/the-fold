;;; playpen/loom/spell/component.ss — Component Definition Macro
;;;
;;; The def-component macro generates component constructors, predicates,
;;; accessors, and setters from a declarative specification.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Exports:
;;;   def-component    Macro to define a component type with all boilerplate

;;; ====
;;; Component Definition Macro
;;; ====

;;; def-component : Name × ((Field Default)...) -> Definitions
;;;
;;; Generates:
;;;   - make-NAME-component   : Constructor (case-lambda with defaults)
;;;   - NAME-component?       : Predicate to check component type
;;;   - NAME-FIELD            : Accessor for each field
;;;   - NAME-set-FIELD        : Functional setter for each field
;;;
;;; Example:
;;;   (def-component mana
;;;     ((current 0)
;;;      (maximum 100)
;;;      (regen-rate 5)))
;;;
;;;   Generates:
;;;     (make-mana-component)           ; -> mana with defaults
;;;     (make-mana-component 50 100 10) ; -> mana with specified values
;;;     (mana-component? comp)          ; -> #t if comp is mana component
;;;     (mana-current comp)             ; -> value of current field
;;;     (mana-set-current comp val)     ; -> new component with updated current
;;;
;;; Note: Due to Scheme macro limitations, this uses syntax-case for
;;; identifier generation. Works in Chez Scheme.

(define-syntax def-component
  (lambda (stx)
          (syntax-case stx ()
                       [(_ name ((field default) ...))
                        (with-syntax
                         ([make-name
                           (datum->syntax #'name
                                          (string->symbol
                                           (string-append "make-" (symbol->string (syntax->datum #'name)) "-component")))]
                          [name?
                           (datum->syntax #'name
                                          (string->symbol
                                           (string-append (symbol->string (syntax->datum #'name)) "-component?")))]
                          [(accessor ...)
                           (map (lambda (f)
                                        (datum->syntax #'name
                                                       (string->symbol
                                                        (string-append (symbol->string (syntax->datum #'name)) "-"
                                                                       (symbol->string (syntax->datum f))))))
                                (syntax->list #'(field ...)))]
                          [(setter ...)
                           (map (lambda (f)
                                        (datum->syntax #'name
                                                       (string->symbol
                                                        (string-append (symbol->string (syntax->datum #'name)) "-set-"
                                                                       (symbol->string (syntax->datum f))))))
                                (syntax->list #'(field ...)))]
                          [type-sym (syntax->datum #'name)])
                         #'(begin
                            ;; Predicate: name-component?
                            (define (name? comp)
                              (and (component? comp)
                                   (eq? (component-type comp) 'type-sym)))
                            
                            ;; Constructor: make-name-component with case-lambda for defaults
                            (define make-name
                              (case-lambda
                               [() (make-name default ...)]
                               [(field ...)
                                `((type . type-sym)
                                  (field . ,field) ...)]))
                            
                            ;; Accessors: name-field
                            (define (accessor comp)
                              (alist-ref comp 'field default)) ...
                            
                            ;; Setters: name-set-field (functional)
                            (define (setter comp val)
                              (alist-set comp 'field val)) ...))]
                       
                       ;; Shorthand without defaults (all #f)
                       [(_ name (field ...))
                        #'(def-component name ((field #f) ...))])))


;;; ====
;;; Component Utilities
;;; ====

;;; These are re-exported from entity.ss for convenience.
;;; They're needed by def-component but defined in the core.

;;; component-type : Component -> Symbol
;;; Get the type tag of a component.
;;; (Assumes component? from entity.ss is available)

;;; component? : Any -> Bool
;;; Check if something is a component (has a type field).
;;; (Assumes defined in entity.ss)


;;; ====
;;; Example Usage (commented out)
;;; ====

#|
;;; Define a mana component
(def-component mana
  ((current 0)
   (maximum 100)
   (regen-rate 5)))

;;; This generates:
;;; (mana-component? comp)      -> predicate
;;; (make-mana-component)       -> (make-mana-component 0 100 5)
;;; (make-mana-component 50 100 10)
;;; (mana-current comp)         -> (alist-ref comp 'current 0)
;;; (mana-maximum comp)         -> (alist-ref comp 'maximum 100)
;;; (mana-regen-rate comp)      -> (alist-ref comp 'regen-rate 5)
;;; (mana-set-current comp val) -> (alist-set comp 'current val)
;;; etc.

;;; Usage:
(define my-mana (make-mana-component))
;; => ((type . mana) (current . 0) (maximum . 100) (regen-rate . 5))

(mana-current my-mana)
;; => 0

(define updated (mana-set-current my-mana 50))
;; => ((type . mana) (current . 50) (maximum . 100) (regen-rate . 5))
|#
