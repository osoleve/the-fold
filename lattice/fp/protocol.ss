;;; lattice/fp/protocol.ss — Protocol Dispatch
;;; @module protocol
;;; @requires hamt

(require 'hamt)

(doc 'module 'protocol)
(doc 'description "A lightweight dispatch mechanism allowing types to register implementations for generic operations (protocols). Enables the Open/Closed Principle: extend behavior without modifying existing code.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "Usage:
  ;; Define a protocol (creates dispatch function)
  (define-protocol (draw obj ctx) \"Draw object to context\")

  ;; Register implementations for types
  (implement-protocol! 'draw 'circle
    (lambda (c ctx) (draw-circle (circle-center c) (circle-radius c) ctx)))
  (implement-protocol! 'draw 'rectangle
    (lambda (r ctx) (draw-rect (rect-pos r) (rect-size r) ctx)))

  ;; Use the protocol (dispatches on first arg's type tag)
  (draw my-circle canvas)  ; calls circle implementation

Type Dispatch:
  Objects must be tagged lists: (list 'type-tag ...)
  Dispatch uses (car obj) to determine type.")

(doc 'dependencies '(core/base/prelude.ss))

(doc 'section 'registry)

(define *protocol-registry* hamt-empty)
(doc *protocol-registry* 'export #t)
(doc *protocol-registry* 'type '(HAMT Symbol (HAMT Symbol Procedure)))
(doc *protocol-registry* 'description "Global registry: Protocol Name -> (Type Tag -> Implementation). Structure: HAMT of HAMTs")

(define (register-protocol-impl! protocol-name type-tag impl)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol Procedure Void))
  (doc 'description "Register an implementation for a protocol and type")
  (let ([type-table (hamt-lookup protocol-name *protocol-registry*)])
    (let ([new-type-table (hamt-assoc type-tag impl (or type-table hamt-empty))])
      (set! *protocol-registry* (hamt-assoc protocol-name new-type-table *protocol-registry*)))))

(define (get-protocol-impl protocol-name type-tag)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol (Or Procedure #f)))
  (doc 'description "Look up implementation for protocol and type")
  (let ([type-table (hamt-lookup protocol-name *protocol-registry*)])
    (and type-table
         (hamt-lookup type-tag type-table))))

(define (protocol-implementations protocol-name)
  (doc 'export #t)
  (doc 'type '(-> Symbol (List Symbol)))
  (doc 'description "List all types implementing a protocol")
  (let ([type-table (hamt-lookup protocol-name *protocol-registry*)])
    (if type-table
        (hamt-keys type-table)
        '())))

(doc 'section 'type-tag-extraction)

(define (get-type-tag obj)
  (doc 'export #t)
  (doc 'type '(-> Any (Or Symbol #f)))
  (doc 'description "Extract type tag from object. Symbols are their own type tag (for empty collections like 'avl-empty). Tagged lists use (car obj). Returns #f for other values.")
  (cond
    [(symbol? obj) obj]  ; Symbols are their own type tag
    [(and (pair? obj) (symbol? (car obj))) (car obj)]
    [else #f]))

(doc 'section 'dispatch)

(define (protocol-dispatch protocol-name obj . args)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any ... Any))
  (doc 'description "Dispatch to implementation based on the first argument's type tag. Raises error if no implementation found")
  (let ([type-tag (get-type-tag obj)])
    (unless type-tag
      (error protocol-name
             (format "Cannot dispatch: object is not a tagged list")
             obj))
    (let ([impl (get-protocol-impl protocol-name type-tag)])
      (unless impl
        (error protocol-name
               (format "No implementation for type '~a'" type-tag)
               obj))
      (apply impl obj args))))

(define (protocol-dispatch/default protocol-name obj default-fn . args)
  (doc 'export #t)
  (doc 'type '(-> Symbol Any Procedure ... Any))
  (doc 'description "Like protocol-dispatch but calls default-fn if no implementation")
  (let ([type-tag (get-type-tag obj)])
    (if (not type-tag)
        (apply default-fn obj args)
        (let ([impl (get-protocol-impl protocol-name type-tag)])
          (if impl
              (apply impl obj args)
              (apply default-fn obj args))))))

(doc 'section 'macros)

(doc 'define-protocol 'type '(-> (Name Obj Args ...) (Optional DocString) Definition))
(doc 'define-protocol 'description "Define a protocol, creating a dispatch function")
(doc 'define-protocol 'example "(define-protocol (body-pos b) \"Get body position\")
  ; creates: (define (body-pos b) (protocol-dispatch 'body-pos b))")

(define-syntax define-protocol
  (syntax-rules ()
    ;; With docstring (ignored, for documentation purposes)
    [(_ (name obj args ...) docstring)
     (define (name obj args ...)
       (protocol-dispatch 'name obj args ...))]
    ;; Without docstring
    [(_ (name obj args ...))
     (define (name obj args ...)
       (protocol-dispatch 'name obj args ...))]))

(doc 'define-protocol/default 'type '(-> (Name Obj Args ...) Default-Expr Definition))
(doc 'define-protocol/default 'description "Define a protocol with a default implementation for unregistered types")

(define-syntax define-protocol/default
  (syntax-rules ()
    [(_ (name obj args ...) default-fn)
     (define (name obj args ...)
       (protocol-dispatch/default 'name obj default-fn args ...))]))

(define implement-protocol! register-protocol-impl!)
(doc 'implement-protocol! 'type '(-> Symbol Symbol Procedure Void))
(doc 'implement-protocol! 'description "Alias for register-protocol-impl! (more intuitive name)")

(doc 'section 'introspection)

(define (protocol-exists? protocol-name)
  (doc 'export #t)
  (doc 'type '(-> Symbol Boolean))
  (doc 'description "Check if a protocol has any implementations registered")
  (hamt-has-key? protocol-name *protocol-registry*))

(define (type-implements? type-tag protocol-name)
  (doc 'export #t)
  (doc 'type '(-> Symbol Symbol Boolean))
  (doc 'description "Check if a type implements a protocol")
  (and (get-protocol-impl protocol-name type-tag) #t))

(define (list-protocols)
  (doc 'export #t)
  (doc 'type '(-> (List Symbol)))
  (doc 'description "List all registered protocol names")
  (hamt-keys *protocol-registry*))

(doc 'section 'exports)
(doc 'exports "Core:
  - define-protocol      : Macro to define protocol dispatch functions
  - define-protocol/default : Protocol with default for unknown types
  - implement-protocol!  : Register implementation for type
  - protocol-dispatch    : Manual dispatch (used by macro)

Introspection:
  - get-type-tag         : Extract type tag from object
  - protocol-exists?     : Check if protocol is registered
  - type-implements?     : Check if type implements protocol
  - protocol-implementations : List types implementing a protocol
  - list-protocols       : List all protocols")

(display "  Define:    (define-protocol (name obj args...) \"doc\")\n")
(display "  Implement: (implement-protocol! 'name 'type-tag fn)\n")
(display "  Dispatch:  Automatic via (car obj) type tag\n")
