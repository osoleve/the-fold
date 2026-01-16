;;; lattice/fp/protocol.ss — Open Protocol System
;;;
;;; A lightweight dispatch mechanism allowing types to register
;;; implementations for generic operations (protocols). Enables the
;;; Open/Closed Principle: extend behavior without modifying existing code.
;;;
;;; This is Lattice code: pure after load-time registration.
;;;
;;; Usage:
;;;   ;; Define a protocol (creates dispatch function)
;;;   (define-protocol (draw obj ctx) "Draw object to context")
;;;
;;;   ;; Register implementations for types
;;;   (implement-protocol! 'draw 'circle
;;;     (lambda (c ctx) (draw-circle (circle-center c) (circle-radius c) ctx)))
;;;   (implement-protocol! 'draw 'rectangle
;;;     (lambda (r ctx) (draw-rect (rect-pos r) (rect-size r) ctx)))
;;;
;;;   ;; Use the protocol (dispatches on first arg's type tag)
;;;   (draw my-circle canvas)  ; calls circle implementation
;;;
;;; Type Dispatch:
;;;   Objects must be tagged lists: (list 'type-tag ...)
;;;   Dispatch uses (car obj) to determine type.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Registry
;;; ====

;;; Global registry: Protocol Name -> (Type Tag -> Implementation)
;;; Structure: hashtable of hashtables
(define *protocol-registry* (make-hashtable symbol-hash eq?))

;;; register-protocol-impl! : Symbol × Symbol × Procedure → Void
;;; Register an implementation for a protocol and type.
(define (register-protocol-impl! protocol-name type-tag impl)
  (let ([type-table (hashtable-ref *protocol-registry* protocol-name #f)])
    (unless type-table
      (set! type-table (make-hashtable symbol-hash eq?))
      (hashtable-set! *protocol-registry* protocol-name type-table))
    (hashtable-set! type-table type-tag impl)))

;;; get-protocol-impl : Symbol × Symbol → Procedure | #f
;;; Look up implementation for protocol and type.
(define (get-protocol-impl protocol-name type-tag)
  (let ([type-table (hashtable-ref *protocol-registry* protocol-name #f)])
    (and type-table
         (hashtable-ref type-table type-tag #f))))

;;; protocol-implementations : Symbol → (List Symbol)
;;; List all types implementing a protocol.
(define (protocol-implementations protocol-name)
  (let ([type-table (hashtable-ref *protocol-registry* protocol-name #f)])
    (if type-table
        (vector->list (hashtable-keys type-table))
        '())))

;;; ====
;;; Type Tag Extraction
;;; ====

;;; get-type-tag : Any → Symbol | #f
;;; Extract type tag from object. Returns #f for non-tagged values.
(define (get-type-tag obj)
  (and (pair? obj)
       (symbol? (car obj))
       (car obj)))

;;; ====
;;; Dispatch
;;; ====

;;; protocol-dispatch : Symbol × Any × ... → Any
;;; Dispatch to implementation based on the first argument's type tag.
;;; Raises error if no implementation found.
(define (protocol-dispatch protocol-name obj . args)
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

;;; protocol-dispatch/default : Symbol × Any × Procedure × ... → Any
;;; Like protocol-dispatch but calls default-fn if no implementation.
(define (protocol-dispatch/default protocol-name obj default-fn . args)
  (let ([type-tag (get-type-tag obj)])
    (if (not type-tag)
        (apply default-fn obj args)
        (let ([impl (get-protocol-impl protocol-name type-tag)])
          (if impl
              (apply impl obj args)
              (apply default-fn obj args))))))

;;; ====
;;; Macros
;;; ====

;;; define-protocol : (Name Obj Args ...) [DocString] → Definition
;;; Define a protocol, creating a dispatch function.
;;;
;;; Example:
;;;   (define-protocol (body-pos b) "Get body position")
;;;   ; creates: (define (body-pos b) (protocol-dispatch 'body-pos b))
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

;;; define-protocol/default : (Name Obj Args ...) Default-Expr → Definition
;;; Define a protocol with a default implementation for unregistered types.
(define-syntax define-protocol/default
  (syntax-rules ()
    [(_ (name obj args ...) default-fn)
     (define (name obj args ...)
       (protocol-dispatch/default 'name obj default-fn args ...))]))

;;; implement-protocol! : Symbol × Symbol × Procedure → Void
;;; Alias for register-protocol-impl! (more intuitive name).
(define implement-protocol! register-protocol-impl!)

;;; ====
;;; Introspection
;;; ====

;;; protocol-exists? : Symbol → Boolean
;;; Check if a protocol has any implementations registered.
(define (protocol-exists? protocol-name)
  (hashtable-contains? *protocol-registry* protocol-name))

;;; type-implements? : Symbol × Symbol → Boolean
;;; Check if a type implements a protocol.
(define (type-implements? type-tag protocol-name)
  (and (get-protocol-impl protocol-name type-tag) #t))

;;; list-protocols : → (List Symbol)
;;; List all registered protocol names.
(define (list-protocols)
  (vector->list (hashtable-keys *protocol-registry*)))

;;; ====
;;; Exports
;;; ====
;;;
;;; Core:
;;;   - define-protocol      : Macro to define protocol dispatch functions
;;;   - define-protocol/default : Protocol with default for unknown types
;;;   - implement-protocol!  : Register implementation for type
;;;   - protocol-dispatch    : Manual dispatch (used by macro)
;;;
;;; Introspection:
;;;   - get-type-tag         : Extract type tag from object
;;;   - protocol-exists?     : Check if protocol is registered
;;;   - type-implements?     : Check if type implements protocol
;;;   - protocol-implementations : List types implementing a protocol
;;;   - list-protocols       : List all protocols

(display "Protocol System loaded.\n")
(display "  Define:    (define-protocol (name obj args...) \"doc\")\n")
(display "  Implement: (implement-protocol! 'name 'type-tag fn)\n")
(display "  Dispatch:  Automatic via (car obj) type tag\n")
