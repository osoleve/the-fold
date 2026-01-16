;;; lattice/fp/protocol-README.sexp --- Open Protocol System Documentation
;;;
;;; Module documentation in S-expression format.

(module fp/protocol
  (description "Open protocol system for extensible type dispatch")

  (purpose
    "Enable the Open/Closed Principle in Scheme:"
    "- Define protocols (generic operations) once"
    "- Register implementations per type tag at load time"
    "- Dispatch automatically based on (car obj) type tag"
    "- Add new types without modifying existing code")

  (quick-start
    ";;; Load the protocol system"
    "(load \"lattice/fp/protocol.ss\")"
    ""
    ";;; Define a protocol"
    "(define-protocol (draw obj ctx) \"Draw object to context\")"
    ""
    ";;; Register implementations"
    "(implement-protocol! 'draw 'circle"
    "  (lambda (c ctx) (draw-circle (circle-center c) ctx)))"
    "(implement-protocol! 'draw 'rectangle"
    "  (lambda (r ctx) (draw-rect (rect-pos r) ctx)))"
    ""
    ";;; Use the protocol (automatic dispatch)"
    "(draw my-circle canvas)  ; calls circle implementation")

  (exports
    (core
      "define-protocol : Macro to create dispatch function"
      "define-protocol/default : Protocol with fallback for unknown types"
      "implement-protocol! : Register implementation for type"
      "protocol-dispatch : Manual dispatch (used by macro)")

    (introspection
      "get-type-tag : Extract type tag from object"
      "protocol-exists? : Check if protocol is registered"
      "type-implements? : Check if type implements protocol"
      "protocol-implementations : List types implementing a protocol"
      "list-protocols : List all registered protocols"))

  (type-tags
    "Objects must be tagged lists: (list 'type-tag ...)"
    "The type tag is extracted via (car obj)"
    "Example: (list 'circle x y radius) has tag 'circle"
    "Dispatch is O(1) via hashtable lookup")

  (example-physics-protocols
    ";;; From physics/lenses - body protocols"
    "(define-protocol (body-pos b) \"Get body position\")"
    "(define-protocol (body-set-pos b p) \"Set body position\")"
    ""
    "(implement-protocol! 'body-pos 'rigid-body-2d rigid-body-pos)"
    "(implement-protocol! 'body-pos 'particle particle-pos)"
    "(implement-protocol! 'body-pos 'traced-body traced-body-pos)"
    ""
    ";;; Generic lens uses protocols"
    "(define body-pos-lens"
    "  (make-lens body-pos"
    "             (lambda (p b) (body-set-pos b p))))")

  (tier 0)
  (purity total-after-load)
  (stability stable)
  (dependencies
    (prelude.ss "Core Scheme primitives"))

  (test-command "scheme --script lattice/fp/test-protocol.ss")

  (author "Claude Opus 4.5 + Gemini 3 Pro")
  (created "2026-01-16"))
