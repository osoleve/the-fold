;;; persona-prompt-gen.ss - Helper functions for persona prompt DSL
;;;
;;; Provides primitives for building variable persona prompts:
;;;   (choice opt1 opt2 ...) - randomly select one string option
;;;   (cond [(test) val] ... [else default]) - native Scheme conditional
;;;   string-append - native Scheme string concatenation
;;;
;;; Persona DSL files load fragments and use these helpers to build prompts.
;;;
;;; Usage in persona files:
;;;   (load "agents/lib/persona-prompt-gen.ss")
;;;   (load "agents/personas/fragments/response-postures.ss")
;;;   (string-append
;;;     "You are name. "
;;;     (choice "Option A" "Option B")
;;;     ...)

;; Random choice - select one string from a list
(define (choice . options)
  (if (null? options)
      (error 'choice "choice requires at least one option")
      (list-ref options (random (length options)))))

;; Fragment loading helper - simplify loading from fragments directory
(define (load-fragment name)
  (let ([filename (string-append "/home/oso/the-fold/agents/personas/fragments/"
                                (symbol->string name) ".ss")])
    (load filename)))
