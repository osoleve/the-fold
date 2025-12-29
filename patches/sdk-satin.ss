;;; SDK Patch: Satin - DSL Authoring Toolkit
;;;
;;; Satin is The Fold's DSL construction framework.
;;; Provides syntax definition, validation, compilation,
;;; and pretty-printing for building domain-specific languages.

((name . sdk-satin)
 (description . "DSL authoring toolkit for building domain-specific languages")
 (version . "0.1.0")
 (author . "The Fold")
 (requires . ())
 
 (provides .
           ;; === Core System ===
           (satin-version              ; Get SDK version string
                          
                          ;; === Syntax Definition ===
                          define-syntax-rule          ; Define syntax transformation
                          define-production           ; Define grammar production
                          define-terminal             ; Define terminal symbol
                          
                          ;; === Validation ===
                          make-validator              ; Create validator
                          validate                    ; Validate DSL expression
                          validation-errors           ; Get validation errors
                          
                          ;; === Guards & Constraints ===
                          guard-type                  ; Type guard
                          guard-range                 ; Range guard
                          guard-predicate             ; Custom predicate guard
                          
                          ;; === Compilation ===
                          make-compiler               ; Create compiler
                          compile-dsl                 ; Compile DSL to Scheme
                          emit-code                   ; Emit generated code
                          
                          ;; === Effects ===
                          define-effect               ; Define DSL effect
                          handle-effect               ; Effect handler
                          
                          ;; === Pretty Printing ===
                          pretty-print-dsl            ; Pretty print DSL expression
                          format-error                ; Format validation error
                          
                          ;; === Linting ===
                          lint-dsl                    ; Run linter on DSL code
                          define-lint-rule            ; Define custom lint rule
                          
                          ;; === Documentation ===
                          document-syntax             ; Generate syntax documentation
                          ))
 
 (files .
        ("playpen/satin/span.ss"
         "playpen/satin/guards.ss"
         "playpen/satin/validate.ss"
         "playpen/satin/syntax.ss"
         "playpen/satin/compile.ss"
         "playpen/satin/effects.ss"
         "playpen/satin/pretty.ss"
         "playpen/satin/lint.ss"
         "playpen/satin/docs.ss"
         "playpen/satin/satin.ss")))
