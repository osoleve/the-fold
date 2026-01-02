;;; playpen/satin/satin.ss — Satin: Declarative Authoring DSL for Quill
;;;
;;; Satin is an education-first DSL that compiles into Quill stories.
;;; Named for smooth, lustrous fabric — authoring that feels good.
;;;
;;; Usage:
;;;   (load "user/satin/satin.ss")
;;;   (satin-compile spec)      ; → (values quill-story issues)
;;;   (satin-compile! spec)     ; → quill-story | error
;;;
;;; Design principles (see scripture/satin-design.sexp):
;;;   - Declarative first: configuration, not code
;;;   - Education focus: exercises and rubrics are first-class
;;;   - Deterministic: same input → same output
;;;   - Source-mapped: errors point to original DSL source
;;;   - Composable: modules and imports (future)

(define *satin-version* "0.1.0")

;;; ============================================================
;;; Dependencies
;;; ============================================================

;; Core prelude (needed for parsing)
(load "core/base/prelude.ss")

;; Quill runtime (required for compilation targets)
(load "user/quill/quill.ss")

;;; ============================================================
;;; Satin Modules (load order)
;;; ============================================================

(load "user/satin/span.ss")
(load "user/satin/syntax.ss")
(load "user/satin/guards.ss")
(load "user/satin/effects.ss")
(load "user/satin/validate.ss")
(load "user/satin/compile.ss")
(load "user/satin/pretty.ss")
(load "user/satin/lint.ss")
(load "user/satin/docs.ss")

;;; ============================================================
;;; Public API
;;; ============================================================

;;; satin-compile : SatinSpec → (Values QuillStory Issues)
;;; Compile a Satin specification to a Quill story.
;;; Returns both the story and any validation issues.
(define (satin-compile spec)
  (satin-compile-impl spec))

;;; satin-compile! : SatinSpec → QuillStory
;;; Compile a Satin specification, throwing on errors.
(define (satin-compile! spec)
  (let-values ([(story issues) (satin-compile spec)])
              (let ([errors (filter satin-error? issues)])
                   (if (null? errors)
                       story
                       (error 'satin-compile!
                              (string-append
                               "Satin compilation failed:
"
                               (apply string-append
                                      (map (lambda (e)
                                                   (string-append "  - " (satin-issue-message e) "
"))
                                           errors))))))))

;;; satin-example : → SatinSpec
;;; Return an example Satin story specification.
(define (satin-example)
  '(satin-story
    (id demo)
    (title "Satin Demo")
    (author "Shepherd")
    (start welcome)
    
    (node welcome
          (title "Welcome to Satin")
          (body "This is a demonstration of the Satin DSL.")
          (choice "Continue" next))
    
    (node next
          (title "More Content")
          (body "Satin compiles to Quill runtime structures.")
          (on-enter (set-flag! visited-next?))
          (choice "Go back" welcome)
          (choice "Finish" ending :when (flag? visited-next?)))
    
    (node ending
          (title "The End")
          (body "Thanks for trying Satin!")
          (end))))

;;; Optional startup message
(when (and (top-level-bound? '*quiet*) (not *quiet*))
      (display "Satin loaded — declarative authoring DSL for Quill")
      (newline))
