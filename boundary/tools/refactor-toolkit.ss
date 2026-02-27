;;; boundary/tools/refactor-toolkit.ss — Unified Refactoring Toolkit
;;;
;;; Single entry point for all refactoring operations.
;;; Orchestrates existing tools: rename, extract, inline, dead-code, deps.
;;; Adds new capability: move refactoring.
;;;
;;; This is Shell code: coordinates impure modules, manages state.
;;;
;;; Usage:
;;;   (refactor 'help)                        ; Show available operations
;;;   (refactor 'rename 'old 'new)            ; Rename symbol globally
;;;   (refactor 'move 'sym "target-file.ss")  ; Move to another module
;;;   (refactor 'extract 'name expr "file")   ; Extract function
;;;   (refactor 'inline 'func)                ; Inline function
;;;   (refactor 'dead-code)                   ; Find dead code
;;;   (refactor 'dead-code "path")            ; Scan specific path
;;;   (refactor 'deps 'symbol)                ; Show dependencies
;;;   (refactor 'undo)                        ; Undo last operation
;;;   (refactor 'status)                      ; Show pending changes
;;;
;;; Dependencies:
;;;   boundary/tools/refactor-integrated.ss
;;;   boundary/tools/refactor-move.ss
;;;   boundary/tools/dead-code.ss
;;;   lattice/meta/dag.ss

;;; ====
;;; Dependencies
;;; ====

(load "core/base/prelude.ss")

;;; Track which modules are loaded
(define *refactor-modules-loaded* '())

;;; Lazy loader for modules
(define (ensure-module-loaded! name file)
  (unless (memq name *refactor-modules-loaded*)
          (guard (e [else
                     (printf "Warning: Could not load ~a: ~a\n"
                             file (if (message-condition? e)
                                      (condition-message e)
                                      "unknown error"))])
                 (load file)
                 (set! *refactor-modules-loaded*
                       (cons name *refactor-modules-loaded*)))))

;;; ====
;;; Operation Registry
;;; ====

;;; Each operation: (name description handler required-modules)
(define *refactor-operations*
  '((help
     "Show available refactoring operations"
     refactor-help-handler
     ())

    (rename
     "Rename a symbol across the codebase"
     refactor-rename-handler
     (refactor-integrated))

    (move
     "Move a symbol to a different module"
     refactor-move-handler
     (refactor-move))

    (extract
     "Extract code into a new function"
     refactor-extract-handler
     (refactor-integrated))

    (inline
     "Inline a function at its call sites"
     refactor-inline-handler
     (refactor-integrated))

    (dead-code
     "Find unused definitions"
     refactor-dead-code-handler
     (dead-code))

    (dead-code-delete
     "Safe-delete analysis for a symbol"
     refactor-dead-code-delete-handler
     (dead-code))

    (deps
     "Show dependency information for a symbol"
     refactor-deps-handler
     (dag call-graph))

    (undo
     "Undo the last refactoring operation"
     refactor-undo-handler
     (refactor-integrated))

    (status
     "Show refactoring status and pending changes"
     refactor-status-handler
     (refactor-integrated))

    (clear
     "Clear pending changes without applying"
     refactor-clear-handler
     (refactor-integrated))

    (apply
     "Apply all pending changes"
     refactor-apply-handler
     (refactor-integrated))

    (diagnostic
     "Analyze symbol contexts (code vs string/comment)"
     refactor-diagnostic-handler
     (refactor-integrated))))

;;; Module file mapping
(define *module-files*
  '((refactor-integrated . "boundary/tools/refactor-integrated.ss")
    (refactor-move . "boundary/tools/refactor-move.ss")
    (dead-code . "boundary/tools/dead-code.ss")
    (dag . "lattice/meta/dag.ss")
    (call-graph . "boundary/lens/call-graph.ss")))

;;; ====
;;; Main Dispatcher
;;; ====

;;; refactor : Symbol × Any... -> Any
;;; Main entry point for all refactoring operations.
(define (refactor op . args)
  (let ([operation (assq op *refactor-operations*)])
       (if (not operation)
           (begin
            (printf "\n  Unknown refactoring operation: ~a\n" op)
            (printf "  Use (refactor 'help) to see available operations.\n\n")
            #f)
           (let* ([name (car operation)]
                  [handler-name (caddr operation)]
                  [required-modules (cadddr operation)])
                 ;; Load required modules
                 (for-each
                  (lambda (mod)
                          (let ([file-entry (assq mod *module-files*)])
                               (when file-entry
                                     (ensure-module-loaded! mod (cdr file-entry)))))
                  required-modules)
                 ;; Call the handler
                 (guard (e [else
                            (printf "\n  Error in '~a': ~a\n\n"
                                    op (if (message-condition? e)
                                           (condition-message e)
                                           e))
                            #f])
                        (apply (eval handler-name) args))))))

;;; ====
;;; Operation Handlers
;;; ====

;;; refactor-help-handler : -> void
(define (refactor-help-handler)
  (display "\n")
  (display "  ------------------- REFACTORING TOOLKIT --------------------\n")
  (display "\n")
  (display "  Rename & Move:\n")
  (display "    (refactor 'rename 'old-name 'new-name)     Rename symbol globally\n")
  (display "    (refactor 'move 'symbol \"target.ss\")      Move to another module\n")
  (display "\n")
  (display "  Extract & Inline:\n")
  (display "    (refactor 'extract 'name expr \"file\")     Extract to new function\n")
  (display "    (refactor 'inline 'func-name)              Inline at call sites\n")
  (display "\n")
  (display "  Dead Code:\n")
  (display "    (refactor 'dead-code)                      Scan entire codebase\n")
  (display "    (refactor 'dead-code \"path\")              Scan specific path\n")
  (display "    (refactor 'dead-code-delete 'sym)          Safe-delete analysis\n")
  (display "\n")
  (display "  Dependencies:\n")
  (display "    (refactor 'deps 'symbol)                   Show callers/callees\n")
  (display "\n")
  (display "  Change Management:\n")
  (display "    (refactor 'status)                         Show pending changes\n")
  (display "    (refactor 'apply)                          Apply pending changes\n")
  (display "    (refactor 'clear)                          Discard pending changes\n")
  (display "    (refactor 'undo)                           Undo last operation\n")
  (display "\n")
  (display "  Diagnostics:\n")
  (display "    (refactor 'diagnostic 'sym)                Analyze symbol contexts\n")
  (display "\n")
  (void))

;;; refactor-rename-handler : Symbol × Symbol -> void
(define (refactor-rename-handler old-name new-name)
  (refactor-rename-preview old-name new-name))

;;; refactor-move-handler : Symbol × String -> void
(define (refactor-move-handler symbol target-file)
  (refactor-move-preview symbol target-file))

;;; refactor-extract-handler : Symbol × S-expr × String -> void
(define (refactor-extract-handler name expr file)
  (display "\n  Extract refactoring not yet fully integrated.\n")
  (display "  Use (load \"boundary/tools/refactor-integrated.ss\") directly.\n\n"))

;;; refactor-inline-handler : Symbol -> void
(define (refactor-inline-handler func-name)
  (display "\n  Inline refactoring not yet fully integrated.\n")
  (display "  Use (load \"boundary/tools/refactor-integrated.ss\") directly.\n\n"))

;;; refactor-dead-code-handler : [String] -> void
(define (refactor-dead-code-handler . args)
  (if (null? args)
      (dead-code-scan)
      (dead-code-scan (car args))))

;;; refactor-dead-code-delete-handler : Symbol -> void
(define (refactor-dead-code-delete-handler sym)
  (dead-code-suggest-delete sym))

;;; refactor-deps-handler : Symbol -> void
(define (refactor-deps-handler sym)
  (display "\n")
  (printf "  Dependency Analysis: ~a\n" sym)
  (display "  --------------------------------\n\n")

  ;; Try to get call graph info
  (guard (e [else (void)])
         (when (and (top-level-bound? 'call-graph-callers)
                    (procedure? call-graph-callers))
               (let ([callers (call-graph-callers sym)]
                     [callees (call-graph-callees sym)])
                    (printf "  Direct callers (~a):\n" (length callers))
                    (if (null? callers)
                        (display "    (none)\n")
                        (for-each
                         (lambda (c) (printf "    ~a\n" c))
                         (take 15 callers)))
                    (when (> (length callers) 15)
                          (printf "    ... and ~a more\n" (- (length callers) 15)))

                    (display "\n")
                    (printf "  Direct callees (~a):\n" (length callees))
                    (if (null? callees)
                        (display "    (none)\n")
                        (for-each
                         (lambda (c) (printf "    ~a\n" c))
                         (take 15 callees)))
                    (when (> (length callees) 15)
                          (printf "    ... and ~a more\n" (- (length callees) 15))))))

  ;; Try lattice-level deps if available
  (guard (e [else (void)])
         (when (top-level-bound? 'lattice-uses-transitive)
               (display "\n  (Lattice-level deps available via 'ld' and 'lu' commands)\n")))

  (display "\n"))

;;; refactor-undo-handler : -> void
(define (refactor-undo-handler)
  (refactor-undo!))

;;; refactor-status-handler : -> void
(define (refactor-status-handler)
  (refactor-status))

;;; refactor-clear-handler : -> void
(define (refactor-clear-handler)
  (refactor-clear!))

;;; refactor-apply-handler : -> void
(define (refactor-apply-handler)
  (refactor-apply!))

;;; refactor-diagnostic-handler : Symbol -> void
(define (refactor-diagnostic-handler sym)
  (refactor-rename-diagnostic sym))

;;; ====
;;; Utility Functions
;;; ====

;;; ====
;;; Quick Access Aliases
;;; ====

;;; rr : Symbol × Symbol -> void
;;; Quick rename (preview)
(define (rr old new)
  (refactor 'rename old new))

;;; rm : Symbol × String -> void
;;; Quick move (preview)
(define (rm sym target)
  (refactor 'move sym target))

;;; rd : Symbol -> void
;;; Quick dependency check
(define (rd sym)
  (refactor 'deps sym))

;;; rdc : [String] -> void
;;; Quick dead code scan
(define (rdc . args)
  (apply refactor (cons 'dead-code args)))

;;; ====
;;; Initialization
;;; ====

(display "\n")
(display "  Refactoring Toolkit Loaded\n")
(display "  --------------------------------\n")
(display "  Use (refactor 'help) for available commands.\n")
(display "\n")
(display "  Quick aliases:\n")
(display "    (rr 'old 'new)     - Rename preview\n")
(display "    (rm 'sym \"file\")  - Move preview\n")
(display "    (rd 'sym)          - Dependency analysis\n")
(display "    (rdc)              - Dead code scan\n")
(display "\n")
