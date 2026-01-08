;;; shell/debug/README.sexp — Debugging and Error Handling Modules
;;;
;;; Debugger tools, error formatting, and cross-referencing.

((name . debug)
 (purpose . "Debugging tools, error formatting, and code analysis")
 (tier-access . shell)
 (purity . impure)

 (description . "
Debugging infrastructure for The Fold. Includes:
- Time-travel debugger REPL commands
- Enhanced error formatting and suggestions
- Type inspection and visualization
- Code cross-reference and navigation
")

 (modules
  ((file . debug-repl.ss)
   (purpose . "REPL debugger commands")
   (api . (step undo redo watch explain bt)))

  ((file . error-fmt.ss)
   (purpose . "Enhanced error formatting")
   (api . (format-error pretty-error error-context)))

  ((file . error-improvements.ss)
   (purpose . "Error message improvements")
   (api . (enhance-error suggest-fix error-wiki)))

  ((file . exploration-error-handler.ss)
   (purpose . "Error handler for exploration scripts")
   (api . (with-exploration-errors handle-exploration-error)))

  ((file . type-inspect.ss)
   (purpose . "Type inspector and visualizer")
   (api . (inspect-type type-tree type-diagram)))

  ((file . xref.ss)
   (purpose . "Code cross-reference tool")
   (api . (xref find-callers find-callees symbol-uses)))

  ((file . session-debugger.ss)
   (purpose . "Per-session debugger state")
   (api . (debugger-state save-debugger-state restore-debugger-state)))

  ((file . session-debug.ss)
   (purpose . "Debug session persistence issues")
   (api . (debug-session session-diagnostics))))

 (dependencies
  (internal . (core/base/prelude.ss
               core/util/debug.ss)))

 (usage . "
Debug REPL commands:
  (load \"shell/debug/debug-repl.ss\")
  (step)      ; Step forward
  (undo)      ; Undo last step
  (watch x)   ; Watch variable

Format errors:
  (load \"shell/debug/error-fmt.ss\")
  (format-error exn)
")

 (see-also . (
   "core/util/debug.ss"
   "shell/diagnostics/README.sexp")))
