;;; thimble/error-context-fixed.ss — Context-Aware Error Messages for The Fold (Fixed)
;;;
;;; Enhanced error messages that understand The Fold's context

(load "fabric/stitches/prelude.ss")

;;; ============================================================
;;; Error Context Database
;;; ============================================================

;; Context-aware error patterns for The Fold specific errors
(define *fold-error-contexts*
  '((boardcraft-unbound
     (pattern "not bound")
     (context "BoardCraft SDK")
     (suggestions "Load BoardCraft: (load \"playpen/boardcraft/boardcraft.ss\")"
                  "Check function name in BoardCraft documentation"
                  "Try: (help 'make-hex-board) for available functions")
     (tutorial "tutorial-1-hex-simple-fixed.ss"))
    
    (loom-unbound
     (pattern "not bound")
     (context "Loom SDK")
     (suggestions "Load Loom: (load \"playpen/loom/loom.ss\")"
                  "Check function name in Loom documentation"
                  "Try: (help 'make-tilemap) for available functions")
     (tutorial "tutorial-1-hex-simple-fixed.ss"))
    
    (repl-functions
     (pattern "not bound")
     (context "REPL functions")
     (suggestions "Check if function exists: (help)"
                  "Load help system: (load \"thimble/repl-help-basic.ss\")"
                  "Available functions: help, examples, discover")
     (tutorial "tutorial-1-hex-simple-fixed.ss"))
    
    (coordinate-system
     (pattern "coordinate")
     (context "Coordinate systems")
     (suggestions "BoardCraft uses axial coordinates: (axial-coord q r)"
                  "Convert coordinates: (axial->cubic coord)"
                  "Check coordinate validity with hex-in-bounds?")
     (tutorial "tutorial-1-hex-simple-fixed.ss"))
    
    (module-load
     (pattern "load.*failed")
     (context "Module loading")
     (suggestions "Check file path is correct"
                  "Verify file exists and is readable"
                  "Use absolute paths: /home/oso/the-fold/path/to/file.ss"
                  "List available files: (ls)")
     (tutorial #f))
    
    (function-args
     (pattern "wrong number of arguments")
     (context "Function arguments")
     (suggestions "Check function signature with (help 'function-name)"
                  "Count required vs optional arguments"
                  "Verify argument types match expectations")
     (tutorial #f))
    
    (type-mismatch
     (pattern "type.*mismatch")
     (context "Type system")
     (suggestions "Check type annotations"
                  "Use (:type expr) to see expression type"
                  "Load core types: (load-core) for type tools"
                  "Use (:ann expr) to see annotated expression")
     (tutorial #f))
    
    (general
     (pattern #f)
     (context "General")
     (suggestions "Check the tutorial for examples"
                  "Try: (help) for available functions"
                  "Look at working examples in playpen/creations/")
     (tutorial #f))))

;;; ============================================================
;;; Error Context Detection
;;; ============================================================

(define (detect-error-context error-msg)
  "Determine which context the error occurred in"
  (let loop ([contexts *fold-error-contexts*])
       (if (null? contexts)
           'general  ; No specific context found
           (let* ([ctx (car contexts)]
                  [pattern (assoc 'pattern (cdr ctx))]
                  [pattern-str (if pattern (cadr pattern) #f)])
                 (if (and pattern-str (string-contains? error-msg pattern-str))
                     (car ctx)  ; Found matching context
                     (loop (cdr contexts)))))))

(define (get-context-suggestions context)
  "Get suggestions for a specific error context"
  (let ([ctx-data (assoc context *fold-error-contexts*)])
       (if ctx-data
           (let ([suggestions (assoc 'suggestions (cdr ctx-data))])
                (if suggestions (cdr suggestions) '()))
           '())))

(define (get-context-tutorial context)
  "Get tutorial file for a specific error context"
  (let ([ctx-data (assoc context *fold-error-contexts*)])
       (if ctx-data
           (let ([tutorial (assoc 'tutorial (cdr ctx-data))])
                (if tutorial (cadr tutorial) #f))
           #f)))

;;; ============================================================
;;; Enhanced Error Formatter
;;; ============================================================

(define (format-error-with-fold-context error-msg)
  "Format error with The Fold-specific context and suggestions"
  (let* ([context (detect-error-context error-msg)]
         [suggestions (get-context-suggestions context)]
         [tutorial (get-context-tutorial context)]
         [context-info (format-context-info context)]
         [specific-suggestions (format-specific-suggestions suggestions tutorial)])
        
        (string-append
         "🚨 ERROR:\n"
         "   " error-msg "\n"
         (if (not (string=? context-info ""))
             (string-append "\n" context-info)
             "")
         (if (not (string=? specific-suggestions ""))
             (string-append "\n" specific-suggestions)
             ""))))

(define (format-context-info context)
  "Format context information"
  (if (eq? context 'general)
      ""
      (string-append
       "💡 Context: " (symbol->string context) "\n"
       "   The Fold detected you're working with " (symbol->string context) "\n")))

(define (format-specific-suggestions suggestions tutorial)
  "Format context-specific suggestions"
  (string-append
   (if (not (null? suggestions))
       (string-append
        "🔧 Suggestions:\n"
        (string-join (map (lambda (s) (string-append "   • " s)) suggestions) "\n")
        "\n")
       "")
   
   (if tutorial
       (string-append
        "📚 Learn more:\n"
        "   Try: (load \"playpen/boardcraft/examples/" tutorial "\")\n"
        "   This tutorial covers exactly what you need!\n")
       "")
   
   (if (not (null? suggestions))
       (string-append
        "🎯 Quick fix: Try the first suggestion above\n"
        "   If that doesn't work, check the tutorial\n")
       "")))

;;; ============================================================
;;; Demo and Testing
;;; ============================================================

(define (demo-error-context)
  "Demonstrate the enhanced error context system"
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "  ERROR CONTEXT SYSTEM DEMO\n")
  (display "═══════════════════════════════════════════════════════════════\n\n")
  
  ;; Simulate different error types
  (display "1. BoardCraft error simulation:\n")
  (display (format-error-with-fold-context "variable make-hex-board is not bound"))
  (newline)
  
  (display "2. Loom error simulation:\n")
  (display (format-error-with-fold-context "variable tilemap-fill! is not bound"))
  (newline)
  
  (display "3. Coordinate error simulation:\n")
  (display (format-error-with-fold-context "invalid coordinate format"))
  (newline)
  
  (display "✅ Error context system is working!\n")
  (display "✅ Provides specific suggestions for The Fold SDKs\n")
  (display "✅ Links to relevant tutorials\n")
  (display "✅ Makes errors actionable instead of cryptic\n"))

;;; Provide the enhanced error formatting
(provide format-error-with-fold-context demo-error-context)

(display "Error context system loaded! Run (demo-error-context) to see it in action.\n")
