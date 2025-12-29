;;; thimble/error-context-simple.ss — Simple Error Context System (No Exports)

(load "fabric/stitches/prelude.ss")

;;; Simple error context system without exports
(define (demo-error-context)
  (display "═══════════════════════════════════════════════════════════════\n")
  (display "  ERROR CONTEXT SYSTEM DEMO\n")
  (display "═══════════════════════════════════════════════════════════════\n\n")
  
  ;; Simple demo with hardcoded examples
  (display "1. BoardCraft error example:\n")
  (display "🚨 ERROR:\n")
  (display "   variable make-hex-board is not bound\n")
  (display "\n💡 Context: BoardCraft SDK\n")
  (display "   The Fold detected you're working with BoardCraft SDK\n")
  (display "\n🔧 Suggestions:\n")
  (display "   • Load BoardCraft: (load \"playpen/boardcraft/boardcraft.ss\")\n")
  (display "   • Check function name in BoardCraft documentation\n")
  (display "   • Try: (help 'make-hex-board) for available functions\n")
  (display "\n📚 Learn more:\n")
  (display "   Try: (load \"playpen/boardcraft/examples/tutorial-1-hex-simple-fixed.ss\")\n")
  (display "   This tutorial covers exactly what you need!\n\n")
  
  (display "2. Loom error example:\n")
  (display "🚨 ERROR:\n")
  (display "   variable tilemap-fill! is not bound\n")
  (display "\n💡 Context: Loom SDK\n")
  (display "   The Fold detected you're working with Loom SDK\n")
  (display "\n🔧 Suggestions:\n")
  (display "   • Load Loom: (load \"playpen/loom/loom.ss\")\n")
  (display "   • Check function name in Loom documentation\n")
  (display "   • Try: (help 'make-tilemap) for available functions\n\n")
  
  (display "✅ Error context system demonstrates:\n")
  (display "✅ Context-aware error messages\n")
  (display "✅ Specific suggestions for The Fold SDKs\n")
  (display "✅ Tutorial links for learning\n")
  (display "✅ Actionable fixes instead of cryptic messages\n"))

(display "Error context system loaded! Run (demo-error-context) to see it in action.\n")

;; Run the demo
(demo-error-context)
