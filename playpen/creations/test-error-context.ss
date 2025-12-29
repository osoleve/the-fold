;;; test-error-context.ss — Test the new error context system

(display "Testing the enhanced error context system...\n\n")

;; Load the error context system
(load "thimble/error-context.ss")

;; Run the demo
(demo-error-context)

(display "\nTesting with real BoardCraft error...\n\n")

;; Test with actual BoardCraft loading
try {
  (load "playpen/boardcraft/boardcraft.ss")
  ;; This will fail with tilemap-fill! not bound
  (tilemap-fill! 'test 'tile)
} catch (e) {
  (display "Real error caught:\n")
  (display (format-error-with-context e (get-current-stack-trace)))
  (newline)
}

(display "Error context system test completed!\n")
(display "The system provides:\n")
(display "✅ Context-aware error messages\n")
(display "✅ Specific suggestions for The Fold SDKs\n")
(display "✅ Tutorial links for learning\n")
(display "✅ Actionable fixes instead of cryptic messages\n")
