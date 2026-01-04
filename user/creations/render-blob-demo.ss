;;; render-blob-demo.ss --- Render the blob scene to test DSL
;;;
;;; A quick render to verify the full pipeline works.

(load "user/creations/loop-scene.ss")

(display "\n")
(display "============================================================\n")
(display "  Rendering Blob Demo via DSL\n")
(display "============================================================\n\n")

;;; Use the blob preset scene
(define blob (blob-scene))

;;; Preview first
(display "Preview at t=0:\n\n")
(preview-scene blob)

;;; Estimate time
(display "\n")
(estimate-render-time blob)

;;; Render!
(render-loop! blob "user/creations/blob-dsl-demo.gif")

(display "\nDone! Check user/creations/blob-dsl-demo.gif\n")
