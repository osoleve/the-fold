;;; user/mmbn-sdk/mmbn.ss — MMBN SDK Main Entry Point
;;; Load this file once to get the entire SDK
;;;
;;; Usage: (load "user/mmbn-sdk/mmbn.ss")

;; Guard against double-loading
(define *mmbn-sdk-loaded*
  (if (top-level-bound? '*mmbn-sdk-loaded*)
      *mmbn-sdk-loaded*
      #f))

(when (not *mmbn-sdk-loaded*)

  ;; Core dependencies (order matters!)
  (load "core/base/prelude.ss")
  (load "lattice/fp/protocol.ss")
  (load "boundary/ui/color.ss")
  (load "boundary/ui/halfblock.ss")
  (load "boundary/ui/sprite-designer.ss")

  ;; SDK Core
  (load "user/mmbn-sdk/core/entity.ss")
  (load "user/mmbn-sdk/core/panel.ss")
  (load "user/mmbn-sdk/core/scene.ss")

  ;; Renderer
  (load "user/mmbn-sdk/render/renderer.ss")

  ;; Entities (register protocol implementations)
  (load "user/mmbn-sdk/entities/navi.ss")
  (load "user/mmbn-sdk/entities/virus.ss")

  (set! *mmbn-sdk-loaded* #t)

  (display "\n")
  (display "════════════════════════════════════════════════════════\n")
  (display "  MMBN SDK loaded successfully!\n")
  (display "════════════════════════════════════════════════════════\n")
  (display "\n")
  (display "Core:\n")
  (display "  (make-entity type pos sprite . props)  — Generic entity\n")
  (display "  (make-battle-grid)                     — 6x3 battle grid\n")
  (display "  (make-scene-manager)                   — Scene state machine\n")
  (display "\n")
  (display "Entities:\n")
  (display "  (make-navi pos hp)                     — Player character\n")
  (display "  (make-mettaur pos hp)                  — Hardhat virus\n")
  (display "  (make-canodumb pos hp)                 — Turret virus\n")
  (display "\n")
  (display "Rendering:\n")
  (display "  (make-screen-buffer)                   — Pixel buffer\n")
  (display "  (render-battle-scene! buf grid ents ...)\n")
  (display "  (display-battle buffer)                — Show on terminal\n")
  (display "\n"))
