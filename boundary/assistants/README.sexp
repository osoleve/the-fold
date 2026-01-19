;;; boundary/assistants/README.sexp — AI Assistant Modules
;;;
;;; DUCKIE and other AI assistants.

((name . assistants)
 (purpose . "AI assistants and interactive helpers")
 (tier-access . shell)
 (purity . impure)

 (description . "
AI assistant infrastructure for The Fold. Currently includes DUCKIE,
the rubber duck debugging assistant. Assistants provide:
- Interactive conversation and guidance
- State persistence across sessions
- Visual representation (animations, colors)
- Integration with the block store
")

 (modules
  ((file . duckie-interact.ss)
   (purpose . "DUCKIE REPL interaction")
   (api . (duckie-say duckie-ask duckie-think duckie-respond)))

  ((file . duckie-loop.ss)
   (purpose . "DUCKIE main heartbeat loop")
   (api . (duckie-start duckie-stop duckie-heartbeat)))

  ((file . duckie-persist.ss)
   (purpose . "DUCKIE state persistence to CAS")
   (api . (save-duckie-state load-duckie-state duckie-history))))

 (dependencies
  (internal . (core/blocks/block.ss
               boundary/storage/cas-persist.ss
               boundary/ui/color.ss
               boundary/ui/layout-color.ss
               boundary/ui/animation.ss)))

 (usage . "
Start DUCKIE:
  (load \"boundary/assistants/duckie-loop.ss\")
  (duckie-start)

Interact:
  (duckie-say \"What does this function do?\")
")

 (see-also . (
   "boundary/ui/README.sexp"
   "agents/README.sexp")))
