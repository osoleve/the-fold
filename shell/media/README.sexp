;;; shell/media/README.sexp — Creative Media Modules
;;;
;;; Music generation, art creation, and audio synthesis.

((name . media)
 (purpose . "Creative tools for music, art, and audio synthesis")
 (tier-access . builder)
 (purity . impure)

 (description . "
Creative media tools for artistic expression within The Fold.
Includes procedural music generation, art creation with graphics
primitives, and audio waveform synthesis.

These are 'builder' tier tools - they build on shell infrastructure
to enable creative exploration.
")

 (modules
  ((file . music-gen.ss)
   (purpose . "Procedural music pattern generator")
   (api . (generate-melody generate-rhythm music-pattern)))

  ((file . create-art.ss)
   (purpose . "Art creation with graphics primitives")
   (api . (create-art random-art art-from-seed)))

  ((file . wave-synth.ss)
   (purpose . "Audio waveform synthesis and visualization")
   (api . (synthesize-wave wave-plot waveform-export))))

 (dependencies
  (internal . (shell/ui/layout.ss
               shell/easing.ss
               shell/ui/graphics-primitives.ss)))

 (usage . "
Generate procedural music:
  (load \"shell/media/music-gen.ss\")
  (generate-melody 16 'pentatonic)

Create art:
  (load \"shell/media/create-art.ss\")
  (create-art 'mandala 100 100)
")

 (see-also . (
   "shell/ui/README.sexp"
   "user/creations/")))
