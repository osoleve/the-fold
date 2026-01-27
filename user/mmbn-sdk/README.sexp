(readme mmbn-sdk
  (title "MMBN SDK — Mega Man Battle Network Style Game Framework")
  (version "0.1.0")
  (status experimental)

  (description
    "A game development SDK for creating Mega Man Battle Network-style games
     in The Fold. Features terminal-based half-block pixel rendering, entity
     protocols, battle grid system, and scene state machine.")

  (usage
    "Load the SDK:"
    "(load \"user/mmbn-sdk/mmbn.ss\")"
    ""
    "Run the demo:"
    "scheme --script user/mmbn-sdk/demo.ss")

  (modules
    (core
      (entity.ss "Entity protocol: position, sprite, HP, teams, update")
      (panel.ss "Battle grid: 6x3 panels with terrain types")
      (scene.ss "Scene state machine with transitions"))
    (render
      (renderer.ss "Pixel buffer compositing and display"))
    (entities
      (navi.ss "Player character with movement and attack")
      (virus.ss "Mettaur and Canodumb enemy types"))
    (mmbn.ss "Main entry point - loads all modules"))

  (features
    (implemented
      "Entity protocol with extensible dispatch"
      "6x3 battle grid with terrain types (normal, cracked, ice, etc.)"
      "Panel ownership and stealing mechanics"
      "Navi entity with movement validation"
      "Virus entities (Mettaur with guard, Canodumb turret)"
      "Half-block pixel rendering (▀▄█)"
      "Sprite support with transparency"
      "HP bar rendering"
      "Scene state machine with push/pop/replace")
    (planned
      "Chip system (battle cards)"
      "Projectile entities"
      "Input handling"
      "Full battle game loop"
      "PET UI scenes"
      "Net world navigation"
      "Animation sequences"))

  (dependencies
    "lattice/fp/protocol.ss — Extensible type dispatch"
    "boundary/ui/halfblock.ss — Pixel buffer and half-block rendering"
    "boundary/ui/sprite-designer.ss — Sprite creation utilities"
    "boundary/ui/color.ss — Color primitives")

  (bbs-issue fold-zxte))
