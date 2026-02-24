;;; lattice/tiles/manifest.sexp — Board Game SDK Skill Manifest

(skill tiles
  (version "0.1.0")
  (tier 1)
  (path "lattice/tiles")
  (purity total)
  (stability stable)
  (fuel-bound "O(n) for board operations, O(n log n) for pathfinding, O(r^2) for FOV, O(n^3) for topology")
  (deps (data topology))

  (description
   "Board game development SDK supporting square, hexagonal, and triangular
    grids. Includes pathfinding (BFS, Dijkstra, A*), field of view, line of
    sight, unit management, turn-based mechanics, and board rendering.")

  (keywords (board-game tiles grid hex square triangle pathfinding
             fov line-of-sight units turns tactical strategy roguelike))
  (aliases (boardcraft game-board grid-sdk tactical))

  (concepts
    (concept board-games
      (description "Board game SDK for hex, square, and triangle grids: pathfinding, FOV, line-of-sight, and tactical reasoning.")
      (synonyms board-game hex-grid square-grid pathfinding fov line-of-sight roguelike boardcraft game-board grid-sdk tactical)))

  (exports
   (core coord make-tile make-board board-get board-set board-coords
         board-size manhattan-distance chebyshev-distance euclidean-distance)
   (square square-coord make-square-board square-neighbors square-line
           square-range square-in-bounds?)
   (hex axial-coord cubic-coord make-hex-board hex-neighbors hex-distance
        hex-line hex-range hex-ring hex-rotate-left hex-rotate-right)
   (triangle triangle-coord make-triangle-board triangle-neighbors
             triangle-distance-manhattan triangle-line triangle-range)
   (pathfinding bfs-path dijkstra-path astar-path find-path-bfs
                find-path-dijkstra find-path-astar find-reachable board-reachable)
   (visibility has-line-of-sight? board-has-los? calculate-fov board-fov
               shadowcast-fov visible-enemies tiles-in-sight)
   (units make-unit make-game-state game-place-unit game-move-unit
          game-get-unit-at game-all-units game-units-by-team game-visible-units)
   (turns make-turn-state turn-current-unit turn-spend-action turn-next-unit
          turn-end-turn calculate-initiative-order game-execute-action)
   (render render-board render-board-with-overlay display-board
           display-board-with-path display-board-with-fov)
   (topology-analysis board->simplicial-complex board-betti-numbers
                      board-connected-regions board-terrain-holes
                      board-topology-analysis board-critical-edges
                      board-bottleneck-score board-bottleneck-scores-all
                      board-topology-summary board-is-connected?
                      board-has-chokepoints?))

  (modules
   (core "core.ss" "Core board and tile abstractions")
   (square "square.ss" "Square grid coordinate system and operations")
   (hex "hex.ss" "Hexagonal grid with axial/cubic coordinates")
   (triangle "triangle.ss" "Triangular grid coordinate system")
   (pathfinding "pathfinding.ss" "BFS, Dijkstra, A* pathfinding algorithms")
   (visibility "visibility.ss" "Field of view and line of sight")
   (units "units.ss" "Unit placement and game state management")
   (turns "turns.ss" "Turn-based game mechanics and initiative")
   (render "render.ss" "ASCII board rendering with overlays")
   (boardcraft "boardcraft.ss" "Interactive help and documentation")
   (topology-analysis "topology-analysis.ss" "Topological analysis using simplicial homology")))
