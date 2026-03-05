;;; skills/tiles.sexp — Skill curation file

(skill tiles
  (description
    "Board game development SDK supporting square, hexagonal, and triangular\n    grids. Includes pathfinding (BFS, Dijkstra, A*), field of view, line of\n    sight, unit management, turn-based mechanics, and board rendering.")

  (keywords (board-game tiles grid hex square triangle pathfinding fov line-of-sight units turns tactical strategy roguelike))

  (aliases (boardcraft game-board grid-sdk tactical))

  (concepts (board-games))

  (modules
    core
    square
    hex
    triangle
    pathfinding
    visibility
    units
    turns
    render
    boardcraft
    topology-analysis
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
