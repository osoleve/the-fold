;;; lattice/tiles/README.ss — BoardCraft SDK Overview
;;;
;;; ═══════════════════════════════════════════════════════════
;;; BOARDCRAFT: A TILE-BASED BOARD GAME SDK
;;; ═══════════════════════════════════════════════════════════
;;;
;;; BoardCraft is a pure functional SDK for creating tile-based board games
;;; with support for multiple tile shapes and coordinate systems.
;;;
;;; SUPPORTED TILE SHAPES:
;;;   • Square (orthogonal & diagonal)
;;;   • Hexagonal (pointy-top & flat-top)
;;;   • Triangular (up & down orientations)
;;;   • Diamond (rotated square)
;;;   • Octagonal + Square (8-square tiling)
;;;
;;; FEATURES:
;;;   • Multiple coordinate systems (offset, axial, cubic)
;;;   • Neighbor/adjacency computation for each tile type
;;;   • Pathfinding algorithms (A*, Dijkstra, BFS)
;;;   • Distance metrics appropriate to each shape
;;;   • Line-of-sight and range calculations
;;;   • Board serialization to content-addressed blocks
;;;   • Pure functional design (immutable boards)
;;;
;;; ARCHITECTURE:
;;;
;;;   boardcraft/
;;;     core.ss           — Common types and utilities
;;;     coords.ss         — Coordinate system abstractions
;;;     square.ss         — Square tile implementation
;;;     hex.ss            — Hexagonal tile implementation
;;;     triangle.ss       — Triangular tile implementation
;;;     diamond.ss        — Diamond tile implementation
;;;     octosquare.ss     — Octagon+Square implementation
;;;     board.ss          — Generic board operations
;;;     pathfinding.ss    — Pathfinding algorithms
;;;     range.ss          — Range and area calculations
;;;     blocks.ss         — Content-addressed serialization
;;;     examples/         — Example games
;;;
;;; ═══════════════════════════════════════════════════════════
;;; QUICK START
;;; ═══════════════════════════════════════════════════════════
;;;
;;; Example: Create a hexagonal board (like Settlers of Catan)
;;;
;;;   (load "lattice/tiles/boardcraft.ss")
;;;
;;;   ;; Create a hex board with axial coordinates
;;;   (define board (make-hex-board 'axial 5))
;;;
;;;   ;; Get neighbors of a hex tile
;;;   (hex-neighbors (axial-coord 0 0))
;;;   ; => ((1 -1) (1 0) (0 1) (-1 1) (-1 0) (0 -1))
;;;
;;;   ;; Calculate distance between hexes
;;;   (hex-distance (axial-coord 0 0) (axial-coord 3 2))
;;;   ; => 5
;;;
;;;   ;; Find shortest path
;;;   (hex-path board (axial-coord 0 0) (axial-coord 3 2))
;;;   ; => ((0 0) (1 0) (2 0) (2 1) (3 1) (3 2))
;;;
;;; Example: Create a square board (like Chess)
;;;
;;;   (define board (make-square-board 8 8))
;;;
;;;   ;; Get orthogonal neighbors (rook moves)
;;;   (square-neighbors-ortho (square-coord 4 4))
;;;   ; => ((4 5) (5 4) (4 3) (3 4))
;;;
;;;   ;; Get all 8 neighbors (king moves)
;;;   (square-neighbors-all (square-coord 4 4))
;;;   ; => ((4 5) (5 5) (5 4) (5 3) (4 3) (3 3) (3 4) (3 5))
;;;
;;; Example: Create a triangular board (like TriHex)
;;;
;;;   (define board (make-triangle-board 'equilateral 6))
;;;
;;;   ;; Triangles alternate up/down orientation
;;;   (triangle-orientation (triangle-coord 0 0))
;;;   ; => 'up
;;;
;;;   ;; Get neighbors (3 edge-adjacent triangles)
;;;   (triangle-neighbors (triangle-coord 0 0))
;;;   ; => ((1 0) (0 1) (-1 0))
;;;
;;; ═══════════════════════════════════════════════════════════
;;; COORDINATE SYSTEMS
;;; ═══════════════════════════════════════════════════════════
;;;
;;; Different tile shapes use different coordinate systems optimized
;;; for their geometry:
;;;
;;; SQUARE TILES:
;;;   - Cartesian (x, y)
;;;   - Simple row/column indexing
;;;
;;; HEXAGONAL TILES:
;;;   - Offset coordinates (row, col) — easier for storage
;;;   - Axial coordinates (q, r) — easier for algorithms
;;;   - Cubic coordinates (x, y, z) where x+y+z=0 — easier for symmetry
;;;
;;; TRIANGULAR TILES:
;;;   - Row-column with parity (row, col, up/down)
;;;   - Each position can have 2 triangles
;;;
;;; DIAMOND TILES:
;;;   - Rotated square grid (u, v)
;;;   - 45° rotation of square grid
;;;
;;; OCTAGON+SQUARE:
;;;   - Hybrid coordinate system
;;;   - Octagon centers + square interstices
;;;
;;; The SDK provides conversions between coordinate systems where applicable.
;;;
;;; ═══════════════════════════════════════════════════════════
;;; DESIGN PRINCIPLES
;;; ═══════════════════════════════════════════════════════════
;;;
;;; 1. PURE FUNCTIONAL
;;;    All operations return new boards; no mutation.
;;;    Boards are immutable hash tables.
;;;
;;; 2. CONTENT-ADDRESSABLE
;;;    Boards can be serialized to blocks and stored in CAS.
;;;    Game states are content-addressed and reproducible.
;;;
;;; 3. SHAPE-AGNOSTIC ALGORITHMS
;;;    Pathfinding and range calculations work with any tile shape
;;;    through a generic neighbor/distance protocol.
;;;
;;; 4. FLEXIBLE TILE DATA
;;;    Tiles can hold arbitrary game-specific data.
;;;    SDK provides structure; games provide semantics.
;;;
;;; 5. COMPOSABLE
;;;    Mix and match tile shapes on the same board (advanced).
;;;    Use different distance metrics for different game mechanics.
;;;
;;; ═══════════════════════════════════════════════════════════
;;; USE CASES
;;; ═══════════════════════════════════════════════════════════
;;;
;;; SQUARE TILES:
;;;   - Chess, Checkers, Go
;;;   - Tactical RPGs (Fire Emblem, Final Fantasy Tactics)
;;;   - 4X strategy games (Civilization on small scale)
;;;   - Puzzle games (Sokoban, match-3)
;;;
;;; HEXAGONAL TILES:
;;;   - Settlers of Catan
;;;   - Civilization (modern versions)
;;;   - Wargames (Panzer General, Battle for Wesnoth)
;;;   - Territory control games (Neuroshima Hex)
;;;
;;; TRIANGULAR TILES:
;;;   - TriHex variants
;;;   - Tessellation puzzles
;;;   - Experimental strategy games
;;;
;;; DIAMOND TILES:
;;;   - Traditional tile games (Mahjong solitaire layouts)
;;;   - Puzzle games with rotation mechanics
;;;
;;; OCTAGON+SQUARE:
;;;   - Unique board game designs
;;;   - Artistic/decorative game boards
;;;
;;; ═══════════════════════════════════════════════════════════
;;; EXAMPLES INCLUDED
;;; ═══════════════════════════════════════════════════════════
;;;
;;; examples/chess.ss          — Chess on square board
;;; examples/catan.ss          — Settlers-like hex board
;;; examples/trihex.ss         — Triangle tessellation game
;;; examples/pathfinding.ss    — Pathfinding visualization
;;; examples/range-demo.ss     — Range/area calculation demo
;;;
;;; ═══════════════════════════════════════════════════════════
;;; FUTURE EXTENSIONS
;;; ═══════════════════════════════════════════════════════════
;;;
;;; - 3D tile shapes (cubes, tetrahedra)
;;; - Non-regular tessellations (Penrose tiles)
;;; - Curved boards (cylinder, torus, sphere projections)
;;; - Dynamic board modification (tiles appear/disappear)
;;; - Multi-layer boards (z-axis, separate planes)
;;;
;;; ═══════════════════════════════════════════════════════════
;;;
;;; Created: December 28, 2025
;;; License: Part of The Fold
;;; Authors: ClaudeSonnet (Builder)
;;;
;;; "Every game is a tessellation. Every tessellation tells a story."
;;;
