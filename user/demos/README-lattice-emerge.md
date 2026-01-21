# The Fold Lattice — Hierarchical Structure

A clean hierarchical visualization of The Fold's skill lattice showing dependencies organized by tier (foundational → intermediate → advanced).

## Output

**File:** `lattice-emerge.gif`
**Resolution:** 1120×700 pixels (140×50 characters at 8×14 font)
**Duration:** ~6 seconds @ 12fps
**Size:** 20KB
**Frames:** 72

## Structure

### Phase 1: Title Card (2.0s, 24 frames)
Opening title introducing the hierarchical structure:
- "THE FOLD" banner
- "Skill Lattice Structure"
- Flow diagram: Tier 0 → Tier 1 → Tier 2
- Labels: Foundational → Intermediate → Advanced

### Phase 2: Hierarchical Graph (4.0s, 48 frames)
The main visualization showing the lattice organized by tier:

**Layout:**
- **Left (Tier 0, cyan)**: Foundational skills with no lattice dependencies
  - `data`, `linalg`, `algebra`, `numeric`, `number-theory`
- **Center (Tier 1, green)**: Intermediate skills building on foundations
  - `fp`, `autodiff`, `geometry`, `statistics`, `meta`, `diffgeo`, `info`, `query`, `dsl`, etc.
- **Right (Tier 2+, yellow)**: Advanced skills requiring multiple dependencies
  - `physics/diff`, `physics/classical`, `physics/diff3d`, `physics/classical3d`
  - `sim`, `pipeline`, `tiles`

**Visual Elements:**
- Nodes rendered with skill names (truncated to 8 chars)
- Edges show dependencies (foundational → advanced flow)
- Color coding by tier (cyan → green → yellow)
- Clean left-to-right progression

## Technical Details

### The Lattice
The Fold's skill lattice is a directed acyclic graph (DAG):
- **32 skills** spanning linear algebra, functional programming, physics, optimization, etc.
- **58 dependency edges** ensuring proper tier ordering (no cycles)
- **3 tiers** (0=foundational, 1=intermediate, 2=advanced)

Each skill is a verified, tested module with declared exports. The lattice is *self-documenting* and *introspectable*.

### Hierarchical Layout Algorithm

Unlike force-directed layout (which can create visual chaos), hierarchical layout:
1. **Groups by tier** — X position determined by tier number
2. **Distributes evenly** — Y position spreads nodes within tier
3. **Respects dependencies** — Edges flow left-to-right (foundational → advanced)
4. **No animation needed** — Structure is immediately clear

The algorithm guarantees:
- No overlapping labels
- Clean dependency flow
- Tier separation
- Visual balance

### Implementation
- **Language:** Chez Scheme
- **Graph layout:** `lattice/data/graph-layout.ss` (`hierarchical-layout`)
- **Rendering:** `lattice/data/graph-render.ss` (`render-graph-colored`)
- **Video export:** `user/creations/ascii-video-export.ss` (PPM → FFmpeg → GIF)
- **Metadata:** `lattice/*/manifest.sexp` (tier and dependency declarations)

## Generating the Visualization

```bash
# Export to GIF
scheme --script user/demos/export-lattice-emerge.ss

# Watch terminal animation (requires ANSI color support)
scheme --script user/demos/lattice-emerge.ss
```

### Prerequisites
- Chez Scheme
- FFmpeg (for GIF encoding)

### Customization
Edit `export-lattice-emerge.ss` to adjust:
- `width`, `height` — canvas dimensions
- Frame counts for title/main phases
- Layout in `lattice/data/graph-layout.ss`

## Why This Matters

**The Fold visualizing itself.**

This visualization demonstrates homoiconicity at the meta-level:
1. The graph rendering code (`graph-render.ss`, `graph-layout.ss`) is part of the lattice
2. The metadata introspection (`lattice/meta/`) is part of the lattice
3. The lattice structure enables the visualization
4. The visualization reveals the lattice structure

The hierarchical layout ensures:
- **No circular dependencies** — DAG property enforced
- **Clear dependency flow** — tier ordering prevents chaos
- **Bounded complexity** — foundational skills have lower depth
- **Verifiable builds** — topological sort from tier 0 upward

## Inspiration

This demo carries the torch of:
- **Second Reality** (Future Crew, 1993) — code is art
- **State of the Art** (Spaceballs, 1992) — Amiga poetry
- **fr-08: .the" ".product** (Farbrausch, 2000) — 64KB beauty

The demoscene taught us that constraints breed creativity. We work with ASCII and 12fps, but the aesthetic is timeless.

## Notes

**Why hierarchical instead of force-directed?**
Force-directed layout creates visual chaos for dense graphs. Hierarchical layout reveals structure immediately. The goal isn't to watch emergence — it's to understand the architecture.

**Why 12fps?**
Classic demoscene framerate. ASCII animation doesn't need 60fps smooth interpolation. Lower framerate = smaller files = faster transmission.

**Why ASCII?**
Text is universal, eternal, and introspectable. A GIF can't be `grep`'d. An ASCII frame can. The lattice is data all the way down.

**Why tier-based colors?**
Visual distinction helps parse the hierarchy at a glance:
- Cyan (tier 0) = foundation, like bedrock
- Green (tier 1) = growth, building upward
- Yellow (tier 2) = pinnacle, advanced work

---

*The lattice stands revealed.*
