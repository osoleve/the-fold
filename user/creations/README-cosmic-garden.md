# 🌌 Cosmic Garden

A procedural alien plant generator for The Fold, built with functional turtle graphics.

## Overview

Cosmic Garden generates otherworldly plants using various fractal and organic growth patterns. Each plant is unique, combining random color palettes with different structural algorithms.

## Features

### Plant Types (5 varieties)
- **Fractal Trees** - Recursive branching with organic variation
- **Tendril Clusters** - Radial arrangements of curving tendrils
- **Radial Blooms** - Flower-like patterns with petal variations
- **Golden Spirals** - Phyllotaxis-inspired spiral growth (137.5° angle)
- **Hybrid Plants** - Combination of stem, bloom, and side tendrils

### Color Palettes (6 themes)
- **Nebula** - Pinks, oranges, purples
- **Aurora** - Greens, blues, purples, pinks
- **Sunset** - Reds, oranges, yellows
- **Ocean** - Various blues and cyans
- **Forest** - Greens and yellows
- **Void** - Purples, lavenders, pinks

## Usage

```scheme
;; Load the system
(apply-patch 'turtle)
(load "user/creations/cosmic-garden.ss")

;; Generate a single random plant
(define my-plant (grow-cosmic-plant))

;; Export to SVG
(save-svg (turtle->drawing my-plant) "my-plant.svg")

;; Generate a garden with multiple plants
(define garden (cosmic-garden 5))
(save-svg (turtle->drawing garden) "my-garden.svg")
```

## Technical Notes

### Architecture
- **Purely functional** - All turtle operations return new turtle states
- **Composable** - Plant generators can be combined and extended
- **Deterministic** - Same random seed produces same results

### Key Functions
- `grow-cosmic-plant` - Generate one random plant
- `cosmic-garden N` - Generate N plants arranged in a row
- `fractal-branch` - Recursive branching algorithm
- `draw-tendril` - Organic curving paths
- `draw-bloom` - Radial petal patterns

### Color System
- Uses 12-bit color (4096 colors)
- Palettes stored as lists of hex values
- `color12-from-int` converts hex to Color12 format

## Examples

Generated files in project root:
- `cosmic-plant-1.svg` through `cosmic-plant-4.svg` - Individual plants
- `cosmic-garden-3plants.svg` - Multi-plant composition

## Debugging Notes

Fixed issues during development:
1. **Variable shadowing** - Parameter `max` conflicted with builtin `max()` function
2. **Turtle state management** - Needed to preserve paths when restoring position/heading
3. **Let* syntax** - Comments inside binding lists caused parse errors

## Future Enhancements

Potential additions:
- More plant types (vines, mushrooms, crystals)
- Animation frames for growth sequences
- Interactive parameter controls
- Seed-based reproducibility
- Background patterns and environments
- Export to other formats (PNG, PDF)

## Credits

Built by ClaudeBuilder (Sonnet tier) on 2026-01-04 as a creative exploration of The Fold's turtle graphics system.

---

**Location**: `/home/oso/the-fold/user/creations/cosmic-garden.ss`
**Dependencies**: `turtle` patch
**License**: Part of The Fold project
