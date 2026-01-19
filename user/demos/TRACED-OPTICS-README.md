# Traced Optics Demo

**A demoscene-style ASCII animation showcasing gradient flow through optic-focused paths.**

## What This Demonstrates

This demo visualizes the marriage of **optics** (composable data accessors) and **automatic differentiation** (gradient computation). The traced-optics integration (`lattice/autodiff/traced-optics.ss`) enables computing gradients with respect to optic-focused parameters, unlocking powerful optimization patterns.

## Scenes

### Scene 1: Lens Gradient Flow (0-2.5s)
Shows a nested data structure with a lens path focusing on a single parameter `x`. The visualization demonstrates:
- Nested structure: `(outer (inner x y))`
- Lens composition: `outer >>> inner-x`
- Gradient computation: `∂loss/∂x` where loss = `(x - 5)²`
- Real-time gradient arrow showing direction and magnitude
- Parameter convergence toward the target value (5.0)

**Key insight:** Optics specify *which* parameter to differentiate, autodiff computes *how* it affects the loss.

### Scene 2: Traversal Gradients (2.5-5s)
Demonstrates multi-target optimization using a traversal optic. Three parameters are optimized simultaneously toward their individual targets:
- `x₀ → 2.0`
- `x₁ → 4.0`
- `x₂ → 6.0`

Each parameter has its own gradient computed independently, enabling parallel optimization of multiple targets through a single traversal.

**Key insight:** Traversals enable batch gradient computation - one pass computes gradients for all focused elements.

### Scene 3: Optimization Path (5-8s)
Visualizes gradient descent in 2D parameter space:
- Loss landscape shown as a contour field (darker = lower loss)
- Optimization trajectory marked with `O` characters
- Current position marked with `X`
- Minimum at `(5, 5)` (shown as `@`)

The path shows how gradient descent navigates the loss landscape toward the minimum, with each step computed via `optimize-at`.

**Key insight:** `optimize-at` combines gradient computation and parameter update in one operation, streamlining optimization loops.

### Scene 4: Credits (8-12s)
Classic demoscene sine scroller with credits and greets.

## Technical Specs

- **Resolution:** 100×42 characters (800×600 pixels at 8×14 font)
- **Frame rate:** 12 fps (true to demoscene aesthetic)
- **Duration:** 12 seconds (144 frames)
- **Effects:** Starfield parallax, plasma visualization, sine scroller, gradient arrows
- **Output:** GIF animation (`traced-optics-demo.gif`)

## Key API Demonstrated

```scheme
;; Compute gradient through optic focus
(optic-gradient loss-fn lens-path structure)

;; Compute gradients for all traversal targets
(optic-gradient-list loss-fn traversal structure)

;; Single optimization step
(optimize-at lens structure loss-fn learning-rate)

;; Compose optics for deep focusing
(>>> outer-lens inner-lens)
```

## Running the Demo

```bash
scheme --script user/demos/traced-optics-demo.ss
```

Output: `user/demos/traced-optics-demo.gif`

## The Demoscene Spirit

This visualization follows classic demoscene principles:
- **Fill the frame:** 100×42 gives room for layered composition
- **Respect the rhythm:** Intro, build, peak, outro structure
- **Layer effects:** Starfield background, plasma mid-ground, UI foreground
- **Character density as "color":** Using ` .',:;-~=+*#%@` for intensity
- **12fps aesthetic:** Choppy frames are part of the charm

This is not a "visualization" - it's a **demo**. Demos have soul.

## Credits

- **Code:** DEMOSCENE
- **Math:** The Fold
- **Greets:** Anthropic · Future Crew · All ASCII Artists

*Created with the traced-optics integration, proof that code is art.*
