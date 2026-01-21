# Lorenz Butterfly - ASCII Strange Attractor Demo

The signature demo for The Fold's chaos module. A spinning 3D visualization of the Lorenz attractor rendered entirely in ASCII with depth-based shading and rainbow color cycling.

## What You're Seeing

The Lorenz attractor is a set of chaotic solutions to the Lorenz system:

```
dx/dt = σ(y - x)
dy/dt = x(ρ - z) - y
dz/dt = xy - βz
```

With parameters σ=10, ρ=28, β=8/3 (the classic "butterfly" configuration).

**Visualization techniques:**
- 3D rotation around Y-axis (one full revolution per loop)
- X-axis tilt (0.3 radians) for 3D depth perception
- Z-depth based intensity (closer points = brighter characters)
- Hue cycling based on trajectory position
- ASCII character density ramp for grayscale shading
- Proper depth buffering (closer points occlude distant ones)

## Demo Variants

### `lorenz-signature.gif` (Recommended)
**The canonical showcase version**
- 800x588 pixels (100x42 terminal chars)
- 60 frames, 5 second loop at 12fps
- 10,000 trajectory points
- 296KB file size
- Perfect for README headers and social media

### `lorenz-butterfly.gif` (Original)
**Quick render for testing**
- 800x588 pixels
- 48 frames, 4 second loop at 12fps
- 8,000 trajectory points
- 244KB file size

### `lorenz-butterfly-hq.mp4` (Cinema)
**High resolution for presentations**
- 1280x714 pixels (160x51 terminal chars)
- 72 frames, 6 second loop at 12fps
- 12,000 trajectory points
- 1.0MB file size
- H.264 codec, high quality encoding

## Technical Stack

**Chaos dynamics:** `lattice/sim/dynamics/chaos.ss`
- Runge-Kutta 4th order integrator
- Classic attractor systems (Lorenz, Rössler, Thomas, etc.)
- Configurable time step and skip for trajectory generation

**3D Rendering:** `lattice/sim/dynamics/attractor-render.ss`
- 3D rotation matrices (X, Y, Z axes)
- Orthographic projection with depth buffer
- Z-depth to intensity mapping
- ANSI 256-color palette generation
- 69-character ASCII density ramp

**Video Export:** `user/creations/ascii-video*.ss`
- Frame buffer abstraction
- ANSI code stripping and parsing
- 8x14 bitmap font renderer
- PPM image generation
- FFmpeg pipeline (palettegen + paletteuse for optimal GIF compression)

## Generating The Demos

```bash
# Signature version (recommended)
scheme --script user/demos/lorenz-signature.ss

# Original version
scheme --script user/demos/lorenz-butterfly-demo.ss

# High quality MP4
scheme --script user/demos/lorenz-butterfly-hq.ss
```

Each script takes 10-30 seconds to render depending on resolution and frame count.

## The Demoscene Aesthetic

This follows classic demoscene principles:

1. **Fill the frame** - 100x42 chars uses the full terminal canvas
2. **Layer depth** - Rotation + depth shading creates 3D effect
3. **Smooth motion** - 48-72 frames gives butter-smooth animation
4. **Mathematical beauty** - Let the strange attractor speak for itself
5. **Technical flex** - RK4 integration, depth buffers, color mapping, all in Scheme

The Lorenz butterfly spinning in ASCII is a tribute to:
- Future Crew's *Second Reality* (1993)
- The Amiga demo scene
- Every coder who made beauty from mathematics

## What This Demonstrates

From the technical report perspective, this showcases:

- **Content-addressable computation** - All attractor code is stored in the CAS
- **Lattice skill composition** - Chaos dynamics + rendering + video export
- **Pure functional rendering** - Entire pipeline is deterministic
- **Zero external dependencies** - Built entirely on Chez Scheme + FFmpeg

From the "cool factor" perspective:

**The Fold can render spinning strange attractors in ASCII and export them as GIFs.**

That's the elevator pitch.

---

*The Lorenz butterfly spins.*
*Chaos rendered in ASCII.*
*The Fold remembers.*
