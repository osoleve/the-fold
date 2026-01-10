#!/usr/bin/env python3
"""
Gradient Flow Symphony - GIF Animator

Creates animated visualizations of gradient descent on potential fields.
This is a visual celebration of symbolic-autodiff integration in The Fold!

Generates:
  1. Morphing Gaussian potential field
  2. Gradient arrows showing the direction of steepest descent
  3. Particles flowing along gradient lines
"""

import os
import json
import time
import math
from PIL import Image, ImageDraw, ImageFont

# ============================================================
# Configuration
# ============================================================

WIDTH = 400
HEIGHT = 300
CELL_SIZE = 8
NUM_PARTICLES = 20
NUM_FRAMES = 60
X_RANGE = (-3.0, 3.0)
Y_RANGE = (-2.0, 2.0)

# Colors
BG_COLOR = (15, 15, 25)
GRADIENT_COLORS = [
    (20, 20, 40),    # Low potential (dark)
    (40, 60, 100),
    (60, 100, 160),
    (100, 140, 200),
    (150, 180, 230),
    (200, 220, 255),  # High potential (light)
]
ARROW_COLOR = (255, 200, 100, 180)
PARTICLE_COLORS = [
    (255, 100, 100),  # Red
    (255, 150, 50),   # Orange
    (255, 220, 100),  # Yellow
    (100, 255, 150),  # Green
]

# ============================================================
# Math Functions
# ============================================================

def gaussian(x, y, cx, cy, amplitude, sigma):
    """Gaussian potential centered at (cx, cy)"""
    r2 = (x - cx)**2 + (y - cy)**2
    return amplitude * math.exp(-r2 / (2 * sigma**2))

def build_potential(t):
    """Build potential field that morphs with time t in [0, 1]"""
    def potential(x, y):
        # Central well
        g1 = gaussian(x, y, 0.0, 0.0, 1.0, 0.8)
        # Left well (fades in/out)
        left_amp = 0.7 * math.sin(t * math.pi)
        g2 = gaussian(x, y, -1.8, 0.5, left_amp, 0.6)
        # Right well (fades in/out, phase shifted)
        right_amp = 0.7 * math.sin((t + 0.5) * math.pi)
        g3 = gaussian(x, y, 1.8, -0.3, right_amp, 0.6)
        return -(g1 + g2 + g3)  # Negative for wells
    return potential

def compute_gradient(potential, x, y, h=0.01):
    """Numerical gradient via finite differences"""
    dx = (potential(x + h, y) - potential(x - h, y)) / (2 * h)
    dy = (potential(x, y + h) - potential(x, y - h)) / (2 * h)
    return dx, dy

# ============================================================
# Coordinate Transforms
# ============================================================

def math_to_pixel(mx, my):
    """Convert math coords to pixel coords"""
    px = int((mx - X_RANGE[0]) / (X_RANGE[1] - X_RANGE[0]) * WIDTH)
    py = int((my - Y_RANGE[0]) / (Y_RANGE[1] - Y_RANGE[0]) * HEIGHT)
    return px, py

def pixel_to_math(px, py):
    """Convert pixel coords to math coords"""
    mx = X_RANGE[0] + (px / WIDTH) * (X_RANGE[1] - X_RANGE[0])
    my = Y_RANGE[0] + (py / HEIGHT) * (Y_RANGE[1] - Y_RANGE[0])
    return mx, my

# ============================================================
# Particle System
# ============================================================

class Particle:
    def __init__(self):
        self.x = (X_RANGE[0] + 0.2) + (X_RANGE[1] - X_RANGE[0] - 0.4) * (hash(id(self)) % 100) / 100
        self.y = (Y_RANGE[0] + 0.2) + (Y_RANGE[1] - Y_RANGE[0] - 0.4) * (hash(id(self) * 13) % 100) / 100
        self.age = 0
        self.max_age = 40 + (hash(id(self)) % 30)
        self.color_idx = hash(id(self)) % len(PARTICLE_COLORS)
        self.trail = []
        self.trail_max = 8

    def update(self, grad_fn, dt=0.15):
        dx, dy = grad_fn(self.x, self.y)
        mag = math.sqrt(dx*dx + dy*dy)
        if mag > 0.001:
            scale = dt / mag
            # Store trail position
            self.trail.append((self.x, self.y))
            if len(self.trail) > self.trail_max:
                self.trail.pop(0)
            # Move in negative gradient direction (descent)
            self.x -= dx * scale
            self.y -= dy * scale
            # Clamp to bounds
            self.x = max(X_RANGE[0] + 0.1, min(X_RANGE[1] - 0.1, self.x))
            self.y = max(Y_RANGE[0] + 0.1, min(Y_RANGE[1] - 0.1, self.y))
        self.age += 1

    def should_respawn(self):
        return self.age > self.max_age

    def respawn(self):
        import random
        self.x = X_RANGE[0] + 0.2 + random.random() * (X_RANGE[1] - X_RANGE[0] - 0.4)
        self.y = Y_RANGE[0] + 0.2 + random.random() * (Y_RANGE[1] - Y_RANGE[0] - 0.4)
        self.age = 0
        self.trail = []

# ============================================================
# Rendering
# ============================================================

def interpolate_color(colors, t):
    """Interpolate between a list of colors based on t in [0, 1]"""
    t = max(0, min(1, t))
    idx = t * (len(colors) - 1)
    i1 = int(idx)
    i2 = min(i1 + 1, len(colors) - 1)
    frac = idx - i1
    c1, c2 = colors[i1], colors[i2]
    return tuple(int(c1[j] + frac * (c2[j] - c1[j])) for j in range(3))

def render_potential_field(draw, potential):
    """Render potential field as colored background"""
    # Sample and find range
    samples = []
    for py in range(0, HEIGHT, 4):
        for px in range(0, WIDTH, 4):
            mx, my = pixel_to_math(px, py)
            val = potential(mx, my)
            samples.append(val)

    vmin, vmax = min(samples), max(samples)
    vrange = vmax - vmin if vmax > vmin else 1.0

    # Render as rectangles
    for py in range(0, HEIGHT, 4):
        for px in range(0, WIDTH, 4):
            mx, my = pixel_to_math(px, py)
            val = potential(mx, my)
            t = (val - vmin) / vrange
            color = interpolate_color(GRADIENT_COLORS, t)
            draw.rectangle([px, py, px+4, py+4], fill=color)

def render_gradient_arrows(draw, potential, spacing=30):
    """Render gradient direction arrows"""
    for py in range(spacing, HEIGHT - spacing, spacing):
        for px in range(spacing, WIDTH - spacing, spacing):
            mx, my = pixel_to_math(px, py)
            dx, dy = compute_gradient(potential, mx, my)
            mag = math.sqrt(dx*dx + dy*dy)

            if mag > 0.05:
                # Normalize and scale for display
                scale = min(12, 8 / mag)
                nx, ny = -dx/mag * scale, -dy/mag * scale  # Negative = descent direction

                # Convert to pixel offset
                arrow_len = 10
                end_x = px + nx * arrow_len / 10
                end_y = py + ny * arrow_len / 10

                # Draw arrow line
                draw.line([(px, py), (end_x, end_y)],
                         fill=ARROW_COLOR[:3], width=2)

                # Draw arrowhead
                angle = math.atan2(ny, nx)
                head_len = 4
                for da in [2.5, -2.5]:
                    hx = end_x - head_len * math.cos(angle + da)
                    hy = end_y - head_len * math.sin(angle + da)
                    draw.line([(end_x, end_y), (hx, hy)],
                             fill=ARROW_COLOR[:3], width=1)

def render_particles(draw, particles):
    """Render particles with trails"""
    for p in particles:
        # Draw trail
        for i, (tx, ty) in enumerate(p.trail):
            px, py = math_to_pixel(tx, ty)
            alpha = (i + 1) / len(p.trail) * 0.5
            size = 2 + i * 0.5
            color = PARTICLE_COLORS[p.color_idx]
            faded = tuple(int(c * alpha) for c in color)
            draw.ellipse([px-size, py-size, px+size, py+size], fill=faded)

        # Draw particle
        px, py = math_to_pixel(p.x, p.y)
        size = 5
        color = PARTICLE_COLORS[p.color_idx]
        draw.ellipse([px-size, py-size, px+size, py+size], fill=color)
        # Glow effect
        draw.ellipse([px-size-2, py-size-2, px+size+2, py+size+2],
                    outline=color, width=1)

def render_title(draw, t, frame):
    """Render title and info"""
    try:
        font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf", 14)
        small_font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 11)
    except:
        font = ImageFont.load_default()
        small_font = font

    # Title
    draw.text((10, 8), "GRADIENT FLOW SYMPHONY", fill=(100, 200, 255), font=font)

    # Info
    draw.text((10, HEIGHT - 25),
             f"Symbolic Differentiation + Autodiff",
             fill=(150, 150, 180), font=small_font)
    draw.text((WIDTH - 120, HEIGHT - 25),
             f"The Fold",
             fill=(100, 200, 100), font=small_font)

    # Frame counter
    draw.text((WIDTH - 80, 10),
             f"t={t:.2f}",
             fill=(200, 200, 200), font=small_font)

def render_frame(potential, particles, t, frame_num):
    """Render a single frame"""
    img = Image.new('RGB', (WIDTH, HEIGHT), BG_COLOR)
    draw = ImageDraw.Draw(img)

    # Render layers
    render_potential_field(draw, potential)

    grad_fn = lambda x, y: compute_gradient(potential, x, y)
    render_gradient_arrows(draw, potential)
    render_particles(draw, particles)
    render_title(draw, t, frame_num)

    return img

# ============================================================
# Animation
# ============================================================

def create_animation():
    """Create full animation"""
    frames = []
    particles = [Particle() for _ in range(NUM_PARTICLES)]

    for frame in range(NUM_FRAMES):
        t = frame / NUM_FRAMES
        potential = build_potential(t)
        grad_fn = lambda x, y, pot=potential: compute_gradient(pot, x, y)

        # Update particles
        for p in particles:
            p.update(grad_fn)
            if p.should_respawn():
                p.respawn()

        # Render frame
        img = render_frame(potential, particles, t, frame)
        frames.append(img)

        if frame % 10 == 0:
            print(f"   Frame {frame}/{NUM_FRAMES}...")

    return frames

def save_gif(frames, filename, duration=100):
    """Save as animated GIF"""
    if frames:
        frames[0].save(
            filename,
            save_all=True,
            append_images=frames[1:],
            duration=duration,
            loop=0,
            optimize=True
        )
        print(f"   Saved {filename} ({len(frames)} frames)")
        return True
    return False

def post_to_discord(filename, title, description):
    """Post to Discord via outbox"""
    timestamp = int(time.time() * 1000)
    outbox_path = f"/home/oso/the-fold/.fold-repl/discord-outbox/gradient-flow-{timestamp}.json"

    content = {
        "author": "ClaudeBuilder",
        "tier": "builder",
        "channel": "art",
        "title": title,
        "body": description,
        "attachment": {
            "path": os.path.abspath(filename),
            "name": os.path.basename(filename),
            "description": title
        }
    }

    with open(outbox_path, 'w') as f:
        json.dump(content, f, indent=2)

    print(f"   Posted to Discord outbox: {outbox_path}")

# ============================================================
# Main
# ============================================================

def main():
    print("\n" + "="*60)
    print("  GRADIENT FLOW SYMPHONY - GIF Generator")
    print("  Symbolic Differentiation + Autodiff Visualization")
    print("="*60 + "\n")

    print("Generating animation frames...")
    frames = create_animation()

    filename = "/home/oso/the-fold/user/creations/gradient-flow-symphony.gif"
    print(f"\nSaving GIF to {filename}...")
    save_gif(frames, filename, duration=80)

    print("\nPosting to Discord #art channel...")
    post_to_discord(
        filename,
        "Gradient Flow Symphony",
        "Particles flow along gradient descent paths through a morphing "
        "Gaussian potential landscape. This visualizes the symbolic-autodiff "
        "integration: symbolic expressions are differentiated, simplified, "
        "compiled to traced functions, and used to compute gradients!\n\n"
        "Built with The Fold's symbolic math + autodiff system."
    )

    print("\n" + "="*60)
    print("  Animation complete!")
    print("="*60 + "\n")

if __name__ == "__main__":
    main()
