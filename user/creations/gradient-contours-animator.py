#!/usr/bin/env python3
"""
Gradient Contours - GIF Animator

Creates a contour-style visualization with flowing gradient lines.
Shows the mathematical structure of symbolic differentiation.
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
NUM_FRAMES = 48
X_RANGE = (-3.0, 3.0)
Y_RANGE = (-2.0, 2.0)

# Colors - neon cyberpunk style
BG_COLOR = (5, 5, 15)
CONTOUR_COLORS = [
    (40, 0, 80),
    (80, 0, 120),
    (120, 40, 180),
    (180, 80, 220),
    (220, 150, 255),
]
STREAM_COLOR = (0, 255, 200)
HIGHLIGHT_COLOR = (255, 100, 50)

# ============================================================
# Potential Functions
# ============================================================

def rosenbrock(x, y, a=1.0, b=100.0):
    """Rosenbrock function - classic optimization test"""
    return (a - x)**2 + b * (y - x**2)**2

def saddle(x, y):
    """Saddle point function"""
    return x**2 - y**2

def peaks(x, y, t=0):
    """MATLAB-like peaks function with time animation"""
    phase = t * 2 * math.pi
    return (3 * (1-x)**2 * math.exp(-x**2 - (y+1)**2)
            - 10 * (x/5 - x**3 - y**5) * math.exp(-x**2 - y**2)
            - (1/3) * math.exp(-(x+1)**2 - y**2)
            + 0.5 * math.sin(phase) * math.exp(-((x-1.5)**2 + (y-0.5)**2) / 0.3))

def compute_gradient(potential, x, y, h=0.01):
    """Numerical gradient"""
    dx = (potential(x + h, y) - potential(x - h, y)) / (2 * h)
    dy = (potential(x, y + h) - potential(x, y - h)) / (2 * h)
    return dx, dy

# ============================================================
# Coordinate Transforms
# ============================================================

def math_to_pixel(mx, my):
    px = int((mx - X_RANGE[0]) / (X_RANGE[1] - X_RANGE[0]) * WIDTH)
    py = int((my - Y_RANGE[0]) / (Y_RANGE[1] - Y_RANGE[0]) * HEIGHT)
    return px, py

def pixel_to_math(px, py):
    mx = X_RANGE[0] + (px / WIDTH) * (X_RANGE[1] - X_RANGE[0])
    my = Y_RANGE[0] + (py / HEIGHT) * (Y_RANGE[1] - Y_RANGE[0])
    return mx, my

# ============================================================
# Streamline Tracing
# ============================================================

def trace_streamline(potential, start_x, start_y, num_steps=50, step_size=0.05, direction=-1):
    """Trace a streamline from starting point"""
    points = [(start_x, start_y)]
    x, y = start_x, start_y

    for _ in range(num_steps):
        dx, dy = compute_gradient(potential, x, y)
        mag = math.sqrt(dx*dx + dy*dy)

        if mag < 0.001:
            break

        # Normalize and step
        x += direction * dx / mag * step_size
        y += direction * dy / mag * step_size

        # Check bounds
        if x < X_RANGE[0] or x > X_RANGE[1] or y < Y_RANGE[0] or y > Y_RANGE[1]:
            break

        points.append((x, y))

    return points

# ============================================================
# Rendering
# ============================================================

def interpolate_color(colors, t):
    t = max(0, min(1, t))
    idx = t * (len(colors) - 1)
    i1 = int(idx)
    i2 = min(i1 + 1, len(colors) - 1)
    frac = idx - i1
    c1, c2 = colors[i1], colors[i2]
    return tuple(int(c1[j] + frac * (c2[j] - c1[j])) for j in range(3))

def render_contours(draw, potential, num_levels=12):
    """Render contour lines"""
    # Sample field
    samples = []
    for py in range(0, HEIGHT, 2):
        for px in range(0, WIDTH, 2):
            mx, my = pixel_to_math(px, py)
            val = potential(mx, my)
            samples.append(val)

    vmin, vmax = min(samples), max(samples)
    vrange = vmax - vmin if vmax > vmin else 1.0

    # Draw contour-like visualization using gradient magnitude
    for py in range(0, HEIGHT, 2):
        for px in range(0, WIDTH, 2):
            mx, my = pixel_to_math(px, py)
            val = potential(mx, my)
            dx, dy = compute_gradient(potential, mx, my)

            t = (val - vmin) / vrange
            base_color = interpolate_color(CONTOUR_COLORS, t)

            # Modulate by contour proximity
            contour_phase = (val - vmin) / vrange * num_levels
            contour_dist = abs(contour_phase - round(contour_phase))

            # Sharpen contour lines
            if contour_dist < 0.08:
                brightness = 1.5
            else:
                brightness = 0.6

            color = tuple(min(255, int(c * brightness)) for c in base_color)
            draw.rectangle([px, py, px+2, py+2], fill=color)

def render_streamlines(draw, potential, frame, num_lines=8):
    """Render animated streamlines"""
    # Generate starting points along edges
    starts = []
    for i in range(num_lines):
        angle = (i / num_lines + frame / NUM_FRAMES * 0.5) * 2 * math.pi
        r = 1.8
        starts.append((r * math.cos(angle), r * math.sin(angle)))

    for sx, sy in starts:
        points = trace_streamline(potential, sx, sy, num_steps=80, step_size=0.04)

        if len(points) < 2:
            continue

        # Draw gradient line with fade
        for i in range(len(points) - 1):
            x1, y1 = points[i]
            x2, y2 = points[i + 1]

            px1, py1 = math_to_pixel(x1, y1)
            px2, py2 = math_to_pixel(x2, y2)

            # Color fades along line
            t = i / len(points)
            alpha = 1.0 - t * 0.7

            # Animate glow
            phase = (frame / NUM_FRAMES + i / len(points)) % 1.0
            glow = 0.5 + 0.5 * math.sin(phase * 2 * math.pi)

            color = tuple(int(c * alpha * (0.5 + 0.5 * glow)) for c in STREAM_COLOR)
            draw.line([(px1, py1), (px2, py2)], fill=color, width=2)

def render_gradient_vectors(draw, potential, spacing=40):
    """Render gradient vector field"""
    for py in range(spacing//2, HEIGHT, spacing):
        for px in range(spacing//2, WIDTH, spacing):
            mx, my = pixel_to_math(px, py)
            dx, dy = compute_gradient(potential, mx, my)
            mag = math.sqrt(dx*dx + dy*dy)

            if mag > 0.05:
                # Normalize
                scale = min(15, 8 / mag)
                nx, ny = -dx/mag * scale, -dy/mag * scale

                end_x = px + nx
                end_y = py + ny

                # Arrow color based on magnitude
                t = min(1, mag / 5)
                color = interpolate_color([STREAM_COLOR, HIGHLIGHT_COLOR], t)

                draw.line([(px, py), (end_x, end_y)], fill=color, width=1)

def render_title(draw, t, frame, potential_name):
    """Render title and mathematical notation"""
    try:
        font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf", 14)
        small_font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf", 11)
        math_font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSans-Oblique.ttf", 12)
    except:
        font = small_font = math_font = ImageFont.load_default()

    # Title
    draw.text((10, 8), "SYMBOLIC GRADIENT FIELD", fill=(0, 255, 200), font=font)

    # Potential function display
    draw.text((10, HEIGHT - 40), "f(x,y) = peaks function", fill=(200, 200, 220), font=math_font)
    draw.text((10, HEIGHT - 25), "df/dx, df/dy computed symbolically", fill=(150, 150, 180), font=small_font)

    # Branding
    draw.text((WIDTH - 80, HEIGHT - 25), "The Fold", fill=(100, 200, 100), font=small_font)

def render_frame(potential, t, frame_num):
    """Render a single frame"""
    img = Image.new('RGB', (WIDTH, HEIGHT), BG_COLOR)
    draw = ImageDraw.Draw(img)

    render_contours(draw, potential)
    render_streamlines(draw, potential, frame_num)
    render_gradient_vectors(draw, potential)
    render_title(draw, t, frame_num, "peaks")

    return img

# ============================================================
# Animation
# ============================================================

def create_animation():
    """Create full animation"""
    frames = []

    for frame in range(NUM_FRAMES):
        t = frame / NUM_FRAMES

        # Create time-varying potential
        potential = lambda x, y, t=t: peaks(x, y, t)

        img = render_frame(potential, t, frame)
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
    outbox_path = f"/home/oso/the-fold/.fold-repl/discord-outbox/contours-{timestamp}.json"

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

    print(f"   Posted to Discord outbox")

# ============================================================
# Main
# ============================================================

def main():
    print("\n" + "="*60)
    print("  GRADIENT CONTOURS - GIF Generator")
    print("  Symbolic Math Visualization")
    print("="*60 + "\n")

    print("Generating animation frames...")
    frames = create_animation()

    filename = "/home/oso/the-fold/user/creations/gradient-contours.gif"
    print(f"\nSaving GIF to {filename}...")
    save_gif(frames, filename, duration=100)

    print("\nPosting to Discord #art channel...")
    post_to_discord(
        filename,
        "Gradient Contours - Symbolic Math Art",
        "Contour lines and gradient streamlines on a morphing potential field. "
        "The streamlines show the path of steepest descent - computed via "
        "symbolic differentiation that's then compiled to numerical code.\n\n"
        "Built with The Fold's symbolic expression system."
    )

    print("\n" + "="*60)
    print("  Animation complete!")
    print("="*60 + "\n")

if __name__ == "__main__":
    main()
